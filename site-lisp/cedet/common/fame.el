;;; fame.el --- Framework for Applications' MEssages
;;
;; Copyright (C) 2004 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 28 Oct 2004
;; Keywords: status
;; X-RCS: $Id: fame.el,v 1.3 2005/09/30 20:07:29 zappo Exp $
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides a convenient framework for applications to
;; send messages distinguished by their level of importance, allowing
;; to customize how they will be actually rendered.
;;
;; The principle is to define a `channel' where to send messages at
;; particular levels, depending on their importance.  A channel is
;; identified by a non-nil symbol.  For example this library could
;; send its messages to the `fame' channel.  Four levels of importance
;; are recognized, for debug, informational, warning and error
;; messages.
;;
;; Messages at any particular level can be either discarded,
;; temporarily displayed, recorded in the message log buffer without
;; showing them in the echo area, or shown the usual way like through
;; the `message' function.  Messages shown in the echo area can be
;; recorded or not in the message log buffer.
;;
;; The `define-fame-channel' macro permits to easily define a new
;; channel, that is an option to customize how to display the message
;; levels for this channel, and the level specific functions to use to
;; send messages to this channel.
;;
;; Here is a small example:
;;
;;     (require 'fame)
;;     ...
;;     (define-fame-channel feature)
;;     ...
;;     (feature-send-debug "Some useful debug message")
;;     ...
;;     (condition-case err
;;         ...
;;       (error
;;        (feature-send-error "%s" (error-message-string err))))
;;     ...
;;     (feature-send-info "Some useful informational message")
;;     ...
;;     (provide 'feature)

;;; History:
;;

;;; Code:

;;; Constants and options
;;
(defconst fame-valid-levels
  '(:error :warning :info :debug)
  "Valid message levels.")

(defconst fame-valid-level-values
  '(t nolog temp temp-nolog log none)
  "Valid message level values.")

(defconst fame-default-level-values
  '(:debug log :info temp :warning t :error t)
  "Default display value of message levels.")

(define-widget 'fame-display-choice 'radio-button-choice
  "Widget to choose the display value of a level."
  :format "%v\n"
  :entry-format " %v%b"
  :args '((const :format "%v" :value t)
          (const :format "%v" :value nolog)
          (const :format "%v" :value temp)
          (const :format "%v" :value temp-nolog)
          (const :format "%v" :value log)
          (const :format "%v" :value none)))

(define-widget 'fame-level-widget 'const
  "Widget to display a level symbol."
  :format "   %t")

(define-widget 'fame-channel-widget 'list
  "Widget to customize the messages levels of a channel."
  :tag "Display value of message levels"
  :format "%{%t%}:\n%v\n"
  :args '((fame-level-widget :tag ":debug  " :value :debug)
          (fame-display-choice)
          (fame-level-widget :tag ":info   " :value :info)
          (fame-display-choice)
          (fame-level-widget :tag ":warning" :value :warning)
          (fame-display-choice)
          (fame-level-widget :tag ":error  " :value :error)
          (fame-display-choice)))

(defgroup fame nil
  "Framework for Applications' MEssages."
  :prefix "fame"
  :group 'lisp)

(defcustom fame-temp-message-delay 1
  "*Lifetime of a temporary message, in seconds."
  :group 'fame
  :type 'number)

;;; Core message functions
;;
(eval-and-compile

;;;; Read the message currently displayed in the echo area.
  (defalias 'fame-current-message
    (if (fboundp 'current-message)
        'current-message
      'ignore))

;;;; Show a message in the echo area without logging it.
  (if (fboundp 'lmessage)
      ;; XEmacs
      (defun fame-message-nolog (&rest args)
        "Display but don't log a message on the echo area.
ARGS are like those of the function `message'."
        (and args (apply 'lmessage 'no-log args)))
    ;; Emacs
    (defun fame-message-nolog (&rest args)
      "Display but don't log a message on the echo area.
ARGS will be passed to the function `message'."
      (and args
           (let ((message-log-max nil)) ;; No logging
             (apply 'message args))))
    )

;;;; Log a message without showing it in the echo area.
  (if (fboundp 'log-message)
      ;; XEmacs
      (defun fame-log-message (&rest args)
        "Log but don't display a message.
ARGS are like those of the function `message'."
        (and args (log-message 'message (apply 'format args))))
    ;; Emacs
    (defun fame-log-message (&rest args)
      "Log but don't display a message.
ARGS will be passed to the function `message'."
      (and args
           (let ((executing-kbd-macro t)) ;; Inhibit display!
             (apply 'message args))))
    )
  ;; If the above definition fails, here is a portable implementation
  ;; of a `log-message' function.
  '(defun fame-log-message (&rest args)
     "Log but don't display a message.
ARGS are like those of the function `message'."
     (when args
       (let ((text (apply 'format args)))
         (with-current-buffer
             (get-buffer-create (if (featurep 'xemacs)
                                    " *Message-Log*"
                                  "*Messages*"))
           (goto-char (point-max))
           (or (bobp) (bolp) (insert "\n"))
           (forward-line -1)
           (if (search-forward text nil t)
               (if (looking-at " \\[\\([0-9]+\\) times\\]")
                   (replace-match
                    (number-to-string
                     (1+ (string-to-number (match-string 1))))
                    nil nil nil 1)
                 (end-of-line)
                 (insert " [2 times]"))
             (forward-line 1)
             (insert text))))))

;;;; Log and temporarily show a message in the echo area.
  (condition-case nil
      (require 'timer)
    (error nil))
  ;; We need timers to display messages temporarily.
  (if (not (fboundp 'run-with-timer))

      (defun fame-temp-message-internal (fun &rest args)
        "Display a message temporarily through the function FUN.
ARGS are like those of the function `message'."
        ;; Without timers just call FUN.
        (and args (apply fun args)))

    (defvar fame-temp-message-timer nil)
    (defvar fame-temp-message-saved nil)

    (defun fame-temp-restore-message ()
      "Restore a message previously displayed in the echo area."
      (when (timerp fame-temp-message-timer)
        (cancel-timer fame-temp-message-timer)
        (setq fame-temp-message-timer nil))
      (when fame-temp-message-saved
        (prog1 (fame-message-nolog "%s" fame-temp-message-saved)
          (setq fame-temp-message-saved nil))))

    (defun fame-temp-message-internal (fun &rest args)
      "Display a message temporarily through the function FUN.
ARGS are like those of the function `message'."
      (when args
        (condition-case nil
            (progn
              (fame-temp-restore-message)
              (setq fame-temp-message-saved (fame-current-message))
              (prog1 (apply fun args)
                (setq fame-temp-message-timer
                      (run-with-timer fame-temp-message-delay nil
                                      'fame-temp-restore-message))))
          (error
           (fame-temp-restore-message)))))
    )
  )

(defsubst fame-temp-message (&rest args)
  "Display a message temporarily and log it.
ARGS are like those of the function `message'.
The original message is restored to the echo area after
`fame-temp-message-delay' seconds."
  (apply 'fame-temp-message-internal 'message args))

(defsubst fame-temp-message-nolog (&rest args)
  "Display a message temporarily without logging it.
ARGS are like those of the function `message'.
The original message is restored to the echo area after
`fame-temp-message-delay' seconds."
  (apply 'fame-temp-message-internal 'fame-message-nolog args))

;;; Handling of message levels
;;
(defun fame-check-level (level)
  "Check that LEVEL is a valid message level.
If valid, return LEVEL.  Signal an error otherwise."
  (if (memq level fame-valid-levels)
      level
    (signal 'wrong-type-argument
            (list fame-valid-levels level))))

(defun fame-check-level-value (value)
  "Check that VALUE is a valid message level value.
If valid, return VALUE.  Signal an error otherwise."
  (if (memq value fame-valid-level-values)
      value
    (signal 'wrong-type-argument
            (list fame-valid-level-values value))))

(defun fame-check-channel (channel)
  "Check that CHANNEL is a non-nil symbol.
If valid, return CHANNEL.  Signal an error otherwise."
  (if (and channel (symbolp channel))
      channel
    (signal 'wrong-type-argument
            (list 'symbolp channel))))

(defun fame-check-channel-levels (levels)
  "Check that LEVELS is a valid specification of channel levels.
If valid, return a normalized form of the specification.
Signal an error otherwise."
  (let (spec)
    (dolist (level fame-valid-levels)
      (push (fame-check-level-value
             ;; A nil level value means to use the default value.
             (or (plist-get levels level)
                 (plist-get fame-default-level-values level))) spec)
      (push level spec))
    spec))

(defsubst fame-channel-symbol (channel)
  "Return the symbol whose value is CHANNEL's levels."
  (intern (format "%s-fame-levels" (fame-check-channel channel))))

(defun fame-channel-levels (channel)
  "Return the message levels display values of CHANNEL.
If CHANNEL doesn't exist return the default value in constant
`fame-default-level-values'."
  (let ((symbol (fame-channel-symbol channel)))
    (if (boundp symbol)
        (symbol-value symbol)
      fame-default-level-values)))

(defsubst fame-level-display (channel level)
  "For CHANNEL, return the display value of LEVEL.
See also the option `fame-channels'."
  (plist-get (fame-channel-levels channel)
             (fame-check-level level)))

;;; Sending messages to channels
;;
(defconst fame-send-functions-alist
  '((none       . nil)
    (log        . fame-log-message)
    (temp       . fame-temp-message)
    (temp-nolog . fame-temp-message-nolog)
    (nolog      . fame-message-nolog)
    (t          . message)
    ))

(defun fame-send (channel level &rest args)
  "Send a message to CHANNEL at level LEVEL.
ARGS are like those of the function `message'.
The message will be displayed according to what is specified for
CHANNEL in the `fame-channels' option."
  (let ((sender (cdr (assq (fame-level-display channel level)
                           fame-send-functions-alist))))
    (and sender (apply sender args))))

(defsubst fame-send-debug (channel &rest args)
  "Send a debug message to CHANNEL.
CHANNEL must be a non-nil symbol.
ARGS will be passed to the function `fame-send'."
  (apply 'fame-send channel :debug args))

(defsubst fame-send-info (channel &rest args)
  "Send an informational message to CHANNEL.
CHANNEL must be a non-nil symbol.
ARGS will be passed to the function `fame-send'."
  (apply 'fame-send channel :info args))

(defsubst fame-send-warning (channel &rest args)
  "Send a warning message to CHANNEL.
CHANNEL must be a non-nil symbol.
ARGS will be passed to the function `fame-send'."
  (apply 'fame-send channel :warning args))

(defsubst fame-send-error (channel &rest args)
  "Send an error message to CHANNEL.
CHANNEL must be a non-nil symbol.
ARGS will be passed to the function `fame-send'."
  (apply 'fame-send channel :error args))

;;; Defining new channels
;;
;;;###autoload
(defmacro define-fame-channel (channel &optional default docstring)
  "Define the new message channel CHANNEL.
CHANNEL must be a non-nil symbol.
The optional argument DEFAULT specifies the default value of message
levels for this channel.  By default it is the value of
`fame-default-level-values'.
DOCSTRING is an optional channel documentation.

This defines the option `CHANNEL-fame-levels' to customize the current
value of message levels.  And the functions `CHANNEL-send-debug',
`CHANNEL-send-info', `CHANNEL-send-warning', and `CHANNEL-send-error',
that respectively send debug, informational, warning, and error
messages to CHANNEL."
  (let ((c-opt (fame-channel-symbol channel)))
    `(eval-when-compile
       (defcustom ,c-opt ',(fame-check-channel-levels default)
         ,(format "*Display value of message levels in the %s channel.
%s
This is a plist where a message level is a property whose value
defines how messages at this level will be displayed.

The possible levels are :debug, :info, :warning, and :error.
Level values can be:
 - t           to show and log messages the standard way.
 - nolog       to show messages without logging them.
 - temp        to show messages temporarily and log them.
 - temp-nolog  to show messages temporarily without logging them.
 - log         to log but not show messages.
 - none        to discard messages.

The default behavior is specified in `fame-default-level-values'."
                  channel
                  (if docstring (format "%s\n" docstring) ""))
         :group 'fame
         :type 'fame-channel-widget)
       (defsubst ,(intern (format "%s-send-debug" channel))
         (&rest args)
         ,(format "Send a debug message to the `%s' channel.
ARGS will be passed to the function `fame-send'.
To customize how such messages will be displayed, see the option
`%s'." channel c-opt)
         (apply 'fame-send ',channel :debug args))
       (defsubst ,(intern (format "%s-send-info" channel))
         (&rest args)
         ,(format "Send an informational message to the `%s' channel.
ARGS will be passed to the function `fame-send'.
To customize how such messages will be displayed, see the option
`%s'." channel c-opt)
         (apply 'fame-send ',channel :info args))
       (defsubst ,(intern (format "%s-send-warn" channel))
         (&rest args)
         ,(format "Send a warning message to the `%s' channel.
ARGS will be passed to the function `fame-send'.
To customize how such messages will be displayed, see the option
`%s'." channel c-opt)
         (apply 'fame-send ',channel :warning args))
       (defsubst ,(intern (format "%s-send-error" channel))
         (&rest args)
         ,(format "Send an error message to the `%s' channel.
ARGS will be passed to the function `fame-send'.
To customize how such messages will be displayed, see the option
`%s'." channel c-opt)
         (apply 'fame-send ',channel :error args))
       ;; Return the CHANNEL symbol
       ',c-opt)))

(provide 'fame)

;;; fame.el ends here

;;; ecb-mode-line.el --- mode-line for ECB

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2001

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-mode-line.el,v 1.40 2009/05/15 16:40:05 berndl Exp $

;;; Commentary:
;;
;; Contains all mode-line enhancements for ECB.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)
(require 'ecb-face)

;; XEmacs
(silentcomp-defun redraw-modeline)
(silentcomp-defun make-extent)
(silentcomp-defun set-extent-face)
(silentcomp-defun set-extent-property)
;; Emacs
(silentcomp-defun force-mode-line-update)
(silentcomp-defun propertize)

(defgroup ecb-mode-line nil
  "Settings for the modelines of the ECB-tree-buffers."
  :group 'ecb-general
  :prefix "ecb-")

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: update the texi
(defcustom ecb-mode-line-prefixes '((ecb-directories-buffer-name . nil)
                                    (ecb-sources-buffer-name . ecb-sources-filter-modeline-prefix)
                                    (ecb-methods-buffer-name . ecb-methods-filter-modeline-prefix)
                                    (ecb-history-buffer-name . ecb-history-filter-modeline-prefix))
  "*Prefixes shown in the modelines of the special ECB-buffers.
The displayed prefix then looks like: \"[ <PREFIX>[: ]]\", means if a prefix
is defined for an special ECB-buffer then a single space is prepended and if
there is additional text to display \(e.g. the current directory in the
sources buffer, see `ecb-mode-line-data') then also the string \": \" is
appended.

Everey element of this list is a cons-cell where the car is used to define a
buffer-name and the cdr to define the modeline-prefix for that buffer.

The buffer-name can either be defined as plain string or with a symbol which
contains the buffer-name as value. The latter one is recommended to define a
prefix for one of the builtin ECB-tree-buffers because then simply the related
option-symbol can be used. To add a prefix for the builtin directories
tree-buffer just set the symbol `ecb-directories-buffer-name' as car.

The cdr is the prefix for a buffer and can either be a string
which used as it is or a function-symbol which is called with
three argument \(the buffer-name, the current selected directory
and the current selected source whereas the latter one is a cons
as returned by `ecb-path-selected-source') and must return either
nil \(for no prefix) or a string which is then used a prefix.
Such a function can add the text-property 'help-echo to the
result-string. Then this help-string will be displayed when the
user moves the mouse over this section of the modeline.

If a special ECB-buffer should not have a prefix in its modeline then this
buffer-name should either not being added to this option or added with \"No
prefix\" \(= nil as cdr)."
  :group 'ecb-mode-line
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (ecb-mode-line-format))))
  :initialize 'custom-initialize-default
  :type '(repeat (cons :tag "Prefix-definition"
                       (choice :tag "Buffer-name" :menu-tag "Buffer-name"
                               (string :tag "Buffer-name as string")
                               (symbol :tag "Symbol containing buffer-name"))
                       (choice :tag "Prefix" :menu-tag "Prefix"
                               (const :tag "No prefix" :value nil)
                               (string :tag "Prefix-string")
                               (function :tag "Compute prefix with")))))
  


(defcustom ecb-mode-line-display-window-number t
  "*Display in the modeline of every special ECB-window the window-number.
The left-top-most window in a frame has the window-number 0 and all other
windows are numbered with increasing numbers in the sequence, functions like
`other-window' or `next-window' would walk through the frame.

This can be used to jump to windows by number with commands like:

  \(defun my-switch-to-window-number \(number)
    \"Switch to the nth window\"
    \(interactive \"P\")
    \(if \(integerp number)
        \(select-window \(nth number \(window-list)))))

Currently this feature is only available for GNU Emacs 21.X, because neither
GNU Emacs < 21 nor XEmacs can evaluate dynamically forms in the mode-line."
  :group 'ecb-mode-line
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (ecb-mode-line-format))))
  :initialize 'custom-initialize-default
  :type 'boolean)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: update the texi
(defcustom ecb-mode-line-data '((ecb-directories-buffer-name . sel-dir)
                                (ecb-sources-buffer-name . sel-dir)
                                (ecb-methods-buffer-name . sel-source)
                                (ecb-analyse-buffer-name . sel-source)
                                (ecb-history-buffer-name . "History"))
  "*Data shown in the modelines of the special ECB-buffers.
Everey element of this list is a cons-cell where the car is used to define a
buffer-name and the cdr to define the modeline-data for that buffer. For
details about how to defining a buffer-name see `ecb-mode-line-prefixes' - its
completely the same.

The cdr is the data for ths modeline and can either be the symbol 'sel-dir or
'sel-source whereas the former one displays the current selected directory as
modeline-data and the latter one the current selected source-file \(without
path).

In addition to these two predefined values for every special
ECB-buffer either a simple string \(which will be displayed) or a
function can be specified which gets three args \(name of the
buffer, current selected directory and current selected source
whereas the latter one is a cons as returned by
`ecb-path-selected-source') and must return a string which will
be displayed in the modeline \(or nil if no data should be
displayed). Such a function can add the text-property 'help-echo
to the result-string. Then this help-string will be displayed
when the user moves the mouse over this section of the modeline.

If a special ECB-buffer should not display special data in its modeline then
this buffer-name should either not being added to this option or added with
\"No data\" \(= nil as cdr).

The whole modeline of the special ECB-buffer consists of the prefix of
`ecb-mode-line-prefixes' and the data of `ecb-mode-line-data' - eventually
prepended by the window-number, see `ecb-mode-line-display-window-number'."
  :group 'ecb-mode-line
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (ecb-mode-line-format))))
  :initialize 'custom-initialize-default
  :type '(repeat (cons :tag "Data-definition"
                       (choice :tag "Buffer-name" :menu-tag "Buffer-name"
                               (string :tag "Buffer-name as string")
                               (symbol :tag "Symbol containing buffer-name"))
                       (choice :tag "Modeline-data" :menu-tag "Modeline-data"
                               (const :tag "No data" :value nil)
                               (const :tag "Current selected directory"
                                      :value sel-dir)
                               (const :tag "Current selected source"
                                      :value sel-source)
                               (string :tag "Data-string")
                               (function :tag "Compute data with")))))


(defun ecb-mode-line-format ()
  "Update all of the modelines of each ecb buffer."
  (save-excursion
    ;; update the modeline for each visible(!!) ECB-buffer
    (mapc (function
           (lambda (buffer)
             (let* ((prefix-elem (ecb-some (function
                                            (lambda (p)
                                              (cond ((stringp (car p))
                                                     (if (ecb-string= (car p)
                                                                      (buffer-name buffer))
                                                         (cdr p)
                                                       nil))
                                                    ((and (symbolp (car p))
                                                          (boundp (car p))
                                                          (stringp (symbol-value (car p))))
                                                     (if (ecb-string= (symbol-value (car p))
                                                                      (buffer-name buffer))
                                                         (cdr p)
                                                       nil))
                                                    (t (ecb-error "ecb-mode-line-format: Can not get prefix-elem: %s" p)))))
                                           ecb-mode-line-prefixes))
                    (prefix-str (typecase prefix-elem
                                  (null nil)
                                  (string prefix-elem)
                                  (function (funcall prefix-elem
                                                     (buffer-name buffer)
                                                     ecb-path-selected-directory
                                                     (ecb-path-selected-source)))))
                    (data-elem (ecb-some (function
                                          (lambda (p)
                                            (cond ((stringp (car p))
                                                   (if (ecb-string= (car p)
                                                                    (buffer-name buffer))
                                                       (cdr p)
                                                     nil))
                                                  ((and (symbolp (car p))
                                                        (boundp (car p))
                                                        (stringp (symbol-value (car p))))
                                                   (if (ecb-string= (symbol-value (car p))
                                                                    (buffer-name buffer))
                                                       (cdr p)
                                                     nil))
                                                  (t (ecb-error "ecb-mode-line-format: Can not get data-elem: %s" p)))))
                                         ecb-mode-line-data))
                    (data-str (cond ((equal data-elem 'sel-dir)
                                     ecb-path-selected-directory)
                                    ((equal data-elem 'sel-source)
                                     (and ecb-path-selected-source
                                          (ecb-path-selected-source 'buffername)))
                                    ((stringp data-elem)
                                     data-elem)
                                    ((null data-elem)
                                     nil)
                                    ((functionp data-elem)
                                     (funcall data-elem
                                              (buffer-name buffer)
                                              ecb-path-selected-directory
                                              (ecb-path-selected-source))))))
               ;; Display a default help-echo but only if the modeline-data is
               ;; not computed by a user-function.
               (when (and (not (functionp data-elem))
                          (stringp data-str))
                 (put-text-property 0 (length data-str) 'help-echo
                                    "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"
                                    data-str))
               ;; Now set the modeline
               (ecb-mode-line-set (buffer-name buffer)
                                  ecb-frame
                                  prefix-str
                                  data-str))))
          (ecb-get-current-visible-ecb-buffers))))


(defun ecb-mode-line-make-modeline-str (str face)
  "Applies FACE to the STR. In additon it applies a help-echo to STR if STR
contains a text-property 'help-echo."
  (let ((strcp (copy-sequence str)))
    (if ecb-running-xemacs
        (let ((ext (make-extent nil nil))
              (help-echo-str
               (catch 'found
                 (dotimes (i (length strcp))
                   (if (get-text-property i 'help-echo strcp)
                       (throw 'found
                              (get-text-property i 'help-echo strcp))))
                 nil)))
          (set-extent-face ext face)
          (set-extent-property ext 'help-echo help-echo-str)
          (list (cons ext strcp)))
      (list (propertize strcp 'face face)))))
                 
(defun ecb-mode-line-set (buffer-name frame prefix &optional text no-win-nr)
  "Sets the mode line for a buffer. The mode line has the scheme:
\"[WIN-NR ][PREFIX[: ]][TEXT]\". WIN-NR is the number of the window which
displays BUFFER-NAME and is only displayed if the option
`ecb-mode-line-display-window-number' is not nil and if NO-WIN-NR is nil. See
this option for a description of the window-number. WIN-NR will be displayed
as \"W-<number>\"."
  (when (get-buffer-window buffer-name frame)
    (let ((shown-prefix (if (stringp prefix)
                            (concat " " prefix (if (stringp text) ": " ""))
                          (if (stringp text) " " "")))
          (win-width (window-width (get-buffer-window buffer-name)))
          (available-text-width nil))
      (setq shown-prefix (ecb-fit-str-to-width shown-prefix (1- win-width) 'right))
      (setq available-text-width (- win-width
                                   (+ (length shown-prefix)
                                      (if (and (not ecb-running-xemacs)
                                               ecb-mode-line-display-window-number
                                               (not no-win-nr))
                                          4 0))))
      (ecb-mode-line-update-buffer
       buffer-name
       (list (if (and (not ecb-running-xemacs)
                      ecb-mode-line-display-window-number
                      (not no-win-nr))
                 ;; With :eval we must not use a list
                 '(:eval (car (ecb-mode-line-make-modeline-str
                               (format " W-%d"
                                       (1- (ecb-window-in-window-list-number (ecb-canonical-windows-list))))
                               ecb-mode-line-win-nr-face)))
               "")
             (ecb-mode-line-make-modeline-str shown-prefix
                                              ecb-mode-line-prefix-face)
             (ecb-mode-line-make-modeline-str
              (concat (if (stringp text)
                          (ecb-fit-str-to-width text
                                                available-text-width
                                                'left)))
              ecb-mode-line-data-face))))))


(defun ecb-mode-line-update-buffer (buffer-name new-mode-line-format)
  "Update the given buffer...."
  (if (get-buffer buffer-name)
      (save-excursion
        (set-buffer buffer-name)
        (setq mode-line-format new-mode-line-format)
        (if ecb-running-xemacs
            (redraw-modeline)
          (force-mode-line-update)))
    (message "This buffer isn't available: %s"  buffer-name)))

(silentcomp-provide 'ecb-mode-line)

;;; ecb-mode-line.el end here


;;; mmm-mode.el --- Allow Multiple Major Modes in a buffer

;; Copyright (C) 1999, 2004 by Michael Abraham Shulman
;; Copyright (C) 2013-2015 by Dmitry Gutov

;; Emacs Lisp Archive Entry
;; Package: mmm-mode
;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Maintainer: Dmitry Gutov <dgutov@yandex.ru>
;; URL: https://github.com/purcell/mmm-mode
;; Keywords: convenience, faces, languages, tools
;; Version: 0.5.4

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;;; MMM Mode is a minor mode that allows multiple major modes to
;;; coexist in a single buffer. Refer to the documentation of the
;;; function `mmm-mode' for more detailed information. This file
;;; contains mode on/off functions and the mode keymap, but mostly
;;; just loads all the subsidiary files.

;;{{{ Parameter Naming

;;; Since version 0.3.7, I've tried to use a uniform scheme for naming
;;; parameters. Here's a brief summary.

;;; BEG and END refer to the beginning and end of a region.
;;; FRONT and BACK refer to the respective delimiters of a region.
;;; FRONT- and BACK-OFFSET are the offsets from delimiter matches.
;;; FRONT-BEG through BACK-END are the endings of the delimiters.
;;; START and STOP bound actions, like searching, fontification, etc.

;;}}}
;;{{{ CL and Parameters

;;; Keyword parameters can be nice because it makes it easier to see
;;; what's getting passed as what. But I try not to use them in user
;;; functions, because CL doesn't make good documentation strings.
;;; Similarly, any hook or callback function can't take keywords,
;;; since Emacs as a whole doesn't use them. And for small parameter
;;; lists, they are overkill. So I use them only for a large number of
;;; optional parameters, such as `mmm-make-region'.

;;; An exception is the various submode class application functions,
;;; which all take all their arguments as keywords, for consistency
;;; and so the classes alist looks nice.

;;; When using keyword arguments, defaults should *always* be supplied
;;; in all arglists. (This pertains mostly to :start and :stop
;;; arguments, usually defaulting to (point-min) and (point-max)
;;; respectively.) `mmm-save-keywords' should only be used for lists
;;; with more than four arguments, such as in `mmm-ify-by-regexp'.

;;; In general, while I have no qualms about using things from CL like
;;; `mapl', `loop' and `destructuring-bind', I try not to use `defun*'
;;; more than I have to. For one, it sometimes makes bad documentation
;;; strings. Furthermore, to a `defun'ned function, a nil argument is
;;; the same as no argument, so it will use its (manual) default, but
;;; to a `defun*'ned function, a nil argument *is* the argument, so
;;; any default specified in the arglist will be ignored. Confusion of
;;; this type should be avoided when at all possible.

;;}}}

;;; Code:

(require 'cl)
;; If we don't load font-lock now, but it is loaded later, the
;; necessary mmm-font-lock-* properties may not be there.
(require 'font-lock)
(require 'mmm-compat)
(require 'mmm-utils)
(require 'mmm-vars)
(require 'mmm-auto)
(require 'mmm-region)
(require 'mmm-class)
;; This file is set up to autoload by `mmm-auto.el'.
;; (require 'mmm-cmds)
(require 'mmm-univ)

;;{{{ Toggle Function

(defun mmm-mode (&optional arg)
  "Minor mode to allow multiple major modes in one buffer.
Without ARG, toggle MMM Mode. With ARG, turn MMM Mode on iff ARG is
positive and off otherwise.

Commands Available:
\\<mmm-mode-map>
\\{mmm-mode-map}

BASIC CONCEPTS

The idea of MMM Mode is to allow multiple major modes to coexist in
the same buffer.  There is one \"primary\" major mode that controls
most of the buffer, and a number of \"submodes\" that each hold sway
over certain regions.  The submode regions are usually highlighted by
a background color for ease of recognition.  While the point is in a
submode region, the following changes \(are supposed to) occur:

1. The local keymap and the syntax table are that of the submode.
2. The mode line changes to show what submode region is active.
3. The major mode menu and mouse popup menu are that of the submode.
4. Some local variables of the submode shadow the default mode's.
5. Font-lock fontifies correctly for the submode.
6. Indentation function dispatches to the appropriate submode.
7. User-specified hooks run when the point enters or exits a submode.

The user may specify a number of hooks which may run when the point
transitions between submodes, or between the primary mode and a
submode. When a major mode is entered, the hook mmm-x-enter-hook is
run, where x is the name of the major mode. When a major mode is
left, the hook mmm-y-exit-hook is run, where y is the name of the
major mode. Users may also specify hooks with names of the form
mmm-x-y-hook which are run when the point leaves a mode named x,
and enters a mode named y.

For further information, including installation and configuration
instructions, see the Info file mmm.info which is included with the
distribution of MMM Mode.  Many of MMM's configuration variables are
available through M-x customize-group RET mmm."
  (interactive "P")
  (if (if arg (> (prefix-numeric-value arg) 0) (not mmm-mode))
      (mmm-mode-on)
    (mmm-mode-off)))

(add-to-list 'minor-mode-alist (list 'mmm-mode mmm-mode-string))

;;}}}
;;{{{ Mode On

(defun mmm-mode-on ()
  "Turn on MMM Mode. See `mmm-mode'."
  (interactive)
  ;; This function is called from mode hooks, so we need to make sure
  ;; we're not in a temporary buffer.  We don't need to worry about
  ;; recursively ending up in ourself, however, since by that time the
  ;; variable `mmm-mode' will already be set.
  (mmm-valid-buffer
   (unless mmm-mode
     (setq mmm-primary-mode major-mode)
     (when (fboundp 'c-make-styles-buffer-local)
       (c-make-styles-buffer-local t))
     (mmm-update-mode-info major-mode)
     (setq mmm-region-saved-locals-for-dominant
           ;; FIXME: Neither is defined in recent Emacs.
           (list* (list 'font-lock-cache-state nil)
                  (list 'font-lock-cache-position (make-marker))
                  (copy-tree (cdr (assq major-mode mmm-region-saved-locals-defaults)))))
     ;; Without the next line, the (make-marker) above gets replaced
     ;; with the starting value of nil, and all comes to naught.
     (mmm-set-local-variables major-mode nil)
     (mmm-add-hooks)
     (mmm-fixup-skeleton)
     (make-local-variable 'font-lock-fontify-region-function)
     (setq font-lock-fontify-region-function 'mmm-fontify-region)
     (set (make-local-variable 'syntax-begin-function) nil)
     (set (make-local-variable 'syntax-propertize-function)
          'mmm-syntax-propertize-function)
     (set (make-local-variable 'indent-line-function) mmm-indent-line-function)
     (setq mmm-mode t)
     (condition-case err
         (mmm-apply-all)
       (mmm-error
        ;; Complain, but don't die, since we want files to go ahead
        ;; and be opened anyway, and the mode to go ahead and be
        ;; turned on. Should we delete all previously made submode
        ;; regions when we find an invalid one?
        (message "%s" (error-message-string err))))
     (run-hooks 'mmm-mode-hook)
     (mmm-run-major-hook))))

;;}}}
;;{{{ Mode Off

(defun mmm-mode-off ()
  "Turn off MMM Mode. See `mmm-mode'."
  (interactive)
  (when mmm-mode
    (mmm-remove-hooks)
    (mmm-clear-overlays)
    (mmm-clear-history)
    (mmm-clear-mode-ext-classes)
    (mmm-clear-local-variables)
    (mmm-update-submode-region)
    (setq font-lock-fontify-region-function
          (get mmm-primary-mode 'mmm-fontify-region-function))
    (set 'syntax-begin-function
         (get mmm-primary-mode 'mmm-beginning-of-syntax-function))
    (mmm-update-font-lock-buffer)
    (mmm-refontify-maybe)
    (setq mmm-mode nil)
    ;; Restore the mode line
    (setq mmm-primary-mode-display-name nil
	  mmm-buffer-mode-display-name nil)
    (mmm-set-mode-line)))

;;}}}
;;{{{ Mode Keymap

(defvar mmm-mode-map (make-sparse-keymap)
  "Keymap for MMM Minor Mode.")

(defvar mmm-mode-prefix-map (make-sparse-keymap)
  "Keymap for MMM Minor Mode after `mmm-mode-prefix-key'.")

(defvar mmm-mode-menu-map (make-sparse-keymap "MMM")
  "Keymap for MMM Minor Mode menu.")

(defun mmm-define-key (key binding &optional keymap)
  (define-key (or keymap mmm-mode-prefix-map)
    (vector (append mmm-command-modifiers (list key)))
    binding))

(when mmm-use-old-command-keys
  (mmm-use-old-command-keys))

(mmm-define-key ?c 'mmm-ify-by-class)
(mmm-define-key ?x 'mmm-ify-by-regexp)
(mmm-define-key ?r 'mmm-ify-region)

(mmm-define-key ?b 'mmm-parse-buffer)
(mmm-define-key ?g 'mmm-parse-region)
(mmm-define-key ?% 'mmm-parse-block)
(mmm-define-key ?5 'mmm-parse-block)

(mmm-define-key ?k 'mmm-clear-current-region)
(mmm-define-key ?\  'mmm-reparse-current-region)
(mmm-define-key ?e 'mmm-end-current-region)

(mmm-define-key ?z 'mmm-narrow-to-submode-region)

;; This one is exact, since C-h is (usually) already used for help.
(define-key mmm-mode-prefix-map [?h] 'mmm-insertion-help)

;; Default bindings to do insertion (dynamic)
(mmm-set-keymap-default mmm-mode-prefix-map 'mmm-insert-region)

;; Set up the prefix help command, since otherwise the default binding
;; overrides it.
(define-key mmm-mode-prefix-map (vector help-char) prefix-help-command)

;; And put it all onto the prefix key
(define-key mmm-mode-map mmm-mode-prefix-key mmm-mode-prefix-map)

;; Order matters for the menu bar.
(define-key mmm-mode-menu-map [off]
  '("MMM Mode Off" . mmm-mode-off))
(define-key mmm-mode-menu-map [sep0] '(menu-item "----"))

(define-key mmm-mode-menu-map [clhist]
  '("Clear History" . mmm-clear-history))
(define-key mmm-mode-menu-map [end]
  '("End Current" . mmm-end-current-region))
(define-key mmm-mode-menu-map [clear]
  '("Clear Current" . mmm-clear-current-region))
(define-key mmm-mode-menu-map [reparse]
  '("Reparse Current" . mmm-reparse-current-region))

(define-key mmm-mode-menu-map [sep10] '(menu-item "----"))

(define-key mmm-mode-menu-map [ins-help]
  '("List Insertion Keys" . mmm-insertion-help))

(define-key mmm-mode-menu-map [sep20] '(menu-item "----"))

(define-key mmm-mode-menu-map [region]
  '(menu-item "MMM-ify Region" mmm-ify-region :enable mark-active))
(define-key mmm-mode-menu-map [regexp]
  '("MMM-ify by Regexp" . mmm-ify-by-regexp))
(define-key mmm-mode-menu-map [class]
  '("Apply Submode Class" . mmm-ify-by-class))

(define-key mmm-mode-menu-map [sep30] '(menu-item "----"))

(define-key mmm-mode-menu-map [parse-region]
  '(menu-item "Parse Region" mmm-parse-region :enable mark-active))
(define-key mmm-mode-menu-map [parse-buffer]
  '("Parse Buffer" . mmm-parse-buffer))
(define-key mmm-mode-menu-map [parse-block]
  '("Parse Block" . mmm-parse-block))

(define-key mmm-mode-map [menu-bar mmm] (cons "MMM" mmm-mode-menu-map))

(add-to-list 'minor-mode-map-alist (cons 'mmm-mode mmm-mode-map))

;;}}}

(provide 'mmm-mode)

;;; mmm-mode.el ends here

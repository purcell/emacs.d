;;; escreen.el --- emacs window session manager

;;; Copyright (C) 1992, 94, 95, 97, 2001, 2005 Noah S. Friedman

;;; Author: Noah Friedman <friedman@splode.com>
;;; Maintainer: friedman@splode.com
;;; Keywords: extensions
;;; Created: 1992-03-23

;;; $Id: escreen.el,v 1.18 2005/05/23 09:47:13 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To install, put this file in your load-path, byte-compile it, and add
;; the following to your .emacs:
;;
;;   (load "escreen")
;;   (escreen-install)

;; If you are using Emacs 19, you may have trouble loading this program
;; because of the customs syntax officially introduced in Emacs 20.  In
;; that case, first load cust-stub.el, available from
;;
;;     http://www.splode.com/~friedman/software/emacs-lisp/
;;
;; Updates to escreen.el will also be made available on that page.

;; Inspired by:
;;   * wicos.el, written by Heikki Suopanki <suopanki@phoenix.oulu.fi>
;;   * `screen', written by Oliver Laumann, Juergen Weigert,
;;     and Michael Schroeder.

;; Todo:
;;   * make escreen-menu display list of screens for all frames
;;   * ability to lock window configurations against change or deletion
;;   * ability to renumber screens
;;   * symbolic names for screens (a la wicos.el); partly started
;;   * switching active screen from pull-down menu from menubar
;;   * switching active screen from escreen menu
;;   * persistance of screens across instances of emacs
;;     [look at johnw's work on this; depends on additional non-standard
;;     packages but perhaps those parts can be reimplemented inline.]

;;; Code:

;; Variable declarations -- can be set by user

(defgroup escreen nil
  "Window configuration management"
  :group 'escreen
  :group 'extensions)

(defcustom escreen-max-screens 10
  "*Maximum number of screens that may be created."
  :type 'integer
  :group 'escreen)

(defcustom escreen-new-screen-default-buffer "*scratch*"
  "*Default buffer to display in newly-created screens."
  :type 'string
  :group 'escreen)

(defcustom escreen-restore-killed-buffers nil
  "*If non-nil, automatically revisit files if they have been killed.
That is, if a buffer was killed while in another screen session,
recreate them, visiting whatever file they were visiting."
  :type 'boolean
  :group 'escreen)

(defcustom escreen-preserve-buffer-list-order t
  "*If non-nil, preserve buffer list for each screen when switching.
When returning to a previously-saved screen, the buffer list order is
restored.  Buffers which have been created since the saved screen was last
visited, are put at the end of the list but the relative order is preserved.

This buffer list order is returned by the function `buffer-list' and
affects the behavior of `other-buffer', etc.

In Emacs 20 and later, each frame has its own ordered buffer list.
Switching screen sessions affects the selected frame's buffer list only."
  :type 'boolean
  :group 'escreen)

(defcustom escreen-number-mode t
  "*If non-nil, display current escreen number in mode line."
  :type 'boolean
  :group 'escreen)

(defcustom escreen-install-number-mode-format t
  "*If non-nil, install `escreen-mode-line-format' on `global-mode-string'.
This is performed by `escreen-install'."
  :type 'boolean
  :group 'escreen)

(defcustom escreen-goto-screen-before-hook nil
  "*Hook to run in `escreen-goto-screen' before anything else."
  :type 'hook
  :group 'escreen)

(defcustom escreen-goto-screen-hook nil
  "*Hook to run after `escreen-goto-screen' completes.
An example function that can make use of this hook is
`escreen-enable-number-mode-if-more-than-one-screen'."
  :type 'hook
  :group 'escreen)

(defcustom escreen-menu-mode-hook nil
  "*Hook to run by `escreen-menu' after everything else."
  :type 'hook
  :group 'escreen)


;; Keybindings

(defcustom escreen-prefix-char "\C-\\"
  "*Character prefixing escreen commands.
If you wish to change this, you must also do

   (global-set-key escreen-prefix-char 'escreen-prefix)

to update the prefix in the global keymap."
  :type 'string
  :group 'escreen)

(defvar escreen-map nil
  "*Keymap for escreen commands.")
(cond
 ((null escreen-map)
  (setq escreen-map (make-sparse-keymap))
  (define-key escreen-map escreen-prefix-char 'escreen-goto-last-screen)
  (define-key escreen-map "0"    'escreen-goto-screen-0)
  (define-key escreen-map "1"    'escreen-goto-screen-1)
  (define-key escreen-map "2"    'escreen-goto-screen-2)
  (define-key escreen-map "3"    'escreen-goto-screen-3)
  (define-key escreen-map "4"    'escreen-goto-screen-4)
  (define-key escreen-map "5"    'escreen-goto-screen-5)
  (define-key escreen-map "6"    'escreen-goto-screen-6)
  (define-key escreen-map "7"    'escreen-goto-screen-7)
  (define-key escreen-map "8"    'escreen-goto-screen-8)
  (define-key escreen-map "9"    'escreen-goto-screen-9)
  (define-key escreen-map "?"    'escreen-help)
  (define-key escreen-map "\C-b" 'escreen-menu)
  (define-key escreen-map "a"    'escreen-get-active-screen-numbers)
  (define-key escreen-map "b"    'escreen-get-current-screen-number)
  (define-key escreen-map "c"    'escreen-create-screen)
  (define-key escreen-map "g"    'escreen-goto-screen)
  (define-key escreen-map "k"    'escreen-kill-screen)
  (define-key escreen-map "n"    'escreen-goto-next-screen)
  (define-key escreen-map "p"    'escreen-goto-prev-screen)))

(defalias 'escreen-prefix escreen-map)


;;; Internal variables.  Do not set these yourself.

;; This should not be modified by the user.  The information it provides is
;; critical and the calling conventions are different than for
;; escreen-map-data-format.  The order here is important too.
;; Do not change this data structure without also changing the
;; escreen-configuration-data-map-critical-* accessors.
(defvar escreen-map-critical-data-format
  (list 'current-buffer
        (lambda () (buffer-name))
        'buffer-file-name))

;; If you want to add or change this list, it's best to set
;; escreen-configuration-alist to nil and run escreen-install afterward.
;; Otherwise, the new table will be used with old data and may cause errors.
;;
;; Note that resetting escreen in this way loses all but the current
;; window configuration.
(defvar escreen-map-data-format
  '((escreen-map-save-window-start    . escreen-map-restore-window-start)
    (mark-marker                      . escreen-map-restore-mark-marker)
    (escreen-map-save-point           . escreen-map-restore-point)
    (escreen-map-save-narrowed-region . escreen-map-restore-narrowed-region)
    (escreen-map-save-truncate-lines  . escreen-map-restore-truncate-lines)
    (escreen-map-save-mode-line-face  . escreen-map-restore-mode-line-face)
    (escreen-map-save-menu-bar-mode   . escreen-map-restore-menu-bar-mode)
    (buffer-list                      . escreen-map-restore-buffer-list)))

;; Keeps track of escreen state (window config, buffers, etc.)
;; The structure of each elt is
;;
;;  (screen-number screen-name
;;                 #<window-configuration>
;;                 (((critical-data-buffer-1) user-data-buffer-1 ...)
;;                  ((critical-data-buffer-2) user-data-buffer-2 ...)
;;                  ...)
;;                 selected-window-number)
;;
(defvar escreen-configuration-alist nil)

;; Current screen number.  Smallest possible screen number is 0.
(defvar escreen-current-screen-number 0)

;; Current screen number as a string.
;; Smallest possible screen number is 0.
(defvar escreen-current-screen-string
  (int-to-string escreen-current-screen-number))

;; Last-visited screen number.  Smallest possible screen number is 0.
(defvar escreen-last-screen-number 0)

;; Highest screen number currently in use.
(defvar escreen-highest-screen-number-used 0)

;; t or nil depending on if there is more than one screen
;; This is only used by escreen-enable-number-mode-if-more-than-one-screen
;; and escreen-mode-line-format.
;; This defaults to t since initially there is only one screen on a frame.
(defvar escreen-one-screen-p t)

;; It's ok to change this, but it makes use of internal variables
(defvar escreen-mode-line-format
  '(escreen-number-mode
    (escreen-one-screen-p "" ("S" escreen-current-screen-string " "))))

(defvar escreen-frame-local-variables
  '(escreen-configuration-alist
    escreen-current-screen-number
    escreen-current-screen-string
    escreen-last-screen-number
    escreen-highest-screen-number-used
    escreen-one-screen-p))


(defmacro escreen-save-frame-excursion (&rest body)
  "Execute BODY, saving and restoring the selected frame."
  (let ((orig-frame (make-symbol "orig-frame")))
    `(let ((,orig-frame (selected-frame)))
       (unwind-protect
           (progn ,@body)
         (and (frame-live-p ,orig-frame)
              (select-frame ,orig-frame))))))

(put 'escreen-save-frame-excursion 'lisp-indent-function 0)

(defalias 'escreen-mapc (if (fboundp 'mapc) 'mapc 'mapcar))

(defsubst escreen-map-frames (fn)
  (escreen-save-frame-excursion
    (escreen-mapc fn (frame-list))))


;; Older versions of Emacs did not have window-pixel-edges
;; Older versions of XEmacs did not have window-edges
(defalias 'escreen-window-edges
  (if (fboundp 'window-edges) 'window-edges 'window-pixel-edges))


(defun escreen-install ()
  (interactive)
  (global-set-key escreen-prefix-char 'escreen-prefix)

  ;; Install screen number on global-mode-string
  (and escreen-install-number-mode-format
       (let ((elt '("" escreen-mode-line-format)))
         (or (member elt global-mode-string)
             (setq global-mode-string
                   (cons elt global-mode-string)))))

  (cond ((fboundp 'make-variable-frame-local)
         (escreen-mapc 'make-variable-frame-local
                       escreen-frame-local-variables)

         (add-hook 'after-make-frame-functions
                   'escreen-initialize-frame-variables)))

  (if escreen-number-mode
      (escreen-number-mode 1))

  ;; Initialize escreen-configuration-alist by placing current window
  ;; config in it.
  (if (fboundp 'make-variable-frame-local)
      (escreen-map-frames 'escreen-initialize-frame-variables)
    (escreen-save-current-screen-configuration)))

(defun escreen-number-mode (&optional prefix)
  "*Toggle escreen-number-mode (see variable docstring).
If called with a positive prefix argument, always enable.
If called with a negative prefix argument, always disable.
If called with no prefix argument, toggle current state."
  (interactive "P")
  (setq escreen-number-mode
        (cond ((null prefix)
               (not escreen-number-mode))
              (t
               (>= (prefix-numeric-value prefix) 0)))))


(defun escreen-create-screen ()
  "Create a new screen and switch to it.
New screen will display one window with the buffer specified by
`escreen-new-screen-default-buffer'."
  (interactive)
  (let ((new-screen-number (escreen-first-unused-screen-number)))
    (or new-screen-number
        (error "escreen: No more screens (see \"escreen-max-screens\")"))

    ;; Save window configuration before switching to a new one.
    (escreen-save-current-screen-configuration)
    (and (> new-screen-number escreen-highest-screen-number-used)
         (setq escreen-highest-screen-number-used new-screen-number))
    (setq escreen-last-screen-number escreen-current-screen-number)
    (setq escreen-current-screen-number new-screen-number)
    (setq escreen-current-screen-string (int-to-string new-screen-number))

    ;; Don't reuse any of the previous screen's window objects; settings
    ;; like window-dedicated-p, window display tables, etc. will just cause
    ;; grief.
    ;;
    ;; Modify the frame so there is only one window; this insures that we
    ;; have room to split to a second window.  Select new window, then
    ;; delete the previous one.  We now start the new screen with a totally
    ;; new window (the previous window is still saved in the window
    ;; configuration, so its settings are not lost).
    (delete-other-windows)
    (select-window (split-window))
    (delete-other-windows)

    ;; create a new window and switch to that, then delete the other window.
    ;; this is just
    (switch-to-buffer escreen-new-screen-default-buffer)
    ;; Save new window configuration so that it's in the alist.
    (escreen-save-current-screen-configuration))
  ;; We run this hook because, in a sense, we have gone to a new
  ;; screen. but we don't actually call escreen-goto-screen because of the
  ;; extra setup work here.
  (run-hooks 'escreen-goto-screen-hook))

(defun escreen-kill-screen (&optional number)
  "Kill current screen, or screen given by optional argument NUMBER.
No error occurs if the specified screen number doesn't exist.
You cannot kill the last existing screen.
Switch to previous screen if killing active one."
  (interactive)
  (let* ((screen-number (or number escreen-current-screen-number))
         (killing-current-screen-p (eq escreen-current-screen-number
                                       screen-number))
         (screen-data (escreen-configuration-escreen screen-number))
         previous-screen)
    (cond (screen-data
           (and killing-current-screen-p
                (escreen-configuration-one-screen-p)
                (error "escreen: only one screen, can't kill."))
           ;; Don't bother looking for previous screen number unless killing
           ;; current screen, because only then do we need to switch screens.
           (and killing-current-screen-p
                (setq previous-screen (escreen-get-prev-screen-number)))
           (escreen-configuration-escreen-delete screen-data)
           (and (eq screen-number escreen-highest-screen-number-used)
                ;; We're killing the screen with the highest number.
                ;; Look for the next highest number.
                (setq escreen-highest-screen-number-used
                      (car (sort (escreen-configuration-screen-numbers) '>))))
           (and killing-current-screen-p
                (escreen-goto-screen previous-screen 'dont-update-current))))))

;; This is only called in versions of emacs which support frame-local
;; variables; that's Emacs 20.3 and later.
(defun escreen-initialize-frame-variables (&optional frame)
  (escreen-save-frame-excursion
    (select-frame frame)
    (modify-frame-parameters frame
      (mapcar (lambda (s)
                (cons s (default-value s)))
              escreen-frame-local-variables))
    (setq escreen-configuration-alist nil)
    (escreen-save-current-screen-configuration)))


(defun escreen-goto-screen (number &optional dont-update-current)
  "Switch to screen number N.
Optional arg DONT-UPDATE-CURRENT means don't save the current screen
configuration, though this isn't intended to be used interactively."
  (interactive "NGo to escreen number: ")
  (run-hooks 'escreen-goto-screen-before-hook)
  (let ((screen-data (escreen-configuration-escreen number)))
    (or screen-data
        (error "escreen: %d: invalid screen number." number))
    (or dont-update-current
        (escreen-save-current-screen-configuration))
    (escreen-restore-screen-map screen-data)
    (setq escreen-current-screen-string (int-to-string number))
    (or dont-update-current
        (setq escreen-last-screen-number escreen-current-screen-number))
    (setq escreen-current-screen-number number))
  (run-hooks 'escreen-goto-screen-hook))

(defun escreen-goto-last-screen ()
  "Switch to the last visited screen."
  (interactive)
  (let ((n (if (= escreen-last-screen-number escreen-current-screen-number)
               (escreen-get-next-screen-number escreen-last-screen-number)
             escreen-last-screen-number)))
    (setq escreen-last-screen-number escreen-current-screen-number)
    (escreen-goto-screen n)))

(defun escreen-goto-prev-screen (&optional n)
  "Switch to the previous screen.
This is the nearest lower-numbered existing screen from the current one,
wrapping around list of screens if necessary.
If prefix arg N given, jump to the Nth previous screen."
  (interactive "p")
  (if (< n 0)
      (escreen-goto-prev-or-next-screen-internal (- n) 'next)
    (escreen-goto-prev-or-next-screen-internal n 'prev)))

(defun escreen-goto-next-screen (&optional n)
  "Switch to the next screen.
This is the nearest greater-numbered existing screen from the current one,
wrapping around list of screens if necessary.
If prefix arg N given, jump to the Nth next screen."
  (interactive "p")
  (if (< n 0)
      (escreen-goto-prev-or-next-screen-internal (- n) 'prev)
    (escreen-goto-prev-or-next-screen-internal n 'next)))

(defun escreen-goto-prev-or-next-screen-internal (n prev-or-next)
  (let ((total (length (escreen-get-active-screen-numbers)))
        (func (if (eq prev-or-next 'next)
                  'escreen-get-next-screen-number
                'escreen-get-prev-screen-number))
        (i 0)
        (screen-number escreen-current-screen-number))
    (and (> n total)
         ;; Trim off excess amount so we do fewer iterations, since
         ;; wrapping over the total number of screens even once is
         ;; wasteful and slow.
         (setq n (- n (* (/ n total) total))))
    (while (< i n)
      (setq screen-number (funcall func screen-number)
            i (1+ i)))
    (escreen-goto-screen screen-number)))

(defun escreen-goto-screen-0 () (interactive) (escreen-goto-screen 0))
(defun escreen-goto-screen-1 () (interactive) (escreen-goto-screen 1))
(defun escreen-goto-screen-2 () (interactive) (escreen-goto-screen 2))
(defun escreen-goto-screen-3 () (interactive) (escreen-goto-screen 3))
(defun escreen-goto-screen-4 () (interactive) (escreen-goto-screen 4))
(defun escreen-goto-screen-5 () (interactive) (escreen-goto-screen 5))
(defun escreen-goto-screen-6 () (interactive) (escreen-goto-screen 6))
(defun escreen-goto-screen-7 () (interactive) (escreen-goto-screen 7))
(defun escreen-goto-screen-8 () (interactive) (escreen-goto-screen 8))
(defun escreen-goto-screen-9 () (interactive) (escreen-goto-screen 9))


(defun escreen-get-current-screen-number ()
  "Returns the currently selected screen number.
If called interactively, also print this result in the minibuffer."
  (interactive)
  (if (interactive-p)
      (message "escreen: current screen is number %d"
               escreen-current-screen-number)
    escreen-current-screen-number))

(defun escreen-get-active-screen-numbers ()
  "Print a list of the active screen numbers in the echo area.
Returns a list of numbers which represent screen numbers presently in use."
  (interactive)
  (let ((screen-list (sort (escreen-configuration-screen-numbers) '<)))
    (if (interactive-p)
        (message "escreen: active screens: %s"
                 (mapconcat 'number-to-string screen-list " ")))
    screen-list))

(defun escreen-help ()
  "Display a short summary of escreen commands."
  (interactive)
  (if (string-lessp emacs-version "19")
      ;; emacs 18 couldn't list only bindings with a common prefix.
      (describe-bindings)
    ;; Emacs 19 can handle escreen-prefix-char (as a string) directly, but
    ;; for XEmacs, it must be converted to a vector.
    (describe-bindings (escreen-string-to-vector escreen-prefix-char))))

(defun escreen-string-to-vector (s)
  (let* ((l (length s))
         (v (make-vector l nil))
         (i 0))
    (while (< i l)
      (aset v i (aref s i))
      (setq i (1+ i)))
    v))


;; Return the first unused number available for designation as a screen
;; number, or nil if  escreen-max-screens  screens are already in use.
(defun escreen-first-unused-screen-number ()
  (let ((number 0))
    (while (and (< number escreen-max-screens)
                (escreen-configuration-escreen number))
      (setq number (1+ number)))
    (and (< number escreen-max-screens) number)))

;; Save window configuration, buffer configuration, and current marks and
;; point for each displayed buffer for the current screen.
(defun escreen-save-current-screen-configuration ()
  (let ((screen-data (escreen-screen-defined))
        (new-alist-member nil))
    (if screen-data
        (setcdr (cdr screen-data) (escreen-save-screen-map))
      (setq new-alist-member (cons escreen-current-screen-number
                                   (cons nil (escreen-save-screen-map))))
      (setq escreen-configuration-alist
            (cons new-alist-member escreen-configuration-alist)))))

;; Return attributes for screen N, or nil if it doesn't exist.
(defun escreen-screen-defined (&optional n)
  (escreen-configuration-escreen (or n escreen-current-screen-number)))

;; Return nearest number less than current screen number that is
;; an active screen, wrapping around end of screen list if necessary.
(defun escreen-get-prev-screen-number (&optional current-screen-number)
  (or current-screen-number
      (setq current-screen-number escreen-current-screen-number))
  (if (eq 0 escreen-highest-screen-number-used)
      0
    ;; Decrement/wrap current screen number
    (setq current-screen-number (1- current-screen-number))
    (and (< current-screen-number 0)
         (setq current-screen-number escreen-highest-screen-number-used))
    (while (not (assq current-screen-number escreen-configuration-alist))
      ;; Decrement/wrap current screen number
      (setq current-screen-number (1- current-screen-number))
      (and (< current-screen-number 0)
           (setq current-screen-number escreen-highest-screen-number-used)))
    current-screen-number))

;; Return nearest number greater than current screen number that is
;; an active screen, wrapping around end of screen list if necessary.
(defun escreen-get-next-screen-number (&optional current-screen-number)
  (or current-screen-number
      (setq current-screen-number escreen-current-screen-number))
  (if (eq 0 escreen-highest-screen-number-used)
      0
    ;; Increment/wrap current screen number
    (setq current-screen-number (1+ current-screen-number))
    (and (> current-screen-number escreen-highest-screen-number-used)
         (setq current-screen-number 0))
    (while (not (assq current-screen-number escreen-configuration-alist))
      ;; Increment/wrap current screen number
      (setq current-screen-number (1+ current-screen-number))
      (and (> current-screen-number escreen-highest-screen-number-used)
           (setq current-screen-number 0)))
    current-screen-number))


;;; Primitive accessors for escreen-configuration-alist
;;;
;;; These could be made into macros or defsubsts, but it would make
;;; debugging more difficult and they are not critical for speed.

(defun escreen-configuration-escreen (number)
  (assq number escreen-configuration-alist))

(defun escreen-configuration-escreen-delete (data)
  (setq escreen-configuration-alist
        (delq (if (numberp data)
                  (escreen-configuration-escreen data)
                data)
              escreen-configuration-alist)))

(defun escreen-configuration-screen-numbers ()
  (mapcar 'car escreen-configuration-alist))

(defun escreen-configuration-one-screen-p ()
  (>= 1 (length escreen-configuration-alist)))

;; Sort the alist so that they are in order numerically.
(defun escreen-configuration-alist-sort-by-number ()
  (setq escreen-configuration-alist
        (sort escreen-configuration-alist
              (lambda (a b)
                (< (car a) (car b))))))

;;; map-data sub-accessors

(defun escreen-configuration-screen-number (l)
  (nth 0 l))

(defun escreen-configuration-screen-name (l)
  (nth 1 l))

(defun escreen-configuration-window-data-configuration (l)
  (nth 2 l))

(defun escreen-configuration-data-map (l)
  (nth 3 l))

(defun escreen-configuration-selected-window-count (l)
  (nth 4 l))

;;; screen map data accessors

(defun escreen-configuration-data-map-critical (data)
  (car data))

(defun escreen-configuration-data-map-user (data)
  (cdr data))

;;; critical map data accessors

(defun escreen-configuration-data-map-critical-buffer (crit-map)
  (nth 0 crit-map))

(defun escreen-configuration-data-map-critical-buffer-name (crit-map)
  (nth 1 crit-map))

(defun escreen-configuration-data-map-critical-buffer-file-name (crit-map)
  (nth 2 crit-map))


(defun escreen-save-screen-map ()
  (let ((config (current-window-configuration))
        (win-data nil)
        (sel-win-count 0)
        (sel-window (selected-window))
        (first-window (escreen-first-window))
        (window nil))
    (save-excursion
      (save-window-excursion
        (select-window first-window)
        (while (not (eq window first-window))
          (cond ((null sel-window))
                ((eq (selected-window) sel-window)
                 (setq sel-window nil))
                (t
                 (setq sel-win-count (1+ sel-win-count))))
          (setq win-data
                (cons (cons (escreen-save-critical-data)
                            (escreen-save-user-data))
                      win-data))
          (setq window (select-window (next-window)))
          (set-buffer (window-buffer (selected-window))))))
    (list config (nreverse win-data) sel-win-count)))

(defun escreen-restore-screen-map (map)
  (let ((config (escreen-configuration-window-data-configuration map))
        (map (escreen-configuration-data-map map))
        (sel-win-number (escreen-configuration-selected-window-count map))
        (win-count 0)
        (sel-win nil))
    (set-window-configuration config)
    (select-window (escreen-first-window))
    (while map
      (and (= win-count sel-win-number)
           (setq sel-win (selected-window)))
      (setq win-count (1+ win-count))

      (escreen-restore-critical-data
        (escreen-configuration-data-map-critical (car map)))
      (widen)
      (escreen-restore-user-data
        (escreen-configuration-data-map-user (car map)))
      (select-window (next-window))
      (setq map (cdr map)))
    (select-window (or sel-win (escreen-first-window)))))

(defun escreen-save-critical-data ()
  (mapcar 'funcall escreen-map-critical-data-format))

(defun escreen-restore-critical-data (data)
  (let ((buffer (escreen-configuration-data-map-critical-buffer data))
        (buffer-name
         (escreen-configuration-data-map-critical-buffer-name data))
        (buf-file-name
         (escreen-configuration-data-map-critical-buffer-file-name data)))
    (cond ((escreen-killed-buffer-p buffer)
           (cond ((null escreen-restore-killed-buffers)
                  (set-window-buffer (selected-window)
                                     (get-buffer-create
                                      escreen-new-screen-default-buffer)))
                 ((stringp buf-file-name)
                  (setq buffer (find-file-noselect buf-file-name))
                  (set-window-buffer (selected-window) buffer)
                  (or (get-buffer buffer-name)
                      (rename-buffer buffer-name)))
                 (t
                  (set-window-buffer (selected-window)
                                     (get-buffer-create
                                      escreen-new-screen-default-buffer)))))
          (t
           (set-window-buffer (selected-window) buffer)))))

(defun escreen-save-user-data ()
  (mapcar (lambda (pair) (funcall (car pair)))
          escreen-map-data-format))

(defun escreen-restore-user-data (data)
  (let ((funlist escreen-map-data-format))
    (while (and data funlist)
      (funcall (cdr (car funlist)) (car data))
      (setq funlist (cdr funlist))
      (setq data (cdr data)))))


;; Functions used to save and restore screen configuration state.
;; These are mapped over via presence in escreen-map-data-format.

(defun escreen-map-save-window-start ()
  (escreen-make-marker (window-start)))

(defun escreen-map-restore-window-start (p)
  (and (escreen-position-valid-p p)
       (set-window-start (selected-window) p t)))

(defun escreen-map-restore-mark-marker (mark)
  (cond ((escreen-position-valid-p mark)
         (set-marker (or (mark-marker)
                         ;; when XEmacs zmacs-regions are set, mark-marker
                         ;; can return nil unless optional arg forcep is
                         ;; non-nil.
                         ;; In Emacs transient-mark-mode, mark-marker will
                         ;; still return a marker, so no magic needed.
                         (mark-marker t))
                     (marker-position mark)
                     (marker-buffer mark)))))

(defun escreen-map-save-point ()
  ;; If there is a process mark in the current buffer and point is at it,
  ;; then return the process mark also.  That way, when we return to this
  ;; screen, point will be at the end of the process output even if that
  ;; has advanced since then.  Otherwise, just use a before-insertion
  ;; marker (if supported).
  (let* ((point-mark (escreen-make-marker (point-marker) nil t))
         (proc (get-buffer-process (current-buffer)))
         (proc-mark (and proc (process-mark proc))))
    (if (and (escreen-position-valid-p proc-mark)
             (= proc-mark (point)))
        (cons proc-mark point-mark)
      point-mark)))

(defun escreen-map-restore-point (pos)
  (cond ((consp pos)
         (cond ((escreen-position-valid-p (car pos))
                (goto-char (car pos)))
               ((escreen-position-valid-p (cdr pos))
                (goto-char (cdr pos)))))
        (t
         (and (escreen-position-valid-p pos)
              (goto-char pos)))))

(defun escreen-map-save-narrowed-region ()
  (cons (and (> (point-min) 1)
             (escreen-make-marker (point-min)))
        (and (<= (point-max) (buffer-size))
             (escreen-make-marker (point-max) nil t))))

(defun escreen-map-restore-narrowed-region (reg)
  (let ((size (1+ (buffer-size)))
        (beg (or (car reg) (point-min)))
        (end (or (cdr reg) (point-max))))
    (and (escreen-position-valid-p beg)
         (escreen-position-valid-p end)
         (<= beg size)
         (<= end size)
         (narrow-to-region beg end))))

(defun escreen-map-save-truncate-lines ()
  truncate-lines)

(defun escreen-map-restore-truncate-lines (v)
  (setq truncate-lines v))

(defun escreen-map-save-mode-line-face ()
  (cond ((fboundp 'face-reverse-p)
         ;; XEmacs mode line face properties
         (list (face-reverse-p 'modeline)
               (face-background 'modeline)
               (face-foreground 'modeline)))
        ((boundp 'mode-line-inverse-video)
         mode-line-inverse-video)))

(defun escreen-map-restore-mode-line-face (v)
  (cond ((fboundp 'face-reverse-p)
         (set-face-reverse-p 'modeline (nth 0 v))
         (set-face-background 'modeline (nth 1 v))
         (set-face-foreground 'modeline (nth 2 v)))
        ((boundp 'mode-line-inverse-video)
         (setq mode-line-inverse-video v))))

;; Emacs 19.30 and beyond supports menu bars on ascii terminals, but beware
;; of turning them off or on once escreen is loaded; if a stored window
;; configuration was for a frame with a menu bar, but there is no menu bar
;; presently, that will crash emacs.  This fatal bug is present in all
;; versions of Emacs prior to 21.0.
(defun escreen-map-save-menu-bar-mode ()
  (and (boundp 'menu-bar-mode)
       menu-bar-mode))

(defun escreen-map-restore-menu-bar-mode (v)
  (cond ((fboundp 'menu-bar-mode)
         (if v
             (menu-bar-mode 1)
           (menu-bar-mode -1)))))

(defun escreen-map-restore-buffer-list (olist)
  (and escreen-preserve-buffer-list-order
       (escreen-set-buffer-list-order olist)))


(defun escreen-killed-buffer-p (buffer)
  (not (if (fboundp 'buffer-live-p)
           (buffer-live-p buffer)
         ;; Emacs 18 doesn't have buffer-live-p.
         ;; Killed buffers have no names.
         (buffer-name buffer))))

(defun escreen-position-valid-p (pos)
  (cond ((numberp pos)
         (<= pos (1+ (buffer-size))))
        ((markerp pos)
         (and (eq (marker-buffer pos) (current-buffer))
              (numberp (marker-position pos))
              (<= pos (1+ (buffer-size)))))
        (t nil)))

(defun escreen-set-buffer-list-order (olist)
  (let (firstbuf buf)
    (while olist
      (setq buf (car olist))
      (and (stringp buf)
           (setq buf (get-buffer buf)))
      (cond ((escreen-killed-buffer-p buf))
            (t
             (bury-buffer buf)
             (or firstbuf
                 (setq firstbuf buf))))
      (setq olist (cdr olist)))
    (setq olist (buffer-list))
    (while (not (eq (car olist) firstbuf))
      (bury-buffer (car olist))
      (setq olist (cdr olist)))))

;; Copy existing marker, or make a new one from point.
;; Emacs 19.30 and later can create markers which are advanced if text is
;; inserted before them, without needing to call insert-before-markers
;; explicitly.  This is useful for storing point, mark, etc. since the
;; buffer may be edited while we are in other escreens.
(defun escreen-make-marker (pos &optional buffer insertion-type)
  (let ((new-marker nil))
    (cond ((markerp pos)
           (setq new-marker (copy-marker pos))
           (and buffer
                (set-marker new-marker (marker-position pos) buffer)))
          (t
           (setq new-marker (make-marker))
           (set-marker new-marker pos buffer)))
    (and (fboundp 'set-marker-insertion-type)
         (set-marker-insertion-type new-marker insertion-type))
    new-marker))

(defun escreen-first-window ()
  (cond ((fboundp 'frame-highest-window)
         (funcall 'frame-highest-window))
        ((fboundp 'frame-first-window)
         (funcall 'frame-first-window))
        ((one-window-p)
         (selected-window))
        (t
         (let ((win (selected-window)))
           (while (not (escreen-first-window-p win))
             (setq win (next-window win)))
           win))))

(defun escreen-first-window-p (win)
  (let ((edges (escreen-window-edges win)))
    (and (= (nth 0 edges) 0)
         (= (nth 1 edges) 0))))


(defun escreen-menu ()
  (interactive)
  (escreen-configuration-alist-sort-by-number)
  (let ((escreen-menu-buffer (get-buffer-create "*Escreen List*"))
        alist data-map screen-number)
    ;; Display buffer now so update of screen cofiguration will be correct.
    (display-buffer escreen-menu-buffer)
    ;; Update escreen-configuration-alist to contain up-to-date information
    ;; on current screen, since we'll be displaying data about it.
    (escreen-save-current-screen-configuration)
    (setq alist escreen-configuration-alist)
    (save-excursion
      (set-buffer escreen-menu-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert " Screen Buffers\n ------ -------\n")
      (while alist
        (setq screen-data (car alist))
        (setq alist (cdr alist))

        (setq screen-number (escreen-configuration-screen-number screen-data))
        (setq data-map (escreen-configuration-data-map screen-data))

        (if (= screen-number escreen-current-screen-number)
            (insert (format "*%-6d " screen-number))
          (insert (format " %-6d " screen-number)))
        (while data-map
          (insert (if (> (current-column) 0) "" "        ")
                  (escreen-configuration-data-map-critical-buffer-name
                   (escreen-configuration-data-map-critical (car data-map)))
                  "\n")
          (setq data-map (cdr data-map)))
        (insert "\n"))
      (escreen-menu-mode))))

(defun escreen-menu-mode ()
  (fundamental-mode)
  (kill-all-local-variables)
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'escreen-menu-mode)
  (setq mode-name "Escreen Menu")
  (run-hooks 'escreen-menu-mode-hook))


;; Install this by doing
;;
;;    (add-hook 'escreen-goto-screen-hook
;;              'escreen-enable-number-mode-if-more-than-one-screen)
;;
;; By doing so, escreen-number-mode is disabled whenever only a single
;; escreen is in use.  The only reason for doing this, however, is to save
;; valuable mode line real estate.
(defun escreen-enable-number-mode-if-more-than-one-screen ()
  (setq escreen-one-screen-p
        (null (cdr (escreen-get-active-screen-numbers))))
  (force-mode-line-update t))

(provide 'escreen)

;;; escreen.el ends here

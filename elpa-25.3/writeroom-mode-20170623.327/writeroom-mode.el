;;; writeroom-mode.el --- Minor mode for distraction-free writing  -*- lexical-binding: t -*-

;; Copyright (c) 2012-2017 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 11 July 2012
;; Package-Requires: ((emacs "24.1") (visual-fill-column "1.9"))
;; Version: 3.7
;; Keywords: text

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; writeroom-mode is a minor mode for Emacs that implements a
;; distraction-free writing mode similar to the famous Writeroom editor for
;; OS X. writeroom-mode is meant for GNU Emacs 24 and isn't tested on older
;; versions.
;;
;; See the README or info manual for usage instructions.
;;
;;; Code:

(require 'visual-fill-column)

(defvar writeroom--frame nil
  "The frame in which `writeroom-mode' is activated.
The global effects only apply to this frame.")

(defvar writeroom--buffers nil
  "List of buffers in which `writeroom-mode' is activated.")

(defvar writeroom--local-variables '(mode-line-format
                            header-line-format
                            line-spacing)
  "Local variables whose values need to be saved when `writeroom-mode' is activated.")

(defvar writeroom--saved-data nil
  "Buffer-local data to be stored when `writeroom-mode' is activated.
These settings are restored when `writeroom-mode' is
deactivated.")
(make-variable-buffer-local 'writeroom--saved-data)

(defvar writeroom--saved-visual-fill-column nil
  "Status of `visual-fill-column-mode' before activating `writeroom-mode'.")
(make-variable-buffer-local 'writeroom--saved-visual-fill-column)

(defvar writeroom--saved-window-config nil
  "Window configuration active before `writeroom-mode' is activated.")

(defgroup writeroom nil "Minor mode for distraction-free writing."
  :group 'wp
  :prefix "writeroom-")

(defcustom writeroom-width 80
  "Width of the writeroom writing area.
This can be specified as an absolute width (the number of
characters in a line), or as a fraction of the total window
width, in the latter it should be a number between 0 and 1."
  :group 'writeroom
  :type '(choice (integer :tag "Absolute width:")
                 (float :tag "Relative width:" :value 0.5)))

(defcustom writeroom-mode-line nil
  "The mode line format to use with `writeroom-mode'.
By default, this option is set to nil, which disables the mode
line when `writeroom-mode' is activated.  By setting this option
to t, the standard mode line is retained.  Alternatively, it is
possible to specify a special mode line for `writeroom-mode'
buffers.  If this option is chosen, the default is to only show
the buffer's modification status and the buffer name, but the
format can be customized.  See the documentation for the variable
`mode-line-format' for further information.  Note that if you set
this option, it may be more visually pleasing to set
`writeroom-bottom-divider-width' to 0."
  :group 'writeroom
  :type '(choice (const :tag "Disable the mode line" nil)
                 (const :tag "Use default mode line" t)
                 (sexp :tag "Customize mode line"
                       :value ("   " mode-line-modified "   " mode-line-buffer-identification))))

(defcustom writeroom-mode-line-toggle-position 'header-line-format
  "Position to temporarily show the mode line.
When the mode line is disabled, the function
`writeroom-toggle-mode-line' makes the mode line visible.  This
option determines whether it is shown as the mode line or as the
header line."
  :group 'writeroom
  :type '(choice (const :tag "Use the mode line" 'mode-line-format)
                 (const :tag "Use the header line" 'header-line-format)))

(defcustom writeroom-bottom-divider-width 1
  "Width of the bottom window divider in pixels."
  :group 'writeroom
  :type '(integer :tag "Width"))

(make-obsolete-variable 'writeroom-disable-fringe
                        "The variable `writeroom-disable-fringe' is no longer used."
                        "`writeroom-mode' version 2.9")

(defcustom writeroom-maximize-window t
  "Whether to maximize the current window in its frame.
When set to t, `writeroom-mode' deletes all other windows in
the current frame."
  :group 'writeroom
  :type '(choice (const :tag "Maximize window" t)
                 (const :tag "Do not maximize window" nil)))

(defcustom writeroom-fullscreen-effect 'fullboth
  "Effect applied when enabling fullscreen.
The value can be `fullboth', in which case fullscreen is
activated, or `maximized', in which case the relevant frame is
maximized but window decorations are still available."
  :group 'writeroom
  :type '(choice (const :tag "Fullscreen" fullboth)
                 (const :tag "Maximized" maximized)))

(defcustom writeroom-border-width 30
  "Width in pixels of the border.
To use this option, select the option \"Add border\" in `Global
Effects'. This adds a border around the text area."
  :group 'writeroom
  :type '(integer :tag "Border width"))

(defcustom writeroom-fringes-outside-margins t
  "If set, place the fringes outside the margins."
  :group 'writeroom
  :type '(choice (const :tag "Place fringes outside margins" t)
                 (const :tag "Place fringes inside margins" nil)))

(defcustom writeroom-major-modes '(text-mode)
  "List of major modes in which writeroom-mode is activated.
The command `global-writeroom-mode' activates `writeroom-mode' in
every buffer that has one of the major modes listed in this
option.  Modes can be specified as symbols or as regular
expressions.  If a buffer has one of the specified major modes or
if its major mode name matches one of the regular expressions,
`writeroom-mode' is activated."
  :group 'writeroom
  :type '(repeat (choice (symbol :tag "Major mode")
                         (string :tag "Regular expression"))))

(defcustom writeroom-use-derived-modes t
  "Activate `writeroom-mode' in derived modes as well.'.
If this option is set, the command `global-writeroom-mode'
activates `writeroom-mode' in modes that are derived from those
listed in `writeroom-major-modes'.  Note that this option applies
only to symbols in `writeroom-major-modes'.  Regular expressions
are ignored."
  :group 'writeroom
  :type '(choice (const :tag "Use derived modes" t)
                 (const :tag "Do not use derived modes" nil)))

(defcustom writeroom-major-modes-exceptions nil
  "List of major modes in which `writeroom-mode' should not be activated.
This option lists exceptions to `writeroom-major-modes'.  Modes
can be specified as symbols or as regular expressions."
  :group 'writeroom
  :type '(repeat (choice (symbol :tag "Major mode exception")
                         (string :tag "Regular expression"))))

(defcustom writeroom-restore-window-config nil
  "If set, restore window configuration after disabling `writeroom-mode'.
Setting this option makes sense primarily if `writeroom-mode' is
used in one buffer only.  The window configuration that is stored
is the one that exists when `writeroom-mode' is first called, and
it is restored when `writeroom-mode' is deactivated in the last
buffer."
  :group 'writeroom
  :type '(choice (const :tag "Do not restore window configuration" nil)
                 (const :tag "Restore window configuration" t)))

(defcustom writeroom-extra-line-spacing nil
  "Additional line spacing for `writeroom-mode`."
  :group 'writeroom
  :type '(choice (const :tag "Do not add extra line spacing" :value nil)
                 (integer :tag "Absolute height" :value 5)
                 (float :tag "Relative height" :value 0.8)))

(defcustom writeroom-global-effects '(writeroom-set-fullscreen
                             writeroom-set-alpha
                             writeroom-set-menu-bar-lines
                             writeroom-set-tool-bar-lines
                             writeroom-set-vertical-scroll-bars
                             writeroom-set-bottom-divider-width)
  "List of global effects for `writeroom-mode'.
These effects are enabled when `writeroom-mode' is activated in
the first buffer and disabled when it is deactivated in the last
buffer."
  :group 'writeroom
  :type '(set (const :tag "Fullscreen" writeroom-set-fullscreen)
              (const :tag "Disable transparency" writeroom-set-alpha)
              (const :tag "Disable menu bar" writeroom-set-menu-bar-lines)
              (const :tag "Disable tool bar" writeroom-set-tool-bar-lines)
              (const :tag "Disable scroll bar" writeroom-set-vertical-scroll-bars)
              (const :tag "Enable bottom window divider" writeroom-set-bottom-divider-width)
              (const :tag "Add border" writeroom-set-internal-border-width)
              (const :tag "Display frame on all workspaces" writeroom-set-sticky)
              (repeat :inline t :tag "Custom effects" function)))

(define-obsolete-variable-alias 'writeroom-global-functions 'writeroom-global-effects "`writeroom-mode' version 2.0")

(defmacro define-writeroom-global-effect (fp value)
  "Define a global effect for `writeroom-mode'.
The effect is activated by setting frame parameter FP to VALUE.
FP should be an unquoted symbol, the name of a frame parameter;
VALUE must be quoted (unless it is a string or a number, of
course).  It can also be an unquoted symbol, in which case it
should be the name of a global variable whose value is then
assigned to FP.

This macro defines a function `writeroom-set-<FP>' that takes one
argument and activates the effect if this argument is 1 and
deactivates it if it is -1.  When the effect is activated, the
original value of frame parameter FP is stored in a frame
parameter `writeroom-<FP>', so that it can be restored when the
effect is deactivated."
  (declare (indent defun))
  (let ((wfp (intern (format "writeroom-%s" fp))))
    `(fset (quote ,(intern (format "writeroom-set-%s" fp)))
           (lambda (&optional arg)
             (when (frame-live-p writeroom--frame)
               (cond
                ((= arg 1)         ; activate
                 (set-frame-parameter writeroom--frame (quote ,wfp) (frame-parameter writeroom--frame (quote ,fp)))
                 (set-frame-parameter writeroom--frame (quote ,fp) ,value))
                ((= arg -1)        ; deactivate
                 (set-frame-parameter writeroom--frame (quote ,fp) (frame-parameter writeroom--frame (quote ,wfp)))
                 (set-frame-parameter writeroom--frame (quote ,wfp) nil))))))))

(define-writeroom-global-effect fullscreen writeroom-fullscreen-effect)
(define-writeroom-global-effect alpha '(100 100))
(define-writeroom-global-effect vertical-scroll-bars nil)
(define-writeroom-global-effect menu-bar-lines 0)
(define-writeroom-global-effect tool-bar-lines 0)
(define-writeroom-global-effect internal-border-width writeroom-border-width)
(define-writeroom-global-effect sticky t)
(define-writeroom-global-effect bottom-divider-width writeroom-bottom-divider-width)

(defun turn-on-writeroom-mode ()
  "Turn on `writeroom-mode'.
This function activates `writeroom-mode' in a buffer if that
buffer's major mode matchs against one of `writeroom-major-modes'."
  (unless (writeroom--match-major-mode writeroom-major-modes-exceptions)
    (if (writeroom--match-major-mode writeroom-major-modes writeroom-use-derived-modes)
        (writeroom-mode 1))))

(defun writeroom--match-major-mode (modes &optional derived)
  "Match the current buffer's major mode against MODES.
MODES a list of mode names (symbols) or regular expressions.
Return t if the current major mode matches one of the elements of
MODES, nil otherwise.  Comparison is done with `eq` (for symbols
in MODES) or with `string-match-p' (for strings in MODES).  That
is, if the major mode is e.g., `emacs-lisp-mode', it will not
match the symbol `lisp-mode', but it will match the string
\"lisp-mode\".

If DERIVED is non-nil, also return t if the current buffer's
major mode is a derived mode of one of the major mode symbols in
MODES."
  (catch 'match
    (dolist (elem modes)
      (if (cond ((symbolp elem)
                 (or (eq elem major-mode)
                     (and derived (derived-mode-p elem))))
                ((string-match-p elem (symbol-name major-mode))))
          (throw 'match t)))))

(defvar writeroom-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-?") #'writeroom-toggle-mode-line)
    map)
  "Keymap for writeroom-mode.")

;;;###autoload
(define-minor-mode writeroom-mode
  "Minor mode for distraction-free writing."
  :init-value nil :lighter nil :global nil
  (if writeroom-mode
      (writeroom--enable)
    (writeroom--disable)))

;;;###autoload
(define-globalized-minor-mode global-writeroom-mode writeroom-mode turn-on-writeroom-mode
  :require 'writeroom-mode
  :group 'writeroom)

(defun writeroom--kill-buffer-function ()
  "Disable `writeroom-mode' before killing a buffer, if necessary.
This function is for use in `kill-buffer-hook'.  It checks whether
`writeroom-mode' is enabled in the buffer to be killed and
adjusts `writeroom--buffers' and the global effects accordingly."
  (when writeroom-mode
    (setq writeroom--buffers (delq (current-buffer) writeroom--buffers))
    (when (not writeroom--buffers)
      (writeroom--set-global-effects -1)
      (setq writeroom--frame nil))))

(add-hook 'kill-buffer-hook #'writeroom--kill-buffer-function)

(defun writeroom--set-global-effects (arg)
  "Activate or deactivate global effects.
The effects are activated if ARG is 1, deactivated if it is -1."
  (mapc (lambda (fn)
          (funcall fn arg))
        writeroom-global-effects))

(defun writeroom--calculate-width ()
  "Calculate the width of the writing area."
  (if (floatp writeroom-width)
      (truncate (* (window-total-width) writeroom-width))
    writeroom-width))

(defvar writeroom--mode-line-showing nil
  "Flag indicating whether the original mode line is displayed.")
(make-variable-buffer-local 'writeroom--mode-line-showing)

(defvar writeroom--orig-header-line nil
  "Original format of the header line.
When the header line is used to temporarily display the mode
line, its original format is saved here.")
(make-variable-buffer-local 'writeroom--orig-header-line)

(defun writeroom-toggle-mode-line ()
  "Toggle display of the original mode."
  (interactive)
  (unless (eq writeroom-mode-line t) ; This means the original mode-line is displayed already.
    (cond
     ((not writeroom--mode-line-showing)
      (setq writeroom--orig-header-line header-line-format)
      (set writeroom-mode-line-toggle-position (or (cdr (assq 'mode-line-format writeroom--saved-data))
                                          (default-value 'mode-line-format)))
      (setq writeroom--mode-line-showing t))
     (writeroom--mode-line-showing
      (if (eq writeroom-mode-line-toggle-position 'header-line-format)
          (setq header-line-format writeroom--orig-header-line)
        (setq mode-line-format writeroom-mode-line))
      (setq writeroom--mode-line-showing nil)))
    (force-mode-line-update)))

(defun writeroom-adjust-width (amount)
  "Adjust the width of the writing area on the fly by AMOUNT.
A numeric prefix argument can be used to specify the adjustment.
When called without a prefix, this will reset the width to the default value."
  (interactive "P")
  (if amount
      (setq visual-fill-column-width (max 1 (+ visual-fill-column-width amount)))
    (setq visual-fill-column-width (writeroom--calculate-width)))
  (visual-fill-column--adjust-window)
  (message "Writing area is now %d characters wide" visual-fill-column-width))

(defun writeroom-increase-width ()
  "Increase the width of the writing area by 2 characters."
  (interactive)
  (writeroom-adjust-width 2))

(defun writeroom-decrease-width ()
  "Decrease the width of the writing area by 2 characters."
  (interactive)
  (writeroom-adjust-width -2))

(defun writeroom--enable ()
  "Set up writeroom-mode for the current buffer.
Also run the functions in `writeroom-global-effects' if the
current buffer is the first buffer in which `writeroom-mode' is
activated."
  ;; save buffer-local variables, if they have a buffer-local binding
  (setq writeroom--saved-data (mapcar (lambda (sym)
                               (if (local-variable-p sym)
                                   (cons sym (buffer-local-value sym (current-buffer)))
                                 sym))
                             writeroom--local-variables))
  (setq writeroom--saved-visual-fill-column visual-fill-column-mode)

  ;; activate global effects
  (when (not writeroom--buffers)
    (setq writeroom--frame (selected-frame))
    (writeroom--set-global-effects 1)
    (if writeroom-restore-window-config
        (setq writeroom--saved-window-config (current-window-configuration))))

  (push (current-buffer) writeroom--buffers)

  (when writeroom-maximize-window
    (delete-other-windows))

  (when writeroom-extra-line-spacing
    (setq line-spacing writeroom-extra-line-spacing))

  (unless (eq writeroom-mode-line t) ; if t, use standard mode line
    (setq mode-line-format writeroom-mode-line))

  (setq visual-fill-column-width (writeroom--calculate-width)
        visual-fill-column-center-text t
        visual-fill-column-fringes-outside-margins writeroom-fringes-outside-margins)
  (visual-fill-column-mode 1)

  ;; if the current buffer is displayed in some window, the windows'
  ;; margins and fringes must be adjusted.
  (mapc (lambda (w)
          (with-selected-window w
            (visual-fill-column--adjust-window)))
        (get-buffer-window-list (current-buffer) nil)))

(defun writeroom--disable ()
  "Reset the current buffer to its normal appearance.
Also run the functions in `writeroom-global-effects' to undo
their effects if `writeroom-mode' is deactivated in the last
buffer in which it was active."
  ;; disable visual-fill-column-mode
  (visual-fill-column-mode -1)
  (kill-local-variable 'visual-fill-column-width)
  (kill-local-variable 'visual-fill-column-center-text)
  (kill-local-variable 'visual-fill-column-fringes-outside-margins)

  ;; restore global effects if necessary
  (setq writeroom--buffers (delq (current-buffer) writeroom--buffers))
  (when (not writeroom--buffers)
    (writeroom--set-global-effects -1)
    (setq writeroom--frame nil)
    (if writeroom-restore-window-config
        (set-window-configuration writeroom--saved-window-config)))

  ;; restore local variables
  (mapc (lambda (val)
          (if (symbolp val)
              (kill-local-variable val)
            (set (car val) (cdr val))))
        writeroom--saved-data)

  ;; if the current buffer is displayed in some window, the windows'
  ;; margins and fringes must be adjusted.
  (mapc (lambda (w)
          (with-selected-window w
            (set-window-margins (selected-window) 0 0)
            (set-window-fringes (selected-window) nil)))
        (get-buffer-window-list (current-buffer) nil))

  ;; reenable `visual-fill-colummn-mode' with original settings if it was
  ;; active before activating `writeroom-mode'.
  (if writeroom--saved-visual-fill-column
      (visual-fill-column-mode 1)))

(provide 'writeroom-mode)

;;; writeroom-mode.el ends here

;;; better-registers.el --- facilities for more powerful registers
;; -*- emacs-lisp -*-
;; Copyright (C) 2005 Sigurd Meldgaard

;; Author: Sigurd Meldgaard <yogurth @ gmail>
;; Keywords: register macro

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides a row of commands, and keyboard shortcuts to
;; make the powerful concept of registers even more powerful (most
;; important it lets you save macros there), and to save them to a
;; file. It is collected as a minor mode.

;; The code requires the cl.el (common-lisp-wannabe extensions) package
;; It is not tested under xemacs

;;; Installation:

;; dump this file somwhere on your load path. Then insert the
;; following into your .emacs (or other file loaded at startup) (of
;; course removing the ;;'s

;; ;;;Better registers!
;; (require 'better-registers)
;; (better-registers-install-save-registers-hook)
;; (load better-registers-save-file)

;; The last two line are unnecessary if you do not want to have your
;; registers contents saved across sessions.

;; Note that this package is pretty harsh to your std shortcuts you
;; probably should edit them (or at least see them through this file
;; to suit your needs. I e.g. personally never used backwards searches,
;; so C-r was doomed in my hands. I use only enter for indented
;; linebreaks, so C-j also seemed as a nice candidate for something
;; more useful - you might want to bind:
;; (global-set-key "\r" 'newline-and-indent)

;;; Use:

;; Macros: record a macro as usual and then press C-r m to save the
;; macro in a register. You now can play the macro with C-j

;; Markers: (as usual) Place point a place you want to remember C-r
;; space will put the marker in a register. When the buffer is closed
;; or the register is saved and reloaded it is turned into a
;; file-query that is a question about whether to reopen the file. C-j
;; will bring you back to the marker.

;; Window/Frame-configurations: C-rw and C-rf will save the current
;; configuration in the given register. To recall use C-j
;; Remark: these will not be saved for next session :(

;; Other stuff: for rectangles and numbers see through the list of
;; key-shortcuts and get good ideas.

;; In modes which overwrite C-j C-xj should still be bound to the same.

(defvar better-registers-version "0.58"
  "The version of the package better-registers.
   Revision history:
   from 0.57 to 0.58 Improved interactive argument handling of better-registers-save-registers.
   from 0.57 to 0.57 Can now correctly save fontified strings, added convenient macro key (f1)
   from 0.55 to 0.56 No longer blocks enter in the minibuffer
   from 0.5 to 0.55 changed it to a minor mode
   instead of having global keys - much nicer! Thanks to Lennart Borgman
   for the advice.")

(defgroup better-registers nil
  "Some convenience and power for using registers and macros"
  :group 'convenience)

(defcustom better-registers-save-file "~/.emacsregisters.el"
  "The place where the contents of the registers should be saved."
  :group 'better-registers
  :type '(file))

(defvar better-registers-map (make-keymap)
  "Keymap for better-registers minor-mode")

(defvar better-registers-r-map (make-sparse-keymap)
  "Keymap for combinations with C-r first")

;(define-key better-registers-map [f1] 'better-registers-play-macro-if-not-playing) ;;conflict with yari
;(define-key better-registers-map [S-f1] 'better-registers-toggle-macro-recording)
(define-key better-registers-map "\C-j" 'better-registers-jump-to-register)
(define-key better-registers-map "\C-xj" 'better-registers-jump-to-register)
;; C-x r is reserved for column edit
;;(define-key better-registers-map "\C-xr" 'isearch-backward) ;free C-r
(define-key better-registers-map "\C-r" ;Shadow C-r global
  better-registers-r-map)
(define-key better-registers-r-map "n" 'number-to-register)
(define-key better-registers-r-map "+" 'increment-register)
(define-key better-registers-r-map "-" 'better-registers-decrement-register)
(define-key better-registers-r-map "w" 'window-configuration-to-register)
(define-key better-registers-r-map "f" 'frame-configuration-to-register)
(define-key better-registers-r-map "r" 'copy-rectangle-to-register)
(define-key better-registers-r-map "i" 'better-registers-jump-to-register)
(define-key better-registers-r-map "s" 'copy-to-register)
(define-key better-registers-r-map " " 'point-to-register)
(define-key better-registers-r-map "k" 'kill-rectangle)
(define-key better-registers-r-map "m"
  'better-registers-put-keyboard-macro-in-register)
(define-key better-registers-r-map "p"
  'better-registers-put-buffer-filename-in-register)
(define-key better-registers-r-map "b"
  'better-registers-put-buffer-in-register)
;(define-key better-registers-r-map "k" 'kill-rectangle)
;(define-key better-registers-r-map "d" 'delete-rectangle)
;(define-key better-registers-r-map "y" 'yank-rectangle)
;(define-key better-registers-r-map "o" 'open-rectangle)
;(define-key better-registers-r-map "t" 'string-rectangle)

(define-minor-mode better-registers
  "A minor mode for easier and more powerful register commands"
  t
  nil
  better-registers-map
  ())


(defun better-registers-install-save-registers-hook ()
  (add-hook 'kill-emacs-hook
            '(lambda ()
               (better-registers-save-registers))))

(defun better-registers-save-registers (&optional filename queryp)
  "Print the contents of all registers to a file as loadable data.
   Cannot save window/frame configuration.
   But it works with keyboard macros, text, buffernames,
   filenames and rectangles.

   If filename is non-nil and queryp is nil, use that, otherwise
   use the default filename.  If queryp is non-nil (a prefix
   argument is given), query interactively for the file-name."
  (interactive "i\nP")
  (when queryp
    (setq filename (read-file-name nil better-registers-save-file)))
  (let ((fn (or filename better-registers-save-file))
         (print-level nil) ;Let us write anything
         (print-length nil)
        (b (generate-new-buffer "*registers*")))
    (set-buffer b)
    (dolist (i register-alist)
      (let ((char (car i))
            (contents (cdr i)))
        (cond
         ((stringp contents)
          (insert (format "%S\n"
                         `(set-register
                           ,char
                           ,contents))))
         ((numberp contents) ;numbers are printed non-quotes
          (insert (format "%S\n" `(set-register ,char ,contents))))
         ((markerp contents)
          (insert (format
                   "%S\n"
                   `(set-register
                     ,char
                     '(file-query
                      ,(buffer-file-name (marker-buffer contents))
                      ,(marker-position contents))))))
         ((bufferp (cdr contents))
          (insert (format "%s\n"
                          `(set-register ,char
                                         ',(buffer-name (cdr contents))))))
         (t (when (and contents ; different from nil
                       (not (or (window-configuration-p (car contents))
                                (frame-configuration-p (car contents)))))
              (insert (format "%S\n"
                              `(set-register ,char (quote ,contents)))))))))
    (write-file fn)
    (kill-buffer b)))


(defun better-registers-put-buffer-in-register (register &optional delete)
  "Put current buffername in register - this would also work for
  just buffers, as switch-to-buffer can use both, but it
  facilitates for easier saving/restoring of registers."
  (interactive "cPut current buffername in register: \nP.")
  (set-register register (cons 'buffer (buffer-name (current-buffer)))))

(defun better-registers-put-buffer-filename-in-register
  (register &optional delete)
  "This is better than put-buffer-in-register for file-buffers, because a closed
   file can be opened again, but does not work for no-file-buffers."
  (interactive "cPut the filename of current buffer in register: \nP")
  (set-register register (cons 'file (buffer-file-name (current-buffer)))))

(defun better-registers-put-keyboard-macro-in-register
  (register &optional delete)
  "Save the contents of the last keyboard macro to the given register.
   can be played again by jump-to-register."
  (interactive "cPut last keyboard-macro in register: \nP")
  (set-register register (cons 'macro last-kbd-macro)))

(defun better-registers-jump-to-register (register &optional delete)
  "Do what is the most sane thing to do for the thing stored in
   register Either insert text (evt. a rectangle), move point to
   location stored in a register, a buffer stored in a register,
   a file stored in register, or run a macro saved in a register.
   If the register contains a file name, find that file. Or
   restore a saved window/frame configuration."
  (interactive "cJump to register: \nP")
  (let ((val (get-register register)))
    (cond
     ((and (consp val) (frame-configuration-p (car val)))
      (set-frame-configuration (car val) (not delete))
      (goto-char (cadr val)))
     ((and (consp val) (window-configuration-p (car val)))
      (set-window-configuration (car val))
      (goto-char (cadr val)))
     ((markerp val)
      (or (marker-buffer val)
          (error "That register's buffer no longer exists"))
      (switch-to-buffer (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     ((and (consp val) (eq (car val) 'buffer))
      (switch-to-buffer (cdr val)))
     ((and (consp val) (eq (car val) 'macro))
      ;appearently the only way to run a macro is by putting them in
      ;last-kbd-macro (named keyboard macros can only (as far as I
      ;know) be called interactively, but this works quite
      ;unproblematically).
      (let ((old-macro last-kbd-macro))
        (setq last-kbd-macro (cdr val))
        (call-last-kbd-macro)
        (setq last-kbd-macro old-macro)))
     ((and (consp val) (eq (car val) 'file-query))
      (or (find-buffer-visiting (nth 1 val))
          (y-or-n-p (format "Visit file %s again? " (nth 1 val)))
          (error "Register access aborted"))
      (find-file (nth 1 val))
      (goto-char (nth 2 val)))
     ((or (stringp val)
          (consp val)
          (numberp val)
          (and (markerp val)
               (marker-position val)))
          (insert-register register))
     (t
      (error "Register doesn't contain a buffer, buffer position, macro, file, text, rectangle or configuration")))))

;;Just call increment-register with the negative arg.
(defun better-registers-decrement-register (number register)
  "Subtract NUMBER from the contents of register REGISTER.
Interactively, NUMBER is the prefix arg."
  (interactive "p\ncDecrement register: ")
  (increment-register (- number) register))

(defun better-registers-toggle-macro-recording ()
  (interactive)
  (message "hej")
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

(defun better-registers-play-macro-if-not-playing ()
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (call-last-kbd-macro)))

(provide 'better-registers)

;;; better-registers.el ends here.

;; Copyright (C) 2010  Niels Giesen 

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
;; Keywords: twitter
;; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file provides a minor mode to enable operations using link
;; numbers. The code is essentially a modified version of w3m-lnum.el
;; by TSUCHIYA Masatoshi, that does the same for emacs-w3m.

;;; Usage:

;; Install this file to an appropriate directory, and add these
;; expressions to your initialization file.

;; (autoload 'twittering-numbering "twittering-numbering" nil t)
;; (add-hook 'twittering-mode-hook 'twittering-numbering)

;; Toggle numbering mode on and off with "N".

;; Move to a 'thing' with [0-9]+ ENTER

;;; Code:

(require 'twittering-mode)

(defgroup twittering-numbering nil
  "Customization group for numbered things in `twittering-mode'"
  :group 'twittering
  ;; NOTE: no group for `twittering' actually exists
  )

(defcustom twittering-numbering-backwards t
  "Number bottom-up?"
  :group 'twittering-numbering
  :type 'boolean)

(defface twittering-numbering
  '((((class color) (background light)) (:foreground "gray60"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Face used to highlight link numbers.")

(defvar twittering-numbering-mode nil
  "Non-nil if twittering operations using link numbers are enabled.")

(make-variable-buffer-local 'twittering-numbering-mode)

(defun twittering-numbering-after ()
 (run-with-timer 1 nil 'twittering-numbering twittering-new-tweets-spec))

(define-key twittering-mode-map "N" 'twittering-numbering-mode)

(defun twittering-numbering-mode (&optional arg)
  "Minor mode to enable operations using link numbers"
  (interactive "P")
  (add-hook 'twittering-new-tweets-hook 'twittering-numbering-after)
  (if (setq twittering-numbering-mode
            (if arg
                (> (prefix-numeric-value arg) 0)
              (not twittering-numbering-mode)))
      (progn
        (use-local-map twittering-numbering-mode-map)
        (twittering-numbering))
    
    (use-local-map twittering-mode-map)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'twittering-numbering-overlay)
        (delete-overlay overlay)))))

(defun twittering-numbering (&optional twittering-new-tweets-spec)
  "Make overlays that display link numbers."
  (with-current-buffer
      (or 
       (twittering-get-buffer-from-spec twittering-new-tweets-spec)
       (current-buffer))

    (when twittering-numbering-mode
      (save-excursion
        (goto-char
         (if twittering-numbering-backwards
             (point-max)
           (point-min)))
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (overlay-get overlay 'twittering-numbering-overlay)
            (delete-overlay overlay)))
        (let ((i 0)
              overlay num)
          (catch 'already-numbered
            (while
                (if twittering-numbering-backwards
                    (twittering-goto-previous-thing)
                  (twittering-goto-next-thing))
              (when (get-char-property (point)
                                       'twittering-numbering-overlay)
                (throw 'already-numbered nil))
              (setq overlay (make-overlay (point) (1+ (point)))
                    num (format "[%d]" (incf i)))
              (add-text-properties 0 (length num)
                                   '(face
                                     twittering-numbering) num)
              
              (overlay-put overlay 'before-string num)
              (overlay-put overlay 'evaporate t)
              (overlay-put overlay 'twittering-numbering-overlay i))))))))

(defun twittering-move-numbered-anchor (&optional arg)
  "Move the point to the specified anchor.
When no prefix argument is specified, call `twittering-enter' instead
of moving cursor."
  (interactive "P")
  (if (and arg
           (> (setq arg (prefix-numeric-value arg)) 0))
      (catch 'found
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (eq arg (overlay-get overlay 'twittering-numbering-overlay))
            (goto-char (overlay-start overlay))
            (throw 'found nil)))
        (error "Cannot found your specified link: %d" arg))
    (twittering-enter)))

(defvar twittering-numbering-mode-map
  (let ((keymap (copy-keymap twittering-mode-map)))
    (substitute-key-definition
     'twittering-enter
     'twittering-move-numbered-anchor
     keymap twittering-mode-map)
    (dotimes (i 10)
      (define-key keymap
        (format "%d" i) 'digit-argument))
    keymap)
  "Keymap used when `twittering-numbering-mode' is active.")

(provide 'twittering-numbering)
;;; twittering-numbering.el ends here

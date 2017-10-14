;;; diff-hl-margin.el --- Highlight buffer changes on margins -*- lexical-binding: t -*-

;; Copyright (C) 2012-2017  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a global mode, it modifies `diff-hl-mode' to use the margin
;; instead of the fringe. To toggle, type `M-x diff-hl-margin-mode'.
;;
;; Compared to the default behavior, this makes `diff-hl-mode'
;; indicators show up even when Emacs is running in a terminal.
;;
;; On the flip side, the indicators look simpler, and they are
;; incompatible with `linum-mode' or any other mode that uses the
;; margin.
;;
;; You might want to enable it conditionally in your init file
;; depending on whether Emacs is running in graphical mode:
;;
;; (unless (window-system) (diff-hl-margin-mode))

(require 'cl-lib)
(require 'diff-hl)
(require 'diff-hl-dired)

(defvar diff-hl-margin-old-highlight-function nil)

(defgroup diff-hl-margin nil
  "Highlight buffer changes on margin"
  :group 'diff-hl)

(defface diff-hl-margin-insert
  '((default :inherit diff-hl-insert))
  "Face used to highlight inserted lines on the margin.")

(defface diff-hl-margin-delete
  '((default :inherit diff-hl-delete))
  "Face used to highlight deleted lines on the margin.")

(defface diff-hl-margin-change
  '((default :inherit diff-hl-change))
  "Face used to highlight changed lines on the margin.")

(defface diff-hl-margin-ignored
  '((default :inherit dired-ignored))
  "Face used to highlight changed lines on the margin.")

(defface diff-hl-margin-unknown
  '((default :inherit dired-ignored))
  "Face used to highlight changed lines on the margin.")

(defcustom diff-hl-margin-symbols-alist
  '((insert . "+") (delete . "-") (change . "!")
    (unknown . "?") (ignored . "i"))
  "Associative list from symbols to strings."
  :type '(alist :key-type symbol
                :value-type string
                :options (insert delete change unknown ignored))
  :set (lambda (symbol value)
         (defvar diff-hl-margin-spec-cache)
         (set-default symbol value)
         (setq diff-hl-margin-spec-cache nil)))

;;;###autoload
(define-minor-mode diff-hl-margin-mode
  "Toggle displaying `diff-hl-mode' highlights on the margin."
  :lighter "" :global t
  (if diff-hl-margin-mode
      (progn
        (add-hook 'diff-hl-mode-on-hook 'diff-hl-margin-minor-mode)
        (add-hook 'diff-hl-mode-off-hook 'diff-hl-margin-minor-mode-off)
        (add-hook 'diff-hl-dired-mode-on-hook 'diff-hl-margin-minor-mode)
        (add-hook 'diff-hl-dired-mode-off-hook 'diff-hl-margin-minor-mode-off))
    (remove-hook 'diff-hl-mode-on-hook 'diff-hl-margin-minor-mode)
    (remove-hook 'diff-hl-mode-off-hook 'diff-hl-margin-minor-mode-off)
    (remove-hook 'diff-hl-dired-mode-on-hook 'diff-hl-margin-minor-mode)
    (remove-hook 'diff-hl-dired-mode-off-hook 'diff-hl-margin-minor-mode-off))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (cond
       (diff-hl-mode
        (diff-hl-margin-minor-mode (if diff-hl-margin-mode 1 -1))
        (diff-hl-update))
       (diff-hl-dired-mode
        (diff-hl-margin-minor-mode (if diff-hl-margin-mode 1 -1))
        (diff-hl-dired-update))))))

(define-minor-mode diff-hl-margin-minor-mode
  "Toggle displaying `diff-hl-mode' highlights on the margin locally.
You probably shouldn't use this function directly."
  :lighter ""
  (let ((width-var (intern (format "%s-margin-width" diff-hl-side))))
    (if diff-hl-margin-minor-mode
        (progn
          (set (make-local-variable 'diff-hl-margin-old-highlight-function)
               diff-hl-highlight-function)
          (set (make-local-variable 'diff-hl-highlight-function)
               'diff-hl-highlight-on-margin)
          (set width-var 1))
      (setq diff-hl-highlight-function diff-hl-margin-old-highlight-function
            diff-hl-margin-old-highlight-function nil)
      (set width-var 0)))
  (dolist (win (get-buffer-window-list))
    (set-window-buffer win (current-buffer))))

(define-obsolete-variable-alias 'diff-hl-margin-side 'diff-hl-side "1.7.1")

(defun diff-hl-margin-minor-mode-off ()
  (diff-hl-margin-minor-mode -1))

(defvar diff-hl-margin-spec-cache nil)

(defun diff-hl-margin-spec-cache ()
  (or diff-hl-margin-spec-cache
      (setq diff-hl-margin-spec-cache
            (diff-hl-margin-build-spec-cache))))

(defun diff-hl-margin-build-spec-cache ()
  (cl-loop for (type . char) in diff-hl-margin-symbols-alist
           nconc
           (cl-loop for side in '(left right)
                    collect
                    (cons
                     (cons type side)
                     (propertize
                      " " 'display
                      `((margin ,(intern (format "%s-margin" side)))
                        ,(propertize char 'face
                                     (intern (format "diff-hl-margin-%s" type)))))))))

(defun diff-hl-highlight-on-margin (ovl type _shape)
  (let ((spec (cdr (assoc (cons type diff-hl-side)
                          (diff-hl-margin-spec-cache)))))
    (overlay-put ovl 'before-string spec)))

(provide 'diff-hl-margin)

;;; diff-hl-margin.el ends here

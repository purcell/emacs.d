;;; fic-mode.el --- Show FIXME/TODO/BUG/KLUDGE in special face only in comments and strings
;;--------------------------------------------------------------------
;;
;; Copyright (C) 2010, Trey Jackson <bigfaceworm(at)gmail(dot)com>
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; To use, save fic-mode.el to a directory in your load-path.
;;
;; (require 'fic-mode)
;; (add-hook 'c++-mode-hook 'turn-on-fic-mode)
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)
;;
;; or
;;
;; M-x fic-mode
;;
;; NOTE: If you manually turn on fic-mode, you you might need to force re-fontification initially
;;   M-x font-lock-fontify-buffer

(defcustom fic-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE")
  "Words to highlight"
  :group 'fic-mode)

(defcustom fic-foreground-color "Red"
  "Font foreground colour"
  :group 'fic-mode)

(defcustom fic-background-color  "Yellow"
  "Font background color"
  :group 'fic-mode)

(defcustom font-lock-fic-face 'font-lock-fic-face
  "variable storing the face for fic mode"
  :group 'fic-mode)

(make-face 'font-lock-fic-face)
(modify-face 'font-lock-fic-face fic-foreground-color
             fic-background-color nil t nil t nil nil)

(defvar fic-search-list-re (regexp-opt fic-highlighted-words 'words)
  "regexp constructed from 'fic-highlighted-words")

(defun fic-in-doc/comment-region (pos)
  (memq (get-char-property pos 'face)
	(list font-lock-doc-face font-lock-string-face font-lock-comment-face)))

(defun fic-search-for-keyword (limit)
  (let ((match-data-to-set nil)
	found)
    (save-match-data
      (while (and (null match-data-to-set)
		  (re-search-forward fic-search-list-re limit t))
	(if (and (fic-in-doc/comment-region (match-beginning 0))
		 (fic-in-doc/comment-region (match-end 0))) 
	    (setq match-data-to-set (match-data)))))
    (when match-data-to-set
      (set-match-data match-data-to-set)
      (goto-char (match-end 0)) 
      t)))

;;;###autoload
(define-minor-mode fic-mode "highlight FIXMEs in comments and strings (as well as TODO BUG and KLUDGE"
  :lighter " FIC" :group 'fic-mode
  (let ((kwlist '((fic-search-for-keyword (0 'font-lock-fic-face t)))))
    (if fic-mode
        (font-lock-add-keywords nil kwlist)
      (font-lock-remove-keywords nil kwlist))))

(defun turn-on-fic-mode ()
  "turn fic-mode on"
  (interactive)
  (fic-mode 1))

(provide 'fic-mode)

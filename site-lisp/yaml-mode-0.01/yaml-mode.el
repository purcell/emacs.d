;;; yaml-mode.el ---
;; $Id$

;; Copyright (C) 2006  Free Software Foundation, Inc.

;; Author: Yoshiki Kurihara <kurihara@livedoor.jp>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To handle files included with do 'filename.yml';, add something like
;; (setq auto-mode-alist (append (list (cons "\\.yml\\'" 'yaml-mode))
;;                               auto-mode-alist))
;; to your .emacs file; otherwise the .pl suffix defaults to prolog-mode.

;;; Code:

(defvar yaml-font-lock-keywords
  (list
   '("^[ \t]*\\(.+\\)[ \t]*:[ \r\n]" 0 font-lock-variable-name-face)
   '("\\(%YAML\\|# \\(.*\\)\\|\\(---\\|\\.\\.\\.\\)\\(.*\\)\\)" 0 font-lock-comment-face)
   '("\\(\\*\\|\\&\\)\\(.*\\)" 0 (cons font-lock-variable-name-face '(underline)))
   '("\\!\\!\\sw+[ \r\n]" 0 font-lock-function-name-face)
   ))

(define-derived-mode yaml-mode fundamental-mode "YAML"
  "Simple mode to edit yaml

\\{yaml-mode-map}"
  
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "\\(^\\|\\s-\\);?#+ *")
  (set (make-local-variable 'font-lock-defaults) '(yaml-font-lock-keywords))
  )

(provide 'yaml-mode)
;;; yaml-mode.el ends here

;;; mmm-cweb.el --- MMM submode class for CWeb programs

;; Copyright (C) 2001 by Alan Shutko

;; Author: Alan Shutko <ats@acm.org>

;;{{{ GPL

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

;;}}}

;;; Commentary:

;; This file contains the definition of an MMM Mode submode class for
;; editing CWeb programs.

;;; Code:

(require 'mmm-compat)
(require 'mmm-vars)
(require 'mmm-auto)

(defvar mmm-cweb-section-tags
  '("@ " "@*"))

(defvar mmm-cweb-section-regexp
  (concat "^" (mmm-regexp-opt mmm-cweb-section-tags t)))

(defvar mmm-cweb-c-part-tags
  '("@c" "@>=" "@>+=" "@p"))

(defvar mmm-cweb-c-part-regexp
  (concat (mmm-regexp-opt mmm-cweb-c-part-tags t)))

(defun mmm-cweb-in-limbo (pos)
  "Check to see if POS is in limbo, ie before any cweb sections."
  (save-match-data
    (save-excursion
      (goto-char pos)
      (not (re-search-backward mmm-cweb-section-regexp nil t)))))

(defun mmm-cweb-verify-brief-c ()
  "Verify function for cweb-brief-c class.
Checks whether the match is in limbo."
  (not (mmm-cweb-in-limbo (match-beginning 0))))

(mmm-add-group
 'cweb
 `(
   (cweb-c-part
    :submode c-mode
    :front ,mmm-cweb-c-part-regexp
    :back ,mmm-cweb-section-regexp)
   (cweb-label
    :submode tex-mode
    :front "@<"
    :back "@>"
    :face mmm-comment-submode-face
    :insert ((?l cweb-label nil @ "@<" @ "@>")))
   (cweb-brief-c
    :submode c-mode
    :front "[^\\|]\\(|\\)[^|]"
    :front-match 1
    :front-verify mmm-cweb-verify-brief-c
;    :front-offset -1
    :back "[^\\|]\\(|\\)"
    :back-match 1
;    :back-offset 1
    :end-not-begin t
    :insert ((?| cweb-c-in-tex nil "|" @ "|")))
    (cweb-comment
     :submode tex-mode
     :front "/[*]"
     :back "[*]/"
     :face mmm-comment-submode-face)
))

;; (add-to-list 'mmm-mode-ext-classes-alist
;;                   '(plain-tex-mode "\\.w\\'" cweb))
;; (add-to-list 'mmm-mode-ext-classes-alist
;;                   '(latex-mode "\\.w\\'" cweb))
;; (add-to-list 'auto-mode-alist '("\\.w\\'" . tex-mode))

(provide 'mmm-cweb)

;;; mmm-cweb.el ends here

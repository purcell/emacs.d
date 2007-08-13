;;; semantic-ia.el --- Interactive Analysis functions

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-ia.el,v 1.14 2007/02/22 03:32:03 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Interactive access to `semantic-analyze'.
;;

(require 'senator)
(require 'semantic-analyze)

;;; Code:
(defcustom semantic-ia-completion-format-tag-function
  'semantic-prototype-nonterminal
  "*Function used to convert a tag to a string during completion."
  :group 'semantic
  :type semantic-format-tag-custom-list)

(defvar semantic-ia-cache nil
  "Cache of the last completion request.
Of the form ( POINT . COMPLETIONS ) where POINT is a location in the
buffer where the completion was requested.  COMPLETONS is the list
of semantic tag names that provide logical completions from that
location.")
(make-variable-buffer-local 'semantic-ia-cache)

(defun semantic-ia-get-completions (context point)
  "Fetch the completion of CONTEXT at POINT.
Supports caching."
  (let ((symbols
	 (if (and semantic-ia-cache
		  (= point (car semantic-ia-cache)))
	     (cdr semantic-ia-cache)
	   (semantic-analyze-possible-completions context))))
    ;; Set the cache
    (setq semantic-ia-cache (cons point symbols))
    symbols))

;;;###autoload
(defun semantic-ia-complete-symbol (point)
  "Complete the current symbol at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'."
  (interactive "d")
  (let* ((a (semantic-analyze-current-context point))
	 (syms (semantic-ia-get-completions a point))
	 (pre (car (reverse (oref a prefix))))
	 )
    ;; If PRE was actually an already completed symbol, it doesn't
    ;; come in as a string, but as a tag instead.
    (if (semantic-tag-p pre)
	;; We will try completions on it anyway.
	(setq pre (semantic-tag-name pre)))
    ;; Complete this symbol.
    (if (null syms)
	(progn
	  (message "No smart completions found.  Trying senator-complete-symbol.")
	  (if (semantic-analyze-context-p a)
	      (senator-complete-symbol)
	      ))
      ;; Use try completion to seek a common substring.
      (let ((tc (try-completion pre syms)))
	(if (and (stringp tc) (not (string= tc pre)))
	    (let ((tok (semantic-find-first-tag-by-name
			tc syms)))
	      ;; We have some new text.  Stick it in.
	      (delete-region (car (oref a bounds))
			     (cdr (oref a bounds)))
	      (goto-char (car (oref a bounds)))
	      (if tok
		  (semantic-ia-insert-tag tok)
		(insert tc)))
	  ;; We don't have new text.  Show all completions.
	  (goto-char (cdr (oref a bounds)))
	  (with-output-to-temp-buffer "*Completions*"
	    (display-completion-list
	     (mapcar semantic-ia-completion-format-tag-function syms))
	    ))))))

(defcustom semantic-ia-completion-menu-format-tag-function
  'semantic-uml-concise-prototype-nonterminal
  "*Function used to convert a tag to a string during completion."
  :group 'semantic
  :type semantic-format-tag-custom-list)

;;;###autoload
(defun semantic-ia-complete-symbol-menu (point)
  "Complete the current symbol via a menu based at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'."
  (interactive "d")
  (let* ((a (semantic-analyze-current-context point))
	 (syms (semantic-ia-get-completions a point))
	 (pre (car (reverse (oref a prefix))))
	 )
    ;; Complete this symbol.
    (if (not syms)
	(progn
	  (message "No smart completions found.  Trying Senator.")
	  (if (semantic-analyze-context-p a)
	      (senator-completion-menu-popup)))
      (let* ((menu
	      (mapcar
	       (lambda (tag)
		 (cons
		  (funcall semantic-ia-completion-menu-format-tag-function tag)
		  (vector tag)))
	       syms))
	     (ans
	      (imenu--mouse-menu
	       ;; XEmacs needs that the menu has at least 2 items.  So,
	       ;; include a nil item that will be ignored by imenu.
	       (cons nil menu)
	       (senator-completion-menu-point-as-event)
	       "Completions")))
	(when ans
	  (if (not (semantic-tag-p ans))
	      (setq ans (aref (cdr ans) 0)))
	  (delete-region (car (oref a bounds)) (cdr (oref a bounds)))
	  (semantic-ia-insert-tag ans))
	))))

(defun semantic-ia-insert-tag (tag)
  "Insert TAG into the current buffer based on completion."
  ;; I need to convert this into an override method!
  (insert (semantic-tag-name tag))
  (let ((tt (semantic-tag-class tag)))
    (cond ((eq tt 'function)
	   (insert "("))
	  (t nil))))

;;;###autoload
(defun semantic-ia-complete-tip (point)
  "Pop up a tooltip for completion at POINT."
  (interactive "d")
  (let* ((a (semantic-analyze-current-context point))
	 (syms (semantic-ia-get-completions a point))
	 (w (get-buffer-window (current-buffer)))
         (x (mod (- (current-column) (window-hscroll))
                 (window-width)))
         (y (save-excursion
              (save-restriction
                (widen)
                (narrow-to-region (window-start) (point))
                (goto-char (point-min))
                (1+ (vertical-motion (buffer-size))))))
	 (str (mapconcat #'semantic-tag-name
			 syms
			 "\n"))
	 )
    (cond ((fboundp 'x-show-tip)
	   (x-show-tip str
		       (selected-frame)
		       nil
		       nil
		       x y)
	   )
	  (t (message str))
	  )))

;;;###autoload
(defun semantic-ia-show-summary (point)
  "Display a summary for the symbol under POINT."
  (interactive "P")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (reverse (oref ctxt prefix)))
	 (sum nil)
	)
    (while (and pf (not sum))
      (if (semantic-tag-p (car pf))
	  (setq sum (semantic-format-tag-summarize (car pf) nil t)))
      (setq pf (cdr pf)))
    (message "%s" sum)
    ))

;;;###autoload
(defun semantic-ia-show-doc (point)
  "Display the code-level documentation for the symbol at POINT."
  (interactive "P")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (reverse (oref ctxt prefix))))
    ;; If PF, the prefix is non-nil, then the last element is either
    ;; a string (incomplete type), or a semantic TAG.  If it is a TAG
    ;; then we should be able to find DOC for it.
    (cond ((stringp (car pf))
	   (message "Incomplete symbol name."))
	  ((semantic-tag-p (car pf))
	   (let ((doc (semantic-documentation-for-tag (car pf))))
	     (with-output-to-temp-buffer "*TAG DOCUMENTATION*"
	       (princ "Tag: ")
	       (princ (semantic-format-tag-prototype (car pf)))
	       (princ "\n")
	       (princ "\n")
	       (princ "Snarfed Documentation: ")
	       (princ "\n")
	       (princ "\n")
	       (if doc
		   (princ doc)
		 (princ "  Documentation unavailable."))
	       )))
	  (t
	   (message "Unknown tag.")))
    ))

(provide 'semantic-ia)

;;; semantic-ia.el ends here

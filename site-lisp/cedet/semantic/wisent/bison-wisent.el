;;; bison-wisent.el --- Translate Yacc/Bison files to wisent

;;; Copyright (C) 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: bison-wisent.el,v 1.3 2005/09/30 20:22:44 zappo Exp $

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
;; Wisent is a port of Bison to Emacs Lisp.  As such, existing
;; grammars as found in GNU tools such as GCC can be used almost in
;; tact.  This program will translate existing yacc/bison grammars
;; into a form wisent can handle.  It is then up to the programmer to
;; add the actions needed to tag a file, or do something else useful.

;;; Code:
(defalias 'yacc->wisent 'bison->wisent)
(require 'cc-mode)

;;;###autoload
(defun bison->wisent ()
  "Treat the current buffer as a YACC or BISON file, and translate to wisent.
Replaces all comments with wisent compatible comments.
Finds % commands that wisent cannot handle, and comments them out.
Deletes all actions, replacing them with small comments."
  (interactive)
  (with-syntax-table c-mode-syntax-table
    (bison->wisent-program-delete)
    (bison->wisent-action-replace)
    (bison->wisent-comment-replace)
    (bison->wisent-command-replace)
    ))

(defun bison->wisent-program-delete ()
  "Delete the C program section of a yacc/bison gramar."
  (goto-char (point-min))
  (re-search-forward "^%%" nil nil 2)
  (beginning-of-line)
  (delete-region (point) (point-max)))

(defun bison->wisent-in-c-action ()
  "Return non-nil if point is in a C style yacc/bison action."
  (condition-case nil
      (save-excursion
	(up-list -1)
	(and (looking-at "{")
	     (not (eq (preceding-char) ?'))
	     (not (eq (preceding-char) ?%))
	     ))
    (error nil)))

(defun bison->wisent-action-replace-once ()
  "Replace one C action with a amall wisent comment.
Return t on success."
  (while (and (re-search-forward "{" nil t)
	      (not (bison->wisent-in-c-action))))
  (if (not (bison->wisent-in-c-action))
     nil
    ;; Replace this action with a small comment
    (up-list -1)
    (delete-region (point) (progn
			     (forward-sexp 1)
			     (point)))
    (delete-horizontal-space)
    (if (not (looking-at "$"))
	(save-excursion
	  (newline)
	  (indent-for-tab-command)))
    (insert ";; ")
    t))

(defun bison->wisent-action-replace  ()
  "Find all yacc actions and remove, replace with small comments."
  (goto-char (point-min))
  (re-search-forward "^%%" nil t)
  (while (bison->wisent-action-replace-once)))


(defun bison->wisent-comment-replace-once ()
  "Replace one C comment with a wisent comment.
Return t on success."
 (if (not (re-search-forward "/\\*" nil t))
     nil
   (let ((s (match-beginning 0))
	 (e nil)
	 (c nil))
     (if (not (re-search-forward "\\*/" nil t))
	 (error "Unterminated comment!"))
     (setq e (match-end 0))
     (goto-char (match-beginning 0))
     (delete-char 2)
     (delete-horizontal-space)
     (setq e (point))
     (if (not (looking-at "$"))
	 (progn (insert "\n")
		(indent-for-tab-command)))
     (goto-char s)
     (delete-char 2)
     (setq c (current-column))
     (insert ";;")
     (forward-line 1)
     (beginning-of-line-text)
     (while (< (point) e)
       (if (or (looking-at "\\s-\\*")
	       (looking-at "\\*\\s-"))
	   (progn (replace-match ";;")
		  (goto-char (match-end 0))
		  )
	 (beginning-of-line-text)
	 (delete-horizontal-space)
	 (indent-to-column c)
	 (insert ";; "))
       (forward-line 1)
       (beginning-of-line))
     (goto-char e)
     t)))

(defun bison->wisent-comment-replace ()
  "Find all the C comments in this buffer and convert them to .wy comments."
  (goto-char (point-min))
  (while (bison->wisent-comment-replace-once)))

(defun bison->wisent-command-replace ()
  "Find all commands not supported by wisent, and delete them."
  (goto-char (point-min))
  (while (re-search-forward "%\\({\\|union\\|type\\)" nil t)
    (goto-char (match-beginning 0))
    (let ((txt (match-string 0))
	  (s (point))
	  (e (cond
	      ((looking-at "%{")
	       (forward-sexp 1)
	       (point))
	      ((looking-at "%union")
	       (forward-sexp 2)
	       (point))
	      (t
	       (point-at-eol))
	      )))
      (delete-region s e)
      (insert ";; Deleted a " txt " here"))
    ))

(provide 'bison-wisent)

;;; bison-wisent.el ends here

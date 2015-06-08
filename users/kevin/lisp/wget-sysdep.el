;;; wget-sysdep.el --- System dependencies between FSF Emacs 20 and 21.

;; Copyright (C) 2002, 2003, 2004 Masayuki Ataka <masayuki.ataka@gmail.com>

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;; Keywords: hypermedia, WWW

;; This file is a part of emacs-wget.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This module absorbs system dependencies of FSF Emacs ver. 20 and 21.

;;; Code:

(defun wget-window-height ()
  "Return window height"
  (if (fboundp 'window-text-height)
      (window-text-height)
    (1- (window-height))))

(defun wget-replace-regexp-in-string (regexp rep string &optional
					     fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"
  (if (fboundp 'replace-regexp-in-string)
      (replace-regexp-in-string regexp rep string fixedcase literal subexp start)
    ;; don't just call `replace-match' continually.  Walk down the
    ;; string looking for matches of REGEXP and building up a (reversed)
    ;; list MATCHES.  This comprises segments of STRING which weren't
    ;; matched interspersed with replacements for segments that were.
    ;; [For a `large' number of replacements it's more efficient to
    ;; operate in a temporary buffer; we can't tell from the function's
    ;; args whether to choose the buffer-based implementation, though it
    ;; might be reasonable to do so for long enough STRING.]
    (let ((l (length string))
	  (start (or start 0))
	  matches str mb me)
      (save-match-data
	(while (and (< start l) (string-match regexp string start))
	  (setq mb (match-beginning 0)
		me (match-end 0))
	  ;; If we matched the empty string, make sure we advance by one char
	  (when (= me mb) (setq me (min l (1+ mb))))
	  ;; Generate a replacement for the matched substring.
	  ;; Operate only on the substring to minimize string consing.
	  ;; Set up match data for the substring for replacement;
	  ;; presumably this is likely to be faster than munging the
	  ;; match data directly in Lisp.
	  (string-match regexp (setq str (substring string mb me)))
	  (setq matches
		(cons (replace-match (if (stringp rep)
					 rep
				       (funcall rep (match-string 0 str)))
				     fixedcase literal str subexp)
		      (cons (substring string start mb) ; unmatched prefix
			    matches)))
	  (setq start me))
	;; Reconstruct a string from the pieces.
	(setq matches (cons (substring string start l) matches)) ; leftover
	(apply #'concat (nreverse matches))))))

(defun wget-read-directory-name (prompt dir)
  "Read directory name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself."
  (if (fboundp 'read-directory-name)
      (read-directory-name prompt dir dir)
    (read-file-name prompt dir dir)))

(provide 'wget-sysdep)
;;; wget-sysdep.el ends here.

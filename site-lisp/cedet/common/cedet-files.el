;;; cedet-files.el --- Common routines dealing with file names.

;; Copyright (C) 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-files.el,v 1.4 2009/02/04 23:28:28 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Various useful routines for dealing with file names in the tools
;; which are a part of CEDET.

;;; Code:
(defvar cedet-dir-sep-char (if (boundp 'directory-sep-char)
			       (symbol-value 'directory-sep-char)
			     ?/)
  "Character used for directory separation.
Obsoleted in some versions of Emacs.  Needed in others.")


(defun cedet-directory-name-to-file-name (referencedir &optional testmode)
  "Convert the REFERENCEDIR (a full path name) into a filename.
Converts directory seperation characters into ! characters.
Optional argument TESTMODE is used by tests to avoid conversion
to the file's truename, and dodging platform tricks."
  (let ((file referencedir)
	dir-sep-string)
    ;; Expand to full file name
    (when (not testmode)
      (setq file (file-truename file)))
    ;; If FILE is a directory, then force it to end in /.
    (when (file-directory-p file)
      (setq file (file-name-as-directory file)))
    ;; Handle Windows Special cases
    (when (or (memq system-type '(windows-nt ms-dos)) testmode)
      ;; Replace any invalid file-name characters (for the
      ;; case of backing up remote files).
      (when (not testmode)
	(setq file (expand-file-name (convert-standard-filename file))))
      (setq dir-sep-string (char-to-string cedet-dir-sep-char))
      ;; Normalize DOSish file names: convert all slashes to
      ;; directory-sep-char, downcase the drive letter, if any,
      ;; and replace the leading "x:" with "/drive_x".
      (if (eq (aref file 1) ?:)
	  (setq file (concat dir-sep-string
			     "drive_"
			     (char-to-string (downcase (aref file 0)))
			     (if (eq (aref file 2) cedet-dir-sep-char)
				 ""
			       dir-sep-string)
			     (substring file 2)))))
    ;; Make the name unique by substituting directory
    ;; separators.  It may not really be worth bothering about
    ;; doubling `!'s in the original name...
    (setq file (subst-char-in-string
		cedet-dir-sep-char ?!
		(replace-regexp-in-string "!" "!!" file)))
    file))

(defun cedet-file-name-to-directory-name (referencefile &optional testmode)
  "Reverse the process of `cedet-directory-name-to-file-name'.
Convert REFERENCEFILE to a directory name replacing ! with /.
Optional TESTMODE is used in tests to avoid doing some platform
specific conversions during tests."
  (let ((file referencefile))
    ;; Replace the ! with /
    (setq file (subst-char-in-string ?! ?/ file))
    ;; Occurances of // meant there was once a single !.
    (setq file (replace-regexp-in-string "//" "!" file))

    ;; Handle Windows special cases
    (when (or (memq system-type '(windows-nt ms-dos)) testmode)

      ;; Handle drive letters from DOSish file names.
      (when (string-match "^/drive_\\([a-z]\\)/" file)
	(let ((driveletter (match-string 1 file))
	      )
	  (setq file (concat driveletter ":"
			     (substring file (match-end 1))))))

      ;; Handle the \\file\name nomenclature on some windows boxes.
      (when (string-match "^!" file)
	(setq file (concat "//" (substring file 1))))
      )
    
    file))

;;; Tests
;;
(defvar cedet-files-utest-list
  '(
    ( "/home/me/src/myproj/src/foo.c" . "!home!me!src!myproj!src!foo.c" )
    ( "c:/work/myproj/foo.el" . "!drive_c!work!myproj!foo.el" )
    ( "//windows/proj/foo.java" . "!!windows!proj!foo.java" )
    ( "/home/me/proj!bang/foo.c" . "!home!me!proj!!bang!foo.c" )
    )
  "List of different file names to test.
Each entry is a cons cell of ( FNAME . CONVERTED )
where FNAME is some file name, and CONVERTED is what it should be
converted into.")

;;;###autoload
(defun cedet-files-utest ()
  "Test out some file name conversions."
  (interactive)

  (let ((idx 0))
    (dolist (FT cedet-files-utest-list)

      (setq idx (+ idx 1))

      (let ((dir->file (cedet-directory-name-to-file-name (car FT) t))
	    (file->dir (cedet-file-name-to-directory-name (cdr FT) t))
	    )

	(unless (string= (cdr FT) dir->file)
	  (error "Failed: %d.  Found: %S Wanted: %S"
		 idx dir->file (cdr FT))
	  )

	(unless (string= file->dir (car FT))
	  (error "Failed: %d.  Found: %S Wanted: %S"
		 idx file->dir (car FT))
	  )

	))))


;;; Compatibility
;;
;; replace-regexp-in-string is in subr.el in Emacs 21.  Provide
;; here for compatibility.

(when (not (fboundp 'replace-regexp-in-string))

(defun replace-regexp-in-string (regexp rep string &optional
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
    => \" bar foo\""

  ;; To avoid excessive consing from multiple matches in long strings,
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
      (apply #'concat (nreverse matches)))))

)

(provide 'cedet-files)

;;; cedet-files.el ends here

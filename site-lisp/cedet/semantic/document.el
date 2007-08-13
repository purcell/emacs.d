;;; document.el --- Use the semantic parser to generate documentation.

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: doc
;; X-RCS: $Id: document.el,v 1.33 2007/03/17 21:21:01 zappo Exp $

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
;; Document provides the ability to create a documentation framework
;; for functions and variables.  It uses `semantic' to find the
;; current function, and to provide additional context information for
;; generating the documentation.
;;
;;   Document then provides some rules for creating English Text based
;; on the name of a given function, it's return type, or variable
;; type.  It also has output rules for texinfo, or comments.
;;
;; NOTE: Some of the user level commands in document.el dealing with
;; texinfo files have been obsoleted commands in semantic-texi, which
;; can not insert foriegn tokens.

(require 'sformat)
;; This contains most variable settings for auto-comment generation.
(require 'document-vars)

;; XEmacs change: needed to define macros at compile time.
(condition-case nil
    (require 'ede)
  (error nil))
(require 'semantic)
(require 'semantic-util)

;;; Code:

;;; User customizations
;;
(defgroup document nil
  "File and tag browser frame."
  :group 'semantic
  :group 'texinfo
  )

(defcustom document-my-initials (user-login-name)
  "*The initials to use when commenting something."
  :group 'document
  :type 'string)

(defcustom document-copyright-holder (user-full-name)
  "*The string to use as the copyright holder in the file comment."
  :group 'document
  :type 'string)

(defvar document-runflags nil
  "Flags collected while updating a comment.
This is used to create a history element.")

;;; Status tracking
;;
(defvar document-current-output-file nil
  "A marker for the current output file used for documentation.")

;;; User Functions
;;
(defun document (&optional resetfile)
  "Document in a texinfo file the function or variable the cursor is in.
Optional argument RESETFILE is provided w/ universal argument.
When non-nil, query for a new documentation file.
To document a function in a source file, use `document-inline'."
  (interactive (if current-prefix-arg
		   (save-excursion
		     (list (document-locate-file
			    (current-buffer) t)))))
  ;; First, garner some information from Semantic.
  (semantic-fetch-tags)
  (let ((cdi (semantic-current-tag))
	(cdib (current-buffer)))
    ;; Make sure we have a file.
    (document-locate-file (current-buffer))
    ;; Now go and create the documentation
    (if (not document-current-output-file)
	(error "No file found for your documentation"))
    (set-buffer (marker-buffer document-current-output-file))
    (goto-char document-current-output-file)
    (insert "\n")
    (document-insert-texinfo cdi cdib)
    (insert "\n")
    (setq document-current-output-file (point-marker))
    ))

(defun document-inline ()
  "Document the current function with an inline comment."
  (interactive)
  (semantic-fetch-tags)
  (let ((cf (semantic-current-tag)))
    (document-insert-defun-comment cf (current-buffer))))

;;; Documentation insertion functions
;;
(defun document-insert-texinfo (tag buffer)
  "Insert texinfo documentation about TAG from BUFFER."
  (let ((tt (semantic-tag-class tag)))
    (insert "@"
	    (cond ((eq tt 'variable)
		   (if (semantic-tag-get-attribute tag :user-visible-flag)
		       "deffn Option"
		     "defvar"))
		  ((eq tt 'function)
		   (if (semantic-tag-get-attribute tag :user-visible-flag)
		       "deffn Command"
		     "defun"))
		  ((eq tt 'type)
		   "deffn Type")
		  (t (error "Don't know how to document that")))
	    " "
	    (semantic-tag-name tag))
    (if (eq tt 'function)
	(let ((args (semantic-tag-function-arguments tag)))
	  (while args
	    (insert " ")
	    (if (stringp (car args))
		(insert (car args))
	      (insert (semantic-tag-name (car args))))
	    (setq args (cdr args)))))
    (insert "\n@anchor{" (semantic-tag-name tag) "}\n")
    (insert (document-massage-to-texinfo
	     tag
	     buffer
	     (document-generate-documentation tag buffer)))
    (insert "\n@end "
	    (cond ((eq tt 'variable)
		   (if (semantic-tag-get-attribute tag :user-visible-flag)
		       "deffn"
		     "defvar"))
		  ((eq tt 'function)
		   (if (semantic-tag-get-attribute tag :user-visible-flag)
		       "deffn"
		     "defun"))
		  ((eq tt 'type)
		   "deffn"))
	    )))

(defun document-insert-defun-comment (tag buffer)
  "Insert mode-comment documentation about TAG from BUFFER."
  (interactive)
  (let ((document-runflags nil)
	(tt (semantic-tag-class tag)))
    (cond
     ((eq tt 'function)
      (if (semantic-documentation-for-tag tag t)
	  (document-update-comment tag)
	(document-insert-function-comment-new tag))
      (message "Done..."))
     (t
      (error "Type %S is not yet managed by document `document-inline'" tt)))))

(defun document-update-comment (tag)
  "Update an existing comment for TAG."
  (let ((comment (semantic-documentation-for-tag tag 'lex)))
    (save-excursion
      (document-update-paramlist tag comment))
    (semantic-fetch-tags)
    (let ((ct (semantic-brute-find-tag-by-position
	       (point) (current-buffer))))
      (setq comment (semantic-documentation-for-tag tag 'lex))
      (document-update-history comment (document-get-history-elt "")))))

(defun document-insert-new-file-header (header)
  "Insert a new header file into this buffer.  Add reference to HEADER.
Used by `prototype' if this file doesn't have an introductory comment."
  (interactive)
  (goto-char 0)
  (let ((pnt nil))
    (insert (document-new-file-header header))
    (if pnt
	(goto-char pnt))))

;;; Top level comment things.
;;
(defun document-new-file-header (&optional header)
  "Return a comment string customized for the current buffer.
Optional HEADER is the header file to use under Token."
  (Sformat (list (list ?B '(lambda () (document-file-brief-comment)))
		 (list ?D
		       (if (boundp 'pnt)
			   '(lambda () (setq pnt (Sformat-point)) "")
			 ""))
		 (list ?N '(lambda ()
			     (document-copyright-notice)))
		 (list ?O document-copyright-holder)
		 (list ?Y (document-get-date-time-string "%Y"))
		 (list ?T '(lambda ()
			     (concat
			      cpr-header-token
			      " "
			      (if header header
				(semantic-prototype-file (current-buffer))))))
		 (list ?H (document-get-history-elt "Created"))
		 (list ?b (document-comment-start))
		 (list ?m (document-comment-line-prefix))
		 (list ?e (document-comment-end)))
	   ;; This is lame!  Fix it sometime soon.
	   (if (string-match "\\.c$" (buffer-file-name))
	       document-file-comment
	     document-header-comment)))

(defun document-set-copyright-file (f)
  "Interactively find the file name with the copyright blurb.
Argument F is the file to use."
  (interactive "FCopyright Notice File (RET for none): ")
  (if (string= f (buffer-file-name))
      (setq document-copyright-notice-file "")
    (setq document-copyright-notice-file f)))

(defun document-copyright-notice ()
  "Create, or find a copyright notice.
Adds the comment line PREFIX to each line."
  (if (not document-copyright-notice-file)
      (call-interactively 'document-set-copyright-file))
  (if (= (length document-copyright-notice-file) 0)
      "??Public Domain Software??"
    (let* ((b (get-buffer-create "DOCUMENT TEMP"))
	   (s nil)
	   (plen (Sformat-column))
	   (pstr (substring (concat (document-comment-line-prefix)
				    "         ")
			    0 plen)))
      (setq s
	    (save-excursion
	      (set-buffer b)
	      (insert-file-contents document-copyright-notice-file)
	      ;; Now put comment marks all over.
	      (goto-char 0)
	      (forward-line 1)
	      (end-of-line)
	      (while (not (eobp))
		(beginning-of-line)
		(insert pstr)
		(end-of-line)
		(forward-char 1)
		(end-of-line))
	      (forward-char -1)
	      (if (equal (following-char) ?\n)
		  (delete-char 1))
	      (set-buffer-modified-p nil)
	      (buffer-string)))
      (kill-buffer b)
      s)))

(defun document-file-brief-comment ()
  "Make a brief comment about the file we are currently editing."
  (Sformat (list (list ?F (file-name-nondirectory (buffer-file-name)))
		 (list ?C '(lambda ()
			    (read-string "Brief Description of file: "))))
	   document-file-brief-comment))

;;; Documentatation generation functions
;;
(defun document-generate-documentation (tag buffer)
  "Return a plain string documenting TAG from BUFFER."
  (save-excursion
    (set-buffer buffer)
    (let ((doc ;; Second, does this thing have docs in the source buffer which
	   ;; an override method might be able to find?
	   (semantic-documentation-for-tag tag)
	   ))
      (if (not doc)
	  (document-generate-new-documentation tag buffer)
	;; Ok, now lets see what sort of formatting there might be,
	;; and see about removing some of it.. (Tables of arguments,
	;; and that sort of thing.)
	nil
	;; Return the string.
	doc))))

(defun document-generate-new-documentation (tag buffer)
  "Look at elements of TAG in BUFFER to make documentation.
This will create a new documentation string from scratch."
  ;; We probably want more than this, but for now it's close.
  (document-function-name-comment tag))

;;; Inline comment mangling.
;;
(defun document-insert-function-comment-new (tag)
  "Insert a new comment which explains the function found in TAG."
  (let ((hist (document-get-history-elt ""))
	(pnt 0)
	(upnt 0)
	(st 0)
	(zpnt 0)
	(fname (semantic-tag-name tag))
	(returns (semantic-tag-type tag))
	(params (semantic-tag-function-arguments tag))
	)
    (if (listp returns)
	;; convert a type list into a long string to analyze.
	(setq returns (car returns)))
    ;; tag should always be correct.
    (goto-char (semantic-tag-start tag))
    (setq st (point))
    (insert (Sformat (list (list ?F fname)
			   (list ?f '(lambda () (setq zpnt (Sformat-point)) ""))
			   (list ?p '(lambda () (setq pnt (Sformat-point)) ""))
			   (list ?D (document-function-name-comment tag))
			   (list ?R (document-insert-return returns))
			   (list ?P '(lambda ()
				       (document-insert-parameters params)))
			   (list ?H (concat hist document-new-hist-comment))
			   (list ?b (document-comment-start))
			   (list ?m (document-comment-line-prefix))
			   (list ?e (document-comment-end)))
		     document-function-comment))
    (goto-char (+ zpnt st))
    (message "Setting fill prefix to: \"%s\""
	     (setq fill-prefix
		   (concat (document-comment-line-prefix)
			   (make-string
			    (- (current-column)
			       (length (document-comment-line-prefix)))
			    ? ))))
    (goto-char (+ pnt st))
    (auto-fill-mode 1)
    )
  )

(defun document-function-name-comment (tag)
  "Create documentation for the function defined in TAG.
If we can identify a verb in the list followed by some
name part then check the return value to see if we can use that to
finish off the sentence.  ie. any function with 'alloc' in it will be
allocating something based on its type."
  (let ((al document-autocomment-return-first-alist)
	(dropit nil)
	(tailit nil)
	(news "")
	(fname (semantic-tag-name tag))
	(retval (or (semantic-tag-type tag) "")))
    (if (listp retval)
	;; convert a type list into a long string to analyze.
	(setq retval (car retval)))
    ;; check for modifiers like static
    (while al
      (if (string-match (car (car al)) (downcase retval))
	  (progn
	    (setq news (concat news (cdr (car al))))
	    (setq dropit t)
	    (setq al nil)))
      (setq al (cdr al)))
    ;; check for verb parts!
    (setq al document-autocomment-function-alist)
    (while al
      (if (string-match (car (car al)) (downcase fname))
	  (progn
	    (setq news
		  (concat news (if dropit (downcase (cdr (car al)))
				 (cdr (car al)))))
	    ;; if we end in a space, then we are expecting a potential
	    ;; return value.
	    (if (= ?  (aref news (1- (length news))))
		(setq tailit t))
	    (setq al nil)))
      (setq al (cdr al)))
    ;; check for noun parts!
    (setq al document-autocomment-common-nouns-abbrevs)
    (while al
      (if (string-match (car (car al)) (downcase fname))
	  (progn
	    (setq news
		  (concat news (if dropit (downcase (cdr (car al)))
				 (cdr (car al)))))
	    (setq al nil)))
      (setq al (cdr al)))
    ;; add tailers to names which are obviously returning something.
    (if tailit
	(progn
	  (setq al document-autocomment-return-last-alist)
	  (while al
	    (if (string-match (car (car al)) (downcase retval))
		(progn
		  (setq news
			(concat news " "
				;; this one may use parts of the return value.
				(format (cdr (car al))
					(document-programmer->english
					 (substring retval (match-beginning 1)
						    (match-end 1))))))
		  (setq al nil)))
	    (setq al (cdr al)))))
    news))

(defun document-insert-return (returnval)
  "Take the return value, and return a string which is ready to be commented.
Argument RETURNVAL is the string representing the type to be returned."
  (if (not returnval)
      ""
    (if (string-match "^\\(static +\\|STATIC +\\)?\\(void\\|VOID\\)" returnval)
	"Nothing"
      (if (= (length returnval) 0)
	  "int - "
	(concat returnval " - ")))))
  
(defun document-insert-parameters (params &optional commentlist)
  "Convert a parameter list PARAMS into a vertical list separated by -es.
Optional COMMENTLIST is a list of previously known parts with comments."

  (let* ((col (if Sformat-formatting (Sformat-column) (current-column)))
	 (newl params)
	 ;; returns is local to the caller
	 (longest (document-longest-name newl))
	 (numdfs 0)
	 (newp ""))
    (while newl
      (let* ((n (car newl))
	     (nn (if (stringp n) n (semantic-tag-name n)))
	     (al (if (stringp n) nil (semantic-tag-modifiers n)))
	     (nt (if (stringp n) "" (semantic-tag-type n))))
	(if (listp nt)
	    ;; make sure this is a string.
	    (setq nt (car nt)))
	(setq numdfs (1+ numdfs))
	(let ((nextp (Sformat
		      (list (list ?P
				  (substring (concat
					      nn
					      "                   ")
					     0 longest))
			    (list ?p n)
			    (list ?R nt)
			    (list ?D (document-parameter-comment
				      n
				      commentlist)))
		      document-param-element)))
	  (setq newp
		(concat
		 newp nextp
		 ;; the following always assumes that there is
		 ;; always a comment starting with SPC * on
		 ;; every line.  Mabee fix, but this is what I
		 ;; use, so tough noogies as of right now.
		 (concat "\n" (document-comment-line-prefix)
			 (make-string
			  (- col (length (document-comment-line-prefix)))
			  ? ))))))
      (setq newl (cdr newl)))
    (if (= (length newp) 0) (setq newp "None"))
    (if (and document-extra-line-after-short-parameters (<= numdfs 1))
	(setq newp (concat newp "\n *")))
    newp)
  )

(defun document-parameter-comment (param &optional commentlist)
  "Convert tag or string PARAM into a name,comment pair.
Optional COMMENTLIST is list of previously existing comments to
use instead in alist form.  If the name doesn't appear in the list of
standard names, then englishify it instead."
  (let ((cmt "")
	(aso document-autocomment-param-alist)
	(fnd nil)
	(name (if (stringp param) param (semantic-tag-name param)))
	(tt (if (stringp param) nil (semantic-tag-type param))))
    ;; Make sure the type is a string.
    (if (listp tt)
	(setq tt (semantic-tag-name tt)))
    ;; Find name description parts.
    (while aso
      (if (string-match (car (car aso)) name)
	  (progn
	    (setq fnd t)
	    (setq cmt (concat cmt (cdr (car aso))))))
      (setq aso (cdr aso)))
    (if (/= (length cmt) 0)
	nil
      ;; finally check for array parts
      (if (and (not (stringp param)) (semantic-tag-modifiers param))
	  (setq cmt (concat cmt "array of ")))
      (setq aso document-autocomment-param-type-alist)
      (while (and aso tt)
	(if (string-match (car (car aso)) tt)
	    (setq cmt (concat cmt (cdr (car aso)))))
	(setq aso (cdr aso))))
    ;; Convert from programmer to english.
    (if (not fnd)
	(setq cmt (concat cmt " "
			  (document-programmer->english name))))
    cmt))


(defun document-get-history-elt (changes)
  "Return the history element with the change elt set to CHANGES."
  (Sformat (list '(?U document-my-initials)
		 (list ?D (document-get-date))
		 '(?S document-change-number)
		 '(?C changes))
	   document-history-element))

(defun document-get-date-time-string (form)
  "Return a string matching the format of `document-date-element'.
Argument FORM is the format string to use."
  (let* ((date (current-time-string))
         (garbage
          (string-match
           (concat "^\\([A-Z][a-z]*\\) *\\([A-Z][a-z]*\\) *\\([0-9]*\\)"
	   " \\([0-9]*\\):\\([0-9]*\\):\\([0-9]*\\)"
	   " \\([0-9]*\\)$")
           date))
	 (wkdy (substring date (match-beginning 1) (match-end 1)))
	 (hour (string-to-number
		(substring date (match-beginning 4) (match-end 4))))
	 (min (substring date (match-beginning 5) (match-end 5)))
	 (sec (substring date (match-beginning 6) (match-end 6)))
         (month
	  (cdr (assoc (substring date (match-beginning 2) (match-end 2))
		      '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
			("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
			("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))))
	 (ms (substring date (match-beginning 2) (match-end 2)))
	 (day (substring date (match-beginning 3) (match-end 3)))
         (year (substring date (match-beginning 7) (match-end 7))))
    (Sformat (list (list ?H (% hour 12))
		   (list ?h hour)
		   (list ?a (if (> hour 12) "pm" "am"))
		   (list ?I min)
		   (list ?S sec)
		   (list ?D day)
		   (list ?M month)
		   (list ?m ms)
		   (list ?Y year)
		   (list ?y (substring year 2))
		   (list ?w wkdy))
	     form)))

(defun document-get-date ()
  "Return a string which is the current date."
  (document-get-date-time-string document-date-element))

;;; Updating utilities
;;
(defun document-update-history (comment history)
  "Update COMMENT with the text HISTORY.
COMMENT is a `semantic-lex' token."
  (let ((endpos 0))
    (save-excursion
      (goto-char (semantic-lex-token-end comment))
      (if (not (re-search-backward (regexp-quote (document-comment-start))
				   (semantic-lex-token-start comment) t))
	  (error "Comment confuses me"))
      (let ((s (document-just-after-token-regexp ?H document-function-comment)))
	(if (not s) (error "Can't find where to enter new history element"))
	(if (re-search-forward (concat "\\(" s "\\)")
			       (1+ (semantic-lex-token-end comment)) t)
	    (progn
	      (goto-char (match-beginning 1))
	      (insert (concat "\n" (document-comment-line-prefix) " "))
	      (insert history)))
	(setq endpos (point))))
    (goto-char endpos)
    (while document-runflags
      (let ((p (assoc (car (car document-runflags))
		      document-autocomment-modify-alist)))
	(if p (insert (format (cdr p) (cdr (car document-runflags))))))
      (setq document-runflags (cdr document-runflags)))))

(defun document-argument-name (arg)
  "Return a string representing the name of ARG.
Arguments can be semantic tokens, or strings."
  (cond ((semantic-tag-p arg)
	 (semantic-tag-name arg))
	((stringp arg)
	 arg)
	(t (format "%s" arg))))

(defun document-update-paramlist (tag comment)
  "Update TAG's comment found in the `semantic-lex' token COMMENT."
  (let ((endpos 0) st en (il nil)
	(case-fold-search nil)
	(l (semantic-tag-function-arguments tag)))
    (save-excursion
      (goto-char (semantic-lex-token-start comment))
      (let ((s (document-just-after-token-regexp ?P document-function-comment))
	    (s2 (document-just-before-token-regexp ?P document-function-comment)))
	(if (or (not s) (not s2))
	    (error "Cannot break format string into findable begin and end tokens"))
	(if (not (re-search-forward (concat "\\(" s "\\)")
				    (1+ (semantic-lex-token-end comment)) t))
	    (error "Comment is not formatted correctly for param check"))
	(goto-char (match-beginning 1))
	(setq en (point))
	(goto-char (semantic-lex-token-start comment))
	(if (not (re-search-forward s2 (semantic-lex-token-end comment) t))
	    (error "Comment is not formatted correctly for param check"))
	(setq st (point))
	;; At this point we have the beginning and end of the
	;; parameter list in the comment.  Now lets search through
	;; it and generate a list (name . commentpart) so we can
	;; re-build it if it doesn't match L
	(while (re-search-forward
		(concat "\\(\\(\\sw\\|\\s_+\\)+\\)\\s-*-[ \t]*")
		en t)
	  (let ((n (buffer-substring (match-beginning 1) (match-end 1)))
		(c nil))
	    (setq c (point))
	    (re-search-forward "$" (semantic-lex-token-end comment) t)
	    (setq c (buffer-substring c (point)))
	    (setq il (cons (cons n c) il))))
	;; run verify on two lists of parameters to make sure they
	;; are the same.
	(let ((tl l) (stop nil))
	  (while (and tl (not stop))
	    (if (not (assoc (document-argument-name (car tl)) il))
		(setq stop t))
	    (setq tl (cdr tl)))
	  (if (not stop)
	      (setq il nil)))
	;; check if we want to modify the parameter list.
	(if (not (and il (y-or-n-p "Parameter list changed.  Fix? ")))
	    (message "Not fixing.")
	  ;; delete what was there, and insert the new stuff.
	  (let ((ntl l)
		(cs1 nil)
		(num 0))
	    (while ntl
	      (if (not (assoc (document-argument-name (car ntl)) il))
		  (progn
		    (setq num (1+ num))
		    (setq cs1 (concat cs1 (if cs1 ", ")
				      (document-argument-name (car ntl))))))
	      (setq ntl (cdr ntl)))
	    (if cs1
		(if (= num 1)
		    (setq cs1 (concat "Added parameter " cs1))
		  (setq cs1 (concat "Added parameters " cs1)))
	      (setq cs1 "Removed parameters."))
	    (setq document-runflags (cons (cons 'document-newparam cs1)
					  document-runflags)))
	  (let ((dif (- en st))
		(newc nil))
	    (delete-region st en)
	    (setq newc (document-insert-parameters l il))
	    (setq dif (- (length newc) dif))
	    (insert newc)))))
    (goto-char endpos)))

;;; Conversion utilities
;;
(defun document-longest-name (list)
  "Go through LIST, and return the length of the longest name."
  (let ((longest 1)
	(nn nil))
    (while list
      (setq nn (if (stringp (car list)) (car list)
		 (semantic-tag-name (car list))))
      (if (< longest (length nn))
	  (setq longest (length nn)))
      (setq list (cdr list)))
    longest))

(defun document-programmer->english (programmer)
  "Takes PROGRAMMER and converts it into English.
Works with the following rules:
  1) convert all _ into spaces.
  2) inserts spaces in front of all lowerUpper case combos
  3) expands noun names based on common programmer nouns.
  
  This function is designed for variables, not functions.  This does
not account for verb parts."
  (let ((ind 0)				;index in string
	(llow nil)			;lower/upper case flag
	(wlist nil)			;list of words after breaking
	(newstr nil)			;new string being generated
	(al nil))			;autocomment list
    ;;
    ;; 1) Convert underscores
    ;;
    (while (< ind (length programmer))
      (setq newstr (concat newstr
			   (if (= (aref programmer ind) ?_)
			       " " (char-to-string (aref programmer ind)))))
      (setq ind (1+ ind)))
    (setq programmer newstr
	  newstr nil
	  ind 0)
    ;;
    ;; 2) Find word brakes between case changes
    ;;
    (while (< ind (length programmer))
      (setq newstr
	    (concat newstr
		    (let ((tc (aref programmer ind)))
		      (if (and (>= tc ?a) (<= tc ?z))
			  (progn
			    (setq llow t)
			    (char-to-string tc))
			(if llow
			    (progn
			      (setq llow nil)
			      (concat " " (char-to-string tc)))
			  (char-to-string tc))))))
      (setq ind (1+ ind)))
    ;;
    ;; 3) Expand the words if possible
    ;;
    (setq llow nil
	  ind 0
	  programmer newstr
	  newstr nil)
    (while (string-match (concat "^\\s-*\\([^ \t\n]+\\)") programmer)
      (let ((ts (substring programmer (match-beginning 1) (match-end 1)))
	    (end (match-end 1)))
	(setq al document-autocomment-common-nouns-abbrevs)
	(setq llow nil)
	(while al
	  (if (string-match (car (car al)) (downcase ts))
	      (progn
		(setq newstr (concat newstr (cdr (car al))))
		;; don't terminate because we may actuall have 2 words
		;; next to eachother we didn't identify before
		(setq llow t)))
	  (setq al (cdr al)))
	(if (not llow) (setq newstr (concat newstr ts)))
	(setq newstr (concat newstr " "))
	(setq programmer (substring programmer end))))
    newstr))


;;; Sformat string managers
;;
;; These two routines find the string between different % tokens, and
;; returns them as regular expressions vie regexp-quote.  The result
;; will allow a program to find text surrounding major parts within a
;; comment, thus, the parts in a comment that need to be changed.

(defun document-just-before-token-regexp (token format)
  "Return a search expression for text before TOKEN in FORMAT.
This search string can be used to find the text residing in TOKEN
if it were inserted with FORMAT in the past."
  (setq format (document-format-for-native-comments format))
  (sformat-just-before-token-regexp token format))

(defun document-just-after-token-regexp (token format)
  "Return a search expression for text after TOKEN in FORMAT.
This search string can be used to find the text residing in TOKEN
if it were inserted with FORMAT in the past."
  (setq format (document-format-for-native-comments format))
  (sformat-just-after-token-regexp token format))

;; This function adds in the comment thingies so that the above
;; functions can work some magic.
(defun document-format-for-native-comments (formatstr)
  "Return FORMATSTR with the comment formatters filled in.
Leaves other formatting elements the way they are."
  (Sformat (list (list ?b (document-comment-start))
		 (list ?m (document-comment-line-prefix))
		 (list ?e (document-comment-end)))
	   formatstr))


;;; Texinfo mangling.
;;
(defun document-massage-to-texinfo (tag buffer string)
  "Massage TAG's documentation from BUFFER as STRING.
This is to take advantage of TeXinfo's markup symbols."
  (let ((mode (with-current-buffer buffer (semantic-tag-mode tag))))
    (when (eq mode 'emacs-lisp-mode)
      ;; Elisp has a few advantages.  Hack it in.
      (setq string (document-texify-elisp-docstring string)))
    ;; Else, other languages are simpler.  Also, might as well
    ;; run the elisp version through also.
    (let ((case-fold-search nil)
          (start 0))
      (while (string-match
              "\\(^\\|[^{]\\)\\<\\([A-Z0-9_-]+\\)\\>\\($\\|[^}]\\)"
              string start)
	(let ((ms (match-string 2 string)))
	  (when (eq mode 'emacs-lisp-mode)
	    (setq ms (downcase ms)))
	
	  (when (not (or (string= ms "A")
			 (string= ms "a")
			 ))
	    (setq string (concat (substring string 0 (match-beginning 2))
				 "@var{"
				 ms
				 "}"
				 (substring string (match-end 2))))))
	(setq start (match-end 2)))
      )
    string))

;; This FN was taken from EIEIO and modified.  Maybe convert later.
(defun document-texify-elisp-docstring (string)
  "Take STRING, (a normal doc string), and convert it into a texinfo string.
For instances where CLASS is the class being referenced, do not Xref
that class.

 `function' => @dfn{function}
 `variable' => @code{variable}
 `class'    => @code{class} @xref{class}
 `unknown'  => @code{unknonwn}
 \"text\"     => ``text''
 'quoteme   => @code{quoteme}
 non-nil    => non-@code{nil}
 t          => @code{t}
 :tag       => @code{:tag}
 [ stuff ]  => @code{[ stuff ]}
 Key        => @kbd{Key}     (key is C\\-h, M\\-h, SPC, RET, TAB and the like)
 ...        => @dots{}"
  (while (string-match "`\\([-a-zA-Z0-9<>.]+\\)'" string)
    (let* ((vs (substring string (match-beginning 1) (match-end 1)))
	   (v (intern-soft vs)))
      (setq string
	    (concat
	     (replace-match (concat
			     (if (fboundp v)
				 "@dfn{" "@code{")
			     vs "}")
		    nil t string)))))
  (while (string-match "\\( \\|^\\)\\(nil\\|t\\|'[-a-zA-Z0-9]+\\|:[-a-zA-Z0-9]+\\)\\([. ,]\\|$\\)" string)
    (setq string (replace-match "@code{\\2}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\(non-\\)\\(nil\\)\\)\\([. ,]\\|$\\)" string)
    (setq string (replace-match "\\3@code{\\4}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\[[^]]+\\]\\)\\( \\|$\\)" string)
    (setq string (replace-match "@code{\\2}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\(\\(C-\\|M-\\|S-\\)+\\([^ \t\n]\\|RET\\|SPC\\|TAB\\)\\)\\|\\(RET\\|SPC\\|TAB\\)\\)\\( \\|\\s.\\|$\\)" string)
    (setq string (replace-match "@kbd{\\2}" t nil string 2)))
  (while (string-match "\"\\(.+\\)\"" string)
    (setq string (replace-match "``\\1''" t nil string 0)))
  (while (string-match "\\.\\.\\." string)
    (setq string (replace-match "@dots{}" t nil string 0)))
  string)

;;; Buffer finding and managing
;;
(defun document-find-file (file)
  "Load up the document file FILE.
Make it current, and return a marker for the location of newly inserted
documentation."
  (set-buffer (find-file-noselect file))
  ;; Theoretically, we should add some smarts here for positioning
  ;; the cursor.  For now, do some simple stuff.
  (if (eq (point) (point-min))
      (progn
	(switch-to-buffer (current-buffer))
	(error "Position cursor in %s, and try inserting documentation again"
	       file))
    (point-marker)))

(defun document-locate-file (buffer &optional override)
  "Return a file in which documentation belonging to BUFFER should be placed.
Optional argument OVERRIDE indicates to override the last used location."
  (if (and document-current-output-file (not override))
      document-current-output-file
    ;; Else, perform some default behaviors
    (let ((files (if (and (fboundp 'ede-documentation-files) ede-minor-mode)
		     (save-excursion
		       (set-buffer buffer)
		       (ede-documentation-files))
		   ))
	  (choice nil))
      (while files
	(setq choice (cons (list (car files)) choice)
	      files (cdr files)))
      (setq choice
	    (if choice
		(completing-read "Documentation File: "
				 choice
				 nil t (car choice))
	      (read-file-name "Documentation File: "
			      default-directory)))
      (setq document-current-output-file (document-find-file choice)))))
      

(provide 'document)

;;; document.el ends here

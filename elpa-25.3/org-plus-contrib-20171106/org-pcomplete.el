;;; org-pcomplete.el --- In-buffer Completion Code -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;         John Wiegley <johnw at gnu dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;; Require other packages

(require 'org-macs)
(require 'org-compat)
(require 'pcomplete)

(declare-function org-make-org-heading-search-string "org" (&optional string))
(declare-function org-get-buffer-tags "org" ())
(declare-function org-get-tags "org" ())
(declare-function org-buffer-property-keys "org"
		  (&optional specials defaults columns ignore-malformed))
(declare-function org-entry-properties "org" (&optional pom which))
(declare-function org-tag-alist-to-string "org" (alist &optional skip-key))

;;;; Customization variables

(defgroup org-complete nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'org)

(defvar org-drawer-regexp)
(defvar org-property-re)
(defvar org-current-tag-alist)

(defun org-thing-at-point ()
  "Examine the thing at point and let the caller know what it is.
The return value is a string naming the thing at point."
  (let ((beg1 (save-excursion
		(skip-chars-backward "[:alnum:]-_@")
		(point)))
	(beg (save-excursion
	       (skip-chars-backward "a-zA-Z0-9-_:$")
	       (point)))
	(line-to-here (buffer-substring (point-at-bol) (point))))
    (cond
     ((string-match "\\`[ \t]*#\\+begin: clocktable[ \t]+" line-to-here)
      (cons "block-option" "clocktable"))
     ((string-match "\\`[ \t]*#\\+begin_src[ \t]+" line-to-here)
      (cons "block-option" "src"))
     ((save-excursion
	(re-search-backward "^[ \t]*#\\+\\([A-Z_]+\\):.*"
			    (line-beginning-position) t))
      (cons "file-option" (match-string-no-properties 1)))
     ((string-match "\\`[ \t]*#\\+[a-zA-Z_]*\\'" line-to-here)
      (cons "file-option" nil))
     ((equal (char-before beg) ?\[)
      (cons "link" nil))
     ((equal (char-before beg) ?\\)
      (cons "tex" nil))
     ((string-match "\\`\\*+[ \t]+\\'"
		    (buffer-substring (point-at-bol) beg))
      (cons "todo" nil))
     ((equal (char-before beg) ?*)
      (cons "searchhead" nil))
     ((and (equal (char-before beg1) ?:)
	   (equal (char-after (point-at-bol)) ?*))
      (cons "tag" nil))
     ((and (equal (char-before beg1) ?:)
	   (not (equal (char-after (point-at-bol)) ?*))
	   (save-excursion
	     (move-beginning-of-line 1)
	     (skip-chars-backward "[ \t\n]")
	     ;; org-drawer-regexp matches a whole line but while
	     ;; looking-back, we just ignore trailing whitespaces
	     (or (looking-back (substring org-drawer-regexp 0 -1)
			       (line-beginning-position))
		 (looking-back org-property-re
			       (line-beginning-position)))))
      (cons "prop" nil))
     ((and (equal (char-before beg1) ?:)
	   (not (equal (char-after (point-at-bol)) ?*)))
      (cons "drawer" nil))
     (t nil))))

(defun org-command-at-point ()
  "Return the qualified name of the Org completion entity at point.
When completing for #+STARTUP, for example, this function returns
\"file-option/startup\"."
  (let ((thing (org-thing-at-point)))
    (cond
     ((string= "file-option" (car thing))
      (concat (car thing)
	      (and (cdr thing) (concat "/" (downcase (cdr thing))))))
     ((string= "block-option" (car thing))
      (concat (car thing) "/" (downcase (cdr thing))))
     (t (car thing)))))

(defun org-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let ((begin (line-beginning-position))
	(end (line-end-position))
	begins args)
    (save-restriction
      (narrow-to-region begin end)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward " \t\n[")
	  (setq begins (cons (point) begins))
	  (skip-chars-forward "^ \t\n[")
	  (setq args (cons (buffer-substring-no-properties
			    (car begins) (point))
			   args)))
	(cons (reverse args) (reverse begins))))))

(defun org-pcomplete-initial ()
  "Calls the right completion function for first argument completions."
  (ignore
   (funcall (or (pcomplete-find-completion-function
		 (car (org-thing-at-point)))
		pcomplete-default-completion-function))))

(defvar org-options-keywords)		 ; From org.el
(defvar org-element-affiliated-keywords) ; From org-element.el
(declare-function org-get-export-keywords "org" ())
(defun pcomplete/org-mode/file-option ()
  "Complete against all valid file options."
  (require 'org-element)
  (pcomplete-here
   (org-pcomplete-case-double
    (append (mapcar (lambda (keyword) (concat keyword " "))
		    org-options-keywords)
	    (mapcar (lambda (keyword) (concat keyword ": "))
		    org-element-affiliated-keywords)
	    (let (block-names)
	      (dolist (name
		       '("CENTER" "COMMENT" "EXAMPLE" "EXPORT" "QUOTE" "SRC"
			 "VERSE")
		       block-names)
		(push (format "END_%s" name) block-names)
		(push (concat "BEGIN_"
			      name
			      ;; Since language is compulsory in
			      ;; export blocks source blocks, add
			      ;; a space.
			      (and (member name '("EXPORT" "SRC")) " "))
		      block-names)
		(push (format "ATTR_%s: " name) block-names)))
	    (mapcar (lambda (keyword) (concat keyword ": "))
		    (org-get-export-keywords))))
   (substring pcomplete-stub 2)))

(defun pcomplete/org-mode/file-option/author ()
  "Complete arguments for the #+AUTHOR file option."
  (pcomplete-here (list user-full-name)))

(defvar org-time-stamp-formats)
(defun pcomplete/org-mode/file-option/date ()
  "Complete arguments for the #+DATE file option."
  (pcomplete-here (list (format-time-string (car org-time-stamp-formats)))))

(defun pcomplete/org-mode/file-option/email ()
  "Complete arguments for the #+EMAIL file option."
  (pcomplete-here (list user-mail-address)))

(defvar org-export-exclude-tags)
(defun pcomplete/org-mode/file-option/exclude_tags ()
  "Complete arguments for the #+EXCLUDE_TAGS file option."
  (require 'ox)
  (pcomplete-here
   (and org-export-exclude-tags
	(list (mapconcat 'identity org-export-exclude-tags " ")))))

(defvar org-file-tags)
(defun pcomplete/org-mode/file-option/filetags ()
  "Complete arguments for the #+FILETAGS file option."
  (pcomplete-here (and org-file-tags (mapconcat 'identity org-file-tags " "))))

(defvar org-export-default-language)
(defun pcomplete/org-mode/file-option/language ()
  "Complete arguments for the #+LANGUAGE file option."
  (require 'ox)
  (pcomplete-here
   (pcomplete-uniqify-list
    (list org-export-default-language "en"))))

(defvar org-default-priority)
(defvar org-highest-priority)
(defvar org-lowest-priority)
(defun pcomplete/org-mode/file-option/priorities ()
  "Complete arguments for the #+PRIORITIES file option."
  (pcomplete-here (list (format "%c %c %c"
				org-highest-priority
				org-lowest-priority
				org-default-priority))))

(defvar org-export-select-tags)
(defun pcomplete/org-mode/file-option/select_tags ()
  "Complete arguments for the #+SELECT_TAGS file option."
  (require 'ox)
  (pcomplete-here
   (and org-export-select-tags
	(list (mapconcat 'identity org-export-select-tags " ")))))

(defvar org-startup-options)
(defun pcomplete/org-mode/file-option/startup ()
  "Complete arguments for the #+STARTUP file option."
  (while (pcomplete-here
	  (let ((opts (pcomplete-uniqify-list
		       (mapcar 'car org-startup-options))))
	    ;; Some options are mutually exclusive, and shouldn't be completed
	    ;; against if certain other options have already been seen.
	    (dolist (arg pcomplete-args)
	      (cond
	       ((string= arg "hidestars")
		(setq opts (delete "showstars" opts)))))
	    opts))))

(defun pcomplete/org-mode/file-option/tags ()
  "Complete arguments for the #+TAGS file option."
  (pcomplete-here
   (list (org-tag-alist-to-string org-current-tag-alist))))

(defun pcomplete/org-mode/file-option/title ()
  "Complete arguments for the #+TITLE file option."
  (pcomplete-here
   (let ((visited-file (buffer-file-name (buffer-base-buffer))))
     (list (or (and visited-file
		    (file-name-sans-extension
		     (file-name-nondirectory visited-file)))
	       (buffer-name (buffer-base-buffer)))))))


(declare-function org-export-backend-options "ox" (cl-x) t)
(defun pcomplete/org-mode/file-option/options ()
  "Complete arguments for the #+OPTIONS file option."
  (while (pcomplete-here
	  (pcomplete-uniqify-list
	   (append
	    ;; Hard-coded OPTION items always available.
	    '("H:" "\\n:" "num:" "timestamp:" "arch:" "author:" "c:"
	      "creator:" "date:" "d:" "email:" "*:" "e:" "::" "f:"
	      "inline:" "tex:" "p:" "pri:" "':" "-:" "stat:" "^:" "toc:"
	      "|:" "tags:" "tasks:" "<:" "todo:")
	    ;; OPTION items from registered back-ends.
	    (let (items)
	      (dolist (backend (bound-and-true-p
				org-export-registered-backends))
		(dolist (option (org-export-backend-options backend))
		  (let ((item (nth 2 option)))
		    (when item (push (concat item ":") items)))))
	      items))))))

(defun pcomplete/org-mode/file-option/infojs_opt ()
  "Complete arguments for the #+INFOJS_OPT file option."
  (while (pcomplete-here
	  (pcomplete-uniqify-list
	   (mapcar (lambda (item) (format "%s:" (car item)))
		   (bound-and-true-p org-html-infojs-opts-table))))))

(defun pcomplete/org-mode/file-option/bind ()
  "Complete arguments for the #+BIND file option, which are variable names."
  (let (vars)
    (mapatoms
     (lambda (a) (if (boundp a) (setq vars (cons (symbol-name a) vars)))))
    (pcomplete-here vars)))

(defvar org-link-abbrev-alist-local)
(defvar org-link-abbrev-alist)
(defun pcomplete/org-mode/link ()
  "Complete against defined #+LINK patterns."
  (pcomplete-here
   (pcomplete-uniqify-list
    (copy-sequence
     (append (mapcar 'car org-link-abbrev-alist-local)
	     (mapcar 'car org-link-abbrev-alist))))))

(defvar org-entities)
(defun pcomplete/org-mode/tex ()
  "Complete against TeX-style HTML entity names."
  (require 'org-entities)
  (while (pcomplete-here
	  (pcomplete-uniqify-list (remove nil (mapcar 'car-safe org-entities)))
	  (substring pcomplete-stub 1))))

(defvar org-todo-keywords-1)
(defun pcomplete/org-mode/todo ()
  "Complete against known TODO keywords."
  (pcomplete-here (pcomplete-uniqify-list (copy-sequence org-todo-keywords-1))))

(defvar org-todo-line-regexp)
(defun pcomplete/org-mode/searchhead ()
  "Complete against all headings.
This needs more work, to handle headings with lots of spaces in them."
  (while
      (pcomplete-here
       (save-excursion
	 (goto-char (point-min))
	 (let (tbl)
	   (let ((case-fold-search nil))
	     (while (re-search-forward org-todo-line-regexp nil t)
	       (push (org-make-org-heading-search-string
		      (match-string-no-properties 3))
		     tbl)))
	   (pcomplete-uniqify-list tbl)))
       (substring pcomplete-stub 1))))

(defun pcomplete/org-mode/tag ()
  "Complete a tag name.  Omit tags already set."
  (while (pcomplete-here
	  (mapcar (lambda (x) (concat x ":"))
		  (let ((lst (pcomplete-uniqify-list
			      (or (remq
				   nil
				   (mapcar (lambda (x) (org-string-nw-p (car x)))
					   org-current-tag-alist))
				  (mapcar #'car (org-get-buffer-tags))))))
		    (dolist (tag (org-get-tags))
		      (setq lst (delete tag lst)))
		    lst))
	  (and (string-match ".*:" pcomplete-stub)
	       (substring pcomplete-stub (match-end 0))))))

(defun pcomplete/org-mode/prop ()
  "Complete a property name.  Omit properties already set."
  (pcomplete-here
   (mapcar (lambda (x)
	     (concat x ": "))
	   (let ((lst (pcomplete-uniqify-list
		       (copy-sequence
			(org-buffer-property-keys nil t t t)))))
	     (dolist (prop (org-entry-properties))
	       (setq lst (delete (car prop) lst)))
	     lst))
   (substring pcomplete-stub 1)))

(defun pcomplete/org-mode/block-option/src ()
  "Complete the arguments of a begin_src block.
Complete a language in the first field, the header arguments and switches."
  (pcomplete-here
   (mapcar
    (lambda(x) (symbol-name (nth 3 x)))
    (cdr (car (cdr (memq :key-type (plist-get
				    (symbol-plist
				     'org-babel-load-languages)
				    'custom-type)))))))
  (while (pcomplete-here
	  '("-n" "-r" "-l"
	    ":cache" ":colnames" ":comments" ":dir" ":eval" ":exports"
	    ":file" ":hlines" ":no-expand" ":noweb" ":results" ":rownames"
	    ":session" ":shebang" ":tangle" ":tangle-mode" ":var"))))

(defun pcomplete/org-mode/block-option/clocktable ()
  "Complete keywords in a clocktable line."
  (while (pcomplete-here '(":maxlevel" ":scope" ":lang"
			   ":tstart" ":tend" ":block" ":step"
			   ":stepskip0" ":fileskip0"
			   ":emphasize" ":link" ":narrow" ":indent"
			   ":tcolumns" ":level" ":compact" ":timestamp"
			   ":formula" ":formatter" ":wstart" ":mstart"))))

(defun org-pcomplete-case-double (list)
  "Return list with both upcase and downcase version of all strings in LIST."
  (let (e res)
    (while (setq e (pop list))
      (setq res (cons (downcase e) (cons (upcase e) res))))
    (nreverse res)))

;;;; Finish up

(provide 'org-pcomplete)

;;; org-pcomplete.el ends here

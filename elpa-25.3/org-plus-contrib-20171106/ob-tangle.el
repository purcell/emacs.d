;;; ob-tangle.el --- Extract Source Code From Org Files -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extract the code from source blocks out into raw source-code files.

;;; Code:

(require 'cl-lib)
(require 'org-src)
(require 'org-macs)

(declare-function make-directory "files" (dir &optional parents))
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-babel-update-block-body "ob-core" (new-body))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-type "org-element" (element))
(declare-function org-fill-template "org" (template alist))
(declare-function org-heading-components "org" ())
(declare-function org-in-commented-heading-p "org" (&optional no-inheritance))
(declare-function org-link-escape "org" (text &optional table merge))
(declare-function org-open-link-from-string "org" (s &optional arg reference-buffer))
(declare-function org-remove-indentation "org" (code &optional n))
(declare-function org-store-link "org" (arg))
(declare-function org-trim "org" (s &optional keep-lead))
(declare-function outline-previous-heading "outline" ())
(declare-function org-id-find "org-id" (id &optional markerp))

(defvar org-link-types-re)

(defcustom org-babel-tangle-lang-exts
  '(("emacs-lisp" . "el")
    ("elisp" . "el"))
  "Alist mapping languages to their file extensions.
The key is the language name, the value is the string that should
be inserted as the extension commonly used to identify files
written in this language.  If no entry is found in this list,
then the name of the language is used."
  :group 'org-babel-tangle
  :version "24.1"
  :type '(repeat
	  (cons
	   (string "Language name")
	   (string "File Extension"))))

(defcustom org-babel-tangle-use-relative-file-links t
  "Use relative path names in links from tangled source back the Org file."
  :group 'org-babel-tangle
  :type 'boolean)

(defcustom org-babel-post-tangle-hook nil
  "Hook run in code files tangled by `org-babel-tangle'."
  :group 'org-babel
  :version "24.1"
  :type 'hook)

(defcustom org-babel-pre-tangle-hook '(save-buffer)
  "Hook run at the beginning of `org-babel-tangle'."
  :group 'org-babel
  :version "24.1"
  :type 'hook)

(defcustom org-babel-tangle-body-hook nil
  "Hook run over the contents of each code block body."
  :group 'org-babel
  :version "24.1"
  :type 'hook)

(defcustom org-babel-tangle-comment-format-beg "[[%link][%source-name]]"
  "Format of inserted comments in tangled code files.
The following format strings can be used to insert special
information into the output using `org-fill-template'.
%start-line --- the line number at the start of the code block
%file --------- the file from which the code block was tangled
%link --------- Org style link to the code block
%source-name -- name of the code block

Upon insertion the formatted comment will be commented out, and
followed by a newline.  To inhibit this post-insertion processing
set the `org-babel-tangle-uncomment-comments' variable to a
non-nil value.

Whether or not comments are inserted during tangling is
controlled by the :comments header argument."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defcustom org-babel-tangle-comment-format-end "%source-name ends here"
  "Format of inserted comments in tangled code files.
The following format strings can be used to insert special
information into the output using `org-fill-template'.
%start-line --- the line number at the start of the code block
%file --------- the file from which the code block was tangled
%link --------- Org style link to the code block
%source-name -- name of the code block

Upon insertion the formatted comment will be commented out, and
followed by a newline.  To inhibit this post-insertion processing
set the `org-babel-tangle-uncomment-comments' variable to a
non-nil value.

Whether or not comments are inserted during tangling is
controlled by the :comments header argument."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defcustom org-babel-tangle-uncomment-comments nil
  "Inhibits automatic commenting and addition of trailing newline
of tangle comments.  Use `org-babel-tangle-comment-format-beg'
and `org-babel-tangle-comment-format-end' to customize the format
of tangled comments."
  :group 'org-babel
  :type 'boolean)

(defcustom org-babel-process-comment-text 'org-remove-indentation
  "Function called to process raw Org text collected to be
inserted as comments in tangled source-code files.  The function
should take a single string argument and return a string
result.  The default value is `org-remove-indentation'."
  :group 'org-babel
  :version "24.1"
  :type 'function)

(defun org-babel-find-file-noselect-refresh (file)
  "Find file ensuring that the latest changes on disk are
represented in the file."
  (find-file-noselect file 'nowarn)
  (with-current-buffer (get-file-buffer file)
    (revert-buffer t t t)))

(defmacro org-babel-with-temp-filebuffer (file &rest body)
  "Open FILE into a temporary buffer execute BODY there like
`progn', then kill the FILE buffer returning the result of
evaluating BODY."
  (declare (indent 1))
  (let ((temp-path (make-symbol "temp-path"))
	(temp-result (make-symbol "temp-result"))
	(temp-file (make-symbol "temp-file"))
	(visited-p (make-symbol "visited-p")))
    `(let* ((,temp-path ,file)
	    (,visited-p (get-file-buffer ,temp-path))
	    ,temp-result ,temp-file)
       (org-babel-find-file-noselect-refresh ,temp-path)
       (setf ,temp-file (get-file-buffer ,temp-path))
       (with-current-buffer ,temp-file
	 (setf ,temp-result (progn ,@body)))
       (unless ,visited-p (kill-buffer ,temp-file))
       ,temp-result)))
(def-edebug-spec org-babel-with-temp-filebuffer (form body))

;;;###autoload
(defun org-babel-tangle-file (file &optional target-file lang)
  "Extract the bodies of source code blocks in FILE.
Source code blocks are extracted with `org-babel-tangle'.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG can be
used to limit the exported source code blocks by language.
Return a list whose CAR is the tangled file name."
  (interactive "fFile to tangle: \nP")
  (let ((visited-p (get-file-buffer (expand-file-name file)))
	to-be-removed)
    (prog1
	(save-window-excursion
	  (find-file file)
	  (setq to-be-removed (current-buffer))
	  (mapcar #'expand-file-name (org-babel-tangle nil target-file lang)))
      (unless visited-p
	(kill-buffer to-be-removed)))))

(defun org-babel-tangle-publish (_ filename pub-dir)
  "Tangle FILENAME and place the results in PUB-DIR."
  (unless (file-exists-p pub-dir)
    (make-directory pub-dir t))
  (setq pub-dir (file-name-as-directory pub-dir))
  (mapc (lambda (el) (copy-file el pub-dir t)) (org-babel-tangle-file filename)))

;;;###autoload
(defun org-babel-tangle (&optional arg target-file lang)
  "Write code blocks to source-specific files.
Extract the bodies of all source code blocks from the current
file into their own source-specific files.
With one universal prefix argument, only tangle the block at point.
When two universal prefix arguments, only tangle blocks for the
tangle file of the block at point.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG can be
used to limit the exported source code blocks by language."
  (interactive "P")
  (run-hooks 'org-babel-pre-tangle-hook)
  ;; Possibly Restrict the buffer to the current code block
  (save-restriction
    (save-excursion
      (when (equal arg '(4))
	(let ((head (org-babel-where-is-src-block-head)))
	  (if head
	      (goto-char head)
	    (user-error "Point is not in a source code block"))))
      (let ((block-counter 0)
	    (org-babel-default-header-args
	     (if target-file
		 (org-babel-merge-params org-babel-default-header-args
					 (list (cons :tangle target-file)))
	       org-babel-default-header-args))
	    (tangle-file
	     (when (equal arg '(16))
	       (or (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light))))
		   (user-error "Point is not in a source code block"))))
	    path-collector)
	(mapc ;; map over all languages
	 (lambda (by-lang)
	   (let* ((lang (car by-lang))
		  (specs (cdr by-lang))
		  (ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) lang))
		  (lang-f (intern
			   (concat
			    (or (and (cdr (assoc lang org-src-lang-modes))
				     (symbol-name
				      (cdr (assoc lang org-src-lang-modes))))
				lang)
			    "-mode")))
		  she-banged)
	     (mapc
	      (lambda (spec)
		(let ((get-spec (lambda (name) (cdr (assoc name (nth 4 spec))))))
		  (let* ((tangle (funcall get-spec :tangle))
			 (she-bang (let ((sheb (funcall get-spec :shebang)))
                                     (when (> (length sheb) 0) sheb)))
			 (tangle-mode (funcall get-spec :tangle-mode))
			 (base-name (cond
				     ((string= "yes" tangle)
				      (file-name-sans-extension
				       (nth 1 spec)))
				     ((string= "no" tangle) nil)
				     ((> (length tangle) 0) tangle)))
			 (file-name (when base-name
				      ;; decide if we want to add ext to base-name
				      (if (and ext (string= "yes" tangle))
					  (concat base-name "." ext) base-name))))
		    (when file-name
		      ;; Possibly create the parent directories for file.
		      (let ((m (funcall get-spec :mkdirp))
			    (fnd (file-name-directory file-name)))
			(and m fnd (not (string= m "no"))
			     (make-directory fnd 'parents)))
		      ;; delete any old versions of file
		      (and (file-exists-p file-name)
			   (not (member file-name (mapcar #'car path-collector)))
			   (delete-file file-name))
		      ;; drop source-block to file
		      (with-temp-buffer
			(when (fboundp lang-f) (ignore-errors (funcall lang-f)))
			(when (and she-bang (not (member file-name she-banged)))
			  (insert (concat she-bang "\n"))
			  (setq she-banged (cons file-name she-banged)))
			(org-babel-spec-to-string spec)
			;; We avoid append-to-file as it does not work with tramp.
			(let ((content (buffer-string)))
			  (with-temp-buffer
			    (when (file-exists-p file-name)
			      (insert-file-contents file-name))
			    (goto-char (point-max))
			    ;; Handle :padlines unless first line in file
			    (unless (or (string= "no" (cdr (assq :padline (nth 4 spec))))
					(= (point) (point-min)))
			      (insert "\n"))
			    (insert content)
			    (write-region nil nil file-name))))
		      ;; if files contain she-bangs, then make the executable
		      (when she-bang
			(unless tangle-mode (setq tangle-mode #o755)))
		      ;; update counter
		      (setq block-counter (+ 1 block-counter))
		      (unless (assoc file-name path-collector)
			(push (cons file-name tangle-mode) path-collector))))))
	      specs)))
	 (if (equal arg '(4))
	     (org-babel-tangle-single-block 1 t)
	   (org-babel-tangle-collect-blocks lang tangle-file)))
	(message "Tangled %d code block%s from %s" block-counter
		 (if (= block-counter 1) "" "s")
		 (file-name-nondirectory
		  (buffer-file-name
		   (or (buffer-base-buffer) (current-buffer)))))
	;; run `org-babel-post-tangle-hook' in all tangled files
	(when org-babel-post-tangle-hook
	  (mapc
	   (lambda (file)
	     (org-babel-with-temp-filebuffer file
	       (run-hooks 'org-babel-post-tangle-hook)))
	   (mapcar #'car path-collector)))
	;; set permissions on tangled files
	(mapc (lambda (pair)
		(when (cdr pair) (set-file-modes (car pair) (cdr pair))))
	      path-collector)
	(mapcar #'car path-collector)))))

(defun org-babel-tangle-clean ()
  "Remove comments inserted by `org-babel-tangle'.
Call this function inside of a source-code file generated by
`org-babel-tangle' to remove all comments inserted automatically
by `org-babel-tangle'.  Warning, this comment removes any lines
containing constructs which resemble Org file links or noweb
references."
  (interactive)
  (goto-char (point-min))
  (while (or (re-search-forward "\\[\\[file:.*\\]\\[.*\\]\\]" nil t)
             (re-search-forward (org-babel-noweb-wrap) nil t))
    (delete-region (save-excursion (beginning-of-line 1) (point))
                   (save-excursion (end-of-line 1) (forward-char 1) (point)))))

(defvar org-stored-links)
(defvar org-bracket-link-regexp)
(defun org-babel-spec-to-string (spec)
  "Insert SPEC into the current file.

Insert the source-code specified by SPEC into the current source
code file.  This function uses `comment-region' which assumes
that the appropriate major-mode is set.  SPEC has the form:

  (start-line file link source-name params body comment)"
  (pcase-let*
      ((`(,start ,file ,link ,source ,info ,body ,comment) spec)
       (comments (cdr (assq :comments info)))
       (link? (or (string= comments "both") (string= comments "link")
		  (string= comments "yes") (string= comments "noweb")))
       (link-data `(("start-line" . ,(number-to-string start))
		    ("file" . ,file)
		    ("link" . ,link)
		    ("source-name" . ,source)))
       (insert-comment (lambda (text)
			 (when (and comments
				    (not (string= comments "no"))
				    (org-string-nw-p text))
			   (if org-babel-tangle-uncomment-comments
			       ;; Plain comments: no processing.
			       (insert text)
			     ;; Ensure comments are made to be
			     ;; comments, and add a trailing newline.
			     ;; Also ignore invisible characters when
			     ;; commenting.
			     (comment-region
			      (point)
			      (progn (insert (org-no-properties text))
				     (point)))
			     (end-of-line)
			     (insert "\n"))))))
    (when comment (funcall insert-comment comment))
    (when link?
      (funcall insert-comment
	       (org-fill-template
		org-babel-tangle-comment-format-beg link-data)))
    (insert body "\n")
    (when link?
      (funcall insert-comment
	       (org-fill-template
		org-babel-tangle-comment-format-end link-data)))))

(defun org-babel-tangle-collect-blocks (&optional language tangle-file)
  "Collect source blocks in the current Org file.
Return an association list of source-code block specifications of
the form used by `org-babel-spec-to-string' grouped by language.
Optional argument LANGUAGE can be used to limit the collected
source code blocks by language.  Optional argument TANGLE-FILE
can be used to limit the collected code blocks by target file."
  (let ((counter 0) last-heading-pos blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      (let ((current-heading-pos
	     (org-with-wide-buffer
	      (org-with-limited-levels (outline-previous-heading)))))
	(if (eq last-heading-pos current-heading-pos) (cl-incf counter)
	  (setq counter 1)
	  (setq last-heading-pos current-heading-pos)))
      (unless (org-in-commented-heading-p)
	(let* ((info (org-babel-get-src-block-info 'light))
	       (src-lang (nth 0 info))
	       (src-tfile (cdr (assq :tangle (nth 2 info)))))
	  (unless (or (string= src-tfile "no")
		      (and tangle-file (not (equal tangle-file src-tfile)))
		      (and language (not (string= language src-lang))))
	    ;; Add the spec for this block to blocks under its
	    ;; language.
	    (let ((by-lang (assoc src-lang blocks))
		  (block (org-babel-tangle-single-block counter)))
	      (if by-lang (setcdr by-lang (cons block (cdr by-lang)))
		(push (cons src-lang (list block)) blocks)))))))
    ;; Ensure blocks are in the correct order.
    (mapcar (lambda (b) (cons (car b) (nreverse (cdr b)))) blocks)))

(defun org-babel-tangle-single-block (block-counter &optional only-this-block)
  "Collect the tangled source for current block.
Return the list of block attributes needed by
`org-babel-tangle-collect-blocks'.  When ONLY-THIS-BLOCK is
non-nil, return the full association list to be used by
`org-babel-tangle' directly."
  (let* ((info (org-babel-get-src-block-info))
	 (start-line
	  (save-restriction (widen)
			    (+ 1 (line-number-at-pos (point)))))
	 (file (buffer-file-name (buffer-base-buffer)))
	 (src-lang (nth 0 info))
	 (params (nth 2 info))
	 (extra (nth 3 info))
	 (cref-fmt (or (and (string-match "-l \"\\(.+\\)\"" extra)
			    (match-string 1 extra))
		       org-coderef-label-format))
	 (link (let ((l (org-no-properties (org-store-link nil))))
                 (and (string-match org-bracket-link-regexp l)
                      (match-string 1 l))))
	 (source-name
	  (or (nth 4 info)
	      (format "%s:%d"
		      (or (ignore-errors (nth 4 (org-heading-components)))
			  "No heading")
		      block-counter)))
	 (expand-cmd (intern (concat "org-babel-expand-body:" src-lang)))
	 (assignments-cmd
	  (intern (concat "org-babel-variable-assignments:" src-lang)))
	 (body
	  ;; Run the tangle-body-hook.
          (let ((body (if (org-babel-noweb-p params :tangle)
			  (org-babel-expand-noweb-references info)
			(nth 1 info))))
	    (with-temp-buffer
	      (insert
	       ;; Expand body in language specific manner.
	       (cond ((assq :no-expand params) body)
		     ((fboundp expand-cmd) (funcall expand-cmd body params))
		     (t
		      (org-babel-expand-body:generic
		       body params (and (fboundp assignments-cmd)
					(funcall assignments-cmd params))))))
	      (when (string-match "-r" extra)
		(goto-char (point-min))
		(while (re-search-forward
			(replace-regexp-in-string "%s" ".+" cref-fmt) nil t)
		  (replace-match "")))
	      (run-hooks 'org-babel-tangle-body-hook)
	      (buffer-string))))
	 (comment
	  (when (or (string= "both" (cdr (assq :comments params)))
		    (string= "org" (cdr (assq :comments params))))
	    ;; From the previous heading or code-block end
	    (funcall
	     org-babel-process-comment-text
	     (buffer-substring
	      (max (condition-case nil
		       (save-excursion
			 (org-back-to-heading t) ; Sets match data
			 (match-end 0))
		     (error (point-min)))
		   (save-excursion
		     (if (re-search-backward
			  org-babel-src-block-regexp nil t)
			 (match-end 0)
		       (point-min))))
	      (point)))))
	 (result
	  (list start-line
		(if org-babel-tangle-use-relative-file-links
		    (file-relative-name file)
		  file)
		(if (and org-babel-tangle-use-relative-file-links
			 (string-match org-link-types-re link)
			 (string= (match-string 0 link) "file"))
		    (concat "file:"
			    (file-relative-name (match-string 1 link)
						(file-name-directory
						 (cdr (assq :tangle params)))))
		  link)
		source-name
		params
		(if org-src-preserve-indentation
		    (org-trim body t)
		  (org-trim (org-remove-indentation body)))
		comment)))
    (if only-this-block
	(list (cons src-lang (list result)))
      result)))

(defun org-babel-tangle-comment-links (&optional info)
  "Return a list of begin and end link comments for the code block at point."
  (let ((link-data
	 `(("start-line" . ,(number-to-string
			     (org-babel-where-is-src-block-head)))
	   ("file" . ,(buffer-file-name))
	   ("link" . ,(org-link-escape
		       (progn
			 (call-interactively #'org-store-link)
			 (org-no-properties (car (pop org-stored-links))))))
	   ("source-name" .
	    ,(nth 4 (or info (org-babel-get-src-block-info 'light)))))))
    (list (org-fill-template org-babel-tangle-comment-format-beg link-data)
	  (org-fill-template org-babel-tangle-comment-format-end link-data))))

;; de-tangling functions
(defvar org-bracket-link-analytic-regexp)
(defun org-babel-detangle (&optional source-code-file)
  "Propagate changes in source file back original to Org file.
This requires that code blocks were tangled with link comments
which enable the original code blocks to be found."
  (interactive)
  (save-excursion
    (when source-code-file (find-file source-code-file))
    (goto-char (point-min))
    (let ((counter 0) new-body end)
      (while (re-search-forward org-bracket-link-analytic-regexp nil t)
        (when (re-search-forward
	       (concat " " (regexp-quote (match-string 5)) " ends here"))
          (setq end (match-end 0))
          (forward-line -1)
          (save-excursion
	    (when (setq new-body (org-babel-tangle-jump-to-org))
	      (org-babel-update-block-body new-body)))
          (setq counter (+ 1 counter)))
        (goto-char end))
      (prog1 counter (message "Detangled %d code blocks" counter)))))

(defun org-babel-tangle-jump-to-org ()
  "Jump from a tangled code file to the related Org mode file."
  (interactive)
  (let ((mid (point))
	start body-start end
        target-buffer target-char link path block-name body)
    (save-window-excursion
      (save-excursion
	(while (and (re-search-backward org-bracket-link-analytic-regexp nil t)
		    (not ; ever wider searches until matching block comments
		     (and (setq start (line-beginning-position))
			  (setq body-start (line-beginning-position 2))
			  (setq link (match-string 0))
			  (setq path (match-string 3))
			  (setq block-name (match-string 5))
			  (save-excursion
			    (save-match-data
			      (re-search-forward
			       (concat " " (regexp-quote block-name)
				       " ends here") nil t)
			      (setq end (line-beginning-position))))))))
	(unless (and start (< start mid) (< mid end))
	  (error "Not in tangled code"))
        (setq body (buffer-substring body-start end)))
      (when (string-match "::" path)
        (setq path (substring path 0 (match-beginning 0))))
      (find-file (or (car (org-id-find path)) path))
      (setq target-buffer (current-buffer))
      ;; Go to the beginning of the relative block in Org file.
      (org-open-link-from-string link)
      (if (string-match "[^ \t\n\r]:\\([[:digit:]]+\\)" block-name)
          (let ((n (string-to-number (match-string 1 block-name))))
	    (if (org-before-first-heading-p) (goto-char (point-min))
	      (org-back-to-heading t))
	    ;; Do not skip the first block if it begins at point min.
	    (cond ((or (org-at-heading-p)
		       (not (eq (org-element-type (org-element-at-point))
				'src-block)))
		   (org-babel-next-src-block n))
		  ((= n 1))
		  (t (org-babel-next-src-block (1- n)))))
        (org-babel-goto-named-src-block block-name))
      (goto-char (org-babel-where-is-src-block-head))
      ;; Preserve location of point within the source code in tangled
      ;; code file.
      (forward-line 1)
      (forward-char (- mid body-start))
      (setq target-char (point)))
    (org-src-switch-to-buffer target-buffer t)
    (goto-char target-char)
    body))

(provide 'ob-tangle)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob-tangle.el ends here

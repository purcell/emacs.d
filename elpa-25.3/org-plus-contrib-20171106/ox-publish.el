;;; ox-publish.el --- Publish Related Org Mode Files as a Website -*- lexical-binding: t; -*-
;; Copyright (C) 2006-2017 Free Software Foundation, Inc.

;; Author: David O'Toole <dto@gnu.org>
;; Maintainer: Carsten Dominik <carsten DOT dominik AT gmail DOT com>
;; Keywords: hypermedia, outlines, wp

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

;;; Commentary:

;; This program allow configurable publishing of related sets of
;; Org mode files as a complete website.
;;
;; ox-publish.el can do the following:
;;
;; + Publish all one's Org files to a given export back-end
;; + Upload HTML, images, attachments and other files to a web server
;; + Exclude selected private pages from publishing
;; + Publish a clickable sitemap of pages
;; + Manage local timestamps for publishing only changed files
;; + Accept plugin functions to extend range of publishable content
;;
;; Documentation for publishing is in the manual.

;;; Code:

(require 'cl-lib)
(require 'format-spec)
(require 'ox)



;;; Variables

;; Here, so you find the variable right before it's used the first time:
(defvar org-publish-cache nil
  "This will cache timestamps and titles for files in publishing projects.
Blocks could hash sha1 values here.")

(defvar org-publish-after-publishing-hook nil
  "Hook run each time a file is published.
Every function in this hook will be called with two arguments:
the name of the original file and the name of the file
produced.")

(defgroup org-publish nil
  "Options for publishing a set of files."
  :tag "Org Publishing"
  :group 'org)

(defcustom org-publish-project-alist nil
  "Association list to control publishing behavior.
\\<org-mode-map>
Each element of the alist is a publishing project.  The car of
each element is a string, uniquely identifying the project.  The
cdr of each element is in one of the following forms:

1. A well-formed property list with an even number of elements,
   alternating keys and values, specifying parameters for the
   publishing process.

     (:property value :property value ... )

2. A meta-project definition, specifying of a list of
   sub-projects:

     (:components (\"project-1\" \"project-2\" ...))

When the CDR of an element of org-publish-project-alist is in
this second form, the elements of the list after `:components'
are taken to be components of the project, which group together
files requiring different publishing options.  When you publish
such a project with `\\[org-publish]', the components all publish.

When a property is given a value in `org-publish-project-alist',
its setting overrides the value of the corresponding user
variable (if any) during publishing.  However, options set within
a file override everything.

Most properties are optional, but some should always be set:

  `:base-directory'

    Directory containing publishing source files.

  `:base-extension'

    Extension (without the dot!) of source files.  This can be
    a regular expression.  If not given, \"org\" will be used as
    default extension.  If it is `any', include all the files,
    even without extension.

  `:publishing-directory'

    Directory (possibly remote) where output files will be
    published.

If `:recursive' is non-nil files in sub-directories of
`:base-directory' are considered.

The `:exclude' property may be used to prevent certain files from
being published.  Its value may be a string or regexp matching
file names you don't want to be published.

The `:include' property may be used to include extra files.  Its
value may be a list of filenames to include.  The filenames are
considered relative to the base directory.

When both `:include' and `:exclude' properties are given values,
the exclusion step happens first.

One special property controls which back-end function to use for
publishing files in the project.  This can be used to extend the
set of file types publishable by `org-publish', as well as the
set of output formats.

  `:publishing-function'

    Function to publish file.  Each back-end may define its
    own (i.e. `org-latex-publish-to-pdf',
    `org-html-publish-to-html').  May be a list of functions, in
    which case each function in the list is invoked in turn.

Another property allows you to insert code that prepares
a project for publishing.  For example, you could call GNU Make
on a certain makefile, to ensure published files are built up to
date.

  `:preparation-function'

    Function to be called before publishing this project.  This
    may also be a list of functions.  Preparation functions are
    called with the project properties list as their sole
    argument.

  `:completion-function'

    Function to be called after publishing this project.  This
    may also be a list of functions.  Completion functions are
    called with the project properties list as their sole
    argument.

Some properties control details of the Org publishing process,
and are equivalent to the corresponding user variables listed in
the right column.  Back-end specific properties may also be
included.  See the back-end documentation for more information.

  :author                   `user-full-name'
  :creator                  `org-export-creator-string'
  :email                    `user-mail-address'
  :exclude-tags             `org-export-exclude-tags'
  :headline-levels          `org-export-headline-levels'
  :language                 `org-export-default-language'
  :preserve-breaks          `org-export-preserve-breaks'
  :section-numbers          `org-export-with-section-numbers'
  :select-tags              `org-export-select-tags'
  :time-stamp-file          `org-export-time-stamp-file'
  :with-archived-trees      `org-export-with-archived-trees'
  :with-author              `org-export-with-author'
  :with-creator             `org-export-with-creator'
  :with-date                `org-export-with-date'
  :with-drawers             `org-export-with-drawers'
  :with-email               `org-export-with-email'
  :with-emphasize           `org-export-with-emphasize'
  :with-entities            `org-export-with-entities'
  :with-fixed-width         `org-export-with-fixed-width'
  :with-footnotes           `org-export-with-footnotes'
  :with-inlinetasks         `org-export-with-inlinetasks'
  :with-latex               `org-export-with-latex'
  :with-planning            `org-export-with-planning'
  :with-priority            `org-export-with-priority'
  :with-properties          `org-export-with-properties'
  :with-smart-quotes        `org-export-with-smart-quotes'
  :with-special-strings     `org-export-with-special-strings'
  :with-statistics-cookies' `org-export-with-statistics-cookies'
  :with-sub-superscript     `org-export-with-sub-superscripts'
  :with-toc                 `org-export-with-toc'
  :with-tables              `org-export-with-tables'
  :with-tags                `org-export-with-tags'
  :with-tasks               `org-export-with-tasks'
  :with-timestamps          `org-export-with-timestamps'
  :with-title               `org-export-with-title'
  :with-todo-keywords       `org-export-with-todo-keywords'

The following properties may be used to control publishing of
a site-map of files or summary page for a given project.

  `:auto-sitemap'

    Whether to publish a site-map during
    `org-publish-current-project' or `org-publish-all'.

  `:sitemap-filename'

    Filename for output of site-map.  Defaults to \"sitemap.org\".

  `:sitemap-title'

    Title of site-map page.  Defaults to name of file.

  `:sitemap-style'

    Can be `list' (site-map is just an itemized list of the
    titles of the files involved) or `tree' (the directory
    structure of the source files is reflected in the site-map).
    Defaults to `tree'.

  `:sitemap-format-entry'

    Plugin function used to format entries in the site-map.  It
    is called with three arguments: the file or directory name
    relative to base directory, the site map style and the
    current project.  It has to return a string.

    Defaults to `org-publish-sitemap-default-entry', which turns
    file names into links and use document titles as
    descriptions.  For specific formatting needs, one can use
    `org-publish-find-date', `org-publish-find-title' and
    `org-publish-find-property', to retrieve additional
    information about published documents.

  `:sitemap-function'

    Plugin function to use for generation of site-map.  It is
    called with two arguments: the title of the site-map, as
    a string, and a representation of the files involved in the
    project, as returned by `org-list-to-lisp'.  The latter can
    further be transformed using `org-list-to-generic',
    `org-list-to-subtree' and alike.  It has to return a string.

    Defaults to `org-publish-sitemap-default', which generates
    a plain list of links to all files in the project.

If you create a site-map file, adjust the sorting like this:

  `:sitemap-sort-folders'

    Where folders should appear in the site-map.  Set this to
    `first' or `last' to display folders first or last,
    respectively.  When set to `ignore' (default), folders are
    ignored altogether.  Any other value will mix files and
    folders.  This variable has no effect when site-map style is
    `tree'.

  `:sitemap-sort-files'

    The site map is normally sorted alphabetically.  You can
    change this behavior setting this to `anti-chronologically',
    `chronologically', or nil.

  `:sitemap-ignore-case'

    Should sorting be case-sensitive?  Default nil.

The following property control the creation of a concept index.

  `:makeindex'

    Create a concept index.  The file containing the index has to
    be called \"theindex.org\".  If it doesn't exist in the
    project, it will be generated.  Contents of the index are
    stored in the file \"theindex.inc\", which can be included in
    \"theindex.org\".

Other properties affecting publication.

  `:body-only'

    Set this to t to publish only the body of the documents."
  :group 'org-export-publish
  :type 'alist)

(defcustom org-publish-use-timestamps-flag t
  "Non-nil means use timestamp checking to publish only changed files.
When nil, do no timestamp checking and always publish all files."
  :group 'org-export-publish
  :type 'boolean)

(defcustom org-publish-timestamp-directory
  (convert-standard-filename "~/.org-timestamps/")
  "Name of directory in which to store publishing timestamps."
  :group 'org-export-publish
  :type 'directory)

(defcustom org-publish-list-skipped-files t
  "Non-nil means show message about files *not* published."
  :group 'org-export-publish
  :type 'boolean)

(defcustom org-publish-sitemap-sort-files 'alphabetically
  "Method to sort files in site-maps.
Possible values are `alphabetically', `chronologically',
`anti-chronologically' and nil.

If `alphabetically', files will be sorted alphabetically.  If
`chronologically', files will be sorted with older modification
time first.  If `anti-chronologically', files will be sorted with
newer modification time first.  nil won't sort files.

You can overwrite this default per project in your
`org-publish-project-alist', using `:sitemap-sort-files'."
  :group 'org-export-publish
  :type 'symbol)

(defcustom org-publish-sitemap-sort-folders 'ignore
  "A symbol, denoting if folders are sorted first in site-maps.

Possible values are `first', `last', `ignore' and nil.
If `first', folders will be sorted before files.
If `last', folders are sorted to the end after the files.
If `ignore', folders do not appear in the site-map.
Any other value will mix files and folders.

You can overwrite this default per project in your
`org-publish-project-alist', using `:sitemap-sort-folders'.

This variable is ignored when site-map style is `tree'."
  :group 'org-export-publish
  :type '(choice
	  (const :tag "Folders before files" first)
	  (const :tag "Folders after files" last)
	  (const :tag "No folder in site-map" ignore)
	  (const :tag "Mix folders and files" nil))
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe #'symbolp)

(defcustom org-publish-sitemap-sort-ignore-case nil
  "Non-nil when site-map sorting should ignore case.

You can overwrite this default per project in your
`org-publish-project-alist', using `:sitemap-ignore-case'."
  :group 'org-export-publish
  :type 'boolean)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timestamp-related functions

(defun org-publish-timestamp-filename (filename &optional pub-dir pub-func)
  "Return path to timestamp file for filename FILENAME."
  (setq filename (concat filename "::" (or pub-dir "") "::"
			 (format "%s" (or pub-func ""))))
  (concat "X" (if (fboundp 'sha1) (sha1 filename) (md5 filename))))

(defun org-publish-needed-p
  (filename &optional pub-dir pub-func _true-pub-dir base-dir)
  "Non-nil if FILENAME should be published in PUB-DIR using PUB-FUNC.
TRUE-PUB-DIR is where the file will truly end up.  Currently we
are not using this - maybe it can eventually be used to check if
the file is present at the target location, and how old it is.
Right now we cannot do this, because we do not know under what
file name the file will be stored - the publishing function can
still decide about that independently."
  (let ((rtn (if (not org-publish-use-timestamps-flag) t
	       (org-publish-cache-file-needs-publishing
		filename pub-dir pub-func base-dir))))
    (if rtn (message "Publishing file %s using `%s'" filename pub-func)
      (when org-publish-list-skipped-files
	(message "Skipping unmodified file %s" filename)))
    rtn))

(defun org-publish-update-timestamp
  (filename &optional pub-dir pub-func _base-dir)
  "Update publishing timestamp for file FILENAME.
If there is no timestamp, create one."
  (let ((key (org-publish-timestamp-filename filename pub-dir pub-func))
	(stamp (org-publish-cache-ctime-of-src filename)))
    (org-publish-cache-set key stamp)))

(defun org-publish-remove-all-timestamps ()
  "Remove all files in the timestamp directory."
  (let ((dir org-publish-timestamp-directory))
    (when (and (file-exists-p dir) (file-directory-p dir))
      (mapc #'delete-file (directory-files dir 'full "[^.]\\'"))
      (org-publish-reset-cache))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting project information out of `org-publish-project-alist'

(defun org-publish-property (property project &optional default)
  "Return value PROPERTY, as a symbol, in PROJECT.
DEFAULT is returned when PROPERTY is not actually set in PROJECT
definition."
  (let ((properties (cdr project)))
    (if (plist-member properties property)
	(plist-get properties property)
      default)))

(defun org-publish--expand-file-name (file project)
  "Return full file name for FILE in PROJECT.
When FILE is a relative file name, it is expanded according to
project base directory.  Always return the true name of the file,
ignoring symlinks."
  (file-truename
   (if (file-name-absolute-p file) file
     (expand-file-name file (org-publish-property :base-directory project)))))

(defun org-publish-expand-projects (projects-alist)
  "Expand projects in PROJECTS-ALIST.
This splices all the components into the list."
  (let ((rest projects-alist) rtn p components)
    (while (setq p (pop rest))
      (if (setq components (plist-get (cdr p) :components))
	  (setq rest (append
		      (mapcar
		       (lambda (x)
			 (or (assoc x org-publish-project-alist)
			     (user-error "Unknown component %S in project %S"
					 x (car p))))
		       components)
		      rest))
	(push p rtn)))
    (nreverse (delete-dups (delq nil rtn)))))

(defun org-publish-get-base-files (project)
  "Return a list of all files in PROJECT."
  (let* ((base-dir (file-name-as-directory
		    (org-publish-property :base-directory project)))
	 (extension (or (org-publish-property :base-extension project) "org"))
	 (match (if (eq extension 'any) ""
		  (format "^[^\\.].*\\.\\(%s\\)$" extension)))
	 (base-files
	  (cl-remove-if #'file-directory-p
			(if (org-publish-property :recursive project)
			    (directory-files-recursively base-dir match)
			  (directory-files base-dir t match t)))))
    (org-uniquify
     (append
      ;; Files from BASE-DIR.  Apply exclusion filter before adding
      ;; included files.
      (let ((exclude-regexp (org-publish-property :exclude project)))
	(if exclude-regexp
	    (cl-remove-if
	     (lambda (f)
	       ;; Match against relative names, yet BASE-DIR file
	       ;; names are absolute.
	       (string-match exclude-regexp
			     (file-relative-name f base-dir)))
	     base-files)
	  base-files))
      ;; Sitemap file.
      (and (org-publish-property :auto-sitemap project)
	   (list (expand-file-name
		  (or (org-publish-property :sitemap-filename project)
		      "sitemap.org")
		  base-dir)))
      ;; Included files.
      (mapcar (lambda (f) (expand-file-name f base-dir))
	      (org-publish-property :include project))))))

(defun org-publish-get-project-from-filename (filename &optional up)
  "Return a project that FILENAME belongs to.
When UP is non-nil, return a meta-project (i.e., with a :components part)
publishing FILENAME."
  (let* ((filename (file-truename filename))
	 (project
	  (cl-some
	   (lambda (p)
	     ;; Ignore meta-projects.
	     (unless (org-publish-property :components p)
	       (let ((base (file-truename
			    (org-publish-property :base-directory p))))
		 (cond
		  ;; Check if FILENAME is explicitly included in one
		  ;; project.
		  ((cl-some (lambda (f) (file-equal-p f filename))
			    (mapcar (lambda (f) (expand-file-name f base))
				    (org-publish-property :include p)))
		   p)
		  ;; Exclude file names matching :exclude property.
		  ((let ((exclude-re (org-publish-property :exclude p)))
		     (and exclude-re
			  (string-match-p exclude-re
					  (file-relative-name filename base))))
		   nil)
		  ;; Check :extension.  Handle special `any'
		  ;; extension.
		  ((let ((extension (org-publish-property :base-extension p)))
		     (not (or (eq extension 'any)
			      (string= (or extension "org")
				       (file-name-extension filename)))))
		   nil)
		  ;; Check if FILENAME belong to project's base
		  ;; directory, or some of its sub-directories
		  ;; if :recursive in non-nil.
		  ((org-publish-property :recursive p)
		   (and (file-in-directory-p filename base) p))
		  ((file-equal-p base (file-name-directory filename)) p)
		  (t nil)))))
	   org-publish-project-alist)))
    (cond
     ((not project) nil)
     ((not up) project)
     ;; When optional argument UP is non-nil, return the top-most
     ;; meta-project effectively publishing FILENAME.
     (t
      (letrec ((find-parent-project
		(lambda (project)
		  (or (cl-some
		       (lambda (p)
			 (and (member (car project)
				      (org-publish-property :components p))
			      (funcall find-parent-project p)))
		       org-publish-project-alist)
		      project))))
	(funcall find-parent-project project))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tools for publishing functions in back-ends

(defun org-publish-org-to (backend filename extension plist &optional pub-dir)
  "Publish an Org file to a specified back-end.

BACKEND is a symbol representing the back-end used for
transcoding.  FILENAME is the filename of the Org file to be
published.  EXTENSION is the extension used for the output
string, with the leading dot.  PLIST is the property list for the
given project.

Optional argument PUB-DIR, when non-nil is the publishing
directory.

Return output file name."
  (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))
  ;; Check if a buffer visiting FILENAME is already open.
  (let* ((org-inhibit-startup t)
	 (visiting (find-buffer-visiting filename))
	 (work-buffer (or visiting (find-file-noselect filename))))
    (unwind-protect
	(with-current-buffer work-buffer
	  (let ((output (org-export-output-file-name extension nil pub-dir)))
	    (org-export-to-file backend output
	      nil nil nil (plist-get plist :body-only)
	      ;; Add `org-publish--store-crossrefs' and
	      ;; `org-publish-collect-index' to final output filters.
	      ;; The latter isn't dependent on `:makeindex', since we
	      ;; want to keep it up-to-date in cache anyway.
	      (org-combine-plists
	       plist
	       `(:crossrefs
		 ,(org-publish-cache-get-file-property
		   ;; Normalize file names in cache.
		   (file-truename filename) :crossrefs nil t)
		 :filter-final-output
		 (org-publish--store-crossrefs
		  org-publish-collect-index
		  ,@(plist-get plist :filter-final-output)))))))
      ;; Remove opened buffer in the process.
      (unless visiting (kill-buffer work-buffer)))))

(defun org-publish-attachment (_plist filename pub-dir)
  "Publish a file with no transformation of any kind.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (unless (file-directory-p pub-dir)
    (make-directory pub-dir t))
  (let ((output (expand-file-name (file-name-nondirectory filename) pub-dir)))
    (unless (file-equal-p (expand-file-name (file-name-directory filename))
			  (file-name-as-directory (expand-file-name pub-dir)))
      (copy-file filename output t))
    ;; Return file name.
    output))



;;; Publishing files, sets of files

(defun org-publish-file (filename &optional project no-cache)
  "Publish file FILENAME from PROJECT.
If NO-CACHE is not nil, do not initialize `org-publish-cache'.
This is needed, since this function is used to publish single
files, when entire projects are published (see
`org-publish-projects')."
  (let* ((project
	  (or project
	      (org-publish-get-project-from-filename filename)
	      (user-error "File %S is not part of any known project"
			  (abbreviate-file-name filename))))
	 (project-plist (cdr project))
	 (publishing-function
	  (pcase (org-publish-property :publishing-function project)
	    (`nil (user-error "No publishing function chosen"))
	    ((and f (pred listp)) f)
	    (f (list f))))
	 (base-dir
	  (file-name-as-directory
	   (or (org-publish-property :base-directory project)
	       (user-error "Project %S does not have :base-directory defined"
			   (car project)))))
	 (pub-base-dir
	  (file-name-as-directory
	   (or (org-publish-property :publishing-directory project)
	       (user-error
		"Project %S does not have :publishing-directory defined"
		(car project)))))
	 (pub-dir
	  (file-name-directory
	   (expand-file-name (file-relative-name filename base-dir)
			     pub-base-dir))))

    (unless no-cache (org-publish-initialize-cache (car project)))

    ;; Allow chain of publishing functions.
    (dolist (f publishing-function)
      (when (org-publish-needed-p filename pub-base-dir f pub-dir base-dir)
	(let ((output (funcall f project-plist filename pub-dir)))
	  (org-publish-update-timestamp filename pub-base-dir f base-dir)
	  (run-hook-with-args 'org-publish-after-publishing-hook
			      filename
			      output))))
    ;; Make sure to write cache to file after successfully publishing
    ;; a file, so as to minimize impact of a publishing failure.
    (org-publish-write-cache-file)))

(defun org-publish-projects (projects)
  "Publish all files belonging to the PROJECTS alist.
If `:auto-sitemap' is set, publish the sitemap too.  If
`:makeindex' is set, also produce a file \"theindex.org\"."
  (dolist (project (org-publish-expand-projects projects))
    (let ((plist (cdr project)))
      (let ((fun (org-publish-property :preparation-function project)))
	(cond
	 ((consp fun) (dolist (f fun) (funcall f plist)))
	 ((functionp fun) (funcall fun plist))))
      ;; Each project uses its own cache file.
      (org-publish-initialize-cache (car project))
      (when (org-publish-property :auto-sitemap project)
	(let ((sitemap-filename
	       (or (org-publish-property :sitemap-filename project)
		   "sitemap.org")))
	  (org-publish-sitemap project sitemap-filename)))
      ;; Publish all files from PROJECT except "theindex.org".  Its
      ;; publishing will be deferred until "theindex.inc" is
      ;; populated.
      (let ((theindex
	     (expand-file-name "theindex.org"
			       (org-publish-property :base-directory project))))
	(dolist (file (org-publish-get-base-files project))
	  (unless (file-equal-p file theindex)
	    (org-publish-file file project t)))
	;; Populate "theindex.inc", if needed, and publish
	;; "theindex.org".
	(when (org-publish-property :makeindex project)
	  (org-publish-index-generate-theindex
	   project (org-publish-property :base-directory project))
	  (org-publish-file theindex project t)))
      (let ((fun (org-publish-property :completion-function project)))
	(cond
	 ((consp fun) (dolist (f fun) (funcall f plist)))
	 ((functionp fun) (funcall fun plist)))))
    (org-publish-write-cache-file)))


;;; Site map generation

(defun org-publish--sitemap-files-to-lisp (files project style format-entry)
  "Represent FILES as a parsed plain list.
FILES is the list of files in the site map.  PROJECT is the
current project.  STYLE determines is either `list' or `tree'.
FORMAT-ENTRY is a function called on each file which should
return a string.  Return value is a list as returned by
`org-list-to-lisp'."
  (let ((root (expand-file-name
	       (file-name-as-directory
		(org-publish-property :base-directory project)))))
    (pcase style
      (`list
       (cons 'unordered
	     (mapcar
	      (lambda (f)
		(list (funcall format-entry
			       (file-relative-name f root)
			       style
			       project)))
	      files)))
      (`tree
       (letrec ((files-only (cl-remove-if #'directory-name-p files))
		(directories (cl-remove-if-not #'directory-name-p files))
		(subtree-to-list
		 (lambda (dir)
		   (cons 'unordered
			 (nconc
			  ;; Files in DIR.
			  (mapcar
			   (lambda (f)
			     (list (funcall format-entry
					    (file-relative-name f root)
					    style
					    project)))
			   (cl-remove-if-not
			    (lambda (f) (string= dir (file-name-directory f)))
			    files-only))
			  ;; Direct sub-directories.
			  (mapcar
			   (lambda (sub)
			     (list (funcall format-entry
					    (file-relative-name sub root)
					    style
					    project)
				   (funcall subtree-to-list sub)))
			   (cl-remove-if-not
			    (lambda (f)
			      (string=
			       dir
			       ;; Parent directory.
			       (file-name-directory (directory-file-name f))))
			    directories)))))))
	 (funcall subtree-to-list root)))
      (_ (user-error "Unknown site-map style: `%s'" style)))))

(defun org-publish-sitemap (project &optional sitemap-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is `sitemap.org'."
  (let* ((root (expand-file-name
		(file-name-as-directory
		 (org-publish-property :base-directory project))))
	 (sitemap-filename (concat root (or sitemap-filename "sitemap.org")))
	 (title (or (org-publish-property :sitemap-title project)
		    (concat "Sitemap for project " (car project))))
	 (style (or (org-publish-property :sitemap-style project)
		    'tree))
	 (sitemap-builder (or (org-publish-property :sitemap-function project)
			      #'org-publish-sitemap-default))
	 (format-entry (or (org-publish-property :sitemap-format-entry project)
			   #'org-publish-sitemap-default-entry))
	 (sort-folders
	  (org-publish-property :sitemap-sort-folders project
				org-publish-sitemap-sort-folders))
	 (sort-files
	  (org-publish-property :sitemap-sort-files project
				org-publish-sitemap-sort-files))
	 (ignore-case
	  (org-publish-property :sitemap-ignore-case project
				org-publish-sitemap-sort-ignore-case))
	 (org-file-p (lambda (f) (equal "org" (file-name-extension f))))
	 (sort-predicate
	  (lambda (a b)
	    (let ((retval t))
	      ;; First we sort files:
	      (pcase sort-files
		(`alphabetically
		 (let ((A (if (funcall org-file-p a)
			      (concat (file-name-directory a)
				      (org-publish-find-title a project))
			    a))
		       (B (if (funcall org-file-p b)
			      (concat (file-name-directory b)
				      (org-publish-find-title b project))
			    b)))
		   (setq retval
			 (if ignore-case
			     (not (string-lessp (upcase B) (upcase A)))
			   (not (string-lessp B A))))))
		((or `anti-chronologically `chronologically)
		 (let* ((adate (org-publish-find-date a project))
			(bdate (org-publish-find-date b project))
			(A (+ (lsh (car adate) 16) (cadr adate)))
			(B (+ (lsh (car bdate) 16) (cadr bdate))))
		   (setq retval
			 (if (eq sort-files 'chronologically)
			     (<= A B)
			   (>= A B)))))
		(`nil nil)
		(_ (user-error "Invalid sort value %s" sort-files)))
	      ;; Directory-wise wins:
	      (when (memq sort-folders '(first last))
		;; a is directory, b not:
		(cond
		 ((and (file-directory-p a) (not (file-directory-p b)))
		  (setq retval (eq sort-folders 'first)))
		 ;; a is not a directory, but b is:
		 ((and (not (file-directory-p a)) (file-directory-p b))
		  (setq retval (eq sort-folders 'last)))))
	      retval))))
    (message "Generating sitemap for %s" title)
    (with-temp-file sitemap-filename
      (insert
       (let ((files (remove sitemap-filename
			    (org-publish-get-base-files project))))
	 ;; Add directories, if applicable.
	 (unless (and (eq style 'list) (eq sort-folders 'ignore))
	   (setq files
		 (nconc (remove root (org-uniquify
				      (mapcar #'file-name-directory files)))
			files)))
	 ;; Eventually sort all entries.
	 (when (or sort-files (not (memq sort-folders 'ignore)))
	   (setq files (sort files sort-predicate)))
	 (funcall sitemap-builder
		  title
		  (org-publish--sitemap-files-to-lisp
		   files project style format-entry)))))))

(defun org-publish-find-property (file property project &optional backend)
  "Find the PROPERTY of FILE in project.

PROPERTY is a keyword referring to an export option, as defined
in `org-export-options-alist' or in export back-ends.  In the
latter case, optional argument BACKEND has to be set to the
back-end where the option is defined, e.g.,

  (org-publish-find-property file :subtitle 'latex)

Return value may be a string or a list, depending on the type of
PROPERTY, i.e. \"behavior\" parameter from `org-export-options-alist'."
  (let ((file (org-publish--expand-file-name file project)))
    (when (and (file-readable-p file) (not (directory-name-p file)))
      (let* ((org-inhibit-startup t)
	     (visiting (find-buffer-visiting file))
	     (buffer (or visiting (find-file-noselect file))))
	(unwind-protect
	    (plist-get (with-current-buffer buffer
			 (if (not visiting) (org-export-get-environment backend)
			   ;; Protect local variables in open buffers.
			   (org-export-with-buffer-copy
			    (org-export-get-environment backend))))
		       property)
	  (unless visiting (kill-buffer buffer)))))))

(defun org-publish-find-title (file project)
  "Find the title of FILE in PROJECT."
  (let ((file (org-publish--expand-file-name file project)))
    (or (org-publish-cache-get-file-property file :title nil t)
	(let* ((parsed-title (org-publish-find-property file :title project))
	       (title
		(if parsed-title
		    ;; Remove property so that the return value is
		    ;; cache-able (i.e., it can be `read' back).
		    (org-no-properties
		     (org-element-interpret-data parsed-title))
		  (file-name-nondirectory (file-name-sans-extension file)))))
	  (org-publish-cache-set-file-property file :title title)
	  title))))

(defun org-publish-find-date (file project)
  "Find the date of FILE in PROJECT.
This function assumes FILE is either a directory or an Org file.
If FILE is an Org file and provides a DATE keyword use it.  In
any other case use the file system's modification time.  Return
time in `current-time' format."
  (let ((file (org-publish--expand-file-name file project)))
    (if (file-directory-p file) (nth 5 (file-attributes file))
      (let ((date (org-publish-find-property file :date project)))
	;; DATE is a secondary string.  If it contains a time-stamp,
	;; convert it to internal format.  Otherwise, use FILE
	;; modification time.
	(cond ((let ((ts (and (consp date) (assq 'timestamp date))))
		 (and ts
		      (let ((value (org-element-interpret-data ts)))
			(and (org-string-nw-p value)
			     (org-time-string-to-time value))))))
	      ((file-exists-p file) (nth 5 (file-attributes file)))
	      (t (error "No such file: \"%s\"" file)))))))

(defun org-publish-sitemap-default-entry (entry style project)
  "Default format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
	 (format "[[file:%s][%s]]"
		 entry
		 (org-publish-find-title entry project)))
	((eq style 'tree)
	 ;; Return only last subdir.
	 (file-name-nondirectory (directory-file-name entry)))
	(t entry)))

(defun org-publish-sitemap-default (title list)
  "Default site map, as a string.
TITLE is the the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
	  (org-list-to-org list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive publishing functions

;;;###autoload
(defalias 'org-publish-project 'org-publish)

;;;###autoload
(defun org-publish (project &optional force async)
  "Publish PROJECT.

PROJECT is either a project name, as a string, or a project
alist (see `org-publish-project-alist' variable).

When optional argument FORCE is non-nil, force publishing all
files in PROJECT.  With a non-nil optional argument ASYNC,
publishing will be done asynchronously, in another process."
  (interactive
   (list (assoc (completing-read "Publish project: "
				 org-publish-project-alist nil t)
		org-publish-project-alist)
	 current-prefix-arg))
  (let ((project (if (not (stringp project)) project
		   ;; If this function is called in batch mode,
		   ;; PROJECT is still a string here.
		   (assoc project org-publish-project-alist))))
    (cond
     ((not project))
     (async
      (org-export-async-start (lambda (_) nil)
	`(let ((org-publish-use-timestamps-flag
		,(and (not force) org-publish-use-timestamps-flag)))
	   ;; Expand components right now as external process may not
	   ;; be aware of complete `org-publish-project-alist'.
	   (org-publish-projects
	    ',(org-publish-expand-projects (list project))))))
     (t (save-window-excursion
	  (let ((org-publish-use-timestamps-flag
		 (and (not force) org-publish-use-timestamps-flag)))
	    (org-publish-projects (list project))))))))

;;;###autoload
(defun org-publish-all (&optional force async)
  "Publish all projects.
With prefix argument FORCE, remove all files in the timestamp
directory and force publishing all projects.  With a non-nil
optional argument ASYNC, publishing will be done asynchronously,
in another process."
  (interactive "P")
  (if async
      (org-export-async-start (lambda (_) nil)
	`(progn
	   (when ',force (org-publish-remove-all-timestamps))
	   (let ((org-publish-use-timestamps-flag
		  (if ',force nil ,org-publish-use-timestamps-flag)))
	     (org-publish-projects ',org-publish-project-alist))))
    (when force (org-publish-remove-all-timestamps))
    (save-window-excursion
      (let ((org-publish-use-timestamps-flag
	     (if force nil org-publish-use-timestamps-flag)))
	(org-publish-projects org-publish-project-alist)))))


;;;###autoload
(defun org-publish-current-file (&optional force async)
  "Publish the current file.
With prefix argument FORCE, force publish the file.  When
optional argument ASYNC is non-nil, publishing will be done
asynchronously, in another process."
  (interactive "P")
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (if async
	(org-export-async-start (lambda (_) nil)
	  `(let ((org-publish-use-timestamps-flag
		  (if ',force nil ,org-publish-use-timestamps-flag)))
	     (org-publish-file ,file)))
      (save-window-excursion
	(let ((org-publish-use-timestamps-flag
	       (if force nil org-publish-use-timestamps-flag)))
	  (org-publish-file file))))))

;;;###autoload
(defun org-publish-current-project (&optional force async)
  "Publish the project associated with the current file.
With a prefix argument, force publishing of all files in
the project."
  (interactive "P")
  (save-window-excursion
    (let ((project (org-publish-get-project-from-filename
		    (buffer-file-name (buffer-base-buffer)) 'up)))
      (if project (org-publish project force async)
	(error "File %s is not part of any known project"
	       (buffer-file-name (buffer-base-buffer)))))))



;;; Index generation

(defun org-publish-collect-index (output _backend info)
  "Update index for a file in cache.

OUTPUT is the output from transcoding current file.  BACKEND is
the back-end that was used for transcoding.  INFO is a plist
containing publishing and export options.

The index relative to current file is stored as an alist.  An
association has the following shape: (TERM FILE-NAME PARENT),
where TERM is the indexed term, as a string, FILE-NAME is the
original full path of the file where the term in encountered, and
PARENT is a reference to the headline, if any, containing the
original index keyword.  When non-nil, this reference is a cons
cell.  Its CAR is a symbol among `id', `custom-id' and `name' and
its CDR is a string."
  (let ((file (file-truename (plist-get info :input-file))))
    (org-publish-cache-set-file-property
     file :index
     (delete-dups
      (org-element-map (plist-get info :parse-tree) 'keyword
	(lambda (k)
	  (when (equal (org-element-property :key k) "INDEX")
	    (let ((parent (org-export-get-parent-headline k)))
	      (list (org-element-property :value k)
		    file
		    (cond
		     ((not parent) nil)
		     ((let ((id (org-element-property :ID parent)))
			(and id (cons 'id id))))
		     ((let ((id (org-element-property :CUSTOM_ID parent)))
			(and id (cons 'custom-id id))))
		     (t (cons 'name
			      ;; Remove statistics cookie.
			      (replace-regexp-in-string
			       "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
			       (org-element-property :raw-value parent)))))))))
	info))))
  ;; Return output unchanged.
  output)

(defun org-publish-index-generate-theindex (project directory)
  "Retrieve full index from cache and build \"theindex.org\".
PROJECT is the project the index relates to.  DIRECTORY is the
publishing directory."
  (let ((all-files (org-publish-get-base-files project))
	full-index)
    ;; Compile full index and sort it alphabetically.
    (dolist (file all-files
		  (setq full-index
			(sort (nreverse full-index)
			      (lambda (a b) (string< (downcase (car a))
						(downcase (car b)))))))
      (let ((index (org-publish-cache-get-file-property file :index)))
	(dolist (term index)
	  (unless (member term full-index) (push term full-index)))))
    ;; Write "theindex.inc" in DIRECTORY.
    (with-temp-file (expand-file-name "theindex.inc" directory)
      (let ((current-letter nil) (last-entry nil))
	(dolist (idx full-index)
	  (let* ((entry (org-split-string (car idx) "!"))
		 (letter (upcase (substring (car entry) 0 1)))
		 ;; Transform file into a path relative to publishing
		 ;; directory.
		 (file (file-relative-name
			(nth 1 idx)
			(plist-get (cdr project) :base-directory))))
	    ;; Check if another letter has to be inserted.
	    (unless (string= letter current-letter)
	      (insert (format "* %s\n" letter)))
	    ;; Compute the first difference between last entry and
	    ;; current one: it tells the level at which new items
	    ;; should be added.
	    (let* ((rank
		    (if (equal entry last-entry) (1- (length entry))
		      (cl-loop for n from 0 to (length entry)
			       unless (equal (nth n entry) (nth n last-entry))
			       return n)))
		   (len (length (nthcdr rank entry))))
	      ;; For each term after the first difference, create
	      ;; a new sub-list with the term as body.  Moreover,
	      ;; linkify the last term.
	      (dotimes (n len)
		(insert
		 (concat
		  (make-string (* (+ rank n) 2) ?\s) "  - "
		  (if (not (= (1- len) n)) (nth (+ rank n) entry)
		    ;; Last term: Link it to TARGET, if possible.
		    (let ((target (nth 2 idx)))
		      (format
		       "[[%s][%s]]"
		       ;; Destination.
		       (pcase (car target)
			 (`nil (format "file:%s" file))
			 (`id (format "id:%s" (cdr target)))
			 (`custom-id (format "file:%s::#%s" file (cdr target)))
			 (_ (format "file:%s::*%s" file (cdr target))))
		       ;; Description.
		       (car (last entry)))))
		  "\n"))))
	    (setq current-letter letter last-entry entry))))
      ;; Create "theindex.org", if it doesn't exist yet, and provide
      ;; a default index file.
      (let ((index.org (expand-file-name "theindex.org" directory)))
	(unless (file-exists-p index.org)
	  (with-temp-file index.org
	    (insert "#+TITLE: Index\n\n#+INCLUDE: \"theindex.inc\"\n\n")))))))



;;; External Fuzzy Links Resolution
;;
;; This part implements tools to resolve [[file.org::*Some headline]]
;; links, where "file.org" belongs to the current project.

(defun org-publish--store-crossrefs (output _backend info)
  "Store cross-references for current published file.

OUTPUT is the produced output, as a string.  BACKEND is the export
back-end used, as a symbol.  INFO is the final export state, as
a plist.

This function is meant to be used as a final output filter.  See
`org-publish-org-to'."
  (org-publish-cache-set-file-property
   (file-truename (plist-get info :input-file))
   :crossrefs
   ;; Update `:crossrefs' so as to remove unused references and search
   ;; cells.  Actually used references are extracted from
   ;; `:internal-references', with references as strings removed.  See
   ;; `org-export-get-reference' for details.
   (cl-remove-if (lambda (pair) (stringp (car pair)))
		 (plist-get info :internal-references)))
  ;; Return output unchanged.
  output)

(defun org-publish-resolve-external-link (search file)
  "Return reference for element matching string SEARCH in FILE.

Return value is an internal reference, as a string.

This function allows resolving external links with a search
option, e.g.,

  [[file.org::*heading][description]]
  [[file.org::#custom-id][description]]
  [[file.org::fuzzy][description]]

It only makes sense to use this if export back-end builds
references with `org-export-get-reference'."
  (if (not org-publish-cache)
      (progn
	(message "Reference %S in file %S cannot be resolved without publishing"
		 search
		 file)
	"MissingReference")
    (let* ((filename (file-truename file))
	   (crossrefs
	    (org-publish-cache-get-file-property filename :crossrefs nil t))
	   (cells (org-export-string-to-search-cell search)))
      (or
       ;; Look for reference associated to search cells triggered by
       ;; LINK.  It can match when targeted file has been published
       ;; already.
       (let ((known (cdr (cl-some (lambda (c) (assoc c crossrefs)) cells))))
	 (and known (org-export-format-reference known)))
       ;; Search cell is unknown so far.  Generate a new internal
       ;; reference that will be used when the targeted file will be
       ;; published.
       (let ((new (org-export-new-reference crossrefs)))
	 (dolist (cell cells) (push (cons cell new) crossrefs))
	 (org-publish-cache-set-file-property filename :crossrefs crossrefs)
	 (org-export-format-reference new))))))



;;; Caching functions

(defun org-publish-write-cache-file (&optional free-cache)
  "Write `org-publish-cache' to file.
If FREE-CACHE, empty the cache."
  (unless org-publish-cache
    (error "`org-publish-write-cache-file' called, but no cache present"))

  (let ((cache-file (org-publish-cache-get ":cache-file:")))
    (unless cache-file
      (error "Cannot find cache-file name in `org-publish-write-cache-file'"))
    (with-temp-file cache-file
      (let (print-level print-length)
	(insert "(setq org-publish-cache \
\(make-hash-table :test 'equal :weakness nil :size 100))\n")
	(maphash (lambda (k v)
		   (insert
		    (format "(puthash %S %s%S org-publish-cache)\n"
			    k (if (or (listp v) (symbolp v)) "'" "") v)))
		 org-publish-cache)))
    (when free-cache (org-publish-reset-cache))))

(defun org-publish-initialize-cache (project-name)
  "Initialize the projects cache if not initialized yet and return it."

  (unless project-name
    (error "Cannot initialize `org-publish-cache' without projects name in \
`org-publish-initialize-cache'"))

  (unless (file-exists-p org-publish-timestamp-directory)
    (make-directory org-publish-timestamp-directory t))
  (unless (file-directory-p org-publish-timestamp-directory)
    (error "Org publish timestamp: %s is not a directory"
	   org-publish-timestamp-directory))

  (unless (and org-publish-cache
	       (string= (org-publish-cache-get ":project:") project-name))
    (let* ((cache-file
	    (concat
	     (expand-file-name org-publish-timestamp-directory)
	     project-name ".cache"))
	   (cexists (file-exists-p cache-file)))

      (when org-publish-cache (org-publish-reset-cache))

      (if cexists (load-file cache-file)
	(setq org-publish-cache
	      (make-hash-table :test 'equal :weakness nil :size 100))
	(org-publish-cache-set ":project:" project-name)
	(org-publish-cache-set ":cache-file:" cache-file))
      (unless cexists (org-publish-write-cache-file nil))))
  org-publish-cache)

(defun org-publish-reset-cache ()
  "Empty org-publish-cache and reset it nil."
  (message "%s" "Resetting org-publish-cache")
  (when (hash-table-p org-publish-cache)
    (clrhash org-publish-cache))
  (setq org-publish-cache nil))

(defun org-publish-cache-file-needs-publishing
    (filename &optional pub-dir pub-func _base-dir)
  "Check the timestamp of the last publishing of FILENAME.
Return non-nil if the file needs publishing.  Also check if
any included files have been more recently published, so that
the file including them will be republished as well."
  (unless org-publish-cache
    (error
     "`org-publish-cache-file-needs-publishing' called, but no cache present"))
  (let* ((key (org-publish-timestamp-filename filename pub-dir pub-func))
	 (pstamp (org-publish-cache-get key))
	 (org-inhibit-startup t)
	 included-files-ctime)
    (when (equal (file-name-extension filename) "org")
      (let ((visiting (find-buffer-visiting filename))
	    (buf (find-file-noselect filename))
	    (case-fold-search t))
	(unwind-protect
	    (with-current-buffer buf
	      (goto-char (point-min))
	      (while (re-search-forward "^[ \t]*#\\+INCLUDE:" nil t)
		(let* ((element (org-element-at-point))
		       (included-file
			(and (eq (org-element-type element) 'keyword)
			     (let ((value (org-element-property :value element)))
			       (and value
				    (string-match
				     "\\`\\(\".+?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)"
				     value)
				    (let ((m (match-string 1 value)))
				      (org-unbracket-string
				       "\"" "\""
				       ;; Ignore search suffix.
				       (if (string-match "::.*?\"?\\'" m)
					   (substring m 0 (match-beginning 0))
					 m))))))))
		  (when included-file
		    (push (org-publish-cache-ctime-of-src
			   (expand-file-name included-file))
			  included-files-ctime)))))
	  (unless visiting (kill-buffer buf)))))
    (or (null pstamp)
	(let ((ctime (org-publish-cache-ctime-of-src filename)))
	  (or (< pstamp ctime)
	      (cl-some (lambda (ct) (< ctime ct)) included-files-ctime))))))

(defun org-publish-cache-set-file-property
  (filename property value &optional project-name)
  "Set the VALUE for a PROPERTY of file FILENAME in publishing cache to VALUE.
Use cache file of PROJECT-NAME.  If the entry does not exist, it
will be created.  Return VALUE."
  ;; Evtl. load the requested cache file:
  (if project-name (org-publish-initialize-cache project-name))
  (let ((pl (org-publish-cache-get filename)))
    (if pl (progn (plist-put pl property value) value)
      (org-publish-cache-get-file-property
       filename property value nil project-name))))

(defun org-publish-cache-get-file-property
    (filename property &optional default no-create project-name)
  "Return the value for a PROPERTY of file FILENAME in publishing cache.
Use cache file of PROJECT-NAME.  Return the value of that PROPERTY,
or DEFAULT, if the value does not yet exist.  Create the entry,
if necessary, unless NO-CREATE is non-nil."
  (when project-name (org-publish-initialize-cache project-name))
  (let ((properties (org-publish-cache-get filename)))
    (cond ((null properties)
	   (unless no-create
	     (org-publish-cache-set filename (list property default)))
	   default)
	  ((plist-member properties property) (plist-get properties property))
	  (t default))))

(defun org-publish-cache-get (key)
  "Return the value stored in `org-publish-cache' for key KEY.
Return nil, if no value or nil is found.  Raise an error if the
cache does not exist."
  (unless org-publish-cache
    (error "`org-publish-cache-get' called, but no cache present"))
  (gethash key org-publish-cache))

(defun org-publish-cache-set (key value)
  "Store KEY VALUE pair in `org-publish-cache'.
Returns value on success, else nil.  Raise an error if the cache
does not exist."
  (unless org-publish-cache
    (error "`org-publish-cache-set' called, but no cache present"))
  (puthash key value org-publish-cache))

(defun org-publish-cache-ctime-of-src (file)
  "Get the ctime of FILE as an integer."
  (let ((attr (file-attributes
	       (expand-file-name (or (file-symlink-p file) file)
				 (file-name-directory file)))))
    (if (not attr) (error "No such file: \"%s\"" file)
      (+ (lsh (car (nth 5 attr)) 16)
	 (cadr (nth 5 attr))))))


(provide 'ox-publish)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-publish.el ends here

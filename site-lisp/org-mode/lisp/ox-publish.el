;;; ox-publish.el --- Publish Related Org Mode Files as a Website
;; Copyright (C) 2006-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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

(eval-when-compile (require 'cl))
(require 'format-spec)
(require 'ox)



;;; Variables

(defvar org-publish-temp-files nil
  "Temporary list of files to be published.")

;; Here, so you find the variable right before it's used the first time:
(defvar org-publish-cache nil
  "This will cache timestamps and titles for files in publishing projects.
Blocks could hash sha1 values here.")

(defgroup org-publish nil
  "Options for publishing a set of Org-mode and related files."
  :tag "Org Publishing"
  :group 'org)

(defcustom org-publish-project-alist nil
  "Association list to control publishing behavior.
Each element of the alist is a publishing 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is in one of the following forms:

1. A well-formed property list with an even number of elements,
   alternating keys and values, specifying parameters for the
   publishing process.

     \(:property value :property value ... )

2. A meta-project definition, specifying of a list of
   sub-projects:

     \(:components (\"project-1\" \"project-2\" ...))

When the CDR of an element of org-publish-project-alist is in
this second form, the elements of the list after `:components'
are taken to be components of the project, which group together
files requiring different publishing options.  When you publish
such a project with \\[org-publish], the components all publish.

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
    default extension.

  `:publishing-directory'

    Directory (possibly remote) where output files will be
    published.

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
    may also be a list of functions.

  `:completion-function'

    Function to be called after publishing this project.  This
    may also be a list of functions.

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
  :with-priority            `org-export-with-priority'
  :with-smart-quotes        `org-export-with-smart-quotes'
  :with-special-strings     `org-export-with-special-strings'
  :with-statistics-cookies' `org-export-with-statistics-cookies'
  :with-sub-superscript     `org-export-with-sub-superscripts'
  :with-toc                 `org-export-with-toc'
  :with-tables              `org-export-with-tables'
  :with-tags                `org-export-with-tags'
  :with-tasks               `org-export-with-tasks'
  :with-timestamps          `org-export-with-timestamps'
  :with-planning            `org-export-with-planning'
  :with-todo-keywords       `org-export-with-todo-keywords'

The following properties may be used to control publishing of
a site-map of files or summary page for a given project.

  `:auto-sitemap'

    Whether to publish a site-map during
    `org-publish-current-project' or `org-publish-all'.

  `:sitemap-filename'

    Filename for output of sitemap.  Defaults to \"sitemap.org\".

  `:sitemap-title'

    Title of site-map page.  Defaults to name of file.

  `:sitemap-function'

    Plugin function to use for generation of site-map.  Defaults
    to `org-publish-org-sitemap', which generates a plain list of
    links to all files in the project.

  `:sitemap-style'

    Can be `list' (site-map is just an itemized list of the
    titles of the files involved) or `tree' (the directory
    structure of the source files is reflected in the site-map).
    Defaults to `tree'.

  `:sitemap-sans-extension'

    Remove extension from site-map's file-names.  Useful to have
    cool URIs (see http://www.w3.org/Provider/Style/URI).
    Defaults to nil.

If you create a site-map file, adjust the sorting like this:

  `:sitemap-sort-folders'

    Where folders should appear in the site-map.  Set this to
    `first' (default) or `last' to display folders first or last,
    respectively.  Any other value will mix files and folders.

  `:sitemap-sort-files'

    The site map is normally sorted alphabetically.  You can
    change this behaviour setting this to `anti-chronologically',
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

(defcustom org-publish-sitemap-sort-folders 'first
  "A symbol, denoting if folders are sorted first in sitemaps.
Possible values are `first', `last', and nil.
If `first', folders will be sorted before files.
If `last', folders are sorted to the end after the files.
Any other value will not mix files and folders.

You can overwrite this default per project in your
`org-publish-project-alist', using `:sitemap-sort-folders'."
  :group 'org-export-publish
  :type 'symbol)

(defcustom org-publish-sitemap-sort-ignore-case nil
  "Non-nil when site-map sorting should ignore case.

You can overwrite this default per project in your
`org-publish-project-alist', using `:sitemap-ignore-case'."
  :group 'org-export-publish
  :type 'boolean)

(defcustom org-publish-sitemap-date-format "%Y-%m-%d"
  "Format for printing a date in the sitemap.
See `format-time-string' for allowed formatters."
  :group 'org-export-publish
  :type 'string)

(defcustom org-publish-sitemap-file-entry-format "%t"
  "Format string for site-map file entry.
You could use brackets to delimit on what part the link will be.

%t is the title.
%a is the author.
%d is the date formatted using `org-publish-sitemap-date-format'."
  :group 'org-export-publish
  :type 'string)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timestamp-related functions

(defun org-publish-timestamp-filename (filename &optional pub-dir pub-func)
  "Return path to timestamp file for filename FILENAME."
  (setq filename (concat filename "::" (or pub-dir "") "::"
			 (format "%s" (or pub-func ""))))
  (concat "X" (if (fboundp 'sha1) (sha1 filename) (md5 filename))))

(defun org-publish-needed-p
  (filename &optional pub-dir pub-func true-pub-dir base-dir)
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
	(message   "Skipping unmodified file %s" filename)))
    rtn))

(defun org-publish-update-timestamp
  (filename &optional pub-dir pub-func base-dir)
  "Update publishing timestamp for file FILENAME.
If there is no timestamp, create one."
  (let ((key (org-publish-timestamp-filename filename pub-dir pub-func))
	(stamp (org-publish-cache-ctime-of-src filename)))
    (org-publish-cache-set key stamp)))

(defun org-publish-remove-all-timestamps ()
  "Remove all files in the timestamp directory."
  (let ((dir org-publish-timestamp-directory)
	files)
    (when (and (file-exists-p dir) (file-directory-p dir))
      (mapc 'delete-file (directory-files dir 'full "[^.]\\'"))
      (org-publish-reset-cache))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting project information out of `org-publish-project-alist'

(defun org-publish-expand-projects (projects-alist)
  "Expand projects in PROJECTS-ALIST.
This splices all the components into the list."
  (let ((rest projects-alist) rtn p components)
    (while (setq p (pop rest))
      (if (setq components (plist-get (cdr p) :components))
	  (setq rest (append
		      (mapcar (lambda (x) (assoc x org-publish-project-alist))
			      components)
		      rest))
	(push p rtn)))
    (nreverse (delete-dups (delq nil rtn)))))

(defvar org-publish-sitemap-sort-files)
(defvar org-publish-sitemap-sort-folders)
(defvar org-publish-sitemap-ignore-case)
(defvar org-publish-sitemap-requested)
(defvar org-publish-sitemap-date-format)
(defvar org-publish-sitemap-file-entry-format)
(defun org-publish-compare-directory-files (a b)
  "Predicate for `sort', that sorts folders and files for sitemap."
  (let ((retval t))
    (when (or org-publish-sitemap-sort-files org-publish-sitemap-sort-folders)
      ;; First we sort files:
      (when org-publish-sitemap-sort-files
	(case org-publish-sitemap-sort-files
	  (alphabetically
	   (let* ((adir (file-directory-p a))
		  (aorg (and (string-match "\\.org$" a) (not adir)))
		  (bdir (file-directory-p b))
		  (borg (and (string-match "\\.org$" b) (not bdir)))
		  (A (if aorg (concat (file-name-directory a)
				      (org-publish-find-title a)) a))
		  (B (if borg (concat (file-name-directory b)
				      (org-publish-find-title b)) b)))
	     (setq retval (if org-publish-sitemap-ignore-case
			      (not (string-lessp (upcase B) (upcase A)))
			    (not (string-lessp B A))))))
	  ((anti-chronologically chronologically)
	   (let* ((adate (org-publish-find-date a))
		  (bdate (org-publish-find-date b))
		  (A (+ (lsh (car adate) 16) (cadr adate)))
		  (B (+ (lsh (car bdate) 16) (cadr bdate))))
	     (setq retval
		   (if (eq org-publish-sitemap-sort-files 'chronologically) (<= A B)
		     (>= A B)))))))
      ;; Directory-wise wins:
      (when org-publish-sitemap-sort-folders
        ;; a is directory, b not:
        (cond
         ((and (file-directory-p a) (not (file-directory-p b)))
          (setq retval (equal org-publish-sitemap-sort-folders 'first)))
	 ;; a is not a directory, but b is:
         ((and (not (file-directory-p a)) (file-directory-p b))
          (setq retval (equal org-publish-sitemap-sort-folders 'last))))))
    retval))

(defun org-publish-get-base-files-1
  (base-dir &optional recurse match skip-file skip-dir)
  "Set `org-publish-temp-files' with files from BASE-DIR directory.
If RECURSE is non-nil, check BASE-DIR recursively.  If MATCH is
non-nil, restrict this list to the files matching the regexp
MATCH.  If SKIP-FILE is non-nil, skip file matching the regexp
SKIP-FILE.  If SKIP-DIR is non-nil, don't check directories
matching the regexp SKIP-DIR when recursing through BASE-DIR."
  (mapc (lambda (f)
	  (let ((fd-p (file-directory-p f))
		(fnd (file-name-nondirectory f)))
	    (if (and fd-p recurse
		     (not (string-match "^\\.+$" fnd))
		     (if skip-dir (not (string-match skip-dir fnd)) t))
		(org-publish-get-base-files-1
		 f recurse match skip-file skip-dir)
	      (unless (or fd-p ;; this is a directory
			  (and skip-file (string-match skip-file fnd))
			  (not (file-exists-p (file-truename f)))
			  (not (string-match match fnd)))

		(pushnew f org-publish-temp-files)))))
	(let ((all-files (if (not recurse) (directory-files base-dir t match)
			   ;; If RECURSE is non-nil, we want all files
			   ;; matching MATCH and sub-directories.
			   (org-remove-if-not
			    (lambda (file)
			      (or (file-directory-p file)
				  (and match (string-match match file))))
			    (directory-files base-dir t)))))
	  (if (not org-publish-sitemap-requested) all-files
	    (sort all-files 'org-publish-compare-directory-files)))))

(defun org-publish-get-base-files (project &optional exclude-regexp)
  "Return a list of all files in PROJECT.
If EXCLUDE-REGEXP is set, this will be used to filter out
matching filenames."
  (let* ((project-plist (cdr project))
	 (base-dir (file-name-as-directory
		    (plist-get project-plist :base-directory)))
	 (include-list (plist-get project-plist :include))
	 (recurse (plist-get project-plist :recursive))
	 (extension (or (plist-get project-plist :base-extension) "org"))
	 ;; sitemap-... variables are dynamically scoped for
	 ;; org-publish-compare-directory-files:
	 (org-publish-sitemap-requested
	  (plist-get project-plist :auto-sitemap))
	 (sitemap-filename
	  (or (plist-get project-plist :sitemap-filename) "sitemap.org"))
	 (org-publish-sitemap-sort-folders
	  (if (plist-member project-plist :sitemap-sort-folders)
	      (plist-get project-plist :sitemap-sort-folders)
	    org-publish-sitemap-sort-folders))
	 (org-publish-sitemap-sort-files
	  (cond ((plist-member project-plist :sitemap-sort-files)
		 (plist-get project-plist :sitemap-sort-files))
		;; For backward compatibility:
		((plist-member project-plist :sitemap-alphabetically)
		 (if (plist-get project-plist :sitemap-alphabetically)
		     'alphabetically nil))
		(t org-publish-sitemap-sort-files)))
	 (org-publish-sitemap-ignore-case
	  (if (plist-member project-plist :sitemap-ignore-case)
	      (plist-get project-plist :sitemap-ignore-case)
	    org-publish-sitemap-sort-ignore-case))
	 (match (if (eq extension 'any) "^[^\\.]"
		  (concat "^[^\\.].*\\.\\(" extension "\\)$"))))
    ;; Make sure `org-publish-sitemap-sort-folders' has an accepted
    ;; value.
    (unless (memq org-publish-sitemap-sort-folders '(first last))
      (setq org-publish-sitemap-sort-folders nil))

    (setq org-publish-temp-files nil)
    (if org-publish-sitemap-requested
	(pushnew (expand-file-name (concat base-dir sitemap-filename))
		  org-publish-temp-files))
    (org-publish-get-base-files-1 base-dir recurse match
				  ;; FIXME distinguish exclude regexp
				  ;; for skip-file and skip-dir?
				  exclude-regexp exclude-regexp)
    (mapc (lambda (f)
	    (pushnew
	     (expand-file-name (concat base-dir f))
	     org-publish-temp-files))
	  include-list)
    org-publish-temp-files))

(defun org-publish-get-project-from-filename (filename &optional up)
  "Return the project that FILENAME belongs to."
  (let* ((filename (expand-file-name filename))
	 project-name)

    (catch 'p-found
      (dolist (prj org-publish-project-alist)
	(unless (plist-get (cdr prj) :components)
	  ;; [[info:org:Selecting%20files]] shows how this is supposed to work:
	  (let* ((r (plist-get (cdr prj) :recursive))
		 (b (expand-file-name (file-name-as-directory
				       (plist-get (cdr prj) :base-directory))))
		 (x (or (plist-get (cdr prj) :base-extension) "org"))
		 (e (plist-get (cdr prj) :exclude))
		 (i (plist-get (cdr prj) :include))
		 (xm (concat "^" b (if r ".+" "[^/]+") "\\.\\(" x "\\)$")))
	    (when
		(or (and i
			 (member filename
				 (mapcar (lambda (file)
					   (expand-file-name file b))
					 i)))
		    (and (not (and e (string-match e filename)))
			 (string-match xm filename)))
	      (setq project-name (car prj))
	      (throw 'p-found project-name))))))
    (when up
      (dolist (prj org-publish-project-alist)
	(if (member project-name (plist-get (cdr prj) :components))
	    (setq project-name (car prj)))))
    (assoc project-name org-publish-project-alist)))



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
	 (visitingp (find-buffer-visiting filename))
	 (work-buffer (or visitingp (find-file-noselect filename))))
    (prog1 (with-current-buffer work-buffer
	     (let ((output-file
		    (org-export-output-file-name extension nil pub-dir))
		   (body-p (plist-get plist :body-only)))
	       (org-export-to-file backend output-file
		 nil nil nil body-p
		 ;; Add `org-publish-collect-numbering' and
		 ;; `org-publish-collect-index' to final output
		 ;; filters.  The latter isn't dependent on
		 ;; `:makeindex', since we want to keep it up-to-date
		 ;; in cache anyway.
		 (org-combine-plists
		  plist
		  `(:filter-final-output
		    ,(cons 'org-publish-collect-numbering
			   (cons 'org-publish-collect-index
				 (plist-get plist :filter-final-output))))))))
      ;; Remove opened buffer in the process.
      (unless visitingp (kill-buffer work-buffer)))))

(defun org-publish-attachment (plist filename pub-dir)
  "Publish a file with no transformation of any kind.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (unless (file-directory-p pub-dir)
    (make-directory pub-dir t))
  (or (equal (expand-file-name (file-name-directory filename))
	     (file-name-as-directory (expand-file-name pub-dir)))
      (copy-file filename
		 (expand-file-name (file-name-nondirectory filename) pub-dir)
		 t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publishing files, sets of files, and indices

(defun org-publish-file (filename &optional project no-cache)
  "Publish file FILENAME from PROJECT.
If NO-CACHE is not nil, do not initialize org-publish-cache and
write it to disk.  This is needed, since this function is used to
publish single files, when entire projects are published.
See `org-publish-projects'."
  (let* ((project
	  (or project
	      (or (org-publish-get-project-from-filename filename)
		  (error "File %s not part of any known project"
			 (abbreviate-file-name filename)))))
	 (project-plist (cdr project))
	 (ftname (expand-file-name filename))
	 (publishing-function
	  (or (plist-get project-plist :publishing-function)
	      (error "No publishing function chosen")))
	 (base-dir
	  (file-name-as-directory
	   (expand-file-name
	    (or (plist-get project-plist :base-directory)
		(error "Project %s does not have :base-directory defined"
		       (car project))))))
	 (pub-dir
	  (file-name-as-directory
	   (file-truename
	    (or (eval (plist-get project-plist :publishing-directory))
		(error "Project %s does not have :publishing-directory defined"
		       (car project))))))
	 tmp-pub-dir)

    (unless no-cache (org-publish-initialize-cache (car project)))

    (setq tmp-pub-dir
	  (file-name-directory
	   (concat pub-dir
		   (and (string-match (regexp-quote base-dir) ftname)
			(substring ftname (match-end 0))))))
    (if (listp publishing-function)
	;; allow chain of publishing functions
	(mapc (lambda (f)
		(when (org-publish-needed-p
		       filename pub-dir f tmp-pub-dir base-dir)
		  (funcall f project-plist filename tmp-pub-dir)
		  (org-publish-update-timestamp filename pub-dir f base-dir)))
	      publishing-function)
      (when (org-publish-needed-p
	     filename pub-dir publishing-function tmp-pub-dir base-dir)
	(funcall publishing-function project-plist filename tmp-pub-dir)
	(org-publish-update-timestamp
	 filename pub-dir publishing-function base-dir)))
    (unless no-cache (org-publish-write-cache-file))))

(defun org-publish-projects (projects)
  "Publish all files belonging to the PROJECTS alist.
If `:auto-sitemap' is set, publish the sitemap too.  If
`:makeindex' is set, also produce a file theindex.org."
  (mapc
   (lambda (project)
     ;; Each project uses its own cache file:
     (org-publish-initialize-cache (car project))
     (let* ((project-plist (cdr project))
	    (exclude-regexp (plist-get project-plist :exclude))
	    (sitemap-p (plist-get project-plist :auto-sitemap))
	    (sitemap-filename (or (plist-get project-plist :sitemap-filename)
				  "sitemap.org"))
	    (sitemap-function (or (plist-get project-plist :sitemap-function)
				  'org-publish-org-sitemap))
	    (org-publish-sitemap-date-format
	     (or (plist-get project-plist :sitemap-date-format)
		 org-publish-sitemap-date-format))
	    (org-publish-sitemap-file-entry-format
	     (or (plist-get project-plist :sitemap-file-entry-format)
		 org-publish-sitemap-file-entry-format))
	    (preparation-function
	     (plist-get project-plist :preparation-function))
	    (completion-function (plist-get project-plist :completion-function))
	    (files (org-publish-get-base-files project exclude-regexp))
	    (theindex
	     (expand-file-name "theindex.org"
			       (plist-get project-plist :base-directory))))
       (when preparation-function (run-hooks 'preparation-function))
       (if sitemap-p (funcall sitemap-function project sitemap-filename))
       ;; Publish all files from PROJECT excepted "theindex.org".  Its
       ;; publishing will be deferred until "theindex.inc" is
       ;; populated.
       (dolist (file files)
	 (unless (equal file theindex)
	   (org-publish-file file project t)))
       ;; Populate "theindex.inc", if needed, and publish
       ;; "theindex.org".
       (when (plist-get project-plist :makeindex)
	 (org-publish-index-generate-theindex
	  project (plist-get project-plist :base-directory))
	 (org-publish-file theindex project t))
       (when completion-function (run-hooks 'completion-function))
       (org-publish-write-cache-file)))
   (org-publish-expand-projects projects)))

(defun org-publish-org-sitemap (project &optional sitemap-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'."
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory
	       (plist-get project-plist :base-directory)))
	 (localdir (file-name-directory dir))
	 (indent-str (make-string 2 ?\ ))
	 (exclude-regexp (plist-get project-plist :exclude))
	 (files (nreverse
		 (org-publish-get-base-files project exclude-regexp)))
	 (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
	 (sitemap-title (or (plist-get project-plist :sitemap-title)
			  (concat "Sitemap for project " (car project))))
	 (sitemap-style (or (plist-get project-plist :sitemap-style)
			    'tree))
	 (sitemap-sans-extension
	  (plist-get project-plist :sitemap-sans-extension))
	 (visiting (find-buffer-visiting sitemap-filename))
	 (ifn (file-name-nondirectory sitemap-filename))
	 file sitemap-buffer)
    (with-current-buffer
	(let ((org-inhibit-startup t))
	  (setq sitemap-buffer
		(or visiting (find-file sitemap-filename))))
      (erase-buffer)
      (insert (concat "#+TITLE: " sitemap-title "\n\n"))
      (while (setq file (pop files))
	(let ((fn (file-name-nondirectory file))
	      (link (file-relative-name file dir))
	      (oldlocal localdir))
	  (when sitemap-sans-extension
	    (setq link (file-name-sans-extension link)))
	  ;; sitemap shouldn't list itself
	  (unless (equal (file-truename sitemap-filename)
			 (file-truename file))
	    (if (eq sitemap-style 'list)
		(message "Generating list-style sitemap for %s" sitemap-title)
	      (message "Generating tree-style sitemap for %s" sitemap-title)
	      (setq localdir (concat (file-name-as-directory dir)
				     (file-name-directory link)))
	      (unless (string= localdir oldlocal)
		(if (string= localdir dir)
		    (setq indent-str (make-string 2 ?\ ))
		  (let ((subdirs
			 (split-string
			  (directory-file-name
			   (file-name-directory
			    (file-relative-name localdir dir))) "/"))
			(subdir "")
			(old-subdirs (split-string
				      (file-relative-name oldlocal dir) "/")))
		    (setq indent-str (make-string 2 ?\ ))
		    (while (string= (car old-subdirs) (car subdirs))
		      (setq indent-str (concat indent-str (make-string 2 ?\ )))
		      (pop old-subdirs)
		      (pop subdirs))
		    (dolist (d subdirs)
		      (setq subdir (concat subdir d "/"))
		      (insert (concat indent-str " + " d "\n"))
		      (setq indent-str (make-string
					(+ (length indent-str) 2) ?\ )))))))
	    ;; This is common to 'flat and 'tree
	    (let ((entry
		   (org-publish-format-file-entry
		    org-publish-sitemap-file-entry-format file project-plist))
		  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
	      (cond ((string-match-p regexp entry)
		     (string-match regexp entry)
		     (insert (concat indent-str " + " (match-string 1 entry)
				     "[[file:" link "]["
				     (match-string 2 entry)
				     "]]" (match-string 3 entry) "\n")))
		    (t
		     (insert (concat indent-str " + [[file:" link "]["
				     entry
				     "]]\n"))))))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(defun org-publish-format-file-entry (fmt file project-plist)
  (format-spec
   fmt
   `((?t . ,(org-publish-find-title file t))
     (?d . ,(format-time-string org-publish-sitemap-date-format
				(org-publish-find-date file)))
     (?a . ,(or (plist-get project-plist :author) user-full-name)))))

(defun org-publish-find-title (file &optional reset)
  "Find the title of FILE in project."
  (or
   (and (not reset) (org-publish-cache-get-file-property file :title nil t))
   (let* ((org-inhibit-startup t)
	  (visiting (find-buffer-visiting file))
	  (buffer (or visiting (find-file-noselect file))))
     (with-current-buffer buffer
       (org-mode)
       (let ((title
	      (let ((property (plist-get (org-export-get-environment) :title)))
		(if property
		    (org-no-properties (org-element-interpret-data property))
		  (file-name-nondirectory (file-name-sans-extension file))))))
	 (unless visiting (kill-buffer buffer))
	 (org-publish-cache-set-file-property file :title title)
	 title)))))

(defun org-publish-find-date (file)
  "Find the date of FILE in project.
This function assumes FILE is either a directory or an Org file.
If FILE is an Org file and provides a DATE keyword use it.  In
any other case use the file system's modification time.  Return
time in `current-time' format."
  (if (file-directory-p file) (nth 5 (file-attributes file))
    (let* ((visiting (find-buffer-visiting file))
	   (file-buf (or visiting (find-file-noselect file nil)))
	   (date (plist-get
		  (with-current-buffer file-buf
		    (let ((org-inhibit-startup t)) (org-mode))
		    (org-export-get-environment))
		  :date)))
      (unless visiting (kill-buffer file-buf))
      ;; DATE is either a timestamp object or a secondary string.  If it
      ;; is a timestamp or if the secondary string contains a timestamp,
      ;; convert it to internal format.  Otherwise, use FILE
      ;; modification time.
      (cond ((eq (org-element-type date) 'timestamp)
	     (org-time-string-to-time (org-element-interpret-data date)))
	    ((let ((ts (and (consp date) (assq 'timestamp date))))
	       (and ts
		    (let ((value (org-element-interpret-data ts)))
		      (and (org-string-nw-p value)
			   (org-time-string-to-time value))))))
	    ((file-exists-p file) (nth 5 (file-attributes file)))
	    (t (error "No such file: \"%s\"" file))))))



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
   (list
    (assoc (org-icompleting-read
	    "Publish project: "
	    org-publish-project-alist nil t)
	   org-publish-project-alist)
    current-prefix-arg))
  (let ((project-alist  (if (not (stringp project)) (list project)
			  ;; If this function is called in batch mode,
			  ;; project is still a string here.
			  (list (assoc project org-publish-project-alist)))))
    (if async
	(org-export-async-start 'ignore
	  `(let ((org-publish-use-timestamps-flag
		  (if ',force nil ,org-publish-use-timestamps-flag)))
	     (org-publish-projects ',project-alist)))
      (save-window-excursion
	(let* ((org-publish-use-timestamps-flag
		(if force nil org-publish-use-timestamps-flag)))
	  (org-publish-projects project-alist))))))

;;;###autoload
(defun org-publish-all (&optional force async)
  "Publish all projects.
With prefix argument FORCE, remove all files in the timestamp
directory and force publishing all projects.  With a non-nil
optional argument ASYNC, publishing will be done asynchronously,
in another process."
  (interactive "P")
  (if async
      (org-export-async-start 'ignore
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
	(org-export-async-start 'ignore
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

(defun org-publish-collect-index (output backend info)
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
  (let ((file (plist-get info :input-file)))
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
  (let ((all-files (org-publish-get-base-files
		    project (plist-get (cdr project) :exclude)))
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
	    (let* ((rank (if (equal entry last-entry) (1- (length entry))
			   (loop for n from 0 to (length entry)
				 unless (equal (nth n entry) (nth n last-entry))
				 return n)))
		   (len (length (nthcdr rank entry))))
	      ;; For each term after the first difference, create
	      ;; a new sub-list with the term as body.  Moreover,
	      ;; linkify the last term.
	      (dotimes (n len)
		(insert
		 (concat
		  (make-string (* (+ rank n) 2) ? ) "  - "
		  (if (not (= (1- len) n)) (nth (+ rank n) entry)
		    ;; Last term: Link it to TARGET, if possible.
		    (let ((target (nth 2 idx)))
		      (format
		       "[[%s][%s]]"
		       ;; Destination.
		       (case (car target)
			 ('nil (format "file:%s" file))
			 (id (format "id:%s" (cdr target)))
			 (custom-id (format "file:%s::#%s" file (cdr target)))
			 (otherwise (format "file:%s::*%s" file (cdr target))))
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

(defun org-publish-collect-numbering (output backend info)
  (org-publish-cache-set-file-property
   (plist-get info :input-file) :numbering
   (mapcar (lambda (entry)
	     (cons (org-split-string
		    (replace-regexp-in-string
		     "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
		     (org-element-property :raw-value (car entry))))
		   (cdr entry)))
	   (plist-get info :headline-numbering)))
  ;; Return output unchanged.
  output)

(defun org-publish-resolve-external-fuzzy-link (file fuzzy)
  "Return numbering for headline matching FUZZY search in FILE.

Return value is a list of numbers, or nil.  This function allows
to resolve external fuzzy links like:

  [[file.org::*fuzzy][description]]"
  (when org-publish-cache
    (cdr (assoc (org-split-string
		 (if (eq (aref fuzzy 0) ?*) (substring fuzzy 1) fuzzy))
		(org-publish-cache-get-file-property
		 (expand-file-name file) :numbering nil t)))))



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
	(insert "(setq org-publish-cache (make-hash-table :test 'equal :weakness nil :size 100))\n")
	(maphash (lambda (k v)
		   (insert
		    (format (concat "(puthash %S "
				    (if (or (listp v) (symbolp v))
					"'" "")
				    "%S org-publish-cache)\n") k v)))
		 org-publish-cache)))
    (when free-cache (org-publish-reset-cache))))

(defun org-publish-initialize-cache (project-name)
  "Initialize the projects cache if not initialized yet and return it."

  (unless project-name
    (error "Cannot initialize `org-publish-cache' without projects name in `org-publish-initialize-cache'"))

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
  (filename &optional pub-dir pub-func base-dir)
  "Check the timestamp of the last publishing of FILENAME.
Return non-nil if the file needs publishing.  Also check if
any included files have been more recently published, so that
the file including them will be republished as well."
  (unless org-publish-cache
    (error
     "`org-publish-cache-file-needs-publishing' called, but no cache present"))
  (let* ((case-fold-search t)
	 (key (org-publish-timestamp-filename filename pub-dir pub-func))
	 (pstamp (org-publish-cache-get key))
	 (org-inhibit-startup t)
	 (visiting (find-buffer-visiting filename))
	 included-files-ctime buf)

    (when (equal (file-name-extension filename) "org")
      (setq buf (find-file (expand-file-name filename)))
      (with-current-buffer buf
	(goto-char (point-min))
	(while (re-search-forward
		"^#\\+INCLUDE:[ \t]+\"\\([^\t\n\r\"]*\\)\"[ \t]*.*$" nil t)
	  (let* ((included-file (expand-file-name (match-string 1))))
	    (add-to-list 'included-files-ctime
			 (org-publish-cache-ctime-of-src included-file) t))))
      (unless visiting (kill-buffer buf)))
    (if (null pstamp) t
      (let ((ctime (org-publish-cache-ctime-of-src filename)))
	(or (< pstamp ctime)
	    (when included-files-ctime
	      (not (null (delq nil (mapcar (lambda(ct) (< ctime ct))
					   included-files-ctime))))))))))

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
Use cache file of PROJECT-NAME. Return the value of that PROPERTY
or DEFAULT, if the value does not yet exist.  If the entry will
be created, unless NO-CREATE is not nil."
  ;; Evtl. load the requested cache file:
  (if project-name (org-publish-initialize-cache project-name))
  (let ((pl (org-publish-cache-get filename)) retval)
    (if pl
	(if (plist-member pl property)
	    (setq retval (plist-get pl property))
	  (setq retval default))
      ;; no pl yet:
      (unless no-create
	(org-publish-cache-set filename (list property default)))
      (setq retval default))
    retval))

(defun org-publish-cache-get (key)
  "Return the value stored in `org-publish-cache' for key KEY.
Returns nil, if no value or nil is found, or the cache does not
exist."
  (unless org-publish-cache
    (error "`org-publish-cache-get' called, but no cache present"))
  (gethash key org-publish-cache))

(defun org-publish-cache-set (key value)
  "Store KEY VALUE pair in `org-publish-cache'.
Returns value on success, else nil."
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

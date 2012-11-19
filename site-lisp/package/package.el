;;; package.el --- Simple package system for Emacs
;;;

;; Copyright (C) 2007-2011 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 10 Mar 2007
;; Version: 0.9
;; Keywords: tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Change Log:

;;  2 Apr 2007 - now using ChangeLog file
;; 15 Mar 2007 - updated documentation
;; 14 Mar 2007 - Changed how obsolete packages are handled
;; 13 Mar 2007 - Wrote package-install-from-buffer
;; 12 Mar 2007 - Wrote package-menu mode

;;; Commentary:

;; The idea behind package.el is to be able to download packages and
;; install them.  Packages are versioned and have versioned
;; dependencies.  Furthermore, this supports built-in packages which
;; may or may not be newer than user-specified packages.  This makes
;; it possible to upgrade Emacs and automatically disable packages
;; which have moved from external to core.  (Note though that we don't
;; currently register any of these, so this feature does not actually
;; work.)

;; A package is described by its name and version.  The distribution
;; format is either  a tar file or a single .el file.

;; A tar file should be named "NAME-VERSION.tar".  The tar file must
;; unpack into a directory named after the package and version:
;; "NAME-VERSION".  It must contain a file named "PACKAGE-pkg.el"
;; which consists of a call to define-package.  It may also contain a
;; "dir" file and the info files it references.

;; A .el file is named "NAME-VERSION.el" in the remote archive, but is
;; installed as simply "NAME.el" in a directory named "NAME-VERSION".

;; The downloader downloads all dependent packages.  By default,
;; packages come from the official GNU sources, but others may be
;; added by customizing the `package-archives' alist.  Packages get
;; byte-compiled at install time.

;; At activation time we will set up the load-path and the info path,
;; and we will load the package's autoloads.  If a package's
;; dependencies are not available, we will not activate that package.

;; Conceptually a package has multiple state transitions:
;;
;; * Download.  Fetching the package from ELPA.
;; * Install.  Untar the package, or write the .el file, into
;;   ~/.emacs.d/elpa/ directory.
;; * Byte compile.  Currently this phase is done during install,
;;   but we may change this.
;; * Activate.  Evaluate the autoloads for the package to make it
;;   available to the user.
;; * Load.  Actually load the package and run some code from it.

;; Other external functions you may want to use:
;;
;; M-x list-packages
;;    Enters a mode similar to buffer-menu which lets you manage
;;    packages.  You can choose packages for install (mark with "i",
;;    then "x" to execute) or deletion (not implemented yet), and you
;;    can see what packages are available.  This will automatically
;;    fetch the latest list of packages from ELPA.
;;
;; M-x package-list-packages-no-fetch
;;    Like package-list-packages, but does not automatically fetch the
;;    new list of packages.
;;
;; M-x package-install-from-buffer
;;    Install a package consisting of a single .el file that appears
;;    in the current buffer.  This only works for packages which
;;    define a Version header properly; package.el also supports the
;;    extension headers Package-Version (in case Version is an RCS id
;;    or similar), and Package-Requires (if the package requires other
;;    packages).
;;
;; M-x package-install-file
;;    Install a package from the indicated file.  The package can be
;;    either a tar file or a .el file.  A tar file must contain an
;;    appropriately-named "-pkg.el" file; a .el file must be properly
;;    formatted as with package-install-from-buffer.

;;; Thanks:
;;; (sorted by sort-lines):

;; Jim Blandy <jimb@red-bean.com>
;; Karl Fogel <kfogel@red-bean.com>
;; Kevin Ryde <user42@zip.com.au>
;; Lawrence Mitchell
;; Michael Olson <mwolson@member.fsf.org>
;; Sebastian Tennant <sebyte@smolny.plus.com>
;; Stefan Monnier <monnier@iro.umontreal.ca>
;; Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Phil Hagelberg <phil@hagelb.org>

;;; ToDo:

;; - putting info dirs at the start of the info path means
;;   users see a weird ordering of categories.  OTOH we want to
;;   override later entries.  maybe emacs needs to enforce
;;   the standard layout?
;; - put bytecode in a separate directory tree
;; - perhaps give users a way to recompile their bytecode
;;   or do it automatically when emacs changes
;; - give users a way to know whether a package is installed ok
;; - give users a way to view a package's documentation when it
;;   only appears in the .el
;; - use/extend checkdoc so people can tell if their package will work
;; - "installed" instead of a blank in the status column
;; - tramp needs its files to be compiled in a certain order.
;;   how to handle this?  fix tramp?
;; - on emacs 21 we don't kill the -autoloads.el buffer.  what about 22?
;; - maybe we need separate .elc directories for various emacs versions
;;   and also emacs-vs-xemacs.  That way conditional compilation can
;;   work.  But would this break anything?
;; - should store the package's keywords in archive-contents, then
;;   let the users filter the package-menu by keyword.  See
;;   finder-by-keyword.  (We could also let people view the
;;   Commentary, but it isn't clear how useful this is.)
;; - William Xu suggests being able to open a package file without
;;   installing it
;; - Interface with desktop.el so that restarting after an install
;;   works properly
;; - Implement M-x package-upgrade, to upgrade any/all existing packages
;; - Use hierarchical layout.  PKG/etc PKG/lisp PKG/info
;;   ... except maybe lisp?
;; - It may be nice to have a macro that expands to the package's
;;   private data dir, aka ".../etc".  Or, maybe data-directory
;;   needs to be a list (though this would be less nice)
;;   a few packages want this, eg sokoban
;; - package menu needs:
;;     ability to know which packages are built-in & thus not deletable
;;     it can sometimes print odd results, like 0.3 available but 0.4 active
;;        why is that?
;; - Allow multiple versions on the server...?
;;   [ why bother? ]
;; - Don't install a package which will invalidate dependencies overall
;; - Allow something like (or (>= emacs 21.0) (>= xemacs 21.5))
;;   [ currently thinking, why bother.. KISS ]
;; - Allow optional package dependencies
;;   then if we require 'bbdb', bbdb-specific lisp in lisp/bbdb
;;   and just don't compile to add to load path ...?
;; - Have a list of archive URLs?  [ maybe there's no point ]
;; - David Kastrup pointed out on the xemacs list that for GPL it
;;   is friendlier to ship the source tree.  We could "support" that
;;   by just having a "src" subdir in the package.  This isn't ideal
;;   but it probably is not worth trying to support random source
;;   tree layouts, build schemes, etc.
;; - Our treatment of the info path is somewhat bogus
;; - perhaps have an "unstable" tree in ELPA as well as a stable one

;;; Code:

(defgroup package nil
  "Manager for Emacs Lisp packages."
  :group 'applications
  :version "24.1")

;;;###autoload
(defcustom package-enable-at-startup t
  "Whether to activate installed packages when Emacs starts.
If non-nil, packages are activated after reading the init file
and before `after-init-hook'.  Activation is not done if
`user-init-file' is nil (e.g. Emacs was started with \"-q\").

Even if the value is nil, you can type \\[package-initialize] to
activate the package system at any time."
  :type 'boolean
  :group 'package
  :version "24.1")

(defcustom package-load-list '(all)
  "List of packages for `package-initialize' to load.
Each element in this list should be a list (NAME VERSION), or the
symbol `all'.  The symbol `all' says to load the latest installed
versions of all packages not specified by other elements.

For an element (NAME VERSION), NAME is a package name (a symbol).
VERSION should be t, a string, or nil.
If VERSION is t, all versions are loaded, though obsolete ones
 will be put in `package-obsolete-alist' and not activated.
If VERSION is a string, only that version is ever loaded.
 Any other version, even if newer, is silently ignored.
 Hence, the package is \"held\" at that version.
If VERSION is nil, the package is not loaded (it is \"disabled\")."
  :type '(repeat symbol)
  :risky t
  :group 'package
  :version "24.1")

(defvar Info-directory-list)
(declare-function info-initialize "info" ())
(declare-function url-http-parse-response "url-http" ())
(declare-function lm-header "lisp-mnt" (header))
(declare-function lm-commentary "lisp-mnt" (&optional file))
(defvar url-http-end-of-headers)

(defcustom package-archives '(("gnu" . "http://elpa.gnu.org/packages/"))
  "An alist of archives from which to fetch.
The default value points to the GNU Emacs package repository.

Each element has the form (ID . LOCATION).
 ID is an archive name, as a string.
 LOCATION specifies the base location for the archive.
  If it starts with \"http:\", it is treated as a HTTP URL;
  otherwise it should be an absolute directory name.
  (Other types of URL are currently not supported.)"
  :type '(alist :key-type (string :tag "Archive name")
                :value-type (string :tag "URL or directory name"))
  :risky t
  :group 'package
  :version "24.1")

(defconst package-archive-version 1
  "Version number of the package archive understood by this file.
Lower version numbers than this will probably be understood as well.")

(defconst package-el-version "1.0"
  "Version of package.el.")

;; We don't prime the cache since it tends to get out of date.
(defvar package-archive-contents nil
  "Cache of the contents of the Emacs Lisp Package Archive.
This is an alist mapping package names (symbols) to package
descriptor vectors.  These are like the vectors for `package-alist'
but have extra entries: one which is 'tar for tar packages and
'single for single-file packages, and one which is the name of
the archive from which it came.")
(put 'package-archive-contents 'risky-local-variable t)

(defcustom package-user-dir (locate-user-emacs-file "elpa")
  "Directory containing the user's Emacs Lisp packages.
The directory name should be absolute.
Apart from this directory, Emacs also looks for system-wide
packages in `package-directory-list'."
  :type 'directory
  :risky t
  :group 'package
  :version "24.1")

(defcustom package-directory-list
  ;; Defaults are subdirs named "elpa" in the site-lisp dirs.
  (let (result)
    (dolist (f load-path)
      (and (stringp f)
	   (equal (file-name-nondirectory f) "site-lisp")
	   (push (expand-file-name "elpa" f) result)))
    (nreverse result))
  "List of additional directories containing Emacs Lisp packages.
Each directory name should be absolute.

These directories contain packages intended for system-wide; in
contrast, `package-user-dir' contains packages for personal use."
  :type '(repeat directory)
  :risky t
  :group 'package
  :version "24.1")

;; The value is precomputed in finder-inf.el, but don't load that
;; until it's needed (i.e. when `package-intialize' is called).
(defvar package--builtins nil
  "Alist of built-in packages.
The actual value is initialized by loading the library
`finder-inf'; this is not done until it is needed, e.g. by the
function `package-built-in-p'.

Each element has the form (PKG . DESC), where PKG is a package
name (a symbol) and DESC is a vector that describes the package.
The vector DESC has the form [VERSION REQS DOCSTRING].
  VERSION is a version list.
  REQS is a list of packages (symbols) required by the package.
  DOCSTRING is a brief description of the package.")
(put 'package--builtins 'risky-local-variable t)

(defvar package-alist nil
  "Alist of all packages available for activation.
Each element has the form (PKG . DESC), where PKG is a package
name (a symbol) and DESC is a vector that describes the package.

The vector DESC has the form [VERSION REQS DOCSTRING].
  VERSION is a version list.
  REQS is a list of packages (symbols) required by the package.
  DOCSTRING is a brief description of the package.

This variable is set automatically by `package-load-descriptor',
called via `package-initialize'.  To change which packages are
loaded and/or activated, customize `package-load-list'.")
(put 'package-archive-contents 'risky-local-variable t)

(defvar package-activated-list nil
  "List of the names of currently activated packages.")
(put 'package-activated-list 'risky-local-variable t)

(defvar package-obsolete-alist nil
  "Representation of obsolete packages.
Like `package-alist', but maps package name to a second alist.
The inner alist is keyed by version.")
(put 'package-obsolete-alist 'risky-local-variable t)

(defconst package-subdirectory-regexp
  "\\`\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\)[0-9]+\\)*\\)\\'"
  "Regular expression matching the name of a package subdirectory.
The first subexpression is the package name.
The second subexpression is the version string.")

(defun package-version-join (vlist)
  "Return the version string corresponding to the list VLIST.
This is, approximately, the inverse of `version-to-list'.
\(Actually, it returns only one of the possible inverses, since
`version-to-list' is a many-to-one operation.)"
  (if (null vlist)
      ""
    (let ((str-list (list "." (int-to-string (car vlist)))))
      (dolist (num (cdr vlist))
	(cond
	 ((>= num 0)
	  (push (int-to-string num) str-list)
	  (push "." str-list))
	 ((< num -3)
	  (error "Invalid version list `%s'" vlist))
	 (t
	  ;; pre, or beta, or alpha
	  (cond ((equal "." (car str-list))
		 (pop str-list))
		((not (string-match "[0-9]+" (car str-list)))
		 (error "Invalid version list `%s'" vlist)))
	  (push (cond ((= num -1) "pre")
		      ((= num -2) "beta")
		      ((= num -3) "alpha"))
		str-list))))
      (if (equal "." (car str-list))
	  (pop str-list))
      (apply 'concat (nreverse str-list)))))

(defun package-strip-version (dirname)
  "Strip the version from a combined package name and version.
E.g., if given \"quux-23.0\", will return \"quux\""
  (if (string-match package-subdirectory-regexp dirname)
      (match-string 1 dirname)))

(defun package-load-descriptor (dir package)
  "Load the description file in directory DIR for package PACKAGE.
Here, PACKAGE is a string of the form NAME-VER, where NAME is the
package name and VER is its version."
  (let* ((pkg-dir (expand-file-name package dir))
	 (pkg-file (expand-file-name
		    (concat (package-strip-version package) "-pkg")
		    pkg-dir)))
    (when (and (file-directory-p pkg-dir)
	       (file-exists-p (concat pkg-file ".el")))
      (load pkg-file nil t))))

(defun package-load-all-descriptors ()
  "Load descriptors for installed Emacs Lisp packages.
This looks for package subdirectories in `package-user-dir' and
`package-directory-list'.  The variable `package-load-list'
controls which package subdirectories may be loaded.

In each valid package subdirectory, this function loads the
description file containing a call to `define-package', which
updates `package-alist' and `package-obsolete-alist'."
  (let ((all (memq 'all package-load-list))
	name version force)
    (dolist (dir (cons package-user-dir package-directory-list))
      (when (file-directory-p dir)
	(dolist (subdir (directory-files dir))
	  (when (and (file-directory-p (expand-file-name subdir dir))
		     (string-match package-subdirectory-regexp subdir))
	    (setq name    (intern (match-string 1 subdir))
		  version (match-string 2 subdir)
		  force   (assq name package-load-list))
	    (when (cond
		   ((null force)
		    all) ; not in package-load-list
		   ((null (setq force (cadr force)))
		    nil) ; disabled
		   ((eq force t)
		    t)
		   ((stringp force) ; held
		    (version-list-= (version-to-list version)
				    (version-to-list force)))
		   (t
		    (error "Invalid element in `package-load-list'")))
	      (package-load-descriptor dir subdir))))))))

(defsubst package-desc-vers (desc)
  "Extract version from a package description vector."
  (aref desc 0))

(defsubst package-desc-reqs (desc)
  "Extract requirements from a package description vector."
  (aref desc 1))

(defsubst package-desc-doc (desc)
  "Extract doc string from a package description vector."
  (aref desc 2))

(defsubst package-desc-kind (desc)
  "Extract the kind of download from an archive package description vector."
  (aref desc 3))

(defun package--dir (name version)
  "Return the directory where a package is installed, or nil if none.
NAME and VERSION are both strings."
  (let* ((subdir (concat name "-" version))
	 (dir-list (cons package-user-dir package-directory-list))
	 pkg-dir)
    (while dir-list
      (let ((subdir-full (expand-file-name subdir (car dir-list))))
	(if (file-directory-p subdir-full)
	    (setq pkg-dir  subdir-full
		  dir-list nil)
	  (setq dir-list (cdr dir-list)))))
    pkg-dir))

(defun package-activate-1 (package pkg-vec)
  (let* ((name (symbol-name package))
	 (version-str (package-version-join (package-desc-vers pkg-vec)))
	 (pkg-dir (package--dir name version-str)))
    (unless pkg-dir
      (error "Internal error: unable to find directory for `%s-%s'"
	     name version-str))
    ;; Add info node.
    (when (file-exists-p (expand-file-name "dir" pkg-dir))
      ;; FIXME: not the friendliest, but simple.
      (require 'info)
      (info-initialize)
      (push pkg-dir Info-directory-list))
    ;; Add to load path, add autoloads, and activate the package.
    (push pkg-dir load-path)
    (load (expand-file-name (concat name "-autoloads") pkg-dir) nil t)
    (push package package-activated-list)
    ;; Don't return nil.
    t))

(defun package-built-in-p (package &optional version)
  "Return true if PACKAGE, of VERSION or newer, is built-in to Emacs."
  (require 'finder-inf nil t) ; For `package--builtins'.
  (let ((elt (assq package package--builtins)))
    (and elt (version-list-<= version (package-desc-vers (cdr elt))))))

;; This function goes ahead and activates a newer version of a package
;; if an older one was already activated.  This is not ideal; we'd at
;; least need to check to see if the package has actually been loaded,
;; and not merely activated.
(defun package-activate (package version)
  "Activate package PACKAGE, of version VERSION or newer.
If PACKAGE has any dependencies, recursively activate them.
Return nil if the package could not be activated."
  (let ((pkg-vec (cdr (assq package package-alist)))
	available-version found)
    ;; Check if PACKAGE is available in `package-alist'.
    (when pkg-vec
      (setq available-version (package-desc-vers pkg-vec)
	    found (version-list-<= version available-version)))
    (cond
     ;; If no such package is found, maybe it's built-in.
     ((null found)
      (package-built-in-p package version))
     ;; If the package is already activated, just return t.
     ((memq package package-activated-list)
      t)
     ;; Otherwise, proceed with activation.
     (t
      (let ((fail (catch 'dep-failure
		    ;; Activate its dependencies recursively.
		    (dolist (req (package-desc-reqs pkg-vec))
		      (unless (package-activate (car req) (cadr req))
			(throw 'dep-failure req))))))
	(if fail
	    (warn "Unable to activate package `%s'.
Required package `%s-%s' is unavailable"
		  package (car fail) (package-version-join (cadr fail)))
	  ;; If all goes well, activate the package itself.
	  (package-activate-1 package pkg-vec)))))))

(defun package-mark-obsolete (package pkg-vec)
  "Put package on the obsolete list, if not already there."
  (let ((elt (assq package package-obsolete-alist)))
    (if elt
	;; If this obsolete version does not exist in the list, update
	;; it the list.
	(unless (assoc (package-desc-vers pkg-vec) (cdr elt))
	  (setcdr elt (cons (cons (package-desc-vers pkg-vec) pkg-vec)
			    (cdr elt))))
      ;; Make a new association.
      (push (cons package (list (cons (package-desc-vers pkg-vec)
				      pkg-vec)))
	    package-obsolete-alist))))

(defun define-package (name-string version-string
				&optional docstring requirements
				&rest extra-properties)
  "Define a new package.
NAME-STRING is the name of the package, as a string.
VERSION-STRING is the version of the package, as a list of
integers of the form produced by `version-to-list'.
DOCSTRING is a short description of the package, a string.
REQUIREMENTS is a list of dependencies on other packages.
Each requirement is of the form (OTHER-PACKAGE \"VERSION\").

EXTRA-PROPERTIES is currently unused."
  (let* ((name (intern name-string))
	 (version (version-to-list version-string))
	 (new-pkg-desc
	  (cons name
		(vector version
			(mapcar
			 (lambda (elt)
			   (list (car elt)
				 (version-to-list (car (cdr elt)))))
			 requirements)
			docstring)))
	 (old-pkg (assq name package-alist)))
    (cond
     ;; If there's no old package, just add this to `package-alist'.
     ((null old-pkg)
      (push new-pkg-desc package-alist))
     ((version-list-< (package-desc-vers (cdr old-pkg)) version)
      ;; Remove the old package and declare it obsolete.
      (package-mark-obsolete name (cdr old-pkg))
      (setq package-alist (cons new-pkg-desc
				(delq old-pkg package-alist))))
     ;; You can have two packages with the same version, e.g. one in
     ;; the system package directory and one in your private
     ;; directory.  We just let the first one win.
     ((not (version-list-= (package-desc-vers (cdr old-pkg)) version))
      ;; The package is born obsolete.
      (package-mark-obsolete name (cdr new-pkg-desc))))))

;; From Emacs 22.
(defun package-autoload-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  (unless (file-exists-p file)
    (write-region
     (concat ";;; " (file-name-nondirectory file)
	     " --- automatically extracted autoloads\n"
	     ";;\n"
	     ";;; Code:\n\n"
	     "\n;; Local Variables:\n"
	     ";; version-control: never\n"
	     ";; no-byte-compile: t\n"
	     ";; no-update-autoloads: t\n"
	     ";; End:\n"
	     ";;; " (file-name-nondirectory file)
	     " ends here\n")
     nil file))
  file)

(defun package-generate-autoloads (name pkg-dir)
  (let* ((auto-name (concat name "-autoloads.el"))
	 (ignore-name (concat name "-pkg.el"))
	 (generated-autoload-file (expand-file-name auto-name pkg-dir))
	 (version-control 'never))
    (require 'autoload)
    (unless (fboundp 'autoload-ensure-default-file)
      (package-autoload-ensure-default-file generated-autoload-file))
    (update-directory-autoloads pkg-dir)))

(defun package-untar-buffer ()
  "Untar the current buffer.
This uses `tar-untar-buffer' if it is available.
Otherwise it uses an external `tar' program.
`default-directory' should be set by the caller."
  (require 'tar-mode)
  (if (fboundp 'tar-untar-buffer)
      (progn
	;; tar-mode messes with narrowing, so we just let it have the
	;; whole buffer to play with.
	(delete-region (point-min) (point))
	(tar-mode)
	(tar-untar-buffer))
    ;; FIXME: check the result.
    (call-process-region (point) (point-max) "tar" nil '(nil nil) nil
			 "xf" "-")))

(defun package-unpack (name version)
  (let ((pkg-dir (expand-file-name (concat (symbol-name name) "-" version)
				   package-user-dir)))
    (make-directory package-user-dir t)
    ;; FIXME: should we delete PKG-DIR if it exists?
    (let* ((default-directory (file-name-as-directory package-user-dir)))
      (package-untar-buffer)
      (package-generate-autoloads (symbol-name name) pkg-dir)
      (let ((load-path (cons pkg-dir load-path)))
	(byte-recompile-directory pkg-dir 0 t)))))

(defun package--write-file-no-coding (file-name)
  (let ((buffer-file-coding-system 'no-conversion))
    (write-region (point-min) (point-max) file-name)))

(defun package-unpack-single (file-name version desc requires)
  "Install the contents of the current buffer as a package."
  ;; Special case "package".
  (if (string= file-name "package")
      (package--write-file-no-coding
       (expand-file-name (concat file-name ".el") package-user-dir))
    (let* ((pkg-dir  (expand-file-name (concat file-name "-"
					       (package-version-join
						(version-to-list version)))
				       package-user-dir))
	   (el-file  (expand-file-name (concat file-name ".el") pkg-dir))
	   (pkg-file (expand-file-name (concat file-name "-pkg.el") pkg-dir)))
      (make-directory pkg-dir t)
      (package--write-file-no-coding el-file)
      (let ((print-level nil)
	    (print-length nil))
	(write-region
	 (concat
	  (prin1-to-string
	   (list 'define-package
		 file-name
		 version
		 desc
		 (list 'quote
		       ;; Turn version lists into string form.
		       (mapcar
			(lambda (elt)
			  (list (car elt)
				(package-version-join (cadr elt))))
			requires))))
	  "\n")
	 nil
	 pkg-file
	 nil nil nil 'excl))
      (package-generate-autoloads file-name pkg-dir)
      (let ((load-path (cons pkg-dir load-path)))
	(byte-recompile-directory pkg-dir 0 t)))))

(defmacro package--with-work-buffer (location file &rest body)
  "Run BODY in a buffer containing the contents of FILE at LOCATION.
LOCATION is the base location of a package archive, and should be
one of the URLs (or file names) specified in `package-archives'.
FILE is the name of a file relative to that base location.

This macro retrieves FILE from LOCATION into a temporary buffer,
and evaluates BODY while that buffer is current.  This work
buffer is killed afterwards.  Return the last value in BODY."
  `(let* ((http (string-match "\\`http:" ,location))
	  (buffer
	   (if http
	       (url-retrieve-synchronously (concat ,location ,file))
	     (generate-new-buffer "*package work buffer*"))))
     (prog1
	 (with-current-buffer buffer
	   (if http
	       (progn (package-handle-response)
		      (re-search-forward "^$" nil 'move)
		      (forward-char)
		      (delete-region (point-min) (point)))
	     (unless (file-name-absolute-p ,location)
	       (error "Archive location %s is not an absolute file name"
		      ,location))
	     (insert-file-contents (expand-file-name ,file ,location)))
	   ,@body)
       (kill-buffer buffer))))

(defun package-handle-response ()
  "Handle the response from a `url-retrieve-synchronously' call.
Parse the HTTP response and throw if an error occurred.
The url package seems to require extra processing for this.
This should be called in a `save-excursion', in the download buffer.
It will move point to somewhere in the headers."
  ;; We assume HTTP here.
  (require 'url-http)
  (let ((response (url-http-parse-response)))
    (when (or (< response 200) (>= response 300))
      (error "Error during download request:%s"
	     (buffer-substring-no-properties (point) (progn
						       (end-of-line)
						       (point)))))))

(defun package-download-single (name version desc requires)
  "Download and install a single-file package."
  (let ((location (package-archive-base name))
	(file (concat (symbol-name name) "-" version ".el")))
    (package--with-work-buffer location file
      (package-unpack-single (symbol-name name) version desc requires))))

(defun package-download-tar (name version)
  "Download and install a tar package."
  (let ((location (package-archive-base name))
	(file (concat (symbol-name name) "-" version ".tar")))
    (package--with-work-buffer location file
      (package-unpack name version))))

(defun package-installed-p (package &optional min-version)
  "Return true if PACKAGE, of VERSION or newer, is installed.
Built-in packages also qualify."
  (let ((pkg-desc (assq package package-alist)))
    (if pkg-desc
	(version-list-<= min-version
			 (package-desc-vers (cdr pkg-desc)))
      ;; Also check built-in packages.
      (package-built-in-p package min-version))))

(defun package-compute-transaction (package-list requirements)
  "Return a list of packages to be installed, including PACKAGE-LIST.
PACKAGE-LIST should be a list of package names (symbols).

REQUIREMENTS should be a list of additional requirements; each
element in this list should have the form (PACKAGE VERSION),
where PACKAGE is a package name and VERSION is the required
version of that package (as a list).

This function recursively computes the requirements of the
packages in REQUIREMENTS, and returns a list of all the packages
that must be installed.  Packages that are already installed are
not included in this list."
  (dolist (elt requirements)
    (let* ((next-pkg (car elt))
	   (next-version (cadr elt)))
      (unless (package-installed-p next-pkg next-version)
	;; A package is required, but not installed.  It might also be
	;; blocked via `package-load-list'.
	(let ((pkg-desc (assq next-pkg package-archive-contents))
	      hold)
	  (when (setq hold (assq next-pkg package-load-list))
	    (setq hold (cadr hold))
	    (cond ((eq hold nil)
		   (error "Required package '%s' is disabled"
			  (symbol-name next-pkg)))
		  ((null (stringp hold))
		   (error "Invalid element in `package-load-list'"))
		  ((version-list-< (version-to-list hold) next-version)
		   (error "Package `%s' held at version %s, \
but version %s required"
			  (symbol-name next-pkg) hold
			  (package-version-join next-version)))))
	  (unless pkg-desc
	    (error "Package `%s-%s' is unavailable"
		   (symbol-name next-pkg)
		   (package-version-join next-version)))
	  (unless (version-list-<= next-version
				   (package-desc-vers (cdr pkg-desc)))
	    (error
	     "Need package `%s-%s', but only %s is available"
	     (symbol-name next-pkg) (package-version-join next-version)
	     (package-version-join (package-desc-vers (cdr pkg-desc)))))
	  ;; Only add to the transaction if we don't already have it.
	  (unless (memq next-pkg package-list)
	    (push next-pkg package-list))
	  (setq package-list
		(package-compute-transaction package-list
					     (package-desc-reqs
					      (cdr pkg-desc))))))))
  package-list)

(defun package-read-from-string (str)
  "Read a Lisp expression from STR.
Signal an error if the entire string was not used."
  (let* ((read-data (read-from-string str))
	 (more-left
	  (condition-case nil
	      ;; The call to `ignore' suppresses a compiler warning.
	      (progn (ignore (read-from-string
			      (substring str (cdr read-data))))
		     t)
	    (end-of-file nil))))
    (if more-left
        (error "Can't read whole string")
      (car read-data))))

(defun package--read-archive-file (file)
  "Re-read archive file FILE, if it exists.
Will return the data from the file, or nil if the file does not exist.
Will throw an error if the archive version is too new."
  (let ((filename (expand-file-name file package-user-dir)))
    (when (file-exists-p filename)
      (with-temp-buffer
	(insert-file-contents-literally filename)
	(let ((contents (read (current-buffer))))
	  (if (> (car contents) package-archive-version)
	      (error "Package archive version %d is higher than %d"
		     (car contents) package-archive-version))
	  (cdr contents))))))

(defun package-read-all-archive-contents ()
  "Re-read `archive-contents', if it exists.
If successful, set `package-archive-contents'."
  (setq package-archive-contents nil)
  (dolist (archive package-archives)
    (package-read-archive-contents (car archive))))

(defun package-read-archive-contents (archive)
  "Re-read archive contents for ARCHIVE.
If successful, set the variable `package-archive-contents'.
If the archive version is too new, signal an error."
  ;; Version 1 of 'archive-contents' is identical to our internal
  ;; representation.
  (let* ((dir (concat "archives/" archive))
	 (contents-file (concat dir "/archive-contents"))
	 contents)
    (when (setq contents (package--read-archive-file contents-file))
      (dolist (package contents)
	(package--add-to-archive-contents package archive)))))

(defun package--add-to-archive-contents (package archive)
  "Add the PACKAGE from the given ARCHIVE if necessary.
Also, add the originating archive to the end of the package vector."
  (let* ((name    (car package))
         (version (aref (cdr package) 0))
         (entry   (cons (car package)
			(vconcat (cdr package) (vector archive))))
         (existing-package (cdr (assq name package-archive-contents))))
    (when (or (not existing-package)
              (version-list-< (aref existing-package 0) version))
      (add-to-list 'package-archive-contents entry))))

(defun package-download-transaction (package-list)
  "Download and install all the packages in PACKAGE-LIST.
PACKAGE-LIST should be a list of package names (symbols).
This function assumes that all package requirements in
PACKAGE-LIST are satisfied, i.e. that PACKAGE-LIST is computed
using `package-compute-transaction'."
  (dolist (elt package-list)
    (let* ((desc (cdr (assq elt package-archive-contents)))
	   ;; As an exception, if package is "held" in
	   ;; `package-load-list', download the held version.
	   (hold (cadr (assq elt package-load-list)))
	   (v-string (or (and (stringp hold) hold)
			 (package-version-join (package-desc-vers desc))))
	   (kind (package-desc-kind desc)))
      (cond
       ((eq kind 'tar)
	(package-download-tar elt v-string))
       ((eq kind 'single)
	(package-download-single elt v-string
				 (package-desc-doc desc)
				 (package-desc-reqs desc)))
       (t
	(error "Unknown package kind: %s" (symbol-name kind)))))))

;;;###autoload
(defun package-install (name)
  "Install the package named NAME.
Interactively, prompt for the package name.
The package is found on one of the archives in `package-archives'."
  (interactive
   (list (intern (completing-read "Install package: "
				  (mapcar (lambda (elt)
					    (cons (symbol-name (car elt))
						  nil))
					  package-archive-contents)
				  nil t))))
  (let ((pkg-desc (assq name package-archive-contents)))
    (unless pkg-desc
      (error "Package `%s' is not available for installation"
	     (symbol-name name)))
    (package-download-transaction
     (package-compute-transaction (list name)
				  (package-desc-reqs (cdr pkg-desc)))))
  ;; Try to activate it.
  (package-initialize))

(defun package-strip-rcs-id (str)
  "Strip RCS version ID from the version string STR.
If the result looks like a dotted numeric version, return it.
Otherwise return nil."
  (when str
    (when (string-match "\\`[ \t]*[$]Revision:[ \t]+" str)
      (setq str (substring str (match-end 0))))
    (condition-case nil
	(if (version-to-list str)
	    str)
      (error nil))))

(defun package-buffer-info ()
  "Return a vector describing the package in the current buffer.
The vector has the form

   [FILENAME REQUIRES DESCRIPTION VERSION COMMENTARY]

FILENAME is the file name, a string, sans the \".el\" extension.
REQUIRES is a requires list, or nil.
DESCRIPTION is the package description, a string.
VERSION is the version, a string.
COMMENTARY is the commentary section, a string, or nil if none.

If the buffer does not contain a conforming package, signal an
error.  If there is a package, narrow the buffer to the file's
boundaries."
  (goto-char (point-min))
  (unless (re-search-forward "^;;; \\([^ ]*\\)\\.el --- \\(.*\\)$" nil t)
    (error "Packages lacks a file header"))
  (let ((file-name (match-string-no-properties 1))
	(desc      (match-string-no-properties 2))
	(start     (line-beginning-position)))
    (unless (search-forward (concat ";;; " file-name ".el ends here"))
      (error "Package lacks a terminating comment"))
    ;; Try to include a trailing newline.
    (forward-line)
    (narrow-to-region start (point))
    (require 'lisp-mnt)
    ;; Use some headers we've invented to drive the process.
    (let* ((requires-str (lm-header "package-requires"))
	   (requires (if requires-str
			 (package-read-from-string requires-str)))
	   ;; Prefer Package-Version; if defined, the package author
	   ;; probably wants us to use it.  Otherwise try Version.
	   (pkg-version
	    (or (package-strip-rcs-id (lm-header "package-version"))
		(package-strip-rcs-id (lm-header "version"))))
	   (commentary (lm-commentary)))
      (unless pkg-version
	(error
	 "Package lacks a \"Version\" or \"Package-Version\" header"))
      ;; Turn string version numbers into list form.
      (setq requires
	    (mapcar
	     (lambda (elt)
	       (list (car elt)
		     (version-to-list (car (cdr elt)))))
	     requires))
      (vector file-name requires desc pkg-version commentary))))

(defun package-tar-file-info (file)
  "Find package information for a tar file.
FILE is the name of the tar file to examine.
The return result is a vector like `package-buffer-info'."
  (let ((default-directory (file-name-directory file))
	(file (file-name-nondirectory file)))
    (unless (string-match "^\\(.+\\)-\\([0-9.]+\\)\\.tar$" file)
      (error "Invalid package name `%s'" file))
    (let* ((pkg-name (match-string-no-properties 1 file))
	   (pkg-version (match-string-no-properties 2 file))
	   ;; Extract the package descriptor.
	   (pkg-def-contents (shell-command-to-string
			      ;; Requires GNU tar.
			      (concat "tar -xOf " file " "

				      pkg-name "-" pkg-version "/"
				      pkg-name "-pkg.el")))
	   (pkg-def-parsed (package-read-from-string pkg-def-contents)))
      (unless (eq (car pkg-def-parsed) 'define-package)
	(error "No `define-package' sexp is present in `%s-pkg.el'" pkg-name))
      (let ((name-str       (nth 1 pkg-def-parsed))
	    (version-string (nth 2 pkg-def-parsed))
	    (docstring      (nth 3 pkg-def-parsed))
	    (requires       (nth 4 pkg-def-parsed))
	    (readme (shell-command-to-string
		     ;; Requires GNU tar.
		     (concat "tar -xOf " file " "
			     pkg-name "-" pkg-version "/README"))))
	(unless (equal pkg-version version-string)
	  (error "Package has inconsistent versions"))
	(unless (equal pkg-name name-str)
	  (error "Package has inconsistent names"))
	;; Kind of a hack.
	(if (string-match ": Not found in archive" readme)
	    (setq readme nil))
	;; Turn string version numbers into list form.
	(if (eq (car requires) 'quote)
	    (setq requires (car (cdr requires))))
	(setq requires
	      (mapcar (lambda (elt)
			(list (car elt)
			      (version-to-list (cadr elt))))
		      requires))
	(vector pkg-name requires docstring version-string readme)))))

;;;###autoload
(defun package-install-from-buffer (pkg-info type)
  "Install a package from the current buffer.
When called interactively, the current buffer is assumed to be a
single .el file that follows the packaging guidelines; see info
node `(elisp)Packaging'.

When called from Lisp, PKG-INFO is a vector describing the
information, of the type returned by `package-buffer-info'; and
TYPE is the package type (either `single' or `tar')."
  (interactive (list (package-buffer-info) 'single))
  (save-excursion
    (save-restriction
      (let* ((file-name (aref pkg-info 0))
	     (requires  (aref pkg-info 1))
	     (desc (if (string= (aref pkg-info 2) "")
		       "No description available."
		     (aref pkg-info 2)))
	     (pkg-version (aref pkg-info 3)))
	;; Download and install the dependencies.
	(let ((transaction (package-compute-transaction nil requires)))
	  (package-download-transaction transaction))
	;; Install the package itself.
	(cond
	 ((eq type 'single)
	  (package-unpack-single file-name pkg-version desc requires))
	 ((eq type 'tar)
	  (package-unpack (intern file-name) pkg-version))
	 (t
	  (error "Unknown type: %s" (symbol-name type))))
	;; Try to activate it.
	(package-initialize)))))

;;;###autoload
(defun package-install-file (file)
  "Install a package from a file.
The file can either be a tar file or an Emacs Lisp file."
  (interactive "fPackage file name: ")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cond
     ((string-match "\\.el$" file)
      (package-install-from-buffer (package-buffer-info) 'single))
     ((string-match "\\.tar$" file)
      (package-install-from-buffer (package-tar-file-info file) 'tar))
     (t (error "Unrecognized extension `%s'" (file-name-extension file))))))

(defun package-delete (name version)
  (let ((dir (package--dir name version)))
    (if (string-equal (file-name-directory dir)
		      (file-name-as-directory
		       (expand-file-name package-user-dir)))
	(progn
	  (delete-directory dir t t)
	  (message "Package `%s-%s' deleted." name version))
      ;; Don't delete "system" packages
      (error "Package `%s-%s' is a system package, not deleting"
	     name version))))

(defun package-archive-base (name)
  "Return the archive containing the package NAME."
  (let ((desc (cdr (assq (intern-soft name) package-archive-contents))))
    (cdr (assoc (aref desc (- (length desc) 1)) package-archives))))

(defun package--download-one-archive (archive file)
  "Retrieve an archive file FILE from ARCHIVE, and cache it.
ARCHIVE should be a cons cell of the form (NAME . LOCATION),
similar to an entry in `package-alist'.  Save the cached copy to
\"archives/NAME/archive-contents\" in `package-user-dir'."
  (let* ((dir (expand-file-name "archives" package-user-dir))
	 (dir (expand-file-name (car archive) dir)))
    (package--with-work-buffer (cdr archive) file
      ;; Read the retrieved buffer to make sure it is valid (e.g. it
      ;; may fetch a URL redirect page).
      (when (listp (read buffer))
	(make-directory dir t)
	(setq buffer-file-name (expand-file-name file dir))
	(let ((version-control 'never))
	  (save-buffer))))))

(defun package-refresh-contents ()
  "Download the ELPA archive description if needed.
This informs Emacs about the latest versions of all packages, and
makes them available for download."
  (interactive)
  (unless (file-exists-p package-user-dir)
    (make-directory package-user-dir t))
  (dolist (archive package-archives)
    (condition-case-no-debug nil
	(package--download-one-archive archive "archive-contents")
      (error (message "Failed to download `%s' archive."
		      (car archive)))))
  (package-read-all-archive-contents))

(defvar package--initialized nil)

;;;###autoload
(defun package-initialize (&optional no-activate)
  "Load Emacs Lisp packages, and activate them.
The variable `package-load-list' controls which packages to load.
If optional arg NO-ACTIVATE is non-nil, don't activate packages."
  (interactive)
  (setq package-alist nil
	package-obsolete-alist nil)
  (package-load-all-descriptors)
  (package-read-all-archive-contents)
  (unless no-activate
    (dolist (elt package-alist)
      (package-activate (car elt) (package-desc-vers (cdr elt)))))
  (setq package--initialized t))


;;;; Package description buffer.

;;;###autoload
(defun describe-package (package)
  "Display the full documentation of PACKAGE (a symbol)."
  (interactive
   (let* ((guess (function-called-at-point))
	  packages val)
     (require 'finder-inf nil t)
     ;; Load the package list if necessary (but don't activate them).
     (unless package--initialized
       (package-initialize t))
     (setq packages (append (mapcar 'car package-alist)
			    (mapcar 'car package-archive-contents)
			    (mapcar 'car package--builtins)))
     (unless (memq guess packages)
       (setq guess nil))
     (setq packages (mapcar 'symbol-name packages))
     (setq val
	   (completing-read (if guess
				(format "Describe package (default %s): "
					guess)
			      "Describe package: ")
			    packages nil t nil nil guess))
     (list (if (equal val "") guess (intern val)))))
  (if (or (null package) (not (symbolp package)))
      (message "No package specified")
    (help-setup-xref (list #'describe-package package)
		     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
	(describe-package-1 package)))))

(defun describe-package-1 (package)
  (require 'lisp-mnt)
  (let ((package-name (symbol-name package))
	(built-in (assq package package--builtins))
	desc pkg-dir reqs version installable)
    (prin1 package)
    (princ " is ")
    (cond
     ;; Loaded packages are in `package-alist'.
     ((setq desc (cdr (assq package package-alist)))
      (setq version (package-version-join (package-desc-vers desc)))
      (if (setq pkg-dir (package--dir package-name version))
	  (insert "an installed package.\n\n")
	;; This normally does not happen.
	(insert "a deleted package.\n\n")))
     ;; Available packages are in `package-archive-contents'.
     ((setq desc (cdr (assq package package-archive-contents)))
      (setq version (package-version-join (package-desc-vers desc))
	    installable t)
      (if built-in
	  (insert "a built-in package.\n\n")
	(insert "an uninstalled package.\n\n")))
     (built-in
      (setq desc (cdr built-in)
	    version (package-version-join (package-desc-vers desc)))
      (insert "a built-in package.\n\n"))
     (t
      (insert "an orphan package.\n\n")))

    (insert "     " (propertize "Status" 'font-lock-face 'bold) ": ")
    (cond (pkg-dir
	   (insert (propertize "Installed"
			       'font-lock-face 'font-lock-comment-face))
	   (insert " in `")
	   ;; Todo: Add button for uninstalling.
	   (help-insert-xref-button (file-name-as-directory pkg-dir)
				    'help-package-def pkg-dir)
	   (if built-in
	       (insert "',\n             shadowing a "
		       (propertize "built-in package"
				   'font-lock-face 'font-lock-builtin-face)
		       ".")
	     (insert "'.")))
	  (installable
	   (if built-in
	       (insert (propertize "Built-in." 'font-lock-face 'font-lock-builtin-face)
		       "  Alternate version available -- ")
	     (insert "Available -- "))
	   (let ((button-text (if (display-graphic-p) "Install" "[Install]"))
		 (button-face (if (display-graphic-p)
				  '(:box (:line-width 2 :color "dark grey")
					 :background "light grey"
					 :foreground "black")
				'link)))
	     (insert-text-button button-text 'face button-face 'follow-link t
				 'package-symbol package
				 'action 'package-install-button-action)))
	  (built-in
	   (insert (propertize "Built-in." 'font-lock-face 'font-lock-builtin-face)))
	  (t (insert "Deleted.")))
    (insert "\n")
    (and version (> (length version) 0)
	 (insert "    "
		 (propertize "Version" 'font-lock-face 'bold) ": " version "\n"))

    (setq reqs (if desc (package-desc-reqs desc)))
    (when reqs
      (insert "   " (propertize "Requires" 'font-lock-face 'bold) ": ")
      (let ((first t)
	    name vers text)
	(dolist (req reqs)
	  (setq name (car req)
		vers (cadr req)
		text (format "%s-%s" (symbol-name name)
			     (package-version-join vers)))
	  (cond (first (setq first nil))
		((>= (+ 2 (current-column) (length text))
		     (window-width))
		 (insert ",\n               "))
		(t (insert ", ")))
	  (help-insert-xref-button text 'help-package name))
	(insert "\n")))
    (insert "    " (propertize "Summary" 'font-lock-face 'bold)
	    ": " (if desc (package-desc-doc desc)) "\n\n")

    (if built-in
	;; For built-in packages, insert the commentary.
	(let ((fn (locate-file (concat package-name ".el") load-path
			       load-file-rep-suffixes))
	      (opoint (point)))
	  (insert (or (lm-commentary fn) ""))
	  (save-excursion
	    (goto-char opoint)
	    (when (re-search-forward "^;;; Commentary:\n" nil t)
	      (replace-match ""))
	    (while (re-search-forward "^\\(;+ ?\\)" nil t)
	      (replace-match ""))))
      (let ((readme (expand-file-name (concat package-name "-readme.txt")
				      package-user-dir))
	    readme-string)
	;; For elpa packages, try downloading the commentary.  If that
	;; fails, try an existing readme file in `package-user-dir'.
	(cond ((condition-case nil
		   (package--with-work-buffer (package-archive-base package)
					      (concat package-name "-readme.txt")
		     (setq buffer-file-name
			   (expand-file-name readme package-user-dir))
		     (let ((version-control 'never))
		       (save-buffer))
		     (setq readme-string (buffer-string))
		     t)
		 (error nil))
	       (insert readme-string))
	      ((file-readable-p readme)
	       (insert-file-contents readme)
	       (goto-char (point-max))))))))

(defun package-install-button-action (button)
  (let ((package (button-get button 'package-symbol)))
    (when (y-or-n-p (format "Install package `%s'? " package))
      (package-install package)
      (revert-buffer nil t)
      (goto-char (point-min)))))


;;;; Package menu mode.

(defvar package-menu-mode-map
  (let ((map (copy-keymap special-mode-map))
	(menu-map (make-sparse-keymap "Package")))
    (set-keymap-parent map button-buffer-map)
    (define-key map "\C-m" 'package-menu-describe-package)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'package-menu-mark-unmark)
    (define-key map "\177" 'package-menu-backup-unmark)
    (define-key map "d" 'package-menu-mark-delete)
    (define-key map "i" 'package-menu-mark-install)
    (define-key map "r" 'package-menu-refresh)
    (define-key map "~" 'package-menu-mark-obsolete-for-deletion)
    (define-key map "x" 'package-menu-execute)
    (define-key map "h" 'package-menu-quick-help)
    (define-key map "?" 'package-menu-describe-package)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'mouse-select-window)
    (define-key map [menu-bar package-menu] (cons "Package" menu-map))
    (define-key menu-map [mq]
      '(menu-item "Quit" quit-window
		  :help "Quit package selection"))
    (define-key menu-map [s1] '("--"))
    (define-key menu-map [mn]
      '(menu-item "Next" next-line
		  :help "Next Line"))
    (define-key menu-map [mp]
      '(menu-item "Previous" previous-line
		  :help "Previous Line"))
    (define-key menu-map [s2] '("--"))
    (define-key menu-map [mu]
      '(menu-item "Unmark" package-menu-mark-unmark
		  :help "Clear any marks on a package and move to the next line"))
    (define-key menu-map [munm]
      '(menu-item "Unmark backwards" package-menu-backup-unmark
		  :help "Back up one line and clear any marks on that package"))
    (define-key menu-map [md]
      '(menu-item "Mark for deletion" package-menu-mark-delete
		  :help "Mark a package for deletion and move to the next line"))
    (define-key menu-map [mi]
      '(menu-item "Mark for install" package-menu-mark-install
		  :help "Mark a package for installation and move to the next line"))
    (define-key menu-map [s3] '("--"))
    (define-key menu-map [mg]
      '(menu-item "Update package list" revert-buffer
		  :help "Update the list of packages"))
    (define-key menu-map [mr]
      '(menu-item "Refresh package list" package-menu-refresh
		  :help "Download the ELPA archive"))
    (define-key menu-map [s4] '("--"))
    (define-key menu-map [mt]
      '(menu-item "Mark obsolete packages" package-menu-mark-obsolete-for-deletion
		  :help "Mark all obsolete packages for deletion"))
    (define-key menu-map [mx]
      '(menu-item "Execute actions" package-menu-execute
		  :help "Perform all the marked actions"))
    (define-key menu-map [s5] '("--"))
    (define-key menu-map [mh]
      '(menu-item "Help" package-menu-quick-help
		  :help "Show short key binding help for package-menu-mode"))
    (define-key menu-map [mc]
      '(menu-item "View Commentary" package-menu-view-commentary
		  :help "Display information about this package"))
    map)
  "Local keymap for `package-menu-mode' buffers.")

(defvar package-menu-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'package-menu-sort-by-column)
    (define-key map [header-line mouse-2] 'package-menu-sort-by-column)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for package menu sort buttons.")

(put 'package-menu-mode 'mode-class 'special)

(define-derived-mode package-menu-mode special-mode "Package Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (set (make-local-variable 'revert-buffer-function) 'package-menu-revert)
  (setq header-line-format
	(mapconcat
	 (lambda (pair)
	   (let ((column (car pair))
		 (name (cdr pair)))
	     (concat
	      ;; Insert a space that aligns the button properly.
	      (propertize " " 'display (list 'space :align-to column)
			  'face 'fixed-pitch)
	      ;; Set up the column button.
	      (propertize name
			  'column-name name
			  'help-echo "mouse-1: sort by column"
			  'mouse-face 'highlight
			  'keymap package-menu-sort-button-map))))
	 ;; We take a trick from buff-menu and have a dummy leading
	 ;; space to align the header line with the beginning of the
	 ;; text.  This doesn't really work properly on Emacs 21, but
	 ;; it is close enough.
	 '((0 . "")
	   (2 . "Package")
	   (20 . "Version")
	   (32 . "Status")
	   (43 . "Description"))
	 "")))

(defun package-menu-refresh ()
  "Download the Emacs Lisp package archive.
This fetches the contents of each archive specified in
`package-archives', and then refreshes the package menu."
  (interactive)
  (unless (eq major-mode 'package-menu-mode)
    (error "The current buffer is not a Package Menu"))
  (package-refresh-contents)
  (package--generate-package-list))

(defun package-menu-revert (&optional arg noconfirm)
  "Update the list of packages.
This function is the `revert-buffer-function' for Package Menu
buffers.  The arguments are ignored."
  (interactive)
  (unless (eq major-mode 'package-menu-mode)
    (error "The current buffer is not a Package Menu"))
  (package--generate-package-list))

(defun package-menu-describe-package ()
  "Describe the package in the current line."
  (interactive)
  (let ((name (package-menu-get-package)))
    (if name
	(describe-package (intern name))
      (message "No package on this line"))))

(defun package-menu-mark-internal (what)
  (unless (eobp)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert what)
      (forward-line))))

;; fixme numeric argument
(defun package-menu-mark-delete (num)
  "Mark a package for deletion and move to the next line."
  (interactive "p")
  (if (string-equal (package-menu-get-status) "installed")
      (package-menu-mark-internal "D")
    (forward-line)))

(defun package-menu-mark-install (num)
  "Mark a package for installation and move to the next line."
  (interactive "p")
  (if (string-equal (package-menu-get-status) "available")
      (package-menu-mark-internal "I")
    (forward-line)))

(defun package-menu-mark-unmark (num)
  "Clear any marks on a package and move to the next line."
  (interactive "p")
  (package-menu-mark-internal " "))

(defun package-menu-backup-unmark ()
  "Back up one line and clear any marks on that package."
  (interactive)
  (forward-line -1)
  (package-menu-mark-internal " ")
  (forward-line -1))

(defun package-menu-mark-obsolete-for-deletion ()
  "Mark all obsolete packages for deletion."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (not (eobp))
      (if (looking-at ".*\\s obsolete\\s ")
	  (package-menu-mark-internal "D")
	(forward-line 1)))))

(defun package-menu-quick-help ()
  "Show short key binding help for package-menu-mode."
  (interactive)
  (message "n-ext, i-nstall, d-elete, u-nmark, x-ecute, r-efresh, h-elp"))

(define-obsolete-function-alias
  'package-menu-view-commentary 'package-menu-describe-package "24.1")

;; Return the name of the package on the current line.
(defun package-menu-get-package ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". \\([^ \t]*\\)")
	(match-string-no-properties 1))))

;; Return the version of the package on the current line.
(defun package-menu-get-version ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". [^ \t]*[ \t]*\\([0-9.]*\\)")
	(match-string 1))))

(defun package-menu-get-status ()
  (save-excursion
    (if (looking-at ". [^ \t]*[ \t]*[^ \t]*[ \t]*\\([^ \t]*\\)")
	(match-string 1)
      "")))

(defun package-menu-execute ()
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed."
  (interactive)
  (let (install-list delete-list cmd)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq cmd (char-after))
	(cond
	 ((eq cmd ?\s) t)
	 ((eq cmd ?D)
	  (push (cons (package-menu-get-package)
		      (package-menu-get-version))
		delete-list))
	 ((eq cmd ?I)
	  (push (package-menu-get-package) install-list)))
	(forward-line)))
    ;; Delete packages, prompting if necessary.
    (when delete-list
      (if (yes-or-no-p
	   (if (= (length delete-list) 1)
	       (format "Delete package `%s-%s'? "
		       (caar delete-list)
		       (cdr (car delete-list)))
	     (format "Delete these %d packages (%s)? "
		     (length delete-list)
		     (mapconcat (lambda (elt)
				  (concat (car elt) "-" (cdr elt)))
				delete-list
				", "))))
	  (dolist (elt delete-list)
	    (condition-case-no-debug err
		(package-delete (car elt) (cdr elt))
	      (error (message (cadr err)))))
	(error "Aborted")))
    (when install-list
      (if (yes-or-no-p
	   (if (= (length install-list) 1)
	       (format "Install package `%s'? " (car install-list))
	     (format "Install these %d packages (%s)? "
		     (length install-list)
		     (mapconcat 'identity install-list ", "))))
	  (dolist (elt install-list)
	    (package-install (intern elt)))))
    ;; If we deleted anything, regenerate `package-alist'.  This is done
    ;; automatically if we installed a package.
    (and delete-list (null install-list)
	 (package-initialize))
    (if (or delete-list install-list)
	(package-menu-revert)
      (message "No operations specified."))))

(defun package-print-package (package version key desc)
  (let ((face
	 (cond ((string= key "built-in") 'font-lock-builtin-face)
	       ((string= key "available") 'default)
	       ((string= key "held") 'font-lock-constant-face)
	       ((string= key "disabled") 'font-lock-warning-face)
	       ((string= key "installed") 'font-lock-comment-face)
	       (t ; obsolete, but also the default.
		'font-lock-warning-face))))
    (insert (propertize "  " 'font-lock-face face))
    (insert-text-button (symbol-name package)
			'face 'link
			'follow-link t
			'package-symbol package
			'action (lambda (button)
				  (describe-package
				   (button-get button 'package-symbol))))
    (indent-to 20 1)
    (insert (propertize (package-version-join version) 'font-lock-face face))
    (indent-to 32 1)
    (insert (propertize key 'font-lock-face face))
    ;; FIXME: this 'when' is bogus...
    (when desc
      (indent-to 43 1)
      (let ((opoint (point)))
	(insert (propertize desc 'font-lock-face face))
	(upcase-region opoint (min (point) (1+ opoint)))))
    (insert "\n")))

(defun package-list-maybe-add (package version status description result)
  (unless (assoc (cons package version) result)
    (push (list (cons package version) status description) result))
  result)

(defvar package-menu-package-list nil
  "List of packages to display in the Package Menu buffer.
A value of nil means to display all packages.")

(defvar package-menu-sort-key nil
  "Sort key for the current Package Menu buffer.")

(defun package--generate-package-list ()
  "Populate the current Package Menu buffer."
  (let ((inhibit-read-only t)
	info-list name desc hold builtin)
    (erase-buffer)
    ;; List installed packages
    (dolist (elt package-alist)
      (setq name (car elt))
      (when (or (null package-menu-package-list)
		(memq name package-menu-package-list))
	(setq desc (cdr elt)
	      hold (cadr (assq name package-load-list)))
	(setq info-list
	      (package-list-maybe-add
	       name (package-desc-vers desc)
	       ;; FIXME: it turns out to be tricky to see if this
	       ;; package is presently activated.
	       (if (stringp hold) "held" "installed")
	       (package-desc-doc desc)
	       info-list))))

    ;; List built-in packages
    (dolist (elt package--builtins)
      (setq name (car elt))
      (when (and (not (eq name 'emacs)) ; Hide the `emacs' package.
		 (or (null package-menu-package-list)
		     (memq name package-menu-package-list)))
	(setq desc (cdr elt))
	(setq info-list
	      (package-list-maybe-add
	       name (package-desc-vers desc)
	       "built-in"
	       (package-desc-doc desc)
	       info-list))))

    ;; List available and disabled packages
    (dolist (elt package-archive-contents)
      (setq name (car elt)
	    desc (cdr elt)
	    hold (assq name package-load-list))
      (when (or (null package-menu-package-list)
		(memq name package-menu-package-list))
	(setq info-list
	      (package-list-maybe-add name
				      (package-desc-vers desc)
				      (if (and hold (null (cadr hold)))
					  "disabled"
					"available")
				      (package-desc-doc (cdr elt))
				      info-list))))
    ;; List obsolete packages
    (mapc (lambda (elt)
	    (mapc (lambda (inner-elt)
		    (setq info-list
			  (package-list-maybe-add (car elt)
						  (package-desc-vers
						   (cdr inner-elt))
						  "obsolete"
						  (package-desc-doc
						   (cdr inner-elt))
						  info-list)))
		  (cdr elt)))
	  package-obsolete-alist)

    (setq info-list
	  (sort info-list
		(cond ((string= package-menu-sort-key "Package")
		       'package-menu--name-predicate)
		      ((string= package-menu-sort-key "Version")
		       'package-menu--version-predicate)
		      ((string= package-menu-sort-key "Description")
		       'package-menu--description-predicate)
		      (t ; By default, sort by package status
		       'package-menu--status-predicate))))

    (dolist (elt info-list)
      (package-print-package (car (car elt))
			     (cdr (car elt))
			     (car (cdr elt))
			     (car (cdr (cdr elt)))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (current-buffer)))

(defun package-menu--version-predicate (left right)
  (let ((vleft  (or (cdr (car left))  '(0)))
	(vright (or (cdr (car right)) '(0))))
    (if (version-list-= vleft vright)
	(package-menu--name-predicate left right)
      (version-list-< vleft vright))))

(defun package-menu--status-predicate (left right)
  (let ((sleft  (cadr left))
	(sright (cadr right)))
    (cond ((string= sleft sright)
	   (package-menu--name-predicate left right))
	  ((string= sleft  "available") t)
	  ((string= sright "available") nil)
	  ((string= sleft  "installed") t)
	  ((string= sright "installed") nil)
	  ((string= sleft  "held") t)
	  ((string= sright "held") nil)
	  ((string= sleft  "built-in") t)
	  ((string= sright "built-in") nil)
	  ((string= sleft  "obsolete") t)
	  ((string= sright  "obsolete") nil)
	  (t (string< sleft sright)))))

(defun package-menu--description-predicate (left right)
  (let ((sleft  (car (cddr left)))
	(sright (car (cddr right))))
    (if (string= sleft sright)
	(package-menu--name-predicate left right)
      (string< sleft sright))))

(defun package-menu--name-predicate (left right)
  (string< (symbol-name (caar left))
	   (symbol-name (caar right))))

(defun package-menu-sort-by-column (&optional e)
  "Sort the package menu by the column of the mouse click E."
  (interactive "e")
  (let* ((pos (event-start e))
	 (obj (posn-object pos))
	 (col (if obj
		  (get-text-property (cdr obj) 'column-name (car obj))
		(get-text-property (posn-point pos) 'column-name)))
	 (buf (window-buffer (posn-window (event-start e)))))
    (with-current-buffer buf
      (when (eq major-mode 'package-menu-mode)
	(setq package-menu-sort-key col)
	(package--generate-package-list)))))

(defun package--list-packages (&optional packages)
  "Generate and pop to the *Packages* buffer.
Optional PACKAGES is a list of names of packages (symbols) to
list; the default is to display everything in `package-alist'."
  (require 'finder-inf nil t)
  (let ((buf (get-buffer-create "*Packages*")))
    (with-current-buffer buf
      (package-menu-mode)
      (set (make-local-variable 'package-menu-package-list) packages)
      (set (make-local-variable 'package-menu-sort-key) nil)
      (package--generate-package-list))
    ;; The package menu buffer has keybindings.  If the user types
    ;; `M-x list-packages', that suggests it should become current.
    (switch-to-buffer buf)))

;;;###autoload
(defun list-packages ()
  "Display a list of packages.
Fetches the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'."
  (interactive)
  ;; Initialize the package system if necessary.
  (unless package--initialized
    (package-initialize t))
  (package-refresh-contents)
  (package--list-packages))

;;;###autoload
(defalias 'package-list-packages 'list-packages)

(defun package-list-packages-no-fetch ()
  "Display a list of packages.
Does not fetch the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'."
  (interactive)
  (package--list-packages))

(provide 'package)

;;; package.el ends here


;;; w3mhack.el --- a hack to setup the environment for building w3m

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This file provides the stuffs to setup the environment for building
;; emacs-w3m and the installer for non-UNIX systems.


;;; How to install:

;; ( These document is a simple summary of README.  For more detail,
;;   see the original. )

;; In order to install emacs-w3m to non-UNIX systems which lack an
;; ability to execute `configure' script or have no `make' utility,
;; execute this command.
;;
;;     emacs -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install
;;
;; When required packages, such as APEL, are installed into unusual
;; places, the installer may miss them.  In this case, it is necessary
;; to tell their places to the installer, as follows:
;;
;;     emacs -batch -q -no-site-file -l w3mhack.el //c/share/apel://c/share/flim -f w3mhack-nonunix-install
;;
;; If you want to install programs and icons to unusual directories,
;; edit this file and set your request to `w3mhack-nonunix-lispdir'
;; and `w3mhack-nonunix-icondir'.


;;; Code:

(when (featurep 'xemacs)
  (setq log-warning-minimum-level 'info)
  (setenv "XEMACSDEBUG" nil))

(defvar w3mhack-nonunix-lispdir nil
  "*Directory to where emacs-w3m lisp programs are installed.
nil means that all programs are installed to the default directory.")

(defvar w3mhack-nonunix-icondir nil
  "*Directory to where emacs-w3m icons are installed.
nil means that all icons are installed to the default directory.")

(defvar w3mhack-nonunix-dryrun nil
  "*Non-nil means that print the commands to install programs and datas,
but do not execute them.")

(require 'cl)

;; Check whether the shell command can be used.
(let ((test (lambda (shell)
	      (let ((buffer (generate-new-buffer " *temp*"))
		    (msg "Hello World"))
		(with-current-buffer buffer
		  (condition-case nil
		      (call-process shell nil t nil "-c"
				    (concat "MESSAGE=\"" msg "\"&&"
					    "echo \"${MESSAGE}\""))
		    (error))
		  (prog2
		      (goto-char (point-min))
		      (search-forward msg nil t)
		    (kill-buffer buffer)))))))
  (or (funcall test shell-file-name)
      (progn
	(require 'executable)
	(let ((executable-binary-suffixes
	       (if (memq system-type '(OS/2 emx))
		   '(".exe" ".com" ".bat" ".cmd" ".btm" "")
		 executable-binary-suffixes))
	      shell)
	  (or (and (setq shell (executable-find "cmdproxy"))
		   (funcall test shell)
		   (setq shell-file-name shell))
	      (and (setq shell (executable-find "sh"))
		   (funcall test shell)
		   (setq shell-file-name shell))
	      (and (setq shell (executable-find "bash"))
		   (funcall test shell)
		   (setq shell-file-name shell))
	      (not (member (nth 1 (or (member "-f" command-line-args)
				      (member "-funcall" command-line-args)
				      (member "--funcall" command-line-args)
				      (member "-e" command-line-args)))
			   '("batch-byte-compile" "w3mhack-compile"
			     "w3mhack-makeinfo" "w3mhack-make-package")))
	      (error "%s" "\n\
There seems to be no shell command which is equivalent to /bin/sh.
 Try ``make SHELL=foo [option...]'', where `foo' is the absolute
 path name for the proper shell command in your system.\n"))))))

(unless (dolist (var nil t))
  ;; Override the macro `dolist' which may have been defined in egg.el.
  (load "cl-macs" nil t))
(require 'bytecomp)

;; Since Emacs won't optimize the butlast elements of the arguments
;; for the functions and/or, we do it by ourselves.
(when (equal
       (cadr
	(byte-optimize-form
	 '(and
	   (< 0 1)
	   (message "The subform `(< 0 1)' should be optimized to `t'"))
	 'for-effect))
       '(< 0 1))
  (setq max-specpdl-size (* 5 max-specpdl-size));; Needed for Mule 2.
  (defadvice byte-optimize-form-code-walker
    (around fix-bug-in-and/or-forms (form for-effect) activate)
    "Fix a bug in the optimizing and/or forms.
It has already been fixed in XEmacs since 1999-12-06."
    (if (and for-effect (memq (car-safe form) '(and or)))
	(let ((fn (car form))
	      (backwards (reverse (cdr form))))
	  (while (and backwards
		      (null (setcar backwards
				    (byte-optimize-form (car backwards) t))))
	    (setq backwards (cdr backwards)))
	  (if (and (cdr form) (null backwards))
	      (byte-compile-log
	       "  all subforms of %s called for effect; deleted" form))
	  (when backwards
	    (setcdr backwards
		    (mapcar 'byte-optimize-form (cdr backwards))))
	  (setq ad-return-value (cons fn (nreverse backwards))))
      ad-do-it)))

(when (and (not (featurep 'xemacs))
	   (= emacs-major-version 21)
	   (>= emacs-minor-version 3)
	   (condition-case code
	       (let ((byte-compile-error-on-warn t))
		 (byte-optimize-form (quote (pop x)) t)
		 nil)
	     (error (string-match "called for effect"
				  (error-message-string code)))))
  (defadvice byte-optimize-form-code-walker (around silence-warn-for-pop
						    (form for-effect)
						    activate)
    "Silence the warning \"...called for effect\" for the `pop' form.
It is effective only when the `pop' macro is defined by cl.el rather
than subr.el."
    (let (tmp)
      (if (and (eq (car-safe form) 'car)
	       for-effect
	       (setq tmp (get 'car 'side-effect-free))
	       (not byte-compile-delete-errors)
	       (not (eq tmp 'error-free))
	       (eq (car-safe (cadr form)) 'prog1)
	       (let ((var (cadr (cadr form)))
		     (last (nth 2 (cadr form))))
		 (and (symbolp var)
		      (null (nthcdr 3 (cadr form)))
		      (eq (car-safe last) 'setq)
		      (eq (cadr last) var)
		      (eq (car-safe (nth 2 last)) 'cdr)
		      (eq (cadr (nth 2 last)) var))))
	  (progn
	    (put 'car 'side-effect-free 'error-free)
	    (unwind-protect
		ad-do-it
	      (put 'car 'side-effect-free tmp)))
	ad-do-it))))

;; Add `configure-package-path' to `load-path' for XEmacs.  Those paths
;; won't appear in `load-path' when XEmacs starts with the `-vanilla'
;; option or the `-no-autoloads' option because of a bug. :<
(when (and (featurep 'xemacs)
	   (boundp 'configure-package-path)
	   (listp configure-package-path))
  (let ((paths
	 (apply 'nconc
		(mapcar
		 (lambda (path)
		   (when (and (stringp path)
			      (not (string-equal path ""))
			      (file-directory-p
			       (setq path (expand-file-name "lisp" path))))
		     (directory-files path t)))
		 configure-package-path)))
	path adds)
    (while paths
      (setq path (car paths)
	    paths (cdr paths))
      (when (and path
		 (not (or (string-match "/\\.\\.?\\'" path)
			  (member (file-name-as-directory path) load-path)
			  (member path load-path)))
		 (file-directory-p path))
	(push (file-name-as-directory path) adds)))
    (setq load-path (nconc (nreverse adds) load-path))))

;; Add supplementary directories to `load-path'.
(let ((addpath (or (pop command-line-args-left) "NONE"))
      (index 0)
      path paths)
  (while (string-match "\\([^\0-\37:]+\\)[\0-\37:]*" addpath index)
    (setq path (file-name-as-directory
		(expand-file-name (match-string 1 addpath)))
	  index (match-end 0))
    (when (file-directory-p path)
      (push path paths)))
  (unless (null paths)
    (setq load-path (nconc (nreverse paths) load-path))))

;; Check for the required modules.
(when (featurep 'xemacs)
  (let ((apel (locate-library "path-util"))
	(emu (locate-library "pccl")))
    (if (and apel emu)
	(progn
	  (setq apel (file-name-directory apel)
		emu (file-name-directory emu))
	  (if (not (string-equal apel emu))
	      (setq apel (concat apel "\n  " emu)))
	  (unless (if (featurep 'mule)
		      (condition-case nil
			  (progn
			    ;; Checking whether APEL has been compiled for
			    ;; XEmacs with MULE.
			    (require 'pccl)
			    (featurep 'pccl-20))
			(error nil))
		    (condition-case nil
			;; Checking whether APEL has been compiled for
			;; XEmacs without MULE (whether `pccl' does not
			;; require `ccl').
			(require 'pccl)
		      (error nil)))
	    (error "\nError: %s%s%s%s\n"
		   (if (featurep 'mule)
		       "\
APEL package seems to have been compiled for non-MULE XEmacs,
 even though you are using XEmacs with MULE to build emacs-w3m."
		     "\
APEL package seems to have been compiled for XEmacs with MULE,
 even though you are using non-MULE XEmacs to build emacs-w3m.")
		   "  APEL
 modules are installed in:\n\n  "
		   apel
		   "\n
 If you are using the official APEL XEmacs package (or possibly SUMO),
 look for the new one (version 1.32 and later) and install it in your
 system, or send a bug report to the maintainers using
 `M-x report-xemacs-bug'.  Otherwise, you may rebuild APEL from the
 source distribution, see manuals where you could get it from.")))
      (error "
Error: You have to install APEL before building emacs-w3m, see manuals.
 If you have already installed APEL in the non-standard Lisp directory,
 use the `--with-addpath=' configure option with that path name (or
 colon separated those path names) and run configure again."))))

(defconst shimbun-module-directory "shimbun")

(defvar w3mhack-load-file "w3m-load.el")

;; Needed for interdependencies among w3m and shimbun modules.
(push default-directory load-path)
(push (expand-file-name shimbun-module-directory default-directory) load-path)

(defun w3mhack-module-list ()
  "Returna a list of w3m modules should be byte-compile'd."
  (let* ((modules (directory-files default-directory nil "^[^#]+\\.el$"))
	 (version-specific-modules '("w3m-ems.el" "w3m-xmas.el"))
	 (ignores;; modules not to be byte-compiled.
	  (append
	   (list "w3mhack.el" "w3m-setup.el" w3mhack-load-file)
	   (delete (if (featurep 'xemacs) "w3m-xmas.el" "w3m-ems.el")
		   (copy-sequence version-specific-modules))))
	 (shimbun-dir (file-name-as-directory shimbun-module-directory))
	 print-level print-length)
    ;; Exclude very old Mew XEmacs package.
    (unless
	(if (featurep 'xemacs)
	    (let ((el (locate-library "mew"))
		  pkg)
	      (and el
		   (if (file-exists-p (setq pkg (expand-file-name
						 "_pkg.el"
						 (file-name-directory el))))
		       (condition-case nil
			   (let ((packages-package-list packages-package-list))
			     (load pkg nil t)
			     (>= (string-to-number (package-get-key
						    'mew :author-version))
				 2))
			 (error nil))
		     t)))
	  (locate-library "mew"))
      (push "mew-w3m.el" ignores))
    (unless (if (featurep 'xemacs)
		(and (featurep 'mule) (locate-library "pccl"))
	      (locate-library "ccl"))
      (push "w3m-ccl.el" ignores))
    (unless (and (featurep 'mule)
		 (locate-library "un-define"))
      (push "w3m-ucs.el" ignores))
    (if (and (featurep 'mule)
	     (locate-library "mime-def"))
	(progn
	  ;; Add shimbun modules.
	  (dolist (file (directory-files (expand-file-name shimbun-dir)
					 nil "^[^#]+\\.el$"))
	    (setq modules (nconc modules (list (concat shimbun-dir file)))))
	  ;; mew-shimbun check
	  (when (or (member "mew-w3m.el" ignores)
		    (not (locate-library "mew-nntp")))
	    (push (concat shimbun-dir "mew-shimbun.el") ignores))
	  ;; nnshimbun check
	  (unless (let ((gnus (locate-library "gnus")))
		    (when gnus
		      (let ((load-path (list (file-name-directory gnus))))
			(locate-library "nnimap"))))
	    (push (concat shimbun-dir "nnshimbun.el") ignores)))
      (push "mime-w3m.el" ignores)
      (push "octet.el" ignores))
    (unless (featurep 'mule)
      (push "w3m-weather.el" ignores)
      (push "w3m-symbol.el" ignores))
    ;; List shimbun modules which cannot be byte-compiled with this system.
    (let (list)
      ;; To byte-compile w3m-util.el and a version specific module first.
      (push "w3m-util.el" list)
      (setq modules (delete "w3m-util.el" modules))
      (push "w3m-proc.el" list)
      (setq modules (delete "w3m-proc.el" modules))
      (dolist (module version-specific-modules)
	(when (and (not (member module ignores))
		   (member module modules))
	  (setq modules (delete module modules))
	  (push module list)))
      (dolist (module modules)
	(unless (member module ignores)
	  (push module list)))
      (nreverse list))))

(defun w3mhack-examine-modules ()
  "Examine w3m modules should be byte-compile'd."
  (dolist (module (w3mhack-module-list))
    (princ (format "%sc " module))))

(defun w3mhack-compile ()
  "Byte-compile the w3m modules."
  (w3mhack-generate-load-file)
  (let (modules)
    (dolist (el (w3mhack-module-list))
      (let ((elc (concat el "c")))
	(if (file-exists-p elc)
	    (if (file-newer-than-file-p elc el)
		(message " `%s' is up to date" elc)
	      (delete-file elc)
	      (push el modules))
	  (push el modules))))
    (setq modules (nreverse modules))
    (while modules
      (condition-case nil
	  (byte-compile-file (car modules))
	(error))
      (setq modules (cdr modules)))))

(defun w3mhack-nonunix-install ()
  "Byte-compile the w3m modules and install them."
  (w3mhack-compile)
  (unless w3mhack-nonunix-lispdir
    (setq w3mhack-nonunix-lispdir
	  (expand-file-name "../../site-lisp/w3m" data-directory)))
  (and (not w3mhack-nonunix-icondir)
       (setq w3mhack-nonunix-icondir
	     (expand-file-name "images/w3m" data-directory)))
  (labels
      ((mkdir (dir)
	      (unless (file-directory-p dir)
		(message "mkdir %s" dir)
		(unless w3mhack-nonunix-dryrun
		  (make-directory dir 'parents))))
       (install (srcdir dstdir pattern)
		(dolist (src (directory-files srcdir t pattern))
		  (let ((dst (expand-file-name
			      (file-name-nondirectory src) dstdir)))
		    (message "cp %s %s"
			     (file-relative-name src default-directory) dst)
		    (unless w3mhack-nonunix-dryrun
		      (copy-file src dst t t))))))
    (mkdir w3mhack-nonunix-lispdir)
    (install default-directory w3mhack-nonunix-lispdir "\\.elc?\\'")
    (let ((shimbun-directory
	   (expand-file-name shimbun-module-directory default-directory)))
      (when (file-exists-p (expand-file-name "shimbun.elc" shimbun-directory))
	(install shimbun-directory w3mhack-nonunix-lispdir "\\.elc?\\'")))
    (when w3mhack-nonunix-icondir
      (mkdir w3mhack-nonunix-icondir)
      (install (expand-file-name (if (featurep 'xemacs)
				     "icons30"
				   "icons"))
	       w3mhack-nonunix-icondir "\\.\\(?:png\\|xpm\\)\\'"))))

;; Byte optimizers and version specific functions.
(condition-case nil
    (char-after)
  (wrong-number-of-arguments
   (put 'char-after 'byte-optimizer
	(lambda (form)
	  (if (cdr form)
	      form
	    '(char-after (point)))))))

(condition-case nil
    (char-before)
  (wrong-number-of-arguments
   (put 'char-before 'byte-optimizer
	(lambda (form)
	  (if (cdr form)
	      form
	    '(char-before (point))))))
  (void-function
   (put 'char-before 'byte-optimizer
	(lambda (form)
	  (if (cdr form)
	      (let ((pos (car (cdr form))))
		`(let ((cur (point)))
		   (prog2
		       (goto-char ,pos)
		       (if (bobp)
			   nil
			 (preceding-char))
		     (goto-char cur))))
	    '(if (bobp)
		 nil
	       (preceding-char)))))))

(put 'match-string-no-properties 'byte-optimizer
     (lambda (form)
       (let ((num (nth 1 form))
	     (string (nth 2 form)))
	 (cond ((and string (featurep 'xemacs))
		`(let ((num ,num))
		   (if (match-beginning num)
		       (let ((string (substring ,string
						(match-beginning num)
						(match-end num))))
			 (map-extents (lambda (extent maparg)
					(delete-extent extent))
				      string 0 (length string))
			 string))))
	       (string
		`(let ((num ,num))
		   (if (match-beginning num)
		       (let ((string (substring ,string
						(match-beginning num)
						(match-end num))))
			 (set-text-properties 0 (length string) nil string)
			 string))))
	       (t
		`(let ((num ,num))
		   (if (match-beginning num)
		       (buffer-substring-no-properties
			(match-beginning num) (match-end num)))))))))

(cond
 ((featurep 'xemacs)
  ;; Don't warn for the unused non-global variables.
  (setq byte-compile-warnings
	(delq 'unused-vars (copy-sequence byte-compile-default-warnings)))

  (defun w3mhack-byte-optimize-letX (form)
    "Do the source level optimization for the `let' and `let*' forms.
This function removes some obsolete variables in the 1st arg VARLIST.
Here are examples showing how of it works.

  (let ((pop-up-frames t)
	(pop-up-frame-alist PARAMETERS)
	(pop-up-frame-plist PARAMETERS))
    (pop-to-buffer BUFFER))
	|
	V
  (let ((pop-up-frames t)
	(pop-up-frame-plist PARAMETERS))
    (pop-to-buffer BUFFER))

  (let* ((pop-up-frames t)
	 (pop-up-frame-alist PARAMETERS)
	 (pop-up-frame-plist pop-up-frame-alist))
    (pop-to-buffer BUFFER))
	|
	V
  (let* ((pop-up-frames t)
	 (pop-up-frame-plist PARAMETERS))
    (pop-to-buffer BUFFER))
"
    (let ((obsoletes '(pop-up-frame-alist))
	  (varlist (copy-sequence (cadr form)))
	  obsolete elements element value)
      (while (setq obsolete (pop obsoletes))
	(setq elements varlist
	      varlist nil)
	(while (setq element (pop elements))
	  (if (or (prog1
		      (eq obsolete element)
		    (setq value nil))
		  (when (eq obsolete (car-safe element))
		    (setq value (unless (eq obsolete (cadr element))
				  (cadr element)))
		    t))
	      (when (eq 'let* (car form))
		(while (setq element (rassoc (list obsolete) elements))
		  (setcdr element (list value))))
	    (push element varlist)))
	(setq varlist (nreverse varlist)))
      (setcar (cdr form) varlist))
    form)

  (require 'byte-optimize)
  (defadvice byte-optimize-form-code-walker
    (before w3mhack-byte-optimize-letX activate compile)
    "Byte optimize `let' or `let*' FORM in the source level
to remove some obsolete variables in the first argument VARLIST."
    (when (memq (car-safe (ad-get-arg 0)) '(let let*))
      (ad-set-arg 0 (w3mhack-byte-optimize-letX (ad-get-arg 0)))))

  ;; We dare to do it even though it might be a futile work, since the
  ;; doc-string says "You should NEVER use this function".  If the
  ;; function `set-text-properties' is used for a whole string, it
  ;; will make the program run a little bit faster.
  (put 'set-text-properties 'byte-optimizer
       (lambda (form)
	 (let ((start (nth 1 form))
	       (end (nth 2 form))
	       (props (nth 3 form))
	       (string (nth 4 form)))
	   (if (and string
		    (zerop start)
		    (eq 'length (car-safe end))
		    (eq string (car-safe (cdr-safe end))))
	       (if props
		   `(let ((end ,end))
		      (map-extents (lambda (extent maparg)
				     (delete-extent extent))
				   ,string 0 end)
		      (add-text-properties 0 end ,props ,string))
		 `(map-extents (lambda (extent maparg)
				 (delete-extent extent))
			       ,string 0 ,end))
	     form))))

  (defun w3mhack-make-package ()
    "Make some files in the XEmacs package directory."
    (let* ((temp-dir (expand-file-name
		      ;; A dir name must be ended with "/w3m".
		      "w3m"
		      (make-temp-name (expand-file-name "pkgtmp"))))
	   (make-hardlink 'add-name-to-file)
	   (package-dir (pop command-line-args-left))
	   (lisp-dir (expand-file-name "lisp/w3m/" package-dir))
	   (custom-load (expand-file-name "custom-load.el" lisp-dir))
	   (generated-autoload-file (expand-file-name "auto-autoloads.el"
						      temp-dir))
	   (els (nconc (directory-files default-directory nil "^[^#]+\\.el\\'")
		       (directory-files (expand-file-name
					 shimbun-module-directory)
					nil "^[^#]+\\.el\\'")))
	   (elcs (with-temp-buffer
		   (let ((standard-output (current-buffer)))
		     (w3mhack-examine-modules)
		     (split-string (buffer-string) " \\(?:shimbun/\\)?"))))
	   (icons (directory-files (expand-file-name "icons30/") nil
				   "^[^#]+\\.\\(?:png\\|xpm\\)\\'"))
	   (infos (directory-files (expand-file-name "doc/") nil
				   "^[^#]+\\.info\\(?:-[0-9]+\\)?\\'"))
	   (si:message (symbol-function 'message))
	   hardlink manifest make-backup-files noninteractive)
      ;; Non-Mule XEmacs cannot handle .el files containing non-ascii chars.
      ;; So, we should make custom-load.el and auto-autoloads.el for only
      ;; .el files which have been byte-compiled.  The following processings
      ;; are performed in the `temp-dir' only to those files.
      (make-directory temp-dir t)
      (unless (condition-case nil
		  (progn
		    (setq hardlink (expand-file-name "w3m.el" temp-dir))
		    (funcall make-hardlink "w3m.el" hardlink)
		    (prog1
			(file-exists-p hardlink)
		      (delete-file hardlink)))
		(error nil))
	;; This system doesn't allow hard links.
	(setq make-hardlink 'copy-file))
      (dolist (el (cons w3mhack-load-file (w3mhack-module-list)))
	(funcall make-hardlink
		 el (expand-file-name (file-name-nondirectory el) temp-dir)))
      (with-temp-buffer
	(let ((standard-output (current-buffer)))
	  (Custom-make-dependencies temp-dir))
	;; Print messages into stderr.
	(message "%s" (buffer-string)))
      (if (file-exists-p (expand-file-name "custom-load.el" temp-dir))
	  (progn
	    (copy-file (expand-file-name "custom-load.el" temp-dir)
		       custom-load t)
	    (require 'cus-load)
	    (byte-compile-file custom-load)
	    (push "custom-load.el" els)
	    (push "custom-load.elc" elcs))
	(when (file-exists-p custom-load)
	  (delete-file custom-load))
	(when (file-exists-p (concat custom-load "c"))
	  (delete-file (concat custom-load "c"))))
      (message "Updating autoloads for the directory %s..." lisp-dir)
      (w3mhack-generate-xemacs-load-file generated-autoload-file)
      (if (file-exists-p generated-autoload-file)
	  (progn
	    (copy-file generated-autoload-file
		       (expand-file-name "auto-autoloads.el" lisp-dir) t)
	    (byte-compile-file (expand-file-name "auto-autoloads.el" lisp-dir))
	    (push "auto-autoloads.el" els)
	    (push "auto-autoloads.elc" elcs))
	(setq generated-autoload-file (expand-file-name "auto-autoloads.el"
							lisp-dir))
	(when (file-exists-p generated-autoload-file)
	  (delete-file generated-autoload-file))
	(when (file-exists-p (concat generated-autoload-file "c"))
	  (delete-file (concat generated-autoload-file "c"))))
      ;; Clear the `temp-dir'.
      (dolist (el (directory-files temp-dir t "\\.el\\'"))
	(delete-file el))
      (delete-directory temp-dir)
      (delete-directory (expand-file-name ".." temp-dir))
      (when (file-directory-p (expand-file-name "pkginfo/" package-dir))
	(setq manifest (expand-file-name "pkginfo/MANIFEST.w3m" package-dir))
	(message "Generating %s..." manifest)
	(with-temp-file manifest
	  (insert "pkginfo/MANIFEST.w3m\n")
	  (dolist (log (directory-files lisp-dir nil
					"^ChangeLog\\(?:\\.[0-9]+\\)?\\'"))
	    (insert "lisp/w3m/" log "\n"))
	  (dolist (el els)
	    (insert "lisp/w3m/" el "\n")
	    (when (member (concat el "c") elcs)
	      (insert "lisp/w3m/" el "c\n")))
	  (dolist (icon icons)
	    (insert "etc/images/w3m/" icon "\n"))
	  (dolist (info infos)
	    (insert "info/" info "\n")))
	(message "Generating %s...done" manifest)))))

 (t
  ;; Don't warn for the dummy autoloads and mis-judging of the cl
  ;; run-time functions.
  (setq byte-compile-warnings
	(delq 'noruntime
	      (delq 'cl-functions
		    (copy-sequence byte-compile-warning-types))))))

(defun w3mhack-load-path ()
  "Print default value of additional load paths for w3m.el."
  (let (paths x)
    (and (or (featurep 'xemacs)
	     (locate-library "mime-def"))
	 (setq x (locate-library "poe"))
	 (progn
	   (setq x (file-name-directory x))
	   (if (string-match "/emu/\\'" x)
	       (push (substring x 0 (1+ (match-beginning 0))) paths))
	   (push x paths)))
    (if (setq x (locate-library "mime-def"))
	(push (file-name-directory x) paths))
    (if (setq x (locate-library "mew"))
	(push (file-name-directory x) paths))
    (if (setq x (locate-library "gnus"))
	(push (file-name-directory x) paths))
    (if (setq x (locate-library "un-define"))
	(push (file-name-directory x) paths))
    (let (print-level print-length)
      (princ (mapconcat
	      (function directory-file-name)
	      (nreverse paths) ":")))))

(defun w3mhack-what-where ()
  "Show what files should be installed and where should they go."
  (let ((lisp-dir (pop command-line-args-left))
	(icon-dir (pop command-line-args-left))
	(package-dir (pop command-line-args-left))
	(info-dir (pop command-line-args-left)))
    (message "
lispdir=%s
ICONDIR=%s
PACKAGEDIR=%s
infodir=%s"
	     lisp-dir icon-dir package-dir info-dir)
    (setq info-dir (file-name-as-directory info-dir))
    (message "
install:
  *.el, *.elc, ChangeLog* -> %s
  *.info, *.info-*        -> %s"
	     (file-name-as-directory lisp-dir) info-dir)
    (setq icon-dir (file-name-as-directory icon-dir))
    (unless (string-equal "NONE/" icon-dir)
      (message "
install-icons:
  *.gif, *.png, *.xpm     -> %s"
	       icon-dir))
    (setq package-dir (file-name-as-directory package-dir))
    (message "
install-info:
  *.info, *.info-*        -> %s"
	     info-dir)
    (unless (string-equal "NONE/" package-dir)
      (message "
install-package:
  *.el, *.elc, ChangeLog* -> %slisp/w3m/
  *.gif, *.png, *.xpm     -> %setc/images/w3m/
  *.info, *.info-*        -> %sinfo/
  MANIFEST.w3m            -> %spkginfo/"
	       package-dir package-dir package-dir package-dir)))
  (message (if (featurep 'xemacs) "\n" "")))

(defun w3mhack-makeinfo ()
  "Emacs makeinfo in batch mode.
NOTE: This function must be called from the top directory."
  (let ((file (pop command-line-args-left))
	auto-save-default
	find-file-run-dired
	coding-system-for-write
	output-coding-system
	(error 0)
	(load-path load-path)
	(texinfmt (locate-library "texinfmt")))
    ;; ptexinfmt.elc requires texinfmt.elc and texinfmt.elc requires
    ;; texinfo.elc in the same directory where texinfmt.elc is installed.
    ;; However, another version of texinfo.elc (e.g. auctex provides such
    ;; one) may be loaded depending on the value of the `load-path'.  So,
    ;; we need to force it to load the correct one.
    (when texinfmt
      (push (file-name-directory texinfmt) load-path))
    (load "doc/ptexinfmt.el" nil t t)
    (cd "doc")
    (if (and (string-match "-ja\\.texi\\'" file)
	     (not (featurep 'mule)))
	(message "Cannot format %s with this version of %sEmacs (ignored)"
		 file
		 (if (featurep 'xemacs) "X" ""))
      (condition-case err
	  (progn
	    (find-file file)
	    (setq buffer-read-only nil)
	    (buffer-disable-undo (current-buffer))
	    (when (boundp 'buffer-file-coding-system)
	      (setq coding-system-for-write
		    (symbol-value 'buffer-file-coding-system)))
	    ;; Remove unsupported commands.
	    (goto-char (point-min))
	    (while (re-search-forward "@\\(?:end \\)?ifnottex" nil t)
	      (replace-match ""))
	    (goto-char (point-min))
	    (while (search-forward "\n@iflatex\n" nil t)
	      (delete-region (1+ (match-beginning 0))
			     (search-forward "\n@end iflatex\n")))
	    ;; Insert @include files.
	    (goto-char (point-min))
	    (set-syntax-table texinfo-format-syntax-table)
	    (let (start texinfo-command-end filename)
	      (while (re-search-forward "^@include" nil t)
		(setq start (match-beginning 0)
		      texinfo-command-end (point)
		      filename (texinfo-parse-line-arg))
		(delete-region start (point-at-bol 2))
		(message "Reading included file: %s" filename)
		(save-excursion
		  (save-restriction
		    (narrow-to-region
		     (point) (+ (point)
				(car (cdr (insert-file-contents filename)))))
		    (goto-char (point-min))
		    ;; Remove `@setfilename' line from included file, if any,
		    ;; so @setfilename command not duplicated.
		    (if (re-search-forward "^@setfilename"
					   (point-at-eol 100) t)
			(delete-region (point-at-bol 1) (point-at-bol 2)))))))
	    ;; Remove ignored areas.
	    (goto-char (point-min))
	    (while (re-search-forward "^@ignore[\t\r ]*$" nil t)
	      (delete-region (match-beginning 0)
			     (if (re-search-forward
				  "^@end[\t ]+ignore[\t\r ]*$" nil t)
				 (1+ (match-end 0))
			       (point-max))))
	    ;; Format @key{...}.
	    (goto-char (point-min))
	    (while (re-search-forward "@key{\\([^}]+\\)}" nil t)
	      (replace-match "<\\1>"))
	    ;;
	    (texinfo-every-node-update)
	    (set-buffer-modified-p nil)
	    (message "texinfo formatting %s..." file)
	    (let ((si:message (symbol-function 'message))
		  (si:push-mark (symbol-function 'push-mark)))
	      (fset
	       'message
	       (cond ((featurep 'mule)
		      ;; Encode messages to terminal.
		      (let ((coding
			     (or (and (boundp 'current-language-environment)
				      (string-match
				       "\\`Japanese"
				       current-language-environment)
				      (boundp 'locale-coding-system)
				      locale-coding-system)
				 'iso-2022-7bit)))
			(byte-compile
			 (cond ((featurep 'xemacs)
				`(lambda (fmt &rest args)
				   (unless (and (string-equal fmt "%s clean")
						(equal (car args)
						       buffer-file-name))
				     (funcall ,si:message "%s"
					      (encode-coding-string
					       (apply 'format fmt args)
					       ',coding)))))
			       (t
				`(lambda (fmt &rest args)
				   (funcall ,si:message "%s"
					    (encode-coding-string
					     (apply 'format fmt args)
					     ',coding))))))))
		     ((featurep 'xemacs)
		      (byte-compile
		       `(lambda (fmt &rest args)
			  (unless (and (string-equal fmt "%s clean")
				       (equal (car args) buffer-file-name))
			    (apply ,si:message fmt args)))))
		     (t
		      si:message)))
	      ;; Silence it when formatting @multitable section.
	      (fset 'push-mark
		    (byte-compile
		     `(lambda (&rest args)
			(apply ,si:push-mark (car args) t (cddr args)))))
	      (unwind-protect
		  (texinfo-format-buffer t)
		(fset 'message si:message)
		(fset 'push-mark si:push-mark)))
	    (if (buffer-modified-p)
		(progn (message "Saving modified %s" (buffer-file-name))
		       (save-buffer))))
	(error
	 (message ">> Error: %s" (prin1-to-string err))
	 (message ">>  point at")
	 (let ((s (buffer-substring (point) (min (+ (point) 100) (point-max))))
	       (tem 0))
	   (while (setq tem (string-match "\n+" s tem))
	     (setq s (concat (substring s 0 (match-beginning 0))
			     "\n>>  "
			     (substring s (match-end 0)))
		   tem (1+ tem)))
	   (message ">>  %s" s))
	 (setq error 1))))
    (kill-emacs error)))

(defun w3mhack-update-files-autoloads (files)
  "Run `update-file-autoloads' with FILES, silently in XEmacs."
  (cond ((featurep 'xemacs)
	 (let ((si:message (symbol-function 'message)))
	   (defun message (fmt &rest args)
	     "Ignore useless messages while generating autoloads."
	     (cond ((and (string-equal "Generating autoloads for %s..." fmt)
			 (file-exists-p (file-name-nondirectory (car args))))
		    (funcall si:message
			     fmt (file-name-nondirectory (car args))))
		   ((string-equal "No autoloads found in %s" fmt))
		   ((string-equal "Generating autoloads for %s...done" fmt))
		   (t (apply si:message fmt args))))
	   (unwind-protect
	       (dolist (file files)
		 (update-file-autoloads file))
	     (fset 'message si:message))))
	((boundp 'generated-autoload-load-name)
	 (dolist (file files)
	   (let ((generated-autoload-load-name
		  (file-name-sans-extension (file-name-nondirectory file))))
	     (update-file-autoloads file))))
	(t
	 (dolist (file files)
	   (update-file-autoloads file)))))

(defun w3mhack-generate-load-file ()
  "Generate a file including all autoload stubs."
  (require 'autoload)

  (unless (make-autoload '(define-minor-mode foo "bar") "baz")
    ;; for XEmacs 21.4.
    (defadvice make-autoload (around support-unsupported-forms (form file)
				     activate)
      "Support unsupported forms."
      (unless ad-do-it
	(setq ad-return-value
	      (cond ((eq (car form) 'define-minor-mode)
		     (put 'define-minor-mode 'doc-string-elt 3)
		     (list 'autoload (list 'quote (nth 1 form))
			   file (nth 2 form) t nil))
		    (t
		     nil))))))

  (let ((files (w3mhack-module-list))
	(generated-autoload-file (expand-file-name w3mhack-load-file))
	(make-backup-files nil)
	(autoload-package-name "emacs-w3m"))
    (if (and (file-exists-p w3mhack-load-file)
	     (not (catch 'modified
		    (dolist (file files)
		      (when (file-newer-than-file-p file w3mhack-load-file)
			(throw 'modified t))))))
	(message " `%s' is up to date" w3mhack-load-file)
      (when (fboundp 'autoload-ensure-default-file)
	(autoload-ensure-default-file generated-autoload-file))
      (with-current-buffer (find-file-noselect generated-autoload-file)
	(if (fboundp 'autoload-ensure-default-file)
	    (let ((case-fold-search t))
	      (goto-char (point-min))
	      (when (re-search-forward
		     "^[\t ]*\\(;+[\t ]*Local[\t ]+Variables:\\)"
		     nil t)
		(delete-region (point-min) (match-beginning 1))
		(goto-char (point-min))))
	  (erase-buffer))
	(insert ";;; " w3mhack-load-file "\
 --- automatically extracted autoload
;;
;; This file should be generated by make in emacs-w3m source directory.
;;
;;; Code:

\(provide '" (file-name-sans-extension w3mhack-load-file) ")
")
	;; When missing ^L, `update-file-autoloads' of old emacsen
	;; such as Mule2.3 signals an error.
	(unless (featurep 'xemacs)
	  (insert "\C-l\n"))
	(w3mhack-update-files-autoloads files)
	(unless (fboundp 'autoload-ensure-default-file)
	  (goto-char (point-max))
	  (insert (if (featurep 'xemacs) "\C-l\n" "")
		  ";;; " w3mhack-load-file " ends here\n"))
	(save-buffer)))))

(defun w3mhack-generate-xemacs-load-file (file)
  (with-temp-buffer
    (insert "\
;;; DO NOT MODIFY THIS FILE
\(if (featurep 'w3m-autoloads) (error \"Already loaded\"))
\(or (featurep '" (file-name-sans-extension w3mhack-load-file) ")
    (load (expand-file-name \"" w3mhack-load-file "\"
			    (if load-file-name
				(file-name-directory load-file-name)))
	  nil t t))
\(provide 'w3m-autoloads)
")
    (write-region (point-min) (point-max) file)))

(defun w3mhack-locate-library ()
  "Print the precise file name of Emacs library remaining on the commane line."
  (princ (or (locate-library (pop command-line-args-left)) "")))

(defun w3mhack-print-status ()
  (princ "OK"))

(defun w3mhack-version ()
  "Print the value of `emacs-w3m-version'."
  (require 'w3m)
  (let (print-level print-length)
    (princ emacs-w3m-version)))

;;; w3mhack.el ends here

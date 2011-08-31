;; This file is used for the make rule `very-slow' which adds the user
;; specific additional directories and the current source directories
;; to `load-path'.

;; Add `configure-package-path' to `load-path' for XEmacs.  Those paths
;; won't appear in `load-path' when XEmacs starts with the `-vanilla'
;; option or the `-no-autoloads' option because of a bug. :<
(if (and (featurep 'xemacs)
	 (boundp 'configure-package-path)
	 (listp configure-package-path))
    (let ((paths
	   (apply 'nconc
		  (mapcar
		   (lambda (path)
		     (if (and (stringp path)
			      (not (string-equal path ""))
			      (file-directory-p
			       (setq path (expand-file-name "lisp" path))))
			 (directory-files path t)))
		   configure-package-path)))
	  path adds)
      (while paths
	(setq path (car paths)
	      paths (cdr paths))
	(if (and path
		 (not (or (string-match "/\\.\\.?\\'" path)
			  (member (file-name-as-directory path) load-path)
			  (member path load-path)))
		 (file-directory-p path))
	    (setq adds (cons (file-name-as-directory path) adds))))
      (setq load-path (nconc (nreverse adds) load-path))))

(let ((addpath (prog1
		   (or (car command-line-args-left)
		       "NONE")
		 (setq command-line-args-left (cdr command-line-args-left))))
      path paths)
  (while (string-match "\\([^\0-\37:]+\\)[\0-\37:]*" addpath)
    (setq path (expand-file-name (substring addpath
					    (match-beginning 1)
					    (match-end 1)))
	  addpath (substring addpath (match-end 0)))
    (if (file-directory-p path)
	(setq paths (cons path paths))))
  (or (null paths)
      (setq load-path (append (nreverse paths) load-path))))
(setq load-path (append (list default-directory
			      (expand-file-name "shimbun")) load-path))

(if (and (boundp 'emacs-major-version)
	 (>= emacs-major-version 21))
    (defadvice load (before nomessage activate)
      "Shut up `Loading...' message."
      (ad-set-arg 2 t)))

;; Check whether the shell command can be used.
(let ((test (lambda (shell)
	      (let ((buffer (generate-new-buffer " *temp*"))
		    (msg "Hello World"))
		(save-excursion
		  (set-buffer buffer)
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
	      (error "%s" "\n\
There seems to be no shell command which is equivalent to /bin/sh.
 Try ``make SHELL=foo [option...]'', where `foo' is the absolute
 path name for the proper shell command in your system.\n"))))))

;; Load custom and bind defcustom'ed variables for Emacs 19.
(if (>= emacs-major-version 20)
    nil
  (require 'custom)
  (put 'custom-declare-variable 'byte-hunk-handler
       'byte-compile-file-form-custom-declare-variable)
  (defun byte-compile-file-form-custom-declare-variable (form)
    (if (memq 'free-vars byte-compile-warnings)
	(setq byte-compile-bound-variables
	      (cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
    (if (memq ':version (nthcdr 4 form))
	;; Make the variable uncustomizable.
	`(defvar ,(nth 1 (nth 1 form)) ,(nth 1 (nth 2 form))
	   ,(substring (nth 3 form) (if (string-match "^[\t *]+" (nth 3 form))
					(match-end 0)
				      0)))
      ;; Ignore unsupported keyword(s).
      (if (memq ':set-after (nthcdr 4 form))
	  (let ((newform (list (car form) (nth 1 form)
			       (nth 2 form) (nth 3 form)))
		(args (nthcdr 4 form)))
	    (while args
	      (or (eq (car args) ':set-after)
		  (setq newform (nconc newform (list (car args)
						     (car (cdr args))))))
	      (setq args (cdr (cdr args))))
	    newform)
	form)))

  ;; Make it run quietly.
  (defun locate-library (library &optional nosuffix)
    "Show the full path name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY (a la calling `load' instead of `load-library')."
    (interactive "sLocate library: ")
    (catch 'answer
      (mapcar
       '(lambda (dir)
	  (mapcar
	   '(lambda (suf)
	      (let ((try (expand-file-name (concat library suf) dir)))
		(and (file-readable-p try)
		     (null (file-directory-p try))
		     (progn
		       (or noninteractive
			   (message "Library is file %s" try))
		       (throw 'answer try)))))
	   (if nosuffix '("") '(".elc" ".el" ""))))
       load-path)
      (or noninteractive
	  (message "No library %s in search path" library))
      nil))

  (condition-case nil
      (char-after)
    (wrong-number-of-arguments
     (put 'char-after 'byte-optimizer
	  (lambda (form)
	    (if (cdr form)
		form
	      '(char-after (point))))))))

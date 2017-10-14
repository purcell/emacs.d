;;; skewer-bower.el --- dynamic library loading -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This package loads libraries into the current page using the bower
;; infrastructure. Note: bower is not actually used by this package
;; and so does *not* need to be installed. Only git is required (see
;; `skewer-bower-git-executable'). It will try to learn how to run git
;; from Magit if available.

;; The interactive command for loading libraries is
;; `skewer-bower-load'. It will prompt for a library and a version,
;; automatically fetching it from the bower infrastructure if needed.
;; For example, I often find it handy to load some version of jQuery
;; when poking around at a page that doesn't already have it loaded.

;; Caveat: unfortunately the bower infrastructure is a mess; many
;; packages are in some sort of broken state -- missing dependencies,
;; missing metadata, broken metadata, or an invalid repository URL.
;; Some of this is due to under-specification of the metadata by the
;; bower project. Broken packages are unlikely to be loadable by
;; skewer-bower.

;;; Code:

(require 'cl-lib)
(require 'skewer-mode)
(require 'simple-httpd)
(require 'magit nil t) ; optional

(defcustom skewer-bower-cache-dir (locate-user-emacs-file "skewer-cache")
  "Location of library cache (git repositories)."
  :type 'string
  :group 'skewer)

(defcustom skewer-bower-endpoint "https://bower.herokuapp.com"
  "Endpoint for accessing package information."
  :type 'string
  :group 'skewer)

(defcustom skewer-bower-json '("bower.json" "package.json" "component.json")
  "Files to search for package metadata."
  :type 'list
  :group 'skewer)

; Try to match Magit's configuration if available
(defcustom skewer-bower-git-executable "git"
  "Name of the git executable."
  :type 'string
  :group 'skewer)

(defvar skewer-bower-packages nil
  "Alist of all packages known to bower.")

(defvar skewer-bower-refreshed nil
  "List of packages that have been refreshed recently. This keeps
them from hitting the network frequently.")

;;;###autoload
(defun skewer-bower-refresh ()
  "Update the package listing and packages synchronously."
  (interactive)
  (cl-declare (special url-http-end-of-headers))
  (setf skewer-bower-refreshed nil)
  (with-current-buffer
      (url-retrieve-synchronously (concat skewer-bower-endpoint "/packages"))
    (setf (point) url-http-end-of-headers)
    (setf skewer-bower-packages
          (cl-sort
           (cl-loop for package across (json-read)
                    collect (cons (cdr (assoc 'name package))
                                  (cdr (assoc 'url package))))
           #'string< :key #'car))))

;; Git functions

(defun skewer-bower-cache (package)
  "Return the cache repository directory for PACKAGE."
  (unless (file-exists-p skewer-bower-cache-dir)
    (make-directory skewer-bower-cache-dir t))
  (expand-file-name package skewer-bower-cache-dir))

(defun skewer-bower-git (package &rest args)
  "Run git for PACKAGE's repository with ARGS."
  (with-temp-buffer
    (when (zerop (apply #'call-process skewer-bower-git-executable nil t nil
                        (format "--git-dir=%s" (skewer-bower-cache package))
                        args))
      (buffer-string))))

(defun skewer-bower-git-clone (url package)
  "Clone or fetch PACKAGE's repository from URL if needed."
  (if (member package skewer-bower-refreshed)
      t
    (let* ((cache (skewer-bower-cache package))
           (status
            (if (file-exists-p cache)
                (when (skewer-bower-git package "fetch")
                  (push package skewer-bower-refreshed))
              (skewer-bower-git package "clone" "--bare" url cache))))
      (not (null status)))))

(defun skewer-bower-git-show (package version file)
  "Grab FILE from PACKAGE at version VERSION."
  (when (string-match-p "^\\./" file) ; avoid relative paths
    (setf file (substring file 2)))
  (skewer-bower-git package "show" (format "%s:%s" version file)))

(defun skewer-bower-git-tag (package)
  "List all the tags in PACKAGE's repository."
  (split-string (skewer-bower-git package "tag")))

;; Bower functions

(defun skewer-bower-package-ensure (package)
  "Ensure a package is installed in the cache and up to date.
Emit an error if the package could not be ensured."
  (when (null skewer-bower-packages) (skewer-bower-refresh))
  (let ((url (cdr (assoc package skewer-bower-packages))))
    (when (null url)
      (error "Unknown package: %s" package))
    (when (null (skewer-bower-git-clone url package))
      (error "Failed to fetch: %s" url))
    t))

(defun skewer-bower-package-versions (package)
  "List the available versions for a package. Always returns at
least one version."
  (skewer-bower-package-ensure package)
  (or (sort (skewer-bower-git-tag package) #'string<)
      (list "master")))

(defun skewer-bower-get-config (package &optional version)
  "Get the configuration alist for PACKAGE at VERSION. Return nil
if no configuration could be found."
  (skewer-bower-package-ensure package)
  (unless version (setf version "master"))
  (json-read-from-string
   (cl-loop for file in skewer-bower-json
            for config = (skewer-bower-git-show package version file)
            when config return it
            finally (return "null"))))

;; Serving the library

(defvar skewer-bower-history ()
  "Library selection history for `completing-read'.")

(defun skewer-bowser--path (package version main)
  "Return the simple-httpd hosted path for PACKAGE."
  (format "/skewer/bower/%s/%s/%s" package (or version "master") main))

(defun skewer-bower-prompt-package ()
  "Prompt for a package and version from the user."
  (when (null skewer-bower-packages) (skewer-bower-refresh))
  ;; ido-completing-read bug workaround:
  (when (> (length skewer-bower-history) 32)
    (setf skewer-bower-history (cl-subseq skewer-bower-history 0 16)))
  (let* ((packages (mapcar #'car skewer-bower-packages))
         (selection (nconc skewer-bower-history packages))
         (package (completing-read "Library: " selection nil t nil
                                   'skewer-bower-history))
         (versions (reverse (skewer-bower-package-versions package)))
         (version (completing-read "Version: " versions
                                   nil t nil nil (car versions))))
    (list package version)))

(defun skewer-bower--js-p (filename)
  "Return non-nil if FILENAME looks like JavaScript."
  (string-match "\\.js$" filename))

(defun skewer-bower-guess-main (package version config)
  "Attempt to determine the main entrypoints from a potentially
incomplete or incorrect bower configuration. Returns nil if
guessing failed."
  (let ((check (apply-partially #'skewer-bower-git-show package version))
        (main (cdr (assoc 'main config))))
    (cond ((and (vectorp main) (cl-some check main))
           (cl-coerce (cl-remove-if-not #'skewer-bower--js-p main) 'list))
          ((and (stringp main) (funcall check main))
           (list main))
          ((funcall check (concat package ".js"))
           (list (concat package ".js")))
          ((funcall check package)
           (list package)))))

;;;###autoload
(defun skewer-bower-load (package &optional version)
  "Dynamically load a library from bower into the current page."
  (interactive (skewer-bower-prompt-package))
  (let* ((config (skewer-bower-get-config package version))
         (deps (cdr (assoc 'dependencies config)))
         (main (skewer-bower-guess-main package version config)))
    (when (null main)
      (error "Could not load %s (%s): no \"main\" entrypoint specified"
             package version))
    (cl-loop for (dep . version) in deps
             do (skewer-bower-load (format "%s" dep) version))
    (cl-loop for entrypoint in main
             for path = (skewer-bowser--path package version entrypoint)
             do (skewer-eval path nil :type "script"))))

(defservlet skewer/bower "application/javascript; charset=utf-8" (path)
  "Serve a script from the local bower repository cache."
  (cl-destructuring-bind (_ _skewer _bower package version . parts)
      (split-string path "/")
    (let* ((file (mapconcat #'identity parts "/"))
           (contents (skewer-bower-git-show package version file)))
      (if contents
          (insert contents)
        (httpd-error t 404)))))

(provide 'skewer-bower)

;;; skewer-bower.el ends here

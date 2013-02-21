;;----------------------------------------------------------------------------
;; Set load path
;;----------------------------------------------------------------------------
(eval-when-compile (require 'cl))
(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (loop for dir in (directory-files parent-dir)
                   unless (string-match "^\\." dir)
                   collecting (expand-file-name dir))
             load-path)))))

(sanityinc/add-subdirs-to-load-path "~/.emacs.d/site-lisp/")


;;----------------------------------------------------------------------------
;; Ensure we're freshly byte-compiled
;;----------------------------------------------------------------------------
;(require 'bytecomp)
;(byte-recompile-directory "~/.emacs.d/site-lisp" 0)


;;----------------------------------------------------------------------------
;; Utilities for grabbing upstream libs
;;----------------------------------------------------------------------------
(defun site-lisp-dir-for (name)
  (expand-file-name (format "~/.emacs.d/site-lisp/%s" name)))

(defun site-lisp-library-el-path (name)
  (expand-file-name (format "%s.el" name) (site-lisp-dir-for name)))

(defun download-site-lisp-module (name url)
  (let ((dir (site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir)
      (add-to-list 'load-path dir))
    (let ((el-file (site-lisp-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (site-lisp-library-loadable-p name)
    (byte-compile-file (download-site-lisp-module name url))))

(defun site-lisp-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/site-lisp/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (site-lisp-dir-for name)) f))))

(defun ensure-lib-from-svn (name url)
  (let ((dir (site-lisp-dir-for name)))
    (unless (site-lisp-library-loadable-p name)
      (message "Checking out %s from svn" name)
      (save-excursion
        (shell-command (format "svn co %s %s" url dir) "*site-lisp-svn*"))
      (add-to-list 'load-path dir))))


;;----------------------------------------------------------------------------
;; Fix up some load paths for libs from git submodules
;;----------------------------------------------------------------------------

(let ((html5-el-dir "~/.emacs.d/site-lisp/html5-el"))
 (unless (file-directory-p (expand-file-name "relaxng" html5-el-dir))
   (if (and (executable-find "svn") (executable-find "make"))
       (progn
         (message "Setting up html5-el relaxng schema info")
         (shell-command (format "cd %s && make relaxng" html5-el-dir) "*make relaxng*"))
     (error "Please run 'make relaxng' in %s" html5-el-dir))))

(defun refresh-site-lisp-submodules ()
  (interactive)
  (message "Updating site-lisp git submodules")
  (shell-command "cd ~/.emacs.d && git submodule foreach 'git pull' &" "*site-lisp-submodules*"))

;;----------------------------------------------------------------------------
;; Download these upstream libs
;;----------------------------------------------------------------------------

(defun remove-site-lisp-libs ()
  (shell-command "cd ~/.emacs.d && grep -e '^site-lisp/' .gitignore|xargs rm -rf"))

(defun ensure-site-lisp-libs ()
  (unless (> emacs-major-version 23)
    (ensure-lib-from-url
     'package
     "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el")))



(defun refresh-site-lisp ()
  (interactive)
  (refresh-site-lisp-submodules)
  (remove-site-lisp-libs)
  (ensure-site-lisp-libs))


(ensure-site-lisp-libs)

(provide 'init-site-lisp)

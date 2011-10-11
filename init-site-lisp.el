;;----------------------------------------------------------------------------
;; Set load path
;;----------------------------------------------------------------------------
(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path
              (append
               (loop for dir in (directory-files my-lisp-dir)
                     unless (string-match "^\\." dir)
                     collecting (expand-file-name dir))
               load-path)))))


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
    (url-copy-file url (site-lisp-library-el-path name) t nil)))

(defun ensure-lib-from-url (name url)
  (unless (site-lisp-library-loadable-p name)
    (download-site-lisp-module name url)))

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

(unless (file-directory-p (expand-file-name "~/.emacs.d/site-lisp/html5-el/relaxng"))
  (error "Please run 'make relaxng' in site-lisp/html5-el"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/session/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/contrib/lisp"))

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
    (ensure-lib-from-url 'package "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el"))
  (ensure-lib-from-url 'todochiku "http://www.emacswiki.org/emacs/download/todochiku.el")
  ;; TODO: consider smooth-scroll instead
  (ensure-lib-from-url 'smooth-scrolling "http://adamspiers.org/computing/elisp/smooth-scrolling.el")
  (ensure-lib-from-url 'edit-server "http://github.com/stsquad/emacs_chrome/raw/master/servers/edit-server.el")
  (ensure-lib-from-url 'dsvn "http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/emacs/dsvn.el")

  (ensure-lib-from-svn 'rdebug "http://ruby-debug.rubyforge.org/svn/trunk/emacs/")
  (ensure-lib-from-svn 'ruby-mode "http://svn.ruby-lang.org/repos/ruby/trunk/misc/"))



(defun refresh-site-lisp ()
  (interactive)
  (refresh-site-lisp-submodules)
  (remove-site-lisp-libs)
  (ensure-site-lisp-libs))


(ensure-site-lisp-libs)

(provide 'init-site-lisp)

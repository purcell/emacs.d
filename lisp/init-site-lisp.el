;;; Set load path

(eval-when-compile (require 'cl))
(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(sanityinc/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

;;; Utilities for grabbing upstream libs

(defun site-lisp-dir-for (name)
  (expand-file-name (format "site-lisp/%s" name) user-emacs-directory))

(defun site-lisp-library-el-path (name)
  (expand-file-name (format "%s.el" name) (site-lisp-dir-for name)))

(defun download-site-lisp-module (name url)
  (let ((dir (site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
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


(provide 'init-site-lisp)

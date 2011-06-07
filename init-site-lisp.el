;; Utilities for grabbing upstream libs

(defun grab-site-lisp-module (name url)
  (let ((dir (expand-file-name (format "~/.emacs.d/site-lisp/%s" name))))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir)
      (add-to-list 'load-path dir))
    (url-copy-file url (expand-file-name (format "%s.el" name) dir) t nil)))

(defun ensure-lib-from-url (name url)
  (unless (require name nil t)
    (grab-site-lisp-module name url)))

(defun ensure-lib-from-svn (name url)
  (unless (require name nil t)
    (let ((dir (expand-file-name (format "~/.emacs.d/site-lisp/%s" name))))
      (shell-command (format "svn co %s %s" url dir))
      (add-to-list 'load-path dir))))


;; Fix up some load paths for libs from git submodules

(unless (file-directory-p (expand-file-name "~/.emacs.d/site-lisp/html5-el/relaxng"))
  (error "Please run 'make relaxng' in site-lisp/html5-el"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/session/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/contrib/lisp"))

;; Download these upstream libs

(ensure-lib-from-url 'moz "https://github.com/bard/mozrepl/raw/master/chrome/content/moz.el")
(ensure-lib-from-url 'todochiku "http://www.emacswiki.org/emacs/download/todochiku.el")
;; TODO: consider smooth-scroll instead
(ensure-lib-from-url 'smooth-scrolling "http://adamspiers.org/computing/elisp/smooth-scrolling.el")
(ensure-lib-from-url 'edit-server "http://github.com/stsquad/emacs_chrome/raw/master/servers/edit-server.el")
(ensure-lib-from-url 'eol-conversion "http://centaur.maths.qmw.ac.uk/emacs/files/eol-conversion.el")
;; TODO: consider flymake-cursor from elpa?
(ensure-lib-from-url 'flymake-point "http://bitbucket.org/brodie/dotfiles/raw/tip/.emacs.d/plugins/flymake-point.el")
(ensure-lib-from-url 'dsvn "http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/emacs/dsvn.el")
(ensure-lib-from-url 'vc-darcs "http://www.pps.jussieu.fr/~jch/software/repos/vc-darcs/vc-darcs.el")

(ensure-lib-from-svn 'rdebug "http://ruby-debug.rubyforge.org/svn/trunk/emacs/")


(provide 'init-site-lisp)

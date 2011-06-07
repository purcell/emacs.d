(when (not (load "~/.emacs.d/el-get/el-get/el-get.el" t))
  (error "Please bootstrap el-get using the instructions here: http://github.com/dimitri/el-get/, then restart Emacs"))

(unless (require 'color-theme-autoloads nil t)
  (error "Please run 'make color-theme-autoloads.el' in site-lisp/color-theme to generate color-theme-autoloads.el"))

(unless (file-directory-p (expand-file-name "~/.emacs.d/site-lisp/html5-el/relaxng"))
  (error "Please run 'make relaxng' in site-lisp/html5-el"))

(unless (require 'jump nil t)
  (error "Please run 'git submodule update --init' in site-lisp/rinari"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/session/lisp"))

(setq el-get-byte-compile nil
      el-get-generate-autoloads nil
      el-get-sources
      '(el-get
;        package

        ;; Basics

        smooth-scrolling
        todochiku
        edit-server
        eol-conversion
        flymake-point

        ;; Cosmetics

        ;; XML and HTML
        (:name nxml-html5
               :type git
               :url "https://github.com/hober/html5-el.git"
               :build ("make relaxng")
               :features whattf-dt
               :after (lambda ()
                        (message "nxml-html5: after")
                        (eval-after-load "rng-loc"
                          '(add-to-list 'rng-schema-locating-files
                                        (expand-file-name "schemas.xml"
                                                          (el-get-package-directory "nxml-html5"))))))

        ;; Org

        (:name org-mode :type git :url "git://repo.or.cz/org-mode.git" :load-path ("lisp" "contrib/lisp"))

        ;; Ruby & Rails

        (:name ruby-mode :type svn :url "http://svn.ruby-lang.org/repos/ruby/trunk/misc/")
        rdebug

        ;; Version control

        dsvn
        vc-darcs

        ;; Apps

        ;offlineimap

        ;; Javascript

        (:name moz :type http :url "https://github.com/bard/mozrepl/raw/master/chrome/content/moz.el")

        ;; Erlang

        ;; CSS

        tidy

        gnuplot-mode
        haskell-mode  ;; darcs, http://code.haskell.org/haskellmode-emacs/
))

(defun el-get-overridden ()
  (let* ((global (mapcar 'el-get-source-name (el-get-read-all-recipes))))
    (loop for recipe in el-get-sources
          and name = (el-get-source-name recipe)
          when (and (not (symbolp recipe))
                    (member name global))
          collect name)))

(defun el-get-update-all ()
  "Update all el-get packages"
  (interactive)
  (dolist (package (mapcar 'el-get-source-name el-get-sources))
    (unless (memq (plist-get (el-get-package-def package) :type) '(http-tar elpa))
      (el-get-update package))))


(el-get 'sync)
(package-initialize)

(provide 'init-el-get)

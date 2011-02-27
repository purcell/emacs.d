(when (not (load "~/.emacs.d/el-get/el-get/el-get.el" t))
  (error "Please bootstrap el-get using the instructions here: http://github.com/dimitri/el-get/, then restart Emacs"))

(setq el-get-byte-compile nil
      el-get-generate-autoloads nil
      el-get-sources
      '(el-get
        package

        ;; Basics

        smex
        ido-hacks
        maxframe
        session
        revive
        project-local-variables
        dired-plus
        scratch
        diminish
        autopair
        (:name undo-tree :type git :url "http://www.dr-qubit.org/git/undo-tree.git")
        hl-sexp
        highlight-symbol
        highlight-parentheses
        smooth-scrolling
        fuzzy-format
        regex-tool
        todochiku
        edit-server
        fringe-helper
        eol-conversion
        iedit
        mwe-log-commands
        ;(:name all :type http :url "ftp://ftp.dina.kvl.dk/pub/Staff/Per.Abrahamsen/auctex/all.el")
        lively
        whole-line-or-region
        pointback
        flymake-point

        ;; Completion

        auto-complete
        smart-tab
        ac-dabbrev
        ac-slime

        ;; Cosmetics

        color-theme
        color-theme-sanityinc
        color-theme-zenburn
        color-theme-subdued
        color-theme-railscasts

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
        org-fstree

        ;; Ruby & Rails

        ruby-mode
        ruby-compilation
        rinari
        ri-emacs
        yaml-mode
        haml-mode
        sass-mode
        rdebug

        ;; Version control

        magit
        gist
        magithub
        git-blame
        diff-git
        dsvn
        vc-darcs
        darcsum

        ;; Lisps

        (:name paredit :type http :url "http://mumble.net/~campbell/emacs/paredit-beta.el" :localname "paredit.el") ; Overridden to get beta version
        (:name slime :type git :url "git://sbcl.boinkor.net/slime.git" :load-path ("." "./contrib") :compile nil :load "slime-autoloads.el") ; Overridden to prefer git mirror
        clojure-mode
        swank-clojure
        durendal
        quack

        crontab-mode
        mmm-mode

        ;; Apps

        twit
        erc
        ;offlineimap

        google-weather

        ;; Python
        (:name python-mode :type http :url "http://launchpad.net/python-mode/trunk/5.1.0/+download/python-mode.el") ; Overridden because bzr's lp source breaks with python 2.7 (https://bugs.launchpad.net/bzr/+bug/612096)
        pymacs
        ropemacs

        ;; Javascript

        js2-mode
        js-comint
        (:name moz :type http :url "http://github.com/bard/mozrepl/raw/master/chrome/content/moz.el")
        json

        ;; Erlang

        (:name erlware-mode ;; Overridden to use git version
               :type git :url "https://github.com/erlware/erlware-mode.git"
               :load "erlang-start.el")

        ;; PHP

        php-mode
        smarty-mode

        ;; CSS

        rainbow-mode
        tidy

        gnuplot-mode
        csv-mode
        csv-nav
        markdown-mode
        textile-mode
        haskell-mode
        tuareg-mode
        lua-mode
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

(provide 'init-el-get)

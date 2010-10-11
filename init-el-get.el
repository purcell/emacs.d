(when (not (load "~/.emacs.d/el-get/el-get/el-get.el" t))
  (error "Please bootstrap el-get using the instructions here: http://github.com/dimitri/el-get/, then restart Emacs"))

(setq el-get-byte-compile nil
      el-get-sources
      '(el-get
        package

        ;; Basics

        smex
        ido-hacks
        maxframe
        session
        (:name revive :type http :url "http://www.gentei.org/~yuuji/software/revive.el")
        (:name project-local-variables :type elpa)
        (:name dired-plus :type git :url "git://github.com/emacsmirror/dired-plus.git")
        scratch
        (:name diminish :type http :url "http://www.eskimo.com/~seldon/diminish.el")
        (:name autopair :type svn :url "http://autopair.googlecode.com/svn/trunk/")
        (:name undo-tree :type git :url "http://www.dr-qubit.org/git/undo-tree.git")
        hl-sexp
        (:name highlight-symbol :type elpa)
        highlight-parentheses
        (:name smooth-scrolling :type http :url "http://adamspiers.org/computing/elisp/smooth-scrolling.el")
        (:name fuzzy-format :type emacswiki)
        (:name regex-tool :type git :url "git://github.com/jwiegley/regex-tool.git")
        todochiku
        (:name edit-server :type http :url "http://github.com/stsquad/emacs_chrome/raw/master/servers/edit-server.el")
        (:name fringe-helper :type elpa)
        (:name eol-conversion :type http :url "http://centaur.maths.qmw.ac.uk/emacs/files/eol-conversion.el")
        (:name iedit :type emacswiki)
        (:name mwe-log-commands :type http :url "http://www.foldr.org/~michaelw/emacs/mwe-log-commands.el")
        ;(:name all :type http :url "ftp://ftp.dina.kvl.dk/pub/Staff/Per.Abrahamsen/auctex/all.el")
        (:name lively :type emacswiki)

        ;; Completion

        auto-complete
        (:name smart-tab :type elpa)
        (:name ac-dabbrev :type emacswiki)
        (:name ac-slime :type git :url "git://github.com/purcell/ac-slime.git")

        ;; Cosmetics

        color-theme
        (:name color-theme-sanityinc :type git :url "git://github.com/purcell/color-theme-sanityinc.git")
        (:name color-theme-tango-2 :type git :url "git://github.com/wfarr/color-theme-tango-2.git")
        (:name color-theme-ir-black :type git :url "git://github.com/burke/color-theme-ir-black.git")
        (:name color-theme-zen-and-art :type git :url "git://github.com/irfn/zen-and-art.git")
        (:name color-theme-zenburn :type git :url "git://github.com/emacsmirror/zenburn.git")
        (:name color-theme-twilight :type git :url "git://github.com/crafterm/twilight-emacs.git")
        (:name color-theme-subdued :type http :url "http://jblevins.org/git/misc.git/plain/color-theme-subdued.el")

        ;; Org

        (:name org-mode :type git :url "git://repo.or.cz/org-mode.git")
        (:name org-fstree :type git :url "http://repo.or.cz/r/org-fstree.git")

        ;; Ruby & Rails

        (:name ruby-mode :type elpa)
        (:name ruby-compilation :type elpa)
        rinari
        (:name ri-emacs :type git :url "git://github.com/pedz/ri-emacs.git")
        (:name yaml-mode :type elpa)
        (:name haml-mode :type elpa)
        (:name sass-mode :type elpa)
        (:name rdebug :type svn :url "http://ruby-debug.rubyforge.org/svn/trunk/emacs/")

        ;; Version control

        magit
        gist
        (:name magithub :type git :url "git://github.com/nex3/magithub.git")
        (:name git-blame :type http :url "http://git.kernel.org/?p=git/git.git;a=blob_plain;f=contrib/emacs/git-blame.el;hb=HEAD")
        (:name diff-git :type git :url "git://github.com/alanfalloon/diff-git.el.git")
        (:name dsvn :type http :url "http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/emacs/dsvn.el")
        (:name vc-darcs :type http :url "http://www.pps.jussieu.fr/~jch/software/repos/vc-darcs/vc-darcs.el")
        (:name darcsum :type http :url "http://joyful.com/repos/darcsum/darcsum.el")

        ;; Lisps

        (:name paredit :type http :url "http://mumble.net/~campbell/emacs/paredit-beta.el")
        (:name slime :type git :url "git://sbcl.boinkor.net/slime.git" :load-path ("." "./contrib") :compile nil :load "slime-autoloads.el")
        (:name clojure-mode :type git :url "git://github.com/technomancy/clojure-mode.git")
        (:name swank-clojure :type http :url "http://github.com/technomancy/swank-clojure/raw/master/swank-clojure.el")
        (:name durendal :type git :url "git://github.com/technomancy/durendal.git")
        (:name quack :type http :url "http://www.neilvandyke.org/quack/quack.el")

        (:name crontab-mode :type http :url "http://www.mahalito.net/~harley/elisp/crontab-mode.el")
        (:name mmm-mode :type git :url "git://github.com/purcell/mmm-mode.git")

        ;; Apps

        (:name twit :type emacswiki)
        (:name erc :type elpa)
        offlineimap

        google-weather

        ;; Python
        (:name python-mode :type http :url "http://launchpad.net/python-mode/trunk/5.1.0/+download/python-mode.el")
        pymacs
        ropemacs

        ;; Javascript

        (:name js2-mode :type svn :url "http://js2-mode.googlecode.com/svn/trunk/")
        (:name js-comint :type http :url "http://downloads.sourceforge.net/js-comint-el/js-comint.el")
        (:name moz-repl :type http :url "http://github.com/bard/mozrepl/raw/master/chrome/content/moz.el")
        (:name json :type elpa)

        ;; PHP

        (:name php-mode :type http :url "http://php-mode.svn.sourceforge.net/svnroot/php-mode/tags/php-mode-1.5.0/php-mode.el")
        (:name smarty-mode :type http :url "http://lisp.morinie.fr/smarty/download/smarty-mode.el")

        ;; CSS

        rainbow-mode
        (:name tidy :type emacswiki)

        (:name gnuplot-mode :type http :url "http://cars9.uchicago.edu/~ravel/software/gnuplot-mode/gnuplot-mode.0.6.0.tar.gz")
        (:name csv-mode :type http :url "http://centaur.maths.qmul.ac.uk/Emacs/files/csv-mode.el")
        (:name csv-nav :type emacswiki)
        (:name markdown-mode :type git :url "http://jblevins.org/git/markdown-mode.git")
        textile-mode
        haskell-mode
        tuareg-mode
        (:name lua-mode :type elpa)
))

(defun el-get-update-all ()
  "Update all el-get packages"
  (interactive)
  (dolist (package (mapcar 'el-get-source-name el-get-sources))
    (unless (memq (plist-get (el-get-package-def package) :type) '(http-tar elpa))
      (el-get-update package))))


(el-get 'sync)

(provide 'init-el-get)

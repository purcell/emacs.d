(when (not (load "~/.emacs.d/el-get/el-get/el-get.el" t))
  (error "Please bootstrap el-get using the instructions here: http://github.com/dimitri/el-get/, then restart Emacs"))

(setq el-get-byte-compile nil
      el-get-sources
      '(el-get
        package

        ;; Basics

        (:name smex :type git :url "git://github.com/nonsequitur/smex.git")
        ido-hacks
        maxframe
        session
        (:name revive :type http :url "http://www.gentei.org/~yuuji/software/revive.el")
        (:name project-local-variables :type elpa)
        (:name dired-plus :type git :url "git://github.com/emacsmirror/dired-plus.git")
        (:name scratch :type git :url "git://github.com/ieure/scratch-el.git")
        (:name diminish :type git :url "git://github.com/emacsmirror/diminish.git")
        (:name autopair :type git :url "git://github.com/emacsmirror/autopair.git")
        (:name undo-tree :type git :url "http://www.dr-qubit.org/git/undo-tree.git")
        (:name hl-sexp :type git :url "git://github.com/emacsmirror/hl-sexp.git")
        (:name highlight-symbol :type elpa)
        (:name highlight-parentheses :type elpa)
        (:name smooth-scrolling :type http :url "http://adamspiers.org/computing/elisp/smooth-scrolling.el")
        (:name fuzzy-format :type git :url "git://github.com/emacsmirror/fuzzy-format.git")
        (:name regex-tool :type git :url "git://github.com/jwiegley/regex-tool.git")
        todochiku
        (:name edit-server :type http :url "http://github.com/stsquad/emacs_chrome/raw/master/servers/edit-server.el")
        (:name fringe-helper :type elpa)
        (:name eol-conversion :type http :url "http://centaur.maths.qmw.ac.uk/emacs/files/eol-conversion.el")
        (:name iedit :type emacswiki)
        (:name mwe-log-commands :type http :url "http://www.foldr.org/~michaelw/emacs/mwe-log-commands.el")
        ;(:name all :type http :url "ftp://ftp.dina.kvl.dk/pub/Staff/Per.Abrahamsen/auctex/all.el")

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
        (:name rinari :type git :url "git://github.com/eschulte/rinari.git")
        (:name ri-emacs :type git :url "git://github.com/pedz/ri-emacs.git")
        (:name yaml-mode :type elpa)
        (:name haml-mode :type elpa)
        (:name sass-mode :type elpa)
        (:name rdebug :type svn :url "http://ruby-debug.rubyforge.org/svn/trunk/emacs/")

        ;; Version control

        (:name magit :type git :url "git://github.com/philjackson/magit.git")
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

        (:name twit :type git :url "git://github.com/emacsmirror/twit.git")
        (:name erc :type elpa)
        offlineimap

        google-weather

        ;; Python
        (:name python-mode :type http :url "http://launchpad.net/python-mode/trunk/5.1.0/+download/python-mode.el")
        pymacs

        ;; Javascript

        (:name js2-mode :type http :url "http://js2-mode.googlecode.com/svn/trunk/js2-mode.el")
        (:name js-comint :type http :url "http://downloads.sourceforge.net/js-comint-el/js-comint.el")
        (:name moz-repl :type http :url "http://github.com/bard/mozrepl/raw/master/chrome/content/moz.el")
        (:name json :type elpa)

        ;; PHP

        (:name php-mode :type http :url "http://php-mode.svn.sourceforge.net/svnroot/php-mode/tags/php-mode-1.5.0/php-mode.el")
        (:name smarty-mode :type http :url "http://lisp.morinie.fr/smarty/download/smarty-mode.el")

        ;; CSS

        (:name rainbow-mode :type elpa)
        (:name tidy :type emacswiki)

        (:name csv-mode :type http :url "http://centaur.maths.qmul.ac.uk/Emacs/files/csv-mode.el")
        (:name csv-nav :type emacswiki)
        (:name markdown-mode :type git :url "http://jblevins.org/git/markdown-mode.git")
        textile-mode

        (:name haskell-mode :type darcs :url "http://code.haskell.org/haskellmode-emacs/")
        (:name tuareg-mode :type svn :url "svn://svn.forge.ocamlcore.org/svnroot/tuareg/trunk")
        (:name lua-mode :type elpa)
))

(el-get)

(provide 'init-el-get)

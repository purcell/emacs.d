(eval-after-load "package"
  '(progn
     (when (> emacs-major-version 23)
       (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/")))
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
))

(defun require-package (package &optional min-version)
  "Ask elpa to install given PACKAGE."
  (or (package-installed-p package min-version)
      (package-install package)))


(require 'package)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require-package 'all)
(require-package 'color-theme)
(require-package 'erc)
(require-package 'fringe-helper)
(require-package 'gnuplot)
(require-package 'haskell-mode)
(require-package 'highlight-symbol)
(require-package 'json)
(require-package 'lua-mode)
(require-package 'project-local-variables)
(require-package 'ruby-mode)
(require-package 'slime)
(require-package 'slime-fuzzy)
(require-package 'slime-repl)

(provide 'init-elpa)

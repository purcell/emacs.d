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

(require-package 'fringe-helper)
(require-package 'color-theme)
(require-package 'json)
(require-package 'erc)
(require-package 'slime)
(require-package 'slime-fuzzy)
(require-package 'slime-repl)
(require-package 'project-local-variables)
(require-package 'highlight-symbol)
(require-package 'lua-mode)
(require-package 'gnuplot)
(require-package 'haskell-mode)
(require-package 'ruby-mode)

(provide 'init-elpa)

(eval-after-load "package"
  '(progn
     (when (> emacs-major-version 23)
       (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/")))

     (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
     (add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs"))
))

(provide 'init-elpa)

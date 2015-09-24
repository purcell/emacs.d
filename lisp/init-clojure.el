;; Basic clojure support, even in Emacs 23
;; See also init-clojure-cider.el

(require-package 'clojure-mode)
(require-package 'cljsbuild-mode)
(require-package 'elein)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc clojure tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'clojure-mode-hook 'subword-mode))


(provide 'init-clojure)

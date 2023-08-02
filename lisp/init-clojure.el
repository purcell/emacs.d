;;; init-clojure.el --- Clojure support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See also init-clojure-cider.el

(when (maybe-require-package 'clojure-mode)
  (require-package 'cljsbuild-mode)
  (require-package 'elein)

  (with-eval-after-load 'clojure-mode
    (dolist (m '(clojure-mode-hook clojure-ts-mode-hook))
      (add-hook m 'sanityinc/lisp-setup)
      (add-hook m 'subword-mode))))


(provide 'init-clojure)
;;; init-clojure.el ends here

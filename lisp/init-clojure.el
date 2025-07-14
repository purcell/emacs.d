;;; init-clojure.el --- Clojure support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See also init-clojure-cider.el

(when (or (maybe-require-package 'clojure-ts-mode)
          (maybe-require-package 'clojure-mode))
  (require-package 'cljsbuild-mode)

  (with-eval-after-load 'clojure-mode
    (dolist (m '(clojure-mode-hook clojure-ts-mode-hook))
      (add-hook m 'sanityinc/lisp-setup))))


(provide 'init-clojure)
;;; init-clojure.el ends here

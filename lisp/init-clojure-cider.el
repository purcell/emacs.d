;;; init-clojure-cider.el --- Cider support for clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-clojure)
(require-package 'clj-refactor)

(defun clojure-refactor-setup ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ;; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(when (maybe-require-package 'cider)
  (setq nrepl-popup-stacktraces nil)

  (with-eval-after-load 'cider
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode))

  (require-package 'flycheck-clojure)
  (with-eval-after-load 'clojure-mode
    (with-eval-after-load 'cider
      (with-eval-after-load 'flycheck
        (flycheck-clojure-setup)))))


(add-hook 'clojure-mode-hook #'clojure-refactor-setup)

(provide 'init-clojure-cider)
;;; init-clojure-cider.el ends here

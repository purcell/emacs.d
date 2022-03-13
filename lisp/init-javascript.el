;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'json-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'typescript-mode)
(maybe-require-package 'prettier-js)


;;; Basic js-mode setup

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

(with-eval-after-load 'js
  (sanityinc/major-mode-lighter 'js-mode "JS")
  (sanityinc/major-mode-lighter 'js-jsx-mode "JSX"))

(setq-default js-indent-level 2)



;; js2-mode

;; Change some defaults: customize them to override
(setq-default js2-bounce-indent-p nil)
(with-eval-after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)
      (when (derived-mode-p 'js-mode)
        (js2-minor-mode 1))))
  (add-hook 'js-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)
  (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)

  (js2-imenu-extras-setup))

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

(with-eval-after-load 'js2-mode
  (sanityinc/major-mode-lighter 'js2-mode "JS2")
  (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2"))



(when (and (or (executable-find "rg") (executable-find "ag"))
           (maybe-require-package 'xref-js2))
  (when (executable-find "rg")
    (setq-default xref-js2-search-program 'rg))
  (defun sanityinc/enable-xref-js2 ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
  (with-eval-after-load 'js
    (define-key js-mode-map (kbd "M-.") nil)
    (add-hook 'js-mode-hook 'sanityinc/enable-xref-js2))
  (with-eval-after-load 'js2-mode
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook 'sanityinc/enable-xref-js2)))



;;; Coffeescript

(when (maybe-require-package 'coffee-mode)
  (with-eval-after-load 'coffee-mode
    (setq-default coffee-tab-width js-indent-level))

  (when (fboundp 'coffee-mode)
    (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode))))


;; Run and interact with an inferior JS via js-comint.el

(when (maybe-require-package 'js-comint)
  (setq js-comint-program-command "node")

  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    :init-value nil :lighter " InfJS" :keymap inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))


;; Alternatively, use skewer-mode

(when (maybe-require-package 'skewer-mode)
  (with-eval-after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))



(when (maybe-require-package 'add-node-modules-path)
  (dolist (mode '(typescript-mode js-mode js2-mode coffee-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))


(provide 'init-javascript)
;;; init-javascript.el ends here

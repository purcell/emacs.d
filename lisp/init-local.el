(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (condition-case nil (elpy-goto-definition)
    (error (elpy-rgrep-symbol
            (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))


( when (maybe-require-package 'elpy)
  :ensure t
  :init (elpy-enable)
  ;; (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  ;; (setq python-shell-interpreter "jupyter"
  ;;       python-shell-interpreter-args "console --simple-prompt"
  ;;       python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;;              "jupyter")
  (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)
  (define-key elpy-mode-map (kbd "M-?") 'xref-find-references)
  )

(require 'ido)
(ido-mode 'both) ;; for buffers and files

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" default)))
 '(flycheck-flake8-maximum-line-length 120))


(require 'yasnippet)
(require 'yasnippet-snippets)
(require 'cl)
(setq yas-triggers-in-field t)

(provide 'init-local)

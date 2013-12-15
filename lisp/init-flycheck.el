(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)


(after-load 'flycheck
  (flycheck-define-checker ruby
    "A Ruby syntax checker using the standard (MRI) Ruby interpreter.

 See URL `http://www.ruby-lang.org/'."
    :command ("ruby" "-w" "-c" source)
    :error-patterns
    ;; These patterns support output from JRuby, too, to deal with RVM or Rbenv
    ((error line-start
            "SyntaxError in " (file-name) ":" line ": " (message)
            line-end)
     (warning line-start
              (file-name) ":" line ":" (optional column ":")
              " warning: " (message) line-end)
     (error line-start (file-name) ":" line ": " (message) line-end))
    :modes (enh-ruby-mode ruby-mode))
  (add-to-list 'flycheck-checkers 'ruby t))


(provide 'init-flycheck)

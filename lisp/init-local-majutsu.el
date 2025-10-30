;; init-local-majutsu.el -*- lexical-binding: t; -*-

(use-package majutsu
  :ensure t
  :vc (:url "https://github.com/0WD0/majutsu"
            :rev :newest)
  :config
  ;; Use Emacs state instead of Evil in Majutsu buffers
  (with-eval-after-load 'evil
    (dolist (mode '(majutsu-log-mode
                    majutsu-status-mode
                    majutsu-diff-mode
                    majutsu-revision-mode
                    majutsu-process-mode
                    majutsu-mode))
      (evil-set-initial-state mode 'emacs))))

(provide 'init-local-majutsu)

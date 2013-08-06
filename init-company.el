(add-hook 'after-init-hook 'global-company-mode)

;; does not matter, I never use this hotkey
(global-set-key (kbd "C-c o") 'company-complete)
(setq company-require-match nil)

(if (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            )))

(eval-after-load 'company
                   '(add-to-list 'company-backends 'company-cmake))

(provide 'init-company)

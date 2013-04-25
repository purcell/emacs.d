(add-hook 'after-init-hook 'global-company-mode)
;;(company-mode 1)
;; don't know why company-clang-modes does not include c++-mode by default
(global-set-key (kbd "C-c o") 'company-complete)
(setq company-require-match nil)

(mapc #'evil-declare-change-repeat
      '(company-complete-common
        company-select-next
        company-select-previous
        company-complete-selection
        ))

;; remove dabbrev related modes because hippie-expand already supports dabbrev
;; (setq company-backends '(company-elisp company-nxml company-css
;;                                        company-semantic company-clang company-eclim
;;                                        company-xcode company-ropemacs
;;                                        (company-gtags company-etags company-keywords)
;;                                        company-oddmuse company-files)
;;       )
(provide 'init-company)

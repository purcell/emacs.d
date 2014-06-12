(add-hook 'sh-set-shell-hook 'flymake-shell-load)

(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))

(provide 'init-sh)

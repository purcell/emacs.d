(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-toggle-key "C-c z")
 '(evil-want-C-w-delete t)
 '(evil-want-C-w-in-emacs-state nil)
 '(init-color-theme-no-window-system-bg-color "color-16")
 '(init-color-theme-window-system-bg-color "black")
 '(session-use-package t nil (session))
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob) ("C-p" . previous-line) ("C-n" . next-line) ("C-s" . isearch-forward) ("C-r" . isearch-backward) ("C-m" . term-send-raw) ("C-<right>" . term-send-forward-word) ("C-<left>" . term-send-backward-word) ("M-o" . term-send-backspace) ("M-p" . term-send-up) ("M-n" . term-send-down) ("M-d" . term-send-forward-kill-word) ("M-<backspace>" . term-send-backward-kill-word) ("M-r" . term-send-reverse-search-history) ("M-," . term-send-input) ("M-." . comint-dynamic-complete))))
 '(term-buffer-maximum-size 10240)
 '(term-unbind-key-list (quote ("C-x"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(term-color-blue ((t (:background "blue2" :foreground "DeepSkyBlue2")))))

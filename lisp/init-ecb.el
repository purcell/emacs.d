;; ecb

(require-package 'ecb)
(setq ecb-layout-name "left3")
(setq ecb-compile-window-height 10) ; always show the compilation window
;; ; activate and deactivate ecb
;; (global-set-key (kbd "C-x C-;") 'ecb-activate)
;; (global-set-key (kbd "C-x C-'") 'ecb-deactivate)
; show/hide ecb window
(global-set-key (kbd "C-x C-;") 'ecb-show-ecb-windows)
(global-set-key (kbd "C-x C-'") 'ecb-hide-ecb-windows)
; quick navigation between ecb windows
(global-set-key (kbd "C-*") 'ecb-goto-window-edit1)
(global-set-key (kbd "C-+") 'ecb-goto-window-directories)
(global-set-key (kbd "C-#") 'ecb-goto-window-sources)
(global-set-key (kbd "C-@") 'ecb-goto-window-methods)
(global-set-key (kbd "C-!") 'ecb-goto-window-compilation)
(setq ecb-tip-of-the-day nil) ; disable tip of the day

(provide 'init-ecb)

;;----------------------------------------------------------------------------
;; Emmet
;;----------------------------------------------------------------------------

;; Emmet
(require-package 'emmet-mode)
;; Emmet for helm
(require-package 'helm-emmet)
;; Emmet for autocomplete
(require-package 'ac-emmet) ;; Not necessary if using ELPA package
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
;; preview is the default action
(setq emmet-preview-default t)




(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)



(provide 'init-emmet)
;;; init-emmet.el ends here

(autoload 'textile-mode "textile-mode" "Mode for editing Textile documents" t)
(setq auto-mode-alist
      (cons '("\\.textile\\'" . textile-mode) auto-mode-alist))


(provide 'init-textile)

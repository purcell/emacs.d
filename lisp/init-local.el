;;; code:
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)

(require-package 'elpy)
(require 'elpy nil t)
(when (require 'elpy nil t)
  (elpy-enable)
  (add-hook 'python-mode-hook 'elpy-mode))
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))
(color-theme-sanityinc-tomorrow-bright)
(setq org-export-backends (quote (ascii html icalendar latex md)))
(provide 'init-local)
;;; init-local.el ends here

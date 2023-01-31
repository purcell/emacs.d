;;; init-local.el --- Configure of personal settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; set org agenda files
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
(setq org-agenda-span 'week)


;; (maybe-require-package 'json-reformat)
;; (setq json-reformat:indent-width 4)
(setq json-encoding-default-indentation "    ")
(provide 'init-local)
;;; init-locales.el ends here

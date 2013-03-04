(require-package 'csv-mode)
(require-package 'csv-nav)

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
(autoload 'csv-nav-mode "csv-nav" "Major mode for navigating comma-separated value files." t)

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)

;;; init-crontab.el --- Working with crontabs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'crontab-mode)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")

(provide 'init-crontab)
;;; init-crontab.el ends here

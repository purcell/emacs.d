;;; init-local.el --- configure default locale -*- lexical-binding: t -*-
;;; Commentary: users personal customization file in addition to custom.el
;;; See this link https://github.com/eribertto/purcell.emacs.d#installation
(setq-default truncate-lines t) ; good for tiling window managers
(size-indication-mode 1) ; display file size
(display-time)
(display-battery-mode)
(when (version<= "27.1" emacs-version) ; speed up long lines
  (global-so-long-mode 1))




(provide 'init-local)
;;; init-locales.el ends here

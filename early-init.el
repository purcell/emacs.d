;;; early-init.el --- Emacs 27+ pre-initialisation config  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; Emacs 31 added a startup warning for any plain .el source file that lacks
;; a `lexical-binding' cookie. It fires for several bundled packages' (often
;; auto-generated) autoloads files -- e.g. sly-macrostep, org-pomodoro,
;; elm-mode, purescript-mode. 
;; Suppress just this specific warning type so a stock Emacs 31 startup stays 
;; quiet, without hiding other `files'-category warnings/errors.
(require 'warnings)
(add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie))

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here

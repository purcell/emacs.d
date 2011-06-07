;; -*- coding: utf-8 -*-
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *vi-emulation-support-enabled* nil) ; "viper-mode"
(setq *spell-check-support-enabled* nil)
(setq *byte-code-cache-enabled* nil)
(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

;;----------------------------------------------------------------------------
;; Make elisp more civilised
;;----------------------------------------------------------------------------
(require 'cl)

;;----------------------------------------------------------------------------
;; Set $PATH
;;----------------------------------------------------------------------------
(require 'init-exec-path)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-site-lisp)
(require 'init-elpa)
(when *byte-code-cache-enabled*
  (require 'init-byte-code-cache))
(require 'init-utils)
(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-title-bar)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-proxies)
(require 'init-dired)
(require 'init-viper)
(require 'init-isearch)
(require 'init-iedit)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-regex-tool)

(require 'init-recentf)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-themes)
(require 'init-mmm)
(require 'init-growl)

(require 'init-editing-utils)

(require 'init-svn)
(require 'init-darcs)
(require 'init-git)

(require 'init-gnuplot)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(require 'init-javascript)
(require 'init-sh)
(require 'init-php)
(require 'init-org)
(require 'init-htmlize)
(require 'init-nxml)
(require 'init-css)
(require 'init-haml)
(require 'init-python-mode)
(require 'init-haskell)
(require 'init-ocaml)
(require 'init-ruby-mode)
(require 'init-rails)

(require 'init-lisp)
(require 'init-slime)
(require 'init-clojure)
(require 'init-common-lisp)
;(require 'init-scheme)

(when *spell-check-support-enabled*
  (require 'init-spelling))

;; In-Emacs apps
(require 'init-mail)
(require 'init-twitter)

(require 'init-misc)


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(server-start)


;;----------------------------------------------------------------------------
;; Edit-server for Chrome extension: http://github.com/stsquad/emacs_chrome
;;----------------------------------------------------------------------------
(require 'init-edit-server)


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

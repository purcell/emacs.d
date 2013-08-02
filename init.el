;; -*- coding: utf-8 -*-
(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *spell-check-support-enabled* t)
(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *win32* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )
(setq *xemacs* (featurep 'xemacs) )
(setq *emacs23* (and (not *xemacs*) (or (>= emacs-major-version 23))) )
(setq *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))) )

;----------------------------------------------------------------------------
; Functions (load all files in defuns-dir)
; Copied from https://github.com/magnars/.emacs.d/blob/master/init.el
;----------------------------------------------------------------------------
(setq defuns-dir (expand-file-name "~/.emacs.d/defuns"))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
      (load file)))
;----------------------------------------------------------------------------
; Load configs for specific features and modes
;----------------------------------------------------------------------------
(require 'init-modeline)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

;; win32 auto configuration, assuming that cygwin is installed at "c:/cygwin"
(if *win32*
	(progn
		(setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
		(require 'setup-cygwin)
		;(setenv "HOME" "c:/cygwin/home/someuser") ;; better to set HOME env in GUI
		))

(require 'init-elpa)
(require 'init-exec-path) ;; Set up $PATH
(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-maxframe)
(require 'init-proxies)
(require 'init-dired)
(require 'init-isearch)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-artbollocks-mode)
(require 'init-recentf)
(require 'init-ido)
(if *emacs24* (require 'init-helm))
(require 'init-hippie-expand)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)
;(require 'init-growl)
(require 'init-editing-utils)
(require 'init-git)
(require 'init-crontab)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(require 'init-javascript)
(require 'init-sh)
(require 'init-php)
(require 'init-org)
(require 'init-org-mime)
(require 'init-nxml)
(require 'init-css)
(require 'init-haml)
(require 'init-python-mode)
(require 'init-haskell)
(require 'init-ruby-mode)
(if (not (boundp 'light-weight-emacs)) (require 'init-rails))
;(require 'init-rcirc)

(require 'init-lisp)
(require 'init-slime)
(require 'init-clojure)
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-marmalade)

;; Chinese inut method
(require 'init-org2blog)
;;(require 'init-fill-column-indicator) ;make auto-complete dropdown wierd
(if (not (boundp 'light-weight-emacs)) (require 'init-yasnippet))
;; Use bookmark instead
;; (require 'init-better-registers) ; C-x j - jump to register
(require 'init-zencoding-mode) ;behind init-better-register to override C-j
(require 'init-yari)
(require 'init-cc-mode)
(require 'init-semantic)
(require 'init-cmake-mode)
(require 'init-csharp-mode)
(require 'init-linum-mode)
;(require 'init-delicious) ;make startup slow, I don't use delicious in w3m
(require 'init-emacs-w3m)
(if (not (boundp 'light-weight-emacs)) (require 'init-eim))
(require 'init-thing-edit)
(require 'init-which-func)
(require 'init-keyfreq)
;; (require 'init-gist)
(require 'init-emacspeak)
(require 'init-pomodoro)
(require 'init-undo-tree)
(require 'init-moz)
(require 'init-gtags)
;; use evil mode (vi key binding)
(if (not (boundp 'light-weight-emacs)) (require 'init-evil))
(require 'init-misc)
(require 'init-ctags)
(require 'init-ace-jump-mode)
(require 'init-multiple-cursors)
;; (require 'init-uml)
(require 'init-sunrise-commander)
(require 'init-bbdb)
(require 'init-gnus)
(require 'init-twittering-mode)
(require 'init-weibo)
;; itune cannot play flac, so I use mplayer+emms instead (updated, use mpd!)
(if (not (boundp 'light-weight-emacs)) (if *is-a-mac* (require 'init-emms)) )
(require 'init-lua-mode)
(require 'init-doxygen)
(require 'init-workgroups)
(require 'init-move-window-buffer)
(require 'init-term-mode)
;; I'm fine with nxml-mode, so web-mode is not used
;;(require 'init-web-mode)
(require 'init-sr-speedbar)
(require 'init-smartparens)
;; Choose either auto-complete or company-mode by commenting one of below two lines!
;; (require 'init-auto-complete) ; after init-yasnippeta to override TAB
(require 'init-company)
(require 'init-stripe-buffer)
(require 'init-popwin)
(require 'init-elnode)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;; Don't use emacsclient, and this code make emacs start up slow
;;(defconst --batch-mode (member "--batch-mode" command-line-args)
;;          "True when running in batch-mode (--batch-mode command-line switch set).")
;;
;;(unless --batch-mode
;;  (require 'server)
;;  (when (and (= emacs-major-version 23)
;;             (= emacs-minor-version 1)
;;             (equal window-system 'w32))
;;    ;; Suppress error "directory ~/.emacs.d/server is unsafe" on Windows.
;;    (defun server-ensure-safe-dir (dir) "Noop" t))
;;  (condition-case nil
;;      (unless (server-running-p) (server-start))
;;    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;;    (error
;;     (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir)))
;;       (when (and server-use-tcp
;;                  (not (file-accessible-directory-p server-dir)))
;;         (display-warning
;;          'server (format "Creating %S" server-dir) :warning)
;;         (make-directory server-dir t)
;;         (server-start))))))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(if (file-readable-p (expand-file-name "~/.emacs.d/custom.el"))
     (load-file (expand-file-name "~/.emacs.d/custom.el"))
       nil)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
;(require 'init-locales) ;does not work in cygwin


(when (require 'time-date nil t)
   (message "Emacs startup time: %d seconds."
    (time-to-seconds (time-since emacs-load-start-time)))
   )

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'erase-buffer 'disabled nil)

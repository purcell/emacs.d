;; -*- coding: utf-8 -*-

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *vi-emulation-support-enabled* t) ; "viper-mode"
(setq *ecb-support-enabled* t) ; Emacs code browser (IDE)
(setq *haskell-support-enabled* t)
(setq *ocaml-support-enabled* t)
(setq *common-lisp-support-enabled* t)
(setq *scheme-support-enabled* t)
(setq *macbook-pro-support-enabled* t)
(setq *erlang-support-enabled* t)
(setq *darcs-support-enabled* t)
(setq *rails-support-enabled* t)
(setq *spell-check-support-enabled* nil)
(setq *byte-code-cache-enabled* nil)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))


;;----------------------------------------------------------------------------
;; Set load path
;;----------------------------------------------------------------------------
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))


;;----------------------------------------------------------------------------
;; Automatically byte-compile .el files
;;----------------------------------------------------------------------------
(when *byte-code-cache-enabled*
  (load "init-byte-code-cache"))


;;----------------------------------------------------------------------------
;; Use elisp package manager (http://tromey.com/elpa/)
;;----------------------------------------------------------------------------
(setq load-path (cons (expand-file-name "~/.emacs.d/elpa") load-path))
(load "package")
(package-initialize)


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(require 'find-func)
(defun directory-of-library (library-name)
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Outboard initialisation files
;;----------------------------------------------------------------------------
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))
(load "init-python-mode")


;;----------------------------------------------------------------------------
;; Augment search path for external programs (for OSX)
;;----------------------------------------------------------------------------
(when *is-a-mac*
  (eval-after-load "woman"
    '(setq woman-manpath (append (list "/opt/local/man") woman-manpath)))
  (dolist (dir '("/usr/local/bin" "/opt/local/bin"
                 "/opt/local/lib/postgresql83/bin" "~/bin"))
    (setenv "PATH" (concat (expand-file-name dir) ":" (getenv "PATH")))
    (setq exec-path (append (list (expand-file-name dir)) exec-path))))


;;----------------------------------------------------------------------------
;; Console-specific set-up
;;----------------------------------------------------------------------------
(defun fix-up-xterm-control-arrows ()
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;5D" [C-left]))

(unless window-system
  (progn
    (fix-up-xterm-control-arrows)
    (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
    (mwheel-install)
    (menu-bar-mode nil) ; Use F10 from minibuffer to get menus
    ))


;;----------------------------------------------------------------------------
;; Include buffer name and file path in title bar
;;----------------------------------------------------------------------------
(defvar *user*    (user-login-name) "user login name")
(defvar *hostname*
  (let ((n (system-name))) (substring n 0 (string-match "\\." n))) "unqualified host name")
(defun concise-buffer-file-name ()
  (let ((fn (buffer-file-name)))
    (when fn
      (let* ((homedir (getenv "HOME"))
             (homepos (string-match homedir fn)))
        (if homepos
            (concat "~" (substring fn (match-end 0)))
          (homedir))))))
(setq frame-title-format '("%b - " *user* "@" *hostname* " - " (:eval (concise-buffer-file-name))))


;;----------------------------------------------------------------------------
;; Make yes-or-no questions answerable with 'y' or 'n'
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)


;;----------------------------------------------------------------------------
;; OS X usability tweaks
;;----------------------------------------------------------------------------
(when *macbook-pro-support-enabled*
  (setq mac-command-modifier 'meta)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(0.001))
  (when *is-cocoa-emacs*
    ;; Woohoo!!
    (global-set-key "\M-`" 'ns-next-frame)

    (global-set-key "\M-h" 'ns-do-hide-emacs)
    (global-set-key "\M-c" 'ns-copy-including-secondary)
    (global-set-key "\M-v" 'ns-paste-secondary))
  ;; Use Apple-w to close current buffer on OS-X (is normally bound to kill-ring-save)
  (global-set-key [(meta w)] 'kill-this-buffer))


;;----------------------------------------------------------------------------
;; Enhanced dired
;;----------------------------------------------------------------------------
(require 'dired+)
(setq dired-recursive-deletes 'top)


;;----------------------------------------------------------------------------
;; Show and edit all lines matching a regex
;;----------------------------------------------------------------------------
(require 'all)


;;----------------------------------------------------------------------------
;; VI emulation and related key mappings
;;----------------------------------------------------------------------------
(when *vi-emulation-support-enabled*
  ;; C-z is usually 'iconify-or-deiconify-frame, but viper uses it to toggle
  ;; vi/emacs input modes, causing confusion in non-viper buffers
  (global-unset-key "\C-z")
  (setq viper-mode t)
  (setq viper-custom-file-name (convert-standard-filename "~/.emacs.d/.viper"))
  (require 'viper)
  (require 'vimpulse))


;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq default-indicate-empty-lines t)


;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)


;;----------------------------------------------------------------------------
;; Navigate windows "C-<arrow>"
;;----------------------------------------------------------------------------
(windmove-default-keybindings 'control)


;;----------------------------------------------------------------------------
;; Scroll the window smoothly with the up/down arrows
;;----------------------------------------------------------------------------
(require 'smooth-scrolling)
(setq scroll-preserve-screen-position t)


;;----------------------------------------------------------------------------
;; Nicer naming of buffers for files with identical names
;;----------------------------------------------------------------------------
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;;----------------------------------------------------------------------------
;; Use ibuffer instead of the built in buffer list
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;----------------------------------------------------------------------------
;; Dynamic expansion tweaks
;;----------------------------------------------------------------------------
(eval-after-load "hippie-exp"
  '(setq hippie-expand-try-functions-list
         (remove 'try-expand-line hippie-expand-try-functions-list)))


;;----------------------------------------------------------------------------
;; Erlang
;;----------------------------------------------------------------------------
(when *erlang-support-enabled*
  ;;(setq load-path (cons (expand-file-name "/usr/local/share/emacs/site-lisp/distel") load-path))
  ;;(defun my-erlang-load-hook ()
  ;; (setq erlang-root-dir "/opt/otp/lib/erlang"))
  ;;(add-hook 'erlang-load-hook 'my-erlang-load-hook)
  (setq erlang-root-dir "/opt/local/lib/erlang")
  (require 'erlang-start))
  ;;(require 'distel)
  ;;(add-hook 'erlang-mode-hook 'distel-erlang-mode-hook))


;;----------------------------------------------------------------------------
;; Javascript
;;----------------------------------------------------------------------------
(load "init-javascript")

;;----------------------------------------------------------------------------
;; Extensions -> Modes
;;----------------------------------------------------------------------------
(add-auto-mode 'html-mode "\\.(jsp|tmpl)$")
(add-auto-mode 'tcl-mode "Portfile$")


;;----------------------------------------------------------------------------
;; Crontab mode
;;----------------------------------------------------------------------------
(autoload 'crontab-mode "crontab-mode" "Mode for editing crontab files" t)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")


;;----------------------------------------------------------------------------
;; Textile-mode
;;----------------------------------------------------------------------------
(autoload 'textile-mode "textile-mode" "Mode for editing Textile documents" t)


;;----------------------------------------------------------------------------
;; Regex-tool
;;----------------------------------------------------------------------------
(autoload 'regex-tool "regex-tool" "Mode for exploring regular expressions" t)
(setq regex-tool-backend 'perl)


;;----------------------------------------------------------------------------
;; Subversion
;;----------------------------------------------------------------------------
(require 'psvn)


;;----------------------------------------------------------------------------
;; Darcs
;;----------------------------------------------------------------------------
(when *darcs-support-enabled*
  (load "init-darcs"))


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(load "init-git")


;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
(require 'mmm-auto)
(setq mmm-global-mode t)
(setq mmm-submode-decoration-level 2)
(setq-default mmm-never-modes
              (append '(sldb-mode) '(ediff-mode) '(text-mode)
                      '(compilation-mode) '(inferior-haskell-mode)
                      mmm-never-modes))


;;----------------------------------------------------------------------------
;; File and buffer navigation
;;----------------------------------------------------------------------------
(require 'recentf)
(setq recentf-max-saved-items 100)
(load "init-ido")


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)


;;----------------------------------------------------------------------------
;; Desktop saving
;;----------------------------------------------------------------------------
;; save a list of open files in ~/.emacs.d/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-path '("~/.emacs.d"))
(setq desktop-save 'if-exists)
(desktop-save-mode 1)


;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(require 'session)
(setq session-save-file (expand-file-name "~/.emacs.d/.session"))
(add-hook 'after-init-hook 'session-initialize)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)

(load "init-maxframe")


;;----------------------------------------------------------------------------
;; Fonts
;;----------------------------------------------------------------------------
(load "init-fonts")


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  (interactive)
  (or (buffer-file-name) (error "no file is currently being edited"))
  (when (yes-or-no-p "Really delete this file?")
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; ECB (Emacs Code Browser)
;;----------------------------------------------------------------------------
(when *ecb-support-enabled*
  (load "init-ecb"))


;;----------------------------------------------------------------------------
;; Compilation
;;----------------------------------------------------------------------------
(add-hook 'compilation-mode-hook (lambda () (local-set-key [f6] 'recompile)))


;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))


;;----------------------------------------------------------------------------
;; Gnuplot
;;----------------------------------------------------------------------------
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)


;;----------------------------------------------------------------------------
;; Org-mode
;;----------------------------------------------------------------------------
(setq load-path (cons "~/.emacs/site-lisp/org-mode/lisp" load-path))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;;----------------------------------------------------------------------------
;; NXML
;;----------------------------------------------------------------------------
(load "init-nxml")


;;----------------------------------------------------------------------------
;; Integration with tidy for html + xml
;;----------------------------------------------------------------------------
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

(add-hook 'nxml-mode-hook (lambda () (tidy-build-menu nxml-mode-map)))
(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))


;;----------------------------------------------------------------------------
;; Ruby & Rails
;;----------------------------------------------------------------------------
(load "init-ruby-mode")
(when *rails-support-enabled*
  (load "init-rails"))


;;----------------------------------------------------------------------------
; Automatically set execute perms on files if first line begins with '#!'
;;----------------------------------------------------------------------------
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;----------------------------------------------------------------------------
;; htmlize
;;----------------------------------------------------------------------------
(dolist (sym
         (list 'htmlize-file 'htmlize-region 'htmlize-buffer
               'htmlize-many-files 'htmlize-many-files-dired))
  (autoload sym "htmlize"))


;;----------------------------------------------------------------------------
;; CSS mode
;;----------------------------------------------------------------------------
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(add-auto-mode 'css-mode "\\.css$")


;;----------------------------------------------------------------------------
;; YAML mode
;;----------------------------------------------------------------------------
(autoload 'yaml-mode "yaml-mode" "Mode for editing YAML files" t)
(add-auto-mode 'yaml-mode "\\.ya?ml$")


;;----------------------------------------------------------------------------
;; CSV mode and csv-nav mode
;;----------------------------------------------------------------------------
(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
(autoload 'csv-nav-mode "csv-nav-mode" "Major mode for navigating comma-separated value files." t)


;;----------------------------------------------------------------------------
;; Shell mode
;;----------------------------------------------------------------------------
(autoload 'flymake-shell-load "flymake-shell" "On-the-fly syntax checking of shell scripts" t)
(add-hook 'sh-mode-hook 'flymake-shell-load)


;;----------------------------------------------------------------------------
;; PHP
;;----------------------------------------------------------------------------
(autoload 'php-mode "php-mode" "mode for editing php files" t)
(add-auto-mode 'php-mode "\\.php[345]?\\'\\|\\.phtml\\." "\\.(inc|tpl)$" "\\.module$")
(add-hook 'php-mode-hook
          (lambda ()
            (require 'flymake-php)
            (flymake-mode t)))


;;----------------------------------------------------------------------------
;; Lisp / Scheme / Slime
;;----------------------------------------------------------------------------
(load "init-lisp")
(when *common-lisp-support-enabled*
  (load "init-common-lisp"))
(when *scheme-support-enabled*
  ; See http://bc.tech.coop/scheme/scheme-emacs.htm
  (require 'quack))

;;----------------------------------------------------------------------------
;; Haskell
;;----------------------------------------------------------------------------
(when *haskell-support-enabled*
  (load "init-haskell"))


;;----------------------------------------------------------------------------
;; OCaml
;;----------------------------------------------------------------------------
(when *ocaml-support-enabled*
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t))


;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(when *spell-check-support-enabled*
  (load "init-flyspell"))


;;----------------------------------------------------------------------------
;; Log typed commands into a buffer for demo purposes
;;----------------------------------------------------------------------------
(autoload 'mwe:log-keyboard-commands "mwe-log-commands"
  "Log commands executed in the current buffer" t)


;;----------------------------------------------------------------------------
;; Conversion of line endings
;;----------------------------------------------------------------------------
(require 'eol-conversion)


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(server-start)


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(when (or window-system (string-match "UTF-8" (shell-command-to-string "locale")))
  (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  (set-language-environment 'utf-8)
  (when *is-carbon-emacs*
    (set-keyboard-coding-system 'utf-8-mac))
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))


;;----------------------------------------------------------------------------
;; Color themes
;;----------------------------------------------------------------------------
(require 'color-theme-autoloads)
(unless *is-cocoa-emacs*
  (color-theme-initialize)
  ;; (color-theme-pierson) ; Light, favourite
  ;; (color-theme-high-contrast)
  ;; (color-theme-snowish)
  ;; (color-theme-marquardt)
  ;; (color-theme-clarity) ; dark
  ;; (color-theme-dark-laptop) ; dark
  ;; (color-theme-billw) ; dark
  ;; (color-theme-oswald) ; dark
  (color-theme-taylor) ; dark
  ;; (color-theme-standard)

  ;; Set default font size after setting color theme, otherwise wrong size
  ;; is used for new frames
  (steve-set-default-font-size))

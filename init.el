;; -*- coding: utf-8 -*-

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *vi-emulation-support-enabled* t) ; "viper-mode"
(setq *haskell-support-enabled* t)
(setq *ocaml-support-enabled* t)
(setq *common-lisp-support-enabled* t)
(setq *clojure-support-enabled* t)
(setq *scheme-support-enabled* t)
(setq *macbook-pro-support-enabled* t)
(setq *erlang-support-enabled* t)
(setq *darcs-support-enabled* t)
(setq *rails-support-enabled* t)
(setq *spell-check-support-enabled* nil)
(setq *byte-code-cache-enabled* nil)
(setq *twitter-support-enabled* nil)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))


;;----------------------------------------------------------------------------
;; Make elisp more civilised
;;----------------------------------------------------------------------------
(require 'cl)


;;----------------------------------------------------------------------------
;; Set load path
;;----------------------------------------------------------------------------
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))


;;----------------------------------------------------------------------------
;; Automatically byte-compile .el files
;;----------------------------------------------------------------------------
(when *byte-code-cache-enabled*
  (require 'init-byte-code-cache))


;;----------------------------------------------------------------------------
;; Use elisp package manager (http://tromey.com/elpa/)
;;----------------------------------------------------------------------------
(setq load-path (cons (expand-file-name "~/.emacs.d/elpa") load-path))
(require 'package)
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
;; Easy way to check that we're operating on a specific file type
;;----------------------------------------------------------------------------
(defun filename-has-extension-p (extensions)
  (and buffer-file-name
       (string-match (concat "\\." (regexp-opt extensions t) "\\($\\|\\.\\)") buffer-file-name)))


;;----------------------------------------------------------------------------
;; Locate executables
;;----------------------------------------------------------------------------
(defun find-executable (name)
  "Return the full path of an executable file name `name'
in `exec-path', or nil if no such command exists"
  (loop for dir in exec-path
        for full-path = (expand-file-name (concat dir "/" name))
        when (file-executable-p full-path)
        return full-path))


;;----------------------------------------------------------------------------
;; Augment search path for external programs (for OSX)
;;----------------------------------------------------------------------------
(when *is-a-mac*
  (eval-after-load "woman"
    '(setq woman-manpath (append (list "/opt/local/man") woman-manpath)))
  (dolist (dir (mapcar 'expand-file-name '("~/.cabal/bin" "/usr/local/bin" "/opt/local/bin"
                                           "/opt/local/lib/postgresql84/bin" "~/bin")))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))


;;----------------------------------------------------------------------------
;; Add hooks to allow conditional setup of window-system and console frames
;;----------------------------------------------------------------------------
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)


;;----------------------------------------------------------------------------
;; Console-specific set-up
;;----------------------------------------------------------------------------
(defun fix-up-xterm-control-arrows ()
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;5D" [C-left])
  (define-key function-key-map "\e[5A"   [C-up])
  (define-key function-key-map "\e[5B"   [C-down])
  (define-key function-key-map "\e[5C"   [C-right])
  (define-key function-key-map "\e[5D"   [C-left]))

(add-hook 'after-make-console-frame-hooks
          (lambda ()
            (fix-up-xterm-control-arrows)
            (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
            (mwheel-install)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((prev-frame (selected-frame)))
              (select-frame frame)
              (prog1
                  (unless window-system
                    (set-frame-parameter frame 'menu-bar-lines 0))
                (select-frame prev-frame)))))


;;----------------------------------------------------------------------------
;; Include buffer name and file path in title bar
;;----------------------------------------------------------------------------
(defvar *user*    (user-login-name) "user login name")
(defvar *hostname*
  (let ((n (system-name))) (substring n 0 (string-match "\\." n))) "unqualified host name")

(defun network-location ()
  "Report the network location of this computer; only implemented for Macs"
  (when *is-a-mac*
    (let ((scselect (shell-command-to-string "/usr/sbin/scselect")))
      (if (string-match "^ \\* .*(\\(.*\\))$" scselect)
          (match-string 1 scselect)))))

(defun concise-network-location ()
  (let ((l (network-location)))
    (if (and l (not (string-equal "Automatic" l)))
        (concat "[" l "]")
      "")))

(defun concise-buffer-file-name ()
  (when (buffer-file-name)
    (replace-regexp-in-string (regexp-quote (getenv "HOME")) "~" (buffer-file-name))))
(setq frame-title-format '("%b - " *user* "@" *hostname*
                           (:eval (concise-network-location)) " - "
                           (:eval (concise-buffer-file-name))))


;;----------------------------------------------------------------------------
;; Make yes-or-no questions answerable with 'y' or 'n'
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)


;;----------------------------------------------------------------------------
;; To be able to M-x without meta
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x C-m") 'execute-extended-command)


;;----------------------------------------------------------------------------
;; OS X usability tweaks
;;----------------------------------------------------------------------------
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(0.001))
  (when *is-cocoa-emacs*
    ;; Woohoo!!
    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "M-ˍ") 'ns-do-hide-others)  ;; what describe-key reports
    (global-set-key (kbd "M-c") 'ns-copy-including-secondary)
    (global-set-key (kbd "M-v") 'ns-paste-secondary))
  ;; Use Apple-w to close current buffer on OS-X (is normally bound to kill-ring-save)
  (when *vi-emulation-support-enabled*
    (global-set-key [(meta w)] 'kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)


;;----------------------------------------------------------------------------
;; Network proxy configuration
;;----------------------------------------------------------------------------
(require 'init-proxies)


;;----------------------------------------------------------------------------
;; Enhanced dired
;;----------------------------------------------------------------------------
(require 'dired+)
(setq dired-recursive-deletes 'top)
(define-key dired-mode-map [mouse-2] 'dired-find-file)


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
  (define-key viper-insert-global-user-map (kbd "C-n") 'dabbrev-expand)
  (define-key viper-insert-global-user-map (kbd "C-p") 'dabbrev-expand)

  ;; Stop C-u from clobbering prefix-arg -- I always use C-b/C-f to scroll
  (define-key viper-vi-basic-map "\C-u" nil)

  ;; Vim-style searching of the symbol at point, made easy by highlight-symbol
  (autoload 'highlight-symbol-next "highlight-symbol" "Highlight symbol at point")
  (autoload 'highlight-symbol-prev "highlight-symbol" "Highlight symbol at point")
  (setq highlight-symbol-on-navigation-p t)
  (define-key viper-vi-global-user-map "*" 'highlight-symbol-next)
  (define-key viper-vi-global-user-map "#" 'highlight-symbol-prev))


;; Work around a problem in Cocoa emacs, wherein setting the cursor coloring
;; is incredibly slow; viper sets the cursor very frequently in insert mode
(when (and *vi-emulation-support-enabled* *is-cocoa-emacs*)
  (defun viper-change-cursor-color (new-color &optional frame)))


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
;; Use regex searches by default.
;;----------------------------------------------------------------------------
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


;;----------------------------------------------------------------------------
;; Easily count words (http://emacs-fu.blogspot.com/2009/01/counting-words.html)
;;----------------------------------------------------------------------------
(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))


;;----------------------------------------------------------------------------
;; Modeline tweaks
;;----------------------------------------------------------------------------
(size-indication-mode)


;;----------------------------------------------------------------------------
;; Modeline tweaks
;;----------------------------------------------------------------------------
(autoload 'linum-mode "linum" "Toggle line numbering" t)


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
;; Highlight URLs in comments/strings
;;----------------------------------------------------------------------------
(add-hook 'find-file-hooks 'goto-address-prog-mode)


;;----------------------------------------------------------------------------
;; Basic flymake configuration
;;----------------------------------------------------------------------------
(require 'init-flymake)


;;----------------------------------------------------------------------------
;; Luke Gorrie's "lively.el"
;;----------------------------------------------------------------------------
(autoload 'lively "lively" "Interactively updating text" t)


;;----------------------------------------------------------------------------
;; Twitter
;;----------------------------------------------------------------------------
(when *twitter-support-enabled*
  (require 'init-twitter))


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
(require 'init-javascript)

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
;; Markdown-mode
;;----------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)


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
  (require 'init-darcs))


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
(require 'init-git)


;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)


;;----------------------------------------------------------------------------
;; File and buffer navigation
;;----------------------------------------------------------------------------
(require 'recentf)
(setq recentf-max-saved-items 100)
(require 'init-ido)
(require 'init-anything)


;;----------------------------------------------------------------------------
;; Autocomplete
;;----------------------------------------------------------------------------
(require 'auto-complete nil t)
(global-auto-complete-mode t)
(setq ac-auto-start 3)
(setq ac-dwim nil)
(set-default 'ac-sources
             (if (> emacs-major-version 22)
                 (progn
                   (require 'ac-dabbrev)
                   '(ac-source-dabbrev ac-source-words-in-buffer))
               ;; dabbrev is very slow in emacs 22
               '(ac-source-words-in-buffer)))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

;; This stops "end" followed by "RET" getting completed to something
;; like "endomorph" - have to use an explicit "TAB" to complete.
(define-key ac-complete-mode-map (kbd "\r") nil)

(when *vi-emulation-support-enabled*
  (define-key ac-complete-mode-map (kbd "C-n") 'dabbrev-expand)
  (define-key ac-complete-mode-map (kbd "C-p") 'dabbrev-expand)
  (define-key ac-complete-mode-map viper-ESC-key 'viper-intercept-ESC-key))


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(require 'init-window-split)


;;----------------------------------------------------------------------------
;; Desktop saving
;;----------------------------------------------------------------------------
;; save a list of open files in ~/.emacs.d/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-path '("~/.emacs.d"))
(setq desktop-save 'if-exists)
(desktop-save-mode 1)


(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)


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

(require 'init-maxframe)


;;----------------------------------------------------------------------------
;; Fonts
;;----------------------------------------------------------------------------
(require 'init-fonts)


;;----------------------------------------------------------------------------
;; Color themes
;;----------------------------------------------------------------------------
(require 'init-themes)


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
;; Compilation
;;----------------------------------------------------------------------------
(require 'todochiku) ;; growl notifications when compilation finishes
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
(require 'init-org)


;;----------------------------------------------------------------------------
;; NXML
;;----------------------------------------------------------------------------
(require 'init-nxml)


;;----------------------------------------------------------------------------
;; Python
;;----------------------------------------------------------------------------
(require 'init-python-mode)


;;----------------------------------------------------------------------------
;; Ruby & Rails
;;----------------------------------------------------------------------------
(require 'init-ruby-mode)
(when *rails-support-enabled*
  (require 'init-rails))


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
(require 'init-css)

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
(autoload 'smarty-mode "smarty-mode" "Smarty Mode" t)
(add-auto-mode 'smarty-mode "\\.tpl$")


;;----------------------------------------------------------------------------
;; Lisp / Scheme / Slime
;;----------------------------------------------------------------------------
(require 'init-lisp)
(when *common-lisp-support-enabled*
  (require 'init-common-lisp))
(when *clojure-support-enabled*
  (require 'init-clojure))
(when *scheme-support-enabled*
  ; See http://bc.tech.coop/scheme/scheme-emacs.htm
  (require 'quack))

;;----------------------------------------------------------------------------
;; Haskell
;;----------------------------------------------------------------------------
(when *haskell-support-enabled*
  (require 'init-haskell))


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
  (require 'init-flyspell))


;;----------------------------------------------------------------------------
;; Log typed commands into a buffer for demo purposes
;;----------------------------------------------------------------------------
(autoload 'mwe:log-keyboard-commands "mwe-log-commands"
  "Log commands executed in the current buffer" t)


;;----------------------------------------------------------------------------
;; Conversion of line endings
;;----------------------------------------------------------------------------
;; Can also use "C-x ENTER f dos" / "C-x ENTER f unix" (set-buffer-file-coding-system)
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

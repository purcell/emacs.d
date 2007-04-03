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
(setq *darcs-support-enabled* t) ; You can use darcs to update these conf files
(setq *rails-support-enabled* t)


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
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; Outboard initialisation files
;;----------------------------------------------------------------------------
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))
(load "load-ruby-mode.el")
(load "load-python-mode.el")


;;----------------------------------------------------------------------------
;; Augment search path for external programs (for OSX)
;;----------------------------------------------------------------------------
(when *macbook-pro-support-enabled*
  (dolist (dir '("/usr/local/bin" "/opt/local/bin"
                 "/opt/local/lib/postgresql82/bin" "~/bin"))
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
(setq frame-title-format '("%b - " *user* "@" *hostname* " - %f"))


;;----------------------------------------------------------------------------
;; Make yes-or-no questions answerable with 'y' or 'n'
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)


;;----------------------------------------------------------------------------
;; Use Apple-w to close current buffer on OS-X (is normally bound to kill-ring-save)
;;----------------------------------------------------------------------------
(when *macbook-pro-support-enabled*
  (global-set-key [(meta w)] 'kill-this-buffer))


;;----------------------------------------------------------------------------
;; Enhanced dired
;;----------------------------------------------------------------------------
(require 'dired+)


;;----------------------------------------------------------------------------
;; Show and edit all lines matching a regex
;;----------------------------------------------------------------------------
(require 'all)


;;----------------------------------------------------------------------------
;; VI emulation and related key mappings
;;----------------------------------------------------------------------------
(when *vi-emulation-support-enabled*
  (setq viper-mode t)
  (require 'viper)
  (define-key viper-insert-global-user-map "\C-n" 'hippie-expand)
  (define-key viper-insert-global-user-map "\C-p" 'hippie-expand))


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
;; Javascript using ecmascript-mode
;;----------------------------------------------------------------------------
(autoload 'ecmascript-mode "ecmascript-mode" "Mode for editing javascript files" t)
(add-auto-mode 'ecmascript-mode "\\.js$")
(eval-after-load "mmm-mode"
  '(progn
     (mmm-add-group 'ecmascript
		    '((js-script-cdata
		       :submode ecmascript-mode
		       :face mmm-code-submode-face
		       :front "<script[^>]*>[ \t\n]*<!\\[CDATA\\[[ \t]*\n?"
		       :back "[ \t]*]]>[ \t\n]*</script>"
		       :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
				    @ "\n" _ "\n" @ "</script>" @)))
		      (js-script
		       :submode ecmascript-mode
		       :face mmm-code-submode-face
		       :front "<script[^>]*>[ \t]*\n?"
		       :back "[ \t]*</script>"
		       :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
				    @ "\n" _ "\n" @ "</script>" @)))
		      (js-inline
		       :submode ecmascript-mode
		       :face mmm-code-submode-face
		       :front "on\w+=\""
		       :back "\"")))
     (mmm-add-mode-ext-class 'nxml-mode "\\.r?html$" 'ecmascript)))


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
;; Subversion
;;----------------------------------------------------------------------------
(require 'psvn)


;;----------------------------------------------------------------------------
;; Darcs
;;----------------------------------------------------------------------------
(when *darcs-support-enabled*
  (require 'darcs)
  (add-to-list 'vc-handled-backends 'DARCS)
  (autoload 'vc-darcs-find-file-hook "vc-darcs")
  (add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

  (require 'darcsum)
  (setq darcsum-whatsnew-switches "-l")

  (eval-after-load "grep"
  '(add-to-list 'grep-find-ignored-directories "_darcs")))


;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
(require 'mmm-mode)
(setq mmm-global-mode t)
(setq mmm-submode-decoration-level 2)
(mmm-add-mode-ext-class nil "\.jsp$" 'jsp)
(setq-default mmm-never-modes
              (append '(sldb-mode) '(ediff-mode) '(text-mode)
                      '(compilation-mode) '(inferior-haskell-mode)
                      mmm-never-modes))


;;----------------------------------------------------------------------------
;; File and buffer navigation
;;----------------------------------------------------------------------------
;; Use C-f during file selection to switch to regular find-file
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)

(setq ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer nil
      ibuffer-sorting-mode 'recency
      ibuffer-use-header-line t)

(require 'recentf)
(setq recentf-max-saved-items 100)
(defun steve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'steve-ido-choose-from-recentf)


;;----------------------------------------------------------------------------
;; Predictive abbreviations
;;----------------------------------------------------------------------------
(require 'pabbrev)


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
(when *macbook-pro-support-enabled*
  (set-face-attribute 'default nil :family "monaco" :height 120)  
  ;; Default frame size (perfect for Macbook Pro when scrollbar, dock and toolbar hidden...)
  (setq initial-frame-alist '((width  . 202) (height . 53) (top . 0) (left . 3) (tool-bar-lines . 0)))
  (setq default-frame-alist '((width  . 202) (height . 53) (top . 22) (left . 3) (tool-bar-lines . 0))))

(tool-bar-mode nil)
(scroll-bar-mode nil)


;;----------------------------------------------------------------------------
;; ECB (Emacs Code Browser)
;;----------------------------------------------------------------------------
(when *ecb-support-enabled*
  ;; Change default location of semantic.cache files
  (setq semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
  (unless (file-directory-p semanticdb-default-save-directory)
    (make-directory semanticdb-default-save-directory))
  (require 'cedet)
  (require 'ecb-autoloads)

  (add-hook 'ecb-activate-hook
            (lambda () (setq global-semantic-idle-scheduler-mode nil))))


;;----------------------------------------------------------------------------
;; NXML
;;----------------------------------------------------------------------------
(load-library "rng-auto")
(add-to-list 'auto-mode-alist
              (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                    'nxml-mode))
(unify-8859-on-decoding-mode)
(setq magic-mode-alist (cons '("<＼＼?xml " . nxml-mode) magic-mode-alist))
(fset 'html-mode 'nxml-mode)
(fset 'xml-mode 'nxml-mode)
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (flyspell-mode)
	    (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))


;;----------------------------------------------------------------------------
;; Ruby
;;----------------------------------------------------------------------------
(autoload 'ruby-electric-mode "ruby-electric" "Electric brackes/quotes/keywords for Ruby source" t)
(setq ruby-electric-expand-delimiters-list nil)  ; Only use ruby-electric for adding 'end'
(add-hook 'ruby-mode-hook
          (lambda () (ruby-electric-mode t)))
(when *vi-emulation-support-enabled*
  (add-hook 'ruby-mode-hook (lambda () (viper-change-state-to-vi))))

(add-auto-mode 'ruby-mode "Rakefile$" "\.rake$" "\.rxml$" "\.rjs" ".irbrc")
(add-auto-mode 'html-mode "\.rhtml$")
(eval-after-load "compile"
  '(progn
    ;; Jump to lines from Ruby Test::Unit stack traces in 'compile' mode
    (add-to-list 'compilation-error-regexp-alist
                 '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))
    ;; Jump to lines from Ruby stack traces in 'compile' mode
    (add-to-list 'compilation-error-regexp-alist
                 '("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))))
(setq compile-command "rake ")

(mmm-add-classes
 '((eruby :submode ruby-mode :front "<%[#=]?" :back "-?%>"
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))

(mmm-add-mode-ext-class 'nxml-mode "\\.rhtml$" 'eruby)
(mmm-add-mode-ext-class 'yaml-mode "\\.yml$" 'eruby)

(require 'which-func)
(add-to-list 'which-func-modes 'ruby-mode)
(setq imenu-auto-rescan t)       ; ensure function names auto-refresh
(setq imenu-max-item-length 200) ; ensure function names are not truncated
(defun ruby-execute-current-file ()
  "Execute the current ruby file (e.g. to execute all tests)."
  (interactive)
  (compile (concat "ruby " (file-name-nondirectory (buffer-file-name)))))
(defun ruby-test-function ()
  "Test the current ruby function (must be runnable via ruby <buffer> --name <test>)."
  (interactive)
  (let* ((funname (which-function))
         (fn (and funname (and (string-match "\\(#\\|::\\)\\(test.*\\)" funname) (match-string 2 funname)))))
    (compile (concat "ruby " (file-name-nondirectory (buffer-file-name)) (and fn (concat " --name " fn))))))

; run the current buffer using Shift-F8
(add-hook 'ruby-mode-hook (lambda () (local-set-key [S-f8] 'ruby-execute-current-file)))
; run the current test function using F8 key
(add-hook 'ruby-mode-hook (lambda () (local-set-key [f8] 'ruby-test-function)))

(add-hook 'ruby-mode-hook (lambda () (local-set-key [f7] 'recompile)))
(add-hook 'rails-minor-mode-hook (lambda () (local-set-key [f7] 'recompile)))


(require 'find-func)
(defun directory-of-library (library-name)
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

(autoload 'ri "ri-ruby" "Show ri documentation for Ruby symbols" t)
(setq ri-ruby-script (concat (directory-of-library "ri-ruby") "ri-emacs.rb"))


;;----------------------------------------------------------------------------
; Rails (http://rubyforge.org/projects/emacs-rails/)
;;----------------------------------------------------------------------------
(when *rails-support-enabled*
  (defun try-complete-abbrev (old)
    (if (expand-abbrev) t nil))

  (setq hippie-expand-try-functions-list
        '(try-complete-abbrev
          try-complete-file-name
          try-expand-dabbrev))

  ; Remove annoying tab completion behaviour enabled by Rails
  (eval-after-load "rails-lib" '(defun indent-or-complete ()
                                  (interactive)
                                  (unless (when (and (boundp 'snippet)
                                                     snippet)
                                            (snippet-next-field))
                                    (indent-for-tab-command))))

  (require 'rails)
  (setq rails-webrick:use-mongrel t)
  (setq rails-api-root (expand-file-name "~/Documents/External/rails"))

  (when *ecb-support-enabled*
    (require 'ecb)
    (add-to-list 'ecb-compilation-buffer-names '("\\(development\\|test\\|production\\).log" . t))
    (add-to-list 'ecb-compilation-buffer-names '("\\*Rails" . t))))


;;----------------------------------------------------------------------------
; Automatically set execute perms on files if first line begins with '#!'
;;----------------------------------------------------------------------------
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


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
;; CSV mode
;;----------------------------------------------------------------------------
(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")


;;----------------------------------------------------------------------------
;; PHP
;;----------------------------------------------------------------------------
(autoload 'php-mode "php-mode" "mode for editing php files" t)
(add-auto-mode 'php-mode "\\.php[345]?\\'\\|\\.phtml\\." "\\.(inc|tpl)$")


;;----------------------------------------------------------------------------
;; Lisp / Scheme / Slime
;;----------------------------------------------------------------------------
;; pretty lambda (see also slime) ->  "λ"
;;  'greek small letter lambda' / utf8 cebb / unicode 03bb -> \u03BB / mule?!
;; in greek-iso8859-7 -> 107  >  86 ec
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    'font-lock-keyword-face))))))

(autoload 'paredit-mode "paredit-beta"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(defun enable-paredit (keymap)
  (paredit-mode +1)
  (define-key keymap (kbd "(") 'paredit-open-list)
  (define-key keymap (kbd ")") 'paredit-close-list)
  (define-key keymap (kbd "RET") 'paredit-newline))

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook (lambda () (enable-paredit emacs-lisp-mode-map)))

(when *common-lisp-support-enabled*
  (setf slime-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix)
          (cmucl ("cmucl") :coding-system iso-latin-1-unix)))
  (setf slime-default-lisp 'sbcl)
  (require 'slime)
  (slime-setup)
  (add-auto-mode 'lisp-mode "\\.cl$")
  (add-hook 'slime-mode-hook 'pretty-lambdas)
  (add-hook 'slime-mode-hook (lambda () (enable-paredit slime-mode-map)))
  (global-set-key [f4] 'slime-selector))

(when *scheme-support-enabled*
  ; See http://bc.tech.coop/scheme/scheme-emacs.htm
  (require 'quack))
    

;;----------------------------------------------------------------------------
;; Haskell
;;----------------------------------------------------------------------------
(when *haskell-support-enabled*
  (load-library "haskell-site-file")

  (load-library "cabal-mode")

  (require 'hoogle)

  (setq haskell-program-name (executable-find "ghci"))
  (setq haskell-font-lock-symbols t)

  (add-hook 'haskell-mode-hook
            (lambda ()
	      (define-key haskell-mode-map [?\C-c h] 'hoogle-lookup)
              (turn-on-haskell-doc-mode)
              (turn-on-haskell-indent))))


;;----------------------------------------------------------------------------
;; OCaml
;;----------------------------------------------------------------------------
(when *ocaml-support-enabled*
  (add-auto-mode 'caml-mode "\\.ml[iylp]?$")
  (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
  (if window-system (require 'caml-font)))

  
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
  (set-language-environment 'utf-8)
  (set-keyboard-coding-system 'utf-8-mac)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

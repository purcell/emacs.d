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
(setq *darcs-support-enabled* t) ; You can use darcs to update these conf files
(setq *rails-support-enabled* t)
(setq *spell-check-support-enabled* nil)


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
(load "load-ruby-mode.el")
(load "load-python-mode.el")


;;----------------------------------------------------------------------------
;; Augment search path for external programs (for OSX)
;;----------------------------------------------------------------------------
(when *macbook-pro-support-enabled*
  (eval-after-load "woman"
    '(setq woman-manpath (append (list "/opt/local/man") woman-manpath)))
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
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(0.001))
  ;; Use Apple-w to close current buffer on OS-X (is normally bound to kill-ring-save)
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
;; Turn on highline mode globally
;;----------------------------------------------------------------------------
(require 'hl-line+)
(toggle-hl-line-when-idle 1)

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
;; ;; Can't get this to work...
;; (eval-after-load "mmm-mode"
;;   '(progn
;;      (load-library "javascript")
;;      (load-library "css-mode")
;;      (require 'mmm-sample)
;;      (add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil html-js))
;;      (add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil embedded-css))))


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
  (add-to-list 'vc-handled-backends 'DARCS)
  (autoload 'vc-darcs-find-file-hook "vc-darcs")
  (add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

  (require 'darcsum)
  (setq darcsum-whatsnew-switches "-l")

  (eval-after-load "grep"
  '(add-to-list 'grep-find-ignored-directories "_darcs")))


;;----------------------------------------------------------------------------
;; Git
;;----------------------------------------------------------------------------
;;(setq load-path (cons (expand-file-name "/usr/share/doc/git-core/contrib/emacs") load-path))
;; Downloaded from http://git.kernel.org/?p=git/git.git ;a=tree;hb=HEAD;f=contrib/emacs
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)

(eval-after-load "compile"
  '(add-to-list 'compilation-error-regexp-alist-alist
                '(git-svn "^\\t[A-Z]\\t(.*)$" 1)))
(defun git-svn ()
  (interactive)
  (compile (concat "git-svn " (ido-completing-read "git-svn command: " (list "rebase" "dcommit" "log") nil t))))


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
;; Use C-f during file selection to switch to regular find-file
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point t)
(setq ido-auto-merge-work-directories-length -1)

(require 'recentf)
(setq recentf-max-saved-items 100)
(defun steve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'steve-ido-choose-from-recentf)


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
(defun steve-set-default-font (name size)
  (interactive
   (list (ido-completing-read "font-name: " (mapcar (lambda (n) (list n n)) (mapcar (lambda (p) (car p)) (x-font-family-list))) nil t)
         (read-number "size: " 12)))
  (set-face-attribute 'default nil
                      :family name
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height (* 10 size)))

(lexical-let ((font-name "monaco") (font-size 11) (frame-height 57))
  ;;         ((font-name "monaco") (font-size 12) (frame-height 53))
  ;;         ((font-name "bitstream vera sans mono") (font-size 120) (frame-height 58))  ; untested
  (defun steve-set-default-font-size ()
    (steve-set-default-font font-name font-size))

  (when *macbook-pro-support-enabled*
    (steve-set-default-font-size)
    ;; Default frame size (perfect for Macbook Pro when scrollbar, dock and toolbar hidden...)
    (setq initial-frame-alist `((width  . 202) (height . ,frame-height) (top . 0) (left . 3) (tool-bar-lines . 0)))
    (setq default-frame-alist `((width  . 202) (height . ,frame-height) (top . 22) (left . 3) (tool-bar-lines . 0)))))

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

  ;; Force shadowing of the Emacs-bundled speedbar (cedet's "inversion" package tries
  ;; and fails to handle this)
  (setq load-path (cons (concat (directory-of-library "cedet") "/../speedbar/") load-path))
  (require 'cedet)
  (require 'ecb-autoloads)

  (add-hook 'ecb-activate-hook
            (lambda () (setq global-semantic-idle-scheduler-mode nil))))


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
(add-hook 'nxml-mode-hook (lambda ()
                            (make-variable-buffer-local 'ido-use-filename-at-point)
                            (setq ido-use-filename-at-point nil)))
(when *spell-check-support-enabled*
  (add-hook 'nxml-mode-hook
            (lambda ()
              (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))))


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
     ;; Jump to lines from Ruby stack traces in 'compile' mode
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(ruby "\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\):in `" 1 2))))
(setq compile-command "rake ")

(eval-after-load "mmm-mode"
  '(progn
     (mmm-add-classes
      '((eruby :submode ruby-mode :front "<%[#=]?" :back "-?%>"
               :match-face (("<%#" . mmm-comment-submode-face)
                            ("<%=" . mmm-output-submode-face)
                            ("<%"  . mmm-code-submode-face))
               :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
                        (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
                        (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))
     (mmm-add-mode-ext-class 'nxml-mode "\\.rhtml$" 'eruby)
     (mmm-add-mode-ext-class 'yaml-mode "\\.yml$" 'eruby)))


(define-derived-mode ruby-compilation-mode compilation-mode "Compilation[ruby]"
  "Major mode for running ruby scripts and tests."
  (set (make-local-variable 'compilation-error-regexp-alist) '(ruby)))

(defun ruby-compile (command)
  (compile command)
  (with-current-buffer "*compilation*" (ruby-compilation-mode)))


(require 'which-func)
(add-to-list 'which-func-modes 'ruby-mode)
(setq imenu-auto-rescan t)       ; ensure function names auto-refresh
(setq imenu-max-item-length 200) ; ensure function names are not truncated
(defun ruby-execute-current-file ()
  "Execute the current ruby file (e.g. to execute all tests)."
  (interactive)
  (ruby-compile (concat "ruby " (file-name-nondirectory (buffer-file-name)))))
(defun ruby-test-function ()
  "Test the current ruby function (must be runnable via ruby <buffer> --name <test>)."
  (interactive)
  (let* ((funname (which-function))
         (fn (and funname (and (string-match "\\(#\\|::\\)\\(test.*\\)" funname) (match-string 2 funname)))))
    (ruby-compile (concat "ruby " (file-name-nondirectory (buffer-file-name)) (and fn (concat " --name " fn))))))

; run the current buffer using Shift-F7
(add-hook 'ruby-mode-hook (lambda () (local-set-key [S-f7] 'ruby-execute-current-file)))
; run the current test function using F8 key
(add-hook 'ruby-mode-hook (lambda () (local-set-key [f7] 'ruby-test-function)))

(add-hook 'ruby-mode-hook (lambda () (local-set-key [f6] 'recompile)))
(when *rails-support-enabled*
  (add-hook 'rails-minor-mode-hook (lambda () (local-set-key [f6] 'recompile))))


(autoload 'ri "ri-ruby" "Show ri documentation for Ruby symbols" t)
(setq ri-ruby-script (concat (directory-of-library "ri-ruby") "ri-emacs.rb"))


;;----------------------------------------------------------------------------
; Rails (http://rubyforge.org/projects/emacs-rails/)
;;----------------------------------------------------------------------------
(when *rails-support-enabled*
  (require 'rails)
  (setq rails-webrick:use-mongrel t)
  (setq rails-api-root (expand-file-name "~/Documents/External/rails"))

  (when *ecb-support-enabled*
    (require 'ecb)
    ;; Flymake confuses ecb's idea of which buffers are compilation buffers
    (defun comint-but-not-flymake-p (buf)
      (and (comint-check-proc buf)
           (not (buffer-local-value 'flymake-mode-line buf))))
    (setq ecb-compilation-predicates '(comint-but-not-flymake-p))

    (setq ecb-compilation-buffer-names
          (append ecb-compilation-buffer-names
                  '(("\\(development\\|test\\|production\\).log" . t)
                    ("\\*R" . t))))))


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
(add-auto-mode 'php-mode "\\.php[345]?\\'\\|\\.phtml\\." "\\.(inc|tpl)$" "\\.module$")


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
  ;; See http://bc.tech.coop/blog/070927.html
  (setq slime-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix)
          (cmucl ("cmucl") :coding-system iso-latin-1-unix)))
  (require 'slime-autoloads)
  (add-auto-mode 'lisp-mode "\\.cl$")
  (global-set-key [f4] 'slime-selector)
  (add-hook 'lisp-mode-hook (lambda ()
                              (cond ((not (featurep 'slime))
                                     (require 'slime)
                                     (normal-mode)))))

  (eval-after-load "slime"
    '(progn
       (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
       (require 'slime-fancy)
       (require 'slime-banner)
       (require 'slime-asdf)
       (slime-banner-init)
       (slime-asdf-init)
       (setq slime-complete-symbol*-fancy t)
       (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
       (add-hook 'slime-mode-hook 'pretty-lambdas)
       (add-hook 'slime-mode-hook (lambda () (enable-paredit slime-mode-map)))
       (slime-setup)))

  ; From http://bc.tech.coop/blog/070515.html
  (defun lispdoc ()
    "Searches lispdoc.com for SYMBOL, which is by default the symbol currently under the curser"
    (interactive)
    (let* ((word-at-point (word-at-point))
           (symbol-at-point (symbol-at-point))
           (default (symbol-name symbol-at-point))
           (inp (read-from-minibuffer
                 (if (or word-at-point symbol-at-point)
                     (concat "Symbol (default " default "): ")
                   "Symbol (no default): "))))
      (if (and (string= inp "") (not word-at-point) (not
                                                     symbol-at-point))
          (message "you didn't enter a symbol!")
        (let ((search-type (read-from-minibuffer
                            "full-text (f) or basic (b) search (default b)? ")))
          (browse-url (concat "http://lispdoc.com?q="
                              (if (string= inp "")
                                  default
                                inp)
                              "&search="
                              (if (string-equal search-type "f")
                                  "full+text+search"
                                "basic+search")))))))
  (define-key lisp-mode-map (kbd "C-c l") 'lispdoc))

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
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t))


;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(when *spell-check-support-enabled*
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  scheme-mode-hook
                  ruby-mode-hook
                  yaml-mode
                  python-mode-hook
                  shell-mode-hook
                  php-mode-hook
                  css-mode-hook
                  haskell-mode-hook
                  caml-mode-hook
                  nxml-mode-hook
                  crontab-mode-hook
                  perl-mode-hook
                  tcl-mode-hook
                  javascript-mode-hook))
    (add-hook hook 'flyspell-prog-mode)))



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
  (set-keyboard-coding-system 'utf-8-mac)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))


;;----------------------------------------------------------------------------
;; Color themes
;;----------------------------------------------------------------------------
(require 'color-theme-autoloads)
(color-theme-initialize)
(color-theme-pierson)
;; (color-theme-high-contrast)
;; (color-theme-snowish)
;; (color-theme-marquardt)
;; (color-theme-clarity) ; dark
;; (color-theme-dark-laptop) ; dark
;; (color-theme-billw) ; dark
;; (color-theme-oswald) ; dark
;; (color-theme-taylor) ; dark
;; (color-theme-standard)

;; Set default font size after setting color theme, otherwise wrong size
;; is used for new frames
(steve-set-default-font-size)

(setq debug-on-error t)
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
(dolist (dir '("/usr/local/bin" "/opt/local/bin"
               "/opt/local/lib/pgsql8/bin" "~/bin"))
  (setenv "PATH" (concat (expand-file-name dir) ":" (getenv "PATH")))
  (setq exec-path (append (list (expand-file-name dir)) exec-path)))


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
;; Make yes-or-no questions answerable with 'y' or 'n'
;;----------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)


;;----------------------------------------------------------------------------
;; Multiple screen support
;;----------------------------------------------------------------------------
;(require 'winring)
;(winring-initialize)


;;----------------------------------------------------------------------------
;; VI emulation and related key mappings
;;----------------------------------------------------------------------------
(setq viper-mode t)
(require 'viper)
(define-key viper-insert-global-user-map "\C-n" 'hippie-expand)
(define-key viper-insert-global-user-map "\C-p" 'hippie-expand)


;;----------------------------------------------------------------------------
;; Update time stamps marked with 'Last modified:' in HTML and Text mode
;;----------------------------------------------------------------------------
(require 'time-stamp)
(setq time-stamp-line-limit 0
      time-stamp-start "^Last modified: "
      time-stamp-end "$")
(add-hook 'text-mode-hook
          (lambda () (add-hook 'write-file-hooks 'time-stamp nil t)))


;;----------------------------------------------------------------------------
;; Auto line-wrap
;;----------------------------------------------------------------------------
;(add-hook 'text-mode-hook 'auto-fill-mode)  ;; turns it on even for html-mode


;;----------------------------------------------------------------------------
;; Erlang
;;----------------------------------------------------------------------------
;(setq load-path (cons (expand-file-name "/usr/local/share/emacs/site-lisp/distel") load-path))
;;(defun my-erlang-load-hook ()
;; (setq erlang-root-dir "/opt/otp/lib/erlang"))
;;(add-hook 'erlang-load-hook 'my-erlang-load-hook)
(setq erlang-root-dir "/opt/local/lib/erlang")
(require 'erlang-start)
;(require 'distel)
;(add-hook 'erlang-mode-hook 'distel-erlang-mode-hook)


;;----------------------------------------------------------------------------
;; Extensions -> Modes
;;----------------------------------------------------------------------------
(add-auto-mode 'html-mode "\\.(jsp|tmpl)$")


;;----------------------------------------------------------------------------
;; Subversion
;;----------------------------------------------------------------------------
(require 'psvn)


;;----------------------------------------------------------------------------
;; Darcs
;;----------------------------------------------------------------------------
(require 'darcs)
(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(require 'darcsum)
(setq darcsum-whatsnew-switches "-l")


;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
(require 'mmm-mode)
(setq mmm-global-mode t)
(setq mmm-submode-decoration-level 0)
(mmm-add-mode-ext-class nil "\.jsp$" 'jsp)
(setq-default mmm-never-modes
              (append '(ediff-mode) '(text-mode) '(compilation-mode) mmm-never-modes))


;;----------------------------------------------------------------------------
;; File and buffer navigation
;;----------------------------------------------------------------------------
;; Use C-f during file selection to switch to regular find-file
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(setq ido-enable-flex-matching t)

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

;; provide some dired goodies and dired-jump at C-x C-j
(load "dired-x")


;;----------------------------------------------------------------------------
;; Desktop saving
;;----------------------------------------------------------------------------
;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

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
(set-face-attribute 'default nil
                    :family "monaco" :height 120)
;(require 'tool-bar)
(tool-bar-mode nil)
;(require 'scroll-bar)
(scroll-bar-mode nil)
;; Default frame size (perfect for Macbook Pro when scrollbar and toolbar hidden...)
(setq default-frame-alist '((width  . 202)
                            (height . 50)
                            (top . 0)
                            (left . 0)
                            (tool-bar-lines . 0)))


;;----------------------------------------------------------------------------
;; Highlight messy whitespace in red
;;----------------------------------------------------------------------------
(defface we-hate-tabs-face
  '((t (:background "red"))) "Face for tab characters.")

(and standard-display-table (aset standard-display-table ?\C-i  ; Show tabs highlighted
                                  (vector (+ ?\C-i (* (face-id 'we-hate-tabs-face) 524288) ))))


;;----------------------------------------------------------------------------
;; ECB (Emacs Code Browser)
;;----------------------------------------------------------------------------
; Change default location of semantic.cache files
(setq semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
(require 'cedet)
(require 'ecb-autoloads)


;;----------------------------------------------------------------------------
;; Ruby
;;----------------------------------------------------------------------------
(require 'ruby-electric)
(setq ruby-electric-expand-delimiters-list nil)  ; Only use ruby-electric for adding 'end'
(add-hook 'ruby-mode-hook
          (lambda () (ruby-electric-mode)))
(add-hook 'ruby-mode-hook
          (lambda () (viper-change-state-to-vi)))

(add-auto-mode 'ruby-mode "Rakefile$" "\.rake$" "\.rxml$" "\.rjs" ".irbrc")
(add-auto-mode 'html-mode "\.rhtml$")


(require 'compile)
;; Jump to lines from Ruby Test::Unit stack traces in 'compile' mode
(add-to-list 'compilation-error-regexp-alist
             '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))
;; Jump to lines from Ruby stack traces in 'compile' mode
(add-to-list 'compilation-error-regexp-alist
             '("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))
(setq compile-command "rake ")

(mmm-add-classes
 '((eruby :submode ruby-mode :front "<%=?" :back  "-?%>")))

(mmm-add-mode-ext-class 'html-mode "\\.rhtml$" 'eruby)
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

(require 'find-func)
(defun directory-of-library (library-name)
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

(autoload 'ri "ri-ruby" "Show ri documentation for Ruby symbols" t)
(setq ri-ruby-script (concat (directory-of-library "ri-ruby") "ri-emacs.rb"))


;;----------------------------------------------------------------------------
; Rails (http://rubyforge.org/projects/emacs-rails/)
;;----------------------------------------------------------------------------
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

(require 'rails)
(setq rails-webrick:use-mongrel t)
(setq rails-api-root (expand-file-name "~/Documents/External/rails"))

(require 'ecb)
(add-to-list 'ecb-compilation-buffer-names '("\\(development\\|test\\|production\\).log" . t))

;; TODO: Fix ridiculous tab completion behaviour by fixing up 'ruby-indent-or-complete


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
;; Lisp / Slime
;;----------------------------------------------------------------------------
(setf slime-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)
        (cmucl ("cmucl") :coding-system iso-latin-1-unix)))
(setf slime-default-lisp 'sbcl)
(require 'slime)
(slime-setup)

(add-auto-mode 'lisp-mode "\\.cl$")


;;----------------------------------------------------------------------------
;; Haskell
;;----------------------------------------------------------------------------
(load-library "haskell-site-file")

(setq haskell-hugs-program-args '("-98" "+."))
(setq haskell-ghci-program-args '("-fglasgow-exts"))

(add-hook 'haskell-mode-hook
          (lambda ()
            ;(turn-on-haskell-hugs)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)
            (turn-on-haskell-simple-indent)
            (font-lock-mode)))
            ;(turn-on-haskell-ghci)))

;;----------------------------------------------------------------------------
;; Conversion of line endings
;;----------------------------------------------------------------------------
(require 'eol-conversion)


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(server-start)


;;----------------------------------------------------------------------------
;; Mark changed files
;;----------------------------------------------------------------------------
(global-highlight-changes)


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(blink-cursor-delay 0)
 '(blink-cursor-interval 0.4)
 '(buffers-menu-max-size 30)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "make")
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(desktop-path (quote ("~/.emacs.d" "." "~")))
 '(ecb-compile-window-height 0.2)
 '(ecb-grep-find-function (quote rgrep))
 '(ecb-layout-name "left-dir-plus-speedbar")
 '(ecb-layout-window-sizes (quote (("left-analyse" (0.20297029702970298 . 0.33962264150943394) (0.20297029702970298 . 0.20754716981132076) (0.20297029702970298 . 0.20754716981132076) (0.20297029702970298 . 0.22641509433962265)))))
 '(ecb-options-version "2.33beta1")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-run-ediff-in-ecb-frame nil)
 '(ecb-tip-of-the-day nil)
 '(ecb-wget-setup (quote ("wget" . other)))
 '(ecb-windows-width 0.2)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(face-default-stipple "gray3" t)
 '(follow-auto nil)
 '(frame-background-mode nil)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil)
 '(grep-find-ignored-directories (quote ("CVS" ".hg" "{arch}" ".svn" "_darcs")))
 '(highlight-changes-global-changes-existing-buffers t)
 '(highlight-changes-global-initial-state (quote active))
 '(ibuffer-enable t)
 '(indent-tabs-mode nil)
 '(inhibit-splash-screen t)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(recentf-exclude (quote ("/tmp/")))
 '(recentf-mode t)
 '(ruby-electric-expand-delimiters-list nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(slime-autodoc-mode t t)
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.svn\\|_darcs\\)\\'")
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(time-stamp-active t)
 '(time-stamp-format "%3a %3b %2d %02H:%02M:%02S %Z %:y")
 '(tooltip-delay 1.5)
 '(tooltip-mode t nil (tooltip))
 '(truncate-lines t)
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(visible-bell t)
 '(whitespace-check-indent-whitespace t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white"))))
 '(cursor ((t (:background "white" :inverse-video t))))
 '(highlight-changes ((((min-colors 88) (class color)) (:stipple nil :background "#1E1E1E")))))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(when (or window-system (string-match "UTF-8" (shell-command-to-string "locale")))
  (let* ((enc 'utf-8))
    (progn
      (set-keyboard-coding-system enc)
      (setq locale-coding-system enc)
      (set-default-coding-systems enc)
      (set-selection-coding-system enc)
      (prefer-coding-system enc))))

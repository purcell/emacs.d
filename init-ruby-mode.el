(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("ruby" . ruby-mode) interpreter-mode-alist))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook 'inf-ruby-keys)


;;----------------------------------------------------------------------------
;; Ruby - basics
;;----------------------------------------------------------------------------
(autoload 'ruby-electric-mode "ruby-electric" "Electric brackes/quotes/keywords for Ruby source" t)
(require 'rcodetools)
(setq ruby-electric-expand-delimiters-list nil)  ; Only use ruby-electric for adding 'end'
(add-hook 'ruby-mode-hook
          (lambda () (ruby-electric-mode t)))
(when *vi-emulation-support-enabled*
  (add-hook 'ruby-mode-hook (lambda () (viper-change-state-to-vi))))

(add-auto-mode 'ruby-mode "Rakefile$" "\.rake$" "\.rxml$" "\.rjs" ".irbrc")


(setq compile-command "rake ")

(autoload 'ri "ri-ruby" "Show ri documentation for Ruby symbols" t)
(setq ri-ruby-script (concat (directory-of-library "ri-ruby") "ri-emacs.rb"))


;;----------------------------------------------------------------------------
;; Ruby - erb
;;----------------------------------------------------------------------------
(add-auto-mode 'html-mode "\.rhtml$")
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
     (mmm-add-mode-ext-class 'nxml-mode "\\.html\\.erb$" 'eruby)
     (mmm-add-mode-ext-class 'nxml-mode "\\.rhtml$" 'eruby)
     (mmm-add-mode-ext-class 'yaml-mode "\\.yml$" 'eruby)))


;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------
(eval-after-load "mmm-mode"
  '(progn
     (mmm-add-classes
      '((ruby-heredoc-sql :submode sql-mode :front "<<-?end_sql.*\r?\n" :back "[ \t]*end_sql" :face mmm-code-submode-face)))
     (mmm-add-mode-ext-class 'ruby-mode "\\.rb$" 'ruby-heredoc-sql)))


;;----------------------------------------------------------------------------
;; Ruby - haml & sass
;;----------------------------------------------------------------------------
(add-auto-mode 'haml-mode "\.haml$")
(add-auto-mode 'sass-mode "\.sass$")
(autoload 'haml-mode "haml-mode" "Mode for editing haml files" t)
(autoload 'sass-mode "sass-mode" "Mode for editing sass files" t)


;;----------------------------------------------------------------------------
;; Ruby - compilation
;;----------------------------------------------------------------------------

;; Jump to lines from Ruby stack traces in 'compile' mode (regexps borrowed from emacs-rails)
(defun ruby-backtrace-line-regexp (&optional prefix suffix)
  (concat prefix "\\[?\\([^ \f\n\r\t\v]+?\\):\\([0-9]+\\)\\(?::in\s*`\\(.*?\\)'\\)?" suffix))
(eval-after-load "compile"
  '(progn
     (mapcar (lambda (defn) (add-to-list 'compilation-error-regexp-alist-alist defn))
             (list (list 'ruby-backtrace (ruby-backtrace-line-regexp) 1 2 nil 1)
                   (list 'ruby-test-backtrace (ruby-backtrace-line-regexp nil "\\(?:\]:\\|\n$\\)") 1 2 nil 2)))))

(define-derived-mode ruby-compilation-mode compilation-mode "Compilation[ruby]"
  "Major mode for running ruby scripts and tests."
  (set (make-local-variable 'compilation-error-regexp-alist) '(ruby-backtrace ruby-test-backtrace)))

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

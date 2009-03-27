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

(add-hook 'haml-mode-hook (lambda ()
                            (require 'css-mode)
                            (require 'textile-mode)
                            (require 'markdown-mode)
                            (require 'javascript-mode "javascript")))


;;----------------------------------------------------------------------------
;; Ruby - compilation
;;----------------------------------------------------------------------------

(require 'ruby-compilation)

; run the current buffer using Shift-F7
(add-hook 'ruby-mode-hook (lambda () (local-set-key [S-f7] 'ruby-compilation-this-buffer)))
; run the current test function using F8 key
(add-hook 'ruby-mode-hook (lambda () (local-set-key [f7] 'ruby-compilation-this-test)))

(add-hook 'ruby-mode-hook (lambda () (local-set-key [f6] 'recompile)))


(provide 'init-ruby-mode)

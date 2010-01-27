;; Currently loading ruby-mode and inf-ruby from the version bundled with rinari
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq interpreter-mode-alist
      (cons '("ruby" . ruby-mode) interpreter-mode-alist))

(add-auto-mode 'ruby-mode "\\.rb$" "Rakefile$" "\.rake$" "\.rxml$" "\.rjs" ".irbrc" "\.builder" "\.ru")


(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook 'inf-ruby-keys)


(eval-after-load "ruby-mode"
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))


;;----------------------------------------------------------------------------
;; Ruby - flymake
;;----------------------------------------------------------------------------
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)


;;----------------------------------------------------------------------------
;; Ruby - Electric mode
;;----------------------------------------------------------------------------
(autoload 'ruby-electric-mode "ruby-electric" "Electric brackes/quotes/keywords for Ruby source" t)
(require 'rcodetools)
(setq ruby-electric-expand-delimiters-list nil)  ; Only use ruby-electric for adding 'end'
(add-hook 'ruby-mode-hook
          (lambda () (ruby-electric-mode t)))


;;----------------------------------------------------------------------------
;; Ruby - misc
;;----------------------------------------------------------------------------
;; For some unknown reason, viper starts off in insert mode inside ruby-mode buffers
(eval-after-load "viper"
  '(add-to-list 'viper-vi-state-mode-list 'ruby-mode))

(setq compile-command "rake ")

(autoload 'ri "ri-ruby" "Show ri documentation for Ruby symbols" t)
(setq ri-ruby-script (concat (directory-of-library "ri-ruby") "ri-emacs.rb"))


;;----------------------------------------------------------------------------
;; Ruby - erb
;;----------------------------------------------------------------------------
(add-auto-mode 'html-mode "\.rhtml$" "\.html\.erb$")
(eval-after-load "mmm-vars"
  '(progn
     (mmm-add-classes
      '((eruby :submode ruby-mode :front "<%[#=]?" :back "-?%>"
               :match-face (("<%#" . mmm-comment-submode-face)
                            ("<%=" . mmm-output-submode-face)
                            ("<%"  . mmm-code-submode-face))
               :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
                        (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
                        (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))
     (dolist (mode (list 'html-mode 'nxml-mode))
       (add-to-list 'mmm-mode-ext-classes-alist (list mode "\\.r?html\\(\\.erb\\)?$" 'eruby) t))
     (add-to-list 'mmm-mode-ext-classes-alist (list 'yaml-mode "\\.yaml$" 'eruby))))


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

(require 'flymake-haml)
(add-hook 'haml-mode-hook 'flymake-haml-load)
(add-hook 'sass-mode-hook 'flymake-sass-load)


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

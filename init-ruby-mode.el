;; Currently loading ruby-mode and inf-ruby from the version bundled with rinari
(setq interpreter-mode-alist
      (cons '("ruby" . ruby-mode) interpreter-mode-alist))

(add-auto-mode 'ruby-mode "\\.rb$" "Rakefile$" "\.rake$" "\.rxml$" "\.rjs$" ".irbrc$" "\.builder$" "\.ru$" "\.gemspec$" "Gemfile$")


(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook 'inf-ruby-keys)

(setq ruby-use-encoding-map nil)

(eval-after-load "ruby-mode"
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))


;;----------------------------------------------------------------------------
;; Ruby - flymake
;;----------------------------------------------------------------------------
(add-hook 'ruby-mode-hook 'flymake-ruby-load)


;;----------------------------------------------------------------------------
;; Ruby - misc
;;----------------------------------------------------------------------------
;; For some unknown reason, viper starts off in insert mode inside ruby-mode buffers
(eval-after-load "viper"
  '(add-to-list 'viper-vi-state-mode-list 'ruby-mode))

(setq compile-command "rake ")

(defalias 'ri 'yari)


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
       (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?$" 'eruby))
     (mmm-add-mode-ext-class 'yaml-mode "\\.yaml$" 'eruby)
     (dolist (mode (list 'js-mode 'js2-mode))
       (mmm-add-mode-ext-class mode "\\.js\\.erb$" 'eruby))))


;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------
(eval-after-load "mmm-mode"
  '(progn
     (mmm-add-classes
      '((ruby-heredoc-sql :submode sql-mode :front "<<-?end_sql.*\r?\n" :back "[ \t]*end_sql" :face mmm-code-submode-face)))
     (mmm-add-mode-ext-class 'ruby-mode "\\.rb$" 'ruby-heredoc-sql)))


;;----------------------------------------------------------------------------
;; Ruby - compilation
;;----------------------------------------------------------------------------

; run the current buffer using Shift-F7
(add-hook 'ruby-mode-hook (lambda () (local-set-key [S-f7] 'ruby-compilation-this-buffer)))
; run the current test function using F8 key
(add-hook 'ruby-mode-hook (lambda () (local-set-key [f7] 'ruby-compilation-this-test)))

(add-hook 'ruby-mode-hook (lambda () (local-set-key [f6] 'recompile)))



;;----------------------------------------------------------------------------
;; Yaml
;;----------------------------------------------------------------------------
(autoload 'yaml-mode "yaml-mode" "Major mode for YAML source")
(add-auto-mode 'yaml-mode "\\.ya?ml$")



;;----------------------------------------------------------------------------
;; Fix a shortcoming of the outdated rvm.el currently in elpa
;;----------------------------------------------------------------------------
(eval-after-load 'rvm
  '(defun rvm--rvmrc-read-version (path-to-rvmrc)
     (with-temp-buffer
       (insert-file-contents path-to-rvmrc)
       (goto-char (point-min))
       (if (re-search-forward
            (concat "rvm\s+\\(use\s+\\)?\\(.+\\)" rvm--gemset-separator "\\([^\s]*\\)") nil t)
           (list (match-string 2) (match-string 3))
         nil))))
;; Leave a reminder for myself
(when (package-installed-p 'rvm '(1 1 1))
  (message "TODO: remove rvm workaround"))


(provide 'init-ruby-mode)

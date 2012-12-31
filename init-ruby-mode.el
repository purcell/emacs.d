(eval-after-load 'rinari
  '(diminish 'rinari-minor-mode "Rin"))

(add-auto-mode 'ruby-mode "\\.rb\\'" "Rakefile\\'" "\.rake\\'" "\.rxml\\'" "\.rjs\\'" ".irbrc\\'" "\.builder\\'" "\.ru\\'" "\.gemspec\\'" "Gemfile\\'")


(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")

(setq ruby-use-encoding-map nil)

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)))


;;----------------------------------------------------------------------------
;; Ruby - flymake
;;----------------------------------------------------------------------------
(add-hook 'ruby-mode-hook 'flymake-ruby-load)


;;----------------------------------------------------------------------------
;; Ruby - robe
;;----------------------------------------------------------------------------
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-robe)
            (setq completion-at-point-functions '(auto-complete))))


;;----------------------------------------------------------------------------
;; Ruby - misc
;;----------------------------------------------------------------------------
(defalias 'ri 'yari)


;;----------------------------------------------------------------------------
;; Ruby - erb
;;----------------------------------------------------------------------------
(defun sanityinc/ensure-mmm-erb-loaded ()
  (require 'mmm-erb))
(dolist (hook (list 'html-mode-hook 'nxml-mode-hook 'yaml-mode-hook))
  (add-hook hook 'sanityinc/ensure-mmm-erb-loaded))

(dolist (mode (list 'html-mode 'html-erb-mode 'nxml-mode))
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-to-list 'auto-mode-alist '("\\.r?html\\(\\.erb\\)?\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))
(mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\'" 'erb)

(dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
  (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))


;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql :submode sql-mode :front "<<-?end_sql.*\r?\n" :back "[ \t]*end_sql" :face mmm-code-submode-face)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))


;;----------------------------------------------------------------------------
;; Ruby - compilation
;;----------------------------------------------------------------------------

; run the current buffer using Shift-F7
(add-hook 'ruby-mode-hook (lambda () (local-set-key [S-f7] 'ruby-compilation-this-buffer)))
; run the current test function using F8 key
(add-hook 'ruby-mode-hook (lambda () (local-set-key [f7] 'ruby-compilation-this-test)))

(add-hook 'ruby-mode-hook (lambda () (local-set-key [f6] 'recompile)))


(provide 'init-ruby-mode)

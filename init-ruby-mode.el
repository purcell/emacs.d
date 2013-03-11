(require-package 'ruby-mode)
(require-package 'flymake-ruby)
(require-package 'rinari)
(require-package 'ruby-compilation)
(require-package 'inf-ruby)
(require-package 'robe)
(require-package 'yari)
(require-package 'yaml-mode)
(require-package 'haml-mode)
(require-package 'mmm-mode)


(eval-after-load 'rinari
  '(diminish 'rinari-minor-mode "Rin"))

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\.rxml\\'"
               "\\.rjs\\'" ".irbrc\\'" "\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")


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

(require-package 'tagedit)
(eval-after-load "sgml-mode"
  '(progn
     (tagedit-add-paredit-like-keybindings)))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
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


;;----------------------------------------------------------------------------
;; Ruby - handy helpers
;;----------------------------------------------------------------------------

;; Borrowed from https://github.com/textmate/ruby.tmbundle/blob/master/Commands/Convert%20Ruby%20hash%20to%201_9%20syntax.tmCommand
(defun sanityinc/ruby-toggle-hash-syntax (beg end)
  "Toggle between ruby 1.8 and 1.9 hash styles."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (cond
     ((save-excursion (search-forward "=>" end t))
      (replace-regexp ":\\(\\w+\\) +=> +" "\\1: " nil beg end))
     ((save-excursion (re-search-forward "\\w+:" end t))
      (replace-regexp "\\(\\w+\\):\\( *\\(?:\"\\(?:\\\"\\|[^\"]\\)*\"\\|'\\(?:\\'\\|[^']\\)*'\\|\\w+([^)]*)\\|[^,]+\\)\\)" ":\\1 =>\\2" nil beg end)))))


(provide 'init-ruby-mode)

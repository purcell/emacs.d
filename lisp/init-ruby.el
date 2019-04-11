;;; init-ruby.el --- Support for the Ruby language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Basic ruby setup
(require-package 'ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")
(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")

(setq-default
 ruby-use-encoding-map nil
 ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook 'subword-mode)

(after-load 'page-break-lines
  (push 'ruby-mode page-break-lines-modes))

(require-package 'rspec-mode)


(define-derived-mode brewfile-mode ruby-mode "Brewfile"
  "A major mode for Brewfiles, used by homebrew-bundle on MacOS.")

(add-auto-mode 'brewfile-mode "Brewfile\\'")


;;; Inferior ruby
(require-package 'inf-ruby)



;;; Ruby compilation
(require-package 'ruby-compilation)

(after-load 'ruby-mode
  (define-key ruby-mode-map [S-f7] 'ruby-compilation-this-buffer)
  (define-key ruby-mode-map [f7] 'ruby-compilation-this-test))

(after-load 'ruby-compilation
  (defalias 'rake 'ruby-compilation-rake))



;;; Robe
(when (maybe-require-package 'robe)
  (after-load 'ruby-mode
    (add-hook 'ruby-mode-hook 'robe-mode))
  (after-load 'robe
    (after-load 'company
      (push 'company-robe company-backends))))



;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)



(require-package 'goto-gem)


(require-package 'bundler)


(when (maybe-require-package 'yard-mode)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (after-load 'yard-mode
    (diminish 'yard-mode)))


;;; ERB
(require-package 'mmm-mode)

(require 'derived)

(defun sanityinc/set-up-mode-for-erb (mode)
  (add-hook (derived-mode-hook-name mode) (lambda () (require 'mmm-erb)))
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(dolist (mode '(html-mode html-erb-mode nxml-mode))
  (sanityinc/set-up-mode-for-erb mode)
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))

(mapc 'sanityinc/set-up-mode-for-erb
      '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

(mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)
(sanityinc/set-up-mode-for-erb 'yaml-mode)

(dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
  (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))


;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------

;; Needs to run after rinari to avoid clobbering font-lock-keywords?

;; (require-package 'mmm-mode)
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql
;;          :submode sql-mode
;;          :front "<<-?[\'\"]?\\(end_sql\\)[\'\"]?"
;;          :save-matches 1
;;          :front-offset (end-of-line 1)
;;          :back "^[ \t]*~1$"
;;          :delimiter-mode nil)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))

;; (add-to-list 'mmm-set-file-name-for-modes 'ruby-mode)



(provide 'init-ruby)
;;; init-ruby.el ends here

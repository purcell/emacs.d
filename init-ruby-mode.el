(add-auto-mode 'ruby-mode "\\.rb\\'" "Rakefile\\'" "\.rake\\'" "\.rxml\\'" "\.rjs\\'" ".irbrc\\'" "\.builder\\'" "\.ru\\'" "\.gemspec\\'" "Gemfile\\'")


(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")

(setq ruby-use-encoding-map nil)

(add-hook 'robe-mode-hook
          (lambda ()
            ;; {{ rails stuff
            (require 'rinari)
            (setq compile-command "rake ")
            (robe-mode)
            (flymake-ruby-load)))

;; doc look up
(defalias 'ri 'yari)

;;----------------------------------------------------------------------------
;; Ruby - erb
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.rhtml\\(\\.erb\\)?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . web-mode))

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))
;; }}

(provide 'init-ruby-mode)

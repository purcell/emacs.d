;;; init-local-denot.el --- denote settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; denote
;; Remember that the website version of this manual shows the latest
;; developments, which may not be available in the package you are
;; using.  Instead of copying from the web site, refer to the version
;; of the documentation that comes with your package.  Evaluate:
;;
;;     (info "(denote) Sample configuration")
(use-package denote
  :ensure t)

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/Obsidian/Note/"))
(setq denote-save-buffers nil)
(setq denote-known-keywords '("emacs" "git" "software" "network" "ai" "economics"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type 'markdown-yaml) ; Org is the default, set others here
(setq denote-prompts '(title keywords))
(setq denote-excluded-directories-regexp nil)
(setq denote-excluded-keywords-regexp nil)
(setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

;; Read this manual for how to specify `denote-templates'.  We do not
;; include an example here to avoid potential confusion.

(setq denote-date-format nil) ; read doc string

;; By default, we do not show the context of links.  We just display
;; file names.  This provides a more informative view.
(setq denote-backlinks-show-context t)

;; Also see `denote-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)

;; We use different ways to specify a path for demo purposes.
(setq denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "attachments"))
            (expand-file-name "~/Documents/books")))

;; Generic (great if you rename files Denote-style in lots of places):
(add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
(denote-rename-buffer-mode 1)

(use-package denote-journal
  :ensure t
  ;; Bind those to some key for your convenience.
  ;; :commands ( denote-journal-new-entry
  ;;             denote-journal-new-or-existing-entry
  ;;             denote-journal-link-or-create-entry )
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format ""))

(provide 'init-local-denote)
;;; init-local-denote.el ends here

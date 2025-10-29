;; init-local-org.el - org specific settings  -*- lexical-binding: t; -*-

(setq org-directory "~/org/"
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-agenda-files (list org-default-notes-file
                             (expand-file-name "software.org" org-directory)
                             (expand-file-name "web.org" org-directory)
                             (expand-file-name "network.org" org-directory)
                             (expand-file-name "bookmark.org" org-directory)
                             (expand-file-name "now.org" org-directory)))

(with-eval-after-load 'org
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-startup-indented t
        org-adapt-indentation nil
        org-edit-src-content-indentation 0
        org-startup-truncated nil
        org-fontify-done-headline t
        org-fontify-todo-headline t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-pretty-entities t))

(provide 'init-local-org)

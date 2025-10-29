;; init-local-org.el - org specific settings  -*- lexical-binding: t; -*-

(setq org-directory "~/org/"
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-agenda-files (list org-default-notes-file
                             (expand-file-name "software.org" org-directory)
                             (expand-file-name "web.org" org-directory)
                             (expand-file-name "network.org" org-directory)
                             (expand-file-name "bookmark.org" org-directory)
                             (expand-file-name "now.org" org-directory)))

(provide 'init-local-org)

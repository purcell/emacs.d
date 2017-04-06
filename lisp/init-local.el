(setq
 magit-repository-directories
 '(("~" . 1)
   ("~/Projects" . 1)
   ("~/Projects/External" . 1))
 exec-path-from-shell-arguments '("-l")
 org-default-notes-file "~/Documents/org/inbox.org")


(require-package 'evil)

(provide 'init-local)

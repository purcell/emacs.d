(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start t)

(setq edit-server-url-major-mode-alist
      '(("github\\.com" . markdown-mode)
        ("stackoverflow\\.com" . markdown-mode)
        ("lighthouseapp\\.com" . markdown-mode)
        ("rubyonrails\\.org" . textile-mode)))

(provide 'init-edit-server)

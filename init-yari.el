; You can use C-u M-x yari to reload all completion targets.
(require 'yari)
(defun ri-bind-key ()
  (local-set-key "\C-c;y" 'yari-anything))
(add-hook 'ruby-mode-hook 'ri-bind-key)
(add-hook 'nxml-mode-hook 'ri-bind-key)
(add-hook 'html-mode-hook 'ri-bind-key)

(provide 'init-yari)

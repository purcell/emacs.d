; override `better register` which also uses [f1]
; You can use C-u M-x yari to reload all completion targets.
(require 'yari)
(defun ri-bind-key ()
  (local-set-key [f1] 'yari-anything))
(add-hook 'ruby-mode-hook 'ri-bind-key)

(provide 'init-yari)

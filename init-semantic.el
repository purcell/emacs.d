(require 'semantic)

(defun my-semantic-hook ()
  (semantic-add-system-include "/usr/include/wx-2.8" 'c++-mode)
  (semantic-add-system-include "/usr/include/wx-2.8/wx/gtk" 'c++-mode)
  )

(add-hook 'semantic-init-hooks 'my-semantic-hook)

(provide 'init-semantic)


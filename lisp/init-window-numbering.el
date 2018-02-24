(require-package 'window-numbering)


(window-numbering-mode 1)

(setq window-numbering-assign-func
      (lambda () (when (equal (buffer-name) "*Calculator*") 9)))


(provide 'init-window-numbering)

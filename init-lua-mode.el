(add-hook 'lua-mode-hook
          (lambda ()
            (setq safe-local-variable-values
                  '((lua-indent-level . 2)
                    (lua-indent-level . 3)
                    (lua-indent-level . 4)
                    (lua-indent-level . 8)))
            (flymake-lua-load)
            ))
(provide 'init-lua-mode)

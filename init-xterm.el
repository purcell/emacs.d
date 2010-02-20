(require 'init-frame-hooks)

(defun fix-up-xterm-control-arrows ()
  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;5D" [C-left])
  (define-key function-key-map "\e[5A"   [C-up])
  (define-key function-key-map "\e[5B"   [C-down])
  (define-key function-key-map "\e[5C"   [C-right])
  (define-key function-key-map "\e[5D"   [C-left]))

(add-hook 'after-make-console-frame-hooks
          (lambda ()
            (fix-up-xterm-control-arrows)
            (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
            (mwheel-install)))

(provide 'init-xterm)
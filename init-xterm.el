(require 'init-frame-hooks)

(defun fix-up-xterm-control-arrows ()
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
               function-key-map)))
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    (define-key map "\e[5A"   [C-up])
    (define-key map "\e[5B"   [C-down])
    (define-key map "\e[5C"   [C-right])
    (define-key map "\e[5D"   [C-left])))

(add-hook 'after-make-console-frame-hooks
          (lambda ()
            (when (< emacs-major-version 23)
              (fix-up-xterm-control-arrows))
            (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
            ;; Enable wheelmouse support by default
            (cond (window-system
                   (mwheel-install)))
            ))

(provide 'init-xterm)

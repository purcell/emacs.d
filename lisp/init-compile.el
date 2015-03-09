(setq-default compilation-scroll-output t)

(require-package 'alert)

;; Customize `alert-default-style' to get messages after compilation

(defun sanityinc/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (unless (catch 'is-visible
            (walk-windows (lambda (w)
                            (when (eq (window-buffer w) buf)
                              (throw 'is-visible t))))
            nil)
    (alert (concat "Compilation " result)
           :buffer buf
           :category 'compilation)))

(after-load 'compile
  (add-hook 'compilation-finish-functions
            'sanityinc/alert-after-compilation-finish))


(provide 'init-compile)

;;; package --- Summary
;;; Commentary:
;;; Code:

(defun my-java-compile-command ()
  (concat "javac " (buffer-name)))

(add-hook 'java-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command) (my-java-compile-command))))


(provide 'init-javacompile)

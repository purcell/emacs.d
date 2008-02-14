;; Flymake PHP Extension

;; See http://www.openweblog.com/users/hexmode/496282.html

;; Now, whether you're using the emacs you started with or the latest
;; emacs-snapshot, you need to tell emacs to use flymake on PHP files.
;; Add: (add-hook 'php-mode-hook (lambda () (flymake-mode t)))

(require 'flymake)
(unless (fboundp 'flymake-php-init)
  (defun flymake-php-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "php" (list "-f" local-file "-l")))))

(let ((php-ext-re "\\.php[345]?\\'")
      (php-error-re
       "\\(?:Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"))
  (unless (assoc php-ext-re flymake-allowed-file-name-masks)
    (add-to-list 'flymake-allowed-file-name-masks
                 (list php-ext-re
                   'flymake-php-init
                   'flymake-simple-cleanup
                   'flymake-get-real-file-name))
    (add-to-list 'compilation-error-regexp-alist-alist
                 (list 'compilation-php
                   php-error-re  2 3 nil nil))
    (add-to-list 'compilation-error-regexp-alist 'compilation-php)
    (add-to-list 'flymake-err-line-patterns
                 (list php-error-re 2 3 nil 1))))

(provide 'flymake-php)

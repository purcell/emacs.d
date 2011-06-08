;; Flymake support for coffee script
;;
;; Based in part on http://d.hatena.ne.jp/antipop/20110508/1304838383
;;
;; Usage: (add-hook 'coffee-mode-hook '(lambda () (flymake-coffee-load)))

(defconst flymake-allowed-coffee-file-name-masks '(("\\.coffee$" flymake-coffee-init)))

(defvar flymake-coffee-err-line-patterns
  '(("\\(Error: In \\([^,]+\\), .+ on line \\([0-9]+\\).*\\)" 2 3 nil 1)))

(defun flymake-coffee-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-coffee")))

(defun flymake-coffee-init ()
  (list "coffee" (list (flymake-init-create-temp-buffer-copy
                        'flymake-coffee-create-temp-in-system-tempdir))))

(defun flymake-coffee-load ()
  (interactive)
  (require 'flymake)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-coffee-file-name-masks))
  (setq flymake-err-line-patterns flymake-coffee-err-line-patterns)
  (if (executable-find "coffee")
      (flymake-mode t)
    (message "Not enabling flymake: coffee command not found")))


(provide 'flymake-coffee)

;; Change default location of semantic.cache files
(setq semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
(unless (file-directory-p semanticdb-default-save-directory)
  (make-directory semanticdb-default-save-directory))

;; Force shadowing of the Emacs-bundled speedbar (cedet's "inversion" package tries
;; and fails to handle this)
(setq load-path (cons (concat (directory-of-library "cedet") "/../speedbar/") load-path))
(require 'cedet)
(require 'ecb-autoloads)

;; Flymake confuses ecb's idea of which buffers are compilation buffers
(defun comint-but-not-flymake-p (buf)
  (and (comint-check-proc buf)
       (not (buffer-local-value 'flymake-mode-line buf))))
(setq ecb-compilation-predicates '(comint-but-not-flymake-p))

(eval-after-load "ecb"
  `(setq ecb-compilation-buffer-names
	 (append ecb-compilation-buffer-names
		 '(("\\(development\\|test\\|production\\).log" . t)
		   ("\\*R" . t)))))

(add-hook 'ecb-activate-hook
          (lambda () (setq global-semantic-idle-scheduler-mode nil)))

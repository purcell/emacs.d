;; malabar-mode
(require-package 'malabar-mode)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "/usr/share/groovy")
(defun start-malabar ()
  (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode)))

;; emacs-eclim
(require-package 'emacs-eclim)
(defun start-eclim ()
  (interactive)
  (require 'eclim)
  (require 'eclimd)
  (require 'ac-emacs-eclim-source)
  (ac-emacs-eclim-config))

(provide 'init-java)

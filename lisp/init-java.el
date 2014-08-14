;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; malabar-mode for emacs only java development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'malabar-mode)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "/usr/share/groovy")
(defun start-malabar ()
  (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-eclim for java development with Emacs + Eclipse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'emacs-eclim)
(defun start-eclim ()
  (interactive)
  (require 'eclim)
  (require 'eclimd)
  (require 'ac-emacs-eclim-source)
  (ac-emacs-eclim-config))

(provide 'init-java)

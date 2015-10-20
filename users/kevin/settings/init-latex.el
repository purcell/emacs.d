;;; latex --- Summary
;;; Config for latex

(require-package 'auctex)

(require-package 'cdlatex)

;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
;; (add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode

;; latex-preview-pane is a minor mode for Emacs that enables you to preview your LaTeX files directly in Emacs.
(require-package 'latex-preview-pane)
;;(latex-preview-pane-enable)

;; magical syntax highlighting for LaTeX-mode buffers
;;(require-package 'magic-latex-buffer)
;;(add-hook 'latex-mode-hook 'magic-latex-buffer)

;;  Adds several useful functionalities to LaTeX-mode. http://github.com/Bruce-Connor/latex-extra
(require-package 'latex-extra)
;;(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

(provide 'init-latex)
;; init-latex.el end here

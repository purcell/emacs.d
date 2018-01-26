;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))


(provide 'init-flyspell)

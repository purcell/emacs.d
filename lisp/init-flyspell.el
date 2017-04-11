;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(after-load 'flyspell
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))


(provide 'init-flyspell)

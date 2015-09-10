;; settings for ace-isearch, which  combines `isearch', `ace-jump-mode', `avy', and `helm-swoop'.
;;  https://github.com/tam17aki/ace-isearch

(require 'ace-isearch)


(global-ace-isearch-mode +1)


(custom-set-variables
 '(ace-isearch-input-length 7)
 '(ace-isearch-jump-delay 0.25)
 '(ace-isearch-function 'avy-goto-word-or-subword-1)
 '(ace-isearch-use-jump 'printing-char))


;; If you don't want to invoke ace-isearch-funtion-from-isearch, set this variable to nil.
(setq ace-isearch-use-function-from-isearch t)
;;(setq ace-isearch-funtion-from-isearch 'helm-occur-from-isearch)
(setq ace-isearch-funtion-from-isearch 'swoop-from-isearch)

;;(define-key swoop-map (kbd "C-s") 'swoop-action-goto-line-next)
;;(define-key swoop-map (kbd "C-r") 'swoop-action-goto-line-prev)


(define-key isearch-mode-map (kbd "C-x j") 'ace-isearch-jump-during-isearch)



(provide 'init-ace-isearch)
;;; init-ace-isearch.el ends here

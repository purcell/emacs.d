;; @see http://cx4a.org/software/auto-complete/manual.html 
(require 'auto-complete-config)
(global-auto-complete-mode) ; recommended way to (setq global-auto-complete-mode), see info
(setq ac-auto-start nil) ; popup candidates when you press each character is annoying
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
(ac-set-trigger-key "TAB") ; AFTER input prefix, press TAB key ASAP

;; Use C-n/C-p to select candidate ONLY when completionion menu is displayed
;; Below code is copied from offical manual
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; extra modes auto-complete must support 
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                js2-mode js3-mode css-mode less-css-mode))
  (add-to-list 'ac-modes mode))

;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(ac-config-default)

(provide 'init-auto-complete)

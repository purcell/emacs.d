(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(setq ac-dwim t)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)

(defun indent-or-expand-with-ac (&optional arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (not mark-active)
       (not (minibufferp))
       (memq 'auto-complete-mode minor-mode-list)
       (looking-at "\\_>"))
      (ac-start)
    (indent-for-tab-command arg)))

(global-set-key (kbd "TAB") 'indent-or-expand-with-ac)

(set-default 'ac-sources
             '(ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))


(eval-after-load "viper"
  '(progn
     (define-key ac-complete-mode-map (kbd "C-n") 'dabbrev-expand)
     (define-key ac-complete-mode-map (kbd "C-p") 'dabbrev-expand)
     (define-key ac-complete-mode-map viper-ESC-key 'viper-intercept-ESC-key)))

;; Exclude very large buffers from dabbrev
(defun smp-dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'smp-dabbrev-friend-buffer)


(provide 'init-auto-complete)
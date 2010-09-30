(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(setq ac-dwim t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

;; Use SmartTab to trigger AC completion
(require 'smart-tab)
(global-smart-tab-mode 1)

;; Redefine the main smart-tab function to use auto-complete preferentially
(defun smart-tab (prefix)
  "Try to 'do the smart thing' when tab is pressed.
`smart-tab' attempts to expand the text before the point or
indent the current line or selection.

In a regular buffer, `smart-tab' will attempt to expand with
either `hippie-expand' or `dabbrev-expand', depending on the
value of `smart-tab-using-hippie-expand'. If the mark is active,
or PREFIX is \\[universal-argument], then `smart-tab' will indent
the region or the current line (if the mark is not active)."
  (interactive "P")
  (if (smart-tab-must-expand prefix)
      (if (and (not (minibufferp))
               (memq 'auto-complete-mode minor-mode-list))
          (auto-complete)
        (if smart-tab-using-hippie-expand
            (hippie-expxpand nil)
          (dabbrev-expand nil)))
    (smart-tab-default)))


(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))


(eval-after-load "viper"
  '(progn
     (define-key ac-completing-map (kbd "C-n") 'dabbrev-expand)
     (define-key ac-completing-map (kbd "C-p") 'dabbrev-expand)
     (define-key ac-completing-map viper-ESC-key 'viper-intercept-ESC-key)))

;; Exclude very large buffers from dabbrev
(defun smp-dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'smp-dabbrev-friend-buffer)


(provide 'init-auto-complete)

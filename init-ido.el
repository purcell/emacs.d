;; Use C-f during file selection to switch to regular find-file
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

(defun steve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'steve-ido-choose-from-recentf)


;;----------------------------------------------------------------------------
;; ido completion in M-x
;;----------------------------------------------------------------------------
(require 'smex)
(smex-initialize)
(global-set-key "\M-x" 'smex)


(provide 'init-ido)

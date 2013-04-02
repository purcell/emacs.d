(require-package 'ido-ubiquitous)
(require-package 'smex)
(require-package 'idomenu)

;; Use C-f during file selection to switch to regular find-file
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(global-set-key (kbd "M-x") 'smex)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

(when (eval-when-compile (< emacs-major-version 24))
 (defun sanityinc/ido-choose-from-recentf ()
   "Use ido to select a recently opened file from the `recentf-list'"
   (interactive)
   (if (and ido-use-virtual-buffers (fboundp 'ido-toggle-virtual-buffers))
       (ido-switch-buffer)
     (find-file (ido-completing-read "Open file: " recentf-list nil t))))

 (global-set-key [(meta f11)] 'sanityinc/ido-choose-from-recentf))



(provide 'init-ido)

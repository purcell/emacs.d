(setq flymake-gui-warnings-enabled nil)

;; Stop flymake from breaking when ruby-mode is invoked by mmm-mode,
;; at which point buffer-file-name is nil
(eval-after-load "flymake"
  '(progn
     (defun flymake-show-next-error-in-minibuffer ()
       "Move point to the next flymake error and display the error in the minibuffer"
       (interactive)
       (flymake-goto-next-error)
       (let ((err (get-char-property (point) 'help-echo)))
         (when err
           (message err))))

     (global-set-key (kbd "C-`") 'flymake-show-next-error-in-minibuffer)

     (defun flymake-can-syntax-check-file (file-name)
       "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
       (if (and file-name (flymake-get-init-function file-name)) t nil))))


(provide 'init-flymake)

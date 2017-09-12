;; copy shit into osx clipboard
;; TODO: maybe we should only do this for macosx(yes -- we should)
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(global-set-key (kbd "M-j") 'windmove-left)
(global-set-key (kbd "M-k") 'windmove-right)
(global-set-key (kbd "M-p") 'windmove-up)
(global-set-key (kbd "M-n") 'windmove-down)
(global-set-key (kbd "RET") 'newline-and-indent)

(provide 'init-local)

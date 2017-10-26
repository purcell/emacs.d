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
;; These two are actually incredibly useful in code. Consider another
;; set of keys to replicate
;;(global-set-key (kbd "M-p") 'windmove-up)
;;(global-set-key (kbd "M-n") 'windmove-down)
(global-set-key (kbd "RET") 'newline-and-indent)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

(setq compilation-finish-function
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "1 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "Good shit!")))))

(provide 'init-local)

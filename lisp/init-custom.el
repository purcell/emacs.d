;;; --------------------------------------------------
;;; Custom configure file
;;; --------------------------------------------------

;;不生成 #filename# 临时文件
(setq auto-save-default nil)

;; 一打开就起用 org-mode。
(setq default-major-mode 'org-mode)

;; 运行当前文件
(defun run-current-file ()
  (interactive)
  (let* (
         (file_name (buffer-file-name))
         (file_ext (file-name-extension file_name))
         (shell_cmd (cond ((string-equal file_ext "rb") "ruby")
                          ((string-equal file_ext "html") "google-chrome")
                          ((string-equal file_ext "coffee") "coffee --compile"))))
    (unless (eq shell_cmd nil)
      (progn
        (setq cmd_str (format "%s %s" shell_cmd file_name))
        (message cmd_str)
        (shell-command cmd_str)
        ))))
(global-set-key (kbd "C-c r") 'run-current-file)

(provide 'init-custom)

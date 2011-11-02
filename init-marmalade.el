;;; Handy code for uploading new versions of my own packages to marmalade

(autoload 'marmalade-upload-buffer "marmalade")

(defun latest-git-tag ()
  ;; TODO: sort versions properly
  (string-rtrim (shell-command-to-string "git tag|sort -n|tail -1")))

(defun update-version-header (val)
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "^;;;? ?Version:")
    (kill-line)
    (insert " " val)))

(defun submit-to-marmalade (buf)
  (interactive "bSubmit buffer: ")
  (with-current-buffer buf
    (let ((tag (latest-git-tag)))
      (unless tag
        (error "Not tagged"))
      (update-version-header tag)
      (marmalade-upload-buffer buf)
      (revert-buffer t t)
      (message "Submitted version %s to marmalade" tag))))


(provide 'init-marmalade)

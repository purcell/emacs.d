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

(defun submit-tar-to-marmalade (buf)
  (interactive "bSubmit buffer library as tar: ")
  (with-current-buffer buf
    (let* ((tag (or (latest-git-tag) (error "Not tagged")))
           (library-name (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
           (package-dir-name (concat library-name "-" tag))
           (temp-working-dir (make-temp-file "emacs-marmalade" t))
           (dest (expand-file-name package-dir-name temp-working-dir))
           (tar (concat dest ".tar")))
      (message "Building package in %s" dest)
      (make-directory dest)
      (shell-command (format "cp *.el %s && (cd %s && perl -spi -e 's/\\{\\{VERSION\\}\\}/%s/' *.el) && (cd %s && tar cvf %s %s)" dest dest tag temp-working-dir tar package-dir-name))
      (save-excursion
        (find-file tar)
        (marmalade-upload-buffer (current-buffer)))
;;      (delete-directory temp-working-dir t)
      )))

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

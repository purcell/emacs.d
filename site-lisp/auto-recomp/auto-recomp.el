(defun auto-recompile-file-maybe ()
  (when (null (bcc-in-blacklist buffer-file-name bcc-blacklist))
    (byte-compile-file buffer-file-name)))

(defun add-after-save-hook ()
  (add-hook 'after-save-hook 'auto-recompile-file-maybe))

(add-hook 'emacs-lisp-mode-hook 'add-after-save-hook)

(provide 'auto-recomp)

;;; auto-recomp.el ends here

;; add sunrise commander by lujianmei

(require 'sunrise-commander)

;; By default sr-browse-file (b) opens the item under the cursor in your
;; web-browser. You can redefine this function to open the file with the system
;; default application like this:
(defun sr-browse-file (&optional file)
  "Display the selected file with the default appication."
  (interactive)
  (setq file (or file (dired-get-filename)))
  (save-selected-window
    (sr-select-viewer-window)
    (let ((buff (current-buffer))
          (fname (if (file-directory-p file)
                     file
                   (file-name-nondirectory file)))
          (app (cond
                ((eq system-type 'darwin)	"open %s")
                ((eq system-type 'windows-nt)	"open %s")
                (t				"xdg-open %s"))))
      (start-process-shell-command "open" nil (format app file))
      (unless (eq buff (current-buffer))
        (sr-scrollable-viewer (current-buffer)))
      (message "Opening \"%s\" ..." fname))))

(provide 'init-sunrise)
;;; init.el ends here

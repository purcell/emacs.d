(require 'dired-details)
(dired-details-install)

(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
if no files marked, always operate on current line in dired-mode
"
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))

(eval-after-load 'dired
  '(progn
     ;; {dired-details
     (setq-default dired-details-hidden-string "")
     (define-key dired-mode-map "(" 'dired-details-toggle)
     (define-key dired-mode-map ")" 'dired-details-toggle)
     ;; }
     (define-key dired-mode-map "/" 'diredext-exec-git-command-in-shell)

     (require 'dired+)
     (setq dired-recursive-deletes 'top)
     (define-key dired-mode-map [mouse-2] 'dired-find-file)
     (dolist (file `(((if *unix* "zathura" "open") "pdf" "dvi" "pdf.gz" "ps" "eps")
                     ("unrar x" "rar")
                     ((if *unix* "mplayer -stop-xscreensaver" "mplayer")  "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv" "mp4" "m4v" "webm")
                     ("mplayer -playlist" "list" "pls")
                     ((if *unix* "feh" "open") "gif" "jpeg" "jpg" "tif" "png" )
                     ("7z x" "7z")
                     ("djview" "djvu")
                     ("firefox" "xml" "xhtml" "html" "htm" "mht")))
       (add-to-list 'dired-guess-shell-alist-default
                    (list (concat "\\." (regexp-opt (cdr file) t) "$")
                          (car file))))
     ))

(provide 'init-dired)

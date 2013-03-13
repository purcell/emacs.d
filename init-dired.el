(eval-after-load 'dired
  '(progn
     (require 'dired+)
     (setq dired-recursive-deletes 'top)
     (define-key dired-mode-map [mouse-2] 'dired-find-file)
     (dolist (file `(("zathura" "pdf" "dvi" "pdf.gz" "ps" "eps")
                     ("unrar x" "rar")
                     ("mplayer -stop-xscreensaver" "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv" "mp4")
                     ("mplayer -playlist" "list" "pls")
                     ("feh" "gif" "jpeg" "jpg" "tif" "png" )
                     ("display" "gif" "jpeg" "jpg" "tif" "png")
                     ("7z x" "7z")
                     ("djview" "djvu")
                     ("firefox" "xml" "xhtml" "html" "htm" "mht")))
       (add-to-list 'dired-guess-shell-alist-default
                    (list (concat "\\." (regexp-opt (cdr file) t) "$")
                          (car file))))
     ))

(provide 'init-dired)

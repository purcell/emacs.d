;; These are some lines to help compilation of gnuplot-mode proceed
;; with fewer warning messages
(setq load-path             (append (list ".") load-path)
      byte-compile-verbose  nil
      byte-compile-warnings nil)
(require 'font-lock)
(defun hilit-repaint-command (foo))

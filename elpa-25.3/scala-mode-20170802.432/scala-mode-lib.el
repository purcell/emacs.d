;;; scala-mode-lib.el - Major mode for editing scala, common functions
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(defvar scala-mode:debug-messages
  "If true, some debug messages may be printed printed."
  nil)

(defmacro scala-lib:column-after (&rest body)
  `(save-excursion
     ,@body
     (current-column)))

(defmacro scala-lib:point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))

(defun scala-lib:delete-trailing-whitespace ()
  (save-excursion
    (end-of-line)
    (skip-syntax-backward " ")
    (unless (bolp)
      (delete-char (- (line-end-position) (point))))))

(provide 'scala-mode-lib)

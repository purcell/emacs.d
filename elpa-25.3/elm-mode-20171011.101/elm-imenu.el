;;; elm-imenu.el --- imenu support for elm
;;; Commentary:
;;; Code:
(require 'imenu)

(defun elm-imenu-create-index ()
  "Create an imenu index for the current buffer."
  (save-excursion
    (imenu--generic-function
     '((nil "^type \\([A-Z][^ \n]+\\)" 1)
       (nil "^type alias \\([A-Z][^ \n]+\\)" 1)
       (nil "^\\([^ ]+\\) :" 1)))))


(provide 'elm-imenu)
;;; elm-imenu.el ends here

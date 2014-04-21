;;; Package -- Face settings or its utilites
;;; Commentary:
;;; Code:
;; {{@see http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun what-face (pos)
  "POS: Get face from currnet cursor position."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
;;}}

(provide 'init-face)
;;; init-face.el ends here

;; Load Perspective
(require 'perspective)

;; Enable perspective mode
(persp-mode t)

;; TODO: implement persp-last as before-advice on persp-switch (?)

(defmacro custom-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash)))
         (current-perspective persp-curr))
     (persp-switch ,name)
     (when initialize ,@body)
     (setq persp-last current-perspective)))

;; Jump to last perspective
(defun custom-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))

(define-key persp-mode-map (kbd "C-x p -") 'custom-persp-last)

(provide 'init-perspective)

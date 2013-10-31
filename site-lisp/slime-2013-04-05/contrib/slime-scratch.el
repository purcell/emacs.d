
(define-slime-contrib slime-scratch
  "Imitate Emacs' *scratch* buffer"
  (:authors "Helmut Eller  <heller@common-lisp.net>")
  (:license "GPL")
  (:on-load
   (def-slime-selector-method ?s "*slime-scratch* buffer."
     (slime-scratch-buffer))))


;;; Code

(defvar slime-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-map)
    map))

(defun slime-scratch ()
  (interactive)
  (slime-switch-to-scratch-buffer))

(defun slime-switch-to-scratch-buffer ()
  (set-buffer (slime-scratch-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(defvar slime-scratch-file nil)

(defun slime-scratch-buffer ()
  "Return the scratch buffer, create it if necessary."
  (or (get-buffer (slime-buffer-name :scratch))
      (with-current-buffer (if slime-scratch-file
                               (find-file slime-scratch-file)
                             (get-buffer-create (slime-buffer-name :scratch)))
        (rename-buffer (slime-buffer-name :scratch))
	(lisp-mode)
	(use-local-map slime-scratch-mode-map)
	(slime-mode t)
	(current-buffer))))

(slime-define-keys slime-scratch-mode-map
  ("\C-j" 'slime-eval-print-last-expression))

(provide 'slime-scratch)

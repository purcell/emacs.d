;;; slime-scratch.el --- imitate Emacs' *scratch* buffer
;;
;; Author: Helmut Eller  <heller@common-lisp.net>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path ".../slime/contrib")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-scratch)))
;;


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

(defun slime-scratch-buffer ()
  "Return the scratch buffer, create it if necessary."
  (or (get-buffer "*slime-scratch*")
      (with-current-buffer (get-buffer-create "*slime-scratch*")
	(lisp-mode)
	(use-local-map slime-scratch-mode-map)
	(slime-mode t)
	(current-buffer))))

(slime-define-keys slime-scratch-mode-map
  ("\C-j" 'slime-eval-print-last-expression))

(defun slime-scratch-init ()
  (def-slime-selector-method ?s
    "*slime-scratch* buffer."
    (slime-scratch-buffer)))

(provide 'slime-scratch)
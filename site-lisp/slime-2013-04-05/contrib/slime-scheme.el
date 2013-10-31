;;; slime-scheme.el --- Support Scheme programs running under Common Lisp
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;; 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-scheme)))
;;

(defun slime-scheme-mode-hook ()
  (slime-mode 1))

(defun slime-scheme-indentation-update (symbol indent packages)
  ;; Does the symbol have an indentation value that we set?
  (when (equal (get symbol 'scheme-indent-function)
	       (get symbol 'slime-scheme-indent))
    (put symbol 'slime-scheme-indent indent)
    (put symbol 'scheme-indent-function indent)))


;;; Initialization

(defun slime-scheme-init ()
  (add-hook 'scheme-mode-hook 'slime-scheme-mode-hook)
  (add-hook 'slime-indentation-update-hooks 'slime-scheme-indentation-update)
  (add-to-list 'slime-lisp-modes 'scheme-mode))

(defun slime-scheme-unload ()
  (remove-hook 'scheme-mode-hook 'slime-scheme-mode-hook)
  (remove-hook 'slime-indentation-update-hooks 'slime-scheme-indentation-update)
  (setq slime-lisp-modes (remove 'scheme-mode slime-lisp-modes)))

(provide 'slime-scheme)

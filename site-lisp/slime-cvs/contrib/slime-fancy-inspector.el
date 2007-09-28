;;; slime-fancy-inspector.el --- Fancy inspector for CLOS objects
;;
;; Author: Marco Baringer <mb@bese.it> and others
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-fancy-inspector)))
;;   (add-hook 'slime-connected-hook 'slime-install-fancy-inspector)

(defun slime-install-fancy-inspector ()
  (slime-eval-async '(swank:swank-require :swank-fancy-inspector)
		    (lambda (_) 
		      (slime-eval-async '(swank:fancy-inspector-init)))))

(defun slime-deinstall-fancy-inspector ()
  (slime-eval-async '(swank:fancy-inspector-unload)))

(defun slime-fancy-inspector-init ()
  (add-hook 'slime-connected-hook 'slime-install-fancy-inspector))

(defun slime-fancy-inspector-unload ()
  (remove-hook 'slime-connected-hook 'slime-install-fancy-inspector))

(provide 'slime-fancy-inspector)
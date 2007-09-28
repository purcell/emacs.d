;;; slime-tramp.el ---  Filename translations for tramp
;;
;; Authors: Marco Baringer <mb@bese.it>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path ".../slime/contrib")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-tramp)))
;;

(defun slime-make-tramp-file-name (username remote-host lisp-filename)
  "Old (with multi-hops) tramp compatability function"
  (require 'tramp)
  (if (boundp 'tramp-multi-methods)
      (tramp-make-tramp-file-name nil nil
                                  username
                                  remote-host
                                  lisp-filename)
      (tramp-make-tramp-file-name nil
                                  username
                                  remote-host
                                  lisp-filename)))

(defun* slime-create-filename-translator (&key machine-instance
                                         remote-host
                                         username)
  "Creates a three element list suitable for push'ing onto
slime-filename-translations which uses Tramp to load files on
hostname using username. MACHINE-INSTANCE is a required
parameter, REMOTE-HOST defaults to MACHINE-INSTANCE and USERNAME
defaults to (user-login-name).

MACHINE-INSTANCE is the value returned by slime-machine-instance,
which is just the value returned by cl:machine-instance on the
remote lisp. REMOTE-HOST is the fully qualified domain name (or
just the IP) of the remote machine. USERNAME is the username we
should login with.
The functions created here expect your tramp-default-method or
 tramp-default-method-alist to be setup correctly."
  (lexical-let ((remote-host (or remote-host machine-instance))
                (username (or username (user-login-name))))
    (list (concat "^" machine-instance "$")
          (lambda (emacs-filename)
            (tramp-file-name-localname
             (tramp-dissect-file-name emacs-filename)))
          `(lambda (lisp-filename)
            (slime-make-tramp-file-name
             ,username
             ,remote-host
             lisp-filename)))))

(provide 'slime-tramp)
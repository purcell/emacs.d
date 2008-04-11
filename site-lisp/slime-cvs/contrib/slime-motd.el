;;; slime-motd.el --- Message Of The Day in a slime repl
;;
;; Authors: Marco Baringer <mb@bese.it>
;;
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add slime-motd to your slime-setup call.

(require 'slime-banner)

(defcustom slime-motd-pathname nil
  "The local pathnamethe motd is read from."
  :group 'slime-mode
  :type '(file :must-match t))

(defun slime-insert-motd ()
  (slime-eval-async `(cl:progn
                      (swank:swank-require :swank-motd)
                      (swank::read-motd ,slime-motd-pathname))
                    (lambda (motd)
                      (when motd
                        (slime-repl-insert-result (list :values motd))))))

(defun slime-motd-init ()
  (add-hook 'slime-connected-hook 'slime-insert-motd))

(provide 'slime-motd)


;;; slime-motd.el --- 
;;
;; Authors: 
;;
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add slime-motd to your slime-setup call.

(define-slime-contrib slime-motd
  "Message Of The Day in a slime repl"
  (:authors "Marco Baringer <mb@bese.it>")
  (:license "GPL")
  (:slime-dependencies slime-banner)
  (:swank-dependencies swank-motd)
  (:on-load
   (add-hook 'slime-connected-hook 'slime-insert-motd)))

(defcustom slime-motd-pathname nil
  "The local pathname the motd is read from."
  :group 'slime-mode
  :type '(file :must-match t))

(defun slime-insert-motd ()
  (slime-eval-async `(swank::read-motd ,slime-motd-pathname)
                    (lambda (motd)
                      (when motd
                        (slime-repl-insert-result (list :values motd))))))

(provide 'slime-motd)

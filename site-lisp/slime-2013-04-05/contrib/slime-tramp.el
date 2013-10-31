
(define-slime-contrib slime-tramp
  "Filename translations for tramp"
  (:authors "Marco Baringer <mb@bese.it>")
  (:license "GPL")
  (:slime-dependencies tramp)
  (:on-load 
   (setq slime-to-lisp-filename-function #'slime-tramp-to-lisp-filename)
   (setq slime-from-lisp-filename-function #'slime-tramp-from-lisp-filename)))

(defcustom slime-filename-translations nil
  "Assoc list of hostnames and filename translation functions.  
Each element is of the form (HOSTNAME-REGEXP TO-LISP FROM-LISP).

HOSTNAME-REGEXP is a regexp which is applied to the connection's
slime-machine-instance. If HOSTNAME-REGEXP maches then the
corresponding TO-LISP and FROM-LISP functions will be used to
translate emacs filenames and lisp filenames.

TO-LISP will be passed the filename of an emacs buffer and must
return a string which the underlying lisp understandas as a
pathname. FROM-LISP will be passed a pathname as returned by the
underlying lisp and must return something that emacs will
understand as a filename (this string will be passed to
find-file).

This list will be traversed in order, so multiple matching
regexps are possible.

Example:

Assuming you run emacs locally and connect to slime running on
the machine 'soren' and you can connect with the username
'animaliter':

  (push (list \"^soren$\"
              (lambda (emacs-filename)
                (subseq emacs-filename (length \"/ssh:animaliter@soren:\")))
              (lambda (lisp-filename)
                (concat \"/ssh:animaliter@soren:\" lisp-filename)))
        slime-filename-translations)

See also `slime-create-filename-translator'."
  :type '(repeat (list :tag "Host description"
                       (regexp :tag "Hostname regexp")
                       (function :tag "To   lisp function")
                       (function :tag "From lisp function")))
  :group 'slime-lisp)

(defun slime-find-filename-translators (hostname)
  (cond ((cdr (assoc-if (lambda (regexp) (string-match regexp hostname))
                            slime-filename-translations)))
        (t (list #'identity #'identity))))

(defun slime-make-tramp-file-name (username remote-host lisp-filename)
  "Old (with multi-hops) tramp compatability function"
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

(defun slime-tramp-to-lisp-filename (filename)
  (funcall (if (slime-connected-p)
               (first (slime-find-filename-translators (slime-machine-instance)))
               'identity)
           (expand-file-name filename)))

(defun slime-tramp-from-lisp-filename (filename)
  (funcall (second (slime-find-filename-translators (slime-machine-instance)))
           filename))

(provide 'slime-tramp)

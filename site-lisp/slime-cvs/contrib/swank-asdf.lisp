;;; swank-asdf.el -- ASDF support
;;
;; Authors: Daniel Barlow  <dan@telent.net>
;;          Marco Baringer <mb@bese.it>
;;          Edi Weitz <edi@agharta.de>
;;          and others 
;; License: Public Domain
;;

(in-package :swank)

(defslimefun operate-on-system-for-emacs (system-name operation &rest keywords)
  "Compile and load SYSTEM using ASDF.
Record compiler notes signalled as `compiler-condition's."
  (swank-compiler 
   (lambda ()
     (apply #'operate-on-system system-name operation keywords))))

(defun operate-on-system (system-name operation-name &rest keyword-args)
  "Perform OPERATION-NAME on SYSTEM-NAME using ASDF.
The KEYWORD-ARGS are passed on to the operation.
Example:
\(operate-on-system \"SWANK\" \"COMPILE-OP\" :force t)"
  (with-compilation-hooks ()
    (let ((operation (find-symbol operation-name :asdf)))
      (when (null operation)
        (error "Couldn't find ASDF operation ~S" operation-name))
      (apply #'asdf:operate operation system-name keyword-args))))

(defun asdf-central-registry ()
  asdf:*central-registry*)

(defslimefun list-all-systems-in-central-registry ()
  "Returns a list of all systems in ASDF's central registry."
  (mapcar #'pathname-name
          (delete-duplicates
           (loop for dir in (asdf-central-registry)
                 for defaults = (eval dir)
                 when defaults
                   nconc (mapcar #'file-namestring
                                   (directory
                                     (make-pathname :defaults defaults
                                          :version :newest
                                          :type "asd"
                                          :name :wild
                                          :case :local))))
           :test #'string=)))

(defslimefun list-all-systems-known-to-asdf ()
  "Returns a list of all systems ASDF knows already."
  ;; ugh, yeah, it's unexported - but do we really expect this to
  ;; change anytime soon?
  (loop for name being the hash-keys of asdf::*defined-systems*
        collect name))

(defslimefun list-asdf-systems ()
  "Returns the systems in ASDF's central registry and those which ASDF
already knows."
  (nunion (list-all-systems-known-to-asdf)
          (list-all-systems-in-central-registry)
          :test #'string=))

(provide :swank-asdf)

;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-loader.lisp --- Compile and load the Slime backend.
;;;
;;; Created 2003, James Bielman <jamesjb@jamesjb.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;; If you want customize the source- or fasl-directory you can set
;; swank-loader:*source-directory* resp. swank-loader:*fasl-directory*
;; before loading this files. (you also need to create the
;; swank-loader package.)
;; E.g.:
;;
;;   (make-package :swank-loader)
;;   (defparameter swank-loader::*fasl-directory* "/tmp/fasl/")
;;   (load ".../swank-loader.lisp")


(cl:defpackage :swank-loader
  (:use :cl)
  (:export :load-swank
           :*source-directory*
           :*fasl-directory*))

(cl:in-package :swank-loader)

(defvar *source-directory*
  (make-pathname :name nil :type nil
                 :defaults (or *load-pathname* *default-pathname-defaults*))
  "The directory where to look for the source.")

(defparameter *sysdep-files*
  (append
   '("nregex")
   #+cmu '("swank-source-path-parser" "swank-source-file-cache" "swank-cmucl")
   #+scl '("swank-source-path-parser" "swank-source-file-cache" "swank-scl")
   #+sbcl '("swank-sbcl" "swank-source-path-parser"
            "swank-source-file-cache" "swank-gray")
   #+openmcl '("metering" "swank-openmcl" "swank-gray")
   #+lispworks '("swank-lispworks" "swank-gray")
   #+allegro '("swank-allegro" "swank-gray")
   #+clisp '("xref" "metering" "swank-clisp" "swank-gray")
   #+armedbear '("swank-abcl")
   #+cormanlisp '("swank-corman" "swank-gray")
   #+ecl '("swank-ecl" "swank-gray")
   ))

(defparameter *implementation-features*
  '(:allegro :lispworks :sbcl :openmcl :cmu :clisp :ccl :corman :cormanlisp
    :armedbear :gcl :ecl :scl))

(defparameter *os-features*
  '(:macosx :linux :windows :mswindows :win32 :solaris :darwin :sunos :hpux
    :unix))

(defparameter *architecture-features*
  '(:powerpc :ppc :x86 :x86-64 :amd64 :i686 :i586 :i486 :pc386 :iapx386
    :sparc64 :sparc :hppa64 :hppa))

(defun lisp-version-string ()
  #+cmu       (substitute-if #\_ (lambda (x) (find x " /"))
                             (lisp-implementation-version))
  #+scl       (lisp-implementation-version)
  #+sbcl      (lisp-implementation-version)
  #+ecl       (lisp-implementation-version)
  #+openmcl   (format nil "~d.~d"
                      ccl::*openmcl-major-version*
                      ccl::*openmcl-minor-version*)
  #+lispworks (lisp-implementation-version)
  #+allegro   (format nil
                      "~A~A~A"
                      excl::*common-lisp-version-number*
                      (if (eq 'h 'H) "A" "M")     ; ANSI vs MoDeRn
                      (if (member :64bit *features*) "-64bit" ""))
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+cormanlisp (lisp-implementation-version))

(defun unique-directory-name ()
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (flet ((first-of (features)
           (loop for f in features
                 when (find f *features*) return it))
         (maybe-warn (value fstring &rest args)
           (cond (value)
                 (t (apply #'warn fstring args)
                    "unknown"))))
    (let ((lisp (maybe-warn (first-of *implementation-features*)
                            "No implementation feature found in ~a."
                            *implementation-features*))
          (os   (maybe-warn (first-of *os-features*)
                            "No os feature found in ~a." *os-features*))
          (arch (maybe-warn (first-of *architecture-features*)
                            "No architecture feature found in ~a."
                            *architecture-features*))
          (version (maybe-warn (lisp-version-string)
                               "Don't know how to get Lisp ~
                                implementation version.")))
      (format nil "~(~@{~a~^-~}~)" lisp version os arch))))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun slime-version-string ()
  "Return a string identifying the SLIME version.
Return nil if nothing appropriate is available."
  (with-open-file (s (merge-pathnames "ChangeLog" *source-directory*)
                     :if-does-not-exist nil)
    (and s (symbol-name (read s)))))

(defun default-fasl-directory ()
  (merge-pathnames
   (make-pathname
    :directory `(:relative ".slime" "fasl"
                 ,@(if (slime-version-string) (list (slime-version-string)))
                 ,(unique-directory-name)))
   (user-homedir-pathname)))

(defun binary-pathname (source-pathname binary-directory)
  "Return the pathname where SOURCE-PATHNAME's binary should be compiled."
  (let ((cfp (compile-file-pathname source-pathname)))
    (merge-pathnames (make-pathname :name (pathname-name cfp)
                                    :type (pathname-type cfp))
                     binary-directory)))

(defun handle-loadtime-error (condition binary-pathname)
  (pprint-logical-block (*error-output* () :per-line-prefix ";; ")
    (format *error-output*
            "~%Error while loading: ~A~%Condition: ~A~%Aborting.~%"
            binary-pathname condition))
  (when (equal (directory-namestring binary-pathname)
               (directory-namestring (default-fasl-directory)))
    (ignore-errors (delete-file binary-pathname)))
  (abort))

(defun compile-files-if-needed-serially (files fasl-directory)
  "Compile each file in FILES if the source is newer than
its corresponding binary, or the file preceding it was
recompiled."
  (let ((needs-recompile nil))
    (dolist (source-pathname files)
      (let ((binary-pathname (binary-pathname source-pathname
                                              fasl-directory)))
        (handler-case
            (progn
              (when (or needs-recompile
                        (not (probe-file binary-pathname))
                        (file-newer-p source-pathname binary-pathname))
                ;; need a to recompile source-pathname, so we'll
                ;; need to recompile everything after this too.
                (setq needs-recompile t)
                (ensure-directories-exist binary-pathname)
                (compile-file source-pathname :output-file binary-pathname
                              :print nil
                              :verbose t))
              (load binary-pathname :verbose t))
          ;; Fail as early as possible
          (serious-condition (c)
            (handle-loadtime-error c binary-pathname)))))))

#+(or cormanlisp ecl)
(defun compile-files-if-needed-serially (files fasl-directory)
  "Corman Lisp and ECL have trouble with compiled files."
  (declare (ignore fasl-directory))
  (dolist (file files)
    (load file :verbose t)
    (force-output)))

(defun load-user-init-file ()
  "Load the user init file, return NIL if it does not exist."
  (load (merge-pathnames (user-homedir-pathname)
                         (make-pathname :name ".swank" :type "lisp"))
        :if-does-not-exist nil))

(defun load-site-init-file (directory)
  (load (make-pathname :name "site-init" :type "lisp"
                       :defaults directory)
        :if-does-not-exist nil))

(defun swank-source-files (source-directory)
  (mapcar (lambda (name)
            (make-pathname :name name :type "lisp"
                           :defaults source-directory))
          `("swank-backend" ,@*sysdep-files* "swank")))

(defvar *fasl-directory* (default-fasl-directory)
  "The directory where fasl files should be placed.")

(defun load-swank (&key
                   (source-directory *source-directory*)
                   (fasl-directory *fasl-directory*))
  (compile-files-if-needed-serially (swank-source-files source-directory)
                                    fasl-directory)
  (set (read-from-string "swank::*swank-wire-protocol-version*")
       (slime-version-string))
  (funcall (intern (string :warn-unimplemented-interfaces) :swank-backend))
  (load-site-init-file source-directory)
  (load-user-init-file)
  (funcall (intern (string :run-after-init-hook) :swank)))

(load-swank)

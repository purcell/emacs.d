;;; -*- lisp -*-

;; ASDF system definition for loading the Swank server independently
;; of Emacs.
;;
;; This is only useful if you want to start a Swank server in a Lisp
;; processes that doesn't run under Emacs. Lisp processes created by
;; `M-x slime' automatically start the server.

;; Usage:
;;
;;   (require :swank)
;;   (swank:create-swank-server PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Swank server is running on localhost:ACTUAL-PORT. You can
;; use `M-x slime-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

(defpackage :swank-loader
  (:use :cl))

(in-package :swank-loader)

(defclass swank-loader-file (asdf:cl-source-file) ())

;;;; after loading run init

(defmethod asdf:perform ((o asdf:load-op) (f swank-loader-file))
  (load (asdf::component-pathname f))
  (funcall (read-from-string "swank-loader::init")
           :reload (asdf::operation-forced o)
           :delete (asdf::operation-forced o)))

(asdf:defsystem :swank
  :default-component-class swank-loader-file
  :components ((:file "swank-loader")))

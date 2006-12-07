;;; This is the value for *swank-wire-protocol-version*. NB: This file
;;; will be loaded by BOTH emacs and lisp, so the syntax used must
;;; remain compatable between the two dialects. You can assume that
;;; cl:*package* will be bound to (find-package :SWANK).

(setf *swank-wire-protocol-version* 1)

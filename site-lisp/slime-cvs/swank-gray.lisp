;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-gray.lisp --- Gray stream based IO redirection.
;;;
;;; Created 2003
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(in-package :swank-backend)

(defclass slime-output-stream (fundamental-character-output-stream)
  ((output-fn :initarg :output-fn)
   (buffer :initform (make-string 8000))
   (fill-pointer :initform 0)
   (column :initform 0)
   (last-flush-time :initform (get-internal-real-time))
   (lock :initform (make-recursive-lock :name "buffer write lock"))))

(defmethod stream-write-char ((stream slime-output-stream) char)
  (call-with-recursive-lock-held
   (slot-value stream 'lock)
   (lambda ()
     (with-slots (buffer fill-pointer column) stream
       (setf (schar buffer fill-pointer) char)
       (incf fill-pointer)
       (incf column)
       (when (char= #\newline char)
         (setf column 0)
         (force-output stream))
       (when (= fill-pointer (length buffer))
         (finish-output stream)))))
  char)

(defmethod stream-line-column ((stream slime-output-stream))
  (call-with-recursive-lock-held
   (slot-value stream 'lock)
   (lambda ()
     (slot-value stream 'column))))

(defmethod stream-line-length ((stream slime-output-stream))
  75)

(defmethod stream-finish-output ((stream slime-output-stream))
  (call-with-recursive-lock-held
   (slot-value stream 'lock)
   (lambda ()
     (with-slots (buffer fill-pointer output-fn last-flush-time) stream
       (let ((end fill-pointer))
         (unless (zerop end)
           (funcall output-fn (subseq buffer 0 end))
           (setf fill-pointer 0)))
       (setf last-flush-time (get-internal-real-time)))))
  nil)

(defmethod stream-force-output ((stream slime-output-stream))
  (call-with-recursive-lock-held
   (slot-value stream 'lock)
   (lambda ()
     (with-slots (last-flush-time fill-pointer) stream
       (let ((now (get-internal-real-time)))
         (when (> (/ (- now last-flush-time)
                     (coerce internal-time-units-per-second 'double-float))
                  0.2)
           (finish-output stream))))))
  nil)

(defmethod stream-fresh-line ((stream slime-output-stream))
  (call-with-recursive-lock-held
   (slot-value stream 'lock)
   (lambda ()
     (with-slots (column) stream
       (cond ((zerop column) nil)
             (t (terpri stream) t))))))

(defclass slime-input-stream (fundamental-character-input-stream)
  ((output-stream :initarg :output-stream)
   (input-fn :initarg :input-fn)
   (buffer :initform "") (index :initform 0)
   (lock :initform (make-lock :name "buffer read lock"))))

(defmethod stream-read-char ((s slime-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index output-stream input-fn) s
       (when (= index (length buffer))
         (when output-stream
           (finish-output output-stream))
         (let ((string (funcall input-fn)))
           (cond ((zerop (length string))
                  (return-from stream-read-char :eof))
                 (t
                  (setf buffer string)
                  (setf index 0)))))
       (assert (plusp (length buffer)))
       (prog1 (aref buffer index) (incf index))))))

(defmethod stream-listen ((s slime-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (< index (length buffer))))))

(defmethod stream-unread-char ((s slime-input-stream) char)
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (decf index)
       (cond ((eql (aref buffer index) char)
              (setf (aref buffer index) char))
             (t
              (warn "stream-unread-char: ignoring ~S (expected ~S)"
                    char (aref buffer index)))))))
  nil)

(defmethod stream-clear-input ((s slime-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s 
       (setf buffer ""  
             index 0))))
  nil)

(defmethod stream-line-column ((s slime-input-stream))
  nil)

(defmethod stream-line-length ((s slime-input-stream))
  75)


;;; CLISP extensions

;; We have to define an additional method for the sake of the C
;; function listen_char (see src/stream.d), on which SYS::READ-FORM
;; depends.

;; We could make do with either of the two methods below.

(defmethod stream-read-char-no-hang ((s slime-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (when (< index (length buffer))
         (prog1 (aref buffer index) (incf index)))))))

;; This CLISP extension is what listen_char actually calls.  The
;; default method would call STREAM-READ-CHAR-NO-HANG, so it is a bit
;; more efficient to define it directly.

(defmethod stream-read-char-will-hang-p ((s slime-input-stream))
  (with-slots (buffer index) s
    (= index (length buffer))))


;;;
(defimplementation make-fn-streams (input-fn output-fn)
  (let* ((output (make-instance 'slime-output-stream 
                                :output-fn output-fn))
         (input  (make-instance 'slime-input-stream
                                :input-fn input-fn 
                                :output-stream output)))
    (values input output)))
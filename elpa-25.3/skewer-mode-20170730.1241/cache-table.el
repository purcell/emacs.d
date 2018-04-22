;;; cache-table.el --- a hash table with expiring entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <mosquitopsu@gmail.com>
;; Version: 1.0

;;; Commentary:

;; See the docstring of `cache-table-create'. There is no
;; `cache-table-put': use `setf' on `cache-table-get' instead.

;;; Code:

(require 'cl-lib)

(cl-defstruct (cache-table (:constructor cache-table--create))
  "A cache table with expiring entries."
  expire-time table)

(defun cache-table-create (expire-time &rest keyword-args)
  "Create a new cache-table with entries automatically removed
from the table after EXPIRE-TIME seconds. This function accepts
the same keyword arguments as `make-hash-table'. Entries are not
actually removed from the cache-table until an access is made to
the cache-table.

Use `cache-table-get' to get and put (via setf) entries."
  (cache-table--create :expire-time expire-time
                       :table (apply #'make-hash-table keyword-args)))

(defun cache-table-clear-expired (cache-table)
  "Remove all expired entries from CACHE-TABLE."
  (cl-loop with expire-time = (cache-table-expire-time cache-table)
           with table = (cache-table-table cache-table)
           with dead-time = (- (float-time) expire-time)
           for key being the hash-keys of table using (hash-value entry)
           for (time . value) = entry
           when (< time dead-time) do (remhash key table)))

(defun cache-table-get (key cache-table &optional default)
  "Access the value for KEY in CACHE-TABLE if it has not yet
expired. Behaves just like `gethash'."
  (cache-table-clear-expired cache-table)
  (cdr (gethash key (cache-table-table cache-table) (cons 0 default))))

(gv-define-setter cache-table-get (value key cache-table)
  "Put an entry in the hash table, like (setf (gethash key table) value)."
  `(progn
     (cache-table-clear-expired ,cache-table)
     (puthash ,key (cons (float-time) ,value)
              (cache-table-table ,cache-table))))

(defun cache-table-map (f cache-table)
  "Like `maphash', call F for all non-expired entries in CACHE-TABLE."
  (cache-table-clear-expired cache-table)
  (maphash (lambda (k v) (funcall f k (cdr v)))
           (cache-table-table cache-table)))

(defun cache-table-count (cache-table)
  "Like `hash-table-count', count the number of non-expired entries."
  (hash-table-count (cache-table-table cache-table)))

(provide 'cache-table)

;;; cache-table.el ends here

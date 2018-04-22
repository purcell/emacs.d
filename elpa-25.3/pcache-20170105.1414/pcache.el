;;; pcache.el --- persistent caching for Emacs.

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:
;; Package-Version: 20170105.1414
;; Version: 0.4.2
;; Package-Requires: ((eieio "1.3"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; pcache provides a persistent way of caching data, in a hashtable-like
;; structure. It relies on `eieio-persistent' in the backend, so that any
;; object that can be serialized by EIEIO can be stored with pcache.

;; pcache handles objects called "repositories" (`pcache-repository') and
;; "entries" (`pcache-entry'). Each repository is identified by a unique name,
;; that defines an entry in `pcache-directory'. Subdirectories are allowed, by
;; the use of a directory separator in the repository name.

;; Example:
;; (let ((repo (pcache-repository "plop")))
;;   (pcache-put repo 'foo 42) ; store value 42 with key 'foo
;;   (pcache-get repo 'foo) ; => 42
;; )

;; Keys can be pretty much any Lisp object, and are compared for equality using
;; `eql'

;; Optionally, cache entries can expire:
;; (let ((repo (pcache-repository "plop")))
;;   (pcache-put repo 'foo 42 1) ; store value 42 with key 'foo for 1 second
;;   (sleep-for 1)
;;   (pcache-get repo 'foo) ; => nil
;; )

;;; Code:

(require 'cl)
(require 'eieio)
(require 'eieio-base)

(defvar pcache-directory
  (let ((dir (concat user-emacs-directory "var/pcache/")))
    (make-directory dir t)
    dir))

(defvar *pcache-repositories* (make-hash-table :test 'equal))

(defconst pcache-default-save-delay 300)

(defconst pcache-internal-version-constant "0.4")

(defconst pcache-version-constant
  (format "%s/%s" emacs-version pcache-internal-version-constant))

(defclass pcache-repository (eieio-persistent eieio-named)
  ((version :initarg :version :initform nil)
   (version-constant :allocation :class)
   (entries :initarg :entries :initform (make-hash-table))
   (entry-cls :initarg :entry-cls :initform pcache-entry)
   (timestamp :initarg :timestamp :initform (float-time (current-time)))
   (save-delay :initarg :save-delay)))

(oset-default 'pcache-repository :save-delay pcache-default-save-delay)
(oset-default 'pcache-repository version-constant pcache-version-constant)

(defvar *pcache-repository-name* nil)

(defmethod constructor :static ((cache pcache-repository) &rest args)
  (let* ((newname (or (and (stringp (car args)) (car args))
		      (plist-get args :object-name)
		      *pcache-repository-name*
		      (symbol-name cache)))
	 (e (gethash newname *pcache-repositories*))
	 (path (concat pcache-directory newname)))
    (setq args (append args (list :object-name newname)))
    (or e
        (and (not (boundp 'pcache-avoid-recursion))
             (file-exists-p path)
             (condition-case nil
                 (let* ((pcache-avoid-recursion t)
			(*pcache-repository-name* newname)
                        (obj (eieio-persistent-read path 'pcache-repository t)))
                   (and (or (pcache-validate-repo obj)
                            (error "wrong version"))
                        (puthash newname obj *pcache-repositories*)
                        obj))
               (error nil)))
        (let ((obj (call-next-method))
              (dir (file-name-directory path)))
          (unless (file-exists-p dir)
            (make-directory dir t))
          (oset obj :file path)
          (oset obj :version (oref-default obj version-constant))
          (puthash newname obj *pcache-repositories*)
          obj))))

(defun pcache-hash-table-values (h)
  (let (values)
    (maphash (lambda (k v) (push v values)) h)
    values))

(defun pcache-validate-repo (cache)
  (and
   (equal (oref cache :version)
          (oref-default (object-class cache) version-constant))
   (hash-table-p (oref cache :entries))
   (every
    (lambda (entry)
      (and (object-of-class-p entry (oref cache :entry-cls))
           (or (null (oref entry :value-cls))
               (object-of-class-p
                (oref entry :value) (oref entry :value-cls)))))
    (pcache-hash-table-values (oref cache :entries)))))

(defclass pcache-entry ()
  ((timestamp :initarg :timestamp
              :initform (float-time (current-time)))
   (ttl :initarg :ttl :initform nil)
   (value :initarg :value :initform nil)
   (value-cls :initarg :value-cls :initform nil)))

(defmethod pcache-entry-valid-p ((entry pcache-entry))
  (let ((ttl (oref entry :ttl)))
    (or (null ttl)
        (let ((time (float-time (current-time))))
          (< time (+ ttl (oref entry :timestamp)))))))

(defmethod pcache-get ((cache pcache-repository) key &optional default)
  (let* ((table (oref cache :entries))
         (entry (gethash key table)))
    (if entry
        (if (pcache-entry-valid-p entry)
            (oref entry :value)
          (remhash key table)
          default)
      default)))

(defmethod pcache-has ((cache pcache-repository) key)
  (let* ((default (make-symbol ":nil"))
         (table (oref cache :entries))
         (entry (gethash key table default)))
    (if (eq entry default) nil
      (if (pcache-entry-valid-p entry)
          t nil))))

(defmethod pcache-put ((cache pcache-repository) key value &optional ttl)
  (let ((table (oref cache :entries))
        (entry (or (and (eieio-object-p value)
                        (object-of-class-p value 'pcache-entry)
                        value)
                   (make-instance
                    (oref cache :entry-cls)
                    :value value
                    :value-cls (and (object-p value) (object-class value))))))
    (when ttl
      (oset entry :ttl ttl))
    (prog1
        (puthash key entry table)
      (pcache-save cache))))

(defmethod pcache-invalidate ((cache pcache-repository) key)
  (let ((table (oref cache :entries)))
    (remhash key table)
    (pcache-save cache)))

(defmethod pcache-clear ((cache pcache-repository))
  (let* ((entries (oref cache :entries))
         (test (hash-table-test entries))
         (resize (hash-table-rehash-size entries))
         (threshold (hash-table-rehash-threshold entries))
         (weakness (hash-table-weakness entries)))
    (oset cache :entries (make-hash-table :test test :rehash-size resize
                                          :rehash-threshold threshold
                                          :weakness weakness)))
  (pcache-save cache))

(defmethod pcache-purge-invalid ((cache pcache-repository))
  (let ((table (oref cache :entries)))
    (maphash #'(lambda (k e)
                 (unless (pcache-entry-valid-p e)
                   (remhash k table)))
             table)
    (pcache-save cache)))

(defmethod pcache-save ((cache pcache-repository) &optional force)
  (let ((timestamp (oref cache :timestamp))
        (delay (oref cache :save-delay))
        (time (float-time (current-time))))
    (when (or force (> time (+ timestamp delay)))
      (oset cache :timestamp time)
      ;; make sure version is saved to file
      (oset cache :version (oref-default (object-class cache) version-constant))
      (eieio-persistent-save cache))))

(defmethod pcache-map ((cache pcache-repository) func)
  (let ((table (oref cache :entries)))
    (maphash func table)))

(defun pcache-kill-emacs-hook ()
  (maphash #'(lambda (k v)
               (condition-case nil
                   (pcache-purge-invalid v)
                 (error nil))
	       (condition-case nil
		   (pcache-save v t)
		 (error nil)))
           *pcache-repositories*))

(defun pcache-destroy-repository (name)
  (remhash name *pcache-repositories*)
  (let ((fname (concat pcache-directory name)))
    (when (file-exists-p fname)
      (delete-file fname))))

(add-hook 'kill-emacs-hook 'pcache-kill-emacs-hook)

;; in case we reload in place, clean all repositories with invalid version
(let (to-clean)
  (maphash #'(lambda (k v)
               (condition-case nil
                   (unless (eql (oref v :version)
                                pcache-version-constant)
                     (signal 'error nil))
                 (error
                  (setq to-clean (cons k to-clean)))))
           *pcache-repositories*)
  (dolist (k to-clean)
    (remhash k *pcache-repositories*)))

(provide 'pcache)
;;; pcache.el ends here

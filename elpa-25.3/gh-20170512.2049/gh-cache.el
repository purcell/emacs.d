;;; gh-cache.el --- caching for gh.el

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

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

;;

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload
(require 'eieio)

(require 'pcache)

(defconst gh-cache-outdated-expiration-delay (* 60 60 24))

(defconst gh-cache-internal-version-constant 4)

(defconst gh-cache-version-constant
  (format "%s/gh-%s" pcache-version-constant gh-cache-internal-version-constant))

;;;###autoload
(defclass gh-cache (pcache-repository)
  ((version-constant :allocation :class)
   (entries :initarg :entries :initform (make-hash-table :test 'equal))
   (safe-methods :allocation :class :initform ("HEAD" "GET" "OPTIONS" "TRACE"))
   (invalidation-chain :allocation :class :initform nil)

   (entry-cls :initarg :entry-cls :initform gh-cache-entry)))

(oset-default 'gh-cache version-constant gh-cache-version-constant)

;;;###autoload
(defclass gh-cache-entry (pcache-entry)
  ((etag :initarg :etag :initform nil)
   (outdated :initarg :outdated :initform nil)
   ;; (ttl :initarg :ttl :initform 0)
   ))

(defmethod pcache-invalidate :after ((cache gh-cache) key)
  (let ((resource (car key)))
    (pcache-map cache #'(lambda (k v)
                          (when (equal (car k) resource)
                            (pcache-invalidate cache k))))
    (dolist (next (oref cache invalidation-chain))
      (let ((nextresource
             (replace-regexp-in-string (car next) (cdr next) resource)))
        (when (not (equal nextresource resource))
          (pcache-map cache #'(lambda (k v)
                                (when (equal (car k) nextresource)
                                  (pcache-invalidate cache k)))))))))

(defmethod pcache-get ((cache gh-cache) key &optional default)
  (let* ((table (oref cache :entries))
         (entry (gethash key table)))
    (if (not entry)
        default
      (unless (pcache-entry-valid-p entry)
        (oset entry :outdated t))
      (oref entry :value))))

(defmethod pcache-has ((cache pcache-repository) key)
  (let* ((default (make-symbol ":nil"))
         (table (oref cache :entries))
         (entry (gethash key table default)))
    (not (eq entry default))))

(defmethod pcache-purge-invalid ((cache gh-cache))
  (let ((table (oref cache :entries)))
    (maphash #'(lambda (k e)
                 (unless (gh-cache-expired-p e)
                   (remhash k table)))
             table)
    (pcache-save cache)))

(defmethod gh-cache-outdated-p ((cache gh-cache) key)
  (let* ((table (oref cache :entries))
         (entry (gethash key table)))
    (and entry
         (oref entry :outdated))))

(defmethod gh-cache-expired-p ((cache gh-cache) key)
  (let* ((table (oref cache :entries))
         (entry (gethash key table)))
    (and (gh-cache-outdated-p cache key)
         (not
          (let ((time (float-time (current-time))))
            (< time (+ gh-cache-outdated-expiration-delay
                       (oref entry :timestamp))))))))

(defmethod gh-cache-revive ((cache gh-cache) key)
  (let* ((table (oref cache :entries))
         (entry (gethash key table)))
    (and entry
         (oset entry :outdated nil)
         (oset entry :timestamp (float-time (current-time)))
         t)))

(defmethod gh-cache-etag ((cache gh-cache) key)
  (let* ((table (oref cache :entries))
         (entry (gethash key table)))
    (and entry
         (oref entry :etag))))

(defmethod gh-cache-set-etag ((cache gh-cache) key etag)
  (let* ((table (oref cache :entries))
         (entry (gethash key table)))
    (and entry
         (oset entry :etag etag))))

(provide 'gh-cache)
;;; gh-cache.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

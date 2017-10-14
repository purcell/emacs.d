;;; gh-common.el --- common objects for gh.el

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

(require 'dash)
(require 'marshal)
(require 's)
(require 'gh-profile)

(defgroup gh nil
  "Github API client libraries."
  :group 'applications)

(defcustom gh-use-local-git-config nil
  "If `t' use git configuration from the machine running
Emacs. This makes a difference when running with TRAMP."
  :type 'boolean
  :group 'gh)

;;; Helper functions

(defun gh-read (obj field)
  (cdr (assoc field obj)))

(defun gh-namespaced-key (key)
  (let ((profile (gh-profile-current-profile)))
    (concat "github."
            (if (string= profile gh-profile-default-profile)
                ""
              (concat profile "."))
            key)))

(defun gh-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((strip (lambda (string)
                 (if (> (length string) 0)
                     (substring string 0 (- (length string) 1))))))
    (funcall strip (gh-command-to-string "config" (gh-namespaced-key key)))))

(defun gh-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (gh-command-to-string "config" "--global" (gh-namespaced-key key) value))

(defun gh-command-to-string (&rest args)
  (let ((git (executable-find "git"))
        (runner (if gh-use-local-git-config
                    'call-process
                  'process-file)))
    (with-output-to-string
      (apply runner git nil standard-output nil args))))

;;; Base classes for common objects

;;;###autoload
(defun gh-marshal-default-spec (slot)
  (let ((slot-name (symbol-name slot)))
    (list (cons 'alist
                (intern (s-replace "-" "_" slot-name))))))

;;;###autoload
(defmacro gh-defclass (name superclass slots &rest options-and-doc)
  `(marshal-defclass ,name ,superclass ,slots ,@options-and-doc
                     :marshal-default-spec gh-marshal-default-spec))

;;;###autoload
(gh-defclass gh-object ()
  ())

(defmethod gh-object-read :static ((obj gh-object) data)
  (let ((target (if (object-p obj) obj
                    (make-instance obj))))
    (when data
      (gh-object-read-into target data))
    target))

(defmethod gh-object-reader :static ((obj gh-object))
  (apply-partially 'gh-object-read obj))

(defmethod gh-object-list-read :static ((obj gh-object) data)
  (mapcar (gh-object-reader obj) data))

(defmethod gh-object-list-reader :static ((obj gh-object))
  (apply-partially 'gh-object-list-read obj))

(defmethod gh-object-read-into ((obj gh-object) data)
  (unmarshal obj data 'alist))

(defmethod slot-unbound ((obj gh-object) cls slot-name fn)
  (if (eq fn 'oref) nil
      (call-next-method)))

;;;###autoload
(gh-defclass gh-ref-object (gh-object)
  ((id :initarg :id)
   (url :initarg :url)
   (html-url :initarg :html-url)))

(defmethod gh-ref-object-base ((obj gh-ref-object))
  (let ((url (oref obj :url)))
    (--> (s-split "/" url t)
      (-slice it 2)
      (s-join "/" it)
      (concat "/" it))))

(defmethod gh-ref-object-base (obj)
  (if (stringp obj) obj
    (error "illegal input for `gh-ref-object-base'")))

;;;###autoload
(gh-defclass gh-user (gh-ref-object)
  ((login :initarg :login)
   (gravatar-url :initarg :gravatar-url))
  "Github user object")

;;;###autoload
(gh-defclass gh-comment (gh-ref-object)
  ((body :initarg :body)
   (user :initarg :user :initform nil :marshal-type gh-user)
   (created-at :initarg :created_at)
   (updated-at :initarg :updated_at))
  "Github comment object")

(defmethod gh-comment-req-to-update ((req gh-comment))
  `(("body" . ,(oref req :body))))

(provide 'gh-common)
;;; gh-common.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

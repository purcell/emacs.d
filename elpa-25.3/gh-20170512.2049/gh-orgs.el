;;; gh-org.el --- orgs module for gh.el

;; Copyright (C) 2012  Yann Hodique

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

(require 'gh-api)
(require 'gh-auth)
(require 'gh-common)

;;;###autoload
(defclass gh-orgs-api (gh-api-v3)
  ((org-cls :allocation :class :initform gh-orgs-org))
  "Orgs API")

;;;###autoload
(gh-defclass gh-orgs-org-stub (gh-ref-object)
  ((login :initarg :login)
   (avatar-url :initarg :avatar-url)
   (description :initarg :description)))

;;;###autoload
(gh-defclass gh-orgs-plan (gh-object)
  ((name :initarg :name)
   (space :initarg :space)
   (private-repos :initarg :private-repos)))

;;;###autoload
(gh-defclass gh-orgs-org (gh-orgs-org-stub)
  ((name :initarg :name)
   (company :initarg :company)
   (blog :initarg :blog)
   (location :initarg :location)
   (email :initarg :email)
   (public-repos :initarg :public-repos)
   (public-gists :initarg :public-gists)
   (followers :initarg :followers)
   (following :initarg :following)
   (created-at :initarg :created-at)
   (type :initarg :type)
   (total-private-repos :initarg :total-private-repos)
   (owned-private-repos :initarg :owned-private-repos)
   (private-gists :initarg :private-gists)
   (disk-usage :initarg :disk-usage)
   (collaborators :initarg :collaborators)
   (billing-email :initarg :billing-email)
   (plan :initarg :plan :initform nil :marshal-type gh-orgs-plan))
  "Class for GitHub organizations")

(defmethod gh-orgs-org-to-obj ((org gh-orgs-org))
  `(,@(when (slot-boundp org :billing-email)
        (list (cons "billing_email" (oref org :billing-email))))
    ,@(when (slot-boundp org :blog)
        (list (cons "blog" (oref org :blog))))
    ,@(when (slot-boundp org :company)
        (list (cons "company" (oref org :company))))
    ,@(when (slot-boundp org :email)
        (list (cons "email" (oref org :email))))
    ,@(when (slot-boundp org :location)
        (list (cons "location" (oref org :location))))
    ,@(when (slot-boundp org :name)
        (list (cons "name" (oref org :name))))))

(defmethod gh-orgs-list ((api gh-orgs-api) &optional username)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api org-cls)) "GET"
   (format "/users/%s/orgs" (or username (gh-api-get-username api)))))

(defmethod gh-orgs-get ((api gh-orgs-api) org)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api org-cls)) "GET"
   (format "/orgs/%s" org)))

(defmethod gh-orgs-update ((api gh-orgs-api) org-obj)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api org-cls)) "PATCH"
   (format "/orgs/%s" (oref org-obj :login))
   (apply 'gh-orgs-org-to-obj org-obj nil)))

(provide 'gh-orgs)
;;; gh-org.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

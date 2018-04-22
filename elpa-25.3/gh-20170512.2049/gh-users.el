;;; gh-users.el --- users module for gh.el

;; Copyright (C) 2013  Yann Hodique

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
(defclass gh-users-api (gh-api-v3)
  ((users-cls :allocation :class :initform gh-users-user))
  "Users API")

;;;###autoload
(gh-defclass gh-users-user (gh-user)
  ((gravatar-id :initarg :gravatar-id)
   (html-url :initarg :html-url)
   (followers-url :initarg :followers-url)
   (following-url :initarg :following-url)
   (gists-url :initarg :gists-url)
   (starred-url :initarg :starred-url)
   (subscriptions-url :initarg :subscriptions-url)
   (organizations-url :initarg :organizations-url)
   (repos-url :initarg :repos-url)
   (events-url :initarg :events-url)
   (received-events-url :initarg :received-events-url)
   (type :initarg :type)
   (site-admin :initarg :site-admin)
   (name :initarg :name)
   (company :initarg :company)
   (blog :initarg :blog)
   (location :initarg :location)
   (email :initarg :email)
   (hireable :initarg :hireable)
   (bio :initarg :bio)
   (public-repos :initarg :public-repos)
   (public-gists :initarg :public-gists)
   (followers :initarg :followers)
   (following :initarg :following)
   (created-at :initarg :created-at)
   (update-at :initarg :update-at)))

(defmethod gh-users-get ((api gh-users-api) &optional username)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api users-cls)) "GET"
   (if username
       (format "/users/%s" username)
     "/user")))

(defmethod gh-users-list ((api gh-users-api))
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api users-cls)) "GET"
   "/users"))

(provide 'gh-users)
;;; gh-users.el ends here

;;; gh-oauth.el --- oauth module for gh.el

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
(defclass gh-oauth-api (gh-api-v3)
  ((auth-cls :allocation :class :initform gh-oauth-authorization))
  "OAuth API")

;;;###autoload
(defclass gh-oauth-password-authenticator (gh-password-authenticator)
  ((remember :allocation :class :initform nil)))

(defmethod initialize-instance ((api gh-oauth-api) &rest args)
  ;; force password authentication for this API
  (let ((gh-api-v3-authenticator 'gh-oauth-password-authenticator))
    (call-next-method)))

;;;###autoload
(gh-defclass gh-oauth-authorization (gh-ref-object)
  ((scopes :initarg :scopes)
   (token :initarg :token)
   (app :initarg :app :initform nil :marshal-type gh-oauth-app)
   (updated-at :initarg :updated-at)
   (created-at :initarg :created-at)))

;;;###autoload
(gh-defclass gh-oauth-app (gh-object)
  ((url :initarg :url)
   (name :initarg :name)))

(defmethod gh-oauth-auth-list ((api gh-oauth-api))
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api auth-cls)) "GET"
   (format "/authorizations")))

(defmethod gh-oauth-auth-get ((api gh-oauth-api) id)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api auth-cls)) "GET"
   (format "/authorizations/%s" id)))

(defmethod gh-oauth-auth-new ((api gh-oauth-api) &optional scopes)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api auth-cls)) "POST"
   (format "/authorizations") (list (cons 'scopes scopes)
                                    (cons 'note (format "gh.el - %s"
                                                        (system-name))))))

(defmethod gh-oauth-auth-update ((api gh-oauth-api) id &optional scopes)
  (gh-api-authenticated-request
   api (gh-object-reader (oref api auth-cls)) "PATCH"
   (format "/authorizations/%s" id) (list (cons 'scopes scopes))))

(defmethod gh-oauth-auth-delete ((api gh-oauth-api) id)
  (gh-api-authenticated-request
   api nil "DELETE" (format "/authorizations/%s" id)))

(provide 'gh-oauth)
;;; gh-oauth.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

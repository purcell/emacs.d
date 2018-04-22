;;; gh-auth.el --- authentication for gh.el

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

(require 'gh-profile)
(require 'gh-common)
(require 'gh-url)

(defgroup gh-auth nil
  "Github authentication."
  :group 'gh)

(defvar gh-auth-alist nil)

(defun gh-auth-remember (profile key value)
  (let ((cell (assoc profile gh-auth-alist)))
    (when (not cell)
      (setq cell (cons profile nil))
      (setq gh-auth-alist (append gh-auth-alist (list cell))))
    (setcdr cell (plist-put (cdr cell) key value))))

(defun gh-auth-get-username ()
  (let* ((profile (gh-profile-current-profile))
         (user (or (plist-get (cdr (assoc profile gh-auth-alist)) :username)
                   (plist-get (cdr (assoc profile gh-profile-alist)) :username)
                   (gh-config "user"))))
    (when (not user)
      (setq user (read-string "GitHub username: "))
      (gh-set-config "user" user))
    (gh-auth-remember profile :username user)
    user))

(defun gh-auth-get-password (&optional remember)
  (let* ((profile (gh-profile-current-profile))
         (pass (or (plist-get (cdr (assoc profile gh-auth-alist)) :password)
                   (plist-get (cdr (assoc profile gh-profile-alist)) :password)
                   (gh-config "password"))))
    (when (not pass)
      (setq pass (read-passwd "GitHub password: "))
      (when remember
        (gh-set-config "password" pass)))
    (when remember
      (gh-auth-remember profile :password pass))
    pass))

(declare-function 'gh-oauth-auth-new "gh-oauth")

(defun gh-auth-get-oauth-token ()
  (let* ((profile (gh-profile-current-profile))
         (token (or (plist-get (cdr (assoc profile gh-auth-alist)) :token)
                    (plist-get (cdr (assoc profile gh-profile-alist)) :token)
                    (gh-config "oauth-token"))))
    (when (not token)
      (let* ((api (make-instance 'gh-oauth-api))
             (tok (and (fboundp 'gh-oauth-auth-new)
                       (oref (oref (funcall 'gh-oauth-auth-new api
                                            '(user repo gist)) :data)
                             :token))))
        (setq token (or tok (read-string "GitHub OAuth token: ")))
        (gh-set-config "oauth-token" token)))
    (gh-auth-remember profile :token token)
    token))

;;;###autoload
(defclass gh-authenticator ()
  ((username :initarg :username :initform nil))
  "Abstract authenticator")

(defmethod initialize-instance ((auth gh-authenticator) &rest args)
  (call-next-method)
  (or (oref auth :username)
      (oset auth :username (gh-auth-get-username))))

(defmethod gh-auth-modify-request ((auth gh-authenticator) req)
  req)

;;;###autoload
(defclass gh-auth-2fa-callback (gh-url-callback)
  ((req :initarg :req :initform nil))
  "2-factor callback")

(defmethod gh-url-callback-run ((cb gh-auth-2fa-callback) resp)
  (when (equal (oref resp :http-status) 401)
    (let* ((otp-header "X-GitHub-OTP")
           (h (assoc otp-header (oref resp :headers))))
      (when (and h (string-prefix-p "required;" (cdr h)))
        (let ((otp (read-from-minibuffer "Enter dual-factor auth code: "))
              (req (oref cb :req)))
          ;; reset resp
          (oset resp :data nil)
          (oset resp :data-received nil)

          (object-add-to-list req :headers
                              (cons otp-header otp))
          (gh-url-run-request req resp))))))

;;;###autoload
(defclass gh-password-authenticator (gh-authenticator)
  ((password :initarg :password :protection :private :initform nil)
   (remember :allocation :class :initform t)

   (2fa-cls :initform gh-auth-2fa-callback :allocation :class))
  "Password-based authenticator")

(defmethod initialize-instance ((auth gh-password-authenticator) &rest args)
  (call-next-method)
  (or (oref auth :password)
      (oset auth :password (gh-auth-get-password (oref auth remember)))))

(defmethod gh-auth-modify-request ((auth gh-password-authenticator) req)
  (object-add-to-list req :headers
                      (cons "Authorization"
                            (concat "Basic "
                                    (base64-encode-string
                                     (format "%s:%s" (oref auth :username)
                                             (encode-coding-string
                                              (oref auth :password) 'utf-8))))))
  (object-add-to-list req :install-callbacks
                      (make-instance (oref auth 2fa-cls) :req req))
  req)

;;;###autoload
(defclass gh-oauth-authenticator (gh-authenticator)
  ((token :initarg :token :protection :private :initform nil))
  "Oauth-based authenticator")

(defmethod initialize-instance ((auth gh-oauth-authenticator) &rest args)
  (call-next-method)
  (or (oref auth :token)
      (oset auth :token (gh-auth-get-oauth-token))))

(defmethod gh-auth-modify-request ((auth gh-oauth-authenticator) req)
  (object-add-to-list req :headers
                      (cons "Authorization"
                            (encode-coding-string
                             (format "token %s" (oref auth :token)) 'utf-8)))
  req)

(provide 'gh-auth)
;; to avoid circular dependencies...
(require 'gh-oauth)
;;; gh-auth.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

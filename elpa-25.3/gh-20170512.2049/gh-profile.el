;;; gh-profile.el --- profile support for gh.el

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

(require 'rx)
(require 'url-parse)

(defgroup gh-profile nil
  "Github profile."
  :group 'gh)

(defun gh-profile-remote-regexp (domain)
  (eval
   `(rx bol (or ,(concat  "git@" domain ":")
                (and (or "git" "ssh" "http" "https") "://"
                     (* nonl) (? "@") ,domain "/"))
        (and (group (* nonl)) "/" (group (* nonl))) (? ".git"))))

(defcustom gh-profile-alist `(("github"
                               :url "https://api.github.com"
                               :remote-regexp
                               ,(gh-profile-remote-regexp "github.com")))
  "List of profiles for Github access. List every Github
Enterprise server and/or Github accounts you have access
to here."
  :type '(alist :key-type string
                :value-type (plist :key-type (choice (const :url)
                                                     (const :username)
                                                     (const :password)
                                                     (const :token)
                                                     (const :remote-regexp))
                                   :value-type string))
  :group 'gh-profile)

(defun gh-profile-get-remote-regexp (profile)
  (let* ((profile-plist (cdr (assoc profile gh-profile-alist)))
         (regexp (plist-get profile-plist :remote-regexp)))
    (if regexp
        regexp
      ;; try to guess remote format (just use the hostname)
      (let* ((url (url-generic-parse-url (plist-get profile-plist :url)))
             (host (url-host url)))
        (gh-profile-remote-regexp host)))))

(defcustom gh-profile-default-profile "github"
  "Default profile. This needs to be a key present in
  `gh-profile-alist'"
  :type 'string
  :group 'gh-profile)

(defvar gh-profile-current-profile nil)
(make-variable-buffer-local 'gh-profile-current-profile)

(defun gh-profile-current-profile ()
  (or gh-profile-current-profile
      gh-profile-default-profile))

(defun gh-profile-url ()
  (plist-get (cdr (assoc (or gh-profile-current-profile
                             gh-profile-default-profile)
                         gh-profile-alist)) :url))

(defun gh-profile-completing-read ()
  (let ((profiles (mapcar #'car gh-profile-alist)))
    (if (> (length profiles) 1)
        (completing-read "Github profile: " profiles nil t nil nil (first profiles))
      (car profiles))))

(defun gh-profile-get-remote-profile (remote-url)
  (loop for (id . props) in gh-profile-alist
        if (string-match (gh-profile-get-remote-regexp id)
                         remote-url)
        return id))

(provide 'gh-profile)
;;; gh-profile.el ends here

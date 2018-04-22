;;; gntp.el --- Growl Notification Protocol for Emacs -*- lexical-binding: t -*-

;; Author: Engelke Eschner <tekai@gmx.li>
;; Version: 0.1
;; Package-Version: 20141024.1950
;; Created: 2013-03-21

;; LICENSE
;; Copyright (c) 2013 Engelke Eschner
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.
;;     * Neither the name of the gntp.el nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT
;; HOLDER> BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
;; OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Commentary:
;; This package implements the Growl Notification Protocol GNTP
;; described at http://www.growlforwindows.com/gfw/help/gntp.aspx
;; It is incomplete as it only lets you send but not receive
;; notifications.

;;; Code:

(defgroup gntp nil
  "GNTP, send/register growl notifications via GNTP from within emacs."
  :group 'external)

(defcustom gntp-application-name "Emacs/gntp.el"
  "Name of the application gntp registers itself."
  :type '(string))

(defcustom gntp-application-icon nil
  "Icon to display as the application icon.
Either a URL or a path to a file."
  :type '(string))

(defcustom gntp-server "localhost"
  "Default port of the server.
Standard says can't be changed, but port-forwarding etc."
  :type '(string))

(defcustom gntp-server-port 23053
  "Default port of the server.
Standard says can't be changed, but port-forwarding etc."
  :type '(integer))

(defcustom gntp-register-alist nil
  "Registration item list."
  :type '(choice string (const nil)))

(defun gntp-register (&optional notifications server  port)
  (interactive)
  "Register NOTIFICATIONS at SERVER:PORT.
PORT defaults to `gntp-server-port'."
  (let ((message (gntp-build-message-register (if notifications notifications gntp-register-alist))))
    (gntp-send message (if server server gntp-server) port)))

;;;###autoload
(defun gntp-notify (name title text server &optional port priority icon)
  "Send notification NAME with TITLE, TEXT, PRIORITY and ICON to SERVER:PORT.
PORT defaults to `gntp-server-port'"
  (let ((message (gntp-build-message-notify name title text priority icon)))
    (gntp-send message server port)))

(defun gntp-build-message-register (notifications)
  "Build the message to register NOTIFICATIONS types."
  (let ((lines (list "GNTP/1.0 REGISTER NONE"
                     (format "Application-Name: %s"
                             gntp-application-name)
                     (format "Notifications-Count: %d"
                             (length notifications))))
        (icon-uri (gntp-app-icon-uri))
        (icon-data (gntp-app-icon-data))
        (icons (list)))

    ;; append icon uri
    (when icon-uri
      (nconc lines (list (format "Application-Icon: %s" icon-uri)))
      ;; and data when it exists
      (when icon-data
        (setq icons (cons icon-data icons))))

    (dolist (notice notifications)
      ;; "For each notification being registered:
      ;; Each notification being registered should be seperated by a
      ;; blank line, including the first notification
      (nconc lines (cons "" (gntp-notification-lines notice)))
      ;; c
      (let ((icon (gntp-notice-icon-data notice)))
        (when icon
          (nconc icons (list "" icon)))))

    ;; icon data must come last
    (when icons
      (nconc lines (cons "" icons)))

    (mapconcat 'identity (remove nil lines) "\r\n")))

(defun gntp-notification-lines (notice)
  "Transform NOTICE into a list of strings."
  (let ((display-name (gntp-notice-get notice :display))
        (enabled (gntp-notice-get notice :enabled))
        (icon-uri (gntp-notice-icon-uri notice)))
  (list
   ;; Required - The name (type) of the notification being registered
   (concat "Notification-Name: " (gntp-notice-name notice))
   ;; Optional - The name of the notification that is displayed to
   ;; the user (defaults to the same value as Notification-Name)
   (when display-name
     (concat "Notification-Display-Name: " display-name))
   ;; Optional - Indicates if the notification should be enabled by
   ;; default (defaults to False)
   (when enabled
     "Notification-Enabled: True")
   ;; Optional - The default icon to use for notifications of this type
   (when icon-uri
     (concat "Notification-Icon: " icon-uri)))))

(defun gntp-build-message-notify (name title text &optional priority icon)
  "Build a message of type NAME with TITLE and TEXT."

  (format
   "GNTP/1.0 NOTIFY NONE\r\n\
Application-Name: %s\r\n\
Notification-Name: %s\r\n\
Notification-Title: %s\r\n\
Notification-Text: %s\r\n\
Notification-Priority: %s\r\n\
Notification-Icon: %s\r\n\
\r\n"
          gntp-application-name
          (if (symbolp name) (symbol-name name) name)
          title
          ;; no CRLF in the text to avoid accidentel msg end
          (replace-regexp-in-string "\r\n" "\n" text)
          (if priority priority "0")
          (if icon (gntp-icon-uri icon) "")))

;; notice
;;(list name ; everthing else is optional
;;      :display "name to display"
;;      :enabled nil
;;      :icon "url or file")


(defun gntp-notice-icon-uri (notice)
  "Get the icon URI from NOTICE."
  (gntp-icon-uri (gntp-notice-get notice :icon)))

(defun gntp-notice-icon-data (notice)
  "Get icon data from NOTICE."
  (gntp-icon-data (gntp-notice-get notice :icon)))

(defun gntp-app-icon-uri ()
  "Return the value to be used in the Application-Icon header."
  (gntp-icon-uri gntp-application-icon))

(defun gntp-app-icon-data ()
  "Return the value to be used in the Application-Icon header."
  (gntp-icon-data gntp-application-icon))

(defun gntp-icon-uri (icon)
  "Get the URI of ICON."
  (when icon
    (cond ((string-equal (substring icon 0 7) "http://") icon)
          ((and (file-exists-p icon) (file-readable-p icon))
           (concat "x-growl-resource://" (md5 icon))))))

(defun gntp-icon-data (icon)
  "Get the URI of ICON."
  (when (and icon (not (string-equal (substring icon 0 7) "http://"))
             (file-exists-p icon) (file-readable-p icon))
    (let ((id (md5 icon))
          (data (gntp-file-string icon)))
      (format "Identifier: %s\r\nLength: %d\r\n\r\n%s"
              id (length data) data))))

(defun gntp-notice-name (notice)
  "Get the name of NOTICE.  The name must be either a symbol or string."
  (let ((name (car notice)))
    (if (symbolp name)
        (symbol-name name)
      name)))

(defun gntp-notice-get (notice property)
  "Get PROPERTY from NOTICE."
  (plist-get (cdr notice) property))

(defun gntp-send (message server &optional port)
  "Send MESSAGE to SERVER:PORT.  PORT defaults to `gntp-server-port'."
  (let ((proc (make-network-process
               :name "gntp"
               :host server
               :server nil
               :service (if port port gntp-server-port)
               ;;:sentinel 'gntp-sentinel
               :filter 'gntp-filter)))
    ;; hmm one CRLF too much?
    (process-send-string proc (concat message "\r\n\r\n\r\n"))))

(defun gntp-filter (proc string)
  "Filter for PROC started by `gntp-send'.
Argument STRING reply from the server."
  (when (string-equal "GNTP/1.0 -ERROR" (substring string 0 15))
    (error "GNTP: Something went wrong take a look at the reply:\n %s"
           string)))

;; (defun gntp-sentinel (proc msg)
;;   (when (string= msg "connection broken by remote peer\n")
;;     (message (format "client %s has quit" proc))))


(defun gntp-file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(provide 'gntp)

;;; gntp.el ends here

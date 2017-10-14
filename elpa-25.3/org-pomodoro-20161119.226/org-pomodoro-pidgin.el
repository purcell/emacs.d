;; -*- lexical-binding: t; -*-
;;; org-pomodoro-pidgin.el --- Integrate org-pomodoro and Pidgin
;;
;; Copyright (C) 2013 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Created: 2013-07-11
;; Keywords: emacs package elisp pidgin pomodoro org-mode
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; commentary:
;;
;; Update Pidgin status as the Pomodoro progresses.
;;
;; (require 'org-pomodoro-pidgin)
;;
;;; code:
;;
;; The following code uses the "org-pompid" prefix for all "private"
;; functions (and an addition dash "-") and the "org-pomodoro-pidgin"
;; prefix for all "public" functions.

(require 'org-pomodoro)

(defgroup org-pomodoro-pidgin nil
  "Customization group for the Pidgin integration with
org-pomodoro."
  :group 'org-pomodoro)

(defcustom org-pomodoro-pidgin-busy-status
  "Pomodoro ends at %s"
  "Status message when a pomodoro is in progress.
The string will be passed to `format' with the time when pomodoro
ends."
  :group 'org-pomodoro-pidgin)

(defcustom org-pomodoro-pidgin-break-status
  "Available"
  "Status message when a pomodoro is in progress."
  :group 'org-pomodoro-pidgin)

(defun org-pompid--status-type-to-id (type)
  "Convert the symbol TYPE to the correspond int32.

https://developer.pidgin.im/wiki/DbusHowto#CallingPidginmethods."
  (cl-case type
    (offline 1)
    (available 2)
    (unavailable 3)
    (invisible 4)
    (away 5)
    (mobile 7)
    (tune 8)))

(defun org-pompid--call-method (method handler &rest args)
  "Call METHOD with D-Bus and execute HANDLER upon answer.

ARGS lists additional parameters for METHOD."
  (when
      (member "im.pidgin.purple.PurpleService" (dbus-list-known-names :session))
    (apply #'dbus-call-method-asynchronously
         :session
         "im.pidgin.purple.PurpleService"
         "/im/pidgin/purple/PurpleObject"
         "im.pidgin.purple.PurpleInterface"
         method
         handler
         args)))

(defun org-pompid--set-status-message (status message)
  "Update STATUS with the MESSAGE."
  (org-pompid--call-method
   "PurpleSavedstatusSetMessage"
   nil
   :int32 status
   message))

(defun org-pompid--new-transient-status (type handler)
  "Create a new status of TYPE and execute HANDLER when created."
  (org-pompid--call-method
   "PurpleSavedstatusNew"
   handler
   ""
   :int32 (org-pompid--status-type-to-id type)))

(defun org-pompid--activate (status)
  "Make STATUS the current one in Piding."
  (org-pompid--call-method
   "PurpleSavedstatusActivate"
   nil
   :int32 status))

(defun org-pompid--change-status-message (type message)
  "Create a new status of TYPE with MESSAGE.

TYPE must be valid for `org-pompid--status-type-to-id'."
  (org-pompid--new-transient-status
   type
   (lambda (status)
     (org-pompid--set-status-message status message)
     (org-pompid--activate status))))

(defun org-pompid--format-message (message)
  "Replace the %s in MESSAGE with the time when pomodoro ends."
  (format
   message
   (format-time-string
    "%H:%M"
    (time-add (current-time) (seconds-to-time org-pomodoro-countdown)))))

(add-hook
 'org-pomodoro-started-hook
 (lambda ()
   (org-pompid--change-status-message
    'unavailable
    (org-pompid--format-message org-pomodoro-pidgin-busy-status))))

(add-hook
 'org-pomodoro-finished-hook
 (lambda ()
   (org-pompid--change-status-message
    'available
    org-pomodoro-pidgin-break-status)))

(add-hook
 'org-pomodoro-killed-hook
 (lambda ()
   (org-pompid--change-status-message
    'available
    org-pomodoro-pidgin-break-status)))

(provide 'org-pomodoro-pidgin)
;;; org-pomodoro-pidgin.el ends here

;;; slp.el --- An SLP interface.

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: SLP

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;
;; slp.el is an elisp library providing an interface for SLP (RFC2614)
;; using OpenSLP(http://www.openslp.org/) slptool .
;;
;;; History:
;; 28 Aug 2001 Created.

;;; Code:
(eval-when-compile (require 'cl))

(defgroup slp nil
  "Interface for `Service Location Protocol'."
  :group 'comm)

(defcustom slp-program "slptool"
  "SLP client program (OpenSLP's slptool)."
  :type 'string
  :group 'slp)

(defcustom slp-program-arguments nil
  "Option argument for SLP client program."
  :type '(repeat string)
  :group 'slp)

(defun slp-exec-wait (type &rest args)
  "Synchronous execution of slp-program.
TYPE is a symbol (one of `srvs', `attrs', `srvtypes', `as-is', `ignore')."
  (with-temp-buffer
    (let ((result (apply 'call-process slp-program nil t nil
			 (append slp-program-arguments (delq nil args)))))
      (unless (zerop result)
	(error "SLP error: %s" (buffer-string)))
      (goto-char (point-min))
      (case type
	(srvs (slp-parse-srvs))
	(attrs (slp-parse-attrs))
	(srvtypes (slp-parse-srvtypes))
	(as-is (buffer-string))))))

;; Response parser.
(defun slp-parse-srvs ()
  (let (srvtype hostport host port lifetime srvs)
    (while (and
	    (not (eobp))
	    (looking-at "service:\\([^:]+\\):/[^/]*/\\([^,]+\\),\\([0-9]+\\)"))
      (setq srvtype (match-string 1)
	    hostport (match-string 2)
	    lifetime (string-to-number (match-string 3)))
      (if (string-match ":\\([0-9]+\\)" hostport)
	  (setq host (substring hostport 0 (match-beginning 0))
		port (string-to-number (match-string 1 hostport)))
	(setq host hostport
	      port nil))
      (push (cons (list srvtype host port) lifetime) srvs)
      (forward-line))
    (list 'srvs (nreverse srvs))))

(defsubst slp-forward ()
  (or (eobp) (forward-char)))

(defun slp-parse-attr ()
  (when (looking-at "(\\([^=]+\\)=\\([^)]+\\))")
    (prog1 (cons (match-string 1) (match-string 2))
      (goto-char (match-end 0)))))

(defun slp-parse-attrs ()
  (let (attrs)
    (push (slp-parse-attr) attrs)
    (while (eq (char-after (point)) ?,)
      (slp-forward)
      (push (slp-parse-attr) attrs))
    (list 'attrs (nreverse attrs))))

(defun slp-parse-srvtypes ()
  (let (types)
    (while (not (eobp))
      (when (looking-at "^service:\\([^/\n]+\\)$")
	(push (buffer-substring (match-beginning 1) (match-end 1)) types))
      (forward-line))
    (list 'srvtypes (nreverse types))))

;; Response accessor.
(defsubst slp-response-type (response)
  (nth 0 response))

(defsubst slp-response-body (response)
  (nth 1 response))

(defsubst slp-response-srv-url-service-type (srv)
  (nth 0 (car srv)))

(defsubst slp-response-srv-url-host (srv)
  (nth 1 (car srv)))

(defsubst slp-response-srv-url-port (srv)
  (nth 2 (car srv)))

(defsubst slp-response-srv-lifetime (srv)
  (cdr srv))

;; Commands
(defun slp-findsrvs (service-type &optional filter)
  (slp-exec-wait 'srvs "findsrvs" service-type filter))

(defun slp-findattrs (url &rest attrids)
  (apply 'slp-exec-wait 'attrs "findattrs" url attrids))

(defun slp-findsrvtypes (&optional authority)
  (slp-exec-wait 'srvtypes "findsrvtypes" authority))

(defun slp-findscopes ()
  (slp-exec-wait 'as-is "findscopes"))

(defun slp-register (url &optional attrs)
  (slp-exec-wait 'ignore "register" url (mapconcat
					 (lambda (pair)
					   (format "(%s=%s)"
						   (car pair)
						   (cdr pair)))
					 attrs
					 ",")))

(defun slp-deregister (url)
  (slp-exec-wait 'ignore "deregister" url))

(defun slp-getproperty (propertyname)
  (slp-exec-wait 'as-is "getproperty" propertyname))

(provide 'slp)

;;; slp.el ends here

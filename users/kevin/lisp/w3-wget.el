;;; w3-wget.el --- Interface program of wget on emacs-w3.

;; Copyright (C) 2002, 2004 Masayuki Ataka <masayuki.ataka@gmail.com>

;; Authors: Masayuki Ataka <masayuki.ataka@gmail.com>
;; Keywords: w3, WWW, hypermedia

;; This file is a part of emacs-wget.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Code:
(autoload 'wget-api "wget" "Application Program Interface for wget")

(defun w3-wget (arg)
  "Downlod anchor, image, or current page.
With prefix argument ARG, you can change uri and wget options."
  (interactive "P")
  (let ((current-uri (if (fboundp 'url-recreate-url)
			 (url-recreate-url (if (boundp 'url-current-object) url-current-object))
		       (error "Function url-recreate-url is not available")))
	(uri (if (fboundp 'w3-view-this-url) 
		 (w3-view-this-url t)
	       (error "Function w3-view-this-url is not available"))))
    (wget-api uri current-uri arg)))


(provide 'w3-wget)
;;; w3-wget.el ends here

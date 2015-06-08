;;; w3m-wget.el --- Interface program of wget on emacs-w3m.

;; Copyright (C) 2001, 2002 Masayuki Ataka <masayuki.ataka@gmail.com>

;; Authors: Masayuki Ataka <masayuki.ataka@gmail.com>
;; Keywords: w3m, WWW, hypermedia

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

(defgroup w3m-wget nil
  "wget interface for emacs-w3m."
  :group 'wget
  :group 'w3m
  :prefix "w3m-wget-")

(defcustom w3m-wget-substitute-download-command t
  "*If non-nil, substitute download command from emacs-w3m default to `w3m-wget'."
  :group 'w3m-wget
  :type 'boolean)


;;
;; Eval after load w3m.el
;;
(eval-after-load "w3m" '(progn

(defun w3m-wget (arg)
  "Download anchor, image, or current page.
With prefix argument ARG, you can change uri."
  (interactive "P")
  (let ((uri (or (w3m-anchor) (w3m-image))))
    (setq wget-current-title w3m-current-title)
    (wget-api uri w3m-current-url arg)))

(if w3m-wget-substitute-download-command
    (substitute-key-definition 'w3m-download-this-url
			       'w3m-wget w3m-mode-map))

)) ;; eval after load w3m


(provide 'w3m-wget)
;;; w3m-wget.el ends here

;;; elmo-version.el --- Version information for ELMO.

;; Copyright (C) 2000-2001 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000-2001 TAKAHASHI Kaoru <kaoru@kaisei.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	TAKAHASHI Kaoru <kaoru@kaisei.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

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
;; Put the following lines to each file of ELMO package.
;;
;; (require 'product)
;; (product-provide (provide FEATURE) (require 'elmo-version))

;;; Code:
;;
(require 'product)
(provide 'elmo-version)			; before product-provide

;; product-define in the first place
(product-provide 'elmo-version
  ;; Don't forget to run `make update-version' and `make test'.
  ;; Don't forget to check codename in `wl-version.el'.
  (product-define "ELMO" nil '(2 15 9)))

;; set version-string
(product-version-as-string 'elmo-version)

(defun elmo-version ()
  "Return ELMO version."
  (product-string-1 'elmo-version))

;; for backward compatibility
(defconst elmo-appname (product-name (product-find 'elmo-version)))
(make-obsolete-variable
 'elmo-appname
 "use (product-name (product-find 'elmo-version)) instead.")

(defconst elmo-version (product-version-string (product-find 'elmo-version)))
(make-obsolete-variable
 'elmo-version
 "use (product-version-string (product-find 'elmo-version)) instead.")

;;; elmo-version.el ends here

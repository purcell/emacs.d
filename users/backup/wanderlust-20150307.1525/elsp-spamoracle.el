;;; elsp-spamoracle.el --- SpamOracle support for elmo-spam.

;; Copyright (C) 2004 Daishi Kato <daishi@axlight.com>

;; Author: Daishi Kato <daishi@axlight.com>
;; Keywords: mail, net news, spam

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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

;;; Code:
;;
(require 'elmo-spam)

(defgroup elmo-spam-spamoracle nil
  "Spam spamoracle configuration."
  :group 'elmo-spam)

(defcustom elmo-spam-spamoracle-program "spamoracle"
  "Program name of the SpamOracle."
  :type '(string :tag "Program name of the SpamOracle")
  :group 'elmo-spam-spamoracle)

(defcustom elmo-spam-spamoracle-config-filename nil
  "Filename of the SpamOracle config."
  :type '(file :tag "Filename of the SpamOracle config")
  :group 'elmo-spam-spamoracle)

(defcustom elmo-spam-spamoracle-database-filename
  (expand-file-name ".spamoracle.db" elmo-msgdb-directory)
  "Filename of the SpamOracle database."
  :type '(file :tag "Filename of the SpamOracle database")
  :group 'elmo-spam-spamoracle)

(defcustom elmo-spam-spamoracle-spam-header-regexp "^X-Spam: yes;"
  "Regexp of the SpamOracle spam header."
  :type '(string :tag "Regexp of the SpamOracle spam header")
  :group 'elmo-spam-spamoracle)

(eval-and-compile
  (luna-define-class elsp-spamoracle (elsp-generic)))

(defsubst elmo-spam-spamoracle-call (type)
  (let ((args (cond
	       ((eq type 'check)
		(list "mark"))
	       ((eq type 'add-spam)
		(list "add" "-v" "-spam"))
	       ((eq type 'add-good)
		(list "add" "-v" "-good"))))
	(output-buffer (get-buffer-create "*Output ELMO SpamOracle*")))
    (with-current-buffer output-buffer
      (erase-buffer))
    (apply #'call-process-region
	   (point-min) (point-max)
	   elmo-spam-spamoracle-program
	   nil output-buffer
	   nil (delq nil
		     (append (if elmo-spam-spamoracle-config-filename
				 (list "-config"
				       elmo-spam-spamoracle-config-filename))
			     (if elmo-spam-spamoracle-database-filename
				 (list "-f"
				       elmo-spam-spamoracle-database-filename))
			     args)))
    (if (eq type 'check)
	(with-current-buffer output-buffer
	  (goto-char (point-min))
	  (let ((body-point (re-search-forward "^$" nil t)))
	    (goto-char (point-min))
	    (re-search-forward elmo-spam-spamoracle-spam-header-regexp
			       body-point t)))
      t)))

(luna-define-method elmo-spam-buffer-spam-p ((processor elsp-spamoracle)
					     buffer &optional register)
  (let ((result (with-current-buffer buffer
		  (elmo-spam-spamoracle-call 'check))))
    (when register
      (if result
	  (elmo-spam-register-spam-buffer processor buffer)
	(elmo-spam-register-good-buffer processor buffer)))
    result))

(luna-define-method elmo-spam-register-spam-buffer ((processor elsp-spamoracle)
						    buffer &optional restore)
  (with-current-buffer buffer
    (elmo-spam-spamoracle-call 'add-spam)))

(luna-define-method elmo-spam-register-good-buffer ((processor elsp-spamoracle)
						    buffer &optional restore)
  (with-current-buffer buffer
    (elmo-spam-spamoracle-call 'add-good)))

(require 'product)
(product-provide (provide 'elsp-spamoracle) (require 'elmo-version))

;;; elsp-spamoracle.el ends here

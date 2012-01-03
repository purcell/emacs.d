;;; rest-api.el --- Utilities for use in REST API clients

;; Copyright (C) 2005  Edward O'Connor

;; Author: Edward O'Connor <ted@evdb.com>
;; Keywords: convenience

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; I found myself needing the same elisp utilities over and over again
;; while working on various REST API clients (Backpack, Technorati,
;; EVDB, LiveJournal, etc.), so I've consolidated them here.


;;; History:
;; 2005-10-23: initial version.

;;; Code:

(require 'pp)
(require 'url)
(require 'xml)

(defsubst rest-api-join (strings &optional separator)
  "Join STRINGS with SEPARATOR (or a space if none is supplied)."
  (mapconcat 'identity strings (or separator " ")))

(defun rest-api-xmlify (node)
  "Return a string of XML representing NODE.
NODE is an s-expression of the form `xml-parse-region' returns; see
xml.el for more."
  (with-temp-buffer
    (xml-print node)
    (buffer-substring (point-min) (point-max))))

(defun rest-api-ensure-string (object)
  "Return a string representation of OBJECT."
  (cond ((stringp object) object)
        ((symbolp object) (symbol-name object))
        ((numberp object) (number-to-string object))
        (t (pp-to-string object))))

(defun rest-api-format-url-parameters (alist)
  "Format ALIST as HTTP query parameters."
  (mapconcat
   (lambda (cons)
     (format "%s=%s"
             (url-hexify-string (rest-api-ensure-string (car cons)))
             (url-hexify-string (rest-api-ensure-string (cdr cons)))))
   alist
   "&"))

(provide 'rest-api)
;;; rest-api.el ends here

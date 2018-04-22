;;; logito.el --- logging library for Emacs

;; Copyright (C) 2012  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp, tool
;; Package-Version: 20120225.1255
;; Version: 0.1
;; Package-Requires: ((eieio "1.3"))

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

;; This module provides logging facility for Emacs

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'eieio)

(defclass logito-object ()
  ((level :initarg :level :initform nil)))

(defmethod logito-insert-log ((log logito-object) format &rest objects)
  "Base implementation, do nothing")

(defmethod logito-should-log ((log logito-object) level)
  (let ((l (oref log :level)))
    (and (integerp l)
         (<= level l))))

(defmethod logito-log ((log logito-object) level tag string &rest objects)
  (when (logito-should-log log level)
    (apply 'logito-insert-log log (format "[%s] %s" tag string) objects)))

(defmethod logito-log (log level tag string &rest objects)
  "Fallback implementation, do nothing. This allows in particular
  to pass nil as the log object."
  nil)

(defclass logito-message-object (logito-object)
  ())

(defmethod logito-insert-log ((log logito-message-object) format &rest objects)
  (apply 'message format objects))

(defclass logito-buffer-object (logito-object)
  ((buffer :initarg :buffer :initform nil)))

(defmethod logito-should-log ((log logito-buffer-object) level)
  (and (oref log :buffer)
       (call-next-method)))

(defmethod logito-insert-log ((log logito-buffer-object) format &rest objects)
  (let ((buffer (get-buffer-create (oref log :buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (apply 'format format objects) "\n\n"))))

(defmacro logito-def-level (sym val &optional pkg)
  "Define a constant logito-<SYM>-level and a macro logito:<SYM>
associated with this level."
  (let* ((pkg (or pkg 'logito))
         (const (intern (format "%s:%s-level"
                                (symbol-name pkg) (symbol-name sym))))
         (mac (intern (format "%s:%s"
                              (symbol-name pkg) (symbol-name sym)))))
    `(progn
       (defconst ,const ,val)
       (defmacro ,mac (log string &rest objects)
         (append
          (list 'logito-log log ,const '',sym string)
          objects)))))

;; built-in log levels
(logito-def-level error 0)
(logito-def-level info 5)
(logito-def-level verbose 10)
(logito-def-level debug 15)

(provide 'logito)
;;; logito.el ends here

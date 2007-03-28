;;; nxml-util.el --- utility functions for nxml-*.el

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;;; Code:

(defun nxml-make-namespace (str)
  "Return a symbol for the namespace URI STR.
STR must be a string. If STR is the empty string, return nil.
Otherwise, return the symbol whose name is STR prefixed with a colon."
  (if (string-equal str "")
      nil
    (intern (concat ":" str))))

(defun nxml-namespace-name (ns)
  "Return the namespace URI corresponding to the symbol NS.
This is the inverse of `nxml-make-namespace'."
  (and ns (substring (symbol-name ns) 1)))

(defconst nxml-xml-namespace-uri 
  (nxml-make-namespace "http://www.w3.org/XML/1998/namespace"))

(defconst nxml-xmlns-namespace-uri
  (nxml-make-namespace "http://www.w3.org/2000/xmlns/"))

(defmacro nxml-with-unmodifying-text-property-changes (&rest body)
  "Evaluate BODY without any text property changes modifying the buffer.
Any text properties changes happen as usual but the changes are not treated as
modifications to the buffer."
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
	   (inhibit-read-only t)
	   (inhibit-modification-hooks t)
	   (buffer-undo-list t)
	   (deactivate-mark nil)
	   ;; Apparently these avoid file locking problems.
	   (buffer-file-name nil)
	   (buffer-file-truename nil))
       (unwind-protect
	   (progn ,@body)
	 (unless ,modified
	   (restore-buffer-modified-p nil))))))

(put 'nxml-with-unmodifying-text-property-changes 'lisp-indent-function 0)
(def-edebug-spec nxml-with-unmodifying-text-property-changes t)

(defmacro nxml-with-invisible-motion (&rest body)
  "Evaluate body without calling any point motion hooks."
  `(let ((inhibit-point-motion-hooks t))
     ,@body))

(put 'nxml-with-invisible-motion 'lisp-indent-function 0)
(def-edebug-spec nxml-with-invisible-motion t)

(defun nxml-display-file-parse-error (err)
  (let* ((filename (nth 1 err))
	 (buffer (find-file-noselect filename))
	 (pos (nth 2 err))
	 (message (nth 3 err)))
    (pop-to-buffer buffer)
    ;; What's the right thing to do if the buffer's modified?
    ;; The position in the saved file could be completely different.
    (goto-char (if (buffer-modified-p) 1 pos))
    (error "%s" message)))

(defun nxml-signal-file-parse-error (file pos message &optional error-symbol)
  (signal (or error-symbol 'nxml-file-parse-error)
	  (list file pos message)))

(put 'nxml-file-parse-error
     'error-conditions
     '(error nxml-file-parse-error))

(put 'nxml-parse-file-error
     'error-message
     "Error parsing file")

(provide 'nxml-util)

;;; nxml-util.el ends here

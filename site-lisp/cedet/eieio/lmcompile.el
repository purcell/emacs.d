;;; lmcompile.el --- highlight compile error lines

;;
;; Author: Eric M. Ludlam <eludlam@mathworks.com>
;; Maintainer: Eric M. Ludlam <eludlam@mathworks.com>
;; Keywords: lisp
;;
;; Copyright (C) 2003, 2004, 2005, 2009 Eric M. Ludlam
;;
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;;  This package uses the compile package, and the linemark package to
;; highlight all lines showing errors.

;;; Notes:
;;
;; Thanks to Markus Gritsch for adding support for grep-buffers, where
;; no file is associated with a buffer.  (Similar work in linemark.el)

(require 'linemark)

;;; Code:
(defclass lmcompile-linemark-group (linemark-group)
  (
   )
  "Linemark Group for compile error highlights.")

(defclass lmcompile-linemark-entry (linemark-entry)
  ((errormarker :initarg :errormarker
		:type marker
		:documentation
		"Marker pointing to the source of the match.")
   (errmsg :initarg :errmsg
	   :type string
	   :documentation
	   "The match text of the error in question.")
   )
  "Linemark Group for one compile error highlight.
Tracks additional information about the error.")

(defmethod linemark-new-entry ((g linemark-group) &rest args)
  "Create a new entry for G using init ARGS."
  (let ((f (plist-get args :filename))
	(l (plist-get args :line)))
    (apply 'lmcompile-linemark-entry (format "%s %d" f l)
	   args)))

(defmethod linemark-display ((e lmcompile-linemark-entry) active-p)
  "Set object E to be active or inactive."
  ;; Do the rest of our work
  (call-next-method)

  ;; Add a tool tip
  (when (and active-p
	     (slot-boundp e 'overlay)
	     (oref e overlay)
	     (slot-boundp e 'errmsg)
	     )

    (linemark-overlay-put (oref e overlay)
			  'help-echo
			  (oref e errmsg))
    )
  )

(defun lmcompile-create-group (name)
  "Create a group object for tracking linemark entries.
Do not permit multiple groups with the same NAME."
  (let ((newgroup (lmcompile-linemark-group name))
	(foundgroup nil)
	(lmg linemark-groups))
    (while (and (not foundgroup) lmg)
      (if (string= name (object-name-string (car lmg)))
	  (setq foundgroup (car lmg)))
      (setq lmg (cdr lmg)))
    (if foundgroup
	(setq newgroup foundgroup)
      (setq linemark-groups (cons newgroup linemark-groups))
      newgroup)))

(defvar lmcompile-error-group
  (linemark-new-group 'lmcompile-linemark-group "compiler errors")
  "The LMCOMPILE error group object.")

(defun lmcompile-clear ()
  "Flush all compile error entries."
  (interactive)
  (mapcar (lambda (e) (linemark-delete e))
	  (oref lmcompile-error-group marks)))

;; Compatibility
(if (fboundp 'compile-reinitialize-errors)
    (defalias 'lmcompile-reinitialize-errors 'compile-reinitialize-errors)
  ;; Newer versions of Emacs:
  (defun lmcompile-reinitialize-errors (&rest foo)
    "Find out what this should be."
    (error "Need replacement for `compile-reinitialize-errors")
    )
  )

;;;###autoload
(defun lmcompile-do-highlight ()
  "Do compilation mode highlighting.
Works on grep, compile, or other type mode."
  (interactive)

  ;; Flush out the old
  (lmcompile-clear)

  ;; Set the buffer appropriately
  (setq compilation-last-buffer (compilation-find-buffer))

  ;; Get the list of errors to be activated.
  (lmcompile-reinitialize-errors nil)
  
  (let ((marks
	 (save-excursion
	   (set-buffer compilation-last-buffer)
	   compilation-error-list))
	)
    (while marks
      (let (errmark
            file
            line
            (face nil)
            (case-fold-search t)
            (txt nil)
            )

        (setq errmark (car (car marks)))
        (if (listp (cdr (car marks)))
            (progn ; So a list containing filename, linenumber, ... like (grep) provides is used.
              (setq file (nth 1 (car marks)))
              (setq line (nth 2 (car marks)))

              (setq file (concat (car (cdr file))
                                 (car file)))

              ;; In case file contains an absolute path, the above doesn't work, at least not on Win32.  Use this version.
              ;; Originally suggested by: Markus Gritsch
              (if (not (file-exists-p file))
                  (setq file (car (nth 1 (car marks))))))

          (progn ; Otherwise we assume that we have a marker, which works also on buffers which have no file associated.
            (setq file (buffer-name (marker-buffer (cdr (car marks)))))

            (setq line (save-excursion
                         (set-buffer (marker-buffer (cdr (car marks))))
                         (save-excursion
                           (goto-char (cdr (car marks)))
                           (count-lines 1 (1+ (point))))))))

	;; We've got the goods, lets add in an entry.
	;; If we can't find the file, skip it.  It'll be
	;; found eventually.
	(when (or (file-exists-p file) (bufferp (marker-buffer (cdr (car marks)))))

	  (condition-case nil
	      (save-excursion
		(set-buffer (marker-buffer errmark))
		(save-excursion
		  (goto-char errmark)
	  
		  (setq face (cond
			      ((re-search-forward "error" (point-at-eol) t)
			       'linemark-stop-face)
			      ((re-search-forward "warning" (point-at-eol) t)
			       'linemark-caution-face)
			      (t
			       'linemark-go-face)))))
	    (error nil))

	  (condition-case nil
	      (save-excursion
		(set-buffer (marker-buffer errmark))
		(save-excursion
		  (goto-char errmark)
		  (setq txt (buffer-substring-no-properties
			     (point-at-bol) (point-at-eol)))
		  ;; Strip positional information
		  (while (string-match "[0-9]:" txt)
		    (setq txt (substring txt (match-end 0))))
		  ;; Strip leading whitespace (if any)
		  (when (string-match "^\\s-++" txt)
		    (setq txt (substring txt (match-end 0))))
		  ))
	    (error nil))

	  (linemark-add-entry
	   lmcompile-error-group
	   :filename file
	   :line line
	   :errormarker errmark
	   :face face
	   :errmsg txt
	   )

	  ))
      (setq marks (cdr marks)))))

(provide 'lmcompile)

;;; lmcompile.el ends here

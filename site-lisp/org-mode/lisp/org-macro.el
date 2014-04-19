;;; org-macro.el --- Macro Replacement Code for Org Mode

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Macros are expanded with `org-macro-replace-all', which relies
;; internally on `org-macro-expand'.

;; Default templates for expansion are stored in the buffer-local
;; variable `org-macro-templates'.  This variable is updated by
;; `org-macro-initialize-templates', which recursively calls
;; `org-macro--collect-macros' in order to read setup files.

;; Along with macros defined through #+MACRO: keyword, default
;; templates include the following hard-coded macros:
;; {{{time(format-string)}}}, {{{property(node-property)}}},
;; {{{input-file}}} and {{{modification-time(format-string)}}}.

;; Upon exporting, "ox.el" will also provide {{{author}}}, {{{date}}},
;; {{{email}}} and {{{title}}} macros.

;;; Code:
(require 'org-macs)

(declare-function org-element-at-point "org-element" (&optional keep-trail))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-remove-double-quotes "org" (s))
(declare-function org-mode "org" ())
(declare-function org-file-contents "org" (file &optional noerror))
(declare-function org-with-wide-buffer "org-macs" (&rest body))

;;; Variables

(defvar org-macro-templates nil
  "Alist containing all macro templates in current buffer.
Associations are in the shape of (NAME . TEMPLATE) where NAME
stands for macro's name and template for its replacement value,
both as strings.  This is an internal variable.  Do not set it
directly, use instead:

  #+MACRO: name template")
(make-variable-buffer-local 'org-macro-templates)


;;; Functions

(defun org-macro--collect-macros ()
  "Collect macro definitions in current buffer and setup files.
Return an alist containing all macro templates found."
  (let* (collect-macros			; For byte-compiler.
	 (collect-macros
	  (lambda (files templates)
	    ;; Return an alist of macro templates.  FILES is a list of
	    ;; setup files names read so far, used to avoid circular
	    ;; dependencies.  TEMPLATES is the alist collected so far.
	    (let ((case-fold-search t))
	      (org-with-wide-buffer
	       (goto-char (point-min))
	       (while (re-search-forward
		       "^[ \t]*#\\+\\(MACRO\\|SETUPFILE\\):" nil t)
		 (let ((element (org-element-at-point)))
		   (when (eq (org-element-type element) 'keyword)
		     (let ((val (org-element-property :value element)))
		       (if (equal (org-element-property :key element) "MACRO")
			   ;; Install macro in TEMPLATES.
			   (when (string-match
				  "^\\(.*?\\)\\(?:\\s-+\\(.*\\)\\)?\\s-*$" val)
			     (let* ((name (match-string 1 val))
				    (template (or (match-string 2 val) ""))
				    (old-cell (assoc name templates)))
			       (if old-cell (setcdr old-cell template)
				 (push (cons name template) templates))))
			 ;; Enter setup file.
			 (let ((file (expand-file-name
				      (org-remove-double-quotes val))))
			   (unless (member file files)
			     (with-temp-buffer
			       (org-mode)
			       (insert (org-file-contents file 'noerror))
			       (setq templates
				     (funcall collect-macros (cons file files)
					      templates)))))))))))
	      templates))))
    (funcall collect-macros nil nil)))

(defun org-macro-initialize-templates ()
  "Collect macro templates defined in current buffer.
Templates are stored in buffer-local variable
`org-macro-templates'.  In addition to buffer-defined macros, the
function installs the following ones: \"property\",
\"time\". and, if the buffer is associated to a file,
\"input-file\" and \"modification-time\"."
  (let* ((templates (org-macro--collect-macros))
	 (update-templates
	  (lambda (cell)
	    (let ((old-template (assoc (car cell) templates)))
	      (if old-template (setcdr old-template (cdr cell))
		(push cell templates))))))
    ;; Install hard-coded macros.
    (mapc (lambda (cell) (funcall update-templates cell))
	  (list (cons "property" "(eval (org-entry-get nil \"$1\" 'selective))")
		(cons "time" "(eval (format-time-string \"$1\"))")))
    (let ((visited-file (buffer-file-name (buffer-base-buffer))))
      (when (and visited-file (file-exists-p visited-file))
	(mapc (lambda (cell) (funcall update-templates cell))
	      (list (cons "input-file" (file-name-nondirectory visited-file))
		    (cons "modification-time"
			  (format "(eval (format-time-string \"$1\" '%s))"
				  (prin1-to-string
				   (nth 5 (file-attributes visited-file)))))))))
    (setq org-macro-templates templates)))

(defun org-macro-expand (macro templates)
  "Return expanded MACRO, as a string.
MACRO is an object, obtained, for example, with
`org-element-context'.  TEMPLATES is an alist of templates used
for expansion.  See `org-macro-templates' for a buffer-local
default value.  Return nil if no template was found."
  (let ((template
	 ;; Macro names are case-insensitive.
	 (cdr (assoc-string (org-element-property :key macro) templates t))))
    (when template
      (let ((value (replace-regexp-in-string
                    "\\$[0-9]+"
                    (lambda (arg)
                      (or (nth (1- (string-to-number (substring arg 1)))
                               (org-element-property :args macro))
                          ;; No argument: remove place-holder.
                          ""))
                    template nil 'literal)))
        ;; VALUE starts with "(eval": it is a s-exp, `eval' it.
        (when (string-match "\\`(eval\\>" value)
          (setq value (eval (read value))))
        ;; Return string.
        (format "%s" (or value ""))))))

(defun org-macro-replace-all (templates)
  "Replace all macros in current buffer by their expansion.
TEMPLATES is an alist of templates used for expansion.  See
`org-macro-templates' for a buffer-local default value."
  (save-excursion
    (goto-char (point-min))
    (let (record)
      (while (re-search-forward "{{{[-A-Za-z0-9_]" nil t)
	(let ((object (org-element-context)))
	  (when (eq (org-element-type object) 'macro)
	    (let* ((value (org-macro-expand object templates))
		   (begin (org-element-property :begin object))
		   (signature (list begin
				    object
				    (org-element-property :args object))))
	      ;; Avoid circular dependencies by checking if the same
	      ;; macro with the same arguments is expanded at the same
	      ;; position twice.
	      (if (member signature record)
		  (error "Circular macro expansion: %s"
			 (org-element-property :key object))
		(when value
		  (push signature record)
		  (delete-region
		   begin
		   ;; Preserve white spaces after the macro.
		   (progn (goto-char (org-element-property :end object))
			  (skip-chars-backward " \t")
			  (point)))
		  ;; Leave point before replacement in case of recursive
		  ;; expansions.
		  (save-excursion (insert value)))))))))))


(provide 'org-macro)
;;; org-macro.el ends here

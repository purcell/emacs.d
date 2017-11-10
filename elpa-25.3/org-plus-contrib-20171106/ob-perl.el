;;; ob-perl.el --- Babel Functions for Perl          -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Authors: Dan Davison
;;	 Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating perl source code.

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("perl" . "pl"))

(defvar org-babel-default-header-args:perl '())

(defvar org-babel-perl-command "perl"
  "Name of command to use for executing perl code.")

(defun org-babel-execute:perl (body params)
  "Execute a block of Perl code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (cdr (assq :session params)))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:perl params)))
	 (session (org-babel-perl-initiate-session session)))
    (org-babel-reassemble-table
     (org-babel-perl-evaluate session full-body result-type result-params)
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(defun org-babel-prep-session:perl (_session _params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (error "Sessions are not supported for Perl"))

(defun org-babel-variable-assignments:perl (params)
  "Return list of perl statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (org-babel-perl--var-to-perl (cdr pair) (car pair)))
   (org-babel--get-vars params)))

;; helper functions

(defvar org-babel-perl-var-wrap "q(%s)"
  "Wrapper for variables inserted into Perl code.")

(defvar org-babel-perl--lvl)
(defun org-babel-perl--var-to-perl (var &optional varn)
  "Convert an elisp value to a perl variable.
The elisp value, VAR, is converted to a string of perl source code
specifying a var of the same value."
  (if varn
      (let ((org-babel-perl--lvl 0) (lvar (listp var)))
	(concat "my $" (symbol-name varn) "=" (when lvar "\n")
		(org-babel-perl--var-to-perl var)
		";\n"))
    (let ((prefix (make-string (* 2 org-babel-perl--lvl) ?\ )))
      (concat prefix
	      (if (listp var)
		  (let ((org-babel-perl--lvl (1+ org-babel-perl--lvl)))
		    (concat "[\n"
			    (mapconcat #'org-babel-perl--var-to-perl var "")
			    prefix "]"))
		(format "q(%s)" var))
	      (unless (zerop org-babel-perl--lvl) ",\n")))))

(defvar org-babel-perl-buffers '(:default . nil))

(defun org-babel-perl-initiate-session (&optional _session _params)
  "Return nil because sessions are not supported by perl."
  nil)

(defvar org-babel-perl-wrapper-method "{
    my $babel_sub = sub {
        %s
    };
    open my $BOH, qq(>%s) or die qq(Perl: Could not open output file.$/);
    my $rv = &$babel_sub();
    my $rt = ref $rv;
    select $BOH;
    if (qq(ARRAY) eq $rt) {
        local $\\=$/;
        local $,=qq(\t);
	foreach my $rv ( @$rv ) {
	    my $rt = ref $rv;
	    if (qq(ARRAY) eq $rt) {
		print @$rv;
	    } else {
		print $rv;
	    }
	}
    } else {
	print $rv;
    }
}")

(defvar org-babel-perl-preface nil)

(defvar org-babel-perl-pp-wrapper-method
  nil)

(defun org-babel-perl-evaluate (session ibody &optional result-type result-params)
  "Pass BODY to the Perl process in SESSION.
If RESULT-TYPE equals `output' then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals `value' then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Perl"))
  (let* ((body (concat org-babel-perl-preface ibody))
	 (tmp-file (org-babel-temp-file "perl-"))
	 (tmp-babel-file (org-babel-process-file-name
			  tmp-file 'noquote)))
    (let ((results
           (pcase result-type
             (`output
              (with-temp-file tmp-file
                (insert
                 (org-babel-eval org-babel-perl-command body))
                (buffer-string)))
             (`value
              (org-babel-eval org-babel-perl-command
                              (format org-babel-perl-wrapper-method
                                      body tmp-babel-file))))))
      (when results
        (org-babel-result-cond result-params
	  (org-babel-eval-read-file tmp-file)
          (org-babel-import-elisp-from-file tmp-file '(16)))))))

(provide 'ob-perl)



;;; ob-perl.el ends here

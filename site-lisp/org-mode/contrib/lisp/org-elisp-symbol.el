;;; org-elisp-symbol.el --- Org links to emacs-lisp symbols
;;
;; Copyright 2007-2012 Free Software Foundation, Inc.
;;
;; Author: bzg AT gnu DOT org
;; Version: 0.2
;; Keywords: org, remember, lisp
;; URL: http://www.cognition.ens.fr/~guerry/u/org-elisp-symbol.el
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; Org-mode already lets you store/insert links to emacs-lisp files,
;; just like any other file.  This package lets you precisely link to
;; any emacs-lisp symbol and access useful information about the symbol.
;;
;; Here is the list of available properties when linking from a elisp-symbol:
;;
;; :name        The symbol's name.
;; :stype       The symbol's type (commandp, function, etc.)
;; :def         The function used to set the symbol's value (defun, etc.)
;; :keys        The keys associated with the command.
;; :args        The arguments of the function.
;; :docstring   The docstring of the symbol.
;; :doc         The first line of the dostring.
;; :comment     A comment line just above the sexp, if any.
;; :fixme       A FIXME comment line just above the sexp, if any.
;;
;; Let's say we have a defun like this one:
;;
;; ;; FIXME update docstring
;; (defun org-export-latex-lists ()
;;   "Convert lists to LaTeX."
;;   (goto-char (point-min))
;;   (while (re-search-forward org-export-latex-list-beginning-re nil t)
;;     (beginning-of-line)
;;     (insert (org-list-to-latex (org-list-parse-list t)) "\n")))
;;
;; And a remember template like:
;;
;; (setq org-remember-templates
;;   '((?s "* DEBUG `%:name' (%:args)\n\n%?\n\nFixme: %:fixme\n  \
;;          Doc: \"%:doc\"\n\n%a")))
;;
;; Then M-x `org-remember' on this sexp will produce this buffer:
;;
;; =====================================================================
;; * DEBUG `org-export-latex-lists' ()
;;
;; <== point
;;
;; Fixme: update the docstring
;; Doc: "Convert lists to LaTeX."
;;
;; [[file:~/path/file.el::defun%20my-func][Function: my-func]]
;; =====================================================================
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-elisp-symbol)

;;; Code:

(provide 'org-elisp-symbol)

(require 'org)

(org-add-link-type "elisp-symbol" 'org-elisp-symbol-open)
(add-hook 'org-store-link-functions 'org-elisp-symbol-store-link)

(defun org-elisp-symbol-open (path)
  "Visit the emacs-lisp elisp-symbol at PATH."
  (let* ((search (when (string-match "::\\(.+\\)\\'" path)
		   (match-string 1 path)))
	 (path (substring path 0 (match-beginning 0))))
    (org-open-file path t nil search)))

(defun org-elisp-symbol-store-link ()
  "Store a link to an emacs-lisp elisp-symbol."
  (when (eq major-mode 'emacs-lisp-mode)
    (save-excursion
      (or (looking-at "^(") (beginning-of-defun))
      (looking-at "^(\\([a-z]+\\) \\([^)\n ]+\\) ?\n?[ \t]*\\(?:(\\(.*\\))\\)?")
      (let* ((end (save-excursion
		    (save-match-data
		      (end-of-defun) (point))))
	     (def (match-string 1))
	     (name (match-string 2))
	     (sym-name (intern-soft name))
	     (stype (cond ((commandp sym-name) "Command")
			  ((functionp sym-name) "Function")
			  ((user-variable-p sym-name) "User variable")
			  ((string= def "defvar") "Variable")
			  ((string= def "defmacro") "Macro")
			  ((string= def "defun") "Function or command")
			  (t "Symbol")))
	     (args (if (match-string 3)
		       (mapconcat (lambda (a) (unless (string-match "^&" a) a))
				  (split-string (match-string 3)) " ")
		     "no arg"))
	     (docstring (cond ((functionp sym-name)
			       (or (documentation sym-name)
				   "[no documentation]"))
			      ((string-match "[Vv]ariable" stype)
			       (documentation-property sym-name
						       'variable-documentation))
			      (t "no documentation")))
	     (doc (and (string-match "^\\([^\n]+\\)$" docstring)
		       (match-string 1 docstring)))
	     (fixme (save-excursion
		      (beginning-of-defun) (end-of-defun)
		      (if (re-search-forward "^;+ ?FIXME[ :]*\\(.*\\)$" end t)
			  (match-string 1) "nothing to fix")))
	     (comment (save-excursion
			(beginning-of-defun) (end-of-defun)
			(if (re-search-forward "^;;+ ?\\(.*\\)$" end t)
			    (match-string 1) "no comment")))
	     keys keys-desc link description)
	(if (equal stype "Command")
	    (setq keys (where-is-internal sym-name)
		  keys-desc
		  (if keys (mapconcat 'key-description keys " ") "none")))
	(setq link (concat "file:" (abbreviate-file-name buffer-file-name)
			   "::" def " " name))
	(setq description (concat stype ": " name))
	(org-store-link-props
	 :type "elisp-symbol"
	 :link link
	 :description description
	 :def def
	 :name name
	 :stype stype
	 :args args
	 :keys keys-desc
	 :docstring docstring
	 :doc doc
	 :fixme fixme
	 :comment comment)))))

(provide 'org-elisp-symbol)


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

;;; org-elisp-symbol.el ends here

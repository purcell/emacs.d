;;; erlang-edoc.el --- Erlang programs documenting support for Semantic

;; Copyright (C) 2002, 2004, 2007 Vladimir G. Sekissov

;; Author:  <svg@surnet.ru>
;; Keywords: languages, docs
;; $Id: erlang-edoc.el,v 1.5 2007/02/19 13:35:04 zappo Exp $

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
;;
;; Derived from document.el of Eric M. Ludlam <zappo@gnu.org>

(require 'document)
(require 'document-vars)

;;; Code:
(defcustom erlang-edoc-function-comment "
%b
%m @spec %F( %P ) -> Return
%m
%m       %T
%m @doc %f%p
%m
%m @end
%e
"
  "See `document-function-comment'"
  :group 'document
  :type 'string)

(defcustom erlang-edoc-record-comment "
%b
%m @type %F().
%m    <dl>
%m      %T
%m    </dl>
%m      %f%p
%m @end
%e
"
  "See `document-function-comment'"
  :group 'document
  :type 'string)

(defcustom erlang-edoc-type-spec "%P = %D"
  "Parameter type spec.
%P - align parameter name to longest,
%p - as is,
%D - description."
  :group 'semantic
  :type 'string)

(defcustom erlang-edoc-desc-spec "<dt>%P</dt><dd>%D</dd>"
  "Parameter description spec.
%P - align parameter name to longest,
%p - as is,
%D - description."
  :group 'semantic
  :type 'string)

(defsubst erlang-edoc--tag-name (nonterm)
  "Nonterminal name."
  (if (stringp nonterm) nonterm (semantic-tag-name nonterm)))

(defun erlang-edoc-inline ()
  "Document the current nonterminal with an inline comment."
  (interactive)
  (semantic-fetch-tags)
  (let ((ct (semantic-brute-find-tag-by-position (point) (current-buffer))))
    (erlang-edoc-insert-comment ct (current-buffer))))

(defun erlang-edoc-insert-comment-new (nonterm template)
  "Insert a new comment which explains the function found in NONTERM."
  (let ((pnt 0)
	(st 0)
	(zpnt 0)
	)
    ;; nonterm should always be correct.
    (goto-char (semantic-tag-start nonterm))
    (setq st (point))
    (insert (funcall template nonterm 'zpnt 'pnt))
    (goto-char (+ zpnt st))
    (message "Setting fill prefix to: \"%s\""
	     (setq fill-prefix
		   (concat (document-comment-line-prefix)
			   (make-string
			    (- (current-column)
			       (length (document-comment-line-prefix)))
			    ? ))))
    (goto-char (+ pnt st))
    (auto-fill-mode 1)
    ))

(defun erlang-edoc-insert-comment (nonterm buffer)
  "Insert mode-comment documentation about NONTERM from BUFFER."
  (let ((tt (semantic-tag-class nonterm)))
    (cond
     ((eq tt 'function)
      (erlang-edoc-insert-comment-new nonterm #'erlang-edoc--function-template)
      (message "Done..."))
      ((eq tt 'type)
       (erlang-edoc-insert-comment-new nonterm #'erlang-edoc--record-template)
       (message "Done..."))
      (t
      (error "Type %S is not yet managed by document `erlang-edoc-inline'" tt))
      )))

(defun erlang-edoc--function-template (nonterm pref-var focus-var)
  "Generate NONTERM function template for insertion."
  (let 	((fname (erlang-edoc--strip-arity (semantic-tag-name nonterm)))
	 (params (semantic-tag-function-arguments nonterm)))
	 (Sformat (list (list ?F fname)
			(list ?P (erlang-edoc--param-specs params))
			(list ?T '(lambda ()
				    (erlang-edoc--type-specs
				     params t)))
			(list ?f '(lambda ()
				    (set pref-var (Sformat-point)) ""))
			(list ?p '(lambda ()
				    (setq focus-var (Sformat-point)) ""))
			(list ?b (document-comment-start))
			(list ?m (document-comment-line-prefix))
			(list ?e (document-comment-end)))
		  erlang-edoc-function-comment)
	 ))

(defun erlang-edoc--record-template (nonterm pref-var focus-var)
  "Generate NONTERM record template for insertion."
  (let ((tname (semantic-tag-name nonterm))
	(params (semantic-tag-type-members nonterm)))
    (Sformat (list (list ?F tname)
		   (list ?T '(lambda ()
			       (erlang-edoc--type-specs
				params t erlang-edoc-desc-spec)))
		   (list ?f '(lambda () (set pref-var (Sformat-point)) ""))
		   (list ?p '(lambda () (set focus-var (Sformat-point)) ""))
		   (list ?b (document-comment-start))
		   (list ?m (document-comment-line-prefix))
		   (list ?e (document-comment-end)))
	     erlang-edoc-record-comment)
    ))

(defun erlang-edoc--strip-arity (tag-name)
  "Strip arity from TAG-NAME"
  ;;stripping arity
  (substring  tag-name 0 (string-match "/[0-9]+$" tag-name)))

(defun erlang-edoc--param-specs (params)
  "Parameters specification string for PARAMS"
  (apply 'concat (cons (erlang-edoc--tag-name (car params))
		       (mapcar (lambda (p)
				 (concat ", "
					 (erlang-edoc--tag-name p)))
			       (cdr params))))
  )

(defun erlang-edoc--type-specs (params &optional add-comment template)
  "Convert a parameter list PARAMS into a vertical list separated by =es."
  (let* ((tmpl (if template
		   template
		 erlang-edoc-type-spec))
	 (col (if Sformat-formatting (Sformat-column) (current-column)))
	 (newl params)
	 (longest (document-longest-name newl))
	 (newp ""))
    (while newl
      (let* ((n (car newl))
	     (nn (erlang-edoc--tag-name n))
	     (nc (if add-comment
		     (or (erlang-edoc--nonterm-comment n)
			 "undocumented")
		   ""))
	     )
	(let ((nextp (Sformat
		      (list (list ?P
				  (substring (concat
					      nn
					      "                   ")
					     0 longest))
			    (list ?D nc)
			    (list ?p n)
			    )
		      tmpl)))
	  (setq newp
		(concat
		 newp nextp
		 (concat "\n" (document-comment-line-prefix)
			 (make-string
			  (- col (length (document-comment-line-prefix)))
			  ? ))))))
      (setq newl (cdr newl)))
    (if (= (length newp) 0) "" newp)
    ))

(defun erlang-edoc--nonterm-comment (nonterm)
  "Extract inline comment for NONTERM."
  (cond ((stringp nonterm) nil)
	((not (semantic-tag-end nonterm)) nil)
	((not (semantic-tag-start nonterm)) nil)
	(t
	 (save-excursion
	   (goto-char (semantic-tag-start nonterm))
	   (let*
	       ((le (line-end-position))
		(ss (cond ((re-search-forward ",\\s-*" le t 1)
			   (match-end 0))
			  ((re-search-forward "%" le t 1)
			   (- (match-end 0) 1))
			  (t nil)))
		(str (if ss (buffer-substring-no-properties ss le) ""))
		(ds (progn
		      (and (string-match "^\\(\\s-*\\s<+\\)\\s-*" str)
			   (match-end 0))))
		(de (or (string-match "\\s-+$" str)
			(length str)))
		)
	     (cond ((not ds) nil)
		   ((<= de ds ) nil)
		   (t (let ((ret (substring str ds de)))
			(if (= (length ret) 0) nil ret)))
		   ))
	   ))))
	      
(provide 'erlang-edoc)
;;; erlang-edoc.el ends here

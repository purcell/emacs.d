;;; semantic-matlab.el --- Semantic details for MATLAB files

;;; Copyright (C) 2004, 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-matlab.el,v 1.4 2005/09/30 20:15:54 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Parse a MATLAB M file.  Uses constructs in matlab.el, available at
;; http://www.mathworks.com/matlabcentral/fileexchange/loadCategory.do?objectId=19&objectType=Category
;; or on the MATLAB distribution CD.
;;
;; The MATLAB language is pretty simple from a functional standpoint in that
;; you can only declare functions.  In addition, the language itself is not
;; expressable in a yacc style grammar.  It is therefore more expedient
;; to scan for regular expressions.

(require 'semantic)
(require 'semantic-format)
;; Allow a build without MATLAB installed.
(condition-case nil
    (require 'matlab)
  (error nil))

;;(eval-when-compile
;;  (require 'semanticdb)
;;  (require 'semanticdb-find)
;;  (require 'semantic-ctxt)
;;  (require 'semantic-imenu)
;;  (require 'semantic-doc)
;;  (require 'document)
;;  (require 'senator))

;; The version of this variable in MATLAB.el is not condusive to extracting
;; the information we need.
;;; Code:
(defvar semantic-matlab-match-function-re
  "\\(^\\s-*function\\b[ \t\n.]*\\)\\(.*\\)\\(\\sw+\\)("
  "Expression to match a function start line.
There are no reliable numeric matches in this expression.
Know that `match-end' of 0 is the end of the functin name.")

;; This function may someday be a part of matlab.el.
;; It does the raw scan and split for function tags.
(defun semantic-matlab-function-tags (&optional buffer)
  "Find all MATLAB function tags in BUFFER.
Return argument is:
  (START END RETURNVARS NAME ARGUMENTS)"
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((re semantic-matlab-match-function-re)
	  start ret fn arg end
	  (taglist nil)
	  )
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(setq start (match-beginning 0)
	      ret (buffer-substring-no-properties
		   (match-beginning 2) (match-end 2))
	      fn (buffer-substring-no-properties
		  (match-beginning 3) (match-end 3))
	      arg (buffer-substring-no-properties
		   (match-end 3) (save-excursion
				   (matlab-end-of-command)
				   (point)))
	      end (save-excursion
		    (goto-char start)
		    (if matlab-functions-have-end
			(matlab-forward-sexp)
		      (matlab-end-of-defun))
		    (point)))
	(setq taglist
	      (cons (list start end
			  (split-string ret "[][,=. \t\n]+")
			  fn
			  (split-string arg "[(), \n\t.]+")
			  )
		    taglist))
	)
      (nreverse taglist))))

;;; BEGIN PARSER
;;
(defun semantic-matlab-parse-region (&rest ignore)
  "Parse the current MATLAB buffer for function definitions.
IGNORE any arguments which specify a subregion to parse.
Each tag returned is a semantic FUNCTION tag.  See
`semantic-tag-new-function'."
  (mapcar 'semantic-matlab-expand-tag
	  (semantic-matlab-parse-functions)))

(defun semantic-matlab-parse-changes ()
  "Parse all changes for the current MATLAB buffer."
  ;; NOTE: For now, just schedule a full reparse.
  ;;       To be implemented later.
  (semantic-parse-tree-set-needs-rebuild))

(defun semantic-matlab-expand-tag (tag)
  "Expand the MATLAB function tag TAG."
  (let ((chil (semantic-tag-components-with-overlays tag)))
    (if chil
        (semantic-tag-put-attribute
         tag :members (mapcar 'semantic-matlab-expand-tag chil)))
    (car (semantic--tag-expand tag))))

(defun semantic-matlab-parse-functions ()
  "Parse all functions from the current MATLAB buffer."
  (car
   (semantic-matlab-sort-raw-tags (semantic-matlab-function-tags)
				  (point-max))
   ))

(defun semantic-matlab-sort-raw-tags (tag-list &optional end)
  "Return a split list of tags from TAG-LIST before END.
Return list is:
  (TAGS-BEFORE-END REMAINING-TAGS)"
  (let ((newlist nil)
	(rest tag-list))
    ;; Loop until there are no more tags, or no tags before END.
    (while (and tag-list (> end (car (car tag-list))))
      (let* ((tag (car tag-list))
	     (start (car tag))
	     (end (nth 1 tag))
	     (ret (nth 2 tag))
	     (name (nth 3 tag))
	     (args (nth 4 tag))
	     (parts (semantic-matlab-sort-raw-tags (cdr tag-list) end))
	     (chil (car parts)))
	(setq rest (car (cdr parts)))
	(setq newlist
	      (cons (append
		     (semantic-tag-new-function name nil args
						:return ret
						:subfunctions chil)
		     (list start end))
		    newlist))
	(setq tag-list rest)))
    (list (nreverse newlist) tag-list)))

(define-mode-local-override semantic-tag-components-with-overlays
  matlab-mode (tag)
  "Return the list of subfunctions in TAG."
  (semantic-tag-get-attribute tag :subfunctions))

;;;###autoload
(defun semantic-default-matlab-setup ()
  "Set up a buffer for parsing of MATLAB files."
  ;; This will use our parser.
  (semantic-install-function-overrides
   '((parse-region . semantic-matlab-parse-region)
     (parse-changes . semantic-matlab-parse-changes)))
  (setq semantic-parser-name "MATLAB"
        ;; Setup a dummy parser table to enable parsing!
        semantic--parse-table t
        imenu-create-index-function 'semantic-create-imenu-index
	;; semantic-command-separation-character "."
	semantic-type-relation-separator-character '(".")
	semantic-symbol->name-assoc-list '((function . "Function")
					   )
	semantic-imenu-expandable-tag-classes '(function)
	semantic-imenu-bucketize-file nil
	semantic-imenu-bucketize-type-members nil
	senator-step-at-start-end-tag-classes '(function)
	semantic-stickyfunc-sticky-classes '(function)
	)
  )

;; Enable this autoload once versions of matlab.el are synchronized and
;; generally available.
;;;###autoload
(add-hook 'matlab-mode-hook 'semantic-default-matlab-setup)

(provide 'semantic-matlab)

;;; semantic-matlab.el ends here

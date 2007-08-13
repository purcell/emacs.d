;;; semantic-ia-sb.el --- Speedbar analysis display interactor

;;; Copyright (C) 2002, 2003, 2004, 2006 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-ia-sb.el,v 1.18 2006/07/29 15:05:00 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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
;; Speedbar node for displaying derived context information.
;;

(require 'semantic-analyze)
(require 'speedbar)

;;; Code:
(defvar semantic-ia-sb-key-map nil
  "Keymap used when in semantic analysis display mode.")

(if semantic-ia-sb-key-map
    nil
  (setq semantic-ia-sb-key-map (speedbar-make-specialized-keymap))

  ;; Basic featuers.
  (define-key semantic-ia-sb-key-map "\C-m" 'speedbar-edit-line)
  (define-key semantic-ia-sb-key-map "I" 'semantic-ia-sb-show-tag-info)
  )

(defvar semantic-ia-sb-easymenu-definition
  '( "---"
;     [ "Expand" speedbar-expand-line nil ]
;     [ "Contract" speedbar-contract-line nil ]
     [ "Tag Information" semantic-ia-sb-show-tag-info t ]
     [ "Jump to Tag" speedbar-edit-line t ]
     [ "Complete" speedbar-edit-line t ]
     )
  "Extra menu items Analysis mode.")

;; Make sure our special speedbar major mode is loaded
(speedbar-add-expansion-list '("Analyze"
			       semantic-ia-sb-easymenu-definition
			       semantic-ia-sb-key-map
			       semantic-ia-speedbar))

(speedbar-add-mode-functions-list
 (list "Analyze"
       ;;'(speedbar-item-info . eieio-speedbar-item-info)
       '(speedbar-line-directory . semantic-ia-sb-line-path)))

;;;###autoload
(defun semantic-speedbar-analysis ()
  "Start Speedbar in semantic analysis mode.
The analyzer displays information about the current context, plus a smart
list of possible completions."
  (interactive)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into Analyze  mode on speedbar.
  (speedbar-change-initial-expansion-list "Analyze")
  )

(defun semantic-ia-speedbar (directory zero)
  "Create buttons in speedbar which define the current analysis at POINT.
DIRECTORY is the current directory, which is ignored, and ZERO is 0."
  (let ((analysis nil)
	(buffer nil)
	(completions nil)
	(fnargs nil)
	(cf (selected-frame))
	(cnt nil)
	(mode-local-active-mode nil)
	)
    ;; Try and get some sort of analysis
    (condition-case nil
	(progn
	  (speedbar-select-attached-frame)
	  (setq buffer (current-buffer))
	  (setq mode-local-active-mode major-mode)
	  (save-excursion
	    ;; We usd to cache the last analysis, but the analyzer
	    ;; now has a newer and improved analysis cache system.
	    (setq analysis (semantic-analyze-current-context (point)))
	    (setq cnt (semantic-find-tag-by-overlay))
	    (when analysis
	      (setq completions (semantic-analyze-possible-completions analysis))
	      (setq fnargs (semantic-get-local-arguments (point)))
	      )
	    ))
      (error nil))
    (select-frame cf)
    (set-buffer speedbar-buffer)
    ;; If we have something, do something spiff with it.
    (erase-buffer)
    (speedbar-insert-separator "Buffer/Function")
    ;; Note to self: Turn this into an expandable file name.
    (speedbar-make-tag-line 'bracket ?  nil nil
			    (buffer-name buffer)
			    nil nil 'speedbar-file-face 0)
    (when analysis
      ;; Now insert information about the context
      ;;     (insert "Context:\n")
      ;;     (speedbar-insert-button (object-name-string analysis)
      ;; 			    'speedbar-tag-face
      ;; 			    nil nil nil nil)
      (when cnt
	(semantic-ia-sb-string-list cnt
				    'speedbar-tag-face
				    'semantic-sb-token-jump))
      (when fnargs
	(speedbar-insert-separator "Arguments")
	(semantic-ia-sb-string-list fnargs
				    'speedbar-tag-face
				    'semantic-sb-token-jump))
      ;; Let different classes draw more buttons.
      (semantic-ia-sb-more-buttons analysis)
      (when completions
	(speedbar-insert-separator "Completions")
	(save-excursion
	  (semantic-ia-sb-completion-list completions
					  'speedbar-tag-face
					  'semantic-ia-sb-complete)))
      )))

(defmethod semantic-ia-sb-more-buttons ((context semantic-analyze-context))
  "Show a set of speedbar buttons specific to CONTEXT."
  (let ((localvars (oref context localvariables)))
    (when localvars
      (speedbar-insert-separator "Local Variables")
      (semantic-ia-sb-string-list localvars
				  'speedbar-tag-face
				  ;; This is from semantic-sb
				  'semantic-sb-token-jump)))
  (let ((prefix (oref context prefix)))
    (when prefix
      (speedbar-insert-separator "Prefix")
      (semantic-ia-sb-string-list prefix
				  'speedbar-tag-face
				  'semantic-sb-token-jump))
    ))

(defmethod semantic-ia-sb-more-buttons ((context semantic-analyze-context-assignment))
  "Show a set of speedbar buttons specific to CONTEXT."
  (call-next-method)
  (let ((assignee (oref context assignee)))
    (when assignee
      (speedbar-insert-separator "Assignee")
      (semantic-ia-sb-string-list assignee
				  'speedbar-tag-face
				  'semantic-sb-token-jump))))

(defmethod semantic-ia-sb-more-buttons ((context semantic-analyze-context-functionarg))
  "Show a set of speedbar buttons specific to CONTEXT."
  (call-next-method)
  (let ((func (oref context function)))
    (when func
      (speedbar-insert-separator "Function")
      (semantic-ia-sb-string-list func
				  'speedbar-tag-face
				  'semantic-sb-token-jump)
      ;; An index for the argument the prefix is in:
      (let ((arg (oref context argument)))
	(when arg
	  (speedbar-insert-separator
	   (format "Argument # %d" (oref context index)))
	  (semantic-ia-sb-string-list arg
				      'speedbar-tag-face
				      'semantic-sb-token-jump))))))

(defun semantic-ia-sb-string-list (list face function)
  "Create some speedbar buttons from LIST.
Each button will use FACE, and be activated with FUNCTION."
  (while list
    (let* ((usefn nil)
	   (string (cond ((stringp (car list))
			  (car list))
			 ((semantic-tag-p (car list))
			  (setq usefn (semantic-tag-with-position-p (car list)))
			  (semantic-format-tag-uml-concise-prototype (car list)))
			 (t "<No Tag>"))))
      (if (semantic-tag-p (car list))
	  (speedbar-make-tag-line 'angle ?i
				  'semantic-ia-sb-tag-info (car list)
				  string (if usefn function) (car list) face
				  0)
	(speedbar-make-tag-line 'statictag ??
				nil nil
				string (if usefn function) (car list) face
				0))
      (setq list (cdr list)))))
		 
(defun semantic-ia-sb-completion-list (list face function)
  "Create some speedbar buttons from LIST.
Each button will use FACE, and be activated with FUNCTION."
  (while list
    (let* ((documentable nil)
	   (string (cond ((stringp (car list))
			  (car list))
			 ((semantic-tag-p (car list))
			  (setq documentable t)
			  (semantic-format-tag-uml-concise-prototype (car list)))
			(t "foo"))))
      (if documentable
	  (speedbar-make-tag-line 'angle ?i
				  'semantic-ia-sb-tag-info
				  (car list)
				  string function (car list) face
				  0)
	(speedbar-make-tag-line 'statictag ?  nil nil
				string function (car list) face
				0))
      (setq list (cdr list)))))

(defun semantic-ia-sb-show-tag-info ()
  "Display information about the tag on the current line.
Same as clicking on the <i> button.
See `semantic-ia-sb-tag-info' for more."
  (interactive)
  (let ((tok nil))
    (save-excursion
      (end-of-line)
      (forward-char -1)
      (setq tok (get-text-property (point) 'speedbar-token)))
    (semantic-ia-sb-tag-info nil tok 0)))

(defun semantic-ia-sb-tag-info (text tag indent)
  "Display as much information as we can about tag.
Show the information in a shrunk split-buffer and expand
out as many details as possible.
TEXT, TAG, and INDENT are speedbar function arguments."
  (when (semantic-tag-p tag)
    (unwind-protect
	(let ((ob nil))
	  (speedbar-select-attached-frame)
	  (setq ob (current-buffer))
	  (with-output-to-temp-buffer "*Tag Information*"
	    ;; Output something about this tag:
	    (save-excursion
	      (set-buffer "*Tag Information*")
	      (goto-char (point-max))
	      (insert
	       (semantic-format-tag-prototype tag nil t)
	       "\n")
	      (let ((typetok
		     (condition-case nil
			 (save-excursion
			   (set-buffer ob)
			   (semantic-analyze-tag-type tag))
		       (error nil))))
		(if typetok
		    (insert (semantic-format-tag-prototype
			     typetok nil t))
		  ;; No type found by the analyzer
		  ;; The below used to try and select the buffer from the last
		  ;; analysis, but since we are already in the correct buffer, I
		  ;; don't think that is needed.
		  (let ((type (semantic-tag-type tag)))
		    (cond ((semantic-tag-p type)
			   (setq type (semantic-tag-name type)))
			  ((listp type)
			   (setq type (car type))))
		    (if (semantic-lex-keyword-p type)
			(setq typetok
			      (semantic-lex-keyword-get type 'summary))))
		  (if typetok
		      (insert typetok))
		  ))
	      ))
	  ;; Make it small
	  (shrink-window-if-larger-than-buffer
	   (get-buffer-window "*Tag Information*")))
      (select-frame speedbar-frame))))

(defun semantic-ia-sb-line-path (&optional depth)
  "Return the file name associated with DEPTH."
  (save-match-data
    (let* ((tok (speedbar-line-token))
	   (buff (if (semantic-tag-buffer tok)
		     (semantic-tag-buffer tok)
		   (current-buffer))))
      (buffer-file-name buff))))

(defun semantic-ia-sb-complete (text tag indent)
  "At point in the attached buffer, complete the symbol clicked on.
TEXT TAG and INDENT are the details."
  ;; Find the specified bounds from the current analysis.
  (speedbar-select-attached-frame)
  (unwind-protect
      (let* ((a (semantic-analyze-current-context (point)))
	     (bounds (oref a bounds))
	     (movepoint nil)
	     )
	(save-excursion
	  (if (and (<= (point) (cdr bounds)) (>= (point) (car bounds)))
	      (setq movepoint t))
	  (goto-char (car bounds))
	  (delete-region (car bounds) (cdr bounds))
	  (insert (semantic-tag-name tag))
	  (if movepoint (setq movepoint (point)))
	  ;; I'd like to use this to add fancy () or what not at the end
	  ;; but we need the parent file whih requires an upgrade to the
	  ;; analysis tool.
	  ;;(semantic-insert-foreign-tag tag ??))
	  )
	(if movepoint
	    (let ((cf (selected-frame)))
	      (speedbar-select-attached-frame)
	      (goto-char movepoint)
	      (select-frame cf))))
    (select-frame speedbar-frame)))

(provide 'semantic-ia-sb)

;;; semantic-ia-sb.el ends here

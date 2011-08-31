;;; semantic-ia.el --- Interactive Analysis functions

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-ia.el,v 1.29 2009/01/09 23:09:14 zappo Exp $

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
;; Interactive access to `semantic-analyze'.
;;
;; These routines are fairly simple, and show how to use the Semantic
;; analyzer to provide things such as completion lists, summaries,
;; locations, or documentation.
;;

(require 'senator)
(require 'semantic-analyze)
(require 'pulse)
(eval-when-compile
  (require 'semantic-analyze)
  (require 'semantic-analyze-refs))

;;; Code:

;;; COMPLETION
;;
;; This set of routines provides some simplisting completion
;; functions.

(defcustom semantic-ia-completion-format-tag-function
  'semantic-prototype-nonterminal
  "*Function used to convert a tag to a string during completion."
  :group 'semantic
  :type semantic-format-tag-custom-list)

(defvar semantic-ia-cache nil
  "Cache of the last completion request.
Of the form ( POINT . COMPLETIONS ) where POINT is a location in the
buffer where the completion was requested.  COMPLETONS is the list
of semantic tag names that provide logical completions from that
location.")
(make-variable-buffer-local 'semantic-ia-cache)

(defun semantic-ia-get-completions (context point)
  "Fetch the completion of CONTEXT at POINT.
Supports caching."
  ;; Cache the current set of symbols so that we can get at
  ;; them quickly the second time someone presses the
  ;; complete button.
  (let ((symbols
	 (if (and semantic-ia-cache
		  (= point (car semantic-ia-cache)))
	     (cdr semantic-ia-cache)
	   (semantic-analyze-possible-completions context))))
    ;; Set the cache
    (setq semantic-ia-cache (cons point symbols))
    symbols))

;;;###autoload
(defun semantic-ia-complete-symbol (point)
  "Complete the current symbol at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'."
  (interactive "d")
  ;; Calculating completions is a two step process.
  ;;
  ;; The first analyzer the current context, which finds tags
  ;; for all the stuff that may be references by the code around
  ;; POINT.
  ;;
  ;; The second step derives completions from that context.
  (let* ((a (semantic-analyze-current-context point))
	 (syms (semantic-ia-get-completions a point))
	 (pre (car (reverse (oref a prefix))))
	 )
    ;; If PRE was actually an already completed symbol, it doesn't
    ;; come in as a string, but as a tag instead.
    (if (semantic-tag-p pre)
	;; We will try completions on it anyway.
	(setq pre (semantic-tag-name pre)))
    ;; Complete this symbol.
    (if (null syms)
	(progn
	  ;(message "No smart completions found.  Trying senator-complete-symbol.")
	  (if (semantic-analyze-context-p a)
	      ;; This is a clever hack.  If we were unable to find any
	      ;; smart completions, lets divert to how senator derives
	      ;; completions.
	      ;;
	      ;; This is a way of making this fcn more useful since the
	      ;; smart completion engine sometimes failes.
	      (senator-complete-symbol)
	      ))
      ;; Use try completion to seek a common substring.
      (let ((tc (try-completion pre syms)))
	(if (and (stringp tc) (not (string= tc pre)))
	    (let ((tok (semantic-find-first-tag-by-name
			tc syms)))
	      ;; We have some new text.  Stick it in.
	      (delete-region (car (oref a bounds))
			     (cdr (oref a bounds)))
	      (goto-char (car (oref a bounds)))
	      (if tok
		  (semantic-ia-insert-tag tok)
		(insert tc)))
	  ;; We don't have new text.  Show all completions.
	  (goto-char (cdr (oref a bounds)))
	  (with-output-to-temp-buffer "*Completions*"
	    (display-completion-list
	     (mapcar semantic-ia-completion-format-tag-function syms))
	    ))))))

(defcustom semantic-ia-completion-menu-format-tag-function
  'semantic-uml-concise-prototype-nonterminal
  "*Function used to convert a tag to a string during completion."
  :group 'semantic
  :type semantic-format-tag-custom-list)

;;;###autoload
(defun semantic-ia-complete-symbol-menu (point)
  "Complete the current symbol via a menu based at POINT.
Completion options are calculated with `semantic-analyze-possible-completions'."
  (interactive "d")
  (let* ((a (semantic-analyze-current-context point))
	 (syms (semantic-ia-get-completions a point))
	 )
    ;; Complete this symbol.
    (if (not syms)
	(progn
	  (message "No smart completions found.  Trying Senator.")
	  (when (semantic-analyze-context-p a)
	    ;; This is a quick way of getting a nice completion list
	    ;; in the menu if the regular context mechanism fails.
	    (senator-completion-menu-popup)))

      (let* ((menu
	      (mapcar
	       (lambda (tag)
		 (cons
		  (funcall semantic-ia-completion-menu-format-tag-function tag)
		  (vector tag)))
	       syms))
	     (ans
	      (imenu--mouse-menu
	       ;; XEmacs needs that the menu has at least 2 items.  So,
	       ;; include a nil item that will be ignored by imenu.
	       (cons nil menu)
	       (senator-completion-menu-point-as-event)
	       "Completions")))
	(when ans
	  (if (not (semantic-tag-p ans))
	      (setq ans (aref (cdr ans) 0)))
	  (delete-region (car (oref a bounds)) (cdr (oref a bounds)))
	  (semantic-ia-insert-tag ans))
	))))

;;; COMPLETION HELPER
;;
;; This overload function handles inserting a tag
;; into a buffer for these local completion routines.
;;
;; By creating the functions as overloadable, it can be
;; customized.  For example, the default will put a paren "("
;; character after function names.  For Lisp, it might check
;; to put a "(" in front of a function name.

(define-overloadable-function semantic-ia-insert-tag (tag)
  "Insert TAG into the current buffer based on completion.")

(defun semantic-ia-insert-tag-default (tag)
  "Insert TAG into the current buffer based on completion."
  (insert (semantic-tag-name tag))
  (let ((tt (semantic-tag-class tag)))
    (cond ((eq tt 'function)
	   (insert "("))
	  (t nil))))

;;; Completions Tip
;;
;; This functions shows how to get the list of completions,
;; to place in a tooltip.  It doesn't actually do any completion.

;;;###autoload
(defun semantic-ia-complete-tip (point)
  "Pop up a tooltip for completion at POINT."
  (interactive "d")
  (let* ((a (semantic-analyze-current-context point))
	 (syms (semantic-ia-get-completions a point))
         (x (mod (- (current-column) (window-hscroll))
                 (window-width)))
         (y (save-excursion
              (save-restriction
                (widen)
                (narrow-to-region (window-start) (point))
                (goto-char (point-min))
                (1+ (vertical-motion (buffer-size))))))
	 (str (mapconcat #'semantic-tag-name
			 syms
			 "\n"))
	 )
    (cond ((fboundp 'x-show-tip)
	   (x-show-tip str
		       (selected-frame)
		       nil
		       nil
		       x y)
	   )
	  (t (message str))
	  )))

;;; Summary
;;
;; Like idle-summary-mode, this shows how to get something to
;; show a summary on.

;;;###autoload
(defun semantic-ia-show-summary (point)
  "Display a summary for the symbol under POINT."
  (interactive "P")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (when ctxt
	       ;; The CTXT is an EIEIO object.  The below
	       ;; method will attempt to pick the most interesting
	       ;; tag associated with the current context.
	       (semantic-analyze-interesting-tag ctxt)))
	)
    (when pf
      (message "%s" (semantic-format-tag-summarize pf nil t)))))

;;; FAST Jump
;;
;; Jump to a destination based on the local context.
;;
;; This shows how to use the analyzer context, and the
;; analyer references objects to choose a good destination.

(defun semantic-ia--fast-jump-helper (dest)
  "Jump to DEST, a Semantic tag.
This helper manages the mark, buffer switching, and pulsing."
  ;; We have a tag, but in C++, we usually get a prototype instead
  ;; because of header files.  Lets try to find the actual
  ;; implementaion instead.
  (when (semantic-tag-prototype-p dest)
    (let* ((refs (semantic-analyze-tag-references dest))
	   (impl (semantic-analyze-refs-impl refs t))
	   )
      (when impl (setq dest (car impl)))))

  ;; Make sure we have a place to go...
  (if (not (and (or (semantic-tag-with-position-p dest)
		    (semantic-tag-get-attribute dest :line))
		(semantic-tag-file-name dest)))
      (error "Tag %s has no buffer information"
	     (semantic-format-tag-name dest)))

  ;; Once we have the tag, we can jump to it.  Here
  ;; are the key bits to the jump:

  ;; 1) Push the mark, so you can pop global mark back, or
  ;;    use semantic-mru-bookmark mode to do so.
  (push-mark)
  (when (fboundp 'push-tag-mark)
    (push-tag-mark))
  ;; 2) Visits the tag.
  (semantic-go-to-tag dest)
  ;; 3) go-to-tag doesn't switch the buffer in the current window,
  ;;    so it is like find-file-noselect.  Bring it forward.
  (switch-to-buffer (current-buffer))
  ;; 4) Fancy pulsing.
  (pulse-momentary-highlight-one-line (point))
  )

;;;###autoload
(defun semantic-ia-fast-jump (point)
  "Jump to the tag referred to by the code at POINT.
Uses `semantic-analyze-current-context' output to identify an accurate
origin of the code at point."
  (interactive "d")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (and ctxt (reverse (oref ctxt prefix))))
	 ;; In the analyzer context, the PREFIX is the list of items
	 ;; that makes up the code context at point.  Thus the c++ code
	 ;; this.that().theothe
	 ;; would make a list:
	 ;; ( ("this" variable ..) ("that" function ...) "theothe")
	 ;; Where the first two elements are the semantic tags of the prefix.
	 ;;
	 ;; PF is the reverse of this list.  If the first item is a string,
	 ;; then it is an incomplete symbol, thus we pick the second.
	 ;; The second cannot be a string, as that would have been an error.
	 (first (car pf))
	 (second (nth 1 pf))
	 )
    (cond
     ((semantic-tag-p first)
      ;; We have a match.  Just go there.
      (semantic-ia--fast-jump-helper first))
     
     ((semantic-tag-p second)
      ;; Because FIRST failed, we should visit our second tag.
      ;; HOWEVER, the tag we actually want that was only an unfound
      ;; string may be related to some take in the datatype that belongs
      ;; to SECOND.  Thus, instead of visiting second directly, we
      ;; can offer to find the type of SECOND, and go there.
      (let ((secondclass (car (reverse (oref ctxt prefixtypes)))))
	(cond
	 ((and (semantic-tag-with-position-p secondclass)
	       (y-or-n-p (format "Could not find `%s'.  Jump to %s? "
				 first (semantic-tag-name secondclass))))
	  (semantic-ia--fast-jump-helper secondclass)
	  )
	 ;; If we missed out on the class of the second item, then
	 ;; just visit SECOND.
	 ((and (semantic-tag-p second)
	       (y-or-n-p (format "Could not find `%s'.  Jump to %s? "
				 first (semantic-tag-name second))))
	  (semantic-ia--fast-jump-helper second)
	  ))))

     ((semantic-tag-of-class-p (semantic-current-tag) 'include)
      ;; Just borrow this cool fcn.
      (semantic-decoration-include-visit)
      )

     (t
      (error "Could not find suitable jump point for %s"
	     first))
     )))

;;; DOC/DESCRIBE
;;
;; These routines show how to get additional information about a tag
;; for purposes of describing or showing documentation about them.
;;;###autoload
(defun semantic-ia-show-doc (point)
  "Display the code-level documentation for the symbol at POINT."
  (interactive "d")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (reverse (oref ctxt prefix)))
	 )
    ;; If PF, the prefix is non-nil, then the last element is either
    ;; a string (incomplete type), or a semantic TAG.  If it is a TAG
    ;; then we should be able to find DOC for it.
    (cond 
     ((stringp (car pf))
      (message "Incomplete symbol name."))
     ((semantic-tag-p (car pf))
      ;; The `semantic-documentation-for-tag' fcn is language
      ;; specific.  If it doesn't return what you expect, you may
      ;; need to implement something for your language.
      ;;
      ;; The default tries to find a comment in front of the tag
      ;; and then strings off comment prefixes.
      (let ((doc (semantic-documentation-for-tag (car pf))))
	(with-output-to-temp-buffer "*TAG DOCUMENTATION*"
	  (princ "Tag: ")
	  (princ (semantic-format-tag-prototype (car pf)))
	  (princ "\n")
	  (princ "\n")
	  (princ "Snarfed Documentation: ")
	  (princ "\n")
	  (princ "\n")
	  (if doc
	      (princ doc)
	    (princ "  Documentation unavailable."))
	  )))
     (t
      (message "Unknown tag.")))
    ))

;;;###autoload
(defun semantic-ia-describe-class (typename)
  "Display all known parts for the datatype TYPENAME.
If the type in question is a class, all methods and other accessible
parts of the parent classes are displayed."
  ;; @todo - use a fancy completing reader.
  (interactive "sType Name: ")

  ;; When looking for a tag of any name there are a couple ways to do
  ;; it.  The simple `semanticdb-find-tag-by-...' are simple, and
  ;; you need to pass it the exact name you want.
  ;; 
  ;; The analyzer function `semantic-analyze-tag-name' will take
  ;; more complex names, such as the cpp symbol foo::bar::baz,
  ;; and break it up, and dive through the namespaces.
  (let ((class (semantic-analyze-find-tag typename)))

    (when (not (semantic-tag-p class))
      (error "Cannot find class %s" class))
    (with-output-to-temp-buffer "*TAG DOCUMENTATION*"
      ;; There are many semantic-format-tag-* fcns.
      ;; The summarize routine is a fairly generic one.
      (princ (semantic-format-tag-summarize class))
      (princ "\n")
      (princ "  Type Members:\n")
      ;; The type tag contains all the parts of the type.
      ;; In complex languages with inheritance, not all the
      ;; parts are in the tag.  This analyzer fcn will traverse
      ;; the inheritance tree, and find all the pieces that
      ;; are inherited.
      (let ((parts (semantic-analyze-scoped-type-parts class)))
	(while parts
	  (princ "    ")
	  (princ (semantic-format-tag-summarize (car parts)))
	  (princ "\n")
	  (setq parts (cdr parts)))
	)
      )))

(provide 'semantic-ia)

;;; semantic-ia.el ends here

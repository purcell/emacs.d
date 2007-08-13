;;; eieio-opt.el -- eieio optional functions (debug, printing, speedbar)

;;; Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002, 2003, 2005 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-opt.el,v 1.26 2005/09/30 20:18:11 zappo Exp $
;; Keywords: OO, lisp
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
;; Please send bug reports, etc. to zappo@gnu.org

;;; Commentary:
;;
;;   This contains support functions to eieio.  These functions contain
;; some small class browser and class printing functions.
;;

(require 'eieio)

;;; Code:
;;;###autoload
(defun eieio-browse (&optional root-class)
  "Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'."
  (interactive (if current-prefix-arg
		   (list (read (completing-read "Class: "
						(eieio-build-class-alist)
						nil t)))
		 nil))
  (if (not root-class) (setq root-class 'eieio-default-superclass))
  (if (not (class-p root-class)) (signal 'wrong-type-argument (list 'class-p root-class)))
  (display-buffer (get-buffer-create "*EIEIO OBJECT BROWSE*") t)
  (save-excursion
    (set-buffer (get-buffer "*EIEIO OBJECT BROWSE*"))
    (erase-buffer)
    (goto-char 0)
    (eieio-browse-tree root-class "" "")
    ))

(defun eieio-browse-tree (this-root prefix ch-prefix)
  "Recursively, draws the children of the given class on the screen.
Argument THIS-ROOT is the local root of the tree.
Argument PREFIX is the character prefix to use.
Argument CH-PREFIX is another character prefix to display."
  (if (not (class-p (eval this-root))) (signal 'wrong-type-argument (list 'class-p this-root)))
  (let ((myname (symbol-name this-root))
	(chl (aref (class-v this-root) class-children))
	(fprefix (concat ch-prefix "  +--"))
	(mprefix (concat ch-prefix "  |  "))
	(lprefix (concat ch-prefix "     ")))
    (insert prefix myname "\n")
    (while (cdr chl)
      (eieio-browse-tree (car chl) fprefix mprefix)
      (setq chl (cdr chl)))
    (if chl
	(eieio-browse-tree (car chl) fprefix lprefix))
    ))

;;;###autoload
(defalias 'describe-class 'eieio-describe-class)
;;;###autoload
(defun eieio-describe-class (class)
  "Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect."
  (interactive (list (eieio-read-class "Class: ")))
  (with-output-to-temp-buffer "*Help*"
    (if (class-option class :abstract)
	(princ "Abstract "))
    (princ "Class ")
    (prin1 class)
    (terpri)
    ;; Inheritence tree information
    (let ((pl (class-parents class)))
      (when pl
	(princ " Inherits from ")
	(while pl
	  (princ "`") (prin1 (car pl)) (princ "'")
	  (setq pl (cdr pl))
	  (if pl (princ ", ")))
	(terpri)))
    (let ((ch (class-children class)))
      (when ch
	(princ " Children ")
	(while ch
	  (princ "`") (prin1 (car ch)) (princ "'")
	  (setq ch (cdr ch))
	  (if ch (princ ", ")))
	(terpri)))
    (terpri)
    ;; System documentation
    (let ((doc (documentation-property class 'variable-documentation)))
      (when doc
	(princ "Documentation:")
	(terpri)
	(princ doc)
	(terpri)
	(terpri)))
    ;; Describe all the slots in this class
    (eieio-describe-class-slots class)
    ;; Describe all the methods specific to this class.
    (let ((methods (eieio-all-generic-functions class))
	  (doc nil))
      (if (not methods) nil
	(princ "Specialized Methods:")
	(terpri)
	(terpri)
	(while methods
	  (setq doc (eieio-method-documentation (car methods) class))
	  (princ "`")
	  (prin1 (car methods))
	  (princ "'")
	  (if (not doc)
	      (princ "  Undocumented")
	    (if (car doc)
		(progn
		  (princ "  :STATIC ")
		  (prin1 (car (car doc)))
		  (terpri)
		  (princ (cdr (car doc)))))
	    (setq doc (cdr doc))
	    (if (car doc)
		(progn
		  (princ "  :BEFORE ")
		  (prin1 (car (car doc)))
		  (terpri)
		  (princ (cdr (car doc)))))
	    (setq doc (cdr doc))
	    (if (car doc)
		(progn
		  (princ "  :PRIMARY ")
		  (prin1 (car (car doc)))
		  (terpri)
		  (princ (cdr (car doc)))))
	    (setq doc (cdr doc))
	    (if (car doc)
		(progn
		  (princ "  :AFTER ")
		  (prin1 (car (car doc)))
		  (terpri)
		  (princ (cdr (car doc)))))
	    (terpri)
	    (terpri))
	  (setq methods (cdr methods)))))
    (buffer-string)))

(defun eieio-describe-class-slots (class)
  "Describe the slots in CLASS.
Outputs to the standard output."
  (let* ((cv (class-v class))
	 (docs   (aref cv class-public-doc))
	 (names  (aref cv class-public-a))
	 (deflt  (aref cv class-public-d))
	 (types  (aref cv class-public-type))
	 (i      0)
	 (prot   (aref cv class-protection))
	 )
    (princ "Instance Allocated Slots:")
    (terpri)
    (terpri)
    (while names
      (if (car prot) (princ "Private "))
      (princ "Slot: ")
      (prin1 (car names))
      (when (not (eq (aref types i) t))
	(princ "    type = ")
	(prin1 (aref types i)))
      (unless (eq (car deflt) eieio-unbound)
	(princ "    default = ")
	(prin1 (car deflt)))
      (when (car docs)
	(terpri)
	(princ "  ")
	(princ (car docs))
	(terpri))
      (terpri)
      (setq names (cdr names)
	    docs (cdr docs)
	    deflt (cdr deflt)
	    prot (cdr prot)
	    i (1+ i)))
    (setq docs  (aref cv class-class-allocation-doc)
	  names (aref cv class-class-allocation-a)
	  types (aref cv class-class-allocation-type)
	  i     0
	  prot  (aref cv class-class-allocation-protection))
    (when names
	(terpri)
	(princ "Class Allocated Slots:"))
	(terpri)
	(terpri)
    (while names
      (when (car prot)
	(princ "Private "))
      (princ "Slot: ")
      (prin1 (car names))
      (unless (eq (aref types i) t)
	(princ "    type = ")
	(prin1 (aref types i)))
      (when (car docs)
	(terpri)
	(princ "  ")
	(princ (car docs))
	(terpri))
      (terpri)
      (setq names (cdr names)
	    docs (cdr docs)
	    prot (cdr prot)
	    i (1+ i)))))

(defun eieio-build-class-alist (&optional class instantiable-only buildlist)
  "Return an alist of all currently active classes for completion purposes.
Optional argument CLASS is the class to start with.
If INSTANTIABLE-ONLY is non nil, only allow names of classes which
are not abstract, otherwise allow all classes.
Optional argument BUILDLIST is more list to attach and is used internally."
  (let* ((cc (or class eieio-default-superclass))
	 (sublst (aref (class-v cc) class-children)))
    (if (or (not instantiable-only) (not (class-abstract-p cc)))
	(setq buildlist (cons (cons (symbol-name cc) 1) buildlist)))
    (while sublst
      (setq buildlist (eieio-build-class-alist
		       (car sublst) instantiable-only buildlist))
      (setq sublst (cdr sublst)))
    buildlist))

(defvar eieio-read-class nil
  "History of the function `eieio-read-class' prompt.")

(defun eieio-read-class (prompt &optional histvar instantiable-only)
  "Return a class chosen by the user using PROMPT.
Optional argument HISTVAR is a variable to use as history.
If INSTANTIABLE-ONLY is non nil, only allow names of classes which
are not abstract."
  (intern (completing-read prompt (eieio-build-class-alist) nil t nil
			   (or histvar 'eieio-read-class))))

(defun eieio-read-subclass (prompt class &optional histvar instantiable-only)
  "Return a class chosen by the user using PROMPT.
CLASS is the base class, and completion occurs across all subclasses.
Optional argument HISTVAR is a variable to use as history.
If INSTANTIABLE-ONLY is non nil, only allow names of classes which
are not abstract."
  (intern (completing-read prompt
			   (eieio-build-class-alist class instantiable-only)
			   nil t nil
			   (or histvar 'eieio-read-class))))

;;; Collect all the generic functions created so far, and do cool stuff.
;;
;;;###autoload
(defalias 'describe-method 'eieio-describe-generic)
;;;###autoload
(defalias 'describe-generic 'eieio-describe-generic)
;;;###autoload
(defalias 'eieio-describe-method 'eieio-describe-generic)
;;;###autoload
(defun eieio-describe-generic (generic)
  "Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic."
  (interactive (list (eieio-read-generic "Generic Method: ")))
  (if (not (generic-p generic))
      (signal 'wrong-type-argument '(generic-p generic)))
  (with-output-to-temp-buffer "*Help*"
    (prin1 generic)
    (princ " is a generic function.")
    (terpri)
    (terpri)
    (let ((d (documentation generic)))
      (if (not d)
	  (princ "The generic is not documented.\n")
	(princ "Documentation:")
	(terpri)
	(princ d)
	(terpri)
	(terpri)))
    (princ "Implementations:")
    (terpri)
    (terpri)
    (let ((i 3)
	  (prefix [ ":STATIC" ":BEFORE" ":PRIMARY" ":AFTER" ] ))
      ;; Loop over fanciful generics
      (while (< i 6)
	(let ((gm (aref (get generic 'eieio-method-tree) i)))
	  (when gm
	    (princ "Generic ")
	    (princ (aref prefix (- i 3)))
	    (terpri)
	    (princ (or (nth 2 gm) "Undocumented"))
	    (terpri)
	    (terpri)))
	(setq i (1+ i)))
      (setq i 0)
      ;; Loop over defined class-specific methods
      (while (< i 3)
	(let ((gm (reverse (aref (get generic 'eieio-method-tree) i))))
	  (while gm
	    (princ "`")
	    (prin1 (car (car gm)))
	    (princ "'")
	    ;; prefix type
	    (princ " ")
	    (princ (aref prefix i))
	    (princ " ")
	    ;; argument list
	    (let* ((func (cdr (car gm)))
		   (arglst (eieio-lambda-arglist func)))
	      (prin1 arglst))
	    (terpri)
	    ;; 3 because of cdr
	    (princ (or (documentation (cdr (car gm)))
		       "Undocumented"))
	    (setq gm (cdr gm))
	    (terpri)
	    (terpri)))
	(setq i (1+ i))))
    (buffer-string)))

(defun eieio-lambda-arglist (func)
  "Return the argument list of FUNC, a function body."
  (if (symbolp func) (setq func (symbol-function func)))
  (if (byte-code-function-p func)
      (eieio-compiled-function-arglist func)
    (car (cdr func))))

(defun eieio-all-generic-functions (&optional class)
  "Return a list of all generic functions.
Optional CLASS argument returns only those functions that contain methods for CLASS."
  (let ((l nil) tree (cn (if class (symbol-name class) nil)))
    (mapatoms
     (lambda (symbol)
       (setq tree (get symbol 'eieio-method-obarray))
       (if tree
	   (progn
	     ;; A symbol might be interned for that class in one of
	     ;; these three slots in the method-obarray.
	     (if (or (not class)
		     (fboundp (intern-soft cn (aref tree 0)))
		     (fboundp (intern-soft cn (aref tree 1)))
		     (fboundp (intern-soft cn (aref tree 2))))
		 (setq l (cons symbol l)))))))
    l))

(defun eieio-method-documentation (generic class)
  "Return a list of the specific documentation of GENERIC for CLASS.
If there is not an explicit method for CLASS in GENERIC, or if that
function has no documentation, then return nil."
  (let ((tree (get generic 'eieio-method-obarray))
	(cn (symbol-name class))
	before primary after)
    (if (not tree)
	nil
      ;; A symbol might be interned for that class in one of
      ;; these three slots in the method-obarray.
      (setq before (intern-soft cn (aref tree 0))
	    primary (intern-soft cn (aref tree 1))
	    after (intern-soft cn (aref tree 2)))
      (if (not (or (fboundp before)
		   (fboundp primary)
		   (fboundp after)))
	  nil
	(list (if (fboundp before)
		  (cons (eieio-lambda-arglist before)
			(documentation before))
		nil)
	      (if (fboundp primary)
		  (cons (eieio-lambda-arglist primary)
			(documentation primary))
		nil)
	      (if (fboundp after)
		  (cons (eieio-lambda-arglist after)
			(documentation after))
		nil))))))

(defvar eieio-read-generic nil
  "History of the `eieio-read-generic' prompt.")

(defun eieio-read-generic-p (fn)
  "Function used in function `eieio-read-generic'.
This is because `generic-p' is a macro.
Argument FN is the function to test."
  (generic-p fn))

(defun eieio-read-generic (prompt &optional historyvar)
  "Read a generic function from the minibuffer with PROMPT.
Optional argument HISTORYVAR is the variable to use as history."
  (intern (completing-read prompt obarray 'eieio-read-generic-p
			   t nil (or historyvar 'eieio-read-generic))))

;;; Help system augmentation
;;
(defun eieio-help-mode-augmentation-maybee (&rest unused)
  "For buffers thrown into help mode, augment for eieio.
Arguments UNUSED are not used."
  ;; Scan created buttons so far if we are in help mode.
  (when (eq major-mode 'help-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((pos t) (inhibit-read-only t))
	(while pos
	  (if (get-text-property (point) 'help-xref) ; move off reference
	      (goto-char
	       (or (next-single-property-change (point) 'help-xref)
		   (point))))
	  (setq pos (next-single-property-change (point) 'help-xref))
	  (when pos
	    (goto-char pos)
	    (let* ((help-data (get-text-property (point) 'help-xref))
		   (method (car help-data))
		   (args (cdr help-data)))
	      (when (symbolp (car args))
		(cond ((class-p (car args))
		       (setcar help-data 'eieio-describe-class))
		      ((generic-p (car args))
		       (setcar help-data 'eieio-describe-generic))
		      (t nil))
		))))
	;; start back at the beginning, and highlight some sections
	(goto-char (point-min))
	(while (re-search-forward "^\\(Documentation\\|Implementations\\):$" nil t)
	    (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
	(goto-char (point-min))
	(if (re-search-forward "^Specialized Methods:$" nil t)
	    (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
	(goto-char (point-min))
	(while (re-search-forward "^\\(Instance\\|Class\\) Allocated Slots:$" nil t)
	    (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
	(goto-char (point-min))
	(while (re-search-forward ":\\(STATIC\\|BEFORE\\|AFTER\\|PRIMARY\\)" nil t)
	    (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
	(goto-char (point-min))
	(while (re-search-forward "^\\(Private \\)?Slot:" nil t)
	    (put-text-property (match-beginning 0) (match-end 0) 'face 'bold))
	))))

(defun eieio-help-augment-keymap ()
  "Augment the help keymap for cool EIEIO stuff."
  (define-key help-map "g" 'describe-generic)
  (define-key help-map "C" 'describe-class))

(if (and (boundp 'help-map) help-map)
    (eieio-help-augment-keymap)
  (eval-after-load 'help 'eieio-help-augment-keymap))

;;; How about showing the hierarchy in speedbar?  Cool!
;;
(eval-when-compile
  (condition-case nil
      (require 'speedbar)
    (error (message "Error loading speedbar... ignored."))))

(defvar eieio-class-speedbar-key-map nil
  "Keymap used when working with a project in speedbar.")

(defun eieio-class-speedbar-make-map ()
  "Make a keymap for eieio under speedbar."
  (setq eieio-class-speedbar-key-map (speedbar-make-specialized-keymap))

  ;; General viewing stuff
  (define-key eieio-class-speedbar-key-map "\C-m" 'speedbar-edit-line)
  (define-key eieio-class-speedbar-key-map "+" 'speedbar-expand-line)
  (define-key eieio-class-speedbar-key-map "-" 'speedbar-contract-line)
  )

(if eieio-class-speedbar-key-map
    nil
  (if (not (featurep 'speedbar))
      (add-hook 'speedbar-load-hook (lambda ()
				      (eieio-class-speedbar-make-map)
				      (speedbar-add-expansion-list
				       '("EIEIO"
					 eieio-class-speedbar-menu
					 eieio-class-speedbar-key-map
					 eieio-class-speedbar))))
    (eieio-class-speedbar-make-map)
    (speedbar-add-expansion-list '("EIEIO"
				   eieio-class-speedbar-menu
				   eieio-class-speedbar-key-map
				   eieio-class-speedbar))))

(defvar eieio-class-speedbar-menu
  ()
  "Menu part in easymenu format used in speedbar while in `eieio' mode.")

(defun eieio-class-speedbar (dir-or-object depth)
  "Create buttons in speedbar that represents the current project.
DIR-OR-OBJECT is the object to expand, or nil, and DEPTH is the current
expansion depth."
  (when (eq (point-min) (point-max))
    ;; This function is only called once, to start the whole deal.
    ;; Ceate, and expand the default object.
    (eieio-class-button eieio-default-superclass 0)
    (forward-line -1)
    (speedbar-expand-line)))

(defun eieio-class-button (class depth)
  "Draw a speedbar button at the current point for CLASS at DEPTH."
  (if (not (class-p class))
      (signal 'wrong-type-argument (list 'class-p class)))
  (let ((subclasses (aref (class-v class) class-children)))
    (if subclasses
	(speedbar-make-tag-line 'angle ?+
				'eieio-sb-expand
				class
				(symbol-name class)
				'eieio-describe-class-sb
				class
				'speedbar-directory-face
				depth)
      (speedbar-make-tag-line 'angle ?  nil nil
			      (symbol-name class)
			      'eieio-describe-class-sb
			      class
			      'speedbar-directory-face
			      depth))))

(defun eieio-sb-expand (text class indent)
  "For button TEXT, expand CLASS at the current location.
Argument INDENT is the depth of indentation."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (let ((subclasses (aref (class-v class) class-children)))
	       (while subclasses
		 (eieio-class-button (car subclasses) (1+ indent))
		 (setq subclasses (cdr subclasses)))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun eieio-describe-class-sb (text token indent)
  "Describe the class TEXT in TOKEN.
INDENT is the current indentation level."
  (speedbar-with-attached-buffer
   (eieio-describe-class token))
  (speedbar-maybee-jump-to-attached-frame))

(provide 'eieio-opt)

;;; eieio-opt.el ends here

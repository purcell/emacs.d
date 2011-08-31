;;; cedet-edebug.el --- Special EDEBUG augmentation code

;;;
;; Copyright (C) 2003, 2004, 2007, 2008 Eric M. Ludlam
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
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org

;;; Commentary:
;;
;; Some aspects of EDEBUG are not extensible.  It is possible to extend
;; edebug through other means, such as alias or advice, but those don't stack
;; very well when there are multiple tools trying to do the same sort of thing.
;;
;; This package provides a way to extend some aspects of edebug, such as value
;; printing.


;;; Code:
(defvar cedet-edebug-prin1-extensions
  nil
  "An alist of of code that can extend PRIN1 for edebug.
Each entry has the value: (CONDITION . PRIN1COMMAND).")

(defun cedet-edebug-prin1-recurse (object)
  "Recurse into OBJECT for prin1 on `cedet-edebug-prin1-to-string'."
  (concat "(" (mapconcat 'cedet-edebug-prin1-to-string object " ") ")"))

(defun cedet-edebug-rebuild-prin1 ()
  "Rebuild the function `cedet-edebug-prin1-to-string'.
Use the values of `cedet-edebug-prin1-extensions' as the means of
constructing the function."
  (interactive)
  (let ((c cedet-edebug-prin1-extensions)
	(code nil))
    (while c
      (setq code (append (list (list (car (car c))
				     (cdr (car c))))
			 code))
      (setq c (cdr c)))
    (fset 'cedet-edebug-prin1-to-string-inner
	  `(lambda (object &optional noescape)
	     "Display eieio OBJECT in fancy format.  Overrides the edebug default.
Optional argument NOESCAPE is passed to `prin1-to-string' when appropriate."
	     (cond
	      ,@(nreverse code)
	      (t (prin1-to-string object noescape)))))
    ))

(defun cedet-edebug-prin1-to-string (object &optional noescape)
  "CEDET version of `edebug-prin1-to-string' that adds specialty
print methods for very large complex objects."
  (if (not (fboundp 'cedet-edebug-prin1-to-string-inner))
      ;; Recreate the official fcn now.
      (cedet-edebug-rebuild-prin1))

  ;; Call the auto-generated version.
  ;; This is not going to be available at compile time.
  (cedet-edebug-prin1-to-string-inner object noescape))


(defun cedet-edebug-add-print-override (testfcn printfcn)
  "Add a new EDEBUG print override.
TESTFCN is a routine that returns nil if the first argument
passed to it is not to use PRINTFCN.
PRINTFCN accepts an object identified by TESTFCN and
returns a string.
New tests are always added to the END of the list of tests.
See `cedet-edebug-prin1-extensions' for the official list."
  (condition-case nil
      (add-to-list 'cedet-edebug-prin1-extensions
		   (cons testfcn printfcn)
		   t)
    (error ;; That failed, it must be an older version of Emacs
     ;; withouth the append argument for `add-to-list'
     ;; Doesn't handle the don't add twice case, but that's a
     ;; development thing and developers probably use new emacsen.
     (setq cedet-edebug-prin1-extensions
	   (append cedet-edebug-prin1-extensions
		   (list (cons testfcn printfcn))))))
  ;; whack the old implementation to force a rebuild.
  (fmakunbound 'cedet-edebug-prin1-to-string-inner))

;;; NOTE TO SELF.  Make this system used as an extension
;;; and then autoload the below.
;;;###autoload
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (require 'cedet-edebug)
	    ;; I suspect this isn't the best way to do this, but when
	    ;; cust-print was used on my system all my objects
	    ;; appeared as "#1 =" which was not useful.  This allows
	    ;; edebug to print my objects in the nice way they were
	    ;; meant to with `object-print' and `class-name'
	    (defalias 'edebug-prin1-to-string 'cedet-edebug-prin1-to-string)
	    ;; Add a fancy binding into EDEBUG's keymap for ADEBUG.
	    (define-key edebug-mode-map "A" 'data-debug-edebug-expr)
	    ))

;;; DEBUG MODE TOO
;; This seems like as good a place as any to stick this hack.
;;;###autoload
(add-hook 'debugger-mode-hook
	  (lambda ()
	    (require 'cedet-edebug)
	    ;; Add a fancy binding into the debug mode map for ADEBUG.
	    (define-key debugger-mode-map "A" 'data-debug-edebug-expr)
	    ))

(provide 'cedet-edebug)

;;; cedet-edebug.el ends here

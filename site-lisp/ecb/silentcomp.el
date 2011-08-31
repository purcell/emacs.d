;;; silentcomp.el --- compile time setup for proper compilation

;; Copyright (C) 2000 - 2005 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: bytecompiling
;; Created: 2002

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: silentcomp.el,v 1.5 2005/02/28 11:31:51 berndl Exp $

;;; Location

;; The latest version of this package is always available at
;; http://www.emacswiki.org/elisp/index.html

;;; Commentary:

;; This file provides features to defeat the compiler warnings for selected
;; symbols.
;;
;; This functionality ought to be provided by the byte compilers or some
;; accompanying library. To use it from some package "foo.el", begin by
;; putting the following code at the top of the file:
;;
;;   (eval-when-compile
;;       (require 'silentcomp))
;;
;; At the end of foo.el there should normally be a "(provide 'foo)". Replace
;; it with "(silentcomp-provide 'foo)"; that is necessary to restore the
;; environment after the byte compilation. If you don't have a `provide' at
;; the end, you have to add the following as the very last form in the file:
;;
;;   (eval-when-compile (silentcomp-restore-environment))
;;
;; Now everything is set to use the various macros in this package.
;;
;; To suppress byte compiler warnings, use the macros `silentcomp-defun',
;; `silentcomp-defvar'
;;
;;
;; Example:
;;
;; Suppose a file foo.el with the following code:
;;
;;     (defun nolog-message (&rest args)
;;       "Works exactly like `message' but does not log the message"
;;       (let ((msg (cond ((or (null args)
;;                             (null (car args)))
;;                         nil)
;;                        ((null (cdr args))
;;                         (car args))
;;                        (t
;;                         (apply 'format args)))))
;;         ;; Now message is either nil or the formated string.
;;         (if running-xemacs
;;             ;; XEmacs way of preventing log messages.
;;             (if msg
;;                 (display-message 'no-log msg)
;;               (clear-message 'no-log))
;;           ;; Emacs way of preventing log messages.
;;           (let ((message-log-max nil)
;;                 (message-truncate-lines nil))
;;             (if msg
;;                 (message "%s" msg)
;;               (message nil))))
;;         msg))
;;
;; foo.el should be work for both of Emacs and XEmacs, so we use the functions
;; `display-message' and `clear-message' for the XEmacs stuff and the
;; variables `message-log-max' and `message-truncate-lines' for Emacs. The
;; XEmacs-symbols are unbound in Emacs and vice versa. Therefore the
;; bytecompiler throws warnings about undefined symbols. These warnings can
;; savely be ignored but are nevertheless annoying and ugly especially if you
;; have a lot of symbols existing in only one version of (X)Emacs.
;;
;; With silentcomp.el you can silence the bytecompiler for foo.el:
;;
;;      ,----
;;      | foo.el
;;      |
;;      | (eval-when-compile
;;      |     (require 'silentcomp))
;;      | 
;;      | ;; XEmacs
;;      | (silentcomp-defun display-message)
;;      | (silentcomp-defun clear-message)
;;      | ;; Emacs
;;      | (silentcomp-defvar message-log-max)
;;      | (silentcomp-defvar message-truncate-lines)
;;      |
;;      | ;; Code
;;      | (defun nolog-message (&rest args)
;;      |    ...
;;      | )
;;      | 
;;      | (silentcomp-provide 'foo)
;;      | ;; foo.el ends here
;;      `----


;; This package is stolen from original cc-bytecomp.el from Martin Stjernholm
;; who is the maintainer of cc-mode.



;;; Code:

(defvar silentcomp-unbound-variables nil)
(defvar silentcomp-original-functions nil)
(defvar silentcomp-environment-set nil)

(defun silentcomp-restore-environment ()
  ;; Eval'ed during compilation to restore variables, functions etc
  ;; declared with `silentcomp-defvar' et al.
  (if (not load-in-progress)
      (let (p)
	(setq p silentcomp-unbound-variables)
	(while p
	  (let ((var (car p)))
	    (if (and (boundp var)
		     (eq (intern (concat "silentcomp-ignore-var:"
					 (symbol-name var)))
			 var))
		(makunbound var)))
	  (setq p (cdr p)))
	(setq p silentcomp-original-functions)
	(while p
	  (let ((fun (car (car p)))
		(def (car (cdr (cdr (car p))))))
	    (if (and (fboundp fun)
		     (eq (intern (concat "silentcomp-ignore-fun:"
					 (symbol-name fun)))
			 (symbol-function fun)))
		(if (eq def 'unbound)
		    (fmakunbound fun)
		  (fset fun def))))
	  (setq p (cdr p)))
	(setq silentcomp-environment-set nil))))

(defun silentcomp-is-compiling ()
  "Return non-nil if eval'ed during compilation.  Don't use outside
`eval-when-compile'."
  (and (boundp 'byte-compile-dest-file)
       (stringp byte-compile-dest-file)))

(defmacro silentcomp-defvar (var)
  "Binds the symbol as a variable during compilation of the file,
to silence the byte compiler.  Don't use within `eval-when-compile'."
  `(eval-when-compile
     (if (boundp ',var)
	 nil
       (if (not (memq ',var silentcomp-unbound-variables))
	   (setq silentcomp-unbound-variables
		 (cons ',var silentcomp-unbound-variables)))
       (if (and (silentcomp-is-compiling)
		(not load-in-progress))
	   (progn
	     (defvar ,var)
	     (set ',var (intern (concat "silentcomp-ignore-var:"
					(symbol-name ',var)))))))))

(defmacro silentcomp-defun (fun)
  "Bind the symbol as a function during compilation of the file,
to silence the byte compiler.  Don't use within `eval-when-compile'.

If the symbol already is bound as a function, it will keep that
definition.  That means that this macro will not shut up warnings
about incorrect number of arguments.  It's dangerous to try to replace
existing functions since the byte compiler might need the definition
at compile time, e.g. for macros and inline functions."
  `(eval-when-compile
     (if (not (assq ',fun silentcomp-original-functions))
	 (setq silentcomp-original-functions
	       (cons (list ',fun
			   nil
			   (if (fboundp ',fun)
			       (symbol-function ',fun)
			     'unbound))
		     silentcomp-original-functions)))
     (if (and (silentcomp-is-compiling)
	      (not load-in-progress)
	      (not (fboundp ',fun)))
	 (fset ',fun (intern (concat "silentcomp-ignore-fun:"
				     (symbol-name ',fun)))))))

(defmacro silentcomp-provide (feature)
  "A replacement for the `provide' form that restores the environment
after the compilation.  Don't use within `eval-when-compile'."
  `(progn
     (eval-when-compile (silentcomp-restore-environment))
     (provide ,feature)))

(provide 'silentcomp)

;;; silentcomp.el ends here

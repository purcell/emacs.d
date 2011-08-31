;;; ede-ede-grammar.el --- EDE support for Semantic Grammar Files

;;;  Copyright (C) 2003, 2004, 2007, 2008, 2009  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: semantic-ede-grammar.el,v 1.16 2009/01/31 13:09:28 zappo Exp $

;; This software is free software; you can redistribute it and/or modify
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
;; Handle .by or .wy files.

(require 'ede-proj)
(require 'ede-pmake)
(require 'ede-pconf)
(require 'ede-proj-elisp)

;;; Code:
;;;###autoload
(defclass semantic-ede-proj-target-grammar (ede-proj-target-makefile)
  ((menu :initform nil)
   (keybindings :initform nil)
   (phony :initform t)
   (sourcetype :initform
	       (semantic-ede-source-grammar-wisent
		semantic-ede-source-grammar-bovine
		))
   (availablecompilers :initform
		       (semantic-ede-grammar-compiler-wisent
			semantic-ede-grammar-compiler-bovine
			))
   )
  "This target consists of a group of grammar files.
A grammar target consists of grammar files that build Emacs Lisp programs for
parsing different languages.")

(defvar semantic-ede-source-grammar-wisent
  (ede-sourcecode "semantic-ede-grammar-source-wisent"
		  :name "Wisent Grammar"
		  :sourcepattern "\\.wy$"
		  )
  "Semantic Grammar source code definition for wisent.")

(defclass semantic-ede-grammar-compiler-class (ede-compiler)
  nil
  "Specialized compiler for semantic grammars.")

(defvar semantic-ede-grammar-compiler-wisent
  (semantic-ede-grammar-compiler-class
   "ede-emacs-wisent-compiler"
   :name "emacs"
   :variables '(("EMACS" . "emacs"))
   :commands
   '(
     "@echo \"(add-to-list 'load-path nil)\" > grammar-make-script"
     "@for loadpath in . ${LOADPATH}; do \\"
     "   echo \"(add-to-list 'load-path \\\"$$loadpath\\\")\" >> grammar-make-script; \\"
     "done;"
     "@echo \"(require 'semantic-load)\" >> grammar-make-script"
     "@echo \"(require 'semantic-grammar)\" >> grammar-make-script"
     ;; "@echo \"(setq debug-on-error t)\" >> grammar-make-script"
     "\"$(EMACS)\" -batch --no-site-file -l grammar-make-script -f semantic-grammar-batch-build-packages $^"
     )
   ;; :autoconf '("AM_PATH_LISPDIR")
   :sourcetype '(semantic-ede-source-grammar-wisent)
   :objectextention "-wy.elc"
   )
  "Compile Emacs Lisp programs.")


(defvar semantic-ede-source-grammar-bovine
  (ede-sourcecode "semantic-ede-grammar-source-bovine"
		  :name "Bovine Grammar"
		  :sourcepattern "\\.by$"
		  )
  "Semantic Grammar source code definition for the bovinator.")

(defvar semantic-ede-grammar-compiler-bovine
  (semantic-ede-grammar-compiler-class
   "ede-emacs-wisent-compiler"
   :name "emacs"
   :variables '(("EMACS" . "emacs"))
   :commands
   '(
     "@echo \"(add-to-list 'load-path nil)\" > grammar-make-script"
     "@for loadpath in . ${LOADPATH}; do \\"
     "   echo \"(add-to-list 'load-path \\\"$$loadpath\\\")\" >> grammar-make-script; \\"
     "done;"
     "@echo \"(require 'semantic-load)\" >> grammar-make-script"
     "@echo \"(require 'semantic-grammar)\" >> grammar-make-script"
     ;; "@echo \"(setq debug-on-error t)\" >> grammar-make-script"
     "\"$(EMACS)\" -batch --no-site-file -l grammar-make-script -f semantic-grammar-batch-build-packages $^"
     )
   ;; :autoconf '("AM_PATH_LISPDIR")
   :sourcetype '(semantic-ede-source-grammar-bovine)
   :objectextention "-by.elc"
   )
  "Compile Emacs Lisp programs.")

(require 'semantic-grammar)

;;; Target options.
(defmethod ede-buffer-mine ((this semantic-ede-proj-target-grammar) buffer)
  "Return t if object THIS lays claim to the file in BUFFER.
Lays claim to all -by.el, and -wy.el files."
  ;; We need to be a little more careful than this, but at the moment it
  ;; is common to have only one target of this class per directory.
  (if (string-match "-[bw]y\\.elc?$" (buffer-file-name buffer))
      t
    (call-next-method) ; The usual thing.
    ))

(defmethod project-compile-target ((obj semantic-ede-proj-target-grammar))
  "Compile all sources in a Lisp target OBJ."
  (let* ((cb (current-buffer))
	 (proj (ede-target-parent obj))
	 (default-directory (oref proj directory)))
    (mapc (lambda (src)
	    (save-excursion
	      (set-buffer (find-file-noselect src))
	      (semantic-grammar-create-package)
	      (save-buffer)
	      (let ((cf (concat (semantic-grammar-package) ".el")))
		(if (or (not (file-exists-p cf))
			(file-newer-than-file-p src cf))
		    (byte-compile-file cf)))))
	    (oref obj source)))
  (message "All Semantic Grammar sources are up to date in %s" (object-name obj)))

;;; Makefile generation functions
;;
(defmethod ede-proj-makefile-sourcevar ((this semantic-ede-proj-target-grammar))
  "Return the variable name for THIS's sources."
  (cond ((ede-proj-automake-p)
	 (error "No Automake support for Semantic Grammars"))
	(t (concat (ede-pmake-varname this) "_SEMANTIC_GRAMMAR"))))

(defmethod ede-proj-makefile-insert-variables :AFTER ((this semantic-ede-proj-target-grammar))
  "Insert variables needed by target THIS."
  (ede-proj-makefile-insert-loadpath-items
   (ede-proj-elisp-packages-to-loadpath
    (list "eieio" "semantic" "inversion" "ede")))
  ;; eieio for object system needed in ede
  ;; semantic because it is
  ;; Inversion for versioning system.
  ;; ede for project regeneration
  (ede-pmake-insert-variable-shared
      (concat (ede-pmake-varname this) "_SEMANTIC_GRAMMAR_EL")
    (insert
     (mapconcat (lambda (src)
		  (save-excursion
		    (set-buffer (find-file-noselect src))
		    (concat (semantic-grammar-package) ".el")))
		(oref this source)
		" ")))
  )

(defmethod ede-proj-makefile-insert-rules ((this semantic-ede-proj-target-grammar))
  "Insert rules needed by THIS target."
  ;; Add in some dependencies.
;;  (mapc (lambda (src)
;;	  (let ((nm (file-name-sans-extension src)))
;;	    (insert nm "-wy.el: " src "\n"
;;		    nm "-wy.elc: " nm "-wy.el\n\n")
;;	    ))
;;	(oref this source))
  ;; Call the normal insertion of rules.
  (call-next-method)
  )

(defmethod ede-proj-makefile-insert-dist-dependencies ((this semantic-ede-proj-target-grammar))
  "Insert dist dependencies, or intermediate targets.
This makes sure that all grammar lisp files are created before the dist
runs, so they are always up to date.
Argument THIS is the target that should insert stuff."
  (call-next-method)
  (insert " $(" (ede-pmake-varname this) "_SEMANTIC_GRAMMAR_EL)")
  )

;;;###autoload
(autoload 'ede-proj-target-elisp "semantic-ede-proj-target-grammar"
  "Target class for Emacs/Semantic grammar files." nil nil)

(ede-proj-register-target "semantic grammar"
			  semantic-ede-proj-target-grammar)

(provide 'semantic-ede-grammar)

;;;###autoload
(eval-after-load "ede-proj"
    (quote
     (require 'semantic-ede-grammar)
     ))

;;; semantic-ede-grammar.el ends here

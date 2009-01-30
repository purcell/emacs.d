;;; ede-locate.el --- Locate support

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: ede-locate.el,v 1.6 2009/01/28 19:38:20 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Support for various LOCATE type functions.
;;
;; A key feature of EDE is `ede-expand-filename', which allows a
;; project to expand a filename reference in one file to some actual
;; filename.
;;
;; In that way, you may #include <foo.h>, and without knowing how to
;; read a Makefile, find it in <root>/include/foo.h.
;;
;; Some projects are regular, such as the Emacs project.  Some
;; projects are completely controlled by EDE, such sh the Project.ede
;; based projects.
;;
;; For other projects, have a "quick hack" to support these location
;; routines is handy.
;;
;; The baseclass `ede-locate-base' provides the abstract interface to
;; finding files in a project.
;;
;; New location routines will subclass `ede-locate-base'.
;;
;; How to use:
;;
;; Configure `ede-locate-setup-options' to add the types of locate
;; features you have available.  EDE will then enable the correct one
;; when it is available.

(require 'ede)
(eval-when-compile (require 'data-debug)
		   (require 'eieio-datadebug)
		   (require 'cedet-global))

;; Older [X]Emacs don't have locate
(condition-case nil
    (require 'locate)
  (error nil))

;;; Code:
(defcustom ede-locate-setup-options
  '(ede-locate-base)
  "List of locate objects to try out by default.
Listed in order of preference.  If the first item cannot be used in
a particular project, then the next one is tried.
It is always assumed that `ede-locate-base' is at end of the list."
  :group 'ede
  :type '(repeat
	  (choice (const :tag "None" ede-locate-base)
		  (const :tag "locate" ede-locate-locate)
		  (const :tag "GNU Global" ede-locate-global)))
  )

;;;###autoload
(defun ede-enable-locate-on-project (&optional project)
  "Enable an EDE locate feature on PROJECT.
Attempt to guess which project locate style to use
based on `ede-locate-setup-options'."
  (interactive)
  (let* ((proj (or project (ede-toplevel)))
	 (root (ede-project-root-directory proj))
	 (opts ede-locate-setup-options)
	 (ans nil))
    (while (and opts (not ans))
      (when (ede-locate-ok-in-project (car opts) root)
	;; If interactive, check with the user.
	(when (interactive-p)
	  (when (or (eq (car opts) ede-locate-base)
		    (y-or-n-p (format "Set project locator to %s? " (car opts))))
	    (setq ans (car opts)))))
      (setq opts (cdr opts)))
    ;; No match?  Always create the baseclass for the hashing tool.
    (when (not ans)
      (setq ans 'ede-locate-base))
    (oset proj locate-obj (make-instance ans "Loc" :root root))
    (when (interactive-p)
      (message "Satting locator to %s." ans))
    ))

;;; LOCATE BASECLASS
;;
;; The baseclass for all location style queries.
(defclass ede-locate-base ()
  ((root :initarg :root
	 :documentation
	 "The root of these locat searches.")
   (file :documentation
	 "The last file search for with EDE locate.")
   (lastanswer :documentation
	      "The last answer provided by the locator.")
   (hash :documentation
	 "Hash table of previously found files.")
   )
  "Baseclass for LOCATE feature in EDE.")

(defmethod initialize-instance ((loc ede-locate-base) &rest fields)
  "Make sure we have a hash table."
  ;; Basic setup.
  (call-next-method)
  ;; Make sure we have a hash table.
  (oset loc hash (make-hash-table :test 'equal))
  )

(defmethod ede-locate-ok-in-project :static ((loc ede-locate-base)
					     root)
  "Is it ok to use this project type under ROOT."
  t)

(defmethod ede-locate-file-in-hash ((loc ede-locate-base)
				    filestring)
  "For LOC, is the file FILESTRING in our hashtable?"
  (gethash filestring (oref loc hash)))

(defmethod ede-locate-add-file-to-hash ((loc ede-locate-base)
					filestring fullfilename)
  "For LOC, add FILESTR to the hash with FULLFILENAME."
  (puthash filestring fullfilename (oref loc hash)))

(defmethod ede-locate-file-in-project ((loc ede-locate-base)
				       filesubstring
				       )
  "Locate with LOC occurances of FILESUBSTRING.
Searches are done under the current root of the EDE project
that crated this ede locat object."
  (let ((ans (ede-locate-file-in-project-impl loc filesubstring))
	)
    (oset loc file filesubstring)
    (oset loc lastanswer ans)
    ans))

(defmethod ede-locate-file-in-project-impl ((loc ede-locate-base)
					    filesubstring
					    )
  "Locate with LOC occurances of FILESUBSTRING.
Searches are done under the current root of the EDE project
that crated this ede locat object."
  nil
  )

;;; LOCATE
;;
;; Using the standard unix "locate" command.
;; Since locate is system wide, we need to hack the search
;; to restrict it to within just this project.

(defclass ede-locate-locate (ede-locate-base)
  ()
  "EDE Locator using the locate command.
Configure the Emacs `locate-program' variable to also
configure the use of EDE locate.")

(defmethod ede-locate-ok-in-project :static ((loc ede-locate-locate)
					     root)
  "Is it ok to use this project type under ROOT."
  (or (featurep 'locate) (locate-library "locate"))
  )

(defmethod ede-locate-file-in-project-impl ((loc ede-locate-locate)
					    filesubstring)
  "Locate with LOC occurances of FILESUBSTRING under PROJECTROOT.
Searches are done under the current root of the EDE project
that crated this ede locat object."
  ;; We want something like:
  ;;  /my/project/root*/filesubstring.c
  (let* ((searchstr (concat (directory-file-name (oref loc root))
			    "*/" filesubstring))
	 (b (get-buffer-create "*LOCATE*"))
	 (cd default-directory)
	 )
    (save-excursion
      (set-buffer b)
      (setq default-directory cd)
      (erase-buffer))
    (require 'locate)
    (apply 'call-process locate-command
	   nil b nil
	   searchstr nil)
    (save-excursion
      (set-buffer b)
      (split-string (buffer-string) "\n" t))
    )
  )

;;; GLOBAL
;;
(defclass ede-locate-global (ede-locate-base)
  ()
  "EDE Locator using GNU Global.
Configure EDE's use of GNU Global through the cedet-global.el
variable `cedet-global-command'.")

(defmethod initialize-instance ((loc ede-locate-global)
				&rest slots)
  "Make sure that we can use GNU Global."
  ;; Get ourselves initialized.
  (call-next-method)
  ;; Do the checks.
  (cedet-gnu-global-version-check)
  (let* ((default-directory (oref loc root))
	 (root (cedet-gnu-global-root)))
    (when (not root)
      (error "Cannot use GNU Global in %s"
	     (oref loc root))))
  )

(defmethod ede-locate-ok-in-project :static ((loc ede-locate-global)
					     root)
  "Is it ok to use this project type under ROOT."
  (cedet-gnu-global-version-check)
  (let* ((default-directory root)
	 (newroot (cedet-gnu-global-root)))
    newroot))

(defmethod ede-locate-file-in-project-impl ((loc ede-locate-global)
					    filesubstring)
  "Locate with LOC occurances of FILESUBSTRING under PROJECTROOT.
Searches are done under the current root of the EDE project
that crated this ede locat object."
  (let ((default-directory (oref loc root))
	(ans (cedet-gnu-global-expand-filename filesubstring))
	)

    ans))

;;; TESTS
;;
;; Some testing routines.
(defun ede-locate-test-locate (file)
  "Test EDE Locate on FILE using LOCATE type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-locate
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

(defun ede-locate-test-global (file)
  "Test EDE Locate on FILE using LOCATE type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-global
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

(provide 'ede-locate)
;;; ede-locate.el ends here

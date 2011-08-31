;;; cedet-build.el --- Build CEDET within Emacs.

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-build.el,v 1.8 2009/02/05 01:30:16 zappo Exp $

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
;; Build all the CEDET parts interactively through EDE.
;;
;; NOTE: This does not support XEmacs, which cannot use
;;       `batch-update-autoloads' in interactive mode.
;;
;;; USAGE:
;;
;; Step 1:  Compile CEDET in a fresh Emacs:
;;
;;     emacs -Q -l cedet-build.el -f cedet-build
;;     
;;       or, if -Q isn't supported
;;
;;     emacs -q --no-site-file -l cedet-build.el -f cedet-build
;;
;;   or
;;
;;     Eval this buffer and then start compilation:
;;
;;     M-x eval-buffer
;;     M-x cedet-build-in-default-emacs
;;
;;   or
;;
;;     if this is an incremental build, you can do:
;;
;;     M-x eval-buffer
;;     M-x cedet-build-in-this-emacs
;;
;;     If EIEIO needs recompile, it will switch to compiling in
;;     a subprocess with `cedet-build-in-default-emacs'.
;;
;; Step 2: Check Output.
;;
;;   If Compilation of grammars exceeds Emacs' stack size, exit Emacs,
;;   and re-run the compilation steps above.  Once most of CEDET is
;;   compiled, this problem goes away.


;;; Code:

(defvar cedet-build-location
  (let ((dir (file-name-directory
	      (or load-file-name (buffer-file-name)))))
    ;; (add-to-list 'load-path dir)
    dir)
  "Root of the CEDET tree.")

(defun cedet-build-in-default-emacs()
  "Build CEDET in a new Emacs instance started with -Q."
  (interactive)
  (let ((default-directory cedet-build-location))
    (call-process (expand-file-name invocation-name invocation-directory)
		  nil 0 nil
                  "-Q" "-l" "cedet-build.el" "-f" "cedet-build")
    (message "Started new Emacs instance to build CEDET ...")))

(defun cedet-build-in-this-emacs ()
  "Build CEDET in this version of Emacs.
This only works if EIEIO does not need to be compiled."
  (interactive)
  (let ((src "eieio/eieio.el") (dst "eieio/eieio.elc"))
    (if (file-newer-than-file-p src dst)
	(when (y-or-n-p "EIEIO needs to be recompiled.  Use subprocess? ")
	  (cedet-build-in-default-emacs))
      (cedet-build t))))

(defun cedet-build-msg (fmt &rest args)
  "Show a build message."
  (if noninteractive
      (princ (apply 'format fmt args) t)
    (switch-to-buffer "*CEDET BYTECOMPILE*" t)
    (goto-char (point-max))
    (insert (apply 'format fmt args))
    (sit-for 0)))

(defun cedet-build (&optional override-check)
  "Build CEDET via EDE.
OVERRIDE-CHECK to override cedet short-cicuit."
  (setq inhibit-splash-screen t)

  ;; Make sure CEDET is not loaded
  (if (and (not override-check) (featurep 'cedet))
      (error "To use cedet-build, start Emacs with -q"))

  ;; Setup a logging buffer
  (switch-to-buffer "*CEDET BYTECOMPILE*")
  (delete-other-windows)
  (erase-buffer)
  (cedet-build-msg "CEDET BYTE COMPILATION STATUS:\n\n")
  (cedet-build-msg "Step 1: Byte compile EIEIO...")

  ;; Get EIEIO built first.
  (save-excursion
    (load-file "common/inversion.el")
    (load-file "eieio/eieio-comp.el")
    (let ((src "eieio/eieio.el") (dst "eieio/eieio.elc"))
      (if (file-newer-than-file-p src dst)
	  (progn
	    (when (featurep 'eieio)
	      (error "You should not recompile EIEIO after it has been loaded"))
	    (byte-compile-file src)
	    (cedet-build-msg "done\n"))
	(cedet-build-msg "not needed\n")))
    )

  (load-file "common/cedet-autogen.el")

  ;; Get EDE autoloads built...
  (cedet-build-msg "Step 2: EDE Autloads...")
  (save-excursion
    (let ((default-directory (expand-file-name "ede")))
      (cedet-update-autoloads "ede-loaddefs.el" ".")))
  (cedet-build-msg "done.\n")

  ;; Get Semantic autoloads built...
  (cedet-build-msg "Step 3: Semantic Autloads...")
  (save-excursion
    (let ((default-directory (expand-file-name "semantic")))
      (cedet-update-autoloads "semantic-loaddefs.el" "." "bovine" "wisent")))
  (cedet-build-msg "done.\n")

  ;; Get SRecode autoloads built...
  (cedet-build-msg "Step 4: SRecode Autloads...")
  (save-excursion
    (let ((default-directory (expand-file-name "srecode")))
      (cedet-update-autoloads "srecode-loaddefs.el" ".")))
  (cedet-build-msg "done.\n")

  ;; Fire up CEDET and EDE
  (cedet-build-msg "Step 5: Load common/cedet.el ...")
  (save-excursion
    (load-file (expand-file-name "common/cedet.el" cedet-build-location)))

  (cedet-build-msg "done\nStep 6: Turning on EDE ...")
  (save-excursion
    (global-ede-mode 1)
    (require 'semantic-ede-grammar)
    (require 'wisent))
  (cedet-build-msg "done.\n\n")

  ;; Load in the Makefile
  (let ((buf (get-buffer-create "CEDET MAKE"))
	(pkgs nil)
	(subdirs nil)
	)
    (cedet-build-msg "Step 7: Scan Makefile for targets...")
    (save-excursion
      (set-buffer buf)
      (insert-file-contents "Makefile" nil)
      (goto-char (point-min))
      (re-search-forward "CEDET_PACKAGES\\s-*=\\s-*\\\\\n")
      (while (looking-at "\\(\\w+\\)\\s-*\\\\?\n")
	(setq subdirs (cons (buffer-substring-no-properties
			     (match-beginning 1) (match-end 1))
			    subdirs))
	(end-of-line)
	(forward-char 1))
      (setq subdirs (nreverse subdirs))
      )
    (cedet-build-msg "%S\n\n" subdirs)

    (cedet-build-msg "Build Emacs Lisp Targets:\n-------------------\n")
    (dolist (d subdirs)
      ;; For each directory, get the project, and then targets
      ;; and run a build on them.
      (cedet-build-msg "Building project %s\n" d)

      (let ((Tproj (ede-current-project (file-name-as-directory
					 (expand-file-name
					  d cedet-build-location))))
	    )
	(dolist (proj (cons Tproj (oref Tproj subproj)))
	  (cedet-build-msg "  Project: %s\n" (object-name-string proj))
	  (dolist (targ (oref proj targets))
	    (when (and (or (ede-proj-target-elisp-p targ)
			   (ede-proj-target-elisp-autoloads-p targ)
			   (semantic-ede-proj-target-grammar-p targ))
		       (condition-case nil
			   (oref targ :partofall)
			 (error nil)))

	      (let ((ns (object-name-string targ)))
		(cedet-build-msg "   Target %s...%s" ns
				 (make-string (- 20 (length ns)) ? )))

	      ;; If it is an autoload or elisp target, then
	      ;; do that work here.
	      (let ((ans (save-excursion
			   (project-compile-target targ))))
		(if (and (consp ans)
			 (numberp (car ans)))
		    (cedet-build-msg "%d compiled, %d up to date.\n"
				     (car ans) (cdr ans))
		  (cedet-build-msg "done.\n"))
		))
	    )))
      )))


(provide 'cedet-build)
;;; cedet-build.el ends here

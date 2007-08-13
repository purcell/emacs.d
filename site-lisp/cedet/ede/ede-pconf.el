;;; ede-pconf.el --- configure.in maintenance for EDE

;;  Copyright (C) 1998, 1999, 2000, 2005  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project
;; RCS: $Id: ede-pconf.el,v 1.12 2005/09/30 20:16:30 zappo Exp $

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
;; Code generator for autoconf configure.in, and support files.

(require 'ede-proj)
(require 'autoconf-edit)

;;; Code:
(defmethod ede-proj-configure-file ((this ede-proj-project))
  "The configure.in script used by project THIS."
  (ede-expand-filename (ede-toplevel this) "configure.in" t))

(defmethod ede-proj-configure-test-required-file ((this ede-proj-project) file)
  "For project THIS, test that the file FILE exists, or create it."
  (if (not (ede-expand-filename (ede-toplevel this) file))
      (save-excursion
	(find-file (ede-expand-filename (ede-toplevel this) file t))
	(cond ((string= file "AUTHORS")
	       (insert (user-full-name) " <" (user-login-name) ">"))
	      ((string= file "NEWS")
	       (insert "NEWS file for " (ede-name this)))
	      (t (insert "\n")))
	(save-buffer)
	(if (not (y-or-n-p
		  (format "I had to create the %s file for you.  Ok? " file)))
	    (error "Quit")))))


(defmethod ede-proj-configure-synchronize ((this ede-proj-project))
  "Synchronize what we know about project THIS into configure.in."
  (let ((b (find-file-noselect (ede-proj-configure-file this)))
	(td (file-name-directory (ede-proj-configure-file this)))
	(targs (oref this targets))
	(postcmd "")
	(add-missing nil))
    ;; First, make sure we have a file.
    (if (not (file-exists-p (ede-proj-configure-file this)))
	(autoconf-new-program b (oref this name) "Project.ede"))
    (set-buffer b)
    ;; Next, verify all targets of all subobjects.
    (autoconf-set-version (oref this version))
    (autoconf-set-output '("Makefile"))
     ;;
     ;; NOTE TO SELF.  TURN THIS INTO THE OFFICIAL LIST
     ;;
    (ede-proj-dist-makefile this)
    ;; Loop over all targets to clean and then add themselves in.
    (ede-map-targets this 'ede-proj-flush-autoconf)
    (ede-map-targets this 'ede-proj-tweak-autoconf)
    ;; Now save
    (save-buffer)
    ;; Verify aclocal
    (if (not (file-exists-p (ede-expand-filename (ede-toplevel this)
						 "aclocal.m4" t)))
	(setq postcmd "aclocal;autoconf;autoheader;")
      ;; Verify the configure script...
      (if (not (ede-expand-filename (ede-toplevel this)
				    "configure"))
	  (setq postcmd "autoconf;autoheader;")
	(if (not (ede-expand-filename (ede-toplevel this) "config.h.in"))
	    (setq postcmd "autoheader;"))))
    ;; Verify Makefile.in, and --add-missing files (cheaply)
    (setq add-missing (ede-map-any-target-p this
					    'ede-proj-configure-add-missing))
    (if (not (ede-expand-filename (ede-toplevel this) "Makefile.in"))
	(progn
	  (setq postcmd (concat postcmd "automake"))
	  (if (or (not (ede-expand-filename (ede-toplevel this) "COPYING"))
		  add-missing)
	      (setq postcmd (concat postcmd " --add-missing")))
	  (setq postcmd (concat postcmd ";")))
      (if (or (not (ede-expand-filename (ede-toplevel this) "COPYING"))
	      add-missing)
	  (setq postcmd (concat postcmd "automake --add-missing;"))))
    ;; Verify a bunch of files that are required by automake.
    (ede-proj-configure-test-required-file this "AUTHORS")
    (ede-proj-configure-test-required-file this "NEWS")
    (ede-proj-configure-test-required-file this "README")
    (ede-proj-configure-test-required-file this "ChangeLog")
    ;; Let specific targets get missing files.
    (mapc 'ede-proj-configure-create-missing targs)
    ;; Verify that we have a make system.
    (if (or (not (ede-expand-filename (ede-toplevel this) "Makefile"))
	    ;; Now is this one of our old Makefiles?
	    (save-excursion
	      (set-buffer (find-file-noselect
			   (ede-expand-filename (ede-toplevel this)
						"Makefile" t) t))
	      (goto-char (point-min))
	      ;; Here is the unique piece for our makefiles.
	      (re-search-forward "For use with: make" nil t)))
	(setq postcmd (concat postcmd "./configure;")))
    (if (not (string= "" postcmd))
	(progn
	  (compile postcmd)
	  (switch-to-buffer "*Help*")
	  (toggle-read-only -1)
	  (erase-buffer)
	  (insert "Preparing build environment

Rerun the previous ede command when automake and autoconf are completed.")
	  (goto-char (point-min))
	  (let ((b (get-file-buffer
		    (ede-expand-filename (ede-toplevel this)
					 "Makefile"))))
	    ;; This makes sure that if Makefile was loaded, and old,
	    ;; that it gets flushed so we don't keep rebuilding
	    ;; the autoconf system.
	    (if b (kill-buffer b)))
	  (error "Preparing build environment: Rerun your command when done")
	  ))))

(defmethod ede-proj-configure-recreate ((this ede-proj-project))
  "Delete project THISes configure script and start over."
  (if (not (ede-proj-configure-file this))
      (error "Could not determine configure.in for %S" (object-name this)))
  (let ((b (get-file-buffer (ede-proj-configure-file this))))
    ;; Destroy all evidence of the old configure.in
    (delete-file (ede-proj-configure-file this))
    (if b (kill-buffer b)))
  (ede-proj-configure-synchronize this))

(defmethod ede-proj-tweak-autoconf ((this ede-proj-target))
  "Tweak the configure file (current buffer) to accomodate THIS."
  ;; Check the compilers belonging to THIS, and call the autoconf
  ;; setup for those compilers.
  (mapc 'ede-proj-tweak-autoconf (ede-proj-compilers this)))

(defmethod ede-proj-flush-autoconf ((this ede-proj-target))
  "Flush the configure file (current buffer) to accomodate THIS.
By flushing, remove any cruft that may be in the file.  Subsequent
calls to `ede-proj-tweak-autoconf' can restore items removed by flush."
  nil)

(defmethod ede-proj-configure-add-missing ((this ede-proj-target))
  "Query if any files needed by THIS provided by automake are missing.
Results in --add-missing being passed to automake."
  nil)

(defmethod ede-proj-configure-create-missing ((this ede-proj-target))
  "Add any missing files for THIS by creating them."
  nil)

(provide 'ede-pconf)

;;; ede-pconf.el ends here

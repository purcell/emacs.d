;;; cedet.el --- Setup CEDET environment

;; Copyright (C) 2007, 2008, 2009 by Eric M. Ludlam
;; Copyright (C) 2002, 2003, 2004, 2005, 2006 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: CEDET developers <http://sf.net/projects/cedet>
;; Created: 09 Dec 2002
;; Keywords: syntax
;; X-RCS: $Id: cedet.el,v 1.31 2009/01/29 02:55:12 zappo Exp $

;; This file is not part of Emacs

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
;; This library automatically setups your [X]Emacs to use CEDET tools.
;;
;; First download the latest CEDET distribution, provided in a
;; cedet-<VERSION>.tar.gz tarball, from the project page at:
;; <http://sf.net/projects/cedet>.
;;  
;; Unpack the tarball in a directory of your choice.  It will install
;; the following directory tree:
;;
;;   cedet
;;     |
;;     +- common
;;     |
;;     +- cogre
;;     |
;;     +- ede
;;     |
;;     +- eieio
;;     |
;;     +- semantic
;;     |
;;     +- speedbar
;;     |
;;     +- srecode
;;     |
;;     \- contrib
;;
;; Then, add the following into your ~/.emacs startup file:
;;
;;   (load-file "<INSTALL-PATH>/cedet/common/cedet.el")
;;
;; If you want to turn on useful or all Semantic features by default,
;; respectively add:
;;
;;   (setq semantic-load-turn-useful-things-on t)
;; or
;;   (setq semantic-load-turn-everything-on t)
;;
;; before loading this file, like this:
;;
;;   (setq semantic-load-turn-useful-things-on t)
;;   (load-file "<INSTALL-PATH>/cedet/common/cedet.el")
;;
;; That's it!
;;

;;; Code:
(eval-when-compile
  (require 'cl)
  )

(defconst cedet-version "1.0pre6"
  "Current version of CEDET.")

(defconst cedet-emacs-min-version "21.1"
  "Minimum version of GNU Emacs supported by CEDET.")
(defconst cedet-xemacs-min-version "21.4"
  "Minimum version of XEmacs supported by CEDET.")

(defconst cedet-packages
  `(
    ;;PACKAGE   MIN-VERSION      INSTALLDIR DOCDIR
    (cedet         ,cedet-version "common"  "common" 	   )
    (eieio         "1.2"           nil      "eieio"        )
    (semantic      "2.0pre6"       nil      "semantic/doc" )
    (srecode       "0.1"           nil      "srecode"      ) 
    (ede           "1.0pre6"       nil      "ede"    	   )    
    (speedbar      "1.0.2"         nil      "speedbar"     )
    (cogre         "0.7"           nil      "cogre"  	   )
    (cedet-contrib "1.0pre6"      "contrib"  nil           )
    )
  "Table of CEDET packages to install.")

;; This file must be in "<INSTALL-DIR>/cedet/common"!
(let ((default-directory
        (file-name-directory
         (or load-file-name (buffer-file-name)))))
  
  ;; Add "<INSTALL-DIR>/cedet/common" to `load-path'.
  (add-to-list 'load-path default-directory)
  ;;(message "%S added to `load-path'" default-directory)
  ;; Require the inversion library.
  (require 'inversion)
  
  ;; Require specific Emacs versions
  (inversion-require-emacs cedet-emacs-min-version
			   cedet-xemacs-min-version)

  ;; Go up to the parent "<INSTALL-DIR>/cedet" directory.
  (let ((default-directory (expand-file-name ".."))
        package min-version installdir docdir)

    ;; Add the CEDET packages subdirectories to the `load-path' if
    ;; necessary.
    (dolist (package-spec cedet-packages)
      (setq package     (nth 0 package-spec)
            min-version (nth 1 package-spec)
            installdir  (nth 2 package-spec)
	    docdir      (nth 3 package-spec)
	    )
      ;; Add package to load path
      (when installdir
        (setq installdir (expand-file-name installdir)))
      (inversion-add-to-load-path package min-version installdir)
      ;; Add doc to Info path
      (when docdir
	(let ((fulldocpath (expand-file-name docdir default-directory)))
	  ;; Set up one of the info paths depending on if info is
	  ;; loaded yet.	  
	  (if (featurep 'info)
	      (progn
		(condition-case nil ; Not all emacs versions have this.
		    (info-initialize)
		  (error nil))
		(add-to-list 'Info-directory-list fulldocpath))
	    (add-to-list 'Info-default-directory-list fulldocpath))
	  )))

    ;; Force EIEIO to load so that the autoloads work.
    (require 'eieio)

    ;; Then run every package setup.
    (message "Setting up CEDET packages...")
    (dolist (package-spec cedet-packages)
      (setq package (nth 0 package-spec))
      (condition-case err
	  (progn
	    (require (intern (format "%s-load" package)))
	    )
	(error
	 (message "%s" (error-message-string err)))))
    (message "Setting up CEDET packages...done")
    ))

(eval-when-compile
  (require 'inversion))

(defun cedet-version ()
  "Display all active versions of CEDET and Dependant packages.

The PACKAGE column is the name of a given package from CEDET.

REQUESTED VERSION is the version requested by the CEDET load script.
See `cedet-packages' for details.

FILE VERSION is the version number found in the source file
for the specificed PACKAGE.

LOADED VERSION is the version of PACKAGE current loaded in Emacs
memory and (presumably) running in this Emacs instance.  Value is X
if the package has not been loaded."
  (interactive)
  (with-output-to-temp-buffer "*CEDET*"
    (princ "CEDET Version:\t") (princ cedet-version)
    (princ "\n  \t\t\tRequested\tFile\t\tLoaded")
    (princ "\n  Package\t\tVersion\t\tVersion\t\tVersion")
    (princ "\n  ----------------------------------------------------------")
    (let ((p cedet-packages))
      (while p
	(let ((sym (symbol-name (car (car p)))))
	  (princ "\n  ")
	  (princ sym)
	  (princ ":\t")
	  (if (< (length sym) 5)
	      (princ "\t"))
	  (if (< (length sym) 13)
	      (princ "\t"))
	  (let ((reqver (nth 1 (car p)))
		(filever (car (inversion-find-version sym)))
		(loadver (when (featurep (car (car p)))
			   (symbol-value (intern-soft (concat sym "-version"))))))
	    (princ reqver)
	    (if (< (length reqver) 8) (princ "\t"))
	    (princ "\t")
	    (if (string= filever reqver)
		;; I tried the words "check" and "match", but that
		;; just looked lame.
		(princ "ok\t")
	      (princ filever)
	      (if (< (length filever) 8) (princ "\t")))
	    (princ "\t")
	    (if loadver
		(if (string= loadver reqver)
		    (princ "ok")
		  (princ loadver))
	      (princ "Not Loaded"))
	    ))
	(setq p (cdr p))))
    (princ "\n\n\nC-h f cedet-version RET\n  for details on output format.")
    ))

(provide 'cedet)

;;; cedet.el ends here

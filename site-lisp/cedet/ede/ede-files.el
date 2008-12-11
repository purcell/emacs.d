;;; ede-files.el --- Associate projects with files and directories.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: ede-files.el,v 1.6 2008/12/10 15:18:27 zappo Exp $

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
;; Directory and File scanning and matching functions.
;;
;; Basic Model:
;;
;; A directory belongs to a project if a ede-project-autoload structure
;; matches your directory.
;;
;; A toplevel project is one where there is no active project above
;; it.  Finding the toplevel project involves going up a directory
;; till no ede-project-autoload structure matches.
;; 

(require 'ede)

;;; Code:

;;; Placeholders for ROOT directory scanning on base objects
;;
(defmethod ede-project-root ((this ede-project-placeholder))
  "If a project knows it's root, return it here.
Allows for one-project-object-for-a-tree type systems."
  (oref this rootproject))

(defmethod ede-project-root-directory ((this ede-project-placeholder)
				       &optional file)
  "If a project knows it's root, return it here.
Allows for one-project-object-for-a-tree type systems.
Optional FILE is the file to test.  It is ignored in preference
of the anchor file for the project."
  (file-name-directory (expand-file-name (oref this file))))


(defmethod ede-project-root ((this ede-project-autoload))
  "If a project knows it's root, return it here.
Allows for one-project-object-for-a-tree type systems."
  nil)

(defmethod ede-project-root-directory ((this ede-project-autoload)
				       &optional file)
  "If a project knows it's root, return it here.
Allows for one-project-object-for-a-tree type systems.
Optional FILE is the file to test.  If there is no FILE, use
the current buffer."
  (when (not file)
    (setq file default-directory))
  (when (slot-boundp this :proj-root)
    (let ((rootfcn (oref this proj-root)))
      (when rootfcn
	(condition-case err
	    (funcall rootfcn file)
	  (error 
	   (funcall rootfcn)))
	))))

(defmethod ede-project-inode ((proj ede-project-placeholder))
  "Get the inode of the directory project PROJ is in."
  (if (slot-boundp proj 'dirinode)
      (oref proj dirinode)
    (oset proj dirinode (ede-inode-for-dir (oref proj :directory)))))

(defmethod ede-find-subproject-for-directory ((proj ede-project-placeholder)
					      dir)
  "Find a subproject of PROJ that corresponds to DIR."
  (let ((ans nil)
	(inode (ede-inode-for-dir dir)))
    (ede-map-subprojects 
     proj
     (lambda (SP)
       (when (not ans)
	 (if (equal (ede-project-inode SP) inode)
	     (setq ans SP)
	   (ede-find-subproject-for-directory SP dir)))))
    ans))

;;; DIRECTORY IN OPEN PROJECT
;;
;; These routines match some directory name to one of the many pre-existing
;; open projects.  This should avoid hitting the disk, or asking lots of questions
;; if used throughout the other routines.
(defvar ede-inode-directory-hash (make-hash-table
				  ;; Note on test.  Can we compare inodes or something?
				  :test 'equal)
  "A hash of directory names and inodes.")

(defun ede-put-inode-dir-hash (dir inode)
  "Add to the EDE project hash DIR associated with INODE."
  (when (fboundp 'puthash)
    (puthash dir inode ede-inode-directory-hash)
    inode))

(defun ede-get-inode-dir-hash (dir)
  "Get the EDE project hash DIR associated with INODE."
  (when (fboundp 'gethash)
    (gethash dir ede-inode-directory-hash)
    ))

(defun ede-inode-for-dir (dir)
  "Return the inode for the directory DIR."
  (let ((hashnode (ede-get-inode-dir-hash (expand-file-name dir))))
    (or hashnode
	(let ((fattr (file-attributes dir)))
	  (ede-put-inode-dir-hash dir (nth 10 fattr))))))

(defun ede-directory-get-open-project (dir)
  "Return an already open project that is managing DIR."
  (let* ((inode (ede-inode-for-dir dir))
	 (ft (file-name-as-directory (expand-file-name dir)))
	 (proj (ede-directory-get-toplevel-open-project ft))
	 (ans proj))
    (when (and proj (not (equal inode (ede-project-inode proj))))
      (setq ans (ede-find-subproject-for-directory proj ft)))
    ans))

(defun ede-directory-get-toplevel-open-project (dir)
  "Return an already open toplevel project that is managing DIR."
  (let ((ft (file-name-as-directory (expand-file-name dir)))
	(all ede-projects)
	(ans nil))
    (while (and all (not ans))
      ;; Do the check.
      (let ((pd (oref (car all) :directory))	    
	    )
	(cond
	 ;; Exact text match.
	 ((string= pd ft)
	  (setq ans (car all)))
	 ;; Some sub-directory
	 ((string-match (concat "^" (regexp-quote pd)) ft)
	  (setq ans (car all)))
	 ;; Exact inode match.  Useful with symlinks or complex automounters.
	 ((let ((pin (ede-project-inode (car all)))
		(inode (ede-inode-for-dir dir)))
	    (equal pin inode))
	  (setq ans (car all)))
	 ;; Subdir via truename - slower by far, but faster than a traditional lookup.
	 ((let ((ftn (file-truename ft))
		(ptd (file-truename (oref (car all) :directory))))
	    (string-match (concat "^" (regexp-quote ptd)) ftn))
	  (setq ans (car all)))
	 ))
      (setq all (cdr all)))
    ans))

;;; DIRECTORY-PROJECT-P
;;
;; For a fresh buffer, or for a path w/ no open buffer, use this
;; routine to determine if there is a known project type here.
(defvar ede-project-directory-hash (make-hash-table
				    ;; Note on test.  Can we compare inodes or something?
				    :test 'equal)
  "A hash of directory names and associated EDE objects.")

(defun ede-project-directory-remove-hash (dir)
  "Reset the directory hash for DIR.
Do this whenever a new project is created, as opposed to loaded."
  ;; TODO - Use maphash, and delete by regexp, not by dir searching!

  (when (fboundp 'remhash)
    (remhash (file-name-as-directory dir) ede-project-directory-hash)
    ;; Look for all subdirs of D, and remove them.
    (let ((match (concat "^" (regexp-quote dir))))
      (maphash (lambda (K O)
		 (when (string-match match K)
		   (remhash K ede-project-directory-hash)))
	       ede-project-directory-hash))
    ))

(defun ede-directory-project-from-hash (dir)
  "If there is an already loaded project for DIR, return it from the hash."
  (when (fboundp 'gethash)
    (gethash dir ede-project-directory-hash nil)))

(defun ede-directory-project-add-description-to-hash (dir desc)
  "Add to the EDE project hash DIR associated with DESC."
  (when (fboundp 'puthash)
    (puthash dir desc ede-project-directory-hash)
    desc))

(defun ede-directory-project-p (dir)
  "Return a project description object if DIR has a project.
This depends on an up to date `ede-project-class-files' variable."
  (let* ((dirtest (expand-file-name dir))
	 (match (ede-directory-project-from-hash dirtest)))
    (cond
     ((eq match 'nomatch)
      nil)
     (match match)
     (t
      (let ((types ede-project-class-files)
	    (ret nil))
	;; Loop over all types, loading in the first type that we find.
	(while (and types (not ret))
	  (if (ede-dir-to-projectfile (car types) dirtest)
	      (progn
		;; We found one!  Require it now since we will need it.
		(require (oref (car types) file))
		(setq ret (car types))))
	  (setq types (cdr types)))
	(ede-directory-project-add-description-to-hash dirtest (or ret 'nomatch))
	ret)))))

;;; TOPLEVEL
;;
;; These utilities will identify the "toplevel" of a project.
;;
(defun ede-toplevel-project-or-nil (dir)
  "Starting with DIR, find the toplevel project directory, or return nil.
nil is returned if the current directory is not a part ofa project."
  (let* ((ans (ede-directory-get-toplevel-open-project dir)))
    (if ans
	(oref ans :directory)
      (if (ede-directory-project-p dir)
	  (ede-toplevel-project dir)
	nil))))

(defun ede-toplevel-project (dir)
  "Starting with DIR, find the toplevel project directory."
  (let* ((ans (ede-directory-get-toplevel-open-project dir)))
    (if ans
	(oref ans :directory)
      (let* ((toppath (expand-file-name dir))
	     (newpath toppath)
	     (proj (ede-directory-project-p dir))
	     (ans nil))
	(if proj
	    ;; If we already have a project, ask it what the root is.
	    (setq ans (ede-project-root-directory proj)))

	;; If PROJ didn't know, or there is no PROJ, then

	;; Loop up to the topmost project, and then load that single
	;; project, and it's sub projects.  When we are done, identify the
	;; sub-project object belonging to file.
	(while (and (not ans) newpath proj)
	  (setq toppath newpath
		newpath (ede-up-directory toppath))
	  (when newpath
	    (setq proj (ede-directory-project-p newpath)))

	  (when proj
	    ;; We can home someone in the middle knows too.
	    (setq ans (ede-project-root-directory proj)))
	  )
	(or ans toppath)))))

;;; TOPLEVEL PROJECT
;;
;; The toplevel project is a way to identify the EDE structure that belongs
;; to the top of a project.

(defun ede-toplevel (&optional subproj)
  "Return the ede project which is the root of the current project.
Optional argument SUBPROJ indicates a subproject to start from
instead of the current project."
  (let* ((cp (or subproj (ede-current-project)))
	 )
    (or (and cp (ede-project-root cp))
	(progn
	  (while (ede-parent-project cp)
	    (setq cp (ede-parent-project cp)))
	  cp))))

;;; UTILITIES
;;

(defun ede-up-directory (dir)
  "Return a dir that is up one directory.
Argument DIR is the directory to trim upwards."
  (let* ((fad (directory-file-name dir))
	 (fnd (file-name-directory fad)))
    (if (string= dir fnd) ; This will catch the old string-match against
			  ; c:/ for DOS like systems.
	nil
      fnd)))
  
(provide 'ede-files)
;;; ede-files.el ends here

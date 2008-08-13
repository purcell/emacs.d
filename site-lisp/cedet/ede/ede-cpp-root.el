;;; ede-cpp-root.el --- A simple way to wrap a C++ project with a single root

;; Copyright (C) 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: ede-cpp-root.el,v 1.10 2008/07/02 14:23:55 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version $?GPL$, or (at
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
;; Not everyone can use automake, or an EDE project type.  For
;; pre-existing code, it is often helpful jut to be able to wrap the
;; whole thing up in as simple a way as possible.
;;
;; The cpp-root project type will allow you to create a single object
;; with no save-file in your .emacs file that will be recognized, and
;; provide a way to easilly allow EDE to provide Semantic with the
;; ability to find header files, and other various source files
;; quickly.
;;
;; The cpp-root class knows a few things about C++ projects, such as
;; the prevalence of "include" directories, and typical file-layout
;; stuff.  If this isn't sufficient, you can subclass
;; `ede-cpp-root-project' and add your own tweaks in just a few lines.
;; See the end of this file for an example.
;;
;;; EXAMPLE
;; 
;; Add this to your .emacs file, modifying apropriate bits as needed.
;;
;; (ede-cpp-root-project "SOMENAME" :file "/dir/to/some/file")
;;
;; Replace SOMENAME with whatever name you want, and the filename to
;; an actual file at the root of your project.  It might be a
;; Makefile, a README file.  Whatever.  It doesn't matter.  It's just
;; a key to hang the rest of EDE off of.
;;
;; The most likely reason to create this project, is to help make
;; finding files within the project faster.  In conjunction with
;; Semantic completion, having a short include path is key.  You can
;; override the include path like this:
;;
;; (ede-cpp-root-project "NAME" :file "FILENAME"
;;     :include-path '( "/include" "../include" "/c/include" )
;;     :system-include-path '( "/usr/include/c++/3.2.2/" )
;;     :spp-table '( ("MOOSE" . "")
;;                   ("CONST" . "const") ) )
;;
;;  In this case each item in the include path list is searched.  If
;;  the directory starts with "/", then that expands to the project
;;  root directory.  If a directory does not start with "/", then it
;;  is relative to the default-directory of the current buffer when
;;  the file name is expanded.
;;
;;  The include path only affects C/C++ header files.  Use the slot
;;  :header-match-regexp to change it.
;;
;;  The :system-include-path allows you to specify full directory
;;  names to include directories where system header files can be
;;  found.  These will be applied to files in this project only.
;;
;;  The :spp-table provides a list of project specific #define style
;;  macros that are unique to this project, passed in to the compiler
;;  on the command line, or are in special headers.
;;
;; If you want to override the file-finding tool with your own
;; function you can do this:
;;
;; (ede-cpp-root-project "NAME" :file "FILENAME" :locate-fcn 'MYFCN)
;;
;; Where MYFCN is a symbol for a function.  See:
;;
;; M-x describe-class RET ede-cpp-root-project RET
;;
;; for documentation about the locate-fcn extension.
;;
;;; ADVANCED EXAMPLE
;;
;; If the cpp-root project style is right for you, but you want a
;; dynamic loader, instead of hard-coding values in your .emacs, you
;; can do that too, but you will need to write some lisp code.
;;
;; To do that, you need to add an entry to the
;; `ede-project-class-files' list, and also provide two functions to
;; teach EDE how to load your project pattern
;;
;; It would oook like this:
;;
;; (defun MY-FILE-FOR-DIR (&optional dir)
;;   "Return a full file name to the project file stored in DIR."
;;   <write your code here, or return nil>
;;   )
;;
;; (defun MY-ROOT-FCN ()
;;   "Return the root directory for `default-directory'"
;;   ;; You might be able to use `ede-cpp-root-project-root'.
;;   )
;; 
;; (defun MY-LOAD (dir)
;;   "Load a project of type `cpp-root' for the directory DIR.
;; Return nil if there isn't one."
;;   (ede-cpp-root-project "NAME" :file (expand-file-name "FILE" dir)
;;                                :locate-fcn 'MYFCN)
;;   )
;;
;; (add-to-list 'ede-project-class-files
;; 	     (ede-project-autoload "cpp-root"
;; 	      :name "CPP ROOT"
;; 	      :file 'ede-cpp-root
;; 	      :proj-file 'MY-FILE-FOR-DIR
;;            :proj-root 'MY-ROOT-FCN
;; 	      :load-type 'MY-LOAD
;; 	      :class-sym 'ede-cpp-root)
;; 	     t)
;; 

(require 'ede)

;;; Code:
(defvar ede-cpp-root-project-list nil
  "List of projects created by option `ede-cpp-root-project'.")

(defun ede-cpp-root-file-existing (dir)
  "Find a cpp-root project in the list of cpp-root projects.
DIR is the directory to search from."
  (let ((projs ede-cpp-root-project-list)
	(ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
	(when (string-match (concat "^" (regexp-quote root)) dir)
	  (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))


;;;###autoload
(defun ede-cpp-root-project-file-for-dir (&optional dir)
  "Return a full file name to the project file stored in DIR."
  (let ((proj (ede-cpp-root-file-existing dir)))
    (when proj (oref proj :file))))

;;;###autoload
(defun ede-cpp-root-project-root ()
  "Get the root directory for DIR."
  (let ((projfile (ede-cpp-root-project-file-for-dir default-directory)))
    (when projfile
      (file-name-directory projfile))))

;;;###autoload
(defun ede-cpp-root-load (dir &optional rootproj)
  "Return a CPP root object if you created one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  ;; Snoop through our master list.
  (ede-cpp-root-file-existing dir))

;;;###autoload
(add-to-list 'ede-project-class-files
	     (ede-project-autoload "cpp-root"
	      :name "CPP ROOT"
	      :file 'ede-cpp-root
	      :proj-file 'ede-cpp-root-project-file-for-dir
	      :proj-root 'ede-cpp-root-project-root
	      :load-type 'ede-cpp-root-load
	      :class-sym 'ede-cpp-root
	      :new-p nil)
	     t)

;;; CLASSES
;;
(defclass ede-cpp-root-target (ede-target)
  ()
  "EDE cpp-root project target.
All directories need at least one target.")

;;;###autoload
(defclass ede-cpp-root-project (eieio-instance-tracker ede-project)
  ((tracking-symbol :initform 'ede-cpp-root-project-list)
   (include-path :initarg :include-path
		 :initform '( "/include" "../include/" )
		 :type list
		 :documentation
		 "The default locate function expands filenames within a project.
If a header file (.h, .hh, etc) name is expanded, and
the :locate-fcn slot is nil, then the include path is checked
first, and other directories are ignored.  For very large
projects, this optimization can save a lot of time.

Directory names in the path can be relative to the current
buffer's `default-directory' (not starting with a /).  Directories
that are relative to the project's root should start with a /, such
as  \"/include\", meaning the directory `include' off the project root
directory.")
   (system-include-path :initarg :system-include-path
			:initform nil
			:type list
			:documentation
			"The system include path for files in this project.
C files initialized in an ede-cpp-root-project have their semantic
system include path set to this value.  If this is nil, then the
semantic path is not modified.")
   (spp-table :initarg :spp-table
	      :initform nil
	      :type list
	      :documentation
	      "C Preprocessor macros for your files.
These macros might be passed in through the command line compiler, or
are critical symbols derived from header files.  Providing header files
macro values through this slot improves accuracy and performance.
See `semantic-lex-c-preprocessor-symbol-map' for more.")
   (header-match-regexp :initarg :header-match-regexp
			:initform
			"\\.\\(h\\(h\\|xx\\|pp\\|\\+\\+\\)?\\|H\\)$\\|\\<\\w+$"
			:type string
			:documentation
			"Regexp used to identify C/C++ header files.")
   (locate-fcn :initarg :locate-fcn
	       :initform nil
	       :type (or null function)
	       :documentation
	       "The locate function can be used in place of
`ede-expand-filename' so you can quickly customize your custom target
to use specialized local routines instead of the EDE routines.
The function symbol must take two arguments:
  NAME - The name of the file to find.
  DIR - The directory root for this cpp-root project.

It should return the fully qualified file name passed in from NAME.  If that file does not
exist, it should return nil."
	       )
   )
  "EDE cpp-root project class.
Each directory needs a a project file to control it.")

(defmethod initialize-instance ((this ede-cpp-root-project)
				&rest fields)
  "Make sure the :file is fully expanded."
  (call-next-method)
  (let ((f (expand-file-name (oref this :file))))
    (when (or (not (file-exists-p f))
	      (file-directory-p f))
      (delete-instance this)
      (error ":file for ede-cpp-root must be a file."))
    (oset this :file f)
    (unless (slot-boundp this 'targets)
      (oset this :targets nil))))

;;; TARGET MANAGEMENT
;;
(defmethod ede-find-target ((proj ede-cpp-root-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj targets))
	 (dir default-directory)
	 (ans (object-assoc dir :path targets))
	 )
    (when (not ans)
      (setq ans (ede-cpp-root-target dir
		 :path dir
		 :source nil))
      (object-add-to-list proj :targets ans)
      )
    ans))

(defmethod ede-cpp-root-header-file-p ((proj ede-cpp-root-project) name)
  "Non nil if in PROJ the filename NAME is a header."
  (save-match-data
    (string-match (oref proj header-match-regexp) name)))

(defmethod ede-expand-filename ((proj ede-cpp-root-project) name)
  "Within this project PROJ, find the file NAME.
This knows details about or source tree."
  ;; The slow part of the original is looping over subprojects.
  ;; This version has no subprojects, so this will handle some
  ;; basic cases.
  (let ((ans (call-next-method)))
    (unless ans
      (let* ((lf (oref proj locate-fcn))
	     (file (oref proj file))
	     (dir (file-name-directory file)))
	(if lf
	    (setq ans (funcall lf name dir))
	  (if (ede-cpp-root-header-file-p proj name)
	      ;; Else, use our little hack.
	      (let ((ip (oref proj include-path))
		    (tmp nil))
		(while ip
		  (setq tmp (car ip))
		  (if (and (not (string= tmp "")) (= (aref tmp 0) ?/))
		      ;; Check relative to root
		      (setq tmp (expand-file-name (substring tmp 1)
						  dir))
		    (setq tmp (expand-file-name tmp)))
		  (setq tmp (expand-file-name name tmp))
		  (if (file-exists-p tmp)
		      (setq ans tmp))
		  (setq ip (cdr ip)) ))
	    ;; Else, do the usual.
	    (setq ans (call-next-method)))
	  )))
    ans))

(defmethod ede-project-root ((this ede-cpp-root-project))
  "Return my root."
  this)

(defmethod ede-project-root-directory ((this ede-cpp-root-project))
  "Return my root."
  (file-name-directory (oref this file)))

;;; Special Init
;;
(defmethod ede-set-project-variables ((project ede-cpp-root-project) &optional buffer)
  "Set variables local to PROJECT in BUFFER.
Also set up the lexical preprocessor map."
  (call-next-method)
  (when (and (featurep 'semantic-c) (featurep 'semantic-lex-spp))
    (setq semantic-lex-spp-project-macro-symbol-obarray
	  (semantic-lex-make-spp-table (oref project spp-table)))
    ))  

;;; C++ special options.
;;

(defmethod ede-system-include-path ((this ede-cpp-root-project))
  "Get the system include path used by project THIS."
  (oref this system-include-path))
  
(defmethod ede-preprocessor-map ((this ede-cpp-root-project))
  "Get the pre-processor map for project THIS."
  (oref this spp-table))

(defmethod ede-system-include-path ((this ede-cpp-root-target))
  "Get the system include path used by project THIS."
  (ede-system-include-path (ede-target-parent this)))
  
(defmethod ede-preprocessor-map ((this ede-cpp-root-target))
  "Get the pre-processor map for project THIS."
  (ede-preprocessor-map  (ede-target-parent this)))


(provide 'ede-cpp-root)
;;; ede-cpp-root.el ends here

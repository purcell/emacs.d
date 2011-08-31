;;; w3m-xmas.el --- XEmacs stuff for emacs-w3m

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Yuuichi Teranishi  <teranisi@gohome.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This file contains XEmacs stuff that emacs-w3m uses.  For more
;; detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:

(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-image)
(require 'w3m-favicon)
(eval-and-compile
  (when (featurep 'mule)
    (require 'w3m-ccl)))

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-coding-system)
  (defvar w3m-current-title)
  (defvar w3m-current-url)
  (defvar w3m-display-inline-images)
  (defvar w3m-icon-directory)
  (defvar w3m-menu-on-forefront)
  (defvar w3m-menubar)
  (defvar w3m-modeline-process-status-on)
  (defvar w3m-show-graphic-icons-in-mode-line)
  (defvar w3m-toolbar)
  (defvar w3m-toolbar-buttons)
  (defvar w3m-use-tab)
  (defvar w3m-use-tab-menubar)
  (defvar w3m-work-buffer-name)
  (defvar w3m-work-buffer-list)
  (defvar w3m-default-coding-system)
  (defvar w3m-coding-system)
  (autoload 'update-tab-in-gutter "gutter-items")
  (autoload 'w3m-image-type "w3m")
  (autoload 'w3m-retrieve "w3m")
  (autoload 'w3m-setup-tab-menu "w3m-tabmenu")
  (autoload 'w3m-setup-bookmark-menu "w3m-bookmark")
  (autoload 'w3m-setup-session-menu "w3m-session"))

;; Dummies to shut some XEmacs variants up.
(eval-when-compile
  (autoload 'unicode-to-char "XEmacs-21.5-b6_and_later")
  (unless (featurep 'mule)
    (defalias 'find-charset 'ignore)))

(require 'path-util)
(require 'poe)
(require 'poem)
(require 'pccl)

;;; Handle coding system:
(eval-when-compile
  (unless (fboundp 'find-coding-system)
    (defalias 'find-coding-system 'ignore)
    (defalias 'coding-system-name 'ignore)))

(defalias 'w3m-find-coding-system
  (if (fboundp 'find-coding-system)
      (lambda (obj)
	(and obj
	     (setq obj (find-coding-system obj))
	     (coding-system-name obj)))
    'ignore))

;; Under XEmacs 21.5-b6 and later, `make-ccl-coding-system' will
;; signal an error if the coding-system has already been defined.
;; To make w3m.elc reloadable, we'll define the function as follows:
;;
;;(defun w3m-make-ccl-coding-system (coding-system args...)
;;  (or (find-coding-system coding-system)
;;      (make-ccl-coding-system coding-system args...)))
(eval-when-compile
  (defmacro w3m-define-w3m-make-ccl-coding-system ()
    "Make the source form for the function `w3m-make-ccl-coding-system'."
    (if (and (fboundp 'make-ccl-coding-system)
	     (fboundp 'find-coding-system))
	`(defun w3m-make-ccl-coding-system (coding-system mnemonic docstring
							  decoder encoder)
	   ,(concat (documentation 'make-ccl-coding-system)
		    "\n\n\
NOTE: This function is slightly modified from `make-ccl-coding-system'
      to be recallable for the existing coding-systems without errors.")
	   (or (find-coding-system coding-system)
	       (,(symbol-function 'make-ccl-coding-system)
		coding-system mnemonic docstring decoder encoder)))
      '(defalias 'w3m-make-ccl-coding-system 'ignore))))

(w3m-define-w3m-make-ccl-coding-system)

(eval-and-compile
  (dolist (fn '(coding-priority-list
		coding-system-category
		coding-system-list
		coding-system-name
		coding-system-type
		set-coding-category-system
		set-coding-priority-list))
    (unless (fboundp fn)
      (defalias fn 'ignore))))

;; If pccl.elc has been mis-compiled for XEmacs with MULE, the macro
;; `define-ccl-program' wouldn't be an empty macro because of advice.
(when (and (not (featurep 'mule))
	   (featurep 'advice))
  (ad-unadvise 'define-ccl-program))

(unless (fboundp 'define-ccl-program)
  (defmacro define-ccl-program (&rest args)))

(defmacro w3m-detect-coding-with-priority (from to priority-list)
  (cond
   ((featurep 'mule)
    `(detect-coding-with-priority ,from ,to ,priority-list))
   ((featurep 'file-coding)
    `(detect-coding-region ,from ,to))
   (t
    'w3m-default-coding-system)))

(defun w3m-detect-coding-region (start end &optional priority-list)
  "Detect coding system of the text in the region between START and END.
Return the first possible coding system.

PRIORITY-LIST is a list of coding systems ordered by priority."
  (let (category categories codesys)
    (dolist (codesys priority-list)
      (setq category (or (coding-system-category codesys)
			 (coding-system-name codesys)))
      (unless (assq category categories)
	(push (cons category codesys) categories)))
    (if (consp (setq codesys (w3m-detect-coding-with-priority
			      start end (nreverse categories))))
	(car codesys)
      codesys)))

(defun w3m-decode-coding-string-with-priority (str coding)
  "Decode the string STR which is encoded in CODING.
If CODING is a list, look for the coding system using it as a priority
list."
  (w3m-static-if (and (fboundp 'find-coding-system)
		      (subrp (symbol-function 'find-coding-system)))
      (if (listp coding)
	  (with-temp-buffer
	    (insert str)
	    (let* ((orig-category-list (coding-priority-list))
		   (orig-category-systems (mapcar #'coding-category-system
						  orig-category-list))
		   codesys category priority-list)
	      (unwind-protect
		  (progn
		    (while coding
		      (setq codesys (car coding)
			    coding (cdr coding)
			    category (or (coding-system-category codesys)
					 (coding-system-name codesys)))
		      (unless (or (eq (coding-system-type codesys) 'undecided)
				  (assq category priority-list))
			(set-coding-category-system category codesys)
			(push category priority-list)))
		    (set-coding-priority-list (nreverse priority-list))
		    ;; `detect-coding-region' always returns `undecided'
		    ;; ignoring `priority-list' in XEmacs 21.5-b19, but
		    ;; that's okay.
		    (when (consp (setq codesys (detect-coding-region
						(point-min) (point-max))))
		      (setq codesys (car codesys)))
		    (decode-coding-region (point-min) (point-max)
					  (or codesys
					      w3m-default-coding-system
					      w3m-coding-system
					      'iso-2022-7bit))
		    (buffer-string))
		(set-coding-priority-list orig-category-list)
		(while orig-category-list
		  (set-coding-category-system (car orig-category-list)
					      (car orig-category-systems))
		  (setq orig-category-list (cdr orig-category-list)
			orig-category-systems (cdr orig-category-systems))))))
	(decode-coding-string str coding))
    str))

(eval-when-compile (autoload 'ucs-to-char "unicode"))

;; This might be redefined by w3m-ucs.el.
(cond ((and (fboundp 'unicode-to-char) ;; XEmacs 21.5.
	    (subrp (symbol-function 'unicode-to-char)))
       (if (featurep 'mule)
	   (defalias 'w3m-ucs-to-char 'unicode-to-char)
	 (defun w3m-ucs-to-char (codepoint)
	   (or (unicode-to-char codepoint) ?~))))
      ((featurep 'mule)
       (defun w3m-ucs-to-char (codepoint)
	 (if (fboundp 'ucs-to-char) ;; Mule-UCS is loaded.
	     (progn
	       (defalias 'w3m-ucs-to-char
		 (lambda (codepoint)
		   (condition-case nil
		       (or (ucs-to-char codepoint) ?~)
		     (error ?~))))
	       (w3m-ucs-to-char codepoint))
	   (condition-case nil
	       (or (int-to-char codepoint) ?~)
	     (error ?~)))))
      (t
       (defun w3m-ucs-to-char (codepoint)
	 (condition-case nil
	     (or (int-to-char codepoint) ?~)
	   (error ?~)))))

;;; Handle images:

;; Function which returns non-nil when the current display device can
;; show images inline.
(defalias 'w3m-display-graphic-p 'device-on-window-system-p)

(defun w3m-display-inline-images-p ()
  "Returns non-nil when images can be displayed under the present
circumstances."
  (and w3m-display-inline-images (device-on-window-system-p)))

(defvar w3m-should-unoptimize-animated-gifs t
  "Specify whether w3m should unoptimize animated gif images for showing.")

(defvar w3m-should-convert-interlaced-gifs t
  "Specify whether w3m should convert interlaced gif images to be non-
interlaced.  There is a known bug in the whole version of XEmacs so
far that it may crash when some kind of an interlaced gif image is
displayed.")

(defvar w3m-gifsicle-program (when (exec-installed-p "gifsicle")
			       "gifsicle")
  "*Name of the gifsicle program used to unoptimize animated gif images
or to convert interlaced gif images to be non-interlaced.")

(defvar w3m-cache-fixed-gif-images nil
  "Cache used to keep fixed gif images.  Fixed gif means that an
unoptimized animated gif or a non-interlaced gif which is converted
from an interlaced gif.  The value is an alist of a URL and a glyph.
Glyph will be nil if there is no need to be unoptimized nor to convert
to be non-interlaced.  Each element should be updated when a URL is
newly retrieved.")

(defvar w3m-cache-fixed-gif-images-max-length 32
  "*Number to limit the length of `w3m-cache-fixed-gif-images'.")

(defvar w3m-animated-gif-maximum-size 1048579
  "*Maximum size (width * height * frames) of animated gif images.  If a
size of an image is larger than this (it might be a bomb!), only the
first frame will be shown.  You can make it to be unlimited with the
value nil if your computer has TerrrrrrraBytes of memories.")

(defun w3m-fix-gif (url data no-cache)
  "Return a glyph image of a gif DATA corresponding to a URL which is
suitable for XEmacs.  It will unoptimize an animated gif or convert
an interlaced gif to be non-interlaced.  Otherwise it returns nil when
there is no need to be unoptimized nor to convert to be non-interlaced
\(or, unfortunately, the unoptimization or the conversion is failed).
It manages the cache `w3m-cache-fixed-gif-images'.  If NO-CACHE is
non-nil, a cached data will not be used and it will be updated by a
new glyph image.  See also the documentation for the variable
`w3m-animated-gif-maximum-size'."
  (let ((cache (assoc url w3m-cache-fixed-gif-images)))
    ;; Move the element which is associated with `url' to the
    ;; top of the cache.
    (when (and cache
	       (not (eq cache (car w3m-cache-fixed-gif-images))))
      (setq w3m-cache-fixed-gif-images
	    (cons cache (delq cache w3m-cache-fixed-gif-images))))
    (if (or no-cache
	    (not cache))
	(with-temp-buffer
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary)
		should-be-non-interlaced should-unoptimize size1 size2 glyph)
	    (insert data)
	    (when (looking-at "GIF8[79]")
	      (call-process-region (point-min) (point-max)
				   w3m-gifsicle-program
				   t t nil "--info")
	      (goto-char (point-min))
	      ;; Check whether a `data' is interlaced.
	      (setq should-be-non-interlaced
		    (re-search-forward
		     "  \\+ image #[0-9]+ \\(?:[0-9]+x[0-9]+\\).* interlaced"
		     nil t))
	      (goto-char (point-min))
	      ;; Check whether a `data' is optimized or larger than
	      ;; the value of `w3m-animated-gif-maximum-size'.
	      (when (and w3m-animated-gif-maximum-size
			 (looking-at ".+ \\([0-9]+\\) images\r?$"))
		(setq size1 (string-to-number (match-string 1)))
		(forward-line 1)
		(unless (and (looking-at ".+ \\([0-9]+\\)x\\([0-9]+\\)\r?$")
			     (natnump (setq size1 (* size1
						     (string-to-number
						      (match-string 1)))))
			     (<= size1 w3m-animated-gif-maximum-size)
			     (natnump (setq size1 (* size1
						     (string-to-number
						      (match-string 2)))))
			     (<= size1 w3m-animated-gif-maximum-size))
		  ;; It should be truncated to be only one frame.
		  (setq should-unoptimize "#0"))
		(setq size1 nil)
		(while (and (not should-unoptimize)
			    (re-search-forward
			     "  \\+ image #[0-9]+ \\([0-9]+x[0-9]+\\)"
			     nil t))
		  (if size1
		      (if (string-equal size1 (setq size2 (match-string 1)))
			  (setq size1 size2)
			(setq should-unoptimize "--unoptimize"))
		    (setq size1 (match-string 1)))))
	      (when (or should-unoptimize should-be-non-interlaced)
		(erase-buffer)
		(insert data)
		;; Unoptimize anyway.
		(if should-unoptimize
		    (call-process-region (point-min) (point-max)
					 w3m-gifsicle-program
					 t t nil should-unoptimize
					 "--no-interlace")
		  (call-process-region (point-min) (point-max)
				       w3m-gifsicle-program
				       t t nil "--no-interlace"))
		(goto-char (point-min))
		(when (or (looking-at "GIF8[79]")
			  ;; Unoptimization is failed. :-<
			  ;; Attempt to extract the first frame.
			  (progn
			    (erase-buffer)
			    (insert data)
			    (call-process-region (point-min) (point-max)
						 w3m-gifsicle-program
						 t t nil "#0" "--no-interlace")
			    (goto-char (point-min))
			    (looking-at "GIF8[79]")))
		  ;; Perhaps the unoptimization is succeeded.
		  (setq glyph
			(make-glyph (vector 'gif :data (buffer-string)))))))
	    ;; Update a cache.
	    (if cache
		(setcdr cache glyph)
	      (push (cons url glyph) w3m-cache-fixed-gif-images)
	      (let ((maxlen w3m-cache-fixed-gif-images-max-length))
		(when (and (integerp maxlen)
			   (>= maxlen 1)
			   (> (length w3m-cache-fixed-gif-images) maxlen))
		  (setcdr (nthcdr (1- maxlen) w3m-cache-fixed-gif-images)
			  nil))))
	    glyph))
      ;; Use a cached glyph.
      (cdr cache))))

(defun w3m-make-glyph (data type)
  (or (and (eq type 'xbm)
	   (let (width height content)
	     (with-temp-buffer
	       (insert data)
	       (goto-char (point-min))
	       (if (re-search-forward "width[ \t]+\\([0-9]+\\)")
		   (setq width (string-to-number (match-string 1))))
	       (if (re-search-forward "height[ \t]+\\([0-9]+\\)")
		   (setq height (string-to-number (match-string 1))))
	       (while (re-search-forward "0x\\(..\\)" nil t)
		 (setq content (cons
				(string-to-number
				 (match-string 1) 16) content)))
	       (setq content (concat (nreverse content))))
	     (make-glyph (vector 'xbm :data (list width height content)))))
      (make-glyph (vector type :data data))))

(defun w3m-create-image (url &optional no-cache referer size handler)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used.
If second optional argument REFERER is non-nil, it is used as Referer: field.
Third optional argument SIZE is currently ignored."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url no-cache referer handler))
    (lexical-let ((url url)
		  (set-size size)
		  (no-cache no-cache)
		  size)
      (w3m-process-do-with-temp-buffer
	  (type (condition-case err
		    (w3m-retrieve url nil no-cache nil referer handler)
		  (error (message "While retrieving %s: %s" url err) nil)))
	(goto-char (point-min))
	(when (w3m-image-type-available-p
	       (setq type
		     (or (and (let (case-fold-search)
				(looking-at
				 "\\(GIF8\\)\\|\\(\377\330\\)\\|\211PNG\r\n"))
			      (cond ((match-beginning 1) 'gif)
				    ((match-beginning 2) 'jpeg)
				    (t 'png)))
			 (w3m-image-type type))))
	  (let ((data (buffer-string))
		glyph)
	    (setq glyph (or (and (eq type 'gif)
				 (or w3m-should-unoptimize-animated-gifs
				     w3m-should-convert-interlaced-gifs)
				 w3m-gifsicle-program
				 (w3m-fix-gif url data no-cache))
			    (w3m-make-glyph data type)))
	    (if (and w3m-resize-images set-size)
		(progn
		  (setq size (cons (glyph-width glyph)
				   (glyph-height glyph)))
		  (if (and (null (car set-size)) (cdr set-size))
		      (setcar set-size
			      (/ (* (car size) (cdr set-size))
				 (cdr size))))
		  (if (and (null (cdr set-size)) (car set-size))
		      (setcdr set-size
			      (/ (* (cdr size) (car set-size))
				 (car size))))
		  (if (or (not (eq (car size)
				   (car set-size))) ;; width is different
			  (not (eq (cdr size)
				   (cdr set-size)))) ;; height is different
		      (lexical-let ((type type))
			(w3m-process-do
			    (resized (w3m-resize-image
				      data
				      (car set-size)(cdr set-size)
				      handler))
			  (w3m-make-glyph resized type)))
		    glyph))
	      glyph)))))))

(defun w3m-create-resized-image (url rate &optional referer size handler)
  "Resize an cached image object.
URL is the image file's url.
RATE is resize percentage.
If REFERER is non-nil, it is used as Referer: field.
If SIZE is non-nil, its car element is used as width
and its cdr element is used as height."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url nil referer size handler))
    (lexical-let ((url url)
		  (rate rate)
		  fmt data)
      (w3m-process-do-with-temp-buffer
	  (type (w3m-retrieve url nil nil nil referer handler))
	(when (w3m-image-type-available-p (setq type (w3m-image-type type)))
	  (setq data (buffer-string)
		fmt type)
	  (w3m-process-do
	      (resized (w3m-resize-image-by-rate data rate handler))
	    (when resized
	      (or (and (eq fmt 'gif)
		       (or w3m-should-unoptimize-animated-gifs
			   w3m-should-convert-interlaced-gifs)
		       w3m-gifsicle-program
		       (let (w3m-cache-fixed-gif-images)
			 (w3m-fix-gif url resized t)))
		  (w3m-make-glyph resized fmt)))))))))

(defun w3m-insert-image (beg end image &rest args)
  "Display IMAGE in the current buffer.
A buffer string between BEG and END are replaced with IMAGE."
  (let (glyphs)
    (map-extents
     (lambda (extent maparg)
       (push (extent-end-glyph extent) glyphs)
       (set-extent-end-glyph extent nil)
       nil)
     nil beg beg nil nil 'w3m-xmas-icon)
    ;; Display an image on the right hand.
    (push image glyphs)
    (when (extent-at end nil 'invisible nil 'at)
      (setq end (next-single-property-change end 'invisible))
      (map-extents
       (lambda (extent maparg)
	 (push (extent-end-glyph extent) glyphs)
	 (set-extent-end-glyph extent nil)
	 nil)
       nil end end nil nil 'w3m-xmas-icon))
    (set-extent-properties (make-extent beg end) (list 'w3m-xmas-icon t
						       'invisible t))
    (while glyphs
      (set-extent-properties (make-extent end end)
			     (list 'w3m-xmas-icon t
				   'end-glyph (pop glyphs))))))

(defun w3m-remove-image (beg end)
  "Remove images between BEG and END."
  (map-extents (lambda (extent maparg)
		 (delete-extent extent)
		 nil)
	       nil beg end nil 'end-closed 'w3m-xmas-icon))

(defun w3m-image-type-available-p (image-type)
  "Return non-nil if an image with IMAGE-TYPE can be displayed inline."
  (and (device-on-window-system-p)
       (featurep image-type)))

;;; Toolbar:
(defcustom w3m-use-toolbar (and (featurep 'toolbar) t)
  "Non-nil activates toolbar of w3m."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-toolbar-icon-preferred-image-types '(xpm)
  "List of image types that you prefer to use for the tool bar icons."
  :group 'w3m
  :type '(repeat (symbol :tag "Image type"))
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (when (and (not noninteractive) (boundp 'w3m-toolbar-buttons))
	     (dolist (buffer (w3m-list-buffers t))
	       (w3m-setup-toolbar t buffer))))))

(defcustom w3m-toolbar-use-single-image-per-icon nil
  "Non-nil means use single image (named possibly *-up) per icon.
If it is nil, subsidiaries, e.g., *-down and *-disabled, if any, are
used together."
  :group 'w3m
  :type 'boolean
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (when (and (not noninteractive) (boundp 'w3m-toolbar-buttons))
	     (dolist (buffer (w3m-list-buffers t))
	       (w3m-setup-toolbar t buffer))))))

(defun w3m-find-image (name &optional directory)
  "Find image file for NAME and return cons of file name and type.
This function searches only in DIRECTORY, that defaults to the value of
`w3m-icon-directory', for an image file of which the base name is NAME.
Files of types that XEmacs does not support are ignored."
  (unless directory
    (setq directory w3m-icon-directory))
  (when (and directory
	     (file-directory-p directory)
	     (device-on-window-system-p))
    (let* ((case-fold-search nil)
	   (files (directory-files directory t
				   (concat "\\`" (regexp-quote name) "\\.")))
	   (types (append w3m-toolbar-icon-preferred-image-types
			  (if (boundp 'image-formats-alist)
			      (mapcar 'cdr
				      (symbol-value 'image-formats-alist))
			    '(png gif jpeg tiff xbm xpm bmp))))
	   file type rest)
      (while files
	(when (string-match "\\.\\([^.]+\\)\\'" (setq file (pop files)))
	  (setq type (intern (downcase (match-string 1 file))))
	  (setq type (or (cdr (assq type '((tif . tiff)
					   (jpg . jpeg))))
			 type))
	  (when (featurep type)
	    (push (cons file (memq type types)) rest))))
      (setq rest (car (sort rest (lambda (a b) (> (length a) (length b))))))
      (when (cdr rest)
	(cons (car rest) (cadr rest))))))

(defun w3m-toolbar-make-buttons (buttons &optional force)
  (let (button icon down disabled up)
    (while buttons
      (setq button (pop buttons)
	    icon (intern (concat "w3m-toolbar-" button "-icon")))
      (when (or force (not (boundp icon)))
	(setq down (w3m-find-image (concat button "-down"))
	      disabled (w3m-find-image (concat button "-disabled")))
	(if (setq up (or (w3m-find-image (concat button "-up"))
			 (prog1
			     (or down disabled (w3m-find-image button))
			   (setq down nil
				 disabled nil))))
	    (set icon (if w3m-toolbar-use-single-image-per-icon
			  (toolbar-make-button-list (car up))
			(toolbar-make-button-list (car up)
						  (car down)
						  (car disabled))))
	  (error "Icon file %s-up.* not found" button))))))

(defun w3m-setup-toolbar (&optional force buffer)
  "Setup toolbar."
  (when (and w3m-use-toolbar
	     (w3m-find-image "antenna-up"))
    (w3m-toolbar-make-buttons w3m-toolbar-buttons force)
    (set-specifier default-toolbar
		   (cons (or buffer (current-buffer)) w3m-toolbar))
    t))

(defun w3m-update-toolbar ()
  "Update toolbar."
  (when (and w3m-use-toolbar
	     (or (and (boundp 'w3m-toolbar-antenna-icon)
		      (symbol-value 'w3m-toolbar-antenna-icon))
		 (w3m-setup-toolbar)))
    (set-specifier default-toolbar
		   (cons (current-buffer) w3m-toolbar))))

;;; Menu:
(defun w3m-menu-on-forefront (arg &optional curbuf)
  "Place emacs-w3m menus on the forfront of the menu bar if ARG is non-nil.
If CURBUF is given, this function works only in the current buffer,
otherwise works in all the emacs-w3m buffers."
  (if curbuf
      (let ((w3m (car (find-menu-item current-menubar '("w3m"))))
	    (bookmark (car (find-menu-item current-menubar '("Bookmark"))))
	    (tab (car (find-menu-item current-menubar '("Tab"))))
	    (session (car (find-menu-item current-menubar '("Session"))))
	    (items (copy-sequence current-menubar)))
	(when (or w3m bookmark tab session)
	  (setq items (delq session (delq tab (delq bookmark (delq w3m items)))))
	  (set-buffer-menubar
	   (cond (arg
		  (append (delq nil (list w3m bookmark tab session)) items))
		 ((memq nil items)
		  (append (nreverse (cdr (memq nil (reverse items))))
			  (delq nil (list w3m bookmark tab session))
			  (memq nil items)))
		 (t
		  (nconc items (delq nil (list w3m bookmark tab session))))))))
    (save-current-buffer
      (dolist (buffer (w3m-list-buffers t))
	(set-buffer buffer)
	(w3m-menu-on-forefront arg t)))))

(defun w3m-setup-menu ()
  "Define menubar buttons for XEmacs."
  (when (and (featurep 'menubar)
	     current-menubar
	     (not (assoc (car w3m-menubar) current-menubar)))
    (w3m-setup-session-menu)
    (when w3m-use-tab-menubar (w3m-setup-tab-menu))
    (w3m-setup-bookmark-menu)
    (set-buffer-menubar (cons w3m-menubar current-menubar))
    (w3m-menu-on-forefront w3m-menu-on-forefront t)))

;;; Widget:
(eval-when-compile (require 'wid-edit))

(defun w3m-define-missing-widgets ()
  "Define some missing widgets."
  (unless (get 'coding-system 'widget-type)
    ;; The following codes are imported from wid-edit.el of Emacs 20.7.

    (defvar widget-coding-system-prompt-value-history nil
      "History of input to `widget-coding-system-prompt-value'.")

    (defun widget-coding-system-prompt-value (widget prompt value unbound)
      ;; Read coding-system from minibuffer.
      (intern
       (completing-read (format "%s (default %s) " prompt value)
			(mapcar (lambda (sym)
				  (list (symbol-name sym)))
				(coding-system-list)))))

    (defun widget-coding-system-action (widget &optional event)
      ;; Read a file name from the minibuffer.
      (let ((answer
	     (eval (list 'widget-coding-system-prompt-value
			 widget
			 (widget-apply widget :menu-tag-get)
			 (widget-value widget)
			 t))))
	(widget-value-set widget answer)
	(widget-apply widget :notify widget event)
	(widget-setup))))

    (define-widget 'coding-system 'symbol
      "A MULE coding-system."
      :format "%{%t%}: %v"
      :tag "Coding system"
      :prompt-history 'widget-coding-system-prompt-value-history
      :prompt-value 'widget-coding-system-prompt-value
      :action 'widget-coding-system-action)

  (unless (get 'other 'widget-type)
    ;; The following definition is imported from wid-edit.el of Emacs 20.7.
    (define-widget 'other 'sexp
      "Matches any value, but doesn't let the user edit the value.
This is useful as last item in a `choice' widget.
You should use this widget type with a default value,
as in (other DEFAULT) or (other :tag \"NAME\" DEFAULT).
If the user selects this alternative, that specifies DEFAULT
as the value."
      :tag "Other"
      :format "%t%n"
      :value 'other)))

(eval-after-load "wid-edit" '(w3m-define-missing-widgets))

;;; Header line:
(defvar w3m-header-line-map (make-sparse-keymap))
(define-key w3m-header-line-map 'button2 'w3m-goto-url)

;;; Gutter:
(defcustom w3m-show-current-title-in-buffer-tab
  (and (boundp 'gutter-buffers-tab-enabled)
       gutter-buffers-tab-enabled)
  "If non-nil, show the title strings in the buffers tab.
It has no effect if your XEmacs does not support the gutter items."
  :group 'w3m
  :type 'boolean
  :get (lambda (symbol)
	 (if (boundp 'gutter-buffers-tab-enabled)
	     (default-value symbol)))
  :set (lambda (symbol value)
	 (prog2
	     (or (boundp 'gutter-buffers-tab-enabled)
		 (setq value nil))
	     (custom-set-default symbol value)
	   (if value
	       (add-hook 'w3m-display-functions 'w3m-update-tab-in-gutter)
	     (remove-hook 'w3m-display-functions 'w3m-update-tab-in-gutter))
	   (condition-case nil
	       (progn
		 (if value
		     (ad-enable-advice
		      'format-buffers-tab-line 'around
		      'w3m-show-current-title-in-buffer-tab)
		   (ad-disable-advice
		    'format-buffers-tab-line 'around
		    'w3m-show-current-title-in-buffer-tab))
		 (if (boundp 'gutter-buffers-tab-enabled)
		     (mapc #'update-tab-in-gutter (frame-list))))
	     (error)))))

(when (boundp 'gutter-buffers-tab-enabled)
  (defadvice format-buffers-tab-line
    (around w3m-show-current-title-in-buffer-tab (buffer) activate)
    "Advised by emacs-w3m.
Show the current title string in the buffer tab.  Unfortunately,
existing XEmacs does not support showing non-ascii characters.  When a
title contains non-ascii characters, show a url name by default."
    (with-current-buffer buffer
      (if (and w3m-show-current-title-in-buffer-tab
	       (symbol-value 'gutter-buffers-tab-enabled)
	       (eq 'w3m-mode major-mode))
	  (let* ((len (specifier-instance
		       (symbol-value 'buffers-tab-default-buffer-line-length)))
		 (name (if (and (stringp w3m-current-title)
				(string-match "^[ -~]+$" w3m-current-title))
			   w3m-current-title
			 (if (stringp w3m-current-url)
			     (directory-file-name
			      (if (string-match "^[^/:]+:/+" w3m-current-url)
				  (substring w3m-current-url (match-end 0))
				w3m-current-url))
			   "")))
		 (num (if (string-match ".*<\\(.+\\)>$" (buffer-name buffer))
			  (match-string 1 (buffer-name buffer))))
		 (lnum (length num)))
	    (setq ad-return-value
		  (if num
		      (if (and (> len 0)
			       (> (+ (length name) lnum) len))
			  (concat "[" num "]"
				  (substring name 0 (max 0 (- len lnum 5 )))
				  "...")
			(concat "[" num "]" name))
		    (if (and (> len 0)
			     (> (length name) len))
			(concat (substring name 0 (max 0 (- len 3))) "...")
		      name))))
	ad-do-it)))

  (if w3m-show-current-title-in-buffer-tab
      (ad-enable-advice 'format-buffers-tab-line 'around
			'w3m-show-current-title-in-buffer-tab)
    (ad-disable-advice 'format-buffers-tab-line 'around
		       'w3m-show-current-title-in-buffer-tab))

  (defun w3m-setup-tab-in-gutter ()
    "Set up buffers tab in the gutter."
    (set-specifier default-gutter-visible-p
		   (and w3m-use-tab gutter-buffers-tab-enabled t)
		   (current-buffer)))
  (add-hook 'w3m-mode-setup-functions 'w3m-setup-tab-in-gutter)
  (add-hook 'w3m-select-buffer-mode-hook 'w3m-setup-tab-in-gutter)

  (defun w3m-update-tab-in-gutter (&rest args)
    "Update the tab control in the gutter area."
    (when (and w3m-use-tab gutter-buffers-tab-enabled)
      (update-tab-in-gutter (selected-frame))))
  (add-hook 'w3m-display-functions 'w3m-update-tab-in-gutter))

;;; Graphic icons:
(defcustom w3m-space-before-modeline-icon ""
  "String of space character(s) to be put in front of the modeline icon.
It may be better to use one or more spaces if you are using oblique or
italic font in the modeline."
  :group 'w3m
  :type 'string)

;; Glyph images to be displayed in the modeline.
(defvar w3m-modeline-process-status-on-icon nil)
(defvar w3m-modeline-image-status-on-icon nil)
(defvar w3m-modeline-status-off-icon nil)
(defvar w3m-modeline-ssl-image-status-on-icon nil)
(defvar w3m-modeline-ssl-status-off-icon nil)

(defun w3m-initialize-graphic-icons (&optional force)
  "Make icon images which will be displayed in the modeline."
  (interactive "P")
  ;; Prefer xpm icons rather than png icons since XEmacs doesn't display
  ;; background colors of icon images other than xpm images transparently
  ;; in the mode line.
  (let* ((w3m-toolbar-icon-preferred-image-types '(xpm))
	 (defs `((w3m-modeline-status-off-icon
		  ,(w3m-find-image "state-00")
		  w3m-modeline-status-off)
		 (w3m-modeline-image-status-on-icon
		  ,(w3m-find-image "state-01")
		  w3m-modeline-image-status-on)
		 (w3m-modeline-ssl-status-off-icon
		  ,(w3m-find-image "state-10")
		  w3m-modeline-ssl-status-off)
		 (w3m-modeline-ssl-image-status-on-icon
		  ,(w3m-find-image "state-11")
		  w3m-modeline-ssl-image-status-on)))
	 def icon file type status extent keymap)
    (while defs
      (setq def (car defs)
	    defs (cdr defs)
	    icon (car def)
	    file (car (nth 1 def))
	    type (cdr (nth 1 def))
	    status (nth 2 def))
      (if (and w3m-show-graphic-icons-in-mode-line file)
	  (progn
	    (when (or force (not (symbol-value icon)))
	      (unless extent
		(setq extent (make-extent nil nil)
		      keymap (make-sparse-keymap))
		(define-key keymap 'button2
		  (make-modeline-command-wrapper 'w3m-reload-this-page))
		(set-extent-keymap extent keymap)
		(set-extent-property extent 'help-echo
				     "button2 reloads this page"))
	      (set icon
		   (cons extent
			 (make-glyph
			  (make-image-instance (vector type :file file))))))
	    (when (stringp (symbol-value status))
	      ;; Save the original status strings as properties.
	      (put status 'string (symbol-value status)))
	    (set status (list '("" w3m-space-before-modeline-icon) icon)))
	;; Don't use graphic icons.
	(when (get status 'string)
	  (set status (get status 'string)))))
    ;; Spinner.
    (if (and w3m-show-graphic-icons-in-mode-line
	     (device-on-window-system-p)
	     (featurep 'gif)
	     w3m-icon-directory
	     (file-directory-p w3m-icon-directory)
	     (file-exists-p
	      (setq file (expand-file-name "spinner.gif"
					   w3m-icon-directory))))
	(progn
	  (when (or force (not w3m-modeline-process-status-on-icon))
	    (setq extent (make-extent nil nil)
		  keymap (make-sparse-keymap))
	    (define-key keymap 'button2
	      (make-modeline-command-wrapper 'w3m-process-stop))
	    (set-extent-keymap extent keymap)
	    (set-extent-property extent 'help-echo
				 "button2 kills the current process")
	    (setq
	     w3m-modeline-process-status-on-icon
	     (cons
	      extent
	      (make-glyph
	       (if w3m-gifsicle-program
		   ;; XEmacs doesn't support a transparent color on gifs,
		   ;; so we should replace the background color of the image
		   ;; with the modeline's one.
		   (make-image-instance
		    (vector
		     'gif
		     :data
		     (let ((coding-system-for-read 'binary)
			   (coding-system-for-write 'binary)
			   format-alist)
		       (with-temp-buffer
			 (insert-file-contents file)
			 (call-process-region
			  (point-min) (point-max) w3m-gifsicle-program t t nil
			  "--careful" "--delay" "10" "--loopcount=forever"
			  "--change-color" "255,255,255"
			  (mapconcat
			   (lambda (c) (number-to-string (% c 256)))
			   (color-rgb-components
			    (face-background 'modeline))
			   ","))
			 (buffer-string)))))
		 (make-image-instance (vector 'gif :file file)))))))
	  (when (stringp w3m-modeline-process-status-on)
	    ;; Save the original status string as a property.
	    (put 'w3m-modeline-process-status-on 'string
		 w3m-modeline-process-status-on))
	  (setq w3m-modeline-process-status-on
		'(("" w3m-space-before-modeline-icon)
		  w3m-modeline-process-status-on-icon)))
      ;; Don't use spinner.
      (when (get 'w3m-modeline-process-status-on 'string)
	(setq w3m-modeline-process-status-on
	      (get 'w3m-modeline-process-status-on 'string))))))

;;; Miscellaneous:
(if (featurep 'mule)
    (defalias 'multibyte-string-p 'stringp)
  (defalias 'multibyte-string-p 'ignore))

(if (featurep 'mule)
    (defun w3m-mule-unicode-p ()
      "Check the existence as charsets of mule-unicode."
      (and (find-charset 'mule-unicode-0100-24ff)
	   (find-charset 'mule-unicode-2500-33ff)
	   (find-charset 'mule-unicode-e000-ffff)))
  (defalias 'w3m-mule-unicode-p 'ignore))

(when (featurep 'mule)
  (defun w3m-window-hscroll (&optional window)
    "Replacement of `window-hscroll' for XEmacs-Mule.
XEmacs does not work correctly in the display control in case buffer
contains characters of various width.  This function does not
necessarily solve the problem completely."
    (let ((hs (window-hscroll window))
	  (spos (point-at-bol))
	  (epos (point-at-eol))
	  (buf (window-buffer window)))
      (save-selected-window
	(save-excursion ;; Don't replace it with:
	  (set-buffer buf) ;; (with-current-buffer buf
	  (beginning-of-line)
	  (condition-case nil
	      (forward-char hs)
	    (error (goto-char (point-max))))
	  (if (< epos (point))
	      (+ hs (- (string-width (buffer-substring spos epos))
		       (- epos spos)))
	    (string-width (buffer-substring spos (point))))))))

  (defun w3m-current-column ()
    "Replacement of `current-column' for XEmacs-Mule.
XEmacs does not work correctly in the display control in case buffer
contains characters of various width.  This function does not
necessarily solve the problem completely."
    (- (point) (point-at-bol)))

  (defun w3m-set-window-hscroll (window columns)
    "Replacement of `set-window-hscroll' for XEmacs-Mule.
XEmacs does not work correctly in the display control in case buffer
contains characters of various width.  This function does not
necessarily solve the problem completely."
    (save-excursion
      (move-to-column (max columns 0))
      (if (> columns (current-column))
	  (set-window-hscroll window (+ (- (point-at-eol) (point-at-bol))
					(- columns (current-column))))
	(set-window-hscroll window (- (point) (point-at-bol))))))
  )

(defun w3m-form-coding-system-accept-region-p (&optional from to coding-system)
  "Check whether `coding-system' can encode specified region."
  (let ((string (buffer-substring (or from (point-min))
				  (or to   (point-max)))))
    (unless (string= (decode-coding-string
		      (encode-coding-string string coding-system)
		      coding-system)
		     string)
      (message "Warning: this text may cause coding-system problem."))
    t))

(provide 'w3m-xmas)

;;; w3m-xmas.el ends here

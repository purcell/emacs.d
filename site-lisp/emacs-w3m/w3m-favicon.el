;;; w3m-favicon.el --- utilities for handling favicon in emacs-w3m

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2009
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

;;; Code:

(eval-when-compile
  (require 'cl))

;;(require 'w3m-util)
;;(require 'w3m-proc)
(require 'w3m-image)

(eval-when-compile
  (defvar w3m-current-buffer)
  (defvar w3m-current-url)
  (defvar w3m-favicon-image)
  (defvar w3m-icon-data)
  (defvar w3m-modeline-favicon)
  (defvar w3m-profile-directory)
  (defvar w3m-use-favicon)
  (defvar w3m-work-buffer-name)
  (defvar w3m-work-buffer-list)
  (autoload 'w3m-expand-url "w3m")
  (autoload 'w3m-load-list "w3m")
  (autoload 'w3m-message "w3m")
  (autoload 'w3m-retrieve "w3m")
  (autoload 'w3m-save-list "w3m")
  (autoload 'w3m-url-readable-string "w3m"))

(defcustom w3m-favicon-size nil
  "Size of favicon.  The value should be `(WIDTH . HEIGHT)' or nil.
Where WIDTH and HEIGHT are positive integers; both or any of them can
be omitted."
  :group 'w3m
  :type '(radio (const :tag "Not specified" nil)
		(cons :format "%v"
		      (integer :format "Width: %v " :size 0 :value 16)
		      (integer :format "Height: %v " :size 0 :value 16))))

(defconst w3m-favicon-name "favicon.ico"
  "The favicon name.")

(add-hook 'w3m-display-functions 'w3m-favicon-setup)

(defcustom w3m-favicon-use-cache-file nil
  "*If non-nil, use favicon cache file."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-favicon-cache-file nil
  "Filename of saving favicon cache.
It defaults to the file named \".favicon\" under the directory specified
by the `w3m-profile-directory' variable."
  :group 'w3m
  :type '(radio (const :format "Not specified\n")
		(file :format "%t: %v\n" :size 0)))

(defcustom w3m-favicon-cache-expire-wait (* 30 24 60 60)
  "*The cache will be expired after specified seconds passed since retrieval.
If this variable is nil, never expired."
  :group 'w3m
  :type '(integer :size 0))

(defcustom w3m-favicon-type
  (let ((types '(png gif pbm xpm bmp))
	type)
    (catch 'det
      (while types
	(setq type (car types)
	      types (cdr types))
	(if (if (featurep 'xemacs)
		(featurep type)
	      (image-type-available-p type))
	    (throw 'det type)))))
  "*Image type of display favicon."
  :group 'w3m
  :type (cons 'radio
	      (let ((types (if (or
				(featurep 'xemacs)
				(not (fboundp 'image-types)))
			       (delq nil
				     (mapcar (lambda (type)
					       (if (featurep type) type))
					     '(gif jpeg png tiff xpm)))
			     (delq 'postscript (copy-sequence image-types)))))
		(nconc (mapcar (lambda (x)
				 `(const :format "%v  " ,x))
			       (butlast types))
		       `((const ,(car (last types))))))))

(defcustom w3m-space-before-favicon " "
  "String of space char(s) to be put in front of favicon in the mode-line.
It may be better to use two or more spaces if you are using oblique or
italic font in the modeline."
  :group 'w3m
  :type 'string)

(defcustom w3m-favicon-convert-args nil
  "List of additional arguments passed to ImageMagick's convert program.
Args that are always passed to convert in addition to this value are:

\(\"-geometry\" \"WIDTHxHEIGHT\" \"fromTYPE:temp-file\" \"toTYPE:-\")

Args might also contain (\"-transparent\" \"COLOR\") in the beginning.
Note that this value is effective only with Emacs 22 and greater."
  :group 'w3m
  :type `(repeat (group :inline t
			:match-inline
			(lambda (widget vals)
			  (if (and (eq (aref (car vals) 0) ?-)
				   (cdr vals)
				   (not (eq (aref (nth 1 vals) 0) ?-)))
			      (cons (list (car vals) (nth 1 vals))
				    (nthcdr 2 vals))
			    (cons (list (car vals)) (cdr vals))))
			(string :format "Arg: %v " :value "-" :size 0)
			(checklist :inline t
				   (string :format "Value: %v\n" :size 0)))))

(defcustom w3m-favicon-default-background nil
  "Color name used as transparent color of favicon image.
Nil means to use the background color of the Emacs frame.  The null
string \"\" is special, that will be replaced with the background color
of the header line or the mode line on which the favicon is displayed.
Note that this value is effective only with Emacs 22 and greater."
  :group 'w3m
  :type '(radio (string :format "Color: %v\n" :size 0
			:match (lambda (widget value)
				 (and (stringp value) (> (length value) 0))))
		(const :tag "Use the background color of the Emacs frame" nil)
		(const :tag "Null string" "")))

(defvar w3m-favicon-type-alist '((pbm . ppm))
  "A list of a difference type of image between Emacs and ImageMagick.
 0. Type of Emacs
 1. Type of ImageMagick")

(defvar w3m-favicon-cache-data nil
  "A list of favicon cache (internal variable).
Each information is a list whose elements are:

 0. URL
 1. (RAW_DATA . TYPE)
 2. DATE when the RAW_DATA was retrieved
 3. IMAGE

Where IMAGE highly depends on the Emacs version and is not saved in
the cache file.")

(w3m-static-if (featurep 'xemacs)
    (set 'w3m-modeline-favicon
	 '("" w3m-space-before-favicon w3m-favicon-image))
  (put 'w3m-modeline-favicon 'risky-local-variable t))
(make-variable-buffer-local 'w3m-modeline-favicon)
(make-variable-buffer-local 'w3m-favicon-image)

(defmacro w3m-favicon-cache-p (url)
  "Say whether the favicon data for URL has been chached."
  `(assoc ,url w3m-favicon-cache-data))

(defmacro w3m-favicon-cache-favicon (url)
  "Pull out the favicon image corresponding to URL from the cache."
  `(nth 3 (assoc ,url w3m-favicon-cache-data)))

(defmacro w3m-favicon-cache-retrieved (url)
  "Return the time when the favicon data for URL was retrieved."
  `(nth 2 (assoc ,url w3m-favicon-cache-data)))

(defmacro w3m-favicon-set-image (image)
  "Set IMAGE to `w3m-favicon-image' and `w3m-modeline-favicon'."
  (if (featurep 'xemacs)
      `(set 'w3m-favicon-image ,image)
    `(when (setq w3m-favicon-image ,image)
       (set 'w3m-modeline-favicon
	    (list ""
		  'w3m-space-before-favicon
		  (propertize "  " 'display w3m-favicon-image)
		  (propertize " " 'display '(space :width 0.5)))))))

(defun w3m-favicon-setup (url)
  "Set up the favicon data in the current buffer.  The buffer-local
variable `w3m-favicon-image' will be set to non-nil value when the
favicon is ready."
  (w3m-favicon-set-image nil)
  (when (and w3m-use-favicon
	     w3m-current-url
	     (w3m-static-if (featurep 'xemacs)
		 (and (device-on-window-system-p)
		      (featurep w3m-favicon-type))
	       (and (display-images-p)
		    (image-type-available-p w3m-favicon-type))))
    (let (icon)
      (cond
       ((and (string-match "\\`about://\\([^/]+\\)/" url)
	     (setq icon (intern-soft (concat "w3m-about-" (match-string 1 url)
					     "-favicon"))))
	(with-current-buffer w3m-current-buffer
	  (w3m-favicon-set-image
	   (w3m-favicon-convert
	    (base64-decode-string (symbol-value icon)) 'ico))))
       ((or (string-match "\\`https?://" url)
	    (and (string-match "\\`about://\\(?:header\\|source\\)/https?://"
			       url)
		 (setq url (substring url 15))))
	(if w3m-icon-data
	    (w3m-favicon-retrieve (car w3m-icon-data) (cdr w3m-icon-data)
				  w3m-current-buffer)
	  (w3m-favicon-retrieve (w3m-expand-url (concat "/" w3m-favicon-name)
						url)
				'ico w3m-current-buffer)))))))

(defun w3m-favicon-convert (data type)
  "Convert the favicon DATA in TYPE to the favicon image and return it."
  (when (or (not (eq type 'ico))
	    ;; Since most of favicons are the `ico' types, we make sure
	    ;; of the magic-numbers only as for them.
	    (string-equal "\x00\x00\x01\x00" (substring data 0 4)))
    (let ((height (or (cdr w3m-favicon-size)
		      (w3m-static-if (featurep 'xemacs)
			  (face-height 'default)
			(frame-char-height))))
	  (new (w3m-static-unless (featurep 'xemacs)
		 (>= emacs-major-version 22)))
	  bg args img)
      ;; Examine the transparent color of the image.
      (when (and w3m-imagick-identify-program
		 (equal w3m-favicon-default-background ""))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert data)
	  (let ((coding-system-for-read 'raw-text)
		(coding-system-for-write 'binary))
	    (condition-case nil
		(call-process-region (point-min) (point-max)
				     w3m-imagick-identify-program
				     t t nil "-verbose" (format "%s:-" type))
	      (error)))
	  (goto-char (point-min))
	  (setq case-fold-search t)
	  (while (and (not bg)
		      (re-search-forward "^ *Transparent +color: *\
\\([^\n ]+\\(?: +[^\n ]+\\)*\\)" nil t))
	    (when (string-match "\\`none\\'" (setq bg (match-string 1)))
	      (setq bg nil)))))
      (setq args (list "-geometry"
		       (format "%dx%d"
			       (or (car w3m-favicon-size) height) height)))
      (w3m-static-unless (featurep 'xemacs)
	(when new
	  ;; "-transparent" should precede the other arguments.
	  (setq args (nconc (when bg (list "-transparent" bg))
			    args
			    w3m-favicon-convert-args))))
      (setq img (apply
		 #'w3m-imagick-convert-data
		 data (symbol-name type)
		 (symbol-name (or (cdr (assq w3m-favicon-type
					     w3m-favicon-type-alist))
				  w3m-favicon-type))
		 args))
      (when img
	(w3m-static-if (featurep 'xemacs)
	    (make-glyph
	     (make-image-instance (vector w3m-favicon-type :data img)))
	  (if new
	      (create-image img w3m-favicon-type t
			    :ascent 'center
			    :background w3m-favicon-default-background)
	    (create-image img w3m-favicon-type t :ascent 'center)))))))

(defun w3m-favicon-retrieve (url type target)
  "Retrieve favicon from URL and convert it to image as TYPE in TARGET.
TYPE is a symbol like `ico' and TARGET is a buffer where the image is
stored in the `w3m-favicon-image' buffer-local variable."
  (if (and (w3m-favicon-cache-p url)
	   (or (null w3m-favicon-cache-expire-wait)
	       (< (- (w3m-float-time)
		     (w3m-float-time (w3m-favicon-cache-retrieved url)))
		  w3m-favicon-cache-expire-wait)))
      (with-current-buffer target
	(w3m-favicon-set-image (w3m-favicon-cache-favicon url)))
    (lexical-let ((url url)
		  (type type)
		  (target target)
		  (silent w3m-message-silent))
      (w3m-process-with-null-handler
	(w3m-process-do-with-temp-buffer
	    (ok (w3m-retrieve url 'raw nil nil nil handler))
	  (let ((w3m-message-silent silent)
		idata image)
	    (if (and ok
		     ;; Some broken servers provides empty contents.
		     (>= (buffer-size) 4))
		(setq idata (buffer-string)
		      image (w3m-favicon-convert idata type))
	      (w3m-message "Reading %s...done (no favicon)"
			   (w3m-url-readable-string url)))
	    (with-current-buffer target
	      (w3m-favicon-set-image image)
	      (push (list url idata (current-time) w3m-favicon-image)
		    w3m-favicon-cache-data)))))))
  ;; Emacs frame needs to be redisplayed to make favicon come out.
  (w3m-force-window-update-later target 1))

(defun w3m-favicon-save-cache-file ()
  "Save the cached favicon data into the local file."
  (when w3m-favicon-use-cache-file
    (w3m-save-list (or w3m-favicon-cache-file
		       (expand-file-name ".favicon" w3m-profile-directory))
		   (delq nil (mapcar (lambda (elem)
				       (when (= (length elem) 4)
					 (butlast elem)))
				     w3m-favicon-cache-data))
		   'binary)))

(defun w3m-favicon-load-cache-file ()
  "Load the cached favicon data from the local file."
  (when (and w3m-favicon-use-cache-file
	     (null w3m-favicon-cache-data))
    (let ((cache (w3m-load-list
		  (or w3m-favicon-cache-file
		      (expand-file-name ".favicon" w3m-profile-directory))
		  'binary))
	  elem data image)
      (while cache
	(setq elem (car cache)
	      cache (cdr cache)
	      data (cadr elem))
	(when (stringp data)
	  (setcar (cdr elem) (setq data (cons data 'ico))))
	(when (setq image (condition-case nil
			      (w3m-favicon-convert (car data) (cdr data))
			    (error nil)))
	  (push (nconc elem (list image)) w3m-favicon-cache-data))))))

(add-hook 'w3m-arrived-setup-functions 'w3m-favicon-load-cache-file)
(add-hook 'w3m-arrived-shutdown-functions 'w3m-favicon-save-cache-file)

(provide 'w3m-favicon)

;;; w3m-favicon.el ends here

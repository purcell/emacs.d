;;; w3m-image.el --- Image conversion routines.

;; Copyright (C) 2001, 2002, 2003, 2005, 2007, 2008
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Yuuichi Teranishi  <teranisi@gohome.org>
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

;; This file contains the stuffs to convert images for emacs-w3m.
;; For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/
;;
;; Routines in this file require ImageMagick's convert.
;; For more detail about ImageMagick, see:
;;
;;    http://www.imagemagick.org/

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m-util)
(require 'w3m-proc)

(eval-when-compile
  (if (not (fboundp 'defcustom))
      (require 'pcustom)))

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-async-exec)
  (defvar w3m-current-url)
  (defvar w3m-profile-directory)
  (defvar w3m-work-buffer-name)
  (defvar w3m-work-buffer-list))

(defcustom w3m-imagick-convert-program (if noninteractive
					   nil
					 (w3m-which-command "convert"))
  "*Program name of ImageMagick's `convert'."
  :group 'w3m
  :set (lambda (symbol value)
	 (custom-set-default symbol (if (and (not noninteractive)
					     value)
					(if (file-name-absolute-p value)
					    (if (file-executable-p value)
						value)
					  (w3m-which-command value)))))
  :type 'file)

(defcustom w3m-imagick-identify-program (if noninteractive
					    nil
					  (w3m-which-command "identify"))
  "*Program name of ImageMagick's `identify'."
  :group 'w3m
  :set (lambda (symbol value)
	 (custom-set-default symbol (if (and (not noninteractive)
					     value)
					(if (file-name-absolute-p value)
					    (if (file-executable-p value)
						value)
					  (w3m-which-command value)))))
  :type 'file)

;;; Image handling functions.
(defcustom w3m-resize-images (and w3m-imagick-convert-program t)
  "*If non-nil, resize images to the specified width and height."
  :group 'w3m
  :set (lambda (symbol value)
	 (custom-set-default symbol (and w3m-imagick-convert-program value)))
  :type 'boolean)

(put 'w3m-imagick-convert-program 'available-p 'unknown)

(defun w3m-imagick-convert-program-available-p ()
  "Return non-nil if ImageMagick's `convert' program is available.
If not, `w3m-imagick-convert-program' and `w3m-resize-images' are made
nil forcibly."
  (cond ((eq (get 'w3m-imagick-convert-program 'available-p) 'yes)
	 t)
	((eq (get 'w3m-imagick-convert-program 'available-p) 'no)
	 nil)
	((and (stringp w3m-imagick-convert-program)
	      (file-executable-p w3m-imagick-convert-program))
	 (put 'w3m-imagick-convert-program 'available-p 'yes)
	 ;; Check whether convert supports png32.
	 (put 'w3m-imagick-convert-program 'png32
	      (unless (or (featurep 'xemacs)
			  (< emacs-major-version 22))
		(with-temp-buffer
		  (set-buffer-multibyte nil)
		  (insert "P1 1 1 1")
		  (condition-case nil
		      (call-process-region (point-min) (point-max)
					   w3m-imagick-convert-program
					   t t nil "pbm:-" "png32:-")
		    (error))
		  (goto-char (point-min))
		  (looking-at "\211PNG\r\n"))))
	 t)
	(t
	 (when w3m-imagick-convert-program
	   (message "ImageMagick's `convert' program is not available")
	   (sit-for 1))
	 (setq w3m-imagick-convert-program nil
	       w3m-resize-images nil)
	 (put 'w3m-imagick-convert-program 'available-p 'no)
	 nil)))

;;; Synchronous image conversion.
(defun w3m-imagick-convert-buffer (from-type to-type &rest args)
  (when (w3m-imagick-convert-program-available-p)
    (let* ((in-file (make-temp-name
		     (expand-file-name "w3mel" w3m-profile-directory)))
	   (buffer-file-coding-system 'binary)
	   (coding-system-for-read 'binary)
	   (coding-system-for-write 'binary)
	   (default-process-coding-system (cons 'binary 'binary))
	   return)
      (write-region (point-min) (point-max) in-file nil 'nomsg)
      (erase-buffer)
      (setq return
	    (apply 'call-process
		   w3m-imagick-convert-program
		   nil t nil
		   (append args (list
				 (concat
				  (if from-type
				      (concat from-type ":"))
				  in-file)
				 (if to-type
				     (if (and (string-equal to-type "png")
					      (get 'w3m-imagick-convert-program
						   'png32))
					 "png32:-"
				       (concat to-type ":-"))
				   "-")))))
      (when (file-exists-p in-file) (delete-file in-file))
      (if (and (numberp return)
	       (zerop return))
	  t
	(message "Image conversion failed (code `%s')"
		 (if (stringp return)
		     (string-as-multibyte return)
		   return))
	nil))))

(defun w3m-imagick-convert-data (data from-type to-type &rest args)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert data)
    (and (apply 'w3m-imagick-convert-buffer from-type to-type args)
	 (not (zerop (buffer-size)))
	 (buffer-string))))

;;; Asynchronous image conversion.
(defun w3m-imagick-start-convert-data (handler
				       data from-type to-type &rest args)
  (w3m-process-do-with-temp-buffer
      (success (when (w3m-imagick-convert-program-available-p)
		 (set-buffer-multibyte nil)
		 (insert data)
		 (apply 'w3m-imagick-start-convert-buffer
			handler from-type to-type args)))
    (if (and success
	     (not (zerop (buffer-size))))
	(buffer-string))))

(defun w3m-imagick-start-convert-buffer (handler from-type to-type &rest args)
  (lexical-let ((in-file (make-temp-name
			  (expand-file-name "w3mel" w3m-profile-directory)))
		(out-buffer (current-buffer)))
    (setq w3m-current-url "non-existent")
    (let ((coding-system-for-write 'binary)
	  (buffer-file-coding-system 'binary)
	  jka-compr-compression-info-list
	  format-alist)
      (write-region (point-min) (point-max) in-file nil 'nomsg))
    (w3m-process-do
	(success (with-current-buffer out-buffer
		   (erase-buffer)
		   (w3m-process-start
		    handler
		    w3m-imagick-convert-program
		    (append args
			    (list
			     (concat
			      (if from-type
				  (concat from-type ":"))
			      in-file)
			     (if to-type
				 (if (and (string-equal to-type "png")
					  (get 'w3m-imagick-convert-program
					       'png32))
				     "png32:-"
				   (concat to-type ":-"))
			       "-"))))))
      (when (file-exists-p in-file)
	(delete-file in-file))
      success)))

(defun w3m-resize-image (data width height handler)
  "Resize image DATA to WIDTH and HEIGHT asynchronously.
HANDLER is called after conversion with resized data as an argument."
  (w3m-process-do
      (result (w3m-imagick-start-convert-data
	       handler
	       data nil nil "-geometry"
	       (concat (number-to-string width)
		       "x"
		       (number-to-string height)
		       "!")))
    result))

(defun w3m-resize-image-by-rate (data rate handler)
  "Resize image DATA at RATE asynchronously.
HANDLER is called after conversion with resized data as an argument.
Note that this function requires that the `convert' program allows the
`-resize' option."
  (w3m-process-do
      (result (w3m-imagick-start-convert-data
	       handler
	       data nil nil "-resize"
	       (concat (number-to-string rate) "%")))
    result))

(defun w3m-favicon-usable-p ()
  "Check whether ImageMagick's `convert' supports a Windoze ico format in
a large number of bits per pixel."
  (let ((xpm (condition-case nil
		 (w3m-imagick-convert-data
		  (string 0 0 1 0 1 0 2 1 0 0 1 0 24 0 52 0
			  0 0 22 0 0 0 40 0 0 0 2 0 0 0 2 0
			  0 0 1 0 24 0 0 0 0 0 0 0 0 0 0 0
			  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			  0 255 255 255 0 0 0 0 0 0)
		  "ico" "xpm")
	       (error nil))))
    (and xpm (string-match "\"2 1 2 1\"" xpm) t)))

(provide 'w3m-image)

;;; w3m-image.el ends here

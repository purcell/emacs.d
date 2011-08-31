;;; w3m-ems.el --- GNU Emacs stuff for emacs-w3m

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
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

;; This file contains GNU Emacs stuff that emacs-w3m uses.  For more
;; detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/
;;
;; We can use w3m-static- switches to make the byte code differ between
;; Emacs 2[12] and 2[34], if anything, it is impossible to share the byte
;; code with those versions of Emacsen.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-image)
(require 'w3m-favicon)
(require 'w3m-ccl)
(require 'wid-edit)

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-add-tab-number)
  (defvar w3m-coding-system)
  (defvar w3m-current-process)
  (defvar w3m-current-title)
  (defvar w3m-current-url)
  (defvar w3m-default-coding-system)
  (defvar w3m-display-inline-images)
  (defvar w3m-favicon-image)
  (defvar w3m-form-input-textarea-coding-system)
  (defvar w3m-form-use-fancy-faces)
  (defvar w3m-icon-directory)
  (defvar w3m-image-default-background)
  (defvar w3m-mode-map)
  (defvar w3m-modeline-process-status-on)
  (defvar w3m-new-session-in-background)
  (defvar w3m-process-queue)
  (defvar w3m-show-graphic-icons-in-header-line)
  (defvar w3m-show-graphic-icons-in-mode-line)
  (defvar w3m-toolbar)
  (defvar w3m-toolbar-buttons)
  (defvar w3m-use-favicon)
  (defvar w3m-use-header-line)
  (defvar w3m-use-header-line-title)
  (defvar w3m-use-tab)
  ;; `w3m-tab-move-right' calls the inline function `w3m-buffer-set-number'
  ;; which uses it.
  (defvar w3m-use-title-buffer-name)
  (defvar w3m-work-buffer-list)
  (defvar w3m-work-buffer-name)
  (autoload 'w3m-copy-buffer "w3m")
  (autoload 'w3m-delete-buffer "w3m")
  (autoload 'w3m-image-type "w3m")
  (autoload 'w3m-retrieve "w3m")
  (autoload 'w3m-select-buffer-update "w3m"))

(eval-and-compile
  (unless (fboundp 'frame-current-scroll-bars)
    (defalias 'frame-current-scroll-bars 'ignore))
  (unless (fboundp 'window-fringes)
    (defalias 'window-fringes 'ignore))
  (unless (fboundp 'unencodable-char-position)
    (defalias 'unencodable-char-position 'ignore)))

;;; Coding system and charset.

(defsubst w3m-find-coding-system (obj)
  "Return OBJ if it is a coding-system."
  (if (coding-system-p obj) obj))

(defun w3m-detect-coding-region (start end &optional priority-list)
  "Detect coding system of the text in the region between START and END.
Return the first possible coding system.

PRIORITY-LIST is a list of coding systems ordered by priority."
  (let (category categories)
    (dolist (codesys priority-list)
      (setq category (coding-system-category codesys))
      (unless (or (null category) (assq category categories))
	(push (cons category codesys) categories)))
    (car (detect-coding-with-priority start end (nreverse categories)))))

(defun w3m-mule-unicode-p ()
  "Check the existence as charsets of mule-unicode."
  (and (charsetp 'mule-unicode-0100-24ff)
       (charsetp 'mule-unicode-2500-33ff)
       (charsetp 'mule-unicode-e000-ffff)))

(defalias 'w3m-make-ccl-coding-system
  (if (fboundp 'define-coding-system)
      (eval-when-compile
	(funcall (if (and (fboundp 'define-coding-system)
			  (featurep 'bytecomp))
		     'byte-compile
		   'identity)
		 '(lambda (coding-system mnemonic docstring decoder encoder) "\
Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.
CODING-SYSTEM, DECODER and ENCODER must be symbols.
This function is an interface to `define-coding-system'."
		    (define-coding-system coding-system docstring
		      :mnemonic mnemonic :coding-type 'ccl
		      :ccl-decoder decoder :ccl-encoder encoder))))
    (eval-when-compile
      (funcall (if (featurep 'bytecomp)
		   (lambda (form)
		     (let ((byte-compile-warnings
			    (if (eq (get 'make-coding-system 'byte-compile)
				    'byte-compile-obsolete)
				(delq 'obsolete
				      (copy-sequence
				       (cond ((consp byte-compile-warnings)
					      byte-compile-warnings)
					     (byte-compile-warnings
					      byte-compile-warning-types)
					     (t nil))))
			      byte-compile-warnings)))
		       (byte-compile form)))
		 'identity)
	       '(lambda (coding-system mnemonic docstring decoder encoder) "\
Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.
CODING-SYSTEM, DECODER and ENCODER must be symbols.
This function is an interface to `make-coding-system'."
		  (make-coding-system coding-system 4 mnemonic docstring
				      (cons decoder encoder)))))))

;; For Emacsen of which the `mule-version' is 5.x, redefine the ccl
;; programs that been defined in w3m-ccl.el.
(w3m-static-when (string-match "\\`5\\." mule-version)
  (let ((source
	 ;; CCL program to convert multibyte char to ucs with emacs-unicode.
	 `((if (r1 == ,(charset-id 'latin-iso8859-1))
	       ((r1 = (r0 + 128)))
	     (if (r1 == ,(charset-id 'mule-unicode-0100-24ff))
		 ((r1 = ((((r0 & #x3f80) >> 7) - 32) * 96))
		  (r0 &= #x7f)
		  (r1 += (r0 + 224)))		; 224 == -32 + #x0100
	       (if (r1 == ,(charset-id 'mule-unicode-2500-33ff))
		   ((r1 = ((((r0 & #x3f80) >> 7) - 32) * 96))
		    (r0 &= #x7f)
		    (r1 += (r0 + 9440)))	; 9440 == -32 + #x2500
		 (if (r1 == ,(charset-id 'mule-unicode-e000-ffff))
		     ((r1 = ((((r0 & #x3f80) >> 7) - 32) * 96))
		      (r0 &= #x7f)
		      (r1 += (r0 + 57312)))	; 57312 == -32 + #xe000
		   ,(if (fboundp 'ccl-compile-lookup-character)
			'((lookup-character utf-subst-table-for-encode r1 r0)
			  (if (r7 == 0)		; lookup failed
			      (r1 = #xfffd)))
		      '((r1 = #xfffd)))))))
	   (if (r1 == #xfffd)
	       (write-repeat ?~)		; unknown character.
	     (r0 = r1)))))
    (if (get 'utf-translation-table-for-encode 'translation-table-id)
	;; Emacs 21.3 and later.
	(progn
	  (eval
	   `(define-ccl-program w3m-euc-japan-encoder
	      '(4
		(loop
		 ,@w3m-ccl-write-euc-japan-character
		 (translate-character utf-translation-table-for-encode r1 r0)
		 ,@source
		 ,@w3m-ccl-generate-ncr))))
	  (eval
	   `(define-ccl-program w3m-iso-latin-1-encoder
	      '(4
		(loop
		 ,@w3m-ccl-write-iso-latin-1-character
		 (translate-character utf-translation-table-for-encode r1 r0)
		 ,@source
		 ,@w3m-ccl-generate-ncr)))))
      ;; Emacs 21.2 and earlier.
      (eval
       `(define-ccl-program w3m-euc-japan-encoder
	  '(4
	    (loop
	     ,@w3m-ccl-write-euc-japan-character
	     ,@source
	     ,@w3m-ccl-generate-ncr))))
      (eval
       `(define-ccl-program w3m-iso-latin-1-encoder
	  '(4
	    (loop
	     ,@w3m-ccl-write-iso-latin-1-character
	     ,@source
	     ,@w3m-ccl-generate-ncr)))))))

;; This might be redefined by w3m-ucs.el.
(defun w3m-ucs-to-char (codepoint)
  (or (decode-char 'ucs codepoint) ?~))

(defun w3m-add-local-hook (hook function &optional append)
  "Add to the buffer-local value of HOOK the function FUNCTION."
  (add-hook hook function append t))

(defun w3m-remove-local-hook (hook function)
  "Remove to the buffer-local value of HOOK the function FUNCTION."
  (remove-hook hook function t))

;; Function which returns non-nil when the current display device can
;; show images inline.
(defalias 'w3m-display-graphic-p 'display-images-p)

(defun w3m-display-inline-images-p ()
  "Returns non-nil when images can be displayed under the present
circumstances."
  (and w3m-display-inline-images (display-images-p)))

(eval-and-compile
  (defalias 'w3m-ems-create-image (if (fboundp 'create-animated-image)
				      'create-animated-image
				    'create-image)))

(defun w3m-create-image (url &optional no-cache referer size handler)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used.
If second optional argument REFERER is non-nil, it is used as Referer: field.
If third optional argument SIZE is non-nil, its car element is used as width
and its cdr element is used as height."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url no-cache referer size handler))
    (lexical-let ((set-size size)
		  (url url)
		  image size)
      (w3m-process-do-with-temp-buffer
	  (type (progn
		  (set-buffer-multibyte nil)
		  (w3m-retrieve url nil no-cache nil referer handler)))
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
	  (setq image (w3m-ems-create-image
		       (buffer-string) type t
		       :ascent 'center
		       :background w3m-image-default-background))
	  (if (and w3m-resize-images set-size)
	      (progn
		(set-buffer-multibyte t)
		(setq size (image-size image 'pixels))
		(if (and (null (car set-size)) (cdr set-size))
		    (setcar set-size
			    (/ (* (car size) (cdr set-size)) (cdr size))))
		(if (and (null (cdr set-size)) (car set-size))
		    (setcdr set-size
			    (/ (* (cdr size) (car set-size)) (car size))))
		(if (or (not (eq (car size)
				 (car set-size)))  ; width is different
			(not (eq (cdr size)
				 (cdr set-size)))) ; height is different
		    (lexical-let ((image image))
		      (w3m-process-do
			  (resized (w3m-resize-image
				    (plist-get (cdr image) :data)
				    (car set-size)(cdr set-size)
				    handler))
			(if resized (plist-put (cdr image) :data resized))
			image))
		  image))
	    image))))))

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
		  image)
      (w3m-process-do-with-temp-buffer
	  (type (progn
		  (set-buffer-multibyte nil)
		  (w3m-retrieve url nil nil nil referer handler)))
	(when (w3m-image-type-available-p (setq type (w3m-image-type type)))
	  (setq image (create-image (buffer-string) type t :ascent 'center))
	  (progn
	    (set-buffer-multibyte t)
	    (w3m-process-do
		(resized (w3m-resize-image-by-rate
			  (plist-get (cdr image) :data)
			  rate
			  handler))
	      (if resized (plist-put (cdr image) :data resized))
	      image)))))))

(defun w3m-insert-image (beg end image &rest args)
  "Display image on the current buffer.
Buffer string between BEG and END are replaced with IMAGE."
  (let ((faces (get-text-property beg 'face))
	(idx 0)
	orig len face)
    (add-text-properties beg end (list 'display image
				       'intangible image
				       'invisible nil))
    ;; Hide underlines behind inline images.
    ;; Gerd Moellmann <gerd@gnu.org>, the maintainer of Emacs 21, wrote in
    ;; the article <86heyi7vks.fsf@gerd.segv.de> in the list emacs-pretest-
    ;; bug@gnu.org on 18 May 2001 that to show an underline of a text even
    ;; if it has an image as a text property is the feature of Emacs 21.
    ;; However, that behavior is not welcome to the w3m buffers, so we do
    ;; to fix it with the following stuffs.
    (when faces
      (unless (listp faces)
	(setq faces (list faces)))
      (setq orig (copy-sequence faces)
	    len (length orig))
      (while (< idx len)
	(when (face-underline-p (setq face (nth idx orig)))
	  (setq faces (delq face faces)))
	(setq idx (1+ idx)))
      (when (< (length faces) len)
	(put-text-property beg end 'face faces)
	(put-text-property beg end 'w3m-faces-with-underline orig)))))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (remove-text-properties beg end '(display nil intangible nil))
  (let ((underline (get-text-property beg 'w3m-faces-with-underline)))
    (when underline
      (add-text-properties
       beg end (list 'face underline 'w3m-faces-with-underline nil)))))

(defun w3m-image-type-available-p (image-type)
  "Return non-nil if an image with IMAGE-TYPE can be displayed inline."
  (and (display-images-p)
       (image-type-available-p image-type)))

;;; Form buttons
(defface w3m-form-button
  '((((type x w32 mac ns) (class color))
     :background "lightgrey" :foreground "black"
     :box (:line-width 2 :style released-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify buttons in forms."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-form-button-face 'face-alias 'w3m-form-button)

(defface w3m-form-button-mouse
  '((((type x w32 mac ns) (class color))
     :background "DarkSeaGreen1" :foreground "black"
     :box (:line-width 2 :style released-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify focused buttons in forms."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-form-button-mouse-face 'face-alias 'w3m-form-button-mouse)

(defface w3m-form-button-pressed
  '((((type x w32 mac ns) (class color))
     :background "lightgrey" :foreground "black"
     :box (:line-width 2 :style pressed-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify pressed buttons in forms."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-form-button-pressed-face 'face-alias 'w3m-form-button-pressed)

(defvar w3m-form-button-keymap
  (let ((map (copy-keymap widget-keymap)))
    (substitute-key-definition 'widget-forward nil map)
    (substitute-key-definition 'widget-backward nil map)
    map))

(define-widget 'w3m-form-button 'push-button
  "Widget for w3m form button."
  :keymap w3m-form-button-keymap
  :action (function (lambda (widget &optional e)
		      (eval (widget-get widget :w3m-form-action)))))

(defun w3m-form-make-button (start end properties)
  "Make button on the region from START to END."
  (if w3m-form-use-fancy-faces
      (progn
	(unless (memq (face-attribute 'w3m-form-button :box)
		      '(nil unspecified))
	  (and (eq ?\[ (char-after start))
	       (eq ?\] (char-before end))
	       (save-excursion
		 (goto-char start)
		 (delete-char 1)
		 (insert " ")
		 (goto-char end)
		 (delete-char -1)
		 (insert " ")
		 (setq start (1+ start)
		       end (1- end)))))
	(let ((w (widget-convert-button
		  'w3m-form-button start end
		  :w3m-form-action (plist-get properties 'w3m-action))))
	  (overlay-put (widget-get w :button-overlay) 'evaporate t))
	(add-text-properties start end properties))
    (w3m-add-text-properties start end (append '(face w3m-form) properties))))

(defun w3m-setup-widget-faces ()
  (make-local-variable 'widget-button-face)
  (make-local-variable 'widget-mouse-face)
  (make-local-variable 'widget-button-pressed-face)
  (setq widget-button-face 'w3m-form-button)
  (setq widget-mouse-face 'w3m-form-button-mouse)
  (setq widget-button-pressed-face 'w3m-form-button-pressed))

;;; Menu bar
(defun w3m-menu-on-forefront (arg &optional curbuf)
  "Place emacs-w3m menus on the forfront of the menu bar if ARG is non-nil.
If CURBUF is given, this function works only in the current buffer,
otherwise works in all the emacs-w3m buffers."
  (if curbuf
      (if arg
	  (let ((items (mapcar 'car (cdr (lookup-key global-map [menu-bar])))))
	    (when items
	      (set (make-local-variable 'menu-bar-final-items) items)))
	(kill-local-variable 'menu-bar-final-items))
    (save-current-buffer
      (dolist (buffer (w3m-list-buffers t))
	(set-buffer buffer)
	(w3m-menu-on-forefront arg t)))))

;;; Toolbar
(defcustom w3m-use-toolbar
  (and (featurep 'tool-bar)
       ;; Emacs 22 and greater return t for `(featurep 'tool-bar)'
       ;; even if being launched with the -nw option.
       (display-images-p)
       (or (featurep 'gtk)
	   (image-type-available-p 'xpm)))
  "Non-nil activates toolbar of w3m."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-toolbar-icon-preferred-image-types
  (if (featurep 'gtk)
      '(png)
    '(xpm))
  "List of image types that you prefer to use for the tool bar icons.
By default, `png' is most preferred for Emacs built with GTK and `xpm'
is for others."
  :group 'w3m
  :type '(repeat (symbol :tag "Image type"))
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (when (and (not noninteractive) (boundp 'w3m-toolbar-buttons))
	     (w3m-update-toolbars)))))

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
	     (w3m-update-toolbars)))))

(defcustom w3m-toolbar-configurations
  `((tool-bar-button-margin . global)
    ,@(unless (featurep 'gtk)
	'((tool-bar-button-relief . global))))
  "Alist of the variables and the values controls the tool bar appearance.
The value `global' means to use the global value of the variable.

If you're annoyed with changing of the frame height of Emacs built for
GTK because of the difference of the sizes between the emacs-w3m tool
bar icons and the ones that Emacs natively uses, try customizing this
variable or both the value of this variable and the global value of
`tool-bar-button-margin'.  For examples:

;; The case where the emacs-w3m icons are smaller than the others.
\(setq w3m-toolbar-configurations '((tool-bar-button-margin . 5)))

;; The case where the emacs-w3m icons are bigger than the others.
\(setq w3m-toolbar-configurations '((tool-bar-button-margin . 0))
      tool-bar-button-margin 7)"
  :group 'w3m
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Variable")
		       (radio :format "%v"
			      (const :format "%v " global)
			      (sexp :tag "Local Value"))))
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (when (and (not noninteractive) (featurep 'w3m))
	     (w3m-toolbar-set-configurations)))))

(defun w3m-toolbar-define-keys (keymap defs)
  ;; Invalidate the default bindings.
  (let ((keys (cdr (key-binding [tool-bar] t)))
	item)
    (while (setq item (pop keys))
      (when (setq item (car-safe item))
	(define-key keymap (vector 'tool-bar item) 'undefined))))
  (let ((n (length defs))
	def)
    (while (>= n 0)
      (setq n (1- n)
	    def (nth n defs))
      (define-key keymap (vector 'tool-bar (aref def 1))
	(list 'menu-item (aref def 3) (aref def 1)
	      :enable (aref def 2)
	      :image (symbol-value (aref def 0)))))))

(defun w3m-find-image (name &optional directory)
  "Find image file for NAME and return cons of file name and type.
This function searches only in DIRECTORY, that defaults to the value of
`w3m-icon-directory', for an image file of which the base name is NAME.
Files of types that Emacs does not support are ignored."
  (unless directory
    (setq directory w3m-icon-directory))
  (when (and directory
	     (file-directory-p directory)
	     (display-images-p))
    (let* ((case-fold-search nil)
	   (files (directory-files directory t
				   (concat "\\`" (regexp-quote name) "\\.")))
	   (types (append w3m-toolbar-icon-preferred-image-types image-types))
	   file type rest)
      (while files
	(when (string-match "\\.\\([^.]+\\)\\'" (setq file (pop files)))
	  (setq type (intern (downcase (match-string 1 file))))
	  (setq type (or (cdr (assq type '((tif . tiff)
					   (jpg . jpeg)
					   (ps . postscript)
					   (pgm . pbm)
					   (ppm . pbm))))
			 type))
	  (push (cons file (memq type types)) rest)))
      (setq rest (car (sort rest (lambda (a b) (> (length a) (length b))))))
      (when (cdr rest)
	(cons (car rest) (cadr rest))))))

(defun w3m-toolbar-make-buttons (buttons &optional force)
  (let ((xpm-props '(:color-symbols (("backgroundToolBarColor" . "None"))))
	button icon down disabled up)
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
	    (progn
	      (setq up (apply 'create-image (car up) (cdr up) nil
			      :ascent 'center
			      (when (eq (cdr up) 'xpm)
				xpm-props)))
	      (if (or w3m-toolbar-use-single-image-per-icon
		      (not (or down disabled)))
		  (set icon up)
		(when down
		  (setq down (apply 'create-image (car down) (cdr down) nil
				    :ascent 'center
				    (when (eq (cdr up) 'xpm)
				      xpm-props))))
		(when disabled
		  (setq disabled (apply 'create-image
					(car disabled) (cdr disabled) nil
					:ascent 'center
					(when (eq (cdr disabled) 'xpm)
					  xpm-props))))
		(set icon (vector down up disabled disabled))))
	  (error "Icon file %s-up.* not found" button))))))

(defun w3m-toolbar-set-configurations (&optional curbuf)
  "Set values of variables according to `w3m-toolbar-configurations'.
If CURBUF is given, this function works only in the current buffer,
otherwise works in all the emacs-w3m buffers."
  (if curbuf
      (dolist (config w3m-toolbar-configurations)
	(if (eq (cdr config) 'global)
	    (kill-local-variable (car config))
	  (set (make-local-variable (car config)) (cdr config))))
    (let ((cur (selected-frame))
	  buffer buffers)
      (walk-windows
       (lambda (window)
	 (setq buffer (window-buffer window))
	 (unless (memq buffer buffers)
	   (set-buffer buffer)
	   (when (eq major-mode 'w3m-mode)
	     (push buffer buffers)
	     (select-frame (window-frame window))
	     ;;(set-buffer buffer)
	     (w3m-toolbar-set-configurations t))))
       'ignore 'visible)
      (select-frame-set-input-focus cur)
      (save-current-buffer
	(dolist (buffer (w3m-list-buffers t))
	  (unless (memq buffer buffers)
	    (set-buffer buffer)
	    (w3m-toolbar-set-configurations t))))
      (select-frame-set-input-focus cur))))

(defun w3m-setup-toolbar ()
  (when (and w3m-use-toolbar
	     (w3m-find-image "antenna-up"))
    (w3m-toolbar-make-buttons w3m-toolbar-buttons)
    (w3m-toolbar-set-configurations t)
    (w3m-toolbar-define-keys w3m-mode-map w3m-toolbar)))

(defalias 'w3m-update-toolbar 'ignore)

(defun w3m-update-toolbars ()
  (when (and w3m-use-toolbar
	     (w3m-find-image "antenna-up"))
    (w3m-toolbar-make-buttons w3m-toolbar-buttons t)
    (w3m-toolbar-set-configurations)
    (w3m-toolbar-define-keys w3m-mode-map w3m-toolbar)))

;;; Header line & Tabs
(defcustom w3m-tab-width 16
  "w3m tab width."
  :group 'w3m
  :set (lambda (symbol value)
	 (custom-set-default symbol
			     (if (and (numberp value) (> value 0))
				 value
			       16)))
  :type '(integer :size 0))

(defface w3m-tab-unselected
  '((((type x w32 mac ns) (class color))
     :background "Gray70" :foreground "Gray20"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "blue" :foreground "black")))
  "*Face to fontify unselected tabs."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-tab-unselected-face 'face-alias 'w3m-tab-unselected)

(defface w3m-tab-unselected-retrieving
  '((((type x w32 mac ns) (class color))
     :background "Gray70" :foreground "OrangeRed"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "blue" :foreground "OrangeRed")))
  "*Face to fontify unselected tabs which are retrieving their pages."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-tab-unselected-retrieving-face
     'face-alias 'w3m-tab-unselected-retrieving)

(defface w3m-tab-unselected-unseen
  '((((type x w32 mac ns) (class color))
     :background "Gray70" :foreground "Gray20"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "blue" :foreground "gray60")))
  "*Face to fontify unselected and unseen tabs."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-tab-unselected-unseen-face 'face-alias 'w3m-tab-unselected-unseen)

(defface w3m-tab-selected
  '((((type x w32 mac ns) (class color))
     :background "Gray90" :foreground "black"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "cyan" :foreground "black"))
    (t (:underline t)))
  "*Face to fontify selected tab."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-tab-selected-face 'face-alias 'w3m-tab-selected)

(defface w3m-tab-selected-retrieving
  '((((type x w32 mac ns) (class color))
     :background "Gray90" :foreground "red"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "cyan" :foreground "red"))
    (t (:underline t)))
  "*Face to fontify selected tab which is retrieving its page."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-tab-selected-retrieving-face
     'face-alias 'w3m-tab-selected-retrieving)

(defface w3m-tab-background
  '((((type x w32 mac ns) (class color))
     :background "LightSteelBlue" :foreground "black")
    (((class color))
     (:background "white" :foreground "black")))
  "*Face to fontify background of tab line."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-tab-background-face 'face-alias 'w3m-tab-background)

(defface w3m-tab-selected-background
  '((((type x w32 mac ns) (class color))
     :background "LightSteelBlue" :foreground "black")
    (((class color))
     (:background "white" :foreground "black")))
  "*Face to fontify selected background tab."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-tab-selected-background-face
     'face-alias 'w3m-tab-selected-background)

(defface w3m-tab-mouse
  '((((type x w32 mac ns) (class color))
     :background "Gray75" :foreground "white"
     :box (:line-width -1 :style released-button)))
  "*Face used to highlight tabs under the mouse."
  :group 'w3m-face)
;; backward-compatibility alias
(put 'w3m-tab-mouse-face 'face-alias 'w3m-tab-mouse)

(defvar w3m-modeline-spinner-map nil
  "Keymap used on the spinner in the mode-line.")

(defvar w3m-spinner-map-help-echo "mouse-2 kills the current process"
  "String used for the :help-echo property on the spinner.")

(defun w3m-setup-header-line ()
  (setq header-line-format
	(cond (w3m-use-tab
	       '(:eval (w3m-tab-line)))
	      (w3m-use-header-line
	       (list
		(if w3m-use-header-line-title
		    (list
		     (propertize
		      "Title: "
		      'face (list 'w3m-header-line-location-title))
		     `(:eval
		       (propertize
			(replace-regexp-in-string "%" "%%" (w3m-current-title))
			'face (list 'w3m-header-line-location-content)
			'mouse-face '(highlight :foreground
						,(face-foreground 'default))
			'local-map (let ((map (make-sparse-keymap)))
				     (define-key map [header-line mouse-2]
				       'w3m-goto-url)
				     map)
			'help-echo "mouse-2 prompts to input URL"))
		     ", ")
		  "")
		 (propertize
		  "Location: "
		  'face (list 'w3m-header-line-location-title))
		 `(:eval
		  (propertize
		   (if (stringp w3m-current-url)
		       (replace-regexp-in-string "%" "%%" w3m-current-url)
		     "")
		   'face (list 'w3m-header-line-location-content)
		   'mouse-face '(highlight :foreground
					   ,(face-foreground 'default))
		   'local-map (let ((map (make-sparse-keymap)))
				(define-key map [header-line mouse-2]
				  'w3m-goto-url)
				map)
		   'help-echo "mouse-2 prompts to input URL")))))))

(eval-when-compile
  ;; Shut up the byte-compiler for old Emacsen.
  (unless (fboundp 'force-window-update)
    (defalias 'force-window-update 'ignore)))

(eval-and-compile
  (defalias 'w3m-force-window-update
    (if (and (fboundp 'force-window-update)
	     (not (eq (symbol-function 'force-window-update) 'ignore)))
	(lambda (&optional window) "\
Force redisplay of WINDOW which defaults to the selected window."
	  (force-window-update (or window (selected-window))))
      (lambda (&optional ignore) "\
Wobble the selected window to force redisplay of the header-line."
	(save-window-excursion
	  (split-window-vertically))))))

(defun w3m-tab-drag-mouse-function (event buffer)
  (let ((window (posn-window (event-end event)))
	mpos)
    (when (framep window) ; dropped at outside of the frame.
      (setq window nil
	    mpos (mouse-position))
      (and (framep (car mpos))
	   (car (cdr mpos))
	   (cdr (cdr mpos))
	   (setq window (window-at (car (cdr mpos))
				   (cdr (cdr mpos))
				   (car mpos))))
      (unless window
	(when (one-window-p 'nomini)
	  (split-window))
	(setq window (next-window))))
    (unless (eq (window-buffer window) buffer)
      (select-window window)
      (switch-to-buffer buffer)
      (w3m-force-window-update window))))

(defun w3m-tab-click-mouse-function (event buffer)
  (let ((window (posn-window (event-start event))))
    (select-window window)
    (switch-to-buffer buffer)
    (w3m-force-window-update window)))

(defun w3m-tab-double-click-mouse1-function (event buffer)
  (let ((window (posn-window (event-start event))))
    (when (eq major-mode 'w3m-mode)
      (if w3m-new-session-in-background
	  (save-window-excursion
	    (w3m-copy-buffer))
	(w3m-copy-buffer)))
    (w3m-force-window-update window)))

(defun w3m-tab-double-click-mouse2-function (event buffer)
  (let ((window (posn-window (event-start event))))
    (when (eq major-mode 'w3m-mode)
      (w3m-delete-buffer))
    (w3m-force-window-update window)))

(defvar w3m-tab-line-format nil
  "Internal variable used to keep contents to be shown in the header-line.
This is a buffer-local variable.")
(make-variable-buffer-local 'w3m-tab-line-format)

(defvar w3m-tab-timer nil
  "Internal variable used to say time has not gone by after the tab-line
was updated last time.  It is used to control the `w3m-tab-line'
function running too frequently, set by the function itself and
cleared by a timer.")
(make-variable-buffer-local 'w3m-tab-timer)

(defcustom w3m-tab-track-mouse t
  "Say whether to make the mouse track the selected tab.
It controls the behavior of the commands `w3m-tab-previous-buffer',
`w3m-tab-next-buffer', `w3m-tab-move-right', and `w3m-tab-move-left'
invoked by the mouse.

You may want to set this to nil if you use a proportional font for the
tab faces.  See also `w3m-tab-mouse-position-adjuster'."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-tab-mouse-position-adjuster '(0.5 . -4)
  "Values used to adjust the mouse position on tabs.
It is used when the command `w3m-tab-previous-buffer',
`w3m-tab-next-buffer', `w3m-tab-move-right', or `w3m-tab-move-left' is
invoked by the mouse.  The value consists of the cons of a floating
point number M and an integer N that are applied to calculating of the
mouse position, which is given in pixel units, as follows:

  (TAB_WIDTH + M) * ORDER + N

Where TAB_WIDTH is the pixel width of a tab and ORDER is the order
number in tabs.  The result is rounded towards zero.

Note that the calculation will always fail if you use a proportional
font for the tab faces.  See also `w3m-tab-track-mouse'."
  :group 'w3m
  :type '(cons (number :tag "M") (integer :tag "N")))

(defun w3m-tab-mouse-track-selected-tab (event order
					       &optional buffers decelerate)
  "Make the mouse track the selected tab.
EVENT is a command event.  ORDER is the order number in tabs.
The optional BUFFERS is a list of emacs-w3m buffers, DECELERATE if it
is non-nil means not to respond to too fast operation of mouse wheel."
  (when (and w3m-use-tab window-system w3m-tab-track-mouse
	     (consp event) (symbolp (car event)))
    (let ((e (get (car event) 'event-symbol-elements))
	  (len (* (car w3m-tab-mouse-position-adjuster) order))
	  (fcw (frame-char-width))
	  posn tab start end disp next)
      (when (and (consp e) (symbolp (car e))
		 (or (memq (car e) '(mouse-4 mouse-5 wheel-up wheel-down))
		     (string-match "\\`mouse-" (symbol-name (car e)))))
	;; Ignore values gotten by the mouse pointing to other frames.
	;; It might happen if the frame in question gets out of focus
	;; by a certain desktop tool such as unclutter.
	(let ((frame (selected-frame)))
	  (while (not (cadr (setq posn (mouse-pixel-position))))
	    (select-frame-set-input-focus frame)))
	;; Update the header line.
	(setq w3m-tab-timer nil)
	(sit-for 0)
	(setq tab w3m-tab-line-format)
	(with-temp-buffer
	  (insert tab)
	  (setq start (point-min)
		end (point))
	  (while (and (> order  0)
		      (setq start (text-property-any start end
						     'tab-separator t))
		      (setq start (text-property-not-all start end
							 'tab-separator t)))
	    (setq order (1- order)))
	  (setq end (1- start)
		start (point-min)
		next start
		disp (get-text-property start 'display))
	  (while (and (< next end)
		      (setq next (next-single-property-change
				  start 'display nil end)))
	    (setq len (+ (cond
			  ((eq (car disp) 'image)
			   (or (car w3m-favicon-size) (frame-char-height)))
			  ((eq (car disp) 'space)
			   (* (or (plist-get (cdr disp) :width) 1) fcw))
			  (t
			   (* (string-width (buffer-substring start next))
			      fcw)))
			 len)
		  start next
		  disp (get-text-property start 'display))))
	(set-mouse-pixel-position
	 (car posn)
	 (truncate (+ len (cdr w3m-tab-mouse-position-adjuster)))
	 (cddr posn))
	(when decelerate
	  (sleep-for 0.1))
	(discard-input)))))

(defun w3m-tab-next-buffer (&optional n event)
  "Turn N pages of emacs-w3m buffers ahead."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     last-command-event))
  (unless n (setq n 1))
  (when (and (/= n 0) (eq major-mode 'w3m-mode))
    (let ((buffers (w3m-list-buffers)))
      (switch-to-buffer
       (nth (mod (+ n (w3m-buffer-number (current-buffer)) -1)
		 (length buffers))
	    buffers))
      (run-hooks 'w3m-select-buffer-hook)
      (w3m-select-buffer-update)
      (w3m-tab-mouse-track-selected-tab
       event (w3m-buffer-number (current-buffer)) buffers))))

(defun w3m-tab-previous-buffer (&optional n event)
  "Turn N pages of emacs-w3m buffers behind."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     last-command-event))
  (w3m-tab-next-buffer (- n) event))

(defun w3m-tab-move-right (&optional n event)
  "Move this tab N times to the right (to the left if N is negative)."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     last-command-event))
  (unless n (setq n 1))
  (when (and (/= n 0) (eq major-mode 'w3m-mode))
    (let* ((buffers (if (> n 0)
			(w3m-list-buffers)
		      (setq n (- n))
		      (nreverse (w3m-list-buffers))))
	   (dest (or (nth n (memq (current-buffer) buffers))
		     (car (last buffers))))
	   (next (w3m-buffer-number dest))
	   (cur (w3m-buffer-number (current-buffer)))
	   e posn start)
      (rename-buffer "*w3m*<0>")
      (w3m-buffer-set-number dest cur)
      (w3m-buffer-set-number (current-buffer) next)
      (w3m-select-buffer-update)
      (w3m-tab-mouse-track-selected-tab event next buffers t))))

(defun w3m-tab-move-left (&optional n event)
  "Move this tab N times to the left (to the right if N is negative)."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     last-command-event))
  (w3m-tab-move-right (- n) event))

(defvar w3m-tab-map nil)
(make-variable-buffer-local 'w3m-tab-map)

(defvar w3m-tab-spinner-map nil)
(make-variable-buffer-local 'w3m-tab-spinner-map)

(defun w3m-tab-make-keymap ()
  (unless w3m-tab-map
    (setq w3m-tab-map (make-sparse-keymap))
    (let* ((cur (current-buffer))
	   (f1 (lambda (fn) `(lambda (e) (interactive "e") (,fn e ,cur))))
	   (f2 (lambda (fn) `(lambda (e)
			       (interactive "e")
			       (select-window (posn-window (event-start e)))
			       (switch-to-buffer ,cur)
			       (setq this-command ',fn)
			       (,fn 1 e))))
	   (drag-action (funcall f1 'w3m-tab-drag-mouse-function))
	   (single-action (funcall f1 'w3m-tab-click-mouse-function))
	   (double-action1 (funcall f1 'w3m-tab-double-click-mouse1-function))
	   (double-action2 (funcall f1 'w3m-tab-double-click-mouse2-function))
	   (menu-action (funcall f1 'w3m-tab-button-menu))
	   (menu-action2 (funcall f1 'w3m-tab-button-menu2))
	   (next-buffer-action (funcall f2 'w3m-tab-next-buffer))
	   (previous-buffer-action (funcall f2 'w3m-tab-previous-buffer))
	   (move-left-action (funcall f2 'w3m-tab-move-left))
	   (move-right-action (funcall f2 'w3m-tab-move-right)))
      (define-key w3m-tab-map [header-line down-mouse-1] 'ignore)
      (define-key w3m-tab-map [header-line down-mouse-2] 'ignore)
      (define-key w3m-tab-map [header-line mouse-1] single-action)
      (define-key w3m-tab-map [header-line mouse-2] single-action)
      (define-key w3m-tab-map [header-line drag-mouse-1] drag-action)
      (define-key w3m-tab-map [header-line drag-mouse-2] drag-action)
      (define-key w3m-tab-map [header-line double-mouse-1] double-action1)
      (define-key w3m-tab-map [header-line double-mouse-2] double-action2)
      (define-key w3m-tab-map [header-line mouse-3] menu-action)
      (define-key w3m-tab-map [header-line wheel-up] previous-buffer-action)
      (define-key w3m-tab-map [header-line wheel-down] next-buffer-action)
      (define-key w3m-tab-map [header-line mouse-4] previous-buffer-action)
      (define-key w3m-tab-map [header-line mouse-5] next-buffer-action)
      (define-key w3m-tab-map [header-line C-wheel-up] move-left-action)
      (define-key w3m-tab-map [header-line C-wheel-down] move-right-action)
      (define-key w3m-tab-map [header-line C-mouse-4] move-left-action)
      (define-key w3m-tab-map [header-line C-mouse-5] move-right-action)
      (define-key w3m-mode-map [header-line double-mouse-1]
	'w3m-goto-new-session-url)
      (define-key w3m-mode-map [header-line mouse-3] menu-action2)
      ;; The following bindings in `w3m-mode-map', not `w3m-tab-map',
      ;; are required for some platforms, in which mouse wheel events
      ;; sometimes pass by `w3m-tab-map' for the unresolved reason and
      ;; see `w3m-mode-map', or else the `undefined' errors will arise.
      (define-key w3m-mode-map [header-line mouse-4] 'w3m-tab-previous-buffer)
      (define-key w3m-mode-map [header-line mouse-5] 'w3m-tab-next-buffer)
      (define-key w3m-mode-map [header-line wheel-up] 'w3m-tab-previous-buffer)
      (define-key w3m-mode-map [header-line wheel-down] 'w3m-tab-next-buffer)
      (define-key w3m-mode-map [header-line C-wheel-up] 'w3m-tab-move-left)
      (define-key w3m-mode-map [header-line C-wheel-down] 'w3m-tab-move-right)
      (define-key w3m-mode-map [header-line C-mouse-4] 'w3m-tab-move-left)
      (define-key w3m-mode-map [header-line C-mouse-5] 'w3m-tab-move-right))
    (unless w3m-tab-spinner-map
      (setq w3m-tab-spinner-map (make-sparse-keymap))
      (define-key w3m-tab-spinner-map [header-line mouse-2]
	`(lambda (e)
	   (interactive "e")
	   (save-current-buffer
	     ;; Why the `(w3m-process-stop BUFFER)' doesn't work?
	     (set-buffer ,(current-buffer))
	     (call-interactively 'w3m-process-stop)))))))

(defvar w3m-tab-half-space
  (propertize " " 'display '(space :width 0.5))
  "The space of half width.")

(defvar w3m-tab-separator-map nil)

(unless w3m-tab-separator-map
  (let ((map (make-sparse-keymap)))
    (setq w3m-tab-separator-map map)
    (define-key map [header-line wheel-up] 'w3m-tab-previous-buffer)
    (define-key map [header-line wheel-down] 'w3m-tab-next-buffer)
    (define-key map [header-line mouse-4] 'w3m-tab-previous-buffer)
    (define-key map [header-line mouse-5] 'w3m-tab-next-buffer)
    (define-key map [header-line C-wheel-up] 'w3m-tab-move-left)
    (define-key map [header-line C-wheel-down] 'w3m-tab-move-right)
    (define-key map [header-line C-mouse-4] 'w3m-tab-move-left)
    (define-key map [header-line C-mouse-5] 'w3m-tab-move-right)))

(defvar w3m-tab-separator
  (propertize " "
	      'face (list 'w3m-tab-background)
	      'mouse-face 'w3m-tab-selected-background
	      'display '(space :width 0.5)
	      'tab-separator t
	      'local-map w3m-tab-separator-map)
  "String used to separate tabs.")

(defun w3m-tab-line ()
  (or (and w3m-tab-timer w3m-tab-line-format)
      (let* ((current (current-buffer))
	     (buffers (w3m-list-buffers))
	     (breadth 1)
	     (number 0)
	     (fringes (window-fringes))
	     (width (+ (window-width)
		       (/ (float (+ (or (car fringes) 0)
				    (or (nth 1 fringes) 0)))
			  (frame-char-width))
		       ;; Assume that the vertical scroll-bar has
		       ;; the width of two space characters.
		       (if (car (frame-current-scroll-bars)) 2 0)))
	     (nbuf (length buffers))
	     (graphic (and window-system
			   w3m-show-graphic-icons-in-header-line))
	     (margin (if window-system
			 (+ (if graphic 3.0 0.5)
			    ;; Right and left shadows.
			    (/ 2.0 (frame-char-width)))
		       1))
	     (spinner (when w3m-process-queue
			(w3m-make-spinner-image)))
	     buffer title data datum process unseen favicon keymap face icon
	     line)
	(setq w3m-tab-timer t)
	(run-at-time 0.1 nil
		     (lambda (buffer)
		       (when (buffer-live-p buffer)
			 (with-current-buffer buffer
			   (setq w3m-tab-timer nil)
			   (when (and (eq (selected-window)
					  (get-buffer-window buffer))
				      w3m-process-queue)
			     (inline (w3m-force-window-update))))))
		     current)
	(save-current-buffer
	  (while buffers
	    (set-buffer (setq buffer (pop buffers)))
	    (setq number (1+ number))
	    (setq title (if w3m-add-tab-number
			    (format "%d.%s" number (w3m-current-title))
			  (w3m-current-title)))
	    (setq breadth
		  (max breadth
		       ;; There may be a wide character in the beginning of
		       ;; the title.
		       (if (> (length title) 0)
			   (char-width (aref title 0))
			 0)))
	    (push (list (eq current buffer)
			w3m-current-process
			(w3m-unseen-buffer-p buffer)
			title
			(when w3m-use-favicon w3m-favicon-image)
			w3m-tab-map)
		  data)))
	(setq width (if (> (* nbuf (+ margin w3m-tab-width)) width)
			(max (truncate (- (/ width nbuf) margin)) breadth)
		      w3m-tab-width))
	(while data
	  (setq datum (pop data)
		current (car datum)
		process (nth 1 datum)
		unseen (nth 2 datum)
		title (nth 3 datum)
		favicon (nth 4 datum)
		keymap (nth 5 datum)
		face (list
		      (if process
			  (if current
			      'w3m-tab-selected-retrieving
			    'w3m-tab-unselected-retrieving)
			(if current
			    'w3m-tab-selected
			  (if unseen
			      'w3m-tab-unselected-unseen
			    'w3m-tab-unselected))))
		icon (when graphic
		       (cond
			(process
			 (when spinner
			   (propertize
			    " "
			    'display spinner
			    'face face
			    'local-map w3m-tab-spinner-map
			    'help-echo w3m-spinner-map-help-echo)))
			(favicon
			 (propertize
			  " "
			  'display favicon
			  'face face
			  'local-map keymap
			  'help-echo title))))
		breadth (cond (icon width)
			      (graphic (+ 2 width))
			      (t width)))
	  (push
	   (list
	    icon
	    (propertize
	     (concat
	      (when graphic w3m-tab-half-space)
	      (replace-regexp-in-string
	       "%" "%%"
	       (if (and (> (string-width title) breadth)
			(> breadth 6))
		   (truncate-string-to-width
		    (concat (truncate-string-to-width title (- breadth 3))
			    "...")
		    breadth nil ?.)
		 (truncate-string-to-width title breadth nil ?\ ))))
	     'face face
	     'mouse-face 'w3m-tab-mouse
	     'local-map keymap
	     'help-echo title)
	    w3m-tab-separator)
	   line))
	(setq w3m-tab-line-format
	      (concat (apply 'concat (apply 'nconc line))
		      (propertize (make-string (window-width) ?\ )
				  'face (list 'w3m-tab-background)
				  'mouse-face 'w3m-tab-selected-background
				  'local-map w3m-tab-separator-map))))))

(add-hook 'w3m-mode-setup-functions 'w3m-tab-make-keymap)
(add-hook 'w3m-mode-setup-functions 'w3m-setup-header-line)
(add-hook 'w3m-mode-setup-functions 'w3m-setup-widget-faces)
(add-hook 'w3m-select-buffer-hook 'w3m-force-window-update)

;; Graphic icons.
(defcustom w3m-space-before-modeline-icon ""
  "String of space character(s) to be put in front of the mode-line icon.
It may be better to use one or more spaces if you are using oblique or
italic font in the modeline."
  :group 'w3m
  :type 'string)

(defvar w3m-spinner-image-file nil
  "Image file used to show a spinner in the header-line.")

(defvar w3m-spinner-image-frames 3
  "Number of frames which the spinner image contains.")

(defvar w3m-spinner-image-index 0
  "Counter used to rotate spinner images.  This is a buffer-local variable.")
(make-variable-buffer-local 'w3m-spinner-image-index)

;; Images to be displayed in the modeline.
(defvar w3m-modeline-process-status-on-icon nil)
(defvar w3m-modeline-image-status-on-icon nil)
(defvar w3m-modeline-status-off-icon nil)
(defvar w3m-modeline-ssl-image-status-on-icon nil)
(defvar w3m-modeline-ssl-status-off-icon nil)

(defun w3m-initialize-graphic-icons (&optional force)
  "Make icon images which will be displayed in the mode-line."
  (interactive "P")
  (when (or (image-type-available-p 'xpm)
	    (image-type-available-p 'png))
    ;; Prefer xpm icons rather than png icons since Emacs doesn't display
    ;; background colors of icon images other than xpm images transparently
    ;; in the mode line.
    (let* ((w3m-toolbar-icon-preferred-image-types
	    (if (image-type-available-p 'xpm)
		'(xpm)
	      '(png)))
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
	   def icon file type status keymap)
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
		(unless keymap
		  (setq keymap
			(make-mode-line-mouse-map 'mouse-2
						  'w3m-reload-this-page)))
		(set icon (propertize
			   "  "
			   'display (create-image file type nil
						  :ascent 'center)
			   'local-map keymap
			   'mouse-face 'mode-line-highlight
			   'help-echo "mouse-2 reloads this page"))
		(put icon 'risky-local-variable t)
		(put status 'risky-local-variable t))
	      (when (stringp (symbol-value status))
		;; Save the original status strings as properties.
		(put status 'string (symbol-value status)))
	      (set status (list "" 'w3m-space-before-modeline-icon icon)))
	  ;; Don't use graphic icons.
	  (when (get status 'string)
	    (set status (get status 'string)))))))
    (let (file)
      ;; Spinner
      (when (and (or force (not w3m-spinner-image-file))
		 (image-type-available-p 'gif)
		 w3m-icon-directory
		 (file-directory-p w3m-icon-directory)
		 (file-exists-p
		  (setq file (expand-file-name "spinner.gif"
					       w3m-icon-directory))))
	(setq w3m-spinner-image-file file)
	(define-key (setq w3m-modeline-spinner-map (make-sparse-keymap))
	  [mode-line mouse-2]
	  'w3m-process-stop)
	(put 'w3m-modeline-process-status-on 'risky-local-variable t)
	(put 'w3m-modeline-process-status-on-icon 'risky-local-variable t))
      (if (and window-system
	       w3m-show-graphic-icons-in-mode-line
	       w3m-spinner-image-file)
	  (progn
	    (when (stringp w3m-modeline-process-status-on)
	      ;; Save the original status strings as properties.
	      (put 'w3m-modeline-process-status-on 'string
		   w3m-modeline-process-status-on))
	    (setq w3m-modeline-process-status-on
		  '(""
		    w3m-space-before-modeline-icon
		    w3m-modeline-process-status-on-icon)))
	(when (get 'w3m-modeline-process-status-on 'string)
	  (setq w3m-modeline-process-status-on
		(get 'w3m-modeline-process-status-on 'string))))))

(defun w3m-make-spinner-image ()
  "Make an image used to show a spinner.
It should be called periodically in order to spin the spinner."
  (when w3m-spinner-image-file
    (unless (< (incf w3m-spinner-image-index) w3m-spinner-image-frames)
      (setq w3m-spinner-image-index 0))
    (let ((image (create-image w3m-spinner-image-file 'gif nil
			       :ascent 'center :mask 'heuristic
			       :index w3m-spinner-image-index)))
      (setq w3m-modeline-process-status-on-icon
	    (propertize "  "
			'display image
			'local-map w3m-modeline-spinner-map
			'help-echo w3m-spinner-map-help-echo))
      image)))

(defun w3m-decode-coding-string-with-priority (str coding)
  "Decode the string STR which is encoded in CODING.
If CODING is a list, look for the coding system using it as a priority
list."
  (setq str (string-make-unibyte str))
  (when (listp coding)
    (setq coding
	  (with-temp-buffer
	    (set-buffer-multibyte nil)
	    (insert str)
	    (w3m-detect-coding-region (point-min) (point-max) coding))))
  (decode-coding-string str
			(or coding
			    w3m-default-coding-system
			    w3m-coding-system
			    'iso-2022-7bit)))

(defun w3m-form-coding-system-accept-region-p (&optional from to coding-system)
  "Check whether `coding-system' can encode specified region."
  (let ((from (or from (point-min)))
	(to (or to   (point-max)))
	(coding-system (or coding-system
			   w3m-form-input-textarea-coding-system)))
    (if (fboundp 'unencodable-char-position)
	(let ((pos (unencodable-char-position from to coding-system)))
	  (or (not pos)
	      (y-or-n-p (format "\"%c\" would not be accepted. Continue? "
				(char-after pos)))))
      (let ((select-safe-coding-system-accept-default-p nil))
	(or (eq (select-safe-coding-system from to coding-system)
		coding-system)
	    (y-or-n-p
	     "This text may cause coding-system problem. Continue? "))))))

(provide 'w3m-ems)

;;; w3m-ems.el ends here

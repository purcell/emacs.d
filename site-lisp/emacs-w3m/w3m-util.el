;;; w3m-util.el --- Utility macros and functions for emacs-w3m

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
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

;; This module is a part of emacs-w3m which provides utility macros
;; and inline functions.  Visit <URL:http://emacs-w3m.namazu.org/> for
;; more details of emacs-w3m.

;;; Code:

(eval-when-compile
  (require 'cl))

;; Variables and functions which are used in the following inline
;; functions.  They should be defined in the other module at run-time.
(eval-when-compile
  (defvar w3m-current-process)
  (defvar w3m-current-refresh)
  (defvar w3m-current-title)
  (defvar w3m-current-url)
  (defvar w3m-fb-list-buffers-frame)
  (defvar w3m-fb-mode)
  (defvar w3m-mode-hook)
  (defvar w3m-pop-up-frames)
  (defvar w3m-pop-up-windows)
  (defvar w3m-popup-frame-parameters)
  (defvar w3m-refresh-timer)
  (defvar w3m-select-buffer-name)
  (defvar w3m-use-refresh)
  (defvar w3m-use-tab)
  (defvar w3m-work-buffer-list)
  (defvar w3m-use-japanese-menu)
  (defvar w3m-mode-map)
  (defvar w3m-use-title-buffer-name)
  (defvar w3m-buffer-unseen)
  (defvar w3m-puny-utf-16be)
  (unless (fboundp 'select-frame-set-input-focus)
    (defalias 'select-frame-set-input-focus 'ignore)))

(eval-and-compile
  (when (featurep 'xemacs)
    (require 'poe)
    (require 'poem)))

;;; Things should be defined in advance:
(eval-when-compile
  (autoload 'w3m-fb-frame-parameter "w3m-fb")
  (autoload 'w3m-history-restore-position "w3m-hist" nil t))

;;; Control structures:

(defmacro w3m-static-if (cond then &rest else)
  "Like `if', except that it evaluates COND at compile-time."
  (if (eval cond) then `(progn  ,@else)))
(put 'w3m-static-if 'lisp-indent-function 2)

(put 'w3m-static-when 'lisp-indent-function 1)
(defmacro w3m-static-when (cond &rest body)
  "Like `when', but evaluate COND at compile time."
  (if (eval cond)
      `(progn ,@body)))

(put 'w3m-static-unless 'lisp-indent-function 1)
(defmacro w3m-static-unless (cond &rest body)
  "Like `unless', but evaluate COND at compile time."
  (if (eval cond)
      nil
    `(progn ,@body)))

(defmacro w3m-static-cond (&rest clauses)
  "Like `cond', except that it evaluates CONDITION part of each clause at
compile-time."
  (while (and clauses
	      (not (eval (car (car clauses)))))
    (setq clauses (cdr clauses)))
  (if clauses
      (cons 'progn (cdr (car clauses)))))

(put 'w3m-condition-case lisp-indent-function 2)
(defmacro w3m-condition-case (var bodyform &rest handlers)
  "Like `condition-case', except that signal an error if `debug-on-error'
or `debug-on-quit' is non-nil."
  `(if (or debug-on-error debug-on-quit)
       ,bodyform
     (condition-case ,var
	 ,bodyform
       ,@handlers)))


;;; Text props:

(defmacro w3m-add-text-properties (start end props &optional object)
  "Like `add-text-properties' but always add non-sticky properties."
  (let ((non-stickies
	 (if (featurep 'xemacs)
	     ;; Default to start-closed and end-open in XEmacsen.
	     '(list 'start-open t)
	   ;; Default to front-nonsticky and rear-sticky in Emacsen.
	   '(list 'rear-nonsticky t))))
    `(add-text-properties ,start ,end
			  (append ,non-stickies ,props)
			  ,object)))

(defun w3m-add-face-property (start end name &optional object)
  "Add face NAME to the face text property of the text from START to END.
The value of the existing text property should be a list.
If the optional fourth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices
into it."
  (let ((pos start)
	next prop)
    (while (< pos end)
      (setq prop (get-text-property pos 'face object)
	    next (next-single-property-change pos 'face object end))
      (w3m-add-text-properties pos next (list 'face (cons name prop)) object)
      (setq pos next))))

(defun w3m-remove-face-property (start end name &optional object)
  "Remove face NAME from the face text property of text from START to END.
The value of the existing text property should be a list.
If the optional fourth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices
into it."
  (let ((pos start)
	next prop new-prop elem)
    (while (< pos end)
      (setq prop (get-text-property pos 'face object))
      (setq next (next-single-property-change pos 'face object end))
      (setq new-prop nil)
      (while prop
	(setq elem (pop prop))
	(unless (eq elem name)
	  (push elem new-prop)))
      (when new-prop
	(w3m-add-text-properties pos next
				 (list 'face new-prop)))
      (setq pos next))))

(defmacro w3m-get-text-property-around (prop)
  "Search for the text property PROP in one character before and behind
the current position.  Return the value corresponding to PROP or nil.
If PROP is not found at the current position, point will move to the
position where PROP exists."
  `(let ((position (point))
	 value)
     (or (get-text-property position ,prop)
	 (and (not (bolp))
	      (setq value (get-text-property (1- position) ,prop))
	      (goto-char (1- position))
	      value)
	 (and (not (eolp))
	      (setq value (get-text-property (1+ position) ,prop))
	      (goto-char (1+ position))
	      value))))

(defmacro w3m-action (&optional position)
  "Return the value of the `w3m-action' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      `(get-text-property ,position 'w3m-action)
    `(w3m-get-text-property-around 'w3m-action)))

(defmacro w3m-anchor (&optional position)
  "Return the value of the `w3m-href-anchor' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      `(get-text-property ,position 'w3m-href-anchor)
    `(w3m-get-text-property-around 'w3m-href-anchor)))

(defmacro w3m-image (&optional position)
  "Return the value of the `w3m-image' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      `(get-text-property ,position 'w3m-image)
    `(w3m-get-text-property-around 'w3m-image)))

(defmacro w3m-image-alt (&optional position)
  "Return the value of the `w3m-image-alt' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      `(get-text-property ,position 'w3m-image-alt)
    `(w3m-get-text-property-around 'w3m-image-alt)))

(defmacro w3m-submit (&optional position)
  "Return the value of the `w3m-submit' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      `(get-text-property ,position 'w3m-submit)
    `(w3m-get-text-property-around 'w3m-submit)))

(defmacro w3m-anchor-sequence (&optional position)
  "Return the value of the `w3m-anchor-sequence' property at POSITION.
If POSITION is omitted, the current position is assumed."
  (if position
      `(get-text-property ,position 'w3m-anchor-sequence)
    '(get-text-property (point) 'w3m-anchor-sequence)))


;;; Attributes:

(eval-and-compile
  ;; `eval-and-compile' is necessary since the value of the constant
  ;; is referred to at the compile time.
  (defconst w3m-html-string-regexp
    "\\(\"\\([^\"]+\\)\"\\|'\\([^']+\\)'\\|[^\"'<> \t\r\f\n]*\\)"
    "Regexp matching a string of the field-value like <a href=\"VALUE\">."))

(put 'w3m-parse-attributes 'lisp-indent-function '1)
(def-edebug-spec w3m-parse-attributes
  ((&rest &or (symbolp &optional symbolp) symbolp) body))
(defmacro w3m-parse-attributes (attributes &rest forms)
  "Extract ATTRIBUTES, KEYWORD=\"VALUE\" pairs, in a tag and run FORMS.
ATTRIBUTES is a list of symbols that looks like `(KEYWORD KEYWORD...)'.
A symbol KEYWORD, that will express a value extracted from a tag, can
be used as a Lisp variable within FORMS.  The point has to be within
a tag initially, and only attributes that follow the point will be
extracted.

The value of KEYWORD is a string by default, or is nil if the KEYWORD
is not found in a tag.  KEYWORD can be `(KEYWORD TYPE)', where TYPE is
one of `:case-ignore', `:integer', `:bool', and `:decode-entity'.
Those types mean converting the value into a lower-case string,
an integer, a boolean (t or nil), and a decoded string respectively."
  `(let (,@(mapcar (lambda (attr)
		     (if (listp attr)
			 (car attr)
		       attr))
		   attributes))
     (skip-chars-forward " \t\r\f\n")
     (while
	 (cond
	  ,@(mapcar
	     (lambda (attr)
	       (or (symbolp attr)
		   (and (listp attr)
			(<= (length attr) 2)
			(symbolp (car attr)))
		   (error "Internal error, type mismatch"))
	       (let ((sexp (quote
			    (w3m-remove-redundant-spaces
			     (or (match-string-no-properties 2)
				 (match-string-no-properties 3)
				 (match-string-no-properties 1)))))
		     type)
		 (when (listp attr)
		   (setq type (nth 1 attr))
		   (cond
		    ((eq type :case-ignore)
		     (setq sexp (list 'downcase sexp)))
		    ((eq type :integer)
		     (setq sexp (list 'string-to-number sexp)))
		    ((eq type :bool)
		     (setq sexp t))
		    ((eq type :decode-entity)
		     (setq sexp (list 'w3m-decode-entities-string sexp)))
		    ((nth 1 attr)
		     (error "Internal error, unknown modifier")))
		   (setq attr (car attr)))
		 `((looking-at
		    ,(if (eq type :bool)
			 (format "%s\\(?:[ \t\r\f\n]*=[ \t\r\f\n]*%s\\)?"
				 (symbol-name attr)
				 w3m-html-string-regexp)
		       (format "%s[ \t\r\f\n]*=[ \t\r\f\n]*%s"
			       (symbol-name attr)
			       w3m-html-string-regexp)))
		   (setq ,attr ,sexp))))
	     attributes)
	  ((looking-at ,(concat "[A-Za-z]*[ \t\r\f\n]*=[ \t\r\f\n]*"
				w3m-html-string-regexp)))
	  ((looking-at "[^<> \t\r\f\n]+")))
       (goto-char (match-end 0))
       (skip-chars-forward " \t\r\f\n"))
     (skip-chars-forward "^>")
     (forward-char)
     ,@forms))


;;; Working buffers:

(defun w3m-get-buffer-create (name)
  "Return the buffer named NAME, or create such a buffer and return it."
  (or (get-buffer name)
      (let ((buf (get-buffer-create name)))
	(setq w3m-work-buffer-list (cons buf w3m-work-buffer-list))
	(buffer-disable-undo buf)
	buf)))

(defun w3m-kill-buffer (buffer)
  "Kill the buffer BUFFER and remove it from `w3m-work-buffer-list'.
The argument may be a buffer or may be the name of a buffer.
An argument of nil means kill the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (stringp buffer)
    (setq buffer (get-buffer buffer)))
  (when (buffer-live-p buffer)
    (kill-buffer buffer))
  (setq w3m-work-buffer-list (delq buffer w3m-work-buffer-list))
  nil)

(defun w3m-kill-all-buffer ()
  "Kill all working buffer."
  (dolist (buf w3m-work-buffer-list)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq w3m-work-buffer-list nil))

(defun w3m-current-title ()
  "Return the title of the current buffer."
  (cond
   (w3m-current-process
     "<retrieving>")
   ((and (stringp w3m-current-title)
	 (not (string= w3m-current-title "<no-title>")))
    w3m-current-title)
   ((stringp w3m-current-url)
    (directory-file-name
     (if (string-match "^[^/:]+:/+" w3m-current-url)
	 (substring w3m-current-url (match-end 0))
       w3m-current-url)))
   (t "<no-title>")))

(defun w3m-buffer-title (buffer)
  "Return the title of the buffer BUFFER."
  (with-current-buffer buffer
    (w3m-current-title)))

(defun w3m-buffer-number (buffer)
  (when (and (bufferp buffer)
	     (string-match "\\*w3m\\*\\(<\\([0-9]+\\)>\\)?\\'"
			   (buffer-name buffer)))
    (if (match-beginning 1)
	(string-to-number (match-string 2 (buffer-name buffer)))
      1))) ;; `1' should not be represented in the buffer name.

(defun w3m-buffer-set-number (buffer number)
  (with-current-buffer buffer
    (let ((newname (if w3m-use-title-buffer-name
		       (if (= number 1)
			   (format "%s *w3m*" (w3m-current-title))
			 (format "%s *w3m*<%d>" (w3m-current-title) number))
		     (if (= number 1)
			 "*w3m*"
		       (format "*w3m*<%d>" number)))))
      (if (eq (w3m-buffer-number buffer) number)
	  (when w3m-use-title-buffer-name
	    (unless (get-buffer newname)
	      (rename-buffer newname)))
	(unless (get-buffer newname)
	  (rename-buffer newname))))))

(defun w3m-buffer-name-add-title ()
  "Add current tile to buffer name."
  (when w3m-use-title-buffer-name
    (let ((number (w3m-buffer-number (current-buffer)))
	  newname)
      (if (= number 1)
	  (setq newname (format "%s *w3m*" (w3m-current-title)))
	(setq newname (format "%s *w3m*<%d>" (w3m-current-title) number)))
      (rename-buffer newname))))

(defun w3m-generate-new-buffer (name)
  (if w3m-use-title-buffer-name
      (let* ((maxbuf (let ((w3m-fb-mode nil))
		       (car (nreverse (w3m-list-buffers)))))
	     (number (w3m-buffer-number maxbuf)))
	(when (string-match "\\*w3m\\*\\(<\\([0-9]+\\)>\\)?\\'" name)
	  (setq name "*w3m*"))
	(if (and maxbuf number)
	    (generate-new-buffer (format "%s<%d>" name (1+ number)))
	  (generate-new-buffer name)))
    (generate-new-buffer name)))

(defun w3m-buffer-name-lessp (x y)
  "Return t if first arg buffer's name is less than second."
  (when (bufferp x)
    (setq x (buffer-name x)))
  (when (bufferp y)
    (setq y (buffer-name y)))
  (if (and (string-match "\\*w3m\\*\\(<\\([0-9]+\\)>\\)?\\'" x)
	   (setq x (cons x
			 (if (match-beginning 1)
			     (string-to-number (match-string 2 x))
			   1))))
      (if (string-match "\\*w3m\\*\\(<\\([0-9]+\\)>\\)?\\'" y)
	  (< (cdr x)
	     (if (match-beginning 1)
		 (string-to-number (match-string 2 y))
	       1))
	(string< (car x) y))
    (string< x y)))

(defun w3m-list-buffers (&optional nosort)
  "Return a list of buffers in which emacs-w3m sessions are open.
If the optional NOSORT is nil, the list is sorted in the order of
buffer names."
  (let ((buffers (buffer-list))
	buffer rest)
    (save-current-buffer
      (while buffers
	(set-buffer (setq buffer (pop buffers)))
	(when (eq major-mode 'w3m-mode)
	  (push buffer rest))))
    (setq buffers (if nosort
		      (nreverse rest)
		    (sort rest #'w3m-buffer-name-lessp)))
    (when (and (boundp 'w3m-fb-mode)
	       w3m-fb-mode
	       (if (or w3m-pop-up-frames
		       (not (memq 'w3m-fb-add w3m-mode-hook)))
		   ;; `w3m-fb-mode' might have been set by something
		   ;; other than the `w3m-fb-mode' function.
		   (setq w3m-fb-mode nil)
		 t))
      ;; Don't just return `w3m-fb-buffer-list' for the selected frame
      ;; because `buffers' may have been sorted.
      (let ((fbs (w3m-fb-frame-parameter w3m-fb-list-buffers-frame
					 'w3m-fb-buffer-list)))
	(setq rest buffers)
	(while rest
	  (unless (memq (setq buffer (pop rest)) fbs)
	    (setq buffers (delq buffer buffers))))))
    buffers))


;;; Pop up and delete buffers, windows or frames:

(defmacro w3m-popup-frame-parameters ()
  "Return a pop-up frame plist if this file is compiled for XEmacs,
otherwise return an alist."
  (if (featurep 'xemacs)
      '(let ((params (or w3m-popup-frame-parameters pop-up-frame-plist)))
	 (if (consp (car-safe params))
	     (alist-to-plist params)
	   params))
    '(let ((params (or w3m-popup-frame-parameters pop-up-frame-alist))
	   alist)
       (if (consp (car-safe params))
	   params
	 (while params
	   (push (cons (car params) (cdr params)) alist)
	   (setq params (cddr params)))
	 (nreverse alist)))))

(defun w3m-device-on-window-system-p ()
  "Return non-nil if the selected frame is on a widnow system"
  (w3m-static-if (featurep 'xemacs)
      (device-on-window-system-p)
    window-system))

(defmacro w3m-popup-frame-p ()
  "Return non-nil if `w3m-pop-up-frames' is non-nil and Emacs really
supports separate frames."
  '(and w3m-pop-up-frames (w3m-device-on-window-system-p)))

(defmacro w3m-use-tab-p ()
  "Return non-nil if `w3m-use-tab' is non-nil and Emacs really supports
the tabs line."
  (cond ((featurep 'xemacs)
	 '(and w3m-use-tab (device-on-window-system-p)))
	(t
	 'w3m-use-tab)))

(defun w3m-lefttab-exist-p (&optional buffer)
  (not (eq (or buffer (current-buffer)) (car (w3m-list-buffers)))))

(defun w3m-righttab-exist-p (&optional buffer)
  (let ((bufs (w3m-list-buffers))
	(cbuf (or buffer (current-buffer)))
	buf)
    (catch 'exist
      (while (setq buf (car bufs))
	(setq bufs (cdr bufs))
	(when (eq cbuf buf)
	  (throw 'exist bufs))))))

(defmacro w3m-popup-window-p ()
  "Return non-nil if `w3m-pop-up-windows' is non-nil and the present
situation allows it."
  '(and w3m-pop-up-windows
	(not (w3m-use-tab-p))
	(not (get-buffer-window w3m-select-buffer-name))))

(defvar w3m-initial-frames nil
  "Variable used to keep a list of the frame-IDs when emacs-w3m sessions
are popped-up as new frames.  This variable is used for the control
for not deleting frames made for aims other than emacs-w3m sessions.")
(make-variable-buffer-local 'w3m-initial-frames)

(defun w3m-popup-buffer (buffer)
  "Pop up BUFFER as a new window or a new frame
according to `w3m-pop-up-windows' and `w3m-pop-up-frames' (which see)."
  (let ((window (get-buffer-window buffer t))
	(oframe (selected-frame))
	(popup-frame-p (w3m-popup-frame-p))
	frame pop-up-frames buffers other)
    (if (setq
	 pop-up-frames
	 (if window ;; The window showing BUFFER already exists.
	     ;; Don't pop up a new frame if it is just the current frame.
	     (not (eq (setq frame (window-frame window)) oframe))
	   ;; There is no window for BUFFER, so look for the existing
	   ;; emacs-w3m window if the tabs line is enabled or the
	   ;; selection window exists (i.e., we can reuse it).
	   (if (or (w3m-use-tab-p)
		   (get-buffer-window w3m-select-buffer-name t))
	       (progn
		 (setq buffers (delq buffer (w3m-list-buffers t)))
		 (while (and (not window)
			     buffers)
		   (setq window
			 (get-buffer-window (setq other (pop buffers)) t)))
		 (if window ;; The window showing another buffer exists.
		     (not (eq (setq frame (window-frame window)) oframe))
		   (setq other nil)
		   ;; There is no window after all, so leave to the value
		   ;; of `w3m-pop-up-frames' whether to pop up a new frame.
		   popup-frame-p))
	     ;; Ditto.
	     popup-frame-p)))
	(progn
	  (cond (other
		 ;; Pop up another emacs-w3m buffer and switch to BUFFER.
		 (pop-to-buffer other)
		 ;; Change the value for BUFFER's `w3m-initial-frames'.
		 (setq w3m-initial-frames
		       (prog1
			   (copy-sequence w3m-initial-frames)
			 (switch-to-buffer buffer))))
		(frame
		 ;; Pop up the existing frame which shows BUFFER.
		 (pop-to-buffer buffer))
		(t
		 ;; Pop up a new frame.
		 (let* ((pop-up-frame-alist (w3m-popup-frame-parameters))
			(pop-up-frame-plist pop-up-frame-alist))
		   (pop-to-buffer buffer))
		 (setq frame (window-frame (get-buffer-window buffer t)))))
	  ;; Raise, select and focus the frame.
	  (if (fboundp 'select-frame-set-input-focus)
	      (select-frame-set-input-focus frame)
	    (raise-frame frame)
	    (select-frame frame)
	    (w3m-static-when (featurep 'xemacs)
	      (focus-frame frame)))
	  (w3m-history-restore-position))
      (unless (prog1
		  (eq buffer (current-buffer))
		;; Simply switch to BUFFER in the current frame.
		(if (w3m-popup-window-p)
		    (let ((pop-up-windows t))
		      (pop-to-buffer buffer))
		  (switch-to-buffer buffer)))
	(w3m-history-restore-position)))))

(eval-when-compile
  (when (and (fboundp 'select-frame-set-input-focus)
	     (eq (symbol-function 'select-frame-set-input-focus) 'ignore))
    (fmakunbound 'select-frame-set-input-focus)))

(defun w3m-add-w3m-initial-frames (&optional frame)
  "Add FRAME into `w3m-initial-frames', the buffer-local variable.
It is done when FRAME is newly created for the emacs-w3m session.
This function is added to the hook which is different with the Emacs
version as follows:

XEmacs          `create-frame-hook'
Emacs 21,22     `after-make-frame-functions'
Emacs 19        `after-make-frame-hook'

Note that `after-make-frame-hook' doesn't take an argument."
  (unless frame
    (setq frame (selected-frame)))
  ;; Share the opened frame in `w3m-initial-frames' over all emacs-w3m
  ;; buffers if `w3m-use-tab' is non-nil.  Otherwise, the frame is
  ;; appended into `w3m-initial-frames' only in the current buffer.
  (with-current-buffer (window-buffer (frame-first-window frame))
    (when (eq major-mode 'w3m-mode)
      (unless (memq frame w3m-initial-frames)
	(push frame w3m-initial-frames))
      (when w3m-use-tab
	(dolist (buffer (delq (current-buffer) (w3m-list-buffers t)))
	  (set-buffer buffer)
	  (unless (memq frame w3m-initial-frames)
	    (push frame w3m-initial-frames)))))))

(add-hook (if (featurep 'xemacs)
	      'create-frame-hook
	    'after-make-frame-functions)
	  'w3m-add-w3m-initial-frames)

(defun w3m-delete-w3m-initial-frames (frame)
  "Delete FRAME from `w3m-initial-frames', the buffer-local variable.
It is done when the FRAME in which emacs-w3m is running is deleted.
This function is added to `delete-frame-hook' (`delete-frame-functions'
is used instead in Emacs 22) or merged into the `delete-frame' function
using `defadvice'."
  (save-current-buffer
    (dolist (buffer (w3m-list-buffers t))
      (set-buffer buffer)
      (setq w3m-initial-frames (delq frame w3m-initial-frames)))))

(cond ((boundp 'delete-frame-functions)
       (add-hook 'delete-frame-functions 'w3m-delete-w3m-initial-frames))
      (t
       (add-hook 'delete-frame-hook 'w3m-delete-w3m-initial-frames)))

(defun w3m-delete-frames-and-windows (&optional exception)
  "Delete all frames and windows related to emacs-w3m buffers.
If EXCEPTION is a buffer, a window or a frame, it and related visible
objects will not be deleted.  There are special cases; the following
objects will not be deleted:

1. The sole frame in the display device.
2. Frames created not for emacs-w3m sessions.
3. Frames showing not only emacs-w3m sessions but also other windows.\
"
  (let ((buffers (delq exception (w3m-list-buffers t)))
	buffer windows window frame one-window-p flag)
    (save-current-buffer
      (while buffers
	(setq buffer (pop buffers)
	      windows (delq exception
			    (get-buffer-window-list buffer 'no-minibuf t)))
	(set-buffer buffer)
	(while windows
	  (setq window (pop windows)
		frame (window-frame window))
	  (when (and frame
		     (not (eq frame exception)))
	    (setq one-window-p
		  (w3m-static-if (featurep 'xemacs)
		      (one-window-p t frame)
		    ;; Emulate XEmacs version's `one-window-p'.
		    (prog2
			(setq flag nil)
			(catch 'exceeded
			  (walk-windows (lambda (w)
					  (when (eq (window-frame w) frame)
					    (if flag
						(throw 'exceeded nil)
					      (setq flag t))))
					'no-minibuf t)
			  flag)
		      (set-buffer buffer))))
	    (if (and (memq frame w3m-initial-frames)
		     (not (eq (next-frame frame) frame)))
		(if (or
		     ;; A frame having the sole window can be deleted.
		     one-window-p
		     ;; Also a frame having only windows for emacs-w3m
		     ;; sessions or the buffer selection can be deleted.
		     (progn
		       (setq flag t)
		       (walk-windows
			(lambda (w)
			  (when flag
			    (if (eq w exception)
				(setq flag nil)
			      (set-buffer (window-buffer w))
			      (setq flag (or (memq major-mode
						   '(w3m-mode
						     w3m-select-buffer-mode
						     w3m-session-select-mode))
					     (string-match "\\` ?\\*w3m[ -]"
							   (buffer-name)))))))
			'no-minibuf)
		       (set-buffer buffer)
		       flag))
		    (delete-frame frame)
		  (delete-window window))
	      (unless one-window-p
		(delete-window window)))))))))


;;; Miscellaneous:

(defconst w3m-url-fallback-base "http:///")
(defconst w3m-url-invalid-regexp "\\`http:///")

(defun w3m-url-valid (url)
  (and url (not (string-match w3m-url-invalid-regexp url))
       url))

(defmacro w3m-set-match-data (list)
  "Same as the `set-match-data'; convert points into markers under XEmacs."
  (if (featurep 'xemacs)
      `(let ((list ,list))
	 (store-match-data (dolist (pt (prog1 list (setq list nil))
				       (nreverse list))
			     (push (if (markerp pt)
				       pt
				     (set-marker (make-marker) pt))
				   list))))
    `(set-match-data ,list)))

(defun w3m-search-tag-1 (regexp)
  "Subroutine used by `w3m-search-tag'."
  (let ((start (point))
	begin end)
    (if (and (re-search-forward regexp nil t)
	     (setq begin (match-beginning 0)
		   end (match-end 0))
	     (or (looking-at "/?>")
		 (and (looking-at "[ \t\f\n]")
		      (search-forward ">" nil t))))
	(prog1
	    (goto-char (match-end 0))
	  (w3m-set-match-data
	   (cond ((= end (match-beginning 0))
		  (list begin (match-end 0)
			(1+ begin) end))
		 ((eq (char-before (match-beginning 0)) ?/)
		  (if (= end (1- (match-beginning 0)))
		      (list begin (match-end 0)
			    (1+ begin) end)
		    (list begin (match-end 0)
			  (1+ begin) end
			  end (- (match-end 0) 2))))
		 (t
		  (list begin (match-end 0)
			(1+ begin) end
			end (1- (match-end 0)))))))
      (set-match-data nil)
      (goto-char start)
      nil)))

(defmacro w3m-search-tag (&rest names)
  "Search forward for a tag which begins with one of NAMES.
This macro generates the form equivalent to:

\(re-search-forward \"<\\\\(NAMES\\\\)\\\\([ \\t\\f\\n]+[^>]*\\\\)?/?>\" nil t)

but it works even if the tag is considerably large.

Note: this macro allows only strings for NAMES, that is, a form
something like `(if foo \"bar\" \"baz\")' cannot be used."
  `(w3m-search-tag-1 ,(concat "<" (regexp-opt names t))))

(defun w3m-string-match-url-components-1 (string)
  "Subroutine used by `w3m-string-match-url-components'."

  ;; ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
  ;;  12            3  4          5       6  7        8 9

  (let ((md (make-vector 20 nil))
	pt)
    (with-temp-buffer
      (w3m-static-unless (featurep 'xemacs)
	(set-buffer-multibyte (multibyte-string-p string)))
      (insert string)
      (goto-char (point-min))
      (aset md 0 0)
      (aset md 1 (1- (point-max)))
      (when (looking-at "[^:/?#]+:")
	(aset md 2 0)
	(aset md 4 0)
	(goto-char (match-end 0))
	(aset md 3 (setq pt (1- (point))))
	(aset md 5 (1- pt)))
      (when (looking-at "//")
	(aset md 6 (1- (point)))
	(forward-char 2)
	(aset md 8 (1- (point)))
	(skip-chars-forward "^/?#")
	(aset md 7 (setq pt (1- (point))))
	(aset md 9 pt))
      (aset md 10 (1- (point)))
      (skip-chars-forward "^?#")
      (aset md 11 (setq pt (1- (point))))
      (when (eq (char-after) ??)
	(aset md 12 pt)
	(forward-char 1)
	(aset md 14 (1- (point)))
	(skip-chars-forward "^#")
	(aset md 13 (setq pt (1- (point))))
	(aset md 15 pt))
      (unless (eobp)
	(aset md 16 (1- (point)))
	(aset md 18 (point))
	(aset md 17 (setq pt (1- (point-max))))
	(aset md 19 pt)))
    (set-match-data (append md nil)))
  0)

(defconst w3m-url-components-regexp
  "\\`\\(\\([^:/?#]+\\):\\)?\\(//\\([^/?#]*\\)\\)?\
\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?\\'"
  "Regexp used for parsing a URI Reference.
It matches the potential four components and fragment identifier of a
URI reference.  See RFC2396, Appendix B for details.")

(defmacro w3m-string-match-url-components (string)
  "Do the same thing as `(string-match w3m-url-components-regexp STRING)'.
But this function should work even if STRING is considerably long."
  `(let ((string ,string))
     (condition-case nil
	 (string-match w3m-url-components-regexp string)
       (error ;; Stack overflow in regexp matcher
	(w3m-string-match-url-components-1 string)))))

(defun w3m-time-newer-p (a b)
  "Return t, if A is newer than B.  Otherwise return nil.
A and B are lists which represent time in Emacs-style.  If value is
nil, it is regarded as the oldest time."
  (and a
       (or (not b)
	   (or (> (car a) (car b))
	       (and (= (car a) (car b))
		    (> (nth 1 a) (nth 1 b)))))))

(defun w3m-time-lapse-seconds (start end)
  "Return lapse seconds from START to END.
START and END are lists which represent time in Emacs-style."
  (+ (* (- (car end) (car start)) 65536)
     (cadr end)
     (- (cadr start))))

(defalias 'w3m-float-time
  (if (fboundp 'float-time)
      'float-time
    (lambda (&optional specified-time)
      "Return the current time, as a float number of seconds since the epoch.
If an argument is given, it specifies a time to convert to float
instead of the current time.  The argument should have the forms:
 (HIGH . LOW) or (HIGH LOW USEC) or (HIGH LOW . USEC).

WARNING: Since the result is floating point, it may not be exact.
Do not use this function if precise time stamps are required."
      (let ((time (or specified-time (current-time))))
	(+ (* (car time) 65536.0)
	   (cadr time)
	   (cond ((consp (setq time (cddr time)))
		  (/ (car time) 1000000.0))
		 (time
		  (/ time 1000000.0))
		 (t
		  0)))))))

(defun w3m-url-local-p (url)
  "If URL points a file on the local system, return non-nil value.
Otherwise return nil."
  (string-match "\\`file:" url))

(defconst w3m-url-authinfo-regexp
  "\\`\\([^:/?#]+:\\)?//\\([^/?#:]+\\)\\(?::\\([^/?#@]+\\)\\)?@"
  "Regular expression for parsing the authentication part of a URI reference")

(defun w3m-url-authinfo (url)
  "Return a user name and a password to authenticate URL."
  (when (string-match w3m-url-authinfo-regexp url)
    (cons (match-string 2 url)
	  (match-string 3 url))))

(defun w3m-url-strip-authinfo (url)
  "Remove the authentication part from the URL."
  (if (string-match w3m-url-authinfo-regexp url)
      (concat (match-string 1 url)
	      "//"
	      (substring url (match-end 0)))
    url))

(defun w3m-url-strip-fragment (url)
  "Remove the fragment identifier from the URL."
  (if (string-match "\\`\\([^#]*\\)#" url)
      (match-string 1 url)
    url))

(defun w3m-url-strip-query (url)
  "Remove the query part and the fragment identifier from the URL."
  (if (string-match "\\`\\([^?#]*\\)[?#]" url)
      (match-string 1 url)
    url))

(defun w3m-get-server-hostname (url)
  "Extract a server root from URL."
  (when (string-match "\\`about://[^/?#]+/" url)
    (setq url (substring url (match-end 0))))
  (setq url (w3m-url-strip-authinfo url))
  (if (string-match "\\`[^:/?#]+://\\([^/?#]+\\)" url)
      (downcase (match-string 1 url))
    url))

(defun w3m-which-command (command)
  (when (stringp command)
    (if (and (file-name-absolute-p command)
	     (file-executable-p command))
	command
      (setq command (file-name-nondirectory command))
      (catch 'found-command
	(let (bin)
	  (dolist (dir exec-path)
	    (setq bin (expand-file-name command dir))
	    (when (or (and (file-executable-p bin)
			   (not (file-directory-p bin)))
		      (and (file-executable-p (setq bin (concat bin ".exe")))
			   (not (file-directory-p bin))))
	      (throw 'found-command bin))))))))

(defun w3m-cancel-refresh-timer (&optional buffer)
  "Cancel the timer for REFRESH attribute in META tag."
  (when w3m-use-refresh
    (with-current-buffer (or buffer (current-buffer))
      (setq w3m-current-refresh nil)
      (when w3m-refresh-timer
	(w3m-static-if (featurep 'xemacs)
	    (delete-itimer w3m-refresh-timer)
	  (cancel-timer w3m-refresh-timer))
	(setq w3m-refresh-timer nil)))))

(cond ((featurep 'xemacs)
       ;; The function of the XEmacs version doesn't work correctly
       ;; for wide characters.
       (defun w3m-truncate-string (str end-column)
	 "Truncate string STR to end at column END-COLUMN."
	 (let ((len (length str))
	       (column 0)
	       (idx 0))
	   (condition-case nil
	       (while (< column end-column)
		 (setq column (+ column (char-width (aref str idx)))
		       idx (1+ idx)))
	     (args-out-of-range (setq idx len)))
	   (when (> column end-column)
	     (setq idx (1- idx)))
	   (substring str 0 idx))))
      (t
       (defalias 'w3m-truncate-string 'truncate-string-to-width)))

(defun w3m-assoc-ignore-case (name alist)
  "Return the element of ALIST whose car equals NAME ignoring its case."
  (let ((dname (downcase name))
	match)
    (while alist
      (when (and (consp (car alist))
		 (string= dname (downcase (car (car alist)))))
	(setq match (car alist)
	      alist nil))
      (setq alist (cdr alist)))
    match))

(defun w3m-prin1 (object &optional stream)
  "Like `prin1', except that control chars will be represented with ^ as
`cat -v' does."
  (if (stringp object)
      (let (rest)
	(dolist (char (append object nil) rest)
	  (cond ((eq char ?\C-?)
		 (push "^?" rest))
		((or (memq char '(?\t ?\n))
		     (>= char ?\ ))
		 (push (char-to-string char) rest))
		(t
		 (push (concat "^" (char-to-string (+ 64 char))) rest))))
	(prin1 (apply 'concat (nreverse rest)) stream))
    (prin1 object stream)))

(defun w3m-modify-plist (plist &rest properties)
  "Change values in PLIST corresponding to PROPERTIES.  This is similar
to `plist-put', but handles plural symbol and value pairs and remove
pairs from PLIST whose value is nil."
  (while properties
    (setq plist (plist-put plist (car properties) (cadr properties))
	  properties (cddr properties)))
  (while plist
    (if (cadr plist)
	(setq properties (nconc properties (list (car plist) (cadr plist)))))
    (setq plist (cddr plist)))
  properties)

(def-edebug-spec w3m-insert-string (form))
(defmacro w3m-insert-string (string)
  "Insert STRING at point without conversions in either case the
multibyteness of the buffer."
  (if (and (fboundp 'string-as-multibyte)
	   (subrp (symbol-function 'string-as-multibyte)))
      `(let ((string ,string))
	 (insert (if enable-multibyte-characters
		     (string-as-multibyte string)
		   (string-as-unibyte string))))
    `(insert ,string)))

(defun w3m-custom-hook-initialize (symbol value)
  "Initialize the hook option pointed by the SYMBOL with the default VALUE."
  (if (boundp symbol)
      (progn
	(setq value (eval value))
	(while value
	  (add-hook symbol (car value))
	  (setq value (cdr value))))
    (custom-initialize-set symbol value)))

(defun w3m-run-mode-hooks (&rest funcs)
  "Run `run-mode-hooks' if it is available, otherwise `run-hooks'."
  (if (fboundp 'run-mode-hooks)
      (apply 'run-mode-hooks funcs)
    (apply 'run-hooks funcs)))

(defmacro w3m-keep-region-active ()
  "Keep the region active after evaluating this current command.
In XEmacs, `zmacs-region-stays' is set to nil everywhen a command is
evaluated.  This means that the region is always deactivated after
evaluating the current command.  This macro sets t to it, and keeps
the region active."
  (when (featurep 'xemacs)
    '(if (interactive-p)
	 (setq zmacs-region-stays t))))

(defmacro w3m-deactivate-region ()
  "Deactivate the region."
  (if (featurep 'xemacs)
      '(zmacs-deactivate-region)
    '(deactivate-mark)))

(defmacro w3m-region-active-p ()
  "Say whether the region is active."
  (if (fboundp 'region-active-p)
      (list 'region-active-p)
    (list 'and 'transient-mark-mode 'mark-active)))

(eval-and-compile
  (cond
   ((fboundp 'replace-regexp-in-string)
    (defun w3m-replace-in-string  (string regexp newtext &optional literal)
      ;;(replace-regexp-in-string regexp newtext string nil literal)))
      ;;
      ;; Don't call the symbol function `replace-regexp-in-string' directly
      ;; in order to silence the byte-compiler when an Emacs which doesn't
      ;; provide it is used.  The following form generates exactly the same
      ;; byte-code.
      (funcall (symbol-function 'replace-regexp-in-string)
	       regexp newtext string nil literal)))
   (t
    (defalias 'w3m-replace-in-string 'replace-in-string))))

(if (fboundp 'compare-strings)
    (defalias 'w3m-compare-strings 'compare-strings)
  (defun w3m-compare-strings (string1 start1 end1 string2 start2 end2)
    "Compare the contents of two strings."
    (let* ((str1 (substring string1 start1 end1))
	   (str2 (substring string2 start2 end2))
	   (len (min (length str1) (length str2)))
	   (i 0))
      (if (string= str1 str2)
	  t
	(setq i (catch 'ignore
		  (while (< i len)
		    (when (not (eq (aref str1 i) (aref str2 i)))
		      (throw 'ignore i))
		    (setq i (1+ i)))
		  i))
	(1+ i)))))

(eval-and-compile
  ;; This function will be redefined in w3m-ems.el.
  (unless (fboundp 'w3m-force-window-update)
    (defalias 'w3m-force-window-update 'ignore)))

(if (boundp 'header-line-format)
    (defun w3m-force-window-update-later (buffer &optional seconds)
      "Update the header-line appearance in BUFFER after SECONDS.
If SECONDS is omitted, it defaults to 0.5."
      (run-at-time (or seconds 0.5) nil
		   (lambda (buffer)
		     (when (and (buffer-live-p buffer)
				(eq (get-buffer-window buffer t)
				    (selected-window)))
		       (w3m-force-window-update)))
		   buffer))
  (defalias 'w3m-force-window-update-later 'ignore))

(if (fboundp 'read-number)
    (defalias 'w3m-read-number 'read-number)
  (defun w3m-read-number (prompt &optional default)
    "Read a numeric value in the minibuffer, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
The value of DEFAULT is inserted into PROMPT."
    (let ((n nil))
      (when default
	(setq prompt
	      (if (string-match "\\(\\):[ \t]*\\'" prompt)
		  (replace-match (format " (default %s)" default) t t prompt 1)
		(w3m-replace-in-string prompt "[ \t]*\\'"
				       (format " (default %s) " default)
				       t))))
      (while
	  (progn
	    (let ((str (read-from-minibuffer
			prompt nil nil nil nil
			(and default (number-to-string default)))))
	      (condition-case nil
		  (setq n (cond
			   ((zerop (length str)) default)
			   ((stringp str) (read str))))
		(error nil)))
	    (unless (numberp n)
	      (message "Please enter a number.")
	      (sit-for 1)
	      t)))
      n)))

(defun w3m-make-menu-item (japan english)
  "Make menu item."
  (cond
   ((and w3m-use-japanese-menu (featurep 'xemacs))
    (concat japan "%_ "))
   (w3m-use-japanese-menu
    japan)
   (t
    english)))

(defvar w3m-make-menu-commands-keys nil)

(defun w3m-make-menu-commands (menu-commands)
  "Make menu items."
  (mapcar
   (lambda (c)
     (if (consp c)
	 (vector
	  (cadr c)
	  (if (nth 3 c)
	      `(progn
		 (switch-to-buffer w3m-tab-button-menu-current-buffer)
		 (funcall (function ,(car c)) ,@(nthcdr 4 c)))
	    `(save-window-excursion
	       (switch-to-buffer w3m-tab-button-menu-current-buffer)
	       (funcall (function ,(car c)) ,@(nthcdr 4 c))))
	  :active (nth 2 c)
	  :keys (or (and (assq (car c) w3m-make-menu-commands-keys)
			 (cdr (assq (car c) w3m-make-menu-commands-keys)))
		    (let ((key (where-is-internal (car c) w3m-mode-map)))
		      (when key
			(setq w3m-make-menu-commands-keys
			      (cons (cons (car c)
					  (key-description (car key)))
				    w3m-make-menu-commands-keys))
			(cdr (car w3m-make-menu-commands-keys))))))
       (symbol-name c)))
   menu-commands))

(eval-when-compile (require 'wid-edit))
(defun w3m-widget-type-convert-widget (widget)
  "Convert the car of `:args' as a widget type in WIDGET."
  (apply 'widget-convert (widget-type widget)
	 (eval (car (widget-get widget :args)))))

(defun w3m-unseen-buffer-p (buffer)
  "Return t if buffer unseen."
  (with-current-buffer buffer
    w3m-buffer-unseen))

(defun w3m-visited-file-modtime ()
  "Replacement of `visited-file-modtime'.
It returns a list of two integers if the current buffer visits a file,
otherwise returns the number 0.  In modern Emacsen, this function will
get to be the alias to `visited-file-modtime'."
  (let ((modtime (visited-file-modtime)))
    (cond ((consp (cdr-safe modtime))
	   (defalias 'w3m-visited-file-modtime 'visited-file-modtime)
	   modtime)
	  ((integerp (cdr-safe modtime))
	   ;; XEmacs version returns `(0 . 0)' if no file is visited.
	   (if (and (= (car modtime) 0) (= (cdr modtime) 0))
	       0
	     (list (car modtime) (cdr modtime))))
	  (t
	   modtime))))

;;; Punycode RFC 3492:

(defconst w3m-puny-code-regex "xn--\\([-0-9a-zA-z]+\\)")
(defconst w3m-puny-code-nonascii "[^\000-\177]")

(defconst w3m-puny-base 36)
(defconst w3m-puny-tmin 1)
(defconst w3m-puny-tmax 26)
(defconst w3m-puny-damp 700)
(defconst w3m-puny-skew 38)
(defconst w3m-puny-initial-bias 72)
(defconst w3m-puny-initial-n 128)
(defconst w3m-puny-delimiter ?-)

(defun w3m-puny-adapt (delta numpoints firsttime)
  (let ((k 0))
    (if firsttime
	(setq delta (/ delta w3m-puny-damp))
      (setq delta (/ delta 2)))
    (setq delta (+ delta (/ delta numpoints)))
    (while (> delta (/ (* (- w3m-puny-base w3m-puny-tmin) w3m-puny-tmax) 2))
      (setq delta (/ delta (- w3m-puny-base w3m-puny-tmin)))
      (setq k (+ k w3m-puny-base)))
    (+ k (/ (* (1+ (- w3m-puny-base w3m-puny-tmin)) delta) (+ delta w3m-puny-skew)))))

(defun w3m-puny-decode-digit (cp)
  (if (< (- cp 48) 10)
      (- cp 22)
    (if (< (- cp 65) 26)
	(- cp 65)
      (if (< (- cp 97) 26)
	  (- cp 97)
	w3m-puny-base))))

(defun w3m-puny-encode-digit (d)
  (if (< d 26)
      (+ d 22 75) ;; a-z
    (+ d 22))) ;; 0-9

(defun w3m-puny-decode1 (input)
  (let* ((n w3m-puny-initial-n)
	 (bias w3m-puny-initial-bias)
	 (len (length input))
	 (in 0) (out 0)
	 (i 0) (b 0)
	 digit thr oldi w k output ret)
    (dotimes (j len)
      (if (= (aref input j) w3m-puny-delimiter) (setq b j)))
    (dotimes (j b)
      (setq output (cons (aref input j) output))
      (setq out (1+ out)))
    (setq output (nreverse output))
    (if (> b 0) (setq in (1+ b)) (setq in 0))
    (while (< in len)
      (setq oldi i)
      (setq w 1)
      (setq k w3m-puny-base)
      (catch 'loop
	(while t
	  (if (>= in len) (error "punycode bad input"))
	  (setq digit (w3m-puny-decode-digit (aref input in)))
	  (if (>= digit w3m-puny-base) (error "punycode bad input"))
	  (setq in (1+ in))
	  (setq i (+ i (* digit w)))
	  (if (<= k bias)
	      (setq thr w3m-puny-tmin)
	    (if (>= k (+ bias w3m-puny-tmax))
		(setq thr w3m-puny-tmax)
	      (setq thr (- k bias))))
	  (if (< digit thr) (throw 'loop nil))
	  (setq w (* w (- w3m-puny-base thr)))
	  (setq k (+ k w3m-puny-base))))
      (setq out (1+ out))
      (setq bias (w3m-puny-adapt (- i oldi) out (= oldi 0)))
      (setq n (+ n (/ i out)))
      (setq i (% i out))
      (if (= i 0)
	  (setq output (cons n (nthcdr i output)))
	(setcdr (nthcdr (1- i) output) (cons n (nthcdr i output))))
      (setq i (1+ i)))
    (setq ret (make-string (* out 2) ?a))
    (let ((j 0))
      (dolist (op output)
	(aset ret j (/ op 256))
	(setq j (1+ j))
	(aset ret j (% op 256))
	(setq j (1+ j))))
    ret))

(defun w3m-puny-decode (input)
  (condition-case nil
      (save-match-data
	(decode-coding-string
	 (w3m-puny-decode1 (substring input 4)) ;; xn--
	 w3m-puny-utf-16be))
    (error input)))

(defun w3m-puny-decode-url (url)
  "Decode URL from punycode."
  (let ((case-fold-search t)
	prot host after)
    (when (and w3m-puny-utf-16be
	       (string-match w3m-puny-code-regex url))
      (when (string-match "\\`[^:/]+://\\([^/]+\\)" url)
	(setq prot (substring url 0 (match-beginning 1)))
	(setq host (substring url (match-beginning 1) (match-end 1)))
	(setq after (substring url (match-end 0)))
	(while (string-match w3m-puny-code-regex host)
	  (setq host
		(concat (substring host 0 (match-beginning 0))
			(w3m-puny-decode
			 (substring host (match-beginning 0) (match-end 0)))
			(substring host (match-end 0)))))
	(setq url (concat prot host after))))
    url))

(defun w3m-puny-encode1 (input)
  (let* ((len (length input))
	 (h-len (/ len 2))
	 (n w3m-puny-initial-n)
	 (bias w3m-puny-initial-bias)
	 (delta 0) (out 0)
	 (output (make-string (* len 4) ?a))
	 h b m q k thr uni)
    (dotimes (j len)
      (setq uni (aref input j))
      (setq j (1+ j))
      (setq uni (+ (* uni 256) (aref input j)))
      (when (< uni 128) ;; basic
	(aset output out uni)
	(setq out (1+ out))))
    (setq h out)
    (setq b out)
    (when (> b 0)
      (aset output out w3m-puny-delimiter)
      (setq out (1+ out)))
    (while (< h h-len)
      (setq m 65536) ;; 17bits
      (dotimes (j len)
	(setq uni (aref input j))
	(setq j (1+ j))
	(setq uni (+ (* uni 256) (aref input j)))
	(if (and (>= uni n) (< uni m)) (setq m uni)))
      (setq delta (+ delta (* (- m n) (1+ h))))
      (setq n m)
      (dotimes (j len)
	(setq uni (aref input j))
	(setq j (1+ j))
	(setq uni (+ (* uni 256) (aref input j)))
	(when (< uni n)
	  (setq delta (1+ delta))
	  (if (= delta 0) (error "punycode overflow")))
	(when (= uni n)
	  (setq q delta)
	  (setq k w3m-puny-base)
	  (catch 'loop
	    (while t
	      (if (<= k bias)
		  (setq thr w3m-puny-tmin)
		(if (>= k (+ bias w3m-puny-tmax))
		    (setq thr w3m-puny-tmax)
		  (setq thr (- k bias))))
	      (if (< q thr) (throw 'loop nil))
	      (aset output out (w3m-puny-encode-digit (+ thr (% (- q thr) (- w3m-puny-base thr)))))
	      (setq out (1+ out))
	      (setq q (/ (- q thr) (- w3m-puny-base thr)))
	      (setq k (+ k w3m-puny-base))))
	  (aset output out (w3m-puny-encode-digit q))
	  (setq out (1+ out))
	  (setq bias (w3m-puny-adapt delta (1+ h) (= h b)))
	  (setq delta 0)
	  (setq h (1+ h))))
      (setq delta (1+ delta))
      (setq n (1+ n)))
    (substring output 0 out)))

(defun w3m-puny-encode (input)
  (condition-case nil
      (concat "xn--" (w3m-puny-encode1
		      (encode-coding-string input w3m-puny-utf-16be)))
    (error input)))

(defun w3m-puny-encode-url (url)
  "Encode URL to punycode."
  (if (and w3m-puny-utf-16be
	   (not (w3m-url-local-p url))
	   (string-match w3m-puny-code-nonascii url))
      (let (beg end idn)
	(with-temp-buffer
	  (insert url)
	  (goto-char (point-min))
	  (if (search-forward "://" nil t)
	      (setq beg (point))
	    (setq beg (point-min)))
	  (if (search-forward "/" nil t)
	      (setq end (1- (point)))
	    (setq end (point-max)))
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (while (re-search-forward "[^.]?[^.\000-\177][^.]*" nil t)
	      (setq idn (match-string-no-properties 0))
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert (w3m-puny-encode idn))))
	  (buffer-substring-no-properties (point-min) (point-max))))
    url))

(provide 'w3m-util)

;;; w3m-util.el ends here

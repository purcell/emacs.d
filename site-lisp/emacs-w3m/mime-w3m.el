;;; mime-w3m.el --- mime-view content filter for text

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2009
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>
;; Keywords: HTML, MIME, multimedia, mail, news

;; This file is *NOT* yet part of SEMI (Suite of Emacs MIME Interfaces).

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


;;; Install:

;; (1) Install SEMI.
;; (2) Put this file to appropriate directory.
;; (3) Write these following code to your ~/.emacs or ~/.gnus.
;;
;;        (require 'mime-w3m)


;;; Code:

(eval-when-compile
  (require 'cl)
  ;; mime-parse.el should be loaded before mime.el so as not to make
  ;; `mime-uri-parse-cid' an autoloaded function to which the byte
  ;; compiler might issue a nonsense warning.
  (require 'mime-parse)
  (require 'mime)
  (require 'w3m)
  (defvar mime-preview-condition)
  (defvar mime-setup-enable-inline-html)
  (defvar mime-view-mode-default-map))

(eval-and-compile
  (when (featurep 'xemacs)
    (require 'font)))

(defcustom mime-w3m-display-inline-images 'default
  "*Non-nil means that inline images are displayed.
When this option is equal to `default',
`w3m-default-display-inline-images' is refered instead of this option,
to decide whether inline images are displayed."
  :group 'w3m
  :group 'mime-view
  :type '(radio (const :format "%v " nil)
		(sexp :format "non-nil "
		      :match
		      (lambda (widget value)
			(and value (not (eq value 'default))))
		      :value-to-internal
		      (lambda (widget value)
			(if (and value (not (equal value "default")))
			    (widget-sexp-value-to-internal widget value)
			  "t")))
		(const default)))

(defcustom mime-w3m-safe-url-regexp "\\`cid:"
  "*Regexp that matches safe url names.
Some HTML mails might have the trick of spammers using <img> tags.  It
is likely to be intended to verify whether you have read the mail.
You can prevent your personal informations from leaking by setting
this to the regexp which matches the safe url names.  The value of the
variable `w3m-safe-url-regexp' will be bound with this value.  You may
set this value to nil if you consider all the urls to be safe."
  :group 'mime-w3m
  :type '(choice (regexp :format "%t: %v\n" :size 0)
		 (const :tag "All URLs are safe" nil)))

(defcustom mime-w3m-after-cursor-move-hook
  '(w3m-print-this-url)
  "*Hook run each time after the cursor moves in mime-w3m buffers.
This hook is called by the `mime-w3m-check-current-position' function
by way of `post-command-hook'."
  :group 'mime-w3m
  :type 'hook)

(defcustom mime-w3m-setup-hook nil
  "*Hook run at the end of function `mime-w3m-setup'."
  :group 'mime-w3m
  :type 'hook)

(defvar mime-w3m-message-structure nil)
(make-variable-buffer-local 'mime-w3m-message-structure)

(defun mime-w3m-insinuate ()
  "Insinuate `mime-w3m' module to SEMI."
  (setq mime-setup-enable-inline-html nil)
  (let (flag)
    (when (boundp 'mime-preview-condition)
      (labels ((overwrite (x)
		(if (symbolp x)
		    (if (eq x 'mime-preview-text/html)
			(setq flag 'mime-w3m-preview-text/html)
		      (when (eq x 'mime-w3m-preview-text/html)
			(setq flag t))
		      x)
		  (if (consp x)
		      (cons (overwrite (car x)) (overwrite (cdr x)))
		    x))))
	(setq mime-preview-condition
	      (overwrite mime-preview-condition))))
    (unless flag
      (eval-after-load "mime-view"
	'(progn
	   (ctree-set-calist-strictly
	    'mime-preview-condition
	    '((type . text)
	      (subtype . html)
	      (body . visible)
	      (body-presentation-method . mime-w3m-preview-text/html)))
	   (set-alist 'mime-view-type-subtype-score-alist
		      '(text . html) 3))))))

(defun mime-w3m-setup ()
  "Setup `mime-w3m' module."
  (require 'w3m)
  (when (eq mime-w3m-display-inline-images 'default)
    (setq mime-w3m-display-inline-images w3m-default-display-inline-images))
  (unless (assq 'mime-view-mode w3m-cid-retrieve-function-alist)
    (push (cons 'mime-view-mode 'mime-w3m-cid-retrieve)
	  w3m-cid-retrieve-function-alist))
  (run-hooks 'mime-w3m-setup-hook))

(def-edebug-spec mime-w3m-save-background-color t)
(defmacro mime-w3m-save-background-color (&rest body)
  (if (featurep 'xemacs)
      `(let ((color (color-name (face-background 'default))))
	 (prog1
	     (progn ,@body)
	   (font-set-face-background 'default color (current-buffer))))
    (cons 'progn body)))

;;;###autoload
(defun mime-w3m-preview-text/html (entity situation)
  (mime-w3m-setup)
  (setq mime-w3m-message-structure (mime-find-root-entity entity))
  (let ((p (point))
	(xref
	 (or (mime-entity-fetch-field entity "xref")
	     (mime-entity-fetch-field mime-w3m-message-structure "xref"))))
    (goto-char p)
    (insert "\n")
    (goto-char p)
    (mime-w3m-save-background-color
     (save-restriction
       (narrow-to-region p p)
       (mime-insert-text-content entity)
       (run-hooks 'mime-text-decode-hook)
       (condition-case err
	   (let ((w3m-safe-url-regexp mime-w3m-safe-url-regexp)
		 (w3m-display-inline-images mime-w3m-display-inline-images)
		 w3m-force-redisplay)
	     (w3m-region p (point-max)
			 (and (stringp xref)
			      (string-match "\\`http://" xref)
			      xref)
			 (mime-content-type-parameter
			  (mime-entity-content-type entity)
			  "charset"))
	     (add-text-properties p (point-max)
				  (list 'keymap w3m-minor-mode-map
					'text-rendered-by-mime-w3m t)))
	 (error (message "%s" err)))))))

(let (current-load-list)
  (defadvice mime-display-message
    (after mime-w3m-add-local-hook activate compile)
    "Advised by emacs-w3m.
Set hooks run arround each command is executed."
    (when (featurep 'w3m)
      (w3m-add-local-hook 'pre-command-hook
			  'w3m-store-current-position)
      (w3m-add-local-hook 'post-command-hook
			  'mime-w3m-check-current-position))))

(defun mime-w3m-check-current-position ()
  "Run `mime-w3m-after-cursor-move-hook' if the cursor has been moved."
  (when (and (/= (point) (car w3m-current-position))
	     (ignore-errors
	       (or (get-text-property (point)
				      'text-rendered-by-mime-w3m)
		   (get-text-property (car w3m-current-position)
				      'text-rendered-by-mime-w3m))))
    (run-hooks 'mime-w3m-after-cursor-move-hook)))

(defun mime-w3m-cid-retrieve (url &rest args)
  (let ((entity (mime-find-entity-from-content-id
		 (mime-uri-parse-cid url)
		 (with-current-buffer w3m-current-buffer
		   mime-w3m-message-structure))))
    (when entity
      ;; `mime-decode-string' should be performed in a unibyte buffer.
      (w3m-insert-string (mime-entity-content entity))
      (mime-entity-type/subtype entity))))

(let (current-load-list)
  (defadvice kill-new (before strip-keymap-properties-from-kill activate)
    "Advised by emacs-w3m.
Strip `keymap' or `local-map' properties from a killed string."
    (if (text-property-any 0 (length (ad-get-arg 0))
			   'text-rendered-by-mime-w3m t (ad-get-arg 0))
	(remove-text-properties 0 (length (ad-get-arg 0))
				'(keymap nil local-map nil)
				(ad-get-arg 0)))))

(mime-w3m-insinuate)

(provide 'mime-w3m)

;;; mime-w3m.el ends here

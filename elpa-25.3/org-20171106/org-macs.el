;;; org-macs.el --- Top-level Definitions for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains macro definitions, defsubst definitions, other
;; stuff needed for compilation and top-level forms in Org mode, as
;; well lots of small functions that are not Org mode specific but
;; simply generally useful stuff.

;;; Code:

(defmacro org-with-gensyms (symbols &rest body)
  (declare (debug (sexp body)) (indent 1))
  `(let ,(mapcar (lambda (s)
		   `(,s (make-symbol (concat "--" (symbol-name ',s)))))
                 symbols)
     ,@body))

(defun org-string-nw-p (s)
  "Return S if S is a string containing a non-blank character.
Otherwise, return nil."
  (and (stringp s)
       (string-match-p "[^ \r\t\n]" s)
       s))

(defun org-split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.

SEPARATORS is a regular expression.  When nil, it defaults to
\"[ \f\t\n\r\v]+\".

Unlike to `split-string', matching SEPARATORS at the beginning
and end of string are ignored."
  (let ((separators (or separators "[ \f\t\n\r\v]+")))
    (when (string-match (concat "\\`" separators) string)
      (setq string (replace-match "" nil nil string)))
    (when (string-match (concat separators "\\'") string)
      (setq string (replace-match "" nil nil string)))
    (split-string string separators)))

(defun org-string-display (string)
  "Return STRING as it is displayed in the current buffer.
This function takes into consideration `invisible' and `display'
text properties."
  (let* ((build-from-parts
	  (lambda (s property filter)
	    ;; Build a new string out of string S.  On every group of
	    ;; contiguous characters with the same PROPERTY value,
	    ;; call FILTER on the properties list at the beginning of
	    ;; the group.  If it returns a string, replace the
	    ;; characters in the group with it.  Otherwise, preserve
	    ;; those characters.
	    (let ((len (length s))
		  (new "")
		  (i 0)
		  (cursor 0))
	      (while (setq i (text-property-not-all i len property nil s))
		(let ((end (next-single-property-change i property s len))
		      (value (funcall filter (text-properties-at i s))))
		  (when value
		    (setq new (concat new (substring s cursor i) value))
		    (setq cursor end))
		  (setq i end)))
	      (concat new (substring s cursor)))))
	 (prune-invisible
	  (lambda (s)
	    (funcall build-from-parts s 'invisible
		     (lambda (props)
		       ;; If `invisible' property in PROPS means text
		       ;; is to be invisible, return the empty string.
		       ;; Otherwise return nil so that the part is
		       ;; skipped.
		       (and (or (eq t buffer-invisibility-spec)
				(assoc-string (plist-get props 'invisible)
					      buffer-invisibility-spec))
			    "")))))
	 (replace-display
	  (lambda (s)
	    (funcall build-from-parts s 'display
		     (lambda (props)
		       ;; If there is any string specification in
		       ;; `display' property return it.  Also attach
		       ;; other text properties on the part to that
		       ;; string (face...).
		       (let* ((display (plist-get props 'display))
			      (value (if (stringp display) display
				       (cl-some #'stringp display))))
			 (when value
			   (apply
			    #'propertize
			    ;; Displayed string could contain
			    ;; invisible parts, but no nested display.
			    (funcall prune-invisible value)
			    (plist-put props
				       'display
				       (and (not (stringp display))
					    (cl-remove-if #'stringp
							  display)))))))))))
    ;; `display' property overrides `invisible' one.  So we first
    ;; replace characters with `display' property.  Then we remove
    ;; invisible characters.
    (funcall prune-invisible (funcall replace-display string))))

(defun org-string-width (string)
  "Return width of STRING when displayed in the current buffer.
Unlike to `string-width', this function takes into consideration
`invisible' and `display' text properties."
  (string-width (org-string-display string)))

(defun org-not-nil (v)
  "If V not nil, and also not the string \"nil\", then return V.
Otherwise return nil."
  (and v (not (equal v "nil")) v))

(defmacro org-preserve-lc (&rest body)
  (declare (debug (body)))
  (org-with-gensyms (line col)
    `(let ((,line (org-current-line))
	   (,col (current-column)))
       (unwind-protect
	   (progn ,@body)
	 (org-goto-line ,line)
	 (org-move-to-column ,col)))))

;; Use `org-with-silent-modifications' to ignore cosmetic changes and
;; `org-unmodified' to ignore real text modifications
(defmacro org-unmodified (&rest body)
  "Run BODY while preserving the buffer's `buffer-modified-p' state."
  (declare (debug (body)))
  (org-with-gensyms (was-modified)
    `(let ((,was-modified (buffer-modified-p)))
       (unwind-protect
           (let ((buffer-undo-list t)
		 (inhibit-modification-hooks t))
	     ,@body)
	 (set-buffer-modified-p ,was-modified)))))

(defmacro org-without-partial-completion (&rest body)
  (declare (debug (body)))
  `(if (and (boundp 'partial-completion-mode)
	    partial-completion-mode
	    (fboundp 'partial-completion-mode))
       (unwind-protect
	   (progn
	     (partial-completion-mode -1)
	     ,@body)
	 (partial-completion-mode 1))
     ,@body))

(defmacro org-with-point-at (pom &rest body)
  "Move to buffer and point of point-or-marker POM for the duration of BODY."
  (declare (debug (form body)) (indent 1))
  (org-with-gensyms (mpom)
    `(let ((,mpom ,pom))
       (save-excursion
	 (if (markerp ,mpom) (set-buffer (marker-buffer ,mpom)))
	 (org-with-wide-buffer
	  (goto-char (or ,mpom (point)))
	  ,@body)))))

(defmacro org-with-remote-undo (buffer &rest body)
  "Execute BODY while recording undo information in two buffers."
  (declare (debug (form body)) (indent 1))
  (org-with-gensyms (cline cmd buf1 buf2 undo1 undo2 c1 c2)
    `(let ((,cline (org-current-line))
	   (,cmd this-command)
	   (,buf1 (current-buffer))
	   (,buf2 ,buffer)
	   (,undo1 buffer-undo-list)
	   (,undo2 (with-current-buffer ,buffer buffer-undo-list))
	   ,c1 ,c2)
       ,@body
       (when org-agenda-allow-remote-undo
	 (setq ,c1 (org-verify-change-for-undo
		    ,undo1 (with-current-buffer ,buf1 buffer-undo-list))
	       ,c2 (org-verify-change-for-undo
		    ,undo2 (with-current-buffer ,buf2 buffer-undo-list)))
	 (when (or ,c1 ,c2)
	   ;; make sure there are undo boundaries
	   (and ,c1 (with-current-buffer ,buf1 (undo-boundary)))
	   (and ,c2 (with-current-buffer ,buf2 (undo-boundary)))
	   ;; remember which buffer to undo
	   (push (list ,cmd ,cline ,buf1 ,c1 ,buf2 ,c2)
		 org-agenda-undo-list))))))

(defmacro org-no-read-only (&rest body)
  "Inhibit read-only for BODY."
  (declare (debug (body)))
  `(let ((inhibit-read-only t)) ,@body))

(defconst org-rm-props '(invisible t face t keymap t intangible t mouse-face t
				   rear-nonsticky t mouse-map t fontified t
				   org-emphasis t)
  "Properties to remove when a string without properties is wanted.")

(defsubst org-no-properties (s &optional restricted)
  "Remove all text properties from string S.
When RESTRICTED is non-nil, only remove the properties listed
in `org-rm-props'."
  (if restricted (remove-text-properties 0 (length s) org-rm-props s)
    (set-text-properties 0 (length s) nil s))
  s)

(defsubst org-get-alist-option (option key)
  (cond ((eq key t) t)
	((eq option t) t)
	((assoc key option) (cdr (assoc key option)))
	(t (let ((r (cdr (assq 'default option))))
	     (if (listp r) (delq nil r) r)))))

(defsubst org-check-external-command (cmd &optional use no-error)
  "Check if external program CMD for USE exists, error if not.
When the program does exist, return its path.
When it does not exist and NO-ERROR is set, return nil.
Otherwise, throw an error.  The optional argument USE can describe what this
program is needed for, so that the error message can be more informative."
  (or (executable-find cmd)
      (if no-error
	  nil
	(error "Can't find `%s'%s" cmd
	       (if use (format " (%s)" use) "")))))

(defsubst org-last (list)
  "Return the last element of LIST."
  (car (last list)))

(defun org-let (list &rest body)
  (eval (cons 'let (cons list body))))
(put 'org-let 'lisp-indent-function 1)

(defun org-let2 (list1 list2 &rest body)
  (eval (cons 'let (cons list1 (list (cons 'let (cons list2 body)))))))
(put 'org-let2 'lisp-indent-function 2)

(defsubst org-call-with-arg (command arg)
  "Call COMMAND interactively, but pretend prefix arg was ARG."
  (let ((current-prefix-arg arg)) (call-interactively command)))

(defsubst org-current-line (&optional pos)
  (save-excursion
    (and pos (goto-char pos))
    ;; works also in narrowed buffer, because we start at 1, not point-min
    (+ (if (bolp) 1 0) (count-lines 1 (point)))))

(defsubst org-goto-line (N)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- N))))

(defsubst org-current-line-string (&optional to-here)
  (buffer-substring (point-at-bol) (if to-here (point) (point-at-eol))))

(defsubst org-pos-in-match-range (pos n)
  (and (match-beginning n)
       (<= (match-beginning n) pos)
       (>= (match-end n) pos)))

(defun org-match-line (regexp)
  "Match REGEXP at the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (looking-at regexp)))

(defun org-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
	  (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defmacro org-save-outline-visibility (use-markers &rest body)
  "Save and restore outline visibility around BODY.
If USE-MARKERS is non-nil, use markers for the positions.
This means that the buffer may change while running BODY,
but it also means that the buffer should stay alive
during the operation, because otherwise all these markers will
point nowhere."
  (declare (debug (form body)) (indent 1))
  (org-with-gensyms (data)
    `(let ((,data (org-outline-overlay-data ,use-markers)))
       (unwind-protect
	   (prog1 (progn ,@body)
	     (org-set-outline-overlay-data ,data))
	 (when ,use-markers
	   (dolist (c ,data)
	     (when (markerp (car c)) (move-marker (car c) nil))
	     (when (markerp (cdr c)) (move-marker (cdr c) nil))))))))

(defmacro org-with-wide-buffer (&rest body)
  "Execute body while temporarily widening the buffer."
  (declare (debug (body)))
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defmacro org-with-limited-levels (&rest body)
  "Execute BODY with limited number of outline levels."
  (declare (debug (body)))
  `(progn
     (defvar org-called-with-limited-levels)
     (defvar org-outline-regexp)
     (defvar outline-regexp)
     (defvar org-outline-regexp-bol)
     (let* ((org-called-with-limited-levels t)
            (org-outline-regexp (org-get-limited-outline-regexp))
            (outline-regexp org-outline-regexp)
            (org-outline-regexp-bol (concat "^" org-outline-regexp)))
       ,@body)))

(defvar org-outline-regexp) ; defined in org.el
(defvar org-odd-levels-only) ; defined in org.el
(defvar org-inlinetask-min-level) ; defined in org-inlinetask.el
(defun org-get-limited-outline-regexp ()
  "Return outline-regexp with limited number of levels.
The number of levels is controlled by `org-inlinetask-min-level'"
  (cond ((not (derived-mode-p 'org-mode))
	 outline-regexp)
	((not (featurep 'org-inlinetask))
	 org-outline-regexp)
	(t
	 (let* ((limit-level (1- org-inlinetask-min-level))
		(nstars (if org-odd-levels-only
			    (1- (* limit-level 2))
			  limit-level)))
	   (format "\\*\\{1,%d\\} " nstars)))))

(defmacro org-eval-in-environment (environment form)
  (declare (debug (form form)) (indent 1))
  `(eval (list 'let ,environment ',form)))

(defun org-make-parameter-alist (flat)
  "Return alist based on FLAT.
FLAT is a list with alternating symbol names and values.  The
returned alist is a list of lists with the symbol name in car and
the value in cdr."
  (when flat
    (cons (list (car flat) (cadr flat))
	  (org-make-parameter-alist (cddr flat)))))

;;;###autoload
(defmacro org-load-noerror-mustsuffix (file)
  "Load FILE with optional arguments NOERROR and MUSTSUFFIX."
  `(load ,file 'noerror nil nil 'mustsuffix))

(defun org-unbracket-string (pre post string)
  "Remove PRE/POST from the beginning/end of STRING.
Both PRE and POST must be pre-/suffixes of STRING, or neither is
removed."
  (if (and (string-prefix-p pre string)
	   (string-suffix-p post string))
      (substring string (length pre) (- (length post)))
    string))

(defun org-read-function (prompt &optional allow-empty?)
  "Prompt for a function.
If ALLOW-EMPTY? is non-nil, return nil rather than raising an
error when the user input is empty."
  (let ((func (completing-read prompt obarray #'fboundp t)))
    (cond ((not (string= func ""))
	   (intern func))
	  (allow-empty? nil)
	  (t (user-error "Empty input is not valid")))))

(defconst org-unique-local-variables
  '(org-element--cache
    org-element--cache-objects
    org-element--cache-sync-keys
    org-element--cache-sync-requests
    org-element--cache-sync-timer)
  "List of local variables that cannot be transferred to another buffer.")

(defun org-get-local-variables ()
  "Return a list of all local variables in an Org mode buffer."
  (delq nil
	(mapcar
	 (lambda (x)
	   (let* ((binding (if (symbolp x) (list x) (list (car x) (cdr x))))
		  (name (car binding)))
	     (and (not (get name 'org-state))
		  (not (memq name org-unique-local-variables))
		  (string-match-p
		   "\\`\\(org-\\|orgtbl-\\|outline-\\|comment-\\|paragraph-\\|\
auto-fill\\|normal-auto-fill\\|fill-paragraph\\|indent-\\)"
		   (symbol-name name))
		  binding)))
	 (with-temp-buffer
	   (org-mode)
	   (buffer-local-variables)))))

(defun org-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone."
  (dolist (pair (buffer-local-variables from-buffer))
    (pcase pair
      (`(,name . ,value)		;ignore unbound variables
       (when (and (not (memq name org-unique-local-variables))
		  (or (null regexp) (string-match-p regexp (symbol-name name))))
	 (ignore-errors (set (make-local-variable name) value)))))))


(provide 'org-macs)

;;; org-macs.el ends here

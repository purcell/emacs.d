;;; elmo-util.el --- Utilities for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(eval-when-compile
  (require 'cl)
  (require 'static))
(require 'elmo-vars)
(require 'elmo-date)
(require 'mcharset)
(require 'pces)
(require 'std11)
(require 'eword-decode)
(require 'poem)
(require 'emu)

(eval-and-compile
  (autoload 'md5 "md5"))

(defvar elmo-work-buf-name " *elmo work*")
(defvar elmo-temp-buf-name " *elmo temp*")

(or (boundp 'default-enable-multibyte-characters)
    (defvar default-enable-multibyte-characters (featurep 'mule)
      "The mock variable except for Emacs 20."))

(defconst elmo-multibyte-buffer-name " *elmo-multibyte-buffer*")

(defmacro elmo-with-enable-multibyte (&rest body)
  "Evaluate BODY with `default-enable-multibyte-character' when needed."
  ;; Check whether FLIM's MIME-charset string encoder/decoders work on
  ;; unibyte buffer.
  (static-if (fboundp 'mime-charset-encode-string)
      `(progn ,@body)
    `(with-current-buffer (get-buffer-create elmo-multibyte-buffer-name)
       ,@body)))

(put 'elmo-with-enable-multibyte 'lisp-indent-function 0)
(def-edebug-spec elmo-with-enable-multibyte t)

(static-cond
 ((fboundp 'mime-charset-encode-string)
  (defalias 'elmo-mime-charset-decode-string 'mime-charset-decode-string)
  (defalias 'elmo-mime-charset-encode-string 'mime-charset-encode-string))
 (t
  (defsubst elmo-mime-charset-decode-string (string charset &optional lbt)
    "Decode the STRING as MIME CHARSET.
Buffer's multibyteness is ignored."
    (elmo-with-enable-multibyte
      (decode-mime-charset-string string charset lbt)))

  (defsubst elmo-mime-charset-encode-string (string charset &optional lbt)
    "Encode the STRING as MIME CHARSET.
Buffer's multibyteness is ignored."
    (elmo-with-enable-multibyte
      (encode-mime-charset-string string charset lbt))))
 )

(defun elmo-base64-encode-string (string &optional no-line-break))
(defun elmo-base64-decode-string (string))

;; base64 encoding/decoding
(require 'mel)
(fset 'elmo-base64-encode-string
      (mel-find-function 'mime-encode-string "base64"))
(fset 'elmo-base64-decode-string
      (mel-find-function 'mime-decode-string "base64"))

(eval-and-compile
  (if elmo-use-hardlink
      (defalias 'elmo-add-name-to-file 'add-name-to-file)
    (defun elmo-add-name-to-file
      (filename newname &optional ok-if-already-exists)
      (copy-file filename newname ok-if-already-exists t))))

(defmacro elmo-set-work-buf (&rest body)
  "Execute BODY on work buffer.  Work buffer remains."
  `(with-current-buffer (get-buffer-create elmo-work-buf-name)
     (set-buffer-multibyte default-enable-multibyte-characters)
     (erase-buffer)
     ,@body))

(put 'elmo-set-work-buf 'lisp-indent-function 0)
(def-edebug-spec elmo-set-work-buf t)

(defmacro elmo-bind-directory (dir &rest body)
  "Set current directory DIR and execute BODY."
  `(let ((default-directory (file-name-as-directory ,dir)))
     ,@body))

(put 'elmo-bind-directory 'lisp-indent-function 1)
(def-edebug-spec elmo-bind-directory
  (form &rest form))

(static-if (condition-case nil
	       (plist-get '(one) 'other)
	     (error t))
    (defmacro elmo-safe-plist-get (plist prop)
      `(ignore-errors
	 (plist-get ,plist ,prop)))
  (defalias 'elmo-safe-plist-get 'plist-get))

(eval-when-compile
  (unless (fboundp 'coding-system-base)
    (defalias 'coding-system-base 'ignore))
  (unless (fboundp 'coding-system-name)
    (defalias 'coding-system-name 'ignore))
  (unless (fboundp 'find-file-coding-system-for-read-from-filename)
    (defalias 'find-file-coding-system-for-read-from-filename 'ignore))
  (unless (fboundp 'find-operation-coding-system)
    (defalias 'find-operation-coding-system 'ignore)))

(defun elmo-set-auto-coding (&optional filename)
  "Find coding system used to decode the contents of the current buffer.
This function looks for the coding system magic cookie or examines the
coding system specified by `file-coding-system-alist' being associated
with FILENAME which defaults to `buffer-file-name'."
  (cond
   ((boundp 'set-auto-coding-function) ;; Emacs
    (if filename
	(or (funcall (symbol-value 'set-auto-coding-function)
		     filename (- (point-max) (point-min)))
	    (car (find-operation-coding-system 'insert-file-contents
					       filename)))
      (let (auto-coding-alist)
	(condition-case nil
	    (funcall (symbol-value 'set-auto-coding-function)
		     nil (- (point-max) (point-min)))
	  (error nil)))))
   ((featurep 'file-coding) ;; XEmacs
    (let ((case-fold-search t)
	  (end (point-at-eol))
	  codesys start)
      (or
       (and (re-search-forward "-\\*-+[\t ]*" end t)
	    (progn
	      (setq start (match-end 0))
	      (re-search-forward "[\t ]*-+\\*-" end t))
	    (progn
	      (setq end (match-beginning 0))
	      (goto-char start)
	      (or (looking-at "coding:[\t ]*\\([^\t ;]+\\)")
		  (re-search-forward
		   "[\t ;]+coding:[\t ]*\\([^\t ;]+\\)"
		   end t)))
	    (find-coding-system (setq codesys
				      (intern (match-string 1))))
	    codesys)
       (and (re-search-forward "^[\t ]*;+[\t ]*Local[\t ]+Variables:"
			       nil t)
	    (progn
	      (setq start (match-end 0))
	      (re-search-forward "^[\t ]*;+[\t ]*End:" nil t))
	    (progn
	      (setq end (match-beginning 0))
	      (goto-char start)
	      (re-search-forward
	       "^[\t ]*;+[\t ]*coding:[\t ]*\\([^\t\n\r ]+\\)"
	       end t))
	    (find-coding-system (setq codesys
				      (intern (match-string 1))))
	    codesys)
       (and (progn
	      (goto-char (point-min))
	      (setq case-fold-search nil)
	      (re-search-forward "^;;;coding system: "
;;;				 (+ (point-min) 3000) t))
				 nil t))
	    (looking-at "[^\t\n\r ]+")
	    (find-coding-system
	     (setq codesys (intern (match-string 0))))
	    codesys)
       (and filename
	    (setq codesys
		  (find-file-coding-system-for-read-from-filename
		   filename))
	    (coding-system-name (coding-system-base codesys))))))))

(defun elmo-object-load (filename &optional mime-charset no-err)
  "Load OBJECT from the file specified by FILENAME.
File content is decoded with MIME-CHARSET."
  (if (not (file-readable-p filename))
      nil
    (with-temp-buffer
      (insert-file-contents-as-binary filename)
      (let ((coding-system (or (elmo-set-auto-coding)
			       (mime-charset-to-coding-system
				mime-charset))))
	(when coding-system
	  (decode-coding-region (point-min) (point-max) coding-system)))
      (goto-char (point-min))
      (condition-case nil
	  (read (current-buffer))
	(error (unless no-err
		 (message "Warning: Loading object from %s failed."
			  filename)
		 (elmo-object-save filename nil mime-charset))
	       nil)))))

(defsubst elmo-save-buffer (filename &optional mime-charset)
  "Save current buffer to the file specified by FILENAME.
Directory of the file is created if it doesn't exist.
File content is encoded with MIME-CHARSET."
  (let ((dir (directory-file-name (file-name-directory filename))))
    (if (file-directory-p dir)
	() ; ok.
      (unless (file-exists-p dir)
	(elmo-make-directory dir)))
    (if (file-writable-p filename)
	(progn
	  (when mime-charset
;;;	    (set-buffer-multibyte default-enable-multibyte-characters)
	    (encode-mime-charset-region (point-min) (point-max) mime-charset))
	  (as-binary-output-file
	   (write-region (point-min) (point-max) filename nil 'no-msg)))
      (message "%s is not writable." filename))))

(defun elmo-object-save (filename object &optional mime-charset)
  "Save OBJECT to the file specified by FILENAME.
Directory of the file is created if it doesn't exist.
File content is encoded with MIME-CHARSET."
  (with-temp-buffer
    (let (print-length print-level)
      (prin1 object (current-buffer)))
    (when mime-charset
      (let ((coding (mime-charset-to-coding-system
		     (or (detect-mime-charset-region (point-min) (point-max))
			 mime-charset))))
	(goto-char (point-min))
	(insert ";;; -*- mode: emacs-lisp; coding: "
		(symbol-name coding) " -*-\n")
	(encode-coding-region (point-min) (point-max) coding)))
    (elmo-save-buffer filename)))

;;; Search Condition

(defconst elmo-condition-atom-regexp "[^/ \")|&]*")

(defsubst elmo-condition-parse-error ()
  (error "Syntax error in '%s'" (buffer-string)))

(defun elmo-parse-search-condition (condition)
  "Parse CONDITION.
Return value is a cons cell of (STRUCTURE . REST)"
  (with-temp-buffer
    (insert condition)
    (goto-char (point-min))
    (cons (elmo-condition-parse) (buffer-substring (point) (point-max)))))

;; condition    ::= or-expr
(defun elmo-condition-parse ()
  (or (elmo-condition-parse-or-expr)
      (elmo-condition-parse-error)))

;; or-expr      ::= and-expr /
;;		    and-expr "|" or-expr
(defun elmo-condition-parse-or-expr ()
  (let ((left (elmo-condition-parse-and-expr)))
    (if (looking-at "| *")
	(progn
	  (goto-char (match-end 0))
	  (list 'or left (elmo-condition-parse-or-expr)))
      left)))

;; and-expr     ::= primitive /
;;                  primitive "&" and-expr
(defun elmo-condition-parse-and-expr ()
  (let ((left (elmo-condition-parse-primitive)))
    (if (looking-at "& *")
	(progn
	  (goto-char (match-end 0))
	  (list 'and left (elmo-condition-parse-and-expr)))
      left)))

;; primitive    ::= "(" expr ")" /
;;                  ["!"] search-key SPACE* ":" SPACE* search-value
(defun elmo-condition-parse-primitive ()
  (cond
   ((looking-at "( *")
    (goto-char (match-end 0))
    (prog1 (elmo-condition-parse)
      (unless (looking-at ") *")
	(elmo-condition-parse-error))
      (goto-char (match-end 0))))
;; search-key   ::= [A-Za-z-]+
;;                 ;; "since" / "before" / "last" / "first" /
;;                 ;; "body" / "flag" / field-name
   ((looking-at "\\(!\\)? *\\([A-Za-z-]+\\) *: *")
    (goto-char (match-end 0))
    (let ((search-key (vector
		       (if (match-beginning 1) 'unmatch 'match)
		       (downcase (elmo-match-buffer 2))
		       (elmo-condition-parse-search-value))))
      ;; syntax sugar.
      (if (string= (aref search-key 1) "tocc")
	  (if (eq (aref search-key 0) 'match)
	      (list 'or
		    (vector 'match "to" (aref search-key 2))
		    (vector 'match "cc" (aref search-key 2)))
	    (list 'and
		  (vector 'unmatch "to" (aref search-key 2))
		  (vector 'unmatch "cc" (aref search-key 2))))
	search-key)))))

;; search-value ::= quoted / time / number / atom
;; quoted       ::= <elisp string expression>
;; time         ::= "yesterday" / "lastweek" / "lastmonth" / "lastyear" /
;;                   number SPACE* "daysago" /
;;                   number "-" month "-" number  ; ex. 10-May-2000
;;                   number "-" number "-" number  ; ex. 2000-05-10
;; number       ::= [0-9]+
;; month        ::= "Jan" / "Feb" / "Mar" / "Apr" / "May" / "Jun" /
;;                  "Jul" / "Aug" / "Sep" / "Oct" / "Nov" / "Dec"
;; atom         ::= ATOM_CHARS*
;; SPACE        ::= <ascii space character, 0x20>
;; ATOM_CHARS   ::= <any character except specials>
;; specials     ::= SPACE / <"> / </> / <)> / <|> / <&>
;;                  ;; These characters should be quoted.
(defun elmo-condition-parse-search-value ()
  (cond
   ((looking-at "\"")
    (read (current-buffer)))
   ((or (looking-at elmo-condition-atom-regexp)
	(looking-at "yesterday") (looking-at "lastweek")
	(looking-at "lastmonth") (looking-at "lastyear")
	(looking-at "[0-9]+ *daysago")
	(looking-at "[0-9]+-[A-Za-z]+-[0-9]+")
	(looking-at "[0-9]+-[0-9]+-[0-9]+")
	(looking-at "[0-9]+"))
    (prog1 (elmo-match-buffer 0)
      (goto-char (match-end 0))))
   (t (error "Syntax error '%s'" (buffer-string)))))

(defmacro elmo-filter-condition-p (filter)
  `(or (vectorp ,filter) (consp ,filter)))

(defmacro elmo-filter-type (filter)
  `(aref ,filter 0))

(defmacro elmo-filter-key (filter)
  `(aref ,filter 1))

(defmacro elmo-filter-value (filter)
  `(aref ,filter 2))

(defun elmo-condition-match (condition match-primitive args)
  (cond
   ((vectorp condition)
    (if (eq (elmo-filter-type condition) 'unmatch)
	(not (apply match-primitive condition args))
      (apply match-primitive condition args)))
   ((eq (car condition) 'and)
    (let ((lhs (elmo-condition-match (nth 1 condition) match-primitive args)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-condition-match (nth 2 condition)
					 match-primitive args)))
	  (cond ((elmo-filter-condition-p rhs)
		 (list 'and lhs rhs))
		(rhs
		 lhs))))
       (lhs
	(elmo-condition-match (nth 2 condition) match-primitive args)))))
   ((eq (car condition) 'or)
    (let ((lhs (elmo-condition-match (nth 1 condition) match-primitive args)))
      (cond
       ((elmo-filter-condition-p lhs)
	(let ((rhs (elmo-condition-match (nth 2 condition)
					 match-primitive args)))
	  (cond ((elmo-filter-condition-p rhs)
		 (list 'or lhs rhs))
		(rhs
		 t)
		(t
		 lhs))))
       (lhs
	t)
       (t
	(elmo-condition-match (nth 2 condition) match-primitive args)))))))

(defun elmo-condition-optimize (condition)
  (cond
   ((vectorp condition)
    (let ((key (elmo-filter-key condition)))
      (cond ((cdr (assoc key '(("first"	. 0)
			       ("last"	. 0)
			       ("flag"	. 1)
			       ("body"	. 5)))))
	    ((member key '("since" "before" "from" "subject" "to" "cc"))
	     2)
	    ((member key elmo-msgdb-extra-fields)
	     3)
	    (t
	     4))))
   (t
    (let ((weight-l (elmo-condition-optimize (nth 1 condition)))
	  (weight-r (elmo-condition-optimize (nth 2 condition))))
      (if (> weight-l weight-r)
	  (let ((lhs (nth 1 condition)))
	    (setcar (nthcdr 1 condition) (nth 2 condition))
	    (setcar (nthcdr 2 condition) lhs)
	    weight-l)
	weight-r)))))

;;;
(defsubst elmo-buffer-replace (regexp &optional newtext)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match (or newtext ""))))

(defsubst elmo-delete-char (char string &optional unibyte)
  (save-match-data
    (elmo-set-work-buf
      (let ((coding-system-for-read 'no-conversion)
	    (coding-system-for-write 'no-conversion))
	(if unibyte (set-buffer-multibyte nil))
	(insert string)
	(goto-char (point-min))
	(while (search-forward (char-to-string char) nil t)
	  (replace-match ""))
	(buffer-string)))))

(defsubst elmo-delete-cr-region (start end)
  "Delete CR from region."
  (let (inhibit-eol-conversion)
    (decode-coding-region start (or end (point-max)) 'raw-text-dos)))

(defsubst elmo-delete-cr-buffer ()
  "Delete CR from buffer."
  (elmo-delete-cr-region (point-min) nil))

(defsubst elmo-delete-cr-get-content-type ()
  (save-excursion
    (elmo-delete-cr-buffer)
    (goto-char (point-min))
    (or (std11-field-body "content-type")
	t)))

(defun elmo-delete-cr (string)
  (let (inhibit-eol-conversion)
    (decode-coding-string string 'raw-text-dos)))

(defun elmo-last (list)
  (and list (nth (1- (length list)) list)))

(defun elmo-set-list (vars vals)
  (while vars
    (when (car vars)
      (set (car vars) (car vals)))
    (setq vars (cdr vars)
	  vals (cdr vals))))

(defun elmo-uniq-list (lst &optional delete-function)
  "Destructively uniquify elements of LST."
  (setq delete-function (or delete-function #'delete))
  (let ((tmp lst))
    (while tmp
      (setq tmp
	    (setcdr tmp
		    (and (cdr tmp)
			 (funcall delete-function
				  (car tmp)
				  (cdr tmp)))))))
  lst)

(defun elmo-uniq-sorted-list (list &optional equal-function)
  "Destructively uniquify elements of sorted LIST."
  (setq equal-function (or equal-function #'equal))
  (let ((list list))
    (while list
      (while (funcall equal-function (car list) (cadr list))
	(setcdr list (cddr list)))
      (setq list (cdr list))))
  list)

(defun elmo-sort-uniq-number-list (list)
  (elmo-uniq-sorted-list (sort list #'<) #'eq))

(defun elmo-union (l1 l2)
  "Make a union of two lists"
  (elmo-sort-uniq-number-list (append l1 l2)))

(defun elmo-list-insert (list element after)
  (let* ((match (memq after list))
	 (rest (and match (cdr (memq after list)))))
    (if match
	(progn
	  (setcdr match (list element))
	  (nconc list rest))
      (nconc list (list element)))))

(defun elmo-get-file-string (filename &optional remove-final-newline)
  (elmo-set-work-buf
    (let (insert-file-contents-pre-hook	; To avoid autoconv-xmas...
	  insert-file-contents-post-hook)
      (when (file-exists-p filename)
	(if filename
	    (as-binary-input-file (insert-file-contents filename)))
	(when (and remove-final-newline
		   (> (buffer-size) 0)
		   (= (char-after (1- (point-max))) ?\n))
	  (goto-char (point-max))
	  (delete-char -1))
	(buffer-string)))))

(defun elmo-save-string (string filename)
  (if string
      (elmo-set-work-buf
	(as-binary-output-file
	 (insert string)
	 (write-region (point-min) (point-max)
		       filename nil 'no-msg))
	)))

(defun elmo-max-of-list (nlist)
  (apply #'max 0 nlist))

(defun elmo-concat-path (path filename)
  (if (not (string= path ""))
      (elmo-replace-in-string
       (if (string= elmo-path-sep (substring path (- (length path) 1)))
	   (concat path filename)
	 (concat path elmo-path-sep filename))
       (concat (regexp-quote elmo-path-sep)(regexp-quote elmo-path-sep))
       elmo-path-sep)
    filename))

(defvar elmo-passwd-alist nil)

(defun elmo-passwd-alist-load ()
  (let ((filename (expand-file-name elmo-passwd-alist-file-name
				    elmo-msgdb-directory)))
    (if (not (file-readable-p filename))
	()
      (with-temp-buffer
	(let (insert-file-contents-pre-hook ; To avoid autoconv-xmas...
	      insert-file-contents-post-hook)
	  (insert-file-contents filename)
	  (goto-char (point-min))
	  (ignore-errors
	   (read (current-buffer))))))))

(defun elmo-passwd-alist-clear ()
  "Clear password cache."
  (interactive)
  (dolist (pair elmo-passwd-alist)
    (when (stringp (cdr-safe pair))
      (fillarray (cdr pair) 0)))
  (setq elmo-passwd-alist nil))

(defun elmo-passwd-alist-save ()
  "Save password into file."
  (interactive)
  (with-temp-buffer
    (let ((filename (expand-file-name elmo-passwd-alist-file-name
				      elmo-msgdb-directory))
	  print-length print-level)
      (prin1 elmo-passwd-alist (current-buffer))
      (princ "\n" (current-buffer))
;;;      (if (and (file-exists-p filename)
;;;	       (not (equal 384 (file-modes filename))))
;;;	  (error "%s is not safe.chmod 600 %s!" filename filename))
      (if (file-writable-p filename)
	  (progn
	    (write-region (point-min) (point-max)
			  filename nil 'no-msg)
	    (set-file-modes filename 384))
	(message "%s is not writable." filename)))))

(defun elmo-get-passwd (key)
  "Get password from password pool."
  (let (pair pass)
    (if (not elmo-passwd-alist)
	(setq elmo-passwd-alist (elmo-passwd-alist-load)))
    (setq pair (assoc key elmo-passwd-alist))
    (if pair
	(elmo-base64-decode-string (cdr pair))
      (setq pass (elmo-read-passwd (format "Password for %s: "
					   key) t))
      (setq elmo-passwd-alist
	    (append elmo-passwd-alist
		    (list (cons key
				(elmo-base64-encode-string pass)))))
      (if elmo-passwd-life-time
	  (run-with-timer elmo-passwd-life-time nil
			  `(lambda () (elmo-remove-passwd ,key))))
      pass)))

(defun elmo-remove-passwd (key)
  "Remove password from password pool (for failure)."
  (let (pass-cons)
    (while (setq pass-cons (assoc key elmo-passwd-alist))
      (unwind-protect
	  (fillarray (cdr pass-cons) 0)
	(setq elmo-passwd-alist
	      (delete pass-cons elmo-passwd-alist))))))

(defmacro elmo-read-char-exclusive ()
  (cond ((featurep 'xemacs)
	 '(let ((table (quote ((backspace . ?\C-h) (delete . ?\C-?)
			       (left . ?\C-h))))
		event key)
	    (while (not
		    (and
		     (key-press-event-p (setq event (next-command-event)))
		     (setq key (or (event-to-character event)
				   (cdr (assq (event-key event) table)))))))
	    key))
	((fboundp 'read-char-exclusive)
	 '(read-char-exclusive))
	(t
	 '(read-char))))

(defun elmo-read-passwd (prompt &optional stars)
  "Read a single line of text from user without echoing, and return it."
  (let ((ans "")
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t)
	(log-message-max-size 0)
	message-log-max	done msg truncate)
    (while (not done)
      (if (or (not stars) (string= "" ans))
	  (setq msg prompt)
	(setq msg (concat prompt (make-string (length ans) ?.)))
	(setq truncate
	      (1+ (- (length msg) (window-width (minibuffer-window)))))
	(and (> truncate 0)
	     (setq msg (concat "$" (substring msg (1+ truncate))))))
      (message "%s" msg)
      (setq c (elmo-read-char-exclusive))
      (cond ((= c ?\C-g)
	     (setq quit-flag t
		   done t))
	    ((or (= c ?\r) (= c ?\n) (= c ?\e))
	     (setq done t))
	    ((= c ?\C-u)
	     (setq ans ""))
	    ((and (/= c ?\b) (/= c ?\177))
	     (setq ans (concat ans (char-to-string c))))
	    ((> (length ans) 0)
	     (setq ans (substring ans 0 -1)))))
    (if quit-flag
	(prog1
	    (setq quit-flag nil)
	  (message "Quit")
	  (beep t))
      (message "")
      ans)))

(defun elmo-string-to-list (string)
  (read (concat "(" string ")")))

(defun elmo-list-to-string (list)
  (let ((tlist list)
	str)
    (if (listp tlist)
	(progn
	  (setq str "(")
	  (while (car tlist)
	    (setq str
		  (concat str
			  (if (symbolp (car tlist))
			      (symbol-name (car tlist))
			    (car tlist))))
	    (if (cdr tlist)
		(setq str
		      (concat str " ")))
	    (setq tlist (cdr tlist)))
	  (setq str
		(concat str ")")))
      (setq str
	    (if (symbolp tlist)
		(symbol-name tlist)
	      tlist)))
    str))


(defun elmo-plug-on-by-servers (alist &optional servers)
  (let ((server-list (or servers elmo-plug-on-servers)))
    (catch 'plugged
      (while server-list
	(if (elmo-plugged-p (car server-list))
	    (throw 'plugged t))
	(setq server-list (cdr server-list))))))

(defun elmo-plug-on-by-exclude-servers (alist &optional servers)
  (let ((server-list (or servers elmo-plug-on-exclude-servers))
	server other-servers)
    (while alist
      (when (and (not (member (setq server (caaar alist)) server-list))
		 (not (member server other-servers)))
	(push server other-servers))
      (setq alist (cdr alist)))
    (elmo-plug-on-by-servers alist other-servers)))

(defun elmo-plugged-p (&optional server port stream-type alist label-exp)
  (let ((alist (or alist elmo-plugged-alist))
	plugged-info)
    (cond ((and (not port) (not server))
	   (cond ((eq elmo-plugged-condition 'one)
		  (if alist
		      (catch 'plugged
			(while alist
			  (if (nth 2 (car alist))
			      (throw 'plugged t))
			  (setq alist (cdr alist))))
		    elmo-plugged))
		 ((eq elmo-plugged-condition 'all)
		  (if alist
		      (catch 'plugged
			(while alist
			  (if (not (nth 2 (car alist)))
			      (throw 'plugged nil))
			  (setq alist (cdr alist)))
			t)
		    elmo-plugged))
		 ((functionp elmo-plugged-condition)
		  (funcall elmo-plugged-condition alist))
		 (t ;; independent
		  elmo-plugged)))
	  ((not port) ;; server
	   (catch 'plugged
	     (while alist
	       (when (string= server (caaar alist))
		 (if (nth 2 (car alist))
		     (throw 'plugged t)))
	       (setq alist (cdr alist)))))
	  (t
	   (setq plugged-info (assoc (list server port stream-type) alist))
	   (if (not plugged-info)
	       ;; add elmo-plugged-alist automatically
	       (progn
		 (elmo-set-plugged elmo-plugged server port stream-type
				   nil nil nil label-exp)
		 elmo-plugged)
	     (if (and elmo-auto-change-plugged
		      (> elmo-auto-change-plugged 0)
		      (nth 3 plugged-info)  ;; time
		      (elmo-time-expire (nth 3 plugged-info)
					elmo-auto-change-plugged))
		 t
	       (nth 2 plugged-info)))))))

(defun elmo-set-plugged (plugged &optional server port stream-type time
				 alist label-exp add)
  (let ((alist (or alist elmo-plugged-alist))
	label plugged-info)
    (cond ((and (not port) (not server))
	   (setq elmo-plugged plugged)
	   ;; set plugged all element of elmo-plugged-alist.
	   (while alist
	     (setcdr (cdar alist) (list plugged time))
	     (setq alist (cdr alist))))
	  ((not port)
	   ;; set plugged all port of server
	   (while alist
	     (when (string= server (caaar alist))
	       (setcdr (cdar alist) (list plugged time)))
	     (setq alist (cdr alist))))
	  (t
	   ;; set plugged one port of server
	   (setq plugged-info (assoc (list server port stream-type) alist))
	   (setq label (if label-exp
			   (eval label-exp)
			 (nth 1 plugged-info)))
	   (if plugged-info
	       ;; if add is non-nil, don't reset plug state.
	       (unless add
		 (setcdr plugged-info (list label plugged time)))
	     (setq alist
		   (setq elmo-plugged-alist
			 (nconc
			  elmo-plugged-alist
			  (list
			   (list (list server port stream-type)
				 label plugged time))))))))
    alist))

(defun elmo-delete-plugged (&optional server port alist)
  (let* ((alist (or alist elmo-plugged-alist))
	 (alist2 alist))
    (cond ((and (not port) (not server))
	   (setq alist nil))
	  ((not port)
	   ;; delete plugged all port of server
	   (while alist2
	     (when (string= server (caaar alist2))
	       (setq alist (delete (car alist2) alist)))
	     (setq alist2 (cdr alist2))))
	  (t
	   ;; delete plugged one port of server
	   (setq alist
		 (delete (assoc (cons server port) alist) alist))))
    alist))

(defun elmo-disk-usage (path)
  "Get disk usage (bytes) in PATH."
  (let ((file-attr
	 (condition-case () (file-attributes path) (error nil))))
    (if file-attr
	(if (nth 0 file-attr) ; directory
	    (let ((files (condition-case ()
			     (directory-files path t "^[^\\.]")
			   (error nil)))
		  (result 0.0))
;;;	      (result (nth 7 file-attr))) ; ... directory size
	      (while files
		(setq result (+ result (or (elmo-disk-usage (car files)) 0)))
		(setq files (cdr files)))
	      result)
	  (float (nth 7 file-attr)))
      0)))

(defun elmo-get-last-accessed-time (path &optional dir)
  "Return the last accessed time of PATH."
  (let ((last-accessed (nth 4 (file-attributes (or (and dir
							(expand-file-name
							 path dir))
						   path)))))
    (if last-accessed
	(setq last-accessed (+ (* (nth 0 last-accessed)
				  (float 65536)) (nth 1 last-accessed)))
      0)))

(defun elmo-get-last-modification-time (path &optional dir)
  "Return the last accessed time of PATH."
  (let ((last-modified (nth 5 (file-attributes (or (and dir
							(expand-file-name
							 path dir))
						   path)))))
    (setq last-modified (+ (* (nth 0 last-modified)
			      (float 65536)) (nth 1 last-modified)))))

(defun elmo-make-directory (path &optional mode)
  "Create directory recursively."
  (let ((parent (directory-file-name (file-name-directory path))))
    (if (null (file-directory-p parent))
	(elmo-make-directory parent))
    (make-directory path)
    (set-file-modes path (or mode
			     (+ (* 64 7) (* 8 0) 0))))) ; chmod 0700

(defun elmo-delete-directory (path &optional no-hierarchy)
  "Delete directory recursively."
  (if (stringp path) ; nil is not permitted.
  (let ((dirent (directory-files path))
	relpath abspath hierarchy)
    (while dirent
      (setq relpath (car dirent)
	    dirent (cdr dirent)
	    abspath (expand-file-name relpath path))
      (when (not (string-match "^\\.\\.?$" relpath))
	(if (eq (nth 0 (file-attributes abspath)) t)
	    (if no-hierarchy
		(setq hierarchy t)
	      (elmo-delete-directory abspath no-hierarchy))
	  (delete-file abspath))))
    (unless hierarchy
      (delete-directory path)))))

(defun elmo-delete-match-files (path regexp &optional remove-if-empty)
  "Delete directory files specified by PATH.
If optional REMOVE-IF-EMPTY is non-nil, delete directory itself if
the directory becomes empty after deletion."
  (when (stringp path) ; nil is not permitted.
    (dolist (file (directory-files path t regexp))
      (delete-file file))
    (if remove-if-empty
	(ignore-errors
	  (delete-directory path) ; should be removed if empty.
	  ))))

(defun elmo-list-filter (l1 l2)
  "Return a list from L2 in which each element is a member of L1."
  (let (result)
    (dolist (element l2)
      (if (memq element l1)
	(setq result (cons element result))))
    (nreverse result)))

(defsubst elmo-list-delete-if-smaller (list number)
  (let ((ret-val (copy-sequence list)))
    (while list
      (if (< (car list) number)
	  (setq ret-val (delq (car list) ret-val)))
      (setq list (cdr list)))
    ret-val))

(defun elmo-list-diff (list1 list2)
  (let ((clist1 (sort (copy-sequence list1) #'<))
	(clist2 (sort (copy-sequence list2) #'<))
	list1-only list2-only)
    (while (and clist1 clist2)
      (cond ((= (car clist1) (car clist2))
	     (setq clist1 (cdr clist1)
		   clist2 (cdr clist2)))
	    ((car-less-than-car clist1 clist2)
	     (setq list1-only (cons (car clist1) list1-only)
		   clist1 (cdr clist1)))
	    (t ;; (car-less-than-car clist2 clist1)
	     (setq list2-only (cons (car clist2) list2-only)
		   clist2 (cdr clist2)))))
    ;; Keep sorted orders.
    (list (nconc (nreverse list1-only) clist1)
	  (nconc (nreverse list2-only) clist2))))

(defun elmo-list-diff-nonsortable (list1 list2)
  (let ((clist1 (copy-sequence list1))
	(clist2 (copy-sequence list2)))
    (while list2
      (setq clist1 (delq (car list2) clist1))
      (setq list2 (cdr list2)))
    (while list1
      (setq clist2 (delq (car list1) clist2))
      (setq list1 (cdr list1)))
    (list clist1 clist2)))

(defmacro elmo-get-hash-val (string hashtable)
  (static-if (fboundp 'unintern)
      `(symbol-value (intern-soft ,string ,hashtable))
    `(let ((sym (intern-soft ,string ,hashtable)))
       (and (boundp sym)
	    (symbol-value sym)))))

(defmacro elmo-set-hash-val (string value hashtable)
  `(set (intern ,string ,hashtable) ,value))

(defmacro elmo-clear-hash-val (string hashtable)
  (static-if (fboundp 'unintern)
      (list 'unintern string hashtable)
    (list 'makunbound (list 'intern string hashtable))))

(defmacro elmo-unintern (string)
  "`unintern' symbol named STRING,  When can use `unintern'.
Emacs 19.28 or earlier does not have `unintern'."
  (static-if (fboundp 'unintern)
      (list 'unintern string)))

(defun elmo-make-hash (&optional hashsize)
  "Make a new hash table which have HASHSIZE size."
  (make-vector
   (if hashsize
       (max
	;; Prime numbers as lengths tend to result in good
	;; hashing; lengths one less than a power of two are
	;; also good.
	(min
	 (let ((i 1))
	   (while (< (- i 1) hashsize)
	     (setq i (* 2 i)))
	   (- i 1))
	 elmo-hash-maximum-size)
	elmo-hash-minimum-size)
     elmo-hash-minimum-size)
   0))

(defsubst elmo-mime-string (string)
  "Normalize MIME encoded STRING."
  (and string
       (elmo-mime-charset-encode-string
	(or (ignore-errors
	      (eword-decode-and-unfold-unstructured-field-body string))
	    string)
	elmo-mime-charset)))

(defsubst elmo-collect-field (beg end downcase-field-name)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((regexp (concat "\\(" std11-field-head-regexp "\\)[ \t]*"))
	    dest name body)
	(while (re-search-forward regexp nil t)
	  (setq name (buffer-substring-no-properties
		      (match-beginning 1)(1- (match-end 1))))
	  (if downcase-field-name
	      (setq name (downcase name)))
	  (setq body (buffer-substring-no-properties
		      (match-end 0) (std11-field-end)))
	  (or (assoc name dest)
	      (setq dest (cons (cons name body) dest))))
	dest))))

(defsubst elmo-collect-field-from-string (string downcase-field-name)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((regexp (concat "\\(" std11-field-head-regexp "\\)[ \t]*"))
	  dest name body)
      (while (re-search-forward regexp nil t)
	(setq name (buffer-substring-no-properties
		    (match-beginning 1)(1- (match-end 1))))
	(if downcase-field-name
	    (setq name (downcase name)))
	(setq body (buffer-substring-no-properties
		    (match-end 0) (std11-field-end)))
	(or (assoc name dest)
	    (setq dest (cons (cons name body) dest))))
      dest)))

(defun elmo-safe-filename (filename)
  (let* ((replace-alist '(("/" . " ")
			  (":" . "__")
			  ("|" . "_or_")
			  ("\"" . "_Q_")))
	 (regexp (concat "["
			 (regexp-quote (mapconcat 'car replace-alist ""))
			 "]"))
	 (rest filename)
	 converted)
    (while (string-match regexp rest)
      (setq converted (concat converted
			      (substring rest 0 (match-beginning 0))
			      (cdr (assoc (substring rest
						     (match-beginning 0)
						     (match-end 0))
					  replace-alist)))
	    rest (substring rest (match-end 0))))
    (concat converted rest)))

(defvar elmo-filename-replace-chars nil)

(defsubst elmo-replace-string-as-filename (msgid)
  "Replace string as filename."
  (setq msgid (elmo-replace-in-string msgid " " "  "))
  (if (null elmo-filename-replace-chars)
      (setq elmo-filename-replace-chars
	    (regexp-quote (mapconcat
			   'car elmo-filename-replace-string-alist ""))))
  (while (string-match (concat "[" elmo-filename-replace-chars "]")
		       msgid)
    (setq msgid (concat
		 (substring msgid 0 (match-beginning 0))
		 (cdr (assoc
		       (substring msgid
				  (match-beginning 0) (match-end 0))
		       elmo-filename-replace-string-alist))
		 (substring msgid (match-end 0)))))
  msgid)

(defsubst elmo-recover-string-from-filename (filename)
  "Recover string from FILENAME."
  (let (tmp result)
    (while (string-match " " filename)
      (setq tmp (substring filename
			   (match-beginning 0)
			   (+ (match-end 0) 1)))
      (if (string= tmp "  ")
	  (setq tmp " ")
	(setq tmp (car (rassoc tmp
			       elmo-filename-replace-string-alist))))
      (setq result
	    (concat result
		    (substring filename 0 (match-beginning 0))
		    tmp))
      (setq filename (substring filename (+ (match-end 0) 1))))
    (concat result filename)))

(defsubst elmo-copy-file (src dst &optional ok-if-already-exists)
  (condition-case err
      (elmo-add-name-to-file src dst ok-if-already-exists)
    (error (copy-file src dst ok-if-already-exists t))))

(defsubst elmo-buffer-exists-p (buffer)
  (if (bufferp buffer)
      (buffer-live-p buffer)
    (get-buffer buffer)))

(defsubst elmo-kill-buffer (buffer)
  (when (elmo-buffer-exists-p buffer)
    (kill-buffer buffer)))

(defun elmo-delete-if (pred lst)
  "Return new list contain items which don't satisfy PRED in LST."
  (let (result)
    (while lst
      (unless (funcall pred (car lst))
	(setq result (cons (car lst) result)))
      (setq lst (cdr lst)))
    (nreverse result)))

(defun elmo-list-delete (list1 list2 &optional delete-function)
  "Delete by side effect any occurrences equal to elements of LIST1 from LIST2.
Return the modified LIST2.  Deletion is done with `delete'.
Write `(setq foo (elmo-list-delete bar foo))' to be sure of changing
the value of `foo'.
If optional DELETE-FUNCTION is speficied, it is used as delete procedure."
  (setq delete-function (or delete-function 'delete))
  (while list1
    (setq list2 (funcall delete-function (car list1) list2))
    (setq list1 (cdr list1)))
  list2)

(defun elmo-list-member (list1 list2)
  "If any element of LIST1 is member of LIST2, return t."
  (catch 'done
    (while list1
      (if (member (car list1) list2)
	  (throw 'done t))
      (setq list1 (cdr list1)))))

(defun elmo-count-matches (regexp beg end)
  (let ((count 0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
	(setq count (1+ count)))
      count)))

(if (fboundp 'display-error)
    (defalias 'elmo-display-error 'display-error)
  (defun elmo-display-error (error-object stream)
    "A tiny function to display ERROR-OBJECT to the STREAM."
    (let ((first t)
	  (errobj error-object)
	  err-mes)
      (while errobj
	(setq err-mes (concat err-mes (format
				       (if (stringp (car errobj))
					   "%s"
					 "%S")
				       (car errobj))))
	(setq errobj (cdr errobj))
	(if errobj (setq err-mes (concat err-mes (if first ": " ", "))))
	(setq first nil))
      (princ err-mes stream))))

(if (fboundp 'define-error)
    (defalias 'elmo-define-error 'define-error)
  (defun elmo-define-error (error doc &optional parents)
    (or parents
	(setq parents 'error))
    (let ((conds (get parents 'error-conditions)))
      (or conds
	  (error "Not an error symbol: %s" error))
      (setplist error
		(list 'error-message doc
		      'error-conditions (cons error conds))))))

(defvar elmo-progress-counter nil)

(defalias 'elmo-progress-counter-label 'car-safe)

(defmacro elmo-progress-counter-value (counter)
  `(aref (cdr ,counter) 0))

(defmacro elmo-progress-counter-set-value (counter value)
  `(aset (cdr ,counter) 0 ,value))

(defmacro elmo-progress-counter-total (counter)
  `(aref (cdr ,counter) 1))

(defmacro elmo-progress-counter-set-total (counter value)
  `(aset (cdr ,counter) 1 ,value))

(defmacro elmo-progress-counter-action (counter)
  `(aref (cdr ,counter) 2))

(defmacro elmo-progress-counter-set-action (counter action)
  `(aset (cdr ,counter) 2, action))

(defvar elmo-progress-callback-function nil)

(defun elmo-progress-call-callback (counter &optional value)
  (when elmo-progress-callback-function
    (funcall elmo-progress-callback-function
	     (elmo-progress-counter-label counter)
	     (elmo-progress-counter-action counter)
	     (or value
		 (elmo-progress-counter-value counter))
	     (elmo-progress-counter-total counter))))

(defun elmo-progress-start (label total action)
  (when (and (null elmo-progress-counter)
	     (or (null total)
		 (> total 0)))
    (let ((counter (cons label (vector 0 total action))))
      (elmo-progress-call-callback counter 'start)
      (setq elmo-progress-counter
	    (cond ((null total)
		   counter)
		  ((elmo-progress-call-callback counter 'query)
		   (elmo-progress-call-callback counter)
		   counter)
		  (t
		   t)))
      counter)))

(defun elmo-progress-clear (counter)
  (when counter
    (when (and (elmo-progress-counter-label elmo-progress-counter)
	       (elmo-progress-counter-total elmo-progress-counter))
      (elmo-progress-call-callback elmo-progress-counter 100))
    (setq elmo-progress-counter nil)))

(defun elmo-progress-done (counter)
  (when (elmo-progress-counter-label counter)
    (elmo-progress-call-callback counter 'done)))

(defun elmo-progress-notify (label &rest params)
  (when (eq label (elmo-progress-counter-label elmo-progress-counter))
    (let ((counter elmo-progress-counter))
      (if (or (elmo-progress-counter-total counter)
	      (and (elmo-progress-counter-set-total
		    counter
		    (elmo-safe-plist-get params :total))
		   (elmo-progress-call-callback counter 'query)))
	  (progn
	    (elmo-progress-counter-set-value
	     counter
	     (or (elmo-safe-plist-get params :set)
		 (+ (elmo-progress-counter-value counter)
		    (or (elmo-safe-plist-get params :inc)
			(car params)
			1))))
	    (elmo-progress-call-callback counter))
	(setq elmo-progress-counter t)))))

(defmacro elmo-with-progress-display (spec message &rest body)
  "Evaluate BODY with progress message and return its value.
SPEC is a list as followed (LABEL TOTAL [VAR]).
LABEL is an identifier what is specidied by `elmo-progress-notify'.
If TOTAL is nil, the first `elmo-progress-notify' call must be
with a `:total' parameter.
If optional parameter VAR is specified, bind it with a progress counter object.
MESSAGE is a doing part of progress message."
  (let ((label (nth 0 spec))
	(total (nth 1 spec))
	(var (or (nth 2 spec) (make-symbol "--elmo-progress-temp--"))))
    `(let ((,var (elmo-progress-start (quote ,label) ,total ,message)))
       (prog1
	   (unwind-protect
	       (progn
		 ,@body)
	     (elmo-progress-clear ,var))
	 (elmo-progress-done ,var)))))

(put 'elmo-with-progress-display 'lisp-indent-function '2)
(def-edebug-spec elmo-with-progress-display
  ((symbolp form &optional symbolp) form &rest form))

(defun elmo-time-expire (before-time diff-time)
  (let* ((current (current-time))
	 (rest (when (< (nth 1 current) (nth 1 before-time))
		 (expt 2 16)))
	 diff)
    (setq diff
	  (list (- (+ (car current) (if rest -1 0)) (car before-time))
		(- (+ (or rest 0) (nth 1 current)) (nth 1 before-time))))
    (and (zerop (car diff))
	 (< diff-time (nth 1 diff)))))

(defalias 'elmo-field-body 'std11-fetch-field) ;;no narrow-to-region

(defun elmo-unfold-fetch-field (name)
  (let ((value (std11-fetch-field name)))
    (and value
	 (std11-unfold-string value))))

;; 2012-07-30
(make-obsolete 'elmo-unfold-field-body 'elmo-unfold-fetch-field)

(defun elmo-decoded-fetch-field (field-name &optional mode)
  (let ((field-body (std11-fetch-field field-name)))
    (and field-body
	 (or (ignore-errors
	      (elmo-with-enable-multibyte
		(mime-decode-field-body field-body field-name mode)))
	     field-body))))

;; 2012-07-30
(make-obsolete 'elmo-decoded-field-body 'elmo-decoded-fetch-field)

(defun elmo-address-quote-specials (word)
  "Make quoted string of WORD if needed."
  (let ((lal (std11-lexical-analyze word)))
    (if (or (assq 'specials lal)
	    (assq 'domain-literal lal))
	(prin1-to-string word)
      word)))

(static-cond
 ((fboundp 'substring-no-properties)
  (defalias 'elmo-string 'substring-no-properties))
 (t
  (defmacro elmo-string (string &optional from to)
    "Return a substring of STRING, without text properties.
It starts at zero-indexed index FROM and ends before TO."
    `(let ((obj (substring ,string (or ,from 0) ,to)))
       (set-text-properties 0 (length obj) nil obj)
       obj))))

(defun elmo-flatten (list-of-list)
  "Flatten LIST-OF-LIST."
  (and list-of-list
       (apply #'append
	      (mapcar (lambda (element)
			(if (listp element) element (list element)))
		      list-of-list))))

(defun elmo-y-or-n-p (prompt &optional auto default)
  "Same as `y-or-n-p'.
But if optional argument AUTO is non-nil, DEFAULT is returned."
  (if auto
      default
    (y-or-n-p prompt)))

(defun elmo-string-member (string slist)
  (catch 'found
    (dolist (element slist)
      (cond ((null element))
	    ((stringp element)
	     (when (string= string element)
	       (throw 'found t)))
	    ((symbolp element)
	     (when (string= string (symbol-value element))
	       (throw 'found t)))))))

(static-cond ((fboundp 'member-ignore-case)
       (defalias 'elmo-string-member-ignore-case 'member-ignore-case))
      ((fboundp 'compare-strings)
       (defun elmo-string-member-ignore-case (elt list)
	 "Like `member', but ignores differences in case and text representation.
ELT must be a string.  Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
	 (while (and list (not (eq t (compare-strings elt 0 nil (car list) 0 nil t))))
	   (setq list (cdr list)))
	 list))
      (t
       (defun elmo-string-member-ignore-case (elt list)
	 "Like `member', but ignores differences in case and text representation.
ELT must be a string.  Upper-case and lower-case letters are treated as equal."
	 (let ((str (downcase elt)))
	   (while (and list (not (string= str (downcase (car list)))))
	     (setq list (cdr list)))
	   list))))

(defun elmo-string-match-member (str list &optional case-ignore)
  (let ((case-fold-search case-ignore))
    (catch 'member
      (while list
	(if (string-match (car list) str)
	    (throw 'member (car list)))
	(setq list (cdr list))))))

(defun elmo-string-matched-member (str list &optional case-ignore)
  (let ((case-fold-search case-ignore))
    (catch 'member
      (while list
	(if (string-match str (car list))
	    (throw 'member (car list)))
	(setq list (cdr list))))))

(defsubst elmo-string-delete-match (string pos)
  (concat (substring string
		     0 (match-beginning pos))
	  (substring string
		     (match-end pos)
		     (length string))))

(defun elmo-string-match-assoc (key alist &optional case-ignore)
  (let ((case-fold-search case-ignore)
	a)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (car a))
		 (string-match key (car a)))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun elmo-string-matched-assoc (key alist &optional case-ignore)
  (let ((case-fold-search case-ignore)
	a)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (car a))
		 (string-match (car a) key))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun elmo-string-assoc (key alist)
  (let (a)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (car a))
		 (string= key (car a)))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun elmo-string-assoc-all (key alist)
  (let (matches)
    (while alist
      (if (string= key (car (car alist)))
	  (setq matches
		(cons (car alist)
		      matches)))
      (setq alist (cdr alist)))
    matches))

(defun elmo-string-rassoc (key alist)
  (let (a)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(if (and (consp a)
		 (stringp (cdr a))
		 (string= key (cdr a)))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun elmo-string-rassoc-all (key alist)
  (let (matches)
    (while alist
      (if (string= key (cdr (car alist)))
	  (setq matches
		(cons (car alist)
		      matches)))
      (setq alist (cdr alist)))
    matches))

(defun elmo-expand-newtext (newtext original)
  (let ((len (length newtext))
	(pos 0)
	c expanded beg N did-expand)
    (while (< pos len)
      (setq beg pos)
      (while (and (< pos len)
		  (not (= (aref newtext pos) ?\\)))
	(setq pos (1+ pos)))
      (unless (= beg pos)
	(push (substring newtext beg pos) expanded))
      (when (< pos len)
	;; We hit a \; expand it.
	(setq did-expand t
	      pos (1+ pos)
	      c (aref newtext pos))
	(if (not (or (= c ?\&)
		     (and (>= c ?1)
			  (<= c ?9))))
	    ;; \ followed by some character we don't expand.
	    (push (char-to-string c) expanded)
	  ;; \& or \N
	  (if (= c ?\&)
	      (setq N 0)
	    (setq N (- c ?0)))
	  (when (match-beginning N)
	    (push (substring original (match-beginning N) (match-end N))
		  expanded))))
      (setq pos (1+ pos)))
    (if did-expand
	(apply (function concat) (nreverse expanded))
      newtext)))

;;; Folder parser utils.
(defconst elmo-quoted-specials-list '(?\\ ?\"))

(defun elmo-quoted-token (string)
  (concat "\""
	  (std11-wrap-as-quoted-pairs string elmo-quoted-specials-list)
	  "\""))

(defun elmo-token-valid-p (token requirement)
  (cond ((null requirement))
	((stringp requirement)
	 (string-match requirement token))
	((functionp requirement)
	 (funcall requirement token))))

(defun elmo-parse-token (string &optional seps requirement asis)
  "Parse atom from STRING using SEPS as a string of separator char list.
When optional argument ASIS is non-nil, keep '\\' and '\"' from result."
  (let ((len (length string))
	(seps (and seps (string-to-char-list seps)))
	(i 0)
	(sep nil)
	content c in)
    (if (zerop len)
	(cons "" "")
      (while (and (< i len) (or in (null sep)))
	(setq c (aref string i))
	(cond
	 ((and in (eq c ?\\))
	  (when asis
	    (setq content (cons c content)))
	  (setq i (1+ i)
		content (cons (aref string i) content)
		i (1+ i)))
	 ((eq c ?\")
	  (when asis
	    (setq content (cons c content)))
	  (setq in (not in)
		i (1+ i)))
	 (in (setq content (cons c content)
		   i (1+ i)))
	 ((memq c seps)
	  (setq sep c))
	 (t (setq content (cons c content)
		  i (1+ i)))))
      (if in (error "Parse error in quoted"))
      (let ((atom (if (null content)
		      ""
		    (char-list-to-string (nreverse content)))))
	(if (elmo-token-valid-p atom requirement)
	    (cons atom (substring string i))
	  (cons "" string))))))

(defun elmo-parse-prefixed-element (prefix string &optional seps requirement)
  (let (parsed)
    (if (and (not (zerop (length string)))
	     (eq (aref string 0) prefix)
	     (setq parsed (elmo-parse-token (substring string 1) seps))
	     (elmo-token-valid-p (car parsed) requirement))
	parsed
      (cons "" string))))

(defun elmo-collect-separators (spec)
  (when (listp spec)
    (let ((result (elmo-collect-separators-internal spec)))
      (and result
	   (char-list-to-string (elmo-uniq-list result #'delq))))))

(defun elmo-collect-separators-internal (specs &optional separators)
  (while specs
    (let ((spec (car specs)))
      (cond
       ((listp spec)
	(setq separators (elmo-collect-separators-internal spec separators)
	      specs (cdr specs)))
       ((characterp spec)
	(setq separators (cons spec separators)
	      specs nil))
       (t
	(setq specs nil)))))
  separators)

(defun elmo-collect-trail-separators (element specs)
  (cond
   ((symbolp specs)
    (eq specs element))
   ((vectorp specs)
    (eq (aref specs 0) element))
   ((listp specs)
    (let (spec result)
      (while (setq spec (car specs))
	(if (setq result (elmo-collect-trail-separators element spec))
	    (setq result (concat (if (stringp result) result)
				 (elmo-collect-separators (cdr specs)))
		  specs nil)
	  (setq specs (cdr specs))))
      result))))

(defun elmo-parse-separated-tokens (string spec)
  (let ((result (elmo-parse-separated-tokens-internal string spec)))
    (if (eq (car result) t)
	(cons nil (cdr result))
      result)))

(defun elmo-parse-separated-tokens-internal (string spec &optional separators)
  (cond
   ((symbolp spec)
    (let ((parse (elmo-parse-token string separators)))
      (cons (list (cons spec (car parse))) (cdr parse))))
   ((vectorp spec)
    (let ((parse (elmo-parse-token string separators)))
      (if (elmo-token-valid-p (car parse) (aref spec 1))
	  (cons (list (cons (aref spec 0) (car parse))) (cdr parse))
	(cons nil string))))
   ((characterp spec)
    (if (and (> (length string) 0)
	     (eq (aref string 0) spec))
	(cons t (substring string 1))
      (cons nil string)))
   ((listp spec)
    (catch 'unmatch
      (let ((rest string)
	    result tokens)
	(while spec
	  (setq result (elmo-parse-separated-tokens-internal
			rest
			(car spec)
			(concat (elmo-collect-separators (cdr spec))
				separators)))
	  (cond ((null (car result))
		 (throw 'unmatch (cons t string)))
		((eq t (car result)))
		(t
		 (setq tokens (nconc (car result) tokens))))
	  (setq rest (cdr result)
		spec (cdr spec)))
	(cons (or tokens t) rest))))))

(defun elmo-quote-syntactical-element (value element syntax)
  (let ((separators (elmo-collect-trail-separators element syntax)))
    (if (and separators
	     (string-match (concat "[" separators "]") value))
	(elmo-quoted-token value)
      value)))

;;; Number set defined by OKAZAKI Tetsurou <okazaki@be.to>
;;
;; number          ::= [0-9]+
;; beg             ::= number
;; end             ::= number
;; number-range    ::= "(" beg " . " end ")"      ;; cons cell
;; number-set-elem ::= number / number-range
;; number-set      ::= "(" *number-set-elem ")"   ;; list

(defun elmo-number-set-member (number number-set)
  "Return non-nil if NUMBER is an element of NUMBER-SET.
The value is actually the tail of NUMBER-RANGE whose car contains NUMBER."
  (or (memq number number-set)
      (let (found)
	(while (and number-set (not found))
	  (if (and (consp (car number-set))
		   (and (<= (car (car number-set)) number)
			(<= number (cdr (car number-set)))))
	      (setq found t)
	    (setq number-set (cdr number-set))))
	number-set)))

(defun elmo-number-set-append-list (number-set list)
  "Append LIST of numbers to the NUMBER-SET.
NUMBER-SET is altered."
  (let ((appended number-set))
    (while list
      (setq appended (elmo-number-set-append appended (car list)))
      (setq list (cdr list)))
    appended))

(defun elmo-number-set-append (number-set number)
  "Append NUMBER to the NUMBER-SET.
NUMBER-SET is altered."
  (let ((number-set-1 number-set)
	found elem)
    (while (and number-set (not found))
      (setq elem (car number-set))
      (cond
       ((and (consp elem)
	     (eq (+ 1 (cdr elem)) number))
	(setcdr elem number)
	(setq found t))
       ((and (integerp elem)
	     (eq (+ 1 elem) number))
	(setcar number-set (cons elem number))
	(setq found t))
       ((or (and (integerp elem) (eq elem number))
	    (and (consp elem)
		 (<= (car elem) number)
		 (<= number (cdr elem))))
	(setq found t)))
      (setq number-set (cdr number-set)))
    (if (not found)
	(setq number-set-1 (nconc number-set-1 (list number))))
    number-set-1))

(defun elmo-number-set-delete-list (number-set list)
  "Delete LIST of numbers from the NUMBER-SET.
NUMBER-SET is altered."
  (let ((deleted number-set))
    (dolist (number list)
      (setq deleted (elmo-number-set-delete deleted number)))
    deleted))

(defun elmo-number-set-delete (number-set number)
  "Delete NUMBER from the NUMBER-SET.
NUMBER-SET is altered."
  (let* ((curr number-set)
	 (top (cons 'dummy number-set))
	 (prev top)
	 elem found)
    (while (and curr (not found))
      (setq elem (car curr))
      (if (consp elem)
	  (cond
	   ((eq (car elem) number)
	    (if (eq (cdr elem) (1+ number))
		(setcar curr (cdr elem))
	      (setcar elem (1+ number)))
	    (setq found t))
	   ((eq (cdr elem) number)
	    (if (eq (car elem) (1- number))
		(setcar curr (car elem))
	      (setcdr elem (1- number)))
	    (setq found t))
	   ((and (> number (car elem))
		 (< number (cdr elem)))
	    (setcdr
	     prev
	     (nconc
	      (list
;;;	       (beg . (1- number))
	       (let ((new (cons (car elem) (1- number))))
		 (if (eq (car new) (cdr new))
		     (car new)
		   new))
;;;	       ((1+ number) . end)
	       (let ((new (cons (1+ number) (cdr elem))))
		 (if (eq (car new) (cdr new))
		     (car new)
		   new)))
	      (cdr curr)))))
	(when (eq elem number)
	  (setcdr prev (cdr curr))
	  (setq found t)))
      (setq prev curr
	    curr (cdr curr)))
    (cdr top)))

(defun elmo-make-number-list (beg end)
  (let (number-list i)
    (setq i end)
    (while (>= i beg)
      (setq number-list (cons i number-list))
      (setq i (1- i)))
    number-list))

(defun elmo-number-set-to-number-list (number-set)
  "Return a number list which corresponds to NUMBER-SET."
  (let ((number-list (list 'dummy))
	elem)
    (while number-set
      (setq elem (car number-set))
      (cond
       ((consp elem)
	(nconc number-list (elmo-make-number-list (car elem) (cdr elem))))
       ((integerp elem)
	(nconc number-list (list elem))))
      (setq number-set (cdr number-set)))
    (cdr number-list)))

(defcustom elmo-list-subdirectories-ignore-regexp "^\\(\\.\\.?\\|[0-9]+\\)$"
  "*Regexp to filter subfolders."
  :type 'regexp
  :group 'elmo)

(defun elmo-list-subdirectories-1 (basedir curdir one-level)
  (let ((root (zerop (length curdir)))
	(w32-get-true-file-link-count t) ; for Meadow
	attr dirs dir)
    (catch 'done
      (dolist (file (directory-files (setq dir (expand-file-name curdir basedir))))
	(when (and (not (string-match
			 elmo-list-subdirectories-ignore-regexp
			 file))
		   (car (setq attr (file-attributes
				    (expand-file-name file dir)))))
	  (when (eq one-level 'check) (throw 'done t))
	  (let ((relpath
		 (concat curdir (and (not root) elmo-path-sep) file))
		subdirs)
	    (setq dirs (nconc dirs
			      (if (if elmo-have-link-count (< 2 (nth 1 attr))
				    (setq subdirs
					  (elmo-list-subdirectories-1
					   basedir
					   relpath
					   (if one-level 'check))))
				  (if one-level
				      (list (list relpath))
				    (cons relpath
					  (or subdirs
					      (elmo-list-subdirectories-1
					       basedir
					       relpath
					       nil))))
				(list relpath)))))))
      dirs)))

(defun elmo-list-subdirectories (directory file one-level)
  (let ((subdirs (elmo-list-subdirectories-1 directory file one-level)))
    (if (zerop (length file))
	subdirs
      (cons file subdirs))))

(defun elmo-mapcar-list-of-list (func list-of-list)
  (mapcar
   (lambda (x)
     (cond ((listp x) (elmo-mapcar-list-of-list func x))
	   (t (funcall func x))))
   list-of-list))

(defun elmo-map-recursive (function object)
  (if (consp object)
      (let* ((prev (list 'dummy))
	     (result prev))
	(while (consp object)
	  (setq prev (setcdr prev (list (elmo-map-recursive function
							    (car object))))
		object (cdr object)))
	(when object
	  (setcdr prev (funcall function object)))
	(cdr result))
    (funcall function object)))

(defun elmo-map-until-success (function sequence)
  (let (result)
    (while (and (null result)
		sequence)
      (setq result (funcall function (car sequence))
	    sequence (cdr sequence)))
    result))

(defun elmo-string-match-substring (regexp string &optional matchn)
  (when (string-match regexp string)
    (match-string (or matchn 1) string)))

(defun elmo-parse (string regexp &optional matchn)
  (or matchn (setq matchn 1))
  (let (list)
    (store-match-data nil)
    (while (string-match regexp string (match-end 0))
      (setq list (cons (substring string (match-beginning matchn)
				  (match-end matchn)) list)))
    (nreverse list)))

(defun elmo-find-list-match-value (specs getter)
  (lexical-let ((getter getter))
    (elmo-map-until-success
     (lambda (spec)
       (cond
	((symbolp spec)
	 (funcall getter spec))
	((consp spec)
	 (lexical-let ((value (funcall getter (car spec))))
	   (when value
	     (elmo-map-until-success
	      (lambda (rule)
		(cond
		 ((stringp rule)
		  (elmo-string-match-substring rule value))
		 ((consp rule)
		  (elmo-string-match-substring (car rule) value (cdr rule)))))
	      (cdr spec)))))))
     specs)))

;;; File cache.
(defmacro elmo-make-file-cache (path status)
  "PATH is the cache file name.
STATUS is one of 'section, 'entire or nil.
 nil means no cache exists.
'section means partial section cache exists.
'entire means entire cache exists.
If the cache is partial file-cache, TYPE is 'partial."
  `(cons ,path ,status))

(defmacro elmo-file-cache-path (file-cache)
  "Returns the file path of the FILE-CACHE."
  `(car ,file-cache))

(defmacro elmo-file-cache-status (file-cache)
  "Returns the status of the FILE-CACHE."
  `(cdr ,file-cache))

(defsubst elmo-cache-to-msgid (filename)
  (concat "<" (elmo-recover-string-from-filename filename) ">"))

(defsubst elmo-cache-get-path-subr (msgid)
  (format "%02X" (logand (apply #'+ (string-to-list msgid)) 31)))

;;;
(defun elmo-file-cache-get-path (msgid &optional section)
  "Get cache path for MSGID.
If optional argument SECTION is specified, partial cache path is returned."
  (if (setq msgid (elmo-msgid-to-cache msgid))
      (expand-file-name
       (if section
	   (format "%s/%s/%s/%s"
		   elmo-cache-directory
		   (elmo-cache-get-path-subr msgid)
		   msgid
		   section)
	 (format "%s/%s/%s"
		 elmo-cache-directory
		 (elmo-cache-get-path-subr msgid)
		 msgid)))))

(defmacro elmo-file-cache-expand-path (path section)
  "Return file name for the file-cache corresponds to the section.
PATH is the file-cache path.
SECTION is the section string."
  `(expand-file-name (or ,section "") ,path))

(defun elmo-file-cache-delete (path)
  "Delete a cache on PATH."
  (when (file-exists-p path)
    (if (file-directory-p path)
	(progn
	  (dolist (file (directory-files path t "^[^\\.]"))
	    (delete-file file))
	  (delete-directory path))
      (delete-file path))
    t))

(defun elmo-file-cache-exists-p (msgid)
  "Returns 'section or 'entire if a cache which corresponds to MSGID exists."
  (elmo-file-cache-status (elmo-file-cache-get msgid)))

(defun elmo-file-cache-save (cache-path section)
  "Save current buffer as cache on PATH.
Return t if cache is saved successfully."
  (condition-case nil
      (let ((path (if section (expand-file-name section cache-path)
		    cache-path))
	    files dir)
	(if (and (null section)
		 (file-directory-p path))
	    (progn
	      (setq files (directory-files path t "^[^\\.]"))
	      (while files
		(delete-file (car files))
		(setq files (cdr files)))
	      (delete-directory path))
	  (if (and section
		   (not (file-directory-p cache-path)))
	      (delete-file cache-path)))
	(when path
	  (setq dir (directory-file-name (file-name-directory path)))
	  (if (not (file-exists-p dir))
	      (elmo-make-directory dir))
	  (write-region-as-binary (point-min) (point-max)
				  path nil 'no-msg)
	  t))
    ;; ignore error
    (error)))

(defun elmo-file-cache-load (cache-path section)
  "Load cache on PATH into the current buffer.
Return t if cache is loaded successfully."
  (condition-case nil
      (let (cache-file)
	(when (and cache-path
		   (if (elmo-cache-path-section-p cache-path)
		       section
		     (null section))
		   (setq cache-file (elmo-file-cache-expand-path
				     cache-path
				     section))
		   (file-exists-p cache-file))
	  (insert-file-contents-as-binary cache-file)
	  t))
    ;; igore error
    (error)))

(defun elmo-cache-path-section-p (path)
  "Return non-nil when PATH is `section' cache path."
  (file-directory-p path))

(defun elmo-file-cache-get (msgid &optional section)
  "Returns the current file-cache object associated with MSGID.
MSGID is the message-id of the message.
If optional argument SECTION is specified, get partial file-cache object
associated with SECTION."
  (if msgid
      (let ((path (elmo-cache-get-path msgid)))
	(if (and path (file-exists-p path))
	    (if (elmo-cache-path-section-p path)
		(if section
		    (if (file-exists-p (setq path (expand-file-name
						   section path)))
			(cons path 'section))
		  ;; section is not specified but sectional.
		  (cons path 'section))
	      ;; not directory.
	      (unless section
		(cons path 'entire)))
	  ;; no cache.
	  (cons path nil)))))

;;;
;; Expire cache.

(defun elmo-cache-expire ()
  (interactive)
  (let* ((completion-ignore-case t)
	 (method (completing-read (format "Expire by (%s): "
					  elmo-cache-expire-default-method)
				  '(("size" . "size")
				    ("age" . "age"))
				  nil t)))
    (when (string= method "")
      (setq method elmo-cache-expire-default-method))
    (funcall (intern (concat "elmo-cache-expire-by-" method)))))

(defun elmo-read-float-value-from-minibuffer (prompt &optional initial)
  (let ((str (read-from-minibuffer prompt initial)))
    (cond
     ((string-match "[0-9]*\\.[0-9]+" str)
      (string-to-number str))
     ((string-match "[0-9]+" str)
      (string-to-number (concat str ".0")))
     (t (error "%s is not number" str)))))

(defun elmo-cache-expire-by-size (&optional kbytes)
  "Expire cache file by size.
If KBYTES is kilo bytes (This value must be float)."
  (interactive)
  (let ((size (or kbytes
		  (and (interactive-p)
		       (elmo-read-float-value-from-minibuffer
			"Enter cache disk size (Kbytes): "
			(number-to-string
			 (if (integerp elmo-cache-expire-default-size)
			     (float elmo-cache-expire-default-size)
			   elmo-cache-expire-default-size))))
		  (if (integerp elmo-cache-expire-default-size)
		      (float elmo-cache-expire-default-size))))
	(count 0)
	(Kbytes 1024)
	total beginning)
    (message "Checking disk usage...")
    (setq total (/ (elmo-disk-usage
		    elmo-cache-directory) Kbytes))
    (setq beginning total)
    (message "Checking disk usage...done")
    (let ((cfl (elmo-cache-get-sorted-cache-file-list))
	  (deleted 0)
	  oldest
	  cur-size cur-file)
      (while (and (<= size total)
		  (setq oldest (elmo-cache-get-oldest-cache-file-entity cfl)))
	(setq cur-file (expand-file-name (car (cdr oldest)) (car oldest)))
	(setq cur-size (/ (elmo-disk-usage cur-file) Kbytes))
	(when (elmo-file-cache-delete cur-file)
	  (setq count (+ count 1))
	  (message "%d cache(s) are expired." count))
	(setq deleted (+ deleted cur-size))
	(setq total (- total cur-size)))
      (message "%d cache(s) are expired from disk (%d Kbytes/%d Kbytes)."
	       count deleted beginning))))

(defun elmo-cache-make-file-entity (filename path)
  (cons filename (elmo-get-last-accessed-time filename path)))

(defun elmo-cache-get-oldest-cache-file-entity (cache-file-list)
  (let ((cfl cache-file-list)
	flist firsts oldest-entity wonlist)
    (while cfl
      (setq flist (cdr (car cfl)))
      (setq firsts (append firsts (list
				   (cons (car (car cfl))
					 (car flist)))))
      (setq cfl (cdr cfl)))
;;;    (prin1 firsts)
    (while firsts
      (if (and (not oldest-entity)
	       (cdr (cdr (car firsts))))
	  (setq oldest-entity (car firsts)))
      (if (and (cdr (cdr (car firsts)))
	       (cdr (cdr oldest-entity))
	       (> (cdr (cdr oldest-entity)) (cdr (cdr (car firsts)))))
	  (setq oldest-entity (car firsts)))
      (setq firsts (cdr firsts)))
    (setq wonlist (assoc (car oldest-entity) cache-file-list))
    (and wonlist
	 (setcdr wonlist (delete (car (cdr wonlist)) (cdr wonlist))))
    oldest-entity))

(defun elmo-cache-get-sorted-cache-file-list ()
  (let ((dirs (directory-files elmo-cache-directory t "^[^\\.]"))
	elist ret-val)
    (elmo-with-progress-display (elmo-collecting-cache (length dirs))
	"Collecting cache info"
      (dolist (dir dirs)
	(setq elist (mapcar (lambda (x)
			      (elmo-cache-make-file-entity x dir))
			    (directory-files dir nil "^[^\\.]")))
	(setq ret-val (append ret-val
			      (list (cons
				     dir
				     (sort
				      elist
				      (lambda (x y)
					(< (cdr x)
					   (cdr y))))))))))
    ret-val))

(defun elmo-cache-expire-by-age (&optional days)
  "Expire cache file by age.
Optional argument DAYS specifies the days to expire caches."
  (interactive)
  (let ((age (or (and days (number-to-string days))
		 (and (interactive-p)
		      (read-from-minibuffer
		       (format "Enter days (%s): "
			       elmo-cache-expire-default-age)))
		 (number-to-string elmo-cache-expire-default-age)))
	(dirs (directory-files
	       elmo-cache-directory
	       t "^[^\\.]"))
	(count 0)
	curtime)
    (if (string= age "")
	(setq age elmo-cache-expire-default-age)
      (setq age (string-to-number age)))
    (setq curtime (current-time))
    (setq curtime (+ (* (nth 0 curtime)
			(float 65536)) (nth 1 curtime)))
    (while dirs
      (let ((files (directory-files (car dirs) t "^[^\\.]"))
	    (limit-age (* age 86400)))
	(while files
	  (when (> (- curtime (elmo-get-last-accessed-time (car files)))
		   limit-age)
	    (when (elmo-file-cache-delete (car files))
	      (setq count (+ 1 count))
	      (message "%d cache file(s) are expired." count)))
	  (setq files (cdr files))))
      (setq dirs (cdr dirs)))))

;;;
;; msgid to path.
(defun elmo-msgid-to-cache (msgid)
  (save-match-data
    (when (and msgid
	       (string-match "<\\(.+\\)>$" msgid))
      (elmo-replace-string-as-filename (match-string 1 msgid)))))

(defun elmo-cache-get-path (msgid &optional folder number)
  "Get path for cache file associated with MSGID, FOLDER, and NUMBER."
  (if (setq msgid (elmo-msgid-to-cache msgid))
      (expand-file-name
       (expand-file-name
	(if folder
	    (format "%s/%s/%s@%s"
		    (elmo-cache-get-path-subr msgid)
		    msgid
		    (or number "")
		    (elmo-safe-filename folder))
	  (format "%s/%s"
		  (elmo-cache-get-path-subr msgid)
		  msgid))
	elmo-cache-directory))))

;;;
;; Warnings.

(static-if (fboundp 'display-warning)
    (defmacro elmo-warning (&rest args)
      "Display a warning with `elmo' group."
      `(display-warning 'elmo (format ,@args)))
  (defconst elmo-warning-buffer-name "*elmo warning*")
  (defun elmo-warning (&rest args)
    "Display a warning. ARGS are passed to `format'."
    (with-current-buffer (get-buffer-create elmo-warning-buffer-name)
      (goto-char (point-max))
      (funcall #'insert (apply #'format (append args '("\n"))))
      (ignore-errors (recenter 1))
      (display-buffer elmo-warning-buffer-name))))

(defvar elmo-obsolete-variable-alist nil)

(defcustom elmo-obsolete-variable-show-warnings t
  "Show warning window if obsolete variable is treated."
  :type 'boolean
  :group 'elmo)

(defun elmo-define-obsolete-variable (obsolete var)
  "Define obsolete variable.
OBSOLETE is a symbol for obsolete variable.
VAR is a symbol for new variable.
Definition is stored in `elmo-obsolete-variable-alist'."
  (let ((pair (assq var elmo-obsolete-variable-alist)))
    (if pair
	(setcdr pair obsolete)
      (setq elmo-obsolete-variable-alist
	    (cons (cons var obsolete)
		  elmo-obsolete-variable-alist)))))

(defun elmo-resque-obsolete-variable (obsolete var)
  "Resque obsolete variable OBSOLETE as VAR.
If `elmo-obsolete-variable-show-warnings' is non-nil, show warning message."
  (when (boundp obsolete)
    (static-if (and (fboundp 'defvaralias)
		    (subrp (symbol-function 'defvaralias)))
	(defvaralias var obsolete)
      (set var (symbol-value obsolete)))
    (if elmo-obsolete-variable-show-warnings
	(elmo-warning "%s is obsolete. Use %s instead."
		      (symbol-name obsolete)
		      (symbol-name var)))))

(defun elmo-resque-obsolete-variables (&optional alist)
  "Resque obsolete variables in ALIST.
ALIST is a list of cons cell of
\(OBSOLETE-VARIABLE-SYMBOL . NEW-VARIABLE-SYMBOL\).
If ALIST is nil, `elmo-obsolete-variable-alist' is used."
  (dolist (pair elmo-obsolete-variable-alist)
    (elmo-resque-obsolete-variable (cdr pair)
				   (car pair))))

(defsubst elmo-msgdb-get-last-message-id (string)
  (if string
      (save-match-data
	(let (beg)
	  (elmo-set-work-buf
	    (insert string)
	    (goto-char (point-max))
	    (when (search-backward "<" nil t)
	      (setq beg (point))
	      (if (search-forward ">" nil t)
		  (elmo-replace-in-string
		   (buffer-substring beg (point)) "\n[ \t]*" ""))))))))

(defun elmo-extract-std11-msgid-tokens (msgid-string)
  (let (ids)
    (dolist (token msgid-string ids)
      (when (eq (car token) 'msg-id) (setq ids (cons token ids))))
    (nreverse ids)))

(defun elmo-parse-msgid-field (field)
  (mapcar #'std11-msg-id-string (elmo-extract-std11-msgid-tokens (std11-parse-msg-ids-string field))))

(defun elmo-get-message-id-from-field (field)
  (if (and (not elmo-always-prefer-std11-parser)
	   (string-match "\\`[ \n\t]*\\(<[^<>]+>\\)[ \n\t]*\\'" field))
      (match-string 1 field)
    (let ((msgid-list (elmo-parse-msgid-field field)))
      (when (null (cdr msgid-list)) (car msgid-list)))))

(defun elmo-get-message-id-from-header (&optional when-invalid)
  (let ((msgid-field (std11-fetch-field "message-id")))
    (when msgid-field
      (let ((msgid (elmo-get-message-id-from-field msgid-field)))
	(or msgid
	    (cond
	     ((eq when-invalid 'none) nil)
	     ((eq when-invalid 'msgdb) (concat "<" (std11-unfold-string msgid-field) ">"))
	     (t (std11-unfold-string msgid-field))))))))

(defun elmo-get-message-id-from-buffer (&optional when-invalid)
  (save-excursion
    (save-restriction
      (std11-narrow-to-header)
      (elmo-get-message-id-from-header when-invalid))))

(defun elmo-msgdb-get-message-id-from-header ()
  (or (elmo-get-message-id-from-header 'msgdb)
      ;; no message-id, so put dummy msgid.
      (concat "<"
	      (if (elmo-unfold-fetch-field "date")
		  (timezone-make-date-sortable
		   (elmo-unfold-fetch-field "date"))
		(md5 (string-as-unibyte (buffer-string))))
	      (nth 1 (eword-extract-address-components
		      (or (std11-fetch-field "from") "nobody"))) ">")))

(defun elmo-msgdb-get-message-id-from-buffer ()
  (save-excursion
    (save-restriction
      (std11-narrow-to-header)
      (elmo-msgdb-get-message-id-from-header))))

(defun elmo-msgdb-get-message-ids-from-header (field)
  (mapcar 'std11-msg-id-string
	  (std11-parse-msg-ids-string (std11-fetch-field field))))

(defun elmo-msgdb-get-references-from-header ()
  (let ((irt (elmo-msgdb-get-message-ids-from-header "in-reply-to"))
	(refs (elmo-msgdb-get-message-ids-from-header "references")))
    (delq nil
	  (elmo-uniq-list
	   (nreverse
	    (if elmo-msgdb-prefer-in-reply-to-for-parent
		(nconc refs irt)
	      (nconc irt refs)))))))


(defsubst elmo-msgdb-insert-file-header (file)
  "Insert the header of the article.  Buffer contents after point are deleted."
  (when (file-exists-p file)
    (let ((beg 0)
	  insert-file-contents-pre-hook   ; To avoid autoconv-xmas...
	  insert-file-contents-post-hook
	  format-alist
	  (first t)
	  done)
      ;; Read until header separator is found.
      (while (null done)
	(setq done
	      (/= elmo-msgdb-file-header-chop-length
		  (nth 1 (insert-file-contents-as-binary
			  file nil beg
			  (incf beg elmo-msgdb-file-header-chop-length)))))
	(if first
	    (setq first nil)
	  (backward-char))
	(elmo-delete-cr-region (point) nil)
	(setq done (or (search-forward "\n\n" nil 'move)
		       done)))
      (delete-region (point) (point-max)))))

;;
;; overview handling
;;
(defun elmo-multiple-field-body (name &optional boundary)
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (goto-char (point-min))
      (let ((case-fold-search t)
	    (field-body nil))
	(while (re-search-forward (concat "^" name ":[ \t]*") nil t)
	  (setq field-body
		(nconc field-body
		       (list (buffer-substring-no-properties
			      (match-end 0) (std11-field-end))))))
	field-body))))

(defun elmo-parse-addresses (string)
  (if (null string)
      ()
    (elmo-set-work-buf
      (let (list start s char)
	(insert string)
	(goto-char (point-min))
	(skip-chars-forward "\t\f\n\r ")
	(setq start (point))
	(while (not (eobp))
	  (skip-chars-forward "^\"\\,(")
	  (setq char (following-char))
	  (cond ((= char ?\\)
		 (forward-char)
		 (if (not (eobp))
		     (forward-char)))
		((= char ?,)
		 (setq s (buffer-substring start (point)))
		 (if (or (null (string-match "^[\t\f\n\r ]+$" s))
			 (not (string= s "")))
		     (setq list (cons s list)))
		 (skip-chars-forward ",\t\f\n\r ")
		 (setq start (point)))
		((= char ?\")
		 (re-search-forward "[^\\]\"" nil 0))
		((= char ?\()
		 (let ((parens 1))
		   (forward-char)
		   (while (and (not (eobp)) (not (zerop parens)))
		     (re-search-forward "[()]" nil 0)
		     (cond ((or (eobp)
				(= (char-after (- (point) 2)) ?\\)))
			   ((= (preceding-char) ?\()
			    (setq parens (1+ parens)))
			   (t
			    (setq parens (1- parens)))))))))
	(setq s (buffer-substring start (point)))
	(if (and (null (string-match "^[\t\f\n\r ]+$" s))
		 (not (string= s "")))
	    (setq list (cons s list)))
	(nreverse list)))))

;;; Queue.
(defvar elmo-dop-queue-filename "queue"
  "*Disconnected operation queue is saved in this file.")

(defun elmo-dop-queue-load ()
  (setq elmo-dop-queue
	(elmo-object-load
	 (expand-file-name elmo-dop-queue-filename
			   elmo-msgdb-directory))))

(defun elmo-dop-queue-save ()
  (elmo-object-save
   (expand-file-name elmo-dop-queue-filename
		     elmo-msgdb-directory)
   elmo-dop-queue))

(if (and (fboundp 'regexp-opt)
	 (not (featurep 'xemacs)))
    (defalias 'elmo-regexp-opt 'regexp-opt)
  (defun elmo-regexp-opt (strings &optional paren)
    "Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct."
    (let ((open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" "")))
      (concat open-paren (mapconcat 'regexp-quote strings "\\|")
	      close-paren))))

(require 'product)
(product-provide (provide 'elmo-util) (require 'elmo-version))

;;; elmo-util.el ends here

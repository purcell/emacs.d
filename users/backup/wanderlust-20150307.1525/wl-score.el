;;; wl-score.el --- Scoring in Wanderlust.

;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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
;; Original codes are gnus-score.el and score-mode.el

;;; Code:
;;


(require 'wl-vars)
(require 'wl-util)
(eval-when-compile
  (require 'cl)				; cadaar, cddaar
  (require 'elmo-msgdb))		; for inline functions

(defvar wl-score-edit-header-char
  '((?a "from" nil string)
    (?s "subject" nil string)
    (?i "message-id" nil string)
    (?r "references" "message-id" string)
    (?x "xref" nil string)
    (?e "extra" nil string)
    (?l "lines" nil number)
    (?d "date" nil date)
    (?f "followup" nil string)
    (?t "thread" "message-id" string)))

(defvar wl-score-edit-type-char
  '((?s s "substring" string)
    (?e e "exact string" string)
    (?f f "fuzzy string" string)
    (?r r "regexp string" string)
    (?b before "before date" date)
    (?a after "after date" date)
    (?n at "this date" date)
    (?< < "less than number" number)
    (?> > "greater than number" number)
    (?= = "equal to number" number)))

(defvar wl-score-edit-perm-char
  '((?t temp "temporary")
    (?p perm "permanent")
    (?i now "immediate")))

;;; Global Variable

(defconst wl-score-header-index
  ;; Name to function alist.
  '(("number"     wl-score-integer  number)
    ("subject"    wl-score-string   subject charset)
    ("from"       wl-score-string   from charset)
    ("date"       wl-score-date     date)
    ("message-id" wl-score-string   message-id)
    ("references" wl-score-string   references)
    ("to"	  wl-score-string   to)
    ("cc"	  wl-score-string   cc)
    ("chars"      wl-score-integer  size)
    ("lines"      wl-score-integer  lines)
    ("xref"       wl-score-string   xref)
    ("extra"      wl-score-extra    extra mime)
    ("followup"   wl-score-followup from charset)
    ("thread"     wl-score-thread   references)))

(defvar wl-score-auto-make-followup-entry nil)
(defvar wl-score-debug nil)
(defvar wl-score-trace nil)

(defvar wl-score-alist nil)
(defvar wl-score-index nil)
(defvar wl-score-cache nil)
(defvar wl-scores-messages nil)
(defvar wl-current-score-file nil)
(defvar wl-score-make-followup nil)
(defvar wl-score-stop-add-entry nil)

(defvar wl-prev-winconf nil)
(defvar wl-score-help-winconf nil)
(defvar wl-score-header-buffer-list nil)
(defvar wl-score-alike-hashtb nil)

(defvar wl-score-edit-exit-function nil
  "Function run on exit from the score buffer.")

(make-variable-buffer-local 'wl-current-score-file)
(make-variable-buffer-local 'wl-score-alist)

;; Utility functions

(defun wl-score-simplify-buffer-fuzzy ()
  "Simplify string in the buffer fuzzily.
The string in the accessible portion of the current buffer is simplified.
It is assumed to be a single-line subject.
Whitespace is generally cleaned up, and miscellaneous leading/trailing
matter is removed.  Additional things can be deleted by setting
`wl-score-simplify-fuzzy-regexp'."
  (let ((regexp
	 (if (listp wl-score-simplify-fuzzy-regexp)
	     (mapconcat (function identity) wl-score-simplify-fuzzy-regexp
			"\\|")
	   wl-score-simplify-fuzzy-regexp))
	(case-fold-search t)
	modified-tick)
    (elmo-buffer-replace "\t" " ")
    (while (not (eq modified-tick (buffer-modified-tick)))
      (setq modified-tick (buffer-modified-tick))
      (elmo-buffer-replace regexp)
      (elmo-buffer-replace "^ *\\[[-+?*!][-+?*!]\\] *")
      (elmo-buffer-replace
       "^ *\\(re\\|fw\\|fwd\\|forward\\)[[{(^0-9]*[])}]?[:;] *")
      (elmo-buffer-replace "^[[].*:\\( .*\\)[]]$" "\\1"))
    (elmo-buffer-replace " *[[{(][^()\n]*[]})] *$")
    (elmo-buffer-replace "  +" " ")
    (elmo-buffer-replace " $")
    (elmo-buffer-replace "^ +")))

(defun wl-score-simplify-string-fuzzy (string)
  "Simplify a STRING fuzzily.
See `wl-score-simplify-buffer-fuzzy' for details."
  (elmo-set-work-buf
   (let ((case-fold-search t))
     (insert string)
     (wl-score-simplify-buffer-fuzzy)
     (buffer-string))))

(defun wl-score-simplify-subject (subject)
  "Simplify a SUBJECT fuzzily.
Remove Re, Was, Fwd etc."
  (elmo-set-work-buf
   (let ((regexp
	  (if (listp wl-score-simplify-fuzzy-regexp)
	      (mapconcat (function identity) wl-score-simplify-fuzzy-regexp
			 "\\|")
	    wl-score-simplify-fuzzy-regexp))
	 (case-fold-search t))
     (insert subject)
     (elmo-buffer-replace regexp)
     (elmo-buffer-replace
      "^[ \t]*\\(re\\|was\\|fw\\|fwd\\|forward\\)[:;][ \t]*")
     (buffer-string))))

;;

(defun wl-score-overview-entity-get-lines (entity)
  (let ((lines (elmo-message-entity-field entity 'lines)))
    (and lines
	 (string-to-number lines))))

(defun wl-score-overview-entity-get-xref (entity)
  (or (elmo-message-entity-field entity 'xref)
      ""))

(defun wl-string> (s1 s2)
  (not (or (string< s1 s2)
	   (string= s1 s2))))

(defsubst wl-score-ov-entity-get (entity index &optional extra)
  (elmo-message-entity-field entity (if extra (intern extra) index)
			     ;; FIXME
			     (if (or (eq index 'to) (eq index 'cc))
				 'string
			       nil)))

(defun wl-score-string< (a1 a2)
  (string-lessp (wl-score-ov-entity-get (car a1) wl-score-index)
		(wl-score-ov-entity-get (car a2) wl-score-index)))

(defun wl-score-string-sort (messages index)
  (sort messages 'wl-score-string<))

(defsubst wl-score-get (symbol &optional alist)
  "Get SYMBOL's definition in ALIST."
  ;; Get SYMBOL's definition in ALIST.
  (cdr (assoc symbol
	      (or alist
		  wl-score-alist))))

(defun wl-score-set (symbol value &optional alist warn)
  "Set SYMBOL to VALUE in ALIST."
  ;; Set SYMBOL to VALUE in ALIST.
  (let* ((alist (or alist wl-score-alist))
	 (entry (assoc symbol alist)))
    (cond ((wl-score-get 'read-only alist)
	   ;; This is a read-only score file, so we do nothing.
	   (when warn
	     (message "Note: read-only score file; entry discarded")))
	  (entry
	   (setcdr entry value))
	  ((null alist)
	   (error "Empty alist"))
	  (t
	   (setcdr alist
		   (cons (cons symbol value) (cdr alist)))))))

(defun wl-score-cache-clean ()
  "Cleaning score cache.
Set `wl-score-cache' nil."
  (interactive)
  (setq wl-score-cache nil))

(defun wl-score-load-score-alist (file)
  "Read score FILE."
  (let (alist)
    (if (not (file-readable-p file))
	(setq wl-score-alist nil)
      (with-temp-buffer
	(wl-as-mime-charset wl-score-mode-mime-charset
	  (insert-file-contents file))
	(goto-char (point-min))
	;; Only do the loading if the score file isn't empty.
	(when (save-excursion (re-search-forward "[()0-9a-zA-Z]" nil t))
	  (setq alist
		(condition-case ()
		    (read (current-buffer))
		  (error "Problem with score file %s" file))))
	(cond
	 ((and alist
	       (atom alist))
	  (error "Invalid syntax with score file %s" file))
	 (t
	  (setq wl-score-alist alist)))))))

(defun wl-score-save ()
  "Save all score information."
  ;; Save all score information.
  (let ((cache wl-score-cache)
	entry score file dir)
    (with-temp-buffer
      (setq wl-score-alist nil)
      (while cache
	(setq entry (pop cache)
	      file (car entry)
	      score (cdr entry))
	(unless (or (not (equal (wl-score-get 'touched score) '(t)))
		    (wl-score-get 'read-only score)
		    (and (file-exists-p file)
			 (not (file-writable-p file))))
	  (setq score (setcdr entry (wl-delete-alist 'touched score)))
	  (erase-buffer)
	  (let (emacs-lisp-mode-hook
		(lisp-mode-syntax-table wl-score-mode-syntax-table)
		print-length print-level)
	    (pp score (current-buffer)))
	  (setq dir (file-name-directory file))
	  (if (file-directory-p dir)
	      (); ok.
	    (if (file-exists-p dir)
		(error "File %s already exists" dir)
	      (elmo-make-directory dir)))
	  ;; If the score file is empty, we delete it.
	  (if (zerop (buffer-size))
	      (when (file-exists-p file) ; added by teranisi.
		(delete-file file))
	    ;; There are scores, so we write the file.
	    (when (file-writable-p file)
	      (wl-as-mime-charset wl-score-mode-mime-charset
		(write-region (point-min) (point-max)
			      file nil 'no-msg)))))))))

(defun wl-score-remove-from-cache (file)
  (setq wl-score-cache
	(delq (assoc file wl-score-cache) wl-score-cache)))

(defun wl-score-load-file (file)
  (let* ((file (expand-file-name
		(or (and (string-match
			  (concat "^" (regexp-quote
				       (expand-file-name
					wl-score-files-directory)))
			  (expand-file-name file))
			 file)
		    (expand-file-name
		     file
		     (file-name-as-directory wl-score-files-directory)))))
	 (cached (assoc file wl-score-cache))
	 alist)
    (if cached
	;; The score file was already loaded.
	(setq alist (cdr cached))
      ;; We load the score file.
      (setq wl-score-alist nil)
      (setq alist (wl-score-load-score-alist file))
      (unless (assq 'touched alist)
	(wl-push (list 'touched nil) alist))
      (wl-push (cons file alist) wl-score-cache))
    (let ((a alist))
      (while a
	;; Downcase all header names.
	(cond
	 ((stringp (caar a))
	  (setcar (car a) (downcase (caar a)))))
	(pop a)))
    (setq wl-current-score-file file)
    (setq wl-score-alist alist)))

(defun wl-score-get-score-files (score-alist folder)
  (let ((files (wl-get-assoc-list-value
		score-alist (elmo-folder-name-internal folder)
		(if (not wl-score-folder-alist-matchone) 'all-list)))
	fl f)
    (while (setq f (wl-pop files))
      (wl-append
       fl
       (cond ((functionp f)
	      (funcall f  folder))
	     (t
	      (list f)))))
    fl))

(defun wl-score-get-score-alist ()
  (interactive)
  (let* ((score-alist (reverse
		       (wl-score-get-score-files
			wl-score-folder-alist
			wl-summary-buffer-elmo-folder)))
	 alist scores)
    (setq wl-current-score-file nil)
    (unless (and wl-score-default-file
		 (member wl-score-default-file score-alist))
      (wl-push wl-score-default-file score-alist))
    (while score-alist
      (setq alist
	    (cond ((stringp (car score-alist))	;; file
		   (wl-score-load-file (car score-alist)))
		  ((consp (car score-alist))	;; alist
		   (car score-alist))
		  ((boundp (car score-alist))	;; variable
		   (symbol-value (car score-alist)))
		  (t
		   (error "Void variable: %s" (car score-alist)))))
      (let ((mark (car (wl-score-get 'mark alist)))
	    (expunge (car (wl-score-get 'expunge alist)))
	    (mark-and-expunge (car (wl-score-get 'mark-and-expunge alist)))
	    (temp (car (wl-score-get 'temp alist))) ; obsolate
	    (target (car (wl-score-get 'target alist)))
	    (important (car (wl-score-get 'important alist))))
	(setq wl-summary-important-above
	      (or important wl-summary-important-above))
	(setq wl-summary-target-above
	      (or target temp wl-summary-target-above))
	(setq wl-summary-mark-below
	      (or mark mark-and-expunge wl-summary-mark-below))
	(setq wl-summary-expunge-below
	      (or expunge mark-and-expunge wl-summary-expunge-below)))
      (wl-append scores (list alist))
      (setq score-alist (cdr score-alist)))
    scores))

(defun wl-score-headers (scores &optional force-msgs not-add)
  (let* ((elmo-mime-charset wl-summary-buffer-mime-charset)
	 (folder wl-summary-buffer-elmo-folder)
	 (now (elmo-time-to-days (current-time)))
	 (expire (and wl-score-expiry-days
		      (- now wl-score-expiry-days)))
	 (wl-score-stop-add-entry not-add)
	 entries
	 news new num entry ov header)
    (setq wl-scores-messages nil)
    (message "Scoring...")

    ;; Create messages, an alist of the form `(ENTITY . SCORE)'.
    (dolist (num (elmo-folder-list-messages folder 'visible 'in-db))
      (when (and (not (assq num wl-summary-scored))
		 (or (memq num force-msgs)
		     (member (wl-summary-message-mark folder num)
			     wl-summary-score-marks)))
	(setq wl-scores-messages
	      (cons (cons (elmo-message-entity folder num)
			  (or wl-summary-default-score 0))
		    wl-scores-messages))))

    (save-excursion
      (setq news scores)
      (while news
	(setq scores news
	      news nil)
	;; Run each header through the score process.
	(setq entries wl-score-header-index)
	(while entries
	  (setq entry (pop entries)
		header (car entry))
	  (if (> (length wl-scores-messages) 500)
	      (message "Scoring...\"%s\"" header))
	  (when (< 0 (apply 'max (mapcar
				  (lambda (score)
				    (length (wl-score-get header score)))
				  scores)))
	    ;; Call the scoring function for this type of "header".
	    (when (setq new (funcall (nth 1 entry) scores header now expire))
	      (wl-push new news))))))

    ;; Add messages to `wl-summary-scored'.
    (let (entry num score)
      (while wl-scores-messages
	(when (or (/= wl-summary-default-score
		      (cdar wl-scores-messages)))
	  (setq num (elmo-message-entity-number
		     (caar wl-scores-messages))
		score (cdar wl-scores-messages))
	  (if (setq entry (assq num wl-summary-scored))
	      (setcdr entry (+ score (cdr entry)))
	    (wl-push (cons num score)
		  wl-summary-scored)))
	(setq wl-scores-messages (cdr wl-scores-messages))))
    (message "Scoring...done")
    ;; Remove buffers.
    (while wl-score-header-buffer-list
      (elmo-kill-buffer (pop wl-score-header-buffer-list)))))

(defun wl-score-integer (scores header now expire)
  (let ((wl-score-index (nth 2 (assoc header wl-score-header-index)))
	entries alist)

    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) '>))
	       (score (or (nth 1 kill) wl-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (match-func (if (memq type '(> < <= >= =))
			       type
			     (error "Invalid match type: %s" type)))
	       (messages wl-scores-messages))
	  (while messages
	    (when (funcall match-func
			   (or (wl-score-ov-entity-get
				(caar messages) wl-score-index)
			       0)
			   match)
	      (setq found t)
	      (setcdr (car messages) (+ score (cdar messages))))
	    (setq messages (cdr messages)))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		((and found wl-score-update-entry-dates) ;Match, update date.
		 (wl-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((and expire (< date expire)) ;Old entry, remove.
		 (wl-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest)))))
  nil)

(defun wl-score-date (scores header now expire)
  (let ((wl-score-index (nth 2 (assoc header wl-score-header-index)))
	entries alist match match-func message)
    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))
	       (kill (car rest))
	       (type (or (nth 3 kill) 'before))
	       (score (or (nth 1 kill) wl-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (messages wl-scores-messages)
	       l)
	  (cond
	   ((eq type 'after)
	    (setq match-func 'string<
		  match (wl-date-iso8601 (nth 0 kill))))
	   ((eq type 'before)
	    (setq match-func 'wl-string>
		  match (wl-date-iso8601 (nth 0 kill))))
	   ((eq type 'at)
	    (setq match-func 'string=
		  match (wl-date-iso8601 (nth 0 kill))))
	   ((eq type 'regexp)
	    (setq match-func 'string-match
		  match (nth 0 kill)))
	   (t (error "Invalid match type: %s" type)))
	  (while (setq message (pop messages))
	    (when (and
		   (setq l (wl-score-ov-entity-get
			    (car message) wl-score-index))
		   (funcall match-func match (wl-date-iso8601 l)))
	      (setq found t)
	      (setcdr message (+ score (cdr message)))))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		((and found wl-score-update-entry-dates) ;Match, update date.
		 (wl-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((and expire (< date expire)) ;Old entry, remove.
		 (wl-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest)))))
  nil)

(defun wl-score-extra (scores header now expire)
  (let ((score-list scores)
	entries alist extra extras)
    (while score-list
      (setq alist (pop score-list)
	    entries (assoc header alist))
      (while (cdr entries)
	(setq extra (nth 4 (cadr entries)))
	(unless (member extra extras)
	  (wl-push extra extras))
	(setq entries (cdr entries))))
    (while extras
      (wl-score-string scores header now expire (car extras))
      (setq extras (cdr extras)))
    nil))

(defmacro wl-score-put-alike (alike)
  `(elmo-set-hash-val (format "#%d" (wl-count-lines))
		      ,alike
		      wl-score-alike-hashtb))

(defsubst wl-score-get-alike ()
  (elmo-get-hash-val (format "#%d" (wl-count-lines))
		     wl-score-alike-hashtb))

(defun wl-score-insert-header (header messages &optional extra-header)
  (let ((mime-decode (nth 3 (assoc header wl-score-header-index)))
	(buffer-name (concat "*Score-Headers-" header
			     (if extra-header
				 (concat "-" extra-header)
			       "")
			     "*"))
	buf)
    (if (setq buf (get-buffer buffer-name))
	(set-buffer buf)
      (set-buffer (setq buf (get-buffer-create buffer-name)))
      (wl-append wl-score-header-buffer-list (list buf))
      (buffer-disable-undo (current-buffer))
      (make-local-variable 'wl-score-alike-hashtb)
      (setq wl-score-alike-hashtb (elmo-make-hash (* (length messages) 2)))
      (when mime-decode
	(set-buffer-multibyte default-enable-multibyte-characters))
      (let (art last this alike)
	(while (setq art (pop messages))
	  (setq this (wl-score-ov-entity-get (car art)
					     wl-score-index
					     extra-header))
	  (when (stringp this)
	    (setq this (std11-unfold-string this)))
	  (if (equal last this)
	      ;; O(N*H) cons-cells used here, where H is the number of
	      ;; headers.
	      (wl-push art alike)
	    (when last
	      (wl-score-put-alike alike)
	      (insert last ?\n))
	    (setq alike (list art)
		  last this)))
	(when last
	  (wl-score-put-alike alike)
	  (insert last ?\n))
	(when mime-decode
	  (decode-mime-charset-region (point-min) (point-max)
				      elmo-mime-charset)
	  (when (eq mime-decode 'mime)
	    (eword-decode-region (point-min) (point-max))))))))

(defun wl-score-string (scores header now expire &optional extra-header)
  "Insert the unique message headers in the buffer."
  ;; Insert the unique message headers in the buffer.
  (let ((wl-score-index (nth 2 (assoc header wl-score-header-index)))
	entries alist messages
	fuzzies kill)
    (when (integerp wl-score-index)
      (setq wl-scores-messages
	    (wl-score-string-sort wl-scores-messages wl-score-index)))
    (setq messages wl-scores-messages)

    (wl-score-insert-header header messages extra-header)

    ;; Go through all the score alists and pick out the entries
    ;; for this header.
    (while scores
      (setq alist (pop scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((kill (cadr entries))
	       (type (or (nth 3 kill) 's))
	       (score (or (nth 1 kill) wl-score-interactive-default-score))
	       (date (nth 2 kill))
	       (extra (nth 4 kill))	; non-standard header; string.
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search (not (memq mt '(?R ?S ?E ?F))))
	       (dmt (downcase mt))
	       (match (nth 0 kill))
	       (search-func
		(cond ((= dmt ?r) 're-search-forward)
		      ((memq dmt '(?e ?s ?f)) 'search-forward)
		      ((= dmt ?w) nil)
		      (t (error "Invalid match type: %s" type))))
	       arts art found)
	  (if (and extra-header
		   (or (not extra)
		       (not (string= extra-header extra))))
	      (setq entries (cdr entries))
	    (cond
	     ;; Fuzzy matches.  We save these for later.
	     ((= dmt ?f)
	      (wl-push (cons entries alist) fuzzies)
	      (setq entries (cdr entries)))
	     (t
	      ;; Regexp, substring and exact matching.
	      (goto-char (point-min))
	      (when (and (not (= dmt ?e))
			 (string= match ""))
		(setq match "\n"))
	      (while (and (not (eobp))
			  (funcall search-func match nil t))
		(when (or (not (= dmt ?e))
			  ;; Is it really exact?
			  (and (eolp)
			       (= (save-excursion (forward-line 0) (point))
				  (match-beginning 0))))
;;;		  (end-of-line)
		  (setq found (setq arts (wl-score-get-alike)))
		  ;; Found a match, update scores.
		  (while (setq art (pop arts))
		    (setcdr art (+ score (cdr art)))))
		(forward-line))
	      ;; Update expiry date
	      (cond
	       ;; Permanent entry.
	       ((null date)
		(setq entries (cdr entries)))
	       ;; We have a match, so we update the date.
	       ((and found wl-score-update-entry-dates)
		(wl-score-set 'touched '(t) alist)
		(setcar (nthcdr 2 kill) now)
		(setq entries (cdr entries)))
	       ;; This entry has expired, so we remove it.
	       ((and expire (< date expire))
		(wl-score-set 'touched '(t) alist)
		(setcdr entries (cddr entries)))
	       ;; No match; go to next entry.
	       (t
		(setq entries (cdr entries))))))))))

    ;; Find fuzzy matches.
    (when fuzzies
      ;; Simplify the entire buffer for easy matching.
      (wl-score-simplify-buffer-fuzzy)
      (while (setq kill (cadaar fuzzies))
	(let* ((match (nth 0 kill))
	       (type (nth 3 kill))
	       (score (or (nth 1 kill) wl-score-interactive-default-score))
	       (date (nth 2 kill))
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search (not (= mt ?F)))
	       arts art found)
	  (goto-char (point-min))
	  (while (and (not (eobp))
		      (search-forward match nil t))
	    (when (and (eolp)
		       (= (save-excursion (forward-line 0) (point))
			  (match-beginning 0)))
	      (setq found (setq arts (wl-score-get-alike)))
	      (while (setq art (pop arts))
		(setcdr art (+ score (cdr art)))))
	    (forward-line))
	  ;; Update expiry date
	  (cond
	   ;; Permanent.
	   ((null date))
	   ;; Match, update date.
	   ((and found wl-score-update-entry-dates)
	    (wl-score-set 'touched '(t) (cdar fuzzies))
	    (setcar (nthcdr 2 kill) now))
	   ;; Old entry, remove.
	   ((and expire (< date expire))
	    (wl-score-set 'touched '(t) (cdar fuzzies))
	    (setcdr (caar fuzzies) (cddaar fuzzies))))
	  (setq fuzzies (cdr fuzzies)))))
    nil))

(defun wl-score-thread (scores header now expire)
  (wl-score-followup scores header now expire t))

(defun wl-score-followup (scores header now expire &optional thread)
  "Insert the unique message headers in the buffer."
  (let ((wl-score-index (nth 2 (assoc header wl-score-header-index)))
	(all-scores scores)
	entries alist messages
	new news)
    (when (integerp wl-score-index)
      (setq wl-scores-messages
	    (wl-score-string-sort wl-scores-messages wl-score-index)))
    (setq messages wl-scores-messages)

    (wl-score-insert-header (if thread "references" "from") messages)

    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) 's))
	       (score (or (nth 1 kill) wl-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search (not (memq mt '(?R ?S ?E ?F))))
	       (dmt (downcase mt))
	       (search-func
		(cond ((= dmt ?r) 're-search-forward)
		      ((memq dmt '(?e ?s ?f)) 'search-forward)
		      (t (error "Invalid match type: %s" type))))
	       arts art day)
	  (goto-char (point-min))
	  (while (funcall search-func match nil t)
	    (when (or (not (= dmt ?e))
		      (and (eolp)
			   (= (progn (beginning-of-line) (point))
			      (match-beginning 0))))
;;;	      (end-of-line)
	      (setq found (setq arts (wl-score-get-alike)))
	      ;; Found a match, update scores.
	      (while (setq art (pop arts))
		(setq day nil)
		(when (or (not wl-score-make-followup)
			  (and wl-score-update-entry-dates
			       expire
			       (< expire
				  (setq day
					(elmo-time-to-days
					 (elmo-message-entity-field
					  (car art) 'date))))))
		  (when (setq new (wl-score-add-followups
				   (car art) score all-scores alist thread
				   day))
		    (when thread
		      (unless wl-score-stop-add-entry
			(wl-append rest (list new)))
		      (setcdr art (+ score (cdr art))))
		    (wl-push new news))))
	      (forward-line)))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		((and found wl-score-update-entry-dates) ;Match, update date.
		 (wl-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((and expire (< date expire)) ;Old entry, remove.
		 (wl-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))
    (when (and news (not thread))
      (list (cons "references" news)))))

(defun wl-score-add-followups (header score scores alist &optional thread day)
  (let* ((id (elmo-message-entity-field header 'message-id))
	 (scores (car scores))
	 entry dont)
    (when id
      ;; Don't enter a score if there already is one.
      (while (setq entry (pop scores))
	(and (member (car entry) '("thread" "references"))
	     (memq (nth 3 (cadr entry)) '(s nil))
	     (assoc id entry)
	     (setq dont t)))
      (unless dont
	(let ((entry (list id score
			   (or day (elmo-time-to-days (current-time))) 's)))
	  (unless (or thread wl-score-stop-add-entry)
	    (wl-score-update-score-entry "references" entry alist))
	  (wl-score-set 'touched '(t) alist)
	  entry)))))

(defun wl-score-flush-cache ()
  "Flush the cache of score files."
  (interactive)
  (wl-score-save)
  (setq wl-score-cache nil
	wl-score-alist nil)
  (message "The score cache is now flushed"))

(defun wl-score-set-mark-below (score)
  "Automatically mark messages with score below SCORE as read."
  (interactive
   (list (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
	     (string-to-number (read-string "Mark below: ")))))
  (setq score (or score wl-summary-default-score 0))
  (wl-score-set 'mark (list score))
  (wl-score-set 'touched '(t))
  (setq wl-summary-mark-below score)
  (wl-summary-score-update-all-lines t))

(defun wl-score-set-expunge-below (score)
  "Automatically expunge messages with score below SCORE."
  (interactive
   (list (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
	     (string-to-number (read-string "Expunge below: ")))))
  (setq score (or score wl-summary-default-score 0))
  (wl-score-set 'expunge (list score))
  (wl-score-set 'touched '(t)))

(defun wl-score-change-score-file (file)
  "Change current score alist."
  (interactive
   (list (read-file-name "Change to score file: " wl-score-files-directory)))
  (wl-score-load-file file))

(defun wl-score-default (level)
  (if level (prefix-numeric-value level)
    wl-score-interactive-default-score))

(defun wl-summary-lower-score (&optional score)
  (interactive "P")
  (wl-summary-increase-score score t))

(defun wl-summary-increase-score (&optional score lower)
  (interactive "P")
  (if (wl-summary-message-number)
  (let* ((rscore (if lower
		     (- (wl-score-default score))
		   (wl-score-default score)))
	 (increase (> rscore 0))
	 lscore entry list match type)
    (setq entry (wl-score-get-header-entry nil rscore))
    (setq list (nth 1 entry))
    (setq match (car list))
    (setq type (nth 3 list))
    (cond ((memq type '(r R s S nil))
	   (when (and match (string= (car entry) "subject"))
	     (setq match (wl-score-simplify-subject match))))
	  ((memq type '(f F))
	   (setq match (wl-score-simplify-string-fuzzy match))))
    (setq match (read-string
		 (format "Match on %s, %s: "
			 (car entry)
			 (if increase "raise" "lower"))
		 (if (numberp match)
		     (number-to-string match)
		   match)))
    ;; transform from string to int.
    (when (eq (nth 1 (assoc (car entry) wl-score-header-index))
	      'wl-score-integer)
      (setq match (string-to-number match)))
    ;; set score
    (if score
	(setq lscore rscore)
      (setq lscore (nth 1 list))
      (setq lscore
	    (abs (if lscore
		     lscore
		   wl-score-interactive-default-score)))
      (setq lscore (if lower (- lscore) lscore)))
    (setcar (cdr list)
	    (if (eq lscore wl-score-interactive-default-score)
		nil
	      lscore))
    ;; update score file
    (setcar list match)
    (unless (eq (nth 2 list) 'now)
      (let ((alist (if wl-current-score-file
		       (cdr (assoc wl-current-score-file wl-score-cache))
		     wl-score-alist)))
	(wl-score-update-score-entry (car entry) list alist)
	(wl-score-set 'touched '(t) alist)))
    (wl-summary-score-effect (car entry) list (eq (nth 2 list) 'now)))))

(defun wl-score-get-latest-msgs ()
  (let* ((now (elmo-time-to-days (current-time)))
	 (expire (and wl-score-expiry-days
		      (- now wl-score-expiry-days)))
	 (rnumbers (reverse wl-summary-buffer-number-list))
	 msgs)
    (if (not expire)
	(elmo-folder-list-messages wl-summary-buffer-elmo-folder
				   nil t)
      (catch 'break
	(while rnumbers
	  (if (< (elmo-time-to-days
		  (elmo-message-entity-field wl-summary-buffer-elmo-folder
					     (car rnumbers)
					     'date))
		 expire)
	      (throw 'break t))
	  (wl-push (car rnumbers) msgs)
	  (setq rnumbers (cdr rnumbers))))
      msgs)))

(defun wl-score-get-header (header &optional extra)
  (let ((index (nth 2 (assoc header wl-score-header-index)))
	(decode (nth 3 (assoc header wl-score-header-index))))
    (if index
	(wl-score-ov-entity-get
	 (elmo-message-entity wl-summary-buffer-elmo-folder
			      (wl-summary-message-number))
	 index extra))))

(defun wl-score-kill-help-buffer ()
  (when (get-buffer "*Score Help*")
    (kill-buffer "*Score Help*")
    (when wl-score-help-winconf
      (set-window-configuration wl-score-help-winconf))))

(defun wl-score-insert-help (string alist idx)
  (setq wl-score-help-winconf (current-window-configuration))
  (let ((cur-win (selected-window))
	mes-win)
    (with-current-buffer (get-buffer-create "*Score Help*")
      (buffer-disable-undo (current-buffer))
      (delete-windows-on (current-buffer))
      (erase-buffer)
      (insert string ":\n\n")
      (let ((max -1)
	    (list alist)
	    (i 0)
	    n width pad format)
	;; find the longest string to display
	(while list
	  (setq n (length (nth idx (car list))))
	  (unless (> max n)
	    (setq max n))
	  (setq list (cdr list)))
	(setq max (+ max 4))		; %c, `:', SPACE, a SPACE at end
	(setq n (/ (1- (window-width)) max)) ; items per line
	(setq width (/ (1- (window-width)) n)) ; width of each item
	;; insert `n' items, each in a field of width `width'
	(while alist
	  (unless (< i n)
	    (setq i 0)
	    (delete-char -1)		; the `\n' takes a char
	    (insert "\n"))
	  (setq pad (- width 3))
	  (setq format (concat "%c: %-" (number-to-string pad) "s"))
	  (insert (format format (caar alist) (nth idx (car alist))))
	  (setq alist (cdr alist))
	  (setq i (1+ i)))
	(set-buffer-modified-p nil)))
    (when (and wl-message-buffer
	       (get-buffer wl-message-buffer)
	       (setq mes-win (get-buffer-window
			      (get-buffer wl-message-buffer))))
      (select-window mes-win)
      (unless (eq (next-window) cur-win)
	(delete-window (next-window))))
    (split-window)
    (pop-to-buffer "*Score Help*")
    (let ((window-min-height 1))
      (shrink-window-if-larger-than-buffer))
    (select-window cur-win)))

(defun wl-score-get-header-entry (&optional match-func increase)
  (let (hchar tchar pchar
	header score perm type extra hentry entry)
    (unwind-protect
	(progn
	  ;; read the header to score.
	  (while (not hchar)
	    (message "%s header (%s?): "
		     (if increase
			 (if (> increase 0) "Increase" "Lower")
		       "Set")
		     (mapconcat (lambda (s) (char-to-string (car s)))
				wl-score-edit-header-char ""))
	    (setq hchar (read-char))
	    (when (or (= hchar ??) (= hchar ?\C-h))
	      (setq hchar nil)
	      (wl-score-insert-help "Match on header"
				    wl-score-edit-header-char 1)))
	  (wl-score-kill-help-buffer)
	  (unless (setq hentry (assq (downcase hchar)
				     wl-score-edit-header-char))
	    (error "Invalid header type"))

	  (message "")
	  (setq entry (assoc (setq header (nth 1 hentry))
			     wl-score-header-default-entry))
	  (setq score (nth 1 entry)
		perm (nth 2 entry)
		type (nth 3 entry))

	  ;; read extra header.
	  (when (equal header "extra")
	    (setq extra
		  (completing-read
		   "Set extra header: "
		   (mapcar 'list
			   elmo-msgdb-extra-fields))))

	  ;; read the type.
	  (unless type
	    (let ((valid-types
		   (delq nil
			 (mapcar (lambda (s)
				   (if (eq (nth 3 hentry)
					   (nth 3 s))
				       s nil))
				 (copy-sequence
				  wl-score-edit-type-char)))))
	      (while (not tchar)
		(message "Set header '%s' with match type (%s?): "
			 header
			 (mapconcat (lambda (s) (char-to-string (car s)))
				    valid-types ""))
		(setq tchar (read-char))
		(when (or (= tchar ??) (= tchar ?\C-h))
		  (setq tchar nil)
		  (wl-score-insert-help "Match type" valid-types 2)))
	      (wl-score-kill-help-buffer)
	      (unless (setq type (nth 1 (assq (downcase tchar) valid-types)))
		(error "Invalid match type"))
	      (message "")))

	  ;; read the permanence.
	  (unless perm
	    (while (not pchar)
	      (message "Set permanence (%s?): "
		       (mapconcat (lambda (s) (char-to-string (car s)))
				  wl-score-edit-perm-char ""))
	      (setq pchar (read-char))
	      (when (or (= pchar ??) (= pchar ?\C-h))
		(setq pchar nil)
		(wl-score-insert-help "Match permanence"
				      wl-score-edit-perm-char 2)))
	    (wl-score-kill-help-buffer)
	    (unless (setq perm (nth 1 (assq (downcase pchar)
					    wl-score-edit-perm-char)))
	      (error "Invalid match duration"))
	    (message ""))

	  ;; read the score.
	  (unless (or score increase)
	    (setq score (string-to-number (read-string "Set score: ")))))
      (message "")
      (wl-score-kill-help-buffer))

    (let* ((match-header (or (nth 2 hentry) header))
	   (match (if match-func
		      (funcall match-func match-header extra)
		    (wl-score-get-header match-header extra)))
	   (match (cond ((memq type '(r R regexp Regexp))
			 (regexp-quote match))
			((eq (nth 1 (assoc (car entry) wl-score-header-index))
			     'wl-score-integer)
			 match)
			(t
			 (or match ""))))
	   (perm (cond ((eq perm 'perm)
			nil)
		       ((eq perm 'temp)
			(elmo-time-to-days (current-time)))
		       ((eq perm 'now)
			perm)))
	   (new (list match score perm type extra)))
      (list header new))))

(defun wl-score-update-score-entries (header entries &optional alist)
  (while entries
    (wl-score-update-score-entry header (car entries) alist)
    (setq entries (cdr entries)))
  (wl-score-set 'touched '(t) alist))

(defun wl-score-update-score-entry (header new &optional alist)
  (let ((old (wl-score-get header alist))
	(match (nth 0 new))
	elem)
    (if (and old
	     (setq elem (assoc match old))
	     (eq (nth 3 elem) (nth 3 new))
	     (or (and (numberp (nth 2 elem)) (numberp (nth 2 new)))
		 (and (not (nth 2 elem)) (not (nth 2 new)))))
	(setcar (cdr elem) (+ (or (nth 1 elem)
				  wl-score-interactive-default-score)
			      (or (nth 1 new)
				  wl-score-interactive-default-score)))
      (wl-score-set header (if old (cons new old) (list new)) alist t))))

;; functions for summary mode

(defun wl-summary-score-effect (header entry &optional now)
  (let ((scores (list (list (list header entry)))))
    (setq wl-summary-scored nil)
    (cond ((string= header "followup")
	   (if wl-score-auto-make-followup-entry
	       (let ((wl-score-make-followup t))
		 (wl-score-headers scores (wl-score-get-latest-msgs)))
	     (wl-score-headers scores
			       (if (eq wl-summary-buffer-view 'thread)
				   (wl-thread-get-children-msgs
				    (wl-summary-message-number))
				 (list (wl-summary-message-number)))))
	   (unless now
	     (wl-score-update-score-entries
	      "references"
	      (cdr (assoc "references" (car scores))))))
	  ((string= header "thread")
	   (wl-score-headers scores
			     (if (eq wl-summary-buffer-view 'thread)
				 (wl-thread-get-children-msgs
				  (wl-summary-message-number))
			       (list (wl-summary-message-number))))
	   (unless now
	     (wl-score-update-score-entries header
					    ;; remove parent
					    (cdr (cdaar scores)))))
	  (t
	   (wl-score-headers scores
			     (list (wl-summary-message-number)))))
    (wl-summary-score-update-all-lines t)))

(defun wl-summary-rescore-msgs (numbers)
  (nthcdr
   (max (- (length numbers)
	   wl-summary-rescore-partial-threshold)
	0)
   numbers))

(defun wl-summary-rescore (&optional arg)
  "Redo the entire scoring process in the current summary."
  (interactive "P")
  (let (number-alist expunged)
    (wl-score-save)
    (setq wl-score-cache nil)
    (setq wl-summary-scored nil)
    (wl-summary-score-headers (unless arg
				(wl-summary-rescore-msgs
				 (elmo-folder-list-messages
				  wl-summary-buffer-elmo-folder t t))))
    (setq expunged (wl-summary-score-update-all-lines t))
    (if expunged
	(message "%d message(s) are expunged by scoring." (length expunged)))
    (set-buffer-modified-p nil)))

;; optional argument force-msgs is added by teranisi.
(defun wl-summary-score-headers (&optional force-msgs not-add)
  "Do scoring if scoring is required."
  (let ((scores (wl-score-get-score-alist)))
    (when scores
      (wl-score-headers scores force-msgs not-add))))

(defun wl-summary-score-update-all-lines (&optional update)
  (let ((alist wl-summary-scored)
	(update-unread nil)
	wl-summary-unread-message-hook
	num score dels visible score-mark mark-alist)
    (save-excursion
      (elmo-with-progress-display (wl-update-score (length alist))
	  "Updating score"
	(while alist
	  (setq num (caar alist)
		score (cdar alist))
	  (when wl-score-debug
	    (message "Scored %d with %d" score num)
	    (wl-push (list (elmo-string (wl-summary-buffer-folder-name)) num score)
		     wl-score-trace))
	  (setq score-mark (wl-summary-get-score-mark num))
	  (and (setq visible (wl-summary-jump-to-msg num))
	       (wl-summary-set-score-mark score-mark))
	  (cond ((and wl-summary-expunge-below
		      (< score wl-summary-expunge-below))
		 (wl-push num dels))
		((< score wl-summary-mark-below)
		 (if visible
		     (wl-summary-mark-as-read num); opened
		   (setq update-unread t)
		   (wl-summary-mark-as-read num))) ; closed
		((and wl-summary-important-above
		      (> score wl-summary-important-above))
		 (if (wl-thread-jump-to-msg num);; force open
		     (wl-summary-set-persistent-mark 'important num)))
		((and wl-summary-target-above
		      (> score wl-summary-target-above))
		 (if visible
		     (wl-summary-set-mark "*"))))
	  (setq alist (cdr alist))
	  (elmo-progress-notify 'wl-update-score))
	(when dels
	  (dolist (del dels)
	    (elmo-message-unset-flag wl-summary-buffer-elmo-folder
				     del 'unread))
	  (elmo-folder-kill-messages wl-summary-buffer-elmo-folder dels)
	  (wl-summary-delete-messages-on-buffer dels))
	(when (and update update-unread)
	  ;; Update Folder mode
	  (wl-folder-set-folder-updated (wl-summary-buffer-folder-name)
					(list
					 0
					 (let ((flag-count
						(wl-summary-count-unread)))
					   (or (cdr (assq 'unread flag-count))
					       0))
					 (elmo-folder-length
					  wl-summary-buffer-elmo-folder)))
	  (wl-summary-update-modeline)))
      dels)))

(defun wl-score-edit-done ()
  (let ((bufnam (buffer-file-name (current-buffer)))
	(winconf wl-prev-winconf))
    (when winconf
      (set-window-configuration winconf))
    (wl-score-remove-from-cache bufnam)
    (wl-score-load-file bufnam)))

(defun wl-score-edit-current-scores (file)
  "Edit the current score alist."
  (interactive (list wl-current-score-file))
  (if file
      (wl-score-edit-file file)
    (call-interactively 'wl-score-edit-file)))

(defun wl-score-edit-file (file)
  "Edit a score FILE."
  (interactive
   (list (read-file-name "Edit score file: " wl-score-files-directory)))
  (when (wl-collect-summary)
    (wl-score-save))
  (let ((winconf (current-window-configuration))
	(edit-buffer (wl-as-mime-charset wl-score-mode-mime-charset
		       (find-file-noselect file)))
	(sum-buf (current-buffer)))
    (if (string-match (concat "^" wl-summary-buffer-name) (buffer-name))
	(let ((cur-buf (current-buffer)))
	  (when wl-message-buffer
	    (wl-message-select-buffer wl-message-buffer)
	    (delete-window)
	    (select-window (get-buffer-window cur-buf)))
	  (wl-message-select-buffer edit-buffer))
      (switch-to-buffer edit-buffer))
    (wl-score-mode)
    (setq wl-score-edit-exit-function 'wl-score-edit-done)
    (setq wl-score-edit-summary-buffer sum-buf)
    (make-local-variable 'wl-prev-winconf)
    (setq wl-prev-winconf winconf))
  (message
   (substitute-command-keys
    "\\<wl-score-mode-map>\\[wl-score-edit-exit] to save edits")))

;; score-mode

(defvar wl-score-edit-summary-buffer nil)

(defvar wl-score-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?| "w" table)
    table)
  "Syntax table used in score-mode buffers.")

(defvar wl-score-mode-map nil)
(defvar wl-score-mode-menu-spec
  '("Score"
    ["Exit" wl-score-edit-exit t]
    ["Insert date" wl-score-edit-insert-date t]
    ["Format" wl-score-pretty-print t]))

(unless wl-score-mode-map
  (setq wl-score-mode-map (copy-keymap emacs-lisp-mode-map))
  (define-key wl-score-mode-map "\C-c\C-k" 'wl-score-edit-kill)
  (define-key wl-score-mode-map "\C-c\C-c" 'wl-score-edit-exit)
  (define-key wl-score-mode-map "\C-c\C-p" 'wl-score-pretty-print)
  (define-key wl-score-mode-map "\C-c\C-d" 'wl-score-edit-insert-date)
  (define-key wl-score-mode-map "\C-c\C-s" 'wl-score-edit-insert-header)
  (define-key wl-score-mode-map "\C-c\C-e" 'wl-score-edit-insert-header-entry)

  (unless (boundp 'wl-score-menu)
    (easy-menu-define
     wl-score-menu wl-score-mode-map "Menu used in score mode."
     wl-score-mode-menu-spec)))

(defun wl-score-mode ()
  "Mode for editing Wanderlust score files.
This mode is an extended emacs-lisp mode.

Special commands;
\\{wl-score-mode-map}
Entering Score mode calls the value of `wl-score-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map wl-score-mode-map)
  (set-syntax-table wl-score-mode-syntax-table)
  (setq major-mode 'wl-score-mode)
  (setq mode-name "Score")
  (lisp-mode-variables nil)
  (make-local-variable 'wl-score-edit-exit-function)
  (make-local-variable 'wl-score-edit-summary-buffer)
  (run-hooks 'emacs-lisp-mode-hook 'wl-score-mode-hook))

(defun wl-score-edit-insert-date ()
  "Insert date in numerical format."
  (interactive)
  (princ (elmo-time-to-days (current-time)) (current-buffer)))

(defun wl-score-pretty-print ()
  "Format the current score file."
  (interactive)
  (goto-char (point-min))
  (let ((form (read (current-buffer))))
    (erase-buffer)
    (let ((emacs-lisp-mode-syntax-table wl-score-mode-syntax-table)
	  print-length print-level)
      (pp form (current-buffer))))
  (goto-char (point-min)))

(defun wl-score-edit-exit ()
  "Stop editing the score file."
  (interactive)
  (unless (file-exists-p (file-name-directory (buffer-file-name)))
    (elmo-make-directory (file-name-directory (buffer-file-name))))
  (if (zerop (buffer-size))
      (progn
	(set-buffer-modified-p nil)
	(and (file-exists-p (buffer-file-name))
	     (delete-file (buffer-file-name))))
    (wl-as-mime-charset wl-score-mode-mime-charset
      (save-buffer)))
  (let ((buf (current-buffer)))
    (when wl-score-edit-exit-function
      (funcall wl-score-edit-exit-function))
    (kill-buffer buf)))

(defun wl-score-edit-kill ()
  "Cancel editing the score file."
  (interactive)
  (let ((buf (current-buffer)))
    (set-buffer-modified-p nil)
    (when wl-score-edit-exit-function
      (funcall wl-score-edit-exit-function))
    (kill-buffer buf)))

(defun wl-score-edit-get-summary-buf ()
  (let ((summary-buf (and wl-score-edit-summary-buffer
			  (get-buffer wl-score-edit-summary-buffer))))
    (if (and summary-buf
	     (buffer-live-p summary-buf))
	summary-buf
      (if (and (setq summary-buf (window-buffer (previous-window)))
	       (string-match (concat "^" wl-summary-buffer-name)
			     (buffer-name summary-buf)))
	  summary-buf))))

(defun wl-score-edit-get-header (header &optional extra)
  (let ((sum-buf (wl-score-edit-get-summary-buf))
	(index (nth 2 (assoc header wl-score-header-index))))
    (when (and sum-buf index)
      (with-current-buffer sum-buf
	(wl-score-get-header header extra)))))

(defun wl-score-edit-insert-number ()
  (interactive)
  (let ((sum-buf (wl-score-edit-get-summary-buf))
	num)
    (when sum-buf
      (if (setq num (with-current-buffer sum-buf
		      (wl-summary-message-number)))
	  (prin1 num (current-buffer))))))

(defun wl-score-edit-insert-header ()
  (interactive)
  (let (hchar entry)
    (unwind-protect
	(progn
	  (while (not hchar)
	    (message "Insert header (%s?): "
		     (mapconcat (lambda (s) (char-to-string (car s)))
				wl-score-edit-header-char ""))
	    (setq hchar (read-char))
	    (when (or (= hchar ??) (= hchar ?\C-h))
	      (setq hchar nil)
	      (wl-score-insert-help "Match on header"
				    wl-score-edit-header-char 1)))
	  (wl-score-kill-help-buffer)
	  (unless (setq entry (assq (downcase hchar)
				    wl-score-edit-header-char))
	    (error "Invalid match type")))
      (message "")
      (wl-score-kill-help-buffer)
      (let* ((header (nth 1 entry))
	     (value (wl-score-edit-get-header header)))
	(and value (prin1 value (current-buffer)))))))

(defun wl-score-edit-insert-header-entry ()
  (interactive)
  (let (form entry)
    (goto-char (point-min))
    (setq form (and (not (zerop (buffer-size)))
		    (condition-case ()
			(read (current-buffer))
		      (error "Invalid syntax"))))
    (setq entry (wl-score-get-header-entry 'wl-score-edit-get-header))
    (unless (eq (nth 2 (nth 1 entry)) 'now)
      (if form
	  (wl-score-update-score-entry (car entry) (nth 1 entry) form)
	(setq form (list entry)))
      (erase-buffer)
      (let ((emacs-lisp-mode-syntax-table wl-score-mode-syntax-table)
	    print-length print-level)
	(pp form (current-buffer)))
      (goto-char (point-min)))))

(require 'product)
(product-provide (provide 'wl-score) (require 'wl-version))

;;; wl-score.el ends here

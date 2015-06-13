;;; wl-address.el --- Tiny address management for Wanderlust.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 1998,1999,2000 Shun-ichi GOTO <gotoh@taiyo.co.jp>
;; Copyright (C) 1998,1999,2000 Takeshi Chiba <chiba@d3.bs1.fc.nec.co.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Shun-ichi GOTO <gotoh@taiyo.co.jp>
;;	Takeshi Chiba <chiba@d3.bs1.fc.nec.co.jp>
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
;;

;;; Code:
;;

(require 'wl-util)
(require 'wl-vars)
(require 'std11)
(eval-when-compile (require 'cl))

(defvar wl-address-complete-header-list
  '("To:" "From:" "Cc:" "Bcc:" "Mail-Followup-To:" "Reply-To:"
    "Return-Receipt-To:"))
(defvar wl-address-complete-header-regexp nil) ; auto-generated.
(defvar wl-newsgroups-complete-header-regexp "^\\(Newsgroups\\|Followup-To\\):")
(defvar wl-folder-complete-header-regexp "^\\(Fcc\\):")
(defvar wl-address-list nil)
(defvar wl-address-completion-list nil)
(defvar wl-address-petname-hash nil)
(defvar wl-address-enable-strict-loading t)

(defvar wl-address-ldap-search-hash nil)

(eval-when-compile (require 'pldap))

(defvar wl-ldap-alias-dn-level nil
"Level of dn data to make alias postfix.
Valid value is nit, t, 1 or larget integer.

If this value nil, minimum alias postfix is made depends on uniqness
with other candidates.  In this implementation, it's same to 1.  If t,
always append all dn data.  If number, always append spcified level of
data but maybe appended more uniqness.  If invalid value, treat as
nil.

For example, following dn data is exsist, alias of each level is shown
bellow.

Match: Goto
dn: CN=Shun-ichi GOTO,OU=Mew,OU=Emacs,OU=Lisper,O=Programmers Inc.
  nil => Goto/Shun-ichi_GOTO
    1 => Goto/Shun-ichi_GOTO
    2 => Goto/Shun-ichi_GOTO/Mew
    3 => Goto/Shun-ichi_GOTO/Mew/Emacs
    4 => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper
    5 => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper/Programmers_Inc_
    6 => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper/Programmers_Inc_
    t => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper/Programmers_Inc_

If level 3 is required for uniqness with other candidates,
  nil => Goto/Shun-ichi_GOTO/Mew/Emacs    ... appended more
    1 => Goto/Shun-ichi_GOTO/Mew/Emacs    ... appended more
    2 => Goto/Shun-ichi_GOTO/Mew/Emacs    ... appended more
    3 => Goto/Shun-ichi_GOTO/Mew/Emacs
    4 => Goto/Shun-ichi_GOTO/Mew/Emacs/Lisper
    (so on...)")

(defconst wl-ldap-alias-sep "/")

(defconst wl-ldap-search-attribute-type-list
  '("sn" "cn" "mail" "email"))

(defun wl-ldap-get-value (type entry)
  ""
  (let* ((values (cdr (assoc type entry)))
	 (ret (car values)))
    (if (and ret (not ldap-ignore-attribute-codings))
	(while values
	  (if (not (string-match "^[\000-\177]*$" (car values)))
	      (setq ret (car values)
		    values nil)
	    (setq values (cdr values)))))
    ret))

(defun wl-ldap-get-value-list (type entry)
  ""
  (cdr (assoc type entry)))

(defun wl-ldap-make-filter (pat type-list)
  "Make RFC1558 quiery filter for PAT from ATTR-LIST.
Each are \"OR\" combination, and PAT is beginning-match."
  (concat "(|"
	  (mapconcat (lambda (x) (format "(%s=%s*)" x pat)) ; fixed format
		     type-list
		     "")
	  ")"))

(defun wl-ldap-make-matched-value-list (regexp type-list entry)
  "Correct matching WORD with value of TYPE-LIST in ENTRY.
Returns matched uniq string list."
  (let (type val values result)
    ;; collect matching value
    (while entry
      (setq type (car (car entry))
	    values (mapcar (function wl-ldap-alias-safe-string)
			   (cdr (car entry)))
	    values (elmo-flatten values)
	    entry (cdr entry))
      (if (member type type-list)
	  (while values
	    (setq val (car values)
		  values (cdr values))
	    (if (and (string-match regexp val)
		     (not (member val result)))
		(setq result (cons val result))))))
    result))

(defun wl-ldap-alias-safe-string (str)
  "Modify STR for alias.
Replace space/tab in STR into '_' char.
Replace '@' in STR into list of mailbox and sub-domains."
  (while (string-match "[ \t]+" str)
    (setq str (concat (substring str 0 (match-beginning 0))
		      "_"
		      (substring str (match-end 0)))))
  (if (string-match "\\(@\\)[^/@]+" str)
      (setq str (split-string str  "[@\\.]")))
  str)

(defun wl-ldap-register-dn-string (hash dn &optional str dn-list)
  ""
  (let (sym dnsym value level)
    (setq dnsym (intern (upcase dn) hash))
    (if (and (null str) (boundp dnsym))
	()					; already processed
      ;; make dn-list in fisrt time
      (if (null dn-list)
	  (let ((case-fold-search t))
	    (setq dn-list (mapcar (lambda (str)
				    (if (string-match "[a-z]+=\\(.*\\)" str)
					(wl-ldap-alias-safe-string
					 (wl-match-string 1 str))))
				  (split-string dn "[ \t]*,[ \t]*")))))
      (setq dn-list (elmo-flatten dn-list))
      ;; prepare candidate for uniq str
      (if str
	  (setq str (concat str wl-ldap-alias-sep (car dn-list))
		dn-list (cdr dn-list))
	;; first entry, pre-build with given level
	(cond
	 ((null wl-ldap-alias-dn-level) (setq level 1))
	 ((eq t wl-ldap-alias-dn-level) (setq level 1000)) ; xxx, big enough
	 ((numberp wl-ldap-alias-dn-level)
	  (if (< 0 wl-ldap-alias-dn-level)
	      (setq level  wl-ldap-alias-dn-level)
	    (setq level 1)))
	 (t
	  (setq level 1)))
	(while (and (< 0 level) dn-list)
	  (if (null str)
	      (setq str (car dn-list))
	    (setq str (concat str wl-ldap-alias-sep (car dn-list))))
	  (setq level (1- level)
		dn-list (cdr dn-list))))
      (setq sym (intern (upcase str) hash))
      (if (not (boundp sym))
	  ;; good
	  (progn (set sym (list dn str dn-list))
		 (set dnsym str))
	;; conflict
	(if (not (eq (setq value (symbol-value sym)) t))
	    ;; move away deeper
	    (progn (set sym t)
		   (apply (function wl-ldap-register-dn-string) hash value)))
	(wl-ldap-register-dn-string hash dn str dn-list)))))

(defun wl-address-ldap-search (pattern cl)
  "Make address completion-list matched for PATTERN by LDAP search.
Matched address lists are append to CL."
  (require 'pldap)
  (unless wl-address-ldap-search-hash
    (setq wl-address-ldap-search-hash (elmo-make-hash 7)))
  (let ((pat (if (string-match wl-ldap-alias-sep pattern)
		 (substring pattern 0 (match-beginning 0))
	       pattern))
	(ldap-default-host (or wl-ldap-server ldap-default-host "localhost"))
	(ldap-default-port (or wl-ldap-port ldap-default-port 389))
	(ldap-default-base (or wl-ldap-base ldap-default-base))
	(dnhash (elmo-make-hash))
	cache len sym tmpl regexp entries ent values dn dnstr alias
	result cn mails)
    ;; check cache
    (mapatoms (lambda (atom)
		(if (and (string-match
			  (concat "^" (symbol-name atom) ".*") pat)
			 (or (null cache)
			     (< (car cache)
				(setq len (length (symbol-name atom))))))
		    (setq cache (cons
				 (or len (length (symbol-name atom)))
				 (symbol-value atom)))))
	      wl-address-ldap-search-hash)
    ;; get matched entries
    (if cache
	(setq entries (cdr cache))
      (ignore-errors
	(message "Searching in LDAP...")
	(setq entries (ldap-search-entries
		       (wl-ldap-make-filter
			pat wl-ldap-search-attribute-type-list)
		       nil wl-ldap-search-attribute-type-list nil t))
	(message "Searching in LDAP...done")
	(elmo-set-hash-val pattern entries wl-address-ldap-search-hash)))
    ;;
    (setq tmpl entries)
    (while tmpl
      (wl-ldap-register-dn-string dnhash (car (car tmpl))) ; car is 'dn'.
      (setq tmpl (cdr tmpl)))
    ;;
    (setq regexp (concat "^" pat))
    (while entries
      (setq ent (cdar entries)
	    values (wl-ldap-make-matched-value-list
		    regexp wl-ldap-search-attribute-type-list
		    ent)
	    mails (or (wl-ldap-get-value-list "mail" ent)
		      (wl-ldap-get-value-list "email" ent))
	    cn (wl-ldap-get-value "cn" ent)
	    dn (car (car entries))
	    dnstr (elmo-get-hash-val (upcase dn) dnhash))
      ;; make alias list generated from LDAP data.
      (while (and mails values)
	;; make alias like MATCHED/DN-STRING
	(if (not (string-match (concat "^" (regexp-quote (car values))) dnstr))
	    (setq alias (concat (car values) wl-ldap-alias-sep dnstr))
	  ;; use DN-STRING if DN-STRING begin with MATCHED
	  (setq alias dnstr))
	;; check uniqness then add to list
	(setq sym (intern (downcase alias) dnhash))
	(when (not (boundp sym))
	  (set sym alias)
	  (setq result (cons (cons alias
				   (concat cn " <" (car mails) ">"))
			     result)))
	(setq values (cdr values)))
      ;; make mail addrses list
      (while mails
	(if (null (assoc (car mails) cl)); Not already in cl.
;;;	    (string-match regexp (car mails))
	    ;; add mail address itself to completion list
	    (setq result (cons (cons (car mails)
				     (concat cn " <" (car mails) ">"))
			       result)))
	(setq mails (cdr mails)))
      (setq entries (cdr entries)))
    (append result cl)))

(defun wl-complete-address (string predicate flag)
  "Completion function for completing-read (comma separated addresses)."
  (if (string-match "^\\(.*,\\)\\(.*\\)$" string)
      (let* ((str1 (match-string 1 string))
	     (str2 (match-string 2 string))
	     (str2-comp (wl-complete-address str2 predicate flag)))
	(if (and (not flag) (stringp str2-comp))
	    (concat str1 str2-comp)
	  str2-comp))
    (if (not flag)
	(try-completion string wl-address-list)
      (all-completions string wl-address-list))))

(defalias 'wl-address-quote-specials 'elmo-address-quote-specials)

(defun wl-address-make-completion-list (address-list)
  (let (addr-tuple cl)
    (while address-list
      (setq addr-tuple (car address-list))
      (setq cl
	    (cons
	     (wl-address-make-completion-entry 0 addr-tuple)
	     cl))
      ;; nickname completion.
      (if wl-address-enable-strict-loading
	  (unless (or (equal (nth 1 addr-tuple) (nth 0 addr-tuple))
		      ;; already exists
		      (assoc (nth 1 addr-tuple) cl))
	    (setq cl
		  (cons
		   (wl-address-make-completion-entry 1 addr-tuple)
		   cl)))
	(setq cl
	      (cons
	       (wl-address-make-completion-entry 1 addr-tuple)
	       cl)))
      (setq address-list (cdr address-list)))
    cl))

(defun wl-address-make-completion-entry (index addr-tuple)
  (cons (nth index addr-tuple)
	(if (or (string= (nth 2 addr-tuple) "")
		(string-match ".*:.*;$" (nth 0 addr-tuple)))
	    (nth 0 addr-tuple)
	  (concat
	   (wl-address-quote-specials
	    (nth 2 addr-tuple)) " <"(nth 0 addr-tuple)">"))))

(defun wl-complete-field-body-or-tab ()
  (interactive)
  (let ((case-fold-search t)
	epand-char skip-chars
	(use-ldap nil)
	completion-list)
    (if (wl-draft-on-field-p)
	(wl-complete-field)
      (if (and
	   (< (point)
	      (save-excursion
		(goto-char (point-min))
		(search-forward (concat "\n" mail-header-separator "\n") nil 0)
		(point)))
	   (save-excursion
	     (beginning-of-line)
	     (setq use-ldap nil)
	     (while (and (looking-at "^[ \t]")
			 (not (= (point) (point-min))))
	       (forward-line -1))
	     (cond ((looking-at wl-address-complete-header-regexp)
		    (setq completion-list wl-address-completion-list)
		    (if wl-use-ldap
			(setq use-ldap t))
		    (setq epand-char ?@))
		   ((looking-at wl-folder-complete-header-regexp)
		    (setq completion-list wl-folder-entity-hashtb)
		    (setq skip-chars "^, "))
		   ((looking-at wl-newsgroups-complete-header-regexp)
		    (setq completion-list wl-folder-newsgroups-hashtb)))))
	  (wl-complete-field-body completion-list
				  epand-char skip-chars use-ldap)
	(indent-for-tab-command)))))

(defvar wl-completion-buf-name "*Completions*")

(defvar wl-complete-candidates nil)

(defun wl-complete-window-show (all)
  (if (and (get-buffer-window wl-completion-buf-name)
	   (equal wl-complete-candidates all))
      (let ((win (get-buffer-window wl-completion-buf-name)))
	(with-current-buffer wl-completion-buf-name
	  (if (pos-visible-in-window-p (point-max) win)
	      (set-window-start win 1)
	    (scroll-other-window))))
    (message "Making completion list...")
    (setq wl-complete-candidates all)
    (with-output-to-temp-buffer
	wl-completion-buf-name
      (display-completion-list all))
    (message "Making completion list...done")))

(defun wl-complete-window-delete ()
  (let (comp-buf comp-win)
    (if (setq comp-buf (get-buffer wl-completion-buf-name))
	(if (setq comp-win (get-buffer-window comp-buf))
	    (delete-window comp-win)))))

(defun wl-complete-field ()
  (interactive)
  (let* ((end (point))
	 (start (save-excursion
		  (skip-chars-backward "_a-zA-Z0-9+@%.!\\-")
		  (point)))
	 (completion)
	 (pattern (buffer-substring start end))
	 (cl wl-draft-field-completion-list))
    (if (null cl)
	nil
      (setq completion
	    (let ((completion-ignore-case t))
	      (try-completion pattern cl)))
      (cond ((eq completion t)
	     (let ((alias (assoc pattern cl)))
	       (if alias
		   (progn
		     (delete-region start end)
		     (insert (cdr alias))
;;;		     (wl-highlight-message (point-min)(point-max) t)
		     )))
	     (wl-complete-window-delete))
	    ((null completion)
	     (message "Can't find completion for \"%s\"" pattern)
	     (ding))
	    ((not (string= pattern completion))
	     (delete-region start end)
	     (insert completion)
	     (wl-complete-window-delete)
	     (wl-highlight-message (point-min)(point-max) t))
	    (t
	     (let ((list (all-completions pattern cl)))
	       (wl-complete-window-show list)))))))

(defun wl-complete-insert (start end pattern completion-list)
  (let ((alias (and (consp completion-list)
		    (assoc pattern completion-list)))
	comp-buf comp-win)
    (if alias
	(progn
	  (delete-region start end)
	  (insert (cdr alias))
	  (if (setq comp-buf (get-buffer wl-completion-buf-name))
	      (if (setq comp-win (get-buffer-window comp-buf))
		  (delete-window comp-win)))))))

(defun wl-complete-field-body (completion-list
			       &optional epand-char skip-chars use-ldap)
  (interactive)
  (let* ((end (point))
	 (start (save-excursion
		  (skip-chars-backward (or skip-chars "^:,>\n"))
		  (skip-chars-forward " \t")
		  (point)))
	 (completion)
	 (pattern (buffer-substring start end))
	 (len (length pattern))
	 (completion-ignore-case t)
	 (cl completion-list))
    (when use-ldap
      (setq cl (wl-address-ldap-search pattern cl)))
    (if (null cl)
	nil
      (setq completion (try-completion pattern cl))
      (cond ((eq completion t)
	     (if use-ldap (setq wl-address-ldap-search-hash nil))
	     (wl-complete-insert start end pattern cl)
	     (wl-complete-window-delete)
	     (message "Sole completion"))
	    ((and epand-char
		  (> len 0)
		  (or (char-equal (aref pattern (1- len)) epand-char)
		      (char-equal (aref pattern (1- len)) (string-to-char " ")))
		  (assoc (substring pattern 0 (1- len)) cl))
	     (wl-complete-insert
	      start end
	      (substring pattern 0 (1- len))
	      cl))
	    ((null completion)
	     (message "Can't find completion for \"%s\"" pattern)
	     (ding))
	    ((not (string= pattern completion))
	     (delete-region start end)
	     (insert completion))
	    (t
	     (let ((list (sort (all-completions pattern cl) 'string<)))
	       (wl-complete-window-show list)))))))

(defvar wl-address-init-function 'wl-local-address-init)

(defun wl-address-init ()
  "Call `wl-address-init-function'."
  (funcall wl-address-init-function))

(defun wl-local-address-init ()
  "Reload `wl-address-file'.
Refresh `wl-address-list', `wl-address-completion-list', and
`wl-address-petname-hash'."
  (message "Updating addresses...")
  (setq wl-address-list
	(wl-address-make-address-list wl-address-file))
  (setq wl-address-completion-list
	(wl-address-make-completion-list wl-address-list))
  (if (file-readable-p wl-alias-file)
      (setq wl-address-completion-list
	    (append wl-address-completion-list
		    (wl-address-make-alist-from-alias-file wl-alias-file))))
  (setq wl-address-petname-hash (elmo-make-hash))
  (let ((addresses wl-address-list))
    (while addresses
      (elmo-set-hash-val (downcase (car (car addresses)))
			 (cadr (car addresses))
			 wl-address-petname-hash)
      (setq addresses (cdr addresses))))
  (message "Updating addresses...done"))


(defun wl-address-expand-aliases (alist nest-count)
  (when (< nest-count 5)
    (let (expn-str new-expn-str expn new-expn(n 0) (expanded nil))
      (while (setq expn-str (cdr (nth n alist)))
	(setq new-expn-str nil)
	(while (string-match "^[ \t]*\\([^,]+\\)" expn-str)
	  (setq expn (match-string 1 expn-str))
	  (setq expn-str (wl-string-delete-match expn-str 0))
	  (if (string-match "^[ \t,]+" expn-str)
	      (setq expn-str (wl-string-delete-match expn-str 0)))
	  (if (string-match "[ \t,]+$" expn)
	      (setq expn (wl-string-delete-match expn 0)))
	  (setq new-expn (cdr (assoc expn alist)))
	  (if new-expn
	      (setq expanded t))
	  (setq new-expn-str (concat new-expn-str (and new-expn-str ", ")
				     (or new-expn expn))))
	(when new-expn-str
	  (setcdr (nth n alist) new-expn-str))
	(setq n (1+ n)))
      (and expanded
	   (wl-address-expand-aliases alist (1+ nest-count))))))

(defun wl-address-make-alist-from-alias-file (file)
  (with-temp-buffer
    (let ((case-fold-search t)
	  alias expn alist)
      (insert-file-contents file)
      (while (re-search-forward ",$" nil t)
	(end-of-line)
	(forward-char)
	(delete-char -1))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^#;\n][^:]+\\):[ \t]*\\(.*\\)$" nil t)
	(setq alias (wl-match-buffer 1)
	      expn (wl-match-buffer 2))
	(setq alist (cons (cons alias expn) alist)))
      (wl-address-expand-aliases alist 0)
      (nreverse alist) ; return value
      )))

(defun wl-address-make-address-list (path)
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (let (ret
	    (coding-system-for-read wl-cs-autoconv))
	(insert-file-contents path)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "\
^\\([^#\n][^ \t\n]+\\)[ \t]+\\(\".*\"\\)[ \t]+\\(\".*\"\\)[ \t]*.*$")
	      (setq ret
		    (cons
		     (list (wl-match-buffer 1)
			   (read (wl-match-buffer 2))
			   (read (wl-match-buffer 3)))
		     ret)))
	  (forward-line))
	(nreverse ret)))))


(defsubst wl-address-header-extract-address (str)
  "Extracts a real e-mail address from STR and return it.
e.g. \"Mine Sakurai <m-sakura@example.org>\"
  ->  \"m-sakura@example.org\".
e.g. \"m-sakura@example.org (Mine Sakurai)\"
  ->  \"m-sakura@example.org\"."
  (cond ((string-match ".*<\\([^>]*\\)>" str) ; .* to extract last <>
	 (wl-match-string 1 str))
	((string-match "\\([^ \t\n]*@[^ \t\n]*\\)" str)
	 (wl-match-string 1 str))
	(t str)))

(defsubst wl-address-header-extract-realname (str)
  "Extracts a real name from STR and return it.
e.g. \"Mr. bar <hoge@example.com>\"
  ->  \"Mr. bar\"."
  (cond ((string-match "\\(.*[^ \t]\\)[ \t]*<[^>]*>" str)
	 (wl-match-string 1 str))
	(t "")))


(defun wl-address-get-petname-1 (string)
  (let ((address (downcase (wl-address-header-extract-address string))))
    (elmo-get-hash-val address wl-address-petname-hash)))

(defsubst wl-address-get-petname (string)
  (or (wl-address-get-petname-1 string)
      string))

(defun wl-address-user-mail-address-p (address)
  "Judge whether ADDRESS is user's or not."
  (if wl-user-mail-address-regexp
      (string-match wl-user-mail-address-regexp
		    (wl-address-header-extract-address address))
    (member (downcase (wl-address-header-extract-address address))
	    (or (mapcar 'downcase wl-user-mail-address-list)
		(list (downcase
		       (wl-address-header-extract-address
			wl-from)))))))

(defun wl-address-delete-user-mail-addresses (address-list)
  "Delete user mail addresses from list by side effect.
Deletion is done by using `elmo-list-delete'."
  (if wl-user-mail-address-regexp
      (elmo-list-delete (list wl-user-mail-address-regexp) address-list
			(lambda (elem list)
			  (elmo-delete-if
			   (lambda (item) (string-match elem item))
			   list)))
    (let ((myself (or wl-user-mail-address-list
		      (list (wl-address-header-extract-address wl-from)))))
      (elmo-list-delete myself address-list
			(lambda (elem list)
			  (elmo-delete-if
			   (lambda (item) (string= (downcase elem)
						   (downcase item)))
			   list))))))

(defmacro wl-address-concat-token (string token)
  `(cond
    ((eq 'quoted-string (car ,token))
     (concat ,string "\"" (cdr ,token) "\""))
    ((eq 'comment (car ,token))
     (concat ,string "(" (cdr ,token) ")"))
    (t
     (concat ,string (cdr ,token)))))

(defun wl-address-string-without-group-list-contents (sequence)
  "Return address string from lexical analyzed list SEQUENCE.
Group list contents is not included."
  (let (address-string route-addr-end token seq group-end)
  (while sequence
    (setq token (car sequence))
    (cond
     ;;   group       =  phrase ":" [#mailbox] ";"
     ((and (eq 'specials (car token))
	   (string= (cdr token) ":"))
      (setq address-string (concat address-string (cdr token))) ; ':'
      (setq seq (cdr sequence))
      (setq token (car seq))
      (setq group-end nil)
      (while (not group-end)
	(setq token (car seq))
	(setq seq (cdr seq))
	(setq group-end (and (eq 'specials (car token))
			     (string= (cdr token) ";"))))
      (setq address-string (concat address-string (cdr token))) ; ';'
      (setq sequence seq))
     ;;   route-addr  =  "<" [route] addr-spec ">"
     ;;   route       =  1#("@" domain) ":"           ; path-relative
     ((and (eq 'specials (car token))
	   (string= (cdr token) "<"))
      (setq seq (std11-parse-route-addr sequence))
      (setq route-addr-end (car (cdr seq)))
      (while (not (eq (car sequence) route-addr-end))
	(setq address-string (wl-address-concat-token address-string
						      (car sequence)))
	(setq sequence (cdr sequence))))
     (t
      (setq address-string (wl-address-concat-token address-string token))
      (setq sequence (cdr sequence)))))
  address-string))

(defun wl-address-delete (the-email)
  "Delete address entry in the `wl-address-file'."
  (let ((output-coding-system
	 (mime-charset-to-coding-system wl-mime-charset)))
    (with-temp-buffer
      (message "Deleting Address...")
      (insert-file-contents wl-address-file)
      (delete-matching-lines (concat "^[ \t]*" the-email "[ \t]+\".*\"[ \t]+\".*\"$"))
      (write-region (point-min) (point-max)
		    wl-address-file nil 'no-msg)
      ;; Delete entries.
      (dolist (entry (elmo-string-assoc-all the-email wl-address-list))
	(setq wl-address-list (delete entry wl-address-list)))
      (elmo-set-hash-val the-email nil wl-address-petname-hash)
      (message "Deleting Address...done"))))

(defun wl-address-add-or-change (address
				 &optional default-realname
				 change-address)
  "Add address entry to `wl-address-file', if not registerd.
If already registerd, change it."
  (let ((entry (assoc address wl-address-list))
	the-realname the-petname new-addr addr-changed)
    (setq the-realname
	  (read-from-minibuffer "Real Name: " (or default-realname
						  (nth 2 entry))))
    (setq the-petname (read-from-minibuffer "Petname: "
					    (or (nth 1 entry)
						the-realname)))
    (when change-address
      (setq new-addr (read-from-minibuffer "E-Mail: " address))
      (cond
       ((or (not (stringp new-addr))
	    (string-match "^[ \t]*$" new-addr))
	(error "empty address"))
       ((and (not (string= address new-addr))
	     (assoc new-addr wl-address-list))
	(error "'%s' already exists" new-addr))
       (t
	;; do nothing
	)))
    ;; writing to ~/.address
    (let ((output-coding-system
	   (mime-charset-to-coding-system wl-mime-charset)))
      (with-temp-buffer
	(if (file-exists-p wl-address-file)
	    (insert-file-contents wl-address-file))
	(if (null entry)
	    ;; add
	    (progn
	      (goto-char (point-max))
	      (if (and (> (buffer-size) 0)
		       (not (eq (char-after (1- (point-max))) ?\n)))
		  (insert "\n")))
	  ;; override
	  (while (re-search-forward (concat "^[ \t]*" address) nil t)
	    (delete-region (point-at-bol) (1+ (point-at-eol)))))
	(insert (format "%s\t%s\t%s\n"
			(or new-addr address)
			(prin1-to-string the-petname)
			(prin1-to-string the-realname)))
	(write-region (point-min) (point-max)
		      wl-address-file nil 'no-msg)
	(wl-address-init)
	(list (or new-addr address) the-petname the-realname)))))

;; Read addresses from minibuffer with completion.
(defvar wl-address-minibuffer-history nil)
(defvar wl-address-minibuffer-local-map nil
  "Keymap to use when reading address from the minibuffer.")

(unless wl-address-minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-i"
      (lambda ()
	(interactive)
	(wl-complete-field-body wl-address-completion-list
				?@ nil wl-use-ldap)))
    (setq wl-address-minibuffer-local-map map)))

(defun wl-address-read-from-minibuffer (prompt &optional
					       initial-contents
					       default-value)
  (read-from-minibuffer prompt
			initial-contents
			wl-address-minibuffer-local-map
			nil
			'wl-address-minibuffer-history
			default-value))

(require 'product)
(product-provide (provide 'wl-address) (require 'wl-version))

;;; wl-address.el ends here



(require 'ido)

;; This file contains a couple of advices, mostly to make ido the
;; default `completing-read' function and some optimizations.

(defvar ido-hacks-orgin-completing-read-function (symbol-function 'completing-read))
(defvar ido-hacks-completing-read-recursive nil)
(defvar ido-hacks-flex-narrowed-matches-hash (make-hash-table :test 'equal))
;; Make compiler happy:
(defvar ido-directory-too-big nil)
(defvar ido-directory-nonreadable nil)
(defvar ido-directory-nonreadable nil)
(defvar ido-choice-list nil)
(defvar ido-temp-list nil)
(defvar ido-cur-item nil)

;; Use ido for `dired-do-rename', which is disabled in ido.el.  Also
;; make it possible to choose an empty string and such accepting direds
;; default.
(put 'dired-do-rename 'ido nil)
(put 'dired-do-rename 'ido-hacks-fix-default t)
(put 'dired-do-copy 'ido-hacks-fix-default t)
(put 'elp-instrument-package 'ido 'ignore)

(define-minor-mode ido-hacks-mode
  "Advices for ido-mode."
  nil nil nil :global t
  (if ido-hacks-mode
      (progn
	(ad-enable-regexp "^ido-hacks-")
	(global-set-key (kbd "M-x") 'ido-hacks-execute-extended-command))
    (global-set-key (kbd "M-x") 'execute-extended-command)
    (ad-disable-regexp "^ido-hacks-"))
  (ad-activate-regexp "^ido-hacks-"))

  
(defadvice completing-read (around ido-hacks-completing-read)
  "Advice `completing-read' to always use `ido-read-internal',
unless `this-command' has a (ido ignore) property or the
inherit-input-method argument is non-nil or the collection
argument is a function (which ido can't handle)."
  ;;(completing-read prompt collection &optional predicate require-match
  ;; initial-input hist def inherit-input-method)
  (if (or  ido-hacks-completing-read-recursive
	   (and (listp collection)
		(equal '("dummy" . 1)	;call from ido-read-internal
		       (car collection)))
	   (symbolp collection)
	   inherit-input-method
	   (eq (get this-command 'ido) 'ignore))
      
      ad-do-it
    ;; copied from ido-completing-read
    (let ((ido-hacks-completing-read-recursive t)
	  (ido-current-directory nil)
	  (ido-directory-nonreadable nil)
	  (ido-directory-too-big nil)
	  (ido-context-switch-command (or (get this-command 'ido-context-switch-command) 'ignore))
	  (ido-choice-list  (ido-hacks-make-completions collection predicate)))
      (setq ad-return-value
	    (ido-read-internal 'list prompt hist def require-match initial-input)))))
      


(defadvice ido-read-internal (around ido-hacks-ido-read-internal)
  "This advice tries to fix idos handling of default values. When
a command has a (ido-hacks-fix-default t) property, it inserts an
empty string to the front of the choices list.

Additionally it inserts the complete selected item into the
history, instead of the incomplete input."
  ;;(defun ido-read-internal (item prompt history &optional default require-match initial)
  (let (history-add-new-input
	(hook  (intern (format "ido-make-%s-list-hook" item)))
	(fix-default (get this-command 'ido-hacks-fix-default)))
    
    (clrhash ido-hacks-flex-narrowed-matches-hash)
    (if (or fix-default
	    (and prompt
		 (or (not default)
		     (equal "" default))
		 (string-match "\\(.*\\)([ \t]*default[ \t]+\\(.*\\))\\([ \t]*:[ \t]*\\)\\'"
			       prompt)))
	(cond
	 ((eq item 'list)
	  (if fix-default
	      (push "" ido-choice-list)
	    (setq default (match-string 2 prompt))))
	 (t
	  (apply 'add-hook hook
		 (list 'ido-hacks-fix-default-hook)))))
		 
    ad-do-it

    (apply 'remove-hook hook (list 'ido-hacks-fix-default-hook))
    (unless (eq history 'command-history)
      (setq history (if history
			(if (symbolp history)
			    history
			  (car history))
		      'minibuffer-history))
      (when (> (length ad-return-value) 0)
	(add-to-history history ad-return-value)))))


(defun ido-hacks-fix-default-hook ()
  (push "" ido-temp-list))


(defadvice ido-set-matches-1 (around ido-hacks-ido-set-matches-1)
  "Idos flex mechanism tends to be slow when confronted with lots of items.
 This advice makes it a good deal faster, by caching narrowed
 choices lists."
  ;; (defun ido-set-matches-1 (items &optional do-full)
  (cond
;;    ((and (eq ido-cur-item 'list)
;; 	 (equal ido-text ""))
;;     (setq ad-return-value ido-choice-list))
   (t
    (cond
     ((not ido-enable-flex-matching)
      ad-do-it)
     (t
      (let (ido-enable-flex-matching) 
	ad-do-it
	(when  (and (null ad-return-value)
		    (> (length ido-text) 1)
		    (not ido-enable-regexp))
	
	  (let* ((re (mapconcat #'regexp-quote (split-string ido-text "" t) ".*"))
		 longest-prefix valid new-hash)
	    (maphash
	     #'(lambda (k v)
		 (when (and (string-prefixp ido-text k)
			    (> (length k) (length longest-prefix)))
		   (setq longest-prefix k)
		   (setq valid v)))
	     ido-hacks-flex-narrowed-matches-hash)
	  
	    (if ido-enable-prefix
		(setq re (concat "\\`" re)))

	    (if (and valid
		     (not (eq ido-enable-prefix
			      (cdr valid))))
		(setq valid nil)
	      (setq valid (car valid)))

	    (setq new-hash (make-hash-table :test 'equal))
	    ;; The order of the cached lists is not uptodate, can't
	    ;; simply return them.
	    (mapc
	     #'(lambda (item)
		 (let ((name (ido-name item)))
		   (when (and (or (not valid)
				(gethash name valid))
			    (string-match re name))
		       (puthash item t new-hash)
		       (push item ad-return-value))))
	     items)
	    
	    (puthash ido-text
		     (cons new-hash
			   ido-enable-prefix) ;store current mode
		     ido-hacks-flex-narrowed-matches-hash)))))))))

(defun string-prefixp (string prefix &optional ignore-case)
  "Return t if PREFIX is a prefix of STRING."
  (eq t
	(compare-strings string 0 (length prefix)
			 prefix 0 (length prefix)
			 ignore-case)))


(defun ido-hacks-make-completions (collection &optional predicate) ;funcs)
  (let ((completions
	 (all-completions "" collection predicate)))
    (if (or (hash-table-p collection)
    	    (arrayp collection))
	(ido-hacks-completions-sort completions)
      completions)))

(defun ido-hacks-completions-sort (completions)
  (sort completions
	#'(lambda (k1 k2)
	      (or (< (length k1) (length k2))
		  (and (= (length k1) (length k2))
		       (string< k1 k2))))))

(defun ido-hacks-execute-extended-command (&optional arg)
  (interactive "P")
  (let (old-message command)
    (let ((ido-enable-prefix nil))
      (setq command (intern
	 (completing-read
	  (concat
	   (cond
	    ((eq '- arg) "- ")
	    ((equal arg '(4)) "C-u ")
	    (arg (format "%d " (prefix-numeric-value arg))))
	   "M-x ")
	  obarray 'commandp t nil 'extended-command-history))))

    (call-interactively command)
    (when (and suggest-key-bindings
	       (not executing-kbd-macro)
	       (symbolp command))
      (let ((binding  (where-is-internal command overriding-local-map t))
	    (timeout (if (numberp suggest-key-bindings)
			 suggest-key-bindings
		       2))
	    binding-message waited)
	(when binding
	  (message "%s"
	   (concat "You can run the command "
		   (propertize (format "`%s'" (symbol-name command)) 'face 'font-lock-type-face)
		   "with <"
		   (propertize (ignore-errors
				 (substring (ido-hacks-get-keys (symbol-name command)) 1 -1))
			       'face 'font-lock-keyword-face)
		   ">")))))))

(defun ido-hacks-get-keys (func-name)
  "Return strings naming keys bound to `func-name', or nil if none.
Examines the prior, not current, buffer, presuming that current buffer
is minibuffer. (Stolen from icomplete.)"
  (if (commandp func-name)
      (save-excursion
	(let* ((sym (intern func-name))
	       (buf (other-buffer nil t))
	       (keys (with-current-buffer buf (where-is-internal sym))))
	  (if keys
	      (concat "<"
		      (mapconcat 'key-description
				 (sort keys
				       #'(lambda (x y)
					   (< (length x) (length y))))
				 ", ")
		      ">"))))))


(defun ido-completions (name candidates predicate require-match)
  ;; Return the string that is displayed after the user's text.
  ;; Modified from `icomplete-completions'.
  ;; Redefined for sake of performance by ido-hacks.
  (let* ((comps ido-matches)
	 (ind (and (consp (car comps)) (> (length (cdr (car comps))) 1)
		   ido-merged-indicator))
	 first)

    (if (and ind ido-use-faces)
	(put-text-property 0 1 'face 'ido-indicator ind))

    (if (and ido-use-faces comps)
	(let* ((fn (ido-name (car comps)))
	       (ln (length fn)))
	  (setq first (format "%s" fn))
	  (put-text-property 0 ln 'face
			     (if (= (length comps) 1)
                                 (if ido-incomplete-regexp
                                     'ido-incomplete-regexp
                                   'ido-only-match)
			       'ido-first-match)
			     first)
	  (if ind (setq first (concat first ind)))
	  (setq comps (cons first (cdr comps)))))

    (cond ((null comps)
	   (cond
	    (ido-directory-nonreadable
	     (or (nth 8 ido-decorations) " [Not readable]"))
	    (ido-directory-too-big
	     (or (nth 9 ido-decorations) " [Too big]"))
	    (ido-report-no-match
	     (nth 6 ido-decorations))  ;; [No match]
	    (t "")))
	  (ido-incomplete-regexp
           (concat " " (car comps)))
	  ((null (cdr comps))		;one match
	   (concat (if (if (not ido-enable-regexp)
                           (= (length (ido-name (car comps))) (length name))
                         ;; We can't rely on the length of the input
                         ;; for regexps, so explicitly check for a
                         ;; complete match
                         (string-match name (ido-name (car comps)))
                         (string-equal (match-string 0 (ido-name (car comps)))
                                       (ido-name (car comps))))
                       ""
                     ;; when there is one match, show the matching file name in full
                     (concat (nth 4 ido-decorations)  ;; [ ... ]
                             (ido-name (car comps))
                             (nth 5 ido-decorations)))
		   (if (not ido-use-faces) (nth 7 ido-decorations))))  ;; [Matched]
	  (t				;multiple matches
	   (let ((items (if (> ido-max-prospects 0) (1+ ido-max-prospects) 999))
		 alternatives)

	     ;; ---------------------------
	     (dotimes (i (min items (length comps)))
	       (let* ((com (ido-name (nth i comps)))
		      (str (copy-sequence com)))
		 (if (= i (1- items))
		     (push (nth 3 ido-decorations) alternatives)
		   (push  (or ido-separator (nth 2 ido-decorations)) ; " | "
			  alternatives)
		   (if (and ido-use-faces
			    (not (string= str first))
			    (ido-final-slash str))
		       (put-text-property 0 (length str) 'face 'ido-subdir str))
		   (push str alternatives))))
	     ;; ---------------------------
	     
	     (concat
	      ;; put in common completion item -- what you get by pressing tab
	      (if (and (stringp ido-common-match-string)
		       (> (length ido-common-match-string) (length name)))
		  (concat (nth 4 ido-decorations) ;; [ ... ]
			  (substring ido-common-match-string (length name))
			  (nth 5 ido-decorations)))
	      ;; list all alternatives
	      (nth 0 ido-decorations) ;; { ... }
	      (apply 'concat (cdr (nreverse alternatives)))
	      (nth 1 ido-decorations)))))))


(provide 'ido-hacks)

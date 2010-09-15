;;; haskell-indentation.el -- indentation module for Haskell Mode

;; Copyright 2009 Kristof Bastiaensen

;; Author: 2009 Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Installation:
;;
;; To turn indentation on for all Haskell buffers under Haskell mode
;; <http://www.haskell.org/haskell-mode/> add this to .emacs:
;;
;;    (add-hook haskell-mode-hook 'turn-on-haskell-indentation)
;;
;; Otherwise, call `haskell-indentation-mode'.
;;

;;; Code:

(require 'syntax nil t)			; Emacs 21 add-on

(defgroup haskell-indentation nil
  "Haskell indentation."
  :group 'haskell
  :prefix "haskell-indentation-")

(defcustom haskell-indentation-cycle-warn t
  "Warn before moving to the leftmost indentation, if you tab at the rightmost one."
  :type 'boolean
  :group 'haskell-indentation)

(defcustom haskell-indentation-layout-offset 2
  "Extra indentation to add before expressions in a haskell layout list."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-starter-offset 1
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'haskell-indentation)

(defcustom haskell-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'haskell-indentation)

(defcustom  haskell-indentation-ifte-offset 2
  "Extra indentation after the keywords `if' `then' or `else'."
  :type 'integer
  :group 'haskell-indentation)

;; Avoid a global bogus definition (which the original run-time
;; `defun' made), and support Emacs 21 without the syntax.el add-on.
(eval-when-compile
  (unless (fboundp 'syntax-ppss)
    (defsubst syntax-ppss (&rest pos)
      (parse-partial-sexp (point-min) (or pos (point))))))

(defconst haskell-indentation-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [?\r] 'haskell-newline-and-indent)
    (define-key keymap [backspace] 'haskell-indentation-delete-backward-char)
    (define-key keymap [?\C-d] 'haskell-indentation-delete-char)
    keymap))

;;;###autoload
(define-minor-mode haskell-indentation-mode
  "Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode."
  :lighter " Ind"
  :keymap haskell-indentation-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'normal-auto-fill-function)
  (when haskell-indentation-mode
    (setq max-lisp-eval-depth (max max-lisp-eval-depth 600)) ;; set a higher limit for recursion
    (set (make-local-variable 'indent-line-function)
         'haskell-indentation-indent-line)
    (set (make-local-variable 'normal-auto-fill-function)
         'haskell-indentation-auto-fill-function)
    (set (make-local-variable 'haskell-indent-last-position)
         nil)))

(defun turn-on-haskell-indentation ()
  "Turn on the haskell-indentation minor mode."
  (interactive)
  (haskell-indentation-mode t))

(put 'parse-error
     'error-conditions
     '(error parse-error))
(put 'parse-error 'error-message "Parse error")

(defun parse-error (&rest args)
  (signal 'parse-error (apply 'format args)))

(defmacro on-parse-error (except &rest body)
  `(condition-case parse-error-string
       (progn ,@body)
     (parse-error
      ,except
      (message "%s" (cdr parse-error-string)))))

(defun haskell-current-column ()
  "Compute current column according to haskell syntax rules,
  correctly ignoring composition."
  (save-excursion
    (let ((start (point))
          (cc 0))
      (beginning-of-line)
      (while (< (point) start)
        (if (= (char-after) ?\t)
            (setq cc (* 8 (+ 1 (/ cc 8))))
          (incf cc))
        (forward-char))
      cc)))

(defun kill-indented-line (&optional arg)
  "`kill-line' for indented text.
Preserves indentation and removes extra whitespace"
  (interactive "P")
  (let ((col (haskell-current-column))
	(old-point (point)))
    (cond ((or (and (numberp arg) (< arg 0))
	       (and (not (looking-at "[ \t]*$"))
		    (or (not (numberp arg)) (zerop arg))))
					;use default behavior when calling with a negative argument
					;or killing (once) from the middle of a line
	   (kill-line arg))
	  ((and (skip-chars-backward " \t") ;always true
		(bolp)
		(save-excursion
		  (forward-line arg)
		  (not (looking-at "[ \t]*$"))))
					; killing from an empty line:
					; preserve indentation of the next line
	   (kill-region (point)
			(save-excursion
			  (forward-line arg)
			  (point)))
	   (skip-chars-forward " \t")
	   (if (> (haskell-current-column) col)
	       (move-to-column col)))
	  (t				; killing from not empty line:
					; kill all indentation
	   (goto-char old-point)
	   (kill-region (point)
			(save-excursion
			  (forward-line arg)
			  (skip-chars-forward " \t")
			  (point)))))))

(defun haskell-indentation-auto-fill-function ()
  (when (> (haskell-current-column) fill-column)
    (while (> (haskell-current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((auto-fill-function nil)
	  (indent (car (last (haskell-indentation-find-indentations)))))
      (newline)
      (indent-to indent)
      (end-of-line))))

(defun haskell-indentation-reindent (col)
  (beginning-of-line)
  (delete-region (point)
		 (progn (skip-syntax-forward "-")
			(point)))
  (indent-to col))

(defun haskell-newline-and-indent ()
  (interactive)
  (on-parse-error (newline)
     (let* ((cc (haskell-current-column))
            (ci (current-indentation))
            (indentations (haskell-indentation-find-indentations)))
       (skip-syntax-forward "-")
       (if (prog1 (and (eolp)
                       (not (= (haskell-current-column) ci)))
             (newline))
           (haskell-indentation-reindent
            (max (haskell-indentation-butlast indentations)
                 (haskell-indentation-matching-indentation
                  ci indentations)))
         (haskell-indentation-reindent (haskell-indentation-matching-indentation
                                        cc indentations))))))

(defun haskell-indentation-one-indentation (col indentations)
  (let* ((last-pair (last indentations)))
    (cond ((null indentations)
	   col)
	  ((null (cdr indentations))
	   (car indentations))
	  ((<= col (car last-pair))
	   col)
	  (t (car last-pair)))))

(defun haskell-indentation-butlast (indentations)
  (when (consp (cdr indentations))
    (while (cddr indentations)
      (setq indentations (cdr indentations))))
  (car indentations))

(defun haskell-indentation-next-indentation (col indentations)
  "Find the lefmost indentation which is greater than COL."
  (catch 'return
    (while indentations
      (if (or (< col (car indentations))
	      (null (cdr indentations)))
	  (throw 'return (car indentations))
	(setq indentations (cdr indentations))))
    col))

(defun haskell-indentation-previous-indentation (col indentations)
  "Find the rightmost indentation which is less than COL."
  (and indentations
       (> col (car indentations))
       (catch 'return
	 (while indentations
	   (if (or (null (cdr indentations))
		   (<= col (cadr indentations)))
	       (throw 'return (car indentations))
	     (setq indentations (cdr indentations))))
	 col)))

(defun haskell-indentation-matching-indentation (col indentations)
  "Find the leftmost indentation which is greater than or equal to COL."
  (catch 'return
    (while indentations
      (if (or (<= col (car indentations))
	      (null (cdr indentations)))
	  (throw 'return (car indentations))
	(setq indentations (cdr indentations))))
    col))

(defun haskell-indentation-indent-line ()
  (when (save-excursion
	  (beginning-of-line)
	  (not (nth 8 (syntax-ppss))))
    (let ((ci (current-indentation))
          (start-column (haskell-current-column)))
      (cond ((> (haskell-current-column) ci)
	     (save-excursion
	       (move-to-column ci)
	       (haskell-indentation-reindent
		(haskell-indentation-one-indentation
		 ci (haskell-indentation-find-indentations)))))

	    ((= (haskell-current-column) ci)
	     (haskell-indentation-reindent
	      (haskell-indentation-next-indentation
	       ci (haskell-indentation-find-indentations))))

	    (t (move-to-column ci)
	       (haskell-indentation-reindent
		(haskell-indentation-matching-indentation
		 ci (haskell-indentation-find-indentations)))))
      (cond ((not (= (haskell-current-column) start-column))
             (setq haskell-indent-last-position nil))
            ((not haskell-indentation-cycle-warn)
             (haskell-indentation-reindent
              (haskell-indentation-next-indentation
               -1
               (haskell-indentation-find-indentations))))
            ((not (equal (point) haskell-indent-last-position))
             (message "Press TAB again to go to the leftmost indentation")
             (setq haskell-indent-last-position (point)))
            (t
             (haskell-indentation-reindent
              (haskell-indentation-next-indentation
               -1
               (haskell-indentation-find-indentations))))))))

(defun haskell-indentation-delete-backward-char (n)
  (interactive "p")
  (on-parse-error (backward-delete-char 1)
     (cond
      ((and delete-selection-mode
            mark-active
            (not (= (point) (mark))))
       (delete-region (mark) (point)))
      ((or (= (haskell-current-column) 0)
           (> (haskell-current-column) (current-indentation))
           (nth 8 (syntax-ppss)))
       (delete-backward-char n))
      (t (let* ((ci (current-indentation))
                (pi (haskell-indentation-previous-indentation
                     ci (haskell-indentation-find-indentations))))
           (save-excursion
             (cond (pi
                    (move-to-column pi)
                    (delete-region (point)
                                   (progn (move-to-column ci)
                                          (point))))
                   (t
                    (beginning-of-line)
                    (delete-region (max (point-min) (- (point) 1))
                                   (progn (move-to-column ci)
                                          (point)))))))))))

(defun haskell-indentation-delete-char (n)
  (interactive "p")
  (on-parse-error (delete-char 1)
    (cond
     ((and delete-selection-mode
           mark-active
           (not (= (point) (mark))))
      (delete-region (mark) (point)))
     ((or (eolp)
          (>= (haskell-current-column) (current-indentation))
          (nth 8 (syntax-ppss)))
      (delete-char n))
     (t
      (let* ((ci (current-indentation))
             (pi (haskell-indentation-previous-indentation
                  ci (haskell-indentation-find-indentations))))
        (save-excursion
          (if (and pi (> pi (haskell-current-column)))
              (move-to-column pi))
          (delete-region (point)
                         (progn (move-to-column ci)
                                (point)))))))))

(defun haskell-indentation-goto-least-indentation ()
  (beginning-of-line)
  (catch 'return
    (while (not (bobp))
      (forward-comment (- (buffer-size)))
      (beginning-of-line)
      (let ((ps (nth 8 (syntax-ppss))))
		(when ps ;; inside comment or string
		  (goto-char ps)))
      (when (= 0 (current-indentation))
		(throw 'return nil))))
  (beginning-of-line)
  (when (bobp)
    (forward-comment (buffer-size))))

;; Dynamically scoped variables.
(defvar following-token)
(defvar current-token)
(defvar left-indent)
(defvar starter-indent)
(defvar current-indent)
(defvar layout-indent)
(defvar parse-line-number)
(defvar possible-indentations)
(defvar indentation-point)

(defun haskell-indentation-parse-to-indentations ()
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
	  (layout-indent 0)
	  (parse-line-number 0)
	  (current-indent haskell-indentation-layout-offset)
	  (starter-indent haskell-indentation-layout-offset)
	  (left-indent haskell-indentation-layout-offset)
	  (case-fold-search nil)
	  current-token
	  following-token
	  possible-indentations)
      (haskell-indentation-goto-least-indentation)
      (if (<= indentation-point (point))
	  '(0)
	(setq current-token (haskell-indentation-peek-token))
	(catch 'parse-end
	  (haskell-indentation-toplevel)
	  (when (not (equal current-token 'end-tokens))
	    (parse-error "Illegal token: %s" current-token)))
	possible-indentations))))

(defun haskell-indentation-find-indentations ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) '(0))
     ((nth 4 ppss)
      (if (save-excursion
	    (and (skip-syntax-forward "-")
		 (eolp)
		 (not (> (forward-line 1) 0))
		 (not (nth 4 (syntax-ppss)))))
	  (haskell-indentation-parse-to-indentations)
	'(0)))
     (t
      (haskell-indentation-parse-to-indentations)))))

(defconst haskell-indentation-toplevel-list
  '(("module" . haskell-indentation-module)
    ("data" . haskell-indentation-data)
    ("type" . haskell-indentation-data)
    ("newtype" . haskell-indentation-data)
    ("class" . haskell-indentation-class-declaration)
    ("instance" . haskell-indentation-class-declaration )))

(defconst haskell-indentation-type-list
  '(("::"    . (lambda () (haskell-indentation-statement-right #'haskell-indentation-type)))
    ("("     . (lambda () (haskell-indentation-list #'haskell-indentation-type
						    ")" "," nil)))
    ("["     . (lambda () (haskell-indentation-list #'haskell-indentation-type
						    "]" "," nil)))
    ("{"     . (lambda () (haskell-indentation-list #'haskell-indentation-type
						    "}" "," nil)))))

(defconst haskell-indentation-expression-list
  '(("data" . haskell-indentation-data)
    ("type" . haskell-indentation-data)
    ("newtype" . haskell-indentation-data)
    ("if"    . (lambda () (haskell-indentation-phrase
			   '(haskell-indentation-expression
			     "then" haskell-indentation-expression
			     "else" haskell-indentation-expression))))
    ("let"   . (lambda () (haskell-indentation-phrase
			   '(haskell-indentation-declaration-layout
			     "in" haskell-indentation-expression))))
    ("do"    . (lambda () (haskell-indentation-with-starter
			   #'haskell-indentation-expression-layout nil)))
    ("mdo"   . (lambda () (haskell-indentation-with-starter
			   #'haskell-indentation-expression-layout nil)))
    ("case"  . (lambda () (haskell-indentation-phrase
			   '(haskell-indentation-expression
			     "of" haskell-indentation-case-layout))))
    ("\\"    . (lambda () (haskell-indentation-phrase
			   '(haskell-indentation-expression
			     "->" haskell-indentation-expression))))
    ("proc"  . (lambda () (haskell-indentation-phrase
			   '(haskell-indentation-expression
			     "->" haskell-indentation-expression))))
    ("where" . (lambda () (haskell-indentation-with-starter
			   #'haskell-indentation-declaration-layout nil)))
    ("::"    . (lambda () (haskell-indentation-statement-right #'haskell-indentation-type)))
    ("="     . (lambda () (haskell-indentation-statement-right #'haskell-indentation-expression)))
    ("<-"    . (lambda () (haskell-indentation-statement-right #'haskell-indentation-expression)))
    ("("     . (lambda () (haskell-indentation-list #'haskell-indentation-expression
						    ")" '(list "," "->") nil)))
    ("["     . (lambda () (haskell-indentation-list #'haskell-indentation-expression
						    "]" "," "|")))
    ("{"     . (lambda () (haskell-indentation-list #'haskell-indentation-expression
						    "}" "," nil)))))
	  
(defun haskell-indentation-expression-layout ()
  (haskell-indentation-layout #'haskell-indentation-expression))

(defun haskell-indentation-declaration-layout ()
  (haskell-indentation-layout #'haskell-indentation-declaration))

(defun haskell-indentation-case-layout ()
  (haskell-indentation-layout #'haskell-indentation-case))

(defun haskell-indentation-fundep ()
  (haskell-indentation-with-starter
   (lambda () (haskell-indentation-separated
	       #'haskell-indentation-fundep1 "," nil))
   nil))

(defun haskell-indentation-fundep1 ()
  (let ((current-indent (haskell-current-column)))
    (while (member current-token '(value "->"))
      (haskell-indentation-read-next-token))
    (when (and (equal current-token 'end-tokens)
	       (member following-token '(value "->")))
      (haskell-indentation-add-indentation current-indent))))

(defun haskell-indentation-toplevel ()
  (haskell-indentation-layout
   (lambda ()
	 (let ((parser (assoc current-token haskell-indentation-toplevel-list)))
	   (if parser
		   (funcall (cdr parser))
		 (haskell-indentation-declaration))))))

(defun haskell-indentation-type ()
  (let ((current-indent (haskell-current-column)))
    (catch 'return
      (while t
		(cond
		 ((member current-token '(value operator "->"))
		  (haskell-indentation-read-next-token))

		 ((equal current-token 'end-tokens)
		  (when (member following-token
						'(value operator no-following-token
								"->" "(" "[" "{" "::"))
			(haskell-indentation-add-indentation current-indent))
		  (throw 'return nil))
		 
		 (t (let ((parser (assoc current-token haskell-indentation-type-list)))
			  (if (not parser)
				  (throw 'return nil)
				(funcall (cdr parser))))))))))

(defun haskell-indentation-data ()
  (haskell-indentation-with-starter
   (lambda ()
     (when (equal current-token "instance")
       (haskell-indentation-read-next-token))
     (haskell-indentation-type)
     (cond ((equal current-token "=")
	    (haskell-indentation-with-starter
	     (lambda () (haskell-indentation-separated #'haskell-indentation-type "|" "deriving"))
	     nil))
	   ((equal current-token "where")
	    (haskell-indentation-with-starter
	     #'haskell-indentation-expression-layout nil))))
   nil))

(defun haskell-indentation-class-declaration ()
  (haskell-indentation-with-starter
   (lambda ()
     (haskell-indentation-type)
     (when (equal current-token "|")
       (haskell-indentation-fundep))
     (when (equal current-token "where")
       (haskell-indentation-with-starter
	#'haskell-indentation-expression-layout nil)))
   nil))

(defun haskell-indentation-module ()
  (haskell-indentation-with-starter
   (lambda ()
	 (let ((current-indent (haskell-current-column)))
	   (haskell-indentation-read-next-token)
	   (when (equal current-token "(")
		 (haskell-indentation-list
		  #'haskell-indentation-module-export
		  ")" "," nil))
	   (when (equal current-token 'end-tokens)
		 (haskell-indentation-add-indentation current-indent)
		 (throw 'parse-end nil))
	   (when (equal current-token "where")
		 (haskell-indentation-read-next-token)
		 (when (equal current-token 'end-tokens)
		   (haskell-indentation-add-layout-indent)
		   (throw 'parse-end nil))
		 (haskell-indentation-layout #'haskell-indentation-toplevel))))
   nil))

(defun haskell-indentation-module-export ()
  (cond ((equal current-token "module")
		 (let ((current-indent (haskell-current-column)))
		   (haskell-indentation-read-next-token)
		   (cond ((equal current-token 'end-tokens)
				  (haskell-indentation-add-indentation current-indent))
				 ((equal current-token 'value)
				  (haskell-indentation-read-next-token)))))
		(t (haskell-indentation-type))))

(defun haskell-indentation-list (parser end sep stmt-sep)
  (haskell-indentation-with-starter
   `(lambda () (haskell-indentation-separated #',parser
											  ,sep
											  ,stmt-sep))
   end))

(defun haskell-indentation-with-starter (parser end)
  (let ((starter-column (haskell-current-column))
		(current-indent current-indent)
		(left-indent (if (= (haskell-current-column) (current-indentation))
						 (haskell-current-column) left-indent)))
    (haskell-indentation-read-next-token)
    (when (equal current-token 'end-tokens)
      (if (equal following-token end)
	  (haskell-indentation-add-indentation starter-column)
	  (haskell-indentation-add-indentation
	   (+ left-indent haskell-indentation-left-offset)))
      (throw 'parse-end nil))
    (let* ((current-indent (haskell-current-column))
		   (starter-indent (min starter-column current-indent))
		   (left-indent (if end (+ current-indent haskell-indentation-starter-offset)
						  left-indent)))
      (funcall parser)
      (cond ((equal current-token 'end-tokens)
			 (when (equal following-token end)
			   (haskell-indentation-add-indentation starter-indent))
			 (when end (throw 'parse-end nil))) ;; add no indentations
			((equal current-token end)
			 (haskell-indentation-read-next-token)) ;; continue
			(end (parse-error "Illegal token: %s" current-token))))))

(defun haskell-indentation-case ()
  (haskell-indentation-expression)
  (cond ((equal current-token 'end-tokens)
	 (haskell-indentation-add-indentation current-indent))
	((equal current-token "|")
	 (haskell-indentation-with-starter
	  (lambda () (haskell-indentation-separated #'haskell-indentation-case "|" nil))
	  nil))
	((equal current-token "->")
	 (haskell-indentation-statement-right #'haskell-indentation-expression))
	;; otherwise fallthrough
	))

(defun haskell-indentation-statement-right (parser)
    (haskell-indentation-read-next-token)
    (when (equal current-token 'end-tokens)
      (haskell-indentation-add-indentation
       (+ left-indent haskell-indentation-left-offset))
      (throw 'parse-end nil))
    (let ((current-indent (haskell-current-column)))
	  (funcall parser)))

(defun haskell-indentation-simple-declaration ()
  (haskell-indentation-expression)
  (cond ((equal current-token "=")
	 (haskell-indentation-statement-right #'haskell-indentation-expression))
	((equal current-token "::")
	 (haskell-indentation-statement-right #'haskell-indentation-type))
	((and (equal current-token 'end-tokens)
	      (equal following-token "="))
	 (haskell-indentation-add-indentation current-indent)
	 (throw 'parse-end nil))))

(defun haskell-indentation-declaration ()
  (haskell-indentation-expression)
  (cond ((equal current-token "|")
	 (haskell-indentation-with-starter
	  (lambda () (haskell-indentation-separated #'haskell-indentation-expression "," "|"))
	  nil))
	((equal current-token 'end-tokens)
	 (when (member following-token '("|" "=" "::" ","))
	   (haskell-indentation-add-indentation current-indent)
	   (throw 'parse-end nil)))))

(defun haskell-indentation-layout (parser)
  (if (equal current-token "{")
      (haskell-indentation-list parser "}" ";" nil)
    (haskell-indentation-implicit-layout-list parser)))

(defun haskell-indentation-expression-token (token)
  (member token '("if" "let" "do" "case" "\\" "(" "[" "::"
		  value operator no-following-token)))

(defun haskell-indentation-expression ()
  (let ((current-indent (haskell-current-column)))
    (catch 'return
      (while t
	(cond
	 ((member current-token '(value operator))
	  (haskell-indentation-read-next-token))

	 ((equal current-token 'end-tokens)
	  (cond ((equal following-token "where")
		 (haskell-indentation-add-indentation
		  (+ left-indent haskell-indentation-left-offset)))
		((haskell-indentation-expression-token following-token)
		 (haskell-indentation-add-indentation
		  current-indent)))
	  (throw 'return nil))

	 (t (let ((parser (assoc current-token haskell-indentation-expression-list)))
	      (when (null parser)
		(throw 'return nil))
	      (funcall (cdr parser))
	      (when (and (equal current-token 'end-tokens)
			 (equal (car parser) "let")
			 (= haskell-indentation-layout-offset current-indent)
			 (haskell-indentation-expression-token following-token))
		;; inside a layout, after a let construct
		(haskell-indentation-add-layout-indent)
		(throw 'parse-end nil))
	      (unless (member (car parser) '("(" "[" "{" "do" "case"))
		(throw 'return nil)))))))))

(defun haskell-indentation-test-indentations ()
  (interactive)
  (let ((indentations (save-excursion (haskell-indentation-find-indentations)))
	(str "")
	(pos 0))
    (while indentations
      (when (>= (car indentations) pos)
	(setq str (concat str (make-string (- (car indentations) pos) ?\ )
			  "|"))
	(setq pos (+ 1 (car indentations))))
      (setq indentations (cdr indentations)))
    (end-of-line)
    (newline)
    (insert str)))

(defun haskell-indentation-separated (parser separator stmt-separator)
  (catch 'return
    (while t
      (funcall parser)
      (cond ((if (listp separator) (member current-token separator) (equal current-token separator))
	     (haskell-indentation-at-separator))

	    ((equal current-token stmt-separator)
	     (setq starter-indent (haskell-current-column))
	     (haskell-indentation-at-separator))

	    ((equal current-token 'end-tokens)
	     (cond ((or (equal following-token separator)
			(equal following-token stmt-separator))
		    (haskell-indentation-add-indentation starter-indent)
		    (throw 'parse-end nil)))
	     (throw 'return nil))

	    (t (throw 'return nil))))))

(defun haskell-indentation-at-separator ()
  (let ((separator-column
	 (and (= (haskell-current-column) (current-indentation))
	      (haskell-current-column))))
    (haskell-indentation-read-next-token)
    (cond ((eq current-token 'end-tokens)
	   (haskell-indentation-add-indentation current-indent)
	   (throw 'return nil))
	  (separator-column ;; on the beginning of the line
	   (setq current-indent (haskell-current-column))
	   (setq starter-indent separator-column)))))

(defun haskell-indentation-implicit-layout-list (parser)
  (let* ((layout-indent (haskell-current-column))
		 (current-indent (haskell-current-column))
		 (left-indent (haskell-current-column)))
    (catch 'return
      (while t
	(let ((left-indent left-indent))
	  (funcall parser))
	(cond ((member current-token '(layout-next ";"))
	       (haskell-indentation-read-next-token))
	      ((equal current-token 'end-tokens)
	       (when (or (haskell-indentation-expression-token following-token)
					 (equal following-token ";"))
			 (haskell-indentation-add-layout-indent))
	       (throw 'return nil))
	      (t (throw 'return nil))))))
  ;; put haskell-indentation-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (haskell-indentation-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun haskell-indentation-phrase (phrase)
  (haskell-indentation-with-starter
   `(lambda () (haskell-indentation-phrase-rest ',phrase))
   nil))

(defun haskell-indentation-phrase-rest (phrase)
  (let ((starter-line parse-line-number))
    (let ((current-indent (haskell-current-column)))
      (funcall (car phrase)))
    (cond
     ((equal current-token 'end-tokens)
      (cond ((null (cdr phrase))) ;; fallthrough
	    ((equal following-token (cadr phrase))
	     (haskell-indentation-add-indentation starter-indent)
	     (throw 'parse-end nil))
	    ((equal (cadr phrase) "in")
	     (when (= left-indent layout-indent)
	       (haskell-indentation-add-layout-indent)
	       (throw 'parse-end nil)))
	    (t (throw 'parse-end nil))))

     ((null (cdr phrase)))
     
     ((equal (cadr phrase) current-token)
      (let* ((on-new-line (= (haskell-current-column) (current-indentation)))
	     (lines-between (- parse-line-number starter-line))
	     (left-indent (if (<= lines-between 0)
			      left-indent
			    starter-indent)))
	(haskell-indentation-read-next-token)
	(when (equal current-token 'end-tokens)
	  (haskell-indentation-add-indentation
	   (cond ((member (cadr phrase) '("then" "else"))
		  (+ starter-indent haskell-indentation-ifte-offset))
		 ((member (cadr phrase) '("in" "->"))
		  ;; expression ending in another expression
		  (if on-new-line
		      (+ left-indent haskell-indentation-starter-offset)
		    left-indent))
		 (t (+ left-indent haskell-indentation-left-offset))))
	  (throw 'parse-end nil))
	(haskell-indentation-phrase-rest (cddr phrase))))

     ((equal (cadr phrase) "in")) ;; fallthrough
     (t (parse-error "Expecting %s" (cadr phrase))))))

(defun haskell-indentation-add-indentation (indent)
  (haskell-indentation-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent haskell-indentation-layout-offset)
     indent)))

(defun haskell-indentation-add-layout-indent ()
  (haskell-indentation-push-indentation layout-indent))

(defun haskell-indentation-push-indentation (indent)
  (when (or (null possible-indentations)
	    (< indent (car possible-indentations)))
    (setq possible-indentations
	  (cons indent possible-indentations))))

(defun haskell-indentation-token-test ()
  (let ((current-token nil)
	(following-token nil)
	(layout-indent 0)
	(indentation-point (mark)))
    (haskell-indentation-read-next-token)))

(defun haskell-indentation-read-next-token ()
  (cond ((eq current-token 'end-tokens)
	 'end-tokens)
	((eq current-token 'layout-end)
	 (cond ((> layout-indent (haskell-current-column))
		'layout-end)
	       ((= layout-indent (haskell-current-column))
		(setq current-token 'layout-next))
	       ((< layout-indent (haskell-current-column))
		(setq current-token (haskell-indentation-peek-token)))))
	((eq current-token 'layout-next)
	 (setq current-token (haskell-indentation-peek-token)))
	((> layout-indent (haskell-current-column))
	 (setq current-token 'layout-end))
	(t
	 (haskell-indentation-skip-token)
	 (if (>= (point) indentation-point)
	     (progn
	       (setq following-token
		     (if (= (point) indentation-point)
			 (haskell-indentation-peek-token)
		       'no-following-token))
	       (setq current-token 'end-tokens))
	   (when (= (haskell-current-column) (current-indentation))
	     ;; on a new line
	     (setq current-indent (haskell-current-column))
	     (setq left-indent (haskell-current-column))
	     (setq parse-line-number (+ parse-line-number 1)))
	   (cond ((> layout-indent (haskell-current-column))
		  (setq current-token 'layout-end))
		 ((= layout-indent (haskell-current-column))
		  (setq current-token 'layout-next))
		 (t (setq current-token (haskell-indentation-peek-token))))))))

(defun haskell-indentation-peek-token ()
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alpha:]']\\|$\\)")
	 (match-string 1))
	((looking-at "[][(){}[,;]")
	 (match-string 0))
	((looking-at "\\(\\\\\\|->\\|<-\\|::\\|=\\||\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
	 (match-string 1))
	((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
	 'operator)
	(t 'value)))

(defun haskell-indentation-skip-token ()
  "Skip to the next token."
  (let ((case-fold-search nil))
    (if (or (looking-at "'\\([^\\']\\|\\\\.\\)*'")
            (looking-at "\"\\([^\\\"]\\|\\\\.\\)*\"")
            (looking-at	; Hierarchical names always start with uppercase
             "[[:upper:]]\\(\\sw\\|'\\)*\\(\\.\\(\\sw\\|'\\)+\\)*")
            (looking-at "\\sw\\(\\sw\\|'\\)*") ; Only unqualified vars can start with lowercase
            (looking-at "[0-9][0-9oOxXeE+-]*")
            (looking-at "[-:!#$%&*+./<=>?@\\\\^|~]+")
            (looking-at "[](){}[,;]")
            (looking-at "`[[:alnum:]']*`"))
        (goto-char (match-end 0))
    ;; otherwise skip until space found
      (skip-syntax-forward "^-"))
    (forward-comment (buffer-size))))

(provide 'haskell-indentation)
;;; haskell-indentation.el ends here

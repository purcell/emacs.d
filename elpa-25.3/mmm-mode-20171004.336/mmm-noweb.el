;;; mmm-noweb.el --- MMM submode class for Noweb programs
;;
;; Copyright 2003, 2004 Joe Kelsey <joe@zircon.seattle.wa.us>
;;
;; The filling, completion and chunk motion commands either taken
;; directly from or inspired by code in:
;; noweb-mode.el - edit noweb files with GNU Emacs
;; Copyright 1995 by Thorsten.Ohl @ Physik.TH-Darmstadt.de
;;     with a little help from Norman Ramsey <norman@bellcore.com>
;; 

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file contains the definition of an MMM Mode submode class for
;; editing Noweb programs.
;;
;; FIXME: The more advanced features don't work: `mmm-name-at' and
;; `mmm-syntax-region' are undefined. Need to dig around in the bug reports
;; and/or discussions, wherever the code using them was submitted.

;;; Code:

(require 'cl)
(require 'mmm-region)
(require 'mmm-vars)
(require 'mmm-mode)

;;{{{ Variables

(defvar mmm-noweb-code-mode 'fundamental-mode
  "*Major mode for editing code chunks.
This is set to FUNDAMENTAL-MODE by default, but you might want to change
this in the Local Variables section of your file to something more
appropriate, like C-MODE, FORTRAN-MODE, or even INDENTED-TEXT-MODE.")

(defvar mmm-noweb-quote-mode nil
  "*Major mode for quoted code chunks within documentation chunks.
If nil, defaults to `mmm-noweb-code-mode', which see.")

(defvar mmm-noweb-quote-string "quote"
  "*String used to form quoted code submode region names.
See `mmm-noweb-quote'.")

(defvar mmm-noweb-quote-number 0
  "*Starting value appended to `mmm-noweb-quote-string'.
See `mmm-noweb-quote'.")

(defvar mmm-noweb-narrowing nil
  "*Narrow the region to the current pair of chunks.")

;;}}}
;;{{{ Support for mmm submode stuff

(defun mmm-noweb-chunk (form)
  "Return the noweb code mode chosen by the user.
If the next 100 characters of the buffer contain a string of the form
\"-*- MODE -*-\", then return MODE as the chosen mode, otherwise
return the value of `mmm-noweb-code-mode'."
  ;; Look for -*- mode -*- in the first two lines.
  ;; 120 chars = 40 chars for #! + 80 chars for following line...
  (if (re-search-forward "-\\*-\\s +\\(\\S-+\\)\\s +-\\*-" (+ (point) 120) t)
      (let* ((string (match-string-no-properties 1))
	     (modestr (intern (if (string-match "mode\\'" string)
				  string
				(concat string "-mode")))))
	(or (mmm-ensure-modename modestr)
	    mmm-noweb-code-mode))
    mmm-noweb-code-mode))

(defun mmm-noweb-quote (form)
  "Create a unique name for a quoted code region within a documentation chunk."
  (or mmm-noweb-quote-mode
      mmm-noweb-code-mode))

(defun mmm-noweb-quote-name (form)
  "Create a unique name for a quoted code region within a documentation chunk."
  (setq mmm-noweb-quote-number (1+ mmm-noweb-quote-number))
  (concat mmm-noweb-quote-string "-"
	  (number-to-string mmm-noweb-quote-number)))

(defun mmm-noweb-chunk-name (form)
  "Get the chunk name from FRONT-FORM."
  (string-match "<<\\(.*\\)>>=" form)
  (match-string-no-properties 1 form))

;;}}}
;;{{{ mmm noweb submode group

;; We assume that the global document mode is latex or whatever, the
;; user wants.  This class controls the code chunk submodes.  We use
;; match-submode to either return the value in mmm-noweb-code-mode or to
;; look at the first line of the chunk for a submode setting.  We reset
;; case-fold-search because chunk names are case sensitive.  The front
;; string identifies the chunk name between the <<>>.  Since this is
;; done, name-match can use the same functions as save-matches for back.
;; Our insert skeleton places a new code chunk and the skel-name lets us
;; optimize the skelton naming to use the inserted string.

(mmm-add-group
 'noweb
 '((noweb-chunk
    :match-submode mmm-noweb-chunk
    :case-fold-search nil
    :front "^<<\\(.*\\)>>="
    :match-name "~1"
    :save-name 1
    :front-offset (end-of-line 1)
    :back "^@\\( \\|$\\|\\( %def .*$\\)\\)"
    :insert ((?c noweb-code "Code Chunk Name: "
		"\n" @ "<<" str ">>=" @ "\n" _ "\n" @ "@ " @ "\n"))
    :skel-name t
    )
   (noweb-quote
    :match-submode mmm-noweb-quote
    :face mmm-special-submode-face
    :front "\\[\\["
;    :name-match mmm-noweb-quote-name
    :back "\\]\\]"
    :insert ((?q noweb-quote nil @ "[[" @ _ @ "]]" @))
    )
   ))

;;}}}
;;{{{ Noweb regions

(defun mmm-noweb-regions (start stop regexp &optional delim)
  "Return a liat of regions of the form \(NAME BEG END) that exclude
names which match REGEXP."
  (let* ((remove-next nil)
	 (regions
	  (maplist #'(lambda (pos-list)
		       (if (cdr pos-list)
			   (if remove-next
			       (setq remove-next nil)
			     (let ((name (or (mmm-name-at (car pos-list) 'beg)
					     (symbol-name mmm-primary-mode))))
			       (if (and regexp (string-match regexp name) )
				   (progn
				     (setq remove-next t)
				     nil)
				 (list name
				       (car pos-list) (cadr pos-list)))))))
		   (mmm-submode-changes-in start stop))))
    ;; The above loop leaves lots of nils in the list...
    ;; Removing them saves us from having to do the (last x 2)
    ;; trick that mmm-regions-in does.
    (setq regions (delq nil regions))))

;;}}}
;;{{{ Filling, etc

(defun mmm-noweb-narrow-to-doc-chunk ()
  "Narrow to the current doc chunk.
The current chunk includes all quoted code chunks (i.e., \[\[...\]\]).
This function is only valid when called with point in a doc chunk or
quoted code chunk."
  (interactive)
  (let ((name (mmm-name-at (point))))
    (if (or (null name) (string-match "^quote" name))
	(let ((prev (cond
		     ((= (point) (point-min)) (point))
		     (t (cadar (last (mmm-noweb-regions (point-min) (point)
							"^quote"))))))
	      (next (cond
		     ((= (point) (point-max)) (point))
		     (t (save-excursion
			  (goto-char (cadr
				      (cadr (mmm-noweb-regions (point)
							       (point-max)
							       "^quote"))))
			  (forward-line -1)
			  (point))))))
	  (narrow-to-region prev next)))))

(defun mmm-noweb-fill-chunk (&optional justify)
  "Fill the current chunk according to mode.
Run `fill-region' on documentation chunks and `indent-region' on code
chunks."
  (interactive "P")
  (save-restriction
    (let ((name (mmm-name-at (point))))
      (if (and name (not (string-match "^quote" name)))
	  (if (or indent-region-function indent-line-function)
	      (progn
		(mmm-space-other-regions)
		(indent-region (overlay-start mmm-current-overlay)
			       (overlay-end mmm-current-overlay) nil))
	    (error "No indentation functions defined in %s!" major-mode))
	(progn
	  (mmm-word-other-regions)
	  (fill-paragraph justify)))
      (mmm-undo-syntax-other-regions))))

(defun mmm-noweb-fill-paragraph-chunk (&optional justify)
  "Fill a paragraph in the current chunk."
  (interactive "P")
  (save-restriction
    (let ((name (mmm-name-at (point))))
      (if (and name (not (string-match "^quote" name)))
	  (progn
	    (mmm-space-other-regions)
	    (fill-paragraph justify))
	(progn
	  (mmm-word-other-regions)
	  (fill-paragraph justify)))
      (mmm-undo-syntax-other-regions))))

(defun mmm-noweb-fill-named-chunk (&optional justify)
  "Fill the region containing the named chunk."
  (interactive "P")
  (save-restriction
    (let* ((name (or (mmm-name-at) (symbol-name mmm-primary-mode)))
	   (list (cdr (assoc name (mmm-names-alist (point-min) (point-max))))))
      (if (or (string= name (symbol-name mmm-primary-mode))
	      (string-match "^quote" name))
	  (progn
	    (mmm-word-other-regions)
	    (do-auto-fill))
	(progn
	  (mmm-space-other-regions)
	  (indent-region (caar list) (cadar (last list)) nil)))
      (mmm-undo-syntax-other-regions))))

(defun mmm-noweb-auto-fill-doc-chunk ()
  "Replacement for `do-auto-fill'."
  (save-restriction
    (mmm-noweb-narrow-to-doc-chunk)
    (mmm-word-other-regions)
    (do-auto-fill)
    (mmm-undo-syntax-other-regions)))

(defun mmm-noweb-auto-fill-doc-mode ()
  "Install the improved auto fill function, iff necessary."
  (if auto-fill-function
      (setq auto-fill-function 'mmm-noweb-auto-fill-doc-chunk)))

(defun mmm-noweb-auto-fill-code-mode ()
  "Install the default auto fill function, iff necessary."
  (if auto-fill-function
      (setq auto-fill-function 'do-auto-fill)))

;;}}}
;;{{{ Functions on named chunks

(defun mmm-noweb-complete-chunk ()
  "Try to complete the chunk name."
  (interactive)
  (let ((end (point))
	(beg (save-excursion
	       (if (re-search-backward "<<"
				       (save-excursion
					 (beginning-of-line)
					 (point))
				       t)
		   (match-end 0)
		 nil))))
	(if beg
	    (let* ((pattern (buffer-substring beg end))
		   (alist (mmm-names-alist (point-min) (point-max)))
		   (completion (try-completion pattern alist)))
	      (cond ((eq completion t))
		    ((null completion)
		     (message "Can't find completion for \"%s\"" pattern)
		     (ding))
		    ((not (string= pattern completion))
		     (delete-region beg end)
		     (insert completion)
		     (if (not (looking-at ">>"))
			 (insert ">>")))
		    (t
		     (message "Making completion list...")
		     (with-output-to-temp-buffer "*Completions*"
		       (display-completion-list
			(all-completions pattern alist)))
		     (message "Making completion list...%s" "done"))))
	  (message "Not at chunk name..."))))

(defvar mmm-noweb-chunk-history nil
  "History for `mmm-noweb-goto-chunk'.")

(defun mmm-noweb-goto-chunk ()
  "Goto the named chunk."
  (interactive)
  (widen)
  (let* ((completion-ignore-case t)
	 (alist (mmm-names-alist (point-min) (point-max)))
	 (chunk (completing-read
		 "Chunk: " alist nil t
		 (mmm-name-at (point))
		 mmm-noweb-chunk-history)))
    (goto-char (caadr (assoc chunk alist)))))

(defun mmm-noweb-goto-next (&optional cnt)
  "Goto the continuation of the current chunk."
  (interactive "p")
  (widen)
  (let ((name (mmm-name-at (point))))
    (if name
	(let ((list (cdr (assoc name (mmm-names-alist
				      (overlay-end mmm-current-overlay)
				      (point-max))))))
	  (if list
	      (goto-char (caar (nthcdr (1- cnt) list))))))))

(defun mmm-noweb-goto-previous (&optional cnt)
  "Goto the continuation of the current chunk."
  (interactive "p")
  (widen)
  (let ((name (mmm-name-at (point))))
    (if name
	(let ((list (reverse
		     (cdr (assoc name
				 (mmm-names-alist (point-min)
						  (overlay-start
						   mmm-current-overlay)))))))
	  (if list
	      (goto-char (cadar (nthcdr cnt list))))))))

;;}}}
;;{{{ Key mappings

(defvar mmm-noweb-map (make-sparse-keymap))
(defvar mmm-noweb-prefix-map (make-sparse-keymap))
(define-key mmm-noweb-map mmm-mode-prefix-key mmm-noweb-prefix-map)

(mmm-define-key ?d 'mmm-noweb-narrow-to-doc-chunk mmm-noweb-prefix-map)
(mmm-define-key ?n 'mmm-noweb-goto-next mmm-noweb-prefix-map)
(mmm-define-key ?p 'mmm-noweb-goto-previous mmm-noweb-prefix-map)
(mmm-define-key ?q 'mmm-noweb-fill-chunk mmm-noweb-prefix-map)
;; Cannot use C-g as goto command, so use C-s.
(mmm-define-key ?s 'mmm-noweb-goto-chunk mmm-noweb-prefix-map)

(define-key mmm-noweb-prefix-map "\t" 'mmm-noweb-complete-chunk)

;; Don't want to add to either the mmm mode map (used in other mmm
;; buffers) or the local map (used in other major mode buffers), so we
;; make a full-buffer spanning overlay and add the map there.
(defun mmm-noweb-bind-keys ()
  (save-restriction
    (widen)
    (let ((ovl (make-overlay (point-min) (point-max) nil nil t)))
      ;; 'keymap', not 'local-map'
      (overlay-put ovl 'keymap mmm-noweb-map))))

(add-hook 'mmm-noweb-class-hook 'mmm-noweb-bind-keys)

;; TODO: make this overlay go away if mmm is turned off

;;}}}

;; These functions below living here temporarily until a real place is
;; found.

(defun mmm-syntax-region-list (syntax regions)
  "Apply SYNTAX to a list of REGIONS of the form (BEG END).
If SYNTAX is not nil, set the syntax-table property of each region.
If SYNTAX is nil, remove the region syntax-table property.
See `mmm-syntax-region'."
  (mapcar #'(lambda (reg)
	      (mmm-syntax-region (car reg) (cadr reg) syntax))
	  regions))

(defun mmm-syntax-other-regions (syntax &optional name)
  "Apply SYNTAX cell to other regions.
Regions are separated by name, using either `mmm-name-at' or the
optional NAME to determine the current region name."
  (if (null name)
      (setq name (or (mmm-name-at)
		     (symbol-name mmm-primary-mode))))
  (mapcar #'(lambda (reg)
	      (if (not (string= (car reg) name))
		  (mmm-syntax-region-list syntax (cdr reg))))
	  (mmm-names-alist (point-min) (point-max))))

(defun mmm-word-other-regions ()
  "Give all other regions word syntax."
  (interactive)
  (mmm-syntax-other-regions '(2 . 0))
  (setq parse-sexp-lookup-properties t))

(defun mmm-space-other-regions ()
  "Give all other regions space syntax."
  (interactive)
  (mmm-syntax-other-regions '(0 . 0))
  (setq parse-sexp-lookup-properties t))

(defun mmm-undo-syntax-other-regions ()
  "Remove syntax-table property from other regions."
  (interactive)
  (mmm-syntax-other-regions nil)
  (setq parse-sexp-lookup-properties nil))


(provide 'mmm-noweb)

;;; mmm-noweb.el ends here

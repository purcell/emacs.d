;;; haskell-indent.el --- "semi-intelligent" indentation module for Haskell Mode

;; Copyright 2004, 2005, 2007  Free Software Foundation, Inc.
;; Copyright 1997-1998  Guy Lapalme

;; Author: 1997-1998 Guy Lapalme <lapalme@iro.umontreal.ca>

;; Keywords: indentation Haskell layout-rule
;; Version: 1.2
;; URL: http://www.iro.umontreal.ca/~lapalme/layout/index.html

;;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; Purpose:
;;
;; To support automatic indentation of Haskell programs using
;; the layout rule descrived in section 1.5 and appendix B.3 of the
;; the Haskell report.  The rationale and the implementation principles
;; are described in an article to appear in Journal of Functional Programming.
;;   "Dynamic tabbing for automatic indentation with the layout rule"
;;
;; It supports literate scripts.
;; Haskell indentation is performed
;;     within \begin{code}...\end{code} sections of a literate script
;;     and in lines beginning with > with Bird style literate script
;; TAB aligns to the left column outside of these sections.
;;
;; Installation:
;;
;; To turn indentation on for all Haskell buffers under the Haskell
;; mode of Moss&Thorn <http://www.haskell.org/haskell-mode/>
;; add this to .emacs:
;;
;;    (add-hook haskell-mode-hook 'turn-on-haskell-indent)
;;
;; Otherwise, call `turn-on-haskell-indent'.
;;
;;
;; Customisation:
;;       The "standard" offset for statements is 4 spaces.
;;       It can be changed by setting the variable "haskell-indent-offset" to
;;       another value
;;
;;       The default number of blanks after > in a Bird style literate script
;;       is 1; it can be changed by setting the variable
;;       "haskell-indent-literate-Bird-default-offset"
;;
;;       `haskell-indent-hook' is invoked if not nil.
;;
;; All functions/variables start with
;; `(turn-(on/off)-)haskell-indent' or `haskell-indent-'.

;; This file can also be used as a hook for the Hugs Mode developed by
;;         Chris Van Humbeeck <chris.vanhumbeeck@cs.kuleuven.ac.be>
;; It can be obtained at:
;; http://www-i2.informatik.rwth-aachen.de/Forschung/FP/Haskell/hugs-mode.el
;;
;; For the Hugs mode put the following in your .emacs
;;
;;(setq auto-mode-alist (append auto-mode-alist '(("\\.hs\\'" . hugs-mode))))
;;(autoload 'hugs-mode "hugs-mode" "Go into hugs mode" t)
;;
;; If only the indentation mode is used then replace the two
;; preceding lines with
;;(setq auto-mode-alist (append auto-mode-alist
;;                              '(("\\.hs\\'" . turn-on-haskell-indent))))
;;(autoload 'turn-on-haskell-indent "hindent" "Indentation mode for Haskell" t)
;;
;; For indentation in both cases then add the following to your .emacs
;;(add-hook 'hugs-mode-hook 'turn-on-haskell-indent)
;;(autoload 'haskell-indent-cycle "hindent" "Indentation cycle for Haskell" t)
;;

;;; Code:

(eval-when-compile (require 'cl))	;need defs of push and pop
(defvar haskell-literate)

(defgroup haskell-indent nil
  "Haskell indentation."
  :group 'haskell
  :prefix "haskell-indent-")

(defcustom haskell-indent-offset 4
  "Indentation of Haskell statements with respect to containing block."
  :type 'integer
  :group 'haskell-indent)

(defcustom haskell-indent-literate-Bird-default-offset 1
  "Default number of blanks after > in a Bird style literate script."
  :type 'integer
  :group 'haskell-indent)

(defcustom haskell-indent-rhs-align-column 0
  "Column on which to align right-hand sides (use 0 for ad-hoc alignment)."
  :type 'integer
  :group 'haskell-indent)

(defun haskell-indent-point-to-col (apoint)
  "Return the column number of APOINT."
  (save-excursion
    (goto-char apoint)
    (current-column)))

(defconst haskell-indent-start-keywords-re
  (concat "\\<"
          (regexp-opt '("class" "data" "import" "infix" "infixl" "infixr"
                        "instance" "module" "newtype" "primitive" "type") t)
          "\\>")
  "Regexp describing keywords to complete when standing at the first word
of a line.")


;; Customizations for different kinds of environments
;; in which dealing with low-level events are different.
(defun haskell-indent-mark-active ()
  (if (featurep 'xemacs)
      (if zmacs-regions
          zmacs-region-active-p
        t)
    mark-active))

;;  for pushing indentation information

(defvar haskell-indent-info)            ;Used with dynamic scoping.

(defun haskell-indent-push-col (col &optional name)
  "Push indentation information for the column COL.
The info is followed by NAME (if present).
Makes sure that the same indentation info is not pushed twice.
Uses free var `haskell-indent-info'."
  (let ((tmp (cons col name)))
    (if (member tmp haskell-indent-info)
	haskell-indent-info
      (push tmp haskell-indent-info))))

(defun haskell-indent-push-pos (pos &optional name)
  "Pushes indentation information for the column corresponding to POS
followed by NAME (if present)."
  (haskell-indent-push-col (haskell-indent-point-to-col pos) name))

(defun haskell-indent-push-pos-offset (pos &optional offset)
  "Pushes indentation information for the column corresponding to POS
followed by an OFFSET (if present use its value otherwise use
`haskell-indent-offset')."
  (haskell-indent-push-col (+ (haskell-indent-point-to-col pos)
                              (or offset haskell-indent-offset))))

;; redefinition of some Emacs function for dealing with
;; Bird Style literate scripts

(defun haskell-indent-bolp ()
  "`bolp' but dealing with Bird-style literate scripts."
  (or (bolp)
      (and (eq haskell-literate 'bird)
           (<= (current-column) (1+ haskell-indent-literate-Bird-default-offset))
           (eq (char-after (line-beginning-position)) ?\>))))

(defun haskell-indent-empty-line-p ()
  "Checks if the current line is empty; deals with Bird style scripts."
  (save-excursion
    (beginning-of-line)
    (if (and (eq haskell-literate 'bird)
             (eq (following-char) ?\>))
        (forward-char 1))
    (looking-at "[ \t]*$")))

(defun haskell-indent-back-to-indentation ()
  "`back-to-indentation' function but dealing with Bird-style literate scripts."
  (if (and (eq haskell-literate 'bird)
           (progn (beginning-of-line) (eq (following-char) ?\>)))
      (progn
        (forward-char 1)
        (skip-chars-forward " \t"))
    (back-to-indentation)))

(defun haskell-indent-current-indentation ()
  "`current-indentation' function but dealing with Bird-style literate
scripts."
  (if (eq haskell-literate 'bird)
      (save-excursion
        (haskell-indent-back-to-indentation)
        (current-column))
    (current-indentation)))

(defun haskell-indent-backward-to-indentation (n)
  "`backward-to-indentation' function but dealing with Bird-style literate
scripts."
  (if (eq haskell-literate 'bird)
      (progn
        (forward-line (- n))
        (haskell-indent-back-to-indentation))
    (backward-to-indentation n)))

(defun haskell-indent-forward-line (&optional n)
  "`forward-line' function but dealing with Bird-style literate scripts."
  (prog1
      (forward-line n)
    (if (and (eq haskell-literate 'bird) (eq (following-char) ?\>))
        (progn (forward-char 1)                ; skip > and initial blanks...
               (skip-chars-forward " \t")))))

(defun haskell-indent-line-to (n)
  "`indent-line-to' function but dealing with Bird-style literate scripts."
  (if (eq haskell-literate 'bird)
      (progn
        (beginning-of-line)
        (if (eq (following-char) ?\>)
            (delete-char 1))
        (delete-horizontal-space)       ; remove any starting TABs so
        (indent-line-to n)              ; that indent-line only adds spaces
        (save-excursion
          (beginning-of-line)
          (if (> n 0) (delete-char 1))  ; delete the first space before
          (insert ?\>)))                ; inserting a >
    (indent-line-to n)))

(defun haskell-indent-skip-blanks-and-newlines-forward (end)
  "Skips forward blanks, tabs and newlines until END taking
account of Bird style literate scripts."
  (skip-chars-forward " \t\n" end)
  (if (eq haskell-literate 'bird)
      (while (and (bolp) (eq (following-char) ?\>))
        (forward-char 1)                ; skip >
        (skip-chars-forward " \t\n" end))))

(defun haskell-indent-skip-blanks-and-newlines-backward (start)
  "Skips backward blanks, tabs and newlines upto START
taking account of Bird style literate scripts."
  (skip-chars-backward " \t\n" start)
  (if (eq haskell-literate 'bird)
      (while (and (eq (current-column) 1)
                  (eq (preceding-char) ?\>))
        (forward-char -1)               ; skip back >
        (skip-chars-backward " \t\n" start))))

;; specific functions for literate code

(defun haskell-indent-within-literate-code ()
  "Checks if point is within a part of literate Haskell code and if so
returns its start otherwise returns NIL:
If it is Bird Style, then returns the position of the >
otherwise returns the ending position \\begin{code}."
  (save-excursion
    (case haskell-literate
      (bird
       (beginning-of-line)
       (if (or (eq (following-char) ?\>)
               (and (bolp) (forward-line -1) (eq (following-char) ?\>)))
           (progn
             (while (and (zerop (forward-line -1))
                         (eq (following-char) ?\>)))
             (if (not (eq (following-char) ?\>))
                 (forward-line))
             (point))))
      ;;  Look for a \begin{code} or \end{code} line.
      (latex
       (if (re-search-backward
            "^\\(\\\\begin{code}$\\)\\|\\(\\\\end{code}$\\)" nil t)
           ;; within a literate code part if it was a \\begin{code}.
           (match-end 1)))
      (t (error "haskell-indent-within-literate-code: should not happen!")))))

(defun haskell-indent-put-region-in-literate (beg end &optional arg)
  "Put lines of the region as a piece of literate code.
With C-u prefix arg, remove indication that the region is literate code.
It deals with both Bird style and non Bird-style scripts."
  (interactive "r\nP")
  (unless haskell-literate
    (error "Cannot put a region in literate in a non literate script"))
  (if (eq haskell-literate 'bird)
      (let ((comment-start "> ")        ; Change dynamic bindings for
            (comment-start-skip "^> ?") ; comment-region.
            (comment-end "")
            (comment-end-skip "\n")
            (comment-style 'plain))
        (comment-region beg end arg))
    ;; Not Bird style.
    (if arg                             ; Remove the literate indication.
        (save-excursion
          (goto-char end)               ; Remove end.
          (if (re-search-backward "^\\\\end{code}[ \t\n]*\\="
                                  (line-beginning-position -2) t)
              (delete-region (point) (line-beginning-position 2)))
          (goto-char beg)               ; Remove end.
          (beginning-of-line)
          (if (looking-at "\\\\begin{code}")
              (kill-line 1)))
      (save-excursion                   ; Add the literate indication.
        (goto-char end)
        (unless (bolp) (newline))
        (insert "\\end{code}\n")
        (goto-char beg)
        (unless (bolp) (newline))
        (insert "\\begin{code}\n")))))

;;; Start of indentation code

(defcustom haskell-indent-look-past-empty-line t
  "If nil, indentation engine will not look past an empty line for layout points."
  :type 'boolean)

(defun haskell-indent-start-of-def ()
  "Return the position of the start of a definition.
The start of a def is expected to be recognizable by starting in column 0,
unless `haskell-indent-look-past-empty-line' is nil, in which case we
take a coarser approximation and stop at the first empty line."
  (save-excursion
    (let ((start-code (and haskell-literate
                           (haskell-indent-within-literate-code)))
          (top-col (if (eq haskell-literate 'bird) 2 0))
          (save-point (point)))
      ;; determine the starting point of the current piece of code
      (setq start-code (if start-code (1+ start-code) (point-min)))
      ;; go backward until the first preceding empty line
      (haskell-indent-forward-line -1)
      (while (and (if haskell-indent-look-past-empty-line
                      (or (> (haskell-indent-current-indentation) top-col)
                          (haskell-indent-empty-line-p))
                    (and (> (haskell-indent-current-indentation) top-col)
                         (not (haskell-indent-empty-line-p))))
                  (> (point) start-code)
                  (= 0 (haskell-indent-forward-line -1))))
      ;; go forward after the empty line
      (if (haskell-indent-empty-line-p)
          (haskell-indent-forward-line 1))
      (setq start-code (point))
      ;; find the first line of code which is not a comment
      (forward-comment (point-max))
      (if (> (point) save-point)
	  start-code
	(point)))))

(defun haskell-indent-open-structure (start end)
  "If any structure (list or tuple) is not closed, between START and END,
returns the location of the opening symbol, nil otherwise."
  (save-excursion
    (nth 1 (parse-partial-sexp start end))))

(defun haskell-indent-in-string (start end)
  "If a string is not closed , between START and END, returns the
location of the opening symbol, nil otherwise."
  (save-excursion
    (let ((pps (parse-partial-sexp start end)))
      (if (nth 3 pps) (nth 8 pps)))))

(defun haskell-indent-in-comment (start end)
  "Check, starting from START, if END is at or within a comment.
Returns the location of the start of the comment, nil otherwise."
  (let (pps)
    (assert (<= start end))
    (cond ((= start end) nil)
	  ((nth 4 (save-excursion (setq pps (parse-partial-sexp start end))))
	   (nth 8 pps))
	  ;; We also want to say that we are *at* the beginning of a comment.
	  ((and (not (nth 8 pps))
                (>= (point-max) (+ end 2))
		(nth 4 (save-excursion
			 (setq pps (parse-partial-sexp end (+ end 2))))))
	   (nth 8 pps)))))

(defvar haskell-indent-off-side-keywords-re
      "\\<\\(do\\|let\\|of\\|where\\)\\>[ \t]*")

(defun haskell-indent-type-at-point ()
  "Return the type of the line (also puts information in `match-data')."
  (cond
   ((haskell-indent-empty-line-p) 'empty)
   ((haskell-indent-in-comment (point-min) (point)) 'comment)
   ((looking-at "\\(\\([a-zA-Z]\\(\\sw\\|'\\)*\\)\\|_\\)[ \t\n]*") 'ident)
   ((looking-at "\\(|[^|]\\)[ \t\n]*") 'guard)
   ((looking-at "\\(=[^>=]\\|::\\|->\\|<-\\)[ \t\n]*") 'rhs)
   (t 'other)))

(defvar haskell-indent-current-line-first-ident ""
  "Global variable that keeps track of the first ident of the line to indent.")


(defun haskell-indent-contour-line (start end)
  "Generate contour information between START and END points."
  (if (< start end)
      (save-excursion
	(goto-char end)
	(haskell-indent-skip-blanks-and-newlines-backward start)
        (let ((cur-col (current-column))            ; maximum column number
              (fl 0)     ; number of lines that forward-line could not advance
              contour)
          (while (and (> cur-col 0) (= fl 0) (>= (point) start))
            (haskell-indent-back-to-indentation)
	    (if (< (point) start) (goto-char start))
            (and (not (member (haskell-indent-type-at-point)
                              '(empty comment))) ; skip empty and comment lines
                 (< (current-column) cur-col) ; less indented column found
                 (push (point) contour) ; new contour point found
                 (setq cur-col (current-column)))
            (setq fl (haskell-indent-forward-line -1)))
          contour))))

(defun haskell-indent-next-symbol (end)
  "Puts point to the next following symbol."
  (skip-syntax-forward ")" end)
  (if (< (point) end)
     (progn
       (forward-sexp 1)
       (haskell-indent-skip-blanks-and-newlines-forward end))))

(defun haskell-indent-separate-valdef (start end)
  "Returns a list of positions for important parts of a valdef."
  (save-excursion
    (let (valname valname-string aft-valname
                  guard aft-guard
                  rhs-sign aft-rhs-sign
                  type)
      ;; "parse" a valdef separating important parts
      (goto-char start)
      (setq type (haskell-indent-type-at-point))
      (if (or (memq type '(ident other))) ; possible start of a value def
          (progn
            (if (eq type 'ident)
                (progn
                  (setq valname (match-beginning 0))
                  (setq valname-string (match-string 0))
                  (goto-char (match-end 0)))
              (skip-chars-forward " \t" end)
              (setq valname (point))    ; type = other
              (haskell-indent-next-symbol end))
            (while (and (< (point) end)
                        (setq type (haskell-indent-type-at-point))
                        (or (memq type '(ident other))))
              (if (null aft-valname)
                  (setq aft-valname (point)))
              (haskell-indent-next-symbol end))))
      (if (and (< (point) end) (eq type 'guard)) ; start of a guard
          (progn
            (setq guard (match-beginning 0))
            (goto-char (match-end 0))
            (while (and (< (point) end)
                        (setq type (haskell-indent-type-at-point))
                        (not (eq type 'rhs)))
              (if (null aft-guard)
                  (setq aft-guard (point)))
              (haskell-indent-next-symbol end))))
      (if (and (< (point) end) (eq type 'rhs)) ; start of a rhs
          (progn
            (setq rhs-sign (match-beginning 0))
            (goto-char (match-end 0))
            (if (< (point) end)
                (setq aft-rhs-sign (point)))))
      (list valname valname-string aft-valname
            guard aft-guard rhs-sign aft-rhs-sign))))

(defsubst haskell-indent-no-otherwise (guard)
  "Check if there is no otherwise at GUARD."
  (save-excursion
    (goto-char guard)
    (not (looking-at "|[ \t]*otherwise\\>"))))


(defun haskell-indent-guard (start end end-visible indent-info)
  "Finds indentation information for a line starting with a guard."
  (save-excursion
    (let* ((haskell-indent-info indent-info)
           (sep (haskell-indent-separate-valdef start end))
           (valname (nth 0 sep))
           (guard (nth 3 sep))
           (rhs-sign (nth 5 sep)))
      ;; push information indentation for the visible part
      (if (and guard (< guard end-visible) (haskell-indent-no-otherwise guard))
          (haskell-indent-push-pos guard)
        (if rhs-sign
            (haskell-indent-push-pos rhs-sign) ; probably within a data definition...
          (if valname
              (haskell-indent-push-pos-offset valname))))
      haskell-indent-info)))

(defun haskell-indent-rhs (start end end-visible indent-info)
  "Finds indentation information for a line starting with a rhs."
  (save-excursion
    (let* ((haskell-indent-info indent-info)
           (sep (haskell-indent-separate-valdef start end))
           (valname (nth 0 sep))
           (guard (nth 3 sep))
           (rhs-sign (nth 5 sep)))
      ;; push information indentation for the visible part
      (if (and rhs-sign (< rhs-sign end-visible))
          (haskell-indent-push-pos rhs-sign)
        (if (and guard (< guard end-visible))
            (haskell-indent-push-pos-offset guard)
          (if valname                   ; always visible !!
              (haskell-indent-push-pos-offset valname))))
      haskell-indent-info)))

(defconst haskell-indent-decision-table
  (let ((or "\\)\\|\\("))
    (concat "\\("
            "1.1.11" or                 ; 1= vn gd rh arh
            "1.1.10" or                 ; 2= vn gd rh
            "1.1100" or                 ; 3= vn gd agd
            "1.1000" or                 ; 4= vn gd
            "1.0011" or                 ; 5= vn rh arh
            "1.0010" or                 ; 6= vn rh
            "110000" or                 ; 7= vn avn
            "100000" or                 ; 8= vn
            "001.11" or                 ; 9= gd rh arh
            "001.10" or                 ;10= gd rh
            "001100" or                 ;11= gd agd
            "001000" or                 ;12= gd
            "000011" or                 ;13= rh arh
            "000010" or                 ;14= rh
            "000000"                    ;15=
            "\\)")))

(defun haskell-indent-find-case (test)
  "Find the index that matches in the decision table."
  (if (string-match haskell-indent-decision-table test)
      ;; use the fact that the resulting match-data is a list of the form
      ;; (0 6 [2*(n-1) nil] 0 6) where n is the number of the matching regexp
      ;; so n= ((length match-data)/2)-1
      (- (/ (length (match-data 'integers)) 2) 1)
    (error "haskell-indent-find-case: impossible case: %s" test)))

(defun haskell-indent-empty (start end end-visible indent-info)
  "Finds indentation points for an empty line."
  (save-excursion
    (let* ((haskell-indent-info indent-info)
           (sep (haskell-indent-separate-valdef start end))
           (valname (pop sep))
           (valname-string (pop sep))
           (aft-valname (pop sep))
           (guard (pop sep))
           (aft-guard (pop sep))
           (rhs-sign (pop sep))
           (aft-rhs-sign (pop sep))
           (last-line (= end end-visible))
           (test (string
                  (if valname ?1 ?0)
                  (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                  (if (and guard (< guard end-visible)) ?1 ?0)
                  (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                  (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                  (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match haskell-indent-start-keywords-re valname-string))
          (progn
            (haskell-indent-push-pos valname)
            ;; very special for data keyword
            (if (string-match "\\<data\\>" valname-string)
                (if rhs-sign (haskell-indent-push-pos rhs-sign)
                  (haskell-indent-push-pos-offset valname))
              (haskell-indent-push-pos-offset valname)))
        (case                           ; general case
            (haskell-indent-find-case test)
          ;; "1.1.11"   1= vn gd rh arh
          (1 (haskell-indent-push-pos valname)
             (haskell-indent-push-pos valname valname-string)
             (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
             (haskell-indent-push-pos aft-rhs-sign))
          ;; "1.1.10"   2= vn gd rh
          (2 (haskell-indent-push-pos valname)
             (haskell-indent-push-pos valname valname-string)
             (if last-line
                 (haskell-indent-push-pos-offset guard)
               (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))))
          ;; "1.1100"   3= vn gd agd
          (3 (haskell-indent-push-pos valname)
             (haskell-indent-push-pos aft-guard)
             (if last-line (haskell-indent-push-pos-offset valname)))
          ;; "1.1000"   4= vn gd
          (4 (haskell-indent-push-pos valname)
             (if last-line (haskell-indent-push-pos-offset guard 2)))
          ;; "1.0011"   5= vn rh arh
          (5 (haskell-indent-push-pos valname)
             (if (or (and aft-valname (= (char-after rhs-sign) ?\=))
                     (= (char-after rhs-sign) ?\:))
                 (haskell-indent-push-pos valname valname-string))
             (haskell-indent-push-pos aft-rhs-sign))
          ;; "1.0010"   6= vn rh
          (6 (haskell-indent-push-pos valname)
             (haskell-indent-push-pos valname valname-string)
             (if last-line (haskell-indent-push-pos-offset valname)))
          ;; "110000"   7= vn avn
          (7 (haskell-indent-push-pos valname)
             (if last-line
                 (haskell-indent-push-pos aft-valname)
               (haskell-indent-push-pos valname valname-string)))
          ;; "100000"   8= vn
          (8 (haskell-indent-push-pos valname))
          ;; "001.11"   9= gd rh arh
          (9 (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
             (haskell-indent-push-pos aft-rhs-sign))
          ;; "001.10"  10= gd rh
          (10 (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
	      (if last-line (haskell-indent-push-pos-offset guard)))
          ;; "001100"  11= gd agd
          (11 (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
	      (haskell-indent-push-pos aft-guard))
          ;; "001000"  12= gd
          (12 (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
	      (if last-line (haskell-indent-push-pos-offset guard 2)))
          ;; "000011"  13= rh arh
          (13 (haskell-indent-push-pos aft-rhs-sign))
          ;; "000010"  14= rh
          (14 (if last-line (haskell-indent-push-pos-offset rhs-sign 2 )))
          ;; "000000"  15=
          (t (error "haskell-indent-empty: %s impossible case" test ))))
      haskell-indent-info)))

(defun haskell-indent-ident (start end end-visible indent-info)
  "Finds indentation points for a line starting with an identifier."
  (save-excursion
    (let*
        ((haskell-indent-info indent-info)
         (sep (haskell-indent-separate-valdef start end))
         (valname (pop sep))
         (valname-string (pop sep))
         (aft-valname (pop sep))
         (guard (pop sep))
         (aft-guard (pop sep))
         (rhs-sign (pop sep))
         (aft-rhs-sign (pop sep))
         (last-line (= end end-visible))
         (is-where
          (string-match "where[ \t]*" haskell-indent-current-line-first-ident))
         (diff-first                 ; not a function def with the same name
          (not(string= valname-string haskell-indent-current-line-first-ident)))
         ;; (is-type-def
         ;;  (and rhs-sign (eq (char-after rhs-sign) ?\:)))
         (test (string
                (if valname ?1 ?0)
                (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                (if (and guard (< guard end-visible)) ?1 ?0)
                (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match haskell-indent-start-keywords-re valname-string))
          (progn
            (haskell-indent-push-pos valname)
            (if (string-match "\\<data\\>" valname-string)
                ;; very special for data keyword
                (if aft-rhs-sign (haskell-indent-push-pos aft-rhs-sign)
                  (haskell-indent-push-pos-offset valname))
              (if (not (string-match
                        haskell-indent-start-keywords-re
                        haskell-indent-current-line-first-ident))
                  (haskell-indent-push-pos-offset valname))))
        (if (string= haskell-indent-current-line-first-ident "::")
            (if valname (haskell-indent-push-pos valname))
          (case                         ; general case
              (haskell-indent-find-case test)
            ;; "1.1.11"   1= vn gd rh arh
            (1 (if is-where
                   (haskell-indent-push-pos guard)
                 (haskell-indent-push-pos valname)
                 (if diff-first (haskell-indent-push-pos aft-rhs-sign))))
            ;; "1.1.10"   2= vn gd rh
            (2 (if is-where
                   (haskell-indent-push-pos guard)
                 (haskell-indent-push-pos valname)
                 (if last-line
                     (haskell-indent-push-pos-offset guard))))
            ;; "1.1100"   3= vn gd agd
            (3 (if is-where
                   (haskell-indent-push-pos-offset guard)
                 (haskell-indent-push-pos valname)
                 (if diff-first
                     (haskell-indent-push-pos aft-guard))))
            ;; "1.1000"   4= vn gd
            (4 (if is-where
                   (haskell-indent-push-pos guard)
                 (haskell-indent-push-pos valname)
                 (if last-line
                     (haskell-indent-push-pos-offset guard 2))))
            ;; "1.0011"   5= vn rh arh
            (5 (if is-where
                   (haskell-indent-push-pos-offset valname)
                 (haskell-indent-push-pos valname)
                 (if diff-first
                     (haskell-indent-push-pos aft-rhs-sign))))
            ;; "1.0010"   6= vn rh
            (6 (if is-where
                   (haskell-indent-push-pos-offset valname)
                 (haskell-indent-push-pos valname)
                 (if last-line
                     (haskell-indent-push-pos-offset valname))))
            ;; "110000"   7= vn avn
            (7 (if is-where
                   (haskell-indent-push-pos-offset valname)
                 (haskell-indent-push-pos valname)
                 (if last-line
                     (haskell-indent-push-pos aft-valname))))
            ;; "100000"   8= vn
            (8 (if is-where
                   (haskell-indent-push-pos-offset valname)
                 (haskell-indent-push-pos valname)))
            ;; "001.11"   9= gd rh arh
            (9 (if is-where
                   (haskell-indent-push-pos guard)
                 (haskell-indent-push-pos aft-rhs-sign)))
            ;; "001.10"  10= gd rh
            (10 (if is-where
                    (haskell-indent-push-pos guard)
                  (if last-line
                      (haskell-indent-push-pos-offset guard))))
            ;; "001100"  11= gd agd
            (11 (if is-where
                    (haskell-indent-push-pos guard)
                  (if (haskell-indent-no-otherwise guard)
                      (haskell-indent-push-pos aft-guard))))
            ;; "001000"  12= gd
            (12 (if last-line (haskell-indent-push-pos-offset guard 2)))
            ;; "000011"  13= rh arh
            (13 (haskell-indent-push-pos aft-rhs-sign))
            ;; "000010"  14= rh
            (14 (if last-line (haskell-indent-push-pos-offset rhs-sign 2)))
            ;; "000000"  15=
            (t (error "haskell-indent-ident: %s impossible case" test )))))
      haskell-indent-info)))

(defun haskell-indent-other (start end end-visible indent-info)
  "Finds indentation points for a non-empty line starting with something other
than an identifier, a guard or rhs."
  (save-excursion
    (let* ((haskell-indent-info indent-info)
           (sep (haskell-indent-separate-valdef start end))
           (valname (pop sep))
           (valname-string (pop sep))
           (aft-valname (pop sep))
           (guard (pop sep))
           (aft-guard (pop sep))
           (rhs-sign (pop sep))
           (aft-rhs-sign (pop sep))
           (last-line (= end end-visible))
           (test (string
                  (if valname ?1 ?0)
                  (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                  (if (and guard (< guard end-visible)) ?1 ?0)
                  (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                  (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                  (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match haskell-indent-start-keywords-re valname-string))
          (haskell-indent-push-pos-offset valname)
        (case                           ; general case
         (haskell-indent-find-case test)
         ;; "1.1.11"   1= vn gd rh arh
         (1 (haskell-indent-push-pos aft-rhs-sign))
         ;; "1.1.10"   2= vn gd rh
         (2 (if last-line
                   (haskell-indent-push-pos-offset guard)
               (haskell-indent-push-pos-offset rhs-sign 2)))
         ;; "1.1100"   3= vn gd agd
         (3 (haskell-indent-push-pos aft-guard))
         ;; "1.1000"   4= vn gd
         (4 (haskell-indent-push-pos-offset guard 2))
         ;; "1.0011"   5= vn rh arh
         (5 (haskell-indent-push-pos valname)
            (haskell-indent-push-pos aft-rhs-sign))
         ;; "1.0010"   6= vn rh
         (6 (if last-line
                (haskell-indent-push-pos-offset valname)
              (haskell-indent-push-pos-offset rhs-sign 2)))
         ;; "110000"   7= vn avn
         (7 (haskell-indent-push-pos-offset aft-valname))
         ;; "100000"   8= vn
         (8 (haskell-indent-push-pos valname))
         ;; "001.11"   9= gd rh arh
         (9 (haskell-indent-push-pos aft-rhs-sign))
         ;; "001.10"  10= gd rh
         (10 (if last-line
                   (haskell-indent-push-pos-offset guard)
               (haskell-indent-push-pos-offset rhs-sign 2)))
         ;; "001100"  11= gd agd
         (11 (if (haskell-indent-no-otherwise guard)
                   (haskell-indent-push-pos aft-guard)))
         ;; "001000"  12= gd
         (12 (if last-line (haskell-indent-push-pos-offset guard 2)))
         ;; "000011"  13= rh arh
         (13 (haskell-indent-push-pos aft-rhs-sign))
         ;; "000010"  14= rh
         (14 (if last-line (haskell-indent-push-pos-offset rhs-sign 2)))
         ;; "000000"  15=
         (t (error "haskell-indent-other: %s impossible case" test ))))
      haskell-indent-info)))

(defun haskell-indent-valdef-indentation (start end end-visible curr-line-type
                                          indent-info)
  "Find indentation information for a value definition."
  (let ((haskell-indent-info indent-info))
    (if (< start end-visible)
        (case curr-line-type
          (empty (haskell-indent-empty start end end-visible indent-info))
          (ident (haskell-indent-ident start end end-visible indent-info))
          (guard (haskell-indent-guard start end end-visible indent-info))
          (rhs   (haskell-indent-rhs start end end-visible indent-info))
          (comment (error "Comment indent should never happen"))
          (other (haskell-indent-other start end end-visible indent-info)))
      haskell-indent-info)))

(defun haskell-indent-line-indentation (line-start line-end end-visible
                                         curr-line-type indent-info)
  "Compute indentation info between LINE-START and END-VISIBLE.
Separate a line of program into valdefs between offside keywords
and find indentation info for each part."
  (save-excursion
    ;; point is (already) at line-start
    (assert (eq (point) line-start))
    (let ((haskell-indent-info indent-info)
          (start (or (haskell-indent-in-comment line-start line-end)
                     (haskell-indent-in-string line-start line-end))))
      (if start                         ; if comment at the end
          (setq line-end start))  ; end line before it
      ;; loop on all parts separated by off-side-keywords
      (while (and (re-search-forward haskell-indent-off-side-keywords-re
                                     line-end t)
                  (not (or (haskell-indent-in-comment line-start (point))
                           (haskell-indent-in-string line-start (point)))))
	(let ((beg-match (match-beginning 0)) ; save beginning of match
	      (end-match (match-end 0)))      ; save end of match
          ;; Do not try to find indentation points if off-side-keyword at
          ;; the start...
          (if (or (< line-start beg-match)
                  ;; Actually, if we're looking at a "let" inside a "do", we
                  ;; should add the corresponding indentation point.
                  (eq (char-after beg-match) ?l))
              (setq haskell-indent-info
                    (haskell-indent-valdef-indentation line-start beg-match
                                                       end-visible
                                                       curr-line-type
                                                       haskell-indent-info)))
          ;; ...but keep the start of the line if keyword alone on the line
          (if (= line-end end-match)
              (haskell-indent-push-pos beg-match))
          (setq line-start end-match)
          (goto-char line-start)))
      (haskell-indent-valdef-indentation line-start line-end end-visible
                                         curr-line-type haskell-indent-info))))


(defun haskell-indent-layout-indent-info (start contour-line)
  (let ((haskell-indent-info nil)
        (curr-line-type (haskell-indent-type-at-point))
	line-start line-end end-visible)
    (save-excursion
      (if (eq curr-line-type 'ident)
	  (let				; guess the type of line
	      ((sep
		(haskell-indent-separate-valdef
		 (point) (line-end-position))))
	    ;; if the first ident is where or the start of a def
	    ;; keep it in a global variable
	    (setq haskell-indent-current-line-first-ident
		  (if (string-match "where[ \t]*" (nth 1 sep))
		      (nth 1 sep)
		    (if (nth 5 sep)		; is there a rhs-sign
			(if (= (char-after (nth 5 sep)) ?\:) ;is it a typdef
			    "::" (nth 1 sep))
		      "")))))
      (while contour-line		; explore the contour points
	(setq line-start (pop contour-line))
	(goto-char line-start)
	(setq line-end (line-end-position))
	(setq end-visible		; visible until the column of the
	      (if contour-line		; next contour point
		  (save-excursion
		    (move-to-column
		     (haskell-indent-point-to-col (car contour-line)))
		    (point))
		line-end))
	(unless (or (haskell-indent-open-structure start line-start)
		    (haskell-indent-in-comment start line-start))
	  (setq haskell-indent-info
		(haskell-indent-line-indentation line-start line-end
						 end-visible curr-line-type
						 haskell-indent-info)))))
    haskell-indent-info))

(defun haskell-indent-find-matching-start (regexp limit &optional pred start)
  (let ((open (haskell-indent-open-structure limit (point))))
    (if open (setq limit (1+ open))))
  (unless start (setq start (point)))
  (when (re-search-backward regexp limit t)
    (let ((nestedcase (match-end 1))
          (outer (or (haskell-indent-in-string limit (point))
                     (haskell-indent-in-comment limit (point))
                     (haskell-indent-open-structure limit (point))
                     (if (and pred (funcall pred start)) (point)))))
      (cond
       (outer
        (goto-char outer)
        (haskell-indent-find-matching-start regexp limit pred start))
       (nestedcase
        ;; Nested case.
        (and (haskell-indent-find-matching-start regexp limit pred)
             (haskell-indent-find-matching-start regexp limit pred start)))
       (t (point))))))

(defun haskell-indent-filter-let-no-in (start)
  "Return non-nil if point is in front of a `let' that has no `in'.
START is the position of the presumed `in'."
  ;; We're looking at either `in' or `let'.
  (when (looking-at "let")
    (ignore-errors
      (save-excursion
        (forward-word 1)
        (forward-comment (point-max))
        (if (looking-at "{")
            (progn
              (forward-sexp 1)
              (forward-comment (point-max))
              (< (point) start))
          ;; Use the layout rule to see whether this let is already closed
          ;; without an `in'.
          (let ((col (current-column)))
            (while (progn (forward-line 1) (haskell-indent-back-to-indentation)
                          (< (point) start))
              (when (< (current-column) col)
                (setq col nil)
                (goto-char start)))
            (null col)))))))

(defun haskell-indent-comment (open start)
  "Compute indent info for comments and text inside comments.
OPEN is the start position of the comment in which point is."
  ;; Ideally we'd want to guess whether it's commented out code or
  ;; whether it's text.  Instead, we'll assume it's text.
  (save-excursion
    (if (= open (point))
	;; We're actually just in front of a comment: align with following
	;; code or with comment on previous line.
        (let ((prev-line-info
               (cond
                ((eq (char-after) ?\{) nil) ;Align as if it were code.
                ((and (forward-comment -1)
                      (> (line-beginning-position 3) open))
                 ;; We're after another comment and there's no empty line
                 ;; between us.
                 (list (list (haskell-indent-point-to-col (point)))))
                (t nil))))              ;Else align as if it were code
          ;; Align with following code.
          (forward-comment (point-max))
          ;; There are several possible indentation points for this code-line,
          ;; but the only valid indentation point for the comment is the one
          ;; that the user will select for the code-line.  Obviously we can't
          ;; know that, so we just assume that the code-line is already at its
          ;; proper place.
          ;; Strictly speaking "assume it's at its proper place" would mean
          ;; we'd just use (current-column), but since this is using info from
          ;; lines further down and it's common to reindent line-by-line,
          ;; we'll align not with the current indentation, but with the
          ;; one that auto-indentation "will" select.
          (append
           prev-line-info
           (let ((indent-info (save-excursion
                                (haskell-indent-indentation-info start)))
                 (col (current-column)))
             ;; Sort the indent-info so that the current indentation comes
             ;; out first.
             (setq indent-info
                   (sort indent-info
                         (lambda (x y)
                           (<= (abs (- col (car x))) (abs (- col (car y)))))))
             indent-info)))

      ;; We really are inside a comment.
      (if (looking-at "-}")
	  (progn
	    (forward-char 2)
	    (forward-comment -1)
            (list (list (1+ (haskell-indent-point-to-col (point))))))
	(let ((offset (if (looking-at "--?")
			  (- (match-beginning 0) (match-end 0)))))
	  (forward-line -1)		;Go to previous line.
	  (haskell-indent-back-to-indentation)
	  (if (< (point) start) (goto-char start))

          (list (list (if (looking-at comment-start-skip)
                          (if offset
                              (+ 2 offset (haskell-indent-point-to-col (point)))
                            (haskell-indent-point-to-col (match-end 0)))
                        (haskell-indent-point-to-col (point))))))))))

(defun haskell-indent-closing-keyword (start)
  (let ((open (save-excursion
                (haskell-indent-find-matching-start
                 (case (char-after)
                   (?i "\\<\\(?:\\(in\\)\\|let\\)\\>")
                   (?o "\\<\\(?:\\(of\\)\\|case\\)\\>")
                   (?t "\\<\\(?:\\(then\\)\\|if\\)\\>")
                   (?e "\\<\\(?:\\(else\\)\\|if\\)\\>"))
                 start
                 (if (eq (char-after) ?i)
                     ;; Filter out the `let's that have no `in'.
                     'haskell-indent-filter-let-no-in)))))
    ;; For a "hanging let/case/if at EOL" we should use a different
    ;; indentation scheme.
    (save-excursion
      (goto-char open)
      (if (haskell-indent-hanging-p)
          (setq open (haskell-indent-virtual-indentation start))))
    (list (list (haskell-indent-point-to-col open)))))

(defcustom haskell-indent-after-keywords
  '(("where" 2 0)
    ("of" 2)
    ("do" 2)
    ("in" 2 0)
    ("{" 2)
    "if"
    "then"
    "else"
    "let")
  "Keywords after which indentation should be indented by some offset.
Each keyword info can have the following forms:

   KEYWORD | (KEYWORD OFFSET [OFFSET-HANGING])

If absent OFFSET-HANGING defaults to OFFSET.
If absent OFFSET defaults to `haskell-indent-offset'.

OFFSET-HANGING is the offset to use in the case where the keyword
is at the end of an otherwise-non-empty line."
  :type '(repeat (choice string
                         (cons :tag "" (string :tag "keyword:")
                         (cons :tag "" (integer :tag "offset")
                         (choice (const nil)
                                 (list :tag ""
                                       (integer :tag "offset-pending"))))))))

(defun haskell-indent-skip-lexeme-forward ()
  (and (zerop (skip-syntax-forward "w"))
       (skip-syntax-forward "_")
       (skip-syntax-forward "(")
       (skip-syntax-forward ")")))

(defvar haskell-indent-inhibit-after-offset nil)

(defun haskell-indent-offset-after-info ()
  "Return the info from `haskell-indent-after-keywords' for keyword at point."
  (let ((id (buffer-substring
             (point)
             (save-excursion
               (haskell-indent-skip-lexeme-forward)
               (point)))))
    (or (assoc id haskell-indent-after-keywords)
        (car (member id haskell-indent-after-keywords)))))

(defcustom haskell-indent-dont-hang '("(")
  "Lexemes that should never be considered as hanging."
  :type '(repeat string))

(defun haskell-indent-hanging-p ()
  ;; A Hanging keyword is one that's at the end of a line except it's not at
  ;; the beginning of a line.
  (not (or (= (current-column) (haskell-indent-current-indentation))
           (save-excursion
             (let ((lexeme
                    (buffer-substring
                     (point)
                     (progn (haskell-indent-skip-lexeme-forward) (point)))))
               (or (member lexeme haskell-indent-dont-hang)
                   (> (line-end-position)
                      (progn (forward-comment (point-max)) (point)))))))))

(defun haskell-indent-after-keyword-column (offset-info start &optional default)
  (unless offset-info
    (setq offset-info (haskell-indent-offset-after-info)))
  (unless default (setq default haskell-indent-offset))
  (setq offset-info
        (if haskell-indent-inhibit-after-offset '(0) (cdr-safe offset-info)))
  (if (not (haskell-indent-hanging-p))
      (+ (current-column) (or (car offset-info) default))
    ;; The keyword is hanging at the end of the line.
    (+ (haskell-indent-virtual-indentation start)
       (or (cadr offset-info) (car offset-info) default))))

(defun haskell-indent-inside-paren (open)
  ;; there is an open structure to complete
  (if (looking-at "\\s)\\|[;,]")
      ;; A close-paren or a , or ; can only correspond syntactically to
      ;; the open-paren at `open'.  So there is no ambiguity.
      (progn
        (if (or (and (eq (char-after) ?\;) (eq (char-after open) ?\())
                (and (eq (char-after) ?\,) (eq (char-after open) ?\{)))
            (message "Mismatched punctuation: `%c' in %c...%c"
                     (char-after) (char-after open)
                     (if (eq (char-after open) ?\() ?\) ?\})))
        (save-excursion
          (goto-char open)
          (list (list
                 (if (haskell-indent-hanging-p)
                     (haskell-indent-virtual-indentation nil)
                   (haskell-indent-point-to-col open))))))
    ;; There might still be layout within the open structure.
    (let* ((end (point))
           (basic-indent-info
             ;; Anything else than a ) is subject to layout.
             (if (looking-at "\\s.\\|\\$ ")
                 (haskell-indent-point-to-col open) ; align a punct with (
               (let ((follow (save-excursion
                               (goto-char (1+ open))
                               (haskell-indent-skip-blanks-and-newlines-forward end)
                               (point))))
                 (if (= follow end)
                     (save-excursion
                       (goto-char open)
                       (haskell-indent-after-keyword-column nil nil 1))
                   (haskell-indent-point-to-col follow)))))
           (open-column (haskell-indent-point-to-col open))
           (contour-line (haskell-indent-contour-line (1+ open) end)))
      (if (null contour-line)
          (list (list basic-indent-info))
        (let ((indent-info
               (haskell-indent-layout-indent-info
                (1+ open) contour-line)))
          ;; Fix up indent info.
          (let ((base-elem (assoc open-column indent-info)))
            (if base-elem
                (progn (setcar base-elem basic-indent-info)
                       (setcdr base-elem nil))
              (setq indent-info
                    (append indent-info (list (list basic-indent-info)))))
            indent-info))))))

(defun haskell-indent-virtual-indentation (start)
  "Compute the \"virtual indentation\" of text at point.
The \"virtual indentation\" is the indentation that text at point would have
had, if it had been placed on its own line."
  (let ((col (current-column))
        (haskell-indent-inhibit-after-offset (haskell-indent-hanging-p)))
    (if (save-excursion (skip-chars-backward " \t") (bolp))
        ;; If the text is indeed on its own line, than the virtual indent is
        ;; the current indentation.
        col
      ;; Else, compute the indentation that it would have had.
      (let ((info (haskell-indent-indentation-info start))
            (max -1))
        ;; `info' is a list of possible indent points.  Each indent point is
        ;; assumed to correspond to a different parse.  So we need to find
        ;; the parse that corresponds to the case at hand (where there's no
        ;; line break), which is assumed to always be the
        ;; deepest indentation.
        (dolist (x info)
          (setq x (car x))
          ;; Sometimes `info' includes the current indentation (or yet
          ;; deeper) by mistake, because haskell-indent-indentation-info
          ;; wasn't designed to be called on a piece of text that is not at
          ;; BOL.  So ignore points past `col'.
          (if (and (> x max) (not (>= x col)))
              (setq max x)))
        ;; In case all the indent points are past `col', just use `col'.
        (if (>= max 0) max col)))))

(defun haskell-indent-indentation-info (&optional start)
  "Return a list of possible indentations for the current line.
These are then used by `haskell-indent-cycle'.
START if non-nil is a presumed start pos of the current definition."
  (unless start (setq start (haskell-indent-start-of-def)))
  (let (open contour-line)
    (cond
     ;; in string?
     ((setq open (haskell-indent-in-string start (point)))
      (list (list (+ (haskell-indent-point-to-col open)
                     (if (looking-at "\\\\") 0 1)))))

     ;; in comment ?
     ((setq open (haskell-indent-in-comment start (point)))
      (haskell-indent-comment open start))

     ;; Closing the declaration part of a `let' or the test exp part of a case.
     ((looking-at "\\(?:in\\|of\\|then\\|else\\)\\>")
      (haskell-indent-closing-keyword start))

     ;; Right after a special keyword.
     ((save-excursion
        (forward-comment (- (point-max)))
        (when (and (not (zerop (skip-syntax-backward "w")))
                   (setq open (haskell-indent-offset-after-info)))
          (list (list (haskell-indent-after-keyword-column open start))))))

     ;; open structure? ie  ( { [
     ((setq open (haskell-indent-open-structure start (point)))
      (haskell-indent-inside-paren open))

     ;; full indentation
     ((setq contour-line (haskell-indent-contour-line start (point)))
      (haskell-indent-layout-indent-info start contour-line))

     (t
      ;; simple contour just one indentation at start
      (list (list (if (and (eq haskell-literate 'bird)
                           (eq (haskell-indent-point-to-col start) 1))
                      ;; for a Bird style literate script put default offset
                      ;; in the case of no indentation
                      (1+ haskell-indent-literate-Bird-default-offset)
                    (haskell-indent-point-to-col start))))))))

(defvar haskell-indent-last-info nil)


(defun haskell-indent-cycle ()
  "Indentation cycle.
We stay in the cycle as long as the TAB key is pressed."
  (interactive "*")
  (if (and haskell-literate
           (not (haskell-indent-within-literate-code)))
      ;; use the ordinary tab for text...
      (funcall (default-value 'indent-line-function))
    (let ((marker (if (> (current-column) (haskell-indent-current-indentation))
		      (point-marker)))
	  (bol (progn (beginning-of-line) (point))))
      (haskell-indent-back-to-indentation)
      (unless (and (eq last-command this-command)
		   (eq bol (car haskell-indent-last-info)))
	(save-excursion
	  (setq haskell-indent-last-info
		(list bol (haskell-indent-indentation-info) 0 0))))

      (let* ((il (nth 1 haskell-indent-last-info))
	     (index (nth 2 haskell-indent-last-info))
	     (last-insert-length (nth 3 haskell-indent-last-info))
	     (indent-info (nth index il)))

	(haskell-indent-line-to (car indent-info)) ; insert indentation
	(delete-char last-insert-length)
	(setq last-insert-length 0)
	(let ((text (cdr indent-info)))
	  (if text
	      (progn
		(insert text)
		(setq last-insert-length (length text)))))

	(setq haskell-indent-last-info
	      (list bol il (% (1+ index) (length il)) last-insert-length))

	(if (= (length il) 1)
	    (message "Sole indentation")
	  (message "Indent cycle (%d)..." (length il)))

	(if marker
	    (goto-char (marker-position marker)))))))

;;; alignment functions

(defun haskell-indent-shift-columns (dest-column region-stack)
  "Shifts columns in region-stack to go to DEST-COLUMN.
Elements of the stack are pairs of points giving the start and end
of the regions to move."
  (let (reg col diffcol reg-end)
    (while (setq reg (pop region-stack))
      (setq reg-end (copy-marker (cdr reg)))
      (goto-char (car reg))
      (setq col (current-column))
      (setq diffcol (- dest-column col))
      (if (not (zerop diffcol))
          (catch 'end-of-buffer
            (while (<= (point) (marker-position reg-end))
              (if (< diffcol 0)
                  (backward-delete-char-untabify (- diffcol) nil)
                (insert-char ?\  diffcol))
              (end-of-line 2)           ; should be (forward-line 1)
              (if (eobp)                ; but it adds line at the end...
                  (throw 'end-of-buffer nil))
              (move-to-column col)))))))

(defun haskell-indent-align-def (p-arg type)
  "Align guards or rhs within the current definition before point.
If P-ARG is t align all defs up to the mark.
TYPE is either 'guard or 'rhs."
  (save-excursion
    (let (start-block end-block
          (maxcol (if (eq type 'rhs) haskell-indent-rhs-align-column 0))
          contour sep defname defnamepos
          defcol pos lastpos
          regstack eqns-start start-found)
      ;; find the starting and ending boundary points for alignment
      (if p-arg
          (if (mark)                    ; aligning everything in the region
            (progn
              (when (> (mark) (point)) (exchange-point-and-mark))
              (setq start-block
                    (save-excursion
                      (goto-char (mark))
                      (line-beginning-position)))
              (setq end-block
                  (progn (if (haskell-indent-bolp)
                             (haskell-indent-forward-line -1))
                         (line-end-position))))
            (error "The mark is not set for aligning definitions"))
        ;; aligning the current definition
        (setq start-block (haskell-indent-start-of-def))
        (setq end-block (line-end-position)))
      ;; find the start of the current valdef using the contour line
      ;; in reverse order because we need the nearest one from the end
      (setq contour
            (reverse (haskell-indent-contour-line start-block end-block)))
      (setq pos (car contour))          ; keep the start of the first contour
      ;; find the nearest start of a definition
      (while (and (not defname) contour)
        (goto-char (pop contour))
        (if (haskell-indent-open-structure start-block (point))
            nil
          (setq sep (haskell-indent-separate-valdef (point) end-block))
          (if (nth 5 sep)               ; is there a rhs?
              (progn (setq defnamepos (nth 0 sep))
                     (setq defname (nth 1 sep))))))
      ;; start building the region stack
      (if defnamepos
          (progn                        ; there is a valdef
            ;; find the start of each equation or guard
            (if p-arg      ; when indenting a region
                ;; accept any start of id or pattern as def name
                (setq defname "\\<\\|("))
            (setq defcol (haskell-indent-point-to-col defnamepos))
            (goto-char pos)
            (setq end-block (line-end-position))
            (catch 'top-of-buffer
              (while (and (not start-found)
                          (>= (point) start-block))
                (if (<= (haskell-indent-current-indentation) defcol)
                    (progn
                      (move-to-column defcol)
                      (if (and (looking-at defname) ; start of equation
                               (not (haskell-indent-open-structure start-block (point))))
                          (push (cons (point) 'eqn) eqns-start)
                        ;; found a less indented point not starting an equation
                        (setq start-found t)))
                  ;; more indented line
                  (haskell-indent-back-to-indentation)
                  (if (and (eq (haskell-indent-type-at-point) 'guard) ; start of a guard
                           (not (haskell-indent-open-structure start-block (point))))
                      (push (cons (point) 'gd) eqns-start)))
                (if (bobp)
                    (throw 'top-of-buffer nil)
                  (haskell-indent-backward-to-indentation 1))))
            ;; remove the spurious guards before the first equation
            (while (and eqns-start (eq (cdar eqns-start) 'gd))
              (pop eqns-start))
            ;; go through each equation to find the region to indent
            (while eqns-start
              (let ((eqn (caar eqns-start)))
		(setq lastpos (if (cdr eqns-start)
				  (save-excursion
				    (goto-char (caadr eqns-start))
				    (haskell-indent-forward-line -1)
				    (line-end-position))
				end-block))
		(setq sep (haskell-indent-separate-valdef eqn lastpos)))
              (if (eq type 'guard)
                  (setq pos (nth 3 sep))
                ;; check if what follows a rhs sign is more indented or not
                (let ((rhs (nth 5 sep))
                      (aft-rhs (nth 6 sep)))
                  (if (and rhs aft-rhs
                           (> (haskell-indent-point-to-col rhs)
                              (haskell-indent-point-to-col aft-rhs)))
                      (setq pos aft-rhs)
                    (setq pos rhs))))
              (if pos
                  (progn                ; update region stack
                    (push (cons pos (or lastpos pos)) regstack)
                    (setq maxcol        ; find the highest column number
                          (max maxcol
                               (progn   ;find the previous non-empty column
                                 (goto-char pos)
                                 (skip-chars-backward
                                  " \t"
                                  (line-beginning-position))
                                 (if (haskell-indent-bolp)
                                     ;;if on an empty prefix
                                     (haskell-indent-point-to-col pos) ;keep original indent
                                   (1+ (haskell-indent-point-to-col (point)))))))))
              (pop eqns-start))
            ;; now shift according to the region stack
            (if regstack
                (haskell-indent-shift-columns maxcol regstack)))))))

(defun haskell-indent-align-guards-and-rhs (start end)
  "Align the guards and rhs of functions in the region which must be active."
  ;; The `start' and `end' args are dummys right now: they're just there so
  ;; we can use the "r" interactive spec which properly signals an error.
  (interactive "*r")
  (haskell-indent-align-def t 'guard)
  (haskell-indent-align-def t 'rhs))

;;;  insertion functions

(defun haskell-indent-insert-equal ()
  "Insert an = sign and align the previous rhs of the current function."
  (interactive "*")
  (if (or (haskell-indent-bolp)
          (/= (preceding-char) ?\ ))
      (insert ?\ ))
  (insert "= ")
  (haskell-indent-align-def (haskell-indent-mark-active) 'rhs))

(defun haskell-indent-insert-guard (&optional text)
  "Insert and align a guard sign (|) followed by optional TEXT.
Alignment works only if all guards are to the south-east of their |."
  (interactive "*")
  (let ((pc (if (haskell-indent-bolp) ?\012
                (preceding-char)))
        (pc1 (or (char-after (- (point) 2)) 0)))
    ;; check what guard to insert depending on the previous context
    (if (= pc ?\ )                      ; x = any char other than blank or |
        (if (/= pc1 ?\|)
            (insert "| ")               ; after " x"
          ())                           ; after " |"
      (if (= pc ?\|)
          (if (= pc1 ?\|)
              (insert " | ")            ; after "||"
            (insert " "))               ; after "x|"
        (insert " | ")))                ; general case
    (if text (insert text))
    (haskell-indent-align-def (haskell-indent-mark-active) 'guard)))

(defun haskell-indent-insert-otherwise ()
  "Insert a guard sign (|) followed by 'otherwise' and align the
previous guards of the current function."
  (interactive "*")
  (haskell-indent-insert-guard "otherwise")
  (haskell-indent-insert-equal))

(defun haskell-indent-insert-where ()
  "Insert and a where keyword at point and indent the resulting
line with an indentation cycle."
  (interactive "*")
  (insert "where ")
  (haskell-indent-cycle))


;;; haskell-indent-mode

(defvar haskell-indent-mode nil
  "Indicates if the semi-intelligent Haskell indentation mode is in effect
in the current buffer.")
(make-variable-buffer-local 'haskell-indent-mode)

(defun turn-on-haskell-indent ()
  "Turn on ``intelligent'' haskell indentation mode."
  (set (make-local-variable 'indent-line-function) 'haskell-indent-cycle)
  ;; Removed: remapping DEL seems a bit naughty --SDM
  ;; (local-set-key "\177"  'backward-delete-char-untabify)
  ;; The binding to TAB is already handled by indent-line-function.  --Stef
  ;; (local-set-key "\t"    'haskell-indent-cycle)
  (local-set-key [?\C-c ?\C-=] 'haskell-indent-insert-equal)
  (local-set-key [?\C-c ?\C-|] 'haskell-indent-insert-guard)
  (local-set-key [?\C-c ?\C-o] 'haskell-indent-insert-otherwise)
  (local-set-key [?\C-c ?\C-w] 'haskell-indent-insert-where)
  (local-set-key [?\C-c ?\C-.] 'haskell-indent-align-guards-and-rhs)
  (local-set-key [?\C-c ?\C->] 'haskell-indent-put-region-in-literate)
  (setq haskell-indent-mode t)
  (run-hooks 'haskell-indent-hook))

(defun turn-off-haskell-indent ()
  "Turn off ``intelligent'' haskell indentation mode that deals with
the layout rule of Haskell."
  (kill-local-variable 'indent-line-function)
  ;; (local-unset-key "\t")
  ;; (local-unset-key "\177")
  (local-unset-key [?\C-c ?\C-=])
  (local-unset-key [?\C-c ?\C-|])
  (local-unset-key [?\C-c ?\C-o])
  (local-unset-key [?\C-c ?\C-w])
  (local-unset-key [?\C-c ?\C-.])
  (local-unset-key [?\C-c ?\C->])
  (setq haskell-indent-mode nil))

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'haskell-indent-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((haskell-indent-mode " Ind")))))

;;;###autoload
(defun haskell-indent-mode (&optional arg)
  "``intelligent'' Haskell indentation mode that deals with
the layout rule of Haskell.  \\[haskell-indent-cycle] starts the cycle
which proposes new possibilities as long as the TAB key is pressed.
Any other key or mouse click terminates the cycle and is interpreted
except for RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Note: \\[indent-region] which applies \\[haskell-indent-cycle] for each line
of the region also works but it stops and asks for any line having more
than one possible indentation.
Use TAB to cycle until the right indentation is found and then RET to go the
next line to indent.

Invokes `haskell-indent-hook' if not nil."
  (interactive "P")
  (setq haskell-indent-mode
        (if (null arg) (not haskell-indent-mode)
          (> (prefix-numeric-value arg) 0)))
  (if haskell-indent-mode
      (turn-on-haskell-indent)
    (turn-off-haskell-indent)))

(provide 'haskell-indent)

;; arch-tag: e4e5e90a-12e2-4002-b5cb-7b2375710013
;;; haskell-indent.el ends here

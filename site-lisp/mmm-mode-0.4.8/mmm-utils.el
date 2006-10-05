;;; mmm-utils.el --- Coding Utilities for MMM Mode

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: $Id: mmm-utils.el,v 1.14 2003/03/09 17:04:04 viritrilbia Exp $

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

;; This file provides a number of macros and other coding utilities
;; for MMM Mode.

;;; Code:

(require 'cl)

;;{{{ Valid Buffer

;; We used to wrap almost everything in this, but I realized that
;; only `mmm-mode-on' really needs it. Kept it as a macro, though,
;; for modularity and in case we need it somewhere else.
(defmacro mmm-valid-buffer (&rest body)
  "Execute BODY if in a valid buffer for MMM Mode to be enabled.  This
means not hidden, not a minibuffer, not in batch mode, and not in of
`mmm-never-modes'."
  `(unless (or (eq (aref (buffer-name) 0) ?\ )
               (window-minibuffer-p (selected-window))
               (memq major-mode mmm-never-modes)
               noninteractive
               ;; Unnecessary as now hidden
;;;               (equal (buffer-name) mmm-temp-buffer-name)
               )
     ,@body))

;;;(def-edebug-spec mmm-valid-buffer t)

;;}}}
;;{{{ Save Everything

;; Never trust callback functions to preserve anything.
(defmacro mmm-save-all (&rest body)
  "Execute BODY forms, then restoring point, mark, current buffer,
restrictions, and match data."
  `(save-excursion
     (save-restriction
       (save-match-data
         ,@body))))

;;;(def-edebug-spec mmm-save-all t)

;;}}}
;;{{{ String Formatting

(defun mmm-format-string (string arg-pairs)
  "Format STRING by replacing arguments as specified by ARG-PAIRS.
Each element of ARG-PAIRS is \(REGEXP . STR) where each STR is to be
substituted for the corresponding REGEXP wherever it matches."
  (let ((case-fold-search nil))
    (save-match-data
      (dolist (pair arg-pairs)
        (while (string-match (car pair) string)
          (setq string (replace-match (cdr pair) t t string))))))
  string)

(defun mmm-format-matches (string &optional on-string)
  "Format STRING by matches from the current match data.
Strings like ~N are replaced by the Nth subexpression from the last
global match.  Does nothing if STRING is not a string.

ON-STRING, if supplied, means to use the match data from a
`string-match' on that string, rather than the global match data."
  (when (stringp string)
    (let ((old-data (match-data))
          subexp)
      (save-match-data
        (while (string-match "~\\([0-9]\\)" string)
          (setq subexp (string-to-int (match-string-no-properties 1 string))
                string (replace-match
			(save-match-data
			  (set-match-data old-data)
			  (match-string-no-properties subexp on-string))
			t t string))))))
  string)

;;}}}
;;{{{ Save Keywords

(defmacro mmm-save-keyword (param)
  "If the value of PARAM as a variable is non-nil, return the list
\(:PARAM (symbol-value PARAM)), otherwise NIL. Best used only when it
is important that nil values disappear."
  `(if (and (boundp ',param) ,param)
       (list (intern (concat ":" (symbol-name ',param))) ,param)
     nil))

(defmacro mmm-save-keywords (&rest params)
  "Return a list saving the non-nil elements of PARAMS. E.g.
\(let \(\(a 1) \(c 2)) \(mmm-save-keywords a b c))  ==>  \(:a 1 :c 2)
Use of this macro can make code more readable when there are a lot of
PARAMS, but less readable when there are only a few. Also best used
only when it is important that nil values disappear."
  `(append ,@(mapcar #'(lambda (param)
                         (macroexpand `(mmm-save-keyword ,param)))
                     params)))

;;}}}
;;{{{ Looking Back At

(defun mmm-looking-back-at (regexp &optional bound)
  "Return t if text before point matches REGEXP.
Modifies the match data. If supplied, BOUND means not to look farther
back that that many characters before point. Otherwise, it defaults to
\(length REGEXP), which is good enough when REGEXP is a simple
string."
  (eq (point)
      (save-excursion
        (and (re-search-backward regexp
               (- (point) (or bound (length regexp)))
               t)
             (match-end 0)))))

;;}}}
;;{{{ Markers

;; Mostly for remembering interactively made regions
(defun mmm-make-marker (pos beg-p sticky-p)
  "Make, and return, a marker at POS that is or isn't sticky.
BEG-P represents whether the marker delimits the beginning of a
region \(or the end of it). STICKY-P is whether it should be sticky,
i.e. whether text inserted at the marker should be inside the region."
  (let ((mkr (set-marker (make-marker) pos)))
    (set-marker-insertion-type mkr (if beg-p (not sticky-p) sticky-p))
    mkr))

;;}}}

(provide 'mmm-utils)

;;; mmm-utils.el ends here
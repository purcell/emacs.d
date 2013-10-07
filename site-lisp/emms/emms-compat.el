;;; emms-compat.el --- Compatibility routines for EMMS

;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Michael Olson <mwolson@gnu.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; These are functions and macros that EMMS needs in order to be
;; compatible with various Emacs and XEmacs versions.

;;; Code:


;;; Miscellaneous

(defun emms-propertize (string &rest properties)
  (if (fboundp 'propertize)
      (apply #'propertize string properties)
    (set-text-properties 0 (length string) properties string)
    string))

;; Emacs accepts three arguments to `make-obsolete', but the XEmacs
;; version only takes two arguments
(defun emms-make-obsolete (old-name new-name when)
  "Make the byte-compiler warn that OLD-NAME is obsolete.
The warning will say that NEW-NAME should be used instead.
WHEN should be a string indicating when the function was
first made obsolete, either the file's revision number or an
EMMS release version number."
  (if (featurep 'xemacs)
      (make-obsolete old-name new-name)
    (make-obsolete old-name new-name when)))


;;; Time and timers

(defun emms-cancel-timer (timer)
  "Cancel the given TIMER."
  (when timer
    (cond ((fboundp 'cancel-timer)
           (cancel-timer timer))
          ((fboundp 'delete-itimer)
           (delete-itimer timer)))))

(defun emms-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (nth 1 t1) (nth 1 t2)))))


;;; Highline

(defun emms-activate-highlighting-mode ()
  "Activate highline mode."
  (if (featurep 'xemacs)
      (progn
        (require 'highline)
        (highline-local-mode 1))
    (progn
      (require 'hl-line)
      (hl-line-mode 1))))

(defun emms-line-highlight ()
  "Highlight the current line. You must call
emms-activate-highlighting-mode beforehand."
  (if (featurep 'xemacs)
      (highline-highlight-current-line)
    (hl-line-highlight)))


;;; Movement and position

(defun emms-move-beginning-of-line (arg)
  "Move point to beginning of current line as displayed.
If there's an image in the line, this disregards newlines
which are part of the text that the image rests on."
  (if (fboundp 'move-beginning-of-line)
      (move-beginning-of-line arg)
    (if (numberp arg)
        (forward-line (1- arg))
      (forward-line 0))))

(defun emms-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
  (if (fboundp 'line-number-at-pos)
      (line-number-at-pos pos)
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))


;;; Regular expression matching

(defun emms-replace-regexp-in-string (regexp replacement text
                                      &optional fixedcase literal)
  "Replace REGEXP with REPLACEMENT in TEXT.
If fourth arg FIXEDCASE is non-nil, do not alter case of replacement text.
If fifth arg LITERAL is non-nil, insert REPLACEMENT literally."
  (cond
   ((fboundp 'replace-regexp-in-string)
    (replace-regexp-in-string regexp replacement text fixedcase literal))
   ((and (featurep 'xemacs) (fboundp 'replace-in-string))
    (replace-in-string text regexp replacement literal))
   (t (let ((repl-len (length replacement))
            start)
        (save-match-data
          (while (setq start (string-match regexp text start))
            (setq start (+ start repl-len)
                  text (replace-match replacement fixedcase literal text)))))
      text)))

(defun emms-match-string-no-properties (num &optional string)
  (if (fboundp 'match-string-no-properties)
      (match-string-no-properties num string)
    (match-string num string)))


;;; Common Lisp

(defun emms-delete-if (predicate seq)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
  ;; remove from car
  (while (when (funcall predicate (car seq))
           (setq seq (cdr seq))))
  ;; remove from cdr
  (let ((ptr seq)
        (next (cdr seq)))
    (while next
      (when (funcall predicate (car next))
        (setcdr ptr (if (consp next)
                        (cdr next)
                      nil)))
      (setq ptr (cdr ptr))
      (setq next (cdr ptr))))
  seq)

(defun emms-find-if (predicate seq)
  "Find the first item satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found."
  (catch 'found
    (dolist (el seq)
      (when (funcall predicate el)
        (throw 'found el)))))

(defun emms-remove-if-not (predicate seq)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ to
avoid corrupting the original SEQ."
  (let (newseq)
    (dolist (el seq)
      (when (funcall predicate el)
        (setq newseq (cons el newseq))))
    (nreverse newseq)))

(provide 'emms-compat)
;;; emms-compat.el ends here

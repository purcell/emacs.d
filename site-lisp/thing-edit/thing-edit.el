;;; thing-edit.el --- Extension thing edit

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-06-08 00:42:07
;; Version: 1.2.1
;; Last-Updated: 2009-01-20 17:25:40
;; URL: http://www.emacswiki.org/emacs/download/thing-edit.el
;; Keywords: thingatpt, edit
;; Compatibility: GNU Emacs 23.0.60.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;;      `thingatpt'
;;

;;; Commentary:
;;
;; This package is some useful functions that base on `thingatpt.el'.
;; Those function can copy or paste special data object quickly
;; and don't need to move cursor.
;; Just binding your like keystroke to those functions.
;;
;; thing-paste-sexp                paste regular expression around cursor.
;; thing-copy-sexp                 copy regular expression around cursor.
;;
;; thing-paste-email               paste email string around cursor
;; thing-copy-email                copy email string around cursor.
;;
;; thing-paste-filename            paste filename string around cursor.
;; thing-copy-filename             copy filename string around cursor.
;;
;; thing-paste-url                 paste url string around cursor.
;; thing-copy-url                  copy url string around cursor.
;;
;; thing-paste-word                paste word string around cursor.
;; thing-copy-word                 copy word string around cursor.
;;
;; thing-paste-symbol              paste symbol string around cursor.
;; thing-copy-symbol               copy symbol string around cursor.
;;
;; thing-paste-defun               paste function string around cursor.
;; thing-copy-defun                copy function string around cursor.
;;
;; thing-paste-list                paste list string around cursor.
;; thing-copy-list                 copy list string around cursor.
;;
;; thing-paste-sentence            paste sentence string around cursor.
;; thing-copy-sentence             copy sentence string around cursor.
;;
;; thing-paste-whitespace          paste whitespace string around cursor.
;; thing-copy-whitespace           copy whitespace string around cursor.
;;
;; thing-paste-page                paste page string around cursor.
;; thing-copy-page                 copy page string around cursor.
;;
;; thing-paste-line                paste current line.
;; thing-copy-line                 copy current line.
;;
;; thing-paste-to-line-end         paste string to end of line.
;; thing-copy-to-line-end          copy string to end of line.
;;
;; thing-paste-to-line-beginning   paste string to beginning of line.
;; thing-copy-to-line-beginning    copy string to beginning of line.
;;
;; thing-paste-comment             paste comment.
;; thing-copy-comment              copy comment.
;;

;;; Installation:
;;
;; Copy thing-edit.el to your load-path and add to your ~/.emacs
;;
;;      (require 'thing-edit)
;;
;; No more need

;;; Change log:
;;
;; 2009/01/13
;;      * Add many functions.
;;
;; 2009/01/09
;;      * Move functions `thing-paste-parentheses' and `thing-copy-parentheses'
;;        to file `thing-edit-extension.el', avoid this package depend `paredit'.
;;
;; 2008/09/26
;;      * Rebuild program framework, make code more clearly.
;;
;; 2008/07/28
;;      * Modified function `lazy-search-mark-object' use paredit's function to
;;        select content between parenthesis.
;;
;; 2008/06/19
;;      * Modified search method of `thing-copy-parentheses' and `thing-paste-parentheses'.
;;
;; 2008/06/08
;;      * Add edit of `url', `filename', `email', `sexp'
;;      * And ignore `defun', `page', `whitespace', `list' `sentence'.
;;      * Add basal function that apply thingatpt library.
;;      * Complete that `word', `symbol', `line', `line-to-beg', `line-to-end'
;;      * Comment is not use `thingatpt', but operate is allied.
;;

;;; Acknowledgments:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Require
(require 'thingatpt)

;;; Code:

(defun thing-edit-internal (object-beg object-end &optional kill-conditional)
  "A fast edit complexes object.
Argument OBJECT-BEG the begin position that object.
Argument OBJECT-END the end position of object.
Optional argument KILL-CONDITIONAL default is do copy handle, if KILL-CONDITIONAL is non-nil do paste handle."
  (interactive)
  (if kill-conditional
      (progn
        (message "%s pasted." (buffer-substring object-beg object-end))
        (kill-region object-beg object-end))
    (message "%s copied." (buffer-substring object-beg object-end))
    (kill-ring-save object-beg object-end)))

(defun thing-edit (thing &optional kill-conditional)
  "This function is a simple interface for `thing-edit-internal'.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (save-excursion
    (thing-edit-internal (beginning-of-thing thing)
                         (end-of-thing thing)
                         kill-conditional)))

(defun thing-paste-sexp ()
  "Paste regular expression at current point."
  (interactive)
  (thing-edit 'sexp t))

(defun thing-copy-sexp ()
  "Copy regular expression at current point."
  (interactive)
  (thing-edit 'sexp))

(defun thing-paste-email ()
  "Paste email at current point."
  (interactive)
  (thing-edit 'email t))

(defun thing-copy-email ()
  "Copy email at current point."
  (interactive)
  (thing-edit 'email))

(defun thing-paste-filename ()
  "Paste filename at current point."
  (interactive)
  (thing-edit 'filename t))

(defun thing-copy-filename ()
  "Copy filename at current point."
  (interactive)
  (thing-edit 'filename))

(defun thing-paste-url ()
  "Paste url at current point."
  (interactive)
  (thing-edit 'url t))

(defun thing-copy-url ()
  "Copy url at current point."
  (interactive)
  (thing-edit 'url))

(defun thing-paste-word ()
  "Paste words at point."
  (interactive)
  (thing-edit 'word t))

(defun thing-copy-word ()
  "Copy words at point."
  (interactive)
  (thing-edit 'word))

(defun thing-paste-symbol ()
  "Paste symbol around point."
  (interactive)
  (thing-edit 'symbol t))

(defun thing-copy-symbol ()
  "Copy symbol around point."
  (interactive)
  (thing-edit 'symbol))

(defun thing-paste-line ()
  "Paste current line into Kill-Ring without mark the line."
  (interactive)
  (thing-edit 'line t))

(defun thing-copy-line ()
  "Copy current line into Kill-Ring without mark the line."
  (interactive)
  (thing-edit 'line))

(defun thing-paste-defun ()
  "Paste function around point."
  (interactive)
  (thing-edit 'defun t))

(defun thing-copy-defun ()
  "Paste function around point."
  (interactive)
  (thing-edit 'defun))

(defun thing-paste-list ()
  "Paste list around point."
  (interactive)
  (thing-edit 'list t))

(defun thing-copy-list ()
  "Paste list around point."
  (interactive)
  (thing-edit 'list))

(defun thing-paste-sentence ()
  "Paste sentence around point."
  (interactive)
  (thing-edit 'sentence t))

(defun thing-copy-sentence ()
  "Paste sentence around point."
  (interactive)
  (thing-edit 'sentence))

(defun thing-paste-whitespace ()
  "Paste whitespace around point."
  (interactive)
  (thing-edit 'whitespace t))

(defun thing-copy-whitespace ()
  "Paste whitespace around point."
  (interactive)
  (thing-edit 'whitespace))

(defun thing-paste-page ()
  "Paste page around point."
  (interactive)
  (thing-edit 'page t))

(defun thing-copy-page ()
  "Paste page around point."
  (interactive)
  (thing-edit 'page))

;; Below function is not base on thingatpt, but it's effect like above function.
;; So i add to this package.
(defun thing-paste-to-line-end ()
  "Paste content from current point to line end."
  (interactive)
  (thing-copy-to-line-end t))

(defun thing-copy-to-line-end (&optional kill-conditional)
  "Copy content from current point to line end.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (thing-edit-internal (point)
                         (line-end-position)
                         kill-conditional)))

(defun thing-paste-to-line-beginning ()
  "Paste content from current point to line beginning."
  (interactive)
  (thing-copy-to-line-beginning t))

(defun thing-copy-to-line-beginning (&optional kill-conditional)
  "Copy content from current point tot line beginning.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (thing-edit-internal (line-beginning-position)
                         (point)
                         kill-conditional)))

(defun thing-paste-comment ()
  "Paste the comment around line.
If mark is active, it can paste all comment that in mark."
  (interactive)
  (thing-copy-comment t))

(defun thing-copy-comment (&optional kill-conditional)
  "Copy the comment around line.
If mark is active, it can copy all comment that in mark.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (when mark-active
      (setq beg (region-beginning))
      (setq end (region-end))
      (deactivate-mark))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (while (< (point) end)
        (if (comment-search-forward end t)
            (if kill-conditional
                (call-interactively 'comment-kill)
              (call-interactively 'comment-copy))
          (goto-char end))))))

(provide 'thing-edit)

;;; thing-edit.el ends here

;;; LocalWords:  el thingatpt paredit paredit's

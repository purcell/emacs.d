;;; haskell-simple-indent.el --- Simple indentation module for Haskell Mode

;; Copyright (C) 1998 Heribert Schuetz, Graeme E Moss

;; Authors:
;;   1998 Heribert Schuetz <Heribert.Schuetz@informatik.uni-muenchen.de> and
;;        Graeme E Moss <gem@cs.york.ac.uk>
;; Keywords: indentation files Haskell
;; Version: 1.0
;; URL: http://www.cs.york.ac.uk/~gem/haskell-mode/simple-indent.html

;; This file is not part of GNU Emacs.

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
;; To support simple indentation of Haskell scripts.
;;
;;
;; Installation:
;; 
;; To bind TAB to the indentation command for all Haskell buffers, add
;; this to .emacs:
;;
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;;
;; Otherwise, call `turn-on-haskell-simple-indent'.
;;
;;
;; Customisation:
;;
;; None supported.
;;
;;
;; History:
;;
;; If you have any problems or suggestions, after consulting the list
;; below, email gem@cs.york.ac.uk quoting the version of you are
;; using, the version of Emacs you are using, and a small example of
;; the problem or suggestion.
;;
;; Version 1.0:
;;   Brought over from Haskell mode v1.1.
;;
;; Present Limitations/Future Work (contributions are most welcome!):
;;
;; (None so far.)

;;; Code:

;; All functions/variables start with
;; `(turn-(on/off)-)haskell-simple-indent'.

;; Version.
(defconst haskell-simple-indent-version "1.2"
  "`haskell-simple-indent' version number.")
(defun haskell-simple-indent-version ()
  "Echo the current version of `haskell-simple-indent' in the minibuffer."
  (interactive)
  (message "Using haskell-simple-indent version %s"
           haskell-simple-indent-version))

;; Partly stolen from `indent-relative' in indent.el:
(defun haskell-simple-indent ()
  "Space out to under next visible indent point.
Indent points are positions of non-whitespace following whitespace in
lines preceeding point.  A position is visible if it is to the left of
the first non-whitespace of every nonblank line between the position and
the current line.  If there is no visible indent point beyond the current
column, `tab-to-tab-stop' is done instead."
  (interactive)
  (let* ((start-column (current-column))
         (invisible-from nil)		; `nil' means infinity here
         (indent
          (catch 'haskell-simple-indent-break
            (save-excursion
              (while (progn (beginning-of-line)
                            (not (bobp)))
                (forward-line -1)
                (if (not (looking-at "[ \t]*\n"))
                    (let ((this-indentation (current-indentation)))
                      (if (or (not invisible-from)
                              (< this-indentation invisible-from))
                          (if (> this-indentation start-column)
                              (setq invisible-from this-indentation)
                            (let ((end (line-beginning-position 2)))
                              (move-to-column start-column)
                              ;; Is start-column inside a tab on this line?
                              (if (> (current-column) start-column)
                                  (backward-char 1))
                              (or (looking-at "[ \t]")
                                  (skip-chars-forward "^ \t" end))
                              (skip-chars-forward " \t" end)
                              (let ((col (current-column)))
                                (throw 'haskell-simple-indent-break
                                       (if (or (= (point) end)
                                               (and invisible-from
                                                    (> col invisible-from)))
                                           invisible-from
                                         col)))))))))))))
    (if indent
	(let ((opoint (point-marker)))
	  (indent-line-to indent)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (set-marker opoint nil))
      (tab-to-tab-stop))))

(defvar haskell-simple-indent-old)

;; The main functions.
(defun turn-on-haskell-simple-indent ()
  "Set `indent-line-function' to a simple indentation function.
TAB will now move the cursor to the next indent point in the previous
nonblank line.  An indent point is a non-whitespace character following
whitespace.

Runs `haskell-simple-indent-hook'.

Use `haskell-simple-indent-version' to find out what version this is."
  (set (make-local-variable 'haskell-simple-indent-old) indent-line-function)
  (set (make-local-variable 'indent-line-function) 'haskell-simple-indent)
  (run-hooks 'haskell-simple-indent-hook))

(defun turn-off-haskell-simple-indent ()
  "Return `indent-line-function' to original value.
I.e. the value before `turn-on-haskell-simple-indent' was called."
  (when (local-variable-p 'haskell-simple-indent-old)
    (setq indent-line-function haskell-simple-indent-old)
    (kill-local-variable 'haskell-simple-indent-old)))

;; Provide ourselves:

(provide 'haskell-simple-indent)

;; arch-tag: 18a08122-723b-485e-b958-e1cf8218b816
;;; haskell-simple-indent.el ends here

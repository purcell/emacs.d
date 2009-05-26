;;; fringe-helper.el --- helper functions for fringe bitmaps
;;
;; Copyright (C) 2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1.1
;; Keywords: lisp
;; URL: http://nschum.de/src/emacs/fringe-helper/
;; Compatibility: GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; fringe-helper contains helper functions for fringe bitmaps.
;;
;; `fringe-helper-define' allows you to to define fringe bitmaps using a visual
;; string replesentation.  For example:
;;
;; (fringe-helper-define 'test-bitmap '(top repeat)
;;   "XX......"
;;   "..XX...."
;;   "....XX.."
;;   "......XX")
;;
;; You can also generate arguments for `define-fringe-bitmap' yourself, by
;; using `fringe-helper-convert'.
;;
;; fringe-helper also provides a few stock bitmaps.  They are loaded on demand
;; by `fringe-lib-load' and adapt to the current fringe size to a certain
;; extend.
;;
;; `fringe-helper-insert' inserts a fringe bitmap at point and
;; `fringe-helper-insert-region' inserts a fringe bitmap along a region.
;; `fringe-helper-remove' removes both kinds.
;;
;;
;; Here's an example for enhancing `flymake-mode' with fringe bitmaps:
;;
;; (require 'fringe-helper)
;; (require 'flymake)
;;
;; (defvar flymake-fringe-overlays nil)
;; (make-variable-buffer-local 'flymake-fringe-overlays)
;;
;; (defadvice flymake-make-overlay (after add-to-fringe first
;;                                  (beg end tooltip-text face mouse-face)
;;                                  activate compile)
;;   (push (fringe-helper-insert-region
;;          beg end
;;          (fringe-lib-load (if (eq face 'flymake-errline)
;;                               fringe-lib-exclamation-mark
;;                             fringe-lib-question-mark))
;;          'left-fringe 'font-lock-warning-face)
;;         flymake-fringe-overlays))
;;
;; (defadvice flymake-delete-own-overlays (after remove-from-fringe activate
;;                                         compile)
;;   (mapc 'fringe-helper-remove flymake-fringe-overlays)
;;   (setq flymake-fringe-overlays nil))
;;
;;
;;; Change Log:
;;
;; 2008-06-04 (0.1.1)
;;    Fixed bug where `fringe-helper-remove' missed overlays at the end.
;;    Fixed `fringe-lib-load' to work when already loaded.
;;
;; 2008-04-25 (0.1)
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(defun fringe-helper-convert (&rest strings)
  "Convert STRINGS into a vector usable for `define-fringe-bitmap'.
Each string in STRINGS represents a line of the fringe bitmap.
Periods (.) are background-colored pixel; Xs are foreground-colored.  The
fringe bitmap always is aligned to the right.  If the fringe has half
width, only the left 4 pixels of an 8 pixel bitmap will be shown.

For example, the following code defines a diagonal line.

\(fringe-helper-convert
  \"XX......\"
  \"..XX....\"
  \"....XX..\"
  \"......XX\"\)"
  (unless (cdr strings)
    ;; only one string, probably with newlines
    (setq strings (split-string (car strings) "\n")))
  (apply 'vector
         (mapcar (lambda (str)
                   (let ((num 0))
                     (dolist (c (string-to-list str))
                       (setq num (+ (* num 2) (if (eq c ?.) 0 1))))
                     num))
                 strings)))

(defmacro fringe-helper-define (name alignment &rest strings)
  "Define a fringe bitmap from a visual representation.
Parameters NAME and ALIGNMENT are the same as `define-fringe-bitmap'.
Each string in STRINGS represents a line of the fringe bitmap as in
`fringe-helper-convert'."
  (declare (indent defun))
  `(define-fringe-bitmap ,name
                         (eval-when-compile (fringe-helper-convert ,@strings))
                         nil nil ,alignment))

(defun fringe-helper-insert (bitmap pos &optional side face)
  "Insert a fringe bitmap at POS.
BITMAP is the name of a bitmap defined with `define-fringe-bitmap' or
`fringe-helper-define'.  SIDE defaults to 'left-fringe and can also be
'right-fringe.  FACE is used to determine the bitmap's color.
The function returns an object suitable for passing to
`fringe-helper-remove'."
  (let* ((display-string `(,(or side 'left-fringe) ,bitmap .
                           ,(when face (cons face nil))))
          (before-string (propertize "!" 'display display-string))
          (ov (make-overlay pos pos)))
    (overlay-put ov 'before-string before-string)
    (overlay-put ov 'fringe-helper t)
    ov))

(defun fringe-helper-insert-region (beg end bitmap side &optional face)
  "Insert fringe bitmaps between BEG and END.
BITMAP is the name of a bitmap defined with `define-fringe-bitmap' or
`fringe-helper-define'.  SIDE defaults to 'left-fringe and can also be
'right-fringe.  FACE is used to determine the bitmap's color.  The
function returns an overlay covering the entire region, which is suitable
for passing to `fringe-helper-remove'.  The region grows and shrinks with
input automatically."
  (let* ((display-string `(,(or side 'left-fringe) ,bitmap .
                           ,(when face (cons face nil))))
         (before-string (propertize "!" 'display display-string))
         (parent (make-overlay beg end))
         ov)
    (save-excursion
      (goto-char beg)
      (goto-char (point-at-bol 2))
      ;; can't use <= here, or we'll get an infinity loop at buffer end
      (while (and (<= (point) end) (< (point) (point-max)))
        (setq ov (make-overlay (point) (point)))
        (overlay-put ov 'before-string before-string)
        (overlay-put ov 'fringe-helper-parent parent)
        (goto-char (point-at-bol 2))))
    (overlay-put parent 'fringe-helper t)
    (overlay-put parent 'before-string before-string)
    (overlay-put parent 'insert-in-front-hooks
                 '(fringe-helper-modification-func))
    (overlay-put parent 'modification-hooks
                 '(fringe-helper-modification-func))
    parent))

(defun fringe-helper-modification-func (ov after-p beg end &optional len)
  (if after-p
      (if (eq beg end)
          ;; evaporate overlay
          (when (= (overlay-start ov) (overlay-end ov))
            (delete-overlay ov))
        ;; if new lines are inserted, add new bitmaps
        (let ((before-string (overlay-get ov 'before-string))
              fringe-ov)
          (save-excursion
            (goto-char beg)
            (while (search-forward "\n" end t)
              (setq fringe-ov (make-overlay (point) (point)))
              (overlay-put fringe-ov 'before-string before-string)
              (overlay-put fringe-ov 'fringe-helper-parent ov)))))
    ;; if a \n is removed, remove the fringe overlay
    (unless (= beg end)
      (setq beg (max beg (overlay-start ov)))
      (setq end (min end (overlay-end ov)))
      (save-excursion
        (goto-char beg)
        (while (search-forward "\n" end t)
          (let ((overlays (overlays-in (point) (1+ (point)))))
            (while overlays
              (when (eq (overlay-get (car overlays) 'fringe-helper-parent) ov)
                (delete-overlay (car overlays))
                (setq overlays nil))
              (pop overlays))))))))

(defun fringe-helper-remove (fringe-bitmap-reference)
  "Remove a fringe bitmap."
  (unless (or (not (overlay-buffer fringe-bitmap-reference))
              (overlay-get fringe-bitmap-reference 'fringe-helper-parent))
    ;; region
    (dolist (ov (overlays-in (overlay-start fringe-bitmap-reference)
                             (1+ (overlay-end fringe-bitmap-reference))))
      (when (eq (overlay-get ov 'fringe-helper-parent) fringe-bitmap-reference)
        (delete-overlay ov)))
    (delete-overlay fringe-bitmap-reference)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fringe-lib-load (pattern &optional side)
  "Load a stock bitmap.
It returns the symbol name of the loaded bitmap, which is suitable for passing
to `fringe-helper-insert'.  The actual work of defining the bitmap is only done once.
PATTERN can be one of the following:

`fringe-lib-exclamation-mark':  an exclamation mark

`fringe-lib-question-mark':  a question mark

`fringe-lib-zig-zag':  a zig-zag pattern

`fringe-lib-wave':  a wavy-line pattern

`fringe-lib-stipple':  a stipple pattern

`fringe-lib-full':  a solid color

SIDE should be either 'left-fringe or 'right-fringe and defaults to the former."
  (let ((fringe-width (frame-parameter (selected-frame)
                                       (or side 'left-fringe)))
        (alignment (when (eq (car pattern) 'repeat)
                     (setq pattern (cdr pattern))
                     '(top t))))
    (while (> (caar pattern) fringe-width)
      (pop pattern))
    (setq pattern (cdar pattern))
    (or (car (memq (car pattern) fringe-bitmaps))
        (define-fringe-bitmap (car pattern) (cdr pattern) nil nil alignment))))


(defconst fringe-lib-exclamation-mark
  `((5 fringe-lib-exclamation-mark-5 .
       ,(eval-when-compile
          (fringe-helper-convert "...XX..."
                                 "..XXXX.."
                                 "..XXXX.."
                                 "...XX..."
                                 "...XX..."
                                 "........"
                                 "........"
                                 "...XX..."
                                 "...XX...")))
    (0 fringe-lib-exclamation-mark-0 .
       ,(eval-when-compile
          (fringe-helper-convert ".XX....."
                                 ".XX....."
                                 ".XX....."
                                 ".XX....."
                                 ".XX....."
                                 "........"
                                 "........"
                                 ".XX....."
                                 ".XX.....")))))

(defconst fringe-lib-question-mark
  `((5 fringe-lib-question-mark-5 .
       ,(eval-when-compile
          (fringe-helper-convert "...XX..."
                                 "..XXXX.."
                                 "..X..X.."
                                 "....XX.."
                                 "...XX..."
                                 "...XX..."
                                 "........"
                                 "...XX..."
                                 "...XX...")))
    (0 fringe-lib-question-mark-0 .
       ,(eval-when-compile
          (fringe-helper-convert ".XX....."
                                 "XXXX...."
                                 "X..X...."
                                 "..XX...."
                                 ".XX....."
                                 ".XX....."
                                 "........"
                                 ".XX....."
                                 ".XX.....")))))

(defconst fringe-lib-zig-zag
  `(repeat
    (0 fringe-lib-zig-zag-0 .
       ,(eval-when-compile
          (fringe-helper-convert "X......."
                                 "X......."
                                 ".X......"
                                 ".X......"
                                 "..X....."
                                 "..X....."
                                 ".X......"
                                 ".X......")))))

(defconst fringe-lib-wave
  `(repeat
    (0 fringe-lib-wave-0 .
       ,(eval-when-compile
          (fringe-helper-convert "X......."
                                 ".X......"
                                 "..X....."
                                 "..X....."
                                 "..X....."
                                 ".X......"
                                 "X......."
                                 "X.......")))))

(defconst fringe-lib-stipple
  `(repeat
    (0 fringe-lib-stipple-0 .
       ,(eval-when-compile
          (fringe-helper-convert "XXXXXXXX"
                                 "XXXXXXXX"
                                 "XXXXXXXX"
                                 "........"
                                 "........"
                                 "........")))))

(defconst fringe-lib-full
  `(repeat
    (0 fringe-lib-full-0 .
       ,(eval-when-compile
          (fringe-helper-convert "XXXXXXXX")))))

(provide 'fringe-helper)
;;; fringe-helper.el ends here

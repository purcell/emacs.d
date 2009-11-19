;;; ac-dabbrev.el --- auto-complete.el source for dabbrev
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2009 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 0.0.8
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org, http://trac.codecheck.in

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'ac-dabbrev)
;; (setq ac-sources
;;      (list ac-source-dabbrev
;;            ))

;;; Change Log
;; 0.0.8: fix parameter for auto-complete.el 1.0a
;; 0.0.7: fix parameter for auto-complete.el 0.3.0
;; 0.0.6: fix face for auto-complete.el 0.2.0
;; 0.0.5: add face.
;; 0.0.4: new valiable:ac-dabbrev-reset-char. this valiable is suga idea. this valiable reset count when this valiable have insert char.
;;        new valiable:ac-dabbrev-reset-always. reset count when ac-candidates is nil and this valiable non-nil.
;; -.-.-: document typo fix.
;; 0.0.3: new valiable:ac-dabbrev-sort. sort expansions.
;; 0.0.2: new variable:ac-dabbrev-all-min-count. this valiable is anything-dabbrev-expand.el idea.
;; 0.0.1: ac-dabbrev.el 0.0.1 released.

;;; Code:

(eval-when-compile (require 'cl))
(require 'auto-complete)
(require 'dabbrev)

(defvar ac-dabbrev-all-min-count 4)

(defvar ac-dabbrev-trigger-commands '(self-insert-command delete-backward-char)
  "Increment count when this valiable is last-command")

(defvar ac-dabbrev-reset-char nil
  "Reset count when this valiable have insert char.") ;(setq ac-dabbrev-reset-char '("-"))

(defvar ac-dabbrev-reset-always nil
  "Non-nil means reset count when candidates is 0.")

(defvar ac-dabbrev-sort nil)

(defun ac-dabbrev-find-limit-expansions (abbrev limit ignore-case)
  "Return a list of limit expansions of ABBREV.
If IGNORE-CASE is non-nil, accept matches which differ in case."
  (let ((all-expansions nil) (i 0)
        expansion)
    (save-excursion
      (goto-char (point-min))
      (while
          (and (< i limit)
               (setq expansion (dabbrev--find-expansion abbrev -1 ignore-case)))
        (setq all-expansions (cons expansion all-expansions))
        (setq i (+ i 1)))
      (if ac-dabbrev-sort
          (sort all-expansions (lambda (s1 s2) (if (< (length s1) (length s2)) t nil)))
        all-expansions))))

(defun ac-dabbrev-get-limit-candidates (abbrev &optional all)
  (let ((dabbrev-check-other-buffers all))
    (dabbrev--reset-global-variables)
    (ac-dabbrev-find-limit-expansions abbrev ac-candidate-max nil)))

(lexical-let ((count 1))
  (defun ac-dabbrev-set-count ()
    (setq command-char last-command-char)
    (if (memq last-command ac-dabbrev-trigger-commands)
        (incf count)
      (setq count 1))
    (if (and ac-dabbrev-reset-always
             (not ac-candidates))
        (setq count 1))
    (loop for char in ac-dabbrev-reset-char
          do (if (char-equal last-command-char (string-to-char char))
                 (setq count 1)))
    count))

(defun ac-dabbrev-get-candidates (abbrev)
  (if (>= (ac-dabbrev-set-count) ac-dabbrev-all-min-count)
      (ac-dabbrev-get-limit-candidates abbrev t)
    (ac-dabbrev-get-limit-candidates abbrev nil)))

(defvar ac-source-dabbrev
 '((candidates
     . (lambda () (all-completions ac-target (ac-dabbrev-get-candidates ac-target))))
    (candidate-face . ac-dabbrev-menu-face)
    (selection-face . ac-dabbrev-selection-face))
  "Source for dabbrev")

(defface ac-dabbrev-menu-face
  '((t (:background "lightgray" :foreground "blue")))
  "Face for dabbrev candidate menu."
  :group 'auto-complete)

(defface ac-dabbrev-selection-face
  '((t (:background "blue" :foreground "white")))
  "Face for the dabbrev selected candidate."
  :group 'auto-complete)

;; mode provide
(provide 'ac-dabbrev)

;;; end
;;; ac-dabbrev.el ends here

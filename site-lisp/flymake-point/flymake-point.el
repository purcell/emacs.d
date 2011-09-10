;;; flymake-point.el --- show flymake errors under the point in the minibuffer

;; Copyright (C) 2008 Travis B. Hartwell
;; Copyright (C) 2011 Brodie Rao

;; Author: Travis B. Hartwell
;; Maintainer: Brodie Rao <brodie@bitheap.org>
;; Created: Jun 2008
;; Version: 0.1
;; Keywords: conveniece languages tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Additional functionality that makes flymake error messages appear
;; in the minibuffer when point is on a line containing a flymake
;; error. This saves having to mouse over the error, which is a
;; keyboard user's annoyance.

;;; Code:

(defun flymake-error-under-point ()
  "Display the flymake error under the point in the minibuffer."
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
	  (let ((err (car (nth 1 elem))))
	    (message "%s" (flymake-ler-text err)))))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Show the next error in the ring in the minibuffer."
  (flymake-error-under-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Show the previous error in the ring in the minibuffer."
  (flymake-error-under-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "When there's an error under the point, display it in the
minibuffer."
  (set (make-local-variable 'post-command-hook)
       (cons 'flymake-error-under-point post-command-hook)))

(provide 'flymake-point)

;;; flymake-point.el ends here
- Peer has closed the GnuTLS connection

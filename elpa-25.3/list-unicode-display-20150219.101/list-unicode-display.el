;;; list-unicode-display.el --- Search for and list unicode characters by name

;; Copyright (C) 2015  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience
;; Package-Version: 20150219.101
;; Package-X-Original-Version: 0
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a packaged version of code by @jpkotta, taken from a
;; comment on http://tromey.com/blog/?p=831.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defun list-unicode-display (&optional regexp)
  "Display a list of unicode characters with names matching REGEXP.
If no regexp is supplied, all characters are shown.  This takes
some time."
  (interactive "sRegexp (default \".*\"): ")
  (let* ((regexp (or regexp ".*"))
         (case-fold-search t)
         (cmp (lambda (x y) (< (cdr x) (cdr y))))
         ;; alist like ("name" . code-point)
         (char-alist (sort (cl-remove-if-not (lambda (x) (string-match regexp (car x)))
                                             (ucs-names))
                           cmp)))
    (with-help-window "*Unicode characters*"
      (with-current-buffer standard-output
        (dolist (c char-alist)
          (insert (format "0x%06X\t" (cdr c)))
          (insert (cdr c))
          (insert (format "\t%s\n" (car c))))))))

(provide 'list-unicode-display)
;;; list-unicode-display.el ends here

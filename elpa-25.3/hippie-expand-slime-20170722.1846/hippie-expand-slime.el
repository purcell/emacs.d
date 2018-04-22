;;; hippie-expand-slime.el --- Hook slime's completion into hippie-expand

;; Copyright (c) 2015-2017 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/hippie-expand-slime
;; Package-Version: 20170722.1846
;; Package-X-Original-Version: 0

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

;; Usage:

;; (require 'hippie-expand-slime)
;; (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand))

;;; Code:

(require 'slime)
(require 'hippie-exp)
(autoload 'slime-fuzzy-completions "slime-fuzzy")

;; Return list of completions for PREFIX using `slime-fuzzy-completions'
;; returns a flat, sorted list of completion candidates
(defun hippie-expand-slime-fuzzy-completions (prefix)
  (let ((completions (slime-fuzzy-completions prefix)))
    (and completions
         (mapcar 'car (car completions)))))

(defun try-expand-slime-with-fn (old complete-fn)
  "Completion function for `hippie-expand' which uses one of
slime's completion functions."
  (if (not old)
      (progn
        (he-init-string (slime-symbol-start-pos) (slime-symbol-end-pos))
        (if (not (equal he-search-string ""))
            (setq he-expand-list
                  (funcall complete-fn he-search-string))
          (setq he-expand-list ()))))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list (cdr he-expand-list))
      t)))


;;;###autoload
(defun try-expand-slime (old)
  "Simple slime completion function for `hippie-expand'."
  (try-expand-slime-with-fn old 'slime-simple-completions))

;;;###autoload
(defun try-expand-slime-fuzzy (old)
  "Fuzzy slime completion function for `hippie-expand'."
  (try-expand-slime-with-fn old 'hippie-expand-slime-fuzzy-completions))


;;;###autoload
(defun set-up-slime-hippie-expand (&optional fuzzy)
  "Add an optionally-fuzzy slime completion function to the front of
`hippie-expand-try-functions-list' for the current buffer."
  (interactive)
  (set (make-local-variable 'hippie-expand-try-functions-list) hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               (if fuzzy
                   'try-expand-slime-fuzzy
                 'try-expand-slime)))


(provide 'hippie-expand-slime)

;;; hippie-expand-slime.el ends here

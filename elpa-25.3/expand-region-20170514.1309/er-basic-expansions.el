;;; er-basic-expansions.el --- Words, symbols, strings, et al

;; Copyright (C) 2011-2013 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region

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

;; Expansions that are useful in any major mode.

;;; Code:

(require 'expand-region-core)

(defun er/mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (er/looking-back-on-line word-regexp))
      (skip-syntax-forward "w")
      (set-mark (point))
      (skip-syntax-backward "w"))))

(defun er/mark-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw"))
    (when (or (looking-at symbol-regexp)
              (er/looking-back-on-line symbol-regexp))
      (skip-syntax-forward "_w")
      (set-mark (point))
      (skip-syntax-backward "_w"))))

(defun er/mark-symbol-with-prefix ()
  "Mark the entire symbol around or in front of point, including prefix."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw")
        (prefix-regexp "\\s'"))
    (when (or (looking-at prefix-regexp)
              (looking-at symbol-regexp)
              (er/looking-back-on-line symbol-regexp))
      (skip-syntax-forward "'")
      (skip-syntax-forward "_w")
      (set-mark (point))
      (skip-syntax-backward "_w")
      (skip-syntax-backward "'"))))

;; Mark method call

(defun er/mark-next-accessor ()
  "Presumes that current symbol is already marked, skips over one
period and marks next symbol."
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (let ((symbol-regexp "\\s_\\|\\sw"))
      (when (looking-at "\\.")
        (forward-char 1)
        (skip-syntax-forward "_w")
        (exchange-point-and-mark)))))

(defun er/mark-method-call ()
  "Mark the current symbol (including dots) and then paren to closing paren."
  (interactive)
  (let ((symbol-regexp "\\(\\s_\\|\\sw\\|\\.\\)+"))
    (when (or (looking-at symbol-regexp)
              (er/looking-back-on-line symbol-regexp))
      (skip-syntax-backward "_w.")
      (set-mark (point))
      (when (looking-at symbol-regexp)
        (goto-char (match-end 0)))
      (if (looking-at "(")
          (forward-list))
      (exchange-point-and-mark))))

;; Comments

(defun er--point-is-in-comment-p ()
  "t if point is in comment, otherwise nil"
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun er/mark-comment ()
  "Mark the entire comment around point."
  (interactive)
  (when (er--point-is-in-comment-p)
    (let ((p (point)))
      (while (er--point-is-in-comment-p)
        (forward-char 1))
      (skip-chars-backward " \n\t\r")
      (set-mark (point))
      (goto-char p)
      (while (er--point-is-in-comment-p)
        (forward-char -1))
      (skip-chars-forward " \n\t\r"))))

;; Quotes

(defun er--current-quotes-char ()
  "The char that is the current quote delimiter"
  (nth 3 (syntax-ppss)))

(defalias 'er--point-inside-string-p 'er--current-quotes-char)

(defun er--move-point-forward-out-of-string ()
  "Move point forward until it exits the current quoted string."
  (er--move-point-backward-out-of-string)
  (forward-sexp))

(defun er--move-point-backward-out-of-string ()
  "Move point backward until it exits the current quoted string."
  (goto-char (nth 8 (syntax-ppss))))

(defun er/mark-inside-quotes ()
  "Mark the inside of the current string, not including the quotation marks."
  (interactive)
  (when (er--point-inside-string-p)
    (er--move-point-backward-out-of-string)
    (forward-char)
    (set-mark (point))
    (er--move-point-forward-out-of-string)
    (backward-char)
    (exchange-point-and-mark)))

(defun er/mark-outside-quotes ()
  "Mark the current string, including the quotation marks."
  (interactive)
  (if (er--point-inside-string-p)
      (er--move-point-backward-out-of-string)
    (when (and (not (use-region-p))
               (er/looking-back-on-line "\\s\""))
      (backward-char)
      (er--move-point-backward-out-of-string)))
  (when (looking-at "\\s\"")
    (set-mark (point))
    (forward-char)
    (er--move-point-forward-out-of-string)
    (exchange-point-and-mark)))

;; Pairs - ie [] () {} etc

(defun er--point-inside-pairs-p ()
  "Is point inside any pairs?"
  (> (car (syntax-ppss)) 0))

(defun er/mark-inside-pairs ()
  "Mark inside pairs (as defined by the mode), not including the pairs."
  (interactive)
  (when (er--point-inside-pairs-p)
    (goto-char (nth 1 (syntax-ppss)))
    (set-mark (save-excursion
                (forward-char 1)
                (skip-chars-forward er--space-str)
                (point)))
    (forward-list)
    (backward-char)
    (skip-chars-backward er--space-str)
    (exchange-point-and-mark)))

(defun er--looking-at-pair ()
  "Is point looking at an opening pair char?"
  (looking-at "\\s("))

(defun er--looking-at-marked-pair ()
  "Is point looking at a pair that is entirely marked?"
  (and (er--looking-at-pair)
       (use-region-p)
       (>= (mark)
           (save-excursion
             (forward-list)
             (point)))))

(defun er/mark-outside-pairs ()
  "Mark pairs (as defined by the mode), including the pair chars."
  (interactive)
  (if (er/looking-back-on-line "\\s)+\\=")
      (ignore-errors (backward-list 1))
    (skip-chars-forward er--space-str))
  (when (and (er--point-inside-pairs-p)
             (or (not (er--looking-at-pair))
                 (er--looking-at-marked-pair)))
    (goto-char (nth 1 (syntax-ppss))))
  (when (er--looking-at-pair)
    (set-mark (point))
    (forward-list)
    (exchange-point-and-mark)))

(require 'thingatpt)

(defun er/mark-url ()
  (interactive)
  (end-of-thing 'url)
  (set-mark (point))
  (beginning-of-thing 'url))

(defun er/mark-email ()
  (interactive)
  (end-of-thing 'email)
  (set-mark (point))
  (beginning-of-thing 'email))

(defun er/mark-defun ()
  "Mark defun around or in front of point."
  (interactive)
  (end-of-defun)
  (skip-chars-backward er--space-str)
  (set-mark (point))
  (beginning-of-defun)
  (skip-chars-forward er--space-str))

;; Methods to try expanding to
(setq er/try-expand-list
      (append '(er/mark-word
                er/mark-symbol
                er/mark-symbol-with-prefix
                er/mark-next-accessor
                er/mark-method-call
                er/mark-inside-quotes
                er/mark-outside-quotes
                er/mark-inside-pairs
                er/mark-outside-pairs
                er/mark-comment
                er/mark-url
                er/mark-email
                er/mark-defun)
              er/try-expand-list))

(provide 'er-basic-expansions)
;;; er-basic-expansions.el ends here

;;; ruby-mode-expansions.el --- ruby-specific expansions for expand-region

;; Copyright (C) 2011 Magnar Sveen

;; Author: Matt Briggs
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
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


;; LeWang:
;;
;;      I think `er/ruby-backward-up' and `er/ruby-forward-up' are nifty
;;      functions in their own right.
;;
;;      I would bind them to C-M-u and C-M-d respectively.

;; Expansions:
;;
;;
;;  er/mark-ruby-block-up
;;

;;; Code:
(require 'expand-region-core)
(require 'ruby-mode)

(defvar er/ruby-block-end-re
  (concat ruby-block-end-re "\\|}")
  "like ruby-mode's but also for '}'")

(defun er/ruby-skip-past-block-end ()
  "If line is blockend, move point to next line."
  (when (looking-at er/ruby-block-end-re)
    (forward-line 1)))

(defun er/ruby-end-of-block (&optional arg)
  "By default `ruby-end-of-block' goes to BOL of line containing end-re.

This moves point to the next line to include the end of the block"
  (interactive "p")
  ;; Workaround for `ruby-end-of-block' in Emacs 23.
  (when (re-search-forward (concat "\\<\\(" ruby-block-beg-re "\\)\\>")
                           (point-at-eol) t)
    (goto-char (match-beginning 0)))
  (ruby-end-of-block (or arg 1))
  (er/ruby-skip-past-block-end))

(defun er/point-at-indentation ()
  "Return the point where current line's indentation ends."
  (save-excursion
    (back-to-indentation)
    (point)))

(defun er/ruby-backward-up ()
  "a la `paredit-backward-up'"
  (interactive)
  ;; if our current line ends a block, we back a line, otherwise we
  (when (save-excursion
          (back-to-indentation)
          (looking-at-p ruby-block-end-re))
    (forward-line -1))
  (let ((orig-point (point))
        progress-beg
        progress-end)

    ;; cover the case when point is in the line of beginning of block
    (unless (progn (ruby-end-of-block)
                   (ruby-beginning-of-block)
                   ;; "Block beginning" is often not at indentation in Emacs 24.
                   (< (er/point-at-indentation) orig-point))
      (loop do
            (ruby-beginning-of-block)
            (setq progress-beg (point))
            (when (= (point) (point-min))
              (return))
            (ruby-end-of-block)
            (setq progress-end (if (looking-at-p er/ruby-block-end-re)
                                   (point-at-bol 0)
                                 (point-at-bol 1)))
            (goto-char progress-beg)
            (when (> progress-end orig-point)
              (return))))))

;;; This command isn't used here explicitly, but it's symmetrical with
;;; `er/ruby-backward-up', and nifty for interactive use.
(defun er/ruby-forward-up ()
  "a la `paredit-forward-up'"
  (interactive)
  (er/ruby-backward-up)
  (er/ruby-end-of-block))

(defun er/get-ruby-block (&optional pos)
  "return (beg . end) of current block"
  (setq pos (or pos (point)))
  (save-excursion
    (goto-char pos)
    (let (beg end)
      (cons (progn
              (er/ruby-backward-up)
              (er/point-at-indentation))
            (progn
              (er/ruby-end-of-block)
              (point))))))

(defun er/mark-ruby-block-up-1 ()
  (er/ruby-backward-up)
  (set-mark (er/point-at-indentation))
  (er/ruby-end-of-block)
  (exchange-point-and-mark))

(defun er/mark-ruby-block-up (&optional no-recurse)
  "mark the next level up."
  (interactive)
  (if (use-region-p)
      (let* ((orig-end (region-end))
             (orig-beg (region-beginning))
             (orig-len (- orig-end orig-beg))
             (prev-block-point
              (or (save-excursion
                    (goto-char orig-end)
                    (forward-line 0)
                    (back-to-indentation)
                    (cond ((looking-at-p er/ruby-block-end-re)
                           (point-at-bol 0))
                          ((re-search-forward
                            (concat "\\<\\(" ruby-block-beg-re "\\)\\>")
                            (point-at-eol)
                            t)
                           (point-at-bol 2))) )
                  (point)))
             (prev-block-info (er/get-ruby-block prev-block-point))
             (prev-block-beg (car prev-block-info))
             (prev-block-end (cdr prev-block-info))
             (prev-block-len (- prev-block-end prev-block-beg)))
        (if (and (>= orig-beg prev-block-beg)
                 (<= orig-end prev-block-end)
                 (< orig-len prev-block-len))
            ;; expand to previous block if it contains and grows current
            ;; region
            (progn
              (deactivate-mark)
              (goto-char prev-block-point)
              (or no-recurse
                  (er/mark-ruby-block-up 'no-recurse)))
          (er/mark-ruby-block-up-1)))
    (er/mark-ruby-block-up-1)))

(defun er/mark-ruby-instance-variable ()
  "Marks instance variables in ruby.
Assumes that point is at the @ - if it is inside the word, that will
be marked first anyway."
  (when (looking-at "@")
    (forward-char 1))
  (when (er/looking-back-exact "@")
    (er/mark-symbol)
    (forward-char -1)))

(defun er/mark-ruby-heredoc ()
  "Marks a heredoc, since `er/mark-inside-quotes' assumes single quote chars."
  (let ((ppss (syntax-ppss)))
    (when (elt ppss 3)
      (let ((s-start (elt ppss 8)))
        (goto-char s-start)
        (when (save-excursion
                (beginning-of-line)
                (re-search-forward "<<\\(-?\\)['\"]?\\([a-zA-Z0-9_]+\\)" s-start nil))
          (let ((allow-indent (string= "-" (match-string 1)))
                (terminator (match-string 2))
                (heredoc-start (save-excursion
                                 (forward-line)
                                 (point))))
            (forward-sexp 1)
            (forward-line -1)
            (when (looking-at (concat "^" (if allow-indent "[ \t]*" "") terminator "$"))
              (set-mark heredoc-start)
              (exchange-point-and-mark))))))))

(defun er/add-ruby-mode-expansions ()
  "Adds Ruby-specific expansions for buffers in ruby-mode"
  (set (make-local-variable 'er/try-expand-list)
       (remove 'er/mark-defun 
               (append
                (default-value 'er/try-expand-list)
                '(er/mark-ruby-instance-variable
                  er/mark-ruby-block-up
                  er/mark-ruby-heredoc)))))

(er/enable-mode-expansions 'ruby-mode 'er/add-ruby-mode-expansions)
(provide 'ruby-mode-expansions)

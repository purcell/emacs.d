;;; rails-compat.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails.el $
;; $Id: rails.el 149 2007-03-29 15:07:49Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(eval-when-compile
  (require 'snippet nil t)
  (require 'completion-ui nil t))

(when (fboundp 'indent-and-complete)
  (message "WARNNING: the `indent-and-complete' already defined."))

(defun indent-and-complete ()
  "Indent line and Complete if point is at end of left a leave word."
  (interactive)

  (cond
   ;; snippet
   ((and (boundp 'snippet)
         snippet)
    (snippet-next-field))

   ;; completion-ui
   ((and (fboundp 'completion-overlay-at-point)
         (completion-overlay-at-point))
    (let* ((ov (completion-overlay-at-point))
           (end (overlay-end ov))
           ;; setup <SPACE> as last command
           (last-input-event 32)
           (last-command-event 32))
      ;; skip message output
      (flet ((message (format-string &rest args) nil))
        (completion-self-insert))))

   ;; hippie-expand
   ((looking-at "\\_>")
    ;; skip message output
    (flet ((message (format-string &rest args) nil))
      (hippie-expand nil))))

  ;; always indent line
  (indent-for-tab-command))


(when (fboundp 'try-complete-abbrev)
  (message "WARRNING: the function `try-complete-abbrev' already defined"))

(defun try-complete-abbrev (old)
  (if (abbrev-expansion-point-p)
      (if (expand-abbrev)
          t nil)
    nil))

(defun abbrev-expansion-point-p ()
  "returns true if point is a place that might be expanded"
  (if (memq (get-text-property (- (point) 1) 'face)
            '(font-lock-string-face font-lock-comment-face font-lock-doc-face))
      (return nil) ;; we never expand inside of string literals or comments
    (string-not-empty (syntax-word-before-point))))

(defun syntax-word-before-point ()
  "Yields the word immediately preceding point"
  (buffer-substring-no-properties
   (+ (point) (save-excursion (skip-syntax-backward "w")))
   (point)))


(unless (find 'try-complete-abbrev hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-abbrev))
(provide 'rails-compat)

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
  (require 'flyspell)
  (require 'ruby-mode)
  (require 'snippet nil t)
  (require 'predictive nil t)
  (require 'completion-ui nil t))

(when (fboundp 'indent-or-complete)
  (message "WARNNING: the `indent-or-complete' already defined."))

(defun indent-or-complete ()
  "Complete if point is at end of left a leave word, otherwise indent line."
  (interactive)
  (cond
   ;; snippet
   ((and (boundp 'snippet) snippet)
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
      (hippie-expand nil)))

   ;; run default indent command
   (t (indent-for-tab-command))))

(when (fboundp 'try-complete-abbrev)
  (message "WARRNING: the function `try-complete-abbrev' already defined"))

(defun try-complete-abbrev (old)
  (let ((point-end (point))
        (point-start (point))
        distance)
    (save-excursion
      (while (not (zerop (setq distance (skip-syntax-backward "w"))))
        (setq point-start (+ point-start distance))))
    (when (not (= point-start point-end))
      (let ((abbr (buffer-substring-no-properties point-start point-end)))
        (when (and (abbrev-symbol abbr)
                   (expand-abbrev))
          t)))))

(unless (find 'try-complete-abbrev hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-abbrev))

(defun activate-predictive-inside-strings (start end len)
  "Looking at symbol at point and activate the `predictive-mode'
if there a string or a comment."
  (save-excursion
    (let ((f (get-text-property (point) 'face)))
      (if (memq f flyspell-prog-text-faces)
          (predictive-mode 1)
        (predictive-mode -1)))))

(defun predictive-prog-mode ()
  "Enable the `predictive-mode' inside strings and comments
only, like `flyspell-prog-mode'."
  (interactive)
  (when (fboundp 'predictive-mode)
    ;; load flyspell before
    (unless (featurep 'flyspell)
      (require 'flyspell))
    (if (find 'activate-predictive-inside-strings after-change-functions)
        (progn
          (remove-hook 'after-change-functions 'activate-predictive-inside-strings t)
          (predictive-mode -1))
      (progn
        (set (make-local-variable 'predictive-use-auto-learn-cache) nil)
        (add-hook 'after-change-functions 'activate-predictive-inside-strings nil t)))))

(when (fboundp 'predictive-mode)
  (defadvice ruby-electric-space (around ruby-electric-space-with-completion-ui first (arg) activate)
    "Override the ruby <space> command if predictive-mode enabled."
    (if (find 'predictive-mode
              minor-mode-alist
              :key (lambda(i) (car i)))
        (completion-self-insert)
      ad-do-it)))

(defvar untabify-before-save-enabled nil)

(defun untabify-before-save ()
  "Strip all trailing whitespaces and untabify buffer before
save."
  ;; guard
  (unless untabify-before-save-enabled
    (set (make-local-variable 'untabify-before-save-enabled) t)
    (add-hook 'local-write-file-hooks
              '(lambda ()
                 (when (eq this-command 'save-buffer)
                   (save-excursion
                     (untabify (point-min) (point-max))
                     (delete-trailing-whitespace)))) nil t)))

(provide 'rails-compat)
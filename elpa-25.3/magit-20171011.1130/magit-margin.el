;;; magit-margin.el --- margins in Magit buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library implements support for showing additional information
;; in the margins of Magit buffers.  Currently this is only used for
;; commits, for which the committer date or age, and optionally the
;; author name are shown.

;;; Code:

(require 'dash)

(require 'magit-section)
(require 'magit-mode)

(defgroup magit-margin nil
  "Information Magit displays in the margin.

You can change the STYLE and AUTHOR-WIDTH of all `magit-*-margin'
options to the same values by customizing `magit-log-margin'
*before* `magit' is loaded.  If you do that, then the respective
values for the other options will default to what you have set
for that variable.  Likewise if you set `magit-log-margin's INIT
to nil, then that is used in the default of all other options.  But
setting it to t, i.e. re-enforcing the default for that option,
does not carry to other options."
  :link '(info-link "(magit)Log Margin")
  :group 'magit-log)

(defvar-local magit-buffer-margin nil)
(put 'magit-buffer-margin 'permanent-local t)

(defvar-local magit-set-buffer-margin-refresh nil)

(defvar magit--age-spec)

;;; Commands

(magit-define-popup magit-margin-popup
  "Popup console for changing appearance of the margin."
  :actions '("Margin"
             (?L "toggle visibility" magit-toggle-margin)
             (?l "cycle style"       magit-cycle-margin-style)
             (?d "toggle details"    magit-toggle-margin-details))
  :max-action-columns 1)

(defun magit-toggle-margin ()
  "Show or hide the Magit margin."
  (interactive)
  (unless (magit-margin-option)
    (user-error "Magit margin isn't supported in this buffer"))
  (setcar magit-buffer-margin (not (magit-buffer-margin-p)))
  (magit-set-buffer-margin))

(defun magit-cycle-margin-style ()
  "Cycle style used for the Magit margin."
  (interactive)
  (unless (magit-margin-option)
    (user-error "Magit margin isn't supported in this buffer"))
  ;; This is only suitable for commit margins (there are not others).
  (setf (cadr magit-buffer-margin)
        (pcase (cadr magit-buffer-margin)
          (`age 'age-abbreviated)
          (`age-abbreviated
           (let ((default (cadr (symbol-value (magit-margin-option)))))
             (if (stringp default) default "%Y-%m-%d %H:%M ")))
          (_ 'age)))
  (magit-set-buffer-margin nil t))

(defun magit-toggle-margin-details ()
  "Show or hide details in the Magit margin."
  (interactive)
  (unless (magit-margin-option)
    (user-error "Magit margin isn't supported in this buffer"))
  (setf (nth 3 magit-buffer-margin)
        (not (nth 3 magit-buffer-margin)))
  (magit-set-buffer-margin nil t))

;;; Core

(defun magit-buffer-margin-p ()
  (car magit-buffer-margin))

(defun magit-margin-option ()
  (pcase major-mode
    (`magit-cherry-mode     'magit-cherry-margin)
    (`magit-log-mode        'magit-log-margin)
    (`magit-log-select-mode 'magit-log-select-margin)
    (`magit-reflog-mode     'magit-reflog-margin)
    (`magit-refs-mode       'magit-refs-margin)
    (`magit-stashes-mode    'magit-stashes-margin)
    (`magit-status-mode     'magit-status-margin)))

(defun magit-set-buffer-margin (&optional reset refresh)
  (-when-let (option (magit-margin-option))
    (let* ((default (symbol-value option))
           (default-width (nth 2 default)))
      (when (or reset (not magit-buffer-margin))
        (setq magit-buffer-margin (copy-sequence default)))
      (-let [(enable style _width details details-width)
             magit-buffer-margin]
        (when (functionp default-width)
          (setf (nth 2 magit-buffer-margin)
                (funcall default-width style details details-width)))
        (dolist (window (get-buffer-window-list nil nil 0))
          (with-selected-window window
            (magit-set-window-margin window)
            (if enable
                (add-hook  'window-configuration-change-hook
                           'magit-set-window-margin nil t)
              (remove-hook 'window-configuration-change-hook
                           'magit-set-window-margin t))))
        (when (and enable (or refresh magit-set-buffer-margin-refresh))
          (magit-refresh-buffer))))))

(defun magit-set-window-margin (&optional window)
  (when (or window (setq window (get-buffer-window)))
    (with-selected-window window
      (set-window-margins nil (car (window-margins))
                          (and (magit-buffer-margin-p)
                               (nth 2 magit-buffer-margin))))))

(defun magit-make-margin-overlay (&optional string previous-line)
  (if previous-line
      (save-excursion
        (forward-line -1)
        (magit-make-margin-overlay string))
    ;; Don't put the overlay on the complete line to work around #1880.
    (let ((o (make-overlay (1+ (line-beginning-position))
                           (line-end-position)
                           nil t)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'before-string
                   (propertize "o" 'display
                               (list (list 'margin 'right-margin)
                                     (or string " ")))))))

(defun magit-maybe-make-margin-overlay ()
  (when (or (magit-section-match
             '(unpulled unpushed recent stashes local cherries)
             magit-insert-section--current)
            (and (eq major-mode 'magit-refs-mode)
                 (magit-section-match
                  '(remote commit tags)
                  magit-insert-section--current)))
    (magit-make-margin-overlay nil t)))

;;; Custom Support

(defun magit-margin-set-variable (mode symbol value)
  (set-default symbol value)
  (message "Updating margins in %s buffers..." mode)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode mode)
        (magit-set-buffer-margin t)
        (magit-refresh))))
  (message "Updating margins in %s buffers...done" mode))

(defconst magit-log-margin--custom-type
  '(list (boolean :tag "Show margin initially")
         (choice  :tag "Show committer"
                  (string :tag "date using time-format" "%Y-%m-%d %H:%M ")
                  (const  :tag "date's age" age)
                  (const  :tag "date's age (abbreviated)" age-abbreviated))
         (const   :tag "Calculate width using magit-log-margin-width"
                  magit-log-margin-width)
         (boolean :tag "Show author name by default")
         (integer :tag "Show author name using width")))

;;; Time Utilities

(defvar magit--age-spec
  `((?Y "year"   "years"   ,(round (* 60 60 24 365.2425)))
    (?M "month"  "months"  ,(round (* 60 60 24 30.436875)))
    (?w "week"   "weeks"   ,(* 60 60 24 7))
    (?d "day"    "days"    ,(* 60 60 24))
    (?h "hour"   "hours"   ,(* 60 60))
    (?m "minute" "minutes" 60)
    (?s "second" "seconds" 1))
  "Time units used when formatting relative commit ages.

The value is a list of time units, beginning with the longest.
Each element has the form (CHAR UNIT UNITS SECONDS).  UNIT is the
time unit, UNITS is the plural of that unit.  CHAR is a character
abbreviation.  And SECONDS is the number of seconds in one UNIT.

This is defined as a variable to make it possible to use time
units for a language other than English.  It is not defined
as an option, because most other parts of Magit are always in
English.")

(defun magit--age (date &optional abbreviate)
  (cl-labels ((fn (age spec)
                  (-let [(char unit units weight) (car spec)]
                    (let ((cnt (round (/ age weight 1.0))))
                      (if (or (not (cdr spec))
                              (>= (/ age weight) 1))
                          (list cnt (cond (abbreviate char)
                                          ((= cnt 1) unit)
                                          (t units)))
                        (fn age (cdr spec)))))))
    (fn (abs (- (float-time) (string-to-number date)))
        magit--age-spec)))

(provide 'magit-margin)
;;; magit-margin.el ends here

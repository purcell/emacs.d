;;; ledger-fontify.el --- Provide custom fontification for ledger-mode


;; Copyright (C) 2014 Craig P. Earls (enderw88 at gmail dot com)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:
;;  Font-lock-mode doesn't handle multiline syntax very well.  This
;;  code provides font lock that is sensitive to overall transaction
;;  states


;;; Code:

(require 'ledger-navigate)
(require 'ledger-regex)
(require 'ledger-state)

(defcustom ledger-fontify-xact-state-overrides nil
  "If t the highlight entire xact with state."
  :type 'boolean
  :group 'ledger)

(defun ledger-fontify-buffer-part (&optional beg end len)
  "Fontify buffer from BEG to END, length LEN."
  (save-excursion
    (unless beg (setq beg (point-min)))
    (unless end (setq end (point-max)))
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (cond ((or (looking-at ledger-xact-start-regex)
                 (looking-at ledger-posting-regex)
                 (looking-at ledger-recurring-line-regexp))
             (ledger-fontify-xact-at (point)))
            ((looking-at ledger-directive-start-regex)
             (ledger-fontify-directive-at (point))))
      (ledger-navigate-next-xact-or-directive))))

(defun ledger-fontify-xact-at (position)
  "Fontify the xact at POSITION."
  (interactive "d")
  (save-excursion
    (goto-char position)
    (let ((extents (ledger-navigate-find-element-extents position))
          (state (ledger-transaction-state)))
      (if (and ledger-fontify-xact-state-overrides state)
          (cond ((eq state 'cleared)
                 (ledger-fontify-set-face extents 'ledger-font-xact-cleared-face))
                ((eq state 'pending)
                 (ledger-fontify-set-face extents 'ledger-font-xact-pending-face)))
        (ledger-fontify-xact-by-line extents)))))

(defun ledger-fontify-xact-by-line (extents)
  "Do line-by-line detailed fontification of xact in EXTENTS."
  (save-excursion
    (ledger-fontify-xact-start (car extents))
    (while (< (point) (cadr extents))
      (if (looking-at "[ \t]+;")
          (ledger-fontify-set-face (list (point) (progn
                                                   (end-of-line)
                                                   (point))) 'ledger-font-comment-face)
        (ledger-fontify-posting (point)))
      (forward-line))))

(defun ledger-fontify-xact-start (pos)
  "POS should be at the beginning of a line starting an xact.
Fontify the first line of an xact"
  (goto-char pos)
  (let ((line-start (line-beginning-position)))
    (goto-char line-start)
    (re-search-forward "[ \t]")
    (ledger-fontify-set-face (list line-start (match-beginning 0)) 'ledger-font-posting-date-face)
    (goto-char line-start)
    (re-search-forward ledger-xact-after-date-regex)
    (let ((state (save-match-data (ledger-state-from-string (match-string 1)))))
      (ledger-fontify-set-face (list (match-beginning 3) (match-end 3))
                               (cond ((eq state 'pending)
                                      'ledger-font-payee-pending-face)
                                     ((eq state 'cleared)
                                      'ledger-font-payee-cleared-face)
                                     (t
                                      'ledger-font-payee-uncleared-face))))
    (when (match-beginning 2)
      (ledger-fontify-set-face (list (match-beginning 2)
                                     (match-end 2)) 'ledger-font-code-face))
    (when (match-beginning 4)
      (ledger-fontify-set-face (list (match-beginning 4)
                                     (match-end 4)) 'ledger-font-comment-face))
    (forward-line)))

(defun ledger-fontify-posting (pos)
  "Fontify the posting at POS."
  (let* ((state nil)
         (end-of-line-comment nil)
         (end (progn (end-of-line)
                     (point)))
         (start (progn (beginning-of-line)
                       (point))))

    ;; Look for a posting status flag
    (set-match-data nil 'reseat)
    (re-search-forward "^[[:blank:]]+\\([*!]\\)[[:blank:]]" end t)
    (if (match-string 1)
        (setq state (ledger-state-from-string  (match-string 1))))
    (beginning-of-line)
    (re-search-forward "[[:graph:]]\\([ \t][ \t]\\)" end 'end)  ;; find the end of the account, or end of line

    (when (<= (point) end)  ;; we are still on the line
      (ledger-fontify-set-face (list start (point))
                               (cond ((eq state 'cleared)
                                      'ledger-font-posting-account-cleared-face)
                                     ((eq state 'pending)
                                      'ledger-font-posting-account-pending-face)
                                     (t
                                      'ledger-font-posting-account-face)))


      (when (< (point) end)  ;; there is still more to fontify
        (setq start (point))  ;; update start of next font region
        (setq end-of-line-comment (re-search-forward ";" end 'end))  ;; find the end of the line, or start of a comment
        (ledger-fontify-set-face (list start (point) )
                                 (cond ((eq state 'cleared)
                                        'ledger-font-posting-amount-cleared-face)
                                       ((eq state 'pending)
                                        'ledger-font-posting-amount-pending-face)
                                       (t
                                        'ledger-font-posting-amount-face)))
        (when end-of-line-comment
          (setq start (point))
          (end-of-line)
          (ledger-fontify-set-face (list (- start 1) (point)) ;; subtract 1 from start because we passed the semi-colon
                                   'ledger-font-comment-face))))))

(defun ledger-fontify-directive-at (pos)
  "Fontify the directive at POS."
  (let ((extents (ledger-navigate-find-element-extents pos))
        (face 'ledger-font-default-face)
        (case-fold-search nil))
    (cond ((looking-at "^=[[:blank:]]")
           (setq face 'ledger-font-auto-xact-face))
          ((looking-at "^~[[:blank:]]")
           (setq face 'ledger-font-periodic-xact-face))
          ((looking-at "^[;#%|\\*]")
           (setq face 'ledger-font-comment-face))
          ((looking-at "^account[[:blank:]]")
           (setq face 'ledger-font-account-directive-face))
          ((looking-at "^alias[[:blank:]]")
           (setq face 'ledger-font-alias-directive-face))
          ((looking-at "^apply[[:blank:]]")
           (setq face 'ledger-font-apply-directive-face))
          ((looking-at "^assert[[:blank:]]")
           (setq face 'ledger-font-assert-directive-face))
          ((looking-at "^\\(bucket\\|A\\)[[:blank:]]")
           (setq face 'ledger-font-bucket-directive-face))
          ((looking-at "^C[[:blank:]]")
           (setq face 'ledger-font-C-directive-face))
          ((looking-at "^capture[[:blank:]]")
           (setq face 'ledger-font-capture-directive-face))
          ((looking-at "^check[[:blank:]]")
           (setq face 'ledger-font-check-directive-face))
          ((looking-at "^\\(comment\\|test[[:blank:]]\\)")
           (setq face 'ledger-font-comment-face))
          ((looking-at "^commodity[[:blank:]]")
           (setq face 'ledger-font-commodity-directive-face))
          ((looking-at "^D[[:blank:]]")
           (setq face 'ledger-font-D-directive-face))
          ((looking-at "^\\(define\\|def\\)[[:blank:]]")
           (setq face 'ledger-font-define-directive-face))
          ((looking-at "^end")
           (setq face 'ledger-font-end-directive-face))
          ((looking-at "^expr[[:blank:]]")
           (setq face 'ledger-font-expr-directive-face))
          ((looking-at "^fixed[[:blank:]]")
           (setq face 'ledger-font-fixed-directive-face))
          ((looking-at "^include[[:blank:]]")
           (setq face 'ledger-font-include-directive-face))
          ((looking-at "^N[[:blank:]]")
           (setq face 'ledger-font-N-directive-face))
          ((looking-at "^payee[[:blank:]]")
           (setq face 'ledger-font-payee-directive-face))
          ((looking-at "^P[[:blank:]]")
           (setq face 'ledger-font-price-directive-face))
          ((looking-at "^tag[[:blank:]]")
           (setq face 'ledger-font-tag-directive-face))
          ((looking-at "^[IiOobh][[:blank:]]")
           (setq face 'ledger-font-timeclock-directive-face))
          ((looking-at "^\\(year\\|Y\\)[[:blank:]]")
           (setq face 'ledger-font-year-directive-face)))
    (ledger-fontify-set-face extents face)))

(defun ledger-fontify-set-face (extents face)
  "Set the text in EXTENTS to FACE."
  (put-text-property (car extents) (cadr extents) 'font-lock-face face))


(provide 'ledger-fontify)

;;; ledger-fontify.el ends here

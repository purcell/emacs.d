;;; ledger-post.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

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
;; Utility functions for dealing with postings.

(require 'ledger-regex)
(require 'ledger-navigate)

;;; Code:

(defgroup ledger-post nil
  "Options for controlling how Ledger-mode deals with postings and completion"
  :group 'ledger)

(defcustom ledger-post-account-alignment-column 4
  "The column Ledger-mode attempts to align accounts to."
  :type 'integer
  :group 'ledger-post)

(defcustom ledger-post-amount-alignment-column 52
  "The column Ledger-mode attempts to align amounts to."
  :type 'integer
  :group 'ledger-post)

(defcustom ledger-post-amount-alignment-at :end
  "Position at which the amount is ailgned.

Can be :end to align on the last number of the amount (can be
followed by unaligned commodity) or :decimal to align at the
decimal separator."
  :type '(radio (const :tag "align at the end of amount" :end)
                (const :tag "align at the decimal separator" :decimal))
  :group 'ledger-post)

(defun ledger-next-amount (&optional end)
  "Move point to the next amount, as long as it is not past END.
Return the width of the amount field as an integer and leave
point at beginning of the commodity."
  ;;(beginning-of-line)
  (let ((case-fold-search nil))
    (when (re-search-forward ledger-amount-regex end t)
      (goto-char (match-beginning 0))
      (skip-syntax-forward " ")
      (cond
       ((eq ledger-post-amount-alignment-at :end)
        (- (or (match-end 4) (match-end 3)) (point)))
       ((eq ledger-post-amount-alignment-at :decimal)
        (- (match-end 3) (point)))))))

(defun ledger-next-account (&optional end)
  "Move to the beginning of the posting, or status marker.
Return the column of the beginning of the account and leave point
at beginning of account.
Looks only as far as END, if supplied, otherwise `point-max'."
  (let ((end (or end (point-max))))
    (if (> end (point))
        (when (re-search-forward ledger-account-any-status-regex (1+ end) t)
          ;; the 1+ is to make sure we can catch the newline
          (if (match-beginning 1)
              (goto-char (match-beginning 1))
            (goto-char (match-beginning 2)))
          (current-column)))))

(defun ledger-post-align-xact (pos)
  "Align all the posting in the xact at POS."
  (interactive "d")
  (let ((bounds (ledger-navigate-find-xact-extents pos)))
    (ledger-post-align-postings (car bounds) (cadr bounds))))

(defun ledger-post-align-postings (beg end)
  "Align all accounts and amounts between BEG and END, or the current region, or, if no region, the current line."
  (interactive "r")

  (save-excursion
    (let ((inhibit-modification-hooks t)
          acct-start-column acct-end-column acct-adjust amt-width amt-adjust
          (lines-left 1))

      ;; Extend region to whole lines
      (let ((start-marker (set-marker (make-marker) (save-excursion
                                                      (goto-char beg)
                                                      (line-beginning-position))))
            (end-marker (set-marker (make-marker) (save-excursion
                                                    (goto-char end)
                                                    (line-end-position)))))
        (untabify start-marker end-marker)
        (goto-char start-marker)

        ;; This is the guts of the alignment loop
        (while (and (or (setq acct-start-column (ledger-next-account (line-end-position)))
                        lines-left)
                    (< (point) end-marker))
          (when acct-start-column
            (setq acct-end-column (save-excursion
                                    (goto-char (match-end 2))
                                    (current-column)))
            (when (/= (setq acct-adjust (- ledger-post-account-alignment-column acct-start-column)) 0)
              (setq acct-end-column (+ acct-end-column acct-adjust))  ;;adjust the account ending column
              (if (> acct-adjust 0)
                  (insert (make-string acct-adjust ? ))
                (delete-char acct-adjust)))
            (when (setq amt-width (ledger-next-amount (line-end-position)))
              (if (/= 0 (setq amt-adjust (- (if (> (- ledger-post-amount-alignment-column amt-width)
                                                   (+ 2 acct-end-column))
                                                ledger-post-amount-alignment-column ;;we have room
                                              (+ acct-end-column 2 amt-width))
                                            amt-width
                                            (current-column))))
                  (if (> amt-adjust 0)
                      (insert (make-string amt-adjust ? ))
                    (delete-char amt-adjust)))))
          (forward-line)
          (setq lines-left (not (eobp)))))
      (setq inhibit-modification-hooks nil))))

(defun ledger-post-align-dwim ()
  "Align all the posting of the current xact or the current region.

If the point is in a comment, fill the comment paragraph as
regular text."
  (interactive)
  (cond
   ((nth 4 (syntax-ppss))
    (call-interactively 'ledger-post-align-postings)
    (fill-paragraph))
   ((use-region-p) (call-interactively 'ledger-post-align-postings))
   (t (call-interactively 'ledger-post-align-xact))))

(defun ledger-post-edit-amount ()
  "Call 'calc-mode' and push the amount in the posting to the top of stack."
  (interactive)
  (goto-char (line-beginning-position))
  (when (re-search-forward ledger-post-line-regexp (line-end-position) t)
    (goto-char (match-end ledger-regex-post-line-group-account)) ;; go to the and of the account
    (let ((end-of-amount (re-search-forward "[-.,0-9]+" (line-end-position) t)))
      ;; determine if there is an amount to edit
      (if end-of-amount
          (let ((val-string (match-string 0)))
            (goto-char (match-beginning 0))
            (delete-region (match-beginning 0) (match-end 0))
            (push-mark)
            (calc)
            (calc-eval val-string 'push)) ;; edit the amount
        (progn ;;make sure there are two spaces after the account name and go to calc
          (if (search-backward "  " (- (point) 3) t)
              (goto-char (line-end-position))
            (insert "  "))
          (push-mark)
          (calc))))))

(provide 'ledger-post)



;;; ledger-post.el ends here

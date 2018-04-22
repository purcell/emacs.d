;;; ledger-mode.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; Package-Requires: ((emacs "24.3"))

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
;; Most of the general ledger-mode code is here.

;;; Code:

(require 'ledger-regex)
(require 'cus-edit)
(require 'esh-util)
(require 'esh-arg)
(require 'easymenu)
(require 'org)
(require 'ledger-commodities)
(require 'ledger-complete)
(require 'ledger-context)
(require 'ledger-exec)
(require 'ledger-fonts)
(require 'ledger-fontify)
(require 'ledger-init)
(require 'ledger-navigate)
(require 'ledger-occur)
(require 'ledger-post)
(require 'ledger-reconcile)
(require 'ledger-report)
(require 'ledger-sort)
(require 'ledger-state)
(require 'ledger-test)
(require 'ledger-texi)
(require 'ledger-xact)
(require 'ledger-schedule)
(require 'ledger-check)

;;; Code:

(defgroup ledger nil
  "Interface to the Ledger command-line accounting program."
  :group 'data)

(defconst ledger-version "3.0"
  "The version of ledger.el currently loaded.")

(defconst ledger-mode-version "3.0.0")

(defun ledger-mode-dump-variable (var)
  "Format VAR for dump to buffer."
  (if var
      (insert (format "         %s: %S\n" (symbol-name var) (eval var)))))

(defun ledger-mode-dump-group (group)
  "Dump GROUP customizations to current buffer."
  (let ((members (custom-group-members group nil)))
    (dolist (member members)
      (cond ((eq (cadr member) 'custom-group)
             (insert (format "Group %s:\n" (symbol-name (car member))))
             (ledger-mode-dump-group (car member)))
            ((eq (cadr member) 'custom-variable)
             (ledger-mode-dump-variable (car member)))))))

(defun ledger-mode-dump-configuration ()
  "Dump all customizations."
  (interactive)
  (find-file "ledger-mode-dump")
  (ledger-mode-dump-group 'ledger))

(defun ledger-read-account-with-prompt (prompt)
  "Read an account from the minibuffer with PROMPT."
  (let* ((context (ledger-context-at-point))
         (account (ledger-context-field-value context 'account)))
    (ledger-read-string-with-default prompt
                                     (when account
                                       (regexp-quote account)))))

(defun ledger-read-date (prompt)
  "Return user-supplied date after `PROMPT', defaults to today."
  (ledger-format-date (let ((org-read-date-prefer-future nil))
                        (org-read-date nil t nil prompt))))

(defun ledger-read-string-with-default (prompt default)
  "Return user supplied string after PROMPT, or DEFAULT."
  (read-string (concat prompt
                       (if default
                           (concat " (" default "): ")
                         ": "))
               nil 'ledger-minibuffer-history default))

(defun ledger-display-balance-at-point (&optional arg)
  "Display the cleared-or-pending balance.
And calculate the target-delta of the account being reconciled.

With prefix argument \\[universal-argument] ask for the target commodity and convert
the balance into that."
  (interactive "P")
  (let* ((account (ledger-read-account-with-prompt "Account balance to show"))
         (target-commodity (when arg (ledger-read-commodity-with-prompt "Target commodity: ")))
         (buffer (find-file-noselect (ledger-master-file)))
         (balance (with-temp-buffer
                    (apply 'ledger-exec-ledger buffer (current-buffer) "cleared" account
                           (when target-commodity (list "-X" target-commodity)))
                    (if (> (buffer-size) 0)
                        (buffer-substring-no-properties (point-min) (1- (point-max)))
                      (concat account " is empty.")))))
    (when balance
      (message balance))))

(defun ledger-display-ledger-stats ()
  "Display the cleared-or-pending balance.
And calculate the target-delta of the account being reconciled."
  (interactive)
  (let* ((buffer (find-file-noselect (ledger-master-file)))
         (balance (with-temp-buffer
                    (ledger-exec-ledger buffer (current-buffer) "stats")
                    (buffer-substring-no-properties (point-min) (1- (point-max))))))
    (when balance
      (message balance))))

(defun ledger-magic-tab (&optional interactively)
  "Decide what to with with <TAB>, INTERACTIVELY.
Can indent, complete or align depending on context."
  (interactive "p")
  (if (= (point) (line-beginning-position))
      (indent-to ledger-post-account-alignment-column)
    (if (and (> (point) 1)
             (looking-back "\\([^ \t]\\)" 1))
        (ledger-pcomplete interactively)
      (ledger-post-align-postings (line-beginning-position) (line-end-position)))))

(defvar ledger-mode-abbrev-table)

(defvar ledger-date-string-today (ledger-format-date))

(defun ledger-remove-effective-date ()
  "Remove the effective date from a transaction or posting."
  (interactive)
  (let ((context (car (ledger-context-at-point))))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-at-bol) (point-at-eol))
        (beginning-of-line)
        (cond ((eq 'xact context)
               (re-search-forward ledger-iso-date-regexp)
               (when (= (char-after) ?=)
                 (let ((eq-pos (point)))
                   (delete-region
                    eq-pos
                    (re-search-forward ledger-iso-date-regexp)))))
              ((eq 'acct-transaction context)
               ;; Match "; [=date]" & delete string
               (when (re-search-forward
                      (concat ledger-comment-regex
                              "\\[=" ledger-iso-date-regexp "\\]")
                      nil 'noerr)
                 (replace-match ""))))))))

(defun ledger-insert-effective-date (&optional date)
  "Insert effective date `DATE' to the transaction or posting.

If `DATE' is nil, prompt the user a date.

Replace the current effective date if there's one in the same
line.

With a prefix argument, remove the effective date."
  (interactive)
  (if (and (listp current-prefix-arg)
           (= 4 (prefix-numeric-value current-prefix-arg)))
      (ledger-remove-effective-date)
    (let* ((context (car (ledger-context-at-point)))
           (date-string (or date (ledger-read-date "Effective date: "))))
      (save-restriction
        (narrow-to-region (point-at-bol) (point-at-eol))
        (cond
         ((eq 'xact context)
          (beginning-of-line)
          (re-search-forward ledger-iso-date-regexp)
          (when (= (char-after) ?=)
            (ledger-remove-effective-date))
          (insert "=" date-string))
         ((eq 'acct-transaction context)
          (end-of-line)
          (ledger-remove-effective-date)
          (insert "  ; [=" date-string "]")))))))

(defun ledger-mode-remove-extra-lines ()
  "Get rid of multiple empty lines."
  (goto-char (point-min))
  (while (re-search-forward "\n\n\\(\n\\)+" nil t)
    (replace-match "\n\n")))

(defun ledger-mode-clean-buffer ()
  "Indent, remove multiple line feeds and sort the buffer."
  (interactive)
  (let ((start (point-min-marker))
        (end (point-max-marker)))
    (ledger-navigate-beginning-of-xact)
    (beginning-of-line)
    (let ((target (buffer-substring (point) (progn
                                              (end-of-line)
                                              (point)))))
      (goto-char start)
      (untabify start end)
      (ledger-sort-buffer)
      (ledger-post-align-postings start end)
      (ledger-mode-remove-extra-lines)
      (goto-char start)
      (search-forward target))))

(defvar ledger-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table in use in `ledger-mode' buffers.")

(defvar ledger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?a)] 'ledger-add-transaction)
    (define-key map [(control ?c) (control ?b)] 'ledger-post-edit-amount)
    (define-key map [(control ?c) (control ?c)] 'ledger-toggle-current)
    (define-key map [(control ?c) (control ?d)] 'ledger-delete-current-transaction)
    (define-key map [(control ?c) (control ?e)] 'ledger-toggle-current-transaction)
    (define-key map [(control ?c) (control ?f)] 'ledger-occur)
    (define-key map [(control ?c) (control ?k)] 'ledger-copy-transaction-at-point)
    (define-key map [(control ?c) (control ?r)] 'ledger-reconcile)
    (define-key map [(control ?c) (control ?s)] 'ledger-sort-region)
    (define-key map [(control ?c) (control ?t)] 'ledger-insert-effective-date)
    (define-key map [(control ?c) (control ?u)] 'ledger-schedule-upcoming)
    (define-key map [(control ?c) (control ?p)] 'ledger-display-balance-at-point)
    (define-key map [(control ?c) (control ?l)] 'ledger-display-ledger-stats)
    (define-key map [(control ?c) (control ?q)] 'ledger-post-align-xact)

    (define-key map [tab] 'ledger-magic-tab)
    (define-key map [(control tab)] 'ledger-post-align-xact)
    (define-key map [(control ?i)] 'ledger-magic-tab)
    (define-key map [(control ?c) tab] 'ledger-fully-complete-xact)
    (define-key map [(control ?c) (control ?i)] 'ledger-fully-complete-xact)

    (define-key map [(control ?c) (control ?o) (control ?a)] 'ledger-report-redo)
    (define-key map [(control ?c) (control ?o) (control ?e)] 'ledger-report-edit)
    (define-key map [(control ?c) (control ?o) (control ?g)] 'ledger-report-goto)
    (define-key map [(control ?c) (control ?o) (control ?k)] 'ledger-report-kill)
    (define-key map [(control ?c) (control ?o) (control ?r)] 'ledger-report)
    (define-key map [(control ?c) (control ?o) (control ?s)] 'ledger-report-save)

    (define-key map [(meta ?p)] 'ledger-navigate-prev-xact-or-directive)
    (define-key map [(meta ?n)] 'ledger-navigate-next-xact-or-directive)
    (define-key map [(meta ?q)] 'ledger-post-align-dwim)
    map)
  "Keymap for `ledger-mode'.")

(easy-menu-define ledger-mode-menu ledger-mode-map
  "Ledger menu"
  '("Ledger"
    ["Narrow to REGEX" ledger-occur]
    ["Show all transactions" ledger-occur-mode ledger-occur-mode]
    ["Ledger Statistics" ledger-display-ledger-stats ledger-works]
    "---"
    ["Show upcoming transactions" ledger-schedule-upcoming]
    ["Add Transaction (ledger xact)" ledger-add-transaction ledger-works]
    ["Complete Transaction" ledger-fully-complete-xact]
    ["Delete Transaction" ledger-delete-current-transaction]
    "---"
    ["Calc on Amount" ledger-post-edit-amount]
    "---"
    ["Check Balance" ledger-display-balance-at-point ledger-works]
    ["Reconcile Account" ledger-reconcile ledger-works]
    "---"
    ["Toggle Current Transaction" ledger-toggle-current-transaction]
    ["Toggle Current Posting" ledger-toggle-current]
    ["Copy Trans at Point" ledger-copy-transaction-at-point]
    "---"
    ["Clean-up Buffer" ledger-mode-clean-buffer]
    ["Check Buffer" ledger-check-buffer ledger-works]
    ["Align Region" ledger-post-align-postings mark-active]
    ["Align Xact" ledger-post-align-xact]
    ["Sort Region" ledger-sort-region mark-active]
    ["Sort Buffer" ledger-sort-buffer]
    ["Mark Sort Beginning" ledger-sort-insert-start-mark]
    ["Mark Sort End" ledger-sort-insert-end-mark]
    ["Set effective date" ledger-insert-effective-date]
    "---"
    ["Customize Ledger Mode" (lambda () (interactive) (customize-group 'ledger))]
    "---"
    ["Run Report" ledger-report ledger-works]
    ["Goto Report" ledger-report-goto ledger-works]
    ["Re-run Report" ledger-report-redo ledger-works]
    ["Save Report" ledger-report-save ledger-works]
    ["Edit Report" ledger-report-edit ledger-works]
    ["Kill Report" ledger-report-kill ledger-works]))

;;;###autoload
(define-derived-mode ledger-mode text-mode "Ledger"
  "A mode for editing ledger data files."
  (ledger-check-version)
  (setq font-lock-defaults
        '(ledger-font-lock-keywords t nil nil nil
                                    (font-lock-fontify-region-function . ledger-fontify-buffer-part)))

  (setq-local pcomplete-parse-arguments-function 'ledger-parse-arguments)
  (setq-local pcomplete-command-completion-function 'ledger-complete-at-point)
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  (add-hook 'after-save-hook 'ledger-report-redo nil t)

  (add-hook 'post-command-hook 'ledger-highlight-xact-under-point nil t)

  (ledger-init-load-init-file)
  (setq-local comment-start ";")

  (setq-local indent-region-function 'ledger-post-align-postings))



(provide 'ledger-mode)

;;; ledger-mode.el ends here

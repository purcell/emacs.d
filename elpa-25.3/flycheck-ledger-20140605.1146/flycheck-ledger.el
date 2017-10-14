;;; flycheck-ledger.el --- Flycheck integration for ledger files

;; Copyright (C) 2013-2014  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Version: DEV
;; Package-Version: 20140605.1146
;; Keywords: convenience languages tools
;; Package-Requires: ((flycheck "0.15"))

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

;; This flychecker uses the output of "ledger balance" on the current file to
;; find errors such as unbalanced transactions and syntax errors.

;;;; Setup

;; (eval-after-load 'flycheck '(require 'flycheck-ledger))

;;; Code:

(require 'flycheck)

(flycheck-define-checker ledger
  "A checker for ledger files, showing unmatched balances and failed checks."
  :command ("ledger" (option-flag "--pedantic" flycheck-ledger-pedantic) (eval (when (eq flycheck-ledger-pedantic 'check-payees) "--check-payees")) "-f" source-inplace "balance")
  :error-patterns
  ((error line-start "While parsing file \"" (file-name) "\", line " line ":" (zero-or-more whitespace) "\n"
          (zero-or-more line-start (or "While " "> ") (one-or-more not-newline) "\n" )
          (message (minimal-match (zero-or-more line-start (zero-or-more not-newline) "\n"))
                   "Error: " (one-or-more not-newline) "\n"))
   )
  :modes ledger-mode)

(flycheck-def-option-var flycheck-ledger-pedantic () ledger
  "Whether to be pedantic in ledger.

When equal to `check-payees', be pedantic on account name and payees,
When non-nil, be pedantic on account name,
otherwise don't be pedantic."
  :type '(radio (const :tag "Check only syntax" nil) (const :tag "Check account name" t) (const :tag "Also check payees" check-payees)))

(add-to-list 'flycheck-checkers 'ledger)

(provide 'flycheck-ledger)
;;; flycheck-ledger.el ends here

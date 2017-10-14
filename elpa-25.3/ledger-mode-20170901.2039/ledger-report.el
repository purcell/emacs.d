;;; ledger-report.el --- Helper code for use with the "ledger" command-line tool

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
;;  Provide facilities for running and saving reports in emacs

;;; Code:

(require 'ledger-xact)
(require 'ledger-navigate)
(declare-function ledger-read-string-with-default "ledger-mode" (prompt default))
(declare-function ledger-read-account-with-prompt "ledger-mode" (prompt))

(require 'easymenu)

(defvar ledger-buf)

(defgroup ledger-report nil
  "Customization option for the Report buffer"
  :group 'ledger)

(defcustom ledger-reports
  '(("bal" "%(binary) -f %(ledger-file) bal")
    ("reg" "%(binary) -f %(ledger-file) reg")
    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
    ("account" "%(binary) -f %(ledger-file) reg %(account)"))
  "Definition of reports to run.

Each element has the form (NAME CMDLINE).  The command line can
contain format specifiers that are replaced with context sensitive
information.  Format specifiers have the format '%(<name>)' where
<name> is an identifier for the information to be replaced.  The
`ledger-report-format-specifiers' alist variable contains a mapping
from format specifier identifier to a Lisp function that implements
the substitution.  See the documentation of the individual functions
in that variable for more information on the behavior of each
specifier."
  :type '(repeat (list (string :tag "Report Name")
                       (string :tag "Command Line")))
  :group 'ledger-report)

(defcustom ledger-report-format-specifiers
  '(("ledger-file" . ledger-report-ledger-file-format-specifier)
    ("binary" . (lambda () ledger-binary-path))
    ("payee" . ledger-report-payee-format-specifier)
    ("account" . ledger-report-account-format-specifier)
    ("tagname" . ledger-report-tagname-format-specifier)
    ("tagvalue" . ledger-report-tagvalue-format-specifier))
  "An alist mapping ledger report format specifiers to implementing functions.

The function is called with no parameters and expected to return the
text that should replace the format specifier."
  :type 'alist
  :group 'ledger-report)

(defcustom ledger-report-auto-refresh t
  "If t then automatically rerun the report when the ledger buffer is saved."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-auto-refresh-sticky-cursor nil
  "If t then try to place cursor at same relative position as it was before auto-refresh."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-links-in-register t
  "When non-nil, attempt to link transactions in \"register\"
reports to their location in the currrent ledger file buffer."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-use-header-line nil
  "When non-nil, indicate the report name and command in the `header-line'
instead of in the buffer."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-header-line-fn #'ledger-report--header-function
  "When `ledger-report-use-header-line' is non-nil, evaluate this function
in the `header-line'."
  :type 'function
  :group 'ledger-report)

(defvar ledger-report-buffer-name "*Ledger Report*")

(defvar ledger-report-name nil)
(defvar ledger-report-cmd nil)
(defvar ledger-report-name-prompt-history nil)
(defvar ledger-report-cmd-prompt-history nil)
(defvar ledger-original-window-cfg nil)
(defvar ledger-report-saved nil)
(defvar ledger-minibuffer-history nil)
(defvar ledger-report-mode-abbrev-table)

(defvar ledger-report-is-reversed nil)
(defvar ledger-report-cursor-line-number nil)

(defun ledger-report-reverse-report ()
  "Reverse the order of the report."
  (interactive)
  (ledger-report-reverse-lines)
  (setq ledger-report-is-reversed (not ledger-report-is-reversed)))

(defun ledger-report-reverse-lines ()
  (goto-char (point-min))
  (forward-paragraph)
  (forward-line)
  (save-excursion
    (setq inhibit-read-only t)
    (reverse-region (point) (point-max))))

(defvar ledger-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [? ] 'scroll-up)
    (define-key map [backspace] 'scroll-down)
    (define-key map [?r] 'ledger-report-redo)
    (define-key map [(shift ?r)] 'ledger-report-reverse-report)
    (define-key map [?s] 'ledger-report-save)
    (define-key map [(shift ?s)] 'ledger-report-select-report)
    (define-key map [?k] 'ledger-report-kill)
    (define-key map [?e] 'ledger-report-edit-report)
    (define-key map [( shift ?e)] 'ledger-report-edit-reports)
    (define-key map [?q] 'ledger-report-quit)
    (define-key map [?g] 'ledger-report-redo)
    (define-key map [(control ?c) (control ?l) (control ?r)]
      'ledger-report-redo)
    (define-key map [(control ?c) (control ?l) (control ?S)]
      'ledger-report-save)
    (define-key map [(control ?c) (control ?l) (control ?k)]
      'ledger-report-kill)
    (define-key map [(control ?c) (control ?l) (control ?e)]
      'ledger-report-edit)
    (define-key map [return] 'ledger-report-visit-source)
    map)
  "Keymap for `ledger-report-mode'.")

(easy-menu-define ledger-report-mode-menu ledger-report-mode-map
  "Ledger report menu"
  '("Reports"
    ["Select Report" ledger-report-select-report]
    ["Save Report" ledger-report-save]
    ["Edit Current Report" ledger-report-edit-report]
    ["Edit All Reports" ledger-report-edit-reports]
    ["Re-run Report" ledger-report-redo]
    "---"
    ["Reverse report order" ledger-report-reverse-report]
    "---"
    ["Scroll Up" scroll-up]
    ["Visit Source" ledger-report-visit-source]
    ["Scroll Down" scroll-down]
    "---"
    ["Quit" ledger-report-quit]
    ))

(define-derived-mode ledger-report-mode text-mode "Ledger-Report"
  "A mode for viewing ledger reports.")

(defun ledger-report-tagname-format-specifier ()
  "Return a valid meta-data tag name."
  ;; It is intended completion should be available on existing account
  ;; names, but it remains to be implemented.
  (ledger-read-string-with-default "Tag Name: " nil))

(defun ledger-report-tagvalue-format-specifier ()
  "Return a valid meta-data tag name."
  ;; It is intended completion should be available on existing account
  ;; names, but it remains to be implemented.
  (ledger-read-string-with-default "Tag Value: " nil))

(defun ledger-report-read-name ()
  "Read the name of a ledger report to use, with completion.

The empty string and unknown names are allowed."
  (completing-read "Report name: "
                   ledger-reports nil nil nil
                   'ledger-report-name-prompt-history nil))

(defun ledger-report (report-name edit)
  "Run a user-specified report from `ledger-reports'.

Prompts the user for the REPORT-NAME of the report to run or
EDIT.  If no name is entered, the user will be prompted for a
command line to run.  The command line specified or associated
with the selected report name is run and the output is made
available in another buffer for viewing.  If a prefix argument is
given and the user selects a valid report name, the user is
prompted with the corresponding command line for editing before
the command is run.

The output buffer will be in `ledger-report-mode', which defines
commands for saving a new named report based on the command line
used to generate the buffer, navigating the buffer, etc."
  (interactive
   (progn
     (when (and (buffer-modified-p)
                (y-or-n-p "Buffer modified, save it? "))
       (save-buffer))
     (let ((rname (ledger-report-read-name))
           (edit (not (null current-prefix-arg))))
       (list rname edit))))
  (let ((buf (find-file-noselect (ledger-master-file)))
        (rbuf (get-buffer ledger-report-buffer-name))
        (wcfg (current-window-configuration)))
    (if rbuf
        (kill-buffer rbuf))
    (with-current-buffer
        (pop-to-buffer (get-buffer-create ledger-report-buffer-name))
      (ledger-report-mode)
      (set (make-local-variable 'ledger-report-saved) nil)
      (set (make-local-variable 'ledger-buf) buf)
      (set (make-local-variable 'ledger-report-name) report-name)
      (set (make-local-variable 'ledger-original-window-cfg) wcfg)
      (set (make-local-variable 'ledger-report-is-reversed) nil)
      (ledger-do-report (ledger-report-cmd report-name edit))
      (shrink-window-if-larger-than-buffer)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (message "q to quit; r to redo; e to edit; k to kill; s to save; SPC and DEL to scroll"))))

(defun ledger-report--header-function ()
  "Computes the string to be used as the header in the
`ledger-report' buffer."
  (format "Ledger Report: %s -- Buffer: %s -- Command: %s"
          (propertize ledger-report-name 'face 'font-lock-constant-face)
          (propertize (buffer-name ledger-buf) 'face 'font-lock-string-face)
          (propertize ledger-report-cmd 'face 'font-lock-comment-face)))

(defun ledger-report-string-empty-p (s)
  "Check S for the empty string."
  (string-equal "" s))

(defun ledger-report-name-exists (name)
  "Check to see if the given report NAME exists.

   If name exists, returns the object naming the report,
   otherwise returns nil."
  (unless (ledger-report-string-empty-p name)
    (car (assoc name ledger-reports))))

(defun ledger-reports-add (name cmd)
  "Add a new report NAME and CMD to `ledger-reports'."
  (setq ledger-reports (cons (list name cmd) ledger-reports)))

(defun ledger-reports-custom-save ()
  "Save the `ledger-reports' variable using the customize framework."
  (customize-save-variable 'ledger-reports ledger-reports))

(defun ledger-report-read-command (report-cmd)
  "Read the command line to create a report from REPORT-CMD."
  (read-from-minibuffer "Report command line: "
                        (if (null report-cmd) "ledger " report-cmd)
                        nil nil 'ledger-report-cmd-prompt-history))

(defun ledger-report-ledger-file-format-specifier ()
  "Substitute the full path to master or current ledger file.

   The master file name is determined by the variable `ledger-master-file'
   buffer-local variable which can be set using file variables.
   If it is set, it is used, otherwise the current buffer file is
   used."
  (ledger-master-file))

;; General helper functions

(defvar ledger-master-file nil)

(defun ledger-master-file ()
  "Return the master file for a ledger file.

   The master file is either the file for the current ledger buffer or the
   file specified by the buffer-local variable `ledger-master-file'.  Typically
   this variable would be set in a file local variable comment block at the
   end of a ledger file which is included in some other file."
  (if ledger-master-file
      (expand-file-name ledger-master-file)
    (buffer-file-name)))

(defun ledger-report-payee-format-specifier ()
  "Substitute a payee name.

   The user is prompted to enter a payee and that is substitued.  If
   point is in an xact, the payee for that xact is used as the
   default."
  ;; It is intended completion should be available on existing
  ;; payees, but the list of possible completions needs to be
  ;; developed to allow this.
  (ledger-read-string-with-default "Payee" (regexp-quote (ledger-xact-payee))))

(defun ledger-report-account-format-specifier ()
  "Substitute an account name.

   The user is prompted to enter an account name, which can be any
   regular expression identifying an account.  If point is on an account
   posting line for an xact, the full account name on that line is
   the default."
  ;; It is intended completion should be available on existing account
  ;; names, but it remains to be implemented.
  (ledger-read-account-with-prompt "Account"))

(defun ledger-report-expand-format-specifiers (report-cmd)
  "Expand %(account) and %(payee) appearing in REPORT-CMD with thing under point."
  (save-match-data
    (let ((expanded-cmd report-cmd))
      (set-match-data (list 0 0))
      (while (string-match "%(\\([^)]*\\))" expanded-cmd (if (> (length expanded-cmd) (match-end 0))
                                                             (match-end 0)
                                                           (1- (length expanded-cmd))))
        (let* ((specifier (match-string 1 expanded-cmd))
               (f (cdr (assoc specifier ledger-report-format-specifiers))))
          (if f
              (setq expanded-cmd (replace-match
                                  (save-match-data
                                    (with-current-buffer ledger-buf
                                      (shell-quote-argument (funcall f))))
                                  t t expanded-cmd)))))
      expanded-cmd)))

(defun ledger-report-cmd (report-name edit)
  "Get the command line to run the report name REPORT-NAME.
Optional EDIT the command."
  (let ((report-cmd (car (cdr (assoc report-name ledger-reports)))))
    ;; logic for substitution goes here
    (when (or (null report-cmd) edit)
      (setq report-cmd (ledger-report-read-command report-cmd))
      (setq ledger-report-saved nil)) ;; this is a new report, or edited report
    (setq report-cmd (ledger-report-expand-format-specifiers report-cmd))
    (set (make-local-variable 'ledger-report-cmd) report-cmd)
    (or (ledger-report-string-empty-p report-name)
        (ledger-report-name-exists report-name)
        (progn
          (ledger-reports-add report-name report-cmd)
          (ledger-reports-custom-save)))
    report-cmd))

(defun ledger-do-report (cmd)
  "Run a report command line CMD."
  (goto-char (point-min))
  (setq header-line-format (when ledger-report-use-header-line
                             '(:eval (funcall ledger-report-header-line-fn))))
  (unless ledger-report-use-header-line
    (insert (format "Report: %s\n" ledger-report-name)
            (format "Command: %s\n" cmd)
            (make-string (- (window-width) 1) ?=)
            "\n\n"))
  (let ((data-pos (point))
        (register-report (string-match " reg\\(ister\\)? " cmd))
        files-in-report)
    (shell-command
     ;; --subtotal does not produce identifiable transactions, so don't
     ;; prepend location information for them
     (if (and register-report
              ledger-report-links-in-register
              (not (string-match "--subtotal" cmd)))
         (concat cmd " --prepend-format='%(filename):%(beg_line):'")
       cmd)
     t nil)
    (when (and register-report ledger-report-links-in-register)
      (goto-char data-pos)
      (while (re-search-forward "^\\(/[^:]+\\)?:\\([0-9]+\\)?:" nil t)
        (let ((file (match-string 1))
              (line (string-to-number (match-string 2))))
          (delete-region (match-beginning 0) (match-end 0))
          (when file
            (set-text-properties (line-beginning-position) (line-end-position)
                                 (list 'ledger-source (cons file (save-window-excursion
                                                                   (save-excursion
                                                                     (find-file file)
                                                                     (widen)
                                                                     (ledger-navigate-to-line line)
                                                                     (point-marker))))))
            (add-text-properties (line-beginning-position) (line-end-position)
                                 (list 'font-lock-face 'ledger-font-report-clickable-face))
            (end-of-line)))))
    (goto-char data-pos)))


(defun ledger-report-visit-source ()
  "Visit the transaction under point in the report window."
  (interactive)
  (let* ((prop (get-text-property (point) 'ledger-source))
         (file (if prop (car prop)))
         (line-or-marker (if prop (cdr prop))))
    (when (and file line-or-marker)
      (find-file-other-window file)
      (widen)
      (if (markerp line-or-marker)
          (goto-char line-or-marker)
        (goto-char (point-min))
        (forward-line (1- line-or-marker))
        (re-search-backward "^[0-9]+")
        (beginning-of-line)
        (let ((start-of-txn (point)))
          (forward-paragraph)
          (narrow-to-region start-of-txn (point))
          (backward-paragraph))))))

(defun ledger-report-goto ()
  "Goto the ledger report buffer."
  (interactive)
  (let ((rbuf (get-buffer ledger-report-buffer-name)))
    (if (not rbuf)
        (error "There is no ledger report buffer"))
    (pop-to-buffer rbuf)
    (shrink-window-if-larger-than-buffer)))

(defun ledger-report-redo ()
  "Redo the report in the current ledger report buffer."
  (interactive)
  (let ((cur-buf (current-buffer)))
    (if (and ledger-report-auto-refresh
             (or (string= (format-mode-line 'mode-name) "Ledger")
                 (string= (format-mode-line 'mode-name) "Ledger-Report"))
             (get-buffer ledger-report-buffer-name))
        (progn

          (pop-to-buffer (get-buffer ledger-report-buffer-name))
          (shrink-window-if-larger-than-buffer)
          (setq buffer-read-only nil)
          (setq ledger-report-cursor-line-number (line-number-at-pos))
          (erase-buffer)
          (ledger-do-report ledger-report-cmd)
          (setq buffer-read-only nil)
          (if ledger-report-is-reversed (ledger-report-reverse-lines))
          (if ledger-report-auto-refresh-sticky-cursor (forward-line (- ledger-report-cursor-line-number 5)))
          (pop-to-buffer cur-buf)))))

(defun ledger-report-quit ()
  "Quit the ledger report buffer."
  (interactive)
  (ledger-report-goto)
  (set-window-configuration ledger-original-window-cfg)
  (kill-buffer (get-buffer ledger-report-buffer-name)))

(defun ledger-report-edit-reports ()
  "Edit the defined ledger reports."
  (interactive)
  (customize-variable 'ledger-reports))

(defun ledger-report-edit-report ()
  "Edit the current report command in the mini buffer and re-run the report."
  (interactive)
  (setq ledger-report-cmd (ledger-report-read-command ledger-report-cmd))
  (ledger-report-redo))

(defun ledger-report-select-report ()
  "Select and run one of the named reports."
  (interactive)
  (setq ledger-report-name (ledger-report-read-name)
        ledger-report-cmd (ledger-report-cmd ledger-report-name nil))
  (ledger-report-redo))

(defun ledger-report-read-new-name ()
  "Read the name for a new report from the minibuffer."
  (let ((name ""))
    (while (ledger-report-string-empty-p name)
      (setq name (read-from-minibuffer "Report name: " nil nil nil
                                       'ledger-report-name-prompt-history)))
    name))

(defun ledger-report-save ()
  "Save the current report command line as a named report."
  (interactive)
  (ledger-report-goto)
  (let (existing-name)
    (when (ledger-report-string-empty-p ledger-report-name)
      (setq ledger-report-name (ledger-report-read-new-name)))

    (if (setq existing-name (ledger-report-name-exists ledger-report-name))
        (cond ((y-or-n-p (format "Overwrite existing report named '%s'? "
                                 ledger-report-name))
               (if (string-equal
                    ledger-report-cmd
                    (car (cdr (assq existing-name ledger-reports))))
                   (message "Nothing to save. Current command is identical to existing saved one")
                 (progn
                   (setq ledger-reports
                         (assq-delete-all existing-name ledger-reports))
                   (ledger-reports-add ledger-report-name ledger-report-cmd)
                   (ledger-reports-custom-save))))
              (t
               (progn
                 (setq ledger-report-name (ledger-report-read-new-name))
                 (ledger-reports-add ledger-report-name ledger-report-cmd)
                 (ledger-reports-custom-save)))))))

(provide 'ledger-report)

;;; ledger-report.el ends here

;;; recent-addresses.el --- store and recall recently used email addresses
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1
;; Keywords: convenience, mail
;; URL: http://nschum.de/src/emacs/recent-addresses/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; recent-addresses allows you to quickly look up previously used addresses.
;;
;; Add the following to your .emacs:
;;
;; (add-to-list 'load-path "/path/to/recent-addresses")
;; (require 'recent-addresses)
;; (recent-addresses-mode 1)
;;
;; See the documentation for `recent-addresses-mode' for further instructions.
;;
;;; Change Log:
;;
;; 2009-04-05 (0.1)
;;    Initial release.
;;
;;; Code:

(require 'message)
(eval-when-compile (require 'cl))

(defgroup recent-addresses nil
  "Store and recall recently used email addresses."
  :group 'convenience
  :group 'mail)

(defcustom recent-addresses-file
  (if (fboundp 'locate-user-emacs-file)
      (locate-user-emacs-file "recent-addresses" ".recent-addresses")
    (expand-file-name "~/.recent-addresses"))
  "*File name for storing the recent email addresses."
  :group 'recent-addresses
  :type 'file)

(defcustom recent-addresses-limit 1000
  "*The number of collected addresses to keep.
The addresses are purged when saved to file, and by `recent-addresses-purge'."
  :group 'recent-addresses
  :type 'integer)

(defcustom recent-addresses-headers '("to" "cc")
  "*A list of headers to parse in sent messages."
  :group 'recent-addresses
  :type '(repeat string))

(defcustom recent-addresses-headers-received '("from")
  "*A list of headers to parse in received messages."
  :group 'recent-addresses
  :type '(repeat string))

(defcustom recent-addresses-insert-style message-from-style
  "*The format of inserted addresses.
See `message-from-style'."
  :group 'recent-addresses
  :type '(choice (const :tag "simple" nil)
                 (const parens)
                 (const angles)
                 (const default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar recent-addresses-list 'not-fetched)

(defvar recent-addresses-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [remap message-goto-to] 'recent-addresses-add-to)
    (define-key keymap [remap message-goto-cc] 'recent-addresses-add-cc)
    (define-key keymap [remap message-goto-bcc] 'recent-addresses-add-bcc)
    keymap)
  "Keymap used by `recent-addresses-mode'.")

;;;###autoload
(define-minor-mode recent-addresses-mode
  "Minor mode for keeping track of recently used email addresses.
Addresses are collected when sending through `message-mode' and when reading in
`gnus'.  The headers collected are defined in `recent-addresses-headers' and
`recent-addresses-headers-received'.

Addresses can be added from email messages with `recent-addresses-add-headers'
and `recent-addresses-add-headers-received', as well as manually with
`recent-addresses-add'.

To insert addresses, use `recent-addresses-add-to',
`recent-addresses-add-cc' or `recent-addresses-add-bcc'.  If you want to
be prompted for this automatically when you create an email, add the
following to your .emacs:

\(add-hook 'message-setup-hook 'recent-addresses-add-first-to\)

\\{recent-addresses-mode-map}"
  nil nil recent-addresses-mode-map
  :global t
  (if recent-addresses-mode
      (progn
        (recent-addresses-load)
        (add-hook 'gnus-after-exiting-gnus-hook 'recent-addresses-save)
        (add-hook 'message-send-hook 'recent-addresses-add-headers)
        (add-hook 'gnus-article-prepare-hook
                  'recent-addresses-add-headers-received))
    (remove-hook 'gnus-after-exiting-gnus-hook 'recent-addresses-save)
    (remove-hook 'message-send-hook 'recent-addresses-add-headers)
    (remove-hook 'gnus-article-prepare-hook
                 'recent-addresses-add-headers-received)
    (recent-addresses-save)))

(defun recent-addresses-add (address)
  "Add ADDRESS to the front of `recent-addresses-list'.
Address can be an email address, or a cons of an email address and a name."
  (recent-addresses-load)
  (unless (consp address) (setq address (cons address nil)))
  (let (match)
    (when (setq match (assoc (car address) recent-addresses-list))
      ;; remove
      (setq recent-addresses-list (delete match recent-addresses-list))
      (unless (cdr address)
        (setcdr address (cdr match))))
    ;; add to front
    (push address recent-addresses-list)))

;;; file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar recent-addresses-file-coding-system
  (if (coding-system-p 'utf-8-emacs) 'utf-8-emacs 'emacs-mule))

(defun recent-addresses-purge ()
  "Reduce `recent-addresses-list' to `recent-addresses-limit'."
  (recent-addresses-load)
  (let ((end (nthcdr (1- recent-addresses-limit) recent-addresses-list)))
    (when end
      (setcdr end nil))))

(defun recent-addresses-save ()
  "Store the collected addresses to `recent-addresses-file'."
  (interactive)
  (unless (eq recent-addresses-list 'not-fetched)
    (recent-addresses-purge)
    (with-temp-buffer
      (set-buffer-file-coding-system recent-addresses-file-coding-system)
      (insert (format ";;; Generated by `recent-addresses-mode' on %s\n\n"
                      (current-time-string))
              (format "(setq recent-addresses-list '%S)" recent-addresses-list)
              "\n\n\n;;; Local Variables:\n"
              (format ";;; coding: %s\n" recent-addresses-file-coding-system)
              ";;; End:\n")
      (write-file recent-addresses-file))))

;;;###autoload
(defun recent-addresses-load (&optional force)
  "Load the previously collected addresses from `recent-addresses-file'.
Unless FORCE is set, an existing list will not be overwritten."
  (when (or force (eq recent-addresses-list 'not-fetched))
    (condition-case err
        (progn
          (if (file-readable-p recent-addresses-file)
              (load-file recent-addresses-file)
            (setq recent-addresses-list nil))
          (add-hook 'kill-emacs-hook 'recent-addresses-save))
      (error (message "An error occurred while reading recent addresses: %s"
                      (error-message-string err))))))

;;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar recent-addresses-regexps
  '(("\\([^\n<,]+\\)[ \t]<\\([^ @]+@[^ \n\t]+\\)>" 2 1)
    ("\"?\\([^ \t\n@]+@[^ \t\n,]+\\)\"?\\([ \t](\\([^)]*\\))\\)?" 1 3)))

(defun recent-addresses-parse-at-point ()
  "Parse the email address after point."
  (dolist (regexp recent-addresses-regexps)
    (when (looking-at (car regexp))
      (goto-char (match-end 0))
      (return (cons (match-string-no-properties (cadr regexp))
                    (match-string-no-properties (car (cddr regexp))))))))

(defun recent-addresses-parse-header (header)
  "Parse the email addresses in the current buffer's HEADER."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          address addresses)
      (when (search-forward (concat header ":") nil t)
        (skip-chars-forward " \t")
        (setq address (recent-addresses-parse-at-point))
        (while address
          (push address addresses)
          (skip-chars-forward ",; \t")
          (setq address (recent-addresses-parse-at-point))))
      addresses)))

(defun recent-addresses-add-headers ()
  "Add addresses found in `recent-addresses-headers'."
  (dolist (header recent-addresses-headers)
    (mapc 'recent-addresses-add (recent-addresses-parse-header header))))

(defun recent-addresses-add-headers-received ()
  "Add addresses found in `recent-addresses-headers-received'."
  (let ((recent-addresses-headers recent-addresses-headers-received))
    (recent-addresses-add-headers)))

;;; insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recent-addresses-read (&optional prompt)
  "Prompt the user for an element of `recent-addresses-list'."
  (recent-addresses-load)
  (unless prompt (setq prompt "Email address: "))
  (let* ((choices (mapcar (lambda (pair)
                            (cons (if (cdr pair)
                                      (concat (cdr pair) " <" (car pair) ">")
                                    (car pair))
                                  pair))
                          recent-addresses-list))
         (candidates (mapcar 'car choices))
         (choice (if ido-mode
                     (ido-completing-read prompt candidates)
                   (require 'iswitchb)
                   (with-no-warnings
                     (let ((iswitchb-make-buflist-hook
                            (lambda () (setq iswitchb-temp-buflist
                                             (mapcar 'car choices)))))
                       (iswitchb-read-buffer prompt))))))
    (or (cdr (assoc choice choices))
        choice)))

(defun recent-addresses-append-address (address)
  "Append formatted ADDRESS at point.
ADDRESS can be a string or a cons of address and name.
The address is formatted according to `recent-addresses-insert-style'."
  (unless (looking-back "[:,][[:space:]]*")
    (insert ", "))
  (insert (cond
           ((stringp address) address)
           ((cdr address)
            (let ((message-from-style recent-addresses-insert-style)
                  (user-full-name (cdr address))
                  (user-mail-address (car address)))
              (message-make-from)))
           (t (car address)))))

(defun recent-addresses-message-insert (prompt header &rest afters)
  "Read an email address and add it to HEADER.
If the header doesn't exist, add it behind headers specified in AFTERS.
The address is formatted according to `recent-addresses-insert-style'."
  (save-excursion
    (apply 'message-position-on-field header afters)
    (recent-addresses-append-address (recent-addresses-read prompt))))

;;;###autoload
(defun recent-addresses-add-first-to ()
  "Prompt the user for the To: header, unless there already is one.
The address is formatted according to `recent-addresses-insert-style'.
This function is safe to be run from `message-setup-hook' like this:

\(add-hook 'message-setup-hook 'recent-addresses-add-first-to\)"
  (condition-case q
      (unless (message-fetch-field "to")
        (recent-addresses-add-to))
    ;; Protect against quit, so hooks are continued.
    (quit)))

;;;###autoload
(defun recent-addresses-add-to ()
  "Read an email address and add it to To:.
The address is formatted according to `recent-addresses-insert-style'."
  (interactive)
  (recent-addresses-message-insert "To: " "To"))

;;;###autoload
(defun recent-addresses-add-cc ()
  "Read an email address and add it to CC:.
The address is formatted according to `recent-addresses-insert-style'."
  (interactive)
  (recent-addresses-message-insert "CC: " "CC" "To"))

;;;###autoload
(defun recent-addresses-add-bcc ()
  "Read an email address and add it to BCC:.
The address is formatted according to `recent-addresses-insert-style'."
  (interactive)
  (recent-addresses-message-insert "BCC: " "BCC" "CC" "To"))

(provide 'recent-addresses)
;;; recent-addresses.el ends here

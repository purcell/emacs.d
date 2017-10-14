;;; company-tng.el --- company-mode configuration for single-button interaction

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Nikita Leshenko

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; company-tng (Tab and Go) allows you to perform completion using just TAB.
;; Pressing it will both select the next completion candidate in the list and
;; insert it into the buffer (or make it look like it's inserted, in fact).
;;
;; It cycles the candidates like `yank-pop' or `dabbrev-expand' or Vim:
;; Pressing TAB selects the first item in the completion menu and inserts it in
;; the buffer. Pressing TAB again selects the second item and replaces the
;; "inserted" item with the second one. This can continue as long as the user
;; wishes to cycle through the menu. You can also press S-TAB to select the
;; previous candidate, of course.
;;
;; The benefits are that you only have to use one shortcut key and there is no
;; need to confirm the entry.
;;
;; Usage:
;;
;; To apply the default configuration for company-tng call
;; `company-tng-configure-default' from your init script.
;;
;; You can also configure company-tng manually:
;;
;; Add `company-tng-frontend' to `company-frontends':
;;
;;   (add-to-list 'company-frontends 'company-tng-frontend)
;;
;; We recommend to bind TAB to `company-select-next', S-TAB to
;; `company-select-previous', and unbind RET and other now-unnecessary
;; keys from `company-active-map':
;;
;;   (define-key company-active-map (kbd "TAB") 'company-select-next)
;;   (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;;   (define-key company-active-map (kbd "RET") nil)
;;
;; Note that it's not necessary to rebind keys to use this frontend,
;; you can use the arrow keys or M-n/M-p to select and insert
;; candidates. You also need to decide which keys to unbind, depending
;; on whether you want them to do the Company action or the default
;; Emacs action (for example C-s or C-w).

;;; Code:

(require 'company)
(require 'cl-lib)

(defvar-local company-tng--overlay nil)

;;;###autoload
(defun company-tng-frontend (command)
  "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion."
  (cl-case command
    (show
     (let ((ov (make-overlay (point) (point))))
       (setq company-tng--overlay ov)
       (overlay-put ov 'priority 2))
     (advice-add 'company-select-next :before-until 'company-tng--allow-unselected)
     (advice-add 'company-fill-propertize :filter-args 'company-tng--adjust-tooltip-highlight))
    (update
     (let ((ov company-tng--overlay)
           (selected (nth company-selection company-candidates))
           (prefix (length company-prefix)))
       (move-overlay ov (- (point) prefix) (point))
       (overlay-put ov
                    (if (= prefix 0) 'after-string 'display)
                    (and company-selection-changed selected))))
    (hide
     (when company-tng--overlay
       (delete-overlay company-tng--overlay)
       (kill-local-variable 'company-tng--overlay))
     (advice-remove 'company-select-next 'company-tng--allow-unselected)
     (advice-remove 'company-fill-propertize 'company-tng--adjust-tooltip-highlight))
    (pre-command
     (when (and company-selection-changed
                (not (company--company-command-p (this-command-keys))))
       (company--unread-this-command-keys)
       (setq this-command 'company-complete-selection)))))

;;;###autoload
(defun company-tng-configure-default ()
  "Applies the default configuration to enable company-tng."
  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (let ((keymap company-active-map))
    (define-key keymap [return] nil)
    (define-key keymap (kbd "RET") nil)
    (define-key keymap [tab] 'company-select-next)
    (define-key keymap (kbd "TAB") 'company-select-next)
    (define-key keymap [backtab] 'company-select-previous)
    (define-key keymap (kbd "S-TAB") 'company-select-previous)))

(defun company-tng--allow-unselected (&optional arg)
  "Advice `company-select-next' to allow for an 'unselected'
state. Unselected means that no user interaction took place on the
completion candidates and it's marked by setting
`company-selection-changed' to nil. This advice will call the underlying
`company-select-next' unless we need to transition to or from an unselected
state.

Possible state transitions:
- (arg > 0) unselected -> first candidate selected
- (arg < 0) first candidate selected -> unselected
- (arg < 0 wrap-round) unselected -> last candidate selected
- (arg < 0 no wrap-round) unselected -> unselected

There is no need to advice `company-select-previous' because it calls
`company-select-next' internally."
  (cond
   ;; Selecting next
   ((or (not arg) (> arg 0))
    (unless company-selection-changed
      (company-set-selection (1- (or arg 1)) 'force-update)
      t))
   ;; Selecting previous
   ((< arg 0)
    (when (and company-selection-changed
               (< (+ company-selection arg) 0))
      (company-set-selection 0)
      (setq company-selection-changed nil)
      (company-call-frontends 'update)
      t)
    )))

(defun company-tng--adjust-tooltip-highlight (args)
  "Prevent the tooltip from highlighting the current selection if it wasn't
made explicitly (i.e. `company-selection-changed' is true)"
  (unless company-selection-changed
    ;; The 4th arg of `company-fill-propertize' is selected
    (setf (nth 3 args) nil))
  args)

(provide 'company-tng)
;;; company-tng.el ends here

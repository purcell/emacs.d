;;; haskell-presentation-mode.el --- Presenting Haskell things -*- lexical-binding: t -*-

;; Copyright (C) 2013  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'haskell-mode)
(require 'haskell-session)

(defvar haskell-presentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "c") 'haskell-presentation-clear)
    map)
  "Keymap for `haskell-presentation-mode'.")

(define-derived-mode haskell-presentation-mode
  haskell-mode "Presentation"
  "Major mode for viewing Haskell snippets.
          \\{hypertext-mode-map}"
  (setq case-fold-search nil))

(defconst haskell-presentation-buffer-name
  "*Haskell Presentation*"
  "Haskell Presentation buffer name.")

(defconst haskell-presentation-hint-message
  "-- Hit `q' to close this window; `c' to clear.\n\n"
  "Hint message appered in Haskell Presentation buffer.")

(defun haskell-presentation-buffer ()
  "Return Haskell Presentaion buffer.
Return current presenation buffer or create new one if absent.
Never returns nil."
  ;; TODO Provide interactive calling options: when called interactively make
  ;; the presentation buffer current.
  (let ((may-buffer (get-buffer haskell-presentation-buffer-name)))
    (if may-buffer
        may-buffer
      (let ((buffer (generate-new-buffer haskell-presentation-buffer-name)))
        (with-current-buffer buffer
          (insert haskell-presentation-hint-message)
          (haskell-presentation-mode)
          (setq buffer-read-only t))
          buffer))))

(defun haskell-presentation-clear ()
  "Clear Haskell Presentation buffer."
  (interactive)
  (let ((hp-buf (get-buffer haskell-presentation-buffer-name)))
    (when hp-buf
      (with-current-buffer hp-buf
        (let ((buffer-read-only nil))
          (erase-buffer)
          (insert haskell-presentation-hint-message))))))

(defun haskell-presentation-present (session code &optional clear)
  "Present given code in a popup buffer.
Creates temporal Haskell Presentation buffer and assigns it to
given haskell SESSION; presented CODE will be fontified as
haskell code.  Give an optional non-nil CLEAR arg to clear the
buffer before presenting message."
  (let ((buffer (haskell-presentation-buffer)))
    (with-current-buffer buffer

      (when (boundp 'shm-display-quarantine)
        (setq-local shm-display-quarantine nil))

      (when clear (haskell-presentation-clear))
      (haskell-session-assign session)
      (goto-char (point-min))
      (forward-line 2)
      (save-excursion
        (let ((buffer-read-only nil))
          (insert code "\n\n"))))

    (if (eq major-mode 'haskell-presentation-mode)
        (switch-to-buffer buffer)
      (pop-to-buffer buffer))))

(provide 'haskell-presentation-mode)

;;; haskell-presentation-mode.el ends here

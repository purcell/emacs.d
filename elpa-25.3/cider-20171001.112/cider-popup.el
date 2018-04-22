;;; cider-popup.el --- Creating and quitting popup buffers  -*- lexical-binding: t; -*-

;; Copyright © 2015-2017  Bozhidar Batsov, Artur Malabarba and CIDER contributors

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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

;; Common functionality for dealing with popup buffers.

;;; Code:

(require 'subr-x)
(require 'cider-compat)

(define-minor-mode cider-popup-buffer-mode
  "Mode for CIDER popup buffers"
  nil
  (" cider-tmp")
  '(("q" .  cider-popup-buffer-quit-function)))

(defvar-local cider-popup-buffer-quit-function #'cider-popup-buffer-quit
  "The function that is used to quit a temporary popup buffer.")

(defun cider-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the function `cider-popup-buffer-quit-function'.
KILL-BUFFER-P is passed along."
  (interactive)
  (funcall cider-popup-buffer-quit-function kill-buffer-p))

(defun cider-popup-buffer (name &optional select mode ancillary)
  "Create new popup buffer called NAME.
If SELECT is non-nil, select the newly created window.
If major MODE is non-nil, enable it for the popup buffer.
If ANCILLARY is non-nil, the buffer is added to `cider-ancillary-buffers'
and automatically removed when killed."
  (thread-first (cider-make-popup-buffer name mode ancillary)
    (cider-popup-buffer-display select)))

(defun cider-popup-buffer-display (buffer &optional select)
  "Display BUFFER.
If SELECT is non-nil, select the BUFFER."
  (let ((window (get-buffer-window buffer 'visible)))
    (when window
      (with-current-buffer buffer
        (set-window-point window (point))))
    ;; If the buffer we are popping up is already displayed in the selected
    ;; window, the below `inhibit-same-window' logic will cause it to be
    ;; displayed twice - so we early out in this case. Note that we must check
    ;; `selected-window', as async request handlers are executed in the context
    ;; of the current connection buffer (i.e. `current-buffer' is dynamically
    ;; bound to that).
    (unless (eq window (selected-window))
      ;; Non nil `inhibit-same-window' ensures that current window is not covered
      ;; Non nil `inhibit-switch-frame' ensures that the other frame is not selected
      ;; if that's where the buffer is being shown.
      (funcall (if select #'pop-to-buffer #'display-buffer)
               buffer `(nil . ((inhibit-same-window . ,pop-up-windows)
                               (reusable-frames . visible))))))
  buffer)

(defun cider-popup-buffer-quit (&optional kill)
  "Quit the current (temp) window.
Bury its buffer using `quit-restore-window'.
If prefix argument KILL is non-nil, kill the buffer instead of burying it."
  (interactive)
  (quit-restore-window (selected-window) (if kill 'kill 'append)))

(defvar-local cider-popup-output-marker nil)

(defvar cider-ancillary-buffers nil)

(defun cider-make-popup-buffer (name &optional mode ancillary)
  "Create a temporary buffer called NAME using major MODE (if specified).
If ANCILLARY is non-nil, the buffer is added to `cider-ancillary-buffers'
and automatically removed when killed."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (when mode
      (funcall mode))
    (cider-popup-buffer-mode 1)
    (setq cider-popup-output-marker (point-marker))
    (setq buffer-read-only t)
    (when ancillary
      (add-to-list 'cider-ancillary-buffers name)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (setq cider-ancillary-buffers
                        (remove name cider-ancillary-buffers)))
                nil 'local))
    (current-buffer)))

(defun cider-emit-into-popup-buffer (buffer value &optional face inhibit-indent)
  "Emit into BUFFER the provided VALUE optionally using FACE.
Indent emitted value (usually a sexp) unless INHIBIT-INDENT is specified
and non-nil."
  ;; Long string output renders Emacs unresponsive and users might intentionally
  ;; kill the frozen popup buffer. Therefore, we don't re-create the buffer and
  ;; silently ignore the output.
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (moving (= (point) cider-popup-output-marker)))
        (save-excursion
          (goto-char cider-popup-output-marker)
          (let ((value-str (format "%s" value)))
            (when face
              (if (fboundp 'add-face-text-property)
                  (add-face-text-property 0 (length value-str) face nil value-str)
                (add-text-properties 0 (length value-str) (list 'face face) value-str)))
            (insert value-str))
          (unless inhibit-indent
            (indent-sexp))
          (set-marker cider-popup-output-marker (point)))
        (when moving (goto-char cider-popup-output-marker))))))

(provide 'cider-popup)

;;; cider-popup.el ends here

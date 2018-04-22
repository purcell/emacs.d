;;; haskell-menu.el --- A Haskell sessions menu -*- lexical-binding: t -*-

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

;;; Todo:

;;; Code:

(require 'cl-lib)
(require 'haskell-compat)
(require 'haskell-session)
(require 'haskell-process)
(require 'haskell-interactive-mode)

(defcustom haskell-menu-buffer-name "*haskell-menu*"
  "The name of the Haskell session menu buffer"
  :group 'haskell-interactive
  :type 'string)

;;;###autoload
(defun haskell-menu ()
  "Launch the Haskell sessions menu."
  (interactive)
  (or (get-buffer haskell-menu-buffer-name)
      (with-current-buffer (get-buffer-create haskell-menu-buffer-name)
        (haskell-menu-mode)))
  (switch-to-buffer-other-window (get-buffer haskell-menu-buffer-name))
  (haskell-menu-revert-function nil nil))

(defvar haskell-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "RET") 'haskell-menu-mode-ret)
    map)
  "Keymap for `haskell-menu-mode'.")

(define-derived-mode haskell-menu-mode special-mode "Haskell Session Menu"
  "Major mode for managing Haskell sessions.
Each line describes one session.
Letters do not insert themselves; instead, they are commands."
  (setq buffer-read-only t)
  (setq-local revert-buffer-function 'haskell-menu-revert-function)
  (setq truncate-lines t)
  (haskell-menu-revert-function nil t))

(suppress-keymap haskell-menu-mode-map t)

(defun haskell-menu-revert-function (_arg1 _arg2)
  "Function to refresh the display."
  (let ((buffer-read-only nil)
        (orig-line (line-number-at-pos))
        (orig-col (current-column)))
    (or (eq buffer-undo-list t)
        (setq buffer-undo-list nil))
    (erase-buffer)
    (haskell-menu-insert-menu)
    (goto-char (point-min))
    (forward-line (1- orig-line))
    (forward-char orig-col)))

(defun haskell-menu-insert-menu ()
  "Insert the Haskell sessions menu to the current buffer."
  (if (null haskell-sessions)
      (insert "No Haskell sessions.")
    (haskell-menu-tabulate
     (list "Name" "PID" "Time" "RSS" "Cabal directory" "Working directory" "Command")
     (mapcar (lambda (session)
               (let ((process (haskell-process-process (haskell-session-process session))))
                 (cond
                  (process
                   (let ((id (process-id process)))
                     (list (propertize (haskell-session-name session) 'face 'buffer-menu-buffer)
                           (if (process-live-p process) (number-to-string id) "-")
                           (if (process-live-p process)
                               (format-time-string "%H:%M:%S"
                                                   (encode-time (cl-caddr (assoc 'etime (process-attributes id)))
                                                                0 0 0 0 0))
                             "-")
                           (if (process-live-p process)
                               (concat (number-to-string (/ (cdr (assoc 'rss (process-attributes id)))
                                                            1024))
                                       "MB")
                             "-")
                           (haskell-session-cabal-dir session)
                           (haskell-session-current-dir session)
                           (mapconcat 'identity (process-command process) " "))))
                  (t (list (propertize (haskell-session-name session) 'face 'buffer-menu-buffer)
                           "—"
                           "—"
                           "—"
                           (haskell-session-cabal-dir session)
                           (haskell-session-current-dir session))))))
             haskell-sessions))))

(defun haskell-menu-tabulate (headings rows)
  "Prints a list of lists as a formatted table to the current buffer."
  (let* ((columns (length headings))
         (widths (make-list columns 0)))
    ;; Calculate column widths. This is kind of hideous.
    (dolist (row rows)
      (setq widths
            (let ((list (list)))
              (dotimes (i columns)
                (setq list (cons (max (nth i widths)
                                      (1+ (length (nth i row)))
                                      (1+ (length (nth i headings))))
                                 list)))
              (reverse list))))
    ;; Print headings.
    (let ((heading (propertize " " 'display '(space :align-to 0))))
      (dotimes (i columns)
        (setq heading (concat heading
                              (format (concat "%-" (number-to-string (nth i widths)) "s")
                                      (nth i headings)))))
      (setq header-line-format heading))
    ;; Print tabulated rows.
    (dolist (row rows)
      (dotimes (i columns)
        (insert (format (concat "%-" (number-to-string (nth i widths)) "s")
                        (nth i row))))
      (insert "\n"))))

(defun haskell-menu-mode-ret ()
  "Handle RET key."
  (interactive)
  (let* ((name (save-excursion
                 (goto-char (line-beginning-position))
                 (buffer-substring-no-properties (point)
                                                 (progn (search-forward " ")
                                                        (forward-char -1)
                                                        (point)))))
         (session (car (cl-remove-if-not (lambda (session)
                                           (string= (haskell-session-name session)
                                                    name))
                                         haskell-sessions))))
    (switch-to-buffer (haskell-session-interactive-buffer session))))

(provide 'haskell-menu)

;;; haskell-menu.el ends here

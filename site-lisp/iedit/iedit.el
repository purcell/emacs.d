;;; iedit.el --- Edit multiple regions with the same content simultaneously.

;; Copyright (C) 2010 Victor Ren

;; Time-stamp: <2010-01-25 10:37:47 erenyin>
;; Author: Victor Ren <victor.ren <at> ericsson.com>
;; Keywords: region replace simultaneous
;; Version: 0.31
;; X-URL: 

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

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

;; This package provides a more intuitive way of replace-string operation:
;; - Mark the contents in the buffer
;; - Start iedit minor mode - by press C-;
;;   All of the same contents in the buffer are highlighted
;; - Edit one of the contents
;;   The change is applied to other contents simultaneously
;; - Finish - by pressing C-; again
;;
;; So I call it in-place "replace-string".
;; 
;;; Suggested key bindings:
;;
;; (define-key global-map (kbd "C-;") 'iedit-mode)
;;
;; todo:
;; - Start from isearch
;; - Use symbol at current point by default
;; - Toggle displaying lines except lines contains matches
;; - Adjust region when iedit mode is active
;; - Lazy highlight feature (from isearch)
;; - Help information
;; 
;; Change log:
;;
;; 2010/01/25 Disable updating other regions when undo-in-progress since they
;;            are already updated by undo
;;	
;;; Code:

(eval-when-compile (require 'cl))

(defgroup iedit nil
  "Edit multiple regions with the same content simultaneously."
  :prefix "iedit-"
  :group 'replace
  :group 'convenience)

(defcustom iedit-occurrence-face 'highlight
  "*Face used for the occurrences' default values."
  :type 'face
  :group 'iedit)

(defvar iedit-mode-hook nil
  "Function(s) to call after starting up an iedit.")

(defvar iedit-mode-end-hook nil
  "Function(s) to call after terminating an iedit.")

(defvar iedit-mode nil) ;; Name of the minor mode

(make-variable-buffer-local 'iedit-mode)

(or (assq 'iedit-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(iedit-mode iedit-mode))))

(defvar iedit-occurrences nil
  "The occurrences slot contains a list of overlays used to indicate
the position of each occurrence.  In addition, the occurrence overlay is
used to provide a different face configurable via
`iedit-occurrence-face'."  )

(make-variable-buffer-local 'iedit-occurrences)

(defvar iedit-mode-map nil
  "Keymap used while iedit mode is enabled.")

(if iedit-mode-map
    nil
  (setq iedit-mode-map (make-sparse-keymap))
  ;; Default key bindings
  (define-key iedit-mode-map (kbd "TAB")             'iedit-next-occurrence)
  (define-key iedit-mode-map (kbd "<S-tab>")         'iedit-prev-occurrence)
  (define-key iedit-mode-map (kbd "<S-iso-lefttab>") 'iedit-prev-occurrence))

(or (assq 'iedit-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'iedit-mode iedit-mode-map) minor-mode-map-alist)))

(defun iedit-mode (beg end)
  "Toggle iedit mode.
Commands:
\\{iedit-mode-map}"
  (interactive "r")
  (if iedit-mode
      (iedit-done)
    (progn
      (or (and mark-active (not (equal beg end)))
          (error "No region select, cannot enable iedit mode"))
      (deactivate-mark)
      (iedit-start (buffer-substring beg end)))))

(defun iedit-start (occurrence-exp)
  "Start an iedit for the occurrence-exp in the current buffer."
  (setq	iedit-mode " Iedit")
  (setq iedit-occurrences nil)
  (force-mode-line-update)
  (run-hooks 'iedit-mode-hook)
  (add-hook 'mouse-leave-buffer-hook 'iedit-done)
  (add-hook 'kbd-macro-termination-hook 'iedit-done)
  ;; Find and record each occurrence's markers and add the overlay to the occurrences
  (save-excursion
    (goto-char (point-min))
    (while (search-forward occurrence-exp nil t)
      (let ((start (copy-marker (match-beginning 0) t))
            (end (copy-marker (point) t)))
        (let ((occurrence (iedit-make-occurrence-overlay)))
          (push occurrence iedit-occurrences)
          (move-overlay occurrence start end))))))

(defun iedit-done ()
  "Exit iedit mode."
  (when iedit-occurrences
    (dolist (occurrence iedit-occurrences)
      (delete-overlay occurrence))
    (setq iedit-occurrences nil))
  (remove-hook 'mouse-leave-buffer-hook 'iedit-done)
  (remove-hook 'kbd-macro-termination-hook 'iedit-done)
  (setq iedit-mode nil)
  (force-mode-line-update)
  (run-hooks 'iedit-mode-end-hook))

(defun iedit-make-occurrence-overlay (&optional name)
  "Create an overlay for an occurrence in a iedit.
Add the appropriate properties for the overlay to provide: a face
used to display a occurrence's default value, and modification hooks
to update occurrences if the user starts typing."
  (let ((occurrence (make-overlay (point) (point) (current-buffer) nil t)))
    (overlay-put occurrence 'face iedit-occurrence-face)
    (overlay-put occurrence 'insert-in-front-hooks '(iedit-occurrence-update))
    (overlay-put occurrence 'insert-behind-hooks '(iedit-occurrence-update))
    (overlay-put occurrence 'modification-hooks '(iedit-occurrence-update))
    occurrence))

(defun iedit-occurrence-update (occurrence after beg end &optional change)
  "Update all occurrences.
This modification hook is triggered when a user edits any occurrence
and is responsible for updating all other occurrences."
  (when (and after (not undo-in-progress)) ; undo will do all the work
    (let ((value (buffer-substring (overlay-start occurrence) (overlay-end occurrence)))
          (inhibit-modification-hooks t))
      (save-excursion
        (dolist (like-occurrence iedit-occurrences)
          (if (not (eq like-occurrence occurrence))
              (progn
              (goto-char (overlay-start like-occurrence))
              (delete-region (overlay-start like-occurrence)
                             (overlay-end like-occurrence))
              (insert value))))))))

(defun iedit-next-occurrence ()
  "Move point forward to the next occurrence in the `iedit'.
If there are no more occurrences in the iedit, point is moved to the
last occurrence."
  (interactive)
  (let* ((occurrences iedit-occurrences)
         (next-pos (loop for occurrence in (reverse occurrences)
                         for start = (overlay-start occurrence)
                         when (< (point) start)
                         return start)))
    (if (not (null next-pos))
        (goto-char next-pos))))

(defun iedit-prev-occurrence ()
  "Move point backward to the previous occurrence in the `iedit'.
If there are no more occurrences in the iedit, point is moved to the first occurrence."
  (interactive)
  (let* ((occurrences iedit-occurrences)
         (prev-pos (loop for occurrence in occurrences
                         for end = (overlay-end occurrence)
                         when  (> (point) end)
                         return (overlay-start occurrence))))
    (if (not (null prev-pos))
        (goto-char prev-pos))))

(provide 'iedit)

;;; iedit.el ends here
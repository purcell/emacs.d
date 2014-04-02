;;; company-template.el

;; Copyright (C) 2009, 2010, 2013 Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

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

;;; Code:

(eval-when-compile (require 'cl))

(defface company-template-field
  '((((background dark)) (:background "yellow" :foreground "black"))
    (((background light)) (:background "orange" :foreground "black")))
  "Face used for editable text in template fields."
  :group 'company)

(defvar company-template-nav-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [tab] 'company-template-forward-field)
    (define-key keymap (kbd "TAB") 'company-template-forward-field)
    keymap))

(defvar company-template--buffer-templates nil)
(make-variable-buffer-local 'company-template--buffer-templates)

;; interactive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-templates-at (pos)
  (let (os)
    (dolist (o (overlays-at pos))
      ;; FIXME: Always return the whole list of templates?
      ;; We remove templates not at point after every command.
      (when (memq o company-template--buffer-templates)
        (push o os)))
    os))

(defun company-template-move-to-first (templ)
  (interactive)
  (goto-char (overlay-start templ))
  (company-template-forward-field))

(defun company-template-forward-field ()
  (interactive)
  (let* ((start (point))
         (templates (company-template-templates-at (point)))
         (minimum (apply 'max (mapcar 'overlay-end templates)))
         (fields (loop for templ in templates
                       append (overlay-get templ 'company-template-fields))))
    (dolist (pos (mapcar 'overlay-start fields))
      (and pos
           (> pos (point))
           (< pos minimum)
           (setq minimum pos)))
    (push-mark)
    (goto-char minimum)
    (company-template-remove-field (company-template-field-at start))))

(defun company-template-field-at (&optional point)
  (loop for ovl in (overlays-at (or point (point)))
        when (overlay-get ovl 'company-template-parent)
        return ovl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-declare-template (beg end)
  (let ((ov (make-overlay beg end)))
    ;; (overlay-put ov 'face 'highlight)
    (overlay-put ov 'keymap company-template-nav-map)
    (overlay-put ov 'priority 101)
    (overlay-put ov 'evaporate t)
    (push ov company-template--buffer-templates)
    (add-hook 'post-command-hook 'company-template-post-command nil t)
    ov))

(defun company-template-remove-template (templ)
  (mapc 'company-template-remove-field
        (overlay-get templ 'company-template-fields))
  (setq company-template--buffer-templates
        (delq templ company-template--buffer-templates))
  (delete-overlay templ))

(defun company-template-add-field (templ pos text &optional display)
  "Add new field to template TEMPL at POS, inserting TEXT.
When DISPLAY is non-nil, set the respective property on the overlay.
Leave point at the end of the field."
  (assert templ)
  (goto-char pos)
  (insert text)
  (when (> (point) (overlay-end templ))
    (move-overlay templ (overlay-start templ) (point)))
  (let ((ov (make-overlay pos (+ pos (length text))))
        (siblings (overlay-get templ 'company-template-fields)))
    ;; (overlay-put ov 'evaporate t)
    (overlay-put ov 'intangible t)
    (overlay-put ov 'face 'company-template-field)
    (when display
      (overlay-put ov 'display display))
    (overlay-put ov 'company-template-parent templ)
    (overlay-put ov 'insert-in-front-hooks '(company-template-insert-hook))
    (push ov siblings)
    (overlay-put templ 'company-template-fields siblings)))

(defun company-template-remove-field (ovl &optional clear)
  (when (overlayp ovl)
    (when (overlay-buffer ovl)
      (when clear
        (delete-region (overlay-start ovl) (overlay-end ovl)))
      (delete-overlay ovl))
    (let* ((templ (overlay-get ovl 'company-template-parent))
           (siblings (overlay-get templ 'company-template-fields)))
      (setq siblings (delq ovl siblings))
      (overlay-put templ 'company-template-fields siblings))))

(defun company-template-clean-up (&optional pos)
  "Clean up all templates that don't contain POS."
  (let ((local-ovs (overlays-at (or pos (point)))))
    (dolist (templ company-template--buffer-templates)
      (unless (memq templ local-ovs)
        (company-template-remove-template templ)))))

;; hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-insert-hook (ovl after-p &rest _ignore)
  "Called when a snippet input prompt is modified."
  (unless after-p
    (company-template-remove-field ovl t)))

(defun company-template-post-command ()
  (company-template-clean-up)
  (unless company-template--buffer-templates
    (remove-hook 'post-command-hook 'company-template-post-command t)))

;; common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-template-c-like-templatify (call)
  (let* ((end (point-marker))
         (beg (- (point) (length call)))
         (cnt 0))
    (when (re-search-backward ")" beg t)
      (delete-region (match-end 0) end))
    (goto-char beg)
    (when (search-forward "(" end 'move)
      (if (eq (char-after) ?\))
          (forward-char 1)
        (let ((templ (company-template-declare-template beg end)))
          (while (re-search-forward (concat " *\\([^,)]*\\)[,)]") end t)
            (let ((sig (match-string 1)))
              (delete-region (match-beginning 1) (match-end 1))
              (save-excursion
                (company-template-add-field templ (match-beginning 1)
                                            (format "arg%d" cnt) sig))
              (incf cnt)))
          (company-template-move-to-first templ))))))

(provide 'company-template)
;;; company-template.el ends here

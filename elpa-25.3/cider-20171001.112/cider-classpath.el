;;; cider-classpath.el --- Basic Java classpath browser

;; Copyright © 2014-2017 Bozhidar Batsov and CIDER contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic Java classpath browser for CIDER.

;;; Code:

(require 'cider-client)
(require 'cider-popup)
(require 'subr-x)
(require 'cider-compat)

(defvar cider-classpath-buffer "*cider-classpath*")
(add-to-list 'cider-ancillary-buffers cider-classpath-buffer)

(defvar cider-classpath-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map (kbd "RET") #'cider-classpath-operate-on-point)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map))

(defvar cider-classpath-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-classpath-handle-mouse)
    map))

(define-derived-mode cider-classpath-mode special-mode "classpath"
  "Major mode for browsing the entries in Java's classpath.

\\{cider-classpath-mode-map}"
  (setq-local electric-indent-chars nil)
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t)))

(defun cider-classpath-list (buffer items)
  "Populate BUFFER with ITEMS."
  (with-current-buffer buffer
    (cider-classpath-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (item items)
        (insert item "\n"))
      (goto-char (point-min)))))

(defun cider-classpath-properties (text)
  "Decorate TEXT with a clickable keymap and function face."
  (let ((face (cond
               ((not (file-exists-p text)) 'font-lock-warning-face)
               ((file-directory-p text) 'dired-directory)
               (t 'default))))
    (propertize text
                'font-lock-face face
                'mouse-face 'highlight
                'keymap cider-classpath-mouse-map)))

(defun cider-classpath-operate-on-point ()
  "Expand browser according to thing at current point."
  (interactive)
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (line (buffer-substring-no-properties bol eol)))
    (find-file-other-window line)))

(defun cider-classpath-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (cider-classpath-operate-on-point))

;;;###autoload
(defun cider-classpath ()
  "List all classpath entries."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "classpath")
  (with-current-buffer (cider-popup-buffer cider-classpath-buffer t)
    (cider-classpath-list (current-buffer)
                          (mapcar (lambda (name)
                                    (cider-classpath-properties name))
                                  (cider-sync-request:classpath)))))

;;;###autoload
(defun cider-open-classpath-entry ()
  "Open a classpath entry."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "classpath")
  (when-let ((entry (completing-read "Classpath entries: " (cider-sync-request:classpath))))
    (find-file-other-window entry)))

(provide 'cider-classpath)

;;; cider-classpath.el ends here

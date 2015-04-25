;;; eproject-anything.el --- anything.el integration for eproject
;; Copyright (C) 2011 Antono Vasiljev

;; Author: Antono Vasiljev <self@antono.info>
;; Keywords: convenience, search, navigation

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
;;
;;; Commentary:

;; (require 'anything)
;; (require 'eproject)
;; (require 'eproject-anything)
;; (global-set-key (kbd "s-L") 'anything-eproject-files)
;; (global-set-key (kbd "s-b") 'anything-eproject-buffers)

;;; Code:
;;

(declare-function anything "anything")

;;;###autoload
(defvar anything-c-source-eproject-files
  '((name . "Files in eProject")
    (init . (lambda () (if (buffer-file-name)
                           (setq anything-eproject-root-dir
                                 (eproject-maybe-turn-on))
                         (setq anything-eproject-root-dir 'nil))))
    (candidates . (lambda () (if anything-eproject-root-dir
                                 (eproject-list-project-files
                                  anything-eproject-root-dir))))
    (type . file))
  "Search for files in the current eProject.")

;;;###autoload
(defvar anything-c-source-eproject-buffers
  '((name . "Buffers in this eProject")
    (init . (lambda () (if (buffer-file-name)
                           (setq anything-eproject-root-dir
                                 (eproject-maybe-turn-on))
                         (setq anything-eproject-root-dir 'nil))))
    (candidates . (lambda () (if anything-eproject-root-dir
                                 (mapcar 'buffer-name
                                         (cdr (assoc
                                               anything-eproject-root-dir
                                               (eproject--project-buffers)))))))
    (volatile)
    (type . buffer))
  "Search for buffers in this project.")

;;;###autoload
(defun anything-eproject-files ()
  "Preconfigured `anything' for searching files inside current eproject."
  (interactive)
  (let ((anything-sources
         '(anything-c-source-eproject-files))
        anything-samewindow)
    (anything nil nil nil nil nil "eproject")))

;;;###autoload
(defun anything-eproject-buffers ()
  "Preconfigured `anything' for opening buffers. Searches for
buffers in the current project, then other buffers, also gives
option of recentf. Replaces switch-to-buffer."
  (interactive)
  (anything '(anything-c-source-eproject-buffers
              anything-c-source-buffers+
              anything-c-source-buffer-not-found
              anything-c-source-recentf)))

(provide 'eproject-anything)
;;; eproject-anything.el ends here

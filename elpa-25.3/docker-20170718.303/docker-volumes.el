;;; docker-volumes.el --- Emacs interface to docker-volume

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'docker-process)
(require 'docker-utils)
(require 'magit-popup)
(require 'tablist)

(defun docker-volumes-entries ()
  "Returns the docker volumes data for `tabulated-list-entries'."
  (let* ((data (docker "volume" "ls"))
         (lines (cdr (s-split "\n" data t))))
    (-map #'docker-volume-parse lines)))

(defun docker-volume-parse (line)
  "Convert a LINE from \"docker volume ls\" to a `tabulated-list-entries' entry."
  (let ((data (s-split " \\{3,15\\}" line t)))
    (list (nth 1 data) (apply #'vector data))))

(defun docker-read-volume-name (prompt)
  "Read a volume name using PROMPT."
  (completing-read prompt (-map #'car (docker-volumes-entries))))

;;;###autoload
(defun docker-volume-rm (name)
  "Destroy the volume named NAME."
  (interactive (list (docker-read-volume-name "Delete volume: ")))
  (docker "volume rm" name))

(defun docker-volumes-rm-selection ()
  "Run `docker-volume-rm' on the volumes selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker "volume rm" it))
  (tablist-revert))

(docker-utils-define-popup docker-volumes-rm-popup
  "Popup for removing volumes."
  'docker-volumes-popups
  :man-page "docker-volume-rm"
  :actions  '((?D "Remove" docker-volumes-rm-selection)))

(defun docker-volumes-refresh ()
  "Refresh the volumes list."
  (setq tabulated-list-entries (docker-volumes-entries)))

(defvar docker-volumes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "D" 'docker-volumes-rm-popup)
    map)
  "Keymap for `docker-volumes-mode'.")

;;;###autoload
(defun docker-volumes ()
  "List docker volumes."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-volumes*")
  (docker-volumes-mode)
  (tablist-revert))

(define-derived-mode docker-volumes-mode tabulated-list-mode "Volumes Menu"
  "Major mode for handling a list of docker volumes."
  (setq tabulated-list-format [("Driver" 10 t)("Name" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Driver" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-volumes-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-volumes)

;;; docker-volumes.el ends here

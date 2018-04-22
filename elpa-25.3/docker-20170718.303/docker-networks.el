;;; docker-networks.el --- Emacs interface to docker-network

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

(defun docker-networks-entries ()
  "Returns the docker networks data for `tabulated-list-entries'."
  (let* ((data (docker "network" "ls"))
         (lines (cdr (s-split "\n" data t))))
    (-map #'docker-network-parse lines)))

(defun docker-network-parse (line)
  "Convert a LINE from \"docker network ls\" to a `tabulated-list-entries' entry."
  (let ((data (s-split " \\{3,\\}" line t)))
    (list (nth 1 data) (apply #'vector data))))

(defun docker-read-network-name (prompt)
  "Read a network name using PROMPT."
  (completing-read prompt (-map #'car (docker-networks-entries))))

;;;###autoload
(defun docker-network-rm (name)
  "Destroy the network named NAME."
  (interactive (list (docker-read-network-name "Delete network: ")))
  (docker "network rm" name))

(defun docker-networks-rm-selection ()
  "Run `docker-network-rm' on the networks selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker "network rm" it))
  (tablist-revert))

(docker-utils-define-popup docker-networks-rm-popup
  "Popup for removing networks."
  'docker-networks-popups
  :man-page "docker-network-rm"
  :actions  '((?D "Remove" docker-networks-rm-selection)))

(defun docker-networks-refresh ()
  "Refresh the networks list."
  (setq tabulated-list-entries (docker-networks-entries)))

(defvar docker-networks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "D" 'docker-networks-rm-popup)
    map)
  "Keymap for `docker-networks-mode'.")

;;;###autoload
(defun docker-networks ()
  "List docker networks."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-networks*")
  (docker-networks-mode)
  (tablist-revert))

(define-derived-mode docker-networks-mode tabulated-list-mode "Networks Menu"
  "Major mode for handling a list of docker networks."
  (setq tabulated-list-format [("Network ID" 20 t)("Name" 50 t)("Driver" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-networks-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-networks)

;;; docker-networks.el ends here

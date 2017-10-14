;;; docker-images.el --- Emacs interface to docker-images

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

(defun docker-images-entries ()
  "Returns the docker images data for `tabulated-list-entries'."
  (let* ((fmt "{{.Repository}}\\t{{.Tag}}\\t{{.ID}}\\t{{.CreatedSince}}\\t{{.Size}}")
         (data (docker "images" (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-image-parse lines)))

(defun docker-image-parse (line)
  "Convert a LINE from \"docker images\" to a `tabulated-list-entries' entry."
  (let* ((data (s-split "\t" line))
         (name (format "%s:%s" (nth 0 data) (nth 1 data))))
    (list
     (if (s-contains? "<none>" name) (nth 2 data) name)
     (apply #'vector data))))

(defun docker-read-image-name (prompt)
  "Read an image name using PROMPT."
  (completing-read prompt (-map #'car (docker-images-entries))))

;;;###autoload
(defun docker-pull (name &optional all)
  "Pull the image named NAME."
  (interactive (list (docker-read-image-name "Pull image: ") current-prefix-arg))
  (docker "pull" (when all "-a ") name))

;;;###autoload
(defun docker-push (name)
  "Push the image named NAME."
  (interactive (list (docker-read-image-name "Push image: ")))
  (docker "push" name))

;;;###autoload
(defun docker-rmi (name &optional force no-prune)
  "Destroy or untag the image named NAME.

Force removal of the image when FORCE is set.
Do not delete untagged parents when NO-PRUNE is set."
  (interactive (list (docker-read-image-name "Delete image: ") current-prefix-arg))
  (docker "rmi" (when force "-f") (when no-prune "--no-prune") name))

(defun docker-images-rmi-selection ()
  "Run `docker-rmi' on the images selection."
  (interactive)
  (let ((args (docker-images-rmi-arguments)))
    (--each (docker-utils-get-marked-items-ids)
      (docker-rmi it (-contains? args "-f") (-contains? args "--no-prune")))
    (tablist-revert)))

(defun docker-images-pull-selection ()
  "Run `docker-pull' on the images selection."
  (interactive)
  (let ((args (docker-images-pull-arguments)))
    (--each (docker-utils-get-marked-items-ids)
      (docker-pull it (-contains? args "-a")))
    (tablist-revert)))

(defun docker-images-push-selection ()
  "Run `docker-push' on the images selection."
  (interactive)
  (let ((args (s-join " " (docker-images-rmi-arguments))))
    (--each (docker-utils-get-marked-items-ids)
      (docker "push" args it))
    (tablist-revert)))

(defun docker-images-run-selection ()
  "Run `docker-run' on the images selection."
  (interactive)
  (let* ((popup-args (docker-images-run-arguments))
         (last-item (-last-item popup-args))
         (has-command (s-contains? "--command" last-item))
         (docker-args (if has-command (-slice popup-args 0 -1) popup-args)))
    (--each (docker-utils-get-marked-items-ids)
      (let ((command-args `("docker" "run" ,@docker-args ,it)))
        (when has-command
          (add-to-list 'command-args (s-chop-prefix "--command " last-item) t))
        (async-shell-command (s-join " " command-args) (format "*run %s*" it))))
    (tablist-revert)))

(defun docker-images-inspect-selection ()
  "Run `docker-inspect' on the images selection."
  (interactive)
  (docker-utils-run-command-on-selection-print
   (lambda (id) (docker "inspect" id))
   #'json-mode))

;;;###autoload
(defun docker-images-tag-entry()
  (interactive)
  (docker-utils-select-if-empty)
  (let ((ids (docker-utils-get-marked-items-ids)))
    (if (/= 1 (length ids))
        (error "Multiple images cannot be selected.")
      (let ((tag-name (read-string "Tag Name: ")))
        (docker "tag" (nth 0 ids) tag-name)
        (tablist-revert)))))

(docker-utils-define-popup docker-images-rmi-popup
  "Popup for removing images."
  'docker-images-popups
  :man-page "docker-rmi"
  :switches '((?f "Force" "-f")
              (?n "Don't prune" "--no-prune"))
  :actions  '((?D "Remove" docker-images-rmi-selection)))

(docker-utils-define-popup docker-images-pull-popup
  "Popup for pulling images."
  'docker-images-popups
  :man-page "docker-pull"
  :switches '((?a "All" "-a"))
  :actions  '((?F "Pull" docker-images-pull-selection)))

(docker-utils-define-popup docker-images-push-popup
  "Popup for pushing images."
  'docker-images-popups
  :man-page "docker-push"
  :actions  '((?P "Push" docker-images-push-selection)))

(docker-utils-define-popup docker-images-inspect-popup
  "Popup for inspecting images."
  'docker-images-popups
  :man-page "docker-inspect"
  :actions  '((?I "Inspect" docker-images-inspect-selection)))

(docker-utils-define-popup docker-images-run-popup
  "Popup for running images."
  'docker-images-popups
  :man-page "docker-run"
  :switches '((?d "Daemonize" "-d")
              (?i "Interactive" "-i")
              (?t "TTY" "-t")
              (?r "Remove" "--rm")
              (?p "Privileged" "--privileged")
              (?o "Read only" "--read-only")
              (?T "Synchronize time" "-v /etc/localtime:/etc/localtime:ro")
              (?W "Web ports" "-p 80:80 -p 443:443 -p 8080:8080")
              (?D "With display" "-v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=unix$DISPLAY"))
  :options  '((?v "volume" "-v ")
              (?m "name" "--name ")
              (?e "environment" "-e ")
              (?p "port" "-p ")
              (?w "workdir" "-w ")
              (?u "user" "-u ")
              (?n "entrypoint" "--entrypoint ")
              (?c "command" "--command "))
  :actions  '((?R "Run images" docker-images-run-selection))
  :default-arguments '("-i" "-t" "--rm"))

(defun docker-images-refresh ()
  "Refresh the images list."
  (setq tabulated-list-entries (docker-images-entries)))

(defvar docker-images-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "D" 'docker-images-rmi-popup)
    (define-key map "F" 'docker-images-pull-popup)
    (define-key map "P" 'docker-images-push-popup)
    (define-key map "R" 'docker-images-run-popup)
    (define-key map "I" 'docker-images-inspect-popup)
    (define-key map "T" 'docker-images-tag-entry)
    map)
  "Keymap for `docker-images-mode'.")

;;;###autoload
(defun docker-images ()
  "List docker images."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-images*")
  (docker-images-mode)
  (tablist-revert))

(define-derived-mode docker-images-mode tabulated-list-mode "Images Menu"
  "Major mode for handling a list of docker images."
  (setq tabulated-list-format [("Repository" 30 t)("Tag" 20 t)("Id" 16 t)("Created" 25 t)("Size" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Repository" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-images-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-images)

;;; docker-images.el ends here

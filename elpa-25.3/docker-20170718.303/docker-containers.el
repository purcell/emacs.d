;;; docker-containers.el --- Emacs interface to docker-containers

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;;         Yuki Inoue <inouetakahiroki@gmail.com>

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
(require 'json)

(defcustom docker-containers-show-all t
  "When nil, `docker-containers' will only show running containers."
  :group 'docker
  :type 'boolean)

(defun docker-containers-entries ()
  "Return the docker containers data for `tabulated-list-entries'."
  (let* ((fmt "[{{json .ID}},{{json .Image}},{{json .Command}},{{json .RunningFor}},{{json .Status}},{{json .Ports}},{{json .Names}}]")
         (data (docker "ps" (format "--format=\"%s\"" fmt) (when docker-containers-show-all "-a ")))
         (lines (s-split "\n" data t)))
    (-map #'docker-container-parse lines)))

(defun docker-container-parse (line)
  "Convert a LINE from \"docker ps\" to a `tabulated-list-entries' entry."
  (let (data)
    (condition-case err
        (setq data (json-read-from-string line))
      (json-readtable-error
       (error "could not read following string as json:\n%s" line)))
    (list (aref data 6) data)))

(defun docker-read-container-name (prompt)
  "Read an container name using PROMPT."
  (completing-read prompt (-map #'car (docker-containers-entries))))

;;;###autoload
(defun docker-start (name)
  "Start the container named NAME."
  (interactive (list (docker-read-container-name "Start container: ")))
  (docker "start" name))

;;;###autoload
(defun docker-stop (name &optional timeout)
  "Stop the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it."
  (interactive (list (docker-read-container-name "Stop container: ") current-prefix-arg))
  (docker "stop" (when timeout (format "-t %d" timeout)) name))

;;;###autoload
(defun docker-restart (name &optional timeout)
  "Restart the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it."
  (interactive (list (docker-read-container-name "Restart container: ") current-prefix-arg))
  (docker "restart" (when timeout (format "-t %d" timeout)) name))

;;;###autoload
(defun docker-pause (name)
  "Pause the container named NAME."
  (interactive (list (docker-read-container-name "Pause container: ")))
  (docker "pause" name))

;;;###autoload
(defun docker-unpause (name)
  "Unpause the container named NAME."
  (interactive (list (docker-read-container-name "Unpause container: ")))
  (docker "unpause" name))

;;;###autoload
(defun docker-rm (name &optional force link volumes)
  "Remove the container named NAME.

With prefix argument, sets FORCE to true.

Force the removal even if the container is running when FORCE is set.
Remove the specified link and not the underlying container when LINK is set.
Remove the volumes associated with the container when VOLUMES is set."
  (interactive (list (docker-read-container-name "Delete container: ") current-prefix-arg))
  (docker "rm" (when force "-f") (when link "-l") (when volumes "-v") name))

;;;###autoload
(defun docker-kill (name &optional signal)
  "Kill the container named NAME using SIGNAL."
  (interactive (list (docker-read-container-name "Kill container: ")))
  (docker "kill" (when signal (format "-s %s" signal)) name))

;;;###autoload
(defun docker-inspect (name)
  "Inspect the container named NAME."
  (interactive (list (docker-read-container-name "Inspect container: ")))
  (docker "inspect" name))

;;;###autoload
(defun docker-container-find-file (container file)
  (interactive
   (let* ((container-name (docker-read-container-name "container: "))
          (tramp-filename (read-file-name "file: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (find-file (format "/docker:%s:%s" container file)))

;;;###autoload
(defun docker-container-dired (container directory)
  (interactive
   (let* ((container-name (docker-read-container-name "container: "))
          (tramp-filename (read-directory-name "directory: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (dired (format "/docker:%s:%s" container directory)))

;;;###autoload
(defun docker-container-shell (container)
  (interactive (list (docker-read-container-name "container: ")))
  (let* ((container-address (format "docker:%s:/" container))
         (file-prefix (if (file-remote-p default-directory)
                          (with-parsed-tramp-file-name default-directory nil
                            (format "/%s:%s|" method host))
                        "/"))
         (default-directory (format "%s%s" file-prefix container-address)))
    (shell (format "*shell %s*" default-directory))))

(defun docker-containers-find-file-selection ()
  "Run `docker-container-find-file' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-find-file it "/")))

(defun docker-containers-shell-selection ()
  "Run `docker-container-shell' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-shell it)))

(defun docker-containers-run-command-on-selection (command arguments)
  "Run a docker COMMAND on the containers selection with ARGUMENTS."
  (interactive "sCommand: \nsArguments: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker command arguments it))
  (tablist-revert))

(defun docker-containers-run-command-on-selection-print (command arguments)
  "Run a docker COMMAND on the containers selection with ARGUMENTS and print"
  (interactive "sCommand: \nsArguments: ")
  (docker-utils-run-command-on-selection-print
   (lambda (id) (docker command arguments id))))

(defmacro docker-containers-create-selection-functions (&rest functions)
  (declare (indent defun) (doc-string 2))
  `(progn ,@(--map
             `(defun ,(intern (format "docker-containers-%s-selection" it)) ()
                ,(format "Run `docker-%s' on the containers selection." it)
                (interactive)
                (docker-containers-run-command-on-selection ,(symbol-name it)
                                                            (s-join " " ,(list (intern (format "docker-containers-%s-arguments" it))))))
             functions)))

;;;###autoload
(defun docker-containers-rename ()
  (interactive)
  (docker-utils-select-if-empty)
  (let ((ids (docker-utils-get-marked-items-ids)))
    (if (/= 1 (length ids))
        (error "Multiple containers cannot be selected.")
      (let ((new-name (read-string "New Name: ")))
        (docker "rename" (nth 0 ids) new-name)
        (tablist-revert)))))

(defalias 'docker-rename-entry 'docker-containers-rename)

(defun docker-containers-cp-from (container-path host-path)
  "Run `docker-cp' on the container to copy files from."
  (interactive "sContainerPath: \nFHostFile: ")
  (docker "cp" (concat (tabulated-list-get-id) ":" container-path) host-path))

(defun docker-containers-cp-to-selection (host-path container-path)
  "Run `docker-cp' on the containers selection to copy file into."
  (interactive "fHostFile: \nsContainerPath: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker "cp" host-path (concat it ":" container-path))))

(defun docker-containers-convert-container-info-to-command (container-info)
  (-map
   (lambda (container-info)
     `("docker" "run"
       ,(assoc-default 'Image container-info)
       ,@(->>
          (assoc-default 'Config container-info)
          (assoc-default 'Env)
          (append)
          (-map
           (lambda (env-cmd) (list "-e" env-cmd)))
          (apply '-concat))
       )) container-info))

(defun docker-containers-inspect-command-selection ()
  (interactive)
  (-each (docker-utils-get-marked-items-ids)
    (lambda (id)
      (let* ((json (docker "inspect" id))
             (parsed (json-read-from-string json))
             (commands
              (docker-containers-convert-container-info-to-command parsed)))
        (docker-utils-with-result-buffer
         (--each commands
           (insert (combine-and-quote-strings it))))))))

(defmacro docker-containers-create-selection-print-functions (&rest functions)
  `(progn ,@(--map
             `(defun ,(intern (format "docker-containers-%s-selection" it)) ()
                ,(format "Run `docker-%s' on the containers selection." it)
                (interactive)
                (docker-containers-run-command-on-selection-print ,(symbol-name it)
                                                                  (s-join " " ,(list (intern (format "docker-containers-%s-arguments" it))))))
             functions)))

(docker-containers-create-selection-functions
  start
  stop
  restart
  pause
  unpause
  rm
  kill)

(docker-containers-create-selection-print-functions inspect logs diff)

(docker-utils-define-popup docker-containers-diff-popup
  "Popup for showing containers diffs."
  'docker-containers-popups
  :man-page "docker-diff"
  :actions  '((?d "Diff" docker-containers-diff-selection)))

(docker-utils-define-popup docker-containers-find-file-popup
  "Popup for opening containers files."
  'docker-containers-popups
  :actions  '((?f "Open file" docker-containers-find-file-selection)))

(docker-utils-define-popup docker-containers-shell-popup
  "Popup for doing M-x `shell' to containers."
  'docker-containers-popups
  :actions  '((?b "Shell" docker-containers-shell-selection)))

(docker-utils-define-popup docker-containers-inspect-popup
  "Popup for inspecting containers."
  'docker-containers-popups
  :man-page "docker-inspect"
  :actions  '((?I "Inspect" docker-containers-inspect-selection)
              (?C "As Command" docker-containers-inspect-command-selection)))

(docker-utils-define-popup docker-containers-logs-popup
  "Popup for showing containers logs."
  'docker-containers-popups
  :man-page "docker-logs"
  :actions  '((?L "Logs" docker-containers-logs-selection)))

(docker-utils-define-popup docker-containers-start-popup
  "Popup for starting containers."
  'docker-containers-popups
  :man-page "docker-start"
  :actions  '((?S "Start" docker-containers-start-selection)))

(docker-utils-define-popup docker-containers-stop-popup
  "Popup for stoping containers."
  'docker-containers-popups
  :man-page "docker-stop"
  :options '((?t "Timeout" "-t "))
  :actions '((?O "Stop" docker-containers-stop-selection)))

(docker-utils-define-popup docker-containers-restart-popup
  "Popup for restarting containers."
  'docker-containers-popups
  :man-page "docker-restart"
  :options '((?t "Timeout" "-t "))
  :actions '((?R "Restart" docker-containers-restart-selection)))

(docker-utils-define-popup docker-containers-pause-popup
  "Popup for pauseing containers."
  'docker-containers-popups
  :man-page "docker-pause"
  :actions  '((?P "Pause" docker-containers-pause-selection)
              (?U "Unpause" docker-containers-unpause-selection)))

(docker-utils-define-popup docker-containers-rm-popup
  "Popup for removing containers."
  'docker-containers-popups
  :man-page "docker-rm"
  :switches '((?f "Force" "-f")
              (?v "Volumes" "-v"))
  :actions  '((?D "Remove" docker-containers-rm-selection)))

(docker-utils-define-popup docker-containers-kill-popup
  "Popup for kill signaling containers"
  'docker-containers-popups
  :man-page "docker-kill"
  :options  '((?s "Signal" "-s "))
  :actions  '((?K "Kill" docker-containers-kill-selection)))

(docker-utils-define-popup docker-containers-cp-popup
  "Popup for copying files from/to containers."
  'docker-containers-popups
  :man-page "docker-cp"
  :actions  '((?F "Copy From" docker-containers-cp-from)
              (?T "Copy To" docker-containers-cp-to-selection)))

(defun docker-containers-refresh ()
  "Refresh the containers list."
  (setq tabulated-list-entries (docker-containers-entries)))

(defvar docker-containers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'docker-containers-diff-popup)
    (define-key map "f" 'docker-containers-find-file-popup)
    (define-key map "b" 'docker-containers-shell-popup)
    (define-key map "C" 'docker-containers-cp-popup)
    (define-key map "I" 'docker-containers-inspect-popup)
    (define-key map "K" 'docker-containers-kill-popup)
    (define-key map "L" 'docker-containers-logs-popup)
    (define-key map "S" 'docker-containers-start-popup)
    (define-key map "O" 'docker-containers-stop-popup)
    (define-key map "R" 'docker-containers-restart-popup)
    (define-key map "P" 'docker-containers-pause-popup)
    (define-key map "D" 'docker-containers-rm-popup)
    (define-key map "r" 'docker-containers-rename)
    map)
  "Keymap for `docker-containers-mode'.")

;;;###autoload
(defun docker-containers ()
  "List docker containers."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-containers*")
  (docker-containers-mode)
  (tablist-revert))

(defalias 'docker-ps 'docker-containers)

(define-derived-mode docker-containers-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."
  (setq tabulated-list-format [("Id" 16 t)("Image" 15 t)("Command" 30 t)("Created" 15 t)("Status" 20 t)("Ports" 10 t)("Names" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Image" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-containers-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-containers)

;;; docker-containers.el ends here

;;; docker-machine.el --- Emacs interface to docker-machine

;; Author: Ben Swift <ben@benswift.me>

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

(defun docker-machines-entries ()
  "Returns the docker machines data for `tabulated-list-entries'."
  (let* ((fmt "{{.Name}}\\t{{.Active}}\\t{{.DriverName}}\\t{{.State}}\\t{{.URL}}\\t{{.Swarm}}\\t{{.DockerVersion}}\\t{{.Error}}")
         (data (shell-command-to-string (format "docker-machine ls %s" (format "--format=\"%s\"" fmt))))
         (lines (s-split "\n" data t)))
    (-map #'docker-machine-parse lines)))

(defun docker-machine-parse (line)
  "Convert a LINE from \"docker machine ls\" to a `tabulated-list-entries' entry."
  (let ((data (s-split "\t" line)))
    (list (car data) (apply #'vector data))))

(defun docker-read-machine-name (prompt)
  "Read a machine name using PROMPT."
  (completing-read prompt (-map #'car (docker-machines-entries))))

(defun docker-machine (action &rest args)
  "Execute docker-machine ACTION passing arguments ARGS."
  (let ((command (format "docker-machine %s %s" action (s-join " " (-non-nil args)))))
    (message command)
    (shell-command-to-string command)))

(defun docker-machine-active ()
  "Print which machine is active."
  (docker-machine "active"))

;;;###autoload
(defun docker-machine-config (name)
  "Print the connection config for machine."
  (interactive (list (docker-read-machine-name "Config for machine: ")))
  (docker-machine "config" name))

;;;###autoload
(defun docker-machine-inspect (name)
  "Inspect information about a machine."
  (interactive (list (docker-read-machine-name "Inspect machine: ")))
  (docker-machine "inspect" name))

;;;###autoload
(defun docker-machine-ip (name)
  "Get the IP address of a machine."
  (interactive (list (docker-read-machine-name "IP for machine: ")))
  (docker-machine "ip" name))

;;;###autoload
(defun docker-machine-status (name)
  "Get the status of a machine."
  (interactive (list (docker-read-machine-name "Status of machine: ")))
  (docker-machine "status" name))

;;;###autoload
(defun docker-machine-upgrade (name)
  "Upgrade a machine to the latest version of Docker."
  (interactive (list (docker-read-machine-name "Upgrade machine: ")))
  (docker-machine "upgrade" name))

;;;###autoload
(defun docker-machine-kill (name)
  "Kill a machine."
  (interactive (list (docker-read-machine-name "Kill machine: ")))
  (docker-machine "kill" name))

;;;###autoload
(defun docker-machine-create (name driver)
  "Create a machine NAME using DRIVER."
  (interactive "sName: \nsDriver: ")
  (docker-machine "create" name "-d" driver))

;;;###autoload
(defun docker-machine-start (name)
  "Start a machine."
  (interactive (list (docker-read-machine-name "Start machine: ")))
  (docker-machine "start" name))

(defun docker-machine-env-export (line)
  (let ((index (s-index-of "=" line)))
    (unless index
      (error (format "Cannot find separator in %s" line)))
    (setenv (substring line (length "export ") index) (substring line (+ 2 index) -1))))

;;;###autoload
(defun docker-machine-env (name)
  "Parse and set environment variables from \"docker-machine env\" output"
  (interactive (list (docker-read-machine-name "Set up environment for machine: ")))
  (--each-while
      (s-lines (docker-machine "env" name))
      (s-prefix? "export" it)
    (docker-machine-env-export it)))

;;;###autoload
(defun docker-machine-stop (name)
  "Stop a machine."
  (interactive (list (docker-read-machine-name "Stop machine: ") current-prefix-arg))
  (docker-machine "stop" name))

;;;###autoload
(defun docker-machine-restart (name)
  "Restart a machine."
  (interactive (list (docker-read-machine-name "Restart machine: ") current-prefix-arg))
  (docker-machine "restart" name))

;;;###autoload
(defun docker-machine-rm (name &optional force)
  "Destroy or uncommand a machine."
  (interactive (list (docker-read-machine-name "Delete machine: ") current-prefix-arg))
  (docker-machine "rm" (when force "--force") name))

(defun docker-machine-run-command-on-selection (command arguments)
  "Run a docker COMMAND on the machines selection with ARGUMENTS."
  (interactive "sCommand: \nsArguments: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker-machine command arguments it))
  (tablist-revert))

(defmacro docker-machine-create-selection-functions (&rest functions)
  `(progn ,@(--map
             `(defun ,(intern (format "docker-machine-%s-selection" it)) ()
                ,(format "Run `docker-machine-%s' on the machines selection." it)
                (interactive)
                (docker-machine-run-command-on-selection ,(symbol-name it)
                                                         (s-join " " ,(list (intern (format "docker-machine-%s-arguments" it))))))
             functions)))

(docker-machine-create-selection-functions start stop restart rm)

(defun docker-machine-env-selection ()
  "Run docker-machine-env on selected machine"
  (interactive)
  (let ((marked (docker-utils-get-marked-items-ids)))
    (when (/= (length marked) 1)
      (error "Can only set environment vars for one machine at a time."))
    (docker-machine-env (car marked))
    (tablist-revert)))

(docker-utils-define-popup docker-machine-start-popup
  "Popup for starting machines."
  'docker-machine-popups
  :man-page "docker-machine-start"
  :actions  '((?S "Start" docker-machine-start-selection)))

(docker-utils-define-popup docker-machine-env-popup
  "Popup for setting up environment variables."
  'docker-machine-popups
  :man-page "docker-machine-env"
  :actions '((?E "Env" docker-machine-env-selection)))

(docker-utils-define-popup docker-machine-stop-popup
  "Popup for stoping machines."
  'docker-machine-popups
  :man-page "docker-machine-stop"
  :actions '((?O "Stop" docker-machine-stop-selection)))

(docker-utils-define-popup docker-machine-restart-popup
  "Popup for restarting machines."
  'docker-machine-popups
  :man-page "docker-machine-restart"
  :actions '((?R "Restart" docker-machine-restart-selection)))

(docker-utils-define-popup docker-machine-rm-popup
  "Popup for removing machines."
  'docker-machine-popups
  :man-page "docker-machine-rm"
  :switches '((?y "Automatic yes" "-y")(?f "Force" "-f"))
  :actions  '((?D "Remove" docker-machine-rm-selection))
  :default-arguments '("-y"))

(defun docker-machine-refresh ()
  "Refresh the machines list."
  (setq tabulated-list-entries (docker-machines-entries)))

(defvar docker-machine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C" 'docker-machine-create)
    (define-key map "S" 'docker-machine-start-popup)
    (define-key map "E" 'docker-machine-env-popup)
    (define-key map "O" 'docker-machine-stop-popup)
    (define-key map "R" 'docker-machine-restart-popup)
    (define-key map "D" 'docker-machine-rm-popup)
    map)
  "Keymap for `docker-machine-mode'.")

;;;###autoload
(defun docker-machines ()
  "List docker machines."
  (interactive)
  (pop-to-buffer "*docker-machines*")
  (docker-machine-mode)
  (tablist-revert))

(define-derived-mode docker-machine-mode tabulated-list-mode "Machines Menu"
  "Major mode for handling a list of docker machines."
  (setq tabulated-list-format [("Name" 16 t)("Active" 7 t)("Driver" 12 t)("State" 12 t)("URL" 30 t)("Swarm" 10 t)("Docker" 10 t)("Errors" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-machine-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-machine)

;;; docker-machine.el ends here

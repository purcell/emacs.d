;;; docker.el --- Emacs interface to Docker

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker.el
;; Keywords: filename, convenience
;; Version: 0.5.2
;; Package-Requires: ((emacs "24.4") (dash "2.12.1") (docker-tramp "0.1") (magit-popup "2.6.0") (s "1.11.0") (tablist "0.70") (json-mode "1.7.0"))

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

;; # Emacs interface to Docker!
;;
;; This package allows you to manipulate docker images, containers & more from Emacs.

;;; Code:

(defgroup docker nil
  "Docker customization group."
  :group 'convenience)

(defcustom docker-keymap-prefix "C-c d"
  "Prefix for `docker-mode'."
  :group 'docker
  :type 'string)

(defvar docker-images-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'docker-rmi)
    (define-key map "f" 'docker-pull)
    (define-key map "i" 'docker-images)
    (define-key map "p" 'docker-push)
    (define-key map "r" 'docker-run)
    map)
  "Keymap for docker images.")

(defvar docker-containers-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'docker-containers)
    (define-key map "d" 'docker-rm)
    (define-key map "u" 'docker-unpause)
    (define-key map "o" 'docker-stop)
    (define-key map "p" 'docker-pause)
    (define-key map "r" 'docker-restart)
    (define-key map "k" 'docker-kill)
    (define-key map "s" 'docker-start)
    map)
  "Keymap for docker containers.")

(defvar docker-volumes-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'docker-volume-rm)
    (define-key map "v" 'docker-volumes)
    map)
  "Keymap for docker volumes.")

(defvar docker-networks-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'docker-network-rm)
    (define-key map "n" 'docker-networks)
    map)
  "Keymap for docker networks.")

(defvar docker-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" docker-images-command-map)
    (define-key map "I" 'docker-images)
    (define-key map "c" docker-containers-command-map)
    (define-key map "C" 'docker-containers)
    (define-key map "v" docker-volumes-command-map)
    (define-key map "V" 'docker-volumes)
    (define-key map "n" docker-networks-command-map)
    (define-key map "N" 'docker-networks)
    (define-key map "B" 'dockerfile-build-buffer)
    map)
  "Keymap for `docker-mode' after `docker-keymap-prefix' was pressed.")

(defvar docker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd docker-keymap-prefix) docker-command-map)
    map)
  "Keymap for `docker-mode'.")

;;;###autoload
(define-minor-mode docker-mode
  "Minor mode to manage docker."
  nil
  " docker"
  docker-mode-map
  :group 'docker)

;;;###autoload
(define-globalized-minor-mode docker-global-mode
  docker-mode
  docker-mode)

(provide 'docker)

;;; docker.el ends here

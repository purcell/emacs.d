;;; rails-webkick.el --- functions for manadge webrick

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-webrick.el $
;; $Id: rails-webrick.el 60 2007-01-13 20:01:21Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(defcustom rails-webrick:default-env "development"
  "Default WEBrick environment"
  :group 'rails
  :type 'string
  :tag "Rails Default WEBRick Port")

(defcustom rails-webrick:port "3000"
  "Default WEBrick port"
  :group 'rails
  :type 'string
  :tag "Rails WEBRick Default Port")

(defcustom rails-webrick:use-mongrel nil
  "Use Mongrel by default, instead of WEBrick."
  :group 'rails
  :type 'boolean
  :tag "Rails Use Mongrel")

(defcustom rails-webrick:server-name "http://localhost"
  "Protocol and the hostname for WEBrick or other rails server"
  :group 'rails
  :type 'string
  :tag "Rails WEBRick Default Server")

(defvar rails-webrick:buffer-name "*WEBrick*")

(defun rails-webrick:status()
  "Return t if a WEBrick process is running."
  (let ((status (get-buffer-process rails-webrick:buffer-name)))
    (if status t nil)))

(defun rails-webrick:sentinel-proc (proc msg)
  (if (memq (process-status proc) '(exit signal))
      (message
       (concat
        (if rails-webrick:use-mongrel "Mongrel" "WEBrick") " stopped"))))

(defun rails-webrick:toggle-use-mongrel()
  "Toggle  the use of Mongrel as the backend server."
  (interactive)
  (setq rails-webrick:use-mongrel (not rails-webrick:use-mongrel))
  (customize-save-variable 'rails-webrick:use-mongrel rails-webrick:use-mongrel))

(defun rails-webrick:start(&optional env)
  "Start a WEBrick process with ENV environment if ENV is not set
using `rails-webrick:default-env'."
  (interactive (list (rails-read-enviroment-name)))
  (rails-core:with-root
   (root)
   (let ((proc (get-buffer-process rails-webrick:buffer-name))
         (dir default-directory))
     (unless proc
       (progn
         (setq default-directory root)
         (unless env
           (setq env (rails-webrick:default-env)))
         (if rails-webrick:use-mongrel
             (setq proc
                   (apply 'start-process-shell-command
                          "mongrel_rails"
                          rails-webrick:buffer-name
                          "mongrel_rails"
                          (list "start" "0.0.0.0" rails-webrick:port)))
           (setq proc
                 (apply 'start-process-shell-command
                        rails-ruby-command
                        rails-webrick:buffer-name
                        rails-ruby-command
                        (list (rails-core:quoted-file "script/server")
                              (concat " -e " env)
                              (concat " -p " rails-webrick:port)))))
         (set-process-sentinel proc 'rails-webrick:sentinel-proc)
         (setq default-directory dir)
         (message (format "%s (%s) starting with port %s"
                          (if rails-webrick:use-mongrel "Mongrel" "Webrick")
                          env
                          rails-webrick:port)))))))

(defun rails-webrick:start-default-env ()
  "Start WEBrick using the default environment defined in
`rails-webrick:default-env'."
  (interactive)
  (rails-webrick:start rails-webrick:default-env))

(defun rails-webrick:stop()
  "Stop the WEBrick process."
  (interactive)
  (let ((proc (get-buffer-process rails-webrick:buffer-name)))
    (if proc
        (kill-process proc))))

;;;;;;;;;; Open browser ;;;;;;;;;;

(defun rails-webrick:open-browser (&optional address)
  "Open a browser on the main page of the current Rails project
server."
  (interactive)
  (let ((url (concat (concat rails-webrick:server-name
                             ":"
                             rails-webrick:port
                             "/"
                             address ))))
    (message "Opening browser: %s" url)
    (browse-url url)))

(defun rails-webrick:open-browser-on-controller (&optional controller action params)
  "Open browser on the controller/action/id for the current
file."
  (interactive
   (list
    (completing-read "Controller name: "
                     (list->alist (rails-core:controllers t)))
    (read-from-minibuffer "Action name: ")
    (read-from-minibuffer "Params: ")))
  (rails-core:with-root
   (root)
   (when (string-not-empty controller)
     (rails-webrick:open-browser
      (concat (rails-core:file-by-class controller t) "/"
              (if (string-not-empty action) (concat action "/")) params)))))

(defun rails-webrick:auto-open-browser (ask-parameters?)
  "Autodetect the current action and open browser on it with.
Prefix the command to ask parameters for action."
  (interactive "P")
  (rails-core:with-root
   (root)
   (if (find (rails-core:buffer-type) '(:view :controller))
       (when-bind (controller (rails-core:current-controller))
                  (rails-webrick:open-browser-on-controller
                   controller (rails-core:current-action)
                   (when ask-parameters?
                     (read-from-minibuffer "Parameters: "))))
     (message "You can auto-open browser only in view or controller"))))

(provide 'rails-webrick)
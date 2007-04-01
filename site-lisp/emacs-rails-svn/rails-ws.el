;;; rails-ws.el --- functions for manadge application server

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-ws.el $
;; $Id: rails-ws.el 150 2007-03-29 20:48:17Z dimaexe $

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

(defcustom rails-ws:port "3000"
  "Default web server port"
  :group 'rails
  :type 'string
  :tag "Rails Server Port")

(defcustom rails-ws:server-name "http://localhost"
  "Protocol and the hostname for web server or other rails server"
  :group 'rails
  :type 'string
  :tag "Rails Server Default")

(defcustom rails-ws:default-server-type "mongrel"
  "Web server to run Rails application."
  :group 'rails
  :type 'string
  :tag "Rails Server Type")

(defvar rails-ws:available-servers-list (list "mongrel" "lighttpd" "webrick"))
(defvar rails-ws:buffer-name "*RWebServer*")
(defvar rails-ws:process-environment nil)

(defun rails-ws:default-server-type-p (type)
  (string= type rails-ws:default-server-type))

(defun rails-ws:switch-default-server-type (type)
  "Switch default server type to run."
  (interactive (list (completing-read "Server type (use autocomplete): "
                                      rails-ws:available-servers-list
                                      nil t
                                      rails-ws:default-server-type)))
  (setq rails-ws:default-server-type type)
  (customize-save-variable 'rails-ws:default-server-type rails-ws:default-server-type)
  (message (concat "Switching to " (upcase type) " as default server type")))

(defun rails-ws:running-p ()
  "Return t if a WebServer process is running."
  (if (get-buffer-process rails-ws:buffer-name) t nil))

(defun rails-ws:sentinel-proc (proc msg)
  (let ((env rails-ws:process-environment))
    (when (memq (process-status proc) '(exit signal))
      (setq rails-ws:process-environment nil)
      (setq msg (format "stopped (%s)" msg)))
  (message
   (replace-regexp-in-string "\n" ""
                             (format "%s - %s"
                                     (capitalize rails-ws:default-server-type)
                                     msg)))))

(defun rails-ws:start(&optional env)
  "Start a server process with ENV environment if ENV is not set
using `rails-default-environment'."
  (interactive (list (rails-read-enviroment-name)))
  (rails-project:with-root
   (root)
   (let ((proc (get-buffer-process rails-ws:buffer-name)))
     (if proc
         (message "Only one instance rails-ws allowed")
       (let* ((default-directory root)
              (env (if env env rails-default-environment))
              (proc
               (rails-cmd-proxy:start-process rails-ruby-command
                                              rails-ws:buffer-name
                                              rails-ruby-command
                                              (format "script/server %s -p %s -e %s"
                                                      rails-ws:default-server-type
                                                      rails-ws:port env))))
           (set-process-sentinel proc 'rails-ws:sentinel-proc)
           (setq rails-ws:process-environment env)
           (message (format "%s (%s) starting with port %s"
                            (capitalize rails-ws:default-server-type)
                            env
                            rails-ws:port)))))))

(defun rails-ws:stop ()
  "Stop the WebServer process."
  (interactive)
  (let ((proc (get-buffer-process rails-ws:buffer-name)))
    (when proc (kill-process proc t))))


(defun rails-ws:start-default ()
  "Start WebServer using the default environment defined in
`rails-default-environment'."
  (interactive)
  (rails-ws:start rails-default-environment))

(defun rails-ws:start-development ()
  (interactive)
  (rails-ws:start "development"))

(defun rails-ws:start-production ()
  (interactive)
  (rails-ws:start "production"))

(defun rails-ws:start-test ()
  (interactive)
  (rails-ws:start "test"))

(defun rails-ws:toggle-start-stop ()
  "Toggle Rails WebServer start/stop with default environment."
  (interactive)
  (if (rails-ws:running-p)
      (rails-ws:stop)
    (rails-ws:start-default)))

(defun rails-ws:print-status ()
  (interactive)
  (message
   (concat rails-ws:default-server-type
           " (" (if rails-ws:process-environment
                    rails-ws:process-environment
                  rails-default-environment) ")"
           " is "
           (if (rails-ws:running-p)
               (concat "running on port " rails-ws:port)
             "stopped"))))

;;;;;;;;;; Open browser ;;;;;;;;;;

(defun rails-ws:open-browser (&optional address)
  "Open a browser on the main page of the current Rails project
server."
  (interactive)
  (let ((url (concat (concat rails-ws:server-name
                             ":"
                             rails-ws:port
                             "/"
                             address ))))
    (message "Opening browser: %s" url)
    (browse-url url)))

(defun rails-ws:open-browser-on-controller (&optional controller action params)
  "Open browser on the controller/action/id for the current
file."
  (interactive
   (list
    (completing-read "Controller name: "
                     (list->alist (rails-core:controllers t)))
    (read-from-minibuffer "Action name: ")
    (read-from-minibuffer "Params: ")))
  (when (string-not-empty controller)
    (rails-ws:open-browser
     (concat (rails-core:file-by-class controller t) "/"
             (if (string-not-empty action) (concat action "/")) params))))

(defun rails-ws:auto-open-browser (ask-parameters?)
  "Autodetect the current action and open browser on it with.
Prefix the command to ask parameters for action."
  (interactive "P")
  (rails-project:with-root
   (root)
   (if (find (rails-core:buffer-type) '(:view :controller))
       (when-bind (controller (rails-core:current-controller))
                  (rails-ws:open-browser-on-controller
                   controller (rails-core:current-action)
                   (when ask-parameters?
                     (read-from-minibuffer "Parameters: "))))
     (message "You can auto-open browser only in view or controller"))))

(provide 'rails-ws)
;;; rails-cmd-proxy.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-cmd-proxy.el $
;; $Id: rails-cmd-proxy.el 118 2007-03-26 12:59:43Z dimaexe $

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

(defvar rails-cmd-proxy:directories-list
  (list
   (list "^y:" "/mnt/www" "-t @server-cmd")))

(defvar rails-cmd-proxy:remote-cmd
  "plink")

(defun rails-cmd-proxy:get-remote ()
  (rails-core:with-root
   (root)
   (loop for (local remote args) in rails-cmd-proxy:directories-list
         when (string-match local root)
         do (return
             (list (replace-regexp-in-string local remote root)
                   args)))))

(defun rails-cmd-proxy:apply-remote (path command &optional command-args)
  (if command-args
      (format "\"cd %s && %s %s\"" path command command-args)
    (format "\"cd %s && %s\"" path command)))

;; remote wrappers

(defun rails-cmd-proxy:start-process (name buffer command command-args)
  (let ((remote (rails-cmd-proxy:get-remote)))
    (if remote
        (let* ((remote-path (car remote))
               (remote-args (car (cdr remote)))
               (remote-cmd-args (format "%s %s"
                                        remote-args
                                        (rails-cmd-proxy:apply-remote remote-path command command-args))))
           (start-process-shell-command name
                                        buffer
                                        rails-cmd-proxy:remote-cmd
                                        remote-cmd-args))
      (start-process-shell-command name
                                   buffer
                                   command
                                   command-args))))

(defun rails-cmd-proxy:shell-command-to-string (command)
  (let ((remote (rails-cmd-proxy:get-remote)))
    (if remote
        (let* ((remote-path (car remote))
               (remote-args (car (cdr remote)))
               (command (format "%s %s %s"
                                rails-cmd-proxy:remote-cmd
                                remote-args
                                (rails-cmd-proxy:apply-remote remote-path command))))
          (shell-command-to-string command))
      (shell-command-to-string command))))

(provide 'rails-cmd-proxy)
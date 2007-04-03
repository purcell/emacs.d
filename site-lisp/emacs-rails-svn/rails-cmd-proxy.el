;;; rails-cmd-proxy.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-cmd-proxy.el $
;; $Id: rails-cmd-proxy.el 158 2007-04-03 08:45:46Z dimaexe $

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

(defstruct rails-cmd-proxy:struct local remote args)

(defvar rails-cmd-proxy:directories-list
  '(("y:" "/mnt/www" "-t @server-cmd")))

(defvar rails-cmd-proxy:remote-cmd
  "plink")

(defun rails-cmd-proxy:lookup (root &optional lookup-local)
  "Lookup ROOT using `rails-cmd-proxy:directories-list' and
return the `rails-cmd-proxy:struct'. If not found ROOT return
nil."
  (loop for (local remote args) in rails-cmd-proxy:directories-list
        when (string-match (concat "^" (if lookup-local remote local)) root)
        do (return
            (make-rails-cmd-proxy:struct
             :local local
             :remote remote
             :args args))))

(defun rails-cmd-proxy:convert (proxy-struct path &optional reverse)
  "Convert PATH from local to remote using PROXY-STRUCT,
otherwise if set REVERSE convert from remote to local."
  (let* ((local (rails-cmd-proxy:struct-local proxy-struct))
         (remote (rails-cmd-proxy:struct-remote proxy-struct))
         (regexp (concat "^" (if reverse remote local)))
         (replacement (if reverse local remote)))
    (when (string-match regexp path)
      (replace-regexp-in-string regexp replacement path))))

(defun rails-cmd-proxy:construct-remote-cmd (proxy-struct root command &optional command-args)
  (let ((root (rails-cmd-proxy:convert proxy-struct root))
        (args (rails-cmd-proxy:struct-args proxy-struct)))
    (if command-args
        (format "%s \"cd %s && %s %s\"" args root command command-args)
      (format "%s \"cd %s && %s\"" args root command))))

;; remote wrappers

(defun rails-cmd-proxy:start-process (name buffer command command-args)
  ""
  (rails-project:with-root
   (root)
   (let ((proxy-struct (rails-cmd-proxy:lookup root))
         (command command)
         (command-args command-args))
     (when proxy-struct
       (setq command-args
             (rails-cmd-proxy:construct-remote-cmd proxy-struct
                                                   root
                                                   command
                                                   command-args))
       (setq command rails-cmd-proxy:remote-cmd))
     (start-process-shell-command name
                                  buffer
                                  command
                                  command-args))))

(defun rails-cmd-proxy:shell-command-to-string (command)
  (rails-project:with-root
   (root)
   (let ((proxy-struct (rails-cmd-proxy:lookup root))
         (command command))
     (when proxy-struct
       (setq command
             (format "%s %s"
                     rails-cmd-proxy:remote-cmd
                     (rails-cmd-proxy:construct-remote-cmd proxy-struct
                                                           root
                                                           command))))
     (shell-command-to-string command))))

;; helper functions

(defun rails-cmd-proxy:convert-buffer-from-remote (start end len)
  (when-bind
   (struct (rails-cmd-proxy:lookup default-directory))
   (save-excursion
     (goto-char start)
     (let* ((local (rails-cmd-proxy:struct-local struct))
            (remote (rails-cmd-proxy:struct-remote struct))
            (root default-directory)
            (remote-with-root (concat remote (substring root (length local))))
            (buffer-read-only nil)
            point)
       (while (setq point (re-search-forward (format "^\\s-*\\(%s\\)"
                                                     remote-with-root) end t))
         (replace-match (format "%s "
                                (string-repeat " " (- (length (match-string 1)) 1)))
                        nil t nil 1))))))

(provide 'rails-cmd-proxy)

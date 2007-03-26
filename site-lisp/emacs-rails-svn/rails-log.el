;;; rails-log.el --- provide features for Rails log files

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-log.el $
;; $Id: rails-log.el 114 2007-03-25 18:15:35Z dimaexe $

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

(defvar rails-log:last-log nil)

(defun rails-log:files ()
  (directory-files (rails-core:file "log") nil "\\.log$"))

(defun rails-log:buffer-name (log-file)
  (concat "*" log-file "*"))

(defun rails-log:open-file (log-file)
  (let ((buffer (rails-log:buffer-name log-file))
        (current (buffer-name)))
    (unless (get-buffer buffer)
      (get-buffer-create buffer)
      (set-buffer buffer)
      (setq auto-window-vscroll t)
      (rails-minor-mode t)
      (setq buffer-read-only t)
      (set-buffer current)
      (apply-colorize-to-buffer buffer))
    (start-process "tail"
                   buffer
                   "tail"
                   "-f" (rails-core:file (concat "log/" log-file)))))

(defun rails-log:open (log-file)
  (interactive
   (list (completing-read "Select log (with autocomplete): "
                          (list->alist (rails-log:files))
                          nil
                          t
                          rails-log:last-log)))
  (setq rails-log:last-log log-file)
  (let ((name (rails-log:buffer-name log-file)))
    (unless (get-buffer name)
      (rails-log:open-file log-file))
    (switch-to-buffer name)
    (recenter t)))

(defun rails-log:open-production ()
  (interactive)
  (rails-log:open "production.log"))

(defun rails-log:open-development ()
  (interactive)
  (rails-log:open "development.log"))

(defun rails-log:open-test ()
  (interactive)
  (rails-log:open "test.log"))

(provide 'rails-log)
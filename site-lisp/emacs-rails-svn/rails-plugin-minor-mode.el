;;; rails-plugin-minor-mode.el --- minor mode for RubyOnRails plugins

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-plugin-minor-mode.el $
;; $Id: rails-plugin-minor-mode.el 112 2007-03-24 22:34:38Z dimaexe $

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

(defun rails-plugin-minor-mode:switch-to-init ()
  (interactive)
  (rails-core:find-file-if-exist
   (rails-core:plugin-file (rails-core:current-plugin) "init.rb")))

(defun rails-plugin-minor-mode:switch-with-menu ()
  (interactive)
  (let* ((item)
         (plugin (rails-core:current-plugin))
         (menu (rails-core:plugin-files plugin)))
    (setq item
          (rails-core:menu
           (list (concat "Plugin " plugin)
                 (cons "Please select.." (list->alist menu)))))
    (when item
      (rails-core:find-file-if-exist
       (rails-core:plugin-file plugin item)))))

(define-minor-mode rails-plugin-minor-mode
  "Minor mode for RubyOnRails plugins."
  nil
  " plugin"
  nil
  (setq rails-primary-switch-func 'rails-plugin-minor-mode:switch-to-init)
  (setq rails-secondary-switch-func 'rails-plugin-minor-mode:switch-with-menu))

(provide 'rails-plugin-minor-mode)
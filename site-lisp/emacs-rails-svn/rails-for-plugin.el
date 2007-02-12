;;; rails-for-plugin.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-for-rhtml.el $
;; $Id: rails-for-rhtml.el 58 2006-12-17 21:47:39Z dimaexe $

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

(defun rails-plugin:switch-to-init ()
  (interactive)
  (rails-core:find-file-if-exist
   (rails-core:plugin-file (rails-core:current-plugin) "init.rb")))

(defun rails-plugin:switch-with-menu ()
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

(defun rails-for-plugin ()
  "Enable Rails Plugins Configurations."
  (interactive)
  (setq rails-primary-switch-func 'rails-plugin:switch-to-init)
  (setq rails-secondary-switch-func 'rails-plugin:switch-with-menu))

(provide 'rails-for-plugin)
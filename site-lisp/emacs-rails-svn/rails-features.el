;;; rails-features.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-ruby.el $
;; $Id: rails-ruby.el 166 2007-04-05 17:44:57Z dimaexe $

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

(defvar rails-features:list
  '(rails-snippets-feature
    rails-speedbar-feature
    rails-rspec-feature)
  "List of features")

(defvar rails-features:installed-p nil)

(defun rails-features:install ()
  (unless rails-features:installed-p
    (dolist (feature rails-features:list)
      (when (require feature nil t)
        (apply
         (intern (concat (symbol-name feature) ":install"))
         (list))))
    (setq rails-features:installed-p t)))

(provide 'rails-features)
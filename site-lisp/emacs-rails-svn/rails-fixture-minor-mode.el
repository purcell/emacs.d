;;; rails-fixture-minor-mode.el --- minor mode for RubyOnRails fixtures

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-fixture-minor-mode.el $
;; $Id: rails-fixture-minor-mode.el 158 2007-04-03 08:45:46Z dimaexe $

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

(define-minor-mode rails-fixture-minor-mode
  "Minor mode for RubyOnRails fixtures."
  :lighter " Fixture"
  :keymap (rails-model-layout:keymap :fixture)
  (setq rails-primary-switch-func 'rails-model-layout:switch-to-unit-test)
  (setq rails-secondary-switch-func 'rails-model-layout:menu))

(provide 'rails-fixture-minor-mode)
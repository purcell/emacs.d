;;; rails-functional-test-minor-mode.el --- minor mode for RubyOnRails functional tests

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-functional-test-minor-mode.el $
;; $Id: rails-functional-test-minor-mode.el 133 2007-03-27 14:59:21Z dimaexe $

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

(define-minor-mode rails-functional-test-minor-mode
  "Minor mode for RubyOnRails functional tests."
  nil
  " func-test"
  nil
  (setq rails-primary-switch-func (lambda() (interactive) (rails-controller-layout:switch-to :controller)))
  (setq rails-secondary-switch-func 'rails-controller-layout:menu)
  (local-set-key (kbd "\C-c .") 'rails-rake:test-current-method))

(provide 'rails-functional-test-minor-mode)
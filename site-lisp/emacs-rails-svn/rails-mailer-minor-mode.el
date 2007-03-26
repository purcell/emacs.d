;;; rails-mailer-minor-mode.el --- minor mode for RubyOnRails mailers

;; Copyright (C) 2006-2007 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-mailer-minor-mode.el $
;; $Id: rails-mailer-minor-mode.el 111 2007-03-24 22:28:12Z dimaexe $

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

(define-minor-mode rails-mailer-minor-mode
  "Minor mode for RubyOnRails mailers."
  nil
  " mailer"
  nil
  (setq rails-secondary-switch-func 'rails-controller-layout:menu)
  (setq rails-primary-switch-func 'rails-controller-layout:toggle-action-view))

(provide 'rails-mailer-minor-mode)

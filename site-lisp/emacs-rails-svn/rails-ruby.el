;;; rails-ruby.el --- provide features for ruby-mode

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-ruby.el $
;; $Id: rails-ruby.el 52 2006-10-02 20:39:06Z dimaexe $

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

(defun ruby-indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (when snippet
    (snippet-next-field))
  (if (looking-at "\\>")
      (hippie-expand nil)
    (ruby-indent-command)))

(defun ruby-newline-and-indent ()
  (interactive)
  (newline)
  (ruby-indent-command))

(provide 'rails-ruby)
;;; untabify-file.el --- automatic untabify files before save

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails.el $
;; $Id: rails.el 149 2007-03-29 15:07:49Z dimaexe $

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

(require 'cl)
(require 'custom)

(defcustom untabify-exclude-list
  '(makefile-mode
    makefile-bsdmake-mode
    change-log-mode
    "Makefile$")
  "List of regexp or modes to which is not applied untabify."
  :group 'untabify)

(defun untabify-before-write ()
  "Strip all trailing whitespaces and untabify buffer before
save."
  (when (and (eq this-command 'save-buffer)
             (not (find nil
                        untabify-exclude-list
                        :if #'(lambda (r)
                                (typecase r
                                  (string (string-match r (buffer-name)))
                                  (symbol (eq major-mode r)))))))
    (save-excursion
      (untabify (point-min) (point-max))
      (delete-trailing-whitespace))))

(add-hook 'write-file-hooks 'untabify-before-write)

(provide 'untabify-file)

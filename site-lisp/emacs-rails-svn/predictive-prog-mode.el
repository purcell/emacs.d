;;; predictive-prog-mode.el ---

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

(eval-when-compile
  (require 'predictive nil t)
  (require 'completion-ui nil t))

(require 'flyspell)

(defconst predictive-prog-text-faces
  '(font-lock-comment-face font-lock-doc-face)
  "Faces corresponding to text in programming-mode buffers.")

(defvar predictive-prog-mode-main-dict nil)

(defun activate-predictive-inside-comments (start end len)
  "Looking at symbol at point and activate the `predictive-mode'
if there a string or a comment."
  (save-excursion
    (let ((p (get-text-property (- (point) 1) 'face))
          (f (get-text-property (point) 'face)))
      (if (or (memq f predictive-prog-text-faces)
              (memq p predictive-prog-text-faces))
          (setq predictive-main-dict predictive-prog-mode-main-dict)
        (setq predictive-main-dict nil)))))

(defun predictive-prog-mode ()
  "Enable the `predictive-mode' inside strings and comments
only, like `flyspell-prog-mode'."
  (interactive)
  (when (fboundp 'predictive-mode)
    (set (make-local-variable 'predictive-main-dict) nil)
    (set (make-local-variable 'predictive-prog-mode-main-dict) predictive-main-dict)
    (if (find 'activate-predictive-inside-comments after-change-functions)
        (progn
          (remove-hook 'after-change-functions 'activate-predictive-inside-comments t)
          (predictive-mode -1))
      (progn
;        (set (make-local-variable 'predictive-use-auto-learn-cache) nil)
        (set (make-local-variable 'predictive-dict-autosave-on-kill-buffer) nil)
        (predictive-mode 1)
        (add-hook 'after-change-functions 'activate-predictive-inside-comments nil t)))))

(provide 'predictive-prog-mode)

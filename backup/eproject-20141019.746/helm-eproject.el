;;; helm-eproject.el ---
;;
;; Filename: helm-eproject.el
;; Description:
;; Author: Glauber Alex Dias Prado
;; Author: Thomas Frössman
;; Maintainer:
;; Created: Ter Mar 27 20:26:06 2012 (-0300)
;; Version:
;; Last-Updated: sön sep 02 13:44:27 CEST 2012
;;           By: Thomas Frössman
;; URL: github
;; Keywords: emacs eproject helm complection
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This small utility helps eproject to show the project files under helm
;; perhaps it would be better implemented using helm completion hooks but this
;; is working pretty good for now :).
;;
;;; Install:
;;
;; Add the file somewhere to your load path like ~/elisp and add:
;; (require 'helm-eproject)
;; after you've setup both eproject and helm then just open a file that normal
;; eproject would pick up and voila :), you have to setup helm and eproject
;; for yourself so you can get the most out of these extensions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; helm support
(require 'helm-files)
(require 'cl)

(defvar helm-eproject-source
  '((name . "Eproject")
    (type . file)
    (match . helm-files-match-only-basename)
    (candidates . (lambda ()
		    (with-helm-current-buffer
		      (when (and (boundp 'eproject-mode) eproject-mode)
			(eproject-list-project-files-relative)))))
    (action . (lambda (candidate)
                (let ((candidate-abs (concat (eproject-root) candidate)))
                  (find-file candidate-abs))))))

(defun helm-eproject ()
  "helps helm to use eproject to find a file"
  (interactive)
  (helm :sources '(helm-eproject-source)
        :buffer "*Helm Eproject*"))

(defun eproject-helm-configure ()
  "Bind C-x f to `helm-eproject'."
  (global-set-key [(control x) (f)] 'helm-eproject))

(provide 'helm-eproject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-eproject.el ends here

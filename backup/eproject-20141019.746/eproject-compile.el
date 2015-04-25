;;; eproject-compile.el --- eproject compilation extensions

;; Copyright (C) 2009  Jonathan Rockway
;;           (C) 2012  Alex Bennée

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: eproject

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension to eproject adds the ability to compile projects
;; using meta-data defined in your project.  For example:
;;
;; (define-project-type rockbox
;;   (generic-git)
;;   (look-for "../rockbox.git/rbutil")
;;   :common-compiles ("make" "make install" "make fullzip"))
;;
;; Now when you you call eproject-compile you will be presented with
;; a prompt with history pre-filled with your favourite compile sequences.


;;; History:
;;
;; 2012-05-16: `eproject-compile' moved from eproject-extras to this
;; file.  Extended by Alex Bennée to populate the history
;; intelligently.

;;; Code:

(require 'eproject)
(require 'compile)

(defun* eproject--build-new-history (&optional (buffer (current-buffer)))
  "Return a list of compile commands suitable for use as a compile history."
  (eproject--do-in-buffer
   (buffer)
   (let ((potential-compiles (eproject-attribute :common-compiles))
	 (new-compile-history (list ())))
     (if potential-compiles
	 (mapcar
	  '(lambda (c)
	     (format "cd %s && %s" (eproject-root) c))
	  potential-compiles)
       (list (format "cd %s && make -k" (eproject-root)))))))

;;;###autoload
(defun eproject-compile ()
  "Run `compile' in the project root.

This uses a computed history based on project attributes, the
existing `compile-history', and `compile-command' which may have
been locally set by a mode.

To provide defaults for a project or project type, set the
`:common-compiles' attribute to a list of strings representing
the command to invoke."
  (interactive)
  (let* ((default-directory (eproject-root))
	 (ehistory (append (eproject--build-new-history) compile-history))
	 (ecompile (read-shell-command
		    "Compile command: " compile-command 'ehistory)))
    (compile ecompile)))

(defun eproject-compile-repeat ()
  "Run 'compile' in the project root, using most recent command
in compile-command."
  (interactive)
  (let* ((default-directory (eproject-root)))
    (compile compile-command)))

(define-key eproject-mode-map (kbd "C-c C-k") #'eproject-compile)

(provide 'eproject-compile)
;;; eproject-compile.el ends here

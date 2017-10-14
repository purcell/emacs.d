;;; docker-utils.el --- Random utilities

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'magit-popup)

(defun docker-utils-get-marked-items ()
  "Get the marked items data from `tabulated-list-entries'."
  (save-excursion
    (goto-char (point-min))
    (let ((selection ()))
      (while (not (eobp))
        (when (not (null (tablist-get-mark-state)))
          (add-to-list 'selection (cons (tabulated-list-get-id) (tabulated-list-get-entry)) t))
        (forward-line))
      selection)))

(defun docker-utils-get-marked-items-ids ()
  "Get the id part of `docker-utils-get-marked-items'."
  (-map #'car (docker-utils-get-marked-items)))

(defmacro docker-utils-define-popup (name doc &rest args)
  "Wrapper around `docker-utils-define-popup'."
  `(progn
     (magit-define-popup ,name ,doc ,@args)
     (add-function :before (symbol-function ',name) #'docker-utils-select-if-empty)))

(defun docker-utils-select-if-empty (&optional arg)
  "Select current row is selection is empty."
  (save-excursion
    (when (null (docker-utils-get-marked-items))
      (tablist-put-mark))))

(put 'docker-utils-define-popup 'lisp-indent-function 'defun)

(defun docker-utils-pop-to-buffer (name)
  "Like `pop-to-buffer', but suffix NAME with the host if on a remote host."
  (pop-to-buffer
   (if (file-remote-p default-directory)
       (with-parsed-tramp-file-name default-directory nil (concat name " - " host))
     name)))

(defmacro docker-utils-with-result-buffer (&rest body)
  `(let ((buffer (get-buffer-create "*docker result*")))
     (with-current-buffer buffer
       (setq buffer-read-only nil)
       (erase-buffer)
       ,@body
       (setq buffer-read-only t))
     (display-buffer buffer)))

(defun docker-utils-run-command-on-selection-print (cmd &optional post-process)
  "Run COMMAND on the selections and show the result in BUFFER-NAME.
Optionally run POST-PROCESS in BUFFER-NAME."
  (let* ((id-list (docker-utils-get-marked-items-ids))
         (results (mapcar cmd id-list)))
    (docker-utils-with-result-buffer
     (mapc 'insert results)
     (when post-process
       (funcall post-process)))))

(provide 'docker-utils)

;;; docker-utils.el ends here

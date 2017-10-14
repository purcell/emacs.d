;;; historian.el --- Persistently store selected minibuffer candidates -*- lexical-binding: t -*-

;; Copyright (C) 2017 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience
;; Package-Version: 20170722.1714
;; Version: 20170111
;; URL: https://github.com/PythonNut/historian.el
;; Package-Requires: ((emacs "24.4"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Historian.el stores the results of completing-read and similar
;; functions persistently. This provides a way to give completion
;; candidates that are more frequently or more recently used a better
;; position in the candidates list.

;;; Code:

(defgroup historian nil
  "Persistently store selected minibuffer candidates"
  :group 'convenience
  :prefix "historian-")

(defcustom historian-history-length 10
  "Determines how many recently selected candidates Historian should remember."
  :type 'number
  :group 'historian)

(defcustom historian-save-file (locate-user-emacs-file ".historian")
  "File in which Historian saves its state between Emacs sessions."
  :type 'file
  :group 'historian)

(defcustom historian-excluded-commands '(swiper)
  "Any commands in this list will be ignored by historian."
  :type '(repeat symbol)
  :group 'historian)

(defvar historian--history-table (make-hash-table))

(defun historian-push-item (key value)
  (prog1 value
    (unless (member key historian-excluded-commands)
      (puthash key
               (let ((old-history
                      (gethash key
                               historian--history-table
                               (cons (list)
                                     (make-hash-table :test #'equal))))
                     (new-value (substring-no-properties value)))
                 (push new-value (car old-history))
                 (when (> (length (car old-history))
                          historian-history-length)
                   (setcar old-history
                           (let (res)
                             (dotimes (_ historian-history-length res)
                               (push (pop (car old-history)) res)))))
                 (puthash new-value
                          (1+ (gethash new-value
                                       (cdr old-history)
                                       0))
                          (cdr old-history))
                 old-history)
               historian--history-table))))

(defun historian--nadvice/completing-read (return)
  (historian-push-item last-command return))

;;;###autoload
(defun historian-save ()
  (interactive)
  (with-temp-file historian-save-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 historian--history-table (current-buffer)))))

;;;###autoload
(defun historian-load ()
  (interactive)
  (setq historian--history-table
        (if (file-exists-p historian-save-file)
            (with-temp-buffer
              (insert-file-contents historian-save-file)
              (read (current-buffer)))
          (make-hash-table))))

;;;###autoload
(defun historian-clear ()
  (interactive)
  (setq historian--history-table (make-hash-table))
  (delete-file historian-save-file t)
  (with-temp-file historian-save-file
    (prin1 historian--history-table (current-buffer))))

;;;###autoload
(define-minor-mode historian-mode
  "historian minor mode"
  :init-value nil
  :group 'historian
  :global t
  (if historian-mode
      (progn
        (historian-load)

        (advice-add 'completing-read :filter-return
                    #'historian--nadvice/completing-read)

        (add-hook 'kill-emacs-hook #'historian-save))

    (historian-save)

    (advice-remove 'completing-read #'historian--nadvice/completing-read)

    (remove-hook 'kill-emacs-hook #'historian-save)))

(provide 'historian)

;;; historian.el ends here

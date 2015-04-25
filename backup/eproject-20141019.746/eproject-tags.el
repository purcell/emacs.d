;;; eproject-tags.el --- visit project-specific tags table and keep it up to date

;; Copyright (C) 2011  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: convenience, programming, tags

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

;;

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'eproject)
(require 'etags)

(defvar eproject-tags-etags "etags"
  "The command you want to run to generate the tags file.

It must accept a list of relative filenames and send its output
to a file named TAGS.")

(defvar eproject-tags-verbose t
  "Set to T if you want the verbose output of etags to stick around.

Only works with exuberant-ctags; this will cause standard etags to blow up in a giant ball of fire.")

(defvar eproject-tags-callback nil)
(defvar eproject-tags-state nil)
(make-variable-buffer-local 'eproject-tags-callback)
(make-variable-buffer-local 'eproject-tags-state)

(defun eproject-tags--buffer (root)
  (let* ((name (concat (eproject-attribute :name root) "-TAGS"))
         (buf  (get-buffer-create (format "*%s*" name))))
    buf))

(defun eproject-tags--debug-message (root format &rest rest)
  (when eproject-tags-verbose
    (with-current-buffer (eproject-tags--buffer root)
      (goto-char (point-max))
      (insert (apply #'format (concat "** " format) rest))
      (insert "\n"))))

(defun eproject-tags--generate (cb &optional state root)
  "Generate a tags table for this project (or the project in ROOT), calling CB with the project root and STATE upon completion.

All project-relevant files are considered and output goes to root/TAGS.  The tags table is not visited after generation."
  (let* ((root  (or root (eproject-root)))
         (default-directory root)
         (files (eproject-list-project-files-relative root))
         (name  (concat (eproject-attribute :name root) "-TAGS"))
         (buf   (eproject-tags--buffer root))
         (args  (append '("-o" ".TAGS-tmp")
                        (if eproject-tags-verbose '("--verbose"))))
         (proc  (apply #'start-process name buf eproject-tags-etags
                       (append args files))))

    (with-current-buffer buf
      (setq eproject-tags-callback cb)
      (setq eproject-tags-state state)
      (setq eproject-root root))

    (set-process-sentinel proc #'eproject-tags--sentinel))
  nil)

(defun eproject-tags--sentinel (process event)
  (with-current-buffer (process-buffer process)
    (let ((default-directory eproject-root))
      (when (and eproject-tags-callback (equal event "finished\n"))
        (rename-file ".TAGS-tmp" "TAGS" t)
        (apply eproject-tags-callback eproject-tags-state)
        (delete-process process)
        (when (not eproject-tags-verbose)
          (kill-buffer))))))

;;; TODO: un-visit this table when all project buffers go away?
;;; TODO: make the tags table "project-local"

(defun eproject-tags (&optional root)
  "Update (or generate) and visit a tags table for the project in ROOT."
  (interactive)
  (let ((root (or root (eproject-root))))
    (eproject-tags--generate
     #'eproject-tags--visit-table (list root) root)))

(defun eproject-tags--visit-table (root &rest state)
  "Callback called by eproject-tags after tags table has been created."
  (let ((table (concat (file-name-as-directory root) "TAGS"))
        (tags-revert-without-query t))
    (add-to-list 'tags-table-list table t #'equal)
    (tags-verify-table table)))

(defun eproject-tags--from-hook ()
  (let ((root (ignore-errors (eproject-root)))
        (file (ignore-errors (buffer-file-name))))
    (when (and root file
               (eproject-classify-file file root)
               (not (eproject-attribute :suppress-tags root)))
      (eproject-tags--debug-message
       root "Regenerating tags table for %s because %s changed" root file)
      (eproject-tags root))))

(add-hook 'eproject-first-buffer-hook #'eproject-tags--from-hook)
(add-hook 'eproject-project-change-hook #'eproject-tags--from-hook)

(provide 'eproject-tags)
;;; eproject-tags.el ends here

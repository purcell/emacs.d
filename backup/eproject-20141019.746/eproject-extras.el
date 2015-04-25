;;; eproject-extras.el --- various utilities that make eproject more enjoyable

;; Copyright (C) 2009  Jonathan Rockway

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

;; Some of this stuff used to be in eproject "core", but it is a bit
;; bloated, and not strictly necessary.  So now it lives here, leaving
;; the eproject core pristine and minimal.

;;; History:

;; 2012-05-16: `eproject-compile' moved to contrib/eproject-compile.el.

;;; Code:

(require 'eproject)
(require 'cl)
(require 'iswitchb)
(require 'ibuffer)
(require 'ibuf-ext)

;; support for visiting other project files
(defalias 'eproject-ifind-file 'eproject-find-file)  ;; ifind is deperecated

(defun eproject--shorten-filename (filename)
  "Shorten FILENAME in the context of the current project.
Uses the function provided by the `:file-name-map' project attribute.

The default implementation just makes the filename relative to
the project root."
  (cons (funcall (eproject-attribute :file-name-map)
                 (eproject-root)
                 (file-relative-name filename (eproject-root)))
        filename))

;;;###autoload
(defun eproject-find-file ()
  "Present the user with a list of files in the current project.
to select from, open file when selected."
  (interactive)
  (find-file (eproject--icomplete-read-with-alist
              "Project file: "
              (mapcar #'eproject--shorten-filename (eproject-list-project-files)))))

(defun eproject--completing-read (prompt choices)
  "Use completing-read to do a completing read."
  (completing-read prompt choices nil t))

(defun eproject--icompleting-read (prompt choices)
  "Use iswitchb to do a completing read."
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (unwind-protect
        (progn
          (when (not iswitchb-mode)
            (add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup))
          (iswitchb-read-buffer prompt nil t))
      (when (not iswitchb-mode)
        (remove-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)))))

(defun eproject--ido-completing-read (prompt choices)
  "Use ido to do a completing read."
  (ido-completing-read prompt choices nil t))

(defcustom eproject-completing-read-function
  #'eproject--icompleting-read
  "Ask the user select a single file from a list of files.
Used by `eproject-find-file'."
  :group 'eproject
  :type '(radio (function-item :doc "Use emacs' standard completing-read function."
                               eproject--completing-read)
                (function-item :doc "Use iswitchb's completing-read function."
                               eproject--icompleting-read)
                (function-item :doc "Use ido's completing-read function."
                               eproject--ido-completing-read)
                (function)))

(defun eproject--do-completing-read (&rest args)
  "Do a completing read with the user's favorite completing read function."
  (apply eproject-completing-read-function args))

(defun eproject--icomplete-read-with-alist (prompt alist)
  (let ((show (mapcar (lambda (x) (car x)) alist)))
    (cdr (assoc (eproject--do-completing-read prompt show) alist))))

;;;###autoload
(defun eproject--project-buffers ()
  "Return an alist mapping each project root to its open buffers.

Does not list the project if it doesn't have any buffers."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for x in
          (mapcar (lambda (b) (ignore-errors (cons (eproject-root b) b)))
                  (buffer-list))
          when (not (null x))
          do (puthash (car x)
                      (cons (cdr x) (gethash (car x) hash)) hash))
    (loop for key being the hash-keys of hash
          collect (cons key (gethash key hash)))))

(defun* eproject--get-name-root-alist (&key live-only)
  (let ((all-projects (eproject-projects))
        (buffers      (eproject--project-buffers)))

    (when (null all-projects)
      (error "No projects yet"))

    (if live-only
        (remove-if #'null (mapcar (lambda (x) (rassoc (car x) all-projects)) buffers))
      all-projects)))

(defun* eproject--read-project-name (&key live-only)
  (eproject--icomplete-read-with-alist
   "Project name: " (eproject--get-name-root-alist :live-only live-only)))

(defun* eproject--handle-root-prefix-arg (prefix &key live-only)
  (if (= prefix 4)
      (eproject--read-project-name :live-only live-only)
    (eproject-root)))

(defun eproject--generic-switch-to-buffer (prefix switch-func)
  (let* ((root (eproject--handle-root-prefix-arg prefix :live-only t))
	 (calling-buffer (current-buffer))
	 (buffers (delq nil (mapcar (lambda (buf)
				      (ignore-errors
					(when (equal root (eproject-root buf))
					  (unless (eq buf calling-buffer)
					    buf))))
				    (buffer-list))))
	 (chosen-buf (eproject--do-completing-read
		      "switch to buffer in project: " buffers)))
    (funcall switch-func chosen-buf)))

;;;###autoload
(defun eproject-switch-to-buffer (&optional prefix)
  "Interactively switch to a buffer belonging to the same project.

With prefix argument 4, first ask which project."
  (interactive "p")
  (eproject--generic-switch-to-buffer prefix #'switch-to-buffer))

;;;###autoload
(defun eproject-switch-to-buffer-other-window (&optional prefix)
  "Interactively switch the other window to a buffer belonging to the same project.

With prefix argument 4, first ask which project."
  (interactive "p")
  (eproject--generic-switch-to-buffer prefix #'switch-to-buffer-other-window))

;;;###autoload
(defun eproject-switch-to-buffer-other-frame (&optional prefix)
  "Interactively switch the other frame to a buffer belonging to the same project.

With prefix argument 4, first ask which project."
  (interactive "p")
  (eproject--generic-switch-to-buffer prefix #'switch-to-buffer-other-frame))

;; ibuffer support

(define-ibuffer-filter eproject-root
    "Filter buffers that have the provided eproject root"
  (:reader (read-directory-name "Project root: " (ignore-errors (eproject-root)))
   :description "project root")
  (with-current-buffer buf
    (equal (file-name-as-directory (expand-file-name qualifier))
           (ignore-errors (eproject-root)))))

(define-ibuffer-filter eproject
    "Filter buffers that have the provided eproject name"
  (:reader (eproject--do-completing-read "Project name: " (eproject-project-names))
   :description "project name")
  (with-current-buffer buf
    (equal qualifier
           (ignore-errors (eproject-name)))))

(define-ibuffer-column eproject (:name "Project" :inline t)
  (ignore-errors (eproject-name)))

;;;###autoload
(defun eproject-ibuffer (prefix)
  "Open an IBuffer window showing all buffers in the current project, or named project if PREFIX arg is supplied."
  (interactive "p")
  (if (= prefix 4)
      (call-interactively #'eproject--ibuffer-byname)
    (ibuffer nil "*Project Buffers*"
             (list (cons 'eproject-root (eproject-root))))))

(defun eproject--ibuffer-byname (project-name)
  "Open an IBuffer window showing all buffers in the project named PROJECT-NAME."
  (interactive (list
                (eproject--do-completing-read
                 "Project name: " (eproject-project-names))))
  (ibuffer nil (format "*%s Buffers*" project-name)
           (list (cons 'eproject project-name))))

;; extra macros

(defmacro* with-each-buffer-in-project
    ((binding &optional project-root)
     &body body)
  "Given a project root PROJECT-ROOT, finds each buffer visiting a file in that project, and executes BODY with each buffer bound to BINDING (and made current)."
  (declare (indent 2))
  `(progn
     (loop for ,binding in (cdr (assoc (or ,project-root (eproject-root))
                                           (eproject--project-buffers)))
           do
           (with-current-buffer ,binding
             ,@body))))

;; bulk management utils
;;;###autoload
(defun eproject-kill-project-buffers (prefix)
  "Kill every buffer in the current project, including the current buffer.

If PREFIX is specified, prompt for a project name and kill those
buffers instead."
  (interactive "p")
  (with-each-buffer-in-project
      (buf (eproject--handle-root-prefix-arg prefix :live-only t))
    (kill-buffer buf)))

(defun eproject-open-all-project-files (prefix)
  "Open every file in the same project.

If PREFIX arg is supplied, prompt for a project.  Otherwise,
assume the project of the current buffer."
  (interactive "p")
  (let ((total 0)
        (root (eproject--handle-root-prefix-arg prefix)))
    (message "Opening files...")
    (save-window-excursion
      (loop for file in (eproject-list-project-files root)
            do (progn (find-file file) (incf total))))
    (message "Opened %d files" total)))

;; project management

(defun eproject-project-root (project)
  "Given a PROJECT name, return the root directory."
  (let ((projects (eproject--get-name-root-alist)))
    (cdr (assoc project projects))))

;;;###autoload
(defun eproject-revisit-project (prefix)
  "Given a project name, visit the root directory.

If PREFIX arg is supplied, run `eproject-find-file'."
  (interactive "p")
  (let ((eproject-root (eproject--read-project-name))
        (eproject-mode t)) ;; XXX: very messy, needs rewrite
    (if (= prefix 4)
          (eproject-find-file)
      (find-file eproject-root))))

;; grep project files (contributed by Julian Snitow)

;; TODO: make the grep command customizable; to use "Ack", for example
;;;###autoload
(defun eproject-grep (regexp)
  "Search all files in the current project for REGEXP."
  (interactive "sRegexp grep: ")
  (let* ((root (eproject-root))
         (default-directory root)
         (files (eproject-list-project-files-relative root)))
    (grep-compute-defaults)
    (lgrep regexp (combine-and-quote-strings files) root)))

(defcustom eproject-todo-expressions
  '("TODO" "XXX" "FIXME")
  "A list of tags for `eproject-todo' to search for when generating the project's TODO list."
  :group 'eproject
  :type '(repeat string))

;;;###autoload
(defun eproject-todo ()
  "Display a project TODO list.

Customize `eproject-todo-expressions' to control what this function looks for."
  (interactive)
  ;; TODO: display output in a buffer called *<project>-TODO* instead of *grep*.
  (eproject-grep (regexp-opt eproject-todo-expressions)))

;;;###autoload
(defun eproject-multi-isearch-buffers ()
  "Do a `multi-isearch' on opened buffers in the current project.

Run `eproject-open-all-project-files' first or just
`eproject-grep' if you want to search all project files."
  (interactive)
  (multi-isearch-buffers
   (cdr (assoc (eproject-root) (eproject--project-buffers)))))

;;;###autoload
(defun eproject-eshell-cd-here (&optional look-in-invisible-buffers)
  "If there is an EShell buffer, cd to the project root in that buffer.

With the prefix arg LOOK-IN-INVISIBLE-BUFFERS looks in buffers that are not currently displayed."
  (interactive "p")
  (setq look-in-invisible-buffers (cond ((= look-in-invisible-buffers 4) t)))
  (let* ((root (eproject-root))
         (eshell-p (lambda (buf)
                     (with-current-buffer buf (eq major-mode 'eshell-mode))))
         (eshell-buffer (find-if eshell-p
                                 (if look-in-invisible-buffers
                                     (buffer-list)
                                   (mapcar (lambda (w) (window-buffer w))
                                           (window-list))))))

    (cond ((and (not eshell-buffer) look-in-invisible-buffers)
           (error "No EShell buffer!"))
          ((and (not eshell-buffer) (not look-in-invisible-buffers))
           (error "No visible EShell buffer; try re-running with the prefix arg"))
          (eshell-buffer
           (with-current-buffer eshell-buffer
             (goto-char (point-max))
             (eshell/cd root)
             (eshell-send-input nil t)
             eshell-buffer))))) ;; returns eshell-buf so you can focus
                                ;; the window if you want

;;;###autoload
(defun eproject-current-working-directory ()
  "Return the project root directory for most recently visited
buffer.  Fallback to the directory of the buffer when it is
not in a project."
  (let ((current-buffer (car (frame-parameter nil 'buffer-list))))
    (or (ignore-errors (eproject-root current-buffer))
        (with-current-buffer current-buffer
          (let ((filename (buffer-file-name)))
            (if filename
                (file-name-directory filename)
              default-directory))))))

(define-key eproject-mode-map (read-kbd-macro (concat eproject-keybind-prefix " C-f")) #'eproject-find-file)
(define-key eproject-mode-map (read-kbd-macro (concat eproject-keybind-prefix " C-b")) #'eproject-ibuffer)
(define-key eproject-mode-map (read-kbd-macro (concat eproject-keybind-prefix " b")) #'eproject-switch-to-buffer)
(define-key eproject-mode-map (read-kbd-macro (concat eproject-keybind-prefix " 4 b")) #'eproject-switch-to-buffer-other-window)
(define-key eproject-mode-map (read-kbd-macro (concat eproject-keybind-prefix " 5 b")) #'eproject-switch-to-buffer-other-frame)

(provide 'eproject-extras)
;;; eproject-extras.el ends here

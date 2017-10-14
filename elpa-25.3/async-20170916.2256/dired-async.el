;;; dired-async.el --- Asynchronous dired actions -*- lexical-binding: t -*-

;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

;; Authors: John Wiegley <jwiegley@gmail.com>
;;          Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Keywords: dired async network
;; X-URL: https://github.com/jwiegley/dired-async

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provide a redefinition of `dired-create-file' function,
;; performs copies, moves and all what is handled by `dired-create-file'
;; in the background using a slave Emacs process,
;; by means of the async.el module.
;; To use it, put this in your .emacs:

;;     (dired-async-mode 1)

;; This will enable async copy/rename etc...
;; in dired and helm.

;;; Code:

(require 'cl-lib)
(require 'dired-aux)
(require 'async)

(eval-when-compile
  (defvar async-callback))

(defgroup dired-async nil
  "Copy rename files asynchronously from dired."
  :group 'dired)

(defcustom dired-async-env-variables-regexp
  "\\`\\(tramp-\\(default\\|connection\\|remote\\)\\|ange-ftp\\)-.*"
  "Variables matching this regexp will be loaded on Child Emacs."
  :type  'regexp
  :group 'dired-async)

(defcustom dired-async-message-function 'dired-async-mode-line-message
  "Function to use to notify result when operation finish.
Should take same args as `message'."
  :group 'dired-async
  :type  'function)

(defcustom dired-async-log-file "/tmp/dired-async.log"
  "File use to communicate errors from Child Emacs to host Emacs."
  :group 'dired-async
  :type 'string)

(defcustom dired-async-mode-lighter '(:eval
                                      (when (eq major-mode 'dired-mode)
                                        " Async"))
  "Mode line lighter used for `dired-async-mode'."
  :group 'dired-async
  :risky t
  :type 'sexp)

(defface dired-async-message
    '((t (:foreground "yellow")))
  "Face used for mode-line message."
  :group 'dired-async)

(defface dired-async-failures
    '((t (:foreground "red")))
  "Face used for mode-line message."
  :group 'dired-async)

(defface dired-async-mode-message
    '((t (:foreground "Gold")))
  "Face used for `dired-async--modeline-mode' lighter."
  :group 'dired-async)

(define-minor-mode dired-async--modeline-mode
    "Notify mode-line that an async process run."
  :group 'dired-async
  :global t
  :lighter (:eval (propertize (format " [%s Async job(s) running]"
                                      (length (dired-async-processes)))
                              'face 'dired-async-mode-message))
  (unless dired-async--modeline-mode
    (let ((visible-bell t)) (ding))))

(defun dired-async-mode-line-message (text face &rest args)
  "Notify end of operation in `mode-line'."
  (message nil)
  (let ((mode-line-format (concat
                           " " (propertize
                                (if args
                                    (apply #'format text args)
                                    text)
                                'face face))))
    (force-mode-line-update)
    (sit-for 3)
    (force-mode-line-update)))

(defun dired-async-processes ()
  (cl-loop for p in (process-list)
           when (cl-loop for c in (process-command p) thereis
                         (string= "async-batch-invoke" c))
           collect p))

(defun dired-async-kill-process ()
  (interactive)
  (let* ((processes (dired-async-processes))
         (proc (car (last processes))))
    (and proc (delete-process proc))
    (unless (> (length processes) 1)
      (dired-async--modeline-mode -1))))

(defun dired-async-after-file-create (total operation failures skipped)
  "Callback function used for operation handled by `dired-create-file'."
  (unless (dired-async-processes)
    ;; Turn off mode-line notification
    ;; only when last process end.
    (dired-async--modeline-mode -1))
  (when operation
    (if (file-exists-p dired-async-log-file)
        (progn
          (pop-to-buffer (get-buffer-create dired-log-buffer))
          (goto-char (point-max))
          (setq inhibit-read-only t)
          (insert "Error: ")
          (insert-file-contents dired-async-log-file)
          (special-mode)
          (shrink-window-if-larger-than-buffer)
          (delete-file dired-async-log-file))
        (run-with-timer
         0.1 nil
         (lambda ()
           ;; First send error messages.
           (cond (failures
                  (funcall dired-async-message-function
                           "%s failed for %d of %d file%s -- See *Dired log* buffer"
                           'dired-async-failures
                           (car operation) (length failures)
                           total (dired-plural-s total)))
                 (skipped
                  (funcall dired-async-message-function
                           "%s: %d of %d file%s skipped -- See *Dired log* buffer"
                           'dired-async-failures
                           (car operation) (length skipped) total
                           (dired-plural-s total))))
           ;; Finally send the success message.
           (funcall dired-async-message-function
                    "Asynchronous %s of %s on %s file%s done"
                    'dired-async-message
                    (car operation) (cadr operation)
                    total (dired-plural-s total)))))))

(defun dired-async-maybe-kill-ftp ()
  "Return a form to kill ftp process in child emacs."
  (quote
   (progn
     (require 'cl-lib)
     (let ((buf (cl-loop for b in (buffer-list)
                         thereis (and (string-match
                                       "\\`\\*ftp.*"
                                       (buffer-name b)) b))))
       (when buf (kill-buffer buf))))))

(defvar overwrite-query)
(defun dired-async-create-files (file-creator operation fn-list name-constructor
                                 &optional _marker-char)
  "Same as `dired-create-files' but asynchronous.

See `dired-create-files' for the behavior of arguments."
  (setq overwrite-query nil)
  (let ((total (length fn-list))
        failures async-fn-list skipped callback
        async-quiet-switch)
    (let (to)
      (dolist (from fn-list)
        (setq to (funcall name-constructor from))
        (if (and (equal to from)
                 (null (eq file-creator 'backup-file)))
            (progn
              (setq to nil)
              (dired-log "Cannot %s to same file: %s\n"
                         (downcase operation) from)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
            (let* ((overwrite (and (null (eq file-creator 'backup-file))
                                   (file-exists-p to)))
                   (dired-overwrite-confirmed ; for dired-handle-overwrite
                    (and overwrite
                         (let ((help-form `(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." ,to)))
                           (dired-query 'overwrite-query "Overwrite `%s'?" to)))))
              ;; Handle the `dired-copy-file' file-creator specially
              ;; When copying a directory to another directory or
              ;; possibly to itself or one of its subdirectories.
              ;; e.g "~/foo/" => "~/test/"
              ;; or "~/foo/" =>"~/foo/"
              ;; or "~/foo/ => ~/foo/bar/")
              ;; In this case the 'name-constructor' have set the destination
              ;; TO to "~/test/foo" because the old emacs23 behavior
              ;; of `copy-directory' was to not create the subdirectory
              ;; and instead copy the contents.
              ;; With the new behavior of `copy-directory'
              ;; (similar to the `cp' shell command) we don't
              ;; need such a construction of the target directory,
              ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
              (let ((destname (file-name-directory to)))
                (when (and (file-directory-p from)
                           (file-directory-p to)
                           (eq file-creator 'dired-copy-file))
                  (setq to destname))
                ;; If DESTNAME is a subdirectory of FROM, not a symlink,
                ;; and the method in use is copying, signal an error.
                (and (eq t (car (file-attributes destname)))
                     (eq file-creator 'dired-copy-file)
                     (file-in-directory-p destname from)
                     (error "Cannot copy `%s' into its subdirectory `%s'"
                            from to)))
              (if overwrite
                  (or (and dired-overwrite-confirmed
                           (push (cons from to) async-fn-list))
                      (progn
                        (push (dired-make-relative from) failures)
                        (dired-log "%s `%s' to `%s' failed\n"
                                   operation from to)))
                  (push (cons from to) async-fn-list)))))
      ;; Fix tramp issue #80 with emacs-26, use "-q" only when needed.
      (setq async-quiet-switch
            (if (and (boundp 'tramp-cache-read-persistent-data)
                     async-fn-list
                     (cl-loop for (from . to) in async-fn-list
                              thereis (file-remote-p to)))
                "-q" "-Q"))
      ;; When failures have been printed to dired log add the date at bob.
      (when (or failures skipped) (dired-log t))
      ;; When async-fn-list is empty that's mean only one file
      ;; had to be copied and user finally answer NO.
      ;; In this case async process will never start and callback
      ;; will have no chance to run, so notify failures here.
      (unless async-fn-list
        (cond (failures
               (funcall dired-async-message-function
                        "%s failed for %d of %d file%s -- See *Dired log* buffer"
                        'dired-async-failures
                        operation (length failures)
                        total (dired-plural-s total)))
              (skipped
               (funcall dired-async-message-function
                        "%s: %d of %d file%s skipped -- See *Dired log* buffer"
                        'dired-async-failures
                        operation (length skipped) total
                        (dired-plural-s total)))))
      ;; Setup callback.
      (setq callback
            (lambda (&optional _ignore)
               (dired-async-after-file-create
                total (list operation (length async-fn-list)) failures skipped)
               (when (string= (downcase operation) "rename")
                 (cl-loop for (file . to) in async-fn-list
                          for bf = (get-file-buffer file)
                          for destp = (file-exists-p to)
                          do (and bf destp
                                  (with-current-buffer bf
                                    (set-visited-file-name to t t))))))))
    ;; Start async process.
    (when async-fn-list
      (async-start `(lambda ()
                      (require 'cl-lib) (require 'dired-aux) (require 'dired-x)
                      ,(async-inject-variables dired-async-env-variables-regexp)
                          (let ((dired-recursive-copies (quote always))
                                (dired-copy-preserve-time
                                 ,dired-copy-preserve-time))
                            (setq overwrite-backup-query nil)
                            ;; Inline `backup-file' as long as it is not
                            ;; available in emacs.
                            (defalias 'backup-file
                                ;; Same feature as "cp -f --backup=numbered from to"
                                ;; Symlinks are copied as file from source unlike
                                ;; `dired-copy-file' which is same as cp -d.
                                ;; Directories are omitted.
                                (lambda (from to ok)
                                  (cond ((file-directory-p from) (ignore))
                                        (t (let ((count 0))
                                             (while (let ((attrs (file-attributes to)))
                                                      (and attrs (null (nth 0 attrs))))
                                               (cl-incf count)
                                               (setq to (concat (file-name-sans-versions to)
                                                                (format ".~%s~" count)))))
                                           (condition-case err
                                               (copy-file from to ok dired-copy-preserve-time)
                                             (file-date-error
                                              (dired-log "Can't set date on %s:\n%s\n" from err)))))))
                            ;; Now run the FILE-CREATOR function on files.
                            (cl-loop with fn = (quote ,file-creator)
                                     for (from . dest) in (quote ,async-fn-list)
                                     do (condition-case err
                                            (funcall fn from dest t)
                                          (file-error
                                           (dired-log "%s: %s\n" (car err) (cdr err)))
                                          nil))
                        (when (get-buffer dired-log-buffer)
                          (dired-log t)
                          (with-current-buffer dired-log-buffer
                           (write-region (point-min) (point-max)
                                         ,dired-async-log-file))))
                      ,(dired-async-maybe-kill-ftp))
                   callback)
      ;; Run mode-line notifications while process running.
      (dired-async--modeline-mode 1)
      (message "%s proceeding asynchronously..." operation))))

(defvar wdired-use-interactive-rename)
(defun dired-async-wdired-do-renames (old-fn &rest args)
  ;; Perhaps a better fix would be to ask for renaming BEFORE starting
  ;; OLD-FN when `wdired-use-interactive-rename' is non-nil.  For now
  ;; just bind it to nil to ensure no questions will be asked between
  ;; each rename.
  (let (wdired-use-interactive-rename)
    (apply old-fn args)))

(defadvice wdired-do-renames (around wdired-async)
  (let (wdired-use-interactive-rename)
    ad-do-it))

(defadvice dired-create-files (around dired-async)
  (dired-async-create-files file-creator operation fn-list
                            name-constructor marker-char))

;;;###autoload
(define-minor-mode dired-async-mode
  "Do dired actions asynchronously."
  :group 'dired-async
  :lighter dired-async-mode-lighter
  :global t
  (if dired-async-mode
      (if (fboundp 'advice-add)
          (progn (advice-add 'dired-create-files :override #'dired-async-create-files)
                 (advice-add 'wdired-do-renames :around #'dired-async-wdired-do-renames))
        (ad-activate 'dired-create-files)
        (ad-activate 'wdired-do-renames))
      (if (fboundp 'advice-remove)
          (progn (advice-remove 'dired-create-files #'dired-async-create-files)
                 (advice-remove 'wdired-do-renames #'dired-async-wdired-do-renames))
          (ad-deactivate 'dired-create-files)
          (ad-deactivate 'wdired-do-renames))))


(provide 'dired-async)

;;; dired-async.el ends here

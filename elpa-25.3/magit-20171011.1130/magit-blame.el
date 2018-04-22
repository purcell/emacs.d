;;; magit-blame.el --- blame support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2017  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Annotates each line in file-visiting buffer with information from
;; the revision which last modified the line.

;;; Code:

(require 'magit)

;;; Options

(defgroup magit-blame nil
  "Blame support for Magit."
  :link '(info-link "(magit)Blaming")
  :group 'magit-modes)

(defcustom magit-blame-heading-format "%-20a %C %s"
  "Format string used for blame headings.

The following placeholders are recognized:

  %H    hash
  %s    summary
  %a    author
  %A    author time
  %c    committer
  %C    committer time

The author and committer time formats can be specified with
`magit-blame-time-format'."
  :group 'magit-blame
  :type 'string)

(defcustom magit-blame-time-format "%F %H:%M"
  "Format for time strings in blame headings."
  :group 'magit-blame
  :type 'string)

(defcustom magit-blame-show-headings t
  "Whether to initially show blame block headings.
The headings can also be toggled locally using command
`magit-blame-toggle-headings'."
  :group 'magit-blame
  :type 'boolean)

(defcustom magit-blame-disable-modes '(fci-mode yascroll-bar-mode)
  "List of modes not compatible with Magit-Blame mode.
This modes are turned off when Magit-Blame mode is turned on,
and then turned on again when turning off the latter."
  :group 'magit-blame
  :type '(repeat (symbol :tag "Mode")))

(defcustom magit-blame-mode-lighter " Blame"
  "The mode-line lighter of the Magit-Blame mode."
  :group 'magit-blame
  :type '(choice (const :tag "No lighter" "") string))

(defcustom magit-blame-goto-chunk-hook '(magit-blame-maybe-update-revision-buffer)
  "Hook run by `magit-blame-next-chunk' and `magit-blame-previous-chunk'."
  :package-version '(magit . "2.1.0")
  :group 'magit-blame
  :type 'hook
  :get 'magit-hook-custom-get
  :options '(magit-blame-maybe-update-revision-buffer))

(defface magit-blame-heading
  '((((class color) (background light))
     :background "grey80"
     :foreground "black")
    (((class color) (background dark))
     :background "grey25"
     :foreground "white"))
  "Face for blame headings."
  :group 'magit-faces)

(defface magit-blame-summary
  '((t :inherit magit-blame-heading))
  "Face for commit summary in blame headings."
  :group 'magit-faces)

(defface magit-blame-hash
  '((t :inherit magit-blame-heading))
  "Face for commit hash in blame headings."
  :group 'magit-faces)

(defface magit-blame-name
  '((t :inherit magit-blame-heading))
  "Face for author and committer names in blame headings."
  :group 'magit-faces)

(defface magit-blame-date
  '((t :inherit magit-blame-heading))
  "Face for dates in blame headings."
  :group 'magit-faces)

;;; Mode

(defvar magit-blame-mode-map
  (let ((map (make-sparse-keymap)))
    (cond ((featurep 'jkl)
           (define-key map [return]    'magit-show-commit)
           (define-key map (kbd   "i") 'magit-blame-previous-chunk)
           (define-key map (kbd   "I") 'magit-blame-previous-chunk-same-commit)
           (define-key map (kbd   "k") 'magit-blame-next-chunk)
           (define-key map (kbd   "K") 'magit-blame-next-chunk-same-commit)
           (define-key map (kbd   "j") 'magit-blame)
           (define-key map (kbd   "l") 'magit-blame-reverse)
           (define-key map (kbd   "b") 'magit-blame-popup))
          (t
           (define-key map (kbd "C-m") 'magit-show-commit)
           (define-key map (kbd   "p") 'magit-blame-previous-chunk)
           (define-key map (kbd   "P") 'magit-blame-previous-chunk-same-commit)
           (define-key map (kbd   "n") 'magit-blame-next-chunk)
           (define-key map (kbd   "N") 'magit-blame-next-chunk-same-commit)
           (define-key map (kbd   "b") 'magit-blame)
           (define-key map (kbd   "f") 'magit-blame-reverse)
           (define-key map (kbd   "B") 'magit-blame-popup)))
    (define-key map (kbd   "t") 'magit-blame-toggle-headings)
    (define-key map (kbd   "q") 'magit-blame-quit)
    (define-key map (kbd "M-w") 'magit-blame-copy-hash)
    (define-key map (kbd "SPC") 'magit-diff-show-or-scroll-up)
    (define-key map (kbd "DEL") 'magit-diff-show-or-scroll-down)
    map)
  "Keymap for `magit-blame-mode'.")

(defun magit-blame-put-keymap-before-view-mode ()
  "Put `magit-blame-mode' ahead of `view-mode' in `minor-mode-map-alist'."
  (--when-let (assq 'magit-blame-mode
                    (cl-member 'view-mode minor-mode-map-alist :key #'car))
    (setq minor-mode-map-alist
          (cons it (delq it minor-mode-map-alist))))
  (remove-hook 'view-mode-hook #'magit-blame-put-keymap-before-view-mode))

(add-hook 'view-mode-hook #'magit-blame-put-keymap-before-view-mode)

(defvar-local magit-blame-buffer-read-only nil)
(defvar-local magit-blame-cache nil)
(defvar-local magit-blame-disabled-modes nil)
(defvar-local magit-blame-process nil)
(defvar-local magit-blame-recursive-p nil)
(defvar-local magit-blame-reverse-p nil)
(defvar-local magit-blame-separator nil)

(define-minor-mode magit-blame-mode
  "Display blame information inline.
\n\\{magit-blame-mode-map}"
  :lighter magit-blame-mode-lighter
  (cond (magit-blame-mode
         (when (called-interactively-p 'any)
           (setq magit-blame-mode nil)
           (user-error
            (concat "Don't call `magit-blame-mode' directly; "
                    "instead use `magit-blame' or `magit-blame-popup'")))
         (setq magit-blame-buffer-read-only buffer-read-only)
         (read-only-mode 1)
         (dolist (mode magit-blame-disable-modes)
           (when (and (boundp mode) (symbol-value mode))
             (funcall mode -1)
             (push mode magit-blame-disabled-modes)))
         (setq magit-blame-separator (magit-blame-format-separator)))
        (t
         (unless magit-blame-buffer-read-only
           (read-only-mode -1))
         (dolist (mode magit-blame-disabled-modes)
           (funcall mode 1))
         (when (process-live-p magit-blame-process)
           (kill-process magit-blame-process))
         (save-excursion
           (save-restriction
             (widen)
             (dolist (ov (overlays-in (point-min) (point-max)))
               (when (overlay-get ov 'magit-blame)
                 (delete-overlay ov))))))))

(defun auto-revert-handler--unless-magit-blame-mode ()
  "If Magit-Blame mode is on, then do nothing.  See #1731."
  magit-blame-mode)

(advice-add 'auto-revert-handler :before-until
            'auto-revert-handler--unless-magit-blame-mode)

;;; Popup

;;;###autoload (autoload 'magit-blame-popup "magit-blame" nil t)
(magit-define-popup magit-blame-popup
  "Popup console for blame commands."
  :man-page "git-blame"
  :switches '((?w "Ignore whitespace" "-w")
              (?r "Do not treat root commits as boundaries" "--root"))
  :options  '((?M "Detect lines moved or copied within a file" "-M")
              (?C "Detect lines moved or copied between files" "-C"))
  :actions  '((?b "Show blob touching these lines" magit-blame)
              (?f "Show blob removing these lines" magit-blame-reverse))
  :default-arguments '("-w")
  :max-action-columns 1
  :default-action 'magit-blame)

;;; Process

(defun magit-blame-arguments* (reverse)
  (let ((args (magit-blame-arguments)))
    (when (and reverse buffer-file-name)
      (user-error "Only blob buffers can be blamed in reverse"))
    (if (and magit-blame-mode
             (or (and reverse magit-blame-reverse-p)
                 (and (not reverse)
                      (not magit-blame-reverse-p))))
        (--if-let (magit-blame-chunk-get :previous-hash)
            (list it (magit-blame-chunk-get :previous-file)
                  args (magit-blame-chunk-get :previous-start))
          (user-error "Block has no further history"))
      (--if-let (magit-file-relative-name nil (not magit-buffer-file-name))
          (list (or magit-buffer-refname magit-buffer-revision) it args)
        (if buffer-file-name
            (user-error "Buffer isn't visiting a tracked file")
          (user-error "Buffer isn't visiting a file"))))))

;;;###autoload
(defun magit-blame-reverse (revision file &optional args line)
  "For each line show the subsequent revision that removes it.
\n(fn REVISION FILE &optional ARGS)" ; LINE is for internal use
  (interactive (magit-blame-arguments* t))
  (magit-blame revision file (cons "--reverse" args) line))

;;;###autoload
(defun magit-blame (revision file &optional args line)
  "For each line show the revision that last touched it.

Interactively blame the file being visited in the current buffer.
If the buffer visits a revision of that file, then blame up to
that revision, otherwise blame the file's full history, including
uncommitted changes.

If Magit-Blame mode is already turned on then blame recursively, by
visiting REVISION:FILE (using `magit-find-file'), where revision
is the revision before the revision that added the lines at
point.

ARGS is a list of additional arguments to pass to `git blame';
only arguments available from `magit-blame-popup' should be used.
\n(fn REVISION FILE &optional ARGS)" ; LINE is for internal use
  (interactive (magit-blame-arguments* nil))
  (let ((toplevel (or (magit-toplevel)
                      (user-error "Not in git repository"))))
    (let ((default-directory toplevel))
      (if revision
          (magit-find-file revision file)
        (--if-let (find-buffer-visiting file)
            (progn (switch-to-buffer it)
                   (save-buffer))
          (find-file file))))
    (let ((default-directory toplevel)
          (reverse (and (member "--reverse" args) t)))
      (widen)
      (when line
        (setq magit-blame-recursive-p t)
        (goto-char (point-min))
        (forward-line (1- line)))
      (when (or (not magit-blame-mode)
                (and reverse  (not magit-blame-reverse-p))
                (and (not reverse) magit-blame-reverse-p))
        (setq magit-blame-reverse-p reverse)
        (setq magit-blame-cache (make-hash-table :test 'equal))
        (let ((show-headings magit-blame-show-headings))
          (magit-blame-mode 1)
          (setq-local magit-blame-show-headings show-headings))
        (message "Blaming...")
        (let ((magit-process-popup-time -1)
              (inhibit-magit-refresh t))
          (magit-run-git-async
           "blame" "--incremental" args
           "-L" (format "%s,%s"
                        (line-number-at-pos (window-start))
                        (line-number-at-pos (1- (window-end nil t))))
           revision "--" file))
        (setq magit-blame-process magit-this-process)
        (set-process-filter magit-this-process 'magit-blame-process-filter)
        (set-process-sentinel
         magit-this-process
         `(lambda (process event)
            (when (memq (process-status process) '(exit signal))
              (magit-process-sentinel process event)
              (magit-blame-assert-buffer process)
              (with-current-buffer (process-get process 'command-buf)
                (when magit-blame-mode
                  (let ((magit-process-popup-time -1)
                        (inhibit-magit-refresh t)
                        (default-directory ,default-directory))
                    (magit-run-git-async "blame" "--incremental" ,@args
                                         ,revision "--" ,file))
                  (setq magit-blame-process magit-this-process)
                  (set-process-filter
                   magit-this-process 'magit-blame-process-filter)
                  (set-process-sentinel
                   magit-this-process 'magit-blame-process-sentinel))))))))))

(defun magit-blame-process-sentinel (process event)
  (let ((status (process-status process)))
    (when (memq status '(exit signal))
      (magit-process-sentinel process event)
      (if (and (eq status 'exit)
               (zerop (process-exit-status process)))
          (message "Blaming...done")
        (magit-blame-assert-buffer process)
        (with-current-buffer (process-get process 'command-buf)
          (magit-blame-mode -1))
        (message "Blaming...failed")))))

(defvar magit-blame-log nil
  "Whether to log blame output to the process buffer.
This is intended for debugging purposes.")

(defun magit-blame-process-filter (process string)
  (when magit-blame-log
    (magit-process-filter process string))
  (--when-let (process-get process 'partial-line)
    (setq string (concat it string))
    (setf (process-get process 'partial-line) nil))
  (magit-blame-assert-buffer process)
  (with-current-buffer (process-get process 'command-buf)
    (when (and magit-blame-mode
               (zerop (process-exit-status process)))
      (let ((chunk (process-get process 'chunk))
            (lines (split-string string "\n" t)))
        (unless (string-match-p "\n\\'" string)
          (process-put process 'chunk chunk)
          (process-put process 'partial-line (car (last lines)))
          (setq lines (butlast lines)))
        (dolist (line lines)
          (cond
           ((equal line ""))
           ((not chunk)
            (string-match
             "^\\(.\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" line)
            (setq chunk
                  (list :hash (let ((hash (match-string 1 line)))
                                (unless (equal hash (make-string 40 ?0))
                                  hash))
                        :previous-start (string-to-number (match-string 2 line))
                        :start (string-to-number (match-string 3 line))
                        :lines (string-to-number (match-string 4 line)))))
           ((string-match "^filename \\(.+\\)" line)
            (let* ((hash (plist-get chunk :hash))
                   (file (match-string 1 line)))
              (--if-let (gethash hash magit-blame-cache)
                  (setq chunk (nconc chunk it))
                (plist-put chunk :filename file)
                (puthash hash chunk magit-blame-cache)))
            (magit-blame-make-overlay chunk)
            (setq chunk nil))
           ((string-match "^previous \\(.\\{40\\}\\) \\(.+\\)" line)
            (plist-put chunk :previous-hash (match-string 1 line))
            (plist-put chunk :previous-file (match-string 2 line)))
           ((string-match "^\\([^ ]+?-mail\\) <\\([^>]+\\)>" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (string-to-number (match-string 2 line))))
           ((string-match "^\\([^ ]+?-\\(?:time\\|tz\\)\\) \\(.+\\)" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (string-to-number (match-string 2 line))))
           ((string-match "^\\([^ ]+\\) \\(.+\\)" line)
            (plist-put chunk (intern (concat ":" (match-string 1 line)))
                       (match-string 2 line))))
          (process-put process 'chunk chunk))))))

(defun magit-blame-assert-buffer (process)
  (unless (buffer-live-p (process-get process 'command-buf))
    (kill-process process)
    (user-error "Buffer being blamed has been killed")))

;;; Display

(defun magit-blame-make-overlay (chunk)
  (let ((ov (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (forward-line (1- (plist-get chunk :start)))
                (--when-let (--first (overlay-get it 'magit-blame)
                                     (overlays-at (point)))
                  (delete-overlay it))
                (make-overlay (point)
                              (progn (forward-line
                                      (plist-get chunk :lines))
                                     (point))))))
        (heading (magit-blame-format-heading chunk)))
    (overlay-put ov 'magit-blame chunk)
    (overlay-put ov 'magit-blame-heading heading)
    (overlay-put ov 'before-string
                 (if magit-blame-show-headings
                     heading
                   magit-blame-separator))))

(defun magit-blame-format-separator ()
  (propertize
   (concat (propertize " "  'display '(space :height (2)))
           (propertize "\n" 'line-height t))
   'face (list :background (face-attribute 'magit-blame-heading :background))))

(defun magit-blame-format-heading (chunk)
  (with-temp-buffer
    (insert (format-spec
             (concat magit-blame-heading-format "\n")
             `((?H . ,(propertize (or (plist-get chunk :hash) "")
                                  'face 'magit-blame-hash))
               (?s . ,(propertize (or (plist-get chunk :summary) "")
                                  'face 'magit-blame-summary))
               (?a . ,(propertize (or (plist-get chunk :author) "")
                                  'face 'magit-blame-name))
               (?A . ,(propertize (magit-blame-format-time-string
                                   magit-blame-time-format
                                   (plist-get chunk :author-time)
                                   (plist-get chunk :author-tz))
                                  'face 'magit-blame-date))
               (?c . ,(propertize (or (plist-get chunk :committer) "")
                                  'face 'magit-blame-name))
               (?C . ,(propertize (magit-blame-format-time-string
                                   magit-blame-time-format
                                   (plist-get chunk :committer-time)
                                   (plist-get chunk :committer-tz))
                                  'face 'magit-blame-date)))))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((face (get-text-property (point) 'face))
            (next (or (next-single-property-change (point) 'face)
                      (point-max))))
        (unless face
          (put-text-property (point) next 'face 'magit-blame-heading))
        (goto-char next)))
    (buffer-string)))

(defun magit-blame-format-time-string (format time tz)
  (format-time-string
   format (seconds-to-time (+ time (* (/ tz 100) 60 60) (* (% tz 100) 60)))))

;;; Commands

(defun magit-blame-quit ()
  "Turn off Magit-Blame mode.
If the buffer was created during a recursive blame,
then also kill the buffer."
  (interactive)
  (kill-local-variable 'magit-blame-reverse-p)
  (if magit-blame-recursive-p
      (kill-buffer)
    (magit-blame-mode -1)))

(defun magit-blame-next-chunk ()
  "Move to the next chunk."
  (interactive)
  (--if-let (next-single-char-property-change (point) 'magit-blame)
      (progn (goto-char it)
             (run-hooks 'magit-blame-goto-chunk-hook))
    (user-error "No more chunks")))

(defun magit-blame-previous-chunk ()
  "Move to the previous chunk."
  (interactive)
  (--if-let (previous-single-char-property-change (point) 'magit-blame)
      (progn (goto-char it)
             (run-hooks 'magit-blame-goto-chunk-hook))
    (user-error "No more chunks")))

(defun magit-blame-next-chunk-same-commit (&optional previous)
  "Move to the next chunk from the same commit.\n\n(fn)"
  (interactive)
  (-if-let (hash (magit-blame-chunk-get :hash))
      (let ((pos (point)) ov)
        (save-excursion
          (while (and (not ov)
                      (not (= pos (if previous (point-min) (point-max))))
                      (setq pos (funcall
                                 (if previous
                                     'previous-single-char-property-change
                                   'next-single-char-property-change)
                                 pos 'magit-blame)))
            (--when-let (magit-blame-overlay-at pos)
              (when (equal (magit-blame-chunk-get :hash pos) hash)
                (setq ov it)))))
        (if ov
            (goto-char (overlay-start ov))
          (user-error "No more chunks from same commit")))
    (user-error "This chunk hasn't been blamed yet")))

(defun magit-blame-previous-chunk-same-commit ()
  "Move to the previous chunk from the same commit."
  (interactive)
  (magit-blame-next-chunk-same-commit 'previous-single-char-property-change))

(defun magit-blame-toggle-headings ()
  "Show or hide blame chunk headings."
  (interactive)
  (setq-local magit-blame-show-headings (not magit-blame-show-headings))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((next (next-single-char-property-change (point) 'magit-blame)))
          (--when-let (magit-blame-overlay-at (point))
            (overlay-put it 'before-string
                         (if magit-blame-show-headings
                             (overlay-get it 'magit-blame-heading)
                           magit-blame-separator)))
          (goto-char (or next (point-max))))))))

(defun magit-blame-copy-hash ()
  "Save hash of the current chunk's commit to the kill ring.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill nil nil 'region)
    (kill-new (message "%s" (magit-blame-chunk-get :hash)))))

;;; Utilities

(defun magit-blame-chunk-get (key &optional pos)
  (--when-let (magit-blame-overlay-at pos)
    (plist-get (overlay-get it 'magit-blame) key)))

(defun magit-blame-overlay-at (&optional pos)
  (--first (overlay-get it 'magit-blame)
           (overlays-at (or pos (point)))))

(defun magit-blame-maybe-update-revision-buffer ()
  (unless magit--update-revision-buffer
    (setq magit--update-revision-buffer nil)
    (-when-let* ((commit (magit-blame-chunk-get :hash))
                 (buffer (magit-mode-get-buffer 'magit-revision-mode nil t)))
      (setq magit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       magit-update-other-window-delay nil
       (lambda ()
         (-let [(rev buf) magit--update-revision-buffer]
           (setq magit--update-revision-buffer nil)
           (when (buffer-live-p buf)
             (let ((magit-display-buffer-noselect t))
               (apply #'magit-show-commit rev (magit-diff-arguments))))))))))

(provide 'magit-blame)
;;; magit-blame.el ends here

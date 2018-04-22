;;; git-timemachine.el --- Walk through git revisions of a file

;; Copyright (C) 2014 Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Version: 4.3
;; Package-Version: 20170325.220
;; URL: https://github.com/pidu/git-timemachine
;; Keywords: git
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;;; Use git-timemachine to browse historic versions of a file with p
;;; (previous) and n (next).

;;; Code:

(require 'vc-git)
(require 'cl-lib)

(defcustom git-timemachine-abbreviation-length 12
 "Number of chars from the full sha1 hash to use for abbreviation."
 :group 'git-timemachine)

(defcustom git-timemachine-show-minibuffer-details t
 "Non-nil means that details of the commit (its hash and date)
will be shown in the minibuffer while navigating commits."
 :group 'git-timemachine)

(defface git-timemachine-commit
 '((default :weight bold))
 "Face for git timemachine commit sha"
 :group 'git-timemachine)

(defface git-timemachine-minibuffer-detail-face
  '((((class color) (background dark))
     :foreground "yellow")
    (((class color) (background light))
     :foreground "yellow4"))
  "How to display the minibuffer detail"
  :group 'git-timemachine)

(defface git-timemachine-minibuffer-author-face
  '((((class color) (background dark))
     :foreground "orange")
    (((class color) (background light))
     :foreground "DarkOrange4"))
  "How to display the author in minibuffer"
  :group 'git-timemachine)

(defcustom git-timemachine-minibuffer-detail
 'subject
 "What to display when `git-timemachine-show-minibuffer-details` is t.
Available values are:
`commit` : The SHA hash of the commit
`subject`: The subject of the commit message"
 :type '(radio (const :tag "Commit SHA" commit) (const :tag "Commit Subject" subject))
 :group 'git-timemachine)

(defcustom git-timemachine-show-author
 t
 "Prepend author to minibuffer details."
 :type 'boolean
 :group 'git-timemachine)

(defvar-local git-timemachine-directory nil)
(defvar-local git-timemachine-revision nil)
(defvar-local git-timemachine-file nil)
(defvar-local git-timemachine--revisions-cache nil)

(defun git-timemachine-completing-read-fn (&rest args)
  "Apply ARGS to `ido-completing-read' if available and fall back to `completing-read'."
  (if (fboundp 'ido-completing-read)
      (apply 'ido-completing-read args)
    (apply 'completing-read args)
    ))

(defun git-timemachine--revisions (&optional git-branch)
 "List git revisions of current buffers file.

When passed a GIT-BRANCH, lists revisions from that branch."
 (if git-timemachine--revisions-cache
  git-timemachine--revisions-cache
  (setq git-timemachine--revisions-cache
   (prog2
    (message "Fetching Revisions...")
    (let ((default-directory git-timemachine-directory)
          (file git-timemachine-file))
     (with-temp-buffer

      (unless (zerop (if git-branch
                      (process-file vc-git-program nil t nil "--no-pager" "log" git-branch "--name-only" "--follow" "--pretty=format:%H%x00%ar%x00%ad%x00%s%x00%an" "--" file)
                      (process-file vc-git-program nil t nil "--no-pager" "log" "--name-only" "--follow" "--pretty=format:%H%x00%ar%x00%ad%x00%s%x00%an" "--" file)))
       (error "Git log command exited with non-zero exit status for file: %s" file))

      (goto-char (point-min))
      (let ((lines)
            (commit-number (/ (1+ (count-lines (point-min) (point-max))) 3)))
       (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         (string-match "\\([^\0]*\\)\0\\([^\0]*\\)\0\\([^\0]*\\)\0\\(.*\\)\0\\(.*\\)" line)
         (let ((commit (match-string 1 line))
               (date-relative (match-string 2 line))
               (date-full (match-string 3 line))
               (subject (match-string 4 line))
               (author (match-string 5 line)))
          (forward-line 1)
          (let ((file-name (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
           (push (list commit file-name commit-number date-relative date-full subject author) lines))))
        (setq commit-number (1- commit-number))
        (forward-line 2))
       (nreverse lines))))
    (message "Fetching Revisions...done")))))

(defun git-timemachine-show-current-revision ()
 "Show last (current) revision of file."
 (interactive)
 (git-timemachine-show-revision (car (git-timemachine--revisions))))

(defun git-timemachine-show-latest-revision-in-branch (git-branch)
 "Show last (current) revision of file in GIT-BRANCH."
 (interactive "MGit branch: ")
 (git-timemachine-show-revision (car (git-timemachine--revisions git-branch))))

(defun git-timemachine--next-revision (revisions)
 "Return the revision following the current revision in REVISIONS."
 (or (cadr (cl-member (car git-timemachine-revision) revisions :key #'car :test #'string=))
  (car (reverse revisions))))

(defun git-timemachine-show-previous-revision ()
 "Show previous revision of file."
 (interactive)
 (let ((new-line nil)
       (curr-revision git-timemachine-revision)
       (new-revision (git-timemachine--next-revision (git-timemachine--revisions)))
       (cursor-win-pos (git-timemachine--get-cursor-position)))
   (setq new-line (git-timemachine--find-new-current-line curr-revision new-revision (line-number-at-pos)))
   (git-timemachine-show-revision new-revision)
   (forward-line (- new-line (line-number-at-pos)))
   (git-timemachine--set-cursor-position cursor-win-pos)))

(defun git-timemachine-show-next-revision ()
 "Show next revision of file."
 (interactive)
 (let ((new-line nil)
       (curr-revision git-timemachine-revision)
       (new-revision (git-timemachine--next-revision (reverse (git-timemachine--revisions))))
       (cursor-win-pos (git-timemachine--get-cursor-position)))
   (setq new-line (git-timemachine--find-new-current-line curr-revision new-revision (line-number-at-pos)))
   (git-timemachine-show-revision new-revision)
   (forward-line (- new-line (line-number-at-pos)))
   (git-timemachine--set-cursor-position cursor-win-pos)))

(defun git-timemachine-show-nth-revision (rev-number)
 "Show the REV-NUMBER revision."
 (interactive "nEnter revision number: ")
 (let* ((revisions (reverse (git-timemachine--revisions)))
	(num-revisions (length revisions))
	(curr-revision git-timemachine-revision)
	(new-revision (nth (1- rev-number) revisions))
	(new-line nil)
	(cursor-win-pos (git-timemachine--get-cursor-position)))
   (if (not new-revision)
       (message "Only %d revisions exist." num-revisions)
     (setq new-line (git-timemachine--find-new-current-line curr-revision new-revision (line-number-at-pos)))
     (git-timemachine-show-revision new-revision)
     (forward-line (- new-line (line-number-at-pos)))
     (git-timemachine--set-cursor-position cursor-win-pos))))

(defun git-timemachine-show-revision (revision)
 "Show a REVISION (commit hash) of the current file."
 (when revision
  (let ((current-position (point))
        (commit (car revision))
        (revision-file-name (nth 1 revision))
        (commit-index (nth 2 revision))
        (date-relative (nth 3 revision))
        (date-full (nth 4 revision))
        (subject (nth 5 revision)))
   (setq buffer-read-only nil)
   (erase-buffer)
   (let ((default-directory git-timemachine-directory)
         (process-coding-system-alist (list (cons "" (cons buffer-file-coding-system default-process-coding-system)))))
    (process-file vc-git-program nil t nil "--no-pager" "show"
     (concat commit ":" revision-file-name)))
   (setq buffer-read-only t)
   (set-buffer-modified-p nil)
   (let* ((revisions (git-timemachine--revisions))
          (n-of-m (format "(%d/%d %s)" commit-index (length revisions) date-relative)))
    (setq mode-line-buffer-identification
     (list (propertized-buffer-identification "%12b") "@"
      (propertize (git-timemachine-abbreviate commit) 'face 'git-timemachine-commit) " name:" revision-file-name" " n-of-m)))
   (setq git-timemachine-revision revision)
   (goto-char current-position)
   (when git-timemachine-show-minibuffer-details
    (git-timemachine--show-minibuffer-details revision)))))

(defun git-timemachine--show-minibuffer-details (revision)
 "Show details for REVISION in minibuffer."
 (let* ((date-relative (nth 3 revision))
        (date-full (nth 4 revision))
        (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
        (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
  (message "%s%s [%s (%s)]"
   (propertize author 'face 'git-timemachine-minibuffer-author-face)
   (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face) date-full date-relative)))

(defun git-timemachine--find-new-current-line (curr-revision new-revision current-line)
  "Return the new current line after a revision jump"
  (let* ((revisions (reverse (git-timemachine--revisions)))
	 (current-commit (car curr-revision))
	 (curr-rev-number (+ (or (cl-position curr-revision revisions) 0) 1))
	 (new-commit (car new-revision))
	 (new-rev-number (+ (or (cl-position new-revision revisions) 0) 1))
	 (new-line nil)
	 (file git-timemachine-file)
	 (reverse (< curr-rev-number new-rev-number)))
    ;; If no commit change, do nothing
    (if (= curr-rev-number new-rev-number)
	current-line
      ;; Get new current line number using `git-blame`
      (with-temp-buffer
	(if reverse
	    (process-file vc-git-program nil t nil "blame" "--reverse" "-n" (format "-L %s,%s" current-line current-line) file (format "%s..%s" current-commit new-commit))
	  (process-file vc-git-program nil t nil "blame" "-n" (format "-L %s,%s" current-line current-line) file (format "%s..%s" new-commit current-commit)))
	(goto-char (point-min))
	;; If end-of-buffer problem
	(when (search-forward-regexp "^fatal: file .+ has only .+ lines" nil t)
	  (setq current-line (- current-line 1))
	  (erase-buffer)
	  (if reverse
	      (process-file vc-git-program nil t nil "blame" "--reverse" "-n" (format "-L %s,%s" current-line current-line) file (format "%s..%s" current-commit new-commit))
	    (process-file vc-git-program nil t nil "blame" "-n" (format "-L %s,%s" current-line current-line) file (format "%s..%s" new-commit current-commit))))
	(goto-char (point-min))
	(search-forward-regexp "^[^ ]+ \\([^ ]+\\)")
	(setq new-line (string-to-number (match-string 1)))
	;; In case git blame doesn't give what we expect
	(when (= new-line 0) (setq new-line current-line))
	new-line))))

(defun git-timemachine--get-cursor-position ()
  "Return the cursor visual line number with respect to the
current window first line"
  (let* ((win-point-min (save-excursion (move-to-window-line 0) (point)))
	 (cur-pos (count-screen-lines win-point-min (point))))
    cur-pos))

(defun git-timemachine--set-cursor-position (POS)
  "Set the cursor position to the POS visual line with
respect to the window first line"
  (recenter POS))

(defun git-timemachine-abbreviate (revision)
 "Return REVISION abbreviated to `git-timemachine-abbreviation-length' chars."
 (substring revision 0 git-timemachine-abbreviation-length))

(defun git-timemachine-quit ()
 "Exit the timemachine."
 (interactive)
 (kill-buffer))

(defun git-timemachine-blame ()
 "Call magit-blame on current revision."
 (interactive)
 (if (fboundp 'magit-blame)
  (magit-blame (car git-timemachine-revision) (buffer-file-name))
  (message "You need to install magit for blame capabilities")))

(defun git-timemachine-kill-revision ()
 "Kill the current revisions abbreviated commit hash."
 (interactive)
 (let ((revision (car git-timemachine-revision)))
  (message revision)
  (kill-new revision)))

(defun git-timemachine-kill-abbreviated-revision ()
 "Kill the current revisions full commit hash."
 (interactive)
 (let ((revision (git-timemachine-abbreviate (car git-timemachine-revision))))
  (message revision)
  (kill-new revision)))

(define-minor-mode git-timemachine-mode
 "Git Timemachine, feel the wings of history."
 :init-value nil
 :lighter " Timemachine"
 :keymap
 '(("p" . git-timemachine-show-previous-revision)
   ("n" . git-timemachine-show-next-revision)
   ("g" . git-timemachine-show-nth-revision)
   ("q" . git-timemachine-quit)
   ("w" . git-timemachine-kill-abbreviated-revision)
   ("W" . git-timemachine-kill-revision)
   ("b" . git-timemachine-blame))
 :group 'git-timemachine)

(defun git-timemachine-validate (file)
 "Validate that there is a FILE and that it belongs to a git repository.
Call with the value of 'buffer-file-name."
 (unless file
  (error "This buffer is not visiting a file"))
 (unless (vc-git-registered file)
  (error "This file is not git tracked")))

(defun git-timemachine--start (get-revision-fn)
 "Setup a timemachine buffer and populate it from the result of GET-REVISION-FN."
 (setq git-timemachine--revisions-cache nil)
 (git-timemachine-validate (buffer-file-name))
 (let ((git-directory (expand-file-name (vc-git-root (buffer-file-name))))
       (file-name (buffer-file-name))
       (timemachine-buffer (format "timemachine:%s" (buffer-name)))
       (cur-line (line-number-at-pos))
       (cursor-position (git-timemachine--get-cursor-position))
       (new-line nil)
       (mode major-mode)
       (coding-system buffer-file-coding-system))
  (with-current-buffer (get-buffer-create timemachine-buffer)
   (switch-to-buffer timemachine-buffer)
   (setq buffer-file-name file-name)
   (setq buffer-file-coding-system coding-system)
   (funcall mode)
   (setq git-timemachine-directory git-directory
         git-timemachine-file (file-relative-name file-name git-directory)
    git-timemachine-revision nil)
   (funcall get-revision-fn)
   (setq new-line (git-timemachine--find-new-current-line git-timemachine-revision (list "HEAD" "" 0 "" "" "" "") cur-line)) ;; Allow to stay on the same line
   (goto-char (point-min))
   (forward-line (- new-line 1))
   (git-timemachine--set-cursor-position cursor-position)
   (git-timemachine-mode))))

;;;###autoload
(defun git-timemachine-toggle ()
 "Toggle git timemachine mode."
 (interactive)
 (if (bound-and-true-p git-timemachine-mode)
  (git-timemachine-quit)
  (git-timemachine)))

;;;###autoload
(defun git-timemachine ()
 "Enable git timemachine for file of current buffer."
 (interactive)
 (git-timemachine--start #'git-timemachine-show-current-revision))

;;;###autoload
(defun git-timemachine-switch-branch (git-branch)
 "Enable git timemachine for current buffer, switching to GIT-BRANCH."
 (interactive (list (git-timemachine-completing-read-fn "Branch to switch to: "(vc-git-branches))))
 (git-timemachine--start (lambda () (git-timemachine-show-latest-revision-in-branch git-branch))))

(provide 'git-timemachine)

;;; git-timemachine.el ends here

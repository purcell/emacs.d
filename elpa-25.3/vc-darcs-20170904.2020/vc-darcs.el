;;; vc-darcs.el --- a VC backend for darcs    -*- lexical-binding: t -*-

;;; Copyright (C) 2004  Jorgen Schaefer <forcer@forcix.cx>
;;; Copyright (C) 2004-2014  Juliusz Chroboczek <jch@pps.univ-paris-diderot.fr>

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;;         Juliusz Chroboczek <jch@pps.univ-paris-diderot.fr>
;; Maintainer: Libor Čapák <capak@inputwish.com>
;; Keywords: vc
;; Package-Version: 20170904.2020
;; Package-X-Original-Version: 20170409.1521
;; Package-X-Original-Version: 20141122.1326
;; Version: 1.19
;; Package-Requires: ((emacs "24"))

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;; Commentary:

;; Darcs is David's Advanced Revision Control System, available at
;; http://www.darcs.net/

;; This version of vc-darcs was tested with Emacs 23 and 24.  It might
;; still work on Emacs 22 with somewhat reduced functionality.

;; A few ideas for this file are directly taken from vc-svn.el.  Thanks to
;; Jim Blandy.

;; To install, put this file into your load-path and add the following to
;; your .emacs:

;; (add-to-list 'vc-handled-backends 'DARCS)
;; (autoload 'vc-darcs-find-file-hook "vc-darcs")
;; (add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

;; There are a few reasons why vc is difficult to coerce into using darcs
;; as a backend.  By default, vc expects files (not trees) to be versioned
;; as nodes in an AND/OR tree, as is done by RCS and CVS.  Recent version
;; of vc allow some customisation of that, which allows smooth integration
;; with e.g. subversion.

;; Darcs doesn't version files at all; a darcs repository is a collection
;; of patches, and a particular file version is just the set of patches
;; that have been applied in order to build it.  While patches might be
;; reordered when moving between repositories, they usually remain ordered
;; (notable exceptions to that being unpull and optimize); hence,
;; a convenient mental shortcut is to identify a version by the latest
;; patch included in that version.  This is what we do.

;; Internally, darcs identifies a patch by its hash, which you may obtain
;; by using changes --xml.  We follow that approach in this code.  However,
;; as a hash might be difficult to remember at times (it's 65 characters
;; long), all commands that might take an interactive argument also accept
;; a regexp identifying a patch name.  See VC-DARCS-REV-TO-HASH.

;; The fit with vc is still not quite perfect.  A sore point is that vc
;; doesn't normalise versions; hence, if you have a patch called ``Initial
;; import'', you might end up with distinct but identical buffers called
;; vc-darcs.el~Init~, vc-darcs.el~Initial~ and so on.


;;; Code:

(defvar vc-darcs-version-string "1.19"
  "The version string for vc-darcs.el.")

(eval-when-compile
  (require 'xml)
  (require 'vc))

(require 'xml)

(declare-function vc-exec-after "vc-dispatcher" (code))

(defgroup vc-darcs nil
  "*The darcs backend for vc."
  :prefix "vc-darcs-"
  :group 'vc)

(defcustom vc-darcs-program-name "darcs"
  "*The name of the darcs command."
  :type 'string
  :group 'vc-darcs)

(defcustom vc-darcs-program-arguments '((diff "-u"))
  "*An a-list of further arguments to pass to darcs.
Each element consists of a symbol naming the command to work on, and a
list of arguments to pass."
  :type '(alist :key-type symbol :value-type (list string))
  :group 'vc-darcs)

(defcustom vc-darcs-mail-address
  (or (getenv "DARCS_EMAIL")
      (getenv "EMAIL")
      (if (string-match "<" user-mail-address)
          user-mail-address
        (format "%s <%s>"
                (user-full-name) user-mail-address)))
  "*The email address to use in darcs."
  :type '(choice string (const nil))
  :group 'vc-darcs)


(defvar log-view-per-file-logs)
(defvar log-view-file-re)
(defvar log-view-message-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-vc-fileset)

(declare-function vc-annotate-convert-time "vc-annotate" (time))


(defun vc-darcs-root (file)
  "Return the root darcs repository directory for FILE, or nil if not found."
  (vc-find-root file "_darcs"))

(defalias 'vc-darcs-find-root 'vc-darcs-root)

(defun vc-darcs-special-file-p (file)
  (let ((file (expand-file-name file)))
    (and (string-match "/_darcs/" file)
         (not (string-match "/_darcs/prefs/" file)))))

(defun vc-darcs-do-command (command okstatus files &rest flags)
  "Run darcs COMMAND using VC-DO-COMMAND."
  (let ((arguments (cdr (assq command vc-darcs-program-arguments))))
    (apply #'vc-do-command "*vc*" okstatus
           vc-darcs-program-name files (symbol-name command)
           (append arguments flags))))

(defun vc-darcs-changes (&optional files &rest flags)
  "Return a list of hashes of the patches that touch FILES in inverse order."
  (with-temp-buffer
    (apply #'vc-do-command t 0 vc-darcs-program-name files
           "changes" "--xml" flags)
    (let ((changes (xml-parse-region 1 (point-max))))
      (unless (and (null (cdr changes))
                   (eq 'changelog (car (car changes))))
        (error "Unexpected output from darcs changes --xml."))
      (let ((ch (cddr (car changes)))
            (l '()))
        (while (not (null ch))
          (let ((e (pop ch)))
            (when (and (consp e)
                       (eq (car e) 'patch))
              (let ((h (cdr (assoc 'hash (cadr e)))))
                (when h
                  (push h l))))))
        (nreverse l)))))

(defun vc-darcs-hash-p (rev)
  "Return non-nil if REV has the syntax of a darcs hash."
  (and (= (length rev) 40)
       (string-match "[a-z0-9-]" rev)
       t))

(defun vc-darcs-rev-to-hash (rev files &optional off-by-one)
  (cond
   ((or (null rev) (eq rev t) (equal rev "")) nil)
   ((not off-by-one)
    (cond
     ((vc-darcs-hash-p rev) rev)
     (t (car (last (vc-darcs-changes files "--patch" rev))))))
   (t
    (let ((flags
           (if (vc-darcs-hash-p rev)
               (list "--from-match" (concat "hash " rev))
             (list "--from-patch" rev))))
      (let ((changes (apply #'vc-darcs-changes files flags)))
        (and (cdr changes) (car (last changes 2))))))))

(defun vc-darcs-next-revision (files rev)
  "Return the revision number that follows REV for FILES."
  (vc-darcs-rev-to-hash rev files t))

(defalias 'vc-darcs-next-version 'vc-darcs-next-revision)

(defun vc-darcs-previous-revision (files rev)
  "Return the revision number that precedes REV for FILES."
  (let ((flags
         (if (vc-darcs-hash-p rev)
             (list "--to-match" (concat "hash " rev))
           (list "--to-patch" rev))))
    (let ((changes (apply #'vc-darcs-changes files flags)))
      (cadr changes))))

(defalias 'vc-darcs-previous-version 'vc-darcs-previous-revision)

(defun vc-darcs-revision-granularity () 'repository)


;;; State-querying functions

(defun vc-darcs-registered (file)
  "Return non-nil if FILE is handled by darcs."
  (cond
   ((vc-darcs-special-file-p file)
    ;; If vc-directory-exclusion-list is set incorrectly, vc-dired will
    ;; query us for all the files under _darcs.  Get rid of them quickly.
    nil)
   (t
    (when (vc-darcs-root file)
      (let* ((file (expand-file-name file))
             (root (vc-darcs-root file))
             (default-directory (file-name-directory file)))
        (with-temp-buffer
          (catch 'found
            (condition-case nil
                (vc-do-command t nil vc-darcs-program-name
                               nil "show" "files")
              (error (throw 'found nil)))
            (goto-char (point-min))
            (while (looking-at "[^\n]+")
              ;; Darcs always prints relative to the root
              (let* ((line (match-string 0))
                     (file2 (expand-file-name line root)))
                (when (or
                       (equal file2 file)
                       (equal line "darcs: can't mix match and pending flags"))
                  (throw 'found t))
                (forward-line)))
            nil)))))))

(defun vc-darcs-file-times-equal-p (file1 file2)
  (equal (nth 5 (file-attributes file1)) (nth 5 (file-attributes file2))))

(defun vc-darcs-parse-summary (letter)
  (cond
   ((equal "R" letter) 'removed)
   ((equal "A" letter) 'added)
   (t 'edited)))

(defun vc-darcs-state (file)
  "Return the state of FILE."
  (with-temp-buffer
    (vc-do-command t nil vc-darcs-program-name file
                   "whatsnew" "--summary")
    (goto-char (point-min))
    (forward-line 1)
    (cond
     ((looking-at "No changes")
      (if (vc-darcs-registered file) 'up-to-date 'unregistered))
     ((looking-at "\\([A-Z]\\)!? ")
      (vc-darcs-parse-summary (match-string 1)))
     ((looking-at " * \\([^ \n]+\\) *-> *\\([^ \n]+\\)")
      ;; The paths printed by Darcs are relative to the root
      (let* ((root (vc-darcs-root file))
             (f (expand-file-name file))
             (f1 (expand-file-name (match-string 1) root))
             (f2 (expand-file-name (match-string 2) root)))
        (cond
         ((equal f f1) 'removed)
         ((equal f f2) 'added)
         (t nil))))
     (t nil))))

(defun vc-darcs-checkout-model (_file)
  "Indicate how FILE is checked out.  This is always IMPLICIT with darcs."
  'implicit)

(defun vc-darcs-dir-status (dir update-function)
  (let* ((dir (expand-file-name dir))
         (root (vc-darcs-root dir)))
    (vc-do-command t 'async vc-darcs-program-name dir "whatsnew" "--summary")
    (vc-exec-after
     `(vc-darcs-dir-status-continuation
       ',root ',update-function nil))))

(defun vc-darcs-dir-status-files (dir files update-function)
  (let* ((dir (expand-file-name dir))
         (root (vc-darcs-root dir)))
    (vc-do-command t 'async vc-darcs-program-name files "whatsnew" "--summary")
    (vc-exec-after
     `(vc-darcs-dir-status-continuation
       ',root ',update-function ',files))))

(defun vc-darcs-dir-status-continuation (root update-function files)
  (let* ((l '())
         (doit #'(lambda (file status)
                   ;; The paths printed by Darcs are relative to the root
                   (let ((path (file-relative-name
                                (expand-file-name file root))))
                     (unless (file-directory-p path)
                       (push (list path status nil) l)
                       (setq files (delete path files)))))))
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ((looking-at "\\([A-Z]\\)!? \\([^ \n]+\\)")
        (funcall doit (match-string 2)
                 (vc-darcs-parse-summary (match-string 1))))
       ((looking-at " * \\([^ \n]+\\) *-> *\\([^ \n]+\\)")
        (funcall doit (match-string 1) 'removed)
        (funcall doit (match-string 2) 'added)))
      (forward-line))
    (funcall update-function (nreverse l) (not (null files))))
  (while (not (null files))
    (let ((file (pop files)))
      (funcall
       update-function
       (list (list file
                   (if (vc-darcs-registered file) 'up-to-date 'unregistered)
                   nil))
       (not (null files))))))

(defun vc-darcs-get-remote (dir)
  "Get the remote repository location, if any."
  (catch 'found
    (let ((default-directory (expand-file-name dir)))
      (with-temp-buffer
        (vc-do-command t 0 vc-darcs-program-name nil
                       "show" "repo")
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at " *Default Remote: \\([^\n]+\\)")
            (throw 'found (match-string 1)))
          (forward-line))))))

(defun vc-darcs-dir-header (k v)
  (list
   (concat
    (propertize k 'face 'font-lock-type-face)
    " "
    (propertize v 'face 'font-lock-variable-name-face))))

(defun vc-darcs-dir-extra-headers (dir)
  (mapconcat
   #'identity
   (nconc
    (let ((root (vc-darcs-root dir)))
      (and root (not (equal dir root))
           (vc-darcs-dir-header "Repository :" root)))
    (let ((remote (vc-darcs-get-remote dir)))
      (and remote (vc-darcs-dir-header "Remote     :" remote))))
   "\n"))

(defun vc-darcs-responsible-p (file)
  "Return non-nil if we feel responsible for FILE,
 which can also be a directory."
  (and (not (vc-darcs-special-file-p file))
       (not (null (vc-darcs-root file)))))

(defun vc-darcs-could-register (file)
  "Return non-nil if FILE could be registered."
  (and (not (vc-darcs-special-file-p file))
       (not (null (vc-darcs-root file)))))

(defun vc-darcs-working-revision (file)
  "Return the working revision of FILE.
With darcs, this is simply the hash of the last patch that touched this file."
  (car (vc-darcs-changes file "--max-count" "1")))

(defalias 'vc-darcs-workfile-version 'vc-darcs-working-revision)

(defun vc-darcs-workfile-unchanged-p (file)
  "Return non-nil if FILE is unchanged from the repository version."
  (with-temp-buffer
    (vc-do-command t nil vc-darcs-program-name file
                   "whatsnew" "--summary")
    (goto-char (point-max))
    (forward-line -1)
    (looking-at "No changes")))

(defun vc-darcs-mode-line-string (file)
  "Return the mode line string to show for FILE."
  (let ((state (vc-state file)))
    (if (eq state 'up-to-date)
        "darcs"
      (format "darcs/%s" (vc-state file)))))


;;; State-changing functions

(defun vc-darcs-create-repo ()
  (vc-darcs-do-command 'init 0 nil))

(defun vc-darcs-register (files &optional _rev _comment)
  "Add FILES to the darcs repository, and record this.
REV and COMMENT are ignored."
  (vc-darcs-do-command 'add 0 files))

(defun vc-darcs-checkin (files rev comment)
  "Record FILES to darcs.  COMMENT is the new comment."
  (when (not (null rev))
    (error "Cannot specify check-in revision with darcs."))
  (let* ((date (format-time-string "%Y%m%d%H%M%S" nil t))
         (match (string-match "\\`\\(Summary:[ \t]*\\)?\\([^\n]*\\)[ \t\n]*\\'"
                              comment))
         (patch-name (if match
                         (match-string 2 comment)
                       comment))
         (log (if match
                  (substring comment (match-end 0))
                "")))
    (vc-darcs-do-command 'record 'async files "-a" "--pipe")
    (with-current-buffer (get-buffer "*vc*")
      (process-send-string nil
                           (format "%s\n%s\n%s\n%s"
                                   date vc-darcs-mail-address patch-name log))
      (process-send-eof))))

(defun vc-darcs-find-revision (file rev buffer)
  "Get revision REV of FILE from the darcs repository."
  (let ((rev (vc-darcs-rev-to-hash rev file)))
    (apply #'vc-do-command buffer 0 vc-darcs-program-name file
           "show" "contents"
           (and rev (list "--match" (concat "hash " rev))))))

(defalias 'vc-darcs-find-version 'vc-darcs-find-revision)

(defun vc-darcs-checkout (file &optional _editable rev)
  "Check out FILE from the Darcs repository.
EDITABLE is ignored."
  (let ((rev (vc-darcs-rev-to-hash rev file)))
    (when (and rev (not (equal rev (vc-darcs-workfile-version file))))
      (error "Cannot checkout old revisions with darcs."))
    (or (file-exists-p file)
        (vc-darcs-do-command 'revert 0 file "-a"))))

(defun vc-darcs-revert (file &optional contents-done)
  "Revert FILE back to the current workfile version."
  (unless contents-done
    (vc-darcs-do-command 'revert 0 file "-a")))


;;; History functions

(define-derived-mode vc-darcs-log-view-mode log-view-mode "Darcs-Log-View"
  (require 'add-log)
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-file-re) "\\`a\\`")
  (set (make-local-variable 'log-view-message-re)
       "^patch \\([0-9a-f]\\{40\\}\\)")
  (set (make-local-variable 'log-view-font-lock-keywords)
       '(("^\\([A-Z][a-z][a-z] .*[0-9]\\)  \\([^<>]+\\) \\(<[^<>]+>\\)"
          (1 'change-log-date)
          (2 'change-log-name)
          (3 'change-log-email))))
  )

(defun vc-darcs-show-log-entry (rev)
  ;; Pretty minimal, but good enough to allow C-x v l to do the right thing
  (cond
   ((equal rev (car (vc-darcs-changes log-view-vc-fileset "--max-count" "1")))
    (goto-char (point-min))
    (re-search-forward log-view-message-re)
    (beginning-of-line))
   (t
    nil)))

(defun vc-darcs-print-log (files &optional buffer _shortlog start-revision limit)
  "Print the logfile for the current darcs repository."
  ;; This is a hack to make C-x v L work
  (when (and (null (cdr files)) (equal (car files) (vc-darcs-root (car files))))
    (setq files nil))
  (let ((start-hash (vc-darcs-rev-to-hash start-revision files)))
    (apply #'vc-do-command buffer 'async vc-darcs-program-name files "changes"
           (append
            (and start-hash (list "--to-hash" start-hash))
            (and limit (list "--max-count" (format "%d" limit)))))))

(defun vc-darcs-diff (file &optional rev1 rev2 buffer _async)
  "Show the differences in FILE between revisions REV1 and REV2."
  (let* ((rev1 (vc-darcs-rev-to-hash rev1 file t))
         (rev2 (vc-darcs-rev-to-hash rev2 file))
         (arguments (cdr (assq 'diff vc-darcs-program-arguments)))
         (from (and rev1 (list "--from-match" (concat "hash " rev1))))
         (to (and rev2 (list "--to-match" (concat "hash " rev2)))))
    (apply #'vc-do-command (or buffer "*vc-diff*")
           nil ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21969
           vc-darcs-program-name file
           "diff"
           (append from to arguments))))

(defun vc-darcs-rename-file (old new)
  "Rename the file OLD to NEW in the darcs repository."
  (vc-darcs-do-command 'mv 0 nil old new))

(defun vc-darcs-delete-file (file)
  (delete-file file))

(defun vc-darcs-parse-integer (string)
  (let* ((c (read-from-string string))
         (n (car c)))
    (if (integerp n) n 0)))

(defun vc-darcs-alist-from-rev (file rev)
  (message rev)
  (let* ((xml
          (with-temp-buffer
            (vc-do-command t 0 vc-darcs-program-name '()
                           "changes" "--xml"
                           "--hash" rev)
            (xml-parse-region 1 (point-max))))
         (patch
          (if (not (eq 'changelog (caar xml)))
              (error "Unexpected output from darcs changes --xml")
            (nth 3 (car xml)))))
    (unless (eq 'patch (car patch))
      (error "Unexpected output from darcs changes --xml"))
    (cadr patch)))

(defun vc-darcs-annotate-command (file buffer &optional rev)
  "Produce an annotated display of fiLE in BUFFER.
For Darcs, hashes and times are stored in text properties."
  (vc-setup-buffer buffer)
  (let* ((rev (vc-darcs-rev-to-hash rev file))
         (data
          (with-temp-buffer
            (apply #'vc-do-command t 0 vc-darcs-program-name file
                   "annotate" "--machine-readable"
                   (and rev (list "--match" (concat "hash " rev))))
            (let ((output ()))
              (goto-char (point-min))
              (if (looking-at "^Rebase in progress:")
                  (forward-line 1))
              (while (looking-at "^\\([-0-9a-f]+\\)\\(?:\\.gz\\)? | \\(.*\\)$")
                (unless (string= (match-string 1) "0000000000000000000000000000000000000000")
                  (push (cons (match-string 1) (match-string 2)) output))
                (forward-line 1))
              (nreverse output)))))
    (with-current-buffer buffer
      (let ((reporter
             (and (fboundp 'make-progress-reporter)
                  (make-progress-reporter "Annotating..."
                                          1 (length data))))
            (count 0)
            (now (vc-annotate-convert-time (current-time)))
            (cache '()))
        (dolist (e data)
          (let* ((rev (car e))
                 (line (cdr e))
                 (alist (or (cdr (assoc rev cache))
                            (let ((a (vc-darcs-alist-from-rev file rev)))
                              (push (cons rev a) cache)
                              a)))
                 (author (cdr (assoc 'author alist)))
                 (date (cdr (assoc 'date alist)))
                 (year (substring date 0 4))
                 (month (substring date 4 6))
                 (day (substring date 6 8))
                 (hour (substring date 8 10))
                 (min (substring date 10 12))
                 (sec (substring date 12 14))
                 (time (vc-annotate-convert-time
                        (encode-time
                         (vc-darcs-parse-integer sec)
                         (vc-darcs-parse-integer min)
                         (vc-darcs-parse-integer hour)
                         (vc-darcs-parse-integer day)
                         (vc-darcs-parse-integer month)
                         (vc-darcs-parse-integer year))))
                 (begin (point))
                 )
            (insert (format "%-40s " rev))
            (cond
             ((string-match "<\\([^ <>@]*\\)@.*>" author)
              (setq author (match-string 1 author)))
             ((string-match "[^ <>@]*" author)
              (setq author (match-string 0 author))))
            (insert (format "%-7s "
                            (if (> (length author) 7)
                                (substring author 0 7)
                              author)))
            (insert
             (if (> (- now time) 0.9)
                 (format "%s/%s/%s " day month (substring year 2 4))
               (format "%s:%s:%s " hour min sec)))
            (insert line)
            (insert "\n")
            (add-text-properties
             begin (point)
             (list 'vc-darcs-annotate (cons rev time))))
          (setq count (+ count 1))
          (when reporter
            (progress-reporter-update reporter count)))
        (when reporter
          (progress-reporter-done reporter))))))

(defun vc-darcs-annotate-extract-revision-at-line ()
  (car (get-text-property (point) 'vc-darcs-annotate (current-buffer))))

(defun vc-darcs-annotate-time ()
  (cdr (get-text-property (point) 'vc-darcs-annotate (current-buffer))))


;;; protection against editing files under _darcs
;;; adapted from an idea by Rob Giardine

(defun vc-darcs-find-file-hook ()
  (let ((f (buffer-file-name (current-buffer))))
    (and f (vc-darcs-special-file-p f)
         (let* ((candidate
                 (let* ((f (buffer-file-name (current-buffer)))
                        (match
                         (and f (string-match
                                 "/_darcs/\\(current\\|pristine\\)/" f))))
                   (and match
                        (concat (substring f 0 (match-beginning 0))
                                "/"
                                (substring f (match-end 0))))))
                (open-instead
                 (and candidate
                      (yes-or-no-p
                       "This is a _darcs file, open the real file instead? "))))
           (cond
            (open-instead
             (find-alternate-file candidate))
            (t
             (setq buffer-read-only t)
             (push '(:propertize "_DARCS-FILE:" face font-lock-warning-face)
                   mode-line-buffer-identification)))))))

(provide 'vc-darcs)
;;; vc-darcs.el ends here

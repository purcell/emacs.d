;;; vc-darcs.el --- a VC backend for darcs

;;; Copyright (C)  2004  Jorgen Schaefer <forcer@forcix.cx>
;;; Copyright (C)  2004-2009  Juliusz Chroboczek <jch@pps.jussieu.fr>

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

;; Darcs is David's Advanced Revision Control System at
;; http://www.darcs.net/

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


(defvar vc-darcs-version-string "1.9"
  "The version string for vc-darcs.el.")

;;; Code:

(eval-when-compile
 (require 'xml)
 (require 'cl))

(require 'xml)
(require 'cl)

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

(defcustom vc-darcs-full-log nil
  "*Whether vc-print-log on a file recorded by darcs prints a full log
or only a log for the current file."
  :type 'boolean
  :group 'vc-darcs)

(defun vc-darcs-find-root (file)
  "Return the root darcs repository directory for FILE, or nil if not found."
  (vc-find-root file "_darcs"))

(defun vc-darcs-special-file-p (file)
  (and (string-match "/_darcs/" file)
       (not (string-match "/_darcs/prefs/" file))))

(defun vc-darcs-do-command (command okstatus file &rest flags)
  "Run darcs COMMAND using VC-DO-COMMAND."
  (let ((arguments (cdr (assq command vc-darcs-program-arguments))))
    (apply #'vc-do-command nil okstatus
           vc-darcs-program-name file (symbol-name command)
           (append arguments flags))))

(defun vc-darcs-changes (&optional file &rest flags)
  "Return a list of hashes of the patches that touch FILE in inverse order."
  (with-temp-buffer
    (apply #'vc-do-command t 0 vc-darcs-program-name file
           "changes" "--xml" flags)
    (let ((changes (xml-parse-region 1 (point-max))))
      (unless (and (null (cdr changes))
                   (eq 'changelog (car (car changes))))
        (error "Unexpected output from darcs changes --xml."))
      (mapcon #'(lambda (e)
                  (and (consp (car e))
                       (eq (caar e) 'patch)
                       (let ((h (cdr (assoc 'hash (cadar e)))))
                         (and h (list (substring h 0 61))))))
              (cddr (car changes))))))

(defun vc-darcs-hash-p (rev)
  "Return non-nil if REV has the syntax of a darcs hash."
  (and (= (length rev) 61)
       (eq (aref rev 14) ?-)
       (eq (aref rev 20) ?-)
       (string-match "[a-z0-9-]" rev)
       t))

(defun vc-darcs-rev-to-hash (rev file &optional off-by-one)
  (cond
    ((or (null rev) (eq rev t) (equal rev "")) nil)
    ((not off-by-one)
     (cond
       ((vc-darcs-hash-p rev) rev)
       (t (car (last (vc-darcs-changes file "--patch" rev))))))
    (t
     (let ((flags
            (if (vc-darcs-hash-p rev)
                (list "--from-match" (concat "hash " rev))
                (list "--from-patch" rev))))
       (let ((changes (apply #'vc-darcs-changes file flags)))
         (and (cdr changes) (car (last changes 2))))))))

(defun vc-darcs-next-version (file rev)
  (vc-darcs-rev-to-hash rev file t))

(defun vc-darcs-previous-version (file rev)
  (let ((flags
         (if (vc-darcs-hash-p rev)
             (list "--to-match" (concat "hash " rev))
             (list "--to-patch" rev))))
    (let ((changes (apply #'vc-darcs-changes file flags)))
      (cadr changes))))

(defun vc-darcs-revision-granularity () 'repository)

(defun vc-darcs-registered (file)
  "Return non-nil if FILE is handled by darcs."
  (let* ((file (expand-file-name file))
         ;; Vc-dired is recursive by default, and will call this function
         ;; for all the files under _darcs.  Get rid of those early.
         (special (vc-darcs-special-file-p file))
         (root (and (not special) (vc-darcs-find-root file))))
    (and root
         (let ((default-directory root))
           (with-temp-buffer
             (vc-do-command (current-buffer) nil vc-darcs-program-name
                            nil "show" "files")
             (goto-char (point-min))
             (catch 'found
               (while (looking-at "[^\n]+")
                 (when (equal (expand-file-name (match-string 0)) file)
                   (throw 'found t))
                 (forward-line))
               nil))))))

(defun vc-darcs-file-times-equal-p (file1 file2)
  (equal (nth 5 (file-attributes file1)) (nth 5 (file-attributes file2))))

(defun vc-darcs-state (file)
  "Return the state of FILE."
  (with-temp-buffer
    (vc-do-command t nil vc-darcs-program-name file
                   "whatsnew" "--summary")
    (goto-char (point-max))
    (forward-line -1)
    (if (looking-at "^No changes!")
        'up-to-date
        'edited)))

(defun vc-darcs-checkout-model (file)
  "Indicate how FILE is checked out.  This is always IMPLICIT with darcs."
  'implicit)

(defun vc-darcs-responsible-p (file)
  "Return non-nil if we feel responsible for FILE,
which can also be a directory."
  (vc-darcs-registered file))

(defun vc-darcs-could-register (file)
  "Return non-nil if FILE could be registered."
  (let ((file (expand-file-name file)))
    (and (not (vc-darcs-special-file-p file))
         (vc-darcs-find-root file)
         t)))

(defun vc-darcs-working-revision (file)
  "Return the working revision of FILE.
With darcs, this is simply the hash of the last patch that touched this file."
  (car (vc-darcs-changes file)))

(defalias 'vc-darcs-workfile-version 'vc-darcs-working-revision
  "Compatibility alias for vc-darcs-working-revision")

(defun vc-darcs-workfile-unchanged-p (file)
  "Return non-nil if FILE is unchanged from the repository version."
  (eq 'up-to-date (vc-darcs-state file)))

(defun vc-darcs-mode-line-string (file)
  "Return the mode line string to show for FILE."
  (let ((state (vc-state file)))
    (if (eq state 'up-to-date)
        "darcs"
        (format "darcs/%s" (vc-state file)))))

(defun vc-darcs-register (file &optional rev comment)
  "Add FILE to the darcs repository, and record this.
REV and COMMENT are ignored."
  (vc-darcs-do-command 'add 0 file))

(defun vc-darcs-checkin (file rev comment)
  "Record FILE to darcs.  COMMENT is the new comment."
  (when (not (null rev))
    (error "Cannot specify check-in revision with darcs."))
  (let* ((date (format-time-string "%Y%m%d%H%M%S" nil t))
         (match (string-match "\n" comment))
         (patch-name (if match 
                         (substring comment 0 (match-beginning 0))
                         comment))
         (log (if match
                  (substring comment (match-end 0))
                  "")))
    (vc-darcs-do-command 'record 'async nil "-a" "--pipe" file)
    (with-current-buffer (get-buffer "*vc*")
      (process-send-string nil
                           (format "%s\n%s\n%s\n%s"
                                   date vc-darcs-mail-address patch-name log))
      (process-send-eof))))

(defun vc-darcs-find-version (file rev buffer)
  "This gets revision REV of FILE from the darcs repository."
  (let ((rev (vc-darcs-rev-to-hash rev file)))
    (apply #'vc-do-command buffer 0 vc-darcs-program-name file
           "show" "contents"
           (and rev (list "--match" (concat "hash " rev))))))

(defun vc-darcs-checkout (file &optional editable rev)
  "Check out FILE from the Darcs repository.
EDITABLE is ignored."
  (let ((rev (vc-darcs-rev-to-hash rev file)))
    (when (and rev (not (equal rev (vc-darcs-workfile-version file))))
      (error "Cannot checkout old revisions with darcs."))
    (or (file-exists-p file)
        (vc-darcs-do-command "revert" 0 file "-a"))))

(defun vc-darcs-revert (file &optional contents-done)
  "Revert FILE back to the current workfile version."
  (vc-darcs-do-command "revert" 0 file "-a"))

(defun vc-darcs-print-log (file &optional buffer)
  "Print the logfile for the current darcs repository."
  (vc-do-command buffer 'async vc-darcs-program-name
                 (and (not vc-darcs-full-log) file) "changes"))

(defun vc-darcs-diff (file &optional rev1 rev2 buffer)
  "Show the differences in FILE between revisions REV1 and REV2."
  (let* ((async (not vc-disable-async-diff))
         (rev1 (vc-darcs-rev-to-hash rev1 file t))
         (rev2 (vc-darcs-rev-to-hash rev2 file))
         (arguments (cdr (assq 'diff vc-darcs-program-arguments)))
         (from (and rev1 (list "--from-match" (concat "hash " rev1))))
         (to (and rev2 (list "--to-match" (concat "hash " rev2)))))
    (let ((status (apply #'vc-do-command (or buffer "*vc-diff*")
                         (if async 'async 1)
                         vc-darcs-program-name file
                         "diff"
                         (append from to arguments))))
      (if async 1 status))))

(defun vc-darcs-rename-file (old new)
  "Rename the file OLD to NEW in the darcs repository."
  (vc-darcs-do-command 'mv 0 nil (expand-file-name old) (expand-file-name new)))

(defun vc-darcs-delete-file (file)
  (delete-file (expand-file-name file)))

(defun vc-darcs-parse-integer (string)
  (let* ((c (read-from-string string))
         (n (car c)))
    (if (integerp n) n 0)))

(declare-function vc-annotate-convert-time "vc-annotate" (time))

(defun vc-darcs-annotate-command (file buffer &optional rev)
  "Produce an annotated display of fiLE in BUFFER.
For Darcs, hashes and times are stored in text properties."
  (let* ((rev (vc-darcs-rev-to-hash rev file))
         (data
          (with-temp-buffer
            (apply #'vc-do-command t 0 vc-darcs-program-name file
                   "annotate" "--xml"
                   (and rev (list "--match" (concat "hash " rev))))
            (let ((output (xml-parse-region 1 (point-max))))
              (unless (and (null (cdr output))
                           (eq 'file (car (car output))))
                (error "Unexpected output from darcs annotate --xml."))
              (car output)))))
    (with-current-buffer buffer
      (let ((modified (assoc 'modified (cddr data)))
            (now (vc-annotate-convert-time (current-time))))
        (dolist (e (cddr data))
          (when (and (listp e)
                     (or (eq 'normal_line (car e)) (eq 'added_line (car e))))
            (let* ((line1
                    (find-if
                     #'(lambda (x) (and (stringp x) (not (equal "\n" x))))
                     (cddr e)))
                   (len (progn (length line1)))
                   (l0 (if (eq ?\n (aref line1 0)) 1 0))
                   (l1 (if (eq ?\n (aref line1 (- len 1))) (- len 1) len))
                   (line (substring line1 l0 l1)))
              (let* ((added-by (or (assoc 'added_by (cddr e)) modified))
                     (patch (assoc 'patch (cddr added-by)))
                     (rev (substring (cdr (assoc 'hash (cadr patch))) 0 61))
                     (author (cdr (assoc 'author (cadr patch))))
                     (date (cdr (assoc 'date (cadr patch))))
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
                     (begin (point)))
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
                 (list 'vc-darcs-annotate (cons rev time)))))))))))

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

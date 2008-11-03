;;; vc-darcs.el --- a VC backend for darcs

;;; Copyright (C)  2004  Jorgen Schaefer <forcer@forcix.cx>
;;; Copyright (C)  2004-2006  Juliusz Chroboczek <jch@pps.jussieu.fr>

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

;; A few ideas for this file are directly taken from vc-svn.el.  Thanks
;; to Jim Blandy.

;; To install, put this file into your load-path and add the following
;; to your .emacs:

;; (add-to-list 'vc-handled-backends 'DARCS)
;; (autoload 'vc-darcs-find-file-hook "vc-darcs")
;; (add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

;; There are a few reasons why vc is difficult to coerce into using
;; darcs as a backend.  Vc expects files (not trees) to be versioned
;; as nodes in an AND/OR tree, as is done by RCS and CVS.  This
;; expectation is hardwired throughout vc, notably in VC-PREVIOUS-VERSION.

;; Subversion versions trees (not files) as integers, which sort of
;; works with vc, although things like VC-DIFF might produce
;; unexpected results (most of the time, the right ``previous
;; version'' of file foo~42~ is not foo~41~ but some earlier version).

;; Darcs doesn't version files at all; a darcs repository is a
;; collection of patches, and a particular file version is just the
;; set of patches that have been applied in order to build it.  While
;; patches might be reordered when moving them between repositories,
;; they usually remain ordered (notable exceptions to that being
;; unpull and, someday, optimize); hence, a convenient mental shortcut
;; is to identify a version by the latest patch included in that
;; version.  This is what we do.

;; Internally, darcs identifies a patch by its hash, which you may
;; obtain by using changes --xml.  We follow that approach in this
;; code.  However, as a hash might be difficult to remember at times
;; (it's 65 characters long), all commands that might take an interac-
;; tive argument also accept a regexp identifying a patch name.  See
;; VC-DARCS-REV-TO-HASH.

;; The fit with vc is not perfect, and there are a number of
;; limitations.  One is that VC-PREVIOUS-VERSION cannot be customised
;; by a backend, and it doesn't take a file as argument; hence, it
;; doesn't do anything useful with darcs.  Another is that vc doesn't
;; normalise versions; hence, if you have a patch called ``Initial
;; import'', you might end up with distinct but identical buffers
;; called vc-darcs.el~Init~, vc-darcs.el~Initial~ and so on.

(defvar vc-darcs-version-string "1.8jch"
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

(defcustom vc-darcs-trust-file-times t
  "*Whether to trust filesystem times when determining the state of a file.
If this is non-nil, vc will consider that a file is up-to-date if its
modification time matches the one of the pristine file.  This may
speed some operations quite a bit, but is potentially unsafe,
especially on non-POSIX filesystems (e.g. vfat).

If you set this, you probably also want to set

  ALL ignore-times

in your Darcs preferences."
  :type 'boolean
  :group 'vc-darcs)

(defun vc-darcs-root-directory (file)
  "Return the root darcs repository directory for FILE, or nil if
there is none."
  (let ((dir (file-name-directory (expand-file-name file)))
        (olddir "/"))
    (while (and (not (equal dir olddir))
                (not (file-directory-p (concat dir "/_darcs"))))
      (setq olddir dir
            dir (file-name-directory (directory-file-name dir))))
    (and (not (equal dir olddir)) dir)))

(defun vc-darcs-darcs-directory (file)
  "Return the darcs directory for FILE, or nil if there is none."
  (let ((dir (vc-darcs-root-directory file)))
    (and dir (concat dir "_darcs/"))))

(defun vc-darcs-pristine-directory (root)
  "Return the pristine directory of repository ROOT."
  (let* ((root (if (eq ?/ (aref root (- (length root) 1)))
                  root
                  (concat root "/")))
         (pristine (concat root "_darcs/pristine/"))
         (current (concat root "_darcs/current/")))
    (or
     (and (file-directory-p pristine) pristine)
     (and (file-directory-p current) current))))

(defun vc-darcs-pristine-file (file)
  "Return the pristine file corresponding to FILE."
  (let* ((root (vc-darcs-root-directory file))
         (pristine (and root (vc-darcs-pristine-directory root))))
    (when (and root pristine
               (string-match (concat "^" (regexp-quote root) "\\(.*\\)") file))
      (concat pristine (match-string 1 file)))))

(defun vc-darcs-do-command (command okstatus file &rest flags)
  "Run darcs COMMAND using `vc-do-command', passing OKSTATUS and FILE
along with FLAGS."
  (let ((arguments (cdr (assq command vc-darcs-program-arguments))))
    (apply #'vc-do-command nil okstatus
           vc-darcs-program-name file (symbol-name command)
           (append arguments flags))))

(defun vc-darcs-report-error ()
  "Report a darcs error in the current buffer."
  (goto-char (point-max))
  (let ((found (search-backward "Fail:" nil t)))
    (if found
        (error (buffer-substring found (point-max)))
        (error (buffer-substring (max 0 (- (point-max) 1000))
                                 (point-max))))))

(defmacro vc-darcs-with-error-reporting (&rest body)
  (list 'condition-case 'nil 
        (if (null (cdr body)) (car body) (cons 'progn body))
        '(vc-darcs-report-error)))

(defun vc-darcs-changes (&optional file &rest flags)
  "Return a list of hashes of the patches that touch FILE in inverse order."
  (with-temp-buffer
    (apply #'vc-do-command t nil vc-darcs-program-name
           (and file (file-name-nondirectory file))
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
    ((null rev) nil)
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

(defun vc-darcs-registered (file)
  "Return non-nil if FILE is handled by darcs.
This is either the case if this file is in the pristine tree, or
if the addition of this file is in pending."
  (let ((pristine (vc-darcs-pristine-file file)))
    (or (and pristine (file-exists-p pristine))
        (let* ((root (vc-darcs-root-directory file))
               (pending (concat root "_darcs/patches/pending"))
               (relative (concat "./" (substring file (length root))))
               (addfile (concat "^addfile " (regexp-quote relative))))
          (when (file-exists-p pending)
            (with-temp-buffer
              (insert-file-contents pending)
              (re-search-forward addfile nil t)))))))

(defun vc-darcs-file-times-equal-p (file1 file2)
  (equal (nth 5 (file-attributes file1)) (nth 5 (file-attributes file2))))

(defun vc-darcs-state (file)
  "Return the state of FILE."
  (cond
   ((and vc-darcs-trust-file-times
         (vc-darcs-file-times-equal-p 
          file (vc-darcs-pristine-file file)))
    'up-to-date)
   (t
    (with-temp-buffer
      (vc-do-command t nil vc-darcs-program-name
                     (file-name-nondirectory file)
                     "whatsnew" "--summary")
      (goto-char (point-max))
      (previous-line 1)
      (if (looking-at "^No changes!")
          'up-to-date
        'edited)))))

(defun vc-darcs-checkout-model (file)
  "Indicate how FILE is checked out.  This is always IMPLICIT with darcs."
  'implicit)

(defun vc-darcs-responsible-p (file)
  "Return non-nil if we feel responsible for FILE, which can also be a
directory."
  (and (vc-darcs-root-directory file) t))

(defun vc-darcs-workfile-version (file)
  "Return the current working-dir version of FILE.
With darcs, this is simply the hash of the last patch that touched this file."
  (car (vc-darcs-changes file)))

(defun vc-darcs-workfile-unchanged-p (file)
  "Return non-nil if FILE is unchanged from the repository version."
  (eq 'up-to-date (vc-darcs-state file)))

(defun vc-darcs-mode-line-string (file)
  "Return the mode line string to show for FILE."
  (format "darcs/%s" (vc-state file)))

(defun vc-darcs-register (file &optional rev comment)
 "Add FILE to the darcs repository, and record this.
  REV must be NIL, COMMENT is ignored."
  (when (not (null rev))
    (error "Cannot specify register revision with darcs."))
 (vc-darcs-do-command 'add 0 file (file-name-nondirectory file)))

(defun vc-darcs-checkin (file rev comment)
  "Record FILE to darcs. REV should always be nil and is ignored,
COMMENT is the new comment."
  (when (not (null rev))
    (error "Cannot specify checkin revision with darcs."))
  (let* ((date (format-time-string "%Y%m%d%H%M%S" nil t))
         (match (string-match "\n" comment))
         (patch-name (if match 
                         (substring comment 0 (match-beginning 0))
                         comment))
         (log (if match
                  (substring comment (match-end 0))
                  "")))
    (vc-darcs-do-command 'record 'async nil
                         "-a" "--pipe" (file-name-nondirectory file))
    (with-current-buffer (get-buffer "*vc*")
      (process-send-string nil
                           (format "%s\n%s\n%s\n%s"
                                   date vc-darcs-mail-address patch-name log))
      (process-send-eof))))

(defun vc-darcs-checkout (file &optional editable rev destfile)
  "This gets revision REV of FILE from the darcs repository.
EDITABLE is ignored."
  (let ((rev (vc-darcs-rev-to-hash rev file t))
        (destfile (or destfile file)))
    (cond
      ((or (not rev) (equal rev (vc-darcs-workfile-version file)))
       (copy-file (vc-darcs-pristine-file file) destfile t))
      (t
       ;; darcs does not currently allow getting an old version of a
       ;; file.  We kludge around this by getting the pristine version and
       ;; reverting the right patch.
       (let ((temp (make-temp-file "vc-darcs")))
         (copy-file (vc-darcs-pristine-file file) temp t)
         (with-temp-buffer
           (vc-do-command t nil vc-darcs-program-name
                          (file-name-nondirectory file)
                          "diff" "-u" 
                          "--from-match" (concat "hash " rev)
                          "--to-match" 
                          (concat "hash " (vc-darcs-workfile-version file)))
           (shell-command-on-region 1 (point-max)
                                    (concat "patch -R " temp)
                                    t))
         (rename-file temp (or destfile file) t))))))

(defun vc-darcs-print-log (file)
  "Print the logfile for the current darcs repository to the *vc* buffer."
  (vc-darcs-do-command 'changes 'async (and (not vc-darcs-full-log) file)))

(defun vc-darcs-diff (file &optional rev1 rev2)
  "Show the differences in FILE between revisions REV1 and REV2."
  (let* ((rev1 (vc-darcs-rev-to-hash rev1 file t))
         (rev2 (vc-darcs-rev-to-hash rev2 file))
         (arguments (cdr (assq 'diff vc-darcs-program-arguments)))
         (from (and rev1 (list "--from-match" (concat "hash " rev1))))
         (to (and rev2 (list "--to-match" (concat "hash " rev2)))))
    (apply #'vc-do-command "*vc-diff*" 'async
           vc-darcs-program-name (file-name-nondirectory file)
           "diff"
           (append from to arguments))))

(defun vc-darcs-rename-file (old new)
  "Rename the file OLD to NEW in the darcs repository."
  (call-process vc-darcs-program-name nil nil nil "mv" old new))

(defun vc-darcs-annotate-command (file buffer &optional rev)
  (let* ((rev (vc-darcs-rev-to-hash rev file))
         (data
          (with-temp-buffer
            (apply #'vc-do-command t nil vc-darcs-program-name
                   (file-name-nondirectory file)
                   "annotate" "--xml"
                   (and rev (list "--match" (concat "hash " rev))))
            (let ((output (xml-parse-region 1 (point-max))))
              (unless (and (null (cdr output))
                           (eq 'file (car (car output))))
                (error "Unexpected output from darcs annotate --xml."))
              (car output)))))
    (with-current-buffer buffer
      (let ((modified (assoc 'modified (cddr data))))
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
                     (begin (point)))
                (insert (format "%-7s %s/%s/%s %s\n"
                                (if (> (length author) 7)
                                    (substring author 0 7)
                                    author)
                                day month year
                                line))))))))))

(defun vc-darcs-parse-integer (s)
  (let ((value 0)
        (index 0)
        (len (length s)))
    (while (< index len)
      (setq value (+ (* 10 value) (- (aref s index) ?0)))
      (incf index))
    value))

(defun vc-darcs-annotate-time ()
  (when (looking-at "........[0-9]")
    (forward-char 8)
    (and
     (looking-at "\\(..\\)/\\(..\\)/\\(....\\)")
     (let ((day (vc-darcs-parse-integer (match-string 1)))
           (month (vc-darcs-parse-integer (match-string 2)))
           (year (vc-darcs-parse-integer (match-string 3))))
       (vc-annotate-convert-time
        (encode-time 0 0 0 day month year))))))

;;; protection against editing files under _darcs
;;; adapted from an idea by Rob Giardine

(defun vc-darcs-find-file-hook ()
  (let ((f (buffer-file-name (current-buffer))))
    (and f (string-match "/_darcs" f) (not (string-match "/_darcs/prefs" f))
         (let ((open-instead
                (yes-or-no-p
                 "This is a _darcs file, open the real file instead? ")))
           (cond
             (open-instead
              (let* ((f (buffer-file-name (current-buffer)))
                     (match
                      (and f (string-match
                              "/_darcs/\\(current\\|pristine\\)/" f))))
                (unless match
                  (error "Couldn't find alternate file name"))
                (find-alternate-file
                 (concat (substring f 0 (match-beginning 0))
                         "/"
                         (substring f (match-end 0))))))
             (t
              (setq buffer-read-only t)
              (push '(:propertize "_DARCS-FILE:" face font-lock-warning-face)
                    mode-line-buffer-identification)))))))

(provide 'vc-darcs)
;;; vc-darcs.el ends here

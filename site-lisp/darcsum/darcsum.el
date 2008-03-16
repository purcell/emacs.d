;;; darcsum.el --- a pcl-cvs like interface for managing darcs patches

;; Copyright (C) 2004  John Wiegley
;; Copyright (C) 2005  Christian Neukirchen
;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Of this fork: Christian Neukirchen <chneukirchen@gmail.com>
;; Keywords: completion convenience tools vc
;; Version: 1.10-chris
;; location: http://www.newartisans.com/johnw/emacs.html
;;           http://chneukirchen.org/repos/darcsum

;; This file is not yet part of GNU Emacs.

;; This module is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This module is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Load this file and run M-x darcsum-whatsnew.  This will display a
;; pcl-cvs like buffer showing modified files.  RET on a file reveals
;; changes; RET on a directory reveals changes to its files.
;;
;; Displayed changes may be recorded with "c", which offers a buffer
;; for inputting the change name (first line) and long description
;; (subsequent lines).  C-c C-c records the patch.
;;
;; If you only want to record a part of your changes, you need to mark
;; those.  If a change is "marked" in the summary buffer with "m"
;; (done on the change, the file (all changes) or the directory (all
;; changes in all files)), only marked changes are recorded,
;; regardless of point.
;;
;; Alternatively, if no changes are marked, then only visible changes
;; are recorded.
;;
;; Move changes between buffers with "M", which prompts for a darcsum
;; buffer to move to (creating one if the buffer doesn't exist).
;;
;; "g" forgets everything and resubmits the "whatsnew" command.
;; Collapsing a file forgets all marks for that file.  Only displayed
;; changes are ever recorded!
;;
;; "n" and "p" move among files.  "q" kills the buffer.

;; TODO (Patches are welcome!):

;; - When merging changesets, check the content of change text too
;; - Better support for moving files
;; - use --interactive with apply, for applying patches from e-mail
;;   via darcsum
;; - Warn users of empty changesets before darcs does
;; - Better logfile handling
;; - Changes from "replace" aren't shown
;; - Interface to darcs replace
;; - Interface to darcs unrecord

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'add-log))

;; Attempt to handle older/other emacs in XEmacs way.
;; If `line-beginning-position' isn't available, use point-at-bol.
(unless (fboundp 'line-beginning-position)
  (defalias 'line-beginning-position 'point-at-bol))

(defgroup darcsum nil
  "Special support for the Darcs versioning system."
;;  :version "21.4"
  :group 'tools
  :prefix "darcsum-")

(defvar darcsum-data nil)
(defvar darcsum-look-for-adds nil)
(defvar darcsum-show-context nil)
(defvar pre-darcsum-ediff-window-configuration nil)

;;make buffer-local variable storing old window configuration, since "let" bindings die before ediff buffers are killed
(make-variable-buffer-local 'pre-darcsum-ediff-window-configuration)

(defface darcsum-header-face
  '((((class color) (background dark))
     (:foreground "lightyellow" :bold t))
    (((class color) (background light))
     (:foreground "blue4" :bold t))
    (t (:bold t)))
  "Face used to highlight directory changes."
  :group 'darcsum)

(defface darcsum-marked-face
  '((t (:bold t)))
  "Face used to highlight marked changes."
  :group 'darcsum)

(defface darcsum-need-action-face
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange"))
    (t (:italic t)))
  ""
  :group 'darcsum)

(defface darcsum-need-action-marked-face
  '((((class color) (background dark))
     (:foreground "orange" :bold t))
    (((class color) (background light))
     (:foreground "orange" :bold t))
    (t (:italic t :bold t)))
  ""
  :group 'darcsum)

(defface darcsum-filename-face
  '((((class color) (background dark))
     (:foreground "lightblue"))
    (((class color) (background light))
     (:foreground "blue4"))
    (t ()))
  "Face used to highlight file names."
  :group 'darcsum)

(defface darcsum-change-line-face
  '((((class color) (background dark))
     (:foreground "grey75" :background "grey25"))
    (((class color) (background light))
     (:foreground "grey25" :background "grey75"))
    (t (:bold t)))
  "Face used to highlight file names."
  :group 'darcsum)

(defun darcsum-add-props (str &rest props)
  (add-text-properties 0 (1- (length str)) (list* props) str)
  str)

(defun darcsum-add-face (str face &optional keymap &rest props)
  (when keymap
    (when (keymapp keymap)
      (setq props (list* 'keymap keymap props)))
    (setq props (list* 'mouse-face 'highlight props)))
  (add-text-properties 0 (length str) (list* 'face face props) str)
  str)

;;; Code to work with changesets

;; A changeset is an alist of the following form:
;;
;;   ((DIR (FILE (LINE CHANGE...))))
;;
;; Where DIR and FILE are plain strings, but LINE is of the following
;; possible formats:
;;
;;   LINE     An integer giving the first line of the hunk
;;   -LINE    Integer line of hunk, but hunk is not "visible"
;;   (LINE)   Integer line of hunk, but hunk is "marked"
;;   SYMBOL   Non-hunk change: 'addfile 'newfile 'rmfile 'binary or 'replace
;;   -SYMBOL  Non-hunk change, but change is not "visible"
;;   (SYMBOL) Non-hunk change, but change is "marked"
;;
;; Each CHANGE is a string which represents a modification to make to
;; the file after the starting LINE.  It begins with either a "+" or
;; "-" to indicate if the line should be removed or added to the file.
;;
;; So, for example, in a buffer with no changes visible or marked yet:
;;
;; (("."
;;  ("TODO" (addfile))
;;  ("report.cc" (-replace "[A-Za-z_0-9] indented intended"))
;;  ("report.cc"
;;   (-606 "-    blah" "+    blah" "+    blah")
;;   (-620 "-    blah" "+    blah" "+    blah")
;;   (-629 "-    blah" "+    blah" "+    blah")
;;   (-634 "-    blah" "+    blah" "+    blah")
;;   (-641 "-    blah" "+    blah" "+    blah")
;;   (-652 "-    blah" "+    blah" "+    blah")
;;   (-664 "-    blah" "+    blah" "+    blah"))
;;  ("report.h"
;;   (-115 "-    blah" "+    blah" "+    blah")
;;   (-126 "+"))))

(defconst darcsum-invisible-item-alist
  '((-replace . replace)
    (-addfile . addfile)
    (-newfile . newfile)
    (-rmfile  . rmfile)
    (-binary  . binary)))

(defun darcsum-item-visible-p (item)
  "Is ITEM visible?
Everything but negative numbers and invisible symbols are visible."
  (if (numberp item) (<= 0 item)
    (not (assq item darcsum-invisible-item-alist))))

(defun darcsum-visible-item (item)
  "Convert ITEM to visible."
  (let (a)
    (cond
     ((numberp item) (abs item))
     ((setq a (assq item darcsum-invisible-item-alist)) (cdr a))
     (t item))))

(defun darcsum-invisible-item (item)
  "Convert ITEM to invisible."
  (let (a)
    (cond
     ((numberp item) (- (abs item)))
     ((setq a (rassq item darcsum-invisible-item-alist)) (car a))
     (t item))))

(defun darcsum-toggle-item (item)
  "Mark visible change ITEM as invisible and vice versa."
  (let (a)
    (cond
     ((numberp item) (- item))
     ((setq a (assq item darcsum-invisible-item-alist)) (cdr a))
     ((setq a (rassq item darcsum-invisible-item-alist)) (car a))
     (t item))))

(defconst darcsum-item-status-alist
  '((addfile . "Added")
    (newfile . "New")
    (rmfile . "Removed")
    (binary . "Modified binary")))

(defun darcsum-item-status (item)
  "Return file-status displayed with ITEM."
  (cdr (assq (darcsum-visible-item item) darcsum-item-status-alist)))

(eval-and-compile
  (if (fboundp 'make-temp-file)
      (defalias 'darcsum-make-temp-file 'make-temp-file)
    ;; make-temp-name generates a unique name when it is called, but
    ;; takes no provisions to ensure that it will remain unique. Thus,
    ;; there is a race condition before we use the name. This is
    ;; probably a bad thing.
    (defalias 'darcsum-make-temp-file 'make-temp-name)))

(defsubst darcsum-change-item (change)
  (if (listp (car change))
      (caar change)
    (car change)))

(defsubst darcsum-change-line (change)
  (let ((ch (darcsum-change-item change)))
    (if (symbolp ch)
        1
      ch)))

(defun darcsum-applicable-p (data predicate)
  (catch 'exit
    (ignore
     (let (dir file change)
       (dolist (dir data)
         (dolist (file (cdr dir))
           (dolist (change (cdr file))
             (if (funcall predicate (car dir) (car file) change)
                 (throw 'exit t)))))))))

(defsubst darcsum-marked-p (data)
  (darcsum-applicable-p data (function
                              (lambda (dir file change)
                                (listp (car change))))))

(defsubst darcsum-changeset-has-change-p (data odir ofile start-line replace)
  (darcsum-applicable-p
   data (function
         (lambda (d f change)
           (and (equal odir d)
                (equal ofile f)
                (eq start-line (darcsum-change-item change))
                (darcsum-item-visible-p (darcsum-change-item change))
                (or (not (eq start-line 'replace))
                    (equal (cadr change) replace)))))))

(defun darcsum-changeset-has-directory-p (changeset name)
  (catch 'exit
    (ignore
     (let (dir)
       (dolist (dir changeset)
         (if (string= name (car dir))
             (throw 'exit t)))))))

(defun darcsum-find-changeset (data predicate)
  (let (dir file change changeset)
    (dolist (dir data)
      (dolist (file (cdr dir))
        (dolist (change (cdr file))
          (if (funcall predicate (car dir) (car file) change)
              (setq changeset
                    (darcsum-add-changeset
                     changeset
                     (list (list (car dir) (list (car file) change)))))))))
    changeset))

(defun darcsum-apply-to-changeset (data func)
  (let (dir file change)
    (dolist (dir data)
      (dolist (file (cdr dir))
        (dolist (change (cdr file))
          (funcall func (car dir) (car file) change))))))

(defun darcsum-remove-changeset (data changeset)
  "Remove DATA from the current CHANGESET."
  (let (dir file change)
    (dolist (dir changeset)
      (dolist (file (cdr dir))
        (dolist (change (cdr file))
          (let* ((dentry (assoc (car dir) data))
                 (fentry (assoc (car file) (cdr dentry))))
            (setcdr fentry (delete (assoc (car change) (cdr fentry))
                                   (cdr fentry)))
            (unless (cdr fentry)
              (setcdr dentry (delete fentry (cdr dentry))))
            (unless (cdr dentry)
              (setq data (delete dentry data))))))))
  data)

(defconst darcsum-item-numeric-alist
  '((addfile . 0)
    (newfile . 0)
    (rmfile . 0)
    (binary . 0)
    (replace . 999999)))

(defun darcsum-change-< (l r)
  (setq l (car l)
        l (if (listp l) (car l) l)
        l (darcsum-visible-item l)
        l (or (cdr (assq l darcsum-item-numeric-alist)) l))
  (setq r (car r)
        r (if (listp r) (car r) r)
        r (darcsum-visible-item r)
        r (or (cdr (assq r darcsum-item-numeric-alist)) r))
  (< l r))

(defun darcsum-add-changeset (data changeset)
  "Add DATA to the current CHANGESET."
  (let (dir file change)
    (dolist (dir changeset)
      (dolist (file (cdr dir))
        (dolist (change (cdr file))
          (let ((dentry (assoc (car dir) data)))
            (if dentry
                (let ((fentry (assoc (car file) dentry)))
                  (if fentry
                      (unless (member change (cdr fentry))
                        (nconc fentry (list change))
                        (setcdr fentry
                                (sort (cdr fentry)
                                      (function darcsum-change-<))))
                    (nconc dentry (list (list (car file) change)))))
              (setq data (cons (list (car dir)
                                     (list (car file) change))
                               data))))))))
  data)

(defun darcsum-merge-changeset (data changeset)
  "Merge DATA into the current CHANGESET."
  (let (dir file change final-data)
    (dolist (dir changeset)
      (dolist (file (cdr dir))
        (dolist (change (cdr file))
          (let ((dentry (assoc (car dir) data)))
            (if dentry
                (let ((fentry (assoc (car file) dentry))
                      (item (darcsum-change-item change)))
                  (if fentry
                      (unless
                          (or (assoc item (cdr fentry))
                              (assoc (darcsum-toggle-item item) (cdr fentry))
                              (assoc (list item) (cdr fentry)))
                        (nconc fentry (list change))
                        (setcdr fentry
                                (sort (cdr fentry)
                                      (function darcsum-change-<))))
                    (nconc dentry (list (list (car file) change)))))
              (setq data (cons (list (car dir)
                                     (list (car file) change))
                               data)))))))
    (dolist (dir data)
      (dolist (file (cdr dir))
        (dolist (change (cdr file))
          (let* ((dentry (assoc (car dir) changeset))
                 (fentry (assoc (car file) dentry))
                 (item (darcsum-change-item change))
                 final-dentry final-fentry)
            (when (and dentry fentry
                       (or (assoc item (cdr fentry))
                           (assoc (darcsum-toggle-item item) (cdr fentry))
                           (assoc (list item) (cdr fentry))))
              (unless (setq final-dentry (assoc (car dir) final-data))
                (setq final-data (cons (list (car dir)) final-data)
                      final-dentry (assoc (car dir) final-data)))
              (unless (setq final-fentry (assoc (car file) final-dentry))
                (nconc final-dentry (list (list (car file))))
                (setq final-fentry (assoc (car file) final-dentry)))
              (nconc final-fentry (list change)))))))
    (nreverse final-data)))

(defun darcsum-parse-changeset (&optional pending visible)
  "Return the patch in the current buffer as a Lisp changeset."
  (forward-line)
  (let ((limit (* 10 (count-lines (point-min) (point-max))))
        data entries)
    (while (and (not (or (eobp) (looking-at "^}")))
                (> limit 0))
      (setq limit (1- limit))
      (cond
       ((looking-at "^adddir\\s-+\\(.+?\\)$")
        (forward-line))
       ((looking-at "^rmdir\\s-+\\(.+?\\)$")
        (forward-line))
       ((looking-at "^move\\s-+\\(.+?\\)$")
        (forward-line))
       ((looking-at "^\\(old\\|new\\)hex$")
        (forward-line)
        (while (looking-at "^\\*")
          (forward-line)))
       ((looking-at "^\\(addfile\\|binary\\|rmfile\\|hunk\\|replace\\)\\s-+\\(.+?\\)\\(\\s-+\\([0-9]+\\|.+\\)\\)?$")
        (forward-line)
        (let* ((kind (match-string 1))
               (file (match-string 2))
               (dir (directory-file-name (file-name-directory file)))
               (base (file-name-nondirectory file))
               (start-line (match-string 4))
               (add-dir dir)
               item lines)
          (cond
           ((string= kind "hunk")
            (when start-line
              (while (looking-at "^\\([+ -].*\\)")
                (setq lines (cons (match-string 1) lines))
                (forward-line)))
            (setq item (string-to-number start-line))
            (setq entries
                  (cons (if visible item (- item))
                        (nreverse lines))))
           (t
            (setq item (intern kind)
                  item (if (and
                            (eq item 'addfile)
                            (not (or (eq pending t)
                                     (darcsum-changeset-has-directory-p
                                      pending dir))))
                           'newfile
                         item)
                  entries (list (if visible item (darcsum-toggle-item item))
                                (if (eq item 'replace) start-line)))))
          (let ((entry (assoc dir data)))
            (if (null entry)
                (setq data
                      (cons (cons dir (list (cons base
                                                  (list entries)))) data))
              (if entry
                  (let ((item (assoc base entry)))
                    (if item
                        (nconc item (list entries))
                      (nconc entry
                             (list (cons base (list entries)))))))))))
;       ((looking-at "^replace\\s-+\\(.+?\\)\\s-+\\(.*\\)+$")
;       (forward-line))
       ))
    (assert (>= limit 0))
    (nreverse data)))

(defun darcsum-read-changeset (&optional visible)
  (let ((pending
         (if (file-readable-p "_darcs/patches/pending")
             (with-temp-buffer
               (insert-file-contents "_darcs/patches/pending")
               (darcsum-parse-changeset t)))))
    (goto-char (point-min))
    (when (looking-at "^What's new in \"\\([^\"]*\\)\":")
      (forward-line 2))
    (if (looking-at "^{")
        (darcsum-parse-changeset pending visible))))


(defun darcsum-display-changeset (data)
  "Display the changeset DATA using a pcl-cvs-like buffer."
  (erase-buffer)
  ;;(when (file-readable-p "_darcs/prefs/lastrepo")
  ;;  (insert "repository : ")
  ;;  (insert-file-contents "_darcs/prefs/lastrepo")
  ;;  (goto-char (point-max)))
  (insert "Working dir: " default-directory "\n\n\n")
  (unless data
    (insert "There are no changes to review.\n"))
  (let (dir file change line beg)
    (dolist (dir data)
      (insert
       (darcsum-add-props
        (concat "in directory "
                (darcsum-add-face (concat (car dir))
                                  'darcsum-header-face t)
                ":\n")
        'darcsum-line-type 'dir
        'darcsum-dir (car dir)))
      (dolist (file (cdr dir))
        (let* ((all-marked (listp (car (cadr file))))
               (action (darcsum-change-item (cadr file)))
               (status (darcsum-item-status action)))
          (when (not status)
            (setq all-marked t)
            (dolist (change (cdr file))
              (if (and all-marked
                       (not (listp (car change))))
                  (setq all-marked nil))))
          (insert
           (darcsum-add-props
            (concat "          "
                    (if (and status (darcsum-item-visible-p action))
                        (darcsum-add-face " * " 'darcsum-change-line-face t)
                      "   ")
                    " "
                    (darcsum-add-face (format "%-24s"
                                              (if status status "Modified"))
                                      (if all-marked
                                          'darcsum-need-action-marked-face
                                        'darcsum-need-action-face) t)
                    (darcsum-add-face (concat (car file))
                                      'darcsum-filename-face t) "\n")
            'darcsum-line-type 'file
            'darcsum-dir (car dir)
            'darcsum-file (car file)))
          (dolist (change (if status nil (cdr file)))
          (let ((item (darcsum-change-item change)))
            (setq beg (point))
            (cond
             ((eq 'replace item)
              (insert (darcsum-add-face
                       "replace   "
                       'darcsum-change-line-face t)
                      (format " %s" (cadr change))
                      ?\n)
              (add-text-properties beg (point)
                                   (list 'darcsum-line-type 'change
                                         'darcsum-dir (car dir)
                                         'darcsum-file (car file)
                                         'darcsum-change change)))
             ((symbolp item)
              ;; 'addfile 'newfile 'rmfile 'binary or '-replace
              ;; xyzzy
              )
             ((> item 0)
              (insert
               (darcsum-add-face
                (format "%-10d" item)
                'darcsum-change-line-face t))
              ;; Avoid trailing whitespace here, so that we could use
              ;; `show-trailing-whitespace' in Emacs, but make it
              ;; display as space.  \000 is unlikely to be searched
              ;; for.  NB "" as display property loses.
              (if (boundp 'show-trailing-whitespace)
                  (if (fboundp 'propertize)
                      (insert (propertize "\000" 'display " "))))
              (insert ?\n)
              (dolist (line (cdr change))
                (insert (if (not (listp (car change)))
                            line
                          (darcsum-add-face (concat line)
                                            'darcsum-marked-face t))
                        ?\n))
              (add-text-properties beg (point)
                                   (list 'darcsum-line-type 'change
                                         'darcsum-dir (car dir)
                                         'darcsum-file (car file)
                                         'darcsum-change change))))))))))
  (insert "
--------------------- End ---------------------\n"))

;;; Code to determine the current changeset in darcsum-mode

(defun darcsum-changeset-at-point (&optional invisible-too)
  (let* ((type (get-text-property (point) 'darcsum-line-type))
         (dir (get-text-property (point) 'darcsum-dir))
         (dentry (and dir (assoc dir darcsum-data)))
         data)
    (cond
     ((eq type 'dir)
      (setq data (list dentry)))
     ((eq type 'file)
      (let* ((file (get-text-property (point) 'darcsum-file))
             (fentry (assoc file dentry)))
        (setq data (list (list (car dentry) fentry)))))
     ((eq type 'change)
      (let* ((file (get-text-property (point) 'darcsum-file))
             (fentry (assoc file dentry)))
        (setq data (list
                    (list (car dentry)
                          (list (car fentry)
                                (get-text-property (point)
                                                   'darcsum-change))))))))
    (if invisible-too
        data
      (darcsum-find-changeset data
                              (function
                               (lambda (dir file change)
                                 (setq change (darcsum-change-item change))
                                 (or (symbolp change) (>= change 0))))))))

(defun darcsum-selected-changeset (&optional all-visible)
  "Return the currently selected changeset.

If marks are active, always returned the marked changes.
Otherwise, return the changes related to point, unless ALL-VISIBLE is
non-nil, in which case return all visible changes."
  (cond
   ((darcsum-marked-p darcsum-data)
    (darcsum-find-changeset darcsum-data
                            (function
                             (lambda (dir file change)
                               (listp (car change))))))
   (all-visible
    (darcsum-find-changeset darcsum-data
                            (function
                             (lambda (dir file change)
                               (equal (darcsum-visible-item (car change))
                                      (car change))))))
   (t
    (darcsum-changeset-at-point))))

;;; Code to record the current changeset

;; If there are any marked changes, these are what get recorded.
;; Otherwise, all *visible* changes are recorded.

(defcustom darcsum-program "darcs"
  "*The program name which darcsum will use to invoke darcs."
  :type 'string
  :group 'darcsum)

(defcustom darcsum-default-expanded nil
  "*Non-nil means the *darcsum* buffer will be expanded by default."
  :type 'boolean
  :group 'darcsum)

(defvar darcsum-process-arg nil)
(defvar darcsum-parent-buffer nil)
(defvar darcsum-changeset-to-record nil)
(defvar darcsum-logfile)

(defvar darcsum-window-configuration-temp nil)

(defsubst darcsum-remember-window-configuration ()
  (setq darcsum-window-configuration-temp (list (current-window-configuration)
                                           (point-marker))))
(defsubst darcsum-recall-window-configuration ()
  (if darcsum-window-configuration-temp
      (progn
         (set-window-configuration (car darcsum-window-configuration-temp))
         (goto-char (cadr darcsum-window-configuration-temp)))
    (error "No window configuration to restore.")))

(defsubst darcsum-changes-handled ()
  (if (buffer-live-p darcsum-parent-buffer)
      (let ((changeset darcsum-changeset-to-record))
        (with-current-buffer darcsum-parent-buffer
          (setq darcsum-data
                (darcsum-remove-changeset darcsum-data changeset))
          (darcsum-refresh)))))

(defun darcsum-start-process (subcommand args
                                         &optional name value &rest localize)
  "Start darcs process."
    (let*
        ((buf (generate-new-buffer (format " *darcs %s*" subcommand)))
         (process-environment
            ;; Use the environment variables to turn off highlighting.  (You
            ;; could use `show-trailing-whitespace' in the buffer to highlight
            ;; trailing space in the diffs.)
            (append (list "DARCS_DONT_ESCAPE_TRAILING_SPACES=1"
                          "DARCS_DONT_COLOR=1"
                          "DARCS_DONT_ESCAPE_TRAILING_CR=1")
                    process-environment))
         (process-connection-type nil)
         (proc (apply 'start-process "darcs"
                      buf darcsum-program subcommand args)))
      (set-process-sentinel proc 'darcsum-process-sentinel)
      (set-process-filter proc 'darcsum-process-filter)
      (with-current-buffer buf
        (while name
          (set (make-local-variable name) value)
          (setq name (car localize)
                value (cadr localize)
                localize (cddr localize))))
      proc))

(defun darcsum-process-sentinel (proc string)
  (cond
   ((and (string-match "^exited abnormally" string) (process-buffer proc))
    (message string))))

(defun darcsum-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((looking-at "\n*Finished \\(recording\\|amending\\) patch")
        (message "Changes recorded.")
        (darcsum-changes-handled)
        (when darcsum-logfile (delete-file darcsum-logfile))
        (kill-buffer (current-buffer)))
       ((looking-at "\n*Ok, if you don't want to \\(record\\|amend\\) anything")
        (message "No changes recorded.")
        (when darcsum-logfile (delete-file darcsum-logfile))
        (kill-buffer (current-buffer)))

       ((looking-at "\n*What is the target email address")
        (process-send-string proc darcsum-process-arg)
        (delete-region (point-min) (point-max)))
       ((looking-at "\n*Successfully sent patch bundle")
        (message "Changes sent to `%s'." darcsum-process-arg)
        (kill-buffer (current-buffer)))
       ((looking-at "\n*You don't want to send any patches")
        (message "No changes sent.")
        (kill-buffer (current-buffer)))

       ((looking-at "\n*Do you really want to .+\\? ") ;; Should the last whitespace be there?
        (process-send-string proc "y\n")
        (delete-region (point-min) (point-max)))
       ((looking-at "\n*Finished reverting.")
        (message "Changes reverted.")
        (darcsum-changes-handled)
        (kill-buffer (current-buffer)))
       ((looking-at "\n*If you don't want to revert")
        (message "No changes reverted.")
        (kill-buffer (current-buffer)))

       ((looking-at "\n*\\(Waiting for lock.*\\)\n+")
        (let ((waiting (match-string 1)))
          (message waiting)
          (delete-region (point-min) (match-end 0))))

       ((looking-at "\n*\\(Couldn't get lock.*\\)\n*")
        (let ((waiting (match-string 1)))
          (message waiting)
          (kill-buffer (current-buffer))))

       ((looking-at "\\(.*\n\\)*Shall I amend this patch\\?.*")
        (process-send-string proc "y")
        (delete-region (point-min) (match-end 0)))

       ((looking-at "\n*Darcs needs to know what name")
        (let* ((default-mail (concat user-full-name
                                     " <" user-mail-address ">"))
               (enable-recursive-minibuffers t)
               (mail-address (read-string
                              (format
                               "What is your email address? (default %s) "
                               default-mail)
                              nil nil default-mail)))
          (process-send-string proc mail-address)
          (process-send-string proc "\n"))
        (re-search-forward "What is your email address\\?.*")
        (delete-region (point-min) (point)))

       ((looking-at "\n*\\(addfile\\|adddir\\|binary\\|rmfile\\|hunk\\|replace\\)\\s-+\\(.+?\\)\\(\\s-+\\([0-9]+\\)?\\)?\\( \\(.+\\)\\)?$")
        (let* ((kind (intern (match-string 1)))
               (file (match-string 2))
               (dir (directory-file-name
                     (file-name-directory file)))
               (base (file-name-nondirectory file))
               (start-line (match-string 4))
               (replace (match-string 6)))
          (goto-char (match-end 0))
          (forward-line)
          (while (looking-at "^\\([+-].*\\)")
            (forward-line))
          (when (looking-at
                 "^Shall I \\(record\\|send\\|revert\\|add\\) this \\(patch\\|change\\)\\?.+[]:] ")
            (if (eq kind 'hunk) (setq kind (string-to-number start-line)))
            (let ((end (match-end 0))
                  (record (darcsum-changeset-has-change-p
                           darcsum-changeset-to-record
                           dir base kind replace)))
              (process-send-string proc (if record "y" "n"))
              (delete-region (point-min) end)))))

       ((looking-at "\n*\\(move\\).+")
        (goto-char (match-end 0))
        (forward-line)
        (when (looking-at
               "^Shall I \\(record\\|send\\|revert\\|add\\) this \\(patch\\|change\\)\\?.+[]:] ")
          (let ((end (match-end 0)))
            (process-send-string proc "n")
            (delete-region (point-min) end))))))))

(defun darcsum-really-record ()
  (interactive)
  (let ((tempfile (darcsum-make-temp-file "darcsum"))
        (parent-buf darcsum-parent-buffer)
        (changeset darcsum-changeset-to-record))
    (save-excursion
      (goto-char (point-max))
      (unless (bolp)
        (insert ?\n))
      (goto-char (point-min))
      (when (looking-at "^\\s-*$")
        (error "No record description entered")))
    (write-region (point-min) (point-max) tempfile)
    (kill-buffer (current-buffer))
    (darcsum-recall-window-configuration)
    (message "Recording changes...")
    ;;;;;;;; TODO: optionally pass in e.g. --no-test somehow
    (darcsum-start-process
     "record" (list "--logfile" tempfile)
     'darcsum-logfile tempfile
     'darcsum-changeset-to-record changeset
     'darcsum-parent-buffer parent-buf)))

(defun darcsum-record ()
  "Record selected changeset.
Note that only changes selected for recording are actually recorded.
If some changes are marked \(with \
\\<darcsum-mode-map>\\[darcsum-toggle-mark]\), \
then only those changes are recorded.
Otherwise, only changes which are selected to be displayed in the buffer
\(with \\<darcsum-mode-map>\\[darcsum-toggle]\) are recorded."
  (interactive)
  (darcsum-remember-window-configuration)
  (let ((parent-buf (current-buffer))
        (changeset (darcsum-selected-changeset t))
        (buf (get-buffer-create "*darcs comment*")))
    (switch-to-buffer-other-window buf)
    (darcsum-comment-mode)
    (set (make-local-variable 'darcsum-changeset-to-record) changeset)
    (set (make-local-variable 'darcsum-parent-buffer) parent-buf)
    (message
     "Title of change on first line, long comment after.  \
C-c C-c to record.")
    (run-hooks 'darcsum-comment-hook)))

(defun darcsum-send (recipient)
  "Send selected changeset via email."
  (interactive "sSend changes to: ")
  (message "Sending changes...")
  (darcsum-start-process
   "send" (list)
   'darcsum-changeset-to-record (darcsum-selected-changeset t)
   'darcsum-parent-buffer (current-buffer)
   'darcsum-process-arg recipient))

(defun darcsum-changes (&optional how-many)
  "Show the changes in another buffer"
  (interactive "P")
  (let ((proc (darcsum-start-process
               "changes" (if how-many
                             (list "--last" (number-to-string how-many))
                             (list))
               'darcsum-parent-buffer (current-buffer))))
    (set-process-filter proc nil)
    (set-process-sentinel proc 'darcsum-changes-sentinel)
    (switch-to-buffer-other-window (process-buffer proc))
    (process-buffer proc)))

(defun darcsum-changes-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (darcsum-changes-mode)
    (goto-char (point-min))))

(defun darcsum-query-manifest ()
  "List the version-controlled files in the working copy."
  (interactive)
  (let ((proc (darcsum-start-process
               "query" '("manifest")
               'darcsum-parent-buffer (current-buffer))))
    (set-process-filter proc nil)
    (set-process-sentinel proc 'darcsum-query-manifest-sentinel)
    (switch-to-buffer-other-window (process-buffer proc))
    (process-buffer proc)))

(defun darcsum-query-manifest-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (setq buffer-read-only t)
    (darcsum-query-mode)
    (goto-char (point-min))))

(defun darcsum-amend ()
  "Amend last patch with selected changeset."
  (interactive)
  (let ((changeset (darcsum-selected-changeset t))
        (parent-buffer (current-buffer)))
    (if (> (length changeset) 0)
        (let ((history-buffer (darcsum-changes 1)))
          (with-current-buffer history-buffer
            (save-excursion
              (goto-char (point-max))
              (insert "
WARNINGS: You should ONLY use amend-record on patches which only exist in a single repository!
Also, running amend-record while another user is pulling from the same repository may cause repository corruption."))
            (sleep-for 2)
            (goto-char (point-min)))
          (let (amend (yes-or-no-p "Amend this latest changeset? (see WARNINGS) "))
          (kill-buffer history-buffer)
          (when amend
            (darcsum-start-process
             "amend" (list)
             'darcsum-logfile nil
             'darcsum-changeset-to-record changeset
             'darcsum-parent-buffer parent-buffer))))
        (message "You need to select something first"))))

(defun darcsum-revert ()
  "Revert selected changeset."
  (interactive)
  (when (yes-or-no-p "Really revert these changes? ")
    (message "Reverting changes...")
    (darcsum-start-process
     "revert" (list)
     'darcsum-changeset-to-record (darcsum-selected-changeset t)
     'darcsum-parent-buffer (current-buffer))))

;;;;;;;; TODO: history of previous record comments, like in vc-mode
(defvar darcsum-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'darcsum-really-record)
    (define-key map "\C-c\C-c" 'darcsum-really-record)
    map))

(defun darcsum-kill-ancillary-buffer ()
  "Kill an ancillary buffer called by darcsum."
  (interactive)
  (kill-this-buffer)
  (delete-window))

(defun darcsum-changes-mode-next-comment (&optional n)
  "Move to the next comment.

If called with a positive argument then move N comments forward."
  (interactive "p")
  (if (and n (< 0 n))
      (let ((comment-start-regexp "^[A-Z][a-z]\\{2\\} [A-Z][a-z]\\{2\\}.*$"))
	(when (looking-at comment-start-regexp)
	  (forward-line 1))
	(let ((next (re-search-forward comment-start-regexp (point-max) t (or n 1))))
	  (if next
	      (goto-char (point-at-bol))
	    (message "No earlier changes"))))
    (darcsum-changes-mode-previous-comment n)))

(defun darcsum-changes-mode-previous-comment (&optional n)
  "Move to the previous comment.

If called with a positive argument then move N comments backward."
  (interactive "p")
  (when (and n (< n 0))
    (error "To move forward call `darcsum-changes-mode-next-comment' instead"))
  (let ((comment-start-regexp "^[A-Z][a-z]\\{2\\} [A-Z][a-z]\\{2\\}.*$"))
    (when (looking-at comment-start-regexp)
      (forward-line -1))
    (let ((next (re-search-backward comment-start-regexp (point-min) t (or n 1))))
      (if next
	  (goto-char (point-at-bol))
	(message "No later changes")))))

(defun darcsum-query-kill-buffer ()
  (interactive)
  (kill-this-buffer)
  (delete-window))

(defvar darcsum-query-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'darcsum-query-kill-buffer)
    map))

(defvar darcsum-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'darcsum-kill-ancillary-buffer)
    (define-key map "n" 'darcsum-changes-mode-next-comment)
    (define-key map "p" 'darcsum-changes-mode-previous-comment)
    map))

(define-derived-mode darcsum-comment-mode indented-text-mode "Darcs Summary"
  "Major mode for output from \\<darcsum-mode-map>\\[darcsum-comment].

\\{darcsum-comment-mode-map}"
  :group 'darcsum
  (setq truncate-lines t))

(define-derived-mode darcsum-query-mode indented-text-mode "Darcs Query"
  "Major mode for output from \\<darcsum-mode-map>\\[darcsum-query].

\\{darcsum-query-mode-map}"
  :group 'darcsum
  (setq truncate-lines t))

(define-derived-mode darcsum-changes-mode indented-text-mode "Darcs Changes"
  "Major mode for output from \\<darcsum-mode-map>\\[darcsum-changes].

\\{darcsum-comment-mode-map}"
  :group 'darcsum
  (setq truncate-lines nil)
  (setq buffer-read-only t))

;;; Major Mode

(defun darcsum-check-darcsum-mode ()
  (unless (eq major-mode 'darcsum-mode)
    (error "Not in a darcsum-mode")))

(defun darcsum-reposition ()
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir)
      (goto-char (+ (line-beginning-position) 13)))
     ((eq type 'file)
      (goto-char (+ (line-beginning-position) 38)))
     ((eq type 'change)
      (goto-char (line-beginning-position))))))

(defsubst darcsum-other-buffer (other-buffer)
  (let ((buf (or other-buffer (generate-new-buffer "*darcs*"))))
    (with-current-buffer buf
      (unless (eq major-mode 'darcsum-mode)
        (darcsum-mode))
      (current-buffer))))

(defun darcsum-move (other-buffer)
  "Move the selected changeset to another darcsum buffer OTHER-BUFFER.

In interactive mode, prompts for the name of a buffer to move the changeset to.

Changesets may be moved around in different buffers, to ease
the collection of changes to record in a single darcs patch."
  (interactive "BMove change to (RET creates new patch): ")
  (let ((buf (darcsum-other-buffer other-buffer))
        (changeset (darcsum-selected-changeset))
        (inhibit-redisplay t))
    (setq darcsum-data
          (darcsum-remove-changeset darcsum-data changeset))
    (with-current-buffer buf
      (darcsum-apply-to-changeset
       changeset
       (function
        (lambda (dir file change)
          (cond
           ((listp (car change))
            (setcar change (caar change)))
           ((not (equal (car change) (darcsum-visible-item (car change))))
            (setcar change (darcsum-visible-item (car change))))))))
      (setq darcsum-data
            (darcsum-add-changeset darcsum-data changeset))
      (darcsum-refresh)))
  (darcsum-refresh))

(defun darcsum-find-file (e &optional other view)
  "Open the selected entry.
With a prefix OTHER, open the buffer in another window.
VIEW non-nil means open in View mode."
  (interactive (list last-input-event current-prefix-arg))
  (let* ((type (get-text-property (point) 'darcsum-line-type))
         (file (if (eq 'type 'dir)
                   (get-text-property (point) 'darcsum-dir)
                 (darcsum-path (point)))))
    (cond
     ((eq type 'dir)
      (find-file (get-text-property (point) 'darcsum-dir)))
     ((eq type 'file)
      (cond ((eq other 'dont-select)
             (display-buffer (find-file-noselect file)))
            ((and other view)
             (view-file-other-window file))
            (view (view-file file))
            (other (find-file-other-window file))
            (t (find-file file))))
     ((eq type 'change)
      (let ((change-line (car (get-text-property (point) 'darcsum-change))))
        (with-current-buffer (cond ((eq other 'dont-select)
                                    (display-buffer (find-file-noselect file)))
                                   ((and other view)
                                    (view-file-other-window file))
                                   (view (view-file file))
                                   (other (find-file-other-window file))
                                   (t (find-file file)))
          (if (listp change-line)
              (setq change-line (car change-line)))
          (goto-line (abs change-line))))))))

(defun darcsum-find-file-other-window (e)
  "Select a buffer containing the file in another window."
  (interactive (list last-input-event))
  (darcsum-find-file e t))

(defun darcsum-goto ()
  "Open the selected entry, possibly moving point to the change's location."
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir)
      (find-file-other-window
       (get-text-property (point) 'darcsum-dir)))
     ((eq type 'file)
      (find-file-other-window (darcsum-path (point))))
     ((eq type 'change)
      (let ((change-line (car (get-text-property (point) 'darcsum-change))))
        (find-file-other-window (darcsum-path (point)))
        (if (listp change-line)
            (setq change-line (car change-line)))
        (goto-line (abs change-line)))))))

(defun darcsum-toggle-context ()
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((dir default-directory)
        (darcsum-default-expanded t)
        darcsum-show-context (not darcsum-show-context))
    (message "Re-running darcsum-whatsnew")
    (let ((changes (darcsum-whatsnew dir nil t darcsum-show-context)))
      (setq darcsum-data changes))
    (darcsum-refresh)))

(defun darcsum-toggle-mark ()
  "Toggle mark on current changeset.

Marked changesets have priority over simply activated ones regarding
the selection of changesets to commit."
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((changeset (darcsum-changeset-at-point t)))
    (darcsum-apply-to-changeset changeset
                                (function
                                 (lambda (dir file change)
                                   (if (listp (car change))
                                       (setcar change (caar change))
                                     (setcar change (list (car change))))))))
  (darcsum-refresh))

(defun darcsum-toggle ()
  "Toggle the activation of the current changeset.

The activation of a changeset exposes the associated change, and selects
it for later commit."
  (interactive)
  (darcsum-check-darcsum-mode)
  ;;;;;;;; TODO: easier to expose a hunk which was made invisible by mistake
  (let ((changeset (darcsum-changeset-at-point t)))
    (let ((any-visible
           (darcsum-applicable-p
            changeset
            (function
             (lambda (d f change)
               (darcsum-item-visible-p (darcsum-change-item change)))))))
        (darcsum-apply-to-changeset
         changeset
         (function
          (lambda (dir file change)
            (let ((item (darcsum-change-item change)))
              (if any-visible
                  (setcar change (darcsum-invisible-item item))
                (if (listp (car change))
                    (setcar change (list (darcsum-visible-item item)))
                  (setcar change (darcsum-visible-item item))))))))))
  (darcsum-refresh))

(defun darcsum-refresh ()
  "Refresh the visualization of the changesets."
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((line (count-lines (point-min) (point)))
        (inhibit-redisplay t))
    (if (/= (point) (line-beginning-position))
        (setq line (1- line)))
    (darcsum-display-changeset darcsum-data)
    (goto-char (point-min))
    (forward-line line)
    (darcsum-reposition)))

(defun darcsum-line-is (sort)
  (save-excursion
    (beginning-of-line)
    (let ((type (get-text-property (point) 'darcsum-line-type)))
      (case sort
        ('new (and (eq 'file type) (looking-at "  +New")))
        ('modified (or (and (eq 'file type) (looking-at "\\s-+Modified"))
                       (eq 'change type)))
        ('file (eq 'file type))
        ('change (eq 'change type))
        ('marked
         (memq (get-text-property (point) 'face)
               '(darcsum-marked-face darcsum-need-action-marked-face)))))))

(defun darcsum-next-entity (&optional arg backward)
  "Move to the next file or change.
With ARG, move that many times.
BACKWARD non-nil means to go backwards."
  (interactive "p")
  (dotimes (i (or arg 1))
    (forward-line (if backward -1))
    (beginning-of-line)
    (while (and (not (if backward (bobp) (eobp)))
                (not (looking-at "[0-9]")) ; stop at line headers
                (darcsum-line-is 'change))
      (forward-line (if backward -1 1)))
    (unless (get-text-property (point) 'darcsum-line-type)
      (goto-char (if backward (point-max) (point-min)))
      (forward-line (if backward -3 3)))
    (darcsum-reposition)))

(defun darcsum-next-line (&optional arg)
  "Move to the next file or change.
With ARG, move that many times."
  (interactive "p")
  (darcsum-next-entity arg))

(defun darcsum-previous-line (&optional arg)
  "Move to the previous file or change.
With ARG, move that many times."
  (interactive "p")
  (darcsum-next-entity arg t))

(defun darcsum-original-path (pos)
  (let ((file (get-text-property pos 'darcsum-file))
        (dir (get-text-property pos 'darcsum-dir)))
    (let ((path (expand-file-name       ; new-style
                 file (file-name-as-directory
                       (expand-file-name dir "_darcs/pristine")))))
      (if (file-readable-p path)
          path
        (let ((path (expand-file-name   ; old-style
                     file (file-name-as-directory
                           (expand-file-name dir "_darcs/current")))))
          (if (file-readable-p path)
              path))))))

(defun darcsum-path (pos)
  (expand-file-name (get-text-property pos 'darcsum-file)
                    (file-name-as-directory
                     (get-text-property pos 'darcsum-dir))))

(defcustom darcsum-diff-switches nil
  "*diff(1) switches used by `darcsum-diff'."
  :type 'string
  :group 'darcsum)

(defun darcsum-diff ()
  "Show the changes made to current selection."
  (interactive)
  (if (not (darcsum-original-path (point)))
      (error "No record of this file in darcs"))
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
          (eq type 'change))
      (require 'diff)                   ; for `diff-switches'
      (diff (darcsum-original-path (point))
            (darcsum-path (point))
            (or darcsum-diff-switches diff-switches))))))

(defun darcsum-delete ()
  "Remove selected changeset from the view."
  (interactive)
  (setq darcsum-data
        (darcsum-remove-changeset darcsum-data
                                  (darcsum-selected-changeset)))
  (darcsum-refresh))

(defun darcsum-remove ()
  "Remove a file from the repository."
  (interactive)
  (darcsum-check-darcsum-mode)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir)
      (error "Cannot remove whole directories yet; try file by file for now"))
     ((memq type '(file change))
      (let* ((dir (get-text-property (point) 'darcsum-dir))
             (dentry (and dir (assoc dir darcsum-data)))
             (file (get-text-property (point) 'darcsum-file))
             (fentry (assoc file dentry))
             (sym (darcsum-change-item (cadr fentry)))
             file-to-remove)
        (cond
         ((not (symbolp sym))
          (when (yes-or-no-p
                 (format "Really delete file with changes `%s'? " file))
            (delete-file (expand-file-name file dir))
            (setq file-to-remove file)))
         ((eq sym 'newfile)
          (delete-file (expand-file-name file dir)))
         ((eq sym 'addfile)
          (setq file-to-remove file)
          (delete-file (expand-file-name file dir)))
         (t
          (error "Removing makes no sense for that entry")))
        (if file-to-remove
            (with-temp-buffer
              (cd (expand-file-name dir))
              (if (/= 0 (call-process darcsum-program nil t nil
                                      "remove" file-to-remove))
                  (error "Error running `darcsum remove'"))))))))
  (darcsum-redo))

(defun darcsum-add ()
  "Put new file or directory under Darcs control."
  (interactive)
  (darcsum-check-darcsum-mode)
  (dolist (dir (darcsum-selected-changeset))
    (dolist (file (cdr dir))
      (let ((item (darcsum-change-item (cadr file))))
        (if (and (symbolp item) (eq item 'newfile))
            (progn
              (setcar (cadr file) 'addfile)
              (with-temp-buffer
                (cd (expand-file-name (car dir)))
                (if (/= 0 (call-process darcsum-program nil t nil
                                        "add" (car file)))
                    (error "Error running `darcsum add' for `%s' in dir `%s'"
                           (car file) (car dir)))))
          (error "Can only add New entries for `%s' in dir `%s'"
                 (car file) (car dir))))))
  (darcsum-refresh))

(defun darcsum-add-to-boring (path)
  "Add current file or directory to the boring file.

Propose the insertion of a regexp suitable to permanently ignore
the file or the directory at point into the boring file."
  (interactive
   (let ((type (get-text-property (point) 'darcsum-line-type)))
     (cond
      ((eq type 'dir)
       (setq path (get-text-property (point) 'darcsum-dir))
       (if (string-match "^\\./" path)
           (setq path (substring path 2)))
       (setq path (concat "(^|/)" (regexp-quote path) "($|/)")))
      ((memq type '(file change))
       (setq path (get-text-property (point) 'darcsum-file))
       (setq path (concat "(^|/)" path "$"))))
     (list (read-string "Add to boring list: " path))))
  (save-excursion
    (set-buffer (find-file-noselect "_darcs/prefs/boring"))
    (goto-char (point-max))
    (insert path ?\n)
    (save-buffer)
    (kill-buffer (current-buffer)))
  (darcsum-redo))

(defun darcsum-add-change-log-entry ()
  "Execute `add-change-log-entry' on the current file."
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
          (eq type 'change))
      (darcsum-goto)
      (add-change-log-entry)))))

(defun darcsum-ediff ()
  "Like `darcsum-diff' but in an Ediff session."
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
          (eq type 'change))
      (let ( (pristine-filename (darcsum-original-path (point)))
             (working-filename (darcsum-path (point)))
             (old-window-configuration (current-window-configuration)) ;;Save the current window configuration, before opening ediff
             )
      (progn
        (save-excursion
          (find-file-read-only pristine-filename) ;;Pristine copy should not be modified
          (rename-buffer (concat "*darcsum-pristine:" pristine-filename "*")) ;;It should be clear this is not a buffer you want to touch.
          )
        (ediff pristine-filename working-filename
               ;;Add this anonymous function as a startup hook in ediff-mode
               (lambda () (progn
                            (setq pre-darcsum-ediff-window-configuration old-window-configuration)
                            ;;After we quit THIS PARTICULAR ediff buffer, restore the old window configuration
                            (add-hook 'ediff-quit-hook (lambda () (set-window-configuration pre-darcsum-ediff-window-configuration)) nil t)
                            )))
        ))))))

(defun darcsum-ediff-merge ()
  "Start an `ediff-merge' session on the current selection."
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
          (eq type 'change))
      (ediff-merge (darcsum-original-path (point))
                   (darcsum-path (point)))))))

(defun darcsum-redo (&optional arg)
  "Refresh the status, redoing `darcs whatsnew'."
  (interactive "P")
  (darcsum-check-darcsum-mode)
  (let ((dir default-directory)
        (look-for-adds (or arg darcsum-look-for-adds))
        (darcsum-default-expanded t))
    (message "Re-running darcsum-whatsnew")
    (let ((changes (darcsum-whatsnew
                    dir look-for-adds t darcsum-show-context)))
      (setq darcsum-data
            (darcsum-merge-changeset darcsum-data changes)))
    (darcsum-refresh)))

(defun darcsum-quit ()
  "Close the darcsum buffer and quit."
  (interactive)
  (darcsum-check-darcsum-mode)
  (kill-buffer (current-buffer)))


(defun darcsum-add-comment ()
  "Similar to `add-change-log-entry'.

Inserts the entry in the darcs comment file instead of the ChangeLog."
  ;; This is mostly copied from add-log.el and Xtla.  Perhaps it would
  ;; be better to split add-change-log-entry into several functions
  ;; and then use them, but that wouldn't work with older versions of
  ;; Emacs.
  (interactive)
  (require 'add-log)
  (let* ((defun (add-log-current-defun))
         (buf-file-name (if (and (boundp 'add-log-buffer-file-name-function)
                                 add-log-buffer-file-name-function)
                            (funcall add-log-buffer-file-name-function)
                          buffer-file-name))
         (buffer-file (if buf-file-name (expand-file-name buf-file-name)))
;         (file-name (tla-make-log))
         ;; Set ENTRY to the file name to use in the new entry.
         (entry (add-log-file-name buffer-file default-directory))
         beg
         bound
         narrowing)
    (switch-to-buffer-other-window (get-buffer-create "*darcs comment*"))

    (goto-char (point-min))
    (forward-line 1)                    ; skip header
    ;; Now insert the new line for this entry.
    (cond ((re-search-forward "^\\s *\\*\\s *$" bound t)
           ;; Put this file name into the existing empty entry.
           (if entry
               (insert entry)))
          ((let (case-fold-search)
             (re-search-forward
              (concat (regexp-quote (concat "* " entry))
                      ;; Don't accept `foo.bar' when
                      ;; looking for `foo':
                      "\\(\\s \\|[(),:]\\)")
              bound t))
           ;; Add to the existing entry for the same file.
           (re-search-forward "^\\s *$\\|^\\s \\*\\|\\'")
           (goto-char (match-beginning 0))
           ;; Delete excess empty lines; make just 2.
           (while (and (not (eobp)) (looking-at "^\\s *$"))
             (delete-region (point) (line-beginning-position 2)))
           (insert-char ?\n 2)
           (forward-line -2)
           (indent-relative-maybe))
          (t
           ;; Make a new entry.
           (goto-char (point-max))
           (re-search-backward "^." nil t)
           (end-of-line)
           (insert "\n* ")
           (if entry (insert entry))))
    ;; Now insert the function name, if we have one.
    ;; Point is at the entry for this file,
    ;; either at the end of the line or at the first blank line.
    (if defun
        (progn
          ;; Make it easy to get rid of the function name.
          (undo-boundary)
          (unless (save-excursion
                    (beginning-of-line 1)
                    (looking-at "\\s *$"))
            (insert ?\ ))
          ;; See if the prev function name has a message yet or not
          ;; If not, merge the two entries.
          (let ((pos (point-marker)))
            (if (and (skip-syntax-backward " ")
                     (skip-chars-backward "):")
                     (looking-at "):")
                     (progn (delete-region (+ 1 (point)) (+ 2 (point))) t)
                     (> fill-column (+ (current-column) (length defun) 3)))
                (progn (delete-region (point) pos)
                       (insert ", "))
              (goto-char pos)
              (insert "("))
            (set-marker pos nil))
          (insert defun "): "))
      ;; No function name, so put in a colon unless we have just a star.
      (unless (save-excursion
                (beginning-of-line 1)
                (looking-at "\\s *\\(\\*\\s *\\)?$"))
        (insert ": ")))))

(defvar darcsum-mode-abbrev-table nil
  "Abbrev table used while in darcsum-mode mode.")
(define-abbrev-table 'darcsum-mode-abbrev-table ())

(global-set-key "\C-xD" 'darcsum-add-comment)

(defvar darcsum-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [return] 'darcsum-toggle) ; ??
    (define-key map "\C-m" 'darcsum-toggle)
    (define-key map "\C-c\C-c" 'darcsum-goto)
    (define-key map [tab] 'darcsum-next-entity)
    (define-key map "?" 'describe-mode)
    (define-key map "f" 'darcsum-find-file)
    (define-key map "=" 'darcsum-diff)
    (define-key map "e" 'darcsum-ediff)
    (define-key map "E" 'darcsum-ediff-merge)
    (define-key map "g" 'darcsum-redo)
    (define-key map "n" 'darcsum-next-line)
    (define-key map "p" 'darcsum-previous-line)
    (define-key map "a" 'darcsum-add)
    (define-key map "l" 'darcsum-add-change-log-entry)
    (define-key map "c" 'darcsum-record)
    (define-key map "R" 'darcsum-record)
    (define-key map "U" 'darcsum-revert)
    (define-key map "u" 'darcsum-toggle-context)
    (define-key map "d" 'darcsum-delete)
    ;; (define-key map "r" 'darcsum-remove)
    (define-key map "M" 'darcsum-move)
    (define-key map "m" 'darcsum-toggle-mark)
    (define-key map "i" 'darcsum-add-to-boring)
    (define-key map "B" 'darcsum-add-to-boring)
    (define-key map "q" 'darcsum-quit)
    map))

(easy-menu-define darcsum-menu darcsum-mode-map "Menu used in `darcsum-mode'."
  '("Darcs summary"
    ["Open file.."              darcsum-find-file
                                (or (darcsum-line-is 'file)
                                    (darcsum-line-is 'change))]
    [" ..other window"          darcsum-find-file-other-window
                                (or (darcsum-line-is 'file)
                                    (darcsum-line-is 'change))]
    ["Display in other window"  darcsum-display-file    t]
    ("Differences"
     ["Interactive diff"        darcsum-ediff           t]
     ["Current diff"            darcsum-diff            t]
     ["Interactive merge"       darcsum-ediff-merge     t])
;;     ["View log"                      darcsum-log             t]
    "--"
    ["Re-examine"               darcsum-redo            t]
    ["Record changes"           darcsum-record          t] ; fixme: condition
    ["Amend last changeset"     darcsum-amend           t] ; fixme: condition
;;     ["Tag"                   darcsum-tag             t]
    ["Undo changes"             darcsum-revert          t] ; fixme: condition
    ["Add"                      darcsum-add             (darcsum-line-is 'new)]
    ["Remove"                   darcsum-remove          (darcsum-line-is 'file)]
    ["Ignore"                   darcsum-add-to-boring   (darcsum-line-is 'file)]
    ["Add ChangeLog"            darcsum-add-change-log-entry t]
    ["Delete"                   darcsum-delete          t]
    "--"
    ["(Un)activate change"      darcsum-toggle          t]
    ["(Un)mark change"          darcsum-toggle-mark
                                :style toggle
                                :selected (darcsum-line-is 'marked)]
    ["Next file/change"         darcsum-next-line       t]
    ["Previous file/change"     darcsum-previous-line   t]
    ["Move changeset"           darcsum-move            t]
    ["Show change context"      darcsum-toggle-context
                                :style toggle :selected darcsum-show-context]
    "--"
    ["Quit"                     darcsum-quit            t]
    ))

(define-derived-mode darcsum-mode fundamental-mode "Darcs"
  "Darcs summary mode is for previewing changes to become part of a patch.
\\{darcsum-mode-map}"
  :group 'darcsum
  (if (featurep 'xemacs)
      (easy-menu-add darcsum-menu darcsum-mode-map)))

(put 'darcsum-mode 'mode-class 'special)

(custom-add-option 'darcsum-mode-hook
                   '(lambda ()           ; Should be a minor mode for this!
                      "Show trailing whitespace in changes."
                      (setq show-trailing-whitespace t)))

;;; This is the entry code, M-x darcsum-whatsnew

(defun darcsum-display (data &optional look-for-adds)
  (with-current-buffer (generate-new-buffer "*darcs*")
    (darcsum-mode)
    (set (make-local-variable 'darcsum-data) data)
    (set (make-local-variable 'darcsum-look-for-adds) look-for-adds)
    (set (make-local-variable 'darcsum-show-context) nil)
    (darcsum-refresh)
    (goto-char (point-min))
    (forward-line 3)
    (darcsum-reposition)
    (switch-to-buffer (current-buffer))))

(defun darcsum-repository-root (&optional start-directory)
  "Return the root of the repository, or nil if there isn't one."
  (let ((dir (or start-directory
                 default-directory
                 (error "No start directory given"))))
    (if (car (directory-files dir t "^_darcs$"))
        dir
      (let ((next-dir (file-name-directory (directory-file-name
                                            (file-truename dir)))))
        (unless (or (equal dir next-dir) (null next-dir))
          (darcsum-repository-root next-dir))))))

(defcustom darcsum-whatsnew-switches nil
  "*Switches for `darcsum-whatsnew'."
  :type 'string
  :group 'darcsum)

(defcustom darcsum-whatsnew-at-toplevel t
  "*Use top-level repository directory as default argument to \
`darcsum-whatsnew'."
  :type 'boolean
  :group 'darcsum)

;;;###autoload
(defun darcsum-whatsnew (directory
                         &optional look-for-adds no-display show-context)
  "Run `darcs whatsnew' in DIRECTORY, displaying the output in `darcsum-mode'.

When invoked interactively, prompt for the directory to display changes for."
  (interactive
   ; fancy "DDirectory: \nP"
   (let ((root
          (if darcsum-whatsnew-at-toplevel
              (darcsum-repository-root)
            default-directory)))
     (list (funcall (if (fboundp 'read-directory-name)
                        'read-directory-name
                      'read-file-name)
                    "Directory: " root root)
           (or darcsum-look-for-adds current-prefix-arg))))
  (with-temp-buffer
    (cd directory)
    (let ((repo (darcsum-repository-root)))
      (unless repo
        (error "Directory `%s' is not under darcs version control"
               directory))
      (cd repo))
    (let* ((process-environment (append
                                 (list "DARCS_DONT_ESCAPE_TRAILING_SPACES=1"
                                       "DARCS_DONT_COLOR=1"
                                       "DARCS_DONT_ESCAPE_TRAILING_CR=1")
                                 process-environment))
           (args (append
                  ;; Build a list of arguments for call-process
                  (list darcsum-program nil t nil)
                  (list "whatsnew" "--no-summary")
                  (darcsum-fix-switches darcsum-whatsnew-switches)
                  ; Arguments override user preferences
                  (unless (null look-for-adds) (list "--look-for-adds"))
                  (unless (null show-context) (list "--unified"))
                  (unless (string= directory default-directory)
                    (list (file-relative-name
                           directory default-directory)))
                  nil))
           (result (apply 'call-process args)))
      (if (/= result 0)
          (if (= result 1)
              (progn (and (interactive-p) (message "No changes!"))
                     nil)
            (progn (if (member "*darcs-output*"
                               (mapcar (lambda (&rest x)
                                         (apply 'buffer-name x))
                                       (buffer-list)) )
                       (kill-buffer "*darcs-output*"))
                   (if (fboundp 'clone-buffer)
                       (clone-buffer "*darcs-output*" t))
                   (error "Error running darcsum whatsnew")))
        (let ((changes (darcsum-read-changeset darcsum-default-expanded)))
          (if (and changes (not no-display))
              (darcsum-display changes look-for-adds))
          changes)))))

; lifted from diff.el
(defun darcsum-fix-switches (switch-spec)
  "Parse SWITCH-SPEC into a list of switches.
Leave it be if it's not a string."
  (if (stringp switch-spec)
      (let (result (start 0))
        (while (string-match "\\(\\S-+\\)" switch-spec start)
          (setq result (cons (substring switch-spec (match-beginning 1)
                                        (match-end 1))
                             result)
                start (match-end 0)))
        (nreverse result))
    switch-spec))

;;;###autoload
(defun darcsum-view (directory)
  "View the contents of the current buffer as a darcs changeset for DIRECTORY.
More precisely, searches forward from point for the next changeset-like region,
and attempts to parse that as a darcs patch.

When invoked interactively, prompts for a directory; by default, the current
working directory is assumed."
  (interactive
   (list (funcall (if (fboundp 'read-directory-name)
                      'read-directory-name
                    'read-file-name)
                  "Directory: "
                  (darcsum-repository-root))))
  (unless (file-directory-p (expand-file-name "_darcs" directory))
    (error "Directory `%s' is not under darcs version control"
           directory))
  (if (or (and (search-forward "{" nil t)
               (goto-char (1- (point))))
          (search-backward "{" nil t))
      (let ((changes (darcsum-parse-changeset))
            (default-directory directory))
        (darcsum-display changes))
    (error "Cannot find a darcs patch in the current buffer")))

;;; Gnus integration code, for viewing darcs patches in a changeset
;;; buffer.  They cannot be recorded from there, however, since the
;;; changes have not been applied to the working tree.  To do this,
;;; you must still pipe the message to "darcs apply".  This code only
;;; works as a browser for now.

(defvar darcsum-install-gnus-code nil)

(when darcsum-install-gnus-code
  (eval-when-compile (require 'gnus)
                     (require 'gnus-sum)
                     (require 'gnus-art)
                     (require 'gnus-fun)
                     (require 'gnus-win)
                     (require 'gnus-util)
                     (require 'mm-view)
                     (require 'mail-parse)

  (defun mm-view-darcs-patch (handle)
    "View HANDLE as a darcs patch, using darcsum.el."
    (let* ((name (mail-content-type-get (mm-handle-type handle) 'name))
           (directory
            (funcall (if (fboundp 'read-directory-name)
                         'read-directory-name
                       'read-file-name)
                     "Apply patch to directory: ")))
      (mm-with-unibyte-buffer
        (mm-insert-part handle)
        (let ((coding-system-for-write 'binary))
          (goto-char (point-min))
          (darcsum-view directory)
          (delete-other-windows)))))

  (defun gnus-mime-view-darcs-patch ()
    "Pipe the MIME part under point to a process."
    (interactive)
    (gnus-article-check-buffer)
    (let ((data (get-text-property (point) 'gnus-data)))
      (when data
        (mm-view-darcs-patch data))))

  (defun gnus-article-view-darcs-patch (n)
    "Pipe MIME part N, which is the numerical prefix."
    (interactive "p")
    (gnus-article-part-wrapper n 'mm-view-darcs-patch))

  (eval-after-load "gnus-art"
    '(progn
       (nconc gnus-mime-action-alist
              '(("apply darcs patch" . gnus-mime-view-darcs-patch)))
       (nconc gnus-mime-button-commands
              '((gnus-mime-view-darcs-patch "V" "Apply darcs patch...")))))

  (defun gnus-summary-view-darcs-patch (directory)
    "Apply the current article as a darcs patch to DIRECTORY."
    (interactive "DApply patch to directory: ")
    (gnus-summary-select-article)
    (let ((mail-header-separator ""))
      (gnus-eval-in-buffer-window gnus-article-buffer
        (save-restriction
          (widen)
          (goto-char (point-min))
          (darcsum-view directory)))))

  (eval-after-load "gnus-sum"
    '(progn
       (define-key gnus-summary-mime-map "V" 'gnus-article-view-darcs-patch)
       (define-key gnus-summary-article-map "V"
         'gnus-summary-view-darcs-patch)))))

(provide 'darcsum)
;;; darcsum.el ends here

;;; magit-notes.el --- notes support  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
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

;; This library implements support for `git-notes'.

;;; Code:

(require 'magit)

;;; Popup

;;;###autoload (autoload 'magit-notes-popup "magit" nil t)
(magit-define-popup magit-notes-popup
  "Popup console for notes commands."
  :man-page "git-tag"
  :switches '("Switch for prune"
              (?n "Dry run"          "--dry-run"))
  :options  '("Option for edit and remove"
              (?r "Manipulate ref"   "--ref=" magit-notes-popup-read-ref)
              "Option for merge"
              (?s "Merge strategy"   "--strategy="))
  :actions  '((?T "Edit"             magit-notes-edit)
              (?r "Remove"           magit-notes-remove)
              (?m "Merge"            magit-notes-merge)
              (?p "Prune"            magit-notes-prune)
              (?s "Set ref"          magit-notes-set-ref)
              (?S "Set display refs" magit-notes-set-display-refs))
  :sequence-actions '((?c "Commit merge" magit-notes-merge-commit)
                      (?a "Abort merge"  magit-notes-merge-abort))
  :sequence-predicate 'magit-notes-merging-p
  :default-action 'magit-notes-edit)

(defun magit-notes-merging-p ()
  (let ((dir (magit-git-dir "NOTES_MERGE_WORKTREE")))
    (and (file-directory-p dir)
         (directory-files dir nil "^[^.]"))))

(defun magit-notes-popup-read-ref (prompt &optional initial-input)
  (magit-completing-read prompt (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                         nil nil initial-input))

;;; Commands

(defun magit-notes-edit (commit &optional ref)
  "Edit the note attached to COMMIT.
REF is the notes ref used to store the notes.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (magit-notes-read-args "Edit notes"))
  (magit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "edit" commit))

(defun magit-notes-remove (commit &optional ref)
  "Remove the note attached to COMMIT.
REF is the notes ref from which the note is removed.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (magit-notes-read-args "Remove notes"))
  (magit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "remove" commit))

(defun magit-notes-merge (ref)
  "Merge the notes ref REF into the current notes ref.

The current notes ref is the value of Git variable
`core.notesRef' or \"refs/notes/commits\" if that is undefined.

When there are conflicts, then they have to be resolved in the
temporary worktree \".git/NOTES_MERGE_WORKTREE\".  When
done use `magit-notes-merge-commit' to finish.  To abort
use `magit-notes-merge-abort'."
  (interactive (list (magit-read-string-ns "Merge reference")))
  (magit-run-git-with-editor "notes" "merge" ref))

(defun magit-notes-merge-commit ()
  "Commit the current notes ref merge.
Also see `magit-notes-merge'."
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--commit"))

(defun magit-notes-merge-abort ()
  "Abort the current notes ref merge.
Also see `magit-notes-merge'."
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--abort"))

(defun magit-notes-prune (&optional dry-run)
  "Remove notes about unreachable commits."
  (interactive (list (and (member "--dry-run" (magit-notes-arguments)) t)))
  (when dry-run
    (magit-process-buffer))
  (magit-run-git-with-editor "notes" "prune" (and dry-run "--dry-run")))

(defun magit-notes-set-ref (ref &optional global)
  "Set the current notes ref to REF.
The ref is made current by setting the value of the Git variable
`core.notesRef'.  With a prefix argument GLOBAL change the global
value, else the value in the current repository.  When this is
undefined, then \"refs/notes/commit\" is used.

Other `magit-notes-*' commands, as well as the sub-commands
of Git's `note' command, default to operate on that ref."
  (interactive
   (list (magit-completing-read "Set notes ref"
                                (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                                nil nil
                                (--when-let (magit-get "core.notesRef")
                                  (if (string-match "^refs/notes/\\(.+\\)" it)
                                      (match-string 1 it)
                                    it)))
         current-prefix-arg))
  (if ref
      (magit-run-git "config" (and global "--global") "core.notesRef"
                     (if (string-prefix-p "refs/" ref)
                         ref
                       (concat "refs/notes/" ref)))
    (magit-run-git "config" (and global "--global")
                   "--unset" "core.notesRef")))

(defun magit-notes-set-display-refs (refs &optional global)
  "Set notes refs to be display in addition to \"core.notesRef\".
REFS is a colon separated list of notes refs.  The values are
stored in the Git variable `notes.displayRef'.  With a prefix
argument GLOBAL change the global values, else the values in
the current repository."
  (interactive
   (list (magit-completing-read "Set additional notes ref(s)"
                                (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                                nil nil
                                (mapconcat #'identity
                                           (magit-get-all "notes.displayRef")
                                           ":"))
         current-prefix-arg))
  (when (and refs (atom refs))
    (setq refs (split-string refs ":")))
  (when global
    (setq global "--global"))
  (magit-git-success "config" "--unset-all" global "notes.displayRef")
  (dolist (ref refs)
    (magit-call-git "config" "--add" global "notes.displayRef" ref))
  (magit-refresh))

(defun magit-notes-read-args (prompt)
 (list (magit-read-branch-or-commit prompt)
       (--when-let (--first (string-match "^--ref=\\(.+\\)" it)
                            (magit-notes-arguments))
         (match-string 1 it))))

(provide 'magit-notes)
;;; magit-notes.el ends here

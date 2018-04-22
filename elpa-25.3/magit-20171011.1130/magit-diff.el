;;; magit-diff.el --- inspect Git diffs  -*- lexical-binding: t -*-

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

;; This library implements support for looking at Git diffs and
;; commits.

;;; Code:

(require 'git-commit)
(require 'magit-core)

;; For `magit-diff-popup'
(declare-function magit-stash-show 'magit-stash)
;; For `magit-diff-visit-file'
(declare-function dired-jump 'dired-x)
(declare-function magit-find-file-noselect 'magit)
(declare-function magit-status-internal 'magit)
;; For `magit-diff-while-committing'
(declare-function magit-commit-message-buffer 'magit)
;; For `magit-insert-revision-gravatar'
(defvar gravatar-size)
;; For `magit-show-commit' and `magit-diff-show-or-scroll'
(declare-function magit-blame-chunk-get 'magit-blame)
(declare-function magit-blame-mode 'magit-blame)
(defvar magit-blame-mode)
(defvar git-rebase-line)

(require 'diff-mode)
(require 'smerge-mode)

(defvar bookmark-make-record-function)

;;; Options
;;;; Diff Mode

(defgroup magit-diff nil
  "Inspect and manipulate Git diffs."
  :link '(info-link "(magit)Diffing")
  :group 'magit-modes)

(defcustom magit-diff-mode-hook nil
  "Hook run after entering Magit-Diff mode."
  :group 'magit-diff
  :type 'hook)

(defcustom magit-diff-arguments '("--stat" "--no-ext-diff")
  "The diff arguments used in buffers whose mode derives from `magit-diff-mode'."
  :group 'magit-git-arguments
  :group 'magit-diff
  :type '(repeat (string :tag "Argument")))

(defcustom magit-diff-sections-hook
  '(magit-insert-diff
    magit-insert-xref-buttons)
  "Hook run to insert sections into a `magit-diff-mode' buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-diff
  :type 'hook)

(defcustom magit-diff-expansion-threshold 60
  "After how many seconds not to expand anymore diffs.

Except in status buffers, diffs are usually start out fully
expanded.  Because that can take a long time, all diffs that
haven't been fontified during a refresh before the threshold
defined here are instead displayed with their bodies collapsed.

Note that this can cause sections that were previously expanded
to be collapsed.  So you should not pick a very low value here.

The hook function `magit-diff-expansion-threshold' has to be a
member of `magit-section-set-visibility-hook' for this option
to have any effect."
  :package-version '(magit . "2.9.0")
  :group 'magit-diff
  :type 'float)

(defcustom magit-diff-highlight-hunk-body t
  "Whether to highlight bodies of selected hunk sections.
This only has an effect if `magit-diff-highlight' is a
member of `magit-section-highlight-hook', which see."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-show-lines-boundary t
  "This option is obsolete.
If you have set this to nil, then remove that customization, and
instead customize `magit-diff-highlight-hunk-region-functions'."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-highlight-hunk-region-functions
  '(magit-diff-highlight-hunk-region-dim-outside
    magit-diff-highlight-hunk-region-using-overlays)
  "The functions used to highlight the hunk-internal region.

`magit-diff-highlight-hunk-region-dim-outside' overlays the outside
of the hunk internal selection with a face that causes the added and
removed lines to have the same background color as context lines.
This function should not be removed from the value of this option.

`magit-diff-highlight-hunk-region-using-overlays' and
`magit-diff-highlight-hunk-region-using-underline' emphasize the
region by placing delimiting horizonal lines before and after it.
Both of these functions have glitches which cannot be fixed due
to limitations of Emacs' display engine.  For more information
see https://github.com/magit/magit/issues/2758 ff.

Instead of, or in addition to, using delimiting horizontal lines,
to emphasize the boundaries, you may which to emphasize the text
itself, using `magit-diff-highlight-hunk-region-using-face'.

In terminal frames it's not possible to draw lines as the overlay
and underline variants normally do, so there they fall back to
calling the face function instead."
  :package-version '(magit . "2.9.0")
  :set-after '(magit-diff-show-lines-boundaries)
  :group 'magit-diff
  :type 'hook
  :options '(magit-diff-highlight-hunk-region-dim-outside
             magit-diff-highlight-hunk-region-using-underline
             magit-diff-highlight-hunk-region-using-overlays
             magit-diff-highlight-hunk-region-using-face))

(defcustom magit-diff-unmarked-lines-keep-foreground t
  "Whether `magit-diff-highlight-hunk-region-dim-outside' preserves foreground.
When this is set to nil, then that function only adjusts the
foreground color but added and removed lines outside the region
keep their distinct foreground colors."
  :package-version '(magit . "2.9.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-refine-hunk nil
  "Whether to show word-granularity differences within diff hunks.

nil    never show fine differences.
t      show fine differences for the current diff hunk only.
`all'  show fine differences for all displayed diff hunks."
  :group 'magit-diff
  :safe (lambda (val) (memq val '(nil t all)))
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Current" t)
                 (const :tag "All" all)))

(put 'magit-diff-refine-hunk 'permanent-local t)

(defcustom magit-diff-paint-whitespace t
  "Specify where to highlight whitespace errors.
See `magit-diff-highlight-trailing',
`magit-diff-highlight-indentation'.  The symbol t means in all
diffs, `status' means only in the status buffer, and nil means
nowhere."
  :group 'magit-diff
  :safe (lambda (val) (memq val '(t nil status)))
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In status buffer" status)))

(defcustom magit-diff-highlight-trailing t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `magit-diff-paint-whitespace' is non-nil."
  :group 'magit-diff
  :safe 'booleanp
  :type 'boolean)

(defcustom magit-diff-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `magit-diff-paint-whitespace' is non-nil.

The value is a list of cons cells.  The car is a regular
expression, and the cdr is the value that applies to repositories
whose directory matches the regular expression.  If more than one
element matches, then the *last* element in the list applies.
The default value should therefore come first in the list.

If the value is `tabs', highlight indentation with tabs.  If the
value is an integer, highlight indentation with at least that
many spaces.  Otherwise, highlight neither."
  :group 'magit-diff
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil)))))

(defcustom magit-diff-hide-trailing-cr-characters
  (and (memq system-type '(ms-dos windows-nt)) t)
  "Whether to hide ^M characters at the end of a line in diffs."
  :package-version '(magit . "2.6.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-visit-previous-blob t
  "Whether `magit-diff-visit-file' may visit the previous blob.

When this is t and point is on a removed line in a diff for a
committed change, then `magit-diff-visit-file' visits the blob
from the last revision which still had that line.

Currently this is only supported for committed changes, for
staged and unstaged changes `magit-diff-visit-file' always
visits the file in the working tree."
  :package-version '(magit . "2.9.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-highlight-keywords t
  "Whether to highlight bracketed keywords in commit messages."
  :package-version '(magit . "2.12.0")
  :group 'magit-diff
  :type 'boolean)

;;;; File Diff

(defcustom magit-diff-buffer-file-locked t
  "Whether `magit-diff-buffer-file' uses a dedicated buffer."
  :package-version '(magit . "2.7.0")
  :group 'magit-commands
  :group 'magit-diff
  :type 'boolean)

;;;; Revision Mode

(defgroup magit-revision nil
  "Inspect and manipulate Git commits."
  :link '(info-link "(magit)Revision Buffer")
  :group 'magit-modes)

(defcustom magit-revision-mode-hook '(bug-reference-mode)
  "Hook run after entering Magit-Revision mode."
  :group 'magit-revision
  :type 'hook
  :options '(bug-reference-mode))

(defcustom magit-revision-sections-hook
  '(magit-insert-revision-tag
    magit-insert-revision-headers
    magit-insert-revision-message
    magit-insert-revision-notes
    magit-insert-revision-diff
    magit-insert-xref-buttons)
  "Hook run to insert sections into a `magit-revision-mode' buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-revision
  :type 'hook)

(defcustom magit-revision-headers-format "\
Author:     %aN <%aE>
AuthorDate: %ad
Commit:     %cN <%cE>
CommitDate: %cd
"
  "Format string used to insert headers in revision buffers.

All headers in revision buffers are inserted by the section
inserter `magit-insert-revision-headers'.  Some of the headers
are created by calling `git show --format=FORMAT' where FORMAT
is the format specified here.  Other headers are hard coded or
subject to option `magit-revision-insert-related-refs'."
  :package-version '(magit . "2.3.0")
  :group 'magit-revision
  :type 'string)

(defcustom magit-revision-insert-related-refs t
  "Whether to show related refs in revision buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type 'boolean)

(defcustom magit-revision-show-gravatars nil
  "Whether to show gravatar images in revision buffers.

If non-nil, then the value has to be a cons-cell which specifies
where the gravatar images for the author and/or the committer are
inserted inside the text that was previously inserted according
to `magit-revision-header-format'.

Both cells are regular expressions.  The car specifies where to
insert the author gravatar image.  The top half of the image is
inserted right after the matched text, the bottom half on the
next line at the same offset.  The cdr specifies where to insert
the committer image, accordingly.  Either the car or the cdr may
be nil."
  :package-version '(magit . "2.3.0")
  :group 'magit-revision
  :type '(choice (const :tag "Don't show gravatars" nil)
                 (cons  :tag "Show gravatars"
                        (regexp :tag "Author regexp"    "^Author:     ")
                        (regexp :tag "Committer regexp" "^Commit:     "))))

(defcustom magit-revision-use-gravatar-kludge nil
  "Whether to work around a bug which affects display of gravatars.

Gravatar images are spliced into two halves which are then
displayed on separate lines.  On OS X the splicing has a bug in
some Emacs builds, which causes the top and bottom halves to be
interchanged.  Enabling this option works around this issue by
interchanging the halves once more, which cancels out the effect
of the bug.

See https://github.com/magit/magit/issues/2265
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=7847."
  :package-version '(magit . "2.3.0")
  :group 'magit-revision
  :type 'boolean)

;;;; Diff Sections

(defcustom magit-diff-section-arguments '("--no-ext-diff")
  "The diff arguments used in buffers that show other things besides diffs."
  :group 'magit-git-arguments
  :group 'magit-diff
  :group 'magit-status
  :type '(repeat (string :tag "Argument")))

;;; Faces

(defface magit-diff-file-heading
  '((t :weight bold))
  "Face for diff file headings."
  :group 'magit-faces)

(defface magit-diff-file-heading-highlight
  '((t :inherit (magit-section-highlight)))
  "Face for current diff file headings."
  :group 'magit-faces)

(defface magit-diff-file-heading-selection
  '((((class color) (background light))
     :inherit magit-diff-file-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     :inherit magit-diff-file-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff file headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading
  '((((class color) (background light))
     :background "grey80"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey25"
     :foreground "grey70"))
  "Face for diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading-highlight
  '((((class color) (background light))
     :background "grey75"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey35"
     :foreground "grey70"))
  "Face for current diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading-selection
  '((((class color) (background light))
     :inherit magit-diff-hunk-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     :inherit magit-diff-hunk-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-hunk-region
  '((t :inherit bold))
  "Face used by `magit-diff-highlight-hunk-region-using-face'.

This face is overlayed over text that uses other hunk faces,
and those normally set the foreground and background colors.
The `:foreground' and especially the `:background' properties
should be avoided here.  Setting the latter would cause the
lose of information.  Good properties to set here are `:weight'
and `:slant'."
  :group 'magit-faces)

(defface magit-diff-lines-heading
  '((((class color) (background light))
     :inherit magit-diff-hunk-heading-highlight
     :background "LightSalmon3")
    (((class color) (background dark))
     :inherit magit-diff-hunk-heading-highlight
     :foreground "grey80"
     :background "salmon4"))
  "Face for diff hunk heading when lines are marked."
  :group 'magit-faces)

(defface magit-diff-lines-boundary
  '((t :inherit magit-diff-lines-heading))
  "Face for boundary of marked lines in diff hunk."
  :group 'magit-faces)

(defface magit-diff-conflict-heading
  '((t :inherit magit-diff-hunk-heading))
  "Face for conflict markers."
  :group 'magit-faces)

(defface magit-diff-added
  '((((class color) (background light))
     :background "#ddffdd"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#335533"
     :foreground "#ddffdd"))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed
 '((((class color) (background light))
    :background "#ffdddd"
    :foreground "#aa2222")
   (((class color) (background dark))
    :background "#553333"
    :foreground "#ffdddd"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-our
  '((t :inherit magit-diff-removed))
  "Face for lines in a diff for our side in a conflict."
  :group 'magit-faces)

(defface magit-diff-base
  '((((class color) (background light))
     :background "#ffffcc"
     :foreground "#aaaa11")
    (((class color) (background dark))
     :background "#555522"
     :foreground "#ffffcc"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'magit-faces)

(defface magit-diff-their
  '((t :inherit magit-diff-added))
  "Face for lines in a diff for their side in a conflict."
  :group 'magit-faces)

(defface magit-diff-context
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'magit-faces)

(defface magit-diff-added-highlight
  '((((class color) (background light))
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#336633"
     :foreground "#cceecc"))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed-highlight
  '((((class color) (background light))
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#663333"
     :foreground "#eecccc"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-our-highlight
  '((t :inherit magit-diff-removed-highlight))
  "Face for lines in a diff for our side in a conflict."
  :group 'magit-faces)

(defface magit-diff-base-highlight
  '((((class color) (background light))
     :background "#eeeebb"
     :foreground "#aaaa11")
    (((class color) (background dark))
     :background "#666622"
     :foreground "#eeeebb"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'magit-faces)

(defface magit-diff-their-highlight
  '((t :inherit magit-diff-added-highlight))
  "Face for lines in a diff for their side in a conflict."
  :group 'magit-faces)

(defface magit-diff-context-highlight
  '((((class color) (background light))
     :background "grey95"
     :foreground "grey50")
    (((class color) (background dark))
     :background "grey20"
     :foreground "grey70"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-whitespace-warning
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors added lines."
  :group 'magit-faces)

(defface magit-diffstat-added
  '((((class color) (background light)) :foreground "#22aa22")
    (((class color) (background  dark)) :foreground "#448844"))
  "Face for plus sign in diffstat."
  :group 'magit-faces)

(defface magit-diffstat-removed
  '((((class color) (background light)) :foreground "#aa2222")
    (((class color) (background  dark)) :foreground "#aa4444"))
  "Face for minus sign in diffstat."
  :group 'magit-faces)

;;; Commands
;;;; Diff popups

(defconst magit-diff-popup-common
  '(:variable magit-diff-arguments
    :man-page "git-diff"
    :options  ((?f "Limit to files" "-- " magit-read-files)
               (?u "Context lines"  "-U")
               (?m "Detect renames" "-M")
               (?c "Detect copies"  "-C")
               (?a "Diff algorithm" "--diff-algorithm="
                   magit-diff-select-algorithm))))

(defvar magit-diff-popup
  `(,@magit-diff-popup-common
    :switches ((?f "Show surrounding functions"     "--function-context")
               (?b "Ignore whitespace changes"      "--ignore-space-change")
               (?w "Ignore all whitespace"          "--ignore-all-space")
               (?x "Disallow external diff drivers" "--no-ext-diff")
               (?s "Show stats"                     "--stat"))
    :actions  ((?d "Dwim"          magit-diff-dwim)
               (?u "Diff unstaged" magit-diff-unstaged)
               (?c "Show commit"   magit-show-commit)
               (?r "Diff range"    magit-diff)
               (?s "Diff staged"   magit-diff-staged)
               (?t "Show stash"    magit-stash-show)
               (?p "Diff paths"    magit-diff-paths)
               (?w "Diff worktree" magit-diff-working-tree))
    :default-action magit-diff-dwim
    :max-action-columns 3))

(defvar magit-diff-refresh-popup
  `(,@magit-diff-popup-common
    :switches ((?f "Show surrounding functions"     "--function-context")
               (?b "Ignore whitespace changes"      "--ignore-space-change")
               (?w "Ignore all whitespace"          "--ignore-all-space")
               (?x "Disallow external diff drivers" "--no-ext-diff"))
    :actions  ((?g "Refresh"                magit-diff-refresh)
               (?t "Toggle hunk refinement" magit-diff-toggle-refine-hunk)
               (?s "Set defaults"           magit-diff-set-default-arguments)
               (?F "Toggle file filter"     magit-diff-toggle-file-filter)
               (?w "Save defaults"          magit-diff-save-default-arguments))
    :max-action-columns 2))

(defvar magit-diff-mode-refresh-popup
  `(,@magit-diff-popup-common
    :switches ((?f "Show surrounding functions"     "--function-context")
               (?b "Ignore whitespace changes"      "--ignore-space-change")
               (?w "Ignore all whitespace"          "--ignore-all-space")
               (?x "Disallow external diff drivers" "--no-ext-diff")
               (?s "Show stats"                     "--stat"))
    :actions  ((?g "Refresh"                magit-diff-refresh)
               (?t "Toggle hunk refinement" magit-diff-toggle-refine-hunk)
               (?s "Set defaults"           magit-diff-set-default-arguments)
               (?r "Switch range type"      magit-diff-switch-range-type)
               (?w "Save defaults"          magit-diff-save-default-arguments)
               (?f "Flip revisions"         magit-diff-flip-revs) nil
               (?F "Toggle file filter"     magit-diff-toggle-file-filter))
    :max-action-columns 2))

(defvar magit-revision-mode-refresh-popup
  `(,@magit-diff-popup-common
    :switches ((?f "Show surrounding functions"     "--function-context")
               (?b "Ignore whitespace changes"      "--ignore-space-change")
               (?w "Ignore all whitespace"          "--ignore-all-space")
               (?x "Disallow external diff drivers" "--no-ext-diff")
               (?s "Show stats"                     "--stat"))
    :actions  ((?g "Refresh"                magit-diff-refresh)
               (?t "Toggle hunk refinement" magit-diff-toggle-refine-hunk)
               (?s "Set defaults"           magit-diff-set-default-arguments)
               (?F "Toggle file filter"     magit-diff-toggle-file-filter)
               (?w "Save defaults"          magit-diff-save-default-arguments))
    :max-action-columns 2))

(magit-define-popup-keys-deferred 'magit-diff-popup)
(magit-define-popup-keys-deferred 'magit-diff-refresh-popup)
(magit-define-popup-keys-deferred 'magit-diff-mode-refresh-popup)
(magit-define-popup-keys-deferred 'magit-revision-mode-refresh-popup)

(defvar magit-diff-section-file-args nil)
(put 'magit-diff-section-file-args 'permanent-local t)
(put 'magit-diff-section-arguments 'permanent-local t)

(defun magit-diff-get-buffer-args ()
  (cond ((and magit-use-sticky-arguments
              (derived-mode-p 'magit-diff-mode))
         (list (nth 2 magit-refresh-args)
               (nth 3 magit-refresh-args)))
        ((and (eq magit-use-sticky-arguments t)
              (--when-let (magit-mode-get-buffer 'magit-diff-mode)
                (with-current-buffer it
                  (list (nth 2 magit-refresh-args)
                        (nth 3 magit-refresh-args))))))
        (t
         (list (default-value 'magit-diff-arguments) nil))))

(defun magit-diff-arguments (&optional refresh)
  (cond ((memq magit-current-popup '(magit-diff-popup magit-diff-refresh-popup))
         (magit-popup-export-file-args magit-current-popup-args))
        ((and refresh (not (derived-mode-p 'magit-diff-mode)))
         (list magit-diff-section-arguments
               magit-diff-section-file-args))
        (t
         (magit-diff-get-buffer-args))))

;;;###autoload
(defun magit-diff-popup (arg)
  "Popup console for diff commands."
  (interactive "P")
  (let ((magit-diff-arguments
         ;; We cannot possibly know what suffix command the user is
         ;; about to invoke, so we also don't know from which buffer
         ;; we should get the current values.  However it is much
         ;; more likely that we will end up updating the diff buffer,
         ;; and we therefore use the value from that buffer.
         (apply #'magit-popup-import-file-args (magit-diff-get-buffer-args))))
    (magit-invoke-popup 'magit-diff-popup nil arg)))

;;;###autoload
(defun magit-diff-buffer-file-popup ()
  "Popup console for diff commands.

This is a variant of `magit-diff-popup' which shows the same popup
but which limits the diff to the file being visited in the current
buffer."
  (interactive)
  (-if-let (file (magit-file-relative-name))
      (let ((magit-diff-arguments
             (magit-popup-import-file-args
              (-if-let (buffer (magit-mode-get-buffer 'magit-diff-mode))
                  (with-current-buffer buffer
                    (nth 3 magit-refresh-args))
                (default-value 'magit-diff-arguments))
              (list file))))
        (magit-invoke-popup 'magit-diff-popup nil nil))
    (user-error "Buffer isn't visiting a file")))

(defun magit-diff-refresh-popup (arg)
  "Popup console for changing diff arguments in the current buffer."
  (interactive "P")
  (let ((magit-diff-refresh-popup
         (pcase major-mode
           (`magit-revision-mode magit-revision-mode-refresh-popup)
           (`magit-diff-mode     magit-diff-mode-refresh-popup)
           (_                    magit-diff-refresh-popup)))
        (magit-diff-arguments
         (if (derived-mode-p 'magit-diff-mode)
             (magit-popup-import-file-args (nth 2 magit-refresh-args)
                                           (nth 3 magit-refresh-args))
           (magit-popup-import-file-args magit-diff-section-arguments
                                         magit-diff-section-file-args))))
    (magit-invoke-popup 'magit-diff-refresh-popup nil arg)))

(defun magit-diff-select-algorithm (&rest _ignore)
  (magit-read-char-case nil t
    (?d "[d]efault"   "default")
    (?m "[m]inimal"   "minimal")
    (?p "[p]atience"  "patience")
    (?h "[h]istogram" "histogram")))

;;;; Diff commands

;;;###autoload
(defun magit-diff-dwim (&optional args files)
  "Show changes for the thing at point."
  (interactive (magit-diff-arguments))
  (pcase (magit-diff--dwim)
    (`unstaged (magit-diff-unstaged args files))
    (`staged (magit-diff-staged nil args files))
    (`(commit . ,value)
     (magit-diff (format "%s^..%s" value value) args files))
    (`(stash  . ,value) (magit-stash-show value args))
    ((and range (pred stringp))
     (magit-diff range args files))
    (_
     (call-interactively #'magit-diff))))

(defun magit-diff--dwim ()
  "Return information for performing DWIM diff.

The information can be in three forms:
1. TYPE
   A symbol describing a type of diff where no additional information
   is needed to generate the diff.  Currently, this includes `staged'
   and `unstaged'.
2. (TYPE . VALUE)
   Like #1 but the diff requires additional information, which is
   given by VALUE.  Currently, this includes `commit' and `stash',
   where VALUE is the given commit or stash, respectively.
3. RANGE
   A string indicating a diff range.

If no DWIM context is found, nil is returned."
  (cond
   ((--when-let (magit-region-values 'commit 'branch)
      (deactivate-mark)
      (concat (car (last it)) ".." (car it))))
   (magit-buffer-refname
    (cons 'commit magit-buffer-refname))
   ((derived-mode-p 'magit-stash-mode)
    (cons 'commit
          (magit-section-case
            (commit (magit-section-value it))
            (file (-> it
                      magit-section-parent
                      magit-section-value))
            (hunk (-> it
                      magit-section-parent
                      magit-section-parent
                      magit-section-value)))))
   ((derived-mode-p 'magit-revision-mode)
    (cons 'commit (car magit-refresh-args)))
   ((derived-mode-p 'magit-diff-mode)
    (nth 0 magit-refresh-args))
   (t
    (magit-section-case
      ([* unstaged] 'unstaged)
      ([* staged] 'staged)
      (unpushed (magit-section-value it))
      (unpulled (magit-section-value it))
      (branch (let ((current (magit-get-current-branch))
                    (atpoint (magit-section-value it)))
                (if (equal atpoint current)
                    (--if-let (magit-get-upstream-branch)
                        (format "%s...%s" it current)
                      (if (magit-anything-modified-p)
                          current
                        (cons 'commit current)))
                  (format "%s...%s"
                          (or current "HEAD")
                          atpoint))))
      (commit (cons 'commit (magit-section-value it)))
      (stash (cons 'stash (magit-section-value it)))))))

(defun magit-diff-read-range-or-commit (prompt &optional secondary-default mbase)
  "Read range or revision with special diff range treatment.
If MBASE is non-nil, prompt for which rev to place at the end of
a \"revA...revB\" range.  Otherwise, always construct
\"revA..revB\" range."
  (--if-let (magit-region-values 'commit 'branch)
      (let ((revA (car (last it)))
            (revB (car it)))
        (deactivate-mark)
        (if mbase
            (let ((base (magit-git-string "merge-base" revA revB)))
              (cond
               ((string= (magit-rev-parse revA) base)
                (format "%s..%s" revA revB))
               ((string= (magit-rev-parse revB) base)
                (format "%s..%s" revB revA))
               (t
                (let ((main (magit-completing-read "View changes along"
                                                   (list revA revB)
                                                   nil t nil nil revB)))
                  (format "%s...%s"
                          (if (string= main revB) revA revB) main)))))
          (format "%s..%s" revA revB)))
    (magit-read-range prompt
                      (or (pcase (magit-diff--dwim)
                            (`(commit . ,value)
                             (format "%s^..%s" value value))
                            ((and range (pred stringp))
                             range))
                          secondary-default
                          (magit-get-current-branch)))))

(defun magit-diff-setup (rev-or-range const args files)
  (require 'magit)
  (magit-mode-setup #'magit-diff-mode rev-or-range const args files))

;;;###autoload
(defun magit-diff (rev-or-range &optional args files)
  "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range)."
  (interactive (cons (magit-diff-read-range-or-commit "Diff for range"
                                                      nil current-prefix-arg)
                     (magit-diff-arguments)))
  (magit-diff-setup rev-or-range nil args files))

;;;###autoload
(defun magit-diff-working-tree (&optional rev args files)
  "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (magit-read-branch-or-commit "Diff working tree and commit"))
         (magit-diff-arguments)))
  (magit-diff-setup (or rev "HEAD") nil args files))

;;;###autoload
(defun magit-diff-staged (&optional rev args files)
  "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (magit-read-branch-or-commit "Diff index and commit"))
         (magit-diff-arguments)))
  (magit-diff-setup rev (list "--cached") args files))

;;;###autoload
(defun magit-diff-unstaged (&optional args files)
  "Show changes between the working tree and the index."
  (interactive (magit-diff-arguments))
  (magit-diff-setup nil nil args files))

;;;###autoload
(defun magit-diff-while-committing (&optional args files)
  "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed."
  (interactive (magit-diff-arguments))
  (let ((toplevel (magit-toplevel))
        (diff-buf (magit-mode-get-buffer 'magit-diff-mode)))
    (if (magit-commit-message-buffer)
        (if (and (or ;; most likely an explicit amend
                     (not (magit-anything-staged-p))
                     ;; explicitly toggled from within diff
                     (and (eq (current-buffer) diff-buf)))
                 (or (not diff-buf)
                     (with-current-buffer diff-buf
                       (or ;; default to include last commit
                           (not (equal (magit-toplevel) toplevel))
                           ;; toggle to include last commit
                           (not (car magit-refresh-args))))))
            (magit-diff-while-amending args files)
          (magit-diff-staged nil args files))
      (user-error "No commit in progress"))))

(define-key git-commit-mode-map
  (kbd "C-c C-d") 'magit-diff-while-committing)

(defun magit-diff-while-amending (&optional args files)
  (magit-diff-setup "HEAD^" (list "--cached") args files))

;;;###autoload
(defun magit-diff-buffer-file ()
  "Show diff for the blob or file visited in the current buffer."
  (interactive)
  (require 'magit)
  (-if-let (file (magit-file-relative-name))
      (magit-mode-setup-internal #'magit-diff-mode
                                 (list (or magit-buffer-refname
                                           (magit-get-current-branch)
                                           "HEAD")
                                       nil
                                       (cadr (magit-diff-arguments))
                                       (list file))
                                 magit-diff-buffer-file-locked)
    (user-error "Buffer isn't visiting a file")))

;;;###autoload
(defun magit-diff-paths (a b)
  "Show changes between any two files on disk."
  (interactive (list (read-file-name "First file: " nil nil t)
                     (read-file-name "Second file: " nil nil t)))
  (magit-diff-setup nil (list "--no-index")
                    nil (list (expand-file-name a)
                              (expand-file-name b))))

(defvar-local magit-buffer-revision-hash nil)

(defun magit-show-commit--arguments ()
  (-let [(args diff-files) (magit-diff-arguments)]
    (list args (if (derived-mode-p 'magit-log-mode)
                   (and (not (member "--follow" (nth 1 magit-refresh-args)))
                        (nth 2 magit-refresh-args))
                 diff-files))))

;;;###autoload
(defun magit-show-commit (rev &optional args files module)
  "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision."
  (interactive
   (let* ((mcommit (magit-section-when module-commit))
          (atpoint (or (and (bound-and-true-p magit-blame-mode)
                            (magit-blame-chunk-get :hash))
                       mcommit
                       (magit-branch-or-commit-at-point))))
     (nconc (cons (or (and (not current-prefix-arg) atpoint)
                      (magit-read-branch-or-commit "Show commit" atpoint))
                  (magit-show-commit--arguments))
            (and mcommit (list (magit-section-parent-value
                                (magit-current-section)))))))
  (require 'magit)
  (magit-with-toplevel
    (when module
      (setq default-directory
            (expand-file-name (file-name-as-directory module))))
    (unless (magit-rev-verify-commit rev)
      (user-error "%s is not a commit" rev))
    (-when-let (buffer (magit-mode-get-buffer 'magit-revision-mode))
      (with-current-buffer buffer
        (let ((prev (car magit-refresh-args)))
          (unless (equal rev prev)
            (dolist (child (cdr (magit-section-children magit-root-section)))
              (when (eq (magit-section-type child) 'file)
                (magit-section-cache-visibility child)))))))
    (magit-mode-setup #'magit-revision-mode rev nil args files)))

;;;; Setting commands

(defun magit-diff-refresh (args files)
  "Set the local diff arguments for the current buffer."
  (interactive (magit-diff-arguments t))
  (cond ((derived-mode-p 'magit-diff-mode)
         (setcdr (cdr magit-refresh-args) (list args files)))
        (t
         (setq-local magit-diff-section-arguments args)
         (setq-local magit-diff-section-file-args files)))
  (magit-refresh))

(defun magit-diff-set-default-arguments (args files)
  "Set the global diff arguments for the current buffer."
  (interactive (magit-diff-arguments t))
  (cond ((derived-mode-p 'magit-diff-mode)
         (customize-set-variable 'magit-diff-arguments args)
         (setcdr (cdr magit-refresh-args) (list args files)))
        (t
         (customize-set-variable 'magit-diff-section-arguments args)
         (kill-local-variable 'magit-diff-section-arguments)
         (kill-local-variable 'magit-diff-section-file-args)))
  (magit-refresh))

(defun magit-diff-save-default-arguments (args files)
  "Set and save the global diff arguments for the current buffer."
  (interactive (magit-diff-arguments t))
  (cond ((derived-mode-p 'magit-diff-mode)
         (customize-save-variable 'magit-diff-arguments args)
         (setcdr (cdr magit-refresh-args) (list args files)))
        (t
         (customize-save-variable 'magit-diff-section-arguments args)
         (kill-local-variable 'magit-diff-section-arguments)
         (kill-local-variable 'magit-diff-section-file-args)))
  (magit-refresh))

(defun magit-diff-switch-range-type ()
  "Convert diff range type.
Change \"revA..revB\" to \"revB...revA\", or vice versa."
  (interactive)
  (let ((range (car magit-refresh-args)))
    (if (and range
             (derived-mode-p 'magit-diff-mode)
             (string-match magit-range-re range))
        (progn
          (setcar magit-refresh-args
                  (concat (match-string 1 range)
                          (if (string= (match-string 2 range) "..")
                              "..."
                            "..")
                          (match-string 3 range)))
          (magit-refresh))
      (user-error "No range to change"))))

(defun magit-diff-flip-revs ()
  "Swap revisions in diff range.
Change \"revA..revB\" to \"revB..revA\"."
  (interactive)
  (let ((range (car magit-refresh-args)))
    (if (and range
             (derived-mode-p 'magit-diff-mode)
             (string-match magit-range-re range))
        (progn
          (setcar magit-refresh-args
                  (concat (match-string 3 range)
                          (match-string 2 range)
                          (match-string 1 range)))
          (magit-refresh))
      (user-error "No range to swap"))))

(defvar-local magit-diff--last-file-args nil)
(defun magit-diff--toggle-file-args (files)
  (cond (files
         (setq magit-diff--last-file-args files)
               nil)
        (magit-diff--last-file-args)
        (t
         (user-error "No diff file filter to toggle"))))

(defun magit-diff-toggle-file-filter ()
  "Toggle the file restriction of the current buffer's diffs.
If the current buffer's mode is derived from `magit-log-mode',
toggle the file restriction in the repository's revision buffer
instead."
  (interactive)
  (--if-let (and (derived-mode-p 'magit-log-mode)
                 (magit-mode-get-buffer 'magit-revision-mode))
      (with-current-buffer it
        (setf (nth 3 magit-refresh-args)
              (magit-diff--toggle-file-args (nth 3 magit-refresh-args)))
        (magit-refresh))
    (if (derived-mode-p 'magit-diff-mode)
        (setf (nth 3 magit-refresh-args)
              (magit-diff--toggle-file-args (nth 3 magit-refresh-args)))
      (setq-local magit-diff-section-file-args
                  (magit-diff--toggle-file-args magit-diff-section-file-args)))
    (magit-refresh)))

(defun magit-diff-less-context (&optional count)
  "Decrease the context for diff hunks by COUNT lines."
  (interactive "p")
  (magit-diff-set-context `(lambda (cur) (max 0 (- (or cur 0) ,count)))))

(defun magit-diff-more-context (&optional count)
  "Increase the context for diff hunks by COUNT lines."
  (interactive "p")
  (magit-diff-set-context `(lambda (cur) (+ (or cur 0) ,count))))

(defun magit-diff-default-context ()
  "Reset context for diff hunks to the default height."
  (interactive)
  (magit-diff-set-context #'ignore))

(defun magit-diff-set-context (fn)
  (let* ((def (--if-let (magit-get "diff.context") (string-to-number it) 3))
         (val (car (magit-diff-arguments t)))
         (arg (--first (string-match "^-U\\([0-9]+\\)?$" it) val))
         (num (--if-let (and arg (match-string 1 arg)) (string-to-number it) def))
         (val (delete arg val))
         (num (funcall fn num))
         (arg (and num (not (= num def)) (format "-U%i" num)))
         (val (if arg (cons arg val) val)))
    (if (derived-mode-p 'magit-diff-mode)
        (setcar (cddr magit-refresh-args) val)
      (setq magit-diff-section-arguments val)))
  (magit-refresh))

(defun magit-diff-context-p ()
  (--if-let (--first (string-match "^-U\\([0-9]+\\)$" it)
                     (car (magit-diff-arguments t)))
      (not (equal "-U0" it))
    t))

(defun magit-diff-toggle-refine-hunk (&optional style)
  "Turn diff-hunk refining on or off.

If hunk refining is currently on, then hunk refining is turned off.
If hunk refining is off, then hunk refining is turned on, in
`selected' mode (only the currently selected hunk is refined).

With a prefix argument, the \"third choice\" is used instead:
If hunk refining is currently on, then refining is kept on, but
the refining mode (`selected' or `all') is switched.
If hunk refining is off, then hunk refining is turned on, in
`all' mode (all hunks refined).

Customize variable `magit-diff-refine-hunk' to change the default mode."
  (interactive "P")
  (setq-local magit-diff-refine-hunk
              (if style
                  (if (eq magit-diff-refine-hunk 'all) t 'all)
                (not magit-diff-refine-hunk)))
  (magit-diff-update-hunk-refinement))

;;;; Visit commands

(defun magit-diff-visit-file (file &optional other-window force-worktree)
  "From a diff, visit the corresponding file at the appropriate position.

If the diff shows changes in the worktree, the index, or `HEAD',
then visit the actual file.  Otherwise, when the diff is about an
older commit or a range, then visit the appropriate blob.

If point is on a removed line, then visit the blob for the first
parent of the commit which removed that line, i.e. the last
commit where that line still existed.  Otherwise visit the blob
for the commit whose changes are being shown.

When the file or blob to be displayed is already being displayed
in another window of the same frame, then just select that window
and adjust point.  Otherwise, or with a prefix argument, display
the buffer in another window."
  (interactive (list (--if-let (magit-file-at-point)
                         (expand-file-name it)
                       (user-error "No file at point"))
                     current-prefix-arg))
  (if (magit-file-accessible-directory-p file)
      (magit-diff-visit-directory file other-window)
    (let* ((hunk (magit-diff-visit--hunk))
           (line (and hunk (magit-diff-hunk-line   hunk)))
           (col  (and hunk (magit-diff-hunk-column hunk)))
           (rev  (if (and magit-diff-visit-previous-blob
                          (magit-section-match 'hunk)
                          (save-excursion
                            (goto-char (line-beginning-position))
                            (looking-at "-")))
                     (magit-diff-visit--range-beginning)
                   (magit-diff-visit--range-end)))
           (buf  (if (and (not force-worktree)
                          (stringp rev))
                     (magit-find-file-noselect rev file)
                   (or (get-file-buffer file)
                       (find-file-noselect file)))))
      (magit-display-file-buffer buf)
      (with-selected-window
          (or (get-buffer-window buf 'visible)
              (error "File buffer is not visible"))
        (when line
          (setq line
                (cond ((eq rev 'staged)
                       (apply 'magit-diff-visit--offset file nil line))
                      ((and force-worktree
                            (stringp rev))
                       (apply 'magit-diff-visit--offset file rev line))
                      (t
                       (apply '+ line))))
          (let ((pos (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (move-to-column col)
                       (point))))
            (unless (<= (point-min) pos (point-max))
              (widen)
              (goto-char pos))))
        (when (magit-anything-unmerged-p file)
          (smerge-start-session))
        (run-hooks 'magit-diff-visit-file-hook)))))

(defvar magit-display-file-buffer-function
  'magit-display-file-buffer-traditional
  "The function used by `magit-diff-visit-file' to display blob buffers.

Other commands such as `magit-find-file' do not use this
function.  Instead they use high-level functions to select the
window to be used to display the buffer.  This variable and the
related functions are an experimental feature and should be
treated as such.")

(defun magit-display-file-buffer (buffer)
  (funcall magit-display-file-buffer-function buffer))

(defun magit-display-file-buffer-traditional (buffer)
  "Display BUFFER in the current window.
With a prefix argument display it in another window.
Option `magit-display-file-buffer-function' controls
whether `magit-diff-visit-file' uses this function."
  (if (or current-prefix-arg (get-buffer-window buffer))
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))

(defun magit-display-file-buffer-other-window (buffer)
  "Display BUFFER in another window.
With a prefix argument display it in the current window.
Option `magit-display-file-buffer-function' controls
whether `magit-diff-visit-file' uses this function."
  (if (or current-prefix-arg (get-buffer-window buffer))
      (switch-to-buffer buffer)
    (pop-to-buffer buffer)))

(defun magit-diff-visit-file-worktree (file &optional other-window)
  "From a diff, visit the corresponding file at the appropriate position.

When the file is already being displayed in another window of the
same frame, then just select that window and adjust point.  With
a prefix argument also display in another window.

The actual file in the worktree is visited. The positions in the
hunk headers get less useful the \"older\" the changes are, and
as a result, jumping to the appropriate position gets less
reliable.

Also see `magit-diff-visit-file' which visits the respective
blob, unless the diff shows changes in the worktree, the index,
or `HEAD'."
  (interactive (list (or (magit-file-at-point)
                         (user-error "No file at point"))
                     current-prefix-arg))
  (magit-diff-visit-file file other-window t))

(defun magit-diff-visit--range-end ()
  (let ((rev (magit-diff--dwim)))
    (if (symbolp rev)
        rev
      (setq rev (if (consp rev)
                    (cdr rev)
                  (cdr (magit-split-range rev))))
      (if (magit-rev-head-p rev)
          'unstaged
        rev))))

(defun magit-diff-visit--range-beginning ()
  (let ((rev (magit-diff--dwim)))
    (cond ((consp rev)
           (concat (cdr rev) "^"))
          ((stringp rev)
           (car (magit-split-range rev)))
          (t
           rev))))

(defun magit-diff-visit--hunk ()
  (-when-let (scope (magit-diff-scope))
    (let ((section (magit-current-section)))
      (cl-case scope
        ((file files)
         (setq section (car (magit-section-children section))))
        (list
         (setq section (car (magit-section-children section)))
         (when section
           (setq section (car (magit-section-children section))))))
      (and
       ;; Unmerged files appear in the list of staged changes
       ;; but unlike in the list of unstaged changes no diffs
       ;; are shown here.  In that case `section' is nil.
       section
       ;; Currently the `hunk' type is also abused for file
       ;; mode changes, which we are not interested in here.
       ;; Such sections have no value.
       (magit-section-value section)
       section))))

(defun magit-diff-visit--offset (file rev hunk-start line-offset)
  (let ((offset 0))
    (with-temp-buffer
      (save-excursion
        (magit-with-toplevel
          (magit-git-insert "diff" rev "--" file)))
      (catch 'found
        (while (re-search-forward
                "^@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@"
                nil t)
          (let* ((abeg (string-to-number (match-string 1)))
                 (alen (string-to-number (match-string 2)))
                 (bbeg (string-to-number (match-string 3)))
                 (blen (string-to-number (match-string 4)))
                 (aend (+ abeg alen))
                 (bend (+ bbeg blen))
                 (hend (+ hunk-start line-offset)))
            (if (<= abeg hunk-start)
                (if (or (>= aend hend)
                        (>= bend hend))
                    (let ((line 0))
                      (while (<= line alen)
                        (forward-line 1)
                        (cl-incf line)
                        (cond ((looking-at "^\\+") (cl-incf offset))
                              ((looking-at "^-")   (cl-decf offset)))))
                  (cl-incf offset (- blen alen)))
              (throw 'found nil))))))
    (+ hunk-start line-offset offset)))

(defun magit-diff-hunk-line (section)
  (let* ((value  (magit-section-value section))
         (prefix (- (length value) 2))
         (cpos   (marker-position (magit-section-content section)))
         (stop   (line-number-at-pos))
         (cstart (save-excursion (goto-char cpos)
                                 (line-number-at-pos)))
         (prior  (and (= (length value) 3)
                      (save-excursion (goto-char (line-beginning-position))
                                      (looking-at "-"))))
         (offset 0)
         (line   (if prior
                     (cadr value)
                   (car (last value)))))
    (string-match (format "^%s\\([0-9]+\\)" (if prior "-" "\\+")) line)
    (setq line (string-to-number (match-string 1 line)))
    (when (> cstart stop)
      (save-excursion
        (goto-char cpos)
        (re-search-forward "^[-+]")
        (setq stop (line-number-at-pos))))
    (save-excursion
      (goto-char cpos)
      (while (< (line-number-at-pos) stop)
        (unless (string-match-p
                 (if prior "\\+" "-")
                 (buffer-substring (point) (+ (point) prefix)))
          (cl-incf offset))
        (forward-line)))
    (list line offset)))

(defun magit-diff-hunk-column (section)
  (if (or (< (point) (magit-section-content section))
          (save-excursion (beginning-of-line) (looking-at-p "-")))
      0
    (max 0 (- (+ (current-column) 2)
              (length (magit-section-value section))))))

(defun magit-diff-visit-directory (directory &optional other-window)
  (if (equal (magit-toplevel directory)
             (magit-toplevel))
      (dired-jump other-window (concat directory "/."))
    (let ((display-buffer-overriding-action
           (if other-window
               '(nil (inhibit-same-window t))
             '(display-buffer-same-window))))
      (magit-status-internal directory))))

;;;; Scroll commands

(defun magit-diff-show-or-scroll-up ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer up.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-diff-show-or-scroll 'scroll-up))

(defun magit-diff-show-or-scroll-down ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer down.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-diff-show-or-scroll 'scroll-down))

(defun magit-diff-show-or-scroll (fn)
  (let (rev cmd buf win)
    (cond
     (magit-blame-mode
      (setq rev (magit-blame-chunk-get :hash))
      (setq cmd 'magit-show-commit)
      (setq buf (magit-mode-get-buffer 'magit-revision-mode)))
     ((derived-mode-p 'git-rebase-mode)
      (save-excursion
        (goto-char (line-beginning-position))
        (--if-let (and git-rebase-line
                       (looking-at git-rebase-line)
                       (match-string 2))
            (progn (setq rev it)
                   (setq cmd 'magit-show-commit)
                   (setq buf (magit-mode-get-buffer 'magit-revision-mode)))
          (user-error "No commit on this line"))))
     (t
      (magit-section-case
        (branch
         (setq rev (magit-ref-maybe-qualify (magit-section-value it)))
         (setq cmd 'magit-show-commit)
         (setq buf (magit-mode-get-buffer 'magit-revision-mode)))
        (commit
         (setq rev (magit-section-value it))
         (setq cmd 'magit-show-commit)
         (setq buf (magit-mode-get-buffer 'magit-revision-mode)))
        (stash
         (setq rev (magit-section-value it))
         (setq cmd 'magit-stash-show)
         (setq buf (magit-mode-get-buffer 'magit-stash-mode))))))
    (if rev
        (if (and buf
                 (setq win (get-buffer-window buf))
                 (with-current-buffer buf
                   (and (equal rev (car magit-refresh-args))
                        (equal (magit-rev-parse rev)
                               magit-buffer-revision-hash))))
            (with-selected-window win
              (condition-case nil
                  (funcall fn)
                (error
                 (goto-char (pcase fn
                              (`scroll-up   (point-min))
                              (`scroll-down (point-max)))))))
          (let ((magit-display-buffer-noselect t))
            (if (eq cmd 'magit-show-commit)
                (apply #'magit-show-commit rev (magit-show-commit--arguments))
              (funcall cmd rev))))
      (call-interactively #'magit-show-commit))))

;;; Diff Mode

(defvar magit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-c\C-d" 'magit-diff-while-committing)
    (define-key map "\C-c\C-b" 'magit-go-backward)
    (define-key map "\C-c\C-f" 'magit-go-forward)
    (define-key map "\s" 'scroll-up)
    (define-key map "\d" 'scroll-down)
    (define-key map "j" 'magit-jump-to-diffstat-or-diff)
    map)
  "Keymap for `magit-diff-mode'.")

(define-derived-mode magit-diff-mode magit-mode "Magit Diff"
  "Mode for looking at a Git diff.

This mode is documented in info node `(magit)Diff Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the hunk or file at point.

Staging and applying changes is documented in info node
`(magit)Staging and Unstaging' and info node `(magit)Applying'.

\\<magit-hunk-section-map>Type \
\\[magit-apply] to apply the change at point, \
\\[magit-stage] to stage,
\\[magit-unstage] to unstage, \
\\[magit-discard] to discard, or \
\\[magit-reverse] to reverse it.

\\{magit-diff-mode-map}"
  :group 'magit-diff
  (hack-dir-local-variables-non-file-buffer)
  (setq imenu-prev-index-position-function
        'magit-imenu--diff-prev-index-position-function)
  (setq imenu-extract-index-name-function
        'magit-imenu--diff-extract-index-name-function)
  (setq-local bookmark-make-record-function
              'magit-bookmark--diff-make-record))

(defun magit-diff-refresh-buffer (rev-or-range const _args files)
  "Refresh the current `magit-diff-mode' buffer.

In such buffers the buffer-local value of `magit-refresh-args'
has the same form as the arguments of this function.  The value
is set in `magit-mode-setup'."
  (setq header-line-format
        (propertize
         (if (member "--no-index" const)
             (apply #'format " Differences between %s and %s" files)
           (concat (if rev-or-range
                       (if (string-match-p "\\(\\.\\.\\|\\^-\\)"
                                           rev-or-range)
                           (format " Changes in %s" rev-or-range)
                         (format " Changes from %s to working tree" rev-or-range))
                     (if (member "--cached" const)
                         " Staged changes"
                       " Unstaged changes"))
                   (pcase (length files)
                     (0)
                     (1 (concat " in file " (car files)))
                     (_ (concat " in files "
                                (mapconcat #'identity files ", "))))))
         'face 'magit-header-line))
  (magit-insert-section (diffbuf)
    (run-hook-with-args 'magit-diff-sections-hook rev-or-range)))

(defun magit-insert-diff (rev-or-range)
  "Insert the diff into this `magit-diff-mode' buffer."
  (let ((magit-git-global-arguments
         (remove "--literal-pathspecs" magit-git-global-arguments)))
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" rev-or-range "-p" "--no-prefix"
      (and (member "--stat" (nth 2 magit-refresh-args)) "--numstat")
      (nth 1 magit-refresh-args)
      (nth 2 magit-refresh-args) "--"
      (nth 3 magit-refresh-args))))

(defvar magit-file-section-map
  (let ((map (make-sparse-keymap)))
    (unless (featurep 'jkl)
      (define-key map (kbd "C-j") 'magit-diff-visit-file-worktree))
    (define-key map [C-return] 'magit-diff-visit-file-worktree)
    (define-key map [remap magit-visit-thing]      'magit-diff-visit-file)
    (define-key map [remap magit-delete-thing]     'magit-discard)
    (define-key map [remap magit-revert-no-commit] 'magit-reverse)
    (define-key map "a" 'magit-apply)
    (define-key map "C" 'magit-commit-add-log)
    (define-key map "s" 'magit-stage)
    (define-key map "u" 'magit-unstage)
    (define-key map "&" 'magit-do-async-shell-command)
    map)
  "Keymap for `file' sections.")

(defvar magit-hunk-section-map
  (let ((map (make-sparse-keymap)))
    (unless (featurep 'jkl)
      (define-key map (kbd "C-j") 'magit-diff-visit-file-worktree))
    (define-key map [C-return] 'magit-diff-visit-file-worktree)
    (define-key map [remap magit-visit-thing]      'magit-diff-visit-file)
    (define-key map [remap magit-delete-thing]     'magit-discard)
    (define-key map [remap magit-revert-no-commit] 'magit-reverse)
    (define-key map "a" 'magit-apply)
    (define-key map "C" 'magit-commit-add-log)
    (define-key map "s" 'magit-stage)
    (define-key map "u" 'magit-unstage)
    (define-key map "&" 'magit-do-async-shell-command)
    map)
  "Keymap for `hunk' sections.")

(defconst magit-diff-headline-re
  (concat "^\\(@@@?\\|diff\\|Submodule\\|"
          "\\* Unmerged path\\|merged\\|changed in both\\|"
          "added in remote\\|removed in remote\\)"))

(defconst magit-diff-statline-re
  (concat "^ ?"
          "\\(.*\\)"     ; file
          "\\( +| +\\)"  ; separator
          "\\([0-9]+\\|Bin\\(?: +[0-9]+ -> [0-9]+ bytes\\)?$\\) ?"
          "\\(\\+*\\)"   ; add
          "\\(-*\\)$"))  ; del

(defun magit-diff-wash-diffs (args &optional limit)
  (when (member "--stat" args)
    (magit-diff-wash-diffstat))
  (when (re-search-forward magit-diff-headline-re limit t)
    (goto-char (line-beginning-position))
    (magit-wash-sequence (apply-partially 'magit-diff-wash-diff args))
    (insert ?\n)))

(defun magit-jump-to-diffstat-or-diff ()
  "Jump to the diffstat or diff.
When point is on a file inside the diffstat section, then jump
to the respective diff section, otherwise jump to the diffstat
section or a child thereof."
  (interactive)
  (--if-let (magit-get-section
             (append (magit-section-case
                       ([file diffstat] `((file . ,(magit-section-value it))))
                       (file `((file . ,(magit-section-value it)) (diffstat)))
                       (t '((diffstat))))
                     (magit-section-ident magit-root-section)))
      (magit-section-goto it)
    (user-error "No diffstat in this buffer")))

(defun magit-diff-wash-diffstat ()
  (let (heading (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string 1))
      (magit-delete-match)
      (goto-char beg)
      (magit-insert-section it (diffstat)
        (insert (propertize heading 'face 'magit-diff-file-heading))
        (magit-insert-heading)
        (let (files)
          (while (looking-at "^[-0-9]+\t[-0-9]+\t\\(.+\\)$")
            (push (magit-decode-git-path (match-string 1)) files)
            (magit-delete-line))
          (setq files (nreverse files))
          (while (looking-at magit-diff-statline-re)
            (magit-bind-match-strings (file sep cnt add del) nil
              (magit-delete-line)
              (when (string-match " +$" file)
                (setq sep (concat (match-string 0 file) sep))
                (setq file (substring file 0 (match-beginning 0))))
              (let ((le (length file)) ld)
                (setq file (magit-decode-git-path file))
                (setq ld (length file))
                (when (> le ld)
                  (setq sep (concat (make-string (- le ld) ?\s) sep))))
              (magit-insert-section (file (pop files))
                (insert (propertize file 'face 'magit-filename) sep cnt " ")
                (when add
                  (insert (propertize add 'face 'magit-diffstat-added)))
                (when del
                  (insert (propertize del 'face 'magit-diffstat-removed)))
                (insert "\n")))))
        (if (looking-at "^$") (forward-line) (insert "\n"))))))

(defun magit-diff-wash-diff (args)
  (cond
   ((looking-at "^Submodule")
    (magit-diff-wash-submodule))
   ((looking-at "^\\* Unmerged path \\(.*\\)")
    (let ((file (magit-decode-git-path (match-string 1))))
      (magit-delete-line)
      (unless (and (derived-mode-p 'magit-status-mode)
                   (not (member "--cached" args)))
        (magit-insert-section (file file)
          (insert (propertize
                   (format "unmerged   %s%s" file
                           (pcase (cddr (car (magit-file-status file)))
                             (`(?D ?D) " (both deleted)")
                             (`(?D ?U) " (deleted by us)")
                             (`(?U ?D) " (deleted by them)")
                             (`(?A ?A) " (both added)")
                             (`(?A ?U) " (added by us)")
                             (`(?U ?A) " (added by them)")
                             (`(?U ?U) "")))
                   'face 'magit-diff-file-heading))
          (insert ?\n))))
    t)
   ((looking-at (concat "^\\(merged\\|changed in both\\|"
                        "added in remote\\|removed in remote\\)"))
    (let ((status (pcase (match-string 1)
                    ("merged" "merged")
                    ("changed in both" "conflict")
                    ("added in remote" "new file")
                    ("removed in remote" "deleted")))
          file orig base modes)
      (magit-delete-line)
      (while (looking-at
              "^  \\([^ ]+\\) +[0-9]\\{6\\} \\([a-z0-9]\\{40\\}\\) \\(.+\\)$")
        (magit-bind-match-strings (side _blob name) nil
          (pcase side
            ("result" (setq file name))
            ("our" (setq orig name))
            ("their" (setq file name))
            ("base" (setq base name))))
        (magit-delete-line))
      (when orig (setq orig (magit-decode-git-path orig)))
      (when file (setq file (magit-decode-git-path file)))
      (magit-diff-insert-file-section (or file base) orig status modes nil)))
   ((looking-at
     "^diff --\\(?:\\(git\\) \\(?:\\(.+?\\) \\2\\)?\\|\\(cc\\|combined\\) \\(.+\\)\\)")
    (let ((status (cond ((equal (match-string 1) "git")        "modified")
                        ((derived-mode-p 'magit-revision-mode) "resolved")
                        (t                                     "unmerged")))
          (file (or (match-string 2) (match-string 4)))
          (beg (point))
          orig header modes)
      (save-excursion
        (forward-line 1)
        (setq header (buffer-substring
                      beg (if (re-search-forward magit-diff-headline-re nil t)
                              (match-beginning 0)
                            (point-max)))))
      (magit-delete-line)
      (while (not (or (eobp) (looking-at magit-diff-headline-re)))
        (if (looking-at "^old mode \\([^\n]+\\)\nnew mode \\([^\n]+\\)\n")
            (progn (setq modes (match-string 0))
                   (magit-delete-match))
          (cond
           ((looking-at "^--- \\([^/].*?\\)\t?$") ; i.e. not /dev/null
            (setq orig (match-string 1)))
           ((looking-at "^\\+\\+\\+ \\([^/].*?\\)\t?$")
            (setq file (match-string 1)))
           ((looking-at "^\\(copy\\|rename\\) from \\(.+\\)$")
            (setq orig (match-string 2)))
           ((looking-at "^\\(copy\\|rename\\) to \\(.+\\)$")
            (setq file (match-string 2))
            (setq status (if (equal (match-string 1) "copy") "new file" "renamed")))
           ((looking-at "^\\(new file\\|deleted\\)")
            (setq status (match-string 1))))
          (magit-delete-line)))
      (when orig
        (setq orig (magit-decode-git-path orig)))
      (setq file (magit-decode-git-path file))
      ;; KLUDGE `git-log' ignores `--no-prefix' when `-L' is used.
      (when (and (derived-mode-p 'magit-log-mode)
                 (--first (string-match-p "\\`-L" it)
                          (nth 1 magit-refresh-args)))
        (setq file (substring file 2))
        (when orig
          (setq orig (substring orig 2))))
      (magit-diff-insert-file-section file orig status modes header)))))

(defun magit-diff-insert-file-section (file orig status modes header)
  (magit-insert-section section
    (file file (or (equal status "deleted")
                   (derived-mode-p 'magit-status-mode)))
    (insert (propertize (format "%-10s %s\n" status
                                (if (or (not orig) (equal orig file))
                                    file
                                  (format "%s -> %s" orig file)))
                        'face 'magit-diff-file-heading))
    (magit-insert-heading)
    (unless (equal orig file)
      (setf (magit-section-source section) orig))
    (setf (magit-section-diff-header section) header)
    (when modes
      (magit-insert-section (hunk)
        (insert modes)))
    (magit-wash-sequence #'magit-diff-wash-hunk)))

(defun magit-diff-wash-submodule ()
  ;; See `show_submodule_summary' in submodule.c and "this" commit.
  (when (looking-at "^Submodule \\([^ ]+\\)")
    (let ((module (match-string 1))
          untracked modified)
      (when (looking-at "^Submodule [^ ]+ contains untracked content$")
        (magit-delete-line)
        (setq untracked t))
      (when (looking-at "^Submodule [^ ]+ contains modified content$")
        (magit-delete-line)
        (setq modified t))
      (cond
       ((and (looking-at "^Submodule \\([^ ]+\\) \\([^ :]+\\)\\( (rewind)\\)?:$")
             (equal (match-string 1) module))
        (magit-bind-match-strings (_module range rewind) nil
          (magit-delete-line)
          (while (looking-at "^  \\([<>]\\) \\(.+\\)$")
            (magit-delete-line))
          (when rewind
            (setq range (replace-regexp-in-string "[^.]\\(\\.\\.\\)[^.]"
                                                  "..." range t t 1)))
          (magit-insert-section (file module t)
            (magit-insert-heading
              (concat (propertize (concat "modified   " module)
                                  'face 'magit-diff-file-heading)
                      " ("
                      (cond (rewind "rewind")
                            ((string-match-p "\\.\\.\\." range) "non-ff")
                            (t "new commits"))
                      (and (or modified untracked)
                           (concat ", "
                                   (and modified "modified")
                                   (and modified untracked " and ")
                                   (and untracked "untracked")
                                   " content"))
                      ")"))
            (let ((default-directory
                    (file-name-as-directory
                     (expand-file-name module (magit-toplevel)))))
              (magit-git-wash (apply-partially 'magit-log-wash-log 'module)
                "log" "--oneline" "--left-right" range)
              (delete-char -1)))))
       ((and (looking-at "^Submodule \\([^ ]+\\) \\([^ ]+\\) (\\([^)]+\\))$")
             (equal (match-string 1) module))
        (magit-bind-match-strings (_module _range msg) nil
          (magit-delete-line)
          (magit-insert-section (file module)
            (magit-insert-heading
              (concat (propertize (concat "submodule  " module)
                                  'face 'magit-diff-file-heading)
                      " (" msg ")")))))
       (t
        (magit-insert-section (file module)
          (magit-insert-heading
            (concat (propertize (concat "modified   " module)
                                'face 'magit-diff-file-heading)
                    " ("
                    (and modified "modified")
                    (and modified untracked " and ")
                    (and untracked "untracked")
                    " content)"))))))))

(defun magit-diff-wash-hunk ()
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    (let ((heading (match-string 0))
          (value (cons (match-string 2) (split-string (match-string 1)))))
      (magit-delete-line)
      (magit-insert-section it (hunk value)
        (insert (propertize (concat heading "\n") 'face 'magit-diff-hunk-heading))
        (magit-insert-heading)
        (while (not (or (eobp) (looking-at "^[^-+\s\\]")))
          (forward-line))
        (setf (magit-section-end it) (point))
        (setf (magit-section-washer it) #'magit-diff-paint-hunk)))
    t))

(defun magit-diff-expansion-threshold (section)
  "Keep new diff sections collapsed if washing takes too long."
  (and (memq (magit-section-type section) '(file))
       (> (float-time (time-subtract (current-time) magit-refresh-start-time))
          magit-diff-expansion-threshold)
       'hide))

;;; Revision Mode

(define-derived-mode magit-revision-mode magit-diff-mode "Magit Rev"
  "Mode for looking at a Git commit.

This mode is documented in info node `(magit)Revision Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the hunk or file at point.

Staging and applying changes is documented in info node
`(magit)Staging and Unstaging' and info node `(magit)Applying'.

\\<magit-hunk-section-map>Type \
\\[magit-apply] to apply the change at point, \
\\[magit-stage] to stage,
\\[magit-unstage] to unstage, \
\\[magit-discard] to discard, or \
\\[magit-reverse] to reverse it.

\\{magit-revision-mode-map}"
  :group 'magit-revision
  (hack-dir-local-variables-non-file-buffer)
  (setq-local bookmark-make-record-function
              'magit-bookmark--revision-make-record))

(defun magit-revision-refresh-buffer (rev __const _args files)
  (setq header-line-format
        (propertize (concat " " (capitalize (magit-object-type rev))
                            " " rev
                            (pcase (length files)
                              (0)
                              (1 (concat " limited to file " (car files)))
                              (_ (concat " limited to files "
                                         (mapconcat #'identity files ", ")))))
                    'face 'magit-header-line))
  (setq magit-buffer-revision-hash (magit-rev-parse rev))
  (magit-insert-section (commitbuf)
    (run-hook-with-args 'magit-revision-sections-hook rev)))

(defun magit-insert-revision-diff (rev)
  "Insert the diff into this `magit-revision-mode' buffer."
  (let ((magit-git-global-arguments
         (remove "--literal-pathspecs" magit-git-global-arguments)))
    ;; Before v2.2.0, "--format=" did not mean "no output".
    ;; Instead the default format was used.  So use "--format=%n"
    ;; and then delete the empty lines.
    (magit-git-wash (lambda (args)
                      (delete-region (point) (progn (forward-line 3) (point)))
                      (magit-diff-wash-diffs args))
      "show" "-p" "--cc" "--format=%n" "--no-prefix"
      (and (member "--stat" (nth 2 magit-refresh-args)) "--numstat")
      (nth 2 magit-refresh-args) (concat rev "^{commit}") "--"
      (nth 3 magit-refresh-args))))

(defun magit-insert-revision-tag (rev)
  "Insert tag message and headers into a revision buffer.
This function only inserts anything when `magit-show-commit' is
called with a tag as argument, when that is called with a commit
or a ref which is not a branch, then it inserts nothing."
  (when (equal (magit-object-type rev) "tag")
    (magit-insert-section (taginfo)
      (let ((beg (point)))
        (magit-git-insert "cat-file" "tag" rev)
        (goto-char beg)
        (forward-line 3)
        (delete-region beg (point)))
      (looking-at "^tagger \\([^<]+\\) <\\([^>]+\\)")
      (let ((heading (format "Tagger: %s <%s>"
                             (match-string 1)
                             (match-string 2))))
        (magit-delete-line)
        (insert (propertize heading 'face 'magit-section-secondary-heading)))
      (magit-insert-heading)
      (goto-char (point-max))
      (insert ?\n))))

(defun magit-insert-revision-message (rev)
  "Insert the commit message into a revision buffer."
  (magit-insert-section (message)
    (let ((beg (point)))
      (magit-rev-insert-format "%B" rev)
      (if (= (point) (+ beg 2))
          (progn (backward-delete-char 2)
                 (insert "(no message)\n"))
        (goto-char beg)
        (save-excursion
          (while (search-forward "\r\n" nil t) ; Remove trailing CRs.
            (delete-region (match-beginning 0) (1+ (match-beginning 0)))))
        (save-excursion
          (forward-line)
          (put-text-property beg (point) 'face 'magit-section-secondary-heading)
          (magit-insert-heading))
        (when magit-diff-highlight-keywords
          (save-excursion
            (while (re-search-forward "\\[[^[]*\\]" nil t)
              (put-text-property (match-beginning 0)
                                 (match-end 0)
                                 'face 'magit-keyword))))
        (goto-char (point-max))))))

(defun magit-insert-revision-notes (rev)
  "Insert commit notes into a revision buffer."
  (magit-insert-section (notes)
    (let ((beg (point)))
      (magit-git-insert "notes" "show" rev)
      (if (= (point) beg)
          (magit-cancel-section)
        (goto-char beg)
        (forward-line)
        (put-text-property beg (point) 'face 'magit-section-secondary-heading)
        (magit-insert-heading)
        (goto-char (point-max))
        (insert ?\n)))))

(defun magit-insert-revision-headers (rev)
  "Insert headers about the commit into a revision buffer."
  (magit-insert-section (headers)
    ;; Before v2.2.0, "%D" was not supported.
    (--when-let (magit-rev-format "%d" rev "--decorate=full")
      (insert (magit-format-ref-labels it) ?\s))
    (insert (propertize (magit-rev-parse (concat rev "^{commit}"))
                        'face 'magit-hash))
    (magit-insert-heading)
    (let ((beg (point)))
      (magit-rev-insert-format magit-revision-headers-format rev)
      (magit-insert-revision-gravatars rev beg))
    (when magit-revision-insert-related-refs
      (dolist (parent (magit-commit-parents rev))
        (magit-insert-section (commit parent)
          (let ((line (magit-rev-format "%h %s" parent)))
            (string-match "^\\([^ ]+\\) \\(.*\\)" line)
            (magit-bind-match-strings (hash msg) line
              (insert "Parent:     ")
              (insert (propertize hash 'face 'magit-hash))
              (insert " " msg "\n")))))
      (-when-let (merged (magit-list-merged-branches rev))
        (insert "Merged:    ")
        (let (branch)
          (while (and (< (+ (- (point) (line-beginning-position))
                            (length (car merged)) 9)
                         (window-width))
                      (setq branch (pop merged)))
            (insert ?\s)
            (magit-insert-section (branch branch)
              (insert (propertize branch 'face 'magit-branch-local)))))
        (when merged
          (insert (format " (%s more)" (length merged))))
        (insert ?\n))
      (-when-let (containing (magit-list-containing-branches rev))
        (insert "Containing:")
        (let (branch)
          (while (and (< (+ (- (point) (line-beginning-position))
                            (length (car containing)) 9)
                         (window-width))
                      (setq branch (pop containing)))
            (insert ?\s)
            (magit-insert-section (branch branch)
              (insert (propertize branch 'face 'magit-branch-local)))))
        (when containing
          (insert (format " (%s more)" (length containing))))
        (insert ?\n))
      (-when-let (follows (magit-get-current-tag rev t))
        (let ((tag (car  follows))
              (cnt (cadr follows)))
          (magit-insert-section (tag tag)
            (insert (format "Follows:    %s (%s)\n"
                            (propertize tag 'face 'magit-tag)
                            (propertize (number-to-string cnt)
                                        'face 'magit-branch-local))))))
      (-when-let (precedes (magit-get-next-tag rev t))
        (let ((tag (car  precedes))
              (cnt (cadr precedes)))
          (magit-insert-section (tag tag)
            (insert (format "Precedes:   %s (%s)\n"
                            (propertize tag 'face 'magit-tag)
                            (propertize (number-to-string cnt)
                                        'face 'magit-tag))))))
      (insert ?\n))))

(defun magit-insert-revision-gravatars (rev beg)
  (when (and magit-revision-show-gravatars (window-system))
    (require 'gravatar)
    (magit-insert-revision-gravatar beg (magit-rev-format "%aE" rev)
                                    (car magit-revision-show-gravatars))
    (magit-insert-revision-gravatar beg (magit-rev-format "%cE" rev)
                                    (cdr magit-revision-show-gravatars))
    (goto-char (point-max))))

(defun magit-insert-revision-gravatar (beg email regexp)
  (when (and email (goto-char beg) (re-search-forward regexp nil t))
    (ignore-errors
      (let* ((offset   (length (match-string 0)))
             (font-obj (query-font (font-at (point) (get-buffer-window))))
             (size     (* 2 (+ (aref font-obj 4) (aref font-obj 5))))
             (align-to (+ offset (ceiling (/ size (aref font-obj 7) 1.0))))
             (gravatar-size (- size 2))
             (slice1  '(slice .0 .0 1.0 0.5))
             (slice2  '(slice .0 .5 1.0 1.0))
             (image    (gravatar-retrieve-synchronously email)))
        (unless (eq image 'error)
          (when magit-revision-use-gravatar-kludge
            (cl-rotatef slice1 slice2))
          (insert (propertize " " 'display `((,@image :ascent center :relief 1)
                                             ,slice1)))
          (insert (propertize " " 'display `((space :align-to ,align-to))))
          (insert " ")
          (forward-line)
          (forward-char offset)
          (insert (propertize " " 'display `((,@image :ascent center :relief 1)
                                             ,slice2)))
          (insert (propertize " " 'display `((space :align-to ,align-to))))
          (insert " "))))))

;;; Diff Sections

(defvar magit-unstaged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]  'magit-diff-unstaged)
    (define-key map [remap magit-delete-thing] 'magit-discard)
    (define-key map "s" 'magit-stage)
    (define-key map "u" 'magit-unstage)
    map)
  "Keymap for the `unstaged' section.")

(magit-define-section-jumper magit-jump-to-unstaged "Unstaged changes" unstaged)

(defun magit-insert-unstaged-changes ()
  "Insert section showing unstaged changes."
  (magit-insert-section (unstaged)
    (magit-insert-heading "Unstaged changes:")
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" magit-diff-section-arguments "--no-prefix"
      "--" magit-diff-section-file-args)))

(defvar magit-staged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]      'magit-diff-staged)
    (define-key map [remap magit-delete-thing]     'magit-discard)
    (define-key map [remap magit-revert-no-commit] 'magit-reverse)
    (define-key map "s" 'magit-stage)
    (define-key map "u" 'magit-unstage)
    map)
  "Keymap for the `staged' section.")

(magit-define-section-jumper magit-jump-to-staged "Staged changes" staged)

(defun magit-insert-staged-changes ()
  "Insert section showing staged changes."
  ;; Avoid listing all files as deleted when visiting a bare repo.
  (unless (magit-bare-repo-p)
    (magit-insert-section (staged)
      (magit-insert-heading "Staged changes:")
      (magit-git-wash #'magit-diff-wash-diffs
        "diff" "--cached" magit-diff-section-arguments "--no-prefix"
        "--" magit-diff-section-file-args))))

;;; Diff Type

(defun magit-diff-type (&optional section)
  "Return the diff type of SECTION.

The returned type is one of the symbols `staged', `unstaged',
`committed', or `undefined'.  This type serves a similar purpose
as the general type common to all sections (which is stored in
the `type' slot of the corresponding `magit-section' struct) but
takes additional information into account.  When the SECTION
isn't related to diffs and the buffer containing it also isn't
a diff-only buffer, then return nil.

Currently the type can also be one of `tracked' and `untracked'
but these values are not handled explicitly everywhere they
should be and a possible fix could be to just return nil here.

The section has to be a `diff' or `hunk' section, or a section
whose children are of type `diff'.  If optional SECTION is nil,
return the diff type for the current section.  In buffers whose
major mode is `magit-diff-mode' SECTION is ignored and the type
is determined using other means.  In `magit-revision-mode'
buffers the type is always `committed'.

Do not confuse this with `magit-diff-scope' (which see)."
  (--when-let (or section (magit-current-section))
    (cond ((derived-mode-p 'magit-revision-mode 'magit-stash-mode) 'committed)
          ((derived-mode-p 'magit-diff-mode)
           (let ((range (nth 0 magit-refresh-args))
                 (const (nth 1 magit-refresh-args)))
             (cond ((member "--no-index" const) 'undefined)
                   ((not range)
                    (if (member "--cached" const)
                        'staged
                      'unstaged))
                   ((member "--cached" const)
                    (if (magit-rev-head-p range)
                        'staged
                      'undefined)) ; i.e. committed and staged
                   (t 'committed))))
          ((derived-mode-p 'magit-status-mode)
           (let ((stype (magit-section-type it)))
             (if (memq stype '(staged unstaged tracked untracked))
                 stype
               (pcase stype
                 (`file (let* ((parent (magit-section-parent it))
                               (type   (magit-section-type parent)))
                          (if (eq type 'file)
                              (magit-diff-type parent)
                            type)))
                 (`hunk (-> it magit-section-parent magit-section-parent
                            magit-section-type))))))
          ((derived-mode-p 'magit-log-mode)
           (if (or (and (magit-section-match 'commit section)
                        (magit-section-children section))
                   (magit-section-match [* file commit] section))
               'committed
           'undefined))
          (t 'undefined))))

(cl-defun magit-diff-scope (&optional (section nil ssection) strict)
  "Return the diff scope of SECTION or the selected section(s).

A diff's \"scope\" describes what part of a diff is selected, it is
a symbol, one of `region', `hunk', `hunks', `file', `files', or
`list'.  Do not confuse this with the diff \"type\", as returned by
`magit-diff-type'.

If optional SECTION is non-nil, then return the scope of that,
ignoring the sections selected by the region.  Otherwise return
the scope of the current section, or if the region is active and
selects a valid group of diff related sections, the type of these
sections, i.e. `hunks' or `files'.  If SECTION, or if that is nil
the current section, is a `hunk' section; and the region region
starts and ends inside the body of a that section, then the type
is `region'.  If the region is empty after a mouse click, then
`hunk' is returned instead of `region'.

If optional STRICT is non-nil, then return nil if the diff type of
the section at point is `untracked' or the section at point is not
actually a `diff' but a `diffstat' section."
  (let ((siblings (and (not ssection) (magit-region-sections))))
    (setq section (or section (car siblings) (magit-current-section)))
    (when (and section
               (or (not strict)
                   (and (not (eq (magit-diff-type section) 'untracked))
                        (not (eq (--when-let (magit-section-parent section)
                                   (magit-section-type it))
                                 'diffstat)))))
      (pcase (list (magit-section-type section)
                   (and siblings t)
                   (magit-diff-use-hunk-region-p)
                   ssection)
        (`(hunk nil   t  ,_)
         (if (magit-section-internal-region-p section) 'region 'hunk))
        (`(hunk   t   t nil) 'hunks)
        (`(hunk  ,_  ,_  ,_) 'hunk)
        (`(file   t   t nil) 'files)
        (`(file  ,_  ,_  ,_) 'file)
        (`(,(or `staged `unstaged `untracked)
           nil ,_ ,_) 'list)))))

(defun magit-diff-use-hunk-region-p ()
  (and (region-active-p)
       (not (and (if (version< emacs-version "25.1")
                     (eq this-command 'mouse-drag-region)
                   ;; TODO implement this from first principals
                   ;; currently it's trial-and-error
                   (or (eq this-command 'mouse-drag-region)
                       (eq last-command 'mouse-drag-region)
                       ;; When another window was previously
                       ;; selected then the last-command is
                       ;; some byte-code function.
                       (byte-code-function-p last-command)))
                 (eq (region-end) (region-beginning))))))

;;; Diff Highlight

(defun magit-diff-unhighlight (section selection)
  "Remove the highlighting of the diff-related SECTION."
  (when (eq (magit-section-type section) 'hunk)
    (magit-diff-paint-hunk section selection nil)
    t))

(defun magit-diff-highlight (section selection)
  "Highlight the diff-related SECTION.
If SECTION is not a diff-related section, then do nothing and
return nil.  If SELECTION is non-nil, then it is a list of sections
selected by the region, including SECTION.  All of these sections
are highlighted."
  (if (and (magit-section-match 'commit section)
           (magit-section-children section))
      (progn (if selection
                 (dolist (section selection)
                   (magit-diff-highlight-list section selection))
               (magit-diff-highlight-list section))
             t)
    (-when-let (scope (magit-diff-scope section t))
      (cond ((eq scope 'region)
             (magit-diff-paint-hunk section selection t))
            (selection
             (dolist (section selection)
               (magit-diff-highlight-recursive section selection)))
            (t
             (magit-diff-highlight-recursive section)))
      t)))

(defun magit-diff-highlight-recursive (section &optional selection)
  (pcase (magit-diff-scope section)
    (`list (magit-diff-highlight-list section selection))
    (`file (magit-diff-highlight-file section selection))
    (`hunk (magit-diff-highlight-heading section selection)
           (magit-diff-paint-hunk section selection t))
    (_     (magit-section-highlight section nil))))

(defun magit-diff-highlight-list (section &optional selection)
  (let ((beg (magit-section-start   section))
        (cnt (magit-section-content section))
        (end (magit-section-end     section)))
    (unless (and (region-active-p)
                 (= end (1+ (region-end))))
      (magit-section-make-overlay beg cnt 'magit-section-highlight)
      (unless (magit-section-hidden section)
        (dolist (child (magit-section-children section))
          (magit-diff-highlight-recursive child selection))))
    (when magit-diff-highlight-hunk-body
      (magit-section-make-overlay (1- end) end 'magit-section-highlight))))

(defun magit-diff-highlight-file (section &optional selection)
  (magit-diff-highlight-heading section selection)
  (unless (magit-section-hidden section)
    (dolist (child (magit-section-children section))
      (magit-diff-highlight-recursive child selection))))

(defun magit-diff-highlight-heading (section &optional selection)
  (magit-section-make-overlay
   (magit-section-start section)
   (or (magit-section-content section)
       (magit-section-end     section))
   (pcase (list (magit-section-type section)
                (and (member section selection) t))
     (`(file   t) 'magit-diff-file-heading-selection)
     (`(file nil) 'magit-diff-file-heading-highlight)
     (`(hunk   t) 'magit-diff-hunk-heading-selection)
     (`(hunk nil) 'magit-diff-hunk-heading-highlight))))

;;; Hunk Paint

(cl-defun magit-diff-paint-hunk
    (section &optional selection
             (highlight (magit-section-selected-p section selection)))
  (let (paint)
    (unless magit-diff-highlight-hunk-body
      (setq highlight nil))
    (cond (highlight
           (unless (magit-section-hidden section)
             (add-to-list 'magit-section-highlighted-sections section)
             (cond ((memq section magit-section-unhighlight-sections)
                    (setq magit-section-unhighlight-sections
                          (delq section magit-section-unhighlight-sections)))
                   (magit-diff-highlight-hunk-body
                    (setq paint t)))))
          (t
           (cond ((and (magit-section-hidden section)
                       (memq section magit-section-unhighlight-sections))
                  (add-to-list 'magit-section-highlighted-sections section)
                  (setq magit-section-unhighlight-sections
                        (delq section magit-section-unhighlight-sections)))
                 (t
                  (setq paint t)))))
    (when paint
      (save-excursion
        (goto-char (magit-section-start section))
        (let ((end (magit-section-end section))
              (merging (looking-at "@@@"))
              (stage nil))
          (forward-line)
          (while (< (point) end)
            (when (and magit-diff-hide-trailing-cr-characters
                       (char-equal ?\r (char-before (line-end-position))))
              (put-text-property (1- (line-end-position)) (line-end-position)
                                 'invisible t))
            (put-text-property
             (point) (1+ (line-end-position)) 'face
             (cond
              ((looking-at "^\\+\\+?\\([<=|>]\\)\\{7\\}")
               (setq stage (pcase (list (match-string 1) highlight)
                             (`("<" nil) 'magit-diff-our)
                             (`("<"   t) 'magit-diff-our-highlight)
                             (`("|" nil) 'magit-diff-base)
                             (`("|"   t) 'magit-diff-base-highlight)
                             (`("=" nil) 'magit-diff-their)
                             (`("="   t) 'magit-diff-their-highlight)
                             (`(">" nil) nil)))
               'magit-diff-conflict-heading)
              ((looking-at (if merging  "^\\(\\+\\| \\+\\)" "^\\+"))
               (magit-diff-paint-whitespace merging)
               (or stage
                   (if highlight 'magit-diff-added-highlight 'magit-diff-added)))
              ((looking-at (if merging  "^\\(-\\| -\\)" "^-"))
               (if highlight 'magit-diff-removed-highlight 'magit-diff-removed))
              (t
               (if highlight 'magit-diff-context-highlight 'magit-diff-context))))
            (forward-line))))))
  (magit-diff-update-hunk-refinement section))

(defun magit-diff-paint-whitespace (merging)
  (when (and magit-diff-paint-whitespace
             (or (derived-mode-p 'magit-status-mode)
                 (not (eq magit-diff-paint-whitespace 'status))))
    (let ((prefix (if merging "^[-\\+\s]\\{2\\}" "^[-\\+]"))
          (indent
           (if (local-variable-p 'magit-diff-highlight-indentation)
               magit-diff-highlight-indentation
             (setq-local
              magit-diff-highlight-indentation
              (cdr (--first (string-match-p (car it) default-directory)
                            (nreverse
                             (default-value
                               'magit-diff-highlight-indentation))))))))
      (when (and magit-diff-highlight-trailing
                 (looking-at (concat prefix ".*?\\([ \t]+\\)$")))
        (let ((ov (make-overlay (match-beginning 1) (match-end 1) nil t)))
          (overlay-put ov 'face 'magit-diff-whitespace-warning)
          (overlay-put ov 'evaporate t)))
      (when (or (and (eq indent 'tabs)
                     (looking-at (concat prefix "\\( *\t[ \t]*\\)")))
                (and (integerp indent)
                     (looking-at (format "%s\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                         prefix indent))))
        (let ((ov (make-overlay (match-beginning 1) (match-end 1) nil t)))
          (overlay-put ov 'face 'magit-diff-whitespace-warning)
          (overlay-put ov 'evaporate t))))))

(defun magit-diff-update-hunk-refinement (&optional section)
  (if section
      (unless (magit-section-hidden section)
        (pcase (list magit-diff-refine-hunk
                     (magit-section-refined section)
                     (eq section (magit-current-section)))
          ((or `(all nil ,_) `(t nil t))
           (setf (magit-section-refined section) t)
           (save-excursion
             (goto-char (magit-section-start section))
             ;; `diff-refine-hunk' does not handle combined diffs.
             (unless (looking-at "@@@")
               ;; Avoid fsyncing many small temp files
               (let ((write-region-inhibit-fsync t))
                 (diff-refine-hunk)))))
          ((or `(nil t ,_) `(t t nil))
           (setf (magit-section-refined section) nil)
           (remove-overlays (magit-section-start section)
                            (magit-section-end   section)
                            'diff-mode 'fine))))
    (cl-labels ((recurse (section)
                         (if (magit-section-match 'hunk section)
                             (magit-diff-update-hunk-refinement section)
                           (--each (magit-section-children section)
                             (recurse it)))))
      (recurse magit-root-section))))


;;; Hunk Region

(defun magit-diff-hunk-region-beginning ()
  (save-excursion (goto-char (region-beginning))
                  (line-beginning-position)))

(defun magit-diff-hunk-region-end ()
  (save-excursion (goto-char (region-end))
                  (line-end-position)))

(defun magit-diff-update-hunk-region (section)
  "Highlight the hunk-internal region if any."
  (when (eq (magit-diff-scope section t) 'region)
    (magit-diff--make-hunk-overlay
     (magit-section-start section)
     (1- (magit-section-content section))
     'face 'magit-diff-lines-heading
     'display (magit-diff-hunk-region-header section)
     'after-string (magit-diff--hunk-after-string 'magit-diff-lines-heading))
    (run-hook-with-args 'magit-diff-highlight-hunk-region-functions section)
    t))

(defun magit-diff-highlight-hunk-region-dim-outside (section)
  "Dim the parts of the hunk that are outside the hunk-internal region.
This is done by using the same foreground and background color
for added and removed lines as for context lines."
  (let ((face (if magit-diff-highlight-hunk-body
                  'magit-diff-context-highlight
                'magit-diff-context)))
    (when magit-diff-unmarked-lines-keep-foreground
      (setq face (list :background (face-attribute face :background))))
    (magit-diff--make-hunk-overlay (magit-section-content section)
                                   (magit-diff-hunk-region-beginning)
                                   'face face
                                   'priority 2)
    (magit-diff--make-hunk-overlay (1+ (magit-diff-hunk-region-end))
                                   (magit-section-end section)
                                   'face face
                                   'priority 2)))

(defun magit-diff-highlight-hunk-region-using-face (_section)
  "Highlight the hunk-internal region by making it bold.
Or rather highlight using the face `magit-diff-hunk-region', though
changing only the `:weight' and/or `:slant' is recommended for that
face."
  (magit-diff--make-hunk-overlay (magit-diff-hunk-region-beginning)
                                 (1+ (magit-diff-hunk-region-end))
                                 'face 'magit-diff-hunk-region))

(defun magit-diff-highlight-hunk-region-using-overlays (section)
  "Emphasize the hunk-internal region using delimiting horizontal lines.
This is implemented as single-pixel newlines places inside overlays.
Although creating overlays containing newlines is discouraged,
this version turns out to be less glitchy on Emacs 24 than the
other method."
  (if (window-system)
      (let ((beg (magit-diff-hunk-region-beginning))
            (end (magit-diff-hunk-region-end))
            (str (propertize
                  (concat (propertize "\s" 'display '(space :height (1)))
                          (propertize "\n" 'line-height t))
                  'face 'magit-diff-lines-boundary)))
        (magit-diff--make-hunk-overlay beg (1+ beg) 'before-string str)
        (magit-diff--make-hunk-overlay end (1+ end) 'after-string  str))
    (magit-diff-highlight-hunk-region-using-face section)))

(defun magit-diff-highlight-hunk-region-using-underline (section)
  "Emphasize the hunk-internal region using delimiting horizontal lines.
This is implemented by overlining and underlining the first and
last (visual) lines of the region.  In Emacs 24, using this
method causes `move-end-of-line' to jump to the next line, so
we only use it in Emacs 25 where that glitch was fixed (see
https://github.com/magit/magit/pull/2293 for more details)."
  (if (window-system)
      (let* ((beg (magit-diff-hunk-region-beginning))
             (end (magit-diff-hunk-region-end))
             (beg-eol (save-excursion (goto-char beg)
                                      (end-of-visual-line)
                                      (point)))
             (end-bol (save-excursion (goto-char end)
                                      (beginning-of-visual-line)
                                      (point)))
             (color (face-background 'magit-diff-lines-boundary nil t)))
        (cl-flet ((ln (b e &rest face)
                      (magit-diff--make-hunk-overlay
                       b e 'face face 'after-string
                       (magit-diff--hunk-after-string face))))
          (if (= beg end-bol)
              (ln beg beg-eol :overline color :underline color)
            (ln beg beg-eol :overline color)
            (ln end-bol end :underline color))))
    (magit-diff-highlight-hunk-region-using-face section)))

(defun magit-diff--make-hunk-overlay (start end &rest args)
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'evaporate t)
    (while args (overlay-put ov (pop args) (pop args)))
    (push ov magit-region-overlays)
    ov))

(defun magit-diff--hunk-after-string (face)
  (propertize "\s"
              'face face
              'display (list 'space :align-to
                             `(+ (0 . right)
                                 ,(min (window-hscroll)
                                       (- (line-end-position)
                                          (line-beginning-position)))))
              ;; This prevents the cursor from being rendered at the
              ;; edge of the window.
              'cursor t))

;;; Hunk Utilities

(defun magit-diff-inside-hunk-body-p ()
  "Return non-nil if point is inside the body of a hunk."
  (and (magit-section-match 'hunk)
       (> (point) (magit-section-content (magit-current-section)))))

;;; Diff Extract

(defun magit-diff-file-header (section)
  (when (eq (magit-section-type section) 'hunk)
    (setq section (magit-section-parent section)))
  (when (eq (magit-section-type section) 'file)
    (magit-section-diff-header section)))

(defun magit-diff-hunk-region-header (section)
  (let ((patch (magit-diff-hunk-region-patch section)))
    (string-match "\n" patch)
    (substring patch 0 (1- (match-end 0)))))

(defun magit-diff-hunk-region-patch (section &optional args)
  (let ((op (if (member "--reverse" args) "+" "-"))
        (sbeg (magit-section-start section))
        (rbeg (magit-diff-hunk-region-beginning))
        (rend (region-end))
        (send (magit-section-end section))
        (patch nil))
    (save-excursion
      (goto-char sbeg)
      (while (< (point) send)
        (looking-at "\\(.\\)\\([^\n]*\n\\)")
        (cond ((or (string-match-p "[@ ]" (match-string-no-properties 1))
                   (and (>= (point) rbeg)
                        (<= (point) rend)))
               (push (match-string-no-properties 0) patch))
              ((equal op (match-string-no-properties 1))
               (push (concat " " (match-string-no-properties 2)) patch)))
        (forward-line)))
    (with-temp-buffer
      (insert (mapconcat 'identity (reverse patch) ""))
      (diff-fixup-modifs (point-min) (point-max))
      (setq patch (buffer-string)))
    patch))

(provide 'magit-diff)
;;; magit-diff.el ends here

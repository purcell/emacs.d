;;; sunrise-x-tabs.el --- tabs for the Sunrise Commander File Manager -*- lexical-binding: t -*-

;; Copyright (C) 2009-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 24 Oct 2009
;; Version: 1
;; RCS Version: $Rev: 445 $
;; Keywords: sunrise commander, tabs
;; URL: http://www.emacswiki.org/emacs/sunrise-x-tabs.el
;; Compatibility: GNU Emacs 22+

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more de-
;; tails.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension brings tab‐based navigation to the Sunrise Commander. It adds
;; to the list of optional mechanisms already available in Sunrise for moving
;; around the file system (like regular bookmarks, checkpoints, history rings,
;; materialized virtual buffers, navigable paths and file‐following) another way
;; to maintain a list of selected locations one wants to return later on, or to
;; compose "breadcrumb trails" for complex repetitive operations.

;; The main difference between tabs and other mechanisms is that once a buffer
;; has been assigned to a tab, it will not be killed automatically by Sunrise,
;; so it's possible to keep it around as long as necessary with all its marks
;; and state untouched. Tabs can be persisted across sessions using the DeskTop
;; feature.

;; Creating, using and destroying tabs are fast and easy operations, either with
;; mouse or keyboard:

;; * Press C-j (or select Sunrise > Tabs > Add Tab in the menu) to create a new
;; tab or to rename an already existing tab.

;; * Press C-k (or right-click the tab) to kill an existing tab. Combine with M-
;; (M-C-k) to kill the tab on the passive pane. Prefix with a digit to kill tabs
;; by relative order (e.g. 2 C-k kills the second tab in the current pane, while
;; 4 M-C-k kills the fourth tab in the passive pane).

;; * Press C‐n and C‐p to move from tab to tab ("Next", "Previous"), or simply
;; left‐click on the tab to focus its assigned buffer. These two keybindings can
;; be prefixed with an integer to move faster.

;; * The last four bindings can be combined with Meta (i.e. M‐C‐j, M‐C‐k, M‐C‐n
;; and M‐C‐p) to perform the equivalent operation on the passive pane or (when
;; in synchronized navigation mode) on both panes simultaneously.

;; * Press * C-k to kill in one go all the tabs in the current pane. Similarly,
;; press * M-C-k to wipe all the tabs off the passive pane or (when synchronized
;; mode is active) on both panes simultaneously.

;; * Killing the current buffer with C‐x k automatically switches to the one
;; assigned to the first available tab (if any).

;; The extension is provided as a minor mode, so you can enable / disable it
;; totally by using the command `sr-tabs-mode'.

;; It does *not* pretend to be a generic solution for tabs in Emacs. If you need
;; one, have a look at TabBar mode (http://www.emacswiki.org/emacs/TabBarMode)
;; by David Ponce. I wrote this just because it turned out to be easier to write
;; this than to customize tabbar to behave exactly like I wanted inside the
;; Sunrise panes. It's meant to be simple and to work nicely with Sunrise with
;; just a few tabs (up to 10‐15 per pane, maybe).

;; It was written on GNU Emacs 23 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 23) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise‐x‐tabs) expression to your .emacs file somewhere
;; after the (require 'sunrise‐commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;; 4) Enjoy ;-)

;;; Code:

(require 'sunrise-commander)
(eval-when-compile (require 'cl)
                   (require 'desktop))

(defcustom sr-tabs-follow-panes t
  "Whether tabs should be swapped too when transposing the Sunrise panes."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-tabs-max-tabsize 10
  "Maximum width of a Sunrise Commander tab."
  :group 'sunrise
  :type 'integer)

(defface sr-tabs-active-face
  '((((type tty) (class color) (min-colors 88))
     :background "white")
    (((type tty) (class color) (min-colors 8))
     :background "green" :foreground "yellow" :bold t)
    (((type tty) (class mono)) :inverse-video t)
    (t
     :inherit variable-pitch :bold t :background "white" :height 0.9))
  "Face of the currently selected tab in any of the Sunrise panes."
  :group 'sunrise)

(defface sr-tabs-inactive-face
  '((((type tty) (class color) (min-colors 88))
     :background "color-84" :foreground "white")
    (((type tty) (class color) (min-colors 8))
     :background "white" :foreground "cyan")
    (t
     :inherit variable-pitch :background "gray95" :height 0.9))
  "Face of all non-selected tabs in both Sunrise panes."
  :group 'sunrise)

(defface sr-tabs-separator-face
  '((t (:height 0.3)))
  "Face of the string used to separate the Sunrise tabs from one another."
  :group 'sunrise)

(defconst sr-tabs-sep #(" " 0 1 (face sr-tabs-separator-face))
  "Sunrise Tabs separator character.")

(defconst sr-tabs-ligature #(" ║" 0 1 (face sr-tabs-separator-face))
  "Sunrise Tabs line separator string.")

(defconst sr-tabs-max-cache-length 30
  "Max number of tab labels cached for reuse.")

(defvar sr-tabs '((left) (right)))
(defvar sr-tabs-labels-cache '((left) (right)))
(defvar sr-tabs-line-cache '((left) (right)))
(defvar sr-tabs-mode nil)
(defvar sr-tabs-on nil)

;;; ============================================================================
;;; Core functions:

(defun sr-tabs-add ()
  "Assign the current buffer to exactly one tab in the active pane.
If a tab for the current buffer already exists, invoke `sr-tabs-rename'."
  (interactive)
  (let ((tab-name (buffer-name))
        (tab-set (assq sr-selected-window sr-tabs)))
      (if (member tab-name (cdr tab-set))
          (call-interactively 'sr-tabs-rename)
        (setcdr tab-set (cons tab-name (cdr tab-set)))))
  (sr-tabs-refresh))

(defun sr-tabs-remove (&optional tab-buffer side)
  "Remove the tab to which TAB-BUFFER is assigned in the active pane.
If TAB-BUFFER is nil, removes the tab to which the current buffer
is assigned, if any."
  (interactive "P")
  (let* ((side (or side sr-selected-window))
         (tab-name (if (integerp tab-buffer)
                       (nth tab-buffer (assoc side sr-tabs))
                     (buffer-name tab-buffer)))
         (tab-buffer (and tab-name (get-buffer tab-name)))
         (tab-set (assq side sr-tabs)))
    (setcdr tab-set (delete tab-name (cdr tab-set)))
    (unless (or (null tab-buffer)
                (eq tab-buffer (current-buffer))
                (eq tab-buffer (sr-other 'buffer)))
      (kill-buffer (get-buffer tab-name))))
  (sr-tabs-refresh))

(defun sr-tabs-clean ()
  "Remove all tabs from the current pane."
  (interactive)
  (while (nth 1 (assoc sr-selected-window sr-tabs))
    (sr-tabs-remove 1)))

(defun sr-tabs-kill (&optional name side)
  "Remove the tab named NAME from the active pane and kill its buffer.
The buffer is not killed when currently visible or assigned to
another tab."
  (interactive)
  (let ((to-kill (or (and name (get-buffer name)) (current-buffer)))
        (side (or side sr-selected-window)))
    (sr-tabs-remove to-kill side)
    (if (and (not (memq to-kill (list sr-left-buffer sr-right-buffer)))
             (not (member to-kill (apply 'append (mapcar 'cdr sr-tabs)))))
        (kill-buffer to-kill))
    (sr-tabs-refresh)))

(defun sr-tabs-next (&optional n)
  "Move focus to the next tab (left to right) in the active pane.
With a prefix argument N, moves focus to the tab N places ahead,
or to the last one if there are fewer tabs than requested."
  (interactive "p")
  (sr-tabs-step n))

(defun sr-tabs-prev (&optional n)
  "Move focus to the previous tab (right to left) in the active pane.
With a prefix argument N, moves focus to the tab N places behind,
or to the first one if there are fewer tabs than requested."
  (interactive "p")
  (sr-tabs-step n t))

(defun sr-tabs-step (count &optional back)
  "Move focus from the current tab to the one COUNT places ahead or behind.
The direction depends on the value of BACK."
  (let* ((stack (cdr (assq sr-selected-window sr-tabs)))
         (stack (if back (reverse stack) stack))
         (target (member (buffer-name) stack)))
    (unless (null stack)
      (if (or (null count) (zerop count))
          (setq count 1))
      (if (< 1 (length target))
          (sr-tabs-switch-to-buffer (or (nth count target) (car (last target))))
        (sr-tabs-switch-to-buffer (car stack))))))

(defun sr-tabs-switch-to-buffer (to-buffer)
  "Change context of the active Sunrise pane when switching buffers."
  (let ((from-buffer (current-buffer))
        (sr-current-path-faces
         (with-current-buffer to-buffer sr-current-path-faces)))
    (unless (eq from-buffer to-buffer)
      (sr-save-aspect (switch-to-buffer to-buffer))
      (setq sr-this-directory default-directory)
      (set (sr-symbol sr-selected-window 'buffer) (current-buffer))
      (set (sr-symbol sr-selected-window 'directory) default-directory)
      (unless (eq from-buffer (sr-other 'buffer))
        (with-current-buffer from-buffer
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))))
      (condition-case nil
          (revert-buffer t t)
        (error (ignore)))
      (sr-history-push default-directory))
    (sr-tabs-refresh)))

(defun sr-tabs-focus (name side)
  "Give focus to the tab with name NAME in SIDE pane."
  (unless (eq side sr-selected-window)
    (sr-change-window))
  (sr-tabs-switch-to-buffer name))

(defun sr-tabs-kill-and-go ()
  "Kill the current Sunrise buffer and move to the next one.
This kills the buffer, removes its assigned tab (if any) and
moves to the next buffer tabbed in the active pane, unless there
are no more tabbed buffers to fall back to, in which case just
removes the tab."
  (interactive)
  (let ((to-kill (current-buffer))
        (stack (cdr (assq sr-selected-window sr-tabs))))
    (if (null stack)
        (sr-kill-pane-buffer)
      (sr-tabs-kill)
      (setq stack (cdr stack))
      (sr-tabs-next)
      (unless (or (null stack)
                  (eq to-kill (current-buffer))
                  (eq to-kill (sr-other 'buffer)))
        (kill-buffer to-kill)))))

(defun sr-tabs-rename (&optional new-name)
  (interactive "sRename current tab to: ")
  (let* ((key (buffer-name))
         (cache (assq sr-selected-window sr-tabs-labels-cache))
         (label (cadr cache)))
    (if label
        (sr-tabs-redefine-label key new-name))))

(defun sr-tabs-transpose ()
  "Swap the sets of tabs from one pane to the other."
  (interactive)
  (cl-labels ((flip (side) (setcar side (cdr (assq (car side) sr-side-lookup)))))
    (dolist (registry (list sr-tabs sr-tabs-labels-cache))
      (mapc #'flip registry)))
  (sr-in-other (sr-tabs-refresh))
  (sr-tabs-refresh))

(defadvice sr-transpose-panes (after sr-tabs-advice-sr-transpose-panes ())
  "Synchronize the tabs with the panes if so required (see the variable
`sr-tabs-follow-panes'). Activated in the function `sr-tabs-engage'."
  (if sr-tabs-follow-panes (sr-tabs-transpose)))

;;; ============================================================================
;;; Graphical interface:

(defun sr-tabs-focus-cmd (name side)
  "Return a function to give focus to the named NAME in the SIDE pane."
  (let ((selector (if (eq side (caar sr-tabs)) #'caar #'caadr)))
    `(lambda ()
       (interactive)
       (sr-tabs-focus ,name (funcall ',selector sr-tabs)))))

(defun sr-tabs-rename-cmd (name)
  "Return a function to rename the tab named NAME in both panes."
  `(lambda (&optional new-name)
     (interactive "sRename tab to: ")
     (sr-tabs-redefine-label ,name new-name)))

(defun sr-tabs-kill-cmd (name side)
  "Return a function to delete the tab named NAME in the SIDE pane."
  (let ((selector (if (eq side (caar sr-tabs)) #'caar #'caadr)))
    `(lambda ()
       (interactive)
       (if (eq sr-selected-window (funcall ',selector sr-tabs))
           (sr-tabs-kill ,name)
         (sr-in-other
          (sr-tabs-kill ,name))))))

(defsubst sr-tabs-propertize-tag (string face keymap)
  "Propertize STRING with FACE and KEYMAP so it can be used as a tab tag."
  (propertize string
              'face face
              'help-echo "mouse-1: select tab\n\mouse-2: rename tab\n\mouse-3: kill tab"
              'local-map keymap))

(defun sr-tabs-make-tag (name as-active &optional tag)
  "Return a propertized string for decorating a tab named NAME.
AS-ACTIVE determines whether to propertize it as an active or a
passive tab (nil = passive, t = active). The optional argument
TAG allows to provide a pretty name to label the tab."
  (let ((tag (or tag name))
        (side sr-selected-window)
        (keymap (make-sparse-keymap)))
    (if (< sr-tabs-max-tabsize (length tag))
        (setq tag (concat (substring tag 0 sr-tabs-max-tabsize) "…")))
    (setq tag (concat sr-tabs-sep tag sr-tabs-sep))
    (define-key keymap [header-line mouse-1] (sr-tabs-focus-cmd name side))
    (define-key keymap [header-line mouse-2] (sr-tabs-rename-cmd name))
    (define-key keymap [header-line mouse-3] (sr-tabs-kill-cmd name side))
    (if as-active
        (sr-tabs-propertize-tag tag 'sr-tabs-active-face keymap)
      (sr-tabs-propertize-tag tag 'sr-tabs-inactive-face keymap))))

(defun sr-tabs-make-label (name &optional alias)
  "Return a new label for decorating a tab named NAME.
A label is a dotted pair of tags, for active and passive state.
The new label is put in cache for later reuse. The optional
argument ALIAS allows to provide a pretty name to label the tab."
  (let* ((alias (or alias name))
         (label (cons (sr-tabs-make-tag name t alias)
                      (sr-tabs-make-tag name nil alias)))
         (entry (list (cons name label)))
         (cache (assq sr-selected-window sr-tabs-labels-cache)))
    (setcdr cache (append (cdr cache) entry))
    label))

(defun sr-tabs-trim-label (label)
  "Remove all properties and trailing whitespace from the given string."
  (replace-regexp-in-string "^\\s-+\\|\\s-+$"
                            ""
                            (substring-no-properties label)))

(defun sr-tabs-redefine-label (name alias)
  "Change the name displayed on the tab with assigned buffer NAME to ALIAS.
By default, a tab is named after its assigned buffer. This function allows to
give tabs names that are more readable or simply easier to remember."
  (let* ((alias (sr-tabs-trim-label (or alias ""))) (cache))
    (when (string= "" alias)
        (setq alias (buffer-name)))
    (setq cache (assq sr-selected-window sr-tabs-labels-cache))
    (setcdr cache (delq nil
                        (mapcar (lambda(x)
                                  (and (not (equal (car x) name)) x))
                                (cdr cache))))
    (sr-tabs-make-label name alias)
    (sr-tabs-refresh)))

(defun sr-tabs-get-tag (name is-active)
  "Retrieve the cached tag for the tab named NAME in state IS-ACTIVE.
nil = inactive, t = active. Creates new labels when needed."
  (let* ((cache (assq sr-selected-window sr-tabs-labels-cache))
         (label (cdr (assoc name (cdr cache)))))
    (if (null label)
        (setq label (sr-tabs-make-label name)))
    (if (< sr-tabs-max-cache-length (length (cdr cache)))
        (setcdr cache (cddr cache)))
    (if is-active (car label) (cdr label))))

(defun sr-tabs-make-line ()
  "Assemble a new tab line from cached tags and put it in the line cache."
  (if (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
      (let ((tab-set (cdr (assq sr-selected-window sr-tabs)))
            (tab-line (if (or (cdar sr-tabs)
                              (cdr (cadr sr-tabs))) "" nil))
            (current-name (buffer-name)))
        (mapc (lambda (x)
                (let ((is-current (equal current-name x)))
                  (setq tab-line (concat tab-line sr-tabs-sep
                                         (sr-tabs-get-tag x is-current)))))
              tab-set)
        (setcdr (assq sr-selected-window sr-tabs-line-cache) tab-line)
        tab-line)
    nil))

(defsubst sr-tabs-empty-p (line)
  (or (null line) (string= "" line)))

(defsubst sr-tabs-empty-mask (line)
  (or (and (null line) "") line))

(defsubst sr-tabs-empty-null (line)
  (if (sr-tabs-empty-p line) nil line))

(defun sr-nonempty-p (line-list)
  "Return non-nil if LINE-LIST contains at least one non-nil element."
  (or (not (sr-tabs-empty-p (car line-list)))
      (and (cdr line-list) (sr-nonempty-p (cdr line-list)))))

(defun sr-tabs-xor (list1 list2)
  "Replacement for function `set-exclusive-or'.
Used to avoid dependency on cl-seq.el."
  (cond ((null list1) list2)
        ((null list2) list1)
        ((equal list1 list2) nil)
        (t
         (let (result)
           (mapc (lambda (element)
                   (if (member element result)
                       (setq result (delete element result))
                     (setq result (cons element result))))
                 (append list1 list2))
           result))))

(defun sr-tabs-refresh ()
  "Update `header-line-format' in both panes.
Uses the line cache for the passive one, and assembles a new tab
line for the active one. In the (corner) case when both panes
contain the same buffer, glues together the tab lines with a
``double bar'' separator."
  (setq sr-tabs-mode sr-tabs-on)
  (sr-tabs-make-line)
  (let ((line-list (mapcar 'cdr sr-tabs-line-cache))
        (same-buffer (eq sr-left-buffer sr-right-buffer)))
    (if same-buffer
        (setq header-line-format
              (and (sr-nonempty-p line-list)
                   (mapconcat 'concat line-list sr-tabs-ligature)))
      (let ((other-buffer (sr-other 'buffer)))
        (if (eq 'right sr-selected-window)
            (setq line-list (nreverse line-list)))
        (if (apply 'sr-tabs-xor (mapcar 'sr-tabs-empty-p line-list))
            (setq line-list (mapcar 'sr-tabs-empty-mask line-list))
          (setq line-list (mapcar 'sr-tabs-empty-null line-list)))

        (setq header-line-format (car line-list))

        (when (buffer-live-p other-buffer)
          (with-current-buffer other-buffer
            (setq header-line-format (cadr line-list)))))))
  (force-window-update))

;;; ============================================================================
;;; Private interface:

(defun sr-tabs-bury-all ()
  "Bury all currently tabbed buffers."
  (let ((all-buffers (apply 'append (mapcar 'cdr sr-tabs))))
    (if all-buffers
        (mapc 'bury-buffer all-buffers))))

(defun sr-tabs-protect-buffer ()
  "Protect the current buffer from being automatically disposed
by Sunrise when moving to another directory (called from
`kill-buffer-query-functions' hook.)"
  (let ((tab-name (buffer-name)))
    (not (or (member tab-name (car sr-tabs))
             (member tab-name (cadr sr-tabs))))))

(defun sr-tabs-engage ()
  "Enable the Sunrise Tabs extension."
  (setq sr-tabs-on t)
  (add-hook 'sr-refresh-hook 'sr-tabs-refresh)
  (add-hook 'sr-quit-hook 'sr-tabs-bury-all)
  (add-hook 'kill-buffer-query-functions 'sr-tabs-protect-buffer)
  (ad-activate 'sr-transpose-panes)
  (ad-activate 'sr-editable-pane)
  (sr-tabs-refresh))

(defun sr-tabs-disengage ()
  "Disable the Sunrise Tabs extension."
  (setq sr-tabs-on nil)
  (remove-hook 'sr-refresh-hook 'sr-tabs-refresh)
  (remove-hook 'sr-quit-hook 'sr-tabs-bury-all)
  (remove-hook 'kill-buffer-query-functions 'sr-tabs-protect-buffer)
  (ad-deactivate 'sr-transpose-panes)
  (ad-deactivate 'sr-editable-pane)
  (setq header-line-format (default-value 'header-line-format))
  (sr-in-other (setq header-line-format (default-value 'header-line-format))))

;;; ============================================================================
;;; User interface:

(defvar sr-tabs-mode-map (make-sparse-keymap))
(define-key sr-tabs-mode-map [(control ?j)] 'sr-tabs-add)
(define-key sr-tabs-mode-map [(control ?k)] 'sr-tabs-remove)
(define-key sr-tabs-mode-map "*\C-k" 'sr-tabs-clean)
(define-key sr-tabs-mode-map [(control ?p)] 'sr-tabs-prev)
(define-key sr-tabs-mode-map [(control ?n)] 'sr-tabs-next)
(define-key sr-tabs-mode-map [(meta tab)] 'sr-tabs-next)

(define-key sr-tabs-mode-map [(control meta ?j)]
  (lambda () (interactive) (sr-in-other (sr-tabs-add))))
(define-key sr-tabs-mode-map [(control meta ?k)]
  (lambda () (interactive) (sr-in-other (call-interactively 'sr-tabs-remove))))
(define-key sr-tabs-mode-map [(control meta ?p)]
  (lambda () (interactive) (sr-in-other (sr-tabs-prev))))
(define-key sr-tabs-mode-map [(control meta ?n)]
  (lambda () (interactive) (sr-in-other (sr-tabs-next))))
(define-key sr-tabs-mode-map [(control meta tab)]
  (lambda () (interactive) (sr-in-other (sr-tabs-next))))
(define-key sr-tabs-mode-map "*\C-\M-k"
  (lambda () (interactive) (sr-in-other (sr-tabs-clean))))

(define-key sr-tabs-mode-map "\C-xk" 'sr-tabs-kill-and-go)
(define-key sr-tabs-mode-map "\M-T"  'sr-tabs-transpose)

(define-minor-mode sr-tabs-mode
  "Tabs support for the Sunrise Commander file manager.
This minor mode provides the following keybindings:

        C-j ........... Create new tab (or rename existing tab) in active pane.
        C-k ........... Kill the tab of the current buffer in the active pane.
        C-n ........... Move to the next tab in the active pane.
        C-p ........... Move to the previous tab in the active pane.

        C-M-j ......... Assign the current buffer to a tab in the passive pane.
        C-M-k ......... Kill the tab of the current buffer in the passive pane.
        C-M-n ......... Move to the next tab in the passive pane.
        C-M-p ......... Move to the previous tab in the passive pane.

        C-x k ......... Kill buffer and move to the next tabbed one (if any).
"
  nil nil sr-tabs-mode-map
  (unless (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
    (setq sr-tabs-mode nil)
    (error "Sorry, this mode can be used only within the Sunrise Commander."))
  (if sr-tabs-mode
      (sr-tabs-engage)
    (sr-tabs-disengage)))

(defvar sr-tabs-editable-dired-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map sr-tabs-mode-map)
    (define-key map "\C-n"  'dired-next-line)
    (define-key map "\C-p"  'dired-previous-line)
    (define-key map "\C-tn" 'sr-tabs-next)
    (define-key map "\C-tp" 'sr-tabs-prev)
    map)
  "Keymap for managing tabs inside Editable Dired mode panes.")

(defadvice sr-editable-pane (after sr-tabs-advice-sr-editable-pane ())
  "Install `sr-tabs-editable-dired-map' when in Editable Dired mode."
  (add-to-list 'minor-mode-overriding-map-alist
               `(sr-tabs-mode . ,sr-tabs-editable-dired-map)))

;;; ============================================================================
;;; Bootstrap:

(defun sr-tabs-menu-init ()
  "Initialize the Sunrise Tabs extension menu."
  (unless (lookup-key sr-mode-map [menu-bar Sunrise])
    (define-key sr-mode-map [menu-bar Sunrise]
      (cons "Sunrise" (make-sparse-keymap))))
  (let ((menu-map (make-sparse-keymap "Tabs")))
    (define-key sr-mode-map [menu-bar Sunrise tabs] (cons "Tabs" menu-map))
    (define-key menu-map [help] '("Help" . (lambda ()
                                             (interactive)
                                             (describe-function 'sr-tabs-mode))))
    (define-key menu-map [transpose] '("Transpose" . sr-tabs-transpose))
    (define-key menu-map [kill]      '("Kill and go to next" . sr-tabs-kill-and-go))
    (define-key menu-map [next]      '("Next"         . sr-tabs-next))
    (define-key menu-map [prev]      '("Previous"     . sr-tabs-prev))
    (define-key menu-map [remove]    '("Remove"       . sr-tabs-remove))
    (define-key menu-map [add]       '("Add/Rename"   . sr-tabs-add))))
(defun sr-tabs-start-once ()
  "Bootstrap the tabs mode on the first execution of the Sunrise Commander,
after module installation."
  (sr-tabs-mode t)
  (sr-tabs-menu-init)
  (remove-hook 'sr-start-hook 'sr-tabs-start-once)
  (unintern 'sr-tabs-menu-init obarray)
  (unintern 'sr-tabs-start-once obarray))
(add-hook 'sr-start-hook 'sr-tabs-start-once)

;;; ============================================================================
;;; Desktop support:

(defun sr-tabs-desktop-save-buffer (_desktop-dir)
  "Return additional desktop data for saving tabs of the current Sunrise buffer."
  (let* ((left-tab (car (member (buffer-name) (assoc 'left sr-tabs))))
         (left-cache (cdr (assq 'left sr-tabs-labels-cache)))
         (left-label (cadr (assoc left-tab left-cache)))
         (right-tab (car (member (buffer-name) (assoc 'right sr-tabs))))
         (right-cache (cdr (assq 'right sr-tabs-labels-cache)))
         (right-label (cadr (assoc right-tab right-cache))))
    (delq
     nil
     (list
      (if left-label (cons 'left-tab (sr-tabs-trim-label left-label)))
      (if right-label (cons 'right-tab (sr-tabs-trim-label right-label)))))))

(defun sr-tabs-desktop-restore-buffer (_desktop-buffer-file-name
                                       _desktop-buffer-name
                                       desktop-buffer-misc)
  "Restore all tabs in a Sunrise (normal or VIRTUAL) buffer from a desktop file."
  (mapc (lambda (side)
          (let* ((sr-selected-window side)
                 (tab-symbol (intern (concat (symbol-name side) "-tab")))
                 (name (buffer-name))
                 (label (cdr (assq tab-symbol desktop-buffer-misc)))
                 (tab-set (assq side sr-tabs)))
            (when label
              (setcdr tab-set (cons name (cdr tab-set)))
              (sr-tabs-make-label name label))))
        '(left right))
  (unless sr-tabs-on
    (sr-tabs-engage)))

(defun sr-tabs-reset-state ()
  "Reset some environment variables that control the behavior of
tabs in the Sunrise Commander (used for desktop support)."
  (mapc (lambda (x) (setcdr x nil)) sr-tabs-labels-cache)
  (mapc (lambda (x) (setcdr x nil)) sr-tabs)
  nil)

;; These append the previous functions to the generic desktop support in Sunrise:
(add-to-list 'sr-desktop-save-handlers 'sr-tabs-desktop-save-buffer)
(add-to-list 'sr-desktop-restore-handlers 'sr-tabs-desktop-restore-buffer)

;; This activates the tabs support after desktop restoration:
(add-hook
 'desktop-after-read-hook
 (defun sr-tabs-desktop-after-read-function ()
   (unless (assq 'sr-tabs-on desktop-globals-to-clear)
     (add-to-list 'desktop-globals-to-clear
                  '(sr-tabs-on . (sr-tabs-reset-state))))))

(defun sunrise-x-tabs-unload-function ()
  (sr-ad-disable "^sr-tabs-"))

(provide 'sunrise-x-tabs)

;;;###autoload (eval-after-load 'sunrise-commander '(sr-extend-with 'sunrise-x-tabs))

;;; sunrise-x-tabs.el ends here

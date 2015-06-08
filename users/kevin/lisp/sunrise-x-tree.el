;;; sunrise-x-tree.el --- Tree View for the Sunrise Commander File Manager -*- lexical-binding: t -*-

;; Copyright (C) 2010-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 4 May 2010
;; Version: 1
;; RCS Version: $Rev: 450 $
;; Keywords: sunrise commander, directories tree navigation
;; URL: http://www.emacswiki.org/emacs/sunrise-x-tree.el
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

;; This extension adds to the Sunrise Commander file manager a directories-only
;; tree view that can be used for fast navigation, as well as for several basic
;; operations on files and directories. It uses the excellent "tree-widget.el"
;; library written by David Ponce and works the same in text consoles as well as
;; in graphical environments, using either the mouse or just the keyboard.

;; For more information on the Sunrise Commander, other extensions and cool tips
;; & tricks visit http://www.emacswiki.org/emacs/Sunrise_Commander

;; This extension was developed on GNU Emacs 24 on Linux and tested on GNU Emacs
;; 22 and 24 for Linux, and on EmacsW32 (version 23) for Windows.

;;; Installation:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise‐x‐tree) expression to your .emacs file somewhere
;; after the (require 'sunrise‐commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;; 4) You may have to customize the `tree-widget-image-enable' variable if all
;; you get are text-only icons (e.g. "[+]" and "[X]") in your graphical
;; environment, while you'd rather prefer looking at pretty graphical ones.

;; WARNING: If you use Slime be aware that some versions of this package include
;; an older version of tree-widget.el that may clobber the one in Emacs and make
;; this extension work improperly. At least that's the case in Debian for i386:
;; slime comes with version 21.4 of tree-widget, but the extension requires 22.1
;; or better.

;;; Usage:

;; In order to explain the different ways this extension is used, it's necessary
;; to first introduce a few concepts:
;; * A Sunrise Tree View pane displays a list of directories arranged in a tree-
;;   like structure. There is exactly one such TREE in every Tree View pane.
;; * Each node in this tree is called FOLDER and represents one directory in the
;;   file system. A folder can be in one of two states: OPEN or CLOSED. When the
;;   folder is open, its children (if any) are displayed under it in the tree.
;; * The top-most folder in every tree is called the ROOT of the tree. All other
;;   folders in the same tree represent sub-directories of the root directory.
;; * To FOCUS a given folder means to replace the current tree with one that has
;;   that folder as its root.
;; * The opposite operation of focusing a folder, i.e. showing it in the context
;;   of a broader tree, is called BLURRING the folder.
;; * Finally, to EXPLODE a given folder means to open it, then all its children,
;;   then all the children of its children and so on, as many times as the value
;;   of  the   `sr-tree-explosion-ratio'  option   (which  can   be  customized)
;;   specifies. This  is an additive  operation, which means that  exploding the
;;   same directory  many times  will open  more of  its descendants  deeper and
;;   deeper until the tree runs out of closed folders in that branch.

;; The Sunrise Tree View mode offers three different ways of navigating the file
;; system: with the mouse (for rodent lovers), with the arrow keys (for rookies)
;; and with other keys nearer the home row (for keyboard junkies).

;; 1. With the mouse:

;; * Meta + Shift + left click anywhere inside a pane to switch between tree and
;;   normal modes.
;; * Left click on a folder or anywhere beside it to open or close the folder.
;; * Middle click on a folder, or anywhere beside it, to just select it without
;;   changing its state.
;; * Shift + left click on a folder or anywhere beside it to focus it.
;; * Meta + left click on a folder or anywhere beside it to blur it. Meta + left
;;   click anywhere else in the pane to blur the currently selected folder.
;; * Control + left click on a folder or anywhere beside it to explode it.
;; * Double click on a folder to dismiss tree view and visit the folder.
;; * Left or Middle click anywhere on the path line at the top of the pane to go
;;   directly to the directory which path ends at that point in the line.

;; 2. With the arrow keys:

;; * Meta + Shift + down switches between tree and normal modes.
;; * Up and down move the cursor up and down (duh!)
;; * Right opens a folder if it was closed, or browses it if it was open.
;; * Left closes a folder if it was open, or jumps up to its parent folder if it
;;   was closed.
;; * Shift + right focuses the selected folder.
;; * Shift + left blurs the selected folder.
;; * Control + right explodes the selected folder.
;; * If you're in a text console and  the bindings above don't work for you, try
;;   using Escape instead of Shift (not combined -- first press escape, then the
;;   arrow key) and C-c instead of Control.

;; 3. With alphanumeric keys:

;; * C-t Space (alternatively C-t Return) - switch between modes.
;; * n, p - move cursor up/down.
;; * Space, Return - open closed folder / browse already open folder.
;; * Backspace - close open folder / jump to parent of already closed folder.
;; * C-c f - focus the selected folder.
;; * C-c b - blur the selected folder.
;; * C-Return, C-c Return - explode the selected folder.
;; * f - browse the selected folder in normal mode.
;; * v, o - view the selected folder in the passive pane, in whatever mode it
;;   happens to be at that moment.
;; * C-c C-c - dismiss tree view and return to normal mode.

;; * C-q is simply another binding for the usual pane synchronization (C-c C-z)
;;   already present in Sunrise Commander Core, which in tree mode performs the
;;   "Quick View" operation required by the OFM standard.

;; * C-u C-s, C-u C-r - "sticky" interactive search. This works like the regular
;; isearch, but when the current search is finished with a Return, the folder
;; the cursor ends on is automatically opened and a new (forward) Isearch
;; starts, so one can continue searching among the children of that folder. This
;; allows for extremely fast navigation across lengthy paths of directories with
;; just a few keystrokes. To terminate a sticky search, press C-g or (once
;; again) Return. Sticky searches can be made default in tree panes by
;; customizing the variable `sr-tree-isearch-always-sticky' - when set, prefix
;; the command to start a regular (non-sticky) interactive search.

;; * When AVFS support is active, press "#" to toggle the display of compressed
;; archives in Tree View panes.

;; Additionally, most of the original keybindings from Sunrise apply (wherever
;; it makes sense, of course). For instance switching/transposing/laying out
;; panes (Tab, M-Tab, C-c, C-s), showing / hiding hidden directories (C-o),
;; jumping to parent/arbitrary directory (J, j) and many more, including the
;; following file manipulation commands: copy (C), clone (K), rename (R), delete
;; (D), symlink (S), relative symlink (Y), create a new directory (+) and show
;; file size (y).

;; All directory commands from the Sunrise Buttons extension are also supported.
;; It is required to upgrade the Buttons extension to version 1R293 or better to
;; make this integration work correctly, though.

;; Hey, and don't forget to enjoy ;-)

;;; Code:

(require 'sunrise-commander)
(require 'tree-widget)
(require 'hl-line)
(eval-when-compile (require 'cl)
                   (require 'desktop))

(eval-and-compile
  (unless (fboundp 'cl-letf)
    (defalias 'cl-letf 'letf)))

(defcustom sr-tree-explosion-ratio 3
  "Maximum number of directory levels to recursively open at a time.
Used by the command `sr-tree-explode-branch'."
  :group 'sunrise
  :type 'integer)

(defcustom sr-tree-isearch-always-sticky nil
  "Whether interactive searches are always sticky in tree panes."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-tree-avfs-handlers-alist '(("\\.od[fgpst]$" . "#uzip/")
                                         ("\\.oxt$"       . "#uzip/")
                                         ("\\.sx[dmicw]$" . "#uzip/"))
  "List of AVFS handlers to manage specific file extensions in Tree View mode."
  :group 'sunrise
  :type 'alist)

(defvar sr-tree-root nil
  "Root widget of the current tree view.")

(defvar sr-tree-open-paths nil
  "List of paths to all the directories open in the current tree view.")

(defvar sr-tree-avfs-seen nil
  "List of paths to big compressed archives visited through AVFS.")

(defvar sr-tree-cursor nil
  "Cons cell of the from (LABEL . FILEPATH).
FILEPATH is the path to the selected directory in the current tree
view. LABEL is the name displayed in the tree representing FILEPATH")

(defvar sr-tree-mode-map (make-sparse-keymap)
  "Keymap for the Sunrise Commander Tree View.")

(defvar sr-buttons-command-adapter nil
  "Compiler pacifier.
See `sr-buttons-command-adapter' in sunrise-x-buttons.el.")

(defvar sr-tree-omit-archives t "")

(define-widget 'sr-tree-dir-widget 'tree-widget
  "Directory Tree widget."
  :dynargs  'sr-tree-expand-dir
  :has-children   t)

;;; ============================================================================
;;; GUI Management functions:

(defun sr-tree-focus-widget ()
  "Move point to the first button widget in the current line (if any)."
  (interactive)
  (beginning-of-line)
  (unless (get-char-property (point) 'button)
    (while (not (or (eolp) (get-char-property (point) 'button)))
      (forward-char))))

(defun sr-tree-get-button ()
  "Return the first button widget in the current line (if any)."
  (sr-tree-focus-widget)
  (get-char-property (point) 'button))

(defun sr-tree-get-branch ()
  "Return the first tree widget in the current line (if any)."
  (widget-get (sr-tree-get-button) :parent))

(defun sr-tree-get-cursor ()
  "Return a cursor as documented in `sr-tree-cursor'."
  (let* ((cursor-node (sr-tree-get-button))
         (cursor-tree (if cursor-node (widget-get cursor-node :parent)))
         (cursor-node (widget-get cursor-node :node))
         (cursor-tag (widget-get cursor-node :tag))
         (cursor-path (sr-tree-path-line (widget-get cursor-tree :path))))
    (and cursor-tag cursor-path (cons cursor-tag cursor-path))))

(defun sr-tree-update-cursor ()
  "Update the cursor (cf. `sr-tree-cursor').
Also updates other graphical elements of the interface, depending
on the position of the point."
  (setq sr-tree-cursor (sr-tree-get-cursor))
  (when sr-tree-cursor
    (setq sr-this-directory (cdr sr-tree-cursor))
    (sr-tree-highlight)
    (setq default-directory (file-name-as-directory (cdr sr-tree-cursor)))
    (when (and (featurep 'sunrise-x-modeline)
               (not (eq mode-line-format (default-value 'mode-line-format))))
      (if (fboundp 'sr-modeline-refresh)
          (sr-modeline-refresh))
      (force-mode-line-update))
    (if (and sr-synchronized
             (not (eq sr-left-buffer sr-right-buffer))
             (eq (selected-window) (sr-this 'window)))
        (sr-tree-advertised-find-file-other))))

(defun sr-tree-refresh-dir (widget &rest _ignore)
  "Refresh WIDGET parent (or own) tree children. IGNORE other arguments."
  (let ((tree (if (tree-widget-p widget)
                  widget
                (widget-get widget :parent))))
    (widget-put tree :args nil) ;; Clear the tree children cache.
    (widget-value-set tree (widget-value tree))) ;; Redraw the tree node.
  (if (fboundp 'sr-tabs-refresh)
      (sr-tabs-refresh))
  (sr-highlight))

(defun sr-tree-refresh-branch (&optional prefix)
  "Revert the currently selected branch in the directory tree.
If no branch is selected, then select the root node and revert
the whole tree. If PREFIX is non-nil, close all open
subdirectories in the tree first."
  (interactive "P")
  (if prefix
      (setq sr-tree-open-paths nil))
  (let ((button (sr-tree-get-button)))
    (unless button
      (sr-tree-beginning-of-buffer)
      (setq button (sr-tree-get-button)))
    (sr-tree-refresh-dir button)))

(defun sr-tree-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert the current Sunrise Tree View buffer."
  (interactive)
  (sr-tree-refresh-branch))

(defun sr-tree-widget (e &optional open)
  "Return a widget to display directory E.
With a non-nil optional argument OPEN, display the widget as open
initially."
  (let ((is-open (or open (member e sr-tree-open-paths)))
        (tag (sr-chop ?/ e)))
    (setq tag (file-name-as-directory (file-name-nondirectory tag)))
    `(sr-tree-dir-widget
      :open ,is-open
      :node (push-button
             :tag ,tag
             :format "%[%t%]\n"
             :notify sr-tree-refresh-dir)
      :path ,e)))

(defun sr-tree-path-line (&optional path)
  "Transform PATH into a suitable path line for displaying at the pane top."
  (sr-chop ?/ (expand-file-name (or path (cdr sr-tree-cursor) ""))))

(defun sr-tree-highlight ()
  "Set up the path line in the current Sunrise Tree buffer."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (delete-region (point) (line-end-position))
      (widget-insert (propertize (concat (sr-tree-path-line nil) " ")
                                 'face 'sr-passive-path-face))
      (forward-line 1)
      (delete-region (line-beginning-position) (line-end-position))
      (widget-insert
       (format "%s" (if sr-tree-omit-archives "" "virtual directories: ON ")))
      (sr-highlight))))

(defun sr-tree-check-virtual-size (entry)
  "Allow user to abort before trying to access a large archive through AVFS."
  ;; TODO: use function abort-if-file-too-large instead:
  (if (and sr-avfs-root
           (sr-overlapping-paths-p sr-avfs-root entry)
           (string-match "^\\([^#]+\\)#" entry))
      (let* ((root (match-string 1 entry))
             (root (substring root (length sr-avfs-root)))
             (size (nth 7 (file-attributes root))))
        (when (and large-file-warning-threshold
                   (not (member root sr-tree-avfs-seen))
                   (> size large-file-warning-threshold))
          (or (y-or-n-p
               (format "File %s is large (%dMB), really open? "
                       (file-name-nondirectory root) (/ size 1048576)))
              (error "Aborted"))
          (sr-tree-avfs-register-seen root)))))

(defun sr-tree-list (dir)
  "Return the list of subdirectories in DIR."
  (let ((entries (directory-files dir 'full)) dirs entry rel-entry)
    (while entries
      (setq entry (car entries)
            rel-entry (file-relative-name entry (concat entry "/.."))
            entries (cdr entries))

      (cond ((eq ?. (string-to-char (substring entry -1)))
             (ignore))

            ((and dired-omit-mode (eq ?. (string-to-char rel-entry)))
             (ignore))

            ((file-directory-p entry)
             (setq dirs (cons entry dirs)))

            ((and (not sr-tree-omit-archives) (sr-avfs-directory-p entry))
             (setq dirs (cons (sr-tree-avfs-dir entry) dirs)))

            (t (ignore))))
    (nreverse dirs)))

(defun sr-tree-avfs-dir (filename)
  "Return the virtual path for accessing FILENAME through AVFS in Tree View panes.
Returns nil if AVFS cannot manage this kind of file."
  (let* ((handler
          (or (assoc-default filename sr-tree-avfs-handlers-alist 'string-match)
              (assoc-default filename sr-avfs-handlers-alist 'string-match)))
          (vdir (concat filename handler)))
    (unless (sr-overlapping-paths-p sr-avfs-root vdir)
      (setq vdir (concat sr-avfs-root vdir)))
    (sr-tree-path-line vdir)))

(defun sr-tree-expand-dir (tree)
  "Return TREE widget children. Reuse :args cache if it exists."
  (or (widget-get tree :args)
      (let ((dir (widget-get tree :path)))
        (message "Reading directory '%s'..." dir)
        (condition-case err
            (prog1
                (mapcar 'sr-tree-widget (sr-tree-list dir))
              (message "Reading directory '%s'...done" dir))
          (error
           (widget-put tree :open nil)
           (message "%s" (error-message-string err))
           nil)))))

(defun sr-tree-register-path (widget)
  "Add path from WIDGET to the current Sunrise Tree buffer's list of open paths."
  (let ((path (sr-tree-path-line (widget-get widget :path))))
    (setq sr-tree-open-paths
          (if (widget-get widget :open)
              (cons path sr-tree-open-paths)
            (delete path sr-tree-open-paths)))))
(add-hook 'tree-widget-after-toggle-functions 'sr-tree-register-path)

(defun sr-tree-avfs-register-seen (path)
  "Add the PATH to the list of (big) archives visited through AVFS."
  (setq sr-tree-avfs-seen (cons path (delete path sr-tree-avfs-seen))))

(defun sr-tree-build (root)
  "Delete the current tree widget and build a new one at ROOT."
  (interactive "DSunrise Tree Root: ")
  (setq default-directory
        (file-name-as-directory (setq root (expand-file-name root))))
  (let ((inhibit-read-only t)
        (all (overlay-lists)))
    (erase-buffer)
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all))
    (tree-widget-set-theme "folder")
    (widget-insert (format "%s\n\n" (sr-tree-path-line root)))
    (set
     (make-local-variable 'sr-tree-root)
     (widget-create (sr-tree-widget root t)))
    (widget-setup)
    (if sr-tree-cursor
        (sr-tree-search-cursor)
      (sr-tree-beginning-of-buffer))
    (sr-tree-refresh-branch)
    (sr-tree-register-path sr-tree-root)))

(defun sr-tree-build-new (root)
  "Build a new Tree View pane in a new buffer at ROOT."
  (interactive "DSunrise Tree Root: ")
  (let ((default-directory root))
    (sr-save-aspect
     (sr-alternate-buffer
      (switch-to-buffer (generate-new-buffer "Sunrise Tree")))
     (sr-tree-mode))))

(defun sr-tree-goto-dir (root &optional keep-state)
  "`sr-goto-dir' replacement for buffers in Sunrise Tree mode.
See also the variable `sr-goto-dir-function'."
  (interactive)
  (setq root (expand-file-name root))
  (let ((cursor sr-tree-cursor)
        (open-paths sr-tree-open-paths))
    (sr-tree-build-new root)
    (when keep-state
        (setq sr-tree-cursor cursor
              sr-tree-open-paths (mapcar 'identity open-paths))))
  (sr-keep-buffer)
  (sr-history-push root))

(defadvice sr-focus-filename
  (around sr-tree-advice-sr-focus-filename (filename))
  "Force deactivation of Sunrise Tree View before focusing a regular file."
  (if (eq major-mode 'sr-tree-mode)
      (if (file-directory-p filename)
          (let* ((path (directory-file-name (expand-file-name filename)))
                 (label (file-name-as-directory (file-name-nondirectory path))))
            (with-no-warnings (sr-tree-search-cursor (cons label path))))
        (with-no-warnings (sr-tree-dismiss))
        ad-do-it)
    ad-do-it))
(ad-activate 'sr-focus-filename)

;;; ============================================================================
;;; GUI interaction functions:

(defun sr-tree-next-line ()
  "Move point to the next line in the current pane."
  (interactive)
  (forward-line)
  (sr-tree-update-cursor))

(defun sr-tree-previous-line ()
  "Move point to the previous line in the current pane."
  (interactive)
  (forward-line -1)
  (sr-tree-update-cursor))

(defun sr-tree-scroll-down (&optional arg)
  "Scroll down the current pane without moving the point (if possible)."
  (interactive)
  (scroll-down arg)
  (sr-tree-update-cursor))

(defun sr-tree-scroll-up (&optional arg)
  "Scroll up the current pane without moving the point (if possible)."
  (interactive)
  (scroll-up arg)
  (sr-tree-update-cursor))

(defun sr-tree-beginning-of-buffer ()
  "Move cursor to the top of the current Sunrise Tree View pane."
  (interactive)
  (goto-char (widget-get sr-tree-root :from))
  (sr-tree-update-cursor))

(defun sr-tree-end-of-buffer ()
  "Move cursor to the bottom of the current Sunrise Tree View pane."
  (interactive)
  (forward-sentence)
  (sr-tree-update-cursor))

(defun sr-tree-toggle-branch (&optional action)
  "Open/close (graphically) the node selected in the current Sunrise Tree pane.
Optional ACTION is one of the symbols `open' or `close' and
allows to specify whether the node has to be open only if closed,
or closed only if open."
  (interactive)
  (let* ((branch (sr-tree-get-branch))
         (is-open (widget-get branch :open))
         (path))
    (unless is-open
      (setq path (widget-get branch :path))
      (sr-tree-check-virtual-size path)
      t)
    (when (or (and is-open (eq action 'close))
              (and (not is-open) (eq action 'open))
              (null action))
      (sr-tree-focus-widget)
      (widget-button-press (point))
      t)))

(defun sr-tree-open-branch ()
  "Unfold (graphically) the directory selected in the current Sunrise Tree pane.
Displays the subdirectories directly under it."
  (interactive)
  (if (widget-get (sr-tree-get-branch) :open)
      (sr-tree-advertised-find-file)
    (sr-tree-toggle-branch 'open)))

(defun sr-tree-close-branch ()
  "Fold the selected directory.
Hides all subdirectories being displayed under it or any of its
subdirectories."
  (interactive)
  (sr-tree-toggle-branch 'close))

(defun sr-tree-collapse-branch ()
  "If the current folder is open, close it.
If it is closed, move to its parent directory, building a new
tree if necessary."
  (interactive)
  (let ((branch (sr-tree-get-branch)))
    (if (widget-get branch :open)
        (sr-tree-close-branch)
      (sr-tree-prev-subdir)
      (unless (eq (sr-tree-get-branch) sr-tree-root)
        (sr-tree-close-branch)))))

(defun sr-tree-explode-branch (&optional level branch)
  "Open the selected directory and all its open subdirectories recursively.
The number of levels is determined by the variable
`sr-tree-explosion-ratio'. LEVEL and BRANCH optional arguments
are used only internally to control recursion."
  (interactive)
  (unless (or level branch)
    (recenter (truncate (/ (window-body-height) 10.0))))
  (let ((level (or level sr-tree-explosion-ratio))
        (branch (or branch (sr-tree-get-branch))))
    (when (and branch (< 0 level))
      (unless (widget-get branch :open)
        (setq level (1- level))
        (funcall #'tree-widget-action branch))
      (dolist (child (cdr (widget-get branch :children)))
        (sr-tree-explode-branch level child)))))

(defun sr-tree-search-cursor (&optional init-cursor recursing)
  "Try to move the point to the node represented by INIT-CURSOR.
If it is nil, use the value of `sr-tree-cursor' instead. On
failure, put the point at the top of the pane."
  (let ((cursor (or init-cursor sr-tree-cursor)) new-cursor)
    (if (null cursor)
        (sr-tree-beginning-of-buffer)
      (unless recursing (goto-char (point-min)))
      (when (search-forward (car cursor) (point-max) t)
        (setq new-cursor (sr-tree-get-cursor))
        (if (and new-cursor (not (sr-equal-dirs (cdr cursor) (cdr new-cursor))))
            (progn
              (sr-tree-next-line)
              (sr-tree-search-cursor cursor t))
          (sr-tree-update-cursor))))))

(defun sr-tree-isearch-prompt ()
  "Display the message that appears when a sticky search is launched."
  (message (propertize "Sunrise Tree sticky I-search (C-g to exit): "
                       'face 'minibuffer-prompt)))

(defvar sr-tree-isearch-mode-commands
  '(([S-return]  . 'sr-tree-focus-branch)
    ([S-right]   . 'sr-tree-focus-branch)
    ([?\e right] . 'sr-tree-focus-branch)
    ("\C-cf"     . 'sr-tree-focus-branch)

    ([M-return]  . 'sr-tree-blur-branch)
    ([S-left]    . 'sr-tree-blur-branch)
    ([?\e left]  . 'sr-tree-blur-branch)
    ("\C-cb"     . 'sr-tree-blur-branch)

    ([C-return]  . 'sr-tree-explode-branch)
    ([C-right]   . 'sr-tree-explode-branch)
    ([3 right]   . 'sr-tree-explode-branch)
    ("\C-c\C-m"  . 'sr-tree-explode-branch)

    ) "Keybindings installed in `isearch-mode' during a sticky search.")

(defsubst sr-tree-isearch-command (binding)
  `(lambda () (interactive) (sr-tree-post-isearch ,(cdr binding))))

(defun sr-tree-isearch-setup ()
  "Set up Isearch to perform sticky searches in Sunrise Tree panes.
Used from `isearch-mode-hook'."
  (add-hook 'isearch-mode-end-hook 'sr-tree-post-isearch)
  (set (make-local-variable 'search-nonincremental-instead) nil)
  (define-key isearch-mode-map "\C-c" (make-sparse-keymap))
  (mapc (lambda (binding)
          (define-key
            isearch-mode-map
            (car binding) (sr-tree-isearch-command binding)))
        sr-tree-isearch-mode-commands)
  (run-hooks 'sr-refresh-hook))

(defun sr-tree-isearch-done ()
  "Clean up the Isearch hook and keymap after a sticky search."
  (remove-hook 'isearch-mode-end-hook 'sr-tree-post-isearch)
  (kill-local-variable 'search-nonincremental-instead)
  (mapc (lambda (binding)
          (define-key isearch-mode-map (car binding) nil))
        sr-tree-isearch-mode-commands)
  (define-key isearch-mode-map "\C-c" 'isearch-other-control-char)
  (isearch-done)
  (setq isearch-mode-end-hook-quit t))

(defun sr-tree-isearch-forward (&optional prefix)
  "Prefixable version of `isearch-forward' used in Sunrise Tree mode.
With PREFIX, starts a new `isearch-forward' immediately after the
previous one exits as long as C-g is not pressed."
  (interactive "P")
  (if (or (and prefix (not sr-tree-isearch-always-sticky))
          (and (not prefix) sr-tree-isearch-always-sticky))
      (sr-tree-sticky-isearch-forward)
    (isearch-forward nil t)))

(defun sr-tree-sticky-isearch-forward ()
  "Chain Isearch operations to allow fast navigation through long file paths.
Press C-g to abort, or Return twice on a folder to dismiss Tree
View and visit that folder."
  (interactive)
  (sr-tree-isearch-setup)
  (isearch-forward nil t)
  (run-with-idle-timer 0.01 nil 'sr-tree-isearch-prompt))

(defun sr-tree-isearch-backward (&optional prefix)
  "Prefixable version of `isearch-backward' used in Sunrise Tree mode.
With PREFIX, starts a new `isearch-forward' immediately after the
previous search exits until C-g is pressed."
  (interactive "P")
  (if (or (and prefix (not sr-tree-isearch-always-sticky))
          (and (not prefix) sr-tree-isearch-always-sticky))
      (sr-tree-isearch-setup))
  (isearch-backward nil t)
  (run-with-idle-timer 0.01 nil 'sr-tree-isearch-prompt))

(defun sr-tree-sticky-isearch-backward ()
  "Chain Isearch operations to allow fast navigation through long file paths.
Press C-g to abort, or Return twice on a folder to dismiss Tree
View and visit that folder."
  (interactive)
  (sr-tree-isearch-setup)
  (isearch-backward nil t))

(defun sr-tree-post-isearch (&optional command)
  "Installed in `isearch-mode-end-hook' during sticky Isearch operations."
  (sr-tree-update-cursor)
  (cond (command (sr-tree-isearch-command-loop command))
        (isearch-mode-end-hook-quit (sr-tree-isearch-done))
        ((equal "" isearch-string) (sr-tree-open-branch))
        ((and (sr-tree-toggle-branch 'open)
              (widget-get (sr-tree-get-branch) :args))
         (recenter (truncate (/ (window-body-height) 10.0))))
        (t (ignore)))
  (unless isearch-mode-end-hook-quit
    (sr-tree-sticky-isearch-forward)))

(defun sr-tree-isearch-command-loop (command)
  (funcall command)
  (let* ((msg "Sunrise Tree: sticky Isearch (C-g to exit)")
         (key (read-key-sequence msg))
         (next-command (lookup-key sr-tree-mode-map key)))
    (while (memq next-command '(sr-tree-explode-branch
                                sr-tree-focus-branch
                                sr-tree-blur-branch))
      (funcall next-command)
      (setq key (read-key-sequence msg)
            next-command (lookup-key sr-tree-mode-map key)))
    (isearch-unread-key-sequence (listify-key-sequence key))
    (setq isearch-mode-end-hook-quit nil)))

(defun sr-tree-focus-branch ()
  "Replace the current tree with a new one rooted in the selected directory."
  (interactive)
  (unless (eq (sr-tree-get-branch) sr-tree-root)
    (sr-tree-goto-dir (cdr sr-tree-cursor) t)
    (if sr-tree-open-paths (revert-buffer))))

(defun sr-tree-blur-branch ()
  "Change root of the current tree to its parent, keeping the cursor position."
  (interactive)
  (let ((cursor sr-tree-cursor))
    (unless (eq (sr-tree-get-branch) sr-tree-root)
      (sr-tree-beginning-of-buffer))
    (sr-tree-prev-subdir t)
    (revert-buffer)
    (sr-tree-search-cursor cursor))
  (recenter))

(defun sr-tree-omit-mode (&optional force)
  "Toggle `dired-omit-mode' in the current Sunrise Tree View pane."
  (interactive)
  (setq dired-omit-mode (or force (not dired-omit-mode)))
  (revert-buffer))

(defun sr-tree-avfs-toggle ()
  "Toggle display of compressed archives in Sunrise Tree View panes."
  (interactive)
  (if sr-avfs-root
      (let ((cursor sr-tree-cursor))
        (setq sr-tree-omit-archives (not sr-tree-omit-archives))
        (sr-tree-refresh-dir sr-tree-root)
        (sr-tree-search-cursor cursor)
        (recenter (truncate (/ (window-body-height) 10.0))))))

(defun sr-tree-handle-mouse-event (e handler)
  "Handle mouse event E by updating point and calling HANDLER.
Return t if the event was successfully handled."
  (when (and e (eq major-mode 'sr-tree-mode))
    (mouse-set-point e)
    (when (sr-tree-get-button)
      (sr-tree-update-cursor)
      (funcall handler)
      t)))

(defun sr-tree-mouse-toggle-branch (e)
  "Open/close (graphically) the folder clicked with the mouse.
Also handle the case when the click occurs on the path line."
  (interactive "e")
  (or (sr-tree-handle-mouse-event e 'sr-tree-toggle-branch)
      (sr-mouse-advertised-find-file e)))

(defun sr-tree-mouse-focus-branch (e)
  "Version of `sr-tree-focus-branch' (which see) for the mouse."
  (interactive "e")
  (sr-tree-handle-mouse-event e 'sr-tree-focus-branch))

(defun sr-tree-mouse-blur-branch (e)
  "Version of `sr-tree-blur-branch' (which see) for the mouse."
  (interactive "e")
  (or (sr-tree-handle-mouse-event e 'sr-tree-blur-branch)
      (sr-tree-blur-branch)))

(defun sr-tree-mouse-explode-branch (e)
  "Version of `sr-tree-explode-branch' (which see) for the mouse."
  (interactive "e")
  (sr-tree-handle-mouse-event e 'sr-tree-explode-branch))

;;; ============================================================================
;;; File system navigation functions:

(defun sr-tree-prev-subdir (&optional keep-state)
  "Move to the parent of currently selected directory in Tree View mode.
Resets the list of open directories unless KEEP-STATE is not
nil."
  (interactive)
  (let* ((branch (sr-tree-get-branch))
         (parent (widget-get branch :parent)))
    (cond
     ((tree-widget-p parent)
      (goto-char (widget-get parent :from))
      (sr-tree-update-cursor))

     ((sr-equal-dirs default-directory "/")
      (ignore))

     ((eq branch sr-tree-root)
      (sr-tree-register-path sr-tree-root)
      (sr-tree-goto-dir ".." keep-state)
      (sr-tree-beginning-of-buffer)))))

(defun sr-tree-jump-up ()
  (interactive)
  (sr-tree-prev-subdir t)
  (revert-buffer))

(defun sr-tree-advertised-find-file ()
  "Visit the currently selected file or directory in Sunrise Tree View mode."
  (interactive)
  (let ((target (cdr sr-tree-cursor))
        (sr-goto-dir-function nil)
        (in-search (memq 'sr-tree-post-isearch isearch-mode-end-hook)))
    (sr-tree-check-virtual-size target)
    (if in-search (sr-tree-isearch-done))
    (sr-save-aspect (sr-alternate-buffer (sr-goto-dir target)))
    (if in-search (sr-sticky-isearch))))

(defun sr-tree-mouse-advertised-find-file (e)
  "Visit a file or directory selected using the mouse in the current pane."
  (interactive "e")
  (sr-tree-handle-mouse-event e 'sr-tree-advertised-find-file))

(defun sr-tree-advertised-find-file-other ()
  "Visit the currently selected file or directory in the passive pane."
  (interactive)
  (let ((target (cdr sr-tree-cursor)) (side (sr-other))
        (sr-inhibit-highlight t))
    (sr-tree-check-virtual-size target)
    (save-selected-window
      (select-window (sr-other 'window))
      (sr-goto-dir target)
      (sr-display-attributes (point-min) (point-max) sr-show-file-attributes)
      (sr-keep-buffer side)
      (if (fboundp 'sr-modeline-refresh) (sr-modeline-refresh))
      (if (fboundp 'sr-tabs-refresh) (sr-tabs-refresh)))))

(defun sr-tree-sync ()
  "Toggle the synchronized navigation feature in Sunrise Tree View panes."
  (interactive)
  (sr-sync)
  (sr-tree-update-cursor))

;;; ============================================================================
;;; File system manipulation functions:

(defun sr-tree-get-filename (&optional _localp _no-error)
  "Replacement for `dired-get-filename' in Sunrise Tree functions."
  (cdr sr-tree-cursor))

(defun sr-tree-show-file-type (_file &optional _deref-symlinks)
  "Replacement for `dired-show-file-type' in Sunrise Tree functions."
  (message "%s: directory" (directory-file-name (car sr-tree-cursor))))

(defmacro sr-tree-adapt-dired-command (form)
  "Evaluate FORM with a few Dired functions locally redefined.
Necessary so the basic Dired file manipulation commands can work
in Sunrise Tree View mode."
  `(let ((ad-redefinition-action 'accept))
     (cl-letf (((symbol-function 'dired-get-filename)
                (symbol-function 'sr-tree-get-filename))
               ((symbol-function 'dired-show-file-type)
                (symbol-function 'sr-tree-show-file-type)))
       ,form)))

(defun sr-tree-do-copy ()
  "Recursively copy all selected files and directories between panes.
Copies files from the active to the passive pane."
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-copy)))

(defun sr-tree-do-clone ()
  "Recursively clone all selected files and directories between panes.
Clones files from active to the passive pane."
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-clone)))

(defun sr-tree-do-symlink ()
  "Create symbolic links in the passive pane to selected files in the active pane."
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-symlink)))

(defun sr-tree-do-relsymlink ()
  "Make relative symbolic links in the passive pane to selected files in the active pane."
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-relsymlink)))

(defun sr-tree-do-rename ()
  "Recursively move all selected files and directories between panes.
Moves files from the active pane to the passive pane."
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-rename))
  (unless (file-exists-p (cdr sr-tree-cursor))
    (sr-tree-prev-subdir)
    (sr-tree-refresh-branch)))

(defun sr-tree-do-delete ()
  "Remove the directory selected in the current Sunrise Tree View pane."
  (interactive)
  (sr-tree-adapt-dired-command (sr-do-delete))
  (unless (file-exists-p (cdr sr-tree-cursor))
    (sr-tree-prev-subdir)
    (sr-tree-refresh-branch)))

(defun sr-tree-show-files-info ()
  "Version of `sr-show-files-info' (which see) for Sunrise Tree View panes."
  (interactive)
  (sr-tree-adapt-dired-command (sr-show-files-info)))

(defun sr-tree-create-directory (directory)
  "Create a new directory in Sunrise Tree View mode."
  (interactive
   (list (read-file-name "Create directory: "
                         (file-name-as-directory (cdr sr-tree-cursor)))))
  (let* ((expanded (directory-file-name (expand-file-name directory)))
         (parent (file-name-directory expanded)))
    (make-directory expanded t)
    (when (sr-equal-dirs parent (cdr sr-tree-cursor))
      (sr-tree-toggle-branch 'open)
      (sr-tree-refresh-branch)
      (search-forward
       (file-name-as-directory (file-name-nondirectory expanded)) nil t)
      (sr-tree-update-cursor))))

;;;###autoload
(define-derived-mode sr-tree-mode nil "Sunrise Tree View"
  "Tree view for the Sunrise Commander file manager."
  :group 'sunrise
  (set-keymap-parent sr-tree-mode-map sr-mode-map)
  (mapc 'make-local-variable '(sr-tree-open-paths
                               sr-tree-cursor
                               hl-line-sticky-flag
                               isearch-mode-end-hook
                               desktop-save-buffer
                               revert-buffer-function
                               sr-goto-dir-function
                               sr-buttons-command-adapter))
  (add-hook 'isearch-mode-end-hook 'sr-tree-update-cursor)
  (setq desktop-save-buffer        'sr-tree-desktop-save-buffer
        hl-line-sticky-flag        nil
        revert-buffer-function     'sr-tree-revert-buffer
        sr-goto-dir-function       'sr-tree-goto-dir
        sr-buttons-command-adapter 'sr-tree-buttons-command-adapter)
  (setq dired-omit-mode t)
  (set (make-local-variable 'buffer-quit-function) 'sr-quit)
  (set (make-local-variable 'track-mouse) sr-cursor-follows-mouse)
  (hl-line-mode 1)
  (unless sr-tree-root
    (sr-tree-build default-directory)))

;;; ============================================================================
;;; Sunrise Core + Tabs + Mode Line integration:

(defun sr-tree-view (&optional desktop-mode)
  "Switch from Sunrise normal mode to Tree View.
If DESKTOP-MODE is non-nil, do not kill the current
buffer (necessary during `desktop-read')."
  (interactive)
  (sr-save-aspect
   (if desktop-mode
       (switch-to-buffer (generate-new-buffer "Sunrise Tree"))
     (sr-alternate-buffer
      (switch-to-buffer (generate-new-buffer "Sunrise Tree"))))
   (sr-tree-mode))
  (if (fboundp 'sr-modeline-setup)
      (sr-modeline-setup))
  (if (fboundp 'sr-tabs-engage)
      (sr-tabs-engage))
  (sr-force-passive-highlight))

(defun sr-tree-mouse-view (e)
  "Switch from Sunrise normal mode to Tree View using the mouse."
  (interactive "e")
  (sr-mouse-change-window e)
  (sr-tree-view))

(defun sr-tree-dismiss ()
  "Switch from Tree View to normal mode."
  (interactive)
  (let ((target (widget-get sr-tree-root :path))
        (sr-goto-dir-function nil))
    (sr-save-aspect
     (sr-alternate-buffer (sr-goto-dir target)))))

(defun sr-tree-mouse-dismiss (e)
  "Switch from Tree View to normal mode using the mouse."
  (interactive "e")
  (sr-mouse-change-window e)
  (sr-tree-dismiss))

;;; ============================================================================
;;; Sunrise Buttons integration:

(defvar sr-tree-button-commands
  '((sr-do-copy               . sr-tree-do-copy)
    (sr-do-clone              . sr-tree-do-clone)
    (sr-do-symlink            . sr-tree-do-symlink)
    (sr-do-relsymlink         . sr-tree-do-relsymlink)
    (sr-do-rename             . sr-tree-do-rename)
    (sr-do-delete             . sr-tree-do-delete)
    (sr-goto-dir              . sr-goto-dir)
    (sr-advertised-find-file  . sr-tree-advertised-find-file)
    (sr-quick-view            . sr-tree-advertised-find-file-other)
    (sr-dired-prev-subdir     . sr-tree-prev-subdir)
    (sr-change-window         . sr-change-window)
    (sr-synchronize-panes     . sr-synchronize-panes)
    (sr-sync                  . sr-tree-sync)
    (sr-beginning-of-buffer   . sr-tree-beginning-of-buffer)
    (sr-end-of-buffer         . sr-tree-end-of-buffer)
    (sr-term                  . sr-term)
    (sr-describe-mode         . sr-describe-mode)
    (sr-transpose-panes       . sr-transpose-panes)
    (revert-buffer            . revert-buffer)
    (sr-split-toggle          . sr-split-toggle)
    (sr-toggle-truncate-lines . sr-toggle-truncate-lines)
    (dired-create-directory   . sr-tree-create-directory)
    (sr-history-prev          . sr-history-prev)
    (sr-history-next          . sr-history-next)
    ) "Sunrise Buttons-to-Tree commands translation table.")

(defun sr-tree-buttons-command-adapter (command)
  "Execute the given buttons command in the current Sunrise Tree View pane.
If the command doesn't make sense in the current context, first
switch to normal mode, then execute."
  (let ((translation (cdr (assq command sr-tree-button-commands))))
    (if translation
        (call-interactively translation)
      (sr-tree-dismiss)
      (call-interactively command))))

;;; ============================================================================
;;; Tree Widget adapter functions:

(defun sr-tree-widget-forward (arg)
  "`widget-forward' replacement for use in `tree-widget-button-keymap'."
  (interactive "p")
  (if (eq major-mode 'sr-tree-mode)
      (sr-change-window)
    (widget-forward arg)))

(defun sr-tree-widget-button-press (pos &optional event)
  "`widget-button-press' replacement for use in `tree-widget-button-keymap'."
  (interactive "@d")
  (if (eq major-mode 'sr-tree-mode)
      (sr-tree-open-branch)
    (widget-button-press pos event)))

(defun sr-tree-widget-button-click (event)
  "`widget-button-click' replacement for use in `tree-widget-button-keymap'."
  (interactive "e")
  (unless (eq major-mode 'sr-tree-mode)
    (tree-widget-button-click event)))

;;; ============================================================================
;;; Sunrise Tree View keybindings:

(define-key sr-mode-map "\C-t "             'sr-tree-view)
(define-key sr-mode-map "\C-t\C-m"          'sr-tree-view)
(define-key sr-mode-map [(shift meta down)] 'sr-tree-view)
(define-key sr-mode-map [?\e down]          'sr-tree-view)

(define-key sr-tree-mode-map "\C-m"     'sr-tree-open-branch)
(define-key sr-tree-mode-map " "        'sr-tree-toggle-branch)
(define-key sr-tree-mode-map "\C-o"     'sr-tree-omit-mode)
(define-key sr-tree-mode-map "n"        'sr-tree-next-line)
(define-key sr-tree-mode-map "p"        'sr-tree-previous-line)
(define-key sr-tree-mode-map "\t"       'sr-change-window)
(define-key sr-tree-mode-map "g"        'sr-tree-refresh-branch)
(define-key sr-tree-mode-map "J"        'sr-tree-jump-up)
(define-key sr-tree-mode-map "j"        'sr-tree-build-new)
(define-key sr-tree-mode-map "f"        'sr-tree-advertised-find-file)
(define-key sr-tree-mode-map "v"        'sr-tree-advertised-find-file-other)
(define-key sr-tree-mode-map "o"        'sr-tree-advertised-find-file-other)
(define-key sr-tree-mode-map "\M-a"     'sr-tree-beginning-of-buffer)
(define-key sr-tree-mode-map "\M-e"     'sr-tree-end-of-buffer)
(define-key sr-tree-mode-map "\C-s"     'sr-tree-isearch-forward)
(define-key sr-tree-mode-map "\C-cs"    'sr-tree-sticky-isearch-forward)
(define-key sr-tree-mode-map "\C-r"     'sr-tree-isearch-backward)
(define-key sr-tree-mode-map "\C-cr"    'sr-tree-sticky-isearch-backward)
(define-key sr-tree-mode-map "\C-c\C-c" 'sr-tree-dismiss)
(define-key sr-tree-mode-map "\C-cf"    'sr-tree-focus-branch)
(define-key sr-tree-mode-map "\C-cb"    'sr-tree-blur-branch)
(define-key sr-tree-mode-map "\C-c\C-m" 'sr-tree-explode-branch)
(define-key sr-tree-mode-map "\C-t "    'sr-tree-dismiss)
(define-key sr-tree-mode-map "\C-t\C-m" 'sr-tree-dismiss)

(define-key sr-tree-mode-map "#" 'sr-tree-avfs-toggle)

(define-key sr-tree-mode-map [up]   'sr-tree-previous-line)
(define-key sr-tree-mode-map [down] 'sr-tree-next-line)

(define-key sr-tree-mode-map [right]     'sr-tree-open-branch)
(define-key sr-tree-mode-map [S-right]   'sr-tree-focus-branch)
(define-key sr-tree-mode-map [?\e right] 'sr-tree-focus-branch)
(define-key sr-tree-mode-map [C-right]   'sr-tree-explode-branch)
(define-key sr-tree-mode-map [3 right]   'sr-tree-explode-branch) ;; "\C-c <right>"

(define-key sr-tree-mode-map [left]     'sr-tree-collapse-branch)
(define-key sr-tree-mode-map [S-left]   'sr-tree-blur-branch)
(define-key sr-tree-mode-map [?\e left] 'sr-tree-blur-branch)
(define-key sr-tree-mode-map [C-left]   'sr-tree-collapse-branch)
(define-key sr-tree-mode-map [3-left]   'sr-tree-collapse-branch) ;; "\C-c <left>"
(define-key sr-tree-mode-map [delete]   'sr-tree-collapse-branch)

(define-key sr-tree-mode-map [next]              'sr-tree-scroll-up)
(define-key sr-tree-mode-map [prior]             'sr-tree-scroll-down)
(define-key sr-tree-mode-map [backspace]         'sr-tree-collapse-branch)
(define-key sr-tree-mode-map [C-return]          'sr-tree-explode-branch)
(define-key sr-tree-mode-map [S-return]          'sr-tree-focus-branch)
(define-key sr-tree-mode-map [M-return]          'sr-tree-blur-branch)
(define-key sr-tree-mode-map [(shift meta down)] 'sr-tree-dismiss)
(define-key sr-tree-mode-map [?\e down]          'sr-tree-dismiss)

(define-key sr-tree-mode-map "C" 'sr-tree-do-copy)
(define-key sr-tree-mode-map "K" 'sr-tree-do-clone)
(define-key sr-tree-mode-map "R" 'sr-tree-do-rename)
(define-key sr-tree-mode-map "D" 'sr-tree-do-delete)
(define-key sr-tree-mode-map "S" 'sr-tree-do-symlink)
(define-key sr-tree-mode-map "Y" 'sr-tree-do-relsymlink)
(define-key sr-tree-mode-map "y" 'sr-tree-show-files-info)
(define-key sr-tree-mode-map "+" 'sr-tree-create-directory)

(define-key sr-tree-mode-map "\C-c\C-z" 'sr-tree-sync)
(define-key sr-tree-mode-map "\C-q"     'sr-tree-sync)

(dotimes (n 10)
  (define-key sr-tree-mode-map (number-to-string n) 'digit-argument))

(mapc (lambda (x)
        (define-key sr-tree-mode-map x nil))
      '("Q" "F" "A" "k" "s" "\C-c/"))

(define-key sr-tree-mode-map [mouse-1] 'sr-tree-mouse-toggle-branch)
(define-key sr-tree-mode-map [mouse-2] (lambda ()
                                         (interactive)
                                         (call-interactively 'mouse-set-point)
                                         (sr-advertised-find-file)
                                         (sr-tree-update-cursor)))
(define-key sr-tree-mode-map [double-mouse-1] 'sr-tree-mouse-advertised-find-file)
(define-key sr-tree-mode-map [S-mouse-1] 'sr-tree-mouse-focus-branch)
(define-key sr-tree-mode-map [M-mouse-1] 'sr-tree-mouse-blur-branch)
(define-key sr-tree-mode-map [C-mouse-1] 'sr-tree-mouse-explode-branch)

(define-key sr-tree-mode-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key sr-tree-mode-map (kbd "<M-down-mouse-1>") 'ignore)
(define-key sr-tree-mode-map (kbd "<C-down-mouse-1>") 'ignore)

(define-key sr-mode-map (kbd "<M-S-down-mouse-1>") 'sr-tree-mouse-view)
(define-key sr-tree-mode-map (kbd "<M-S-down-mouse-1>") 'sr-tree-mouse-dismiss)

(define-key tree-widget-button-keymap "\t" 'sr-tree-widget-forward)
(define-key tree-widget-button-keymap "\C-m" 'sr-tree-widget-button-press)
(define-key tree-widget-button-keymap [down-mouse-1] 'sr-tree-widget-button-click)
(define-key tree-widget-button-keymap [mouse-1] 'tree-widget-button-click)
(define-key tree-widget-button-keymap [double-mouse-1] 'sr-tree-mouse-advertised-find-file)
(define-key tree-widget-button-keymap [C-mouse-1] 'sr-tree-mouse-explode-branch)

;;; ============================================================================
;;; Bootstrap:

(defun sr-tree-menu-init ()
  "Initialize the Sunrise Tree extension menu."

  (unless (lookup-key sr-mode-map [menu-bar Sunrise])
    (define-key sr-mode-map [menu-bar Sunrise]
      (cons "Sunrise" (make-sparse-keymap))))
  (let ((menu-map (make-sparse-keymap "Tree View")))
    (define-key sr-mode-map [menu-bar Sunrise tree-view]
      (cons "Tree View" menu-map))
    (define-key menu-map [enable-tree-view] '("enable" . sr-tree-view)))

  (define-key sr-tree-mode-map [menu-bar Sunrise]
    (cons "Sunrise" (make-sparse-keymap)))
  (let ((menu-map (make-sparse-keymap "Tree View")))
    (define-key sr-tree-mode-map [menu-bar Sunrise tree-view]
      (cons "Tree View" menu-map))
    (define-key menu-map [enable-tree-view] nil)
    (define-key menu-map [disable-tree-view] '("dismiss" . sr-tree-dismiss)))

  (remove-hook 'sr-start-hook 'sr-tree-menu-init)
  (unintern 'sr-tree-menu-init obarray))

(add-hook 'sr-start-hook 'sr-tree-menu-init)

(defun sunrise-x-tree-unload-function ()
  (sr-ad-disable "^sr-tree-"))

;;; ============================================================================
;;; Desktop support:

(defun sr-tree-desktop-save-buffer (desktop-dir)
  "Return additional data for saving a Sunrise tree buffer to a desktop file."
  (append `((root . ,(widget-get sr-tree-root :path))
            (open-paths . ,sr-tree-open-paths)
            (cursor ,sr-tree-cursor)
            (omit-mode . ,dired-omit-mode))
          (if (eq (current-buffer) sr-left-buffer) '((left . t)))
          (if (eq (current-buffer) sr-right-buffer) '((right . t)))
          (if (fboundp 'sr-tabs-desktop-save-buffer)
              (sr-tabs-desktop-save-buffer desktop-dir))))

(defun sr-tree-desktop-restore-buffer (desktop-buffer-file-name
                                       desktop-buffer-name
                                       desktop-buffer-misc)
  "Restore a Sunrise tree buffer from a description in a desktop file."
  (sr-tree-view t)
  (setq sr-tree-open-paths (cdr (assoc 'open-paths desktop-buffer-misc)))
  (setq dired-omit-mode (cdr (assoc 'omit-mode desktop-buffer-misc)))
  (setq sr-tree-cursor (cadr (assoc 'cursor desktop-buffer-misc)))
  (sr-tree-build (cdr (assoc 'root desktop-buffer-misc)))
  (sr-tree-update-cursor)
  (mapc (lambda (side)
          (when (cdr (assoc side desktop-buffer-misc))
            (set (sr-symbol side 'buffer) (current-buffer))
            (set (sr-symbol side 'directory) (cdr sr-tree-cursor))))
        '(left right))
  (if (fboundp 'sr-tabs-desktop-restore-buffer)
      (sr-tabs-desktop-restore-buffer desktop-buffer-file-name
                                      desktop-buffer-name
                                      desktop-buffer-misc)))

(add-to-list 'desktop-buffer-mode-handlers
             '(sr-tree-mode . sr-tree-desktop-restore-buffer))

(provide 'sunrise-x-tree)

;;;###autoload (eval-after-load 'sunrise-commander '(sr-extend-with 'sunrise-x-tree))

;;; sunrise-x-tree.el ends here

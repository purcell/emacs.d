;;; sunrise-x-buttons.el --- mouse-clickable shortcut buttons for the Sunrise Commander File Manager -*- lexical-binding: t -*-

;; Copyright (C) 2008-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 11 Jun 2008
;; Version: 1
;; RCS Version: $Rev: 444 $
;; Keywords: sunrise commander, shortcut buttons
;; URL: http://www.emacswiki.org/emacs/sunrise-x-buttons.el
;; Compatibility: GNU Emacs 22+

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation,  either  version  3 of the License, or (at your option) any later
;; version.
;;
;; This  program  is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See the GNU General Public License for more de-
;; tails.

;; You  should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Here is a small extension that may be of help to new users who want to get
;; acquainted fast with the most frequent functions found in the Sunrise
;; Commander and their keybindings. Once installed, it displays a panel with
;; mouse clickable buttons that show some of the most useful actions performed
;; by Sunrise and their respective bindings in the bottom window (a.k.a. viewer
;; window here) every time the main panels are invoked. You can execute any of
;; these functions by clicking the appropriate button, but the extension was
;; conceived more as a simple cheat sheet (a very, very limited one, as you can
;; easily learn by pressing the last button, labeled "More...") than as a real
;; interface to Sunrise and Dired functions. Eventually, if you like this kind
;; of interaction with the program you can add your own commands to the list and
;; let this extension manage the creation and layout of the buttons for you.

;; This extension was developed on GNU Emacs 23 on Linux, and tested on
;; GNU Emacs 22 and 23 for Linux and on EmacsW32 (version 22) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-x-buttons) to your .emacs file, preferably right
;; after (require 'sunrise-commander).

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;; That's it - the next time you activate Sunrise you'll see a nice button panel
;; in the viewer window.

;;; Code:

(require 'sunrise-commander)
(require 'cus-edit)
(eval-when-compile (require 'cl))

(defvar sr-buttons-buffer-name "*Sunrise Buttons*"
  "Name of the Sunrise buttons buffer")

(defvar sr-buttons-command-adapter nil
  "Function to use to execute button commands, or nil to do the default.")

(defvar sr-buttons-list
  '(
    ("GotoDir([F2,]j,/)" 'sr-goto-dir                "Go to any directory in active pane")
    ("View([F3,]v,o)"    'sr-quick-view              "View selected file or directory in this window")
    ("Open([F4,]Enter)"  'sr-advertised-find-file    "Visit selected file or directory")
    ("Copy([F5,]C)"      'sr-do-copy                 "Copy selected files to passive pane")
    ("Rename([F6,]R)"    'sr-do-rename               "Move selected files to passive pane")
    ("Clone(K)"          'sr-do-clone                "Clone selected files to passive pane")
    ("NewDir([F7,]+)"    'dired-create-directory     "Create new directory in active pane")
    ("Delete([F8,]D)"    'sr-do-delete               "Delete selected files from active pane")
    nil
    ("DirUp([C-PgUp,]J)" 'sr-dired-prev-subdir       "Go to parent directory in active pane")
    ("DirBack(M-y)"      'sr-history-prev            "Go to previous directory in history")
    ("DirFrwd(M-u)"      'sr-history-next            "Go to next directory in history")
    ("HardLink(H)"       'sr-do-hardlink             "Make hard link of selected file in passive pane")
    ("SymLink(S)"        'sr-do-symlink              "Make absolute symlink of selected entry in passive pane")
    ("RelSymLink(Y)"     'sr-do-relsymlink           "Make relative symlink of selected entry in passive pane")
    ("Hidden(C-o)"       'sr-omit-mode               "Hide/Show hidden files in active pane")
    ("Attrs(C-Bksp)"     'sr-toggle-attributes       "Hide/Show file attributes in active pane")
    nil
    ("Other(Tab)"        'sr-change-window           "Switch to passive pane")
    ("ClonePane(M-o)"    'sr-synchronize-panes       "Make both panes contain the same directory")
    ("Swap(M-t)"         'sr-transpose-panes         "Transpose panes")
    ("Refresh(g)"        'revert-buffer              "Rescan directory in active pane")
    ("Align(C-cC-s)"     'sr-split-toggle            "Change panes alignment (vertical/horizontal/top)")
    ("Sort(s)"           'sr-interactive-sort        "Sort interactively entries in active pane")
    ("Mark([Ins,]m)"     'dired-mark                 "Mark selected entry in active pane")
    ("Unmark(Bksp)"      'dired-unmark-backward      "Unmark last selected entry inactive pane")
    nil
    ("History(C-cC-d)"   'sr-recent-directories      "Display listing of recently visited directories")
    ("Recent(C-cC-r)"    'sr-recent-files            "Display listing of recently visited files")
    ("Restore(C-cC-c)"   'sr-buttons-restore-mode    "Dismiss VIRTUAL or WDired mode")
    ("Find(C-cC-f)"      'sr-find                    "Find files and directories interactively")
    ("FName(C-cC-n)"     'sr-find-name               "Find files and directories by name pattern")
    ("FGrep(C-cC-g)"     'sr-find-grep               "Find files containing some expression")
    ("Follow(;)"         'sr-follow-file             "Follow file (go to same directory as file)")
    ("Locate(C-cC-l)"    'sr-locate                  "Find files and directories using locate database")
    nil
    ("Search(A)"         'sr-do-search               "Search for string/regexp in all marked entries")
    ("Compare(C-M-=)"    'sr-compare-dirs            "Compare directories in panes")
    ("Diff(=)"           'sr-diff                    "Compare selected entries using diff")
    ("Ediff(C-=)"        'sr-ediff                   "Compare selected entries using ediff")
    ("Store(C-c>)"       'sr-checkpoint-save         "Remember current position of panes as name")
    ("Recall(C-c.)"      'sr-checkpoint-restore      "Set panes to a previously remembered position")
    ("Home(M-a)"         'sr-beginning-of-buffer     "Go to first entry in active pane")
    ("End(M-e)"          'sr-end-of-buffer           "Go to last entry in active pane")
    nil
    ("FindReplace(Q)"    'sr-do-query-replace-regexp "Find and replace in all selected entries")
    ("Fuzzy(C-c/)"       'sr-fuzzy-narrow            "Narrow pane contents with fuzzy matching")
    ("CmdLine(C-ct)"     'sr-term                    "Open Command line in this window")
    ("WDired(C-xC-q)"    'sr-buttons-editable-pane   "Edit active pane using wdired")
    ("SyncNav(C-cC-z)"   'sr-sync                    "Toggle on/off synchronized navigation mode")
    ("LongLines(M-l)"    'sr-toggle-truncate-lines   "Truncate/Restore long lines in active pane")
    ("More...(h)"        'sr-describe-mode           "More commands and keybindings")
    ("Quit([F10,]q)"     'sr-quit                    "Dismiss Sunrise Commander")
    )
  "Sunrise button definitions.")

(eval-and-compile
  (unless (fboundp 'Custom-mode)
    (defalias 'Custom-mode 'custom-mode)))

(define-derived-mode sr-buttons-mode Custom-mode "Sunrise Buttons"
  "Sunrise Commander Buttons panel mode."
  :group 'sunrise
  (set-keymap-parent sr-buttons-mode-map custom-mode-map)

  (make-local-variable 'double-click-time)
  (setq double-click-time nil)
  (make-local-variable 'double-click-fuzz)
  (setq double-click-fuzz 0)

  (defun sr-buttons-click ()
    "Handle all click events that take place in the buttons buffer."
    (interactive)
    (unwind-protect
        (call-interactively 'widget-button-click)
      (sr-select-window sr-selected-window)))

  (mapc (lambda (x) (define-key sr-buttons-mode-map x 'sr-buttons-click))
        '([down-mouse-1] [down-mouse-2] [down-mouse-3]))

  (mapc (lambda (x) (define-key sr-buttons-mode-map x
                      (lambda () (interactive)
                        (sr-select-window sr-selected-window))))
        '([(control tab)] "\C-c\t"
          [mouse-1]             [mouse-2]             [mouse-3]
          [drag-mouse-1]        [drag-mouse-2]        [drag-mouse-3]
          [double-mouse-1]      [double-mouse-2]      [double-mouse-3]
          [triple-mouse-1]      [triple-mouse-2]      [triple-mouse-3]
          [double-drag-mouse-1] [double-drag-mouse-2] [double-drag-mouse-3]
          [triple-drag-mouse-1] [triple-drag-mouse-2] [triple-drag-mouse-3]
          [double-down-mouse-1] [double-down-mouse-2] [double-down-mouse-3]
          [triple-down-mouse-1] [triple-down-mouse-2] [triple-down-mouse-3])))

(add-hook 'sr-start-hook 'sr-buttons-display)
(add-hook 'sr-quit-hook (defun sr-buttons-sr-quit-function ()
                          (let ((buttons (get-buffer sr-buttons-buffer-name)))
                            (if buttons (bury-buffer buttons)))))
(add-hook 'kill-buffer-hook
          (defun sr-buttons-kill-buffer-function ()
            (if (and sr-running
                     (eq (current-buffer) other-window-scroll-buffer))
                (sr-buttons-display))))

(defun sr-buttons-display ()
  "Display the buttons buffer in the viewer window.
If no buttons buffer exists yet, creates one."
  (unless (and (boundp 'sr-popviewer-mode) (symbol-value 'sr-popviewer-mode))
    (apply 'require '(cus-edit))
    (sr-select-viewer-window t)
    (cond ((buffer-live-p other-window-scroll-buffer) ;;<-- don't nuke quick views!
           (switch-to-buffer other-window-scroll-buffer))
          ((get-buffer "*terminal*")                  ;;<-- prefer terminals
           (switch-to-buffer "*terminal*"))
          (t
           (switch-to-buffer sr-buttons-buffer-name)
           (setq truncate-lines t)
           (setq line-spacing 5)
           (setq cursor-in-non-selected-windows nil)
           (if (not (eq major-mode 'sr-buttons-mode))
               (let ((line-spacing 2)
                     (cursor-in-non-selected-windows nil))
                 (sr-buttons-render)))))
  (sr-select-window sr-selected-window)))

(defun sr-buttons-render ()
  "Populate current buffer with all widgets described in `sr-buttons-list'."
  (sr-buttons-mode)
  (let ((mc-keys-on (sr-buttons-mc-keys-p))
        (maxlen (sr-buttons-maxtaglen)))
    (mapc (lambda (x) (sr-buttons-build x mc-keys-on maxlen)) sr-buttons-list))
  (sr-buttons-eol)
  (goto-char (point-min)))

(defun sr-buttons-build (spec mc-keys-on maxlen)
  "Build and render a new widget in the buttons buffer.
The first argument is an element of `sr-buttons-list' (list
containing tag, action and hint), the second one is a flag that
indicates whether mc style keybindings have been activated in
Sunrise, and the last one is the length of the longest tag in the
list."
  (if (or (null spec)
          (> (+ (current-column) maxlen) (- (window-width) (/ maxlen 2))))
      (sr-buttons-eol)
    (let ((tag (first spec))
          (action (second spec))
          (hint (third spec)))
      (if mc-keys-on
          (setq tag (replace-regexp-in-string "\\[\\|\\]" "" tag))
        (setq tag (replace-regexp-in-string "\\[.*\\]" "" tag)))
      (setq tag (sr-buttons-normalize-tag tag maxlen ? ))
      (widget-create 'push-button :tag tag
                                  :action (sr-buttons-action action)
                                  :help-echo hint)
      (insert-char ?  1)
      (put-text-property
       (1- (point)) (point) 'display (list 'space :width 0.15)))))

(defun sr-buttons-eol ()
  "Terminate the current row of buttons while building the buttons buffer.
Centers it if necessary."
  (let* ((gap (- (window-width) (current-column) 2))
         (margin (/ gap 2)))
    (if (> margin 0)
        (save-excursion (beginning-of-line) (insert-char ?  margin)))
    (unless (eq ?\n (char-before)) (insert "\n"))))

(defun sr-buttons-mc-keys-p ()
  "Determine whether mc-style keybindings have been activated in Sunrise."
  (eq 'sr-goto-dir (cdr (assq 'f2 sr-mode-map))))

(defun sr-buttons-maxtaglen ()
  "Calculate the length of the longest tag in `sr-buttons-list'."
  (let* ((regexp (if (sr-buttons-mc-keys-p) "\\[\\|\\]" "\\[.*\\]"))
         (lenfun (lambda (x)
                   (if x
                       (length (replace-regexp-in-string regexp "" (car x)))
                     0))))
    (apply 'max (mapcar lenfun sr-buttons-list))))

(defun sr-buttons-normalize-tag (tag total-length fill-char)
  "Lengthen the given tag to TOTAL-LENGTH.
Works by prepending and appending the appropriate number of fill
characters, so the text appears approximately centered on its
button."
  (let* ((fill-length (- total-length (length tag)))
         (before (/ fill-length 2))
         (after (- fill-length before)))
    (concat (make-string before fill-char)
            tag
            (make-string after fill-char))))

(defun sr-buttons-action (action)
  "Return a button command to perform ACTION inside the currently active pane."
  `(lambda (&rest ignore)
     (interactive)
     (sr-select-window sr-selected-window)
     (if sr-buttons-command-adapter
         (run-with-timer 0.01 nil (funcall sr-buttons-command-adapter ,action))
       (run-with-timer 0.01 nil (sr-buttons-do ,action)))))

(defun sr-buttons-do (action)
  "Execute ACTION interactively as response to the click of a button."
  (hl-line-mode -1)
  (call-interactively action)
  (when (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
    (hl-line-mode 1)
    (sr-graphical-highlight))
  t)

(defun sr-buttons-editable-pane ()
  "Call `sr-editable-pane' and display an informative message.
Used inside the Sunrise Buttons buffer."
  (interactive)
  (sr-editable-pane)
  (message "Push [Restore] button or C-c C-c when done, ESC C-c C-c to cancel"))

(defun sr-buttons-restore-mode ()
  "Implement the [Restore] action in the Sunrise buttons panel."
  (interactive)
  (cond ((eq major-mode 'sr-virtual-mode) (sr-virtual-dismiss))
        ((eq major-mode 'sr-tree-mode) (eval '(sr-tree-dismiss)))
        ((string= mode-name "Editable Dired") (eval '(wdired-finish-edit)))
        (t (message "Already in regular mode"))))

(provide 'sunrise-x-buttons)

;;;###autoload (eval-after-load 'sunrise-commander '(sr-extend-with 'sunrise-x-buttons))

;;; sunrise-x-buttons.el ends here

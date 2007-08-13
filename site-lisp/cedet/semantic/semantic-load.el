;;; semantic-load.el --- Autoload definitions for Semantic

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-load.el,v 1.56 2007/05/31 02:25:55 zappo Exp $

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Initialize semantic for all supported conditions.

;;; Code:
;;

(require 'semantic-fw)

;;; Add parser and doc directories
;;
(let ((dir (file-name-directory (locate-library "semantic"))))
  (add-to-list 'load-path (expand-file-name "bovine" dir))
  (add-to-list 'load-path (expand-file-name "wisent" dir))
  (add-to-list 'Info-default-directory-list (expand-file-name "doc" dir))
  )

;;; Some speedbar major modes
(eval-after-load "speedbar"
  '(progn
     (require 'semantic-cb)
     (require 'semantic-ia-sb)))

;;; Useful predefined setup
;;
(defvar semantic-load-imenu-string "TAGS"
  "String used in `semantic-load' startup for the Imenu menu item.")

(defvar semantic-load-turn-everything-on nil
  "Non-nil means turn on all features in the semantic package.")

(defvar semantic-load-turn-useful-things-on nil
  "Non-nil means turn on all `useful' features.
Sadly `useful' here means things Eric wants on as opposed to some
other criteria.")

(defvar semantic-load-system-cache-loaded nil
  "Non nil when the system caches have been loaded.
Prevent this load system from loading files in twice.")

(defun semantic-load-enable-minimum-features ()
  "Enable the minimum number of semantic features for basic usage.
This includes:
 `semantic-idle-scheduler-mode' - Keeps a buffer's parse tree up to date.
 `semanticdb-minor-mode' - Stores tags when a buffer is not in memory.
 `semanticdb-load-system-caches' - Loads any systemdbs created earlier.
 `semanticdb-load-ebrowse-caches' - Loads any ebrowse dbs created earlier."
  (interactive)

  (global-semantic-idle-scheduler-mode 1)

  (global-semanticdb-minor-mode 1)

  ;; Don't do the loads from semantic-load twice.
  (when (null semantic-load-system-cache-loaded)

    ;; This loads any created system databases which get linked into
    ;; any searches performed.
    (setq semantic-load-system-cache-loaded t)
    (when (and (boundp 'semanticdb-default-system-save-directory)
	       (stringp semanticdb-default-system-save-directory)
	       (file-exists-p semanticdb-default-system-save-directory))
      (semanticdb-load-system-caches))

    ;; This loads any created ebrowse databases which get linked into
    ;; any searches performed.
    (when (and (not (featurep 'xemacs))
	       (boundp 'semanticdb-default-system-save-directory)
	       (stringp semanticdb-default-system-save-directory)
	       (file-exists-p semanticdb-default-system-save-directory))
      (semanticdb-load-ebrowse-caches))
    )
  )

(defun semantic-load-enable-code-helpers ()
  "Enable some semantic features that provide coding assistance.
This includes `semantic-load-enable-minimum-features' plus:
  `imenu' - Lists Semantic generated tags in the menubar.
  `semantic-idle-summary-mode' - Show a summary for the tag indicated by
                                 code under point.  (intellisense)
  `senator-minor-mode' - Semantic Navigator, and global menu for all
                         features semantic.
  `semantic-mru-bookmark-mode' - Provides a `switch-to-buffer' like
                       keybinding for tag names."
  (interactive)

  (semantic-load-enable-minimum-features)

  (when (and (eq window-system 'x)
	     (locate-library "imenu"))
    (add-hook 'semantic-init-hooks (lambda ()
				     (condition-case nil
					 (imenu-add-to-menubar
					  semantic-load-imenu-string)
				       (error nil)))))

  (global-semantic-idle-summary-mode 1)

  ;; Do this last.  This allows other minor modes to get loaded
  ;; in so they appear in the menu properly.
  (global-senator-minor-mode 1)

  )

(defun semantic-load-enable-gaudy-code-helpers ()
  "Enable semantic features that provide gaudy coding assistance.
This includes `semantic-load-enable-code-helpers'.
  `semantic-stickyfunc-mode' - Tracks current function in header-line
                               (when available).
  `semantic-idle-completions-mode' - Provide smart symbol completion
                                 automatically at idle time.
  `semantic-decoration-mode' - Decorate tags based on various attributes."
  (interactive)

  (global-semantic-decoration-mode 1)

  (when (boundp 'header-line-format)
    (global-semantic-stickyfunc-mode 1))

  ;; Idle Completions mode is more annoying than useful
  ;; when it keeps splitting the window to show you completions.
  ;; Using speedbar for this would be better.
  (condition-case nil
      (when (and (featurep 'tooltip) tooltip-mode)
	(global-semantic-idle-completions-mode 1))
    (error nil))

  (semantic-load-enable-code-helpers)
  )

(semantic-alias-obsolete 'semantic-load-enable-guady-code-helpers
                         'semantic-load-enable-gaudy-code-helpers)

(defun semantic-load-enable-excessive-code-helpers ()
  "Enable all semantic features that provide coding assistance.
This includes all features of `semantic-load-enable-code-helpers' plus:
  `which-func-mode' - Display the current function in the mode line."
  (interactive)

  (semantic-load-enable-code-helpers)

  (if (fboundp #'which-func-mode)
      (add-hook 'semantic-init-hooks (lambda ()
				       (which-func-mode 1))))
  )

(defun semantic-load-enable-semantic-debugging-helpers ()
  "Enable all semantic features that assist with debugging semantic.
These modes include:
  `semantic-highlight-edits-mode' - Highlight text that has been edited
                            since the last parse step.
  `semantic-show-unmatched-syntax-mode' - Highlight lexical tokens which
                            failed to be parsed.
  `semantic-show-parser-state-mode' - Show the current buffer state via
                            small indicators in the mode line."
  (interactive)

  (global-semantic-highlight-edits-mode 1)

  ;; This ought to be a code helper, but it is still
  ;; a bit on the lame side.  Opinions?
  (global-semantic-show-unmatched-syntax-mode 1)

  (global-semantic-show-parser-state-mode 1)

  ;; This enables debug output from the incremental parser.
  ;; Perhaps a mode for that dumps stuff in a `messages' like buffer
  ;; would be better?
  (setq semantic-edits-verbose-flag t)

  )

;; Old style variables.  Use only if we are not in batch mode.
(when (not noninteractive)
  (cond
   (semantic-load-turn-everything-on
    (semantic-load-enable-excessive-code-helpers))
   (semantic-load-turn-useful-things-on
    (semantic-load-enable-code-helpers))
   ))

(provide 'semantic-load)

;;; semantic-load.el ends here

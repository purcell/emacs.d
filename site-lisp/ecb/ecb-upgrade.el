;;; ecb-upgrade.el --- Upgrade an old ecb-version to the latest one

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2002

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-upgrade.el,v 1.115 2009/05/15 15:19:53 berndl Exp $

;;; Commentary:
;;
;; This file upgrades an old ECB-version best possible to the latest one.
;;
;; What is the intention of this library:
;;
;; Big packages like ECB will be enhanced and developed continuously so
;; sometimes a new version must be released. Such packages offer in general a
;; lot of customizable options so probably some of these options change the
;; type or are renamed because the old type and/or name of the option makes no
;; sense in the new release.
;;
;; Especially options which have changed the type of their value are now a
;; problem for the user which want to upgrade to the latest ECB-version: If
;; the user has saved a certain value for option X in its .emacs-file but the
;; type of this saved value doesn't match the new defined type in the
;; defcustom-form after an ECB-upgrade then there can occur serious problems
;; like ECB can not be started anymore or even Emacs can not be started
;; without errors.
;;
;; Until now there was only one way to fix these problems: The user must
;; manually edit his .emacs-file and remove all entries for options which have
;; now another type. After this and after restarting Emacs the new
;; default-values of the type-changed options in the new ECB-release are
;; active and the user can go on using Emacs and ECB. But this approach to fix
;; the incompatible-option-problem has two serious drawbacks:
;; 1. The user must manually edit the customize-section in his .emacs-file.
;;    This should normally not be done and if then only by old-handed
;;    Emacs-users.
;; 2. The customized value of the option X in the old-release (with the old
;;    type) is lost because after removing the related entry from the
;;    .emacs-file the new default-value is active, so the user must
;;    re-customize the option X.
;;
;; OK, this is one half of the option-upgrade-problem but a new ECB-release
;; can also rename a option from name X to name Y because the new name Y makes
;; much more sense and/or is more mnemonic. If only the name has changed but
;; not the type this is not a serious problem like above but also annoying
;; because the customized value of the old-option X takes no effect in the new
;; release but instead the default-value of the new-option Y is now active.
;; But nevertheless this problem has the drawback number 2 (see above).
;;
;; The last category of upgrade-problems is a renamed option which has also
;; changed its type.
;;
;; ecb-upgrade.el is the solution for all these problems:

;; - It checks all customized values of all ECB-options if they are still
;;   type-compatible. If not then it tries to upgrade the old-value to the new
;;   value-type and if this is not possible then it resets the option to the
;;   new default value and store it via customize in the .emacs-file (or in
;;   any file which is used for customized options).
;; - It offers a special constant `ecb-upgradable-option-alist' which allows
;;   the ECB-maintainers to define special transformings for renamed options
;;   so even the value of an old-option X can be savely transformed to the
;;   new-option Y and the old setting is not lost.
;;
;; All these checks and transformings are done at beginning of activating ECB.
;; If ECB has recognized incompatible or renamed options it does its
;; upgrading/reseting-job so all ECB-options have correct types so ECB can
;; start correct. After ECB is started it displays a list of all upgraded or
;; reseted option with their old and new values.
;;
;; How does this library work:
;;
;; The important functions are `ecb-check-not-compatible-options' and
;; `ecb-upgrade-not-compatible-options':
;;
;; The former one checks if all customized values of ECB-options have still
;; correct type. If not the incompatible options and their old values are
;; stored in an alist `ecb-not-compatible-options'. Only this function is
;; allowed to changed this alist!!
;;
;; The latter one processes now this alist and looks for every incompatible
;; option if there is an entry in `ecb-upgradable-option-alist'. If yes then a
;; special value-transforming is tried by `ecb-option-upgrade'. If no or if
;; the special transforming has been failed for any reason then it resets the
;; option to the default-value of current active ECB-version and save it via
;; `customize-save-variable'.
;;
;; So if the ECB-maintainers define no special transforming in the alist
;; `ecb-upgradable-option-alist' for a re-typed option X then all incompatible
;; options are at least reset to their current default-value and therefore ECB
;; can start correct.
;;
;; But there is another function `ecb-upgrade-renamed-options': This function
;; processes the alist `ecb-upgradable-option-alist' and call for every
;; element-key (an old option-symbol) of this alist `ecb-option-upgrade' but
;; only if this element-key is not also contained in the alist
;; `ecb-not-compatible-options' because in this case this option has been
;; already be upgraded/reseted by `ecb-upgrade-not-compatible-options' (see
;; above).
;;
;; So the calling sequence of these three functions must be:
;; 1. `ecb-check-not-compatible-options'
;; 2. `ecb-upgrade-not-compatible-options'
;;    `ecb-upgrade-renamed-options' or vice versa.
;; 
;; There are also two interactive commands:
;; - `ecb-display-upgraded-options' displays a temp. buffer with all upgraded
;;   or reseted ECB-options with their old and new values.
;; - `ecb-upgrade-options': Does all necessary beginning with the
;;   incompatibility-check and ending with the display of the options.
;;
;; What must an ECB-maintainer do:
;;
;; + If he wants only a save and correct ECB-start with the new release:
;;   NOTHING
;; + If he wants to preserve best possible the customized values of now
;;   type-incompatible and/or renamed options:
;;   - Adding entries to the alist `ecb-upgradable-option-alist' and
;;   - Defining suitable transforming-functions for every of these options.
;;   See the comment of `ecb-upgradable-option-alist'.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.


;;; Code

(eval-when-compile
  (require 'silentcomp))

;; IMPORTANT: The version-number is auto-frobbed from the Makefile. Do not
;; change it here!
;; (defconst ecb-version "2.33beta1"
;;   "Current ECB version.")

(defconst ecb-version "2.40"
  "Current ECB version.")

(eval-when-compile
  (require 'cl))

(require 'ecb-util)

(silentcomp-defun widget-convert)
(silentcomp-defun ecb-find-optionsym-for-tree-buffer-name)

;; -------------------------------------------------------------------------
;; define in this defconst all important NEWS which a user should know after
;; upgrading to the new version.
;; -------------------------------------------------------------------------

;; Each NEWS-string should be a one-liner shorter than 70 chars
(defconst ecb-upgrade-news
  '(("2.40" . ("ECB now requires full CEDET being installed (at least 1.0pre6)."
               "More user-responsible buffer-parsing based on the idle-mechanism of semantic."
               "ECB is able to work with indirect buffers it the base-buffer is filebased."
               "The history can now be bucketized, see new `ecb-history-make-buckets'."
               "New faces `ecb-history-bucket-node-face', `ecb-history-dead-buffer-face', `ecb-history-indirect-buffer-face'."
               "Sticky parwnt-node for all ECB-windows of type tree-buffer."
               "New support for Git and Monotone as version-control systems."
               "New command `ecb-goto-window-edit-by-smart-selection'."
               "New command `ecb-goto-window-ecb-by-smart-selection'."
               "New option `ecb-ignore-pop-up-frames'."
               "Full compatibility with Emacs 22 and 23 - see NEWS file."
               "Better compatibility with CEDET 1.0preX and semantic 2.0preX - see NEWS file."
               "The internal download-feature of ECB has been completely disabled."
               ))
    ("2.32" . ("New ECB-window (tree-buffer) for the semantic-analyser."
               "New ECB-window for displaying definition for current symbol at point."
               "Up- and down-arrow are now also smart in the tree-buffers."
               "Much better maximizing/minimizing of the ecb-tree-windows."
               "New option `ecb-maximize-next-after-maximized-select'."
               "`ecb-truncate-lines' has been renamed to `ecb-tree-truncate-lines'"))
    ("2.30" . ("Support for displaying the VC-state in the tree-buffers; see NEWS." ;;
               "ECB is now capable of handling remote paths (e.g. TRAMP-paths)"
               "Precisely expanding of current node via popup-menu of the methods-buffer."
               "Time consuming tasks are performed stealthy; see `ecb-stealthy-tasks-delay'"))
    ("2.27" . ("Much saver resizing-mechanism for permanent compile-window. See NEWS."))
    ("2.26" . ("Some regexp-options has been changed to regexp-list-options. See NEWS."
               "New option `ecb-history-exclude-file-regexps'."
               "`ecb-expand-methods-nodes' works for non-semantic-buffers too."
               "Readonly-sourcefiles are display in a different face."))
    ("2.25" . ("`ecb-sort-history-items' has been renamed to `ecb-history-sort-method'"
               "New options `ecb-sources-sort-ignore-case' and `ecb-history-sort-ignore-case'"
               "New icons for parent-display in the Methods-buffer"))
    ("2.24" . ("New \"current-type\"-filter for the Methods-buffer"
               "Now directories are prescanned for emptyness"))
    ("2.23" . ("New cedet1.0beta2 is supported."
               "Distinction between functions and function-prototypes in the Methods-buffer"
               "The command `ecb-toggle-layout' now has a prefix-argument"
               "Default tag-filters for certain files which are applied automatically"
               "Double-clicking the mouse-1-button now works with integrated speedbar"
               "A new hook `ecb-speedbar-before-activate-hook'"))
    ("2.22" . ("New nifty feature for filtering the tags displayed in the Methods-buffer"
               "Much smarter mechanism to highlight the current tag in the methods-buffer"
               "New option `ecb-auto-expand-tag-tree-collapse-other'."
               "Fixed a bug preventing the native Windows-port of XEmacs from working."))
    ("2.21" . ("Advice for `balance-windows' so only the edit-windows are balanced."
               "Gnus, BBDB, VM, Xrefactory etc. work even when ECB-windows are visible."
               "Commands using `Electric-pop-up-window' now work correctly with ECB."
               "Fixed some annoying bugs and one fatal bug."))
    ("2.20" . ("Fixed a bug preventing tree-buffers with expand-symbol \'before\' to work"
               "'ecb-major-modes-\(de)activate' replaced by `ecb-major-modes-show-or-hide'"
               "New keybinding for the online-help: [C-c . h]"
               "The edit-area can be splitted in more than 2 windows."
               "`ecb-other-window-jump-behavior' renamed in `ecb-other-window-behavior'"
               "New option `ecb-maximize-ecb-window-after-selection'"
               "popup-menus of the tree-buffers can be used with the tmm-library"
               "New option `ecb-change-layout-preserves-compwin-state'"
               "`delete-window' and `delete-other-windows' handle the compile-window"
               "Support of the default modeline-mechanisms for deleting other windows"))
    ("2.11" . ("Using semanticdb to jump to type-tags defined in other files"))
    ("2.01" . ("Support for semantic 2.0"
               "The tree-buffers can be displayed graphically with images"
               "Popup-menus of the tree-buffers support submenus"
               "The sources- and the history-buffer can be filtered"
               "Ediff runs per default in the ECB-frame"))
    ("1.96" . ("ECB can work together with the window-managers escreen and winring"
               "Much better support of the ECB-compile-window"))))



;; ----------------------------------------------------------------------
;; define in this defconst all options which should be upgraded
;; ----------------------------------------------------------------------

(defconst ecb-upgradable-option-alist
  '((ecb-compile-window-temporally-enlarge . (ecb-compile-window-temporally-enlarge
                                              ecb-upgrade-compile-window-temporally-enlarge))
    ;;(ecb-window-sync . (ecb-window-sync ecb-upgrade-window-sync))
    (ecb-hide-ecb-windows-hook . (ecb-hide-ecb-windows-before-hook identity))
    (ecb-show-ecb-windows-hook . (ecb-show-ecb-windows-before-hook identity))
    (ecb-layout-nr . (ecb-layout-name ecb-upgrade-layout-nr))
    (ecb-toggle-layout-sequence . (ecb-toggle-layout-sequence
                                   ecb-upgrade-toggle-layout-sequence))
    (ecb-cache-directory-contents . (ecb-cache-directory-contents
                                     ecb-upgrade-cache-directory-contents))
    (ecb-layout-always-operate-in-edit-window . (ecb-layout-always-operate-in-edit-window
                                                 ecb-upgrade-alway-operate-in-edit-window))
    (ecb-truncate-lines . (ecb-tree-truncate-lines ecb-upgrade-truncate-lines))
    (ecb-mode-line-prefixes . (ecb-mode-line-prefixes
                               ecb-upgrade-mode-line-prefixes))
    (ecb-mode-line-data . (ecb-mode-line-data
                               ecb-upgrade-mode-line-data))
    (ecb-use-speedbar-for-directories . (ecb-use-speedbar-instead-native-tree-buffer
                                         ecb-upgrade-use-speedbar-for-directories))

    (ecb-directories-menu-user-extension . (ecb-directories-menu-user-extension
                                            ecb-upgrade-directories-menu-ext))
    (ecb-sources-menu-user-extension . (ecb-sources-menu-user-extension
                                        ecb-upgrade-sources-menu-ext))
    (ecb-methods-menu-user-extension . (ecb-methods-menu-user-extension
                                        ecb-upgrade-methods-menu-ext))
    (ecb-history-menu-user-extension . (ecb-history-menu-user-extension
                                        ecb-upgrade-history-menu-ext))
    (ecb-bucket-token-display . (ecb-bucket-node-display identity))
    (ecb-auto-expand-token-tree . (ecb-auto-expand-tag-tree identity))
    (ecb-font-lock-tokens . (ecb-font-lock-tags identity))
    (ecb-token-jump-sets-mark . (ecb-tag-jump-sets-mark identity))
    (ecb-token-display-function . (ecb-tag-display-function ecb-upgrade-token-display-function))
    (ecb-type-token-display . (ecb-type-tag-display ecb-upgrade-type-token-display))
    (ecb-post-process-semantic-tokenlist . (ecb-post-process-semantic-taglist
                                            ecb-upgrade-post-process-semantic-tokenlist))
    (ecb-show-only-positioned-tokens . (ecb-show-only-positioned-tags identity))
    (ecb-show-tokens . (ecb-show-tags ecb-upgrade-show-tags))
    (ecb-show-tags . (ecb-show-tags ecb-upgrade-show-tags))
    (ecb-highlight-token-with-point . (ecb-highlight-tag-with-point identity))
    (ecb-highlight-token-with-point-delay . (ecb-highlight-tag-with-point-delay identity))
    (ecb-token-visit-post-actions . (ecb-tag-visit-post-actions
                                     ecb-upgrade-token-visit-post-actions))
    (ecb-token-header-face . (ecb-tag-header-face
                              ecb-upgrade-token-header-face))
    (ecb-post-process-semantic-taglist . (ecb-post-process-semantic-taglist
                                          ecb-upgrade-post-process-semantic-taglist))
    (ecb-primary-mouse-jump-destination . (ecb-mouse-click-destination identity))
    (ecb-split-edit-window . (ecb-split-edit-window-after-start ecb-upgrade-split-edit-window))
    (ecb-sort-history-items . (ecb-history-sort-method ecb-upgrade-sort-history-items))
    (ecb-other-window-jump-behavior . (ecb-other-window-behavior ecb-upgrade-other-window-jump-behavior))
    (ecb-excluded-directories-regexp . (ecb-excluded-directories-regexps
                                        ecb-upgrade-excluded-directories-regexp))
    (ecb-source-file-regexps . (ecb-source-file-regexps
                                ecb-upgrade-source-file-regexps))
    (ecb-exclude-parents-regexp . (ecb-exclude-parents-regexps
                                   ecb-upgrade-exclude-parents-regexp))
    (ecb-auto-expand-tag-tree-collapse-other . (ecb-auto-expand-tag-tree-collapse-other
                                                ecb-upgrade-auto-expand-tag-tree-collapse-other))
    (ecb-prescan-directories-for-emptyness . (ecb-prescan-directories-for-emptyness
                                              ecb-upgrade-prescan-directories-for-emptyness))
    (ecb-sources-perform-read-only-check . (ecb-sources-perform-read-only-check
                                            ecb-upgrade-sources-perform-read-only-check))
    (ecb-vc-enable-support . (ecb-vc-enable-support
                              ecb-upgrade-vc-enable-support))
    (ecb-tree-image-icons-directories . (ecb-tree-image-icons-directories
                                         ecb-upgrade-tree-image-icons-directories))
    (ecb-tree-RET-selects-edit-window . (ecb-tree-do-not-leave-window-after-select
                                         ecb-upgrade-tree-RET-selects-edit-window))
    (ecb-grep-find-function . (ecb-grep-recursive-function identity))
    )
  "Alist of all options which should be upgraded for current ECB-version.
There are several reasons why an option should be contained in this alist:
a) An old option has just be renamed in current-ECB version but has still the
   same type of value so the new option should get the value of the old one.
b) An old option has changed its type and we try to transform the old-typed
   value to the new type.
c) An old option has be renamed and also changed its type so we try to
   transform the value of the old option to the type of the new option and set
   the new option to this transformed value.

If an old option has changed its type and we can not savely transform the
old-value to the new type then this option should NOT be contained in this
alist! Such an option is auto. reset to the current default-value by
`ecb-upgrade-not-compatible-options'!

Every element of this alist has the following form:
The car is the old option symbol and the cdr is a 2-element-list with:
1. elem: The new option symbol \(can be equal with the old option symbol, see
   b) above)
2. elem: A function which converts the value of the old option to the new
   option. If the type of the options is identical \(i.e. only the option name
   has been changed, see a) above) then this function should be `identity'
   otherwise a function which gets one argument \(the value of the old option)
   and returns either a corresponding value for the new option with the new
   correct type or the symbol 'ecb-no-upgrade-conversion if no correct
   conversion can be performed! Maybe the function `ecb-option-get-value' can
   be helpful within such a transforming-function.")

;; ----------------------------------------------------------------------
;; define here all necessary upgrade functions
;; ----------------------------------------------------------------------

;; upgrading ecb-compile-window-temporally-enlarge
(defun ecb-upgrade-compile-window-temporally-enlarge (old-val)
  (case old-val
    ((t after-compilation) 'after-display)
    ((nil) nil)
    ((after-selection both) old-val)
    (otherwise 'ecb-no-upgrade-conversion)))

;; upgrading ecb-window-sync
(defun ecb-upgrade-window-sync (old-val)
  (if (equal old-val t)
      (ecb-option-get-value 'ecb-window-sync 'standard-value)
    nil))

;; upgrading old layout-numbers (ECB <= 1.80) to new layout-names (ECB
;; >= 1.90)
(defun ecb-upgrade-layout-nr2name (number)
  (let ((number-name-alist '((nil . "left8")
                             (0 . "left1")
                             (1 . "left2")
                             (2 . "left3")
                             (3 . "left4")
                             (4 . "left5")
                             (5 . "right1")
                             (6 . "left6")
                             (7 . "top1")
                             (8 . "left7")
                             (9 . "left8")
                             (10 . "top2")
                             (11 . "left9")
                             (12 . "left10")
                             (13 . "left11")
                             (14 . "left12")
                             (15 . "left13")
                             (16 . "left14")
                             (17 . "left15")
                             (18 . "leftright1")
                             (19 . "leftright2")
                             (20 . "speedbar1"))))
    (cdr (assoc number number-name-alist))))

(defun ecb-upgrade-layout-nr (old-val)
  (let ((name (ecb-upgrade-layout-nr2name old-val)))
    (if (stringp name)
        name
      'ecb-no-upgrade-conversion)))

(defun ecb-upgrade-toggle-layout-sequence (old-val)
  (mapcar (function (lambda (elem)
                      (ecb-upgrade-layout-nr2name elem)))
          old-val))

(defun ecb-upgrade-use-speedbar-for-directories (old-val)
  (if old-val
      'dir))

(defun ecb-upgrade-major-modes-activate (old-val)
  (if (not (listp old-val))
      old-val
    (let ((l (copy-tree old-val)))
      (dolist (elem l)
        (if (and (consp elem)
                 (integerp (cdr elem)))
            (setcdr elem (ecb-upgrade-layout-nr2name (cdr elem)))))
      l)))

(defun ecb-upgrade-cache-directory-contents (old-val)
  (mapcar (function (lambda (elem)
                      (cons (nth 0 elem) (nth 1 elem))))
          old-val))

(defun ecb-upgrade-truncate-lines (old-val)
  (cond ((equal t old-val)
         '(ecb-directories-buffer-name
           ecb-sources-buffer-name
           ecb-methods-buffer-name
           ecb-history-buffer-name))
        ((equal nil old-val)
         nil)
        ((listp old-val)
         (let ((new-list nil))
           (if (nth 0 old-val)
               (setq new-list (cons 'ecb-directories-buffer-name new-list)))
           (if (nth 1 old-val)
               (setq new-list (cons 'ecb-sources-buffer-name new-list)))
           (if (nth 2 old-val)
               (setq new-list (cons 'ecb-methods-buffer-name new-list)))
           (if (nth 3 old-val)
               (setq new-list (cons 'ecb-history-buffer-name new-list)))
           new-list))
        (t
         '(ecb-directories-buffer-name
           ecb-sources-buffer-name
           ecb-methods-buffer-name
           ecb-history-buffer-name))))

(defun ecb-upgrade-alway-operate-in-edit-window (old-val)
  (let ((l (copy-tree old-val)))
    (setq l (delete 'switch-to-buffer-other-window l))
    l))

(defun ecb-upgrade-mode-line-prefixes (old-val)
  (list (cons 'ecb-directories-buffer-name
              (nth 0 old-val))
        (cons 'ecb-sources-buffer-name
              (nth 1 old-val))
        (cons 'ecb-methods-buffer-name
              (nth 2 old-val))
        (cons 'ecb-history-buffer-name
              (nth 3 old-val))))

(defun ecb-upgrade-mode-line-data (old-val)
  (list (cons 'ecb-directories-buffer-name
              (if (equal (nth 0 old-val) 'selected)
                  'sel-dir
                (nth 0 old-val)))
        (cons 'ecb-sources-buffer-name
              (if (equal (nth 1 old-val) 'selected)
                  'sel-dir
                (nth 1 old-val)))
        (cons 'ecb-methods-buffer-name
              (if (equal (nth 2 old-val) 'selected)
                  'sel-source
                (nth 2 old-val)))
        (cons 'ecb-history-buffer-name
              (nth 3 old-val))))

(defun ecb-upgrade-menu-extension (old-list)
  (mapcar (function (lambda (i)
                      (reverse i)))
          old-list))

(defun ecb-upgrade-directories-menu-ext (old-val)
  (append (ecb-upgrade-menu-extension old-val)
          (ecb-option-get-value 'ecb-directories-menu-user-extension
                                'standard-value)))

(defun ecb-upgrade-sources-menu-ext (old-val)
  (append (ecb-upgrade-menu-extension old-val)
          (ecb-option-get-value 'ecb-sources-menu-user-extension
                                'standard-value)))

(defun ecb-upgrade-methods-menu-ext (old-val)
  (append (ecb-upgrade-menu-extension old-val)
          (ecb-option-get-value 'ecb-methods-menu-user-extension
                                'standard-value)))

(defun ecb-upgrade-history-menu-ext (old-val)
  (append (ecb-upgrade-menu-extension old-val)
          (ecb-option-get-value 'ecb-history-menu-user-extension
                                'standard-value)))

(defun ecb-upgrade-token-display-function (old-val)
  (let ((l (copy-tree old-val))
        (mapping-list
         '((semantic-name-nonterminal                  . ecb--semantic-format-tag-name)
           (semantic-abbreviate-nonterminal            . ecb--semantic-format-tag-abbreviate)
           (semantic-summarize-nonterminal             . ecb--semantic-format-tag-summarize)
           (semantic-prototype-nonterminal             . ecb--semantic-format-tag-prototype)
           (semantic-concise-prototype-nonterminal     . ecb--semantic-format-tag-concise-prototype)
           (semantic-uml-abbreviate-nonterminal        . ecb--semantic-format-tag-uml-abbreviate)
           (semantic-uml-prototype-nonterminal         . ecb--semantic-format-tag-uml-prototype)
           (semantic-uml-concise-prototype-nonterminal . ecb--semantic-format-tag-uml-concise-prototype)
           (semantic-prin1-nonterminal                 . ecb--semantic-format-tag-prin1)
           (ecb-name-nonterminal                  . ecb-format-tag-name)
           (ecb-abbreviate-nonterminal            . ecb-format-tag-abbreviate)
           (ecb-summarize-nonterminal             . ecb-format-tag-summarize)
           (ecb-prototype-nonterminal             . ecb-format-tag-prototype)
           (ecb-concise-prototype-nonterminal     . ecb-format-tag-concise-prototype)
           (ecb-uml-abbreviate-nonterminal        . ecb-format-tag-uml-abbreviate)
           (ecb-uml-prototype-nonterminal         . ecb-format-tag-uml-prototype)
           (ecb-uml-concise-prototype-nonterminal . ecb-format-tag-uml-concise-prototype)
           (ecb-prin1-nonterminal                 . ecb-format-tag-prin1))))
    (mapc (function (lambda (e)
                      (if (assoc (cdr e) mapping-list)
                          (setcdr e (cdr (assoc (cdr e) mapping-list))))))
          l)
    l))


(defun ecb-upgrade-type-token-display (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((ecb-type-token-class-face . ecb-type-tag-class-face)
           (ecb-type-token-interface-face . ecb-type-tag-interface-face)
           (ecb-type-token-struct-face . ecb-type-tag-struct-face)
           (ecb-type-token-typedef-face . ecb-type-tag-typedef-face)
           (ecb-type-token-enum-face . ecb-type-tag-enum-face)
           (ecb-type-token-group-face . ecb-type-tag-group-face))))
    (mapc (function (lambda (e)
                      (dolist (l (cdr e))
                        (if (assoc (nth 2 l) mapping-list)
                            (ecb-set-elt l 2
                                         (cdr (assoc (nth 2 l) mapping-list)))))))
          val-copy)
    val-copy))

(defun ecb-upgrade-post-process-semantic-tokenlist (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((ecb-group-function-tokens-with-parents . ecb-group-function-tags-with-parents))))
    (mapc (function (lambda (e)
                      (if (assoc (cdr e) mapping-list)
                          (setcdr e (cdr (assoc (cdr e) mapping-list))))))
          val-copy)
    val-copy))

(defun ecb-upgrade-token-visit-post-actions (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((ecb-token-visit-highlight-token-header . ecb-tag-visit-highlight-tag-header)
           (ecb-token-visit-smart-token-start . ecb-tag-visit-smart-tag-start)
           (ecb-token-visit-recenter . ecb-tag-visit-recenter)
           (ecb-token-visit-recenter-top . ecb-tag-visit-recenter-top)
           (ecb-token-visit-goto-doc-start . ecb-tag-visit-goto-doc-start)
           (ecb-token-visit-narrow-token . ecb-tag-visit-narrow-tag))))
    (mapc (function (lambda (e)
                      (dotimes (i (length (cdr e)))
                        (if (assoc (nth i (cdr e)) mapping-list)
                            (ecb-set-elt (cdr e) i
                                         (cdr (assoc (nth i (cdr e))
                                                     mapping-list)))))))
          val-copy)
    val-copy))

(defun ecb-upgrade-token-header-face (old-val)
  (if (equal old-val 'ecb-token-header-face)
      'ecb-tag-header-face
    old-val))

(defun ecb-upgrade-post-process-semantic-taglist (old-val)
  (let ((l (copy-tree old-val)))
    (dolist (elem l)
      (if (cdr elem)
          (setcdr elem (list (cdr elem)))))
    l))

(defun ecb-upgrade-split-edit-window (old-val)
  (if (equal old-val t)
      'before-activation
    old-val))

(defun ecb-upgrade-other-window-jump-behavior (old-val)
  (if (equal old-val 'all)
      'all
    (ecb-option-get-value 'ecb-other-window-behavior
                          'standard-value)))

(defun ecb-upgrade-show-tags (old-val)
  (ecb-option-get-value 'ecb-show-tags
                        'standard-value))

(defun ecb-upgrade-sort-history-items (old-val)
  (if old-val ecb-sources-sort-method))

(defun ecb-upgrade-excluded-directories-regexp (old-val)
  (list old-val))

(defun ecb-upgrade-source-file-regexps (old-val)
  (let ((l (copy-tree old-val)))
    (dolist (elem l)
      (setcdr elem (list (list (cadr elem)) (list (caddr elem)))))
    l))
    
(defun ecb-upgrade-exclude-parents-regexp (old-val)
  (if old-val (list old-val)))

(defun ecb-upgrade-auto-expand-tag-tree-collapse-other (old-val)
  (if old-val
      'only-if-on-tag
    nil))

(defun ecb-upgrade-prescan-directories-for-emptyness (old-val)
  (if old-val 'unless-remote nil))

(defun ecb-upgrade-sources-perform-read-only-check (old-val)
  (if old-val 'unless-remote nil))
  
(defun ecb-upgrade-vc-enable-support (old-val)
  (if old-val 'unless-remote nil))

(defun ecb-upgrade-tree-image-icons-directories (old-val)
  (let ((l (copy-tree old-val)))
    (cons (nth 0 l)
          (delq nil (list (if (nth 1 l)
                              (cons 'ecb-directories-buffer-name
                                    (nth 1 l)))
                          (if (nth 2 l)
                              (cons 'ecb-sources-buffer-name
                                    (nth 2 l)))
                          (if (nth 3 l)
                              (cons 'ecb-methods-buffer-name
                                    (nth 3 l)))
                          (if (nth 4 l)
                              (cons 'ecb-history-buffer-name
                                    (nth 4 l))))))))

(defun ecb-upgrade-tree-RET-selects-edit-window (old-val)
  (delq nil (mapcar (lambda (b)
                      (and (not (ecb-member-of-symbol/value-list
                                 (symbol-value b)
                                 old-val))
                           b))
                    '(ecb-directories-buffer-name
                      ecb-sources-buffer-name
                      ecb-methods-buffer-name
                      ecb-history-buffer-name
                      ecb-analyse-buffer-name))))
    

;; ----------------------------------------------------------------------
;; internal functions. Don't change anything below this line
;; ----------------------------------------------------------------------

(defgroup ecb-upgrade-internal nil
  "Only Internal setting for the ECB upgrade-mechanism - no user-options!"
  :group 'ecb-general
  :prefix "ecb-")

(defcustom ecb-options-version ecb-version
  "*DO NOT CUSTOMIZE THIS VALUE - IT IS ONLY FOR INTERNAL USAGE!"
  :group 'ecb-upgrade-internal
  :type 'string)

(defun ecb-custom-file-writeable-p ()
  "Returns not nil if and only if the custom-file is writable for ECB, which
means it is neither a bytecompiled-file nor a read-only-file."
  (let ((file (ecb-custom-file)))
    (and file
         (not (equal (file-name-extension file) "elc"))
         (file-writable-p file))))

(defun ecb-customize-save-variable (option value)
  ;; because the adviced version of `custom-save-all' do only all the special
  ;; needed things if `ecb-minor-mode' is on we must temporally set here this
  ;; variable to not nil because at that time this function is called this
  ;; variable is maybe still nil.
  (let ((ecb-minor-mode t))
    (if (ecb-custom-file-writeable-p)
        (customize-save-variable option value)
      (customize-set-variable option value))))

(defun ecb-customize-set-variable (option value)
  (customize-set-variable option value))

(defun ecb-option-set-default (option)
  "Save the ECB-option OPTION with current default value."
  (ecb-customize-set-variable option
                              (ecb-option-get-value option 'standard-value)))

(defun ecb-option-upgrade (old-option)
  "Upgrade the old ECB-option OLD-OPTION if the following conditions are ALL
true:
1. OLD-OPTION is the key of an element of `ecb-upgradable-option-alist'
2. 'saved-value of OLD-OPTION is not nil
3. Either
   + the new-option from `ecb-upgradable-option-alist' has the same name
     as OLD-OPTION and
   + the type of the value of OLD-OPTION is not compatible with the current
     type of OLD-OPTION \(this prevents from doing an upgrade twice!)
   or
   + OLD-OPTION is not a valid option in current ECB and
   + The related new-option `ecb-upgradable-option-alist' is not already
     customized, i.e. the 'saved-value of new-option is nil.

If all conditions are true then the value of OLD-OPTION is transformed by the
transforming-function of the related element of `ecb-upgradable-option-alist'
to the correct new type and then the related new option is saved with this new
value.

Return nil if no upgrade is necessary because at least one of the conditions
above is not true. Returns the transformed value of OLD-OPTION or
'ecb-no-upgrade-conversion in form of a list, to distinguish a transformed
value nil from the nil-result which indicates that no upgrade was necessary
\(see above). This means the \"real\" new value is the car of this
result-list!"
  (let ((upgrade-elem (cdr (assoc old-option ecb-upgradable-option-alist)))
        new-value)
    ;; check if an upgrade is necessary or allowed
    (when (and upgrade-elem
               (or (and (equal old-option (nth 0 upgrade-elem))
                        (not (ecb-option-compatible-p old-option)))
                   (and (not (member old-option ecb-all-options))
                        (null (get (nth 0 upgrade-elem) 'saved-value))))
               (get old-option 'saved-value))
      ;; try to transform the old-value in the new type.
      (setq new-value
            (condition-case nil
                (funcall (nth 1 upgrade-elem)
                         (ecb-option-get-value old-option 'saved-value))
              (error 'ecb-no-upgrade-conversion)))
      (when (not (equal new-value 'ecb-no-upgrade-conversion))
        ;; the old-value has been transformed successfully into the new type
        ;; so we can save it.
        (ecb-customize-set-variable (nth 0 upgrade-elem) new-value))
      ;; we return the value of the transforming-function even if it is
      ;; 'ecb-no-upgrade-conversion!
      (list new-value))))

(defun ecb-option-compatible-p (option)
  "Return not nil only if the type of the value of OPTION is compatible with
its current defcustom-definition."
  (require 'cus-edit)
  (widget-apply (widget-convert (get option 'custom-type))
                :match (symbol-value option)))

(defvar ecb-old-ecb-version nil
  "Only not nil if ECB has upgraded the options to a newer options-version
after an ECB-upgrade.")

(defun ecb-options-version=ecb-version-p ()
  "Return not nil if the saved value of `ecb-options-version' is equal to
`ecb-version'."
  (equal (ecb-option-get-value 'ecb-options-version 'saved-value)
         ecb-version))

(defun ecb-store-current-options-version ()
  (when (not (ecb-options-version=ecb-version-p))
    (setq ecb-old-ecb-version (ecb-option-get-value 'ecb-options-version
                                                    'saved-value))
    (ecb-customize-save-variable 'ecb-options-version ecb-version)))
  

(defvar ecb-not-compatible-options nil
  "This variable is only set by `ecb-check-not-compatible-options'! It is an
alist with car is the symbol of an incompatible option and the cdr is the not
compatible value of this option.
This option is evaluated by `ecb-upgrade-not-compatible-options' and
`ecb-display-upgraded-options'.")


(defvar ecb-all-options nil)

(defun ecb-get-all-ecb-options ()
  (or ecb-all-options
      (mapatoms
       (lambda (symbol)
         (when (and (save-match-data (string-match "ecb-" (symbol-name symbol)))
                    (get symbol 'custom-type))
           (setq ecb-all-options (cons symbol ecb-all-options)))))))

(defun ecb-check-not-compatible-options ()
  "Check for all ECB-options if their current value is compatible to the
defined type. If not store it in `ecb-not-compatible-options'."
  (setq ecb-not-compatible-options nil)

  ;; get all options of ECB
;;   (let ((ecb-options nil))
  (ecb-get-all-ecb-options)
  
  ;; check if all current values of ECB options match their types. Add not
  ;; matching options to `ecb-not-compatible-options'.
  (dolist (option ecb-all-options)
    (require 'cus-edit)
    (unless (ecb-option-compatible-p option)
      (setq ecb-not-compatible-options
            (cons (cons option
                        (symbol-value option))
                  ecb-not-compatible-options)))))

(defun ecb-upgrade-not-compatible-options ()
  "Upgrade all not anymore compatible options of `ecb-not-compatible-options'.
If such an option is contained in `ecb-upgradable-option-alist' then try to
perform a special upgrade with `ecb-option-upgrade'. If no special upgrade is
done then the option is reset to the default-value of current ECB-version."
  ;; For every not compatible option perform an upgrade
  (let ((is-not-a-downgrade
         (not (ecb-package-version-list<
               (ecb-package-version-str2list ecb-version)
               (ecb-package-version-str2list ecb-options-version)))))
    (dolist (option ecb-not-compatible-options)
      ;; if the incompatible option is not upgraded by `ecb-option-upgrade'
      ;; then we reset it to the standard-value of current ECB-version. If we
      ;; make a downgrade we always reset to the default!
      (let ((upgrade-result
             (if is-not-a-downgrade (ecb-option-upgrade (car option)))))
        (when (or (null upgrade-result) ;; no upgrade necessary or allowed
                  ;; the upgrade has been tried but has failed.
                  (equal (car upgrade-result) 'ecb-no-upgrade-conversion))
          (ecb-option-set-default (car option)))))))
    

(defvar ecb-renamed-options nil)

(defun ecb-upgrade-renamed-options ()
  "Upgrade all renamed options of `ecb-upgradable-option-alist' and store
every option in `ecb-renamed-options' if at least an upgrade was tried \(see
`ecb-option-upgrade').

Note: This function upgrades only the renamed but not the incompatible options
\(i.e. only the type but not the name of the option has changed) of
`ecb-upgradable-option-alist' because the latter ones will be upgraded by
`ecb-upgrade-not-compatible-options'!"
  (setq ecb-renamed-options nil)
  (when (not (ecb-package-version-list<
              (ecb-package-version-str2list ecb-version)
              (ecb-package-version-str2list ecb-options-version)))
    (ecb-get-all-ecb-options)
    (dolist (option ecb-upgradable-option-alist)
      ;; perform only an upgrade if the option is not contained in
      ;; `ecb-not-compatible-options' too because then ECB has auto.
      ;; recognized that this option is not compatible and the upgrade (or
      ;; reset) is performed by `ecb-upgrade-not-compatible-options'!
      (when (not (assoc (car option) ecb-not-compatible-options))
        (let ((new-value-list (ecb-option-upgrade (car option))))
          ;; if an upgrade was tried then store the option in
          ;; `ecb-renamed-options'.
          (when (and new-value-list
                     (not (equal (car new-value-list)
                                 'ecb-no-upgrade-conversion)))
            (setq ecb-renamed-options
                  (cons (list (car option)
                              (ecb-option-get-value (car option) 'saved-value)
                              (car (cdr option))
                              (car new-value-list))
                        ecb-renamed-options))))))))

(require 'wid-edit)
(silentcomp-defvar widget-button-keymap)
(silentcomp-defvar widget-keymap)

(defvar ecb-upgrade-button-keymap
  (let (parent-keymap mouse-button1 keymap)
    (if ecb-running-xemacs
        (setq parent-keymap widget-button-keymap
              mouse-button1 [button1])
      (setq parent-keymap widget-keymap
            mouse-button1 [down-mouse-1]))
    (setq keymap (copy-keymap parent-keymap))
    (define-key keymap mouse-button1 #'widget-button-click)
    keymap)
  "Keymap used inside buttons.")


(defun ecb-not-compatible-or-renamed-options-detected ()
  (or ecb-not-compatible-options ecb-renamed-options))

(defun ecb-upgrade-make-copy-of-custom-file ()
  "Make a backup of the file returned by `ecb-custom-file' in the same directory."
  (when (ecb-custom-file-writeable-p)
    (let* ((file (ecb-custom-file))
           (backup-file-base (format "%s.before_ecb_%s" file ecb-version))
           (backup-file backup-file-base)
           (i 0))
      (while (file-exists-p backup-file)
        (setq i (1+ i))
        (setq backup-file (format "%s__%d" backup-file-base i)))
      (copy-file file backup-file))))
      

(defun ecb-display-upgraded-options ()
  "Display a information-buffer which options have been upgraded or reset.
Offers two buttons where the user can decide if the upgraded options should
also being saved by ECB for future settings or if the buffer should be
killed.

If saving is possible this command display where the options would be saved.
It is that file Emacs uses to save customize-settings. This file is
\"computed\" from the settings in `custom-file' and `user-init-file' \(see the
documentation of these variables).

ECB automatically makes a backup-file of that file which will be modified by
storing the upgraded rsp. renamed ECB-options. This backup file gets a unique
name by adding a suffix \".before_ecb_<version>\" to the name of the modified
file. If such a file already exists ECB adds a unique number to the end of the
filename to make the filename unique. This is a safety mechanism if something
fails during storing the upgraded options, so you never lose the contents of
your customization-file!"
  (interactive)
  (if (ecb-not-compatible-or-renamed-options-detected)
      (progn
        (with-current-buffer (get-buffer-create "*ECB upgraded options*")
          (switch-to-buffer (current-buffer))
          (kill-all-local-variables)
          (let ((inhibit-read-only t))
            (erase-buffer))
          (if (not (ecb-custom-file-writeable-p))
              (progn
                (widget-insert "Emacs can not save the upgraded incompatible options (s.b.) because that file\n")
                (widget-insert "specified for storing all customizations (see documentation of the option\n")
                (widget-insert "`custom-file') because the file")
                (widget-insert (if (ecb-custom-file)
                                   (concat (ecb-custom-file) " is not writeable by Emacs!")
                                 " does either not exist or Emacs has been\nstarted with -q (in the latter case Emacs prevents from writing in the\ncustomizations-file)!\n"))
                (widget-insert "\nPlease restart Emacs with a writeable custom- or init-file or without -q\nso the new option-values can be stored!\n\n"))
            (when (not (get 'ecb-display-upgraded-options
                            'ecb-upgrades-saved))
              (widget-insert (format "Click on [Save] to save all changed options (s.b.) into %s.\n"
                                     (ecb-custom-file)))
              (widget-insert (format "This makes a backup of this file uniquely named with a suffix .before_ecb_%s.\n\n"
                                     ecb-version))))
          (widget-insert "Click on [Close] to kill this buffer (do this also after clicking [Save]).\n\n")
          (when ecb-not-compatible-options
            (widget-insert "The values of the following options are incompatible with current type.\nECB has tried to transform the old-value to the new type. In cases where\nthis was not possible ECB has reset to the current default-value.")
            (widget-insert "\n\n"))
          (dolist (option ecb-not-compatible-options)
            (let ((option-name (symbol-name (car option)))
                  (old-value (cdr option))
                  (new-value (symbol-value (car option))))
              (widget-insert (concat "+ Option:   " option-name))
              (widget-insert "\n")
              (widget-insert (concat "  Old value: "
                                     (if (and (not (equal old-value nil))
                                              (not (equal old-value t))
                                              (or (symbolp old-value)
                                                  (listp old-value)))
                                         "'")
                                     (prin1-to-string old-value)))
              (widget-insert "\n")
              (widget-insert (concat "  New value: "
                                     (if (and (not (equal new-value nil))
                                              (not (equal new-value t))
                                              (or (symbolp new-value)
                                                  (listp new-value)))
                                         "'")
                                     (prin1-to-string new-value)))
              (widget-insert "\n\n")))
          (when ecb-renamed-options
            (widget-insert "The following options are not longer valid and have now new names. ECB has\ntried to transform the old value to the new option. In cases where this\nwas not possible the current default value is active!")
            (widget-insert "\n\n"))
          (dolist (option ecb-renamed-options)
            (let ((old-option-name (symbol-name (nth 0 option)))
                  (old-value (nth 1 option))
                  (new-option-name (symbol-name (nth 2 option)))
                  (new-value (nth 3 option)))
              (widget-insert (concat "+ Old option: " old-option-name))
              (widget-insert "\n")
              (widget-insert (concat "  Old value:  "
                                     (if (and (not (equal old-value nil))
                                              (not (equal old-value t))
                                              (or (symbolp old-value)
                                                  (listp old-value)))
                                         "'")
                                     (prin1-to-string old-value)))
              (widget-insert "\n")
              (widget-insert (concat "  New option: " new-option-name))
              (widget-insert "\n")
              (widget-insert (concat "  New value:  "
                                     (if (equal new-value 'ecb-no-upgrade-conversion)
                                         ;; we print the new default value.
                                         (prin1-to-string (symbol-value (nth 2 option)))
                                       (concat (if (and (not (equal new-value nil))
                                                        (not (equal new-value t))
                                                        (or (symbolp new-value)
                                                            (listp new-value)))
                                                   "'")
                                               (prin1-to-string new-value)))))
              (if (equal new-value 'ecb-no-upgrade-conversion)
                  (widget-insert "\n  (The old value couldn't be transformed - this is the current default!)"))
              (widget-insert "\n\n")))
          (widget-insert "If the new values are not what you want please re-customize!")
          (widget-insert "\n\n")
          (widget-insert "For a list of the most important NEWS call `ecb-display-news-for-upgrade'!\n\n")
          (widget-insert "\n")
          (when (ecb-custom-file-writeable-p)
            (when (not (get 'ecb-display-upgraded-options
                            'ecb-upgrades-saved))
              ;; Insert the Save button
              (widget-create 'push-button
                             :button-keymap ecb-upgrade-button-keymap ; XEmacs
                             :keymap ecb-upgrade-button-keymap ; Emacs
                             :notify (lambda (&rest ignore)
                                       (if (get 'ecb-display-upgraded-options
                                                'ecb-upgrades-saved)
                                           (ecb-info-message "Upgraded options are already saved!")
                                         (ecb-upgrade-make-copy-of-custom-file)
                                         (dolist (option ecb-not-compatible-options)
                                           (ecb-customize-save-variable
                                            (car option) (symbol-value (car option))))
                                         (dolist (option ecb-renamed-options)
                                           (ecb-customize-save-variable
                                            (nth 2 option)
                                            (symbol-value (nth 2 option))))
                                         ;; store the information that the
                                         ;; upgradings have already been saved now
                                         (put 'ecb-display-upgraded-options
                                              'ecb-upgrades-saved t)
                                         (ecb-store-current-options-version)
                                         (ecb-info-message "Upgraded options saved!")))
                             "Save")
              (widget-insert " ")))
          ;; Insert the Cancel button
          (widget-create 'push-button
                         :button-keymap ecb-upgrade-button-keymap ; XEmacs
                         :keymap ecb-upgrade-button-keymap ; Emacs
                         :notify (lambda (&rest ignore)
                                   (kill-buffer (current-buffer)))
                         "Close")
          (widget-setup)
          (goto-char (point-min)))
        t)
    ;; now we display only the choice to save the ecb-options-version but only
    ;; if ecb-options-version != ecb-version and (either the command is called
    ;; interactively or first-time called by program)
    (when (and (or (interactive-p)
                   (not (get 'ecb-display-upgraded-options
                         'ecb-options-version-save-displayed)))
               (not (ecb-options-version=ecb-version-p)))
      (put 'ecb-display-upgraded-options 'ecb-options-version-save-displayed t)
      (with-current-buffer (get-buffer-create "*ECB upgraded options*")
        (switch-to-buffer (current-buffer))
        (kill-all-local-variables)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (widget-insert "There are no incompatible or renamed options. Your settings are correct.\n")
        (widget-insert (format "But ECB must store that the ecb-settings are uptodate with %s.\n\n"
                               ecb-version))
        (if (not (ecb-custom-file-writeable-p))
            (progn
              (widget-insert (format "Emacs can not save the value of `ecb-options-version' (%s) in that file\n" ecb-options-version))
              (widget-insert "specified for storing all customizations (see documentation of the option\n")
              (widget-insert "`custom-file') because the file")
              (widget-insert (if (ecb-custom-file)
                                 (concat (ecb-custom-file) " is not writeable by Emacs!")
                               " does either not exist or Emacs has been\nstarted with -q (in the latter case Emacs prevents from writing in the\ncustomizations-file)!\n"))
              (widget-insert "\nPlease restart Emacs with a writeable custom- or init-file or without -q\nso the value of `ecb-options-version' (s.a.) can be stored!\n\n"))
          (widget-insert (format "Click on [Save] to save `ecb-options-version' (%s) into %s.\n"
                                 ecb-options-version (ecb-custom-file)))
          (widget-insert (format "This makes a backup of this file unique named with a suffix .before_ecb_%s.\n\n"
                                 ecb-version)))
        (widget-insert "Click on [Close] to kill this buffer (do this also after clicking [Save]).\n\n")
        (widget-insert "For a list of the most important NEWS call `ecb-display-news-for-upgrade'!\n\n")
        (widget-insert "\n")
        (when (ecb-custom-file-writeable-p)
          ;; Insert the Save button
          (widget-create 'push-button
                         :button-keymap ecb-upgrade-button-keymap ; XEmacs
                         :keymap ecb-upgrade-button-keymap ; Emacs
                         :notify (lambda (&rest ignore)
                                   (ecb-upgrade-make-copy-of-custom-file)
                                   (ecb-store-current-options-version)
                                   (ecb-info-message "ecb-options-version saved!"))
                         "Save")
          (widget-insert " "))
        ;; Insert the Close button
        (widget-create 'push-button
                       :button-keymap ecb-upgrade-button-keymap ; XEmacs
                       :keymap ecb-upgrade-button-keymap ; Emacs
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer)))
                       "Close")
        (widget-setup)
        (goto-char (point-min))))
    nil))

(defun ecb-display-news-for-upgrade (&optional full-news)
  "Display the most important NEWS after an ECB-upgrade.
If you call this function but no ECB-upgrade has been performed before
starting ECB then nothing is display unless FULL-NEWS is not nil.

If FULL-NEWS is not nil then the NEWS-file is displayed in another window."
  (interactive "P")
  (if full-news
      (find-file-other-window (concat ecb-ecb-dir "NEWS"))
    (if (and ecb-old-ecb-version
             (or (not (get 'ecb-display-news-for-upgrade
                           'ecb-news-for-upgrade-displayed))
                 (interactive-p)))
        (progn
          (with-output-to-temp-buffer "*News for the new ECB-version*"
            (princ (format "You have upgraded ECB from version %s to %s.\n\n"
                           ecb-old-ecb-version ecb-version))
            (princ "Here are the most important NEWS:\n\n")
            (mapc (function (lambda (version)
                              (if (ecb-package-version-list<
                                   (ecb-package-version-str2list ecb-old-ecb-version)
                                   (ecb-package-version-str2list (car version)))
                                  (dolist (news (cdr version))
                                    (princ (concat "* " news "\n"))))))
                  ecb-upgrade-news)
            (princ "\nFor more details see the attached NEWS-file."))
          ;; We want this being displayed only once
          (put 'ecb-display-news-for-upgrade 'ecb-news-for-upgrade-displayed t))
      (message "There are no NEWS to display."))))
    
  
(defun ecb-upgrade-options ()
  "Check for all ECB-options if the current value is compatible to the type.
If not upgrade it to the new type or reset it to the default-value of current
ECB. Try also to upgrade renamed options. Displays all upgraded or reset
options with their old \(before the upgrade/reset) and new values."
  (interactive)
  (ecb-check-not-compatible-options)
  (ecb-upgrade-not-compatible-options)
  (ecb-upgrade-renamed-options)
  (ecb-display-upgraded-options))

;; ----------------------------------------------------------------------
;; all needs for the requirements check
;; ----------------------------------------------------------------------

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should set this to pre6
(defconst ecb-required-cedet-version-min '(1 0 2 6))
(defconst ecb-required-cedet-version-max '(1 0 4 9))

(defvar ecb-all-requirements-available nil)

(defun ecb-check-requirements ()
  "Ensure that if all requirements of ECB are fulfilled.

Currently this is a check if the right `cedet-version is loaded."
  ;; we do not support (X)Emacs 18, 19 or 20!
  (when ecb-running-unsupported-emacs
    (ecb-error "Sorry, but ECB requires an (X)Emacs-version >= 21!"))

  (when ecb-regular-xemacs-package-p
    (ecb-error "Sorry, but ECB is currently not runnable as XEmacs-package. Install \"by hand\"."))

  (when (and (or (not (boundp 'ecb-version-check)) ecb-version-check)
             (not ecb-all-requirements-available))
    (let ((cedet-required-version-str-min (ecb-package-version-list2str
                                           ecb-required-cedet-version-min))
          (cedet-required-version-str-max (ecb-package-version-list2str
                                           ecb-required-cedet-version-max))
          (version-error nil))
      ;; check if vedet-version is correct
      (when (or (not (boundp 'cedet-version))
                (ecb-package-version-list<
                 (ecb-package-version-str2list cedet-version)
                 ecb-required-cedet-version-min)
                (ecb-package-version-list<
                 ecb-required-cedet-version-max
                 (ecb-package-version-str2list cedet-version)))
        (setq version-error (concat "cedet ["
                                    cedet-required-version-str-min
                                    ", "
                                    cedet-required-version-str-max
                                    "]")))
      (if (null version-error)
          ;; this is the only place where this variable is set
          (setq ecb-all-requirements-available t)
        (ecb-error "ECB can only be used with %s! Please install it and restart Emacs!"
                   version-error))))
  (when ecb-all-requirements-available
    (message "All requirements for ECB %s fulfilled - Enjoy it!" ecb-version)))


(defun ecb-package-version-str2list (ver-str)
  "Convert the version-str VER-STR to the internal version-list format with
the following elements of the version-list:
1. Major-version
2. Minor-version
3. 0 = alpha, 1 = beta, 2 = pre, 3 = nothing \(e.g. \"1.4\"), 4 = . \(e.g. \"1.4.3\"
4. Subversion after the alpha, beta, pre or .

Return nil if ver-str has not the required syntax:
<major>.<minor>\[.|pre|beta|alpha]\[<sub-stable/pre/beta/alpha-version>]"
  (let ((str ver-str))
    (save-match-data 
      (if (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)\\(pre\\|beta\\|alpha\\|\\.\\)?\\([0-9]+\\)?$" str)
          (list (string-to-number (match-string 1 str))
                (string-to-number (match-string 2 str))
                (if (ecb-string= (match-string 3 str) "alpha")
                    0
                  (if (ecb-string= (match-string 3 str) "beta")
                      1
                    (if (ecb-string= (match-string 3 str) "pre")
                        2
                      (if (ecb-string= (match-string 3 str) ".")
                          4
                        3))))
                (if (match-string 4 str)
                    (string-to-number (match-string 4 str))
                  0))))))

;; (ecb-package-version-str2list "1.0")
;; (ecb-package-version-str2list "1.0alpha")
;; (ecb-package-version-str2list "1.0alpha3")
;; (ecb-package-version-str2list "1.0beta")
;; (ecb-package-version-str2list "1.0beta3")
;; (ecb-package-version-str2list "1.0pre")
;; (ecb-package-version-str2list "1.0pre3")
;; (ecb-package-version-str2list "1.0.1")



(defun ecb-package-version-list< (ver1 ver2)
  "Return non-nil if VER1 is less than VER2."
  (let ((v1-0 (nth 0 ver1))
	(v1-1 (nth 1 ver1))
	(v1-2 (nth 2 ver1))
	(v1-3 (nth 3 ver1))
	;; v2
	(v2-0 (nth 0 ver2))
	(v2-1 (nth 1 ver2))
	(v2-2 (nth 2 ver2))
	(v2-3 (nth 3 ver2)))
    (or (< v1-0 v2-0)
        (and (= v1-0 v2-0)
             (< v1-1 v2-1))
        (and (= v1-0 v2-0)
             (= v1-1 v2-1)
             (< v1-2 v2-2))
        (and (= v1-0 v2-0)
             (= v1-1 v2-1)
             (= v1-2 v2-2)
             (< v1-3 v2-3)))))

(defun ecb-package-version-string< (ver1-str ver2-str)
  "Return non nil if VER-STR1 is logically less then VER-STR2."
  (let ((ver1 (ecb-package-version-str2list ver1-str))
        (ver2 (ecb-package-version-str2list ver2-str)))
    (ecb-package-version-list< ver1 ver2)))

(defun ecb-package-version-list2str (ver)
  "Complementary function to `ecb-package-version-str2list'."
  (concat (number-to-string (nth 0 ver))
          "."
          (number-to-string (nth 1 ver))
          (case (nth 2 ver)
            (0 "alpha")
            (1 "beta")
            (2 "pre")
            (4 ".")
            (otherwise ""))
          (if (and (not (= (nth 2 ver) 3))
                   (not (= (nth 3 ver) 0)))
              (number-to-string (nth 3 ver))
            "")))

;; (ecb-package-version-list2str (ecb-package-version-str2list "1.0"))
;; (ecb-package-version-list2str (ecb-package-version-str2list "1.0alpha"))
;; (ecb-package-version-list2str (ecb-package-version-str2list "1.0alpha3"))
;; (ecb-package-version-list2str (ecb-package-version-str2list "1.0beta"))
;; (ecb-package-version-list2str (ecb-package-version-str2list "1.0beta3"))
;; (ecb-package-version-list2str (ecb-package-version-str2list "1.0pre"))
;; (ecb-package-version-list2str (ecb-package-version-str2list "1.0pre3"))
;; (ecb-package-version-list2str (ecb-package-version-str2list "1.0.1"))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: remove from texi the whole
;; download stuff inkl. in the command ssection ecb-download-*

(silentcomp-provide 'ecb-upgrade)

;;; ecb-upgrade.el ends here

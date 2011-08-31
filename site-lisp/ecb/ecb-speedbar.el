;;; ecb-speedbar.el --- Integration of speedbar into ECB

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
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

;; $Id: ecb-speedbar.el,v 1.73 2009/05/08 14:05:55 berndl Exp $

;;; Commentary:

;; This package provide speedbar integration and using for the ECB.
;;
;; There are two complete different aspects of integration/using speedbar for
;; ECB:
;;
;; 1. Integration the speedbar itself into the ecb-frame:
;;
;;    This allows you to:
;;    
;;    - Sync up to the speedbar with the current buffer.
;;    
;;    - Files opened with the speedbar are displayed in the ecb source window.
;;
;; 2. Using the speedbar-mechanism for parsing files supported not by semantic
;;    but by imenu and/or etags.
;;
;;    This is not done via the speedbar-display but only the parsing mechanism
;;    of `speedbar-fetch-dynamic-tags' is used and the tags are natively
;;    display in the methods-buffer of ECB!
;;
;; Note that this is tested with recent speedbars >= 0.14beta1. If the
;; speedbar implementation changes a lot this could break.
;;
;; If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:

;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'speedbar)
(require 'ecb-util)
(require 'ecb-cedet-wrapper)
(require 'ecb-common-browser)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))


;; imenu
(silentcomp-defvar imenu--rescan-item)
(silentcomp-defvar imenu--index-alist)
;; XEmacs
(silentcomp-defun event-button)
(silentcomp-defvar mouse-motion-handler)

(defgroup ecb-speedbar nil
  "Settings for the speedbar-integration of ECB."
  :group 'ecb-general
  :prefix "ecb-")

(defcustom ecb-speedbar-before-activate-hook nil
  "*Hook running directly before ECB activates the integrated speedbar.

For example this hook can be used to change the expansion-mode of the
integrated speedbar via `speedbar-change-initial-expansion-list'.
Example: \(speedbar-change-initial-expansion-list \"buffers\")."
  :group 'ecb-speedbar
  :type 'hook)

(defcustom ecb-speedbar-buffer-sync 'basic
  "*Synchronize the speedbar-buffer of ECB automatically with current
edit buffer.

If 'always then the synchronization takes place always a buffer changes in the
edit window, if nil then never. If a list of major-modes then only if the
`major-mode' of the new buffer belongs NOT to this list.

If the special value 'basic is set then ECB uses the setting of the option
`ecb-basic-buffer-sync'.

IMPORTANT NOTE: Every time the synchronization is done the hook
`ecb-speedbar-buffer-sync-hook' is evaluated."
  :group 'ecb-speedbar
  :type '(radio :tag "Synchronize ECBs speedbar buffer"
                (const :tag "Use basic value" :value basic)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)
                (repeat :tag "Not with these modes"
                        (symbol :tag "mode"))))
    

(defcustom ecb-speedbar-buffer-sync-delay 'basic
  "*Time Emacs must be idle before the speedbar-buffer of ECB is synchronized.
Synchronizing is done with the current source displayed in the edit window. If
nil then there is no delay, means synchronization takes place immediately. A
small value of about 0.25 seconds saves CPU resources and you get even though
almost the same effect as if you set no delay.

If the special value 'basic is set then ECB uses the setting of the option
`ecb-basic-buffer-sync-delay'."
  :group 'ecb-speedbar
  :type '(radio (const :tag "Use basic value" :value basic)
                (const :tag "No synchronizing delay" :value nil)
                (number :tag "Idle time before synchronizing" :value 2))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (ecb-activate-ecb-autocontrol-function
                        value 'ecb-analyse-buffer-sync))))
  :initialize 'custom-initialize-default)
  
(defcustom ecb-speedbar-buffer-sync-hook nil
  "Hook run at the end of `ecb-speedbar-buffer-sync'.
See documentation of `ecb-speedbar-buffer-sync' for conditions when
synchronization takes place and so in turn these hooks are evaluated.

Preconditions for such a hook:
- Current buffer is the buffer of the currently selected
  edit-window.
- The speedbar-buffer is displayed in a visible window of the
  ecb-frame \(so no check for visibilty of the speedbar-buffer in
  the ecb-frame is necessary in a hook function)

Postcondition for such a hook:
Point must stay in the same edit-window as before evaluating the hook.

Important note: If the option `ecb-speedbar-buffer-sync' is not
nil the function `ecb-speedbar-buffer-sync' is running either
every time Emacs is idle or even after every command \(see
`ecb-speedbar-buffer-sync-delay'). So if the speedbar-buffer is
displayed in a window of the ecb-frame \(see preconditions above)
these hooks can be really called very often! Therefore each
function of this hook should/must check in an efficient way at
beginning if its task have to be really performed and then do
them only if really necessary! Otherwise performance of Emacs
could slow down dramatically!"
  :group 'ecb-speedbar
  :type 'hook)


(defecb-advice-set ecb-speedbar-adviced-functions 
  "These functions of speedbar are always adviced if ECB is active.")

(defconst ecb-speedbar-buffer-name " SPEEDBAR"
  "Name of the ECB speedbar buffer.")

(defun ecb-speedbar-buffer-selected ()
  (equal (current-buffer) (get-buffer ecb-speedbar-buffer-name)))

(defecb-advice speedbar-click around ecb-speedbar-adviced-functions
  "Makes the function compatible with ECB. If ECB is active and the window of
`ecb-speedbar-buffer-name' is visible \(means a layouts uses the
speedbar-integration) and the clicked node in speedbar is a file then the
ECB-edit-window is selected at the end. So always the edit-window is selected
after clicking onto a filename in the speedbar."
  ;; Klaus Berndl <klaus.berndl@sdm.de>: We must use an around-advice because
  ;; we need exactly the information if the *clicked* item is a file or not.
  ;; This is only available before the original speedbar-click actions because
  ;; speedbar seems to do some intelligent stuff like autom. using the first
  ;; file if a clicked directory contains any.
  (let ((item (and (fboundp 'speedbar-line-file)
                   (speedbar-line-file))))
    ad-do-it
    (if (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (window-live-p (get-buffer-window ecb-speedbar-buffer-name))
             (and item
                  (ecb-file-exists-p item)
                  (not (ecb-file-directory-p item))))
        (ecb-select-edit-window))))


(defecb-advice speedbar-frame-mode around ecb-speedbar-adviced-functions
  "During running speedbar within ECB this command is disabled!"
  (if ecb-minor-mode
      (when (interactive-p)
        (ecb-info-message "speedbar-frame-mode takes no effect when running within ECB!"))
    ad-do-it))


(defecb-advice speedbar-get-focus around ecb-speedbar-adviced-functions
  "During running speedbar within ECB this function behaves like follows:
Change window focus to or from the ECB-speedbar-window. If the selected window
is not speedbar-window, then the speedbar-window is selected. If the
speedbar-window is active, then select the edit-window."
  (if ecb-minor-mode
      (if (ecb-speedbar-buffer-selected)
          (ecb-select-edit-window)
        (ecb-speedbar-select-speedbar-window))
    ad-do-it))

;; Klaus Berndl <klaus.berndl@sdm.de>: This implementation is done to make
;; clear where the bug is fixed...a better impl. can be seen in
;; tree-buffer-mouse-set-point (does the same but better code - IMHO).
(defecb-advice dframe-mouse-set-point around ecb-speedbar-adviced-functions
  "Fixes a bug in the original implementation: if clicked onto an image then
the point was not set by `mouse-set-point'."
  (if (and (fboundp 'event-over-glyph-p) (event-over-glyph-p e))
      ;; We are in XEmacs, and clicked on a picture
      (let ((ext (event-glyph-extent e)))
        ;; This position is back inside the extent where the
        ;; junk we pushed into the property list lives.
        (if (extent-end-position ext)
            (progn
              (mouse-set-point e)
              (goto-char (1- (extent-end-position ext))))
          (mouse-set-point e)))
    ;; We are not in XEmacs, OR we didn't click on a picture.
    (mouse-set-point e)))

;; Klaus Berndl <klaus.berndl@sdm.de>: we can not advice
;; speedbar-select-attached-frame because this is a defsubst and is therefore
;; inline compiled into all users of this function. So our advice would never
;; take effect. But dframe-select-attached-frame is a defun so we can advice
;; it!
(defecb-advice dframe-select-attached-frame after ecb-speedbar-adviced-functions
  "Run `ecb-speedbar-dframe-select-attached-window' but only if
`dframe-after-select-attached-frame-hook' is not available."
  (unless (boundp 'dframe-after-select-attached-frame-hook)
    (ecb-speedbar-dframe-select-attached-window)))

(defun ecb-speedbar-dframe-select-attached-window ()
  (when (and ecb-last-edit-window-with-point
             ecb-last-source-buffer
             (window-live-p ecb-last-edit-window-with-point)
             (equal (window-buffer ecb-last-edit-window-with-point)
                    ecb-last-source-buffer))
    (select-window ecb-last-edit-window-with-point)
    (set-buffer ecb-last-source-buffer)
;;     nil
    ))

(defun ecb-speedbar-select-speedbar-window ()
  (ignore-errors
    (and (window-live-p (get-buffer-window ecb-speedbar-buffer-name))
         (select-window (get-buffer-window ecb-speedbar-buffer-name)))))

(defun ecb-speedbar-set-buffer()
  "Set the speedbar buffer within ECB."
  (ecb-speedbar-activate)
  (set-window-buffer (selected-window)
                     (get-buffer-create ecb-speedbar-buffer-name))
  (unless ecb-running-xemacs
    (set (make-local-variable 'automatic-hscrolling) nil)))


(defvar ecb-speedbar-verbosity-level-old nil)
(defvar ecb-speedbar-select-frame-method-old nil)
(defvar ecb-speedbar-update-flag-old -1)

(defun ecb-speedbar-activate()
  "Make sure the speedbar is running. WARNING: This could be dependent on the
current speedbar implementation but normally it should work with recent
speedbar versions >= 0.14beta1. But be aware: If the speedbar impl changes in
future this could break."

  ;; enable the advices for speedbar
  (ecb-enable-advices 'ecb-speedbar-adviced-functions)
  
  (run-hooks 'ecb-speedbar-before-activate-hook)

  (add-hook 'dframe-after-select-attached-frame-hook
            'ecb-speedbar-dframe-select-attached-window)

  ;;disable automatic speedbar updates... let the ECB handle this with
  ;;its sync-mechanism
  (speedbar-disable-update)

  ;;always stay in the current frame
  ;; save the old value but only first time!
  (if (null ecb-speedbar-select-frame-method-old)
      (setq ecb-speedbar-select-frame-method-old speedbar-select-frame-method))
  (setq speedbar-select-frame-method 'attached)

  (when (not (buffer-live-p speedbar-buffer))
    (save-excursion
      (setq speedbar-buffer (get-buffer-create ecb-speedbar-buffer-name))
      (set-buffer speedbar-buffer)
      (speedbar-mode)

      (if ecb-running-xemacs
          ;; Hack the XEmacs mouse-motion handler
          (progn
            ;; Hack the XEmacs mouse-motion handler
            (set (make-local-variable 'mouse-motion-handler)
                 'dframe-track-mouse-xemacs)
            ;; Hack the double click handler
            (make-local-variable 'mouse-track-click-hook)
            (add-hook 'mouse-track-click-hook
                      (lambda (event count)
                        (if (/= (event-button event) 1)
                            nil		; Do normal operations.
                          (case count
                            (1 (dframe-quick-mouse event))
                            ((2 3) (dframe-click event)))
                          ;; Don't do normal operations.
                          t))))
        ;; Enable mouse tracking in emacs
        (if dframe-track-mouse-function
            (set (make-local-variable 'track-mouse) t)) ;this could be messy.
        ;; disable auto-show-mode for Emacs
        ;; obsolete with beginning of Emacs 21...
;;         (setq auto-show-mode nil)
        )))

  ;;Start up the timer
  (speedbar-reconfigure-keymaps)
  (speedbar-update-contents)
  (speedbar-set-timer 1)

  ;;Set the frame that the speedbar should use.  This should be the selected
  ;;frame.  AKA the frame that ECB is running in.
  (setq speedbar-frame ecb-frame)
  (setq dframe-attached-frame ecb-frame)
  
  ;;this needs to be 0 because we can't have the speedbar too chatty in the
  ;;current frame because this will mean that the minibuffer will be updated too
  ;;much.
  ;; save the old value but only first time!
  (if (null ecb-speedbar-verbosity-level-old)
      (setq ecb-speedbar-verbosity-level-old speedbar-verbosity-level))
  (setq speedbar-verbosity-level 0)

  ;; save old update-flag but only the first time
  (if (equal ecb-speedbar-update-flag-old -1)
      (setq ecb-speedbar-update-flag-old speedbar-update-flag))
  (setq speedbar-update-flag nil)

  (ecb-activate-ecb-autocontrol-function ecb-speedbar-buffer-sync-delay 
                                          'ecb-speedbar-buffer-sync)

  ;;reset the selection variable
  (setq speedbar-last-selected-file nil))


(defun ecb-speedbar-deactivate ()
  "Reset things as before activating speedbar by ECB"
  (ecb-disable-advices 'ecb-speedbar-adviced-functions)
  
  (remove-hook 'dframe-after-select-attached-frame-hook
               'ecb-speedbar-dframe-select-attached-window)

  (setq speedbar-frame nil)
  (setq dframe-attached-frame nil)

  (speedbar-enable-update)
  
  (if ecb-speedbar-select-frame-method-old
      (setq speedbar-select-frame-method ecb-speedbar-select-frame-method-old))
  (setq ecb-speedbar-select-frame-method-old nil)

  (if ecb-speedbar-verbosity-level-old
      (setq speedbar-verbosity-level ecb-speedbar-verbosity-level-old))
  (setq ecb-speedbar-verbosity-level-old nil)

  (if (not (equal ecb-speedbar-update-flag-old -1))
      (setq speedbar-update-flag ecb-speedbar-update-flag-old))

  (ecb-stop-autocontrol/sync-function 'ecb-speedbar-buffer-sync)

  (when (and speedbar-buffer
             (buffer-live-p speedbar-buffer))
    (kill-buffer speedbar-buffer)
    (setq speedbar-buffer nil)))


(defun ecb-speedbar-active-p ()
  "Return not nil if speedbar is active and integrated in the `ecb-frame'."
  (and (get-buffer ecb-speedbar-buffer-name)
       (get-buffer-window (get-buffer ecb-speedbar-buffer-name) ecb-frame)))

(defun ecb-speedbar-update-contents ()
  "Encapsulate updating the speedbar."
  (speedbar-update-contents))


(defecb-autocontrol/sync-function ecb-speedbar-buffer-sync
    ecb-speedbar-buffer-name ecb-speedbar-buffer-sync t
  "Update the speedbar so that it's synced up with the current file."
  (let ((speedbar-default-directory
         (save-excursion
           (set-buffer visible-buffer)
           (ecb-fix-filename default-directory)))
        (ecb-default-directory (ecb-fix-filename default-directory)))
    (when (and (or (not (ecb-string= speedbar-default-directory
                                     ecb-default-directory))
                   (member speedbar-initial-expansion-list-name
                           ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: make
                           ;; this customizable
                           '("buffers" "Analyze")))
               speedbar-buffer
               (buffer-live-p speedbar-buffer))
      (ecb-speedbar-update-contents)
      (run-hooks 'ecb-speedbar-buffer-sync-hook))))

(defun ecb-goto-window-speedbar ()
  "Make the ECB-speedbar window the current window.
This command does nothing if no integrated speedbar is visible in the
ECB-frame."
  (interactive)
  (ecb-goto-ecb-window ecb-speedbar-buffer-name))

(defun ecb-maximize-window-speedbar ()
  "Maximize the ECB-speedbar-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Does nothing if the
speedbar-window is not visible within the ECB-frame."
  (interactive)
  (ecb-maximize-ecb-buffer ecb-speedbar-buffer-name t))


;; Handling of files which can not be parsed by semantic (i.e. there is no
;; semantic-grammar available) but which can be parsed by imenu and/or etags
;; via speedbar.

(defun ecb-speedbar-sb-tag-p (tag)
  "Return not nil if TAG is a semantic-tag generated from a speedbar tag."
  (ecb--semantic--tag-get-property tag 'ecb-speedbar-tag))

(require 'tree-buffer)
(require 'ecb-face)
(defun ecb-create-non-semantic-tree (node tag-list)
  "Add all tags of TAG-LIST with side-effects as children to NODE. TAG-LIST is
a list generated by `ecb-get-tags-for-non-semantic-files'. TAG-LIST is of the
form:
\( \(\"name\" . marker-or-number) <-- one tag at this level
  \(\"name\" \(\"name\" . mon) (\"name\" . mon) )  <-- one group of tags
  \(\"name\" mon \(\"name\" . mon) )             <-- group w/ a pos. and tags

Groups can contain tags which are groups again...therefore this function is
called recursive for the elements of a group.

Return NODE."
  (let ((new-node nil)
        (new-tag nil))
    (dolist (tag tag-list)
      (typecase tag
        (null nil) ;; this would be a separator
        (speedbar-generic-list-tag
         ;; the semantic tag for this tag
         (setq new-tag (ecb--semantic-tag (car tag)
                                          (intern (car tag))))
         (ecb--semantic--tag-set-overlay new-tag (make-vector 2 (cdr tag)))
         (ecb--semantic--tag-put-property new-tag 'ecb-speedbar-tag t)
         (ecb-apply-user-filter-to-tags (list new-tag))
         (when (not (ecb-tag-forbidden-display-p new-tag))
           (tree-node-new (progn
                            (set-text-properties
                             0 (length (car tag))
                             `(face ,ecb-method-non-semantic-face) (car tag))
                            (car tag))
                          0
                          new-tag
                          t
                          node)))
        (speedbar-generic-list-positioned-group
         ;; the semantic tag for this tag
         (setq new-tag (ecb--semantic-tag (car tag)
                                          (intern (car tag))))
         (ecb--semantic--tag-set-overlay new-tag
                                         (make-vector 2 (car (cdr tag))))
         (ecb--semantic--tag-put-property new-tag 'ecb-speedbar-tag t)
         (ecb-apply-user-filter-to-tags (list new-tag))
         (when (not (ecb-tag-forbidden-display-p new-tag))             
           (ecb-create-non-semantic-tree
            (setq new-node
                  (tree-node-new (progn
                                   (set-text-properties
                                    0 (length (car tag))
                                    `(face ,ecb-method-non-semantic-face) (car tag))
                                   (car tag))
                                 0
                                 new-tag
                                 nil node))
            (cdr (cdr tag)))
           (setf (tree-node->expanded new-node)
                 (member major-mode
                         ecb-non-semantic-methods-initial-expand))))
        (speedbar-generic-list-group
         (ecb-create-non-semantic-tree
          (setq new-node
                (tree-node-new (progn
                                 (set-text-properties
                                  0 (length (car tag))
                                  `(face ,ecb-method-non-semantic-face) (car tag))
                                 (car tag))
                               1
                               nil nil node))
          (cdr tag))
         (setf (tree-node->expanded new-node)
               (member major-mode
                       ecb-non-semantic-methods-initial-expand)))
        (otherwise
         (ecb-error "ecb-create-non-semantic-tree: malformed tag-list!")
         )))
    node))

(defun ecb-get-tags-for-non-semantic-files ()
  "Get a tag-list for current source-file. This is done via the
`speedbar-fetch-dynamic-tags' mechanism which supports imenu and etags."
  (require 'imenu)
  (if (member major-mode ecb-non-semantic-exclude-modes)
      nil
    (let* ((lst (let ((speedbar-dynamic-tags-function-list
                       (if (not (assoc major-mode
                                       ecb-non-semantic-parsing-function))
                           (list (cons 'speedbar-fetch-dynamic-imenu 'identity)
                                 (cons 'speedbar-fetch-dynamic-etags 'identity))
                         (list (cons (cdr (assoc major-mode
                                                 ecb-non-semantic-parsing-function))
                                     'identity)))))
                  (speedbar-fetch-dynamic-tags (ecb-buffer-file-name
                                                (current-buffer)))))
           (tag-list (cdr lst))
           (methods speedbar-tag-hierarchy-method))
    
      ;; removing the imenu-Rescan-item
      (if (ecb-string= (car (car tag-list)) (car imenu--rescan-item))
          (setq tag-list (cdr tag-list)))
      ;; If imenu or etags returns already groups (etags will do this probably
      ;; not, but imenu will do this sometimes - e.g. with cperl) then we do not
      ;; regrouping with the speedbar-methods of
      ;; `speedbar-tag-hierarchy-method'!
      (when (dolist (tag tag-list t)
              (if (or (speedbar-generic-list-positioned-group-p tag)
                      (speedbar-generic-list-group-p tag))
                  (return nil)))
        (while methods
          (setq tag-list (funcall (car methods) tag-list)
                methods (cdr methods))))
      tag-list)))


(silentcomp-provide 'ecb-speedbar)

;;; ecb-speedbar.el ends here

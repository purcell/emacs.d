;;; ecb-eshell.el --- eshell integration for the ECB.

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2001

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

;; $Id: ecb-eshell.el,v 1.78 2005/06/20 14:34:20 berndl Exp $

;;; Commentary:

;; This package provides eshell integration for the ECB.  This basically allows
;; you to jump to the eshell in the compilation window, sync up the current
;; eshell with the current ECB buffer and run commands without getting in the
;; way.
;;
;; It provides the following features:
;;
;; - ability to jump to the eshell buffer within the compilation window ( C-.e )
;;   If the eshell isn't running it will be started
;;
;; - expands the compilation window when you run commands.  So for example it
;;   allows you to view the eshell in minimized mode and then when you run 'ls'
;;   the window automatically expands.
;;
;; - Synchronizes the current directory of the eshell with the current buffer
;;   of the either the edit-window or the ecb-windows.
;;
;; - Provides smart window layout of the eshell buffer.  This makes sure that
;;   the eshell is taking up the exact amount of space and that nothing is
;;   hidden.
;; 
;; The goal is to make it easy to jump to a command prompt to run OS level
;; commands.  
;; 
;; If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:

;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Design:
;;
;; Syncing the current buffer with the eshell is done two ways.  If the buffer
;; is visible in a window, we always resync.  If it is not visible then
;; ecb-eshell-goto-eshell will sync up when the user goes to the eshell
;; buffer.
;;
;; Integrating of eshell is mostly done by advicing the command `eshell' which
;; uses the mechanism of the display-buffer (adviced version).


;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)
(require 'ecb-compilation)
(require 'ecb-common-browser)

(silentcomp-defvar eshell-buffer-name)
(silentcomp-defun eshell)
(silentcomp-defun eshell/cd)
(silentcomp-defun eshell-send-input)
(silentcomp-defun eshell-bol)

(defgroup ecb-eshell nil
  "Settings for eshell integration within the ECB."
  :group 'ecb
  :prefix "ecb-eshell-")

(defcustom ecb-eshell-enlarge-when-eshell t
  "*Enlarge the compile-window if it is selected by `eshell'.
This takes only effect if the command `eshell' is called!"
  :group 'ecb-eshell
  :type 'boolean)

(defcustom ecb-eshell-fit-window-to-command-output t
  "*Fit the compile-window after an eshell-command to the output.
This is done by the function `ecb-eshell-fit-window-to-output' which is added
to `eshell-post-command-hook' ie. which is running autom. after each
eshell-command."
  :group 'ecb-eshell
  :type 'boolean)

(defcustom ecb-eshell-auto-activate nil
  "*Startup the eshell and display it in the compile-window.
If current layout does not display a compile-window \(see
`ecb-compile-window-height') then nothing is done."
  :group 'ecb-eshell
  :type 'boolean)

(defcustom ecb-eshell-synchronize t
  "*Synchronize eshell with the default-directory of current source-buffer.
The synchronization is done by `ecb-eshell-current-buffer-sync' which can be
called interactively but normally it is called autom. by the
`ecb-current-buffer-sync-hook-internal'."
  :group 'ecb-eshell
  :type 'boolean)

(defvar ecb-eshell-pre-command-point nil
  "Point in the buffer we are at before we executed a command.")

(defvar ecb-eshell-buffer-list nil
  "List of eshell-buffers created until now.
Background: `eshell' creates new eshell-buffers with `generate-new-buffer' if
called with an prefix arg!")


(defconst ecb-eshell-adviced-functions '((eshell . around))
  "These functions of eshell are adviced if ehsell is active during ECB is
active. Each element of the list is a cons-cell where the car is the
function-symbol and the cdr the advice-class \(before, around or after). If a
function should be adviced with more than one class \(e.g. with a before and
an after-advice) then for every class a cons must be added to this list.")

(defadvice eshell (around ecb)
  "Ensure that ehsell is running in the ECB-compile-window if any."
  ;; we tell ECB to handle the eshell-buffers as compilation-buffers so they
  ;; will be displayed in the compile-window (if any). We must add this as
  ;; regexp because ehsell can open new eshell-buffers with a name created by
  ;; generate-new-buffer-name! This approach is not completely save because if
  ;; a users changes `eshell-buffer-name' during acivated ECB we get not
  ;; informed about this and maybe we can handle the new buffer-name of eshell
  ;; not as compilation-buffer. But we have no other chance: Adding the return
  ;; value of `eshell' in the advice to `ecb-compilation-buffer-names-internal'
  ;; does not help because `eshell' uses the new buffer-name already for
  ;; `pop-to-buffer'. So an after advice would add the new buffer-name to late
  ;; and a before-advice does not know the new-buffer name. The only way would
  ;; be to reimplement the whole `eshell'-code in an around advice but this is
  ;; not related to the benefit. IMO is it very improbably that a user changes
  ;; `eshell-buffer-name' at all...
  (let ((new-elem (cons (concat ".*"
                                (regexp-quote eshell-buffer-name)
                                ".*")
                        t)))
    (if ecb-compile-window-height
        (progn
          (add-to-list 'ecb-compilation-buffer-names-internal new-elem)
          (add-to-list 'ecb-compilation-major-modes-internal 'eshell-mode))
      ;; if we have no persistent compile-window we do not handle eshell autom.
      ;; as compilation-buffer. If the user wants this then he has to modify
      ;; `ecb-compilation-buffer-names' and/or `ecb-compilation-major-modes'.
      ;; Therefore we remove the new-elem here from the internal lists.
      (setq ecb-compilation-buffer-names-internal
            (delete new-elem ecb-compilation-buffer-names-internal))
      (setq ecb-compilation-major-modes-internal
            (delete 'eshell-mode ecb-compilation-major-modes-internal))))
  
  ;; maybe we have to auto toggle our compile-window if temporally hidden
  (when (equal 'hidden (ecb-compile-window-state))
    (ecb-layout-debug-error "eshell around-advice: comp-win will be toggled.")
    (ecb-toggle-compile-window 1))

  ;; some hooks
  (add-hook 'ecb-current-buffer-sync-hook-internal
            'ecb-eshell-current-buffer-sync)  
  (add-hook 'eshell-post-command-hook 'ecb-eshell-recenter)
  (add-hook 'eshell-post-command-hook 'ecb-eshell-fit-window-to-output)
  (add-hook 'eshell-pre-command-hook 'ecb-eshell-precommand-hook)
  (add-hook 'window-size-change-functions 'ecb-eshell-window-size-change)

  ;; run `eshell' --------------------------------------------
  (ecb-eshell-save-buffer-history
   ad-do-it)
  ;; ---------------------------------------------------------

  ;; some post processing

  ;; add the buffer of the buffer used/created by `eshell' to
  ;; `ecb-eshell-buffer-list'
  (add-to-list 'ecb-eshell-buffer-list ad-return-value)

  (when ecb-eshell-enlarge-when-eshell
    (ecb-toggle-compile-window-height 1))

  ;;always recenter because if the point is at the top of the eshell buffer
  ;;and we switch to it the user is not going to be able to type a command
  ;;right away.
  (ecb-eshell-recenter)  
  
  ;;sync to the current buffer
  (ecb-eshell-current-buffer-sync))
  
  

(defun ecb-eshell-activate-integration ()
  "Does all necessary to activate the eshell-integration. But this doesn not
load or activate eshell - it just prepares ECB to work perfectly with eshell."
  (ecb-enable-advices ecb-eshell-adviced-functions))

(defun ecb-eshell-deactivate-integration ()
  (ecb-disable-advices ecb-eshell-adviced-functions)
  (remove-hook 'ecb-current-buffer-sync-hook-internal
               'ecb-eshell-current-buffer-sync)
  (remove-hook 'eshell-post-command-hook 'ecb-eshell-recenter)
  (remove-hook 'eshell-post-command-hook 'ecb-eshell-fit-window-to-output)
  (remove-hook 'eshell-pre-command-hook 'ecb-eshell-precommand-hook)
  (remove-hook 'window-size-change-functions 'ecb-eshell-window-size-change))

(defun ecb-eshell-current-buffer-sync()
  "Synchronize the eshell with the directory of current source-buffer.
This is only done if the eshell is currently visible in the compile-window of
ECB and if either this function is called interactively or
`ecb-eshell-synchronize' is not nil."
  (interactive)

  (when (and (equal (selected-frame) ecb-frame)
             (or ecb-eshell-synchronize (interactive-p))
             (ecb-compile-window-live-p)
             (not (ecb-point-in-compile-window)))

    (let* ((my-eshell-buffer
            ;; nil or a living eshell-buffer in the ecb-compile-window
            (car (member (window-buffer ecb-compile-window)
                         ecb-eshell-buffer-list)))
           (my-reference-directory default-directory)
           (my-eshell-directory (and (bufferp my-eshell-buffer)
                                     (save-excursion
                                       (set-buffer my-eshell-buffer)
                                       default-directory))))
      (when (and (bufferp my-eshell-buffer)
                 (stringp my-reference-directory)
                 (stringp my-eshell-directory)
                 (not (ecb-string= (ecb-fix-filename my-reference-directory)
                                   (ecb-fix-filename my-eshell-directory))))
        (ecb-eshell-save-buffer-history
         (save-excursion
           (set-buffer my-eshell-buffer)
           ;; make sure we have a clean eshell-command-line
           (goto-char (point-max))
           (eshell-bol)
           (delete-region (point) (point-at-eol))
           ;;change the directory without showing the cd command
           (eshell/cd my-reference-directory))
           
         ;;execute the command
         (save-selected-window
           (select-window ecb-compile-window)
           (eshell-send-input)))
        
        (ecb-eshell-recenter)
        
        ;; we need to make sure that that the eshell buffer isn't at the
        ;; top of the buffer history list just because we implicitly
        ;; changed its directory and switched to it. It might not be a
        ;; good idea in the long term to put it all the way at the end of
        ;; the history list but it is better than leaving it at the top.
        (bury-buffer eshell-buffer-name)))))

(defmacro ecb-eshell-save-buffer-history (&rest body)
  "Protect the buffer-list so that the eshell buffer name is not placed early
in the buffer list or at all if it currently doesn't exist."
  (let ((eshell-buffer-list (make-symbol "my-buffer-list")))
    `(let ((,eshell-buffer-list (ecb-frame-parameter (selected-frame)
                                                     'buffer-list)))
       (unwind-protect
           (progn
             ,@body)
         (modify-frame-parameters nil (list (cons 'buffer-list
                                                  ,eshell-buffer-list)))))))

(defun ecb-eshell-recenter(&optional display-errors)
  "Recenter the eshell window so that the prompt is at the buffer-end."
  (interactive (list t))

  (if (and (equal (selected-frame) ecb-frame)
           (ecb-compile-window-live-p)
           ;; the buffer in the ecb-compile-window is a living eshell-buffer
           (member (window-buffer ecb-compile-window)
                   ecb-eshell-buffer-list))
      (save-selected-window
        (select-window ecb-compile-window)
        (goto-char (point-max))
        (recenter -2))
    (when display-errors
      (ecb-error "Eshell not running or compile-window not visible!"))))

(defun ecb-eshell-precommand-hook ()
  ;;use the eshell-pre-command-hook to set the point.
  (setq ecb-eshell-pre-command-point (point)))


(defun ecb-eshell-fit-window-to-output()
  "Fit window of eshell to the output of last command. This function is added
to `eshell-post-command-hook' and only called there. This function tries to
fit the height of the compile-window best to the last command-output. The
algorithm fit the window to the height of the last command-output but do not
enlarge the compile-window over half of the frame-height and also not below
`ecb-compile-window-height' (in lines)."
  (when (and (equal (selected-frame) ecb-frame)
             (ecb-compile-window-live-p)
             ;; the buffer in the ecb-compile-window is a living eshell-buffer
             (member (window-buffer ecb-compile-window)
                     ecb-eshell-buffer-list))

    ;; fit the window to the height of the last command-output but do not
    ;; enlarge the compile-window over half of the frame-height and also not
    ;; below `ecb-compile-window-height' (in lines).
    (when (and ecb-eshell-fit-window-to-command-output
               (integer-or-marker-p ecb-eshell-pre-command-point))
      (let* ((compile-window-height-lines
              (ecb-normalize-number ecb-compile-window-height
                                    (1- (frame-height))))
             (ecb-enlarged-compilation-window-max-height
              (max (min (save-excursion
                          (set-buffer (window-buffer ecb-compile-window))
                          ;; we want to see the old command line too and 2
                          ;; must be added because we have a modeline and one
                          ;; empty line cause of the (recenter -2) in
                          ;; `ecb-eshell-recenter'. For XEmacs it would be
                          ;; better to check if a horiz. scrollbar is used.
                          ;; This causes the one line more we need for XEmacs
                          (+ (if ecb-running-xemacs 4 3)
                             (count-lines ecb-eshell-pre-command-point
                                          (point))))
                        (/ (1- (frame-height)) 2))
                   compile-window-height-lines)))
        (ecb-toggle-compile-window-height 1)
        (ecb-eshell-recenter))
        
      ;;reset
      (setq ecb-eshell-pre-command-point nil))))


(defun ecb-eshell-auto-activate-hook()
  "Activate the eshell when ECB is activated. See `ecb-eshell-auto-activate'."
  (when ecb-eshell-auto-activate
    (ignore-errors (eshell))))

(defun ecb-eshell-window-size-change(frame)
  "Called when we change window sizes so that the eshell can resize."
  (when (and ecb-minor-mode
             (equal frame ecb-frame))
    (ignore-errors (ecb-eshell-recenter))))

(add-hook 'ecb-activate-hook 'ecb-eshell-auto-activate-hook)

(silentcomp-provide 'ecb-eshell)

;;; ecb-eshell.el ends here

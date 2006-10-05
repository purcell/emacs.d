;; ecb-winman-support.el - support of several window managers

;; Copyright (C) 2000 - 2005 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools, escreen, winring
;; Created: 2003

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

;; $Id: ecb-winman-support.el,v 1.13 2005/02/28 11:31:54 berndl Exp $

;;; Commentary
;;
;; This library contains support for several window-managers so they interact
;; well with ECB. Currently the following window-managers are supported by ECB:
;; - winring.el: Written by Barry A. Warsaw <bwarsaw@python.org>, get it from
;;   http://www.python.org/emacs/
;; - escreen.el: Written by Noah Friedman <friedman@splode.com>, get it from
;;   http://www.splode.com/~friedman/software/emacs-lisp/
;;
;; Note: With one of these window-managers installed and active you can run
;; applications like Gnus, VM or BBDB in the same frame as ECB! Just use
;; different window-configurations (winring.el) or escreens (escreen.el) for
;; ECB and the other applications. Especially with winring.el you can give
;; every configuration a descriptive name like "ECB" or "Gnus" ; afterwards
;; you can jump to a window-configuration by name!


;;; Installation and enabling
;;
;; This library is installed autom. with ECB. But every support must be
;; enabled explicitly:
;; - winring: Call `ecb-winman-winring-enable-support'. This *must* be done
;;   *before* the first call to any winring-command, so also before calling
;;   `winring-initialize'!
;; - escreen: Call `ecb-winman-escreen-enable-support'. This *must* be done
;;   *before* the first call to any escreen-command, so also before calling
;;   `escreen-install'!
;;
;; You can also put into your .emacs:
;; (ecb-winman-winring-enable-support) or/and
;; (ecb-winman-escreen-enable-support)

;;; Deinstallation
;;
;; Just run `ecb-winman-escreen-disable-support' rsp.
;; `ecb-winman-winring-disable-support'.


;;; Usage
;;
;; After enabling the support of one of the supported window-managers just go
;; on as described in the commentary or introduction of the respective
;; library-file(s) of the window-manager. Here is a short description:

;; - winring: Run `winring-initialize'. If ECB is active then the resulting
;;   window-configuration is the ECB-window-configuration. Otherwise you can
;;   create the ECB-window-configuration when you first time call
;;   `winring-new-configuration' with name equal to `ecb-winman-winring-name'.
;;   In general you can run all commands of the winring-library. If you jump
;;   to the ECB-window-configuration then ECB will be autom. activated and if
;;   you leave the ECB-window-configuration then ECB will autom. deactivated.

;; - escreen: Run `escreen-install' (deactivates ECB if currently
;;   running), `escreen-create-screen', `escreen-goto-screen' etc. The latter
;;   ones activate autom. ECB if creating or selecting the escreen with number
;;   `ecb-escreen-number' (default = 1) and deactivate ECB autom. if leaving
;;   the ECB-escreen.


;;; BUGS
;;
;; Currently not known


;; Thanks to Johann "Myrkraverk" Oskarsson <myrkraverk@users.sourceforge.net>
;; for the first trigger for this support-library. He has suggested to
;; integrate ECB with escreen.


;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)

(silentcomp-defvar escreen-current-screen-number)


(defgroup ecb-winman-support nil
  "Settings for supporting several window-managers.
Currently winring and escreen are supported."
  :group 'ecb
  :prefix "ecb-winman-")

(defcustom ecb-winman-escreen-number 1
  "*Number of the escreen which is reserved for ECB.
If you go to the escreen with this number you go always to the escreen with
activated ECB. All other escreen-numbers are escreens with deactivated ECB!"
  :group 'ecb-winman-support
  :group 'ecb-most-important
  :type 'integer)

(defcustom ecb-winman-winring-name "ECB"
  "*Name of the winring-window-configuration reserved for ECB.
If you go to the window-configuration with this name you go always to the
window-configuration with activated ECB. All other window-configuration are
configurations with deactivated ECB!"
  :group 'ecb-winman-support
  :group 'ecb-most-important
  :type 'string)

;; support for the library escreen.el ----------------------------------------

(defconst ecb-winman-escreen-adviced-functions
  '((escreen-save-current-screen-configuration . before))
  "These functions of escreen are adviced if escreen is active during ECB is
active. Each element of the list is a cons-cell where the car is the
function-symbol and the cdr the advice-class \(before, around or after). If a
function should be adviced with more than one class \(e.g. with a before and
an after-advice) then for every class a cons must be added to this list.")


(defun ecb-winman-escreen-enable-support ()
  "Load the escreen-library and enable the ECB-support for it.
This does not install or activate escreen! For this you have still to call
`escreen-install'! For further documentation about escreen see the file
escreen.el!"
  (interactive)
  (if (locate-library "escreen")
      (condition-case nil
          (progn
            (require 'escreen)
            (ecb-enable-advices ecb-winman-escreen-adviced-functions)
            (add-hook 'escreen-goto-screen-hook
                      'ecb-winman-escreen-goto-escreen-hook)
            (ecb-info-message "Support for escreen enabled."))
        (error
         (ecb-winman-escreen-disable-support)
         (ecb-error "The escreen-support can not be properly installed!")))
    (ecb-error "The library escreen.el can not be found!")))


(defun ecb-winman-escreen-disable-support ()
  "Disable the escreen-support of ECB."
  (interactive)
  (ecb-disable-advices ecb-winman-escreen-adviced-functions)
  (when (featurep 'escreen)
    (remove-hook 'escreen-goto-screen-hook
                 'ecb-winman-escreen-goto-escreen-hook)))
    

(defun ecb-winman-escreen-goto-escreen-hook ()
  "Activate ECB if we go to the escreen with number `ecb-escreen-number'."
  (if (and (boundp 'ecb-minor-mode)
           (not ecb-minor-mode)
           (= escreen-current-screen-number
              ecb-winman-escreen-number))
      (let ((ecb-split-edit-window-after-start 'before-deactivation))
        (ecb-activate))))

(defadvice escreen-save-current-screen-configuration (before ecb)
  "escreen can only handle screen-configurations if ECB is deactivated. This
is because ECB handles its window-creation completely by itself and because it
uses dedicated windows. So we deactivate ECB before running this function."
  (if (and (boundp 'ecb-minor-mode)
           ecb-minor-mode
           (equal ecb-frame (selected-frame)))
      (let ((ecb-split-edit-window-after-start 'before-deactivation))
        (ecb-deactivate))))

;; support for the library winring.el ---------------------------------------

(defconst ecb-winman-winring-adviced-functions
  '((winring-save-current-configuration . before)
    (winring-initialize . after)
    (winring-duplicate-configuration . before)
    (winring-restore-configuration . before)
    (winring-set-name . after))
     "These functions of winring are adviced if winring is active during ECB is
active. Each element of the list is a cons-cell where the car is the
function-symbol and the cdr the advice-class \(before, around or after). If a
function should be adviced with more than one class \(e.g. with a before and
an after-advice) then for every class a cons must be added to this list.")

(defun ecb-winman-winring-enable-support ()
  "Load the winring-library and enable the ECB-support for it.
This does not install or activate winring! For this you have still to call
`winring-initialize'! For further documentation about winring see the file
winring.el!"
  (interactive)
  (if (locate-library "winring")
      (condition-case nil
          (progn
            (require 'winring)
            (ecb-enable-advices ecb-winman-winring-adviced-functions)
            (ecb-info-message "Support for winring enabled."))
        (error
         (ecb-winman-winring-disable-support)
         (ecb-error "The winring-support can not be properly installed!")))
    (ecb-error "The library winring.el can not be found!")))

(defun ecb-winman-winring-disable-support ()
  "Disable the winring-support of ECB."
  (interactive)
  (ecb-disable-advices ecb-winman-winring-adviced-functions))


(defvar ecb-winman-winring-ecb-frame nil
  "Frame for which the ECB-window-configuration was set first time.")

(defadvice winring-set-name (after ecb)
  "Store frame if name is equal with `ecb-winman-winring-name' and activate
ECB if we set the name `ecb-winman-winring-name'."
  ;; Because this is an after advice of winring-name-of-current returns here
  ;; already the new name!
  (when (ecb-string= (winring-name-of-current) ecb-winman-winring-name)
    ;; we do this only the first time
    (when (null ecb-winman-winring-ecb-frame)
      (setq ecb-winman-winring-ecb-frame
            (or (ad-get-arg 1) (selected-frame))))
    ;; now we activate ECB if necessary
    (when (and (boundp 'ecb-minor-mode)
               (not ecb-minor-mode)
               (equal (or (ad-get-arg 1)
                          (selected-frame)) ecb-winman-winring-ecb-frame))
      (let ((ecb-split-edit-window-after-start 'before-deactivation))
        (ecb-activate)))))

(defadvice winring-duplicate-configuration (before ecb)
  "Prevent the ECB-window-configuration from being duplicated."
  (if (ecb-string= (winring-name-of-current) ecb-winman-winring-name)
      (ecb-error "The ECB-window-configuration can not be duplicated!")))

(defadvice winring-restore-configuration (before ecb)
  "Deactivates ECB if the ECB-window-configuration is active."
  (if (and (ecb-string= (winring-name-of-current) ecb-winman-winring-name)
           (boundp 'ecb-minor-mode)
           ecb-minor-mode)
      (let ((ecb-split-edit-window-after-start 'before-deactivation))
        (ecb-deactivate))))
  

(defadvice winring-save-current-configuration (before ecb)
  "winring can only handle window-configurations if ECB is deactivated. This
is because ECB handles its window-creation completely by itself and because it
uses dedicated windows. So we deactivate ECB before running this function."
  (if (and (boundp 'ecb-minor-mode)
           ecb-minor-mode
           (equal ecb-frame (selected-frame)))
      (let ((ecb-split-edit-window-after-start 'before-deactivation))
        (ecb-deactivate))))

  
(defadvice winring-initialize (after ecb)
  "If ECB is active when winring is initialized then this initial
window-configuration gets always the name `ecb-winman-winring-name'."
  (if (and (boundp 'ecb-minor-mode)
           ecb-minor-mode
           (equal ecb-frame (selected-frame)))
      (winring-set-name ecb-winman-winring-name)))


;; not supported window-managing functions------------------------------------

(defconst ecb-winman-not-supported-function-advices
  (if ecb-running-xemacs
      '((winner-mode . before)
        (winner-redo . before)
        (winner-undo . before)
        (push-window-configuration . before)
        (pop-window-configuration . before)
        (unpop-window-configuration . before))
    '((winner-mode . before)
      (winner-redo . before)
      (winner-undo . before))))

(defadvice winner-mode (before ecb)
  "Prevents `winner-mode' from being activated for the ECB-frame."
  (if (equal (selected-frame) ecb-frame)
      (ecb-error "Can't use winner-mode functions in the ecb-frame.")))

(defadvice winner-redo (before ecb)
  "Prevents `winner-redo' from being used within the ECB-frame."
  (if (equal (selected-frame) ecb-frame)
      (ecb-error "Can't use winner-mode functions in the ecb-frame.")))

(defadvice winner-undo (before ecb)
  "Prevents `winner-undo' from being used within the ECB-frame."
  (if (equal (selected-frame) ecb-frame)
      (ecb-error "Can't use winner-mode functions in the ecb-frame.")))

(when ecb-running-xemacs
  (defadvice push-window-configuration (before ecb)
    (if (and (equal (selected-frame) ecb-frame)
             (interactive-p))
        (ecb-error "Can't use interactive push-window-configuration in the ecb-frame.")))

  (defadvice pop-window-configuration (before ecb)
    (if (and (equal (selected-frame) ecb-frame)
             (interactive-p))
        (ecb-error "Can't use interactive pop-window-configuration in the ecb-frame.")))
  
  (defadvice unpop-window-configuration (before ecb)
    (if (and (equal (selected-frame) ecb-frame)
             (interactive-p))
        (ecb-error "Can't use interactive unpop-window-configuration in the ecb-frame.")))
  )

;; we disable all advices per default.

(ecb-disable-advices ecb-winman-winring-adviced-functions)
(ecb-disable-advices ecb-winman-escreen-adviced-functions)
(ecb-disable-advices ecb-winman-not-supported-function-advices)

(silentcomp-provide 'ecb-winman-support)

;;; ecb-winman-support.el ends here

;;; ecb-compatibility.el --- ECB-compatibility for other packages

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2004

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

;; $Id: ecb-compatibility.el,v 1.14 2009/05/15 15:19:53 berndl Exp $

;;; Commentary:
;;
;; Contains compatibility-code for other-packages.
;;
;; Whenever commands of other packages are not fully compatible with ECB then
;; this library should contain the necessary code to make it fully compatible
;; - or at least working acceptable.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

(eval-when-compile
  (require 'silentcomp))


(require 'ecb-util)
(require 'ecb-common-browser)
(require 'ecb-layout)

;; To add compatibilty code for packages just do:
;;
;; 1. Add the needed advice(s) to `ecb-compatibility-advices'
;; 2. Add the advice-code below.
;;
;; All advices of `ecb-compatibility-advices' will be autom. enabled when ECB
;; starts and autom. disabled when ECB shuts down. No advice is enabled just
;; by loading the ECB-library!

(defecb-advice-set ecb-compatibility-advices
  "Contains all advices needed for package-compatibility.")

;; package bs.el ----------------------------------------------------------

(defecb-advice bs-show before ecb-compatibility-advices
  "Ensures `bs-show' works well when called from another window as an
edit-window. Does nothing if called in another frame as the `ecb-frame'."
  (when (equal (selected-frame) ecb-frame)
    (unless (ecb-point-in-edit-window-number)
      (ecb-select-edit-window))
    ;; now we handle if bs-show should always display in the compile-window
    (let ((my-bs-buffer (get-buffer-create "*buffer-selection*")))
      ;; ecb-compilation-buffer-p needs a living buffer!
      (when (and (ecb-compilation-buffer-p my-bs-buffer)
                 ecb-compile-window-height)
        (display-buffer (buffer-name my-bs-buffer))))))

;; package electric.el ------------------------------------------------------


(defecb-advice one-window-p around ecb-always-disabled-advices
  "If called for the `ecb-frame' is only returns not nil if there is exactly
one edit-window. Neither the ecb-windows nor the compile-window nor the
minibuffer-window are considered. This adviced version of `one-window-p' is
not for direct usage therefore it's added to `ecb-always-disabled-advices' and
therefore it's always disabled\; use the macro `ecb-with-ecb-advice' instead
if you need this adviced version of `one-window-p'!"
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame))
      (setq ad-return-value
            (= (length (ecb-canonical-edit-windows-list)) 1))
    ad-do-it))

(defecb-advice Electric-pop-up-window around ecb-compatibility-advices
  "Ensures that the electric-* commands \(e.g. `electric-buffer-list') work
well with ECB. If BUFFER is a \"compilation-buffer\" in the sense of
`ecb-compilation-buffer-p' then BUFFER will be displayed in the compile-window
of ECB - if there is any. If the compile-window is temporally hidden then the
BUFFER is displayed in an edit-window!"
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame))
      (if (and (ecb-compilation-buffer-p (ad-get-arg 0))
               (equal (ecb-compile-window-state) 'visible))
          (pop-to-buffer (ad-get-arg 0))
        (let ((ecb-compilation-buffer-names nil)
              (ecb-compilation-major-modes nil)
              (ecb-compilation-predicates nil))
          (ecb-with-ecb-advice 'one-window-p 'around
            ad-do-it)))
    ad-do-it))

(defecb-advice electric-command-history before ecb-compatibility-advices
  "Ensures that the electric-* commands work well with ECB."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (ecb-point-in-dedicated-special-buffer))
    (ecb-select-edit-window)))

(defecb-advice electric-buffer-list before ecb-compatibility-advices
  "Ensures that the electric-* commands work well with ECB."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (ecb-point-in-dedicated-special-buffer))
    (ecb-select-edit-window)))

(defecb-advice electric-buffer-list after ecb-compatibility-advices
  "Ensures that the electric-* commands work well with ECB."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (if (get-buffer "*Buffer List*")
        (bury-buffer (get-buffer "*Buffer List*")))))

;; package master.el (only Emacs >= 22.X) ------------------------------------

;; The adviced version of switch-to-buffer-other-window can redraw the layout
;; (e.g. if the buffer in the compile-window is the slave and the
;; compile-window has been made visible), so <window> in the code below can be
;; a destroyed window-object! we have to prevent from this (e.g. by selecting
;; the window before by number).
(when-ecb-running-emacs
 (defecb-advice master-says around ecb-compatibility-advices
   "Makes the function compatible with ECB."
   (if (or (not ecb-minor-mode)
           (not (equal (selected-frame) ecb-frame)))
       (ecb-with-original-basic-functions ad-do-it)
     (if (null (buffer-live-p (get-buffer master-of)))
         (error "Slave buffer has disappeared")
       (let ((window  (selected-window))
             (point-loc (ecb-where-is-point))
             (p (point)))
         (if (not (eq (window-buffer window) (get-buffer master-of)))
             (switch-to-buffer-other-window master-of))
         (if (ad-get-arg 0)
             (condition-case nil
                 (apply (ad-get-arg 0) (ad-get-arg 1))
               (error nil)))
         (select-window (case (car point-loc)
                          (ecb
                           (ecb-get-ecb-window-by-number (cdr point-loc)))
                          (edit
                           (ecb-get-edit-window-by-number (cdr point-loc)))
                          (compile
                           ecb-compile-window)
                          (minibuf
                           (minibuffer-window ecb-frame))
                          (other-dedicated
                           (ecb-get-window-by-number (cdr point-loc)))))
         (goto-char (point))))))
   )

;; package scroll-all.el --------------------------------------------------


(defecb-advice count-windows around ecb-always-disabled-advices
  "If the selected frame is the ecb-frame and `scroll-all-mode' is not nil
then return the current number of edit-windows if point is in an edit-window
and always return 1 if point is not in an edit-window. In any other frame or
if `scroll-all-mode' is nil return the number of visible windows."
  (if (and (equal (selected-frame) ecb-frame)
           ecb-minor-mode
           (boundp 'scroll-all-mode)
           scroll-all-mode)
      (setq ad-return-value (if (ecb-point-in-edit-window-number)
                                (length (ecb-canonical-edit-windows-list))
                              1))
    (ecb-with-original-basic-functions
     ad-do-it)))

(defecb-advice scroll-all-function-all around ecb-compatibility-advices
  "Make it compatible with ECB."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions ad-do-it)
    (let (;; This runs the `other-window'-calls in the body in the right mode
          (ecb-other-window-behavior 'only-edit))
      (ecb-with-ecb-advice 'count-windows 'around
        ad-do-it))))


;; package tmm.el --------------------------------------------------------

;; Klaus Berndl <klaus.berndl@sdm.de>: We can not use our
;; Electric-pop-up-window advice instaed of this advice because otherwise
;; some commands of the popup-menus of the ecb-buffers would not work - this
;; comes from the save-window-excursion in the the tmm.
(when-ecb-running-emacs
 (defecb-advice tmm-prompt around ecb-compatibility-advices
   "Make it compatible with ECB."
   (if (or (not ecb-minor-mode)
           (not (equal (selected-frame) ecb-frame)))
       (ecb-with-original-basic-functions ad-do-it)
     ;; we set temporally `ecb-other-window-behavior' to a function which
     ;; always selects the "next" window after the
     ;; `ecb-last-edit-window-with-point'
     (let ((ecb-other-window-behavior
            (lambda (win-list edit-win-list ecb-win-list comp-win
                              mini-win point-loc nth-win)
              (ecb-next-listelem edit-win-list
                                 ecb-last-edit-window-with-point)))
           ;; we must not handle the tmm-stuff as compilation-buffer
           (ecb-compilation-buffer-names nil)
           (ecb-compilation-major-modes nil)
           (ecb-compilation-predicates nil))
       ad-do-it)))
 )

;; ediff-stuff ---------------------------------------------------------------

(silentcomp-defun ediff-cleanup-mess)
(silentcomp-defvar ediff-quit-hook)

;; (defecb-advice ediff-setup-windows around ecb-compatibility-advices
;;   "Ediff can manage all its windows with deactivated ECB-advices.

;; This is possible because we have in the setup-hook cleared the whole ecb-frame."
;;   (if (and (boundp 'ecb-minor-mode)
;;            ecb-minor-mode
;;            (eq (selected-frame) ecb-frame))
;;       (ecb-with-original-basic-functions
;;        ad-do-it)
;;     ad-do-it))

(defvar ecb-before-ediff-window-config nil)

;; We must not add this function to `ediff-before-setup-windows-hook' because
;; this hook is called very often - see docu. The hook
;; `ediff-before-setup-hook' is called only once - so it can be used to store
;; window-configs!
(defun ecb-ediff-before-setup-hook ()
  "Special ecb-setup before starting ediff."
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame))
      (progn
        (setq ecb-before-ediff-window-config (ecb-current-window-configuration))
        (if ecb-run-ediff-in-ecb-frame
            ;; !!!! we must delete all ECB-windows and the compile-window so
            ;; ediff can manage the whole ecb-frame concerning its windows!
            ;; This is the reason why we can advice `ediff-setup-windows' so
            ;; it runs with all original layout basic functions (especially
            ;; delete-other-window is necessary!)
            (progn
              (ecb-toggle-ecb-windows -1)
              (ecb-toggle-compile-window -1))
          (if (not ecb-windows-hidden)
              (delete-other-windows (car (ecb-canonical-edit-windows-list))))))
    (setq ecb-before-ediff-window-config nil)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Should we add this function to
;; `ediff-suspend-hook' too?! We should add something but this functions is
;; not perfectly....in general suspending ediff need some work here...
(defun ecb-ediff-quit-hook ()
  "Added to the end of `ediff-quit-hook' during ECB is activated. It
does all necessary after finishing ediff."
  (when ecb-minor-mode
    (if (and (not (equal (selected-frame) ecb-frame))
             (y-or-n-p
              "Ediff finished. Do you want to delete the extra ediff-frame? "))
        (delete-frame (selected-frame) t))
    (select-frame ecb-frame)
    (when ecb-before-ediff-window-config
      (ecb-set-window-configuration ecb-before-ediff-window-config)
      (setq ecb-before-ediff-window-config nil))))

(defun ecb-activate-ediff-compatibility ()
  (if (boundp 'ediff-quit-hook)
      (put 'ediff-quit-hook 'ecb-ediff-quit-hook-value
           ediff-quit-hook))
  (add-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (add-hook 'ediff-quit-hook 'ecb-ediff-quit-hook t)
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: suspending ediff and
  ;; especially reactivating does currently not really work well...
  ;; (add-hook 'ediff-suspend-hook 'ecb-ediff-quit-hook t)
  (add-hook 'ediff-before-setup-hook
            'ecb-ediff-before-setup-hook))

(defun ecb-deactivate-ediff-compatibility ()
  (if (get 'ediff-quit-hook 'ecb-ediff-quit-hook-value)
      (setq ediff-quit-hook (get 'ediff-quit-hook
                                 'ecb-ediff-quit-hook-value))
    (remove-hook 'ediff-quit-hook 'ecb-ediff-quit-hook))
  (remove-hook 'ediff-before-setup-hook
               'ecb-ediff-before-setup-hook))


;; view-stuff --------------------------------------------------------------------

;; The code of the view-package of GNU Emacs has to be advices when the
;; view-buffer is displayed in the compile-window of ECB.
;; The much simpler view-mechanism of XEmacs (view-less.el) should work out of
;; the box.

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: for Emacs 22 we need also a
;; workaround when no compile-window is active because the return-to-alist
;; stuff in Emacs 22 is not really smart and does not work with layout like
;; ECB - with Emacs 23 it works perfectly without a compile-window.
;; In Emacs 22 the return-to-alist contains in case of a window-layout with
;; more than two windows quit-window which switches the buffer after quiting
;; (which is not what we want); with deactivated ECB and more than 2 windows
;; the same dump behavior - but unfortunatelly ECB contains always more than
;; two windows (at least in by far most cases), so current view-mode-exit
;; stuff wil not work with Emacs 22 and ECB ==> we simplify the logic in the
;; following case:
;; - Emacs 22 is running and
;; - there is no compile-window:
;; - current buffer is in view-mode
;; then we just delete the current-window (for which view-mode-exit is called)
;; and select ecb-last-edit-window-with-point (hmm, is this possible - this
;; one will be the deleted one...how to go back to that window we have to go
;; back??

(when-ecb-running-emacs
 (defecb-advice view-mode-exit around ecb-compatibility-advices
   "Makes view-mode compatible with ECB.

If there is no compile-window \(i.e. the buffer with view-mode is not
displayed in the special compile-window of ECB) then nothing special is done
but the original `view-mode-exit' is performed.

If the view-buffer is displayed in the compile-window \(i.e. this function is
called from within the compile-window) then the whole window-management stuff
of view-mode is disabled only `view-no-disable-on-exit' is taken into acount.

The compile-window will be shrinked down with
`ecb-toggle-compile-window-height' and the last edit-window with point will be
selected afterwards."
   (if (and (boundp 'ecb-minor-mode)
            ecb-minor-mode
            (eq (selected-frame) ecb-frame)
            (eq (selected-window) ecb-compile-window))
       (when view-mode
         (or view-no-disable-on-exit
             (view-mode-disable))
;;          (when (ad-get-arg 1) ;; = exit-action
;;            (setq view-exit-action nil)
;;            (funcall (ad-get-arg 1) (current-buffer)))
         (force-mode-line-update)
         (ecb-toggle-compile-window-height -1)
         (select-window ecb-last-edit-window-with-point))
     ad-do-it))
)


;; not yet done ----------------------------------------------------------------

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
;; *** The new package gdb-ui.el provides an enhanced graphical interface to
;; GDB. You can interact with GDB through the GUD buffer in the usual way, but
;; there are also further buffers which control the execution and describe the
;; state of your program. It separates the input/output of your program from
;; that of GDB and watches expressions in the speedbar. It also uses features
;; of Emacs 21 such as the display margin for breakpoints, and the toolbar.
;; This is new in Emacs 21.4 so maybe we have to make it compatible with ECB!
;; But maybe this could be hard because AFAIK gdb-ui.el uses dedicated
;; windows!




;; we disable the advices at load-time
(ecb-disable-advices 'ecb-compatibility-advices t)

(silentcomp-provide 'ecb-compatibility)

;;; ecb-compatibility.el ends here

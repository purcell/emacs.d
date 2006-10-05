;;; ecb-compatibility.el --- ECB-compatibility for other packages

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
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

;; $Id: ecb-compatibility.el,v 1.6 2005/03/30 12:50:55 berndl Exp $

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
(require 'ecb-layout)

;; To add compatibilty code for packages just do:
;;
;; 1. Add the needed advice(s) to `ecb-compatibility-advices'
;; 2. Add the advice-code below.
;;
;; All advices of `ecb-compatibility-advices' will be autom. enabled when ECB
;; starts and autom. disabled when ECB shuts down. No advice is enabled just
;; by loading the ECB-library!

(defvar ecb-compatibility-advices '((bs-show . before)
                                    (Electric-pop-up-window . around)
                                    (electric-command-history . before)
                                    (electric-buffer-list . before)
                                    (electric-buffer-list . after))
  "Contains all advices needed for package-compatibility.")

(defadvice bs-show (before ecb)
  "Ensures `bs-show' works well when called from another window as an
edit-window. Does nothing if called in another frame as the `ecb-frame'."
  (when (equal (selected-frame) ecb-frame)
    (unless (ecb-point-in-edit-window)
      (ecb-select-edit-window))
    ;; now we handle if bs-show should always display in the compile-window
    (let ((my-bs-buffer (get-buffer-create "*buffer-selection*")))
      ;; ecb-compilation-buffer-p needs a living buffer!
      (when (and (ecb-compilation-buffer-p my-bs-buffer)
                 ecb-compile-window-height)
        (ecb-with-adviced-functions
         (display-buffer (buffer-name my-bs-buffer)))))))

(defadvice Electric-pop-up-window (around ecb)
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

(defadvice electric-command-history (before ecb)
  "Ensures that the electric-* commands work well with ECB."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (ecb-point-in-dedicated-special-buffer))
    (ecb-select-edit-window)))

(defadvice electric-buffer-list (before ecb)
  "Ensures that the electric-* commands work well with ECB."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (ecb-point-in-dedicated-special-buffer))
    (ecb-select-edit-window)))

(defadvice electric-buffer-list (after ecb)
  "Ensures that the electric-* commands work well with ECB."
  (if (get-buffer "*Buffer List*")
      (bury-buffer (get-buffer "*Buffer List*"))))

;; we disable the advices at load-time
(ecb-disable-advices ecb-compatibility-advices)

(silentcomp-provide 'ecb-compatibility)

;;; ecb-compatibility.el ends here

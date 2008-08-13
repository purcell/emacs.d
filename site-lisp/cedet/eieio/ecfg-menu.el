;;; ecfg-menu.el - configureation menu
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; RCS: $Id: ecfg-menu.el,v 1.3 1997/01/18 16:51:22 zappo Exp $
;;; Keywords: OO, dialog, configure
;;;                                                                          
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;      
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;      
;;; ecfg-menu can be found in the eieio or etalk distributions on:
;;;  ftp://ftp.ultranet.com/pub/zappo
;;;
;;; Commentary:
;;;   Maintains a menu in the HELP menu for editing different sets of
;;; options.
;;;           

(defvar econfig-keymap nil
  "Keymap used under emacs config menu")

(defvar econfig-color-keymap nil
  "Submenu keymap for editing different color sets")

(defvar econfig-mail-keymap nil
  "Submenu keymap for editing mail specific paramters.")

(if econfig-keymap
    nil
  (setq econfig-keymap (make-sparse-keymap))
  (define-key global-map [menu-bar help-menu configure ] 
    (cons "Configure" econfig-keymap))
  (if econfig-color-keymap
      nil
    (setq econfig-color-keymap (make-sparse-keymap))
    (define-key econfig-color-keymap [ font-lock-config ]
      '("Font Lock Options" . econfig-flock-options))
    (define-key econfig-color-keymap [ font-lock-color ]
      '("Font Lock Colors" . econfig-font-lock-faces))
    (define-key econfig-color-keymap [ calendar-color ]
      '("Calendar Colors" . econfig-calendar-faces))
    (define-key econfig-color-keymap [ info-color ]
      '("Info Colors" . econfig-info-faces))
    (define-key econfig-color-keymap [ dialog-color ]
      '("Dialog Colors" . dlg-widget-faces))
    (define-key econfig-color-keymap [ basic-color ]
      '("Basic Colors" . dlg-faces)))
  (if econfig-mail-keymap
      nil
    (setq econfig-mail-keymap (make-sparse-keymap))
    (define-key econfig-mail-keymap [ citation ]
      '("Citation" . econfig-supercite))
    (define-key econfig-mail-keymap [ mail ]
      '("Mail" . econfig-mail))
    (define-key econfig-mail-keymap [ rmail ]
      '("RMail" . econfig-rmail))
    )
  (define-key econfig-keymap [ color ] 
    (cons "Face Colors" econfig-color-keymap))
  (define-key econfig-keymap [ email ] 
    (cons "E-mail" econfig-mail-keymap))
  (define-key econfig-keymap [ ange-ftp ]
    '("Ange FTP" . econfig-ange-ftp))
  (define-key econfig-keymap [ ps-print ]
    '("Postscript Printing" . econfig-ps-print))
  (define-key econfig-keymap [ calendar ]
    '("Calendar" . econfig-calendar))
  (define-key econfig-keymap [ bookmarks ]
    '("Bookmarks" . econfig-bookmarks))
  (define-key econfig-keymap [ ispell ]
    '("Ispell" . econfig-ispell))
  (define-key econfig-keymap [ dired ]
    '("Dired" . econfig-dired))
  (define-key econfig-keymap [ programs ]
    '("Program Editing" . econfig-programmer))
  (define-key econfig-keymap [ editing ]
    '("Editing" . econfig-editing))
  (define-key econfig-keymap [ interface ]
    '("Interface" . econfig-interface))
)

(autoload 'econfig-interface "e-config" "configuration dialog" t)
(autoload 'econfig-editing "e-config" "configuration dialog" t)
(autoload 'econfig-ange-ftp "e-config" "configuration dialog" t)
(autoload 'econfig-programmer "e-config" "configuration dialog" t)
(autoload 'econfig-calendar "e-config" "configuration dialog" t)
(autoload 'econfig-bookmarks "e-config" "configuration dialog" t)
(autoload 'econfig-dired "e-config" "configuration dialog" t)
(autoload 'econfig-ispell "e-config" "configuration dialog" t)
(autoload 'econfig-supercite "e-config" "configuration dialog" t)
(autoload 'econfig-mail "e-config" "configuration dialog" t)
(autoload 'econfig-rmail "e-config" "configuration dialog" t)
(autoload 'econfig-font-lock-faces "e-config" "configuration dialog" t)
(autoload 'econfig-info-faces "e-config" "configuration dialog" t)
(autoload 'econfig-calendar-faces "e-config" "configuration dialog" t)
(autoload 'econfig-ps-print "e-config" "configuration dialog" t)
(autoload 'econfig-flock-options "e-config" "configuration dialog" t)

(autoload 'dlg-faces "dlg-config" "configuration dialog" t)
(autoload 'dlg-widget-faces "dlg-config" "configuration dialog" t)

;;; end of lisp
(provide 'ecfg-menu)

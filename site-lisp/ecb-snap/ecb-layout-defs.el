;;; ecb-layout-defs.el --- layout definitions for ECB

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

;; $Id: ecb-layout-defs.el,v 1.21 2005/06/27 17:00:46 berndl Exp $

;;; Commentary:
;;
;; Contains all layout definitions for ECB
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)
(require 'ecb-layout)

;; ========= Current available layouts ===============================

;; Here come all the index layout-functions:

;; Layout left1 -----------------------------------------------------

(ecb-layout-define "left1" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |      |       |                                      |
   | Sour | Hist  |                 Edit                 |
   |      |       |                                      |
   |      |       |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer)
  (select-window (previous-window))
  (ecb-split-hor 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window (next-window))))

;; Layout left2 -----------------------------------------------------

(ecb-layout-define "left2" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-sources-buffer)
  (select-window (next-window)))

;; Layout left3 -----------------------------------------------------

(ecb-layout-define "left3" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Layout left4 -----------------------------------------------------

(ecb-layout-define "left4" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |      |       |                                      |
   |      |       |                                      |
   |      |       |                                      |
   | Sour | Hist  |                                      |
   |      |       |                                      |
   |      |       |                                      |
   |      |       |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-sources-buffer)
  (ecb-split-hor 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window)))

;; Layout left5 -----------------------------------------------------

(ecb-layout-define "left5" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  History     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window)))

;; Layout right1 -----------------------------------------------------

(ecb-layout-define "right1" right
  "This function creates the following layout:

   -------------------------------------------------------
   |                                      |              |
   |                                      |  Directories |
   |                                      |              |
   |                                      |              |
   |                                      |--------------|
   |                                      |              |
   |                                      |              |
   |             Edit                     |  Sources     |
   |                                      |              |
   |                                      |              |
   |                                      |--------------|
   |                                      |              |
   |                                      |  Methods     |
   |                                      |              |
   |                                      |              |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (let ((edit-win (previous-window (selected-window) 0)))
    (ecb-set-directories-buffer)
    (ecb-split-ver 0.3)
    (ecb-set-sources-buffer)
    (ecb-split-ver 0.5)
    (ecb-set-methods-buffer)
    (select-window edit-win)))

;; Layout right2 -----------------------------------------------------

(ecb-layout-define "right2" right
  "This function creates the following layout:

   -------------------------------------------------------
   |                                      |              |
   |                                      |              |
   |                                      |              |
   |                                      |  Directories |
   |                                      |              |
   |                                      |              |
   |                                      |              |
   |             Edit                     |--------------|
   |                                      |              |
   |                                      |              |
   |                                      |              |
   |                                      |  Methods     |
   |                                      |              |
   |                                      |              |
   |                                      |              |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (let ((edit-win (previous-window (selected-window) 0)))
    (ecb-set-directories-buffer)
    (ecb-split-ver 0.5)
    (ecb-set-methods-buffer)
    (select-window edit-win)))

;; Layout left6 -----------------------------------------------------

(ecb-layout-define "left6" left
  "This function creates the following layout:

   -------------------------------------------------------
   |  Sources     |                                      | 
   |--------------|                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      | 
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  History     |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.2)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.75)
  (ecb-set-history-buffer)
  (select-window (next-window)))

;; Layout top1 -----------------------------------------------------

(ecb-layout-define "top1" top
  "This function creates the following layout:

   -------------------------------------------------------
   |                        |             |              |
   |                        |             |              |
   |      Directories       |  Sources    |  Methods     |
   |                        |             |              |
   |                        |             |              |
   |-----------------------------------------------------|
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   |                    Edit                             |
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-hor 0.5)
  (ecb-set-sources-buffer)
  (ecb-split-hor 0.5)
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Layout left7 -----------------------------------------------------

(ecb-layout-define "left7" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  History     |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`ecb-show-sources-in-directories-buffer'!"
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.6)
  (ecb-set-history-buffer)
  (ecb-split-ver 0.4)
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Layout left8 -----------------------------------------------------

(ecb-layout-define "left8" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  History     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.35)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.65)
  (ecb-set-history-buffer)
  (select-window (next-window)))

;; Layout top2 -----------------------------------------------------

(ecb-layout-define "top2" top
  "This function creates the following layout:

   -------------------------------------------------------
   |                                                     |
   |                                                     |
   |                    Methods                          |
   |                                                     |
   |                                                     |
   |-----------------------------------------------------|
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   |                    Edit                             |
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Layout left9 -----------------------------------------------------

(ecb-layout-define "left9" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |   Methods    |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Layout left10 -----------------------------------------------------

(ecb-layout-define "left10" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  Sou | Hist  |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then the
layout contains no persistent compilation window and the other windows get a little
more place."
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.75)
  (ecb-set-sources-buffer)
  (ecb-split-hor 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window)))

;; Layout left11 -----------------------------------------------------

(ecb-layout-define "left11" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |    Hist      |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then the
layout contains no persistent compilation window and the other windows get a little
more place."
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.75)
  (ecb-set-history-buffer)
  (select-window (next-window)))

;; Layout left12 -----------------------------------------------------

(ecb-layout-define "left12" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |   History    |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-history-buffer)
  (select-window (next-window)))

;; Layout left13 -----------------------------------------------------

(ecb-layout-define "left13" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   | Directories  |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`ecb-show-sources-in-directories-buffer'!"
  (ecb-set-directories-buffer)
  (select-window (next-window)))

;; Layout left14 -----------------------------------------------------

(ecb-layout-define "left14" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   | Directories  |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |    Hist      |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`ecb-show-sources-in-directories-buffer'!"
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.75)
  (ecb-set-history-buffer)
  (select-window (next-window)))

;; Layout left15 -----------------------------------------------------

(ecb-layout-define "left15" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`ecb-show-sources-in-directories-buffer'!"
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Layout leftright1 -----------------------------------------------------

(ecb-layout-define "leftright1" left-right
 "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |             |
   |              |                               |             |
   |  Sources     |                               |             |
   |              |                               |             |
   |--------------|                               |             |
   |              |                               |             |
   |  History     |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
 (ecb-set-directories-buffer)
 (ecb-split-ver 0.4)
 (ecb-set-sources-buffer)
 (ecb-split-ver 0.5)
 (ecb-set-history-buffer)
 (select-window (next-window (next-window)))
 (ecb-set-methods-buffer)
 (select-window (previous-window (selected-window) 0)))

;; Layout leftright2 -----------------------------------------------------

(ecb-layout-define "leftright2" left-right
 "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |-------------|
   |              |                               |             |
   |  Sources     |                               |  History    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
 (ecb-set-directories-buffer)
 (ecb-split-ver 0.66)
 (ecb-set-sources-buffer)
 (select-window (next-window (next-window)))
 (ecb-set-methods-buffer)
 (ecb-split-ver 0.66)
 (ecb-set-history-buffer)
 (select-window (previous-window (previous-window (selected-window) 0) 0)))

;; Layout leftright3 -----------------------------------------------------

(ecb-layout-define "leftright3" left-right
  "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |             Edit              |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (select-window (next-window (next-window)))
  (ecb-set-methods-buffer)
  (select-window (previous-window (selected-window) 0)))


(ecb-layout-define "left-dir-plus-speedbar" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Speedbar    |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`ecb-show-sources-in-directories-buffer'!"
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-speedbar-buffer)
  (select-window (next-window)))

(ecb-layout-define "left-analyse" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Analyse     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.35)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-analyse-buffer)
  (select-window (next-window)))

(ecb-layout-define "leftright-analyse" left-right
  "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |-------------|
   |              |                               |             |
   |  Sources     |                               |             |
   |              |                               |             |
   |--------------|                               |  Analyse    |
   |              |                               |             |
   |  History     |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.4)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window (next-window)))
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-analyse-buffer)
  (select-window (previous-window (previous-window (selected-window) 0) 0)))

(ecb-layout-define "left-symboldef" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Symbol-defs |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.35)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-symboldef-buffer)
  (select-window (next-window)))

(defconst ecb-buildin-layouts (ecb-copy-list ecb-available-layouts)
  "All layouts defined until now.")

(silentcomp-provide 'ecb-layout-defs)

;;; ecb-layout-defs.el ends here
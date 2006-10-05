;;; ecb-cycle.el --- cycle buffers through ecb windows.

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

;; $Id: ecb-cycle.el,v 1.28 2005/06/20 14:34:20 berndl Exp $

;;; Commentary:

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:

;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; TODO:
;;
;; - What is the pattern we should use for cycling through other windows?
;;
;;   - ecb-cycle-through-X-buffers (select the next X buffer)
;;   - ecb-cycle-switch-to-X-buffer (set the X buffer using completion)
;;
;; - How do we setup the menubar?
;;
;;          - ECB
;;                Cycle
;;                     - Forward Compilation Buffer
;;                     - Set Compilation Buffer
;;
;; - What do we use for key bindings?
;;
;; - We need an easier way to setup completion and a better way to get the
;;   index.
;;
;; - If possible, try to put fit the buffer so that the end of buffer is at the
;; end of the window... if necessary.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-compilation)
(require 'ecb-layout)

(defgroup ecb-cycle nil
  "Setting for cycling through misc ECB buffers."
  :group 'ecb
  :prefix "ecb-cycle-")


(defun ecb-cycle-through-compilation-buffers(&optional choose-buffer)
  "Cycle through all compilation buffers currently open.
The choosen compilation buffer is displayed within the compilation window
`ecb-compile-window' \(if this window doesn't exist then an error is
displayed). If the currently opened buffer within the compilation window is
not a compilation buffer, we jump to the first compilation buffer. If not we
try to loop through all compilation buffers. If we hit the end we go back to
the beginning.

If CHOOSE-BUFFER is not nil then the user will be prompted for the
compilation-buffer to switch to.

Afterwards always the compile-window of ECB is selected."

  (interactive "P")
  (if (not (numberp ecb-compile-window-height))
      (ecb-error "This command needs a persistent compile window!")
    (if choose-buffer
        (ecb-with-adviced-functions
         (switch-to-buffer (completing-read "ECB compilation buffer: "
                                            (ecb-compilation-get-buffers))))
      
      (let* ((compilation-buffers (ecb-compilation-get-buffers))
             ;; This works even if ecb-compile-window is nil or not alive
             ;; (means temporally hidden) --> then current-buffer is the
             ;; buffer of the currently selected window!
             (current-buffer (or (and (ecb-compile-window-live-p)
                                      (window-buffer ecb-compile-window))
                                 (current-buffer)))
             (current-buffer-name (buffer-name current-buffer))
             (current nil)
             (index nil))
        (when (null compilation-buffers)
          (ecb-error "No compilation buffers available."))
        
        (if (not (ecb-compilation-buffer-p current-buffer))
            ;;if the current buffer is not a compilation buffer, goto the first
            ;;compilation buffer.
            
            (ecb-cycle-set-compilation-buffer 0 compilation-buffers)
          
          ;;else... we need to determine what buffer to display.
          
          (setq current (assoc current-buffer-name compilation-buffers))
          
          (setq index (cdr current))
          
          (if (= (1+ index) (length compilation-buffers))
              ;;go back to the first buffer.
              (ecb-cycle-set-compilation-buffer 0 compilation-buffers)
            (ecb-cycle-set-compilation-buffer (1+ index)
                                              compilation-buffers)))))))
  

(defun ecb-cycle-set-compilation-buffer(index compilation-buffers)
  "Set the buffer in the compilation window."

  (let ((buffer-name (car (nth index compilation-buffers))))
    (ecb-with-adviced-functions
     (switch-to-buffer buffer-name))))


(silentcomp-provide 'ecb-cycle)

;;; ecb-cycle.el ends here

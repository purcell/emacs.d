;;; sunrise-x-old-checkpoints.el --- backward compatibility checkpoint functions for the Sunrise Commander File Manager

;; Copyright (C) 2009-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 28 Dec 2009
;; Version: 1
;; RCS Version: $Rev: 449 $
;; Keywords: sunrise commander, old checkpoints
;; URL: http://www.emacswiki.org/emacs/sunrise-x-old-checkpoints.el
;; Compatibility: GNU Emacs 22

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more de-
;; tails.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Beginning with version 4 of the Sunrise Commander, checkpoints were redefined
;; to be a special form of bookmarks. Unfortunately, creating bookmarks with
;; custom handlers isn't supported in the version of bookmarks.el distributed
;; with Emacs 22, so if you use Sunrise checkpoints and you don't want to update
;; your bookmarks.el, just add this extension to your .emacs.el to get back the
;; original functionality.

;; This extension was written on GNU Emacs 23 on Linux, and tested on GNU Emacs
;; 22 and 23 for Linux and on EmacsW32 (version 22) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'. (Optionally) compile
;; it.

;; 2) Enjoy ;-) -- Sunrise should pick the correct extension automatically. On
;; Emacs 23 it will look for sunrise-x-checkpoints, while on Emacs 22 it'll try
;; to load sunrise-x-old-checkpoints. Only if you *really* want to use the old
;; extensions with a more recent version of bookmarks.el than the one bundled
;; with Emacs 22 you may add a new (require 'sunrise-x-old-checkpoints) to your
;; .emacs file somewhere after (require 'sunrise-commander).

;;; Code:

(require 'sunrise-commander)

(defvar sr-checkpoint-registry '(("~" "~/" "~/"))
  "Registry of currently defined checkpoints.")

(defun sr-checkpoint-save (&optional name)
  "Save the current Sunrise pane directories under NAME for later restoring."
  (interactive "sCheckpoint name to save? ")
  (let ((my-cell (assoc-string name sr-checkpoint-registry)))
    (sr-save-directories)
    (if (null my-cell)
        (setq sr-checkpoint-registry
              (cons (cons name (list sr-left-directory sr-right-directory))
                    sr-checkpoint-registry))
      (setcdr my-cell (list sr-left-directory sr-right-directory)))
  (message "%s" (concat "Checkpoint \"" name "\" saved"))))

(defun sr-checkpoint-restore (&optional name)
  "Restore a checkpoint previously saved under NAME."
  (interactive "sCheckpoint name to restore? " )
  (let* ((cp-list (assoc-string name sr-checkpoint-registry))
         (dirs-list (cdr cp-list)))
    (unless cp-list
      (error (concat "No such checkpoint: " name)))
    (if (eq sr-selected-window 'right)
        (setq dirs-list (reverse dirs-list)))
    (mapc (lambda (x) (sr-goto-dir x) (sr-change-window)) dirs-list)))

(defun sr-checkpoint-handler (&optional arg)
  "Dummy function for compatilibity with the new checkpoints interface."
  (ignore))

(provide 'sunrise-x-old-checkpoints)

;;; sunrise-x-old-checkpoints.el ends here

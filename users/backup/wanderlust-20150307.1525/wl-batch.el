;;; wl-batch.el --- batch functions for Wanderlust.

;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2003 Chihiro Kuroda <chee@iijmio-mail.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>,
;;         Chihiro Kuroda <chee@iijmio-mail.jp>
;; Keywords: mail, net news, batch

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;
;; You can use functions in this file from command line.
;; For example,
;;
;; % emacs -batch -l wl-batch -f wl-batch-prefetch

(require 'wl)

;;; Code:
(defgroup wl-batch nil
  "Wanderlust, batch processing."
  :prefix "wl-"
  :group 'wl)

(defcustom wl-batch-prefetch-folder-list nil
  "A list of folder name to prefetch by `wl-batch-prefetch'."
  :type '(repeat string)
  :group 'wl-batch)

(defcustom wl-batch-prefetch-max-number 100
  "Max number for summary update while prefetching."
  :type 'integer
  :group 'wl-batch)

(defun wl-batch-prefetch ()
  "A batch function to prefetch messages by Emacs batch process."
  (interactive)
  (wl 1)
  (let ((elmo-folder-update-threshold wl-batch-prefetch-max-number)
	wl-demo	elmo-folder-update-confirm
	wl-interactive-exit)
    (dolist (entity wl-batch-prefetch-folder-list)
      (wl-folder-check-entity entity)
      (wl-folder-prefetch-entity entity))
    (wl-exit)))

(provide 'wl-batch)

;;; wl-batch.el ends here

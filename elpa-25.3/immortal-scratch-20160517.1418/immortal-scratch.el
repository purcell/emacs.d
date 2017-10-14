;;; immortal-scratch.el --- respawn the scratch buffer when it's killed -*- lexical-binding: t -*-

;; Copyright (C) 2016 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Maintainer: Jonathan Kotta <jpkotta@gmail.com>
;; Version: 0.1
;; Package-Version: 20160517.1418

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a global minor mode `immortal-scratch-mode'
;; that causes the scratch buffer to respawn after it's killed.  To
;; kill it without respawning, use `immortal-scratch-kill' with a
;; non-nil argument.

;;; Code:

(defgroup immortal-scratch nil
  "Respawn scratch buffer when it's killed."
  :group 'convenience)

(defcustom immortal-scratch-switch-to-respawned-scratch nil
  "If non-nil, switch to the new scratch buffer when it is respawned."
  :group 'immortal-scratch)

(defun immortal-scratch-respawn ()
  "Create *scratch* buffer if it doesn't exist.

If `immortal-scratch-switch-to-respawned-scratch' is not nil,
switch to the newly created *scratch* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*scratch*")
    (with-temp-message ""
      (when (zerop (buffer-size))
        (insert initial-scratch-message)
        (set-buffer-modified-p nil)
        (funcall initial-major-mode))
      (when immortal-scratch-switch-to-respawned-scratch
        (switch-to-buffer "*scratch*")))))

(defun immortal-scratch-kill (&optional with-fire)
  "Add this to `kill-buffer-query-functions' to respawn scratch when it is killed.

When WITH-FIRE is not nil, kill the scratch buffer but do not respawn it."
  (interactive "P")
  ;; kill-buffer-query-functions are called with no arg, so with-fire
  ;; will always be nil in that case.
  (if (or (string= (buffer-name (current-buffer)) "*scratch*")
         with-fire)
      (let ((kill-buffer-query-functions kill-buffer-query-functions))
        (remove-hook 'kill-buffer-query-functions
                     #'immortal-scratch-kill)
        (kill-buffer "*scratch*")
        (unless with-fire
          (immortal-scratch-respawn))
        nil)
    t ;; not scratch
    ))

;;;###autoload
(define-minor-mode immortal-scratch-mode
  "When the scratch buffer is killed, immediately respawn it."

  :group 'immortal-scratch
  :global t
  :lighter ""

  (if immortal-scratch-mode
      (add-hook 'kill-buffer-query-functions #'immortal-scratch-kill)
    (remove-hook 'kill-buffer-query-functions #'immortal-scratch-kill)))

(provide 'immortal-scratch)

;;; immortal-scratch.el ends here

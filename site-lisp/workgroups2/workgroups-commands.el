;;; workgroups-commands --- main commands
;;; Commentary:
;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'ido)
  (require 'iswitchb))

(require 'ring)
(require 'workgroups-variables)
(require 'workgroups-utils-basic)
(require 'workgroups-pickel)
(require 'workgroups-functions)

;;; workgroup switching commands

(defun wg-switch-to-workgroup (workgroup &optional noerror)
  "Switch to WORKGROUP."
  (interactive (list (wg-read-workgroup-name)))
  ;; Set a parameter when using ECB
  (if (wg-current-workgroup t)
      (wg-set-workgroup-parameter (wg-current-workgroup t) 'ecb (and (boundp 'ecb-minor-mode)
                                                                     ecb-minor-mode)))
  (let ((workgroup (wg-get-workgroup-create workgroup))
        (current (wg-current-workgroup t)))
    (when (and (eq workgroup current) (not noerror))
      (error "Already on: %s" (wg-workgroup-name current)))
    (when current (push current wg-deactivation-list))
    (unwind-protect
        (progn
          ;; Before switching - turn off ECB
          ;; https://github.com/pashinin/workgroups2/issues/34
          (if (and (boundp 'ecb-minor-mode)
                   ecb-minor-mode
                   (equal ecb-frame (selected-frame)))
              (let ((ecb-split-edit-window-after-start 'before-deactivation))
                (ecb-deactivate)))

          (wg-restore-workgroup workgroup)
          (wg-set-previous-workgroup current)
          (wg-set-current-workgroup workgroup)

          ;; If a workgroup had ECB - turn it on
          (if (and (boundp 'ecb-minor-mode)
                   (not ecb-minor-mode)
                   (wg-workgroup-parameter (wg-current-workgroup t) 'ecb nil))
              (let ((ecb-split-edit-window-after-start 'before-deactivation))
                (ecb-activate)))

          (run-hooks 'wg-switch-to-workgroup-hook)
          (wg-fontified-message
            (:cmd "Switched: ")
            (wg-workgroup-name (wg-current-workgroup t))
            ))
      (when current (pop wg-deactivation-list)))))

(defun wg-switch-to-workgroup-other-frame (workgroup &optional n)
  "Switch to WORKGROUP in the frame N places cyclically from `selected-frame'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list (wg-read-workgroup-name) current-prefix-arg))
  (with-selected-frame (wg-cyclic-nth-from-frame (or n 1))
    (wg-switch-to-workgroup workgroup)))

(defun wg-switch-to-workgroup-at-index (index)
  "Switch to the workgroup at INDEX in `wg-workgroup-list'."
  (interactive (list (or current-prefix-arg (wg-read-workgroup-index))))
  (let ((wl (wg-workgroup-list-or-error)))
    (wg-switch-to-workgroup
     (or (nth index wl) (error "There are only %d workgroups" (length wl))))))

(cl-macrolet
    ((define-range-of-switch-to-workgroup-at-index (num)
       `(progn
          ,@(wg-docar (i (wg-range 0 num))
              `(defun ,(intern (format "wg-switch-to-workgroup-at-index-%d" i)) ()
                 ,(format "Switch to the workgroup at index %d." i)
                 (interactive)
                 (wg-switch-to-workgroup-at-index ,i))))))
  (define-range-of-switch-to-workgroup-at-index 10))

(defun wg-switch-to-cyclic-nth-from-workgroup (workgroup n)
  "Switch N workgroups cyclically from WORKGROUP in `wg-workgroup-list.'"
  (let ((workgroup-list (wg-workgroup-list-or-error))
        (workgroup (wg-get-workgroup workgroup t)))
    (wg-switch-to-workgroup
     (cond ((not workgroup) (car workgroup-list))
           ((= 1 (length workgroup-list)) (error "There's only one workgroup"))
           (t (wg-cyclic-nth-from-workgroup workgroup n))))))

(defun wg-switch-to-workgroup-left (&optional workgroup n)
  "Switch to the workgroup (- N) places from WORKGROUP in `wg-workgroup-list'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list nil current-prefix-arg))
  (wg-switch-to-cyclic-nth-from-workgroup workgroup (- (or n 1))))

(defun wg-switch-to-workgroup-right (&optional workgroup n)
  "Switch to the workgroup N places from WORKGROUP in `wg-workgroup-list'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list nil current-prefix-arg))
  (wg-switch-to-cyclic-nth-from-workgroup workgroup (or n 1)))

(defun wg-switch-to-workgroup-left-other-frame (&optional n)
  "Like `wg-switch-to-workgroup-left', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-cyclic-nth-from-frame (or n 1))
    (call-interactively 'wg-switch-to-workgroup-left)))

(defun wg-switch-to-workgroup-right-other-frame (&optional n)
  "Like `wg-switch-to-workgroup-right', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-cyclic-nth-from-frame (or n -1))
    (call-interactively 'wg-switch-to-workgroup-right)))

(defun wg-switch-to-previous-workgroup ()
  "Switch to the previous workgroup."
  (interactive)
  (wg-switch-to-workgroup (wg-previous-workgroup)))



;;; workgroup creation commands

(defun wg-create-workgroup (name &optional blank)
  "Create and add a workgroup named NAME.
Optional argument BLANK non-nil (set interactively with a prefix
arg) means use a blank, one window window-config.  Otherwise use
the current window-configuration.  Keep in mind that even though
the current window-config may be used, other parameters of the
current workgroup are not copied to the created workgroup.  For
that, use `wg-clone-workgroup'."
  (interactive (list (wg-read-new-workgroup-name) current-prefix-arg))
  (wg-switch-to-workgroup (wg-make-and-add-workgroup name blank))
  (wg-fontified-message
    (:cmd "Created: ")
    (:cur name) "  "
    (wg-workgroup-list-display)))

(defun wg-clone-workgroup (workgroup name)
  "Create and add a clone of WORKGROUP named NAME.
Keep in mind that only WORKGROUP's top-level alist structure is
copied, so destructive operations on the keys or values of
WORKGROUP will be reflected in the clone, and vice-versa.  Be
safe -- don't mutate them."
  (interactive (list nil (wg-read-new-workgroup-name)))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (clone (wg-copy-workgroup workgroup)))
    (setf (wg-workgroup-name clone) name
          (wg-workgroup-uid clone) (wg-generate-uid))
    (when (wg-check-and-add-workgroup clone)
      (wg-flag-workgroup-modified clone))
    (wg-set-workgroup-working-wconfig
     clone (wg-workgroup-working-wconfig workgroup))
    (wg-switch-to-workgroup clone)
    (wg-fontified-message
      (:cmd "Cloned: ")
      (:cur (wg-workgroup-name workgroup))
      (:msg " to ")
      (:cur name) "  "
      (wg-workgroup-list-display))))



;;; workgroup killing commands

(defun wg-wconfig-kill-ring ()
  "Return `wg-wconfig-kill-ring', creating it first if necessary."
  (or wg-wconfig-kill-ring
      (setq wg-wconfig-kill-ring (make-ring wg-wconfig-kill-ring-max))))

(defun wg-add-to-wconfig-kill-ring (wconfig)
  "Add WCONFIG to `wg-wconfig-kill-ring'."
  (ring-insert (wg-wconfig-kill-ring) wconfig))

(defun wg-kill-workgroup (&optional workgroup)
  "Kill WORKGROUP, saving its working-wconfig to the kill ring."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (to (or (wg-previous-workgroup t)
                 (wg-cyclic-nth-from-workgroup workgroup))))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-working-wconfig workgroup))
    (wg-delete-workgroup workgroup)
    (if (eq workgroup to) (wg-restore-wconfig (wg-make-blank-wconfig))
      (wg-switch-to-workgroup to))
    (wg-fontified-message
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup)) "  "
      (wg-workgroup-list-display))))

(defun wg-kill-ring-save-base-wconfig (&optional workgroup)
  "Save WORKGROUP's base wconfig to the kill ring."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-base-wconfig workgroup))
    (wg-fontified-message
      (:cmd "Saved: ")
      (:cur (wg-workgroup-name workgroup))
      (:cur "'s ")
      (:msg "base wconfig to the kill ring"))))

(defun wg-kill-ring-save-working-wconfig (&optional workgroup)
  "Save WORKGROUP's working-wconfig to `wg-wconfig-kill-ring'."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-working-wconfig workgroup))
    (wg-fontified-message
      (:cmd "Saved: ")
      (:cur (wg-workgroup-name workgroup))
      (:cur "'s ")
      (:msg "working-wconfig to the kill ring"))))

(defun wg-yank-wconfig ()
  "Restore a wconfig from `wg-wconfig-kill-ring'.
Successive yanks restore wconfigs sequentially from the kill
ring, starting at the front."
  (interactive)
  (when (zerop (ring-length (wg-wconfig-kill-ring)))
    (error "The kill-ring is empty"))
  (let ((pos (if (not (eq real-last-command 'wg-yank-wconfig)) 0
               (1+ (or (get 'wg-yank-wconfig :position) 0)))))
    (put 'wg-yank-wconfig :position pos)
    (wg-restore-wconfig-undoably (ring-ref (wg-wconfig-kill-ring) pos))
    (wg-fontified-message
      (:cmd "Yanked: ")
      (:msg (format "%S" pos)) "  "
      (wg-workgroup-list-display))))

(defun wg-kill-workgroup-and-buffers (&optional workgroup)
  "Kill WORKGROUP and the buffers in its working-wconfig."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (bufs (save-window-excursion
                 (wg-restore-workgroup workgroup)
                 (mapcar #'window-buffer (window-list)))))
    (wg-kill-workgroup workgroup)
    (mapc #'kill-buffer bufs)
    (wg-fontified-message
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup))
      (:msg " and its buffers ") "\n"
      (wg-workgroup-list-display))))

(defun wg-delete-other-workgroups (&optional workgroup)
  "Delete all workgroups but WORKGROUP."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (unless (or wg-no-confirm-on-destructive-operation
                (y-or-n-p "Really delete all other workgroups? "))
      (error "Cancelled"))
    (dolist (w (wg-workgroup-list-or-error))
      (unless (eq w workgroup)
        (wg-delete-workgroup w)))
    (unless (wg-current-workgroup-p workgroup)
      (wg-switch-to-workgroup workgroup))
    (wg-fontified-message
      (:cmd "Deleted: ")
      (:msg "All workgroups but ")
      (:cur (wg-workgroup-name workgroup)))))



;;; workgroup updating and reverting commands

(defun wg-revert-workgroup (&optional workgroup)
  "Restore WORKGROUP's window configuration to its state at the last save."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (base-wconfig (wg-workgroup-base-wconfig workgroup)))
    (if (wg-current-workgroup-p workgroup)
        (wg-restore-wconfig-undoably base-wconfig)
      (wg-add-wconfig-to-undo-list workgroup base-wconfig))
    (wg-fontified-message
      (:cmd "Reverted: ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-revert-all-workgroups ()
  "Revert all workgroups to their base wconfigs.
Only workgroups' working-wconfigs in `selected-frame' are
reverted."
  (interactive)
  (mapc #'wg-revert-workgroup (wg-workgroup-list-or-error))
  (wg-fontified-message
    (:cmd "Reverted: ")
    (:msg "All")))



;;; saved wconfig commands

(defun wg-save-wconfig ()
  "Save the current wconfig to the current workgroup's saved wconfigs."
  (interactive)
  (let* ((workgroup (wg-current-workgroup))
         (name (wg-read-saved-wconfig-name workgroup))
         (wconfig (wg-current-wconfig)))
    (setf (wg-wconfig-name wconfig) name)
    (wg-workgroup-save-wconfig workgroup wconfig)
    (wg-fontified-message
      (:cmd "Saved: ")
      (:cur name))))

(defun wg-restore-saved-wconfig ()
  "Restore one of the current workgroup's saved wconfigs in `selected-frame'."
  (interactive)
  (let ((workgroup (wg-current-workgroup)))
    (wg-restore-wconfig-undoably
     (wg-workgroup-get-saved-wconfig
      workgroup
      (wg-completing-read
       "Saved wconfig: "
       (mapcar 'wg-wconfig-name (wg-workgroup-saved-wconfigs workgroup))
       nil t)))))

(defun wg-kill-saved-wconfig ()
  "Kill one of the current workgroup's saved wconfigs.
Also add it to the wconfig kill-ring."
  (interactive)
  (let* ((workgroup (wg-current-workgroup))
         (wconfig (wg-read-saved-wconfig workgroup)))
    (wg-workgroup-kill-saved-wconfig workgroup wconfig)
    (wg-add-to-wconfig-kill-ring wconfig)
    (wg-fontified-message
      (:cmd "Deleted: ")
      (:cur (wg-wconfig-name wconfig)))))



;;; workgroup-list reorganization commands

(defun wg-swap-workgroups ()
  "Swap the previous and current workgroups."
  (interactive)
  (wg-swap-workgroups-in-workgroup-list
   (wg-current-workgroup) (wg-previous-workgroup))
  (wg-fontified-message
    (:cmd "Swapped:  ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-left (&optional workgroup n)
  "Offset WORKGROUP leftward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup (wg-get-workgroup workgroup) (or n -1))
  (wg-fontified-message
    (:cmd "Offset left: ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-right (&optional workgroup n)
  "Offset WORKGROUP rightward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup (wg-get-workgroup workgroup) (or n 1))
  (wg-fontified-message
    (:cmd "Offset right: ")
    (wg-workgroup-list-display)))



;;; undo/redo commands

(defun wg-undo-wconfig-change (&optional workgroup)
  "Undo a change to the current workgroup's window-configuration."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (undid? (wg-workgroup-offset-position-in-undo-list workgroup 1)))
    (wg-fontified-message
      (:cmd "Undo: ")
      (wg-undo-timeline-display workgroup)
      (:cur (if undid? "" "  No more undo info")))))

(defun wg-redo-wconfig-change (&optional workgroup)
  "Redo a change to the current workgroup's window-configuration."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (redid? (wg-workgroup-offset-position-in-undo-list workgroup -1)))
    (wg-fontified-message
      (:cmd "Redo: ")
      (wg-undo-timeline-display workgroup)
      (:cur (if redid? "" "  No more redo info")))))

(defun wg-undo-once-all-workgroups ()
  "Do what the name says.  Useful for instance when you
accidentally call `wg-revert-all-workgroups' and want to return
all workgroups to their un-reverted state."
  (interactive)
  (mapc 'wg-undo-wconfig-change (wg-workgroup-list-or-error))
  (wg-message "Undid once on all workgroups."))

(defun wg-redo-once-all-workgroups ()
  "Do what the name says.  Probably useless.  Included for
symetry with `wg-undo-once-all-workgroups'."
  (interactive)
  (mapc 'wg-redo-wconfig-change (wg-workgroup-list-or-error))
  (wg-message "Redid once on all workgroups."))



;;; buffer-list-filter commands

(defun wg-switch-to-buffer ()
  "Workgroups' version of `switch-to-buffer'."
  (interactive)
  (wg-buffer-internal 'switch-to-buffer "Buffer"))

(defun wg-switch-to-buffer-other-window ()
  "Workgroups' version of `switch-to-buffer-other-window'."
  (interactive)
  (wg-buffer-internal
   'switch-to-buffer-other-window "Switch to buffer in other window"))

(defun wg-switch-to-buffer-other-frame ()
  "Workgroups' version of `switch-to-buffer-other-frame'."
  (interactive)
  (wg-buffer-internal
   'switch-to-buffer-other-frame "Switch to buffer in other frame"))

(defun wg-kill-buffer ()
  "Workgroups' version of `kill-buffer'."
  (interactive)
  (wg-buffer-internal
   'kill-buffer "Kill buffer" (buffer-name (current-buffer))))

(defun wg-display-buffer ()
  "Workgroups' version of `display-buffer'."
  (interactive)
  (wg-buffer-internal 'display-buffer "Display buffer"))

(defun wg-insert-buffer ()
  "Workgroups' version of `insert-buffer'."
  (interactive)
  (wg-buffer-internal 'insert-buffer "Insert buffer"))

;; FIXME: If you C-h i for info, then wg-next-buffer, you occasionally don't
;; switch to the buffer you were on previously.
(defun wg-next-buffer-internal (buffer-list &optional prev noerror)
  "Switch to the next buffer in Workgroups' filtered buffer list."
  (when buffer-list
    (let* ((cur (current-buffer))
           (next (or (wg-cyclic-nth-from-elt cur buffer-list (if prev -1 1))
                     (car buffer-list))))
      (unless (eq cur next)
        (switch-to-buffer next)
        (unless prev (bury-buffer cur))
        next))))

(defun wg-next-buffer (&optional prev)
  "Switch to the next buffer in Workgroups' filtered buffer list.
In the post-command message the current buffer is rotated to the
middle of the list to more easily see where `wg-previous-buffer'
will take you."
  (interactive)
  (let ((command (if prev 'previous-buffer 'next-buffer)))
    (if (not (wg-filter-buffer-list-p))
        (call-interactively (wg-prior-mapping workgroups-mode command))
      (wg-with-buffer-list-filters command
        (wg-awhen (wg-filtered-buffer-list) (wg-next-buffer-internal it prev))
        (wg-message (wg-buffer-command-display))))))

(defun wg-previous-buffer ()
  "Switch to the next buffer in Workgroups' filtered buffer list."
  (interactive)
  (wg-next-buffer t))

(defun wg-bury-buffer (&optional buffer-or-name)
  "Remove BUFFER-OR-NAME from the current workgroup, bury it,
and switch to the next buffer in the buffer-list-filter."
  (interactive (list (current-buffer)))
  (if (not (wg-filter-buffer-list-p))
      (call-interactively (wg-prior-mapping workgroups-mode 'bury-buffer))
    (wg-with-buffer-list-filters 'bury-buffer
      (wg-next-buffer-internal (wg-filtered-buffer-list))
      (bury-buffer buffer-or-name)
      (wg-message (wg-buffer-command-display)))))

(defun wg-banish-buffer (&optional buffer-or-name)
  "Dissociate BUFFER-OR-NAME from the current workgroup, and bury it."
  (interactive)
  (let ((buffer (or buffer-or-name (current-buffer))))
    (wg-workgroup-dissociate-bufobj (wg-current-workgroup) buffer)
    (wg-bury-buffer buffer)))

(defun wg-associate-buffer-with-workgroup (&optional workgroup buffer weak)
  "Associate BUFFER with WORKGROUP.
WEAK non-nil means weakly associate BUFFER."
  (interactive (list nil nil current-prefix-arg))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (buffer (or buffer (current-buffer)))
         (bname (buffer-name buffer))
         (wgname (wg-workgroup-name workgroup)))
    (if (wg-workgroup-associate-bufobj workgroup buffer weak)
        (wg-message "%s-associated %S with %s"
                    (if weak "Weakly" "Strongly") bname wgname)
      (wg-message "%S is already associated with %s" bname wgname))))

(defun wg-associate-visible-buffers-with-workgroup (&optional workgroup weak)
  "Associate all buffers visible in `selected-frame' with WORKGROUP.
WEAK non-nil means weakly associate them.  Otherwise strongly
associate them."
  (interactive (list nil current-prefix-arg))
  (let ((workgroup (wg-get-workgroup workgroup))
        (buffers (mapcar 'window-buffer (window-list))))
    (dolist (buffer buffers)
      (wg-workgroup-associate-bufobj workgroup buffer weak))
    (wg-fontified-message
      (:cmd (format "%s associated: " (if weak "Weakly" "Strongly")))
      (wg-buffer-list-display buffers))))

(defun wg-dissociate-buffer-from-workgroup (&optional workgroup buffer)
  "Dissociate BUFFER from WORKGROUP."
  (interactive (list nil nil))
  (let ((workgroup (wg-get-workgroup workgroup))
        (buffer (or buffer (current-buffer))))
    (wg-message
     (if (wg-workgroup-dissociate-bufobj workgroup buffer)
         "Dissociated %S from %s" "%S isn't associated with %s")
     (wg-buffer-name buffer)
     (wg-workgroup-name workgroup))))

(defun wg-restore-workgroup-associated-buffers (&optional workgroup)
  "Restore all the buffers associated with WORKGROUP that can be restored."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (restored-buffers (wg-restore-workgroup-associated-buffers-internal
                            workgroup)))
    (wg-fontified-message
      (:cmd "Restored: ")
      (wg-buffer-list-display restored-buffers))))

(defun wg-cycle-buffer-association-type ()
  "Cycle the current buffer's association type in the current workgroup.
See `wg-workgroup-cycle-bufobj-association-type' for details."
  (interactive)
  (let* ((workgroup (wg-current-workgroup))
         (buffer (current-buffer))
         (type (wg-workgroup-cycle-bufobj-association-type workgroup buffer)))
    (force-mode-line-update)
    (wg-fontified-message
      (:cur (buffer-name buffer))
      (:cmd (cl-case type
              (strong " strongly associated with ")
              (weak " weakly associated with ")
              (otherwise " unassociated with ")))
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-dissociate-weakly-associated-buffers (&optional workgroup)
  "Dissociate from the current workgroup all weakly associated buffers."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-workgroup-dissociate-weakly-associated-buffers workgroup)
    (wg-fontified-message
      (:cmd "Remaining buffers: ")
      (wg-buffer-list-display (wg-workgroup-associated-buffers workgroup)))))



;;; window-tree commands
;;
;; TODO: These are half-hearted.  Clean them up; allow specification of the
;; window-tree depth at which to operate; add complex window creation commands;
;; and add window splitting, deletion and locking commands.

(defun wg-transpose-window-internal (workgroup offset)
  "Move `selected-window' by OFFSET in its wlist."
  (wg-restore-wconfig-undoably
   (wg-wconfig-move-window
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    offset)))

(defun wg-backward-transpose-window (&optional workgroup offset)
  "Move `selected-window' backward by OFFSET in its wlist."
  (interactive (list nil current-prefix-arg))
  (wg-transpose-window-internal workgroup (or offset -1)))

(defun wg-transpose-window (&optional workgroup offset)
  "Move `selected-window' forward by OFFSET in its wlist."
  (interactive (list nil current-prefix-arg))
  (wg-transpose-window-internal workgroup (or offset 1)))

(defun wg-reverse-frame-horizontally (&optional workgroup)
  "Reverse the order of all horizontally split wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup)))))

(defun wg-reverse-frame-vertically (&optional workgroup)
  "Reverse the order of all vertically split wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    t)))

(defun wg-reverse-frame-horizontally-and-vertically (&optional workgroup)
  "Reverse the order of all wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    'both)))

(defun wg-toggle-window-dedicated-p ()
  "Toggle `window-dedicated-p' in `selected-window'."
  (interactive)
  (set-window-dedicated-p nil (not (window-dedicated-p)))
  (force-mode-line-update t)
  (wg-fontified-message
    (:cmd "Window:")
    (:cur (concat (unless (window-dedicated-p) " not") " dedicated"))))



;;; misc commands

(defun wg-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list nil (wg-read-new-workgroup-name "New name: ")))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (oldname (wg-workgroup-name workgroup)))
    (setf (wg-workgroup-name workgroup) newname)
    (wg-flag-workgroup-modified workgroup)
    (wg-fontified-message
      (:cmd "Renamed: ")
      (:cur oldname)
      (:msg " to ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-reset (&optional force)
  "Reset Workgroups.
Resets all frame parameters, buffer-local vars, the current
Workgroups session object, etc."
  (interactive "P")
  (unless (or force wg-no-confirm-on-destructive-operation
              (y-or-n-p "Really reset Workgroups? "))
    (error "Canceled"))
  (wg-reset-internal)
  (wg-fontified-message
    (:cmd "Reset: ")
    (:msg "Workgroups")))



;;; file commands

(defun wg-read-session-save-file-name ()
  "Read and return a new session filename."
  (read-file-name "Save session as: "))

(defun wg-write-session-file (filename &optional confirm)
  "Write the current session into file FILENAME.
This makes the session visit that file, and marks it as not modified.

If optional second arg CONFIRM is non-nil, this function asks for
confirmation before overwriting an existing file.  Interactively,
confirmation is required unless you supply a prefix argument.

Think of it as `write-file' for Workgroups sessions."
  (interactive (list (wg-read-session-save-file-name)
                     (not current-prefix-arg)))
  (when (and confirm (file-exists-p filename))
    (unless (y-or-n-p (format "File `%s' exists; overwrite? " filename))
      (error "Cancelled")))
  (unless (file-writable-p filename)
    (error "File %s can't be written to" filename))
  (wg-perform-session-maintenance)
  (setf (wg-session-file-name (wg-current-session)) filename)
  (setf (wg-session-version (wg-current-session)) wg-version)
  (wg-write-sexp-to-file
   (wg-pickel-all-session-parameters (wg-current-session))
   filename)
  (wg-mark-everything-unmodified)
  (wg-fontified-message (:cmd "Wrote: ") (:file filename)))

(defun wg-determine-session-save-file-name ()
  "Return the filename in which to save the session."
  (or (wg-session-file-name (wg-current-session))
      (and wg-use-default-session-file wg-default-session-file)))

(defun wg-save-session (&optional force)
  "Save the current Workgroups session if it's been modified.
Think of it as `save-buffer' for Workgroups sessions.  Optional
argument FORCE non-nil, or interactively with a prefix arg, save
the session regardless of whether it's been modified."
  (interactive "P")
  (if (and (not (wg-modified-p)) (not force))
      (wg-message "(The session is unmodified)")
    (wg-write-session-file
     (or (wg-determine-session-save-file-name)
         (wg-read-session-save-file-name)))))

(defun wg-query-and-save-if-modified ()
  "Query for save when `wg-modified-p'."
  (or (not (wg-modified-p))
      (when (y-or-n-p "Save modified workgroups? ")
        (wg-save-session))))

(defun wg-save-session-on-exit (behavior)
  "Perform session-saving operations based on BEHAVIOR."
  (cl-case behavior
    (ask (wg-query-and-save-if-modified))
    (save
     (if (wg-determine-session-save-file-name)
         (wg-save-session)
       (wg-query-and-save-if-modified)))))

(defun wg-find-session-file (filename)
  "Load a session visiting FILENAME, creating one if none already exists."
  (interactive "FFind session file: ")
  (cond ((file-exists-p filename)
         (let ((session (wg-read-sexp-from-file filename)))
           (unless (wg-session-p session)
             (error "%S is not a Workgroups session file." filename))
           (setf (wg-session-file-name session) filename)
           (wg-reset-internal (wg-unpickel-session-parameters session)))
         (wg-awhen (and wg-switch-to-first-workgroup-on-find-session-file
                        (wg-workgroup-list))
           (if (and wg-open-this-wg
                    (member wg-open-this-wg (wg-workgroup-names)))
               (wg-switch-to-workgroup wg-open-this-wg)
             (wg-switch-to-workgroup (car it))))
         (wg-fontified-message (:cmd "Loaded: ") (:file filename)))
        (t
         (wg-query-and-save-if-modified)
         (wg-reset-internal (wg-make-session :file-name filename))
         (wg-fontified-message
           (:cmd "(New Workgroups session file)")))))

(defun wg-find-file-in-new-workgroup (filename)
  "Create a new blank workgroup and find file FILENAME in it."
  (interactive "FFind file in new workgroup: ")
  (wg-create-workgroup (file-name-nondirectory filename) t)
  (find-file filename))

(defun wg-find-file-read-only-in-new-workgroup (filename)
  "Create a new workgroup and find file FILENAME read-only in it."
  (interactive "FFind file read only in new workgroup: ")
  (wg-create-workgroup (file-name-nondirectory filename) t)
  (find-file-read-only filename))

(defun wg-dired-in-new-workgroup (dirname &optional switches)
  "Create a workgroup and open DIRNAME in dired with SWITCHES."
  (interactive (list (read-directory-name "Dired (directory): ")
                     current-prefix-arg))
  (wg-create-workgroup dirname)
  (dired dirname switches))



;;; toggle commands

(defun wg-toggle-and-message (symbol)
  "Toggle SYMBOL's truthiness and message the new value."
  (wg-fontified-message
    (:cmd (format "%s: " symbol))
    (:msg (format "%s" (wg-toggle symbol)))))

(defun wg-toggle-buffer-list-filtration ()
  "Toggle `wg-buffer-list-filtration-on'."
  (interactive)
  (wg-toggle-and-message 'wg-buffer-list-filtration-on))

(defun wg-toggle-mode-line-display ()
  "Toggle `wg-mode-line-display-on'."
  (interactive)
  (wg-toggle-and-message 'wg-mode-line-display-on))



;;; echo commands

(defun wg-echo-current-workgroup ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (wg-fontified-message
    (:cmd "Current: ")
    (:cur (wg-workgroup-name (wg-current-workgroup)))))

(defun wg-echo-all-workgroups ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (wg-fontified-message
    (:cmd "Workgroups: ")
    (wg-workgroup-list-display)))

(defun wg-echo-time ()
  "Echo the current time.  Optionally includes `battery' info."
  (interactive)
  (wg-message ;; Pass through format to escape the % in `battery'
   "%s" (wg-fontify
          (:cmd "Current time: ")
          (:msg (format-time-string wg-time-format))
          (when (and wg-display-battery (fboundp 'battery))
            (wg-fontify "\n" (:cmd "Battery: ") (:msg (battery)))))))

(defun wg-echo-version ()
  "Echo Workgroups' current version number."
  (interactive)
  (wg-fontified-message
    (:cmd "Workgroups version: ")
    (:msg wg-version)))

(defun wg-echo-last-message ()
  "Echo the last message Workgroups sent to the echo area.
The string is passed through a format arg to escape %'s."
  (interactive)
  (message "%s" wg-last-message))



;;; help commands

(defun wg-help ()
  "Just call `apropos-command' on \"^wg-\".
There used to be a bunch of help-buffer construction stuff here,
including a `wg-help' variable that basically duplicated every
command's docstring;  But why, when there's `apropos-command'?"
  (interactive)
  (apropos-command "^wg-"))


(require 'workgroups-commands-minibuffer)

(provide 'workgroups-commands)

;;; workgroups-restore --- Functions to restore buffers and frames
;;; Commentary:
;;
;; TIPS
;;---------
;; Each Emacs frame ("window") can contain several workgroups.
;; WCONFIG is settings for each workgroup (buffers, parameters,...)
;;
;; So restoring WCONFIG using `wg-restore-wconfig' is restoring buffers
;; for current workgroup. Generally speaking each Workgroup can have
;; several WCONFIGs.
;;
;;; Code:

(require 'workgroups-variables)

(defun wg-restore-default-buffer ()
  "Switch to `wg-default-buffer'."
  (switch-to-buffer wg-default-buffer t))

(defun wg-restore-existing-buffer (buf)
  "Switch to and return BUF's referrent (some live buffer) if it exists."
  (wg-awhen (wg-find-buf-in-buffer-list buf (buffer-list))
    (switch-to-buffer it t)
    (wg-set-buffer-uid-or-error (wg-buf-uid buf))
    it))

(defun wg-restore-file-buffer (buf)
  "Restore BUF by finding its file.  Return the created buffer.
If BUF's file doesn't exist, call `wg-restore-default-buffer'"
  (wg-when-let ((file-name (wg-buf-file-name buf)))
    (when (or wg-restore-remote-buffers
              (not (file-remote-p file-name)))
      (cond ((file-exists-p file-name)
             (find-file file-name)
             (rename-buffer (wg-buf-name buf) t)
             (wg-set-buffer-uid-or-error (wg-buf-uid buf))
             (when wg-restore-mark
               (set-mark (wg-buf-mark buf))
               (deactivate-mark))
             (wg-deserialize-buffer-local-variables buf)
             (current-buffer))
            (t
             ;; try directory
             (if (not (file-remote-p file-name))
                 (if (file-directory-p (file-name-directory file-name))
                     (progn
                       (dired (file-name-directory file-name))
                       (current-buffer))
                   (progn
                     (message "Attempt to restore nonexistent file: %S" file-name)
                     nil))
               nil)
             )))))

(defun wg-restore-special-buffer (buf)
  "Restore a buffer BUF with DESERIALIZER-FN."
  (wg-when-let
      ((special-data (wg-buf-special-data buf))
       (buffer (save-window-excursion
                 (condition-case err
                     (funcall (car special-data) buf)
                   (error (message "Error deserializing %S: %S"
                                   (wg-buf-name buf) err)
                          nil)))))
    (switch-to-buffer buffer t)
    (wg-set-buffer-uid-or-error (wg-buf-uid buf))
    buffer))

(defun wg-restore-buffer (buf)
  "Restore BUF and return it."
  (let (wg-buffer-auto-association-on)
    (or (wg-restore-existing-buffer buf)
        (wg-restore-special-buffer buf)
        (wg-restore-file-buffer buf)
        (progn (wg-restore-default-buffer) nil))))

(defun wg-restore-window-positions (win &optional window)
  "Restore various positions in WINDOW from their values in WIN."
  (let ((window (or window (selected-window))))
    (wg-with-slots win
        ((win-point wg-win-point)
         (win-start wg-win-start)
         (win-hscroll wg-win-hscroll))
      (set-window-start window win-start t)
      (set-window-hscroll window win-hscroll)
      (set-window-point
       window
       (cond ((not wg-restore-point) win-start)
             ((eq win-point :max) (point-max))
             (t win-point)))
      (when (>= win-start (point-max)) (recenter)))))

(defun wg-restore-window (win)
  "Restore WIN in `selected-window'."
  (let ((selwin (selected-window))
        (buf (wg-find-buf-by-uid (wg-win-buf-uid win))))
    (if (not buf) (wg-restore-default-buffer)
      (when (wg-restore-buffer buf)
        (wg-restore-window-positions win selwin)
        (when wg-restore-window-dedicated-p
          (set-window-dedicated-p selwin (wg-win-dedicated win)))))))

(defun wg-reset-window-tree ()
  "Delete all but one window in `selected-frame', and reset
various parameters of that window in preparation for restoring
a wtree."
  (delete-other-windows)
  (set-window-dedicated-p nil nil))

(defun wg-restore-window-tree-helper (w)
  "Recursion helper for `wg-restore-window-tree'."
  (if (wg-wtree-p w)
      (cl-loop with dir = (wg-wtree-dir w)
               for (win . rest) on (wg-wtree-wlist w)
               do (when rest (split-window nil (wg-w-size win dir) (not dir)))
               do (wg-restore-window-tree-helper win))
    (wg-restore-window w)
    (when (wg-win-selected w)
      (setq wg-window-tree-selected-window (selected-window)))
    (when (wg-win-minibuffer-scroll w)
      (setq minibuffer-scroll-window (selected-window)))
    (other-window 1)))

(defun wg-restore-window-tree (wtree)
  "Restore WTREE in `selected-frame'."
  (let ((window-min-width wg-window-min-width)
        (window-min-height wg-window-min-height)
        (wg-window-tree-selected-window nil))
    (wg-reset-window-tree)
    (wg-restore-window-tree-helper wtree)
    (wg-awhen wg-window-tree-selected-window (select-window it))))

(defun wg-wconfig-restore-frame-position (wconfig)
  "Restore `selected-frame's position from WCONFIG."
  (wg-when-let ((left (wg-wconfig-left wconfig))
                (top (wg-wconfig-top wconfig)))
    ;; Check that arguments are integers
    ;; Problem: https://github.com/pashinin/workgroups2/issues/15
    (if (and (integerp left)
             (integerp top))
        (set-frame-position (selected-frame) left top))))

(defun wg-wconfig-restore-scroll-bars (wconfig)
  "Restore `selected-frame's scroll-bar settings from WCONFIG."
  (set-frame-parameter
   nil 'vertical-scroll-bars (wg-wconfig-vertical-scroll-bars wconfig))
  (set-frame-parameter
   nil 'scroll-bar-width (wg-wconfig-scroll-bar-width wconfig)))

;;(defun wg-wconfig-restore-fullscreen (wconfig)
;;  "Restore `selected-frame's fullscreen settings from WCONFIG."
;;  (set-frame-parameter
;;   nil 'fullscreen (wg-wconfig-parameters wconfig))
;;  )

(defun wg-scale-wconfig-to-frame (wconfig)
  "Scale WCONFIG buffers to fit current frame size.
Return a scaled copy of WCONFIG."
  (interactive)
  (wg-scale-wconfigs-wtree wconfig
                           (frame-parameter nil 'width)
                           (frame-parameter nil 'height)))

(defun wg-frame-resize-and-position (wconfig)
  "Resize and position a frame based on WCONFIG of current workgroup."
  (interactive)
  (let* ((params (wg-wconfig-parameters wconfig))
         fullscreen)
    (set-frame-parameter nil 'fullscreen (if (assoc 'fullscreen params)
                                             (cdr (assoc 'fullscreen params))
                                           nil))
    (when (and wg-restore-frame-position
               (not (frame-parameter nil 'fullscreen)))
      (wg-wconfig-restore-frame-position wconfig))
    ))

(defun wg-restore-frame-size-position (wconfig)
  "Smart-restore of frame size and position.

Depending on `wg-remember-frame-for-each-wg' frame parameters may
be restored for each workgroup.

If `wg-remember-frame-for-each-wg' is nil (by default) then
current frame parameters are saved/restored to/from first
workgroup. And frame parameters for all other workgroups are just
ignored.
"
  (interactive)
  (let* ((params (wg-wconfig-parameters wconfig))
         fullscreen)
    ;; Frame maximized / fullscreen / none
    (unless wg-remember-frame-for-each-wg
      (setq params (wg-wconfig-parameters (wg-workgroup-working-wconfig (wg-first-workgroup)))))
    (setq fullscreen (if (assoc 'fullscreen params)
                         (cdr (assoc 'fullscreen params))
                       nil))
    (when (and fullscreen
             (or wg-remember-frame-for-each-wg
                 (null (wg-current-workgroup t))))
      (set-frame-parameter nil 'fullscreen fullscreen)
      ;; I had bugs restoring maximized frame:
      ;; Frame could be maximized but buffers are not scaled to fit it.
      ;;
      ;; Maybe because of `set-frame-parameter' takes some time to finish and is async.
      ;; So I tried this and it helped
      (sleep-for 0 100))

    ;; Position
    (when (and wg-restore-frame-position
               wg-remember-frame-for-each-wg
               (not (frame-parameter nil 'fullscreen)))
      (wg-wconfig-restore-frame-position wconfig))
    ))

;; FIXME: throw a specific error if the restoration was unsuccessful
(defun wg-restore-wconfig (wconfig)
  "Restore a workgroup configuration WCONFIG in `selected-frame'.

Runs each time you're switching workgroups."
  (let ((wg-record-incorrectly-restored-bufs t)
        (wg-incorrectly-restored-bufs nil)
        (params (wg-wconfig-parameters wconfig))
        fullscreen wtree)
    (wg-barf-on-active-minibuffer)
    (wg-restore-frame-size-position wconfig)
    (when wg-restore-scroll-bars
      (wg-wconfig-restore-scroll-bars wconfig))
    (setq wtree (wg-scale-wconfig-to-frame wconfig))  ; scale wtree to frame size

    ;; Restore buffers
    (wg-restore-window-tree wtree)

    ;; Restore frame position
    (when (and wg-restore-frame-position
               (not (frame-parameter nil 'fullscreen))
               (null (wg-current-workgroup t)))
      (wg-wconfig-restore-frame-position wconfig))

    (when wg-incorrectly-restored-bufs
      (message "Unable to restore these buffers: %S\
If you want, restore them manually and try again."
               (mapcar 'wg-buf-name wg-incorrectly-restored-bufs)))))



(provide 'workgroups-restore)
;;; workgroups-restore.el ends here

;;; lively.el version 0.1 --- interactively updating text
;;; Copyright 2009 Luke Gorrie <luke@bup.co.nz>
;;;
;;; Go to the end of any of the following lines and run `M-x lively'
;;;   Current time:      (current-time-string)
;;;   Last command:      last-command
;;;   Open buffers:      (length (buffer-list))
;;;   Unix processes:    (lively-shell-command "ps -a | wc -l")
;;;
;;; then the code will be replaced by its formatted result -- and
;;; periodically updated. You can create little dashboards.
;;; Use `M-x lively-stop' to restore order.
;;;
;;; Based on the Squeak hack by Scott Wallace.

(require 'cl)

(defvar lively-overlays nil "List of all overlays representing lively text.")
(defvar lively-timer    nil "Idle timer for updating lively text.")
(defvar lively-interval 0.25 "Idle time before lively text update in seconds.")

(defun lively ()
  "Make the expression before point lively."
  (interactive)
  (lively-region (save-excursion (backward-sexp) (point)) (point)))

(defun lively-region (start end)
  "Make the region lively."
  (interactive "r")
  (when (null lively-timer)
    (lively-init-timer))
  (push (make-overlay start end) lively-overlays))

(defun lively-update ()
  "Update the display of all visible lively text."
  (interactive)
  (dolist (o lively-overlays)
    (when (get-buffer-window (overlay-buffer o))
      (condition-case err
          (lively-update-overlay o)
        (error (message "Error in lively expression: %S" err)
               (lively-delete-overlay o))))))

(defun lively-delete-overlay (o)
  (delete-overlay o)
  (setq lively-overlays (remove o lively-overlays)))

(defun lively-update-overlay (o)
  "Evaluate the lively code for O and update its display text."
  (with-current-buffer (overlay-buffer o)
    (let ((expr (buffer-substring (overlay-start o) (overlay-end o))))
      (overlay-put o 'display (format "%s" (eval (read expr)))))))

(defun lively-init-timer ()
  "Setup background timer to update lively text."
  (setq lively-timer (run-with-timer 0 lively-interval 'lively-update)))

(defun lively-stop ()
  "Remove all lively regions in Emacs."
  (interactive)
  (when lively-timer (cancel-timer lively-timer))
  (setq lively-timer nil)
  (mapc 'delete-overlay lively-overlays)
  (setq lively-overlays nil))

;;; Nice to have:

(defun lively-shell-command (command)
  "Execute COMMAND and return the output, sans trailing newline."
  (let ((result (shell-command-to-string command)))
    (substring result 0 (1- (length result)))))

;;; ensime-popup.el --- popup buffer

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

;;;;; Temporary popup buffers

(defvar ensime-popup-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ensime-popup-buffer-quit-function)
    (define-key map [mouse-1] 'push-button)
    map)
  "Keymap for `ensime-popup-buffer-mode'.")

(define-minor-mode ensime-popup-buffer-mode
  "Mode for displaying read only stuff"
  nil
  nil
  ensime-popup-buffer-map)

(add-to-list 'minor-mode-alist
	     '(ensime-popup-buffer-mode (:eval (ensime-modeline-string))))

(defvar ensime-popup-restore-data nil
  "Data needed when closing popup windows.
This is used as buffer local variable.
The format is (POPUP-WINDOW SELECTED-WINDOW OLD-BUFFER).
POPUP-WINDOW is the window used to display the temp buffer.
That window may have been reused or freshly created.
SELECTED-WINDOW is the window that was selected before displaying
the popup buffer.
OLD-BUFFER is the buffer that was previously displayed in POPUP-WINDOW.
OLD-BUFFER is nil if POPUP-WINDOW was newly created.

See `view-return-to-alist' for a similar idea.")

(make-variable-buffer-local
 (defvar ensime-is-popup-buffer nil
   "So we can query later whether this is a popup buffer."))

(make-variable-buffer-local
 (defvar ensime-popup-buffer-quit-function 'ensime-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

;; Interface
(defun ensime-make-popup-buffer (name buffer-vars &optional major-mode-fn)
  "Return a temporary buffer called NAME.
The buffer also uses the minor-mode `ensime-popup-buffer-mode'."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)

    (when major-mode-fn
      (funcall major-mode-fn))

    (set-syntax-table lisp-mode-syntax-table)
    (ensime-init-popup-buffer buffer-vars)

    (setq ensime-is-popup-buffer t)
    (current-buffer)))

(defun ensime-init-popup-buffer (buffer-vars)
  (ensime-popup-buffer-mode 1)
  (multiple-value-setq (ensime-buffer-connection)
    buffer-vars))

(defun ensime-popup-buffer-p (buffer)
  "Is this an ensime popup buffer?"
  (with-current-buffer buffer
    ensime-is-popup-buffer))

(defun ensime-display-popup-buffer (select)
  "Display the current buffer.
   Save the selected-window in a buffer-local variable, so that we
   can restore it later."
  (let ((selected-window (selected-window))
	(old-windows))
    (walk-windows (lambda (w)
		    (if (not (ensime-popup-buffer-p (window-buffer w)))
			(push (cons w (window-buffer w)) old-windows)))
		  nil t)
    (let ((new-window
	   (cond
	    (ensime-popup-in-other-frame
	     (display-buffer-other-frame (current-buffer)))
	    (t (display-buffer (current-buffer))))))
      (unless ensime-popup-restore-data
	(set (make-local-variable 'ensime-popup-restore-data)
	     (list new-window
		   selected-window
		   (cdr (cl-find new-window old-windows :key #'car)))))
      (when select
	(select-window new-window))
      new-window)))

(defun ensime-close-popup-window ()
  (when ensime-popup-restore-data
    (destructuring-bind (popup-window selected-window old-buffer)
	ensime-popup-restore-data
      (kill-local-variable 'ensime-popup-restore-data)
      (bury-buffer)
      (when (eq popup-window (selected-window))
	(cond ((and (not old-buffer) (not (one-window-p)))
	       (delete-window popup-window))
	      ((and old-buffer (buffer-live-p old-buffer))
	       (set-window-buffer popup-window old-buffer))
	      ))
      (when (window-live-p selected-window)
	(select-window selected-window)))
    ))

(defun ensime-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `ensime-popup-buffer-quit-function'."
  (interactive)
  (funcall ensime-popup-buffer-quit-function kill-buffer-p))

(defun ensime-popup-buffer-quit (&optional kill-buffer-p)
  "Get rid of the current (temp) buffer without asking.
  Restore the window configuration unless it was changed since we
  last activated the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (ensime-close-popup-window)
    (when kill-buffer-p
      (kill-buffer buffer))))



(provide 'ensime-popup)

;; Local Variables:
;; End:

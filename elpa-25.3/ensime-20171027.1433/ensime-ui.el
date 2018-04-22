;;; ensime-ui.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

;; Event Handling

(make-variable-buffer-local
 (defvar ensime-ui-nav-handler nil
   "Handler for ui actions.
 A plist with at least the following:
  (
   :init (lambda (info)) ;; Setup the view
   :update (lambda (info)) ;; Default is to re-call init
   :help-text (lambda (info)) ;; A line across the top
   :keymap map ;; Map from key-combo to func
  )"
   ))

(make-variable-buffer-local
 (defvar ensime-ui-is-nav-buffer nil
   "So we can query later whether this is a nav buffer."))

(make-variable-buffer-local
 (defvar ensime-ui-nav-buffer-info-stack nil
   "So we can query later whether this is a nav buffer."))

(defvar ensime-ui-open-nav-in-other-frame nil)

(make-variable-buffer-local
 (defvar ensime-ui-nav-restore-data nil))


(make-variable-buffer-local
 (defvar ensime-ui-nav-history '()
   "Maintain a history of the info objects viewed in the ui-nav buffer."))

(make-variable-buffer-local
 (defvar ensime-ui-nav-history-cursor 0
   "Where are we in the history?"))

(make-variable-buffer-local
 (defvar ensime-ui-nav-paging-in-progress nil
   "A dynamic variable to inform dynamic extant of user's intent.
   Are we moving in history, or inspecting a new info?"))

(defvar ensime-db-ui-test-handler
  (list
   :init (lambda (info)
	   (insert "Hey this is just a little test."))
   :update (lambda (info))
   :help-text "q to quit."
   :keymap ()
   ))

(defun ensime-ui-nav-handler-for-info (info)
  "Return correct handler for this info."
  (cond
   ((equal info "test") ensime-db-ui-test-handler)
   ((ensime-db-value-p info) ensime-db-ui-value-handler)
   ((ensime-db-backtrace-p info) ensime-db-ui-backtrace-handler)
   ((plist-get info :bytecode) ensime-ui-method-bytecode-handler)
   (t (error
       (format "Can't find ui handler for: %s" info)))
   ))

(defun ensime-ui-nav-backward-page ()
  "Inspect the info object preceding current in history."
  (interactive)
  (setq ensime-ui-nav-history-cursor
	(min (- (length ensime-ui-nav-history) 1)
	     (+ ensime-ui-nav-history-cursor 1)))
  (setq ensime-ui-nav-history-cursor
        (max 0 ensime-ui-nav-history-cursor))
  (ensime-ui-nav-goto-cursor))

(defun ensime-ui-nav-forward-page ()
  "Inspect the info object following current in history."
  (interactive)
  (setq ensime-ui-nav-history-cursor
	(max 0 (- ensime-ui-nav-history-cursor 1)))
  (ensime-ui-nav-goto-cursor))


(defun ensime-ui-nav-goto-cursor ()
  "Helper to jump to a specific point in history."
  (let ((info (nth ensime-ui-nav-history-cursor
		   ensime-ui-nav-history))
	(ensime-ui-nav-paging-in-progress t))
    (ensime-ui-show-nav-buffer
     (current-buffer) info)
    ))


(defun ensime-ui-nav-buffer-p (buf)
  "Is this an ensime nav buffer?"
  (with-current-buffer buf
    ensime-ui-is-nav-buffer))

(defun ensime-ui-make-keymap (handler info)
  (let ((map (make-sparse-keymap)))
    (dolist (pair (plist-get handler :keymap))
      (let ((key (car pair))
	    (func (cadr pair)))
	(define-key map key func))
      )
    map))

(defun ensime-ui-nav-quit ()
  (interactive)
  ;; Restore saved window information
  (when ensime-ui-nav-restore-data
    (destructuring-bind (nav-window selected-window old-buffer)
	ensime-ui-nav-restore-data
      (kill-local-variable 'ensime-ui-nav-restore-data)
      (kill-buffer)
      (when (eq nav-window (selected-window))
	(cond ((and (not old-buffer) (not (one-window-p)))
	       (delete-window nav-window))
	      ((and old-buffer (buffer-live-p old-buffer))
	       (set-window-buffer nav-window old-buffer))
	      ))
      (when (window-live-p selected-window)
	(select-window selected-window))))
  )


(defun ensime-ui-show-nav-buffer (buf-or-name info &optional select conn
					      preserve-point)
  (let* ((connection (or conn (ensime-connection)))
	 (start-point (point))
	 (buf (ensime-ui-make-nav-buffer buf-or-name)))

    (ensime-ui-open-nav-window buf select)

    (with-current-buffer buf
      (save-excursion
	(when (not ensime-ui-nav-paging-in-progress)
	  ;; Clamp the history cursor
	  (setq ensime-ui-nav-history-cursor
		(min (- (length ensime-ui-nav-history) 1)
		     ensime-ui-nav-history-cursor))
	  (setq ensime-ui-nav-history-cursor
		(max 0 ensime-ui-nav-history-cursor))
	  ;; Remove all elements preceding the cursor (the 'redo' history)
	  (setq ensime-ui-nav-history
		(subseq ensime-ui-nav-history
			ensime-ui-nav-history-cursor))
	  ;; Add the new history item
	  (push info ensime-ui-nav-history)
	  ;; Set cursor to point to the new item
	  (setq ensime-ui-nav-history-cursor 0))

	(dolist (ov (overlays-in (point-min) (point-max)))
	  (delete-overlay ov))

	(let ((handler (ensime-ui-nav-handler-for-info info)))
	  (setq ensime-ui-nav-handler handler)

	  (ensime-insert-with-face
	   (plist-get handler :help-text) 'font-lock-constant-face)
	  (ensime-insert-with-face
	   "\n----------------------------------------\n\n"
	   'font-lock-comment-face)

	  (let ((map (ensime-ui-make-keymap handler info)))
	    (define-key map [?\t] 'forward-button)
	    (define-key map (kbd "M-n") 'forward-button)
	    (define-key map (kbd "M-p") 'backward-button)
	    (define-key map (kbd ".") 'ensime-ui-nav-forward-page)
	    (define-key map (kbd ",") 'ensime-ui-nav-backward-page)
	    (define-key map (kbd "q") 'ensime-ui-nav-quit)
	    (use-local-map map))

	  (setq ensime-buffer-connection connection)

	  ;; Call handler's init routine...
	  (funcall (plist-get handler :init) info)

	  (setq buffer-read-only (not (plist-get handler :writable)))
	  ))

      (if preserve-point
	  (goto-char start-point)
	(goto-char (point-min)))
      )

    buf))

(defun ensime-ui-open-nav-window (buf select)
  (with-current-buffer buf
    (let ((selected-window
	   (selected-window))
	  (old-windows))
      (walk-windows
       (lambda (w)
	 (if (not (ensime-ui-nav-buffer-p (window-buffer w)))
	     (push (cons w (window-buffer w)) old-windows))) nil t)
      (let ((new-window
	     (cond
	      (ensime-ui-open-nav-in-other-frame
	       (display-buffer-other-frame
		(current-buffer)))
	      (t (display-buffer (current-buffer))))))

	(unless ensime-ui-nav-restore-data
	  (setq ensime-ui-nav-restore-data
		(list new-window
		      selected-window
		      (cdr (cl-find new-window
				 old-windows
				 :key #'car)))))
	(when select
	  (select-window
	   new-window))))))



(defun ensime-ui-make-nav-buffer (buf-or-name)
  (with-current-buffer (get-buffer-create buf-or-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-syntax-table lisp-mode-syntax-table)
    (setq ensime-ui-is-nav-buffer t)
    (current-buffer)))


(provide 'ensime-ui)

;; Local Variables:
;; End:


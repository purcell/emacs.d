;;; sbt-mode-buffer.el - Functions for discovering the current sbt project
;;
;; Copyright(c) 2013 Heikki Vesalainen
;; For information on the License, see the LICENSE file

(require 'cl-lib)

(require 'sbt-mode-vars)
(require 'sbt-mode-project)

(defcustom sbt:buffer-name-base "*sbt*"
  "Buffer name for sbt"
  :type 'string
  :group 'sbt)

(defun sbt:buffer-name ()
  "Return the buffer name for running sbt."
  (format "%s<%s>"
          sbt:buffer-name-base
          (abbreviate-file-name (sbt:find-root))))

(defun sbt:require-buffer ()
  "Throw error if buffer the current buffer is not an sbt-buffer"
  (unless (derived-mode-p 'sbt-mode)
    (error "Current buffer %s is not an sbt-buffer" (current-buffer))))

(defun sbt:mode-p ()
  "Return non-nil if the current buffer is sbt-buffer"
  (derived-mode-p 'sbt-mode))

;;;###autoload
(defun sbt-switch-to-active-sbt-buffer ()
  "Switch to buffer with running sbt process.
If run in buffer in scala project then it switch to that project sbt buffer (if some exists).
When run in buffer with no scala project then based on number of sbt buffers this happen:
  no sbt buffer exists - do nothing
  one sbt buffer exists - switch to that buffer
  more than one sbt buffer exist - let user choose which buffer to switch to"
  (interactive)
  (let ((current-sbt-root (sbt:find-root))
	(root-and-buffers
	 (cl-loop for process being the elements of (process-list)
	       for current-process-buffer = (process-buffer process)
	       when (and
		     (equal (process-status process) 'run) ;; proces must be running
		     (bufferp current-process-buffer) ;; process must have associated buffer
		     (buffer-live-p current-process-buffer) ;; buffer must not be killed
		     (with-current-buffer current-process-buffer
		       (and
			(sbt:mode-p)
			(process-live-p process)
			(sbt:find-root))))
	       collect (with-current-buffer current-process-buffer
			 (cons (sbt:find-root) current-process-buffer)) into file-buffers
			 finally return file-buffers)))
    (if current-sbt-root
	(let ((buffer-to-switch (cdr (assoc current-sbt-root root-and-buffers))))
	  (if (eq buffer-to-switch (current-buffer))
	      (message "Already in sbt buffer!")
	    (if buffer-to-switch
		(switch-to-buffer-other-window buffer-to-switch)
	      (message "No running sbt buffer for project %s" current-sbt-root))))
      (if root-and-buffers
	  (switch-to-buffer-other-window
	   (if (eq 1 (length root-and-buffers))
	       (cdar root-and-buffers)
	     ;; we have more than one sbt process running, let user choose which one to switch to
	     (let ((sbt-projects (cl-loop for (key . value) in root-and-buffers
					  collect key)))
	       (cdr (assoc
		     (if (fboundp 'ido-completing-read)
			 (ido-completing-read "Switch to sbt buffer for project: " sbt-projects)
		       (completing-read "Switch to sbt buffer for project (hit TAB to auto-complete): "
					sbt-projects nil t (try-completion "" sbt-projects)))
		     root-and-buffers)))))
	(message "No sbt buffers.")))))

(provide 'sbt-mode-buffer)

;; An experimental implementation of multiple REPLs multiplexed over a
;; single Slime socket.  M-x slime-new-mrepl creates a new REPL buffer.
;;
(require 'slime)
(require 'inferior-slime) ; inferior-slime-indent-lime
(require 'cl-lib)

(define-slime-contrib slime-mrepl
  "Multiple REPLs."
  (:authors "Helmut Eller <heller@common-lisp.net>")
  (:license "GPL")
  (:swank-dependencies swank-mrepl))

(require 'comint)

(defvar slime-mrepl-remote-channel nil)
(defvar slime-mrepl-expect-sexp nil)

(define-derived-mode slime-mrepl-mode comint-mode "mrepl"
  ;; idea lifted from ielm
  (unless (get-buffer-process (current-buffer))
    (let* ((process-connection-type nil)
	   (proc (start-process "mrepl (dummy)" (current-buffer) "hexl")))
      (set-process-query-on-exit-flag proc nil)))
  (set (make-local-variable 'comint-use-prompt-regexp) nil)
  (set (make-local-variable 'comint-inhibit-carriage-motion) t)
  (set (make-local-variable 'comint-input-sender) 'slime-mrepl-input-sender)
  (set (make-local-variable 'comint-output-filter-functions) nil)
  (set (make-local-variable 'slime-mrepl-expect-sexp) t)
  ;;(set (make-local-variable 'comint-get-old-input) 'ielm-get-old-input)
  (set-syntax-table lisp-mode-syntax-table)
  )

(slime-define-keys slime-mrepl-mode-map
  ((kbd "RET") 'slime-mrepl-return)
  ([return] 'slime-mrepl-return)
  ;;((kbd "TAB") 'slime-indent-and-complete-symbol)
  ((kbd "C-c C-b") 'slime-interrupt)
  ((kbd "C-c C-c") 'slime-interrupt))

(defun slime-mrepl-process% () (get-buffer-process (current-buffer))) ;stupid
(defun slime-mrepl-mark () (process-mark (slime-mrepl-process%)))

(defun slime-mrepl-insert (string)
  (comint-output-filter (slime-mrepl-process%) string))

(slime-define-channel-type listener)

(slime-define-channel-method listener :prompt (package prompt)
  (with-current-buffer (slime-channel-get self 'buffer)
    (slime-mrepl-prompt package prompt)))

(defun slime-mrepl-prompt (package prompt)
  (setf slime-buffer-package package)
  (slime-mrepl-insert (format "%s%s> "
			      (cl-case (current-column)
				(0 "")
				(t "\n"))
			      prompt))
  (slime-mrepl-recenter))

(defun slime-mrepl-recenter ()
  (when (get-buffer-window)
    (recenter -1)))

(slime-define-channel-method listener :write-result (result)
  (with-current-buffer (slime-channel-get self 'buffer)
    (goto-char (point-max))
    (slime-mrepl-insert result)))

(slime-define-channel-method listener :evaluation-aborted ()
  (with-current-buffer (slime-channel-get self 'buffer)
    (goto-char (point-max))
    (slime-mrepl-insert "; Evaluation aborted\n")))

(slime-define-channel-method listener :write-string (string)
  (slime-mrepl-write-string self string))

(defun slime-mrepl-write-string (self string)
  (with-current-buffer (slime-channel-get self 'buffer)
    (goto-char (slime-mrepl-mark))
    (slime-mrepl-insert string)))

(slime-define-channel-method listener :set-read-mode (mode)
  (with-current-buffer (slime-channel-get self 'buffer)
    (cl-ecase mode
      (:read (setq slime-mrepl-expect-sexp nil)
	     (message "[Listener is waiting for input]"))
      (:eval (setq slime-mrepl-expect-sexp t)))))

(defun slime-mrepl-return (&optional end-of-input)
  (interactive "P")
  (slime-check-connected)
  (goto-char (point-max))
  (cond ((and slime-mrepl-expect-sexp
	      (or (slime-input-complete-p (slime-mrepl-mark) (point))
		  end-of-input))
	 (comint-send-input))
	((not slime-mrepl-expect-sexp)
	 (unless end-of-input
	   (insert "\n"))
	 (comint-send-input t))
        (t
	 (insert "\n")
	 (inferior-slime-indent-line)
         (message "[input not complete]")))
  (slime-mrepl-recenter))

(defun slime-mrepl-input-sender (proc string)
  (slime-mrepl-send-string (substring-no-properties string)))

(defun slime-mrepl-send-string (string &optional command-string)
  (slime-mrepl-send `(:process ,string)))

(defun slime-mrepl-send (msg)
  "Send MSG to the remote channel."
  (slime-send-to-remote-channel slime-mrepl-remote-channel msg))

(defun slime-new-mrepl ()
  "Create a new listener window."
  (interactive)
  (let ((channel (slime-make-channel slime-listener-channel-methods)))
    (slime-eval-async
        `(swank-mrepl:create-mrepl ,(slime-channel.id channel))
      (slime-rcurry 
       (lambda (result channel)
         (cl-destructuring-bind (remote thread-id package prompt) result
           (pop-to-buffer (generate-new-buffer (slime-buffer-name :mrepl)))
           (slime-mrepl-mode)
           (setq slime-current-thread thread-id)
           (setq slime-buffer-connection (slime-connection))
           (set (make-local-variable 'slime-mrepl-remote-channel) remote)
           (slime-channel-put channel 'buffer (current-buffer))
           (slime-channel-send channel `(:prompt ,package ,prompt))))
       channel))))

(defun slime-mrepl ()
  (let ((conn (slime-connection)))
    (cl-find-if (lambda (x) 
	       (with-current-buffer x 
		 (and (eq major-mode 'slime-mrepl-mode)
		      (eq (slime-current-connection) conn))))
	     (buffer-list))))

(def-slime-selector-method ?m
  "First mrepl-buffer"
  (or (slime-mrepl)
      (error "No mrepl buffer (%s)" (slime-connection-name))))

(provide 'slime-mrepl)

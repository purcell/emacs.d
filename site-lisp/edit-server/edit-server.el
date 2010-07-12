;;
;; Emacs edit-server
;;
;; This provides an edit server to respond to requests from the Chrome
;; Emacs Chrome plugin. This is my first attempt at doing something
;; with sockets in Emacs. I based it on the following examples:
;;
;; http://www.emacswiki.org/emacs/EmacsEchoServer
;; http://nullprogram.com/blog/2009/05/17/
;;
;; To use it ensure the file is in your load-path and add something
;; like the following examples to your .emacs:
;;
;; To open pages for editing in new buffers in your existing Emacs
;; instance:
;;
;; (if (locate-library "edit-server")
;;    (progn
;;      (require 'edit-server)
;;      (setq edit-server-new-frame nil)
;;      (edit-server-start)))
;;
;; To open pages for editing in new frames using a running emacs
;; started in --daemon mode:
;;
;; (if (and (daemonp) (locate-library "edit-server"))
;;     (progn
;;       (require 'edit-server)
;;       (edit-server-start)))
;;
;; (C) 2009 Alex Bennee (alex@bennee.com)
;; (C) 2010 Riccardo Murri (riccardo.murri@gmail.com)
;;
;; Licensed under GPLv3
;;

;; uncomment to debug
;(setq debug-on-error 't)
;(setq edebug-all-defs 't)

(if (not (featurep 'make-network-process))
    (error "Incompatible version of [X]Emacs - lacks make-network-process"))

;; Customization
(defcustom edit-server-port 9292
  "Local port the edit server listens to."
  :group 'edit-server
  :type 'integer)

(defcustom edit-server-host nil
  "If not nil, accept connections from HOST address rather than just
localhost. This may present a security issue."
  :group 'edit-server
  :type 'boolean)

(defcustom edit-server-verbose nil
  "If not nil, log connections and progress also to the echo area."
  :group 'edit-server
  :type 'boolean)

(defcustom edit-server-done-hook nil
  "Hook run when done editing a buffer for the Emacs HTTP edit-server.
Current buffer holds the text that is about to be sent back to the client."
  :group 'edit-server
  :type 'hook)

; frame options
(defcustom edit-server-new-frame t
  "If not nil, edit each buffer in a new frame (and raise it)."
  :group 'edit-server
  :type 'boolean)

(defcustom edit-server-new-frame-alist
  '((name . "Emacs TEXTAREA")
    (width . 80)
    (height . 25)
    (minibuffer . t)
    (menu-bar-lines . t))
  "Frame parameters for new frames.  See `default-frame-alist' for examples.
If nil, the new frame will use the existing `default-frame-alist' values."
  :group 'edit-server
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value"))))

(defcustom edit-server-new-frame-mode-line t
  "Show the emacs frame's mode-line if set to t; hide if nil."
  :group 'edit-server
  :type 'boolean)

;; Vars
(defconst edit-server-process-buffer-name " *edit-server*"
  "Template name of the edit-server process buffers.")

(defconst edit-server-log-buffer-name "*edit-server-log*"
  "Template name of the edit-server process buffers.")

(defconst edit-server-edit-buffer-name "TEXTAREA"
  "Template name of the edit-server text editing buffers.")

(defvar edit-server-proc 'nil
  "Network process associated with the current edit, made local when
 the edit buffer is created")

(defvar edit-server-frame 'nil
  "The frame created for a new edit-server process, made local when
 then edit buffer is created")

(defvar edit-server-clients '() 
  "List of all client processes associated with the server process.")

(defvar edit-server-phase nil 
  "Symbol indicating the state of the HTTP request parsing.")

(defvar edit-server-received nil 
  "Number of bytes received so far in the client buffer. 
Depending on the character encoding, may be different from the buffer length.")

(defvar edit-server-request nil 
  "The HTTP request (GET, HEAD, POST) received.")

(defvar edit-server-content-length nil 
  "The value gotten from the HTTP `Content-Length' header.")

(defvar edit-server-url nil 
  "The value gotten from the HTTP `x-url' header.")

;; Mode magic
;
; We want to re-map some of the keys to trigger edit-server-done
; instead of the usual emacs like behaviour. However using
; local-set-key will affect all buffers of the same mode, hence we
; define a special (derived) mode for handling editing of text areas.
;

(define-derived-mode edit-server-text-mode text-mode "Edit Server Text Mode"
  "A derived version of text-mode with a few common Emacs keystrokes
rebound to more functions that can deal with the response to the
edit-server request.

Any of the following keys will close the buffer and send the text
to the HTTP client: C-x #, C-x C-s, C-c C-c.

If any of the above isused with a prefix argument, the
unmodified text is sent back instead.
"
  :group 'edit-server)
(define-key edit-server-text-mode-map (kbd "C-x #") 'edit-server-done)
(define-key edit-server-text-mode-map (kbd "C-x C-s") 'edit-server-done)
(define-key edit-server-text-mode-map (kbd "C-c C-c") 'edit-server-done)
(define-key edit-server-text-mode-map (kbd "C-x C-c") 'edit-server-abort)


;; Edit Server socket code
;

(defun edit-server-start (&optional verbose) 
  "Start the edit server.

If argument VERBOSE is non-nil, logs all server activity to buffer `*edit-server-log*'.
When called interactivity, a prefix argument will cause it to be verbose.
"
  (interactive "P")
  (if (or (process-status "edit-server")
          (null (condition-case err
                    (make-network-process
                     :name "edit-server"
                     :buffer edit-server-process-buffer-name
                     :family 'ipv4
                     :host (if edit-server-host
                               edit-server-host
                             'local)
                     :service edit-server-port
                     :log 'edit-server-accept
                     :server t
                     :noquery t)
                  (file-error nil))))
      (message "An edit-server process is already running")
    (setq edit-server-clients '())
    (if verbose (get-buffer-create edit-server-log-buffer-name))
    (edit-server-log nil "Created a new edit-server process")))

(defun edit-server-stop nil
  "Stop the edit server"
  (interactive)
  (while edit-server-clients
    (edit-server-kill-client (car edit-server-clients))
    (setq edit-server-clients (cdr edit-server-clients)))
  (if (process-status "edit-server")
      (delete-process "edit-server")
    (message "No edit server running"))
  (if (get-buffer edit-server-process-buffer-name)
      (kill-buffer edit-server-process-buffer-name)))

(defun edit-server-log (proc fmt &rest args)
  "If a `*edit-server-log*' buffer exists, write STRING to it for logging purposes.
If `edit-server-verbose' is non-nil, then STRING is also echoed to the message line."
  (let ((string (apply 'format fmt args)))
    (if edit-server-verbose
        (message string))
    (if (get-buffer edit-server-log-buffer-name)
        (with-current-buffer edit-server-log-buffer-name
          (goto-char (point-max))
          (insert (current-time-string) 
                  " " 
                  (if (processp proc)
                      (concat 
                       (buffer-name (process-buffer proc))
                       ": ")
                    "") ; nil is not acceptable to 'insert
                  string)
          (or (bolp) (newline))))))

(defun edit-server-accept (server client msg)
  "Accept a new client connection."
  (let ((buffer (generate-new-buffer edit-server-process-buffer-name)))
    (and (fboundp 'set-buffer-multibyte)
         (set-buffer-multibyte t)) ; djb
    (buffer-disable-undo buffer)
    (set-process-buffer client buffer)
    (set-process-filter client 'edit-server-filter)
    (set-process-query-on-exit-flag client nil) ; kill-buffer kills the associated process
    (with-current-buffer buffer
      (set (make-local-variable 'edit-server-phase) 'wait)
      (set (make-local-variable 'edit-server-received) 0)
      (set (make-local-variable 'edit-server-request) nil))
      (set (make-local-variable 'edit-server-content-length) nil)
      (set (make-local-variable 'edit-server-url) nil))
    (add-to-list 'edit-server-clients client)
    (edit-server-log client msg))

(defun edit-server-filter (proc string)
  "Process data received from the client."
  ;; there is no guarantee that data belonging to the same client
  ;; request will arrive all in one go; therefore, we must accumulate
  ;; data in the buffer and process it in different phases, which
  ;; requires us to keep track of the processing state.
  (with-current-buffer (process-buffer proc)
    (insert string)
    (setq edit-server-received 
          (+ edit-server-received (string-bytes string)))
    (when (eq edit-server-phase 'wait)
      ;; look for a complete HTTP request string
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\([A-Z]+\\)\\s-+\\(\\S-+\\)\\s-+\\(HTTP/[0-9\.]+\\)\r?\n" nil t)
          (edit-server-log proc 
                           "Got HTTP `%s' request, processing in buffer `%s'..." 
                           (match-string 1) (current-buffer))
          (setq edit-server-request (match-string 1))
          (setq edit-server-content-length nil)
          (setq edit-server-phase 'head))))
    
    (when (eq edit-server-phase 'head)
      ;; look for "Content-length" header
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^Content-Length:\\s-+\\([0-9]+\\)" nil t)
          (setq edit-server-content-length (string-to-number (match-string 1)))))
      ;; look for "x-url" header
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^x-url: .*//\\(.*\\)/" nil t)
          (setq edit-server-url (match-string 1))))
      ;; look for head/body separator
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\\(\r?\n\\)\\{2\\}" nil t)
          ;; HTTP headers are pure ASCII (1 char = 1 byte), so we can subtract
          ;; the buffer position from the count of received bytes
          (setq edit-server-received
                (- edit-server-received (- (match-end 0) (point-min))))
          ;; discard headers - keep only HTTP content in buffer
          (delete-region (point-min) (match-end 0))
          (setq edit-server-phase 'body))))
    
    (when (eq edit-server-phase 'body)
      (if (and edit-server-content-length
               (> edit-server-content-length edit-server-received))
          (edit-server-log proc 
                           "Received %d bytes of %d ..." 
                           edit-server-received edit-server-content-length)
        ;; all content transferred - process request now
        (cond
         ((string= edit-server-request "POST")
          ;; create editing buffer, and move content to it
          (edit-server-create-edit-buffer proc))
         (t
          ;; send 200 OK response to any other request
          (edit-server-send-response proc "edit-server is running.\n" t)))
        ;; wait for another connection to arrive
        (setq edit-server-received 0)
        (setq edit-server-phase 'wait)))))

(defun edit-server-create-frame(buffer)
  "Create a frame for the edit server"
  (if edit-server-new-frame
      (let ((new-frame
	     (if (featurep 'aquamacs)
                 (make-frame edit-server-new-frame-alist)
               (make-frame-on-display (getenv "DISPLAY")
                                      edit-server-new-frame-alist))))
	(if (not edit-server-new-frame-mode-line)
            (setq mode-line-format nil))
	(select-frame new-frame)
	(if (and (eq window-system 'x)
		 (fboundp 'x-send-client-message))
	    (x-send-client-message nil 0 nil
				   "_NET_ACTIVE_WINDOW" 32
				   '(1 0 0)))
	(raise-frame new-frame)
        (set-window-buffer (frame-selected-window new-frame) buffer)
	new-frame)
    (pop-to-buffer buffer)
    nil))

(defun edit-server-create-edit-buffer(proc)
  "Create an edit buffer, place content in it and save the network
  process for the final call back"
  (let ((buffer (generate-new-buffer (if edit-server-url
					 edit-server-url
				       edit-server-edit-buffer-name))))
    (with-current-buffer buffer
      (and (fboundp 'set-buffer-multibyte)
           (set-buffer-multibyte t))) ; djb
    (copy-to-buffer buffer (point-min) (point-max))
    (with-current-buffer buffer
      (not-modified)
      (edit-server-text-mode)
      (add-hook 'kill-buffer-hook 'edit-server-abort* nil t)
      (buffer-enable-undo)
      (set (make-local-variable 'edit-server-proc) proc)
      (set (make-local-variable 'edit-server-frame)
	   (edit-server-create-frame buffer)))))

(defun edit-server-send-response (proc &optional body close)
  "Send an HTTP 200 OK response back to process PROC.
Optional second argument BODY specifies the response content:
  - If nil, the HTTP response will have null content.
  - If a string, the string is sent as response content.
  - Any other value will cause the contents of the current 
    buffer to be sent.
If optional third argument CLOSE is non-nil, then process PROC
and its buffer are killed with `edit-server-kill-client'."
  (interactive)
  (if (processp proc)
      (let ((response-header (concat
                          "HTTP/1.0 200 OK\n"
                          (format "Server: Emacs/%s\n" emacs-version)
                          "Date: "
                          (format-time-string
                           "%a, %d %b %Y %H:%M:%S GMT\n"
                           (current-time)))))
        (process-send-string proc response-header)
        (process-send-string proc "\n")
        (cond
         ((stringp body) (process-send-string proc body))
         ((not body) nil)
         (t (process-send-region proc (point-min) (point-max))))
        (process-send-eof proc)
        (if close 
            (edit-server-kill-client proc))
        (edit-server-log proc "Editing done, sent HTTP OK response."))
    (message "edit-server-send-response: invalid proc (bug?)")))

(defun edit-server-kill-client (proc)
  "Kill client process PROC and remove it from the list."
  (let ((procbuf (process-buffer proc)))
    (delete-process proc)
    (kill-buffer procbuf)
    (setq edit-server-clients (delq proc edit-server-clients))))

(defun edit-server-done (&optional abort nokill)
  "Finish editing: send HTTP response back, close client and editing buffers.

The current contents of the buffer are sent back to the HTTP
client, unless argument ABORT is non-nil, in which case then the
original text is sent back.
If optional second argument NOKILL is non-nil, then the editing
buffer is not killed.

When called interactively, use prefix arg to abort editing."
  (interactive "P")
  ;; Do nothing if the connection is closed by the browser (tab killed, etc.)
  (unless (eq (process-status edit-server-proc) 'closed)
    (let ((buffer (current-buffer))
           (proc edit-server-proc)
           (procbuf (process-buffer edit-server-proc)))
      ;; edit-server-* vars are buffer-local, so they must be used before issuing kill-buffer
      (if abort
        ;; send back original content
        (with-current-buffer procbuf
          (run-hooks 'edit-server-done-hook)
          (edit-server-send-response proc t))
        ;; send back edited content
        (save-restriction
          (widen)
          (buffer-disable-undo)
          ;; ensure any format encoding is done (like longlines)
          (dolist (format buffer-file-format)
            (format-encode-region (point-min) (point-max) format))
          ;; send back
          (run-hooks 'edit-server-done-hook)
          (edit-server-send-response edit-server-proc t)
          ;; restore formats (only useful if we keep the buffer)
          (dolist (format buffer-file-format)
            (format-decode-region (point-min) (point-max) format))
          (buffer-enable-undo)))
      (if edit-server-frame (delete-frame edit-server-frame))
      ;; delete-frame may change the current buffer
      (unless nokill (kill-buffer buffer))
      (edit-server-kill-client proc))))

(defun edit-server-abort ()
  "Discard editing and send the original text back to the browser."
  (interactive)
  (edit-server-done t))

(defun edit-server-abort* ()
  "Discard editing and send the original text back to the browser,
but don't kill the editing buffer."
  (interactive)
  (edit-server-done t t))

(provide 'edit-server)

;;; edit-server.el ends here

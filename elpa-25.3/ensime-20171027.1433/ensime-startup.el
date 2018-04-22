;;; ensime-startup.el --- download and launch ENSIME server

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'dash)
(require 's)

(defvar ensime-idle-typecheck-timer nil
  "Timer called when emacs is idle")

(defvar ensime-last-change-time 0
  "Time of last buffer change")

(defvar ensime-server-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defvar ensime--classpath-separator
  (if (member system-type '(cygwin windows-nt)) ";" ":")
  "Separator used in Java classpaths")

(defvar ensime--abort-connection nil)

(defvar ensime--debug-messages nil
  "When true, show debugging information in the echo area")

(defcustom ensime-startup-notification t
  "Show a popup about documentation.
It is important that users know about the documentation."
  :group 'ensime-mode
  :type 'boolean)

(defun ensime-startup-notifications ()
  "Invasive informational messages that users need to be aware of."
  (when ensime-startup-notification
    (let ((welcome (generate-new-buffer "*ENSIME Welcome*")))
      (with-current-buffer welcome
        (insert "Welcome to ENSIME!

ENSIME is more complex than a typical Emacs plugin and interacts
with an external java application (which is downloaded by your
build tool plugin).

You are strongly recommended to read the documentation at

* http://ensime.org/getting_started/
* http://ensime.org/editors/emacs/
* http://ensime.org/getting_help/

before proceeding.

We are all volunteers with our own priorities to improve ENSIME.
Our approach to feature requests is that we will enthusiastically
help you to implement or fix it. The ENSIME codebase is
surprisingly easy to understand and you are invited to read the
contributing guide and jump in: http://ensime.org/contributing/

You can disable this message permanently by setting
`ensime-startup-notification' to `nil', acknowledging
that you have read this message.")
        (goto-char (point-min))
        (read-only-mode t)
        (display-buffer welcome #'display-buffer-pop-up-window)))))

(defun ensime--maybe-update-and-start (orig-buffer-file-name &optional host port)
  (let* ((config-file (ensime-config-find orig-buffer-file-name))
         (config (ensime-config-load config-file)))
    (if (and host port)
        ;; When both host and port are provided, we assume we're connecting to
        ;; an existing, listening server.
        (let ((cache-dir (file-name-as-directory (ensime--get-cache-dir config))))
          (ensime--retry-connect nil host (lambda () port) config cache-dir))
      (ensime--1 config-file))))

(defun ensime--maybe-update-and-start-noninteractive (orig-buffer-file-name)
  (let ((ensime-prefer-noninteractive t))
    (ensime--maybe-update-and-start orig-buffer-file-name)))

(defun ensime-dev-version-p (version)
    "It check VERSION string for few patterns coresponded to dev server version string format."
    (-contains?
     (-map (lambda (s) (s-contains? s version))
           '("-M" "-RC" "SNAPSHOT"))
     t))

(defun* ensime--1 (config-file)
  (when (and (ensime-source-file-p) (not ensime-mode))
    (ensime-mode 1))
  (let* ((config (ensime-config-load config-file))
         (root-dir (ensime--get-root-dir config) )
         (cache-dir (file-name-as-directory (ensime--get-cache-dir config)))
         (name (ensime--get-name config))
         (ensime-server-jars (plist-get config :ensime-server-jars))
         (ensime-server-version (plist-get config :ensime-server-version))
         (scala-compiler-jars (plist-get config :scala-compiler-jars))
         (server-env (or (plist-get config :server-env) ensime-default-server-env))
         (buffer (or (plist-get config :buffer) (concat ensime-default-buffer-prefix name)))
         (server-java (file-name-as-directory (ensime--get-java-home config)))
         (server-flags (or (plist-get config :java-flags) ensime-default-java-flags)))
    (make-directory cache-dir 't)

    (unless (and ensime-server-jars
                ensime-server-version)
      (error (concat
              "\n\n"
              "You are using a .ensime file format that is no longer supported.\n"
              "You must upgrade your build tool or downgrade to ensime stable.\n"
              "See http://ensime.org/editors/emacs/install\n\n")))

    (let* ((server-proc
            (ensime--maybe-start-server
             (generate-new-buffer-name (concat "*" buffer "*"))
             server-java
             (append ensime-server-jars scala-compiler-jars)
             server-flags
             (list* (concat "JAVA_HOME=" server-java)
                    server-env)
             config-file
             cache-dir))
           (host "127.0.0.1")
           (port-fn (lambda () (ensime--read-portfile
                             (concat cache-dir "/port")))))

      ;; Store the config on the server process so we can identify it later.
      (process-put server-proc :ensime-config config)
      (push server-proc ensime-server-processes)
      (ensime--retry-connect server-proc host port-fn config cache-dir))))


;; typecheck continually when idle

(defun ensime-idle-typecheck-set-timer ()
  (when (timerp ensime-idle-typecheck-timer)
    (cancel-timer ensime-idle-typecheck-timer))
  (setq ensime-idle-typecheck-timer
        (run-with-timer nil
                        ensime-typecheck-idle-interval
                        'ensime-idle-typecheck-function)))

(defun ensime-after-change-function (start stop len)
  (set (make-local-variable 'ensime-last-change-time) (float-time)))

(defun ensime-idle-typecheck-function ()
  (when (and ensime-typecheck-when-idle
             (ensime-connected-p)
             (ensime-analyzer-ready))
    (let* ((now (float-time))
           (last-typecheck (ensime-last-typecheck-run-time (ensime-connection)))
           (earliest-allowed-typecheck (+ last-typecheck ensime-typecheck-interval)))
      (when (and (>= now (+ ensime-last-change-time ensime-typecheck-idle-interval))
                 (>= now earliest-allowed-typecheck)
                 (< last-typecheck ensime-last-change-time))
        (ensime-typecheck-current-buffer)
        (ensime-sem-high-refresh-hook)))))

(defun ensime-reload ()
  "Re-initialize the project with the current state of the config file.
Analyzer will be restarted."
  (interactive)
  (ensime-shutdown)
  (call-interactively 'ensime))

(defun ensime--maybe-start-server (buffer java-home classpath flags env config-file cache-dir)
  "Return a new or existing server process."
  (let ((existing (comint-check-proc buffer)))
    (if existing existing
      (ensime--start-server buffer java-home classpath flags env config-file cache-dir))))

(defun ensime--start-server (buffer java-home classpath flags user-env config-file cache-dir)
  "Start an ensime server in the given buffer and return the created process.
BUFFER is the buffer to receive the server output.
FLAGS is a list of JVM flags.
USER-ENV is a list of environment variables.
CACHE-DIR is the server's persistent output directory."
  (message "ENSIME server starting...")
  (with-current-buffer (get-buffer-create buffer)
    (comint-mode)
    (let* ((default-directory cache-dir)
           (tools-jar (expand-file-name "lib/tools.jar" java-home))
           (process-environment (append user-env process-environment))
           (java-command (expand-file-name "bin/java" java-home))
           (args (-flatten (list
                            "-classpath" (ensime--build-classpath (cons tools-jar classpath))
                            flags
                            (concat "-Densime.config=" (expand-file-name config-file))
                            (when ensime-server-logback
                              (concat "-Dlogback.configurationFile=" ensime-server-logback))
                            "org.ensime.server.Server"))))

      (set (make-local-variable 'comint-process-echoes) nil)
      (set (make-local-variable 'comint-use-prompt-regexp) nil)

      ;; Get rid of default filters including ansi coloring, scroll to bottom,
      ;; and scanning for password prompt. These use non-trivial cpu.
      (set (make-local-variable 'comint-output-filter-functions) nil)

      (when ensime--debug-messages
        (make-local-variable 'comint-output-filter-functions)
        (push #'(lambda (str) (message "%s" str)) comint-output-filter-functions))

      (insert (format "Starting ENSIME server: %s %s\n"
                      java-command
                      (mapconcat 'identity args " ")))
      (if (executable-find java-command)
          (comint-exec (current-buffer) buffer java-command nil args)
        (error "java command %s not found" java-command))
      ;; Make sure we clean up nicely (required on Windows, or port files won't
      ;; be removed).
      (add-hook 'kill-emacs-hook 'ensime-kill-emacs-hook-function)
      (add-hook 'kill-buffer-hook 'ensime-interrupt-buffer-process nil t)
      (let ((proc (get-buffer-process (current-buffer))))
        (ensime-set-query-on-exit-flag proc)
        (run-hooks 'ensime-server-process-start-hook)
        proc))))

(defun ensime-kill-emacs-hook-function ()
  "Swallow and log errors on exit."
  (condition-case err
      (ensime-interrupt-all-servers)
    (message "Error while killing emacs: %s" err)))

(defun ensime--scala-binary-version (full-version)
  "The scala binary version given a full version string."
  (pcase (version-to-list (car (s-split "-" full-version)))
    (`(2 10 ,_) "2.10")
    (`(2 11 ,_) "2.11")
    (t (error "unsupported scala version %s" full-version))))

(defun ensime-shutdown ()
  "Terminate the associated ENSIME server (equivalent to killing its buffer)."
  (interactive)
  (let* ((config (ensime-config-for-buffer))
         (server-process (and config (ensime-process-for-config config))))
    (if (not server-process)
        (error "Couldn't find the ENSIME server for this buffer.")
      (kill-buffer (process-buffer server-process)))))

(defun ensime-configured-project-root ()
  "Return root path of the current project as defined in the
config file and stored in the current connection. Nil is returned
if there is no active connection, or if the project root was not
defined."
  (when (ensime-connected-p)
    (let ((config (ensime-config (ensime-connection))))
      (plist-get config :root-dir))))

(defun ensime--read-portfile (portfile)
  "Read the contents of PORTFILE."
  (when (file-exists-p portfile)
    (save-excursion
      (with-temp-buffer
        (insert-file-contents portfile)
        (goto-char (point-min))
        (read (current-buffer))))))

(defun ensime--retry-connect (server-proc host port-fn config cache-dir)
  "When application of port-fn yields a valid port, connect to the port at the
 given host. Otherwise, schedule ensime--retry-connect for re-execution after 5
 seconds."
  (cond (ensime--abort-connection
         (setq ensime--abort-connection nil)
         (message "Aborted"))
        ((and server-proc (eq (process-status server-proc) 'exit))
         (message "Failed to connect: server process exited."))
        (t
         (let ((port (funcall port-fn)))
           (unless (and port
                        (condition-case nil
                            (progn
                              (ensime--connect host port config)
                              t)
                          (error
                           (not (message "failed to connect to port %s, trying again..." port)))))
             (run-at-time
              "5 sec" nil 'ensime-timer-call 'ensime--retry-connect
              server-proc host port-fn config cache-dir))))))

(defun ensime--connect (host port config)
  (let ((c (ensime-connect host port)))
    (ensime-set-config c config)
    (let ((ensime-dispatching-connection c))
      (ensime-eval-async
       ;; hmm, this poses a problem... we can't really do anything
       ;; until we know the protocol version. This shouldn't be async,
       ;; but making it sync gives us "error in timer" errors.
       '(swank:connection-info)
       'ensime-handle-connection-info))))

(defun ensime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.
   The default condition handler for timer functions (see
   `timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defun ensime--abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (setq ensime--abort-connection 't))


(provide 'ensime-startup)

;; Local Variables:
;; End:

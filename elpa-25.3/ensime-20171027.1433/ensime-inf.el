;;; ensime-inf.el - Interaction with a Scala interpreter.

;; Copyright (C) 2010 Aemon Cannon
;;
;; Derived from scala-mode-inf.el
;; Original Copyright and Licensing notice below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop

;;; License

;; SCALA LICENSE
;;
;; Copyright (c) 2002-2010 EPFL, Lausanne, unless otherwise specified.
;; All rights reserved.
;;
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'ensime-overlay)
(require 'comint)

(defgroup ensime-inf nil
  "Support for running the Scala REPL as an inferior process."
  :group 'ensime
  :prefix "ensime-inf-")

(defcustom ensime-inf-cmd-template
  '(:java :java-flags
          "-Djline.terminal=jline.UnsupportedTerminal"
          "-Dscala.usejavacp=true"
          "scala.tools.nsc.MainGenericRunner")
  "The command to launch the scala interpreter. Keywords will be replaced
with data loaded from server."
  :type 'string
  :group 'ensime-inf)

(defcustom ensime-inf-ansi-support t
  "Use comint ansi support"
  :group 'ensime-inf
  :type 'boolean)

(defconst ensime-inf-buffer-name "*Scala REPL*")

(defvar ensime-inf-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last ensime-inf-load-file.
Used for determining the default in the next one.")

(defvar ensime-inf-overlay-marker nil)


(define-derived-mode ensime-inf-mode comint-mode "Scala REPL"
  "Major mode for interacting with a Scala interpreter."
  (define-key ensime-inf-mode-map [(meta return)] 'comint-accumulate)
  (define-key ensime-inf-mode-map (kbd "TAB") 'ensime-inf-send-tab)

  ;; Comint configuration
  (set (make-local-variable 'comint-input-sender)
       'ensime-inf-input-sender)

  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output
         comint-postoutput-scroll-to-bottom
         ensime-inf-postoutput-filter))

  (if ensime-inf-ansi-support
      (set (make-local-variable 'ansi-color-for-comint-mode) t)
    (set (make-local-variable 'ansi-color-for-comint-mode) 'filter))
  )

(defun ensime-inf-input-sender (proc string)
  (comint-send-string proc string)
  (comint-send-string proc "\n"))

(defun ensime-inf-running-p-1 ()
  ;; True iff a Scala interpreter is currently running in a buffer.
  (comint-check-proc ensime-inf-buffer-name))

(defun ensime-inf-assert-running ()
  (unless (ensime-inf-running-p-1)
    (error "Scala interpreter not running")))

(defun ensime-inf-run-scala ()
  "Start a Scala REPL in an interactive buffer."
  (interactive)
  (let* ((conn (or (ensime-connection-or-nil)
                   (ensime-prompt-for-connection)))
         (root-path (or (ensime-configured-project-root) "."))
         (hack (ensime-inf-repl-config))
         (cmd-and-args (ensime-replace-keywords ensime-inf-cmd-template hack))
         (classpath (plist-get hack :classpath))
         (process-environment (append (list (concat "CLASSPATH=" classpath))
                                      process-environment)))

    (switch-to-buffer-other-window
     (get-buffer-create ensime-inf-buffer-name))

    (ensime-inf-mode)

    (cd root-path)
    (ensime-assert-executable-on-path (car cmd-and-args))
    (comint-exec (current-buffer)
                 ensime-inf-buffer-name
                 (car cmd-and-args)
                 nil
                 (cdr cmd-and-args))

    (setq ensime-buffer-connection conn)

    (let ((proc (get-buffer-process (current-buffer))))
      (ensime-set-query-on-exit-flag proc)
      (set-process-sentinel proc 'ensime--inf-process-sentinel)
      proc)))

(defun ensime--inf-process-sentinel (proc ev)
  (unless (process-live-p proc)
    (ensime-event-sig :inf-repl-exit))
  (when (functionp 'internal-default-process-sentinel)
    (internal-default-process-sentinel proc ev)))

(defun ensime-inf-run-and-import ()
  "Run a Scala interpreter and import the package at point, if any."
  (interactive)
  (let ((pack (ensime-package-at-point)))
    (ensime-inf-run-scala)
    (when pack
      (ensime-inf-import-package pack))))

(defun ensime-inf-get-project-root ()
  "Return root path of the current project."
  (let ((config (ensime-config (ensime-connection))))
    (or (plist-get config :root-dir) ".")))

(defun ensime-inf-repl-config (&optional config)
  "Return a plist of values to use in the template, extracted from CONFIG."
  (cl-flet
      ((get-deps (c) (append (plist-get c :targets)
                             (plist-get c :test-targets)
                             (plist-get c :compile-deps)
                             (plist-get c :runtime-deps)
                             (plist-get c :test-deps))))
    (let ((config (or config (ensime-config-for-buffer))))
      (list
       ;; this is a hacky approach to multiple return values
       :java (expand-file-name "bin/java" (plist-get config :java-home))
       :java-flags (or (plist-get config :java-flags) ensime-default-java-flags)
       :classpath (ensime--build-classpath
                   (delete-dups (append (plist-get config :scala-compiler-jars)
                                        (get-deps config)
                                        (-flatten
                                         (mapcar #'get-deps (plist-get config :subprojects))))))))))

(defun ensime-inf-switch ()
  "Switch to buffer containing the interpreter"
  (interactive)
  (if (equal ensime-inf-buffer-name (buffer-name))
      (switch-to-buffer-other-window (other-buffer))
    (if (and (get-buffer ensime-inf-buffer-name)
             (ensime-inf-process-live-p ensime-inf-buffer-name))
        (switch-to-buffer-other-window ensime-inf-buffer-name)
      (ensime-inf-run-scala)))
  (goto-char (point-max)))

(defun ensime-inf-switch-and-import ()
  "Switch to the buffer containing the interpreter and import the package at point, if any."
  (interactive)
  (let ((pack (ensime-package-at-point)))
    (ensime-inf-switch)
    (when pack
      (ensime-inf-import-package pack))))

(defun ensime-inf-process-live-p (buffer-name)
  "Check if the process associated with the buffer is living."
  (comint-check-proc buffer-name))

(defun ensime-inf-send-tab ()
  (interactive)
  (ensime-inf-assert-running)
  ;; TODO Fix completion...
  )

(defun ensime-inf-send-string (str &rest args)
  (comint-send-string ensime-inf-buffer-name (apply 'format str args))
  (comint-send-string ensime-inf-buffer-name "\n"))

(defun ensime-inf-eval-region (start end)
  "Send current region to Scala interpreter. If no region is active send current line to Scala interpreter."
  (interactive "r")
  (ensime-inf-assert-running)
  (let* ((start (if (use-region-p) start (line-beginning-position)))
         (end   (if (use-region-p) end (line-end-position)))
         (reg (buffer-substring-no-properties start end))
         (buffer (buffer-name)))
    (setq ensime-inf-overlay-marker (copy-marker end))
    (with-current-buffer ensime-inf-buffer-name
      (goto-char (point-max))
      (comint-send-string nil ":paste\n")
      (comint-send-string nil reg)
      (comint-send-string nil "\n")
      (comint-send-eof))))

(defun ensime-inf-eval-result ()
  "Get REPL evaluation result."
  (with-current-buffer ensime-inf-buffer-name
    (save-excursion
      (goto-char (point-max))
      (next-line -2)
      (end-of-line)
      (let ((end (point)))
        (if (search-backward "Exiting paste mode, now interpreting." nil t)
            (progn
              (next-line 2)
              (beginning-of-line)
              (buffer-substring-no-properties (point) end))
          nil)))))

(defun ensime-inf-eval-definition ()
  "Send the current 'definition' to the Scala interpreter.

   This function's idea of a definition is the block of text ending
   in the current line (or the first non-empty line going
   backwards), and begins in the first line that is not empty and
   does not start with whitespace or '{'.

   For example:

   println( \"aja\")
   println( \"hola\" )

   if the cursor is somewhere in the second print statement, the
   interpreter should output 'hola'.

   In the following case, if the cursor is in the second line, then
   the complete function definition will be send to the interpreter:

   def foo =
     1 + 2
   "
  (interactive)
  (save-excursion
    ;; find the first non-empty line
    (beginning-of-line)
    (while (and (not (= (point) (point-min)))
                (looking-at "\\s-*$"))
      (next-line -1))
    (end-of-line)
    (let ((end (point)))
      ;; now we need to find the start
      (beginning-of-line)
      (while (and (not (= (point) (point-min)))
                  (looking-at (mapconcat 'identity
                                         '("^$"       ; empty lines
                                           "^\\s-+"   ; empty lines or lines that start with whitespace
                                           "^\\s-*}") ; lines that start with a '}'
                                         "\\|")))
        (next-line -1)
        (beginning-of-line))
      (message "region %s %s" (point) end)
      (ensime-inf-eval-region (point) end))))


(defun ensime-inf-eval-buffer ()
  "Send whole buffer to Scala interpreter."
  (interactive)
  (ensime-inf-eval-region (point-min) (point-max)))

(defun ensime-inf-load-file (file-name)
  "Load a file in the Scala interpreter."
  (interactive (comint-get-source "Load Scala file: " ensime-inf-prev-l/c-dir/file
				  '(scala-mode) t))
  (ensime-inf-assert-running)
  (comint-check-source file-name)
  (setq ensime-inf-prev-l/c-dir/file (cons (file-name-directory file-name)
					   (file-name-nondirectory file-name)))
  (ensime-inf-send-string ":load %s" file-name))

(defun ensime-inf-import-package-at-point ()
  "Import the contents of the package at point into the repl."
  (interactive)
  (let ((pack (ensime-package-at-point)))
    (if pack
        (ensime-inf-import-package pack)
      (message "No package found."))))

(defun ensime-inf-import-package (package-name)
  "Import the contents of a package into the repl."
  (ensime-inf-send-string "import %s._" package-name))

(defun ensime-inf-quit-interpreter ()
  "Quit Scala interpreter."
  (interactive)
  (ensime-inf-assert-running)
  (ensime-inf-send-string "\n:quit"))

(defun ensime-inf-postoutput-filter (str)
  ;; Ideally we'd base this on comint's decision on whether it's seen
  ;; a prompt, but that decision hasn't been made by this stage
  (unless (or (string-equal str "") (string-equal "\n" (substring str -1)))
    (ensime-event-sig :inf-repl-ready)
    (when (markerp  ensime-inf-overlay-marker)
      (with-current-buffer (buffer-name (marker-buffer ensime-inf-overlay-marker))
        (let ((eval-result (ensime-inf-eval-result)))
          (when eval-result
            (ensime--make-result-overlay
                (format "%S" eval-result)
              :where (marker-position ensime-inf-overlay-marker)
              :duration 'command)
            (setq ensime-inf-overlay-marker nil)))))))

(provide 'ensime-inf)

;; Local Variables:
;; End:

;;; ensime-inf.el ends here

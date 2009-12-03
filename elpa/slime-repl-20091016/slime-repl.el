;;; slime-repl.el --- Read-Eval-Print Loop written in Emacs Lisp
;;
;; Original Author: Helmut Eller
;; Contributors: to many to mention
;; License: GNU GPL (same license as Emacs)
;; URL: http://common-lisp.net/project/slime/
;; Version: 20091016
;; Keywords: languages, lisp, slime
;; Package-Requires: ((slime "20091016"))
;; Adapted-by: Phil Hagelberg
;;
;;; Description:
;;
;; This file implements a Lisp Listener along with some niceties like
;; a persistent history and various "shortcut" commands.  Nothing here
;; depends on comint.el; I/O is multiplexed over SLIME's socket.
;;
;; This used to be the default REPL for SLIME, but it was hard to
;; maintain.
;;
;;; Installation:
;;
;; Call slime-setup and include 'slime-repl as argument: 
;;
;;  (slime-setup '(slime-repl [others conribs ...]))
;;

;;;;; slime-repl

(defgroup slime-repl nil
  "The Read-Eval-Print Loop (*slime-repl* buffer)."
  :prefix "slime-repl-"
  :group 'slime)

(defcustom slime-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish repl commands from lisp forms."
  :type '(character)
  :group 'slime-repl)

(defcustom slime-repl-only-save-lisp-buffers t
  "When T we only attempt to save lisp-mode file buffers. When
  NIL slime will attempt to save all buffers (as per
  save-some-buffers). This applies to all ASDF related repl
  shortcuts."
  :type '(boolean)
  :group 'slime-repl)

(defface slime-repl-prompt-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-output-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Lisp output in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the SLIME REPL."
  :group 'slime-repl)

(defcustom slime-repl-history-file "~/.slime-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'slime-repl)

(defcustom slime-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'slime-repl)

(defcustom slime-repl-history-file-coding-system 
  (cond ((slime-find-coding-system 'utf-8-unix) 'utf-8-unix)
        (t slime-net-coding-system))
  "*The coding system for the history file."
  :type 'symbol
  :group 'slime-repl)


;; dummy defvar for compiler
(defvar slime-repl-read-mode)

(defun slime-reading-p ()
  "True if Lisp is currently reading input from the REPL."
  (with-current-buffer (slime-output-buffer)
    slime-repl-read-mode))


;;;; Stream output

(slime-def-connection-var slime-connection-output-buffer nil
  "The buffer for the REPL.  May be nil or a dead buffer.")

(make-variable-buffer-local
 (defvar slime-output-start nil
   "Marker for the start of the output for the evaluation."))

(make-variable-buffer-local
 (defvar slime-output-end nil
   "Marker for end of output. New output is inserted at this mark."))

;; dummy definitions for the compiler
(defvar slime-repl-package-stack)
(defvar slime-repl-directory-stack)
(defvar slime-repl-input-start-mark)
(defvar slime-repl-prompt-start-mark)

(defun slime-output-buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (slime-connection-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (slime-connection-output-buffer)
              (let ((connection (slime-connection)))
                (with-current-buffer (slime-repl-buffer t connection)
                  (unless (eq major-mode 'slime-repl-mode) 
                    (slime-repl-mode))
                  (setq slime-buffer-connection connection)
		  (setq slime-buffer-package (slime-lisp-package connection))
                  (slime-reset-repl-markers)
                  (unless noprompt 
                    (slime-repl-insert-prompt))
                  (current-buffer)))))))

(defvar slime-repl-banner-function 'slime-repl-insert-banner)

(defun slime-repl-update-banner ()
  (funcall slime-repl-banner-function)
  (goto-char (point-max))
  (slime-mark-output-start)
  (slime-mark-input-start)
  (slime-repl-insert-prompt))

(defun slime-repl-insert-banner ()
  (when (zerop (buffer-size))
    (let ((welcome (concat "; SLIME " (or (slime-changelog-date)
                                          "- ChangeLog file not found"))))
      (insert welcome))))

(defun slime-init-output-buffer (connection)
  (with-current-buffer (slime-output-buffer t)
    (setq slime-buffer-connection connection
          slime-repl-directory-stack '()
          slime-repl-package-stack '())
    (slime-repl-update-banner)))

(defun slime-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (slime-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (slime-repl-show-maximum-output)))

(defmacro slime-with-output-end-mark (&rest body)
  "Execute BODY at `slime-output-end'.  

If point is initially at `slime-output-end' and the buffer is visible
update window-point afterwards.  If point is initially not at
`slime-output-end, execute body inside a `save-excursion' block."
  `(let ((body.. (lambda () ,@body))
         (updatep.. (and (eobp) (pos-visible-in-window-p))))
     (cond ((= (point) slime-output-end)
            (let ((start.. (point)))
              (funcall body..)
              (set-marker slime-output-end (point))
              (when (= start.. slime-repl-input-start-mark) 
                (set-marker slime-repl-input-start-mark (point)))))
           (t 
            (save-excursion 
              (goto-char slime-output-end)
              (funcall body..))))
     (when updatep..
       (slime-repl-show-maximum-output))))

(defun slime-output-filter (process string)
  (with-current-buffer (process-buffer process)
    (when (and (plusp (length string))
               (eq (process-status slime-buffer-connection) 'open))
      (slime-write-string string))))

(defvar slime-open-stream-hooks)

(defun slime-open-stream-to-lisp (port)
  (let ((stream (open-network-stream "*lisp-output-stream*" 
                                     (slime-with-connection-buffer ()
                                       (current-buffer))
				     slime-lisp-host port)))
    (slime-set-query-on-exit-flag stream)
    (set-process-filter stream 'slime-output-filter)
    (let ((pcs (process-coding-system (slime-current-connection))))
      (set-process-coding-system stream (car pcs) (cdr pcs)))
    (when-let (secret (slime-secret))
      (slime-net-send secret stream))
    (run-hook-with-args 'slime-open-stream-hooks stream)
    stream))

(defun slime-io-speed-test (&optional profile)
  "A simple minded benchmark for stream performance.
If a prefix argument is given, instrument the slime package for
profiling before running the benchmark."
  (interactive "P")
  (eval-and-compile
    (require 'elp))
  (elp-reset-all)
  (elp-restore-all)
  (load "slime.el")
  ;;(byte-compile-file "slime-net.el" t)
  ;;(setq slime-log-events nil)
  (setq slime-enable-evaluate-in-emacs t)
  ;;(setq slime-repl-enable-presentations nil)
  (when profile
    (elp-instrument-package "slime-"))
  (kill-buffer (slime-output-buffer))
  (switch-to-buffer (slime-output-buffer))
  (delete-other-windows)
  (sit-for 0)
  (slime-repl-send-string "(swank:io-speed-test 4000 1)")
  (let ((proc (slime-inferior-process)))
    (when proc
      (display-buffer (process-buffer proc) t)
      (goto-char (point-max)))))

(defvar slime-write-string-function 'slime-repl-write-string)

(defun slime-write-string (string &optional target)
  "Insert STRING in the REPL buffer or some other TARGET.
If TARGET is nil, insert STRING as regular process
output.  If TARGET is :repl-result, insert STRING as the result of the
evaluation.  Other values of TARGET map to an Emacs marker via the 
hashtable `slime-output-target-to-marker'; output is inserted at this marker."
  (funcall slime-write-string-function string target))

(defun slime-repl-write-string (string &optional target)
  (case target
    ((nil) (slime-repl-emit string))
    (:repl-result (slime-repl-emit-result string))
    (t (slime-emit-string string target))))

(defvar slime-repl-popup-on-output nil
  "Display the output buffer when some output is written.
This is set to nil after displaying the buffer.")

(defmacro slime-save-marker (marker &rest body)
  (let ((pos (gensym "pos")))
  `(let ((,pos (marker-position ,marker)))
     (prog1 (progn . ,body)
       (set-marker ,marker ,pos)))))

(put 'slime-save-marker 'lisp-indent-function 1)

(defun slime-repl-emit (string)
  ;; insert the string STRING in the output buffer
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (goto-char slime-output-end)
      (slime-save-marker slime-output-start
        (slime-propertize-region '(face slime-repl-output-face 
                                        rear-nonsticky (face))
          (insert-before-markers string)
          (when (and (= (point) slime-repl-prompt-start-mark)
                     (not (bolp)))
            (insert-before-markers "\n")
            (set-marker slime-output-end (1- (point)))))))
    (when slime-repl-popup-on-output
      (setq slime-repl-popup-on-output nil)
      (display-buffer (current-buffer)))
    (slime-repl-show-maximum-output)))

(defun slime-repl-emit-result (string &optional bol)
  ;; insert STRING and mark it as evaluation result
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (slime-save-marker slime-output-start
        (slime-save-marker slime-output-end
          (goto-char slime-repl-input-start-mark)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (slime-propertize-region `(face slime-repl-result-face
                                          rear-nonsticky (face))
            (insert-before-markers string)))))
    (slime-repl-show-maximum-output)))

(defvar slime-last-output-target-id 0
  "The last integer we used as a TARGET id.")

(defvar slime-output-target-to-marker
  (make-hash-table)
  "Map from TARGET ids to Emacs markers.
The markers indicate where output should be inserted.")

(defun slime-output-target-marker (target)
  "Return the marker where output for TARGET should be inserted."
  (case target
    ((nil)
     (with-current-buffer (slime-output-buffer)
       slime-output-end))
    (:repl-result
     (with-current-buffer (slime-output-buffer)
       slime-repl-input-start-mark))
    (t
     (gethash target slime-output-target-to-marker))))

(defun slime-emit-string (string target)
  "Insert STRING at target TARGET.
See `slime-output-target-to-marker'."
  (let* ((marker (slime-output-target-marker target))
         (buffer (and marker (marker-buffer marker))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion 
          ;; Insert STRING at MARKER, then move MARKER behind
          ;; the insertion.
          (goto-char marker)
          (insert-before-markers string)
          (set-marker marker (point)))))))

(defun slime-switch-to-output-buffer ()
  "Select the output buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (slime-pop-to-buffer (slime-output-buffer))
  (goto-char (point-max)))


;;;; REPL
;;
;; The REPL uses some markers to separate input from output.  The
;; usual configuration is as follows:
;; 
;;    ... output ...    ... result ...    prompt> ... input ...
;;    ^            ^                      ^       ^           ^
;;    output-start output-end  prompt-start       input-start point-max
;;
;; input-start is a right inserting marker, because
;; we want it to stay behind when the user inserts text.
;;
;; We maintain the following invariant:
;;
;;  output-start <= output-end <= input-start.
;;
;; This invariant is important, because we must be prepared for
;; asynchronous output and asynchronous reads.  ("Asynchronous" means,
;; triggered by Lisp and not by Emacs.)
;;
;; All output is inserted at the output-end marker.  Some care must be
;; taken when output-end and input-start are at the same position: if
;; we insert at that point, we must move the right markers.  We should
;; also not leave (window-)point in the middle of the new output.  The
;; idiom we use is a combination to slime-save-marker,
;; insert-before-markers, and manually updating window-point
;; afterwards.
;;
;; A "synchronous" evaluation request proceeds as follows: the user
;; inserts some text between input-start and point-max and then hits
;; return.  We send that region to Lisp, move the output and input
;; makers to the line after the input and wait.  When we receive the
;; result, we insert it together with a prompt between the output-end
;; and input-start mark.  See `slime-repl-insert-prompt'.
;;
;; It is possible that some output for such an evaluation request
;; arrives after the result.  This output is inserted before the
;; result (and before the prompt). 
;;
;; If we are in "reading" state, e.g., during a call to Y-OR-N-P,
;; there is no prompt between output-end and input-start.
;;

;; FIXME: slime-lisp-package should be local in a REPL buffer
(slime-def-connection-var slime-lisp-package
    "COMMON-LISP-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-lisp-package-prompt-string
    "CL-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slime-make-variables-buffer-local
 (defvar slime-repl-package-stack nil
   "The stack of packages visited in this repl.")

 (defvar slime-repl-directory-stack nil
   "The stack of default directories associated with this repl.")

 (defvar slime-repl-prompt-start-mark)
 (defvar slime-repl-input-start-mark)
 (defvar slime-repl-old-input-counter 0
   "Counter used to generate unique `slime-repl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together."))

(defun slime-reset-repl-markers ()
  (dolist (markname '(slime-output-start
                      slime-output-end
                      slime-repl-prompt-start-mark
                      slime-repl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

;;;;; REPL mode setup

(defvar slime-repl-mode-map)

(let ((map (copy-keymap slime-parent-map)))
  (set-keymap-parent map lisp-mode-map)
  (setq slime-repl-mode-map (make-sparse-keymap))
  (set-keymap-parent slime-repl-mode-map map)
  (loop for (key command) in slime-editing-keys
        do (define-key slime-repl-mode-map key command)))

(slime-define-keys slime-prefix-map
  ("\C-z" 'slime-switch-to-output-buffer)
  ("\M-p" 'slime-repl-set-package))

(slime-define-keys slime-mode-map 
  ("\C-c~" 'slime-sync-package-and-default-directory)
  ("\C-c\C-y" 'slime-call-defun))

(slime-define-keys slime-connection-list-mode-map
  ((kbd "RET") 'slime-goto-connection)
  ([return] 'slime-goto-connection))

(slime-define-keys slime-repl-mode-map
  ("\C-m" 'slime-repl-return)
  ([return] 'slime-repl-return)
  ("\C-j" 'slime-repl-newline-and-indent)
  ("\C-\M-m" 'slime-repl-closing-return)
  ([(control return)] 'slime-repl-closing-return)
  ("\C-a" 'slime-repl-bol)
  ([home] 'slime-repl-bol)
  ("\M-p" 'slime-repl-previous-input)
  ((kbd "C-<up>") 'slime-repl-backward-input)
  ("\M-n" 'slime-repl-next-input)
  ((kbd "C-<down>") 'slime-repl-forward-input)
  ("\M-r" 'slime-repl-previous-matching-input)
  ("\M-s" 'slime-repl-next-matching-input)
  ("\C-c\C-c" 'slime-interrupt)
  ;("\t"   'slime-complete-symbol)
  ("\t"   'slime-indent-and-complete-symbol)
  ("\M-\t" 'slime-complete-symbol)
  (" "    'slime-space)
  ("\C-c\C-o" 'slime-repl-clear-output)
  ("\C-c\M-o" 'slime-repl-clear-buffer)
  ("\C-c\C-u" 'slime-repl-kill-input)
  ("\C-c\C-n" 'slime-repl-next-prompt)
  ("\C-c\C-p" 'slime-repl-previous-prompt)
  ("\C-c\C-z" 'slime-nop))

(slime-define-keys slime-inspector-mode-map
  ((kbd "M-RET") 'slime-inspector-copy-down-to-repl))

(def-slime-selector-method ?r
  "SLIME Read-Eval-Print-Loop."
  (slime-output-buffer))

(defun slime-repl-mode () 
  "Major mode for interacting with a superior Lisp.
\\{slime-repl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'slime-repl-mode)
  (use-local-map slime-repl-mode-map)
  (lisp-mode-variables t)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (setq font-lock-defaults nil)
  (setq mode-name "REPL")
  (setq slime-current-thread :repl-thread)
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  (when slime-repl-history-file
    (slime-repl-safe-load-history)
    (slime-add-local-hook 'kill-buffer-hook 
                          'slime-repl-safe-save-merged-history))
  (add-hook 'kill-emacs-hook 'slime-repl-save-all-histories)
  (slime-setup-command-hooks)
  ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; the start of the previous prompt or next prompt respectively.
  ;; Notice the interplay with SLIME-REPL-BEGINNING-OF-DEFUN.
  (set (make-local-variable 'beginning-of-defun-function) 
       'slime-repl-mode-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 
       'slime-repl-mode-end-of-defun)
  (slime-run-mode-hooks 'slime-repl-mode-hook))

(defun slime-repl-buffer (&optional create connection)
  "Get the REPL buffer for the current connection; optionally create."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "*slime-repl %s*" (slime-connection-name connection))))

(defun slime-repl ()
  (interactive)
  (slime-switch-to-output-buffer))

(defun slime-repl-mode-beginning-of-defun ()
  (slime-repl-previous-prompt)
  t)

(defun slime-repl-mode-end-of-defun ()
  (slime-repl-next-prompt)
  t)

(defun slime-repl-send-string (string &optional command-string)
  (cond (slime-repl-read-mode
         (slime-repl-return-string string))
        (t (slime-repl-eval-string string))))

(defun slime-repl-eval-string (string)
  (slime-rex ()
      ((list 'swank:listener-eval string) (slime-lisp-package))
    ((:ok result)
     (slime-repl-insert-result result))
    ((:abort)
     (slime-repl-show-abort))))

(defun slime-repl-insert-result (result)
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (when result
        (destructure-case result
          ((:values &rest strings)
           (cond ((null strings)
                  (slime-repl-emit-result "; No value\n" t))
                 (t
                  (dolist (s strings)
                    (slime-repl-emit-result s t)))))))
      (slime-repl-insert-prompt))
    (slime-repl-show-maximum-output)))

(defun slime-repl-show-abort ()
  (with-current-buffer (slime-output-buffer)
    (save-excursion
      (slime-save-marker slime-output-start
        (slime-save-marker slime-output-end
          (goto-char slime-output-end)
          (insert-before-markers "; Evaluation aborted.\n")
          (slime-repl-insert-prompt))))
    (slime-repl-show-maximum-output)))

(defun slime-repl-insert-prompt ()
  "Insert the prompt (before markers!).
Set point after the prompt.  
Return the position of the prompt beginning."
  (goto-char slime-repl-input-start-mark)
  (slime-save-marker slime-output-start
    (slime-save-marker slime-output-end
      (unless (bolp) (insert-before-markers "\n"))
      (let ((prompt-start (point))
            (prompt (format "%s> " (slime-lisp-package-prompt-string))))
        (slime-propertize-region
            '(face slime-repl-prompt-face read-only t intangible t
                   slime-repl-prompt t
                   ;; emacs stuff
                   rear-nonsticky (slime-repl-prompt read-only face intangible)
                   ;; xemacs stuff
                   start-open t end-open t)
          (insert-before-markers prompt))
        (set-marker slime-repl-prompt-start-mark prompt-start)
        prompt-start))))

(defun slime-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max)) 
          (recenter -1))))))

(defvar slime-repl-current-input-hooks)

(defun slime-repl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer."
  (or (run-hook-with-args-until-success 'slime-repl-current-input-hooks 
                                        until-point-p)
      (buffer-substring-no-properties slime-repl-input-start-mark 
                                      (if until-point-p 
                                          (point) 
                                        (point-max)))))

(defun slime-property-position (text-property &optional object)
  "Return the first position of TEXT-PROPERTY, or nil."
  (if (get-text-property 0 text-property object)
      0
    (next-single-property-change 0 text-property object)))
  
(defun slime-mark-input-start ()
  (set-marker slime-repl-input-start-mark (point) (current-buffer)))

(defun slime-mark-output-start ()
  (set-marker slime-output-start (point))
  (set-marker slime-output-end (point)))

(defun slime-mark-output-end ()
  ;; Don't put slime-repl-output-face again; it would remove the
  ;; special presentation face, for instance in the SBCL inspector.
  (add-text-properties slime-output-start slime-output-end
                       '(;;face slime-repl-output-face 
                         rear-nonsticky (face))))

(defun slime-repl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (cond ((and (>= (point) slime-repl-input-start-mark)
              (slime-same-line-p (point) slime-repl-input-start-mark))
         (goto-char slime-repl-input-start-mark))
        (t (beginning-of-line 1)))
  (slime-preserve-zmacs-region))

(defun slime-preserve-zmacs-region ()
  "In XEmacs, ensure that the zmacs-region stays active after this command."
  (when (boundp 'zmacs-region-stays)
    (set 'zmacs-region-stays t)))

(defun slime-repl-in-input-area-p ()
   (<= slime-repl-input-start-mark (point)))

(defun slime-repl-at-prompt-start-p ()
  ;; This will not work on non-current prompts.
  (= (point) slime-repl-input-start-mark))

(defun slime-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  ;; We call BEGINNING-OF-DEFUN if we're at the start of a prompt
  ;; already, to trigger SLIME-REPL-MODE-BEGINNING-OF-DEFUN by means
  ;; of the locally bound BEGINNING-OF-DEFUN-FUNCTION, in order to
  ;; jump to the start of the previous prompt.
  (if (and (not (slime-repl-at-prompt-start-p))
           (slime-repl-in-input-area-p))
      (goto-char slime-repl-input-start-mark)
    (beginning-of-defun))
  t)

;; FIXME: this looks very strange
(defun slime-repl-end-of-defun ()
  "Move to next of defun."
  (interactive)
  ;; C.f. SLIME-REPL-BEGINNING-OF-DEFUN.
  (if (and (not (= (point) (point-max))) 
           (slime-repl-in-input-area-p))
      (goto-char (point-max))
    (end-of-defun))
  t)

(defun slime-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (slime-repl-find-prompt t))

(defun slime-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (slime-repl-find-prompt))
 
(defun slime-repl-find-prompt (&optional backward)
  (let ((origin (point))
        (prop 'slime-repl-prompt))
    (while (progn 
             (slime-search-property-change prop backward)
             (not (or (slime-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (slime-end-of-proprange-p prop)
      (goto-char origin))))

(defun slime-search-property-change (prop &optional backward)
  (cond (backward 
         (goto-char (previous-single-char-property-change (point) prop)))
        (t 
         (goto-char (next-single-char-property-change (point) prop)))))

(defun slime-end-of-proprange-p (property)
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defvar slime-repl-return-hooks)

(defun slime-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.  
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched. 

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (slime-check-connected)
  (cond (end-of-input
         (slime-repl-send-input))
        (slime-repl-read-mode ; bad style?
         (slime-repl-send-input t))
        ((and (get-text-property (point) 'slime-repl-old-input)
              (< (point) slime-repl-input-start-mark))
         (slime-repl-grab-old-input end-of-input)
         (slime-repl-recenter-if-needed))
        ((run-hook-with-args-until-success 'slime-repl-return-hooks))
        ((slime-input-complete-p slime-repl-input-start-mark (point-max))
         (slime-repl-send-input t))
        (t 
         (slime-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun slime-repl-recenter-if-needed ()
  "Make sure that (point) is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun slime-repl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (slime-repl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
    (slime-repl-add-to-input-history 
     (buffer-substring slime-repl-input-start-mark end))
    (when newline 
      (insert "\n")
      (slime-repl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties slime-repl-input-start-mark 
                           (point)
                           `(slime-repl-old-input
                             ,(incf slime-repl-old-input-counter))))
    (let ((overlay (make-overlay slime-repl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'slime-repl-input-face)))
  (let ((input (slime-repl-current-input)))
    (goto-char (point-max))
    (slime-mark-input-start)
    (slime-mark-output-start)
    (slime-repl-send-string input)))

(defun slime-repl-grab-old-input (replace)
  "Resend the old REPL input at point.  
If replace is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `slime-repl-old-input'."
  (multiple-value-bind (beg end) (slime-property-bounds 'slime-repl-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char slime-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion 
        (insert old-input)
        (when (equal (char-before) ?\n) 
          (delete-char -1)))
      (forward-char offset))))

(defun slime-repl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region slime-repl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (slime-repl-return))

(defun slime-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region slime-repl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun slime-repl-delete-current-input ()
  "Delete all text from the prompt."
  (interactive)
  (delete-region slime-repl-input-start-mark (point-max)))

(defun slime-repl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position slime-repl-input-start-mark) (point))
         (kill-region slime-repl-input-start-mark (point)))
        ((= (point) (marker-position slime-repl-input-start-mark))
         (slime-repl-delete-current-input))))

(defun slime-repl-replace-input (string)
  (slime-repl-delete-current-input)
  (insert-and-inherit string))

(defun slime-repl-input-line-beginning-position ()
  (save-excursion
    (goto-char slime-repl-input-start-mark)
    (line-beginning-position)))

(defvar slime-repl-clear-buffer-hook)

(defun slime-repl-clear-buffer ()
  "Delete the output generated by the Lisp process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) slime-repl-prompt-start-mark)
    (delete-region slime-output-start slime-output-end)
    (when (< (point) slime-repl-input-start-mark)
      (goto-char slime-repl-input-start-mark))
    (recenter t))
  (run-hooks 'slime-repl-clear-buffer-hook))

(defun slime-repl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion 
                 (slime-repl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (slime-repl-input-line-beginning-position))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert ";;; output flushed"))))))

(defun slime-repl-set-package (package)
  "Set the package of the REPL buffer to PACKAGE."
  (interactive (list (let* ((p (slime-current-package))
                            (p (and p (slime-pretty-package-name p)))
                            (p (and (not (equal p (slime-lisp-package))) p)))
                       (slime-read-package-name "Package: " p))))
  (with-current-buffer (slime-output-buffer)
    (let ((previouse-point (- (point) slime-repl-input-start-mark)))
      (destructuring-bind (name prompt-string)
          (slime-repl-shortcut-eval `(swank:set-package ,package))
        (setf (slime-lisp-package) name)
        (setf (slime-lisp-package-prompt-string) prompt-string)
        (setf slime-buffer-package name)
        (slime-repl-insert-prompt)
        (when (plusp previouse-point)
          (goto-char (+ previouse-point slime-repl-input-start-mark)))))))


;;;;; History

(defcustom slime-repl-wrap-history nil
  "*T to wrap history around when the end is reached."
  :type 'boolean
  :group 'slime-repl)

(make-variable-buffer-local
 (defvar slime-repl-input-history '()
   "History list of strings read from the REPL buffer."))

(defun slime-repl-add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (unless (or (equal string "")
              (equal string (car slime-repl-input-history)))
    (push string slime-repl-input-history)))

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was 'slime-repl-history-replace,
;; otherwise we reinitialize them.

(defvar slime-repl-input-history-position -1
  "Newer items have smaller indices.")

(defvar slime-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun slime-repl-history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq slime-repl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length slime-repl-input-history))
         (pos0 (cond ((slime-repl-history-search-in-progress-p)
                      slime-repl-input-history-position)
                     (t min-pos)))
         (pos (slime-repl-position-in-history pos0 direction (or regexp "")))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (slime-repl-replace-input (nth pos slime-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not slime-repl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (slime-repl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    ;;(message "%s [%d %d %s]" msg start-pos pos regexp)
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq slime-repl-input-history-position pos)
    (setq this-command 'slime-repl-history-replace)))

(defun slime-repl-history-search-in-progress-p ()
  (eq last-command 'slime-repl-history-replace))

(defun slime-repl-terminate-history-search ()
  (setq last-command this-command))

(defun slime-repl-position-in-history (start-pos direction regexp)
  "Return the position of the history item matching regexp.
Return -1 resp. the length of the history if no item matches"
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history slime-repl-input-history)
         (len (length history)))
    (loop for pos = (+ start-pos step) then (+ pos step)
          if (< pos 0) return -1
          if (<= len pos) return len
          if (string-match regexp (nth pos history)) return pos)))

(defun slime-repl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern."
  (interactive)
  (slime-repl-history-replace 'backward (slime-repl-history-pattern t)))

(defun slime-repl-next-input ()
  "Cycle forwards through input history.
See `slime-repl-previous-input'."
  (interactive)
  (slime-repl-history-replace 'forward (slime-repl-history-pattern t)))

(defun slime-repl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (slime-repl-history-replace 'forward (slime-repl-history-pattern)))

(defun slime-repl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (slime-repl-history-replace 'backward (slime-repl-history-pattern)))

(defun slime-repl-previous-matching-input (regexp)
  (interactive "sPrevious element matching (regexp): ")
  (slime-repl-terminate-history-search)
  (slime-repl-history-replace 'backward regexp))

(defun slime-repl-next-matching-input (regexp)
  (interactive "sNext element matching (regexp): ")
  (slime-repl-terminate-history-search)
  (slime-repl-history-replace 'forward regexp))

(defun slime-repl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands."
  (cond ((slime-repl-history-search-in-progress-p)
         slime-repl-history-pattern)
        (use-current-input
         (assert (<= slime-repl-input-start-mark (point)))
         (let ((str (slime-repl-current-input t)))
           (cond ((string-match "^[ \n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

(defun slime-repl-delete-from-input-history (string)
  "Delete STRING from the repl input history. 

When string is not provided then clear the current repl input and
use it as an input.  This is useful to get rid of unwanted repl
history entries while navigating the repl history."
  (interactive (list (slime-repl-current-input)))
  (let ((merged-history 
         (slime-repl-merge-histories slime-repl-input-history
                                     (slime-repl-read-history nil t))))
    (setq slime-repl-input-history
          (delete* string merged-history :test #'string=))
    (slime-repl-save-history))
  (slime-repl-delete-current-input))

;;;;; Persistent History 

(defun slime-repl-merge-histories (old-hist new-hist)
  "Merge entries from OLD-HIST and NEW-HIST."
  ;; Newer items in each list are at the beginning.
  (let* ((ht (make-hash-table :test #'equal))
         (test (lambda (entry)
                 (or (gethash entry ht)
                     (progn (setf (gethash entry ht) t)
                            nil)))))
    (append (remove-if test new-hist)
            (remove-if test old-hist))))

(defun slime-repl-load-history (&optional filename)
  "Set the current SLIME REPL history.
It can be read either from FILENAME or `slime-repl-history-file' or
from a user defined filename."
  (interactive (list (slime-repl-read-history-filename)))
  (let ((file (or filename slime-repl-history-file)))
    (setq slime-repl-input-history (slime-repl-read-history file t))))

(defun slime-repl-read-history (&optional filename noerrer)
  "Read and return the history from FILENAME.  
The default value for FILENAME is `slime-repl-history-file'.
If NOERROR is true return and the file doesn't exits return nil."
  (let ((file (or filename slime-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun slime-repl-read-history-filename ()
  (read-file-name "Use SLIME REPL history from file: " 
                  slime-repl-history-file))

(defun slime-repl-save-merged-history (&optional filename)
  "Read the history file, merge the current REPL history and save it.
This tries to be smart in merging the history from the file and the
current history in that it tries to detect the unique entries using
`slime-repl-merge-histories'."
  (interactive (list (slime-repl-read-history-filename)))
  (let ((file (or filename slime-repl-history-file)))
    (with-temp-message "saving history..."
      (let ((hist (slime-repl-merge-histories (slime-repl-read-history file t)
                                              slime-repl-input-history)))
        (slime-repl-save-history file hist)))))

(defun slime-repl-save-history (&optional filename history)
  "Simply save the current SLIME REPL history to a file.
When SLIME is setup to always load the old history and one uses only
one instance of slime all the time, there is no need to merge the
files and this function is sufficient.

When the list is longer than `slime-repl-history-size' it will be
truncated.  That part is untested, though!"
  (interactive (list (slime-repl-read-history-filename)))
  (let ((file (or filename slime-repl-history-file))
        (hist (or history slime-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (subseq hist 0 (min (length hist) slime-repl-history-size))))
      ;;(message "saving %s to %s\n" hist file)
      (with-temp-file file
        (let ((cs slime-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for SLIME REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(defun slime-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'slime-repl-mode)
        (slime-repl-safe-save-merged-history)))))

(defun slime-repl-safe-save-merged-history ()
  (slime-repl-call-with-handler 
   #'slime-repl-save-merged-history
   "%S while saving the history. Continue? "))

(defun slime-repl-safe-load-history ()
  (slime-repl-call-with-handler 
   #'slime-repl-load-history
   "%S while loading the history. Continue? "))

(defun slime-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error 
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))


;;;;; REPL Read Mode

(define-key slime-repl-mode-map
  (string slime-repl-shortcut-dispatch-char) 'slime-handle-repl-shortcut)

(define-minor-mode slime-repl-read-mode 
  "Mode the read input from Emacs
\\{slime-repl-read-mode-map}"
  nil
  "[read]"
  '(("\C-m" . slime-repl-return)
    ([return] . slime-repl-return)
    ("\C-c\C-b" . slime-repl-read-break)
    ("\C-c\C-c" . slime-repl-read-break)))

(make-variable-buffer-local
 (defvar slime-read-string-threads nil))

(make-variable-buffer-local
 (defvar slime-read-string-tags nil))

(defun slime-repl-read-string (thread tag)
  (slime-switch-to-output-buffer)
  (push thread slime-read-string-threads)
  (push tag slime-read-string-tags)
  (goto-char (point-max))
  (slime-mark-output-end)
  (slime-mark-input-start)
  (slime-repl-read-mode 1))

(defun slime-repl-return-string (string)
  (slime-dispatch-event `(:emacs-return-string 
                          ,(pop slime-read-string-threads)
                          ,(pop slime-read-string-tags)
                          ,string))
  (slime-repl-read-mode -1))

(defun slime-repl-read-break ()
  (interactive)
  (slime-dispatch-event `(:emacs-interrupt ,(car slime-read-string-threads))))

(defun slime-repl-abort-read (thread tag)
  (with-current-buffer (slime-output-buffer)
    (pop slime-read-string-threads)
    (pop slime-read-string-tags)
    (slime-repl-read-mode -1)
    (message "Read aborted")))


;;;;; REPL handlers

(defstruct (slime-repl-shortcut (:conc-name slime-repl-shortcut.))
  symbol names handler one-liner)

(defvar slime-repl-shortcut-table nil
  "A list of slime-repl-shortcuts")

(defvar slime-repl-shortcut-history '()
  "History list of shortcut command names.")

(defvar slime-within-repl-shortcut-handler-p nil
  "Bound to T if we're in a REPL shortcut handler invoked from the REPL.")

(defun slime-handle-repl-shortcut ()
  (interactive)
  (if (> (point) slime-repl-input-start-mark)
      (insert (string slime-repl-shortcut-dispatch-char))
      (let ((shortcut (slime-lookup-shortcut
                       (completing-read "Command: " 
                                        (slime-bogus-completion-alist
                                         (slime-list-all-repl-shortcuts))
                                        nil t nil
                                        'slime-repl-shortcut-history))))
        (with-struct (slime-repl-shortcut. handler) shortcut
          (let ((slime-within-repl-shortcut-handler-p t))
            (call-interactively handler))))))

(defun slime-list-all-repl-shortcuts ()
  (loop for shortcut in slime-repl-shortcut-table
        append (slime-repl-shortcut.names shortcut)))

(defun slime-lookup-shortcut (name)
  (find-if (lambda (s) (member name (slime-repl-shortcut.names s)))
           slime-repl-shortcut-table))

(defmacro defslime-repl-shortcut (elisp-name names &rest options)
  "Define a new repl shortcut. ELISP-NAME is a symbol specifying
the name of the interactive function to create, or NIL if no
function should be created. 

NAMES is a list of \(full-name . aliases\). 

OPTIONS is an plist specifying the handler doing the actual work
of the shortcut \(`:handler'\), and a help text \(`:one-liner'\)."
  `(progn
     ,(when elisp-name
        `(defun ,elisp-name ()
           (interactive)
           (call-interactively ,(second (assoc :handler options)))))
     (let ((new-shortcut (make-slime-repl-shortcut
                          :symbol ',elisp-name
                          :names (list ,@names)
                          ,@(apply #'append options))))
       (setq slime-repl-shortcut-table
             (remove-if (lambda (s)
                          (member ',(car names) (slime-repl-shortcut.names s)))
                        slime-repl-shortcut-table))
       (push new-shortcut slime-repl-shortcut-table)
       ',elisp-name)))

(defun slime-repl-shortcut-eval (sexp &optional package)
  "This function should be used by REPL shortcut handlers instead
of `slime-eval' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when slime-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (slime-repl-add-to-input-history (prin1-to-string sexp)))
  (slime-eval sexp package))

(defun slime-repl-shortcut-eval-async (sexp &optional cont package)
  "This function should be used by REPL shortcut handlers instead
of `slime-eval-async' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when slime-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (slime-repl-add-to-input-history (prin1-to-string sexp)))
  (slime-eval-async sexp cont package))


(defun slime-list-repl-short-cuts ()
  (interactive)
  (slime-with-popup-buffer ("*slime-repl-help*")
    (let ((table (sort* (copy-list slime-repl-shortcut-table) #'string<
                        :key (lambda (x) 
                               (car (slime-repl-shortcut.names x))))))
      (dolist (shortcut table)
        (let ((names (slime-repl-shortcut.names shortcut)))
          (insert (pop names)) ;; first print the "full" name
          (when names
            ;; we also have aliases
            (insert " (aka ")
            (while (cdr names)
              (insert (pop names) ", "))
            (insert (car names) ")"))
        (insert "\n     " (slime-repl-shortcut.one-liner shortcut)
                "\n"))))))

(defun slime-save-some-lisp-buffers ()
  (if slime-repl-only-save-lisp-buffers
      (save-some-buffers nil (lambda ()
                               (and (memq major-mode slime-lisp-modes)
                                    (not (null buffer-file-name)))))
      (save-some-buffers)))
  

(defslime-repl-shortcut slime-repl-shortcut-help ("help" "?")
  (:handler 'slime-list-repl-short-cuts)
  (:one-liner "Display the help."))

(defslime-repl-shortcut nil ("change-directory" "!d" "cd")
  (:handler 'slime-set-default-directory)
  (:one-liner "Change the current directory."))

(defslime-repl-shortcut nil ("pwd")
  (:handler (lambda () 
              (interactive)
              (let ((dir (slime-eval `(swank:default-directory))))
                (message "Directory %s" dir))))
  (:one-liner "Show the current directory."))

(defslime-repl-shortcut slime-repl-push-directory
    ("push-directory" "+d" "pushd")
  (:handler (lambda (directory)
              (interactive
               (list (read-directory-name
                      "Push directory: "
                      (slime-eval '(swank:default-directory))
                      nil nil "")))
              (push (slime-eval '(swank:default-directory))
                    slime-repl-directory-stack)
              (slime-set-default-directory directory)))
  (:one-liner "Save the current directory and set it to a new one."))

(defslime-repl-shortcut slime-repl-pop-directory
    ("pop-directory" "-d" "popd")
  (:handler (lambda ()
              (interactive)
              (if (null slime-repl-directory-stack)
                  (message "Directory stack is empty.")
                  (slime-set-default-directory
                   (pop slime-repl-directory-stack)))))
  (:one-liner "Restore the last saved directory."))

(defslime-repl-shortcut nil ("change-package" "!p" "in-package" "in")
  (:handler 'slime-repl-set-package)
  (:one-liner "Change the current package."))

(defslime-repl-shortcut slime-repl-push-package ("push-package" "+p")
  (:handler (lambda (package)
              (interactive (list (slime-read-package-name "Package: ")))
              (push (slime-lisp-package) slime-repl-package-stack)
              (slime-repl-set-package package)))
  (:one-liner "Save the current package and set it to a new one."))

(defslime-repl-shortcut slime-repl-pop-package ("pop-package" "-p")
  (:handler (lambda ()
              (interactive)
              (if (null slime-repl-package-stack)
                  (message "Package stack is empty.")
                  (slime-repl-set-package
                   (pop slime-repl-package-stack)))))
  (:one-liner "Restore the last saved package."))

(defslime-repl-shortcut slime-repl-resend ("resend-form")
  (:handler (lambda ()
              (interactive)
              (insert (car slime-repl-input-history))
              (insert "\n")
              (slime-repl-send-input)))
  (:one-liner "Resend the last form."))

(defslime-repl-shortcut slime-repl-disconnect ("disconnect")
  (:handler 'slime-disconnect)
  (:one-liner "Disconnect the current connection."))

(defslime-repl-shortcut slime-repl-disconnect-all ("disconnect-all")
  (:handler 'slime-disconnect-all)
  (:one-liner "Disconnect all connections."))

(defslime-repl-shortcut slime-repl-sayoonara ("sayoonara")
  (:handler (lambda ()
              (interactive)
              (when (slime-connected-p)
                (slime-quit-lisp))
              (slime-kill-all-buffers)))
  (:one-liner "Quit all Lisps and close all SLIME buffers."))

(defslime-repl-shortcut slime-repl-quit ("quit")
  (:handler (lambda ()
	      (interactive)
              ;; `slime-quit-lisp' determines the connection to quit
              ;; on behalf of the REPL's `slime-buffer-connection'.
              (let ((repl-buffer (slime-output-buffer)))
                (slime-quit-lisp)
                (kill-buffer repl-buffer))))
  (:one-liner "Quit the current Lisp."))

(defslime-repl-shortcut slime-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (slime-read-symbol-name "Name (symbol): " t)
                                 (slime-read-from-minibuffer "Value: " "*")))
              (insert "(cl:defparameter " name " " value 
                      " \"REPL generated global variable.\")")
              (slime-repl-send-input t)))
  (:one-liner "Define a new global, special, variable."))

(defslime-repl-shortcut slime-repl-compile-and-load ("compile-and-load" "cl")
  (:handler (lambda (filename)
              (interactive (list (expand-file-name
                                  (read-file-name "File: " nil nil nil nil))))
              (slime-save-some-lisp-buffers)
              (slime-repl-shortcut-eval-async
               `(swank:compile-file-if-needed 
                 ,(slime-to-lisp-filename filename) t)
               #'slime-compilation-finished)))
  (:one-liner "Compile (if neccessary) and load a lisp file."))

(defslime-repl-shortcut nil  ("restart-inferior-lisp")
  (:handler 'slime-restart-inferior-lisp)
  (:one-liner "Restart *inferior-lisp* and reconnect SLIME."))

(defun slime-redirect-inferior-output (&optional noerror)
  "Redirect output of the inferior-process to the REPL buffer."
  (interactive)
  (let ((proc (slime-inferior-process)))
    (cond (proc
           (let ((filter (slime-rcurry #'slime-inferior-output-filter 
                                       (slime-current-connection))))
             (set-process-filter proc filter)))
	  (noerror)
	  (t (error "No inferior lisp process")))))

(defun slime-inferior-output-filter (proc string conn)
  (cond ((eq (process-status conn) 'closed)
         (message "Connection closed.  Removing inferior output filter.")
         (message "Lost output: %S" string)
         (set-process-filter proc nil))
        (t
         (slime-output-filter conn string))))

(defun slime-redirect-trace-output ()
  "Redirect the trace output to a separate Emacs buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*SLIME Trace Output*")))
    (with-current-buffer buffer
      (let ((marker (copy-marker (buffer-size)))
            (target (incf slime-last-output-target-id)))
        (puthash target marker slime-output-target-to-marker)
        (slime-eval `(swank:redirect-trace-output ,target))))
    ;; Note: We would like the entries in
    ;; slime-output-target-to-marker to disappear when the buffers are
    ;; killed.  We cannot just make the hash-table ":weakness 'value"
    ;; -- there is no reference from the buffers to the markers in the
    ;; buffer, so entries would disappear even though the buffers are
    ;; alive.  Best solution might be to make buffer-local variables
    ;; that keep the markers. --mkoeppe
    (pop-to-buffer buffer)))

(defun slime-call-defun ()
  "Insert a call to the toplevel form defined around point into the REPL."
  (interactive)
  (flet ((insert-call (symbol)
           (let* ((qualified-symbol-name (slime-qualify-cl-symbol-name symbol))
                  (symbol-name (slime-cl-symbol-name qualified-symbol-name))
                  (symbol-package (slime-cl-symbol-package qualified-symbol-name))
                  (function-call 
                   (format "(%s " (if (equalp (slime-lisp-package) symbol-package)
                                      symbol-name
                                      qualified-symbol-name))))
             (slime-switch-to-output-buffer)
             (goto-char slime-repl-input-start-mark)
             (insert function-call)
             (save-excursion (insert ")")))))           
    (let ((toplevel (slime-parse-toplevel-form)))
      (if (symbolp toplevel)
          (error "Not in a function definition")
          (destructure-case toplevel
            (((:defun :defgeneric :defmacro :define-compiler-macro) symbol)
             (insert-call symbol))
            ((:defmethod symbol &rest args)
             (declare (ignore args))
             (insert-call symbol))
            (t
             (error "Not in a function definition")))))))

(defun slime-inspector-copy-down-to-repl (number)
   "Evaluate the inspector slot at point via the REPL (to set `*')."
   (interactive (list (or (get-text-property (point) 'slime-part-number)
                          (error "No part at point"))))
   (slime-repl-send-string (format "%s" `(swank:inspector-nth-part ,number)))
   (slime-repl))


(defun slime-set-default-directory (directory)
  "Make DIRECTORY become Lisp's current directory."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (let ((dir (expand-file-name directory)))
    (message "default-directory: %s"
             (slime-from-lisp-filename
              (slime-repl-shortcut-eval `(swank:set-default-directory
                                          ,(slime-to-lisp-filename dir)))))
    (with-current-buffer (slime-output-buffer)
      (setq default-directory dir))))

(defun slime-sync-package-and-default-directory ()
  "Set Lisp's package and directory to the values in current buffer."
  (interactive)
  (let* ((package (slime-current-package))
         (exists-p (or (null package)
                       (slime-eval `(cl:packagep (swank::guess-package ,package)))))
         (directory default-directory))
    (when (and package exists-p)
      (slime-repl-set-package package))
    (slime-set-default-directory directory)
    ;; Sync *inferior-lisp* dir
    (let* ((proc (slime-process))
           (buffer (and proc (process-buffer proc))))
      (when buffer
        (with-current-buffer buffer
          (setq default-directory directory))))
    (message "package: %s%s  directory: %s"
             (with-current-buffer (slime-output-buffer)
               (slime-lisp-package))
             (if exists-p "" (format " (package %s doesn't exist)" package))
             directory)))

(defun slime-goto-connection ()
  "Switch to the REPL buffer for the connection at point."
  (interactive)
  (let ((slime-dispatching-connection (slime-connection-at-point)))
    (switch-to-buffer (slime-output-buffer))))

(defvar slime-repl-easy-menu
  (let ((C '(slime-connected-p)))
    `("REPL"
      [ "Send Input"             slime-repl-return ,C ]
      [ "Close and Send Input "  slime-repl-closing-return ,C ]
      [ "Interrupt Lisp process" slime-interrupt ,C ]
      "--"
      [ "Previous Input"         slime-repl-previous-input t ]
      [ "Next Input"             slime-repl-next-input t ]
      [ "Goto Previous Prompt "  slime-repl-previous-prompt t ]
      [ "Goto Next Prompt "      slime-repl-next-prompt t ]
      [ "Clear Last Output"      slime-repl-clear-output t ]
      [ "Clear Buffer "          slime-repl-clear-buffer t ]
      [ "Kill Current Input"     slime-repl-kill-input t ])))

(defun slime-repl-add-easy-menu ()
  (easy-menu-define menubar-slime-repl slime-repl-mode-map
    "REPL" slime-repl-easy-menu)
  (easy-menu-define menubar-slime slime-repl-mode-map 
    "SLIME" slime-easy-menu)
  (easy-menu-add slime-repl-easy-menu 'slime-repl-mode-map))

(add-hook 'slime-repl-mode-hook 'slime-repl-add-easy-menu)

(defun slime-hide-inferior-lisp-buffer ()
  "Display the REPL buffer instead of the *inferior-lisp* buffer."
  (let* ((buffer (if (slime-process) 
                     (process-buffer (slime-process))))
         (window (if buffer (get-buffer-window buffer t)))
         (repl-buffer (slime-output-buffer t))
         (repl-window (get-buffer-window repl-buffer)))
    (when buffer
      (bury-buffer buffer))
    (cond (repl-window
           (when window
             (delete-window window)))
          (window
           (set-window-buffer window repl-buffer))
          (t
           (pop-to-buffer repl-buffer)
           (goto-char (point-max))))))

(defun slime-repl-connected-hook-function ()
  (destructuring-bind (package prompt) 
      (let ((slime-current-thread t))
	(slime-eval '(swank:create-repl nil)))
    (setf (slime-lisp-package) package)
    (setf (slime-lisp-package-prompt-string) prompt))
  (slime-hide-inferior-lisp-buffer)
  (slime-init-output-buffer (slime-connection)))

(defun slime-repl-event-hook-function (event)
  (destructure-case event
    ((:write-string output &optional target)
     (slime-write-string output target)
     t)
    ((:read-string thread tag)
     (assert thread)
     (slime-repl-read-string thread tag)
     t)
    ((:read-aborted thread tag)
     (slime-repl-abort-read thread tag)
     t)
    ((:open-dedicated-output-stream port)
     (slime-open-stream-to-lisp port)
     t)
    ((:new-package package prompt-string)
     (setf (slime-lisp-package) package)
     (setf (slime-lisp-package-prompt-string) prompt-string)
     (let ((buffer (slime-connection-output-buffer)))
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (setq slime-buffer-package package))))
     t)
    (t nil)))

(defun slime-repl-find-buffer-package ()
  (or (slime-search-buffer-package)
      (slime-lisp-package)))

;;;###autoload
(defun slime-repl-init ()
  (add-hook 'slime-event-hooks 'slime-repl-event-hook-function)
  (add-hook 'slime-connected-hook 'slime-repl-connected-hook-function)
  (setq slime-find-buffer-package-function 'slime-repl-find-buffer-package))

(defun slime-repl-remove-hooks ()
  (remove-hook 'slime-event-hooks 'slime-repl-event-hook-function)
  (remove-hook 'slime-connected-hook 'slime-repl-connected-hook-function))

(def-slime-test package-updating
    (package-name nicknames)
    "Test if slime-lisp-package is updated."
    '(("COMMON-LISP" ("CL"))
      ("KEYWORD" ("" "KEYWORD" "||"))
      ("COMMON-LISP-USER" ("CL-USER")))
  (with-current-buffer (slime-output-buffer)
    (let ((p (slime-eval 
              `(swank:listener-eval 
                ,(format 
                  "(cl:setq cl:*print-case* :upcase)
                   (cl:setq cl:*package* (cl:find-package %S))
                   (cl:package-name cl:*package*)" package-name))
              (slime-lisp-package))))
      (slime-check ("slime-lisp-package is %S." package-name)
        (equal (slime-lisp-package) package-name))
      (slime-check ("slime-lisp-package-prompt-string is in %S." nicknames)
        (member (slime-lisp-package-prompt-string) nicknames)))))

(defmacro with-canonicalized-slime-repl-buffer (&rest body)
  "Evaluate BODY within a fresh REPL buffer. The REPL prompt is
canonicalized to \"SWANK\"---we do actually switch to that
package, though."
  `(let ((%old-prompt% (slime-lisp-package-prompt-string)))
     (unwind-protect
          (progn (with-current-buffer (slime-output-buffer)
                   (setf (slime-lisp-package-prompt-string) "SWANK"))
                 (kill-buffer (slime-output-buffer))
                 (with-current-buffer (slime-output-buffer)
                   ,@body))
       (setf (slime-lisp-package-prompt-string) %old-prompt%))))

(put 'with-canonicalized-slime-repl-buffer 'lisp-indent-function 0)

(def-slime-test repl-test
    (input result-contents)
    "Test simple commands in the minibuffer."
    '(("(+ 1 2)" "SWANK> (+ 1 2)
{}3
SWANK> *[]")
      ("(princ 10)" "SWANK> (princ 10)
{10
}10
SWANK> *[]")
      ("(princ 10)(princ 20)" "SWANK> (princ 10)(princ 20)
{1020
}20
SWANK> *[]")
      ("(dotimes (i 10 77) (princ i) (terpri))" 
       "SWANK> (dotimes (i 10 77) (princ i) (terpri))
{0
1
2
3
4
5
6
7
8
9
}77
SWANK> *[]")
      ("(abort)" "SWANK> (abort)
{}; Evaluation aborted.
SWANK> *[]")
      ("(progn (princ 10) (force-output) (abort))" 
       "SWANK> (progn (princ 10) (force-output) (abort))
{10}; Evaluation aborted.
SWANK> *[]")
      ("(progn (princ 10) (abort))" 
       ;; output can be flushed after aborting
       "SWANK> (progn (princ 10) (abort))
{10}; Evaluation aborted.
SWANK> *[]")
      ("(if (fresh-line) 1 0)"
       "SWANK> (if (fresh-line) 1 0)
{
}1
SWANK> *[]")
      ("(values 1 2 3)" "SWANK> (values 1 2 3)
{}1
2
3
SWANK> *[]")
      ("(with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)"
       "SWANK> (with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]")
      ;; Two times to test the effect of FRESH-LINE.
      ("(with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)"
       "SWANK> (with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]"))
  (with-canonicalized-slime-repl-buffer
    (insert input)
    (slime-check-buffer-contents "Buffer contains input" 
                                 (concat "{}SWANK> [" input "*]"))
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-check-buffer-contents "Buffer contains result" result-contents)))

(defun slime-check-buffer-contents (msg expected)
  (let* ((marks '((point . ?*) 
                  (slime-output-start . ?{) (slime-output-end . ?}) 
                  (slime-repl-input-start-mark . ?\[) (point-max . ?\])))
         (marks (remove-if-not (lambda (m) (position (cdr m) expected))
                               marks))
         (marks (sort (copy-sequence marks) 
                      (lambda (x y)
                        (< (position (cdr x) expected)
                           (position (cdr y) expected)))))
         (content (remove-if (lambda (c) (member* c marks :key #'cdr))
                             expected))
         (marks (do ((result '() (acons (caar m) (1+ (position (cdar m) s))
                                        result))
                     (m marks (cdr m))
                     (s expected (remove* (cdar m) s)))
                    ((null m) (reverse result))))
         (point (point))
         (point-max (point-max)))
    (slime-test-expect (concat msg " [content]") content (buffer-string))
    (macrolet ((test-mark 
                (mark)
                `(when (assoc ',mark marks)
                   (slime-test-expect (format "%s [%s]" msg ',mark)
                                      (cdr (assoc ',mark marks))
                                      ,mark
                                      #'=))))
      (test-mark point)
      (test-mark slime-output-end)
      (test-mark slime-output-start)
      (test-mark slime-repl-input-start-mark)
      (test-mark point-max))))

(def-slime-test repl-return 
    (before after result-contents)
    "Test if slime-repl-return sends the correct protion to Lisp even
if point is not at the end of the line."
    '(("(+ 1 2)" "" "SWANK> (+ 1 2)
3
SWANK> ")
("(+ 1 " "2)" "SWANK> (+ 1 2)
3
SWANK> ")

("(+ 1\n" "2)" "SWANK> (+ 1
2)
3
SWANK> "))
  (with-canonicalized-slime-repl-buffer
    (insert before)
    (save-excursion (insert after))
    (slime-test-expect "Buffer contains input" 
                       (concat "SWANK> " before after)
                       (buffer-string))
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result" 
                       result-contents (buffer-string))))
  
(def-slime-test repl-read
    (prompt input result-contents)
    "Test simple commands in the minibuffer."
    '(("(read-line)" "foo" "SWANK> (values (read-line))
foo
\"foo\"
SWANK> ")
      ("(read-char)" "1" "SWANK> (values (read-char))
1
#\\1
SWANK> ")
      ("(read)" "(+ 2 3
4)" "SWANK> (values (read))
\(+ 2 3
4)
\(+ 2 3 4)
SWANK> "))
  (with-canonicalized-slime-repl-buffer
    (insert (format "(values %s)" prompt))
    (call-interactively 'slime-repl-return)
    (slime-wait-condition "reading" #'slime-reading-p 5)
    (insert input)
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result" 
                       result-contents (buffer-string))))

(def-slime-test repl-read-lines
    (command inputs final-contents)
    "Test reading multiple lines from the repl."
    '(("(list (read-line) (read-line) (read-line))" 
       ("a" "b" "c")
       "SWANK> (list (read-line) (read-line) (read-line))
a
b
c
\(\"a\" \"b\" \"c\")
SWANK> "))
  (with-canonicalized-slime-repl-buffer
    (insert command)
    (call-interactively 'slime-repl-return)
    (dolist (input inputs) 
      (slime-wait-condition "reading" #'slime-reading-p 5)
      (insert input)
      (call-interactively 'slime-repl-return))
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result"
                       final-contents 
                       (buffer-string)
                       #'equal)))

(def-slime-test repl-type-ahead
    (command input final-contents)
    "Ensure that user input is preserved correctly.
In particular, input inserted while waiting for a result."
    '(("(sleep 0.1)" "foo*" "SWANK> (sleep 0.1)
{}NIL
SWANK> [foo*]")
      ("(sleep 0.1)" "*foo" "SWANK> (sleep 0.1)
{}NIL
SWANK> [*foo]")
      ("(progn (sleep 0.1) (abort))" "*foo" "SWANK> (progn (sleep 0.1) (abort))
{}; Evaluation aborted.
SWANK> [*foo]"))
  (with-canonicalized-slime-repl-buffer
    (insert command)
    (call-interactively 'slime-repl-return)
    (save-excursion (insert (delete* ?* input)))
    (forward-char (position ?* input))
    (slime-sync-to-top-level 5)
    (slime-check-buffer-contents "Buffer contains result" final-contents)))


(def-slime-test interrupt-in-blocking-read
    ()
    "Let's see what happens if we interrupt a blocking read operation."
    '(())
  (slime-check-top-level)
  (with-canonicalized-slime-repl-buffer
    (insert "(read-char)")
    (call-interactively 'slime-repl-return)
    (slime-wait-condition "reading" #'slime-reading-p 5)
    (slime-interrupt)
    (slime-wait-condition "Debugger visible" 
                          (lambda () 
                            (and (slime-sldb-level= 1)
                                 (get-buffer-window (sldb-get-default-buffer))))
                          5)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-continue))
    (slime-wait-condition "reading" #'slime-reading-p 5)
    (with-current-buffer (slime-output-buffer)
      (insert "X")
      (call-interactively 'slime-repl-return)
      (slime-sync-to-top-level 5)
      (slime-test-expect "Buffer contains result" 
                         "SWANK> (read-char)
X
#\\X
SWANK> " (buffer-string)))))

(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
	'(slime-repl-event-hook-function
	  slime-write-string
	  slime-repl-write-string
	  slime-repl-emit
	  slime-repl-show-maximum-output)))

;;;###autoload
(add-hook 'slime-load-hook 'slime-repl-init)

(provide 'slime-repl)
;;; slime-repl.el ends here

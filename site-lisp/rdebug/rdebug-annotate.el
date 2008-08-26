;;; rdebug-annotate.el --- Ruby debugger output filtering - which
;;; includes annotation handling.

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-annotate.el 786 2008-04-02 00:50:27Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains code dealing with filter of debugger output a large
;; part of which may contain annotations.

;;; Code:

(require 'gud)
(require 'gdb-ui)
(require 'rdebug-dbg)
(require 'rdebug-error)
(require 'rdebug-fns)
(require 'rdebug-info)
(require 'rdebug-layouts)
(require 'rdebug-locring)
(require 'rdebug-regexp)
(require 'rdebug-shortkey)
(require 'rdebug-source)
(require 'rdebug-vars)

(defvar rdebug-non-annotated-text-kind nil
  "Represent what non-annotated text is.

This can be:
 * nil      -- plain shell output
 * :output  -- output from the command being debugged
 * :info    -- text for the \"info\" secondary window.
 * :message -- message the text to the echo area.
 * :cmd     -- a command + result, which might go into the \"info\" window.

See the function `rdebug-cmd-process' for details on :cmd.")

(defvar rdebug-annotation-setup-map
  (progn
    (define-hash-table-test 'str-hash 'string= 'sxhash)
    (let ((map (make-hash-table :test 'str-hash)))
      (puthash "breakpoints" 'rdebug-setup-breakpoints-buffer           map)
      ;;(puthash "error"       'rdebug-setup-error-buffer               map)
      (puthash "frame"       'rdebug-setup-frame-buffer                 map)
      (puthash "variables"   'rdebug-setup-variables-buffer             map)
      (puthash "watch"       'rdebug-setup-watch-buffer                 map)
      (puthash "output"      'rdebug-setup-output-buffer                map)
      (puthash "info"        'rdebug-setup-info-buffer                  map)
      (puthash "help"        'rdebug-setup-secondary-window-help-buffer map)
      map)))

(defun rdebug-temp-show (text)
  "Arrange to show string as in sort of temporary way. Perhaps like a tooltip"
  (tooltip-show text))

(defun rdebug-marker-filter-next-item (string)
  "The next item for the rdebug marker filter to process.

Return (item . rest) or nil."
  (rdebug-debug-message "ACC: %S" string)
  (cond
   ;; Empty line, we're done.
   ((equal (length string) 0)
    nil)
   ;; A single ^Z, this could become a new annotation, so lets stop here.
   ((string= string "\032")
    nil)
   ;; A half-baked annotation, lets stop here.
   ((and (string-match "^\032\032" string)
         (not (string-match "\n" string)))
    nil)
   (t
    (let ((split-point
           (cond ((string-match "\032\032" string)
                  (let ((beg (match-beginning 0)))
                    (if (equal beg 0)
                        (if (string-match "^\032\032" string 2)
                            (match-beginning 0)
                          (length string))
                      beg)))
                 ((eq (elt string (- (length string) 1)) ?\32)
                  -1)
                 (t
                  (length string)))))
      (cons (substring string 0 split-point) (substring string split-point))))))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-rdebug-marker-filter (string)
  "Filter function for process output of the rdebug Ruby debugger."
  (rdebug-debug-enter "gud-rdebug-marker-filter:"
    (rdebug-debug-message "GOT: %S" string)
    (if rdebug-non-annotated-text-kind
        (rdebug-debug-message "  Text is %S" rdebug-non-annotated-text-kind))
    (setq gud-marker-acc (concat gud-marker-acc string))
    (rdebug-debug-message "TOT: %S" gud-marker-acc)
    (let ((shell-output "")         ; Output to debugger shell window.
          (done nil)
          item)
      ;; The following loop peels of one "item" at a time. An item is
      ;; a un-annotated section or an annotation. (This is taken care
      ;; of by the `rdebug-marker-filter-next-item' function.)
      ;;
      ;; An Annotation can be a one-liner (where anything following
      ;; the annotation is treated as un-annotated text) or a full
      ;; annotation (which stretches to the next annotation).
      ;;
      ;; The concept of one-liners (no phun intended) is to allow
      ;; continuous output, a "starting" annotation simply sets up the
      ;; environment for sending lines to the output window, any text
      ;; following it right now, or in later chunks of data, is
      ;; redirected to the output window.
      (while (and (not done)
                  (let ((pair (rdebug-marker-filter-next-item gud-marker-acc)))
                    (rdebug-debug-message "Next item: %S" pair)
                    (and pair
                         (progn
                           (setq item (car pair))
                           (setq gud-marker-acc (cdr pair))
                           t))))
        ;; Note: Regexp:s are greedy, i.e. the char parts wins over
        ;; the .* part.
        (if (not (string-match "^\032\032\\([-a-z]*\\).*\n" item))
            ;; Non-annotated text (or the content of one-liners) goes
            ;; straight into the debugger shell window, or to the
            ;; output window.
            (cond ((and (eq rdebug-non-annotated-text-kind :output)
                        rdebug-use-separate-io-buffer)
                   (rdebug-process-annotation "starting" item))
                  ((eq rdebug-non-annotated-text-kind :info)
                   (rdebug-process-annotation "info" item))
                  (t
                   (if (eq rdebug-non-annotated-text-kind :cmd)
                       (rdebug-cmd-process item))
                   (setq shell-output (concat shell-output item))))
          ;; Handle annotation.
          (let* ((line-end (match-end 0))
                 (name (match-string 1 item))
                 ;; "prompt" is needed to handle "quit" in the shell correctly.
                 (one-liner
                  (member name
                          '("" "exited" "source" "prompt" "starting")))
                 (next-annotation (string-match "\032\032"
                                                gud-marker-acc)))
            ;; For one-liners, shuffle some text back to the accumulator.
            (when one-liner
              (setq gud-marker-acc (concat (substring item line-end)
                                           gud-marker-acc))
              (setq item (substring item 0 line-end)))
            (if (or next-annotation
                    one-liner)
                ;; ok, annotation complete, process it and remove it
                (let* ((contents (substring item line-end))
                       (old-kind rdebug-non-annotated-text-kind))
                  (rdebug-debug-message "Name: %S Content: %S Kind: %S"
                                        name contents
                                        rdebug-non-annotated-text-kind)

                  ;; This is a global state flag, this allows us to
                  ;; redirect any further text to the output buffer.
                  (set
                   (make-local-variable 'rdebug-non-annotated-text-kind)
                   (cond ((string= name "starting")
                          :output)
                         ((string= name "prompt")
                          (rdebug-cmd-clear)
                          :cmd)
                         ((string= name "exited")
                          ;; Create a fake command whose output we
                          ;; handle in the cmd system. (We might not
                          ;; receive all of the message at once, we we
                          ;; need some kind of accumukator, which the
                          ;; cmd system provides.)
                          (setq rdebug-inferior-status "exited")
                          (rdebug-cmd-clear)
                          (setq rdebug-call-queue
                                (cons '("***exited***" :message)
                                      rdebug-call-queue))
                          :cmd)
                         (t nil)))

                  (when (and (eq old-kind :cmd)
                             (not (eq rdebug-non-annotated-text-kind :cmd)))
                    (rdebug-debug-message
                     "New kind: %S" rdebug-non-annotated-text-kind)
		    (rdebug-cmd-done))

                  ;; Process the annotation.
                  (cond ((string= name "starting")
			 (setq rdebug-inferior-status "running"))
                        ((string= name "stopped")
			 (setq rdebug-inferior-status "stopped"))
                        ((string= name "exited")
			 (setq rdebug-inferior-status "exited"))
                        ((string= name "pre-prompt")
                         ;; Strip of the trailing \n (this is probably
                         ;; a bug in processor.rb).
                         (if (string= (substring contents -1) "\n")
                             (setq contents (substring contents 0 -1)))
                         (if (string-match "post-mortem" contents)
                             (setq rdebug-inferior-status "crashed"))
                         (setq shell-output (concat shell-output contents)))
                        ((string= name "source")
                         (if (string-match gud-rdebug-marker-regexp item)
                             ;; Extract the frame position from the marker.
			     (setq gud-last-frame
				   (cons (match-string 1 item)
					 (string-to-number
					  (match-string 2 item))))))
                        (t (rdebug-process-annotation name contents))))
              ;; This is not a one-liner, and we haven't seen the next
              ;; annotation, so we have to treat this as a partial
              ;; annotation. Save it and hope that the we can process
              ;; it the next time we're called.
              (setq gud-marker-acc (concat item gud-marker-acc))
              (setq done t)))))

      (when gud-last-frame
	;; Display the source file where we want it, gud will only pick
	;; an arbitrary window.
	(rdebug-pick-source-window)
	(rdebug-set-frame-arrow (gud-find-file (car gud-last-frame)))
	(if (equal 0 rdebug-frames-current-frame-number)
	    (rdebug-locring-add gud-last-frame 
				rdebug-source-location-ring)))
      (rdebug-short-key-mode-maybe-activate)

      (unless (string= shell-output "")
        (rdebug-debug-message "Output: %S" shell-output))
      (rdebug-debug-message "REM: %S" gud-marker-acc)

      shell-output)))

(defun rdebug-process-annotation (name contents)
  "Called after `gud-rdebug-marker-filter' found a complete
`name' annotation with string `contents'. Send it to the right
place for processing."
  (rdebug-debug-enter (format "rdebug-process-annotation %s" name)
    ;; Ruby-debug uses the name "starting" for process output (just like
    ;; GDB). However, it's better to present the buffer as "output" to
    ;; the user. Ditto for "display" and "watch".
    (cond ((string= name "starting")
           (setq name "output"))
          ((string= name "display")
           (setq name "watch"))
          ((string= name "stack")
           (setq name "frame"))
          ((string= name "error-begin")
           (setq name "error")))
    ;; New "info"
    (if (string= name "exited")
        (setq name "info"))
    (if (string= name "error")
        (rdebug-errmsg contents))
    (let ((setup-func (gethash name rdebug-annotation-setup-map)))
      (when setup-func
        (let ((buf (rdebug-get-buffer name gud-target-name))
              ;; Buffer local, doesn't survive the buffer change.
              (comint-buffer gud-comint-buffer))
          (with-current-buffer buf
            (setq buffer-read-only t)
            (let ((inhibit-read-only t))
              (set (make-local-variable 'rdebug-current-line-number)
                   (line-number-at-pos))
              (set (make-local-variable 'gud-last-frame) gud-last-frame)
              (if rdebug-accumulative-buffer
                  (goto-char (point-max))
                (erase-buffer))
              (insert contents)
              (funcall setup-func buf comint-buffer))))))
    (cond ((and (string= name "info")
                (not (string= contents "")))
           (save-selected-window
             (rdebug-display-info-buffer))))))


;; ------------------------------------------------------------
;; Mode line displayer.
;;

;; The variable rdebug-mode-line-process uses this to generate the
;; actual string to display.
(defun rdebug-display-inferior-status ()
  "Return a (propertized) string, or nil, to be displayed in the mode line."
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer)
           rdebug-inferior-status)
      (let ((s rdebug-inferior-status))
        (cond ((string= rdebug-inferior-status "running")
               (setq s (propertize s 'face font-lock-type-face)))
              (t
               (setq s (propertize s 'face font-lock-warning-face))))
        (concat ":" s))
    ;; No process, don't display anything.
    nil))

;; ------------------------------------------------------------
;; Command output parser.
;;

(defvar rdebug-cmd-acc ""
  "The accumulated output of the current command.

Note, on some systems the external process echoes the command,
which is included in the output.")

;; Called when a new command starts.
(defun rdebug-cmd-clear ()
  "Called when the Rdebug filter find the start of a new commands."
  (rdebug-debug-enter "rdebug-cmd-clear"
    (setq rdebug-cmd-acc "")))

;; Called with command output, this can be called any number of times.
(defun rdebug-cmd-process (s)
  "Called when the Rdebug filter find the command output.
This may be called any number of times."
  (rdebug-debug-enter (format "rdebug-cmd-process %S" s)
    (setq rdebug-cmd-acc (concat rdebug-cmd-acc s))))

;; Called when command has finished.
(defun rdebug-cmd-done ()
  "Called when the Rdebug filter find the end of a commands."
  (rdebug-debug-enter "rdebug-cmd-done"
    ;; car-safe is used since rdebug-call-queue can be empty.
    (let ((entry (car-safe rdebug-call-queue))
          (text rdebug-cmd-acc))
      (when entry
        (rdebug-debug-message "Entry: %S Acc:%S" rdebug-call-queue rdebug-cmd-acc)
        (setq rdebug-call-queue (cdr rdebug-call-queue))
        (let ((saved-cmd (car entry))
              (options (cdr entry)))
          ;; In cast the external process echoed the actual command,
          ;; remove it.
          (when (and (>= (length text)
			 (length saved-cmd))
		     (string= saved-cmd (substring text 0 (length saved-cmd))))
	    (setq text (substring text (+ 1 (length saved-cmd)))))
          (rdebug-debug-message "Text: %S" text)
          ;; Optionally display the result.
          (if (memq :tooltip options)
              (rdebug-temp-show text))
          (if (memq :info options)
              (rdebug-process-annotation "info" text))
          (when (memq :message options)
            ;; Remove trailing newlines (chomp).
            (while (and (> (length text) 0)
                        (eq (elt text (- (length text) 1)) ?\n))
              (setq text (substring text 0 -1)))
            (message text)))))))


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug-annotate)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-annotate.el ends here

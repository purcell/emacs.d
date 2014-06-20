;;; -*-Emacs-Lisp-*-
;;;%Header
;;; Bridge process filter, V1.0
;;; Copyright (C) 1991 Chris McConnell, ccm@cs.cmu.edu  
;;;
;;; Send mail to ilisp@cons.org if you have problems.
;;;
;;; Send mail to majordomo@cons.org if you want to be on the
;;; ilisp mailing list.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; Send any bugs or comments.  Thanks to Todd Kaufmann for rewriting
;;; the process filter for continuous handlers.

;;; USAGE: M-x install-bridge will add a process output filter to the
;;; current buffer.  Any output that the process does between
;;; bridge-start-regexp and bridge-end-regexp will be bundled up and
;;; passed to the first handler on bridge-handlers that matches the
;;; output using string-match.  If bridge-prompt-regexp shows up
;;; before bridge-end-regexp, the bridge will be cancelled.  If no
;;; handler matches the output, the first symbol in the output is
;;; assumed to be a buffer name and the rest of the output will be
;;; sent to that buffer's process.  This can be used to communicate
;;; between processes or to set up two way interactions between Emacs
;;; and an inferior process.

;;; You can write handlers that process the output in special ways.
;;; See bridge-send-handler for the default handler.  The command
;;; hand-bridge is useful for testing.  Keep in mind that all
;;; variables are buffer local.

;;; YOUR .EMACS FILE:
;;;
;;; ;;; Set up load path to include bridge
;;; (setq load-path (cons "/bridge-directory/" load-path))
;;; (autoload 'install-bridge "bridge" "Install a process bridge." t)
;;; (setq bridge-hook 
;;;       '(lambda ()
;;;         ;; Example options
;;;         (setq bridge-source-insert nil) ;Don't insert in source buffer
;;;         (setq bridge-destination-insert nil) ;Don't insert in dest buffer
;;;         ;; Handle copy-it messages yourself
;;;         (setq bridge-handlers
;;;          '(("copy-it" . my-copy-handler)))))

;;; EXAMPLE:
;;; # This pipes stdin to the named buffer in a Unix shell
;;; alias devgnu '(echo -n "\!* "; cat -; echo -n "")'
;;;
;;; ls | devgnu *scratch*

(eval-when-compile
  (require 'cl))

;;;%Parameters
(defvar bridge-hook nil
  "Hook called when a bridge is installed by install-hook.")

(defvar bridge-start-regexp ""
  "*Regular expression to match the start of a process bridge in
process output.  It should be followed by a buffer name, the data to
be sent and a bridge-end-regexp.")

(defvar bridge-end-regexp ""
  "*Regular expression to match the end of a process bridge in process
output.")

(defvar bridge-prompt-regexp nil
  "*Regular expression for detecting a prompt.  If there is a
comint-prompt-regexp, it will be initialized to that.  A prompt before
a bridge-end-regexp will stop the process bridge.")

(defvar bridge-handlers nil
  "Alist of (regexp . handler) for handling process output delimited
by bridge-start-regexp and bridge-end-regexp.  The first entry on the
list whose regexp matches the output will be called on the process and
the delimited output.")

(defvar bridge-source-insert t
  "*T to insert bridge input in the source buffer minus delimiters.")

(defvar bridge-destination-insert t
  "*T for bridge-send-handler to insert bridge input into the
destination buffer minus delimiters.")

(defvar bridge-chunk-size 512
  "*Long inputs send to comint processes are broken up into chunks of
this size.  If your process is choking on big inputs, try lowering the
value.")

;;;%Internal variables
(defvar bridge-old-filter nil
  "Old filter for a bridged process buffer.")

(defvar bridge-string nil 
  "The current output in the process bridge.")

(defvar bridge-in-progress nil
  "The current handler function, if any, that bridge passes strings on to,
or nil if none.")

(defvar bridge-leftovers nil
  "Because of chunking you might get an incomplete bridge signal - start but the end is in the next packet. Save the overhanging text here.")

(defvar bridge-send-to-buffer nil
  "The buffer that the default bridge-handler (bridge-send-handler) is
currently sending to, or nil if it hasn't started yet.  Your handler
function can use this variable also.")

(defvar bridge-last-failure ()
  "Last thing that broke the bridge handler.  First item is function call
(eval'able); last item is error condition which resulted.  This is provided
to help handler-writers in their debugging.")

;;;%Utilities
(defun bridge-insert (output)
  "Insert process OUTPUT into the current buffer."
  (if output
      (let* ((buffer (current-buffer))
	     (process (get-buffer-process buffer))
	     (mark (process-mark process))
	     (window (selected-window))
	     (at-end nil))
	(if (eq (window-buffer window) buffer)
	    (setq at-end (= (point) mark))
	    (setq window (get-buffer-window buffer)))
	(save-excursion
	  (goto-char mark)
	  (insert output)
	  (set-marker mark (point)))
	(if window 
	    (progn
	      (if at-end (goto-char mark))
	      (if (not (pos-visible-in-window-p (point) window))
		  (let ((original (selected-window)))
		    (save-excursion
		      (select-window window)
		      (recenter '(center))
		      (select-window original)))))))))

;;;
;(defun bridge-send-string (process string)
;  "Send PROCESS the contents of STRING as input.
;This is equivalent to process-send-string, except that long input strings
;are broken up into chunks of size comint-input-chunk-size. Processes
;are given a chance to output between chunks. This can help prevent processes
;from hanging when you send them long inputs on some OS's."
;  (let* ((len (length string))
;	 (i (min len bridge-chunk-size)))
;    (process-send-string process (substring string 0 i))
;    (while (< i len)
;      (let ((next-i (+ i bridge-chunk-size)))
;	(accept-process-output)
;	(process-send-string process (substring string i (min len next-i)))
;	(setq i next-i)))))

;;;
(defun bridge-call-handler (handler proc string)
  "Funcall HANDLER on PROC, STRING carefully.  Error is caught if happens,
and user is signaled.  State is put in bridge-last-failure.  Returns t if
handler executed without error."
  (let ((inhibit-quit nil)
	(failed nil))
    (condition-case err
	(funcall handler proc string)
      (error
       (ding)
       (setq failed t)
       (message "bridge-handler \"%s\" failed %s (see bridge-last-failure)"
		handler err)
       (setq bridge-last-failure
             `((funcall ',handler ',proc ,string)
               "Caused: "
               ,err))))
    (not failed)))

;;;%Handlers
(defun bridge-send-handler (process input)
  "Send PROCESS INPUT to the buffer name found at the start of the
input.  The input after the buffer name is sent to the buffer's
process if it has one.  If bridge-destination-insert is T, the input
will be inserted into the buffer.  If it does not have a process, it
will be inserted at the end of the buffer."
  (if (null input)
      (setq bridge-send-to-buffer nil)  ; end of bridge
      (let (buffer-and-start buffer-name dest to)
	;; if this is first time, get the buffer out of the first line
	(cond ((not bridge-send-to-buffer)
	       (setq buffer-and-start (read-from-string input)
		     buffer-name (format "%s" (car (read-from-string input)))
		     dest        (get-buffer buffer-name)
		     to          (get-buffer-process dest)
		     input (substring input (cdr buffer-and-start)))
	       (setq bridge-send-to-buffer dest))
	      (t
	       (setq buffer-name bridge-send-to-buffer
		     dest        (get-buffer buffer-name)
		     to          (get-buffer-process dest)
		     )))
	(if dest
	    (let ((buffer (current-buffer)))
	      (if bridge-destination-insert
		  (unwind-protect
		       (progn
			 (set-buffer dest)
			 (if to 
			     (bridge-insert process input)
			     (goto-char (point-max))
			     (insert input)))
		    (set-buffer buffer)))
	      (if to
		  ;; (bridge-send-string to input)
		  (process-send-string to input)
		  ))
	    (error "%s is not a buffer" buffer-name)))))

;;;%Filter
(defun bridge-filter (process output)
  "Given PROCESS and some OUTPUT, check for the presence of
bridge-start-regexp.  Everything prior to this will be passed to the
normal filter function or inserted in the buffer if it is nil.  The
output up to bridge-end-regexp will be sent to the first handler on
bridge-handlers that matches the string.  If no handlers match, the
input will be sent to bridge-send-handler.  If bridge-prompt-regexp is
encountered before the bridge-end-regexp, the bridge will be cancelled."
  (let ((inhibit-quit t)
	(match-data (match-data))
	(buffer (current-buffer))
	(process-buffer (process-buffer process))
	(case-fold-search t)
	(start 0) (end 0)
	function
	b-start b-start-end b-end)
    (set-buffer process-buffer)	;; access locals

    ;; Handle bridge messages that straddle a packet by prepending
    ;; them to this packet.

    (when bridge-leftovers
      (setq output (concat bridge-leftovers output))
      (setq bridge-leftovers nil))

    (setq function bridge-in-progress)

    ;; How it works:
    ;;
    ;; start, end delimit the part of string we are interested in;
    ;; initially both 0; after an iteration we move them to next string.

    ;; b-start, b-end delimit part of string to bridge (possibly whole string);
    ;; this will be string between corresponding regexps.

    ;; There are two main cases when we come into loop:

    ;;  bridge in progress
    ;;0    setq b-start = start
    ;;1    setq b-end (or end-pattern end)
    ;;4    process string
    ;;5    remove handler if end found
     
    ;;  no bridge in progress
    ;;0    setq b-start if see start-pattern
    ;;1    setq b-end if bstart to (or end-pattern end)
    ;;2    send (substring start b-start)  to normal place
    ;;3    find handler (in b-start, b-end) if not set
    ;;4    process string
    ;;5    remove handler if end found

    ;; equivalent sections have the same numbers here;
    ;; we fold them together in this code.

    (block bridge-filter
      (unwind-protect
	  (while (< end (length output))

	    ;;0    setq b-start if find
	    (setq b-start
		  (cond (bridge-in-progress
			 (setq b-start-end start)
			 start)
			((string-match bridge-start-regexp output start)
			 (setq b-start-end (match-end 0))
			 (match-beginning 0))
			(t nil)))
	    ;;1    setq b-end
	    (setq b-end
		  (if b-start
		      (let ((end-seen (string-match bridge-end-regexp
						    output b-start-end)))
			(if end-seen (setq end (match-end 0)))

			end-seen)))

	    ;; Detect and save partial bridge messages
	    (when (and b-start b-start-end (not b-end))
	      (setq bridge-leftovers (substring output b-start))
	      )

	    (if (and b-start (not b-end))
	      (setq end b-start)
	    (if (not b-end)
		(setq end (length output))))

	    ;;1.5 - if see prompt before end, remove current
	    (if (and b-start b-end)
		(let ((prompt (string-match bridge-prompt-regexp
					    output b-start-end)))
		  (if (and prompt (<= (match-end 0) b-end))
		      (setq b-start nil	; b-start-end start
			    b-end   start
			    end     (match-end 0)
			    bridge-in-progress nil
			    ))))

	    ;;2    send (substring start b-start) to old filter, if any
	    (when (not (equal start (or b-start end))) ; don't bother on empty string
	      (let ((pass-on (substring output start (or b-start end))))
		(if bridge-old-filter
		    (let ((old bridge-old-filter))
		      (store-match-data match-data)
		      (funcall old process pass-on)
		      ;; if filter changed, re-install ourselves
		      (let ((new (process-filter process)))
			(if (not (eq new 'bridge-filter))
			    (progn (setq bridge-old-filter new)
				   (set-process-filter process 'bridge-filter)))))
		  (set-buffer process-buffer)
		  (bridge-insert pass-on))))

	    (if (and b-start-end (not b-end)) 
		(return-from bridge-filter t) ; when last bit has prematurely ending message, exit  early.
	      (progn
		;;3 find handler (in b-start, b-end) if none current
		(if (and b-start (not bridge-in-progress))
		    (let ((handlers bridge-handlers))
		      (while (and handlers (not function))
			(let* ((handler (car handlers))
			       (m (string-match (car handler) output b-start-end)))
			  (if (and m (< m b-end))
			      (setq function (cdr handler))
			    (setq handlers (cdr handlers)))))
		      ;; Set default handler if none
		      (if (null function)
			  (setq function 'bridge-send-handler))
		      (setq bridge-in-progress function)))
		;;4    process strin
		(if function
		    (let ((ok t))
		      (if (/=  b-start-end b-end)
			  (let ((send (substring output b-start-end b-end)))
			    ;; also, insert the stuff in buffer between
			    ;; iff bridge-source-insert.
			    (if bridge-source-insert (bridge-insert send))
			    ;; call handler on string
			    (setq ok (bridge-call-handler function process send))))
		      ;;5    remove handler if end found
		      ;; if function removed then tell it that's all
		      (if (or (not ok) (/= b-end end)) ;; saw end before end-of-string
			  (progn
			    (bridge-call-handler function process nil)
			    ;; have to remove function too for next time around
			    (setq function nil
				  bridge-in-progress nil)
			    ))
		      ))
     
		;; continue looping, in case there's more string
		(setq  start end))
	      ))
	;; protected forms:  restore buffer, match-data
	(set-buffer buffer)
	(store-match-data match-data)
	))))


;;;%Interface
(defun install-bridge ()
  "Set up a process bridge in the current buffer."
  (interactive)
  (if (not (get-buffer-process (current-buffer)))
      (error "%s does not have a process" (buffer-name (current-buffer)))
      (make-local-variable 'bridge-start-regexp)
      (make-local-variable 'bridge-end-regexp)
      (make-local-variable 'bridge-prompt-regexp)
      (make-local-variable 'bridge-handlers)
      (make-local-variable 'bridge-source-insert)
      (make-local-variable 'bridge-destination-insert)
      (make-local-variable 'bridge-chunk-size)
      (make-local-variable 'bridge-old-filter)
      (make-local-variable 'bridge-string)
      (make-local-variable 'bridge-in-progress)
      (make-local-variable 'bridge-send-to-buffer)
      (make-local-variable 'bridge-leftovers)
      (setq bridge-string nil bridge-in-progress nil
	    bridge-send-to-buffer nil)
      (if (boundp 'comint-prompt-regexp)
	  (setq bridge-prompt-regexp comint-prompt-regexp))
      (let ((process (get-buffer-process (current-buffer))))
	(if process
	    (if (not (eq (process-filter process) 'bridge-filter))
		(progn
		  (setq bridge-old-filter (process-filter process))
		  (set-process-filter process 'bridge-filter)))
	    (error "%s does not have a process" 
		   (buffer-name (current-buffer)))))
      (run-hooks 'bridge-hook)
      (message "Process bridge is installed")))
	      
;;;
(defun reset-bridge ()
  "Must be called from the process's buffer.  Removes any active bridge."
  (interactive)
  ;; for when things get wedged
  (if bridge-in-progress
      (unwind-protect
	   (funcall bridge-in-progress (get-buffer-process
					(current-buffer))
		    nil)
	(setq bridge-in-progress nil))
      (message "No bridge in progress.")))

;;;
(defun remove-bridge ()
  "Remove bridge from the current buffer."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (if (or (not process) (not (eq (process-filter process) 'bridge-filter)))
	(error "%s has no bridge" (buffer-name (current-buffer)))
	;; remove any bridge-in-progress
	(reset-bridge)
	(set-process-filter process bridge-old-filter)
	(funcall bridge-old-filter process bridge-string)
	(message "Process bridge is removed."))))

;;;% Utility for testing
(defun hand-bridge (start end)
  "With point at bridge-start, sends bridge-start + string +
bridge-end to bridge-filter.  With prefix, use current region to send."
  (interactive "r")
  (let ((p0 (if current-prefix-arg (min start end)
		(if (looking-at bridge-start-regexp) (point)
		    (error "Not looking at bridge-start-regexp"))))
	(p1 (if current-prefix-arg (max start end)
		(if (re-search-forward bridge-end-regexp nil t)
		    (point) (error "Didn't see bridge-end-regexp")))))
    
    (bridge-filter (get-buffer-process (current-buffer))
		   (buffer-substring-no-properties p0 p1))
    ))

(provide 'bridge)

;;; haskell-ghci.el --- A GHCi interaction mode

;; Copyright (C) 2004, 2005, 2006  Free Software Foundation, Inc.
;; Copyright (C) 2001  Chris Webb
;; Copyright (C) 1998, 1999  Guy Lapalme

;; Keywords: inferior mode, GHCi interaction mode, Haskell

;;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Purpose:
;;
;; To send a Haskell buffer to another buffer running a GHCi
;; interpreter.
;;
;; This mode is derived from version 1.1 of Guy Lapalme's
;; haskell-hugs.el, which can be obtained from:
;;
;;   http://www.iro.umontreal.ca/~lapalme/Hugs-interaction.html
;;
;; This in turn was adapted from Chris Van Humbeeck's hugs-mode.el,
;; which can be obtained from:
;;
;;   http://www-i2.informatik.rwth-aachen.de/Forschung/FP/Haskell/hugs-mode.el
;;
;;
;; Installation:
;;
;; To use with Moss and Thorn's haskell-mode.el
;;
;;   http://www.haskell.org/haskell-mode
;;
;; add this to .emacs:
;;
;;   (add-hook haskell-mode-hook 'turn-on-haskell-ghci)
;;
;;
;; Customisation:
;;
;; The name of the GHCi interpreter is in haskell-ghci-program-name.
;;
;; Arguments can be sent to the GHCi interpreter when it is started by
;; setting haskell-ghci-program-args (empty by default) to a list of
;; string args to pass it.  This value can be set interactively by
;; calling C-c C-s with an argument (i.e. C-u C-c C-s).
;;
;; `haskell-ghci-hook' is invoked in the *ghci* buffer once GHCi is
;; started.
;;
;; All functions/variables start with `turn-{on,off}-haskell-ghci' or
;; `haskell-ghci-'.

;;; Code:

(defgroup haskell-ghci nil
  "Major mode for interacting with an inferior GHCi session."
  :group 'haskell
  :prefix "haskell-ghci-")

(defun turn-on-haskell-ghci ()
  "Turn on Haskell interaction mode with a GHCi interpreter running in an
another Emacs buffer named *ghci*.
Maps the following commands in the haskell keymap:
    \\<haskell-mode-map>\\[haskell-ghci-start-process] to create the GHCi buffer and start a GHCi process in it.
    \\[haskell-ghci-load-file] to save the current buffer and load it by sending the :load command to GHCi.
    \\[haskell-ghci-reload-file] to send the :reload command to GHCi without saving the buffer.
    \\[haskell-ghci-show-ghci-buffer] to show the GHCi buffer and go to it."
  (local-set-key "\C-c\C-s" 'haskell-ghci-start-process)
  (local-set-key "\C-c\C-l" 'haskell-ghci-load-file)
  (local-set-key "\C-c\C-r" 'haskell-ghci-reload-file)
  (local-set-key "\C-c\C-n" 'haskell-ghci-locate-next-error)
  (local-set-key "\C-c\C-b" 'haskell-ghci-show-ghci-buffer))

(defun turn-off-haskell-ghci ()
  "Turn off Haskell interaction mode with a GHCi interpreter within a buffer."
  (local-unset-key  "\C-c\C-s")
  (local-unset-key  "\C-c\C-l")
  (local-unset-key  "\C-c\C-r")
  (local-unset-key  "\C-c\C-b"))

(define-derived-mode haskell-ghci-mode comint-mode "Haskell GHCi"
  "Major mode for interacting with an inferior GHCi session.

The commands available from within a Haskell script are:
    \\<haskell-mode-map>\\[haskell-ghci-start-process] to create the GHCi buffer and start a GHCi process in it.
    \\[haskell-ghci-load-file] to save the current buffer and load it by sending the :load command to GHCi.
    \\[haskell-ghci-reload-file] to send the :reload command to GHCi without saving the buffer.
    \\[haskell-ghci-show-ghci-buffer] to show the GHCi buffer and go to it.

\\<haskell-ghci-mode-map>Commands:
\\[comint-send-input] after end of GHCi output sends line as input to GHCi.
\\[comint-send-input] before end of GHCI output copies rest of line and sends it to GHCI as input.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the comint or its current subjob if any.
\\[comint-stop-subjob] stops, likewise. \\[comint-quit-subjob] sends quit signal.")


;; GHCi interface:

(require 'comint)
(require 'shell)

(defvar haskell-ghci-process nil
  "The active GHCi subprocess corresponding to current buffer.")

(defvar haskell-ghci-process-buffer nil
  "*Buffer used for communication with GHCi subprocess for current buffer.")

(defcustom haskell-ghci-program-name "ghci"
  "*The name of the GHCi interpreter program."
  :type 'string
  :group 'haskell-ghci)

(defcustom haskell-ghci-program-args nil
  "*A list of string args to pass when starting the GHCi interpreter."
  :type '(repeat string)
  :group 'haskell-ghci)

(defvar haskell-ghci-load-end nil
  "Position of the end of the last load command.")

(defvar haskell-ghci-error-pos nil
  "Position of the end of the last load command.")

(defvar haskell-ghci-send-end nil
  "Position of the end of the last send command.")

(defun haskell-ghci-start-process (arg)
  "Start a GHCi process and invoke `haskell-ghci-hook' if not nil.
Prompt for a list of args if called with an argument."
  (interactive "P")
  (if arg
      ;; XXX [CDW] Fix to use more natural 'string' version of the
      ;; XXX arguments rather than a sexp.
      (setq haskell-ghci-program-args
            (read-minibuffer (format "List of args for %s:"
                                     haskell-ghci-program-name)
                             (prin1-to-string haskell-ghci-program-args))))

  ;; Start the GHCi process in a new comint buffer.
  (message "Starting GHCi process `%s'." haskell-ghci-program-name)
  (setq haskell-ghci-process-buffer
        (apply 'make-comint
               "ghci" haskell-ghci-program-name nil
               haskell-ghci-program-args))
  (setq haskell-ghci-process
        (get-buffer-process haskell-ghci-process-buffer))

  ;; Select GHCi buffer temporarily.
  (set-buffer haskell-ghci-process-buffer)
  (haskell-ghci-mode)
  (make-local-variable 'shell-cd-regexp)
  (make-local-variable 'shell-dirtrackp)

  ;; Track directory changes using the `:cd' command.
  (setq shell-cd-regexp ":cd")
  (setq shell-dirtrackp t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil 'local)

  ;; GHCi prompt should be of the form `ModuleName> '.
  (setq comint-prompt-regexp  "^\\*?[A-Z][\\._a-zA-Z0-9]*\\( \\*?[A-Z][\\._a-zA-Z0-9]*\\)*> ")

  ;; History syntax of comint conflicts with Haskell, e.g. !!, so better
  ;; turn it off.
  (setq comint-input-autoexpand nil)
  (run-hooks 'haskell-ghci-hook)

  ;; Clear message area.
  (message ""))

(defun haskell-ghci-wait-for-output ()
  "Wait until output arrives and go to the last input."
  (while (progn			
	   (goto-char comint-last-input-end)
	   (not (re-search-forward comint-prompt-regexp nil t)))
    (accept-process-output haskell-ghci-process)))

(defun haskell-ghci-send (&rest string)
  "Send `haskell-ghci-process' the arguments (one or more strings).
A newline is sent after the strings and they are inserted into the
current buffer after the last output."
  (haskell-ghci-wait-for-output)        ; wait for prompt
  (goto-char (point-max))               ; position for this input
  (apply 'insert string)
  (comint-send-input)
  (setq haskell-ghci-send-end (marker-position comint-last-input-end)))

(defun haskell-ghci-go (load-command cd)
  "Save the current buffer and load its file into the GHCi process.
The first argument LOAD-COMMAND specifies how the file should be
loaded: as a new file (\":load \") or as a reload (\":reload \").

If the second argument CD is non-nil, change directory in the GHCi
process to the current buffer's directory before loading the file.

If the variable `haskell-ghci-command' is set then its value will be
sent to the GHCi process after the load command. This can be used for a
top-level expression to evaluate."
  (hack-local-variables)		; in case they've changed
  (save-buffer)
  (let ((file (if (string-equal load-command ":load ")
		  (concat "\"" buffer-file-name "\"")
		""))
	(dir (expand-file-name default-directory))
	(cmd (and (boundp 'haskell-ghci-command)
		  haskell-ghci-command
		  (if (stringp haskell-ghci-command)
		      haskell-ghci-command
		    (symbol-name haskell-ghci-command)))))
    (if (and haskell-ghci-process-buffer
	     (eq (process-status haskell-ghci-process) 'run))
	;; Ensure the GHCi buffer is selected.
	(set-buffer haskell-ghci-process-buffer) 
      ;; Start Haskell-GHCi process.
      (haskell-ghci-start-process nil))

    (if cd (haskell-ghci-send (concat ":cd " dir)))
    ;; Wait until output arrives and go to the last input.
    (haskell-ghci-wait-for-output)
    (haskell-ghci-send load-command file)
    ;; Error message search starts from last load command.
    (setq haskell-ghci-load-end (marker-position comint-last-input-end))
    (setq haskell-ghci-error-pos haskell-ghci-load-end)
    (if cmd (haskell-ghci-send cmd))
    ;; Wait until output arrives and go to the last input.
    (haskell-ghci-wait-for-output)))

(defun haskell-ghci-load-file (cd)
  "Save a ghci buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change directory in
the GHCi process to the current buffer's directory before loading the
file. If there is an error, set the cursor at the error line otherwise
show the *ghci* buffer."
  (interactive "P")
  (haskell-ghci-gen-load-file ":load " cd))

(defun haskell-ghci-reload-file (cd)
  "Save a ghci buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change the GHCi
process to the current buffer's directory before loading the file.
If there is an error, set the cursor at the error line otherwise show
the *ghci* buffer."
  (interactive "P")
  (haskell-ghci-gen-load-file ":reload " cd))

(defun haskell-ghci-gen-load-file (cmd cd)
  "Save a ghci buffer file and load its file or reload depending on CMD.
If CD is non-nil, change the process to the current buffer's directory
before loading the file. If there is an error, set the cursor at the
error line otherwise show the *ghci* buffer."

  ;; Execute (re)load command.
  (save-excursion (haskell-ghci-go cmd cd))

  ;; Show *ghci* buffer.
  (pop-to-buffer haskell-ghci-process-buffer)
  (goto-char haskell-ghci-load-end)

  ;; Did we finish loading without error?
  (if (re-search-forward
       "^Ok, modules loaded" nil t)
      (progn (goto-char (point-max))
             (recenter 2)
             (message "There were no errors."))

    ;; Something went wrong. If possible, be helpful and pinpoint the
    ;; first error in the file whilst leaving the error visible in the
    ;; *ghci* buffer.
    (goto-char haskell-ghci-load-end)
    (haskell-ghci-locate-next-error)))


(defun haskell-ghci-locate-next-error () 
  "Go to the next error shown in the *ghci* buffer."
  (interactive)
  (if (buffer-live-p haskell-ghci-process-buffer)
      (progn (pop-to-buffer haskell-ghci-process-buffer)
	     (goto-char haskell-ghci-error-pos)
	     (if (re-search-forward
		  "^[^\/]*\\([^:\n]+\\):\\([0-9]+\\)" nil t)
		 (let ((efile (buffer-substring (match-beginning 1)
						(match-end 1)))
		       (eline (string-to-int 
			       (buffer-substring (match-beginning 2)
						 (match-end 2)))))

		   (recenter 2)
		   (setq haskell-ghci-error-pos (point))
		   (message "GHCi error on line %d of %s."
                   eline (file-name-nondirectory efile))
		   (if (file-exists-p efile)
		       (progn (find-file-other-window efile)
			      (goto-line eline)
			      (recenter))))

      ;; We got an error without a file and line number, so put the
      ;; point at end of the *ghci* buffer ready to deal with it.
               (goto-char (point-max))
               (recenter -2)
	       (message "No more errors found.")))
    (message "No *ghci* buffer found.")))     

(defun haskell-ghci-show-ghci-buffer ()
  "Go to the *ghci* buffer."
  (interactive)
  (if (or (not haskell-ghci-process-buffer)
          (not (buffer-live-p haskell-ghci-process-buffer)))
      (haskell-ghci-start-process nil))
  (pop-to-buffer  haskell-ghci-process-buffer))

(provide 'haskell-ghci)			

;; arch-tag: f0bade4b-288d-4329-9791-98c1e24167ac
;;; haskell-ghci.el ends here

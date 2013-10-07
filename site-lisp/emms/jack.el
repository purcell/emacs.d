;;; jack.el --- Jack Audio Connection Kit support

;; Copyright (C) 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: multimedia, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; JACK is a low-latency audio server, written for POSIX conformant
;; operating systems such as GNU/Linux and Apple's OS X. It can connect a
;; number of different applications to an audio device, as well as
;; allowing them to share audio between themselves. Its clients can run in
;; their own processes (ie. as normal applications), or they can run
;; within the JACK server (ie. as a "plugin").
;;
;; JACK was designed from the ground up for professional audio work, and
;; its design focuses on two key areas: synchronous execution of all
;; clients, and low latency operation.
;;
;; jack.el provides a fascility for starting jackd from within Emacs.
;; It also povides convenience functions for prompting the user for
;; jack client and port names in the minibuffer, as well as the
;; functions `jack-connect' and `jack-disconnect' which can be used to
;; rearrange jack port wiring with a minimum of keystrokes.

;;; Code:

(require 'emms-compat)

(defgroup jack ()
  "Jack Audio Connection Kit"
  :group 'processes)

(defcustom jack-rc '("~/.jackdrc" "/etc/jackd.conf")
  "*JACK run control paths."
  :group 'jack
  :type 'repeat)

(defcustom jack-use-jack-rc t
  "*If non-nil, try to retrieve jack startup arguments from run control files
listed in `jack-rc'.  If no rc file is found or this variable is set
to nil, use the Emacs variables to build the startup args."
  :group 'jack
  :type 'boolean)

(defcustom jack-program (executable-find "jackd")
  "*JACK executable path."
  :group 'jack
  :type 'file)

(defcustom jack-sample-rate 44100
  "*Default sampling rate for JACK."
  :group 'jack
  :type 'integer)

(defcustom jack-period-size 128
  "*Period size to use when launching new JACK process."
  :group 'jack
  :type 'integer)

(defcustom jack-alsa-device nil
  "*ALSA soundcard to use."
  :group 'jack
  :type '(choice (const :tag "Ask" nil) string))

(defun jack-read-alsa-device ()
  "Read an ALSA device name using the minibuffer."
  (let (cards)
    (with-temp-buffer
      (insert-file-contents "/proc/asound/cards")
      (while (not (eobp))
	(if (looking-at "^\\([0-9]\\) \\[.+\\]: \\(.+\\)\n +\\(.*\\)$")
	    (setq cards (append (list (cons (match-string 3) (match-string 1))) cards)))
	(forward-line 1)))
    (concat "hw:" (cdr (assoc (completing-read "Card: " cards nil t) cards)))))

(defun jack-alsa-device ()
  (or jack-alsa-device (jack-read-alsa-device)))

(defcustom jack-output-buffer-name "*JACK output*"
  "*Output buffer name."
  :group 'jack
  :type 'string)

(defun jack-args ()
  "Return a list of startup arguments to use.
First element is the executable path."
  (or (and jack-use-jack-rc
	   (catch 'rc-found
	     (let ((files (mapcar 'expand-file-name jack-rc)))
	       (while files
		 (if (file-exists-p (car files))
		     (with-temp-buffer
		       (insert-file-contents (car files))
		       (when (> (buffer-size) 0)
			 (throw 'rc-found
				(split-string (buffer-string) "[\n \t]+")))))
		 (setq files (cdr files))))
	     nil))
      (list jack-program
	    "-v"
	    "-R"
	    "-dalsa"
	    (format "-d%s" (jack-alsa-device))
	    (format "-r%d" jack-sample-rate)
	    (format "-p%d" jack-period-size))))

(defcustom jack-set-rtlimits t
  "*Use set_rtlimits (if available) to gain realtime priorities if -R
is given in jackd command-line."
  :group 'jack
  :type 'boolean)

(defcustom jack-set-rtlimits-program (executable-find "set_rtlimits")
  "*Path to set_rtlimits."
  :group 'jack
  :type 'file)

(defun jack-maybe-rtlimits (args)
  (if (and jack-set-rtlimits
	   (or (member "-R" args) (member "--realtime" args))
	   (file-exists-p jack-set-rtlimits-program))
      (append (list jack-set-rtlimits-program "-r") args)
    args))

(defvar jack-process nil)

(defvar jack-load 0)

(defvar jack-max-usecs 0)

(defvar jack-spare 0)

(defun jack-output-buffer ()
  (or (get-buffer jack-output-buffer-name)
      (with-current-buffer (get-buffer-create jack-output-buffer-name)
	(setq major-mode 'jack-mode
	      mode-name "JACK"
	      mode-line-format (copy-tree mode-line-format))
	(setcar (nthcdr 16 mode-line-format)
		`(:eval (format "load:%.2f" jack-load)))
	(add-hook 'kill-buffer-hook 'jack-kill nil t)
	(current-buffer))))

(defvar jack-xruns nil)

(defun jack-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(save-match-data
	  (if (string-match "^load = \\([^ ]+\\) max usecs: \\([^,]+\\), spare = \\(.+\\)$" string)
	      (setq jack-load (string-to-number (match-string 1 string))
		    jack-max-usecs (string-to-number (match-string 2 string))
		    jack-spare (string-to-number (match-string 3 string)))
	    (if (string-match "^**** alsa_pcm: xrun of at least \\([^ ]+\\) msecs$" string)
		(push (string-to-number (match-string 1 string)) jack-xruns)
	      (goto-char (process-mark proc))
	      (insert string)
	      (set-marker (process-mark proc) (point))))))
      (when moving (goto-char (process-mark proc))))))

(defun jack-running-p ()
  (and jack-process (processp jack-process)
       (eq (process-status jack-process) 'run)))

(defcustom jack-started-hook nil
  "*Hook run when `jack-start' successfully started a new JACK intance."
  :group 'jack
  :type 'hook)

(defun jack-start ()
  "Start the JACK process."
  (interactive)
  (if (jack-running-p) (error "JACK already running")
    (setq jack-process
	  (apply 'start-process "jack" (jack-output-buffer)
		 (jack-maybe-rtlimits (jack-args))))
    (set-process-filter jack-process #'jack-filter)
    (run-hooks 'jack-started-hook)
    (switch-to-buffer (jack-output-buffer))))

(defun jack-kill ()
  "Kill the currently running JACK process."
  (interactive)
  (when (jack-running-p) (delete-process jack-process))
  (setq jack-process nil))

(defun jack-restart ()
  "Restart JACK."
  (interactive)
  (if (jack-running-p) (jack-kill))
  (sit-for 0)
  (jack-start))

(defun jack-list ()
  "Retrieve a list of JACK clients/ports."
  (with-temp-buffer
    (call-process "jack_lsp" nil t nil "-cpl")
    (goto-char (point-min))
    (let (result current-port)
      (while (not (eobp))
	(cond
	 ((looking-at "^\\([^ \t:]+\\):\\(.+\\)$")
	  (let ((program (match-string 1))
		(port (match-string 2)))
	    (if (assoc program result)
		(setcdr (assoc program result)
			(append (cdr (assoc program result)) (list (setq current-port (list port)))))
	      (setq result
		    (append (list (list program (setq current-port (list port)))) result)))))
	 ((looking-at "^   \\([^ \t:]+\\):\\(.+\\)$")
	  (if (assoc 'connections (cdr current-port))
	      (setcdr (assoc 'connections (cdr current-port))
		      (append (cdr (assoc 'connections current-port))
			      (list (list (match-string 1) (match-string 2)))))
	    (setcdr current-port
		    (append (list (list 'connections (list (match-string 1) (match-string 2)))) (cdr current-port)))))
	 ((looking-at "^\tproperties: \\(.+\\),$")
	  (setcdr current-port
		  (append (list (append (list 'properties) (mapcar #'intern (split-string (match-string 1) ",")))) (cdr current-port)))))
	(forward-line 1))
      result)))
	  
(defun jack-ports (program)
  (cdr (assoc program (jack-list))))

(defun jack-get-port-connections (program port)
  (cdr (assoc 'connections (cdr (assoc port (jack-ports program))))))

(defun jack-get-port-properties (program port)
  (cdr (assoc 'properties (cdr (assoc port (jack-ports program))))))

(defun jack-get-direction (program port)
  (let ((props (jack-get-port-properties program port)))
    (or (car (member 'output props))
	(car (member 'input props))
	(error "Neither input nor output port"))))
      
(defun jack-read-program (prompt &optional predicate)
  (let ((progs (if (functionp predicate)
		   (emms-remove-if-not predicate (jack-list))
		 (jack-list))))
    (unless progs (error "No matching JACK clients found"))
    (if (< (length progs) 2) (caar progs)
      (completing-read prompt progs nil t))))

(defun jack-unique-port-name (strings)
  (let ((start "")
	(maxlen (apply 'min (mapcar #'length strings))))
    (while (and (< (length start) maxlen)
		(catch 'not-ok
		  (let ((nextchar (substring (car strings) (length start) (1+ (length start)))))
		    (mapc (lambda (str)
			    (unless (string= (concat start nextchar) (substring str 0 (1+ (length start))))
			      (throw 'not-ok nil)))
			  strings)
		    t)))
      (setq start (substring (car strings) 0 (1+ (length start)))))
    start))

(defun jack-read-port (program prompt &optional predicate)
  (let ((ports (if (functionp predicate)
		   (emms-remove-if-not predicate (jack-ports program))
		 (jack-ports program))))
    (if (< (length ports) 2) (caar ports)
      (completing-read prompt ports nil t (jack-unique-port-name (mapcar 'car ports))))))

(defun jack-connect (from-program from-port to-program to-port)
  "Connect FROM-PROGRAM's output port FROM-PORT to TO-PROGRAM's input port
TO-PORT.
If called interactively, the direction does not matter."
  (interactive
   (let* ((prog (jack-read-program "Connect: "))
	  (port (jack-read-port prog (format "Connect %s port: " prog)))
	  (to-type (if (eq (jack-get-direction prog port) 'input) 'output 'input))
	  (to-prog (jack-read-program
		 (format "Connect %s port %s to: " prog port)
		 (lambda (prog)
		   (emms-find-if (lambda (port)
				   (member to-type (assoc 'properties
							  (cdr port))))
				 (cdr prog)))))
	  (to-port (jack-read-port
		    to-prog
		    (format "Connect %s port %s to %s port: " prog port to-prog)
		    (lambda (port)
		      (member to-type (cdr (assoc 'properties (cdr port))))))))
     (if (eq to-type 'input)
	 (list prog port to-prog to-port)
       (list to-prog to-port prog port))))
  (let ((result (call-process "jack_connect" nil nil nil
			      (format "%s:%s" from-program from-port)
			      (format "%s:%s"  to-program to-port))))
    (if (= result 0)
	(message "JACK: Connected %s:%s to %s:%s"
		 from-program from-port to-program to-port))))

(defun jack-disconnect (from-program from-port to-program to-port)
  "Disconnect FROM-PROGRAM's output port FROM-PORT from TO-PROGRAM's
input port TO-PORT.
If called interactively, the direction is not relevant."
  (interactive
   (let* ((prog (jack-read-program
		 "Disconnect: "
		 (lambda (prog)
		   (emms-find-if (lambda (port) (assoc 'connections (cdr port)))
				 (cdr prog)))))
	  (port (jack-read-port prog
		 (format "Disconnect %s port: " prog)
		 (lambda (port)
		   (assoc 'connections (cdr port)))))
	  (connections (jack-get-port-connections prog port))
	  (from (list prog port))
	  (to (if (< (length connections) 2)
		  (car connections)
		(let* ((to-progs (let (result)
				   (mapc (lambda (conn)
					   (if (not (member (car conn) result))
					       (setq result
						     (append (list (car conn))
							     result))))
					 connections)
				   (mapcar #'list result)))
		       (to-prog (if (< (length to-progs) 2)
				    (caar to-progs)
				  (completing-read
				   (format "Disconnect %s port %s from: "
					   prog port) to-progs nil t))))
		  (setq connections (emms-remove-if-not
				     (lambda (conn)
				       (string= (car conn) to-prog))
				     connections))
		  (if (< (length connections) 2)
		      (car connections)
		    (let ((to-port (completing-read
				    (format "Disconnect %s port %s from %s port: "
					    prog port to-prog)
				    (mapcar #'cdr connections) nil t)))
		      (list to-prog to-port)))))))
     (if (eq (jack-get-direction prog port) 'output)
	 (append from to)
       (append to from))))
  (let ((result (call-process "jack_disconnect" nil nil nil
			      (format "%s:%s" from-program from-port)
			      (format "%s:%s"  to-program to-port))))
    (if (= result 0)
	(message "JACK: Disconnected %s:%s from %s:%s"
		 from-program from-port to-program to-port))))

(provide 'jack)
;;; jack.el ends here

;;; emms-player-simple.el --- A generic simple player.

;; Copyright (C) 2003, 2004, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Authors: Ulrik Jensen <terryp@daimi.au.dk>
;;          Jorgen Schäfer <forcer@forcix.cx>
;; Keywords: emms, mpg321, ogg123, mplayer

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a simple player interface - if you have an external player
;; that just expects the filename to play as an argument, this should
;; be able to use it. See the define-emms-simple-player lines at the
;; end of this file for examples.

;; Add the following to your `emms-player-list':

;; emms-player-mpg321
;; emms-player-ogg123
;; emms-player-mplayer

;;; Code:

;; Version control
(defvar emms-player-simple-version "0.2 $Revision: 1.26 $"
  "Simple player for EMMS version string.")
;; $Id: emms-player-simple.el,v 1.26 2005/08/02 15:27:51 forcer Exp $

(require 'emms)

;; Customization

(defmacro define-emms-simple-player (name types regex command &rest args)
  "Define a simple player with the use of `emms-define-player'.
NAME is used to contruct the name of the function like
emms-player-NAME. TYPES is a list of track types understood by
this player. REGEX must be a regexp that matches the filenames
the player can play. COMMAND specifies the command line arguement
to call the player and ARGS are the command line arguements."
  (let ((group (intern (concat "emms-player-" (symbol-name name))))
        (command-name (intern (concat "emms-player-"
                                      (symbol-name name)
                                      "-command-name")))
        (parameters (intern (concat "emms-player-"
                                    (symbol-name name)
                                    "-parameters")))
        (player-name (intern (concat "emms-player-" (symbol-name name))))
        (start (intern (concat "emms-player-" (symbol-name name) "-start")))
        (stop (intern (concat "emms-player-" (symbol-name name) "-stop")))
        (playablep (intern (concat "emms-player-" (symbol-name name) "-playable-p"))))
  `(progn
     (defgroup ,group nil
       ,(concat "EMMS player for " command ".")
       :group 'emms-player
       :prefix ,(concat "emms-player-" (symbol-name name) "-"))
     (defcustom ,command-name ,command
       ,(concat "*The command name of " command ".")
       :type  'string
       :group ',group)
     (defcustom ,parameters ',args
       ,(concat "*The arguments to `" (symbol-name command-name) "'.")
       :type  '(repeat string)
       :group ',group)
     (defcustom ,player-name (emms-player ',start ',stop ',playablep)
       ,(concat "*A player for EMMS.")
       :type '(cons symbol alist)
       :group ',group)
     (emms-player-set ,player-name 'regex ,regex)
     (emms-player-set ,player-name 'pause 'emms-player-simple-pause)
     (emms-player-set ,player-name 'resume 'emms-player-simple-resume)
     (defun ,start (track)
       "Start the player process."
       (emms-player-simple-start (emms-track-name track)
                                 ,player-name
                                 ,command-name
                                 ,parameters))
     (defun ,stop ()
       "Stop the player process."
       (emms-player-simple-stop))
     (defun ,playablep (track)
       "Return non-nil when we can play this track."
       (and (executable-find ,command-name)
            (memq (emms-track-type track) ,types)
            (string-match (emms-player-get ,player-name 'regex)
                          (emms-track-name track)))))))

;; Global variables
(defvar emms-player-simple-process-name "emms-player-simple-process"
  "The name of the simple player process")

(defun emms-player-simple-stop ()
  "Stop the currently playing process, if indeed there is one"
  (let ((process (get-process emms-player-simple-process-name)))
    (when process
      (kill-process process)
      (delete-process process))))

;; Utility-functions
(defun emms-player-simple-start (filename player cmdname params)
  "Starts a process playing FILENAME using the specified CMDNAME with
the specified PARAMS.
PLAYER is the name of the current player."
  (let ((process (apply 'start-process
                        emms-player-simple-process-name
                        nil
                        cmdname
                        ;; splice in params here
                        (append params (list filename)))))
    ;; add a sentinel for signaling termination
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started player))

(defun emms-player-simple-sentinel (proc str)
  "Sentinel for determining the end of process"
  (when (or (eq (process-status proc) 'exit)
            (eq (process-status proc) 'signal))
    (emms-player-stopped)))

(defun emms-player-simple-pause ()
  "Pause the player by sending a SIGSTOP."
  (signal-process (get-process emms-player-simple-process-name)
                  'SIGSTOP))

(defun emms-player-simple-resume ()
  "Resume the player by sending a SIGCONT."
  (signal-process (get-process emms-player-simple-process-name)
                  'SIGCONT))

(defun emms-player-simple-regexp (&rest extensions)
  "Return a regexp matching all EXTENSIONS, case-insensitively."
  (concat "\\.\\("
          (mapconcat (lambda (extension)
                       (mapconcat (lambda (char)
                                    (let ((u (upcase char))
                                          (d (downcase char)))
                                      (if (= u d)
                                          (format "%c" char)
                                        (format "[%c%c]" u d))))
                                  extension
                                  ""))
                     extensions
                     "\\|")
          "\\)\\'"))

(define-emms-simple-player mpg321 '(file url)
  (emms-player-simple-regexp "mp3" "mp2")
  "mpg321")
(define-emms-simple-player ogg123 '(file)
  (emms-player-simple-regexp "ogg" "flac")
  "ogg123")
(define-emms-simple-player speexdec '(file)
  (emms-player-simple-regexp "spx")
  "speexdec")
(define-emms-simple-player playsound '(file)
  (emms-player-simple-regexp "wav")
  "playsound")
(define-emms-simple-player mikmod '(file)
  (emms-player-simple-regexp "669" "amf" "dsm" "far" "gdm" "it"
                             "imf" "mod" "med" "mtm" "okt" "s3m"
                             "stm" "stx" "ult" "apun" "xm" "mod")
  "mikmod" "-q" "-p" "1" "-X")
(define-emms-simple-player timidity '(file)
  (emms-player-simple-regexp "mid" "rmi" "rcp" "r36" "g18" "g36" "mfi")
  "timidity")
(define-emms-simple-player fluidsynth '(file)
  (emms-player-simple-regexp "mid")
  "fluidsynth" "-aalsa" "-in" "/media/music/sf/FluidR3-GM.SF2")
(define-emms-simple-player alsaplayer '(file url)
  (concat "\\`http://\\|"
          (emms-player-simple-regexp "ogg" "mp3" "wav" "flac" "pls" "m3u"))
  "alsaplayer" "--quiet" "--nosave" "\"--interface text\"")

(emms-player-set emms-player-alsaplayer
		 'pause
		 'emms-player-alsaplayer-pause)

;;; Pause is also resume for alsaplayer
(emms-player-set emms-player-alsaplayer
                 'resume
                 nil)

(emms-player-set emms-player-alsaplayer
		 'seek
		 'emms-player-alsaplayer-seek)

(defun emms-player-alsaplayer-pause ()
  (call-process "alsaplayer" nil nil nil "--pause"))

(defun emms-player-alsaplayer-seek (sec)
  (call-process "alsaplayer" nil nil nil "--relative" (format "%d" sec)))

(provide 'emms-player-simple)
;;; emms-player-simple.el ends here

;;; todochiku.el - A mode for interfacing with Growl, Snarl, and the like.
(defconst todochiku-version "0.0.7")

;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;; todochiku is **NOTHING** in japanese.   todoroku "growl" in Japanese.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `todochiku-in'
;;    Send a todochiku message in a set ammount of time. Can take a prefix arg for the number of mins to wait.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `todochiku-command'
;;    Path to command for sending growl-like messages.
;;    default = (case system-type (windows-nt "C:/Program Files/full phat/Snarl/exrras/sncmd/snarl_command.exe") (darwin "/usr/local/bin/growlnotify") (t "notify-send"))
;;  `todochiku-message-too'
;;    Display todochiku as messages as well
;;    default = nil
;;  `todochiku-tooltip-too'
;;    Display todochiku as a tooltip.
;;    default = nil
;;  `todochiku-icons-directory'
;;    Path to the todochiku icons directory.
;;    default = "~/.emacs-cfg/todochiku-icons"
;;  `todochiku-timeout'
;;    Time to display a todochiku notification (not used by all backends)
;;    default = 10
;;  `todochiku-icons'
;;    An alist containing an icon name, and a path to the icon.
;;    default = (quote ((default . "announcements.png") (alert . "alert.png") (bell . "bell.png") (compile . "binary.png") (irc . "chat.png") ...))
;;  `todochiku-compile-message'
;;    Automatically add a hook to send a todochiku on compilation finsih.
;;    default = (quote t)
;;  `todochiku-appts'
;;    Install todochiku as the apt-disp-window-function.
;;    default = (quote t)
;;  `todochiku-display-appts-in-window-too'
;;    Whether or not pings from apt-disp-window should show up in emacs as well as growl.
;;    default = (quote t)

;; This is more a smaller library package to start with.  Maybe it will grow one day.

;; Installation:
;; Make sure you have snarl/growl/libnotify installed, and load this file from your .Emacs
;; i.e. (load-file "~/.emacs-cfg/todochiku.el")
;;
;; If you have an external notification program, you can use that. 
;; use customize-group todochiku, and make sure you set the todochiku-command.
;;
;; If you do not have an external notification program, there is basic support
;; for similar notifications using the message window and/or tooltips.
;; See todochiku-message-too and todochiku-tooltip-too.
; 
;; For icon support, customize the todochiku-icons-directory variable.  I have
;; a directory of png icons available at
;; http://bunny.jonnay.net/todochiku-icons.tar.gz
;; In the future, the downloading and installing of this directory will be
;; automagickal.

;; interactive commands: 
;; todochiku-in - do a todochiku.  This is a great way to set up a reminder
;;                for yourself.

;; elisp:
;; If you want to send a growl message from elisp, just use the function
;; todochiku-message

;;; TODO:
;; - maybe look at advising the message function? that might be super dumb.
;;   if done, the value of todochiku-message-too NEEDS to be looked at,
;;   so that an infinite loop doesn't occur.
;; - Update the icon support.  Automagickally download and install the icons
;;   with the url package.
;; - Build better backend support.

;;; CHANGELOG:
;; V0.7   - Added YaOddMuse interface
;; V0.0.6.1 - Bugfixing fom Jason McBrayer  (thanks!)
;; V0.0.6 - Added patches from Jason McBrayer for *nix notifications.
;; V0.0.5 - Added Initial support for emacs only notification
;;        - Added variables to the customization group.
;;        - Added some better documentation.
;; V0.0.4 - Added initial icon support.  Right now it kinds sucks
;;          Becuase it takes too much configuration.
;;        - Fixed the comment about todochiku being growl in
;;          japanese.  Doh.  I coulda sworn the 'ro' was a 'chi'. 
;; v0.0.3 - Added growl.el interface
;;        - Added rcirc notify when you are mentioned in IRC (from Brian Templetons growl.el)
;; v0.0.2 - fixed to use growl properly
;;        - set the debug constent to false (duh)
;;        - added some system checks around command and arguments
;;        - 
;; v0.0.1 - first release
;;
;;; BUGS:
;; - For some reason my face isn't working properly

(defconst todochiku-debug nil)

(defgroup todochiku nil
  "Todochiku (とどろく), send growl/snarl/libnotify notifications from within emacs."
  :group 'external)

(defcustom todochiku-command 
  (case system-type 
    (windows-nt "C:/Program Files/full phat/Snarl/exrras/sncmd/snarl_command.exe")
    (darwin "/usr/local/bin/growlnotify")
    (t "notify-send"))
  "Path to command for sending growl-like messages.
If you do not have an external notification program, leave this blank.
For MacOS Growl: /usr/local/bin/growlnotify (a shot in the dark here)
For Win32 Snarl: C:/Program Files/full phat/Snarl/extras/sncmd/sncmd.exe
                 or
                 C:/Program Files/full phat/Snarl/extras/sncmd/snarl_command.exe
For Unix-like systems libnotify: notify-send (or /usr/bin/notify-send)"
  :type '(string)
  :group 'todochiku)

(defcustom todochiku-message-too
  nil
  "Display todochiku as messages as well
Whether or not to display todochiku-messages as well as send
to the external notification program.

Has no effect if todochiku-command is nil."
  :type 'boolean
  :group 'todochiku)

(defcustom todochiku-tooltip-too
  nil
  "Display todochiku as a tooltip.
Whether or not to display todochiku-messages as a tooltip."
  :type 'boolean
  :group 'todochiku)

(defcustom todochiku-icons-directory
  "~/.emacs-cfg/todochiku-icons"
  "Path to the todochiku icons directory."
  :type 'directory
  :group 'todochiku)

;;*JasonMcBrayer
(defcustom todochiku-timeout
  10
  "Time to display a todochiku notification (not used by all backends)"
  :type 'integer
  :group 'todochiku)

(defcustom todochiku-icons
  '((default   . "announcements.png")
	(alert     . "alert.png")
	(bell      . "bell.png")
	(compile   . "binary.png")
	(irc       . "chat.png")
	(check     . "clean.png")
	(emacs     . "emacs_32.png")
	(star      . "favorites.png")
	(social    . "groupevent.png")
	(alarm     . "kalarm.png")
	(music     . "kbemusedsrv.png")
	(mail      . "kmail.png")
	(term      . "terminal.png")
	(package   . "zip.png"))
  "An alist containing an icon name, and a path to the icon.
The PNG format seems to be most compatable.  This is done in
an a-list so that elisp developers have a set of icons that
they can depend on."
  :type '(alist)
  :group 'todochiku)

(defcustom todochiku-compile-message 't
  "Automatically add a hook to send a todochiku on compilation finsih."
  :type '(boolean))

(defcustom todochiku-appts 't
  "Install todochiku as the apt-disp-window-function."
  :type '(boolean)
  :group 'todochiku)

(defcustom todochiku-display-appts-in-window-too 't
  "Whether or not pings from apt-disp-window should show up in emacs as well as growl.
This is really only useful if you use the appt package (i.e. from planner mode)."
  :type '(boolean)
  :group 'todochiku)

(defface todochiku-message-face
  '((default
	  :forground "black"
	  :background "white"
	  :box (:line-width 2 :color "grey40")))
  "This is the text that is displayed in the message window on a notification."
  :group 'todochiku)


(defun todochiku-message (title message icon)
  "Send a message via growl, snarl, etc.
If you don't wnat to set a title or icon, just use an ampty string \"\"
as an argument.

`icon' is a path to a PNG image that is displayed with the notification.
you can use `todochiku-icon' to figure out which icon you want to display.

See the variable `todochiku-icons' for a list of available icons." 
  (if todochiku-debug (message "Sent todochiku message.  Title:%s Message:%30s... Icon:%s" title message icon))
  (when (not (string= todochiku-command ""))
		(apply 'start-process 
			   "todochiku" 
			   nil 
			   todochiku-command 
			   (todochiku-get-arguments title message icon)))
  (when todochiku-tooltip-too
		(let ((tooltip-frame-parameters '((name . "todochiku")
										  (internal-border-width . 4)
										  (border-width . 2)
										  (left . 0)
										  (top . 0))))
		  (tooltip-show message)))
  (when (or (string= todochiku-command "")
			todochiku-message-too)
		(message "%s" (propertize message 'face 'todochiku-message-face))))

(defun growl (title message)
  "Alias for `todochiku-message'."
  (todochiku-message title message ""))

;;*JasonMcBrayer backend
(defun todochiku-get-arguments (title message icon)
  "Gets todochiku arguments.
This would be better done through a customization probably."
  (case system-type
    ('windows-nt (list "/M" title message icon))
    ('darwin (list title "-m" message "--image" icon ))
    (t (list "-i" icon "-t" (int-to-string (* 1000 todochiku-timeout)) title message))))

(defun todochiku-icon (icon)
  "Pull out an actual icon from the variable `todochiku-icons'."
  (expand-file-name (concat todochiku-icons-directory "/" (cdr (assoc icon todochiku-icons)))))

(defun todochiku-in (message mins)
  "Send a todochiku message in a set ammount of time. Can take a prefix arg for the number of mins to wait."
  (interactive "sMessage: \nNTime to wait: ")
  (run-at-time (* mins 60)
			   nil
			   'todochiku-message
			   "Todohiku Timer"
			   message
			   (todochiku-icon 'bell)))

(defun todochiku-appt-disp-window (min-to-app new-time appt-msg)
  "A helper function to interface with appt-disp-window-function."
  (todochiku-message (concat "Appt in " min-to-app)
					 (concat appt-msg "\n" min-to-app " Mins\n" new-time)
					 (todochiku-icon 'alarm))
  (if todochiku-display-appts-in-window-too
	  (appt-disp-window min-to-app new-time appt-msg)))

(if todochiku-appts
	(setq appt-disp-window-function 'todochiku-appt-disp-window))

;;* external 
(if todochiku-compile-message
	(add-hook 'compilation-mode-hook
			  (lambda ()
				(add-to-list 'compilation-finish-functions
							 (lambda (buf finish) (todochiku-message "Compilation Finished" finish (todochiku-icon 'compile)))))))

;;* external
(defun growl-rcirc-print-hook (process sender response target text)
  (when (and (string-match (rcirc-nick process) text)
             (not (string= (rcirc-nick process) sender))
             (not (string= (rcirc-server process) sender)))
		(growl "You Were Mentioned"
			   (format "You were mentioned by %s in %s" sender target))))

(eval-after-load 'rcirc
				 '(add-hook 'rcirc-print-hooks 'growl-rcirc-print-hook))


;;* external
(defun yaoddmuse-todochiku (msg)
  "Hook into yaoddmuses notification system."
  (todochiku-message "YaOddMuse" msg (todochiku-icon 'social)))

(todochiku-message "Emacs" "Todochiku (growl for emacs) is ready." (todochiku-icon 'check))

;; This idea doesn't quite work, and given that message is a C function, it might not be that smart anyway
;; (defcustom todochiku-on-message nil
;;   "Wrap advice around the emacs message command to send a todochiku message."
;;   :type '(boolean))

;; (if todochiku-on-message
;; 	(defadvice 'message
;; 	  '(after todochiku-on-message-advice)
;; 	  (todochiku-message "Emacs Message" (apply 'format args) "")))

;; We basically provide the same thing as growl.el
(provide 'growl)

(provide 'todochiku)

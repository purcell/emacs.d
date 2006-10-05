;;; winring.el --- Window configuration rings

;; Copyright (C) 1998, 1999 Free Software Foundation, Inc.

;; Author:   1997-2004 Barry A. Warsaw
;; Contact:  gnu@wooz.org (Barry A. Warsaw)
;; Homepage: http://barry.warsaw.us/elisp
;; Created:  March 1997
;; Keywords: frames tools

(defconst winring-version "$Revision: 3.11 $"
  "winring version number.")

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package provides lightweight support for circular rings of
;; window configurations.  A window configuration is the layout of
;; windows and associated buffers within a frame.  There is always at
;; least one configuration on the ring, the current configuration.
;; You can create new configurations and cycle through the layouts in
;; either direction.  You can also delete configurations from the ring
;; (except the last one of course!).  Window configurations are named,
;; and you can jump to and delete named configurations.  Display of
;; the current window configuration name in the mode line is only
;; supported as of Emacs 20.3 and XEmacs 21.0.
;;
;; Window configuration rings are frame specific.  That is, each frame
;; has its own ring which can be cycled through independently of other
;; frames.  This is the way I like it.
;;
;; You are always looking at the current window configuration for each
;; frame, which consists of the windows in the frame, the buffers in
;; those windows, and point in the current buffer.  As you run
;; commands such as "C-x 4 b", "C-x 2", and "C-x 0" you are modifying
;; the current window configuration.  When you jump to a new
;; configuration, the layout that existed before the jump is captured,
;; and the ring is rotated to the selected configuration.  Window
;; configurations are captured with `current-window-configuration',
;; however winring also saves point for the current buffer.

;; To use, make sure this file is on your `load-path' and put the
;; following in your .emacs file:
;;
;; (require 'winring)
;; (winring-initialize)
;;
;; Note that by default, this binds the winring keymap to the C-x 7
;; prefix, but you can change this by setting the value of
;; `winring-keymap-prefix', before you call `winring-initialize'.
;; Note that this is a change from previous versions of winring; the
;; prefix used to be M-o but was changed on the suggestion of RMS.

;; The following commands are defined:
;;
;;    C-x 7 n -- Create a new window configuration.  The new
;;               configuration will contain a single buffer, the one
;;               named in the variable `winring-new-config-buffer-name'
;;
;;               With C-u, winring prompts for the name of the new
;;               configuration.  If you don't use C-u the function in
;;               `winring-name-generator' will be called to get the
;;               new configuration's name.
;;
;;    C-x 7 2 -- Create a duplicate of the current window
;;               configuration.  C-u has the same semantics as with
;;               "C-x 7 c".
;;
;;    C-x 7 j -- Jump to a named configuration (prompts for the name).
;;
;;    C-x 7 0 -- Kill the current window configuration and rotate to the
;;               previous layout on the ring.  You cannot delete the
;;               last configuration in the ring.  With C-u, prompts
;;               for the name of the configuration to kill.
;;
;;    C-x 7 o -- Go to the next configuration on the ring.
;;
;;    C-x 7 p -- Go to the previous configuration on the ring.
;;
;;               Note that the sequence `C-x 7 o C-x 7 p' is a no-op;
;;               it leaves you in the same configuration you were in
;;               before the sequence.
;;
;;    C-x 7 r -- Rename the current window configuration.
;;
;;    C-x 7 b -- Submit a bug report on winring.
;;
;;    C-x 7 v -- Echo the winring version.

;; As mentioned, window configuration names can be displayed in the
;; modeline, but this feature only works with Emacs 20.3 and XEmacs
;; 21.0.  A patch for XEmacs 20.4 to support this feature is available
;; at the URL below.  Note that the default value of
;; `winring-show-names' is currently nil by default because if your
;; X/Emacs doesn't have the necessary support, ugly things can happen
;; (no you won't crash your X/Emacs -- it just won't do what you
;; want).
;;
;; If your X/Emacs has the necessary support, you can turn on display
;; of window configuration names by setting `winring-show-names' to
;; t.  If you don't like the position in the modeline where winring
;; names are shown, you can change this by passing in your own
;; modeline hacker function to `winring-initialize'.

;;; History:
;;
;; A long long time ago there was a package called `wicos' written by
;; Heikki Suopanki, which was based on yet another earlier package
;; called `screens' also written by Suopanki.  This in turn was based
;; on the Unix tty session manager `screen' (unrelated to Emacs) by
;; Oliver Laumann, Juergen Weigert, and Michael Schroeder.
;;
;; Wicos essentially provided fancy handling for window
;; configurations.  I liked the basic ideas, but wicos broke with
;; later versions of Emacs and XEmacs.  I re-implemented just the
;; functionality I wanted, simplifying things in the process, and
;; porting the code to run with XEmacs 19 and 20, and Emacs 20 (I
;; don't know if winring works in Emacs 19.34).
;;
;; Wicos used the M-o prefix which I've recently changed to C-x 7 as
;; the default, by suggestion of RMS.  Wicos also had some support for
;; multiple frames, and saving configurations on all visible frames,
;; but it didn't work too well, and I like frame independent rings
;; better.
;;
;; I know of a few other related packages:
;;
;;    - `escreen' by Noah Friedman.  A much more ambitious package
;;       that does Emacs window session management.  Very cool, but I
;;       wanted something more lightweight.
;;
;;    - `wconfig' by Bob Weiner as part of Hyperbole.  I think wconfig
;;      is similar in spirit to winring; it seems to have also have
;;      named window configurations, but not frame-specific window
;;      rings.
;;
;;    - `winner' by Ivar Rummelhoff.  This package comes with Emacs
;;      20, and appears to differ from winring by providing undo/redo
;;      semantics to window configuration changes.  winner is a minor
;;      mode and does seem to support frame-specific window rings.
;;
;;    - `tapestry' by Kyle Jones and distributed with the VM package.
;;      A much more featured package, but there is a lot of overlap
;;      and it seems more designed for programmatic rather than
;;      interactive use.

;;    - XEmacs has some built-in support for window configuration
;;      stacks, but I wanted to use a ring structure for managing
;;      configurations.

;; Please feel free to email me if my rendition of history, or my
;; explanation of the related packages, is inaccurate.

;;; Code:

(require 'ring)


(defgroup winring nil
  "Window configuration rings"
  :prefix "winring-"
  :group 'frames)

(defcustom winring-ring-size 7
  "*Size of the window configuration ring."
  :type 'integer
  :group 'winring)

(defcustom winring-prompt-on-create 'usually
  "*When true, prompt for new configuration name on creation.
If not t and not nil, prompt for configuration name on creation,
except when creating the initial configuration on a new frame."
  :type '(radio
	  (const :tag "Never prompt for configuration name" nil)
	  (const :tag "Always prompt for configuration name" t)
	  (const :tag "Prompt for all but initial configuration name"
		 usually)
	  )
  :group 'winring)

(defcustom winring-new-config-buffer-name "*scratch*"
  "*Name of the buffer to switch to when a new configuration is created."
  :type 'string
  :group 'winring)

(defcustom winring-show-names nil
  "*If non-nil, window configuration names are shown in the modeline.
If nil, the name is echoed in the minibuffer when switching window
configurations."
  :type 'boolean
  :group 'winring)

(defcustom winring-name-generator 'winring-next-name
  "*Function that generates new automatic window configuration names.
When a new window configuration is created with `winring-new-configuration',
and the user did not specify an explicit name, this function is called with
no arguments to get the new name.  It must return a string."
  :type 'function
  :group 'winring)

;; Not yet customized
(defvar winring-keymap-prefix "\C-x7"
  "*Prefix key that the `winring-map' is placed on in the global keymap.
If you change this, you must do it before calling `winring-initialize'.")


;; Set up keymap
(defvar winring-map nil
  "Keymap used for winring, window configuration rings.")
(if winring-map
    nil
  (setq winring-map (make-sparse-keymap))
  (define-key winring-map "b" 'winring-submit-bug-report)
  (define-key winring-map "n" 'winring-new-configuration)
  (define-key winring-map "2" 'winring-duplicate-configuration)
  (define-key winring-map "j" 'winring-jump-to-configuration)
  (define-key winring-map "0" 'winring-delete-configuration)
  (define-key winring-map "o" 'winring-next-configuration)
  (define-key winring-map "p" 'winring-prev-configuration)
  (define-key winring-map "r" 'winring-rename-configuration)
  (define-key winring-map "v" 'winring-version)
  )



;; Winring names
(defvar winring-name nil
  "The name of the currently displayed window configuration.")

(defvar winring-name-index 1
  "Index used as a sequence number for new unnamed window configurations.")

(defvar winring-name-history nil
  "History variable for window configuration name prompts.")

(defun winring-next-name ()
  (let ((name (format "%03d" winring-name-index)))
    (setq winring-name-index (1+ winring-name-index))
    name))



;; Compatibility
(defun winring-set-frame-ring (frame ring)
  (cond
   ;; XEmacs
   ((fboundp 'set-frame-property)
    (set-frame-property frame 'winring-ring ring))
   ;; Emacs
   ((fboundp 'modify-frame-parameters)
    (modify-frame-parameters frame (list (cons 'winring-ring ring))))
   ;; Not supported
   (t (error "This version of Emacs is not supported by winring"))))

(defun winring-get-frame-ring (frame)
  (cond
   ;; XEmacs
   ((fboundp 'frame-property)
    (frame-property frame 'winring-ring))
   ;; Emacs 20
   ((fboundp 'frame-parameter)
    (frame-parameter frame 'winring-ring))
   ;; Emacs 19.34
   ((fboundp 'frame-parameters)
    (cdr (assq 'winring-ring (frame-parameters frame))))
   ;; Unsupported
   (t (error "This version of Emacs is not supported by winring"))))

(defun winring-create-frame-hook (frame)
  ;; generate the name, but specify the newly created frame
  (winring-set-name (and (eq winring-prompt-on-create t)
			 (read-string "Initial window configuration name? "
				      nil 'winring-name-history))
		    frame))


;; Utilities
(defun winring-set-name (&optional name frame)
  "Set the window configuration name.
Optional NAME is the name to use; if not given, then
`winring-name-generator' is `funcall'd with no arguments to get the
generated name.  Optional FRAME is the frame to set the name for; if
not given then the currently selected frame is used."
  (let ((name (or name (funcall winring-name-generator)))
	(frame (or frame (selected-frame))))
    (if (fboundp 'add-spec-to-specifier)
	;; The XEmacs way.  Only supported in hacked 20.4 or 21.0
	(add-spec-to-specifier winring-name name frame)
      ;; the Emacs way.  Only supported in Emacs 20.3
      (modify-frame-parameters frame (list (cons 'winring-name name)))
      ))
  (if (not winring-show-names)
      (message "Switching to window configuration: %s" name)))

(defun winring-get-ring ()
  (let* ((frame (selected-frame))
	 (ring (winring-get-frame-ring frame)))
    (when (not ring)
      (setq ring (make-ring winring-ring-size))
      (winring-set-frame-ring frame ring))
    ring))

(defsubst winring-name-of (config)
  (car config))

(defsubst winring-conf-of (config)
  (car (cdr config)))

(defsubst winring-point-of (config)
  (nth 2 config))

(defsubst winring-name-of-current ()
  (if (fboundp 'specifier-instance)
      ;; In XEmacs, this variable holds a specifier which
      ;; must be instanced to get the current
      ;; configuration name.
      (specifier-instance winring-name)
    ;; In Emacs, just use the variable's string value
    ;; directly, since the `displayed' value is kept as a
    ;; frame parameter
    winring-name))

(defun winring-save-current-configuration (&optional at-front)
  (let* ((ring (winring-get-ring))
	 (name (winring-name-of-current))
	 (here (point))
	 (conf (list name (current-window-configuration) here)))
    (if at-front
	(ring-insert-at-beginning ring conf)
      (ring-insert ring conf))))

(defun winring-restore-configuration (item)
  (let ((conf (winring-conf-of item))
	(name (winring-name-of item))
	(here (winring-point-of item)))
    (set-window-configuration conf)
    ;; current-window-configuration does not save point in current
    ;; window.  That sucks!
    (goto-char here)
    (winring-set-name name))
  (force-mode-line-update))

(defun winring-complete-name ()
  (let* ((ring (winring-get-ring))
	 (n (1- (ring-length ring)))
	 (current (winring-name-of-current))
	 (table (list (cons current -1)))
	 name)
    ;; populate the completion table
    (while (<= 0 n)
      (setq table (cons (cons (winring-name-of (ring-ref ring n)) n) table)
	    n (1- n)))
    (setq name (completing-read
		(format "Window configuration name (%s): " current)
		table nil 'must nil 'winring-name-history))
    (if (string-equal name "")
	(setq name current))
    (cdr (assoc name table))))

(defun winring-read-name (prompt)
  (let* ((ring (winring-get-ring))
	 (n (1- (ring-length ring)))
	 (table (list (winring-name-of-current)))
	 name)
    ;; get the list of all the names in the ring
    (while (<= 0 n)
      (setq table (cons (winring-name-of (ring-ref ring n)) table)
	    n (1- n)))
    (setq name (read-string prompt nil 'winring-name-history))
    (if (member name table)
	(error "Window configuration name already in use: %s" name))
    name))


;; Commands

;;;###autoload
(defun winring-new-configuration (&optional arg)
  "Save the current window configuration and create an empty new one.
The buffer shown in the new empty configuration is defined by
`winring-new-config-buffer-name'.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name."
  (interactive "P")
  (let ((name (and (or arg winring-prompt-on-create)
		   (winring-read-name "New window configuration name? "))))
    ;; Empty string is not allowed
    (if (string-equal name "")
	(setq name (funcall winring-name-generator)))
    (winring-save-current-configuration)
    (delete-other-windows)
    (switch-to-buffer winring-new-config-buffer-name)
    (winring-set-name name)))

;;;###autoload
(defun winring-duplicate-configuration (&optional arg)
  "Push the current window configuration on the ring, and duplicate it.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name."
  (interactive "P")
  (let ((name (and (or arg winring-prompt-on-create)
		   (winring-read-name "New window configuration name? "))))
    ;; Empty string is not allowed
    (if (string-equal name "")
	(setq name (funcall winring-name-generator)))
    (winring-save-current-configuration)
    (winring-set-name name)))

;;;###autoload
(defun winring-next-configuration ()
  "Switch to the next window configuration for this frame."
  (interactive)
  (let ((next (ring-remove (winring-get-ring))))
    (winring-save-current-configuration)
    (winring-restore-configuration next)))

;;;###autoload
(defun winring-prev-configuration ()
  "Switch to the previous window configuration for this frame."
  (interactive)
  (let ((prev (ring-remove (winring-get-ring) 0)))
    (winring-save-current-configuration 'at-front)
    (winring-restore-configuration prev)))

;;;###autoload
(defun winring-jump-to-configuration ()
  "Go to the named window configuration."
  (interactive)
  (let* ((ring (winring-get-ring))
	 (index (winring-complete-name))
	 item)
    ;; if the current configuration was chosen, winring-complete-name
    ;; returns -1
    (when (<= 0 index)
      (setq item (ring-remove ring index))
      (winring-save-current-configuration)
      (winring-restore-configuration item))
    ))

;;;###autoload
(defun winring-delete-configuration (&optional arg)
  "Delete the current configuration and switch to the next one.
With \\[universal-argument] prompt for named configuration to delete."
  (interactive "P")
  (let ((ring (winring-get-ring))
	index)
    (if (or (not arg)
	    (> 0 (setq index (winring-complete-name))))
	;; remove the current one, so install the next one
	(winring-restore-configuration (ring-remove ring))
      ;; otherwise, remove the named one but don't change the current config
      (ring-remove ring index)
      )))

;;;###autoload
(defun winring-rename-configuration ()
  "Rename the current configuration to NAME."
  (interactive)
  (winring-set-name (winring-read-name "New window configuration name? ")))



(defconst winring-help-address "bwarsaw@python.org"
  "Address accepting bug report submissions.")

(defun winring-version ()
  "Echo the current version of winring in the minibuffer."
  (interactive)
  (message "Using winring version %s" winring-version)
  ;;(setq zmacs-region-stays t)
  )

(defun winring-submit-bug-report (comment-p)
  "Submit via mail a bug report on winring.
With \\[universal-argument] just send any type of comment."
  (interactive
   (list (not (y-or-n-p
	       "Is this a bug report? (hit `n' to send other comments) "))))
  (let ((reporter-prompt-for-summary-p (if comment-p
					   "(Very) brief summary: "
					 t)))
    (require 'reporter)
    (reporter-submit-bug-report
     winring-help-address		 ;address
     (concat "winring " winring-version) ;pkgname
     ;; varlist
     (if comment-p nil
       '(winring-ring-size
	 winring-new-config-buffer-name
	 winring-show-names
	 winring-name-generator
	 winring-keymap-prefix))
     nil				;pre-hooks
     nil				;post-hooks
     "Dear Barry,")			;salutation
    (if comment-p nil
      (set-mark (point))
      (insert
"Please replace this text with a description of your problem.\n\
The more accurately and succinctly you can describe the\n\
problem you are encountering, the more likely I can fix it\n\
in a timely way.\n\n")
      (exchange-point-and-mark)
      ;;(setq zmacs-region-stays t)
      )))



;; Initialization.  This is completely different b/w Emacs and XEmacs.
;; The Emacs 20.3 way is to create a frame-local variable (this is a
;; new feature with Emacs 20.3), and save the config name as a frame
;; property.
;;
;; In XEmacs 21.0 (a.k.a. 20.5), you create a generic specifier, and
;; save the config name as an instantiator over the current frame
;; locale.

;; Be sure to do this only once
(defvar winring-initialized nil)

(defun winring-initialize (&optional hack-modeline-function)
  (unless winring-initialized
    ;;
    ;; Create the variable that holds the window configuration name
    ;;
    (cond
     ;; The Emacs 20.3 way: frame-local variables
     ((fboundp 'make-variable-frame-local)
      (make-variable-frame-local 'winring-name))
     ;; The XEmacs 21 way: specifiers
     ((fboundp 'make-specifier)
      (setq winring-name (make-specifier 'generic)))
     ;; Not supported in older X/Emacsen
     (t nil))
    ;;
    ;; Glom the configuration name into the mode-line.  I've
    ;; experimented with a couple of different locations, including
    ;; for Emacs 20.3 mode-line-frame-identification, and for XEmacs,
    ;; just splicing it before the modeline-buffer-identification.
    ;; Sticking it on the very left side of the modeline, even before
    ;; mode-line-modified seems like the most useful and
    ;; cross-compatible place.
    ;;
    ;; Note that you can override the default hacking of the modeline
    ;; by passing in your own `hack-modeline-function'.
    ;;
    (if hack-modeline-function
	(funcall hack-modeline-function)
      ;; Else, default insertion hackery
      (let ((format (list 'winring-show-names
			  '("<" winring-name "> ")))
	    (splice (cdr mode-line-format)))
	(setcar splice (list format (car splice)))))
    ;;
    ;; We need to add a hook so that all newly created frames get
    ;; initialized properly.  Again, different for Emacs and XEmacs.
    ;;
    (if (boundp 'create-frame-hook)
	;; XEmacs
	(add-hook 'create-frame-hook 'winring-create-frame-hook)
      ;; better be Emacs!
      (add-hook 'after-make-frame-functions 'winring-create-frame-hook))
    ;;
    ;; Now set the initial configuration name on the initial frame...
    (winring-create-frame-hook (selected-frame))
    ;; ...the keymap...
    (global-set-key winring-keymap-prefix winring-map)
    ;; ...and the init fence
    (setq winring-initialized t)))



(provide 'winring)
;;; winring.el ends here

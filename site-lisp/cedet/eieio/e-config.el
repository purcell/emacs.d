;;; e-config - configureation for emacs
;;;
;;; Copyright (C) 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; RCS: $Id: e-config.el,v 1.10 1997/01/19 22:08:28 zappo Exp $
;;; Keywords: OO, dialog, configure
;;;                                                                          
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;      
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;      
;;; e-config can be found in the eieio or etalk distributions on:
;;;  ftp://ftp.ultranet.com/pub/zappo
;;;
;;; Commentary:
;;;   Uses dlg-config to edit options for the emacs editor.  Because
;;; large dialogs slow down the system, each individual dialog will
;;; be as small as possible.  Each function mearly creates on dialog
;;; buffer.  See dlg-config or dialog for details on how a dialog works.
;;; These functions can be considered only a screen definitions.
;;;           
(require 'dlg-config)


;;;
;;; General Interface Options
;;;
(defun econfig-interface ()
  "Creates a configure window with variables modifying the visual interface
for emacs."
  (interactive)
  (dlg-init 'dot-emacs)
  (dialog-build-group "Interface Options"
    (dlg-bunch-of-simple-toggles
     "Display Line Number in Modeline" 'line-number-mode
     "Display Column Number in Modeline" 'column-number-mode
     "Truncate Lines" 'truncate-lines
     "Suggest Key Bindings" (if (boundp 'suggest-key-bindings)
				'suggest-key-bindings
			      'teach-extended-commands-p)
     "Visible Bell" 'visible-bell
     "Enable Recursive Minibuffers" 'enable-recursive-minibuffers
     "Search Highlights Current Match" (if (boundp 'search-highlight)
					   'search-highlight
					 'isearch-highlight)
     "Query Replace Highlight" 'query-replace-highlight
     "Inverse Video" 'inverse-video
     "Modeline Inverse Video" 'mode-line-inverse-video
     )

    (create-widget "Scroll Step:" widget-labeled-text
		   :unit "lines" :text-length 10 
		   :value (data-object-symbol-string-to-int 'scroll-step
							    :float-p nil))
    )
  (dialog-build-group "Startup Commands"

    (create-widget "Display time in modeline" widget-toggle-button
		   :state (data-object-command-option "(display-time)"))
    (if (boundp 'type-break-mode)
	(create-widget "Typing break mode (remind you to rest)" widget-toggle-button
		       :state (data-object-symbol 'type-break-mode)))
    (create-widget "Highlight Parenthesis" widget-toggle-button
		   :state (data-object-symbol-feature
			   'paren
			   :unload-commands 
			   (if (> emacs-minor-version 34)
			       '(show-paren-mode -1) nil)
			   ))
    (create-widget "Automatically Scroll Horizontally" widget-toggle-button
		   :state (data-object-symbol-feature
			   'auto-show
			   :unload-commands '(auto-show-mode -1)))
    (if (not dialog-xemacs-p)
	(create-widget "Magically Resize Minibuffer when needed." widget-toggle-button
		       :state (data-object-symbol-feature
			       'rsz-mini
			       :unload-commands
			       '(progn
				  (remove-hook 'minibuffer-setup-hook
					       'resize-minibuffer-setup)
				  (resize-minibuffer-mode -1)))))
    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Editing Options
;;;
(defun econfig-editing ()
  "Creates a configure window with variables modifying the editing interface
for emacs."
  (interactive)
  (dlg-init 'dot-emacs)
  (dialog-build-group "Behavior Options"

    (dlg-bunch-of-simple-toggles
     "Mouse Yanks to Cursor (not mouse)" 'mouse-yank-at-point
     "Next-line adds newline at end of buffer" 'next-line-add-newlines
     "Adaptive Fill Mode" 'adaptive-fill-mode
     "Require Final Newline" 'require-final-newline)

    (create-widget "Auto fill in all text modes" widget-toggle-button
		   :state (data-object-symbol-hook
			   'text-mode-hook
			   :command "turn-on-auto-fill"))

    ;(create-widget "Delete Highlighted Selections on Input" widget-toggle-button
    ;		   :state (data-object-command-option
;			   "(progn (require 'delsel)(setq delete-selection-mode t))"
;			   :disable-command "(setq delete-selection-mode nil)"
;			   :value (and (featurep 'delsel) delete-selection-mode)))
    )
  (dialog-build-group "Suggested Disabled Commands"

    (create-widget "Evaluate Expression  < M-: >" widget-toggle-button
		   :state (data-object-symbol-disabled 'eval-expression))
    (create-widget "Narrow to region     < C-x n n >" widget-toggle-button
		   :state (data-object-symbol-disabled 'eval-expression))
    (create-widget "Set Goal Column      < C-x C-n >" widget-toggle-button
		   :state (data-object-symbol-disabled 'set-goal-column))
    (create-widget "Erase buffer         < M-x erase-buffer >" widget-toggle-button
		   :state (data-object-symbol-disabled 'erase-buffer))
    )
  (dialog-build-group "Backup File Methods"

    (dlg-bunch-of-simple-toggles
     "By Copying (off uses move)" 'backup-by-copying
     "By Copying When Linked" 'backup-by-copying-when-linked
     "By Copying When Owner/Group Mismatch " 'backup-by-copying-when-mismatch)
    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Programming Options
;;;
(defun econfig-programmer ()
  "Creates a configure window with variables modifying variables
useful for programmers."
  (interactive)
  (dlg-init 'dot-emacs)
  (dialog-build-group "Program Editing Options"

    (create-widget "Enable Local Variables" widget-toggle-button
		   :state (data-object-symbol 'enable-local-variables))

    (if (not dialog-xemacs-p)
	(create-widget "Check for copyright update" widget-toggle-button
		       :state (data-object-command-option
			       "copyright"
			       ;; Override the default.. this is more dependable
			       :value (member 'copyright-update write-file-hooks)
			       :command "(load-library \"copyright\")(add-hook 'write-file-hooks 'copyright-update)"
			       :disable-command "(remove-hook 'write-file-hooks 'copyright-update)")))
    ;; compile stuff
    (require 'compile)

    (create-widget "Compile Finish Command:" widget-labeled-text
		   :text-length 50
		   :value (data-object-symbol-lisp-expression
			   'compilation-finish-function))

    (create-widget "Compile Command       :" widget-labeled-text
		   :text-length 50
		   :value (data-object-symbol 'compile-command))

    (dlg-info-button "Want to know more about Compiling?"
		     "(emacs)Compilation"
		     "Click to read info pages about compiling programs.")
    )
  (require 'vc)
  (dialog-build-group "Version Control Interface Options"
    (dlg-bunch-of-simple-toggles
     "I am an expert with vc.el" 'vc-suppress-confirm
     "Prompt for comment for file registration" 'vc-initial-comment
     "Display Run Messages From Back-end" 'vc-command-messages
     "Be super-careful during checkouts" 'vc-checkout-carefully
     )
    (dlg-info-button "Want to know more about Version Control?"
		     "(emacs)Version Control"
		     "Click to read info pages about version control.")
    )
  (dlg-end)
  (dialog-refresh)
  )


;;;
;;; Dired mode configuration
;;;
(defun econfig-dired ()
  "Creates a configure window for modifying behavior of a dired buffer."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'dired)
  (dialog-build-group "Dired Listing Options and Parsing"

    (create-widget "Command Options for ls:" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'dired-listing-switches))

    (create-widget "ls -F marks symlinks" widget-toggle-button
		   :state (data-object-symbol 'dired-ls-F-marks-symlinks))

    )
  (dialog-build-group "Dired Behaviors"

    (create-widget "Copy Preserves Time" widget-toggle-button
		   :state (data-object-symbol 'dired-copy-preserve-time))
		   
    (create-widget "Guess Alternate Window Directory for Copy" widget-toggle-button
		   :state (data-object-symbol 'dired-dwim-target))

    (create-widget "Use the dired-x extensions" widget-toggle-button
		   :state (data-object-symbol-hook 
			   'dired-load-hook
			   :command "(lambda () (require 'dired-x))")
		   :help-hook (lambda (obj reason)
				(message "Adds to `dired-load-hook' a command to require the use of dired-x")))

    (create-widget "Load Now" widget-push-button
		   :x -2 :y t
		   :activate-hook (lambda (obj reason) (require 'dired-x))
		   :help-hook (lambda (obj reason)
				(message "Load the dired-x package now.")))

    (dlg-info-button "Want to know more about dired?"
		     "(emacs)dired"
		     "Click to read info pages about dired mode")
    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Ange-ftp configuration
;;;

(defun econfig-ange-ftp ()
  "Creates a configure window modifying variables useful with the ange-ftp
package."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'ange-ftp)
  (dialog-build-group "Gateway Definition"
    (create-widget "Note: The regular expression `.*' means ANY host.
If you wish to specify those hosts on one side
of a firewall, an example would be: `\\.domain\\.com$'"
		   widget-label)
    (let ((domain (if (string-match "\\.\\w+\\.\\w+$" (system-name))
		      (match-string 0 (system-name))
		    nil)))
		    
      (create-widget "Local Host Regexp: " widget-option-text
		     :text-length 30
		     :value (data-object-symbol 'ange-ftp-local-host-regexp)
		     :option-list (list (regexp-quote domain)
					(regexp-quote (system-name))
					".*"))

      (create-widget "Gateway Host:      " widget-option-text
		     :text-length 30
		     :value (data-object-symbol 'ange-ftp-gateway-host)
		     :option-list (list (concat "gateway" domain)
					(concat "ftp" domain)
					(or ange-ftp-gateway-host "")))
      )
    
    (dialog-build-group "Smart Gateways"
      (create-widget "Smart gateways are FTP proxies where user@host is issued
to the user prompt to access remote sites." 
		     widget-label)
      (create-widget "Use smart gateway proxy." widget-toggle-button
		     :state (data-object-symbol 'ange-ftp-smart-gateway))
      (create-widget "Port number: " widget-labeled-text
		     :text-length 10
		     :value (data-object-symbol 'ange-ftp-smart-gateway-port))
      )
    
    (dialog-build-group "Not so smart gateways"
      (create-widget "Shared Directory  :" widget-labeled-text
		     :text-length 50
		     :value (data-object-symbol
			     'ange-ftp-gateway-tmp-name-template))
      (create-widget "Gateway Shell Prog:" widget-labeled-text
		     :text-length 20
		     :value (data-object-symbol
			     'ange-ftp-gateway-program))

      (create-widget
       "If you must use telnet or rlogin instead of `rsh' or `remsh', then
you will need to set the following as well."
       widget-label)

      (create-widget "Use Interactive Gateway" widget-toggle-button
		     :state (data-object-symbol 
			     'ange-ftp-gateway-program-interactive))

      (create-widget "Prompt Pattern  :" widget-labeled-text
		     :text-length 20
		     :value (data-object-symbol
			     'ange-ftp-gateway-prompt-pattern))

      (create-widget "Set Term Command:" widget-labeled-text
		     :text-length 40
		     :value (data-object-symbol
			     'ange-ftp-gateway-setup-term-command))

      )

    (create-widget "Confused on how to get the gateway to work?" widget-label)
    (create-widget "Read The Source" widget-push-button
		   :x -2 :y t
		   :help-hook
		   (lambda (this reason)
		     (message
		      "Click to load in the ange-ftp source file to read about gateways."))
		   :activate-hook
		   (lambda (this reason)
		     (find-file (locate-library "ange-ftp.el"))
		     (goto-char (point-min))
		     (re-search-forward "^;;\\s-*Gateways:\\s-*$" nil t)
		     (recenter 0)))

    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; bookmark configuration
;;;
(defun econfig-bookmarks ()
  "Creates a configuration window for use with the bookmarks package."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'bookmark)
  (dialog-build-group (if (boundp 'bookmark-version)
			  (format "Bookmark Options for v %s" bookmark-version)
			"Bookmark Options")
    
  (dlg-bunch-of-simple-toggles
   "Require annotations with bookmarks" 'bookmark-use-annotations
   "Show annotations during jump" 'bookmark-automatically-show-annotations
   "Completion ignores case" 'bookmark-completion-ignore-case
   "Sort bookmarks" 'bookmark-sort-flag)

  (create-widget "Save File:" widget-labeled-text
		 :text-length 40
		 :value (data-object-symbol 'bookmark-default-file))

  (dlg-info-button "Want to learn more about bookmarks?"
		   "(emacs)Bookmarks"
		   "Press to read info files about bookmarks")
  )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Ispell configurations.  Unfortunatly, this may prove version
;;; specific.
;;;
(defun econfig-ispell ()
  "Configuration dialog for modifying the behavior of ispell."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'ispell)
  (dialog-build-group (format "Ispell %s Options" 
			      (substring ispell-version 0 4))
  (dlg-bunch-of-simple-toggles
   "Highlight Spelling Errors" 'ispell-highlight-p
   "Spell-check quietly" 'ispell-quietly
   "Check Spelling Of Comments" 'ispell-check-comments
   "Query-Replace Corrections (if same error multiple times)"
   'ispell-query-replace-choices
   "Skip tibs (For TeX bibliographies)" 'ispell-skip-tib
   "Keep Choices Window Visible" 'ispell-keep-choices-win)

  (create-widget "Choices Window Height:" widget-labeled-text
		 :text-length 5 :unit "Lines"
		 :value (data-object-symbol-string-to-int 
			 'ispell-choices-win-default-height
			 :float-p nil))

  (create-widget "Alternate dictionary :" widget-labeled-text
		 :text-length 30
		 :value (data-object-symbol 'ispell-alternate-dictionary))

  (create-widget "Personaly dictionary :" widget-labeled-text
		 :text-length 20 :unit "(blank = ~/.ispell_DICTNAME)"
		 :value (data-object-symbol 'ispell-personal-dictionary))
     
  (dlg-bunch-of-simple-toggles
   "Silently save personal dictionary" 'ispell-silently-savep)

  (dlg-info-button "Want to learn more about Ispell?"
		   "(ispell)Emacs"
		   "Press to read info files about ispell in emacs")
  )  
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Mail configurations
;;;
(defun econfig-rmail ()
  "Creates a configure window with variables modifying variables
useful for sending email."
  (interactive)
  (if dialog-xemacs-p (error "This rmail dialog is not for XEmacs."))
  (dlg-init 'dot-emacs)
  (require 'rmail)
  (dialog-build-group "Rmail Options"

    (create-widget "Rmail File               :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'rmail-file-name))
    
    (create-widget "Secondary File Directory :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'rmail-secondary-file-directory))
    
    (create-widget "Default Secondary File   :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'rmail-default-rmail-file))
    
    (dlg-bunch-of-simple-toggles
     "Delete messages after saving to secondary file" 'rmail-delete-after-output
     "Summary motion scrolls messages" 'rmail-summary-scroll-between-messages)

    (dlg-info-button "Want to know more about reading mail?"
		     "(emacs)rmail"
		     "Click to read info pages about rmail.")
    )
  (dlg-end)
  (dialog-refresh)
  )

(defun econfig-mail-showfrom (style)
  "Return a string which is how the mail address would be shown"
  (cond ((or (eq style 2) (eq style 'angles))
	 (concat (user-full-name) " <" user-mail-address ">"))
  ((or (eq style 1) (eq style 'parens))
   (concat user-mail-address " (" (user-full-name) ")"))
  (t user-mail-address)))

(defun econfig-mail ()
  "Creates a configure window with variables modifying variables
useful for sending email."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'sendmail)
  (dialog-build-group "Mail Address Options"
    
  (let* ((uma (data-object-symbol 'user-mail-address))
	 (opt-list '("nil" "'parens" "'angles"))
	 (fsdo (data-object-symbol-list-index
		'mail-from-style
		:value (cond ((eq mail-from-style 'angles) 2)
			     ((eq mail-from-style 'parens) 1)
			     (t 0))
		:string-list opt-list))
	 (emdo (data-object "example-mail-name" 
			    :value (econfig-mail-showfrom mail-from-style))))

    (create-widget "Mail Address   :" widget-labeled-text :text-length 50
		   :value uma)
      
    (create-widget "Reply-to       :" widget-labeled-text :text-length 50
		   :value (data-object-symbol 'mail-default-reply-to))
    
    (create-widget "From Style     :" widget-label)

    (create-widget "name-type" widget-option-button
		   :title "Name Format"
		   :x -2 :y t :option-list opt-list :state fsdo)

    (create-widget "Looks Like:" widget-label :y -1)
    (create-widget "example-label" widget-label :x -2 :y t 
		   ;; set max width
		   :width (+ 3 (length (user-full-name))
			     (length user-mail-address))
		   :justification 'left
		   :face 'bold
		   :label-value emdo)

    ;; This translates the address from one type to the other
    (create-widget "address-translator" widget-gadget-translator
		   :watch fsdo :change emdo
		   :translate-function 
		   (lambda (a b) 
		     (set-value b (econfig-mail-showfrom (get-value a)) 
				this)))
    (create-widget "address-translator" widget-gadget-translator
		   :watch uma :change emdo
		   :translate-function 
		   (lambda (a b) 
		     (let ((user-mail-address (get-value a)))
		       (set-value b (econfig-mail-showfrom mail-from-style)
				  this))))
    ))
  (dialog-build-group "Editing outbound mail"
    
  (dlg-bunch-of-simple-toggles
   "Add BCC to yourself for outbound message" 'mail-self-blind
   )

  (create-widget "Personal Alias File:" widget-labeled-text
		 :text-length 30
		 :value (data-object-symbol 'mail-personal-alias-file))

  (create-widget "Signature File :" widget-labeled-text :text-length 50
		 :value (data-object-symbol 'mail-signature-file))
    
  (create-widget "Auto load signature file" widget-toggle-button
		 :state (data-object-symbol 'mail-signature))
  (create-widget  "Spellcheck outbound messages (with ispell)"
		  widget-toggle-button
		  :state (data-object-symbol-hook
			  'mail-send-hook
			  :command 
			  "(lambda () (if (y-or-n-p \"Spell message?\") (ispell-message)))"
			  ))
    
  (create-widget "Citation Prefix:" widget-labeled-text :text-length 10
		 :value (data-object-symbol 'mail-yank-prefix))

  )
  (dlg-info-button "Want to know more about sending mail?"
  "(emacs)Sending Mail"
  "Click to read info pages about sending mail.")

  (dlg-end)
  (dialog-refresh)
  )


;;;
;;; Supercite options
;;;
(defun econfig-supercite ()
  "Creates a configure window with variables modifying how supercite behaves."
  (interactive)
  (require 'supercite)
  (dlg-init 'dot-emacs)
  (dialog-build-group (format "Supercite %s Options" sc-version)
    
  (create-widget "Use Supercite in mail mode" widget-toggle-button
		 :state (data-object-symbol-hook 
			 'mail-citation-hook
			 :command "sc-cite-original"))
					;    (create-widget "Disable GNUS default citation method"
					;		   :state (data-object-symbol 'news-reply-header-hook))

  (dlg-bunch-of-simple-toggles
   "Auto-fill cited regions" 'sc-auto-fill-region-p
   "Cite Blank Lines" 'sc-cite-blank-lines-p
   "Confirm Citation Attribution" 'sc-confirm-always-p
   "Downcase All Attribution Strings" 'sc-downcase-p
   "Fixup Leading Whitespace in Citation" 'sc-fixup-whitespace-p
   "Use Nested-Citation Styles" 'sc-nested-citation-p
   "Use Anon When No Attribution String Is Available" 
   'sc-use-only-preference-p
   )

  (let ((nuke-list '("'all" "'none" "'specified" "'keep")))

    (create-widget "Header Nuking Method:" widget-label)

    (create-widget "nuke-me" widget-option-button
		   :title "Nuke Method" :x -2 :y t
		   :option-list nuke-list
		   :state (data-object-symbol-list-index
			   'sc-nuke-mail-headers
			   :value (cond 
				   ((eq sc-nuke-mail-headers 'all) 0)
				   ((eq sc-nuke-mail-headers 'none) 1)
				   ((eq sc-nuke-mail-headers 'specified) 2)
				   (t 3))
			   :string-list nuke-list))
    )
      
  (create-widget "Nuke List" widget-push-button
		 :activate-hook 
		 (lambda (obj reason)
		   (describe-variable 'sc-nuke-mail-header-list)))
  (create-widget "Valid when \"Nuke Method\" is\n'specified or 'keep"
		 widget-label :x -3 :y t)
  (create-widget "nuke-text" widget-scrolled-text
		 :width 40 :height 5 :y -1
		 :value (data-object-symbol-translated
			 'sc-nuke-mail-header-list
			 :set-lambda		
			 (lambda (obj)
			   (dlg-string-to-list obj "[ ]*\n"))
			 :get-lambda
			   (lambda (obj)
			     (dlg-list-to-string obj "\n"))))

    (create-widget "Blanks after header:" widget-labeled-text
		   :text-length 10 :unit "lines"
		   :value (data-object-symbol-string-to-int 
			   'sc-blank-lines-after-headers
			   :float-p nil))

    (dlg-info-button "Want to know more about Supercite?"
		     "(sc)Top"
		     "Click to read info pages about Supercite.")
    )
  (dlg-end)
  (dialog-refresh)
  )


;;;
;;; Calendar/diary options
;;;
(defun econfig-calendar ()
  "Creates a configure window with variables modifying calendar mode."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'calendar)
  (dialog-build-group "Calendar Options"

    (create-widget "Latitude :" widget-labeled-text
		   :unit "Degrees" :text-length 10
		   :value (data-object-symbol-string-to-int 
			   'calendar-latitude))

    (create-widget "Longitude:" widget-labeled-text
		   :unit "Degrees" :text-length 10
		   :value (data-object-symbol-string-to-int 
			   'calendar-longitude))


    )
  (dialog-build-group "Holiday Options"

    (dlg-bunch-of-simple-toggles
     "Show all Christian Holidays" 'all-christian-calendar-holidays
     "Show all Hebrew Holidays" 'all-hebrew-calendar-holidays
     "Show all Islamic Holidays" 'all-islamic-calendar-holidays
     "Show Holidays at Startup" 'view-calendar-holidays-initially)
    )
  (require 'appt)
  (require 'diary-ins)
  (dialog-build-group "Appointment Options"

    (create-widget "Diary File:" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'diary-file))

    (create-widget "Warn of impending appointments" widget-toggle-button
		   :state (data-object-symbol-hook
			   'diary-hook :command "appt-make-list"))
    (dlg-bunch-of-simple-toggles
     "Appointments Audible" 'appt-audible
     "Appointments Displayed" 'appt-display-diary
     "Display Appointment Time in Modeline" 'appt-display-mode-line
     "Show Appointments at Startup" 'view-diary-entries-initially)
    )
  (dlg-info-button "Want to know more about calendar and diary?"
		   "(emacs)Calendar/Diary"
		   "Click to read info pages about calendar mode and using the diary.")
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; ps print options
;;;
(defun econfig-ps-print ()
  "Creates a configure window with variables modifying variables
useful for ps-print."
  (interactive)
  (dlg-init 'dot-emacs)
  (require 'ps-print)
  (dialog-build-group "Postscript Printing Options"

    (dlg-bunch-of-simple-toggles
     "Print header on each page." 'ps-print-header
     "Print page numbers (Must have headers on)." 'ps-show-n-of-n
     "Print gaudy frame around header." 'ps-print-header-frame
     "Print with color." 'ps-print-color-p
     "Auto-detect faces for bold, italic, and underline." 'ps-auto-font-detect)

    (create-widget "Printed Font Size :" widget-labeled-text
		   :unit "Pts" :text-length 10
		   :value (data-object-symbol-string-to-int
			   'ps-font-size))

    (create-widget "Note: You must change the character width and height
whenever you change the font size, or the font family." widget-label
                   :x 5 :y -1 :face 'bold)

    (create-widget "Character Width   :" widget-labeled-text
		   :unit "Pts" :text-length 10 :y -1
		   :value (data-object-symbol-string-to-int 'ps-avg-char-width))
    (create-widget "Space Width       :" widget-labeled-text
		   :unit "Pts" :text-length 10
		   :value (data-object-symbol-string-to-int 'ps-space-width))
    (create-widget "Line Height       :" widget-labeled-text
		   :unit "Pts" :text-length 10
		   :value (data-object-symbol-string-to-int 'ps-line-height))


    (create-widget "Note: All font families listed are optimized
to work with Ghostscript" widget-label :face 'bold-italic :x 5)

    (create-widget "Printed Font Family:" widget-option-text
		   :y -1
		   :text-length 30 :option-list 
		   '("Courier"
		     "CharterBT-Roman"
		     "Times-Roman"
		     "Helvetica"
		     "ZapfChancery"
		     "Palatino-Roman"
		     "NewCenturySchlbk-Roman"
		     "Utopia-Regular"
		     )
		   :value (data-object-symbol 'ps-font))

    (create-widget "Bold Font Family   :" widget-option-text
		   :box-face 'bold
		   :text-length 30 :option-list
		   '("Courier-Bold"
		     "Charter-Bold"
		     "Times-Bold"
		     "Helvetica-Bold"
		     "ZapfChancery-Bold"
		     "Palatino-Bold"
		     "NewCenturySchlbk-Bold"
		     "Utopia-Bold"
		     )
		   :value (data-object-symbol 'ps-font-bold))

    (create-widget "Italic Font Family :" widget-option-text
		   :box-face 'italic
		   :text-length 30 :option-list
		   '("Courier-Oblique"
		     "Charter-Italic"
		     "Times-Italic"
		     "Helvetica-Oblique"
		     "ZapfChancery-Oblique"
		     "Palatino-Italic"
		     "NewCenturySchlbk-Italic"
		     "Utopia-Italic"
		     )
		   :value (data-object-symbol 'ps-font-italic))

    (create-widget "Bold Italic Font   :" widget-option-text
		   :box-face 'bold-italic
		   :text-length 30 :option-list 
		   '("Courier-BoldOblique"
		     "Charter-BoldItalic"
		     "Times-BoldItalic"
		     "Helvetica-BoldOblique"
		     ;; Offer both since there is no bold-oblique
		     "ZapfChancery-Bold"
		     "ZapfChancery-Oblique"
		     "Palatino-BoldItalic"
		     "NewCenturySchlbk-BoldItalic"
		     "Utopia-BoldItalic"
		     )
		   :value (data-object-symbol 'ps-font-bold-italic))

    (create-widget "Print command     :" widget-labeled-text
		   :text-length 20
		   :value (data-object-symbol 'ps-lpr-command))
    (create-widget "lpr parameters    :" widget-labeled-text
		   :text-length 30
		   :value (data-object-symbol-translated 
			   'ps-lpr-switches
			   :set-lambda 
			   (lambda (obj)
			     (dlg-string-to-list obj "[ ]+"))
			   :get-lambda 
			   (lambda (obj)
			     (dlg-list-to-string obj " "))))
    (create-widget "Paper Size        :" widget-label)
    (let* ((opt-list '("'ps-letter" "'ps-legal" "'ps-a4"))
	   (opt-dat (data-object-symbol-list-index
		     'ps-paper-type
		     :value (cond ((eq ps-paper-type 'ps-letter) 0)
				  ((eq ps-paper-type 'ps-legal) 1)
				  ((eq ps-paper-type 'ps-a4) 2)
				  (t 0))
		     :string-list opt-list)))

      (create-widget "paper-size" widget-option-button
		     :title "Paper Size"
		     :x -2 :y t :option-list opt-list :state opt-dat)
      )

    (dlg-info-button "Want to know more about postscript printing?"
		     "(emacs)Postscript"
		     "Click to read info pages about printing with the postscript driver.")

    )
  (dlg-end)
  (dialog-refresh)
  )

;;;
;;; Font locking options
;;;
(defun econfig-flock-options ()
  "Creates a configure window with variables modifying how font lock is used."
  (interactive)
  (dlg-init 'dot-emacs)
  (if dialog-xemacs-p (error "This font-lock dialog is not for XEmacs"))
  (dialog-build-group "Font Lock Options"

    (dlg-info-button "More about font-lock"
		     "(emacs)Font Lock"
		     "Click to read info pages about using font-lock.")
    
    (create-widget "Always activate font-lock" widget-toggle-button
		    :state 
		    (if (or (> emacs-major-version 19)
			    (> emacs-minor-version 31))
			(data-object-command-option
			 "(global-font-lock-mode t)"
			 :value global-font-lock-mode
			 :disable-command "(global-font-lock-mode nil)")
		      (data-object-symbol-hook
		       'find-file-hooks
		       :command "turn-on-font-lock")))

    (create-widget "Always use maximum decoration" widget-toggle-button
		   :state (data-object-symbol 
			   'font-lock-maximum-decoration))

    (dialog-build-group (create-widget 
			 "Font Lock Enhancers" widget-radio-frame
			 ;; In this situation, lets
			 ;; turn off some of the sides, and
			 ;; it will behave like a separator
			 :box-sides [nil nil t nil]
			 :position 'top-right
			 :x 0
			 :state (data-object-symbol-list-index
				 'font-lock-support-mode
				 :string-list 
				 '("nil" "'lazy-lock-mode"
				   "'fast-lock-mode")
				 :value 
				 (cond 
				  ((eq font-lock-support-mode 'fast-lock-mode)
				   2)
				  ((eq font-lock-support-mode 'lazy-lock-mode)
				   1)
				  (t 0))))

      (create-widget "Use Nothing (No locking enhancers)" widget-radio-button)

      (create-widget "Use Lazy-Lock (deferred font locking)" widget-radio-button)

      (create-widget "Use Fast-Lock (cached font lock info)" widget-radio-button)

      )
    (dlg-info-button "How to choose a support mode"
		     "(emacs)Support Modes"
		     "Click to read info pages about font-lock support modes.")
    )

  (require 'lazy-lock)
  (dialog-build-group "Lazy Lock Options"

    (create-widget "Defer Time   :" widget-labeled-text
		   :text-length 5 :unit "Seconds"
		   :value (data-object-symbol-string-to-int 
			   'lazy-lock-defer-time))
    
    (create-widget "Stealth Mode verbosity" widget-toggle-button
		   :state (data-object-symbol
			   'lazy-lock-stealth-verbose))

    (create-widget "Stealth Time :" widget-labeled-text
		   :text-length 5 :unit "Seconds"
		   :value (data-object-symbol-string-to-int 
			   'lazy-lock-stealth-time))

    (create-widget "Stealth Lines:" widget-labeled-text
		   :text-length 5 :unit "Lines"
		   :value (data-object-symbol-string-to-int 
			   'lazy-lock-stealth-lines))

    (dlg-info-button "More about Lazy-lock"
		     "(emacs)Lazy Lock Mode"
		     "Click to read info pages about Lazy Lock.")

    )
  (require 'fast-lock)
  (dialog-build-group "Fast Lock Options"

    (create-widget  "Save font cache for files belonging to others" 
		    widget-toggle-button
		    :state (data-object-symbol 'fast-lock-save-others))

    (dlg-info-button "More about fast-lock"
		     "(emacs)Fast Lock Mode"
		     "Click to read info pages about Fast Lock.")

    )
  (dlg-end)
  (dialog-refresh)
  )

(defun econfig-font-lock-faces ()
  "Edit list of font lock used faces"
  (interactive)
  (require 'font-lock)
  (dlg-faces '(font-lock-comment-face
	       font-lock-function-name-face
	       font-lock-string-face
	       font-lock-keyword-face
	       font-lock-reference-face
	       font-lock-variable-name-face
	       font-lock-type-face))
  )

(defun econfig-calendar-faces ()
  "Edit list of faces associated with the calendar and diary"
  (interactive)
  (require 'calendar)
  (dlg-faces '(calendar-today-face
	       diary-face
	       holiday-face)))

(defun econfig-info-faces ()
  "Edit list of faces associated with INFO"
  (interactive)
  (if (not (featurep 'info))
      (error "You must use info (C-h i) before it's faces are available."))
  (dlg-faces '(info-node
	       info-xref
	       info-menu-5)))

;;; end of lisp
(provide 'e-config)

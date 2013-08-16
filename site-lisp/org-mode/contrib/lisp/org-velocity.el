;;; org-velocity.el --- something like Notational Velocity for Org.

;; Copyright (C) 2010-2012 Paul M. Rodriguez

;; Author: Paul M. Rodriguez <paulmrodriguez@gmail.com>
;; Created: 2010-05-05
;; Version: 3.0

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
;; Org-Velocity.el is an interface for Org inspired by the minimalist
;; notetaking program Notational Velocity. The idea is to let you
;; amass and access brief notes on many subjects with minimal fuss.
;; Each note is an entry in an ordinary Org file.

;; Org-Velocity can be used in two ways: when called outside Org, to
;; store and access notes in a designated bucket file; or, when called
;; inside Org, as a method for navigating any Org file. (Setting the
;; option `org-velocity-always-use-bucket' disables navigation inside
;; Org files by default, although you can still force this behavior by
;; calling `org-velocity-read' with an argument.)

;; Org-Velocity prompts for search terms in the minibuffer. A list of
;; headings of entries whose text matches your search is updated as
;; you type; you can end the search and visit an entry at any time by
;; clicking on its heading.

;; RET displays the results. If there are no matches, Org-Velocity
;; offers to create a new entry with your search string as its
;; heading. If there are matches, it displays a list of results where
;; the heading of each matching entry is hinted with a number or
;; letter; clicking a result, or typing the matching hint, opens the
;; entry for editing in an indirect buffer. 0 forces a new entry; RET
;; reopens the search for editing.

;; You can customize every step in this process, including the search
;; method, completion for search terms, and templates for creating new
;; entries; M-x customize-group RET org-velocity RET to see all the
;; options.

;; Thanks to Richard Riley, Carsten Dominik, Bastien Guerry, and Jeff
;; Horn for their suggestions.

;;; Usage:
;; (require 'org-velocity)
;; (setq org-velocity-bucket (expand-file-name "bucket.org" org-directory))
;; (global-set-key (kbd "C-c v") 'org-velocity)

;;; Code:
(require 'org)
(require 'button)
(require 'electric)
(require 'dabbrev)
(eval-when-compile (require 'cl))

(defgroup org-velocity nil
  "Notational Velocity-style interface for Org."
  :tag "Org-Velocity"
  :group 'outlines
  :group 'hypermedia
  :group 'org)

(defcustom org-velocity-bucket ""
  "Where is the bucket file?"
  :group 'org-velocity
  :type 'file)

(defcustom org-velocity-search-is-incremental t
  "Show results incrementally when possible?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-show-previews t
  "Show previews of the text of each heading?"
  :group 'velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-exit-on-match nil
  "When searching incrementally, exit on a single match?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-force-new nil
  "Should exiting the minibuffer with C-j force a new entry?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-use-search-ring t
  "Push search to `search-ring' when visiting an entry?

This means that C-s C-s will take you directly to the first
instance of the search string."
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-always-use-bucket nil
  "Use bucket file even when called from an Org buffer?"
  :group 'org-velocity
  :type 'boolean
  :safe 'booleanp)

(defcustom org-velocity-use-completion nil
  "Use completion?

Notwithstanding the value of this option, calling
`dabbrev-expand' always completes against the text of the bucket
file."
  :group 'org-velocity
  :type '(choice
          (const :tag "Do not use completion" nil)
          (const :tag "Use completion" t))
  :safe 'booleanp)

(defcustom org-velocity-search-method 'phrase
  "Match on whole phrase, any word, or all words?"
  :group 'org-velocity
  :type '(choice
	  (const :tag "Match whole phrase" phrase)
	  (const :tag "Match any word" any)
	  (const :tag "Match all words" all)
          (const :tag "Match a regular expression" regexp))
  :safe (lambda (v) (memq v '(phrase any all regexp))))

(defcustom org-velocity-capture-templates
  '(("v"
     "Velocity entry"
     entry
     (file "")
     "* %:search\n\n%i%?"))
  "Use these template with `org-capture'.
Meanwhile `org-default-notes-file' is bound to `org-velocity-bucket-file'.
The keyword :search inserts the current search.
See the documentation for `org-capture-templates'."
  :group 'org-velocity
  :type (or (get 'org-capture-templates 'custom-type) 'list))

(defsubst org-velocity-grab-preview ()
  "Grab preview of a subtree.
The length of the preview is determined by `window-width'.

Replace all contiguous whitespace with single spaces."
  (let ((start (progn
                 (forward-line 1)
                 (if (looking-at org-property-start-re)
                     (re-search-forward org-property-end-re)
                   (1- (point))))))
    (mapconcat
     #'identity
     (split-string
      (buffer-substring-no-properties
       start
       (min
        (+ start (window-width))
        (point-max))))
     " ")))

(defstruct org-velocity-heading buffer position name level preview)

(defsubst org-velocity-nearest-heading (position)
  "Return last heading at POSITION.
If there is no last heading, return nil."
  (save-excursion
    (goto-char position)
    (re-search-backward org-velocity-heading-regexp)
    (let ((components (org-heading-components)))
      (make-org-velocity-heading
       :buffer (current-buffer)
       :position (point)
       :name (nth 4 components)
       :level (nth 0 components)
       :preview (if org-velocity-show-previews
                    (org-velocity-grab-preview))))))

(defconst org-velocity-index
  (eval-when-compile
    (nconc (number-sequence 49 57) 	;numbers
           (number-sequence 97 122)	;lowercase letters
           (number-sequence 65 90)))	;uppercase letters
  "List of chars for indexing results.")

(defconst org-velocity-match-buffer-name "*Velocity matches*")

(defconst org-velocity-heading-regexp "^\\* "
  "Regexp to match only top-level headings.")

(defvar org-velocity-search nil
  "Variable to bind to current search.")

(defun org-velocity-buffer-file-name (&optional buffer)
  "Return the name of the file BUFFER saves to.
Same as function `buffer-file-name' unless BUFFER is an indirect
buffer or a minibuffer. In the former case, return the file name
of the base buffer; in the latter, return the file name of
`minibuffer-selected-window' (or its base buffer)."
  (let ((buffer (if (minibufferp buffer)
                    (window-buffer (minibuffer-selected-window))
                  buffer)))
    (buffer-file-name
     (or (buffer-base-buffer buffer)
         buffer))))

(defun org-velocity-minibuffer-contents ()
  "Return the contents of the minibuffer when it is active."
  (if (active-minibuffer-window)
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (minibuffer-contents))))

(defsubst org-velocity-singlep (object)
  "Return t when OBJECT is a list or sequence of one element."
  (if (consp object)
      (null (cdr object))
    (= (length object) 1)))

(defun org-velocity-bucket-file ()
  "Return the proper file for Org-Velocity to search.
If `org-velocity-always-use-bucket' is t, use bucket file;
complain if missing. Otherwise, if an Org file is current, then
use it."
  (let ((org-velocity-bucket
         (when org-velocity-bucket (expand-file-name org-velocity-bucket)))
        (buffer
         (let ((buffer-file (org-velocity-buffer-file-name)))
           (when buffer-file
             ;; Use the target in capture buffers.
             (org-find-base-buffer-visiting buffer-file)))))
    (if org-velocity-always-use-bucket
        (or org-velocity-bucket (error "Bucket required but not defined"))
      (if (and (eq (buffer-local-value 'major-mode (or buffer (current-buffer)))
                   'org-mode)
               (org-velocity-buffer-file-name))
          (org-velocity-buffer-file-name)
        (or org-velocity-bucket
            (error "No bucket and not an Org file"))))))

(defvar org-velocity-bucket-buffer nil)

(defsubst org-velocity-bucket-buffer ()
  (or org-velocity-bucket-buffer
      (find-file-noselect (org-velocity-bucket-file))))

(defsubst org-velocity-match-buffer ()
  "Return the proper buffer for Org-Velocity to display in."
  (get-buffer-create org-velocity-match-buffer-name))

(defun org-velocity-beginning-of-headings ()
  "Goto the start of the first heading."
  (goto-char (point-min))
  ;; If we are before the first heading we could still be at the
  ;; first heading.
  (or (looking-at org-velocity-heading-regexp)
      (re-search-forward org-velocity-heading-regexp)))

(defun org-velocity-make-indirect-buffer (heading)
  "Make or switch to an indirect buffer visiting HEADING."

  (let* ((bucket (org-velocity-heading-buffer heading))
         (name (org-velocity-heading-name heading))
         (existing (get-buffer name)))
    (if (and existing (buffer-base-buffer existing)
             (equal (buffer-base-buffer existing) bucket))
        existing
      (make-indirect-buffer
       bucket
       (generate-new-buffer-name (org-velocity-heading-name heading))))))

(defun org-velocity-capture ()
  "Record a note with `org-capture'."
  (let ((org-capture-templates
         org-velocity-capture-templates))
    (org-capture nil
                 ;; This is no longer automatically selected.
                 (when (org-velocity-singlep org-capture-templates)
                   (caar org-capture-templates)))
    (if org-capture-mode (rename-buffer org-velocity-search t))))

(defvar org-velocity-saved-winconf nil)
(make-variable-buffer-local 'org-velocity-saved-winconf)

(defun org-velocity-edit-entry (heading)
  "Edit entry at HEADING in an indirect buffer."
  (let ((winconf (current-window-configuration)))
    (let ((buffer (org-velocity-make-indirect-buffer heading)))
      (with-current-buffer buffer
        (let ((org-inhibit-startup t))
          (org-mode))
        (setq org-velocity-saved-winconf winconf)
        (goto-char (org-velocity-heading-position heading))
        (narrow-to-region (point)
                          (save-excursion
                            (org-end-of-subtree t)
                            (point)))
        (goto-char (point-min))
        (add-hook 'org-ctrl-c-ctrl-c-hook 'org-velocity-dismiss nil t))
      (pop-to-buffer buffer)
      (set (make-local-variable 'header-line-format)
           (format "%s Use C-c C-c to finish."
                   (abbreviate-file-name
                    (buffer-file-name
                     (org-velocity-heading-buffer heading))))))))

(defun org-velocity-dismiss ()
  "Save current entry and close indirect buffer."
  (let ((winconf org-velocity-saved-winconf))
    (prog1 t                            ;Tell hook we're done.
      (save-buffer)
      (kill-buffer)
      (when (window-configuration-p winconf)
        (set-window-configuration winconf)))))

(defun org-velocity-visit-button (button)
  (run-hooks 'mouse-leave-buffer-hook)
  (if org-velocity-use-search-ring
      (add-to-history 'search-ring
                      (button-get button 'search)
                      search-ring-max))
  (org-velocity-edit-entry (button-get button 'match)))

(define-button-type 'org-velocity-button
  'action #'org-velocity-visit-button)

(defsubst org-velocity-buttonize (heading)
  "Insert HEADING as a text button with no hints."
  (insert-text-button
   (propertize (org-velocity-heading-name heading) 'face 'link)
   :type 'org-velocity-button
   'match heading
   'search org-velocity-search))

(defsubst org-velocity-insert-preview (heading)
  (when org-velocity-show-previews
    (insert-char ?\  1)
    (insert
     (propertize
      (org-velocity-heading-preview heading)
      'face 'shadow))))

(defsubst* org-velocity-present-match (&key hint match)
  (with-current-buffer (org-velocity-match-buffer)
    (when hint (insert "#" hint " "))
    (org-velocity-buttonize match)
    (org-velocity-insert-preview match)
    (newline)))

(defun org-velocity-generic-search (search &optional hide-hints)
  "Display any entry containing SEARCH."
  (let ((hints org-velocity-index) matches)
    (block nil
      (while (and hints (re-search-forward search nil t))
        (let ((match (org-velocity-nearest-heading (point))))
          (org-velocity-present-match
           :hint (unless hide-hints (car hints))
           :match match)
          (push match matches))
        (setq hints (cdr hints))
        (unless (re-search-forward org-velocity-heading-regexp nil t)
          (return))))
    (nreverse matches)))

(defun* org-velocity-all-search (search &optional hide-hints max)
  "Display only entries containing every word in SEARCH."
  (let ((keywords (mapcar 'regexp-quote (split-string search)))
        (hints org-velocity-index)
        matches)
    (org-map-entries
     (lambda ()
       ;; Return if we've run out of hints.
       (when (null hints)
         (return-from org-velocity-all-search (nreverse matches)))
       ;; Only search the subtree once.
       (setq org-map-continue-from
             (save-excursion
               (goto-char (line-end-position))
               (if (re-search-forward org-velocity-heading-regexp nil t)
                   (line-end-position)
                 (point-max))))
       (when (loop for word in keywords
                   always (save-excursion
                            (re-search-forward
                             (concat "\\<" word "\\>")
                             org-map-continue-from t)))
         (let ((match (org-velocity-nearest-heading (match-end 0))))
           (org-velocity-present-match
            :hint (unless hide-hints (car hints))
            :match match)
           (push match matches)
           (setq hints (cdr hints))))))
    (nreverse matches)))

(defun* org-velocity-present (search &key hide-hints)
  "Buttonize matches for SEARCH in `org-velocity-match-buffer'.
If HIDE-HINTS is non-nil, display entries without indices. SEARCH
binds `org-velocity-search'.

Return matches."
  (if (and (stringp search) (not (string= "" search)))
      ;; Fold case when the search string is all lowercase.
      (let ((case-fold-search (equal search (downcase search)))
            (truncate-partial-width-windows t))
        (with-current-buffer (org-velocity-match-buffer)
          (erase-buffer)
          ;; Permanent locals.
          (setq cursor-type nil
                truncate-lines t))
        (prog1
            (with-current-buffer (org-velocity-bucket-buffer)
              (let ((inhibit-point-motion-hooks t)
                    (inhibit-field-text-motion t))
                (save-excursion
                  (org-velocity-beginning-of-headings)
                  (case org-velocity-search-method
                    (all (org-velocity-all-search search hide-hints))
                    (phrase (org-velocity-generic-search
                             (concat "\\<" (regexp-quote search))
                             hide-hints))
                    (any (org-velocity-generic-search
                          (concat "\\<"
                                  (regexp-opt (split-string search)))
                          hide-hints))
                    (regexp (condition-case lossage
                                (org-velocity-generic-search
                                 search hide-hints)
                              (invalid-regexp
                               (minibuffer-message "%s" lossage))))))))
          (with-current-buffer (org-velocity-match-buffer)
            (goto-char (point-min)))))
    (with-current-buffer (org-velocity-match-buffer)
      (erase-buffer))))

(defun org-velocity-store-link ()
  "Function for `org-store-link-functions'."
  (if org-velocity-search
      (org-store-link-props
       :search org-velocity-search)))

(add-hook 'org-store-link-functions 'org-velocity-store-link)

(defun* org-velocity-create (search &key ask)
  "Create new heading named SEARCH.
If ASK is non-nil, ask first."
  (when (or (null ask) (y-or-n-p "No match found, create? "))
    (let ((org-velocity-search search)
	  (org-default-notes-file (org-velocity-bucket-file))
	  ;; save a stored link
	  org-store-link-plist)
      (org-velocity-capture))
    search))

(defun org-velocity-engine (search)
  "Display a list of headings where SEARCH occurs."
  (let ((org-velocity-search search))
    (unless (or
             (not (stringp search))
             (string= "" search))	;exit on empty string
      (case
          (if (and org-velocity-force-new (eq last-command-event ?\C-j))
              :force
            (let ((matches (org-velocity-present search)))
              (cond ((null matches) :new)
                    ((org-velocity-singlep matches) :follow)
                    (t :prompt))))
        (:prompt (progn
                   (pop-to-buffer (org-velocity-match-buffer))
                   (let ((hint (org-velocity-electric-read-hint)))
                     (when hint (case hint
                                  (:edit (org-velocity-read nil search))
                                  (:force (org-velocity-create search))
                                  (otherwise (org-velocity-activate-button hint)))))))
        (:new (unless (org-velocity-create search :ask t)
                (org-velocity-read nil search)))
        (:force (org-velocity-create search))
        (:follow (if (y-or-n-p "One match, follow? ")
                     (progn
                       (set-buffer (org-velocity-match-buffer))
                       (goto-char (point-min))
                       (button-activate (next-button (point))))
                   (org-velocity-read nil search)))))))

(defun org-velocity-position (item list)
  "Return first position of ITEM in LIST."
  (loop for elt in list
        for i from 0
        when (equal elt item)
        return i))

(defun org-velocity-activate-button (char)
  "Go to button on line number associated with CHAR in `org-velocity-index'."
  (goto-char (point-min))
  (forward-line (org-velocity-position char org-velocity-index))
  (goto-char
   (button-start
    (next-button (point))))
  (message "%s" (button-label (button-at (point))))
  (button-activate (button-at (point))))

(defun org-velocity-electric-undefined ()
  "Complain about an undefined key."
  (interactive)
  (message "%s"
	   (substitute-command-keys
	    "\\[org-velocity-electric-new] for new entry,
\\[org-velocity-electric-edit] to edit search,
\\[scroll-up] to scroll up,
\\[scroll-down] to scroll down,
\\[keyboard-quit] to quit."))
  (sit-for 4))

(defun org-velocity-electric-follow (ev)
  "Follow a hint indexed by keyboard event EV."
  (interactive (list last-command-event))
  (if (not (> (org-velocity-position ev org-velocity-index)
              (1- (count-lines (point-min) (point-max)))))
      (throw 'org-velocity-select ev)
    (call-interactively 'org-velocity-electric-undefined)))

(defun org-velocity-electric-click (ev)
  "Follow hint indexed by a mouse event EV."
  (interactive "e")
  (throw 'org-velocity-select
	 (nth (1- (count-lines
		   (point-min)
		   (posn-point (event-start ev))))
	      org-velocity-index)))

(defun org-velocity-electric-edit ()
  "Edit the search string."
  (interactive)
  (throw 'org-velocity-select :edit))

(defun org-velocity-electric-new ()
  "Force a new entry."
  (interactive)
  (throw 'org-velocity-select :force))

(defvar org-velocity-electric-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'org-velocity-electric-undefined)
    (loop for c in org-velocity-index
	  do (define-key map (char-to-string c) 'org-velocity-electric-follow))
    (define-key map "0" 'org-velocity-electric-new)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\M-v" 'scroll-down)
    (define-key map (kbd "RET") 'org-velocity-electric-edit)
    (define-key map [mouse-1] 'org-velocity-electric-click)
    (define-key map [mouse-2] 'org-velocity-electric-click)
    (define-key map [escape] 'keyboard-quit)
    (define-key map "\C-h" 'help-command)
    map))

(defun org-velocity-electric-read-hint ()
  "Read index of button electrically."
  (with-current-buffer (org-velocity-match-buffer)
    (use-local-map org-velocity-electric-map)
    (catch 'org-velocity-select
      (Electric-command-loop 'org-velocity-select "Follow: "))))

(defvar org-velocity-incremental-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'org-velocity-click-for-incremental)
    (define-key map [mouse-2] 'org-velocity-click-for-incremental)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\M-v" 'scroll-down)
    map))

(defun org-velocity-click-for-incremental ()
  "Jump out of search and select hint clicked on."
  (interactive)
  (let ((ev last-command-event))
    (org-velocity-activate-button
     (nth (- (count-lines
              (point-min)
              (posn-point (event-start ev))) 2)
          org-velocity-index)))
  (throw 'click (current-buffer)))

(defun org-velocity-displaying-completions-p ()
  "Is there a *Completions* buffer showing?"
  (get-window-with-predicate
   (lambda (w)
     (eq (buffer-local-value 'major-mode (window-buffer w))
         'completion-list-mode))))

(defun org-velocity-update ()
  "Display results of search without hinting.
Stop searching once there are more matches than can be displayed."
  (unless (org-velocity-displaying-completions-p)
    (let* ((search (org-velocity-minibuffer-contents))
           (matches (org-velocity-present search :hide-hints t)))
      (cond ((null matches)
             (select-window (active-minibuffer-window))
             (unless (or (null search) (string= "" search))
               (minibuffer-message "No match; RET to create")))
            ((and (org-velocity-singlep matches)
                  org-velocity-exit-on-match)
             (throw 'click search))
            (t
             (with-current-buffer (org-velocity-match-buffer)
               (use-local-map org-velocity-incremental-keymap)))))))

(defvar dabbrev--last-abbrev)

(defun org-velocity-dabbrev-completion-list (abbrev)
  "Return all dabbrev completions for ABBREV."
  ;; This is based on `dabbrev-completion'.
  (dabbrev--reset-global-variables)
  (setq dabbrev--last-abbrev abbrev)
  (dabbrev--find-all-expansions abbrev case-fold-search))

(defvar org-velocity-local-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map " " 'self-insert-command)
    (define-key map [remap minibuffer-complete] 'minibuffer-complete-word)
    map)
  "Keymap for completion with `completing-read'.")

(defun org-velocity-read-with-completion (prompt)
  "Completing read with PROMPT."
  (let ((minibuffer-local-completion-map
         org-velocity-local-completion-map)
        (completion-no-auto-exit t)
        (crm-separator " "))
    (funcall
     (case org-velocity-search-method
       (phrase #'completing-read)
       (any    #'completing-read-multiple)
       (all    #'completing-read-multiple))
     prompt
     (completion-table-dynamic
      'org-velocity-dabbrev-completion-list))))

(defun org-velocity-read-string (prompt &optional initial-input)
  "Read string with PROMPT followed by INITIAL-INPUT."
  ;; The use of initial inputs to the minibuffer is deprecated (see
  ;; `read-from-minibuffer'), but in this case it is the user-friendly
  ;; thing to do.
  (minibuffer-with-setup-hook
      (lexical-let ((initial-input initial-input))
        (lambda ()
          (and initial-input (insert initial-input))
          (goto-char (point-max))))
    (if (eq org-velocity-search-method 'regexp)
	(read-regexp prompt)
      (if org-velocity-use-completion
	  (org-velocity-read-with-completion prompt)
	(read-string prompt)))))

(defun org-velocity-incremental-read (prompt)
  "Read string with PROMPT and display results incrementally."
  (let ((res
         (unwind-protect
             (let* ((match-window (display-buffer (org-velocity-match-buffer)))
                    (org-velocity-index
                     ;; Truncate the index to the size of the buffer to be
                     ;; displayed.
                     (with-selected-window match-window
                       (if (> (window-height) (length org-velocity-index))
                           ;; (subseq org-velocity-index 0 (window-height))
                           (let ((hints (copy-sequence org-velocity-index)))
                             (setcdr (nthcdr (window-height) hints) nil)
                             hints)
                         org-velocity-index))))
               (catch 'click
                 (add-hook 'post-command-hook 'org-velocity-update)
                 (if (eq org-velocity-search-method 'regexp)
                     (read-regexp prompt)
                   (if org-velocity-use-completion
                       (org-velocity-read-with-completion prompt)
                     (read-string prompt)))))
           (remove-hook 'post-command-hook 'org-velocity-update))))
    (if (bufferp res) (org-pop-to-buffer-same-window res) res)))

(defun org-velocity (arg &optional search)
  "Read a search string SEARCH for Org-Velocity interface.
This means that a buffer will display all headings where SEARCH
occurs, where one can be selected by a mouse click or by typing
its index.  If SEARCH does not occur, then a new heading may be
created named SEARCH.

If `org-velocity-bucket' is defined and
`org-velocity-always-use-bucket' is non-nil, then the bucket file
will be used; otherwise, this will work when called in any Org
file. Calling with ARG forces current file."
  (interactive "P")
  (let ((org-velocity-always-use-bucket
	 (if arg nil org-velocity-always-use-bucket)))
    ;; complain if inappropriate
    (assert (org-velocity-bucket-file))
    (let ((org-velocity-bucket-buffer
           (find-file-noselect (org-velocity-bucket-file))))
      (unwind-protect
          (let ((dabbrev-search-these-buffers-only
                 (list (org-velocity-bucket-buffer))))
            (org-velocity-engine
             (if org-velocity-search-is-incremental
                 (org-velocity-incremental-read "Velocity search: ")
               (org-velocity-read-string "Velocity search: " search))))
        (progn
          (kill-buffer (org-velocity-match-buffer))
          (delete-other-windows))))))

(defalias 'org-velocity-read 'org-velocity)

(provide 'org-velocity)

;;; org-velocity.el ends here

;;; org-velocity.el --- something like Notational Velocity for Org. -*- lexical-binding: t -*-

;; Copyright (C) 2010-2014 Paul M. Rodriguez

;; Author: Paul M. Rodriguez <paulmrodriguez@gmail.com>
;; Created: 2010-05-05
;; Version: 4.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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
(require 'cl-lib)

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

(defcustom org-velocity-heading-level 1
  "Only match headings at this level or higher.
0 means to match headings at any level."
  :group 'org-velocity
  :type 'integer
  :safe (lambda (x)
          (and (integerp x)
               (>= x 0))))

(defvar crm-separator)                  ;Ensure dynamic binding.

(defsubst org-velocity-grab-preview ()
  "Grab preview of a subtree.
The length of the preview is determined by `window-width'.

Replace all contiguous whitespace with single spaces."
  (let* ((start (progn
                  (forward-line 1)
                  (if (looking-at org-property-start-re)
                      (re-search-forward org-property-end-re)
                    (1- (point)))))
         (string+props (buffer-substring
                        start
                        (min
                         (+ start (window-width))
                         (point-max)))))
    ;; We want to preserve the text properties so that, for example,
    ;; we don't end up with the raw text of links in the preview.
    (with-temp-buffer
      (insert string+props)
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward split-string-default-separators
                                  (point-max)
                                  t)
          (replace-match " ")))
      (buffer-string))))

(cl-defstruct org-velocity-heading buffer position name level preview)

(defsubst org-velocity-nearest-heading (position)
  "Return last heading at POSITION.
If there is no last heading, return nil."
  (save-excursion
    (goto-char position)
    (re-search-backward (org-velocity-heading-regexp))
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
    (nconc (number-sequence 49 57)      ;numbers
           (number-sequence 97 122)	;lowercase letters
           (number-sequence 65 90)))	;uppercase letters
  "List of chars for indexing results.")

(defconst org-velocity-match-buffer-name "*Velocity matches*")

(cl-defun org-velocity-heading-regexp (&optional (level org-velocity-heading-level))
  "Regexp to match headings at LEVEL or deeper."
  (if (zerop level)
      "^\\*+ "
    (format "^\\*\\{1,%d\\} " level)))

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
  (when (active-minibuffer-window)
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (minibuffer-contents))))

(defun org-velocity-nix-minibuffer ()
  "Return the contents of the minibuffer and clear it."
  (when (active-minibuffer-window)
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (prog1 (minibuffer-contents)
        (delete-minibuffer-contents)))))

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
(defvar org-velocity-navigating nil)

(defsubst org-velocity-bucket-buffer ()
  (or org-velocity-bucket-buffer
      (find-file-noselect (org-velocity-bucket-file))))

(defsubst org-velocity-match-buffer ()
  "Return the proper buffer for Org-Velocity to display in."
  (get-buffer-create org-velocity-match-buffer-name))

(defsubst org-velocity-match-window ()
  (get-buffer-window (org-velocity-match-buffer)))

(defun org-velocity-beginning-of-headings ()
  "Goto the start of the first heading."
  (goto-char (point-min))
  ;; If we are before the first heading we could still be at the
  ;; first heading.
  (or (looking-at (org-velocity-heading-regexp))
      (re-search-forward (org-velocity-heading-regexp))))

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
       (generate-new-buffer-name (org-velocity-heading-name heading))
       t))))

(defun org-velocity-capture ()
  "Record a note with `org-capture'."
  (let ((org-capture-templates
         org-velocity-capture-templates))
    (org-capture nil
                 ;; This is no longer automatically selected.
                 (when (null (cdr org-capture-templates))
                   (caar org-capture-templates)))
    (when org-capture-mode
      (rename-buffer org-velocity-search t))))

(defvar org-velocity-saved-winconf nil)
(make-variable-buffer-local 'org-velocity-saved-winconf)

(defun org-velocity-edit-entry (heading)
  (if org-velocity-navigating
      (org-velocity-edit-entry/inline heading)
    (org-velocity-edit-entry/indirect heading)))

(cl-defun org-velocity-goto-entry (heading &key narrow)
  (goto-char (org-velocity-heading-position heading))
  (save-excursion
    (when narrow
      (org-narrow-to-subtree))
    (outline-show-all)))

(defun org-velocity-edit-entry/inline (heading)
  "Edit entry at HEADING in the original buffer."
  (let ((buffer (org-velocity-heading-buffer heading)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (org-velocity-goto-entry heading))))

(defun org-velocity-format-header-line (control-string &rest args)
  (set (make-local-variable 'header-line-format)
       (apply #'format control-string args)))

(defun org-velocity-edit-entry/indirect (heading)
  "Edit entry at HEADING in an indirect buffer."
  (let ((winconf (current-window-configuration))
        (dd default-directory)
        (buffer (org-velocity-make-indirect-buffer heading))
        (inhibit-point-motion-hooks t)
        (inhibit-field-text-motion t))
    (with-current-buffer buffer
      (setq default-directory dd)       ;Inherit default directory.
      (setq org-velocity-saved-winconf winconf)
      (org-velocity-goto-entry heading :narrow t)
      (goto-char (point-max))
      (add-hook 'org-ctrl-c-ctrl-c-hook 'org-velocity-dismiss nil t))
    (pop-to-buffer buffer)
    (org-velocity-format-header-line
     "%s Use C-c C-c to finish."
     (abbreviate-file-name
      (buffer-file-name
       (org-velocity-heading-buffer heading))))))

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
  (when org-velocity-use-search-ring
    (add-to-history 'search-ring
                    (button-get button 'search)
                    search-ring-max))
  (let ((match (button-get button 'match)))
    (throw 'org-velocity-done match)))

(define-button-type 'org-velocity-button
  'action #'org-velocity-visit-button
  'follow-link 'mouse-face)

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

(defvar org-velocity-recursive-headings nil)
(defvar org-velocity-recursive-search nil)

(cl-defun org-velocity-search-with (fun style search
                                        &key (headings org-velocity-recursive-headings))
  (if headings
      (save-restriction
        (dolist (heading headings)
          (widen)
          (let ((start (org-velocity-heading-position heading)))
            (goto-char start)
            (let ((end (save-excursion
                         (org-end-of-subtree)
                         (point))))
              (narrow-to-region start end)
              (org-velocity-search-with fun style search
                                        :headings nil)))))
    (cl-ecase style
      ((phrase any regexp)
       (cl-block nil
         (while (re-search-forward search nil t)
           (let ((match (org-velocity-nearest-heading (point))))
             (funcall fun match))
           ;; Skip to the next heading.
           (unless (re-search-forward (org-velocity-heading-regexp) nil t)
             (cl-return)))))
      ((all)
       (let ((keywords
              (cl-loop for word in (split-string search)
                       collect (concat "\\<" (regexp-quote word) "\\>"))))
         (org-map-entries
          (lambda ()
            ;; Only search the subtree once.
            (setq org-map-continue-from
                  (save-excursion
                    (org-end-of-subtree)
                    (point)))
            (when (cl-loop for word in keywords
                           always (save-excursion
                                    (re-search-forward word org-map-continue-from t)))
              (let ((match (org-velocity-nearest-heading (match-end 0))))
                (funcall fun match))))))))))

(defun org-velocity-all-results (style search)
  (with-current-buffer (org-velocity-bucket-buffer)
    (save-excursion
      (goto-char (point-min))
      (let (matches)
        (org-velocity-search-with (lambda (match)
                                    (push match matches))
                                  style
                                  search)
        (nreverse matches)))))

(defsubst org-velocity-present-match (hint match)
  (with-current-buffer (org-velocity-match-buffer)
    (when hint (insert "#" hint " "))
    (org-velocity-buttonize match)
    (org-velocity-insert-preview match)
    (newline)))

(defun org-velocity-present-search (style search hide-hints)
  (let ((hints org-velocity-index) matches)
    (cl-block nil
      (org-velocity-search-with (lambda (match)
                                  (unless hints
                                    (cl-return))
                                  (let ((hint (if hide-hints
                                                  nil
                                                (car hints))))
                                    (org-velocity-present-match hint match))
                                  (pop hints)
                                  (push match matches))
                                style
                                search))
    (nreverse matches)))

(defun org-velocity-restrict-search ()
  (interactive)
  (let ((search (org-velocity-nix-minibuffer)))
    (when (equal search "")
      (error "No search to restrict to"))
    (push search org-velocity-recursive-search)
    (setq org-velocity-recursive-headings
          (org-velocity-all-results
           org-velocity-search-method
           search))
    ;; TODO We could extend the current search instead of starting
    ;; over.
    (org-velocity-update-match-header)
    (minibuffer-message "Restricting search to %s" search)))

(cl-defun org-velocity-update-match-header (&key (match-buffer (org-velocity-match-buffer))
                                                 (bucket-buffer (org-velocity-bucket-buffer))
                                                 (search-method org-velocity-search-method))
  (let ((navigating? org-velocity-navigating)
        (recursive? org-velocity-recursive-search))
    (with-current-buffer match-buffer
      (org-velocity-format-header-line
       "%s search in %s%s (%s mode)"
       (capitalize (symbol-name search-method))
       (abbreviate-file-name (buffer-file-name bucket-buffer))
       (if (not recursive?)
           ""
         (let ((sep " > "))
           (concat sep (string-join (reverse recursive?) sep))))
       (if navigating? "nav" "notes")))))

(cl-defun org-velocity-present (search &key hide-hints)
  "Buttonize matches for SEARCH in `org-velocity-match-buffer'.
If HIDE-HINTS is non-nil, display entries without indices. SEARCH
binds `org-velocity-search'.

Return matches."
  (let ((match-buffer (org-velocity-match-buffer))
        (bucket-buffer (org-velocity-bucket-buffer))
        (search-method org-velocity-search-method))
    (if (and (stringp search) (not (string= "" search)))
        ;; Fold case when the search string is all lowercase.
        (let ((case-fold-search (equal search (downcase search)))
              (truncate-partial-width-windows t))
          (with-current-buffer match-buffer
            (erase-buffer)
            ;; Permanent locals.
            (setq cursor-type nil
                  truncate-lines t)
            (org-velocity-update-match-header
             :match-buffer match-buffer
             :bucket-buffer bucket-buffer
             :search-method search-method))
          (prog1
              (with-current-buffer bucket-buffer
                (widen)
                (let* ((inhibit-point-motion-hooks t)
                       (inhibit-field-text-motion t)
                       (anchored? (string-match-p "^\\s-" search))
                       (search
                        (cl-ecase search-method
                          (all search)
                          (phrase
                           (if anchored?
                               (regexp-quote search)
                             ;; Anchor the search to the start of a word.
                             (concat "\\<" (regexp-quote search))))
                          (any
                           (concat "\\<" (regexp-opt (split-string search))))
                          (regexp search))))
                  (save-excursion
                    (org-velocity-beginning-of-headings)
                    (condition-case lossage
                        (org-velocity-present-search search-method search hide-hints)
                      (invalid-regexp
                       (minibuffer-message "%s" lossage))))))
            (with-current-buffer match-buffer
              (goto-char (point-min)))))
      (with-current-buffer match-buffer
        (erase-buffer)))))

(defun org-velocity-store-link ()
  "Function for `org-store-link-functions'."
  (if org-velocity-search
      (org-store-link-props
       :search org-velocity-search)))

(add-hook 'org-store-link-functions 'org-velocity-store-link)

(cl-defun org-velocity-create (search &key ask)
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
      (cl-case
          (if (and org-velocity-force-new (eq last-command-event ?\C-j))
              :force
            (let* ((org-velocity-index (org-velocity-adjust-index))
                   (matches (org-velocity-present search)))
              (cond ((null matches) :new)
                    ((null (cdr matches)) :follow)
                    (t :prompt))))
        (:prompt (progn
                   (pop-to-buffer (org-velocity-match-buffer))
                   (let ((hint (org-velocity-electric-read-hint)))
                     (when hint (cl-case hint
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

(defun org-velocity-activate-button (char)
  "Go to button on line number associated with CHAR in `org-velocity-index'."
  (goto-char (point-min))
  (forward-line (cl-position char org-velocity-index))
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
  (if (not (> (cl-position ev org-velocity-index)
              (1- (count-lines (point-min) (point-max)))))
      (throw 'org-velocity-select ev)
    (call-interactively 'org-velocity-electric-undefined)))

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
    (dolist (c org-velocity-index)
      (define-key map (char-to-string c)
        'org-velocity-electric-follow))
    (define-key map "0" 'org-velocity-electric-new)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\M-v" 'scroll-down)
    (define-key map (kbd "RET") 'org-velocity-electric-edit)
    (define-key map [mouse-1] nil)
    (define-key map [mouse-2] nil)
    (define-key map [escape] 'keyboard-quit)
    (define-key map "\C-h" 'help-command)
    map))

(defun org-velocity-electric-read-hint ()
  "Read index of button electrically."
  (with-current-buffer (org-velocity-match-buffer)
    (when (featurep 'evil)
      ;; NB Idempotent.
      (evil-make-overriding-map org-velocity-electric-map))
    (use-local-map org-velocity-electric-map)
    (catch 'org-velocity-select
      (Electric-command-loop 'org-velocity-select "Follow: "))))

(defvar org-velocity-incremental-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\M-v" 'scroll-down)
    map))

(defun org-velocity-displaying-completions-p ()
  "Is there a *Completions* buffer showing?"
  (get-window-with-predicate
   (lambda (w)
     (eq (buffer-local-value 'major-mode (window-buffer w))
         'completion-list-mode))))

(defun org-velocity-update ()
  "Display results of search without hinting."
  (unless (org-velocity-displaying-completions-p)
    (let* ((search (org-velocity-minibuffer-contents))
           (matches (org-velocity-present search :hide-hints t)))
      (cond ((null matches)
             (select-window (active-minibuffer-window))
             (unless (or (null search) (= (length search) 0))
               (minibuffer-message "No match; RET to create")))
            ((and (null (cdr matches))
                  org-velocity-exit-on-match)
             (throw 'click search))
            (t
             (with-current-buffer (org-velocity-match-buffer)
               (use-local-map org-velocity-incremental-keymap)))))))

(defvar dabbrev--last-abbreviation)

(defun org-velocity-dabbrev-completion-list (abbrev)
  "Return all dabbrev completions for ABBREV."
  ;; This is based on `dabbrev-completion'.
  (dabbrev--reset-global-variables)
  (setq dabbrev--last-abbreviation abbrev)
  (dabbrev--find-all-expansions abbrev case-fold-search))

(defvar org-velocity-local-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map " " 'self-insert-command)
    (define-key map "?" 'self-insert-command)
    (define-key map [remap minibuffer-complete] 'minibuffer-complete-word)
    (define-key map [(control ?@)] 'org-velocity-restrict-search)
    (define-key map [(control ?\s)] 'org-velocity-restrict-search)
    map)
  "Keymap for completion with `completing-read'.")

(defun org-velocity-read-with-completion (prompt)
  "Completing read with PROMPT."
  (let ((minibuffer-local-completion-map
         org-velocity-local-completion-map)
        (completion-no-auto-exit t)
        (crm-separator " "))
    (completing-read prompt
                     (completion-table-dynamic
                      'org-velocity-dabbrev-completion-list))))

(cl-defun org-velocity-adjust-index
    (&optional (match-window (org-velocity-match-window)))
  "Truncate or extend `org-velocity-index' to the lines in
MATCH-WINDOW."
  (with-selected-window match-window
    (let ((lines (window-height))
          (hints (length org-velocity-index)))
      (cond ((= lines hints)
             org-velocity-index)
            ;; Truncate the index to the size of
            ;; the buffer to be displayed.
            ((< lines hints)
             (cl-subseq org-velocity-index 0 lines))
            ;; If the window is so tall we run out of indices, at
            ;; least make the additional results clickable.
            ((> lines hints)
             (append org-velocity-index
                     (make-list (- lines hints) nil)))))))

(defun org-velocity-incremental-read (prompt)
  "Read string with PROMPT and display results incrementally.
Stop searching once there are more matches than can be
displayed."
  (let ((res
         (unwind-protect
             (let* ((match-window (display-buffer (org-velocity-match-buffer)))
                    (org-velocity-index (org-velocity-adjust-index match-window)))
               (catch 'click
                 (add-hook 'post-command-hook 'org-velocity-update)
                 (cond ((eq org-velocity-search-method 'regexp)
                        (read-regexp prompt))
                       (org-velocity-use-completion
                        (org-velocity-read-with-completion prompt))
                       (t (read-string prompt)))))
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
file.

Calling with ARG reverses which file – the current file or the
bucket file – to use. If the bucket file would have been used,
then the current file is used instead, and vice versa."
  (interactive "P")
  (let ((org-velocity-always-use-bucket
         (if org-velocity-always-use-bucket
             (not arg)
           arg)))
    ;; complain if inappropriate
    (cl-assert (org-velocity-bucket-file))
    (let* ((starting-buffer (current-buffer))
           (org-velocity-bucket-buffer
            (find-file-noselect (org-velocity-bucket-file)))
           (org-velocity-navigating
            (eq starting-buffer org-velocity-bucket-buffer))
           (org-velocity-recursive-headings '())
           (org-velocity-recursive-search '())
           (org-velocity-heading-level
            (if org-velocity-navigating
                0
              org-velocity-heading-level))
           (dabbrev-search-these-buffers-only
            (list org-velocity-bucket-buffer)))
      (unwind-protect
          (let ((match
                 (catch 'org-velocity-done
                   (org-velocity-engine
                    (or search
                        (org-velocity-incremental-read "Velocity search: ")))
                   nil)))
            (when (org-velocity-heading-p match)
              (org-velocity-edit-entry match)))
        (kill-buffer (org-velocity-match-buffer))))))

(defalias 'org-velocity-read 'org-velocity)

(provide 'org-velocity)

;;; org-velocity.el ends here

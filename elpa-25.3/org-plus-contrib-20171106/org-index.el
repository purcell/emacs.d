;;; org-index.el --- A personal adaptive index for org  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Marc Ihm <org-index@2484.de>
;; Version: 5.5.0
;; Keywords: outlines index

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;;  Fast search for selected org nodes and things outside of org.
;;
;;  org-index creates and updates an index table with keywords; each line
;;  either points to a heading in org, references something outside or
;;  carries a snippet of text to yank.  When searching the index, the set
;;  of matching lines is updated with every keystroke; results are sorted
;;  by usage count and date, so that frequently used entries appear first
;;  in the list of results.
;;
;;  References are decorated numbers (e.g. 'R237' or '--455--'); they are
;;  well suited to be used outside of org, e.g. in folder names, ticket
;;  systems or on printed documents.
;;
;;  On first invocation org-index will assist you in creating the index
;;  table.
;;
;;  To start using your index, invoke subcommands 'add', 'ref' and 'yank'
;;  to create entries and 'occur' to find them.
;;
;;
;; Setup:
;;
;;  - Place this file in a directory of your load-path,
;;    e.g. org-mode/contrib/lisp.
;;
;;  - Add these lines to your .emacs:
;;
;;    (require 'org-index)
;;
;;  - Restart your Emacs to make this effective.
;;
;;  - Invoke `org-index'; on first run it will assist in creating your
;;    index table.
;;
;;  - Optionally invoke `M-x org-customize', group 'Org Index', to tune
;;    some settings, e.g. the global prefix key 'C-c i'.
;;
;;
;; Further information:
;;
;;  - Watch the screencast at http://2484.de/org-index.html.
;;
;;  - See the documentation of `org-index', which can also be read by
;;    invoking `org-index' and choosing the command help or '?'.
;;
;;
;; Updates:
;;
;;  The latest published version of this file can always be found at:
;;
;;    http://orgmode.org/w/?p=org-mode.git;a=blob_plain;f=contrib/lisp/org-index.el;hb=HEAD
;;
;;  Development version under:
;;
;;    https://github.com/marcIhm/org-index

;;; Change Log:

;;   [2017-09-03 So] Version 5.5.0
;;   - In occur: case-sensitive search for upcase letters
;;   - Better handling of nested focus nodes
;;   - Bugfixes
;;
;;   [2017-06-06 Tu] Version 5.4.2
;;   - Dedicated submenu for focus operations
;;   - Occur accepts a numeric argument as a day span
;;   - New customization `org-index-clock-into-focus'
;;   - Fixed delay after choosing an index line
;;   - (Re)introduced lexical binding
;;   - Bugfixes
;;
;;   [2017-03-26 Su] Version 5.3.0
;;   - Focused can now be on a list of nodes (instead of a single one)
;;   - Cleaned up undeclared dependencies
;;
;;   [2017-02-18 Sa] Version 5.2.3
;;   - New command 'focus'
;;   - Speeded up org-index--parse-table with the stored property "max-ref"
;;   - Speeded up org-index--on with search
;;   - Added org-index-prepare-when-idle
;;   - Fixed compatibility issue with emacs 24 (font-lock-ensure)
;;   - Added more customizations
;;   - Bugfixes
;;
;;   [2016-10-19 We] Version 5.1.4
;;   - Bugfixes
;;
;;   [2016-08-26 Fr] Version 5.1.3
;;   - Offering help during query for subcommands
;;   - Removed org-index-default-keybindings
;;   - Renamed subcommand multi-occur to find-ref
;;   - Subcommands add needs no longer be invoked from heading
;;   - Many Bugfixes
;;
;;   [2015-12-29 Tu] Version 5.0.2
;;   - New commands yank, column and edit
;;   - New column tags
;;   - All columns are now required
;;   - References are now optional
;;   - Subcommand enter has been renamed to index
;;   - Subcommands kill and edit can be invoked from an occur buffer
;;   - Many Bugfixes
;;   - Added link to screencast
;;
;;   [2015-08-20 Th] Version 4.3.0
;;   - Configuration is done now via standard customize
;;   - New sorting strategy 'mixed'
;;   - Silenced some compiler warnings
;;
;;   [2015-03-18 We] Version 4.2.1
;;   - No garbage in kill-ring
;;   - No recentering after add
;;
;;   [2015-03-08 Su] Version 4.2.0
;;   - Reference numbers for subcommands can be passed as a prefix argument
;;   - New variable org-index-default-keybindings-list with a list of
;;     default keybindings for org-index-default-keybindings
;;   - Added new column level
;;   - removed flags get-category-on-add and get-heading-on-add
;;
;;   [2015-02-26 Th] to [2015-03-05 Th] Version 4.0.0 to 4.1.2
;;   - Removed command "leave"; rather go back with org-mark-ring-goto
;;   - Renamed column "link" to "id"
;;   - Added maintainance options to find duplicate rows, to check ids,
;;     update index or remove property org-index-ref from nodes
;;   - Shortened versin history
;;
;;   [2014-12-08 Mo] to [2015-01-31 Sa] Version 3.0.0 to 3.2.0:
;;   - Complete sorting of index only occurs in idle-timer
;;   - New command "maintain"  with some subcommands
;;   - Rewrote command "occur" with overlays in an indirect buffer
;;   - Command "add" updates index, if node is already present
;;   - New commands "add" and "delete" to easily add and remove
;;     the current node to or from your index.
;;   - New command "example" to create an example index.
;;   - Several new flags that are explained within index node.
;;   - Removed commands "reuse", "missing", "put", "goto",
;;     "update", "link", "fill", "unhighlight"
;;   - New function `org-index-default-keybindings'
;;
;;   [2012-12-07 Fr] to [2014-04-26 Sa] Version 2.0.0 to 2.4.3:
;;   - New functions org-index-new-line and org-index-get-line
;;     offer access to org-index from other lisp programs
;;   - Regression tests with ert
;;   - Renamed from "org-favtable" to "org-index"
;;   - Added an assistant to set up the index table
;;   - occur is now incremental, searching as you type
;;   - Integrated with org-mark-ring-goto
;;   - Added full support for ids
;;   - Renamed the package from "org-reftable" to "org-favtable"
;;   - Additional columns are required (e.g. "link"). Error messages will
;;     guide you
;;   - Ask user explicitly, which command to invoke
;;   - Renamed the package from "org-refer-by-number" to "org-reftable"
;;
;;   [2011-12-10 Sa] to [2012-09-22 Sa] Version Version 1.2.0 to 1.5.0:
;;    - New command "sort" to sort a buffer or region by reference number
;;    - New commands "highlight" and "unhighlight" to mark references
;;    - New command "head" to find a headline with a reference number
;;    - New commands occur and multi-occur
;;    - Started this Change Log

;;; Code:

(require 'org-table)
(require 'org-id)
(require 'cl-lib)
(require 'widget)

;; Version of this package
(defvar org-index-version "5.5.0" "Version of `org-index', format is major.minor.bugfix, where \"major\" are incompatible changes and \"minor\" are new features.")

;; customizable options
(defgroup org-index nil
  "Options concerning the optional index for org."
  :tag "Org Index"
  :group 'org)

(defcustom org-index-id nil
  "Id of the Org-mode node, which contains the index table."
  :group 'org-index)

(defcustom org-index-sort-by 'mixed
  "Strategy for sorting index table (and whence entries in occur).
Valid values are:

last-access  Sort index by date and time of last access; show
             more recent entries first.
count  Sort by usage count; more often used entries first.
mixed  First, show all index entries, which have been
       used today; sort them by last access.  Then show
       older entries sorted by usage count."
  :group 'org-index
  :set (lambda (s v)
         (set-default s v)
         (if (and org-index-id
                  org-index--buffer
                  (functionp 'org-index--sort-silent))
             (org-index--sort-silent)))
  :initialize 'custom-initialize-default
  :type '(choice
	  (const last-accessed)
	  (const count)
	  (const mixed)))

(defcustom org-index-dispatch-key "i"
  "Key to invoke ‘org-index-dispatch’, which is the central entry function for ‘org-index’."
  :group 'org-index
  :initialize 'custom-initialize-set
  :set (lambda (var val)
         (set-default var val)
         (global-set-key org-index-dispatch-key 'org-index-dispatch))
  :type 'key-sequence)

(defcustom org-index-idle-delay 68
  "Delay in seconds after which buffer will sorted or fontified when Emacs is idle."
  :group 'org-index
  :type 'integer)

(defcustom org-index-prepare-when-idle nil
  "Fontify and sort index-table when idle to make first call faster.
You only need this if your index has grown so large, that first
invocation of `org-index' needs a noticable amount of time."
  :group 'org-index
  :initialize 'custom-initialize-set
  :set (lambda (var val)
         (set-default var val)
         (when val
           (setq org-index--align-interactive 200)
           (run-with-idle-timer org-index-idle-delay nil 'org-index--idle-prepare)))
  :type 'boolean)

(defcustom org-index-yank-after-add 'ref
  "Specifies which column should be yanked after adding a new index row.
Valid values are some columns of index table."
  :group 'org-index
  :type '(choice
	  (const ref)
	  (const category)
	  (const keywords)))

(defcustom org-index-copy-heading-to-keywords t
  "When adding a new node to index: Copy heading to keywords-column ?"
  :group 'org-index
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom org-index-strip-ref-and-date-from-heading t
  "When adding a node to index: strip leading ref or timestamps ?

This can be useful, if you have the habit of adding refs and
dates to the start of your headings; then, if you change your
heading and want to update your index, you do not need to remove
those pieces."
  :group 'org-index
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom org-index-edit-on-add '(category keywords)
  "List of columns to edit when adding a new row."
  :group 'org-index
  :type '(repeat (choice
                  (const category)
                  (const keywords))))

(defcustom org-index-edit-on-yank '(keywords yank)
  "List of columns to edit when adding new text to yank."
  :group 'org-index
  :type '(repeat (choice
                  (const yank)
                  (const category)
                  (const keywords))))

(defcustom org-index-edit-on-ref '(category keywords)
  "List of columns to edit when adding new ref."
  :group 'org-index
  :type '(repeat (choice
                  (const category)
                  (const keywords))))

(defcustom org-index-clock-into-focus nil
  "Clock into focused node."
  :group 'org-index
  :type 'boolean)

;; Variables to hold the configuration of the index table
(defvar org-index--head nil "Header before number (e.g. 'R').")
(defvar org-index--tail nil "Tail after number (e.g. '}' or ')'.")
(defvar org-index--numcols nil "Number of columns in index table.")
(defvar org-index--ref-regex nil "Regular expression to match a reference.")
(defvar org-index--ref-format nil "Format, that can print a reference.")
(defvar org-index--columns nil "Columns of index-table.")
(defvar org-index--buffer nil "Buffer of index table.")
(defvar org-index--point nil "Position at start of headline of index table.")
(defvar org-index--below-hline nil "Position of first cell in first line below hline.")
(defvar org-index--saved-positions nil "Saved positions within current buffer and index buffer; filled by ‘org-index--save-positions’.")
(defvar org-index--headings nil "Headlines of index-table as a string.")
(defvar org-index--headings-visible nil "Visible part of headlines of index-table as a string.")
(defvar org-index--ids-focused-nodes nil "Ids of focused node (if any).")
(defvar org-index--id-last-goto-focus nil "Id of last node, that has been focused to.")

;; Variables to hold context and state
(defvar org-index--last-fingerprint nil "Fingerprint of last line created.")
(defvar org-index--category-before nil "Category of node before.")
(defvar org-index--active-region nil "Active region, initially.  I.e. what has been marked.")
(defvar org-index--below-cursor nil "Word below cursor.")
(defvar org-index--within-index-node nil "True, if we are within node of the index table.")
(defvar org-index--within-occur nil "True, if we are within the occur-buffer.")
(defvar org-index--message-text nil "Text that was issued as an explanation; helpful for regression tests.")
(defvar org-index--occur-help-text nil "Text for help in occur buffer.")
(defvar org-index--occur-help-overlay nil "Overlay for help in occur buffer.")
(defvar org-index--occur-stack nil "Stack with overlays for hiding lines.")
(defvar org-index--occur-tail-overlay nil "Overlay to cover invisible lines.")
(defvar org-index--occur-lines-collected 0 "Number of lines collected in occur buffer; helpful for tests.")
(defvar org-index--last-sort-assumed nil "Last column, the index has been sorted after (best guess).")
(defvar org-index--sort-timer nil "Timer to sort index in correct order.")
(defvar org-index--inhibit-sort-idle nil "If set, index will not be sorted in idle background.")
(defvar org-index--aligned 0 "For this Emacs session: remember number of table lines aligned.")
(defvar org-index--align-interactive most-positive-fixnum "Number of rows to align in ‘org-index--parse-table’.")
(defvar org-index--edit-widgets nil "List of widgets used to edit.")
(defvar org-index--context-index nil "Position and line used for index in edit buffer.")
(defvar org-index--context-occur nil "Position and line used for occur in edit buffer.")
(defvar org-index--context-node nil "Buffer and position for node in edit buffer.")
(defvar org-index--short-help-buffer-name "*org-index commands*" "Name of buffer to display short help.")
(defvar org-index--display-short-help nil "True, if short help should be displayed.")
(defvar org-index--short-help-displayed nil "True, if short help message has been displayed.")
(defvar org-index--prefix-arg nil "True, if prefix argument has been received during input.")
(defvar org-index--minibuffer-saved-key nil "Temporarily save entry of minibuffer keymap.")
(defvar org-index--after-focus-timer nil "Timer to clock in or update focused node after a delay.")
(defvar org-index--after-focus-context nil "Context for after focus action.")
(defvar org-index--this-command nil "Subcommand, that is currently excecuted.")
(defvar org-index--last-command nil "Subcommand, that hast been excecuted last.")

;; static information for this program package
(defconst org-index--commands '(occur add kill head ping index ref yank column edit help short-help focus example sort find-ref highlight maintain) "List of commands available.")
(defconst org-index--valid-headings '(ref id created last-accessed count keywords category level yank tags) "All valid headings.")
(defconst org-index--occur-buffer-name "*org-index-occur*" "Name of occur buffer.")
(defconst org-index--edit-buffer-name "*org-index-edit*" "Name of edit buffer.")
(defvar org-index--short-help-text nil "Cache for result of `org-index--get-short-help-text.")
(defvar org-index--shortcut-chars nil "Cache for result of `org-index--get-shortcut-chars.")
(defvar org-index--after-focus-delay 10 "Number of seconds to wait before invoking after-focus action.")


(defmacro org-index--on (column value &rest body)
  "Execute the forms in BODY with point on index line whose COLUMN is VALUE.
The value returned is the value of the last form in BODY or nil,
if VALUE cannot be found."
  (declare (indent 2) (debug t))
  (let ((pointvar (make-symbol "point"))
        (foundvar (make-symbol "found"))
        (retvar (make-symbol "ret")))
    `(save-current-buffer
       (let ((,pointvar (point))
             ,foundvar
             ,retvar)

         (set-buffer org-index--buffer)

         (setq ,foundvar (org-index--go ,column ,value))
         (when ,foundvar
           (setq ,retvar (progn ,@body)))
       
         (goto-char ,pointvar)
       
         ,retvar))))


(defun org-index (&optional command search-ref arg)
  "Fast search-index for selected org nodes and things outside.

This function creates and updates an index table with keywords;
each line either points to a heading in org, references something
outside or carries a snippet of text to yank.  The index table is
searched for keywords by means of an incremental occur; results
are sorted by usage count and date, so that frequently used
entries appear first.

References are decorated numbers (e.g. 'R237' or '--455--'); they are
well suited to be used outside of org, e.g. in folder names, ticket
systems or on printed documents.

On first invocation this function will help to create a dedicated node
for its index table.

To start building up your index, use subcommands 'add', 'ref' and
'yank' to create entries and use 'occur' to find them.

This is version 5.5.0 of org-index.el.


The function `org-index' is the only interactive function of this
package and its main entry point; it will present you with a list
of subcommands to choose from:

\(Note the one-letter shortcuts, e.g. [o]; used like `\\[org-index-dispatch] o'.)

  occur: [o] Incrementally show matching lines from index.
    Result is updated after every keystroke.  You may enter a
    list of words seperated by space or comma (`,'), to select
    lines that contain all of the given words. With a numeric
    prefix argument, show lines, which have been accessed at
    most this many days ago.

  add: [a] Add the current node to index.
    So that (e.g.) it can be found through the subcommand
    'occur'.  Update index, if node is already present.

  kill: [k] Kill (delete) the current node from index.
    Can be invoked from index, from occur or from a headline.

  head: [h] Search for heading, by ref or from index line.
    If invoked from within index table, go to associated
    node (if any), otherwise ask for ref to search.
  
  index: [i] Enter index table and maybe go to a specific reference.
    Use `org-mark-ring-goto' (\\[org-mark-ring-goto]) to go back.

  ping: [p] Echo line from index table for current node.
    If current node is not in index, than search among its
    parents.

  ref: [r] Create a new index line with a reference.
    This line will not be associated with a node.

  yank: [y] Store a new string, that can be yanked from occur.
    The index line will not be associated with a node.

  column: [c] From within index table: read char and jump to column.
    Shortcut for column movement; stays within one index line.

  edit: [e] Present current line in edit buffer.
    Can be invoked from index, from occur or from a headline.

  focus: [f] Return to first focused node; repeat to see them all.
    The focused nodes are kept in a short list; they need not be
    part of the index though.  This command visits one focus node
    after the other, as long as you invoke it in quick succession
    and without moving to other nodes; otherwise it returns to
    the focus node, where you left off. Finally, with a prefix
    argument, this command offers more options, e.g. to set focus
    in the first place.

  help: Show complete help text of `org-index'.
    I.e. this text.

  short-help: [?] Show this one-line description of each subcommand.
    I.e. from the complete help, show only the first line for each
    subcommand.

  example: Create an example index, that will not be saved.
    May serve as an example.

  sort: Sort lines in index, in region or buffer.
    Region or buffer can be sorted by contained reference; Index
    by count, reference or last access.

  find-ref: Search for given reference in all org-buffers.
    A wrapper to employ Emacs standard `multi-occur' function;
    asks for reference.

  highlight: Highlight or unhighlight all references.
     Operates on active region or whole buffer.  Call with prefix
     argument (`C-u') to remove highlights.

  maintain: Index maintainance.
     Offers some choices to check, update or fix your index.

If you invoke `org-index' for the first time, an assistant will be
invoked, that helps you to create your own index.

Invoke `org-customize' to tweak the behaviour of `org-index'.

This includes the global key `org-index-dispatch-key' to invoke
the most important subcommands with one additional key.

A numeric prefix argument is used as a reference number for
commands, that need one (e.g. 'head') or to modify their
behaviour (e.g. 'occur').

Also, a single prefix argument may be specified just before the
final character (e.g. like `C-c i C-u f') or by just typing an
upper case letter (e.g. `C-c i F').

Use from elisp: Optional argument COMMAND is a symbol naming the
command to execute.  SEARCH-REF specifies a reference to search
for, if needed.  ARG allows passing in a prefix argument as in
interactive calls."

  (interactive "i\ni\nP")

  (let (search-id             ; id to search for
        search-fingerprint    ; fingerprint to search for
        sort-what             ; sort what ?
        kill-new-text         ; text that will be appended to kill ring
        message-text)         ; text that will be issued as an explanation


    (catch 'new-index

      ;;
      ;; Initialize and parse
      ;;

      ;; creates index table, if necessary
      (org-index--verify-id)

      ;; Get configuration of index table
      (org-index--parse-table org-index--align-interactive t)

      ;; store context information
      (org-index--retrieve-context)


      ;;
      ;; Arrange for proper sorting of index
      ;;

      ;; lets assume, that it has been sorted this way (we try hard to make sure)
      (unless org-index--last-sort-assumed (setq org-index--last-sort-assumed org-index-sort-by))
      ;; rearrange for index beeing sorted into default sort order after 300 secs of idle time
      (unless org-index--sort-timer
        (setq org-index--sort-timer
              (run-with-idle-timer org-index-idle-delay t 'org-index--sort-silent)))


      ;;
      ;; Find out, what we are supposed to do
      ;;

      ;; Check or read command
      (if (and command (not (eq command 'short-help)))
          (unless (memq command org-index--commands)
            (error "Unknown command '%s' passed as argument, valid choices are any of these symbols: %s"
                   command (mapconcat 'symbol-name org-index--commands ",")))
        
        ;; read command; if requested display help in read-loop
        (setq org-index--display-short-help (eq command 'short-help))
        (setq command (org-index--read-command))
        (setq org-index--last-command org-index--this-command)
        (setq org-index--this-command command)
	(if org-index--prefix-arg (setq arg (or arg '(4))))
        (setq org-index--display-short-help nil))

      ;;
      ;; Get search string, if required; process possible sources one after
      ;; another (lisp argument, prefix argument, user input).
      ;;

      ;; Try prefix, if no lisp argument given
      (if (and (not search-ref)
               (numberp arg))
          (setq search-ref (format "%s%d%s" org-index--head arg org-index--tail)))
      
      ;; These actions really need a search string and may even prompt for it
      (when (memq command '(index head find-ref))

        ;; search from surrounding text ?
        (unless search-ref
          (if org-index--within-index-node

              (if (org-at-table-p)
                  (setq search-ref (org-index--get-or-set-field 'ref)))
            
            (if (and org-index--below-cursor
                     (string-match (concat "\\(" org-index--ref-regex "\\)")
                                   org-index--below-cursor))
                (setq search-ref (match-string 1 org-index--below-cursor)))))
        
        ;; If we still do not have a search string, ask user explicitly
        (unless search-ref
          (if (eq command 'index)
              (let ((r (org-index--read-search-for-index)))
                (setq search-ref (cl-first r))
                (setq search-id (cl-second r))
                (setq search-fingerprint (cl-third r)))
            (unless (and (eq command 'head)
                         org-index--within-index-node
                         (org-at-table-p))
              (setq search-ref (read-from-minibuffer "Search reference number: ")))))

        ;; Clean up search string
        (when search-ref
          (setq search-ref (org-trim search-ref))
          (if (string-match "^[0-9]+$" search-ref)
              (setq search-ref (concat org-index--head search-ref org-index--tail)))
          (if (string= search-ref "") (setq search-ref nil)))

        (if (and (not search-ref)
                 (not (eq command 'index))
                 (not (and (eq command 'head)
                           org-index--within-index-node
                           (org-at-table-p))))
            (error "Command %s needs a reference number" command)))

      
      ;;
      ;; Command sort needs to know in advance, what to sort for
      ;;
      
      (when (eq command 'sort)
        (setq sort-what (intern (org-completing-read "You may sort:\n  - index  : your index table by various columns\n  - region : the active region by contained reference\n  - buffer : the whole current buffer\nPlease choose what to sort: " (list "index" "region" "buffer") nil t))))
      
      
      ;;
      ;; Enter table
      ;;

      ;; Arrange for beeing able to return
      (when (and (memq command '(occur head index example sort maintain focus))
                 (not (string= (buffer-name) org-index--occur-buffer-name)))
        (org-mark-ring-push))

      ;; These commands will leave user in index table after they are finished
      (when (or (memq command '(index maintain))
                (and (eq command 'sort)
                     (eq sort-what 'index)))

        (pop-to-buffer-same-window org-index--buffer)
        (goto-char org-index--point)
        (org-index--unfold-buffer))


      ;;
      ;; Actually do, what is requested
      ;;

      (cond
   
       ((eq command 'help)

        ;; bring up help-buffer for this function
        (describe-function 'org-index))

       
       ((eq command 'short-help)

        (org-index--display-short-help))

       
       ((eq command 'find-ref)

        ;; Construct list of all org-buffers
        (let (org-buffers)
          (dolist (buff (buffer-list))
            (set-buffer buff)
            (if (string= major-mode "org-mode")
                (setq org-buffers (cons buff org-buffers))))

          ;; Do multi-occur
          (multi-occur org-buffers (org-index--make-guarded-search search-ref))

          ;; Present results
          (if (get-buffer "*Occur*")
              (progn
                (setq message-text (format "Found '%s'" search-ref))
                (other-window 1)
                (toggle-truncate-lines 1))
            (setq message-text (format "Did not find '%s'" search-ref)))))


       ((eq command 'add)

        (let ((r (org-index--do-add-or-update (if (equal arg '(4)) t nil)
                                              (if (numberp arg) arg nil))))
          (setq message-text (car r))
          (setq kill-new-text (cdr r))))


       ((eq command 'kill)
        (setq message-text (org-index--do-kill)))


       ((eq command 'head)

        (if (and org-index--within-index-node
                 (org-at-table-p))
            (setq search-id (org-index--get-or-set-field 'id)))
        
        (if (and (not search-id) search-ref)
            (setq search-id (org-index--id-from-ref search-ref)))
        
        (setq message-text
              (if search-id
                  (org-index--find-id search-id)
                "Current line has no id")))


       ((eq command 'index)

        (goto-char org-index--below-hline)

        (setq message-text

              (if search-ref
                  (if (org-index--go 'ref search-ref)
                      (progn
                        (org-index--update-current-line)
                        (org-table-goto-column (org-index--column-num 'ref))
                        (format "Found index line '%s'" search-ref))
                    (format "Did not find index line with reference '%s'" search-ref))

                (if search-id
                    (if (org-index--go 'id search-id)
                        (progn
                          (org-index--update-current-line)
                          (org-table-goto-column (org-index--column-num 'ref))
                          (format "Found index line '%s'" (org-index--get-or-set-field 'ref)))
                      (format "Did not find index line with id '%s'" search-id))

                  (if search-fingerprint
                      (if (org-index--go 'fingerprint org-index--last-fingerprint)
                          (progn
                            (org-index--update-current-line)
                            (beginning-of-line)
                            (format "Found latest index line"))
                        (format "Did not find index line"))

                    ;; simply go into table
                    "At index table"))))

        (recenter))


       ((eq command 'ping)

        (let ((moved-up 0) id info reached-top done)

          (unless (string= major-mode "org-mode") (error "No node at point"))
          ;; take id from current node or reference
          (setq id (if search-ref
                       (org-index--id-from-ref search-ref)
                     (org-id-get)))

          ;; move up until we find a node in index
          (save-excursion
            (outline-back-to-heading)
            (while (not done)
              (if id
                  (setq info (org-index--on 'id id
                               (mapcar (lambda (x) (org-index--get-or-set-field x))
                                       (list 'keywords 'count 'created 'last-accessed 'category 'ref)))))

              (setq reached-top (= (org-outline-level) 1))

              (if (or info reached-top)
                  (setq done t)
                (outline-up-heading 1 t)
                (cl-incf moved-up))

              (setq id (org-id-get))))
          
          (if info
              (progn
                (setq message-text
                      (apply 'format
                             (append (list "'%s'%s has been accessed %s times between %s and %s; category is '%s', reference is '%s'"
                                           (pop info)
                                           (if (> moved-up 0) (format " (parent node, %d level up)" moved-up) ""))
                                     info)))
                (setq kill-new-text (car (last info))))
            (setq message-text "Neither this node nor any of its parents is part of index"))))


       ((eq command 'occur)

        (set-buffer org-index--buffer)
        (org-index--do-occur (if (numberp arg) arg nil)))


       ((eq command 'ref)

        (let (args newref)

          (setq args (org-index--collect-values-from-user org-index-edit-on-ref))
          (setq newref (org-index--get-save-maxref))
          (setq args (plist-put args 'ref newref))
          (apply 'org-index--do-new-line args)

          (setq kill-new-text newref)

          (setq message-text (format "Added new row with ref '%s'" newref))))


       ((eq command 'yank)

        (let (args)

          (setq args (org-index--collect-values-from-user org-index-edit-on-yank))
          (if (plist-get args 'yank)
              (plist-put args 'yank (replace-regexp-in-string "|" "\\vert" (plist-get args 'yank) nil 'literal)))
          (setq args (plist-put args 'category "yank"))
          (apply 'org-index--do-new-line args)
          
          (setq message-text "Added new row with text to yank")))


       ((eq command 'column)

        (if (and org-index--within-index-node
                 (org-at-table-p))
            (let (char col num)
              (setq char (read-char "Please specify, which column to go to (r=ref, k=keywords, c=category, y=yank): "))
              (unless (memq char (list ?r ?k ?c ?y))
                (error (format "Invalid char '%c', cannot goto this column" char)))
              (setq col (cdr (assoc char '((?r . ref) (?k . keywords) (?c . category) (?y . yank)))))
              (setq num (org-index--column-num col))
              (if num
                  (progn
                    (org-table-goto-column num)
                    (setq message-text (format "At column %s" (symbol-name col))))
                
                (error (format "Column '%s' is not present" col))))
          (error "Need to be in index table to go to a specific column")))
       

       ((eq command 'edit)

        (setq message-text (org-index--do-edit)))
       

       ((eq command 'sort)

        (let ((sorts (list "count" "last-accessed" "mixed" "id" "ref"))
              sort groups-and-counts)

          (cond
           ((eq sort-what 'index)
            (setq sort
                  (intern
                   (completing-read
                    "Please choose column to sort index table: "
                    (cl-copy-list sorts)
                    nil t nil nil (symbol-name org-index-sort-by))))

            (org-index--do-sort-index sort)
            (org-table-goto-column (org-index--column-num (if (eq sort 'mixed) 'last-access sort)))
            ;; When saving index, it should again be sorted correctly
            (with-current-buffer org-index--buffer
              (add-hook 'before-save-hook 'org-index--sort-silent t))
            
            (setq message-text
                  (format
                   (concat "Your index has been sorted temporarily by %s and will be sorted again by %s after %d seconds of idle time"
                           (if groups-and-counts
                               "; %d groups with equal %s and a total of %d lines have been found"
                             ""))
                   (symbol-name sort)
                   org-index-sort-by
                   org-index-idle-delay
                   (cl-second groups-and-counts)
                   (symbol-name sort)
                   (cl-third groups-and-counts))))

           ((memq sort-what '(region buffer))
            (org-index--do-sort-lines sort-what)
            (setq message-text (format "Sorted %s by contained references" sort-what))))))


       ((eq command 'highlight)

        (let ((where "buffer"))
          (save-excursion
            (save-restriction
              (when (and transient-mark-mode
                         mark-active)
                (narrow-to-region (region-beginning) (region-end))
                (setq where "region"))

              (if arg
                  (progn
                    (unhighlight-regexp org-index--ref-regex)
                    (setq message-text (format "Removed highlights for references in %s" where)))
                (highlight-regexp org-index--ref-regex 'isearch)
                (setq message-text (format "Highlighted references in %s" where)))))))


       ((eq command 'focus)
        (setq message-text (if arg
                               (org-index--more-focus-commands)
                             (org-index--goto-focus))))


       ((eq command 'maintain)
        (setq message-text (org-index--do-maintain)))

       
       ((eq command 'example)

        (if (y-or-n-p "This assistant will help you to create a temporary index with detailed comments.\nDo you want to proceed ? ")
            (org-index--create-index t)))


       ((not command) (setq message-text "No command given"))

       
       (t (error "Unknown subcommand '%s'" command)))


      ;; tell, what we have done and what can be yanked
      (if kill-new-text (setq kill-new-text
                              (substring-no-properties kill-new-text)))
      (if (string= kill-new-text "") (setq kill-new-text nil))
      (let ((m (concat
                message-text
                (if (and message-text kill-new-text)
                    " and r"
                  (if kill-new-text "R" ""))
                (if kill-new-text (format "eady to yank '%s'." kill-new-text) (if message-text "." "")))))
        (unless (string= m "")
          (message m)
          (setq org-index--message-text m)))
      (if kill-new-text (kill-new kill-new-text)))))


(defun org-index-dispatch (&optional arg)
  "Read additional chars and call subcommands of `org-index'.
Can be bound in global keyboard map as central entry point.
Optional argument ARG is passed on."
  (interactive "P")
  (let (char command (c-u-text (if arg " C-u " "")))
    (while (not char)
      (if (sit-for 1)
          (message (concat "org-index (? for detailed prompt) -" c-u-text)))
      (setq char (key-description (read-key-sequence nil)))
      (if (string= char "C-g") (keyboard-quit))
      (if (string= char "SPC") (setq char "?"))
      (when (string= char (upcase char))
        (setq char (downcase char))
        (setq arg (or arg '(4))))
      (when (string= char "C-u")
        (setq arg (or arg '(4)))
        (setq c-u-text " C-u ")
        (setq char nil)))
    (setq command (cdr (assoc char (org-index--get-shortcut-chars))))
    (unless command
      (message "No subcommand for '%s'; switching to detailed prompt" char)
      (sit-for 1)
      (setq command 'short-help))
    (org-index command nil arg)))


(defun org-index-new-line (&rest keys-values)
  "Create a new line within the index table, returning its reference.

The function takes a varying number of argument pairs; each pair
is a symbol for an existing column heading followed by its value.
The return value is the new reference.

Example:

  (message \"Created reference %s\"
           (org-index-new-line 'keywords \"foo bar\" 'category \"baz\"))

Optional argument KEYS-VALUES specifies content of new line."

  (let ((ref (plist-get keys-values 'ref)))
    (org-index--verify-id)
    (org-index--parse-table)
    (if (not (memq ref  '(t nil)))
        (error "Column 'ref' accepts only 't' or 'nil'"))
    (when ref
      (setq ref (org-index--get-save-maxref))
      (setq keys-values (plist-put keys-values 'ref ref)))

    (apply 'org-index--do-new-line keys-values)
    ref))


(defun org-index--read-command ()
  "Read subcommand for ‘org-index’ from minibuffer."
  (let (minibuffer-scroll-window
        command)
    (setq org-index--short-help-displayed nil)
    (setq org-index--prefix-arg nil)
    (add-hook 'minibuffer-setup-hook 'org-index--minibuffer-setup-function)
    (add-hook 'minibuffer-exit-hook 'org-index--minibuffer-exit-function)
    (unwind-protect
        (setq command
              (completing-read
               (concat
                "Please choose"
                (if org-index--display-short-help "" " (? for short help)")
                ": ")
               (append (mapcar 'symbol-name org-index--commands)
                       (mapcar 'upcase-initials (mapcar 'symbol-name org-index--commands)))
               nil t))
      (remove-hook 'minibuffer-setup-hook 'org-index--minibuffer-setup-function)
      (remove-hook 'minibuffer-exit-hook 'org-index--minibuffer-exit-function)
      (unless (string= command (downcase command))
        (if command (setq command (downcase command)))
        (setq org-index--prefix-arg '(4)))
      (setq command (intern command))
      (when org-index--short-help-displayed
        (quit-windows-on org-index--short-help-buffer-name)))
    command))


(defun org-index--minibuffer-setup-function ()
  "Prepare minibuffer for `org-index--read-command'."
  (setq org-index--minibuffer-saved-key (local-key-binding (kbd "?")))
  (local-set-key (kbd "?") 'org-index--display-short-help)
  (local-set-key (kbd "C-u") (lambda () (interactive)
			       (setq org-index--prefix-arg t)
			       (message "C-u")))
  (if org-index--display-short-help (org-index--display-short-help)))


(defun org-index--minibuffer-exit-function ()
  "Restore minibuffer after `org-index--read-command'."
  (local-set-key (kbd "?") org-index--minibuffer-saved-key)
  (local-set-key (kbd "C-u") 'universal-argument)
  (setq org-index--minibuffer-saved-key nil))


(defun org-index--display-short-help ()
  "Helper function to show help in minibuffer."
  (interactive)

  (with-temp-buffer-window
   org-index--short-help-buffer-name nil nil
   (setq org-index--short-help-displayed t)
   (princ "Short help; shortcuts in []; capital letter acts like C-u.\n")
   (princ (org-index--get-short-help-text)))
  (with-current-buffer org-index--short-help-buffer-name
    (let ((inhibit-read-only t)
          win)
      (setq win (get-buffer-window))
      (shrink-window-if-larger-than-buffer win)
      (goto-char (point-min))
      (end-of-line)
      (goto-char (point-min)))))


(defun org-index--get-short-help-text ()
  "Extract text for short help message from long help."
  (or org-index--short-help-text
      (with-temp-buffer
        (insert (documentation 'org-index))
        (goto-char (point-min))
        (search-forward (concat "  " (symbol-name (cl-first org-index--commands)) ": "))
        (forward-line 0)
        (kill-region (point-min) (point))
        (search-forward (concat "  " (symbol-name (car (last org-index--commands))) ": "))
        (forward-line 1)
        (kill-region (point) (point-max))
        (keep-lines "^  [-a-z]+:" (point-min) (point-max))
        (align-regexp (point-min) (point-max) "\\(\\s-*\\):")
        (goto-char (point-min))
        (while (re-search-forward "\\. *$" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (re-search-forward "short-help")
        (end-of-line)
        (insert " (this text)")
        (goto-char (point-min))
        (unless (= (line-number-at-pos (point-max)) (1+ (length org-index--commands)))
          (error "Internal error, unable to properly extract one-line descriptions of subcommands"))
        (setq org-index--short-help-text (buffer-string)))))


(defun org-index--get-shortcut-chars ()
  "Collect shortcut chars from short help message."
  (or org-index--shortcut-chars
      (with-temp-buffer
        (insert (org-index--get-short-help-text))
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when (looking-at "^  \\([-a-z]+\\)[ \t]+: +\\[\\([a-z?]\\)\\] ")
            (setq org-index--shortcut-chars
                  (cons (cons (match-string 2) (intern (match-string 1)))
                        org-index--shortcut-chars)))
          (forward-line 1))
        (unless (> (length org-index--shortcut-chars) 0)
          (error "Internal error, did not find shortcut chars"))
        org-index--shortcut-chars)))


(defun org-index--goto-focus ()
  "Goto focus node, one after the other."
  (if org-index--ids-focused-nodes
      (let (this-id target-id following-id last-id again explain marker)
        (setq again (and (eq this-command last-command)
                         (eq org-index--this-command org-index--last-command)))
        (setq last-id (or org-index--id-last-goto-focus
                          (car (last org-index--ids-focused-nodes))))
        (setq this-id (org-id-get))
        (setq following-id (car (or (cdr-safe (member last-id
                                                      (append org-index--ids-focused-nodes
                                                              org-index--ids-focused-nodes)))
                                    org-index--ids-focused-nodes)))
        (if again
            (progn
              (setq target-id following-id)
              (setq explain "Jumped to next"))
          (setq target-id last-id)
          (setq explain "Jumped back to current"))

        (if (member target-id (org-index--ids-up-to-top))
            (setq explain "Staying below current")
          (unless (setq marker (org-id-find target-id 'marker))
            (setq org-index--id-last-goto-focus nil)
            (error "Could not find focus-node with id %s" target-id))

          (pop-to-buffer-same-window (marker-buffer marker))
          (goto-char (marker-position marker))
          (org-index--unfold-buffer)
          (move-marker marker nil))
        
        (when org-index-clock-into-focus
          (if org-index--after-focus-timer (cancel-timer org-index--after-focus-timer))
          (setq org-index--after-focus-context target-id)
          (setq org-index--after-focus-timer
                (run-at-time org-index--after-focus-delay nil
                             (lambda ()
                               (if org-index--after-focus-context
                                   (if org-index-clock-into-focus 
                                       (save-excursion
                                         (org-id-goto org-index--after-focus-context)
                                         (org-clock-in)))
                                 (org-index--update-line org-index--after-focus-context t)
                                 (setq org-index--after-focus-context nil))))))
        (setq org-index--id-last-goto-focus target-id)
        (if (cdr org-index--ids-focused-nodes)
            (format "%s focus node (out of %d)"
                    explain
                    (length org-index--ids-focused-nodes))
          "Jumped to single focus-node"))
      "No nodes in focus, use set-focus"))


(defun org-index--more-focus-commands ()
  "More commands for handling focused nodes."
  (let (id text more-text char prompt ids-up-to-top)

    (setq prompt "Please specify action on the list focused nodes: set, append, delete (s,a,d or ? for short help) - ")
    (while (not (memq char (list ?s ?a ?d)))
        (setq char (read-char prompt))
        (setq prompt "Actions on list of focused nodes:  s)et single focus on this node,  a)ppend this node to list,  d)elete this node from list.  Please choose - "))
    (setq text
          (cond

           ((eq char ?s)
            (setq id (org-id-get-create))
            (setq org-index--ids-focused-nodes (list id))
            (setq org-index--id-last-goto-focus id)
            (if org-index-clock-into-focus (org-clock-in))
            "Focus has been set on current node%s (1 node in focus)")

           ((eq char ?a)
            (setq id (org-id-get-create))
            (unless (member id org-index--ids-focused-nodes)
              ;; remove any children, that are already in list of focused nodes
              (setq org-index--ids-focused-nodes
                    (delete nil (mapcar (lambda (x)
                                          (if (member id (org-with-point-at (org-id-find x t)
                                                           (org-index--ids-up-to-top)))
                                              (progn
                                                (setq more-text ", removing its children")
                                                nil)
                                            x))
                                        org-index--ids-focused-nodes)))
              ;; remove parent, if already in list of focused nodes
              (setq ids-up-to-top (org-index--ids-up-to-top))
              (when (seq-intersection ids-up-to-top org-index--ids-focused-nodes)
                (setq org-index--ids-focused-nodes (seq-difference org-index--ids-focused-nodes ids-up-to-top))
                (setq more-text (concat more-text ", replacing its parent")))
              (setq org-index--ids-focused-nodes (cons id org-index--ids-focused-nodes)))
            (setq org-index--id-last-goto-focus id)
	    (setq org-index--id-last-goto-focus id)
            (if org-index-clock-into-focus (org-clock-in))
            "Current node has been appended to list of focused nodes%s (%d node%s in focus)")

           ((eq char ?d)
            (setq id (org-id-get))
            (if (and id  (member id org-index--ids-focused-nodes))
                (progn
                  (setq org-index--id-last-goto-focus
                        (or (car-safe (cdr-safe (member id (reverse (append org-index--ids-focused-nodes
                                                                            org-index--ids-focused-nodes)))))
                            org-index--id-last-goto-focus))
                  (setq org-index--ids-focused-nodes (delete id org-index--ids-focused-nodes))
		  (setq org-index--id-last-goto-focus nil)
                  "Current node has been removed from list of focused nodes%s (%d node%s in focus)")
              "Current node has not been in list of focused nodes%s (%d node%s in focus)"))))
    
    (with-current-buffer org-index--buffer
      (org-entry-put org-index--point "ids-focused-nodes" (string-join org-index--ids-focused-nodes " ")))
    
    (format text (or more-text "") (length org-index--ids-focused-nodes) (if (cdr org-index--ids-focused-nodes) "s" ""))))


(defun org-index--ids-up-to-top ()
  "Get list of all ids from current node up to top level"
  (when (string= major-mode "org-mode")
    (let (ancestors id level start-level)
      (save-excursion
        (ignore-errors
          (outline-back-to-heading)
          (setq id (org-id-get))
          (if id (setq ancestors (cons id ancestors)))
          (setq start-level (org-outline-level))
          (if (<= start-level 1)
              nil
            (while (> start-level 1)
              (setq level start-level)
              (while (>= level start-level)
                (outline-previous-heading)
                (setq level (org-outline-level)))
              (setq start-level level)
              (setq id (org-id-get))
              (if id (setq ancestors (cons id ancestors))))
            ancestors))))))


(defun org-index--do-edit ()
  "Perform command edit."
  (let ((maxlen 0) cols-vals buffer-keymap field-keymap keywords-pos val)

    (setq org-index--context-node nil)
    (setq org-index--context-occur nil)
    
    ;; change to index, if whithin occur
    (if org-index--within-occur
        (let ((pos (get-text-property (point) 'org-index-lbp)))
          (org-index--occur-test-stale pos)
          (setq org-index--context-occur (cons (point) (org-index--line-in-canonical-form)))
          (set-buffer org-index--buffer)
          (goto-char pos))
      
      ;; change to index, if still not within
      (if (not org-index--within-index-node)
          (let ((id (org-id-get)))
            (setq org-index--context-node (cons (current-buffer) (point)))
            (set-buffer org-index--buffer)
            (unless (and id (org-index--go 'id id))
              (setq org-index--context-node nil)
              (error "This node is not in index")))))
    
    ;; retrieve current content of index line
    (dolist (col (mapcar 'car (reverse org-index--columns)))
      (if (> (length (symbol-name col)) maxlen)
          (setq maxlen (length (symbol-name col))))
      (setq val (org-index--get-or-set-field col))
      (if (and val (eq col 'yank)) (setq val (replace-regexp-in-string (regexp-quote "\\vert") "|" val nil 'literal)))
      (setq cols-vals (cons (cons col val)
                            cols-vals)))

    ;; we need two different keymaps
    (setq buffer-keymap (make-sparse-keymap))
    (set-keymap-parent buffer-keymap widget-keymap)
    (define-key buffer-keymap (kbd "C-c C-c") 'org-index--edit-accept)
    (define-key buffer-keymap (kbd "C-c C-k") 'org-index--edit-abort)
      
    (setq field-keymap (make-sparse-keymap))
    (set-keymap-parent field-keymap widget-field-keymap)
    (define-key field-keymap (kbd "C-c C-c") 'org-index--edit-accept)
    (define-key field-keymap (kbd "C-c C-k") 'org-index--edit-abort)

    ;; prepare buffer
    (setq org-index--context-index (cons (point) (org-index--line-in-canonical-form)))
    (if (get-buffer org-index--edit-buffer-name) (kill-buffer org-index--edit-buffer-name))
    (switch-to-buffer (get-buffer-create org-index--edit-buffer-name))

    ;; create and fill widgets
    (setq org-index--edit-widgets nil)
    (widget-insert "Edit this line from index; type C-c C-c when done, C-c C-k to abort.\n\n")
    (dolist (col-val cols-vals)
      (if (eq (car col-val) 'keywords) (setq keywords-pos (point)))
      (setq org-index--edit-widgets (cons
                                     (cons (car col-val)
                                           (widget-create 'editable-field
                                                          :format (format  (format "%%%ds: %%%%v" maxlen) (symbol-name (car col-val)))
                                                          :keymap field-keymap
                                                          (or (cdr col-val) "")))
                                     org-index--edit-widgets)))

    (widget-setup)
    (goto-char keywords-pos)
    (beginning-of-line)
    (forward-char (+  maxlen 2))
    (use-local-map buffer-keymap)
    (setq org-index--inhibit-sort-idle t)
    "Editing a single line from index"))
  

(defun org-index--edit-accept ()
  "Function to accept editing in Edit buffer."
  (interactive)

  (let ((obuf (get-buffer org-index--occur-buffer-name))
        val line)
    
    ;; Time might have passed
    (org-index--refresh-parse-table)

    (with-current-buffer org-index--buffer
    
      ;; check, if buffer has become stale
      (save-excursion
        (goto-char (car org-index--context-index))
        (unless (string= (cdr org-index--context-index)
                         (org-index--line-in-canonical-form))
          (switch-to-buffer org-index--edit-buffer-name)
          (error "Index table has changed: Cannot find line, that this buffer is editing")))

      (pop-to-buffer-same-window org-index--buffer)
      (goto-char (car org-index--context-index))

      ;; write back line to index
      (dolist (col-widget org-index--edit-widgets)
        (setq val (widget-value (cdr col-widget)))
        (if (eq (car col-widget) 'yank) (setq val (replace-regexp-in-string "|" (regexp-quote "\\vert") val)))
        (org-index--get-or-set-field (car col-widget) val))

      (setq line (org-index--align-and-fontify-current-line))
      (beginning-of-line))

    ;; write line to occur if appropriate
    (if org-index--context-occur
        (if obuf
            (if (string= (cdr org-index--context-index)
                         (cdr org-index--context-occur))
                (progn
                  (pop-to-buffer-same-window obuf)
                  (goto-char (car org-index--context-occur))
                  (beginning-of-line)
                  (let ((inhibit-read-only t))
                    (delete-region (line-beginning-position) (line-end-position))
                    (insert line)
                    (put-text-property (line-beginning-position) (line-end-position)
                                       'org-index-lbp (car org-index--context-index))))
              (error "Occur buffer and index buffer do not match any longer"))
          (message "Occur buffer has gone, cannot switch back."))
      (setq org-index--context-occur nil))

    ;; return to node, if invoked from there
    (when org-index--context-node
        (pop-to-buffer-same-window (car org-index--context-node))
        (goto-char (cdr org-index--context-node)))

    ;; clean up
    (kill-buffer org-index--edit-buffer-name)
    (setq org-index--inhibit-sort-idle nil)
    (setq org-index--context-index nil)
    (setq org-index--edit-widgets nil)
    (beginning-of-line)
    (message "Index line has been edited.")))


(defun org-index--edit-abort ()
  "Function to abort editing in Edit buffer."
  (interactive)
  (kill-buffer org-index--edit-buffer-name)
  (setq org-index--context-index nil)
  (setq org-index--edit-widgets nil)
  (beginning-of-line)
  (message "Edit aborted."))


(defun org-index--do-new-line (&rest keys-values)
  "Do the work for `org-index-new-line'.
Optional argument KEYS-VALUES specifies content of new line."

  (org-index--retrieve-context)
  (with-current-buffer org-index--buffer
    (goto-char org-index--point)

    ;; check arguments early; they might come from userland
    (let ((kvs keys-values)
          k v)
      (while kvs
        (setq k (car kvs))
        (setq v (cadr kvs))
        (if (or (not (symbolp k))
                (and (symbolp v) (not (eq v t)) (not (eq v nil))))
            (error "Arguments must be alternation of key and value"))
        (unless (org-index--column-num k)
          (error "Unknown column or column not defined in table: '%s'" (symbol-name k)))
        (setq kvs (cddr kvs))))

    (let (yank)
      ;; create new line
      (org-index--create-new-line)

      ;; fill columns
      (let ((kvs keys-values)
            k v)
        (while kvs
          (setq k (car kvs))
          (setq v (cadr kvs))
          (org-table-goto-column (org-index--column-num k))
          (insert (org-trim (or v "")))
          (setq kvs (cddr kvs))))

      ;; align and fontify line
      (org-index--promote-current-line)
      (org-index--align-and-fontify-current-line)

      ;; remember fingerprint to be able to return
      (setq org-index--last-fingerprint (org-index--get-or-set-field 'fingerprint))
        
      ;; get column to yank
      (setq yank (org-index--get-or-set-field org-index-yank-after-add))

      yank)))


(defun org-index-get-line (column value)
  "Retrieve an existing line within the index table by ref or id.
Return its contents as a property list.

The function `plist-get' may be used to retrieve specific elements
from the result.

Example:

  (plist-get (org-index-get-line 'ref \"R12\") 'count)

retrieves the value of the count-column for reference number 12.

Argument COLUMN is a symbol, either ref or id,
argument VALUE specifies the value to search for."
  ;; check arguments
  (unless (memq column '(ref id keywords 'yank))
    (error "Argument column can only be 'ref', 'id', 'keywords' or 'yank'"))

  (unless value
    (error "Need a value to search for"))
  
  (org-index--verify-id)
  (org-index--parse-table)

  (org-index--get-line column value))


(defun org-index--get-line (column value)
  "Find a line by ID, return its contents.
Argument COLUMN and VALUE specify line to get."
  (let (content)
    (org-index--on
     column value
     (mapc (lambda (x)
             (if (and (numberp (cdr x))
                      (> (cdr x) 0))
                 (setq content (cons (car x) (cons (or (org-index--get-or-set-field (car x)) "") content)))))
           (reverse org-index--columns)))
    content))


(defun org-index--ref-from-id (id)
  "Get reference from line ID."
  (org-index--on 'id id (org-index--get-or-set-field 'ref)))


(defun org-index--id-from-ref (ref)
  "Get id from line REF."
  (org-index--on 'ref ref (org-index--get-or-set-field 'id)))


(defun org-index--get-fingerprint ()
  "Get fingerprint of current line."
  (replace-regexp-in-string
   "\\s " ""
   (mapconcat (lambda (x) (org-index--get-or-set-field x)) '(id ref yank keywords created) "")))


(defun org-index--read-search-for-index ()
    "Special input routine for command index."

    ;; Accept single char commands or switch to reading a sequence of digits
    (let (char prompt search-ref search-id search-fingerprint)
    
      ;; start with short prompt but give more help on next iteration
      (setq prompt "Please specify, where to go in index (0-9,.,space,backspace,return or ? for short help) - ")
    
      ;; read one character
      (while (not (memq char (append (number-sequence ?0 ?9) (list ?\d ?\b ?\r ?\j ?\s ?.))))
        (setq char (read-char prompt))
        (setq prompt "Go to specific position in index table. Digits specify a reference number, <space> goes to top of index, <backspace> or <delete> to last line created and <return> or `.' to index line of current node.  Please choose - "))
    
      (if (memq char (number-sequence ?0 ?9))
          ;; read rest of digits
          (setq search-ref (read-from-minibuffer "Search reference number: " (char-to-string char))))
      ;; decode single chars
      (if (memq char '(?\r ?\n ?.)) (setq search-id (org-id-get)))
      (if (memq char '(?\d ?\b)) (setq search-fingerprint org-index--last-fingerprint))
      
      (list search-ref search-id search-fingerprint)))


(defun org-index--verify-id ()
  "Check, that we have a valid id."

  ;; Check id
  (unless org-index-id
    (let ((answer (org-completing-read "Cannot find an index (org-index-id is not set). You may:\n  - read-help    : to learn more about org-index\n  - create-index : invoke an assistant to create an initial index\nPlease choose: " (list "read-help" "create-index") nil t nil nil "read-help")))
      (if (string= answer "create-index")
          (org-index--create-missing-index "Variable org-index-id is not set, so probably no index table has been created yet.")
        (describe-function 'org-index)
        (throw 'new-index nil))))

  ;; Find node
  (let (marker)
    (setq marker (org-id-find org-index-id 'marker))
    (unless marker (org-index--create-missing-index "Cannot find the node with id \"%s\" (as specified by variable org-index-id)." org-index-id))
    ; Try again with new node
    (setq marker (org-id-find org-index-id 'marker))
    (unless marker (error "Could not create node"))
    (setq org-index--buffer (marker-buffer marker)
          org-index--point (marker-position marker))
    (move-marker marker nil)))


(defun org-index--retrieve-context ()
  "Collect context information before starting with command."

  ;; Get the content of the active region or the word under cursor
  (setq org-index--active-region
        (if (and transient-mark-mode mark-active)
            (buffer-substring (region-beginning) (region-end))
          nil))
  (setq org-index--below-cursor (thing-at-point 'symbol))

  ;; get category of current node
  (setq org-index--category-before
        (save-excursion ; workaround: org-get-category does not give category when at end of buffer
          (beginning-of-line)
          (org-get-category (point) t)))

  ;; Find out, if we are within index table or occur buffer
  (setq org-index--within-index-node (string= (org-id-get) org-index-id))
  (setq org-index--within-occur (string= (buffer-name) org-index--occur-buffer-name)))


(defun org-index--parse-table (&optional num-lines-to-format check-sort-mixed)
  "Parse content of index table.
Optional argument NUM-LINES-TO-FORMAT limits formatting effort and duration.
Optional argument CHECK-SORT-MIXED triggers resorting if mixed and stale."

  (let (initial-point
        end-of-headings
        start-of-headings)

    (unless num-lines-to-format (setq num-lines-to-format 0))

    (with-current-buffer org-index--buffer

      (setq initial-point (point))

      (org-index--go-below-hline)
      (org-reveal)

      ;; if table is sorted mixed and it was sorted correctly yesterday, it could still be wrong today; so check
      (when (and check-sort-mixed (eq org-index-sort-by 'mixed))
        (goto-char org-index--below-hline)
        (let (count-first-line count-second-line)
          (setq count-first-line (string-to-number (concat (org-index--get-or-set-field 'count) " 0")))
          (forward-line)
          (setq count-second-line (string-to-number (concat (org-index--get-or-set-field 'count) " 0")))
          (forward-line -1)
          (if (and (string< (org-index--get-or-set-field 'last-accessed)
                            (org-index--get-mixed-time))
                   (< count-first-line count-second-line))
              (org-index--do-sort-index org-index-sort-by)))
        (org-index--go-below-hline))

      ;; align and fontify table once for this emacs session
      (when (> num-lines-to-format org-index--aligned)
        (org-index--go-below-hline)
        (message "Aligning and fontifying %s lines of index table (once per emacs session)..."
                 (if (= num-lines-to-format most-positive-fixnum) "all" (format "%d" num-lines-to-format)))
        (save-restriction
          (let (from to)
            (forward-line -3)
            (setq from (point))
            (setq to (org-table-end))
            (when (< num-lines-to-format most-positive-fixnum)
              (forward-line (+ 3 num-lines-to-format))
              (narrow-to-region from (point))
              (setq to (min (point) to)))
            (goto-char org-index--below-hline)
            (org-table-align)
            (setq to (min (point-max) to))
            (font-lock-fontify-region from to)))
        (setq org-index--aligned num-lines-to-format)
        (org-index--go-below-hline)
        (message "Done."))

      (beginning-of-line)
      
      ;; get headings to display during occur
      (setq end-of-headings (point))
      (goto-char (org-table-begin))
      (setq start-of-headings (point))
      (setq org-index--headings-visible (substring-no-properties (org-index--copy-visible start-of-headings end-of-headings)))
      (setq org-index--headings (buffer-substring start-of-headings end-of-headings))
      
      ;; count columns
      (org-table-goto-column 100)
      (setq org-index--numcols (- (org-table-current-column) 1))
      
      ;; go to top of table
      (goto-char (org-table-begin))
      
      ;; parse line of headings
      (org-index--parse-headings)

      ;; read property or go through table to find maximum number
      (goto-char org-index--below-hline)
      (setq max-ref-field (or (org-entry-get org-index--point "max-ref")
                              (org-index--migrate-maxref-to-property)))
      
      (unless org-index--head (org-index--get-decoration-from-ref-field max-ref-field))
    
      ;; Get ids of focused node (if any)
      (setq org-index--ids-focused-nodes (split-string (or (org-entry-get nil "ids-focused-nodes") "")))
      (org-entry-delete (point) "id-focused-node") ; migrate (kind of) from previous versions

      ;; save position below hline
      (org-index--go-below-hline)
      ;; go back to initial position
      (goto-char initial-point))))


(defun org-index--get-decoration-from-ref-field (ref-field)
  "Extract decoration from a REF-FIELD."
  (unless (string-match "^\\([^0-9]*\\)\\([0-9]+\\)\\([^0-9]*\\)$" ref-field)
    (org-index--report-index-error
     "Reference in index table ('%s') does not contain a number" ref-field))
  
  ;; These are the decorations used within the first ref of index
  (setq org-index--head (match-string 1 ref-field))
  (setq org-index--tail (match-string 3 ref-field))
  (setq org-index--ref-regex (concat (regexp-quote org-index--head)
                                     "\\([0-9]+\\)"
                                     (regexp-quote org-index--tail)))
  (setq org-index--ref-format (concat org-index--head "%d" org-index--tail)))


(defun org-index--extract-refnum (ref-field)
  "Extract the number from a complete reference REF-FIELD like 'R102'."
  (unless (string-match org-index--ref-regex ref-field)
    (org-index--report-index-error
     "Reference '%s' is not formatted properly (does not match '%s')" ref-field org-index--ref-regex))
  (string-to-number (match-string 1 ref-field)))


(defun org-index--migrate-maxref-to-property ()
  "One-time migration: No property; need to go through whole table once to find max."
  (org-index--go-below-hline)
  (let ((max-ref-num 0)
        ref-field ref-num ref)
    (message "One-time migration to set index-property maxref...")
    (while (org-at-table-p)
      (setq ref-field (org-index--get-or-set-field 'ref))
      (when ref-field
        (unless org-index--head (org-index--get-decoration-from-ref-field ref-field))
        (setq ref-num (org-index--extract-refnum ref-field))
        (if (> ref-num max-ref-num) (setq max-ref-num ref-num)))
      (forward-line))
    (unless (> max-ref-num 0)
      (org-index--report-index-error "No reference found in property max-ref and none in index"))
    (setq ref-field (format org-index--ref-format max-ref-num))
    (org-index--go-below-hline)
    (org-entry-put org-index--point "max-ref" ref-field)
    (message "Done.")
    ref-field))


(defun org-index--get-save-maxref (&optional no-inc)
  "Get next reference, increment number and store it in index.
Optional argument NO-INC skips automatic increment on maxref."
  (let (ref-field)
    (with-current-buffer org-index--buffer
      (setq ref-field (org-entry-get org-index--point "max-ref"))
      (unless no-inc
        (setq ref-field (format org-index--ref-format (1+ (org-index--extract-refnum ref-field))))
        (org-entry-put org-index--point "max-ref" ref-field)))
    ref-field))


(defun org-index--refresh-parse-table ()
  "Fast refresh of selected results of parsing index table."

  (setq org-index--point (marker-position (org-id-find org-index-id 'marker)))
  (with-current-buffer org-index--buffer
    (save-excursion
      (org-index--go-below-hline))))


(defun org-index--do-maintain ()
  "Choose among and perform some tasks to maintain index."
  (let ((check-what) (max-mini-window-height 1.0) message-text)
    (setq check-what (intern (org-completing-read "These checks and fixes are available:\n  - statistics : compute statistics about index table\n  - check      : check ids by visiting their nodes\n  - duplicates : check index for duplicate rows (ref or id)\n  - clean      : remove obsolete property org-index-id\n  - update     : update content of index lines, with an id \nPlease choose: " (list "statistics" "check" "duplicates" "clean" "update") nil t nil nil "statistics")))
    (message nil)
    
    (cond
     ((eq check-what 'check)
      (setq message-text (or (org-index--check-ids)
                             "No problems found")))

     ((eq check-what 'statistics)
      (setq message-text (org-index--do-statistics)))

     ((eq check-what 'duplicates)
      (setq message-text (org-index--find-duplicates)))

     ((eq check-what 'clean)
      (let ((lines 0))
        (org-map-entries
         (lambda ()
           (when (org-entry-get (point) "org-index-ref")
             (cl-incf lines)
             (org-entry-delete (point) "org-index-ref")))
         nil 'agenda)
        (setq message-text (format "Removed property 'org-index-ref' from %d lines" lines))))
     
     ((eq check-what 'update)
      (if (y-or-n-p "Updating your index will overwrite certain columns with content from the associated heading and category.  If unsure, you may try this for a single, already existing line of your index by invoking `add'.  Are you SURE to proceed for ALL INDEX LINES ? ")
          (setq message-text (org-index--update-all-lines))
        (setq message-text "Canceled."))))
    message-text))


(defun org-index--get-mixed-time ()
  "Get timestamp for sorting order mixed."
  (format-time-string
   (org-time-stamp-format t t)
   (apply 'encode-time (append '(0 0 0) (nthcdr 3 (decode-time))))))


(defun org-index--do-sort-index (sort)
  "Sort index table according to SORT."

  (let ((is-modified (buffer-modified-p))
        top
        bottom
        mixed-time)

    (unless buffer-read-only

      (message "Sorting index table for %s..." (symbol-name sort))
      (undo-boundary)

      (let ((message-log-max nil)) ; we have just issued a message, dont need those of sort-subr

        ;; if needed for mixed sort
        (if (eq sort 'mixed)
            (setq mixed-time (org-index--get-mixed-time)))

        ;; get boundaries of table
        (org-index--go-below-hline)
        (forward-line 0)
        (setq top (point))
        (goto-char (org-table-end))

        ;; kill all empty rows at bottom
        (while (progn
                 (forward-line -1)
                 (org-table-goto-column 1)
                 (and
                  (not (org-index--get-or-set-field 'ref))
                  (not (org-index--get-or-set-field 'id))
                  (not (org-index--get-or-set-field 'yank))))
          (org-table-kill-row))
        (forward-line 1)
        (setq bottom (point))
        
        ;; sort lines
        (save-restriction
          (narrow-to-region top bottom)
          (goto-char top)
          (sort-subr t
                     'forward-line
                     'end-of-line
                     (lambda ()
                       (org-index--get-sort-key sort t mixed-time))
                     nil
                     'string<)
          (goto-char (point-min))

          ;; restore modification state
          (set-buffer-modified-p is-modified)))

      (setq org-index--last-sort-assumed sort))))


(defun org-index--do-sort-lines (what)
  "Sort lines in WHAT according to contained reference."
  (save-restriction
    (cond
     ((eq what 'region)
      (if (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
        (error "No active region, cannot sort")))
     ((eq what 'buffer)
      (unless (y-or-n-p "Sort whole current buffer ? ")
        (error "Canceled"))
      (narrow-to-region (point-min) (point-max))))

    (goto-char (point-min))
    (sort-subr nil 'forward-line 'end-of-line
               (lambda ()
                 (if (looking-at (concat ".*"
                                         (org-index--make-guarded-search org-index--ref-regex 'dont-quote)))
                     (string-to-number (match-string 1))
                   0)))))


(defun org-index--go-below-hline ()
  "Move below hline in index-table."

  (let ((errstring (format "index table within node %s" org-index-id)))

    (goto-char org-index--point)

    ;; go to heading of node
    (while (not (org-at-heading-p)) (forward-line -1))
    (forward-line 1)

    ;; go to first table, but make sure we do not get into another node
    (while (and (not (org-at-table-p))
                (not (org-at-heading-p))
                (not (eobp)))
      (forward-line))

    ;; check, if there really is a table
    (unless (org-at-table-p)
      (org-index--create-missing-index "Cannot find %s." errstring))

    ;; go just after hline
    (while (and (not (org-at-table-hline-p))
                (org-at-table-p))
      (forward-line))
    (forward-line)

    ;; and check
    (unless (org-at-table-p)
      (org-index--report-index-error "Cannot find a hline within %s" errstring))

    (org-table-goto-column 1)
    (setq org-index--below-hline (point))))


(defun org-index--parse-headings ()
  "Parse headings of index table."

  (let (field         ;; field content
        field-symbol) ;; and as a symbol

    (setq org-index--columns nil)

    ;; For each column
    (dotimes (col org-index--numcols)

      (setq field (substring-no-properties (downcase (org-trim (org-table-get-field (+ col 1))))))

      (if (string= field "")
          (error "Heading of column cannot be empty"))
      (if (and (not (string= (substring field 0 1) "."))
               (not (member (intern field) org-index--valid-headings)))
          (error "Column name '%s' is not a valid heading (custom headings may start with a dot, e.g. '.foo')" field))

      (setq field-symbol (intern field))

      ;; check if heading has already appeared
      (if (assoc field-symbol org-index--columns)
          (org-index--report-index-error
           "'%s' appears two times as column heading" (downcase field))
        ;; add it to list at front, reverse later
        (setq org-index--columns (cons (cons field-symbol (+ col 1)) org-index--columns)))))

  (setq org-index--columns (reverse org-index--columns))

  ;; check if all necessary headings have appeared
  (mapc (lambda (head)
          (unless (cdr (assoc head org-index--columns))
            (org-index--report-index-error "No column has heading '%s'" head)))
        org-index--valid-headings))


(defun org-index--create-missing-index (&rest reasons)
  "Create a new empty index table with detailed explanation.  Argument REASONS explains why."

  (org-index--ask-before-create-index "Cannot find index table: "
                                      "new permanent" "."
                                      reasons)
  (org-index--create-index))


(defun org-index--report-index-error (&rest reasons)
  "Report an error (explained by REASONS) with the existing index and offer to create a valid one to compare with."

  (when org-index--buffer
    (pop-to-buffer-same-window org-index--buffer)
    (goto-char org-index--below-hline)
    (org-reveal t))
  (org-index--ask-before-create-index "The existing index contains this error: "
                                      "temporary" ", to compare with."
                                      reasons)
  (org-index--create-index t t))


(defun org-index--ask-before-create-index (explanation type for-what reasons)
                                                  ; checkdoc-params: (explanation type for-what reasons)
  "Ask the user before creating an index or throw error.  Arguments specify bits of issued message."
  (let (reason prompt)

    (setq reason (apply 'format reasons))

    (setq prompt (concat explanation reason "\n"
                         "However, this assistant can help you to create a "
                         type " index with detailed comments" for-what "\n\n"
                         "Do you want to proceed ?"))

    (unless (let ((max-mini-window-height 1.0))
              (y-or-n-p prompt))
      (error (concat explanation reason)))))


(defun org-index--create-index (&optional temporary compare)
  "Create a new empty index table with detailed explanation.
specify flag TEMPORARY for th new table temporary, maybe COMPARE it with existing index."
  (let (buffer
        title
        firstref
        id)

    (if temporary
        (let ((file-name (concat temporary-file-directory "org-index--example-index.org"))
              (buffer-name "*org-index-example-index*"))
          (setq buffer (get-buffer-create buffer-name))
          (with-current-buffer buffer
            ;; but it needs a file for its index to be found
            (unless (string= (buffer-file-name) file-name)
              (set-visited-file-name file-name))
            (rename-buffer buffer-name) ; name is change by line above

            (erase-buffer)
            (org-mode)))

      (setq buffer (get-buffer (org-completing-read "Please choose the buffer, where the new node for the index table should be created; the new node will be inserted at its end.\n\nBuffer: " (mapcar 'buffer-name (org-buffer-list))))))

    (setq title (read-from-minibuffer "Please enter the title of the index node (leave empty for default 'index'): "))
    (if (string= title "") (setq title "index"))
    
    (while (progn
             (setq firstref (read-from-minibuffer "Please enter your first reference-number. This is an integer number preceeded by some and optionally followed by some non-numeric chars; e.g. 'R1', '-1-' or '#1#' (and your initial number does not need to be '1'). The format of your reference-numbers only needs to make sense for yourself, so that you can spot it easily in your texts or write it on a piece of paper; it should however not already appear frequently within your existing notes, to avoid too many false hits when searching.\n\nPlease choose (leave empty for default 'R1'): "))
             (if (string= firstref "") (setq firstref "R1"))
             (let (desc)
               (when (string-match "[[:blank:]]" firstref)
                 (setq desc "Contains whitespace"))
               (when (string-match "[[:cntrl:]]" firstref)
                 (setq desc "Contains control characters"))
               (unless (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)
                 ;; firstref not okay, report details
                 (setq desc
                       (cond ((string= firstref "") "is empty")
                             ((not (string-match "^[^0-9]+" firstref)) "starts with a digit")
                             ((not (string-match "^[^0-9]+[0-9]+" firstref)) "does not contain a number")
                             ((not (string-match "^[^0-9]+[0-9]+[^0-9]*$" firstref)) "contains more than one sequence of digits")

                             )))
               (if desc
                   (progn
                     (read-from-minibuffer (format "Your input '%s' does not meet the requirements because it %s.\nPlease hit RET and try again: " firstref desc))
                     t)
                 nil))))

    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "* %s %s\n" firstref title))
      (org-entry-put org-index--point "max-ref" firstref)
      (if temporary
          (insert "
  Below you find your temporary index table, which WILL NOT LAST LONGER
  THAN YOUR CURRENT EMACS SESSION; please use it only for evaluation.
")
        (insert "
  Below you find your initial index table, which will grow over time.
"))
      (insert "  You may start using it by adding some lines. Just
  move to another heading within org, invoke `org-index' and
  choose the command 'add'.  After adding a few nodes, try the
  command 'occur' to search among them.

  To gain further insight you may invoke the subcommand 'help', or
  (same content) read the help of `org-index'.

  Within the index table below, the sequence of columns does not
  matter. You may reorder them in any way you like.  You may also
  add your own columns, which should start with a dot
  (e.g. '.my-column').

  Invoke `org-customize' to tweak the behaviour of org-index
  (see the group org-index).

  This node needs not be a top level node; its name is completely
  at your choice; it is found through its ID only.
")
      (unless temporary
        (insert "
  Remark: These lines of explanation can be removed at any time.
"))

      (setq id (org-id-get-create))
      (insert (format "

  | ref | category | keywords | tags | count | level | last-accessed | created | id  | yank |
  |     |          |          |      |       |       |               |         | <4> | <4>  |
  |-----+----------+----------+------+-------+-------+---------------+---------+-----+------|
  | %s  |          | %s       |      |       |       |               | %s      | %s  |      |

"
                      firstref
                      title
                      (with-temp-buffer (org-insert-time-stamp nil nil t))
                      id))

      ;; make sure, that node can be found
      (org-id-add-location id (buffer-file-name))
      (setq buffer-save-without-query t)
      (basic-save-buffer)

      (while (not (org-at-table-p)) (forward-line -1))
      (unless buffer-read-only (org-table-align))
      (while (not (org-at-heading-p)) (forward-line -1))

      ;; read back some info about new index
      (let ((org-index-id id))
	(org-index--verify-id))

      ;; remember at least for this session
      (setq org-index-id id)

      ;; present results to user
      (if temporary
          (progn
            ;; Present existing and temporary index together
            (when compare
              (pop-to-buffer-same-window org-index--buffer)
              (goto-char org-index--point)
              (org-index--unfold-buffer)
              (delete-other-windows)
              (select-window (split-window-vertically)))
            ;; show new index
            (pop-to-buffer-same-window buffer)
            (org-id-goto id)
            (org-index--unfold-buffer)
            (if compare
                (progn
                  (message "Please compare your existing index (upper window) and a temporary new one (lower window) to fix your index")
                  (throw 'new-index nil))
              (message "This is your new temporary index, use command add to populate, occur to search.")))
        (progn
          ;; Only show the new index
          (pop-to-buffer-same-window buffer)
          (delete-other-windows)
          (org-id-goto id)
          (org-index--unfold-buffer)
          (if (y-or-n-p "This is your new index table.  It is already set for this Emacs session, so you may try it out.  Do you want to save its id to make it available for future Emacs sessions too ? ")
              (progn
                (customize-save-variable 'org-index-id id)
                (message "Saved org-index-id '%s' to %s." id (or custom-file
                                                                user-init-file))
                (throw 'new-index nil))
            (let (sq)
              (setq sq (format "(setq org-index-id \"%s\")" id))
              (kill-new sq)
              (message "Did not make the id of this new index permanent; you may want to put\n\n   %s\n\ninto your own initialization; it is copied already, just yank it." sq)
              (throw 'new-index nil))))))))


(defun org-index--unfold-buffer ()
  "Helper function to unfold buffer."
  (org-show-context)
  (org-show-subtree)
  (recenter 1)
  (save-excursion
    (org-back-to-heading)
    (forward-line) ;; on property drawer
    (org-cycle)))


(defun org-index--update-line (&optional id-or-pos no-error)
  "Update columns count and last-accessed in line ID-OR-POS.
Optional argument NO-ERROR suppresses error."

  (let (initial)

    (with-current-buffer org-index--buffer
      (unless buffer-read-only

        (setq initial (point))

        (if (if (integerp id-or-pos)
                (goto-char id-or-pos)
              (org-index--go 'id id-or-pos))
            (org-index--update-current-line)
          (unless no-error (error "Did not find reference or id '%s'" (list id-or-pos))))
        
        (goto-char initial)))))


(defun org-index--update-current-line ()
  "Update current lines columns count and last-accessed."
  (let (newcount (count-field (org-index--get-or-set-field 'count)))

    ;; update count field only if number or empty
    (when (or (not count-field)
              (string-match "^[0-9]+$" count-field))
      (setq newcount (+ 1 (string-to-number (or count-field "0"))))
      (org-index--get-or-set-field 'count
                            (number-to-string newcount)))

    ;; update timestamp
    (org-table-goto-column (org-index--column-num 'last-accessed))
    (org-table-blank-field)
    (org-insert-time-stamp nil t t)

    ;; move line according to new content
    (org-index--promote-current-line)
    (org-index--align-and-fontify-current-line)))


(defun org-index--align-and-fontify-current-line (&optional num)
  "Make current line (or NUM lines) blend well among others."
  (let (lines lines-fontified)
    ;; get current content
    (unless num (setq num 1))
    (setq lines (delete-and-extract-region (line-beginning-position) (line-end-position num)))
    ;; create minimum table with fixed-width columns to align and fontify new line
    (insert
     (setq
      lines-fontified
      (with-temp-buffer
        (org-set-font-lock-defaults)
        (insert org-index--headings-visible)
        ;; fill columns, so that aligning cannot shrink them
        (goto-char (point-min))
        (search-forward "|")
        (while (search-forward " " (line-end-position) t)
          (replace-match "." nil t))
        (goto-char (point-min))
        (while (search-forward ".|." (line-end-position) t)
          (replace-match " | " nil t))
        (goto-char (point-min))
        (while (search-forward "|." (line-end-position) t)
          (replace-match "| " nil t))
        (goto-char (point-max))
        (insert lines)
        (forward-line 0)
        (let ((start (point)))
          (while (re-search-forward "^\s +|-" nil t)
            (replace-match "| -"))
          (goto-char start))
        (org-mode)
        (org-table-align)
        (font-lock-fontify-region (point-min) (point-max))
        (goto-char (point-max))
        (if (eq -1 (skip-chars-backward "\n"))
            (delete-char 1))
        (forward-line (- 1 num))
        (buffer-substring (line-beginning-position) (line-end-position num)))))
    lines-fontified))


(defun org-index--promote-current-line ()
  "Move current line up in table according to changed sort fields."
  (let (begin end key
        (to-skip 0))

    (forward-line 0) ; stay at beginning of line

    (setq key (org-index--get-sort-key))
    (setq begin (point))
    (setq end (line-beginning-position 2))

    (forward-line -1)
    (while (and (org-at-table-p)
                (not (org-at-table-hline-p))
                (string< (org-index--get-sort-key) key))

      (cl-incf to-skip)
      (forward-line -1))
    (forward-line 1)

    ;; insert line at new position
    (when (> to-skip 0)
      (insert (delete-and-extract-region begin end))
      (forward-line -1))))


(defun org-index--get-sort-key (&optional sort with-ref mixed-time)
  "Get value for sorting from column SORT, optional WITH-REF; if mixes use MIXED-TIME."
  (let (ref
        ref-field
        key)

    (unless sort (setq sort org-index--last-sort-assumed)) ; use default value

    (when (or with-ref
              (eq sort 'ref))
      ;; get reference with leading zeroes, so it can be
      ;; sorted as text
      (setq ref-field (org-index--get-or-set-field 'ref))
      (if ref-field
          (progn
            (string-match org-index--ref-regex ref-field)
            (setq ref (format
                       "%06d"
                       (string-to-number
                        (match-string 1 ref-field)))))
        (setq ref "000000")))

    (setq key
          (cond
           ((eq sort 'count)
            (format "%08d" (string-to-number (or (org-index--get-or-set-field 'count) ""))))
           ((eq sort 'mixed)
            (let ((last-accessed (org-index--get-or-set-field 'last-accessed)))
              (unless mixed-time (setq mixed-time (org-index--get-mixed-time)))
              (concat
               (if (string< mixed-time last-accessed) last-accessed mixed-time)
               (format "%08d" (string-to-number (or (org-index--get-or-set-field 'count) ""))))))
           ((eq sort 'ref)
            ref)
           ((memq sort '(id last-accessed created))
            (org-index--get-or-set-field sort))
           (t (error "This is a bug: unmatched case '%s'" sort))))

    (if with-ref (setq key (concat key ref)))

    key))


(defun org-index--get-or-set-field (key &optional value)
  "Retrieve field KEY from index table or set it to VALUE."
  (let (field)
    (save-excursion
      (if (eq key 'fingerprint)
          (progn
            (if value (error "Internal error, pseudo-column fingerprint cannot be set"))
            (setq field (org-index--get-fingerprint)))
        (setq field (org-trim (org-table-get-field (cdr (assoc key org-index--columns)) value))))
      (if (string= field "") (setq field nil))

      (org-no-properties field))))


(defun org-index--column-num (key)
  "Return number of column KEY."
  (if (numberp key)
      key
    (cdr (assoc key org-index--columns))))


(defun org-index--make-guarded-search (ref &optional dont-quote)
  "Make robust search string from REF; DONT-QUOTE it, if requested."
  (concat "\\_<" (if dont-quote ref (regexp-quote ref)) "\\_>"))


(defun org-index--find-duplicates ()
  "Find duplicate references or ids in index table."
  (let (ref-duplicates id-duplicates)

    (setq ref-duplicates (org-index--find-duplicates-helper 'ref))
    (setq id-duplicates (org-index--find-duplicates-helper 'id))
    (goto-char org-index--below-hline)
    (if (or ref-duplicates id-duplicates)
        (progn
          ;; show results
          (pop-to-buffer-same-window
           (get-buffer-create "*org-index-duplicates*"))
          (when ref-duplicates
            (insert "These references appear more than once:\n")
            (mapc (lambda (x) (insert "  " x "\n")) ref-duplicates)
            (insert "\n\n"))
          (when id-duplicates
            (insert "These ids appear more than once:\n")
            (mapc (lambda (x) (insert "  " x "\n")) id-duplicates))

          "Some references or ids are duplicates")
      "No duplicate references or ids found")))


(defun org-index--find-duplicates-helper (column)
  "Helper for `org-index--find-duplicates': Go through table and count given COLUMN."
  (let (counts duplicates field found)

    ;; go through table
    (goto-char org-index--below-hline)
    (while (org-at-table-p)

      ;; get column
      (setq field (org-index--get-or-set-field column))

      ;; and increment
      (setq found (assoc field counts))
      (if found
          (cl-incf (cdr found))
        (setq counts (cons (cons field 1) counts)))

      (forward-line))

    (mapc (lambda (x) (if (and (> (cdr x) 1)
                          (car x))
                     (setq duplicates (cons (car x) duplicates)))) counts)
    
    duplicates))


(defun org-index--do-statistics ()
  "Compute statistics about index table."
  (let ((total-lines 0) (total-refs 0)
        ref ref-field min max message)

    ;; go through table
    (goto-char org-index--below-hline)
    (while (org-at-table-p)

      ;; get ref
      (setq ref-field (org-index--get-or-set-field 'ref))

      (when ref-field
        (string-match org-index--ref-regex ref-field)
        (setq ref (string-to-number (match-string 1 ref-field)))

        ;; record min and max
        (if (or (not min) (< ref min)) (setq min ref))
        (if (or (not max) (> ref max)) (setq max ref))

        (setq total-refs (1+ total-refs)))

      ;; count
      (setq total-lines (1+ total-lines))

      (forward-line))

    (setq message (format "%d Lines in index table. First reference is %s, last %s; %d of them are used (%d percent)"
                          total-lines
                          (format org-index--ref-format min)
                          (format org-index--ref-format max)
                          total-refs
                          (truncate (* 100 (/ (float total-refs) (1+ (- max min)))))))

    (goto-char org-index--below-hline)
    message))


(defun org-index--do-add-or-update (&optional create-ref tag-with-ref)
  "For current node or current line in index, add or update in index table.
CREATE-REF and TAG-WITH-REF if given."

  (let* (id id-from-index ref args yank ret)

    (org-index--save-positions)
    (unless (or org-index--within-index-node
                org-index--within-occur)
      (org-back-to-heading))
    
    ;; try to do the same things from within index and from outside
    (if org-index--within-index-node

        (progn
          (unless (org-at-table-p)
            (error "Within index node but not on table"))

          (setq id (org-index--get-or-set-field 'id))
          (setq ref (org-index--get-or-set-field 'ref))
          (setq args (org-index--collect-values-for-add-update-remote id))
          (org-index--write-fields args)
          (setq yank (org-index--get-or-set-field org-index-yank-after-add))

          (setq ret
                (if ref
                    (cons (format "Updated index line %s" ref) yank)
                  (cons "Updated index line" nil))))

      (setq id (org-id-get-create))
      (org-index--refresh-parse-table)
      (setq id-from-index (org-index--on 'id id id))
      (setq ref (org-index--on 'id id (org-index--get-or-set-field 'ref)))

      (if tag-with-ref
          (org-toggle-tag (format "%s%d%s" org-index--head tag-with-ref org-index--tail) 'on))
      (setq args (org-index--collect-values-for-add-update id))

      (when (and create-ref
                 (not ref))
        (setq ref (org-index--get-save-maxref))
        (setq args (plist-put args 'ref ref)))

      
      (if id-from-index
          ;; already have an id in index, find it and update fields
          (progn

            (org-index--on
                'id id
                (org-index--write-fields args)
                (setq yank (org-index--get-or-set-field org-index-yank-after-add)))

            (setq ret
                  (if ref
                      (cons (format "Updated index line %s" ref) yank)
                    (cons "Updated index line" nil))))

        ;; no id here, create new line in index
        (if ref (setq args (plist-put args 'ref ref)))
        (setq yank (apply 'org-index--do-new-line args))

        (setq ret
              (if ref
                  (cons
                   (format "Added new index line %s" ref)
                   (concat yank " "))
                (cons
                 "Added new index line"
                 nil)))))
    
    (org-index--restore-positions)

    ret))


(defun org-index--check-ids ()
  "Check, that ids really point to a node."
  
  (let ((lines 0)
        id ids marker)
    
    (goto-char org-index--below-hline)
    
    (catch 'problem
      (while (org-at-table-p)
        
        (when (setq id (org-index--get-or-set-field 'id))
          
          ;; check for double ids
          (when (member id ids)
            (org-table-goto-column (org-index--column-num 'id))
            (throw 'problem "This id appears twice in index; please use command 'maintain' to check for duplicate ids"))
          (cl-incf lines)
          (setq ids (cons id ids))
          
          ;; check, if id is valid
          (setq marker (org-id-find id t))
          (unless marker
            (org-table-goto-column (org-index--column-num 'id))
            (throw 'problem "This id cannot be found")))
        
        (forward-line))
      
      (goto-char org-index--below-hline)
      nil)))

  
(defun org-index--update-all-lines ()
  "Update all lines of index at once."

  (let ((lines 0)
        id kvs)
    
    ;; check for double ids
    (or
     (org-index--check-ids)

     (progn
       (goto-char org-index--below-hline)
       (while (org-at-table-p)
         
         ;; update single line
         (when (setq id (org-index--get-or-set-field 'id))
           (setq kvs (org-index--collect-values-for-add-update-remote id))
           (org-index--write-fields kvs)
           (cl-incf lines))
         (forward-line))

       (goto-char org-index--below-hline)
       (org-table-align)
       (format "Updated %d lines" lines)))))


(defun org-index--collect-values-for-add-update (id &optional silent category)
  "Collect values for adding or updating line specified by ID, do not ask if SILENT, use CATEGORY, if given."
  
  (let ((args (list 'id id))
        content)
    
    (dolist (col (mapcar 'car org-index--columns))
    
      (setq content "")

      (cond
       ((eq col 'keywords)
        (if org-index-copy-heading-to-keywords
            (setq content (nth 4 (org-heading-components))))
        
        ;; Shift ref and timestamp ?
        (if org-index-strip-ref-and-date-from-heading
            (dotimes (_i 2)
              (if (or (string-match (concat "^\\s-*" org-index--ref-regex) content)
                      (string-match (concat "^\\s-*" org-ts-regexp-both) content))
                  (setq content (substring content (match-end 0)))))))
       
       ((eq col 'category)
        (setq content (or category org-index--category-before)))
       
       ((eq col 'level)
        (setq content (number-to-string (org-outline-level))))
    
       ((eq col 'tags)
        (setq content (org-get-tags-string))))
      
      (unless (string= content "")
        (setq args (plist-put args col content))))

    (if (not silent)
        (let ((args-edited (org-index--collect-values-from-user org-index-edit-on-add args)))
          (setq args (append args-edited args))))

    args))


(defun org-index--collect-values-for-add-update-remote (id)
  "Wrap `org-index--collect-values-for-add-update' by prior moving to remote node identified by ID."
  
  (let (marker point args)

    (setq marker (org-id-find id t))
    ;; enter buffer and collect information
    (with-current-buffer (marker-buffer marker)
      (setq point (point))
      (goto-char marker)
      (setq args (org-index--collect-values-for-add-update id t (org-get-category (point) t)))
      (goto-char point))

    args))


(defun org-index--collect-values-from-user (cols &optional defaults)
  "Collect values for adding a new line.
Argument COLS gives list of columns to edit.
Optional argument DEFAULTS gives default values."
  
  (let (content args)
    
    (dolist (col cols)
    
      (setq content "")

      (setq content (read-from-minibuffer
                     (format "Enter text for column '%s': " (symbol-name col))
                     (plist-get col defaults)))
    
      (unless (string= content "")
        (setq args (plist-put args col content))))
    args))


(defun org-index--write-fields (kvs)
  "Update current line with values from KVS (keys-values)."
  (while kvs
    (org-index--get-or-set-field (car kvs) (org-trim (cadr kvs)))
    (setq kvs (cddr kvs))))


(defun org-index--do-kill ()
  "Perform command kill from within occur, index or node."

  (let (id ref chars-deleted-index text-deleted-from pos-in-index)

    (org-index--save-positions)
    (unless (or org-index--within-index-node
                org-index--within-occur)
      (org-back-to-heading))
    
    ;; Collect information: What should be deleted ?
    (if (or org-index--within-occur
            org-index--within-index-node)

        (progn
          (if org-index--within-index-node
              ;; In index
              (setq pos-in-index (point))
            ;; In occur
            (setq pos-in-index (get-text-property (point) 'org-index-lbp))
            (org-index--occur-test-stale pos-in-index)
            (set-buffer org-index--buffer)
            (goto-char pos-in-index))
          ;; In Index (maybe moved there)
          (setq id (org-index--get-or-set-field 'id))
          (setq ref (org-index--get-or-set-field 'ref)))

      ;; At a headline
      (setq id (org-entry-get (point) "ID"))
      (setq ref (org-index--ref-from-id id))
      (setq pos-in-index (org-index--on 'id id (point)))
      (unless pos-in-index (error "This node is not in index")))

    ;; Remark: Current buffer is not certain here, but we have all the information to delete
    
    ;; Delete from node
    (when id
      (let ((m (org-id-find id 'marker)))
        (set-buffer (marker-buffer m))
        (goto-char m)
        (move-marker m nil)
        (unless (string= (org-id-get) id)
          (error "Could not find node with id %s" id)))

      (org-index--delete-any-ref-from-tags)
      (if ref (org-index--delete-ref-from-heading ref))
      (setq text-deleted-from (cons "node" text-deleted-from)))

    ;; Delete from index
    (set-buffer org-index--buffer)
    (unless pos-in-index "Internal error, pos-in-index should be defined here")
    (goto-char pos-in-index)
    (setq chars-deleted-index (length (delete-and-extract-region (line-beginning-position) (line-beginning-position 2))))
    (setq text-deleted-from (cons "index" text-deleted-from))
    
    ;; Delete from occur only if we started there, accept that it will be stale otherwise
    (if org-index--within-occur
        (let ((inhibit-read-only t))
          (set-buffer org-index--occur-buffer-name)
          (delete-region (line-beginning-position) (line-beginning-position 2))
          ;; correct positions
          (while (org-at-table-p)
            (put-text-property (line-beginning-position) (line-end-position) 'org-index-lbp
                               (- (get-text-property (point) 'org-index-lbp) chars-deleted-index))
            (forward-line))
          (setq text-deleted-from (cons "occur" text-deleted-from))))

    (org-index--restore-positions)
    (concat "Deleted from: " (mapconcat 'identity (sort text-deleted-from 'string<) ","))))


(defun org-index--save-positions ()
  "Save current buffer and positions in index- and current buffer; not in occur-buffer."

  (let (cur-buf cur-mrk idx-pnt idx-mrk)
    (setq cur-buf (current-buffer))
    (setq cur-mrk (point-marker))
    (set-buffer org-index--buffer)
    (if (string= (org-id-get) org-index-id)
        (setq idx-pnt (point))
      (setq idx-mrk (point-marker)))
    (set-buffer cur-buf)
    (setq org-index--saved-positions (list cur-buf cur-mrk idx-pnt idx-mrk))))


(defun org-index--restore-positions ()
  "Restore positions as saved by `org-index--save-positions'."

  (cl-multiple-value-bind
      (cur-buf cur-mrk idx-pnt idx-mrk buf)
      org-index--saved-positions
    (setq buf (current-buffer))
    (set-buffer cur-buf)
    (goto-char cur-mrk)
    (set-buffer org-index--buffer)
    (goto-char (or idx-pnt idx-mrk))
    (set-buffer buf))
  (setq org-index--saved-positions nil))


(defun org-index--delete-ref-from-heading (ref)
  "Delete given REF from current heading."
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (when (search-forward ref end t)
        (delete-char (- (length ref)))
        (just-one-space)))))


(defun org-index--delete-any-ref-from-tags ()
  "Delete any reference from list of tags."
  (let (new-tags)
    (mapc (lambda (tag)
            (unless (or (string-match org-index--ref-regex tag)
			(string= tag ""))
              (setq new-tags (cons tag new-tags))))
          (org-get-tags))
    (org-set-tags-to new-tags)))


(defun org-index--go (column value)
  "Position cursor on index line where COLUMN equals VALUE.
Return t or nil, leave point on line or at top of table, needs to be in buffer initially."
  (let (found)

    (unless (eq (current-buffer) org-index--buffer)
      (error "This is a bug: Not in index buffer"))

    (unless value
      (error "Cannot search for nil"))
    
    (if (string= value "")
        (error "Cannot search for empty string"))

    (if (<= (length value) 2)
        (warn "Searching for short string '%s' will be slow" value))

    (goto-char org-index--below-hline)
    (forward-line 0)
    (save-restriction
      (narrow-to-region (point) (org-table-end))
      (while (and (not found)
                  (search-forward value nil t))
        (setq found (string= value (org-index--get-or-set-field column)))))
    
    ;; return value
    (if found
        t
      (goto-char org-index--below-hline)
      nil)))


(defun org-index--find-id (id &optional other)
  "Perform command head: Find node with ID and present it.
If OTHER in separate window."
  
  (let (message marker)

    (setq marker (org-id-find id t))

    (if marker
        (progn
          (org-index--update-line id)
          (if other
              (progn
                (pop-to-buffer (marker-buffer marker)))
            (pop-to-buffer-same-window (marker-buffer marker)))
              
          (goto-char marker)
          (org-reveal t)
          (org-show-entry)
          (recenter)
          (unless (string= (org-id-get) id)
            (setq message (format "Could not go to node with id %s (narrowed ?)" id)))
          (setq message "Found headline"))
      (setq message (format "Did not find node with %s" id)))
    message))


(defun org-index--do-occur (&optional days)
  "Perform command occur; optional narrow to DAYS back."
  (let ((word "") ; last word to search for growing and shrinking on keystrokes
        (prompt "Search for: ")
        (these-commands " NOTE: If you invoke the subcommands edit (`e') or kill (`C-c i k') from within this buffer, the index is updated accordingly")
        (lines-wanted (window-body-height))
        words                                ; list words that should match
        occur-buffer
        begin                          ; position of first line
        help-text                      ; cons with help text short and long
        search-text                    ; description of text to search for
        done                           ; true, if loop is done
        in-c-backspace                 ; true, while processing C-backspace
        help-overlay                   ; Overlay with help text
        initial-frame                  ; Frame when starting occur
        key                            ; input from user in various forms
        key-sequence
        key-sequence-raw
        days-clause)                   ; clause to display for days back search

    
    ;; make and show buffer
    (if (get-buffer org-index--occur-buffer-name)
        (kill-buffer org-index--occur-buffer-name))
    (setq occur-buffer (make-indirect-buffer org-index--buffer org-index--occur-buffer-name))
    (pop-to-buffer-same-window occur-buffer)
    (setq initial-frame (selected-frame))

    ;; avoid modifying direct buffer
    (setq buffer-read-only t)
    (toggle-truncate-lines 1)

    ;; reset stack and overlays
    (setq org-index--occur-stack nil)
    (setq org-index--occur-tail-overlay nil)
    
    ;; narrow to table rows and one line before
    (goto-char org-index--below-hline)
    (forward-line 0)
    (setq begin (point))
    (forward-line -1)
    (narrow-to-region (point) (org-table-end))
    (forward-line)

    ;; initialize help text
    (setq days-clause (if days (format " (%d days back)" days) ""))
    (setq help-text (cons
                     (concat
                      (propertize (format "Incremental occur%s" days-clause) 'face 'org-todo)
                      (propertize  "; ? toggles help and headlines.\n" 'face 'org-agenda-dimmed-todo-face))
                     (concat
                      (propertize
                       (org-index--wrap
                        (concat
                         "Normal keys add to search word; <space> or <comma> start additional word; <backspace> erases last char, <C-backspace> last word; <return> jumps to heading, <tab> jumps to heading in other window, <S-return> jumps to matching line in index; all other keys end search." these-commands "\n"))
                       'face 'org-agenda-dimmed-todo-face)
                      org-index--headings)))
    
    ;; insert overlays for help text and to cover unsearched lines
    (setq help-overlay (make-overlay (point-min) begin))
    (overlay-put help-overlay 'display (car help-text))
    (setq org-index--occur-tail-overlay (make-overlay (point-max) (point-max)))
    (overlay-put org-index--occur-tail-overlay 'invisible t)

    ;; do not enter loop if number of days is requested
    (when days
      (goto-char begin)
      (org-index--hide-with-overlays (cons word words) lines-wanted days)
      (move-overlay org-index--occur-tail-overlay (org-index--occur-end-of-visible) (point-max))
      
      (goto-char begin)
      (setq done t))

    ;; main loop
    (while (not done)

      (if in-c-backspace
          (setq key "<backspace>")
        (setq search-text (mapconcat 'identity (reverse (cons word words)) ","))

        ;; read key, if selected frame has not changed
        (if (eq initial-frame (selected-frame))
            (progn
              (setq key-sequence
                    (let ((echo-keystrokes 0)
                          (full-prompt (format "%s%s%s"
                                               prompt
                                               search-text
                                               (if (string= search-text "") "" " "))))
                      (read-key-sequence full-prompt nil nil t t)))
              (setq key (key-description key-sequence))
              (setq key-sequence-raw (this-single-command-raw-keys)))
          (setq done t)
          (setq key-sequence nil)
          (setq key nil)
          (setq key-sequence-raw nil)))
      

      (cond


       ((string= key "<C-backspace>")
        (setq in-c-backspace t))


       ((member key (list "<backspace>" "DEL"))   ; erase last char

        (if (= (length word) 0)

            ;; nothing more to delete from current word; try next
            (progn
              (setq word (car words))
              (setq words (cdr words))
              (setq in-c-backspace nil))

          ;; some chars are left; shorten word
          (setq word (substring word 0 -1))
          (when (= (length word) 0) ; when nothing left, use next word from list
            (setq word (car words))
            (setq words (cdr words))
            (setq in-c-backspace nil))

          ;; free top list of overlays and remove list
          (org-index--unhide)
          (move-overlay org-index--occur-tail-overlay
                        (org-index--occur-end-of-visible)
                        (point-max))
          
          ;; make sure, point is still visible
          (goto-char begin)))


       ((member key (list "SPC" ",")) ; space or comma: enter an additional search word

        ;; push current word and clear, no need to change display
        (unless (string= word "")
          (setq words (cons word words))
          (setq word "")))


       ((string= key "?") ; question mark: toggle display of headlines and help
        (setq help-text (cons (cdr help-text) (car help-text)))
        (overlay-put help-overlay 'display (car help-text)))

       ((and (= (length key) 1)
             (aref printable-chars (elt key 0))) ; any printable char: add to current search word

        ;; add to word
        (setq word (concat word key))
                
        ;; make overlays to hide lines, that do not match longer word any more
        (goto-char begin)
        (org-index--hide-with-overlays (cons word words) lines-wanted days)
        (move-overlay org-index--occur-tail-overlay
                      (org-index--occur-end-of-visible)
                      (point-max))
        
        (goto-char begin)
                
        ;; make sure, point is on a visible line
        (line-move -1 t)
        (line-move 1 t))

       ;; anything else terminates input loop
       (t (setq done t))))

    ;; put back input event, that caused the loop to end
    (unless (string= key "C-g")
      (setq unread-command-events (listify-key-sequence key-sequence-raw))
      (message key))
    
    ;; For performance reasons do not show matching lines for rest of table. So no code here.
    
    ;; make permanent copy
    ;; copy visible lines
    (let ((lines-collected 0)
          keymap line all-lines all-lines-lbp header-lines lbp)

      (setq cursor-type t)
      (goto-char begin)
      (let ((inhibit-read-only t))
        (put-text-property begin (org-table-end) 'face nil))

      ;; collect all visible lines
      (while (and (not (eobp))
                  (< lines-collected lines-wanted))
        ;; skip over invisible lines
        (while (and (invisible-p (point))
                    (not (eobp)))
          (goto-char (1+ (overlay-end (car (overlays-at (point)))))))
        (setq lbp (line-beginning-position))
        (setq line (buffer-substring-no-properties lbp (line-end-position)))
        (unless (string= line "")
          (cl-incf lines-collected)
          (setq all-lines (cons (concat line
                                        "\n")
                                all-lines))
          (setq all-lines-lbp (cons lbp all-lines-lbp)))
        (forward-line 1))
        
      (kill-buffer org-index--occur-buffer-name) ; cannot keep this buffer; might become stale soon

      ;; create new buffer
      (setq occur-buffer (get-buffer-create org-index--occur-buffer-name))
      (pop-to-buffer-same-window occur-buffer)
      (insert org-index--headings)
      (setq header-lines (line-number-at-pos))

      ;; insert into new buffer
      (save-excursion
        (apply 'insert (reverse all-lines))
        (if (= lines-collected lines-wanted)
            (insert "\n(more lines omitted)\n")))
      (setq org-index--occur-lines-collected lines-collected)
      
      (org-mode)
      (setq truncate-lines t)
      (if all-lines (org-index--align-and-fontify-current-line (length all-lines)))
      (when (fboundp 'font-lock-ensure)
        (font-lock-ensure)
        (font-lock-flush))
      (when all-lines-lbp
          (while (not (org-at-table-p))
            (forward-line -1))
          (while all-lines-lbp
            (put-text-property (line-beginning-position) (line-end-position) 'org-index-lbp (car all-lines-lbp))
            (setq all-lines-lbp (cdr all-lines-lbp))
            (forward-line -1)))

      ;; prepare help text
      (goto-char (point-min))
      (forward-line (1- header-lines))
      (setq org-index--occur-help-overlay (make-overlay (point-min) (point)))
      (setq org-index--occur-help-text
            (cons
             (org-index--wrap
              (propertize (format "Search is done%s;    ? toggles help and headlines.\n" days-clause) 'face 'org-agenda-dimmed-todo-face))
             (concat
              (org-index--wrap
               (propertize
                (format
                 (concat (format "Search is done%s." days-clause)
                         (if (< lines-collected lines-wanted)
                             " Showing all %d matches for "
                           " Showing one window of matches for ")
                         "\"" search-text
                         "\". <return> jumps to heading, <tab> jumps to heading in other window, <S-return> jumps to matching line in index, <space> increments count.\n" these-commands "\n")
                 (length all-lines))
                'face 'org-agenda-dimmed-todo-face))
              org-index--headings)))
      
      (overlay-put org-index--occur-help-overlay 'display (car org-index--occur-help-text))

      ;; highlight words
      (mapc (lambda (w) (unless (or (not w) (string= w ""))
                     (let ((case-fold-search (not (string= w (downcase w)))))
                       (highlight-regexp (regexp-quote w) 'isearch))))
            (cons word words))

      (setq buffer-read-only t)

      ;; install keyboard-shortcuts
      (setq keymap (make-sparse-keymap))
      (set-keymap-parent keymap org-mode-map)

      (mapc (lambda (x) (define-key keymap (kbd x)
                     (lambda () (interactive)
                       (message "%s" (org-index--occur-action)))))
            (list "<return>" "RET"))

      (define-key keymap (kbd "<tab>")
        (lambda () (interactive)
          (message (org-index--occur-action t))))
      
      (define-key keymap (kbd "e")
        (lambda () (interactive)
          (message (org-index 'edit))))
      
      (define-key keymap (kbd "SPC")
        (lambda () (interactive)
          (org-index--refresh-parse-table)
          ;; increment in index
          (let ((ref (org-index--get-or-set-field 'ref))
                count)
            (org-index--on
                'ref ref
                (setq count (+ 1 (string-to-number (org-index--get-or-set-field 'count))))
                (org-index--get-or-set-field 'count (number-to-string count))
                (org-index--promote-current-line)
                (org-index--align-and-fontify-current-line))
            ;; increment in this buffer
            (let ((inhibit-read-only t))
              (org-index--get-or-set-field 'count (number-to-string count)))
            (message "Incremented count to %d" count))))
      
      (define-key keymap (kbd "<S-return>")
        (lambda () (interactive)
          (let ((pos (get-text-property (point) 'org-index-lbp)))
            (org-index--refresh-parse-table)
            (org-index--occur-test-stale pos)
            (pop-to-buffer org-index--buffer)
            (goto-char pos)
            (org-reveal t)
            (org-index--update-current-line)
            (beginning-of-line))))

      (define-key keymap (kbd "?")
        (lambda () (interactive)
          (org-index--refresh-parse-table)
          (setq-local org-index--occur-help-text (cons (cdr org-index--occur-help-text) (car org-index--occur-help-text)))
          (overlay-put org-index--occur-help-overlay 'display (car org-index--occur-help-text))))
    
      (use-local-map keymap))))


(defun org-index--occur-end-of-visible ()
  "End of visible stretch during occur"
  (if org-index--occur-stack
      (cdr (assoc :end-of-visible (car org-index--occur-stack)))
    (point-max)))


(defun org-index--occur-test-stale (pos)
  "Test, if current line in occur buffer has become stale at POS."
  (let (here there)
    (org-index--refresh-parse-table)
    (setq here (org-index--line-in-canonical-form))
    (with-current-buffer org-index--buffer
      (goto-char pos)
      (setq there (org-index--line-in-canonical-form)))
    (unless (string= here there)
      (error "Occur buffer has become stale; please repeat search"))))


(defun org-index--line-in-canonical-form ()
  "Return current line in its canonical form."
  (org-trim (substring-no-properties (replace-regexp-in-string "\s +" " " (buffer-substring (line-beginning-position) (line-beginning-position 2))))))


(defun org-index--wrap (text)
  "Wrap TEXT at fill column."
  (with-temp-buffer
    (insert text)
    (fill-region (point-min) (point-max) nil t)
    (buffer-string)))


(defun org-index--occur-action (&optional other)
  "Helper for `org-index--occur', find heading with ref or id; if OTHER, in other window; or copy yank column."
  (if (org-at-table-p)
      (let ((id (org-index--get-or-set-field 'id))
            (ref (org-index--get-or-set-field 'ref))
            (yank (org-index--get-or-set-field 'yank)))
        (if id
            (org-index--find-id id other)
          (if ref
              (progn
                (org-mark-ring-goto)
                (format "Found reference %s (no node is associated)" ref))
            (if yank
                (progn
                  (org-index--update-line (get-text-property (point) 'org-index-lbp))
                  (setq yank (replace-regexp-in-string (regexp-quote "\\vert") "|" yank nil 'literal))
                  (kill-new yank)
                  (org-mark-ring-goto)
                  (if (and (>= (length yank) 4) (string= (substring yank 0 4) "http"))
                      (progn
                        (browse-url yank)
                        (format "Opened '%s' in browser (and copied it too)" yank))
                    (format "Copied '%s' (no node is associated)" yank)))
              (error "Internal error, this line contains neither id, nor reference, nor text to yank")))))
    (message "Not at table")))


(defun org-index--hide-with-overlays (words lines-wanted days)
  "Hide lines that are currently visible and do not match WORDS; 
leave LINES-WANTED lines visible.
Argument DAYS hides older lines."
  (let ((lines-found 0)
        (end-of-visible (point))
        overlay overlays start matched places all-places)

    ;; main loop
    (while (and (not (eobp))
                (< lines-found lines-wanted))

      ;; skip invisible lines
      (while (and (not (eobp))
                  (and
                   (invisible-p (point))
                   (< (point) (overlay-start org-index--occur-tail-overlay))))
        (goto-char (overlay-end (car (overlays-at (point))))))

      ;; find stretch of lines, that are currently visible but should be invisible now
      (setq matched nil)
      (setq places nil)
      (setq start (point))
      (while (and (not (eobp))
                  (not (and
                        (invisible-p (point))
                        (< (point) (overlay-start org-index--occur-tail-overlay))))
                  ;; either regard words or days, but not both
                  (if days
                      (let ((last-accessed (org-index--get-or-set-field 'last-accessed)))
                        (if last-accessed
                            (not (and
                                  (<= (- (time-to-days (current-time))
                                         (time-to-days (org-read-date nil t last-accessed nil)))
                                      days)
                                  (setq matched t))) ; for its side effect
                          t))
                    (not (and (setq places (org-index--test-words words))
                              (setq matched t))))) ; for its side effect
        (forward-line 1))

      (setq all-places (append places all-places))

      ;; create overlay to hide this stretch
      (when (< start (point))           ; avoid creating an empty overlay
        (setq overlay (make-overlay start (point)))
        (overlay-put overlay 'invisible t)
        (setq overlays (cons overlay overlays)))

      ;; skip and count line, that matched
      (when matched
        (let ((inhibit-read-only t) (lbp (line-beginning-position)))
          (put-text-property lbp (line-end-position) 'face nil)
          (while places
            (put-text-property (caar places) (+ (caar places) (cdar places)) 'face 'isearch)
            (setq places (cdr places))))
        (forward-line 1)
        (setq end-of-visible (point))
        (cl-incf lines-found)))
    
    ;; put new list on top of stack
    (setq org-index--occur-stack
          (cons (list (cons :overlays overlays)
                      (cons :end-of-visible end-of-visible)
                      (cons :lines lines-found)
		      (cons :places all-places))
                org-index--occur-stack))

    lines-found))


(defun org-index--unhide ()
  "Unhide text that does has been hidden by `org-index--hide-with-overlays'."
  (let (places)
    (when org-index--occur-stack
      ;; delete overlays and make visible again
      (mapc (lambda (y)
              (delete-overlay y))
            (cdr (assoc :overlays (car org-index--occur-stack))))
      ;; remove latest highlights
      (setq places (cdr (assoc :places (car org-index--occur-stack))))
      (while places
        (let ((inhibit-read-only t))
          (put-text-property (caar places) (+ (caar places) (cdar places)) 'face nil))
        (setq places (cdr places)))
      ;; remove top of stack
      (setq org-index--occur-stack (cdr org-index--occur-stack))
      ;; redo older highlights
      (setq places (cdr (assoc :places (car org-index--occur-stack))))
      (while places
        (let ((inhibit-read-only t))
          (put-text-property (caar places) (+ (caar places) (cdar places)) 'face 'isearch))
        (setq places (cdr places))))))


(defun org-index--test-words (words)
  "Test current line for match against WORDS."
  (let ((lbp (line-beginning-position))
	     line dc-line places index)
    (setq line (buffer-substring lbp (line-beginning-position 2)))
    (setq dc-line (downcase line))
    (catch 'not-found
      (dolist (word words)
        (if (setq index (cl-search word (if (string= word (downcase word)) dc-line line)))
            (setq places (cons (cons (+ lbp index) (length word)) places))
          (throw 'not-found nil)))
      places)))


(defun org-index--create-new-line ()
  "Do the common work for `org-index-new-line' and `org-index'."

  ;; insert ref or id as last or first line, depending on sort-column
  (goto-char org-index--below-hline)
  (if (eq org-index-sort-by 'count)
      (progn
        (goto-char (org-table-end))
        (forward-line -1)
        (org-table-insert-row t))
    (org-table-insert-row))

  ;; insert some of the standard values
  (org-table-goto-column (org-index--column-num 'created))
  (org-insert-time-stamp nil nil t)
  (org-table-goto-column (org-index--column-num 'count))
  (insert "1"))


(defun org-index--sort-silent ()
  "Sort index for default column to remove any effects of temporary sorting."
  (unless org-index--inhibit-sort-idle
    (save-excursion
      (org-index--verify-id)
      (org-index--parse-table)
      (with-current-buffer org-index--buffer
        (save-excursion
          (goto-char org-index--below-hline)
          (org-index--do-sort-index org-index-sort-by)
          (remove-hook 'before-save-hook 'org-index--sort-silent))))))


(defun org-index--idle-prepare ()
  "For parsing table when idle."
  (org-index--verify-id)
  (org-index--parse-table most-positive-fixnum t))


(defun org-index--copy-visible (beg end)
  "Copy the visible parts of the region between BEG and END without adding it to `kill-ring'; copy of `org-copy-visible'."
  (let (snippets s)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(setq s (goto-char (point-min)))
	(while (not (= (point) (point-max)))
	  (goto-char (org-find-invisible))
	  (push (buffer-substring s (point)) snippets)
	  (setq s (goto-char (org-find-visible))))))
    (apply 'concat (nreverse snippets))))


(provide 'org-index)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-index.el ends here

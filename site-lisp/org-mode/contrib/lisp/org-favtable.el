;;; org-favtable.el --- Lookup table of favorite references and links

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Marc-Oliver Ihm <org-favtable@ferntreffer.de>
;; Keywords: hypermedia, matching
;; Requires: org
;; Download: http://orgmode.org/worg/code/elisp/org-favtable.el
;; Version: 2.2.0

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
;;  Mark and find your favorite things and locations in org easily: Create
;;  and update a lookup table of your references and links. Often used
;;  entries bubble to the top and entering some keywords displays only the
;;  matching entries. That way the right entry one can be picked easily.
;;
;;  References are essentially small numbers (e.g. "R237" or "-455-"),
;;  which are created by this package; they are well suited to be used
;;  outside of org. Links are just normal org-mode links.
;;
;;
;; Setup:
;;
;;  - Add these lines to your .emacs:
;;
;;    (require 'org-favtable)
;;    ;; Good enough to start, but later you should probably
;;    ;; change this id, as will be explained below
;;    (setq org-favtable-id "00e26bef-1929-4110-b8b4-7eb9c9ab1fd4")
;;    ;; Optionally assign a key. Pick your own favorite.
;;    (global-set-key (kbd "C-+") 'org-favtable)
;;
;;  - Just invoke `org-favtable', which will explain how to complete your
;;    setup by creating the necessary table of favorites.
;;
;;
;; Further reading:
;;
;;  Invoke `org-favtable' and pick one of its help options. You may also
;;  read the documentation of `org-favtable-id' for setup instructions, of
;;  `org-favtable' for regular usage and of `org-favtable--commands' for a
;;  list of available commands.
;;

;;; Change Log:

;;   [2013-02-28 Th] Version 2.2.0:
;;    - Allowed shortcuts like "h237" for command "head" with argument "237"
;;    - Integrated with org-mark-ring-goto
;;
;;   [2013-01-25 Fr] Version 2.1.0:
;;    - Added full support for links
;;    - New commands "missing" and "statistics"
;;    - Renamed the package from "org-reftable" to "org-favtable"
;;    - Additional columns are required (e.g. "link"). Error messages will
;;      guide you
;;
;;   [2012-12-07 Fr] Version 2.0.0:
;;    - The format of the table of favorites has changed ! You need to bring
;;      your existing table into the new format by hand (which however is
;;      easy and explained below)
;;    - Reference table can be sorted after usage count or date of last access
;;    - Ask user explicitly, which command to invoke
;;    - Renamed the package from "org-refer-by-number" to "org-reftable"

;;   [2012-09-22 Sa] Version 1.5.0:
;;    - New command "sort" to sort a buffer or region by reference number
;;    - New commands "highlight" and "unhighlight" to mark references

;;   [2012-07-13 Fr] Version 1.4.0:
;;    - New command "head" to find a headline with a reference number

;;   [2012-04-28 Sa] Version 1.3.0:
;;    - New commands occur and multi-occur
;;    - All commands can now be invoked explicitly
;;    - New documentation
;;    - Many bugfixes

;;   [2011-12-10 Sa] Version 1.2.0:
;;    - Fixed a bug, which lead to a loss of newly created reference numbers
;;    - Introduced single and double prefix arguments
;;    - Started this Change Log

;;; Code:

(require 'org-table)
(require 'cl)

(defvar org-favtable--version "2.2.0")
(defvar org-favtable--preferred-command nil)

(defvar org-favtable--commands '(occur head ref link enter leave goto + help reorder fill sort update highlight unhighlight missing statistics)
  "List of commands known to org-favtable:

Commands known:

  occur: If you supply a keyword (text): Apply emacs standard
    occur operation on the table of favorites; ask for a
    string (keyword) to select lines. Occur will only show you
    lines which contain the given keyword, so you can easily find
    the right one. You may supply a list of words seperated by
    comma (\",\"), to select lines that contain any or all of the
    given words.

    If you supply a reference number: Apply emacs standard
    multi-occur operation all org-mode buffers to search for a
    specific reference.

    You may also read the note at the end of this help on saving
    the keystroke RET to accept this frequent default command.

  head: If invoked outside the table of favorites, ask for a
    reference number and search for a heading containing it. If
    invoked within favtable dont ask; rather use the reference or
    link from the current line.

  ref: Create a new reference, copy any previously selected text.
    If already within reftable, fill in ref-column.

  link: Create a new line in reftable with a link to the current node.
    Do not populate the ref column; this can later be populated by
    calling the \"fill\" command from within the reftable.

  leave: Leave the table of favorites. If the last command has
    been \"ref\", the new reference is copied and ready to yank.
    This \"org-mark-ring-goto\" and can be called several times
    in succession.

  enter: Just enter the node with the table of favorites.

  goto: Search for a specific reference within the table of
    favorites.

  help: Show this list of commands.

  +: Show all commands including the less frequently used ones
    given below. If \"+\" is followd by enough letters of such a
    command (e.g. \"+fi\"), then this command is invoked
    directly.

  reorder: Temporarily reorder the table of favorites, e.g. by
    count, reference or last access.

  fill: If either ref or link is missing, fill it.

  sort: Sort a set of lines (either the active region or the
    whole buffer) by the references found in each line.

  update: For the given reference, update the line in the
    favtable.

  highlight: Highlight references in region or buffer.

  unhighlight: Remove highlights.

  missing : Search for missing reference numbers (which do not
    appear in the reference table). If requested, add additional
    lines for them, so that the command \"new\" is able to reuse
    them.

  statistics : Show some statistics (e.g. minimum and maximum
    reference) about favtable.



Two ways to save keystrokes:

When prompting for a command, org-favtable puts the most likely
one (e.g. \"occur\" or \"ref\") at the front of the list, so that
you may just type RET.

If this command needs additional input (like e.g. \"occur\"), you
may supply this input right away, although you are still beeing
prompted for the command. So do an occur for the string \"foo\",
you can just enter \"foo\" without even entering \"occur\".


Another way to save keystrokes applies if you want to choose a
command, that requrires a reference number (and would normally
prompt for it): In that case you may just enter enough characters
from your command, so that it appears first in the list of
matches; then immediately enter the number of the reference you
are searching for. So the input \"h237\" would execute the
command \"head\" for reference \"237\" right away.

")

(defvar org-favtable--commands-some '(occur head ref link leave enter goto + help))

(defvar org-favtable--columns nil)

(defvar org-favtable-id nil
  "Id of the Org-mode node, which contains the favorite table.

Read below, on how to set up things. See the help options
\"usage\" and \"commands\" for normal usage after setup.

Setup requires two steps:

 - Adjust your .emacs initialization file

 - Create a suitable org-mode node


Here are the lines, you need to add to your .emacs:

  (require 'org-favtable)
  ;; Good enough to start, but later you should probably
  ;; change this id, as will be explained below
  (setq org-favtable-id \"00e26bef-1929-4110-b8b4-7eb9c9ab1fd4\")
  ;; Optionally assign a key. Pick your own favorite.
  (global-set-key (kbd \"C-+\") 'org-favtable)

Do not forget to restart emacs to make these lines effective.


As a second step you need to create the org-mode node, where your
reference numbers and links will be stored. It may look like
this:

  * org-favtable
    :PROPERTIES:
    :ID:       00e26bef-1929-4110-b8b4-7eb9c9ab1fd4
    :END:


    |     |      | Comment, description, details  |         |         |               |
    | ref | link | ;c                             | count;s | created | last-accessed |
    |     | <4>  | <30>                           |         |         |               |
    |-----+------+--------------------------------+---------+---------+---------------|
    | R1  |      | My first reference             |         |         |               |


You may just copy this node into one of your org-files.  Many
things however can or should be adjusted:

 - The node needs not be a top level node.

 - Its name is completely at you choice. The node is found
   through its ID.

 - There are three lines of headings above the first hline. The
   first one is ignored by org-favtable, and you can use them to
   give meaningful names to columns; the second line contains
   configuration information for org-favtable; please read
   further below for its format. The third line is optional and
   may contain width-informations (e.g. <30>) only.

 - The sequence of columns does not matter. You may reorder them
   any way you like; e.g. make the comment-column the last
   columns within the table. Columns ar found by their name,
   which appears in the second heading-line.

 - You can add further columns or even remove the
   \"Comment\"-column. All other columns from the
   example (e.g. \"ref\", \"link\", \"count\", \"created\" and
   \"last-accessed\") are required.

 - Your references need not start at \"R1\"; However, having an
   initial row is required (it serves as a template for subsequent
   references).

 - Your reference need not have the form \"R1\"; you may just as
   well choose any text, that contains a single number,
   e.g. \"reference-{1}\" or \"#7\" or \"++17++\" or \"-344-\". The
   function `org-favtable' will inspect your first reference and
   create all subsequent references in the same way.

 - You may want to change the ID-Property of the node above and
   create a new one, which is unique (and not just a copy of
   mine). You need to change it in the lines copied to your .emacs
   too. However, this is not strictly required to make things
   work, so you may do this later, after trying out this package.


Optionally you may tweak the second header line to adjust
`org-favtable' a bit. In the example above it looks like this
 (with spaces collapsed):


    | ref | link | ;c | count;s | created | last-accessed |


The different fields have different meanings:

 - ref : This denotes the column which contains you references

 - link : Column for org-mode links, which can be used to access
   locations within your files.

 - ;c : The flag \"c\" (\"c\" for \"copy\") denotes this column
   as the one beeing copied on command \"leave\". In the example
   above, it is also the comment-column.

 - count;s : this is the column which counts, how many time this
   line has been accessed (which is the key-feature of this
   package). The flag \"s\" stands for \"sort\", so the table is
   sorted after this column. You may also sort after columns
   \"ref\" or \"last-accessed\".

 - created : Date when this line was created.

 - last-accessed : Date and time, when this line was last accessed.


After this two-step setup process you may invoke `org-favtable'
to create a new favorite. Read the help option \"usage\" for
instructions on normal usage, read the help option \"commands\"
for help on single commands.

")


(defvar org-favtable--text-to-yank nil)
(defvar org-favtable--last-action nil)
(defvar org-favtable--occur-buffer nil)
(defvar org-favtable--ref-regex nil)
(defvar org-favtable--ref-format nil)



(defun org-favtable  (&optional what search search-is-link)
  "Mark and find your favorite items and org-locations easily:
Create and update a lookup table of your favorite references and
links. Often used entries automatically bubble to the top of the
table; entering some keywords narrows it to just the matching
entries; that way the right one can be picked easily.

References are essentially small numbers (e.g. \"R237\" or
\"-455-\"), as created by this package; links are normal org-mode
links. Within org-favtable, both are denoted as favorites.


Read below for a detailed description of this function. See the
help option \"setup\" or read the documentation of
`org-favtable-id' for setup instructions.

The function `org-favtable' operates on a dedicated table (called
the table or favorites or favtable, for short) within a special
Org-mode node. The node has to be created as part of your initial
setup. Each line of the favorite table contains:

 - A reference (optional)

 - A link (optional)

 - A number; counting, how often each reference has been
   used. This number is updated automatically and the table can
   be sorted according to it, so that most frequently used
   references appear at the top of the table and can be spotted
   easily.

 - Its respective creation date

 - Date and time of last access. This column can alternatively be
   used to sort the table.

To be useful, your table of favorites should probably contain a
column with comments too, which allows lines to be selected by
keywords.

The table of favorites is found through the id of the containing
node; this id should be stored within `org-favtable-id' (see there
for details).


The function `org-favtable' is the only interactive function of
this package and its sole entry point; it offers several commands
to create, find and look up these favorites (references and
links). All of them are explained within org-favtable's help.


Finally, org-favtable can also be invoked from elisp; the two
optional arguments accepted are:

         search : string to search for
           what : symbol of the command to invoke
 search-is-link : t, if argument search is actually a link

An example would be:

 (org-favtable \"237\" 'head)   ;; find heading with ref 237

"

  (interactive "P")

  (let (within-node        ; True, if we are within node with favtable
        result-is-visible  ; True, if node or occur is visible in any window
        ref-node-buffer-and-point ; cons with buffer and point of favorites node
        below-cursor              ; word below cursor
        active-region             ; active region (if any)
        link-id                   ; link of starting node, if required
        guarded-search            ; with guard against additional digits
        search-is-ref             ; true, if search is a reference
        commands                ; currently active set of selectable commands
        what-adjusted           ; True, if we had to adjust what
        what-input    ; Input on what question (need not necessary be "what")
        reorder-once  ; Column to use for single time sorting
        parts         ; Parts of a typical reference number (which
                                                  ; need not be a plain number); these are:
        head               ; Any header before number (e.g. "R")
        maxref             ; Maximum number from reference table (e.g. "153")
        tail               ; Tail after number (e.g. "}" or "")
        ref-regex          ; Regular expression to match a reference
        has-reuse          ; True, if table contains a line for reuse
        numcols            ; Number of columns in favtable
        kill-new-text      ; Text that will be appended to kill ring
        message-text       ; Text that will be issued as an explanation,
                           ; what we have done
        initial-ref-or-link      ; Initial position in reftable
        )

    ;;
    ;; Examine current buffer and location, before turning to favtable
    ;;

    ;; Get the content of the active region or the word under cursor
    (if (and transient-mark-mode
             mark-active)
        (setq active-region (buffer-substring (region-beginning) (region-end))))
    (setq below-cursor (thing-at-point 'symbol))


    ;; Find out, if we are within favable or not
    (setq within-node (string= (org-id-get) org-favtable-id))

    ;; Find out, if point in any window is within node with favtable
    (mapc (lambda (x) (with-current-buffer (window-buffer x)
                        (when (or
                               (string= (org-id-get) org-favtable-id)
                               (eq (window-buffer x)
                                   org-favtable--occur-buffer))
                          (setq result-is-visible t))))
          (window-list))



    ;;
    ;; Get decoration of references and highest reference from favtable
    ;;


    ;; Save initial ref or link
    (if (and within-node
             (org-at-table-p))
        (setq initial-ref-or-link
              (or (org-favtable--get-field 'ref)
                  (org-favtable--get-field 'link))))

    ;; Find node
    (setq ref-node-buffer-and-point (org-favtable--id-find))
    (unless ref-node-buffer-and-point
      (org-favtable--report-setup-error
       (format "Cannot find node with id \"%s\"" org-favtable-id)))

    ;; Get configuration of reftable; catch errors
    (let ((error-message
           (catch 'content-error

             (with-current-buffer (car ref-node-buffer-and-point)
               (save-excursion
                 (unless (string= (org-id-get) org-favtable-id)
                   (goto-char (cdr ref-node-buffer-and-point)))

                 ;; parse table while still within buffer
                 (setq parts (org-favtable--parse-and-adjust-table)))

               nil))))
      (when error-message
        (org-pop-to-buffer-same-window (car ref-node-buffer-and-point))
        (org-reveal)
        (error error-message)))

    ;; Give names to parts of configuration
    (setq head (nth 0 parts))
    (setq maxref (nth 1 parts))
    (setq tail (nth 2 parts))
    (setq numcols (nth 3 parts))
    (setq ref-regex (nth 4 parts))
    (setq has-reuse (nth 5 parts))
    (setq org-favtable--ref-regex ref-regex)
    (setq org-favtable--ref-format (concat head "%d" tail))

    ;;
    ;; Find out, what we are supposed to do
    ;;

    (if (equal what '(4)) (setq what 'leave))

    ;; Set preferred action, that will be the default choice
    (setq org-favtable--preferred-command
          (if within-node
              (if (memq org-favtable--last-action '(ref link))
                  'leave
                'occur)
            (if active-region
                'ref
              (if (and below-cursor (string-match ref-regex below-cursor))
                  'occur
                nil))))

    ;; Ask user, what to do
    (unless what
      (setq commands (copy-list org-favtable--commands-some))
      (while (progn
               (setq what-input
                     (org-icompleting-read
                      "Please choose: "
                      (mapcar 'symbol-name
                              ;; Construct unique list of commands with
                              ;; preferred one at front
                              (delq nil (delete-dups
                                         (append
                                          (list org-favtable--preferred-command)
                                          commands))))
                      nil nil))


               ;; if input starts with "+", any command (not only some) may follow
               ;; this allows input like "+sort" to be accepted
               (when (string= (substring what-input 0 1) "+")
                 ;; make all commands available for selection
                 (setq commands (copy-list org-favtable--commands))
                 (unless (string= what-input "+")
                   ;; not just "+", use following string
                   (setq what-input (substring what-input 1))

                   (let ((completions
                          ;; get list of possible completions for what-input
                          (all-completions what-input (mapcar 'symbol-name commands))))
                     ;; use it, if unambigously
                     (if (= (length completions) 1)
                         (setq what-input (car completions))))))


               ;; if input ends in digits, save them away and do completions on head of input
               ;; this allows input like "h224" to be accepted
               (when (string-match "^\\([^0-9+]\\)\\([0-9]+\\)\\s *$" what-input)
                 ;; use first match as input, even if ambigously
                 (setq org-favtable--preferred-command
                       (intern (first (all-completions (match-string 1 what-input)
                                                       (mapcar 'symbol-name commands)))))
                 ;; use digits as argument to commands
                 (setq what-input (format org-favtable--ref-format
                                          (string-to-number (match-string 2 what-input)))))

               (setq what (intern what-input))

               ;; user is not required to input one of the commands; if
               ;; not, take the first one and use the original input for
               ;; next question
               (if (memq what commands)
                   ;; input matched one element of list, dont need original
                   ;; input any more
                   (setq what-input nil)
                 ;; what-input will be used for next question, use first
                 ;; command for what
                 (setq what (or org-favtable--preferred-command
                                (first commands)))
                 ;; remove any trailing dot, that user might have added to
                 ;; disambiguate his input
                 (if (equal (substring what-input -1) ".")
                     ;; but do this only, if dot was really necessary to
                     ;; disambiguate
                     (let ((shortened-what-input (substring what-input 0 -1)))
                       (unless (test-completion shortened-what-input
                                                (mapcar 'symbol-name
                                                        commands))
                         (setq what-input shortened-what-input)))))

               ;; ask for reorder in loop, because we have to ask for
               ;; what right again
               (if (eq what 'reorder)
                   (setq reorder-once
                         (intern
                          (org-icompleting-read
                           "Please choose column to reorder reftable once: "
                           (mapcar 'symbol-name '(ref count last-accessed))
                           nil t))))

               ;; maybe ask initial question again
               (memq what '(reorder +)))))


    ;;
    ;; Get search, if required
    ;;

    ;; These actions need a search string:
    (when (memq what '(goto occur head update))

      ;; Maybe we've got a search string from the arguments
      (unless search
        (let (search-from-table
              search-from-cursor)

          ;; Search string can come from several sources:
          ;; From ref column of table
          (when within-node
            (setq search-from-table (org-favtable--get-field 'ref)))
          ;; From string below cursor
          (when (and (not within-node)
                     below-cursor
                     (string-match (concat "\\(" ref-regex "\\)")
                                   below-cursor))
            (setq search-from-cursor (match-string 1 below-cursor)))

          ;; Depending on requested action, get search from one of the sources above
          (cond ((eq what 'goto)
                 (setq search (or what-input search-from-cursor)))
                ((memq what '(head occur))
                 (setq search (or what-input search-from-table search-from-cursor))))))


      ;; If we still do not have a search string, ask user explicitly
      (unless search

        (if what-input
            (setq search what-input)
          (setq search (read-from-minibuffer
                        (cond ((memq what '(occur head))
                               "Text or reference number to search for: ")
                              ((eq what 'goto)
                               "Reference number to search for, or enter \".\" for id of current node: ")
                              ((eq what 'update)
                               "Reference number to update: ")))))

        (if (string-match "^\\s *[0-9]+\\s *$" search)
            (setq search (format "%s%s%s" head (org-trim search) tail))))

      ;; Clean up and examine search string
      (if search (setq search (org-trim search)))
      (if (string= search "") (setq search nil))
      (setq search-is-ref (string-match ref-regex search))

      ;; Check for special case
      (when (and (memq what '(head goto))
                 (string= search "."))
        (setq search (org-id-get))
        (setq search-is-link t))

      (when search-is-ref
        (setq guarded-search (org-favtable--make-guarded-search search)))

      ;;
      ;; Do some sanity checking before really starting
      ;;

      ;; Correct requested action, if nothing to search
      (when (and (not search)
                 (memq what '(search occur head)))
        (setq what 'enter)
        (setq what-adjusted t))

      ;; For a proper reference as input, we do multi-occur
      (if (and (string-match ref-regex search)
               (eq what 'occur))
          (setq what 'multi-occur))

      ;; Check for invalid combinations of arguments; try to be helpful
      (when (and (memq what '(head goto))
                 (not search-is-link)
                 (not search-is-ref))
        (error "Can do '%s' only for a reference or link (not '%s'), try 'occur' to search for text" what search)))


    ;;
    ;; Prepare
    ;;

    ;; Get link if required before moving in
    (if (eq what 'link)
        (setq link-id (org-id-get-create)))

    ;; Move into table, if outside
    (when (memq what '(enter ref link goto occur multi-occur missing statistics))

      ;; Support orgmode-standard of going back (buffer and position)
      (org-mark-ring-push)

      ;; Switch to favtable
      (org-pop-to-buffer-same-window (car ref-node-buffer-and-point))
      (goto-char (cdr ref-node-buffer-and-point))
      (show-subtree)
      (org-show-context)

      ;; sort favtable
      (org-favtable--sort-table reorder-once))

    ;; Goto back to initial ref, because reformatting of table above might
    ;; have moved point
    (when initial-ref-or-link
      (while (and (org-at-table-p)
                  (not (or
                        (string= initial-ref-or-link (org-favtable--get-field 'ref))
                        (string= initial-ref-or-link (org-favtable--get-field 'link)))))
        (forward-line))
      ;; did not find ref, go back to top
      (if (not (org-at-table-p)) (goto-char top)))


    ;;
    ;; Actually do, what is requested
    ;;

    (cond


     ((eq what 'help)

      (let ((help-what
             ;; which sort of help ?
             (intern
              (concat
               "help-"
               (org-icompleting-read
                "Help on: "
                (mapcar 'symbol-name '(commands usage setup version example))
                nil t)))))

        ;; help is taken from docstring of functions or variables
        (cond ((eq help-what 'help-commands)
               (org-favtable--show-help 'org-favtable--commands))
              ((eq help-what 'help-usage)
               (org-favtable--show-help 'org-favtable))
              ((eq help-what 'help-setup)
               (org-favtable--show-help 'org-favtable-id))
              ((eq help-what 'help-version)
               (org-favtable-version)))))


     ((eq what 'multi-occur)

      ;; Conveniently position cursor on number to search for
      (org-favtable--goto-top)
      (let (found (initial (point)))
        (while (and (not found)
                    (forward-line)
                    (org-at-table-p))
          (save-excursion
            (setq found (string= search
                                 (org-favtable--get-field 'ref)))))
        (if found
            (org-favtable--update-line nil)
          (goto-char initial)))

      ;; Construct list of all org-buffers
      (let (buff org-buffers)
        (dolist (buff (buffer-list))
          (set-buffer buff)
          (if (string= major-mode "org-mode")
              (setq org-buffers (cons buff org-buffers))))

        ;; Do multi-occur
        (multi-occur org-buffers guarded-search)
        (if (get-buffer "*Occur*")
            (progn
              (setq message-text (format "multi-occur for '%s'" search))
              (setq org-favtable--occur-buffer (get-buffer "*Occur*"))
              (other-window 1)
              (toggle-truncate-lines 1))
          (setq message-text (format "Did not find '%s'" search)))))


     ((eq what 'head)

      (let (link)
        ;; link either from table or passed in as argument

        ;; try to get link
        (if search-is-link
            (setq link (org-trim search))
          (if (and within-node
                   (org-at-table-p))
              (setq link (org-favtable--get-field 'link))))

        ;; use link if available
        (if (and link
                 (not (string= link "")))
            (progn
              (org-id-goto link)
              (org-favtable--update-line search)
              (setq message-text "Followed link"))

          (message (format "Scanning headlines for '%s' ..." search))
          (let (buffer point)
            (if (catch 'found
                  (progn
                    ;; loop over all headlines, stop on first match
                    (org-map-entries
                     (lambda ()
                       (when (looking-at (concat ".*" guarded-search))
                         ;; remember location and bail out
                         (setq buffer (current-buffer))
                         (setq point (point))
                         (throw 'found t)))
                     nil 'agenda)
                    nil))

                (progn
                  (org-favtable--update-line search)
                  (setq message-text (format "Found '%s'" search))
                  (org-pop-to-buffer-same-window buffer)
                  (goto-char point)
                  (org-reveal))
              (setq message-text (format "Did not find '%s'" search)))))))


     ((eq what 'leave)

      (when result-is-visible

        ;; If we are within the occur-buffer, switch over to get current line
        (if (and (string= (buffer-name) "*Occur*")
                 (eq org-favtable--last-action 'occur))
            (occur-mode-goto-occurrence)))

      (setq kill-new-text org-favtable--text-to-yank)
      (setq org-favtable--text-to-yank nil)

      ;; If "leave" has been called two times in succession, make
      ;; org-mark-ring-goto believe it has been called two times too
      (if (eq org-favtable--last-action 'leave)
          (let ((this-command nil) (last-command nil))
            (org-mark-ring-goto 1))
        (org-mark-ring-goto 0)))


     ((eq what 'goto)

      ;; Go downward in table to requested reference
      (let (found (initial (point)))
        (org-favtable--goto-top)
        (while (and (not found)
                    (forward-line)
                    (org-at-table-p))
          (save-excursion
            (setq found
                  (string= search
                           (org-favtable--get-field
                            (if search-is-link 'link 'ref))))))
        (if found
            (progn
              (setq message-text (format "Found '%s'" search))
              (org-favtable--update-line nil)
              (org-table-goto-column (org-favtable--column-num 'ref))
              (if (looking-back " ") (backward-char))
              ;; remember string to copy
              (setq org-favtable--text-to-yank
                    (org-trim (org-table-get-field (org-favtable--column-num 'copy)))))
          (setq message-text (format "Did not find '%s'" search))
          (goto-char initial)
          (forward-line)
          (setq what 'missed))))


     ((eq what 'occur)

      ;; search for string: occur
      (let (search-regexp
            all-or-any
            (search-words (split-string search "," t)))

        (if (< (length search-words) 2)
            ;; only one word to search; use it as is
            (setq search-regexp search)
          ;; construct regexp to match any of the words (maybe throw out some matches later)
          (setq search-regexp
                (mapconcat (lambda (x) (concat "\\(" x "\\)")) search-words "\\|"))
          (setq all-or-any
                (intern
                 (org-icompleting-read
                  "Two or more words have been specified; show lines, that match: " '("all" "any")))))

        (save-restriction
          (org-narrow-to-subtree)
          (occur search-regexp)
          (widen)
          (if (get-buffer "*Occur*")
              (with-current-buffer "*Occur*"

                ;; install helpful keyboard-shortcuts within occur-buffer
                (let ((keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap occur-mode-map)

                  (define-key keymap (kbd "RET")
                    (lambda () (interactive)
                      (org-favtable--occur-helper 'head)))

                  (define-key keymap (kbd "<C-return>")
                    (lambda () (interactive)
                      (org-favtable--occur-helper 'multi-occur)))

                  (define-key keymap (kbd "<M-return>")
                    (lambda () (interactive)
                      (org-favtable--occur-helper 'goto)))

                  (define-key keymap (kbd "<C-M-return>")
                    (lambda () (interactive)
                      (org-favtable--occur-helper 'update)))

                  (use-local-map keymap))

                ;; Brush up occur buffer
                (other-window 1)
                (toggle-truncate-lines 1)
                (let ((inhibit-read-only t))
                  ;; insert some help text
                  (insert (substitute-command-keys
                           "Type RET to find heading, C-RET for multi-occur, M-RET to go to occurence and C-M-RET to update line in reftable.\n\n"))
                  (forward-line 1)

                  ;; when matching all of multiple words, remove all lines that do not match one of the words
                  (when (eq all-or-any 'all)
                    (mapc (lambda (x) (keep-lines x)) search-words))

                  ;; replace description from occur
                  (when all-or-any
                    (forward-line -1)
                    (kill-line)
                    (let ((count (- (count-lines (point) (point-max)) 1)))
                      (insert (format "%d %s for %s of %s"
                                      count
                                      (if (= count 1) "match" "matches")
                                      all-or-any
                                      search)))
                    (forward-line)
                    (beginning-of-line))

                  ;; Record link or reference for each line in
                  ;; occur-buffer, that is linked into reftable. Because if
                  ;; we later realign the reftable and then reuse the occur
                  ;; buffer, the original links might point nowehere.
                  (save-excursion
                    (while (not (eq (point) (point-max)))
                      (let ((beg (line-beginning-position))
                            (end (line-end-position))
                            pos ref link)

                        ;; occur has saved the position into a special property
                        (setq pos (get-text-property (point) 'occur-target))
                        (when pos
                          ;; but this property might soon point nowhere; so retrieve ref-or-link instead
                          (with-current-buffer (marker-buffer pos)
                            (goto-char pos)
                            (setq ref (org-favtable--get-field 'ref))
                            (setq link (org-favtable--get-field 'link))))
                        ;; save as text property
                        (put-text-property beg end 'org-favtable--ref ref)
                        (put-text-property beg end 'org-favtable--link link))
                      (forward-line))))

                (setq message-text
                      (format  "Occur for '%s'" search)))
            (setq message-text
                  (format "Did not find any matches for '%s'" search))))))


     ((memq what '(ref link))

      ;; add a new row (or reuse existing one)
      (let (new)

        (when (eq what 'ref)
            ;; go through table to find first entry to be reused
          (when has-reuse
            (org-favtable--goto-top)
            ;; go through table
            (while (and (org-at-table-p)
                        (not new))
              (when (string=
                     (org-favtable--get-field 'count)
                     ":reuse:")
                (setq new (org-favtable--get-field 'ref))
                (if new (org-table-kill-row)))
              (forward-line)))

          ;; no ref to reuse; construct new reference
          (unless new
            (setq new (format "%s%d%s" head (1+ maxref) tail)))

          ;; remember for org-mark-ring-goto
          (setq org-favtable--text-to-yank new))

        ;; insert ref or link as very first row
        (org-favtable--goto-top)
        (org-table-insert-row)

        ;; fill special columns with standard values
        (when (eq what 'ref)
          (org-table-goto-column (org-favtable--column-num 'ref))
          (insert new))
        (when (eq what 'link)
          (org-table-goto-column (org-favtable--column-num 'link))
          (insert link-id))
        (org-table-goto-column (org-favtable--column-num 'created))
        (org-insert-time-stamp nil nil t)

        ;; goto first empty field
        (unless (catch 'empty
                  (dotimes (col numcols)
                    (org-table-goto-column (+ col 1))
                    (if (string= (org-trim (org-table-get-field)) "")
                        (throw 'empty t))))
          ;; none found, goto first
          (org-table-goto-column 1))

        (org-table-align)
        (if active-region (setq kill-new-text active-region))
        (if (eq what 'ref)
            (setq message-text (format "Adding a new row with ref '%s'" new))
          (setq message-text (format "Adding a new row linked to '%s'" link-id)))))


     ((eq what 'enter)

      ;; simply go into table
      (org-favtable--goto-top)
      (show-subtree)
      (recenter)
      (if what-adjusted
          (setq message-text "Nothing to search for; at favtable")
        (setq message-text "At favtable")))


     ((eq what 'fill)

      ;; check, if within reftable
      (unless (and within-node
                   (org-at-table-p))
        (error "Not within table of favorites"))

      ;; applies to missing refs and missing links alike
      (let ((ref (org-favtable--get-field 'ref))
            (link (org-favtable--get-field 'link)))

        (if (and (not ref)
                 (not link))
            ;; have already checked this during parse, check here anyway
            (error "Columns ref and link are both empty in this line"))

        ;; fill in new ref
        (if (not ref)
            (progn
              (setq kill-new-text (format "%s%d%s" head (1+ maxref) tail))
              (org-favtable--get-field 'ref kill-new-text)
              ;; remember for org-mark-ring-goto
              (setq org-favtable--text-to-yank kill-new-text)
              (org-id-goto link)
              (setq message-text "Filled reftable field with new reference"))

          ;; fill in new link
          (if (not link)
              (progn
                (setq guarded-search (org-favtable--make-guarded-search ref))
                (message (format "Scanning headlines for '%s' ..." ref))
                (let (link)
                  (if (catch 'found
                        (org-map-entries
                         (lambda ()
                           (when (looking-at (concat ".*" guarded-search))
                             (setq link (org-id-get-create))
                             (throw 'found t)))
                         nil 'agenda)
                        nil)

                      (progn
                        (org-favtable--get-field 'link link)
                        (setq message-text "Inserted link"))

                    (setq message-text (format "Did not find reference '%s'" ref)))))

            ;; nothing is missing
            (setq message-text "Columns 'ref' and 'link' are already filled; nothing to do")))))


     ((eq what 'sort)

      ;; sort lines according to contained reference
      (let (begin end where)
        (catch 'aborted
          ;; either active region or whole buffer
          (if (and transient-mark-mode
                   mark-active)
              ;; sort only region
              (progn
                (setq begin (region-beginning))
                (setq end (region-end))
                (setq where "region"))
            ;; sort whole buffer
            (setq begin (point-min))
            (setq end (point-max))
            (setq where "whole buffer")
            ;; make sure
            (unless (y-or-n-p "Sort whole buffer ")
              (setq message-text "Sort aborted")
              (throw 'aborted nil)))

          (save-excursion
            (save-restriction
              (goto-char (point-min))
              (narrow-to-region begin end)
              (sort-subr nil 'forward-line 'end-of-line
                         (lambda ()
                           (if (looking-at (concat ".*"
                                                   (org-favtable--make-guarded-search ref-regex 'dont-quote)))
                               (string-to-number (match-string 1))
                             0))))
            (highlight-regexp ref-regex)
            (setq message-text (format "Sorted %s from character %d to %d, %d lines"
                                       where begin end
                                       (count-lines begin end)))))))


     ((eq what 'update)

      ;; simply update line in reftable
      (save-excursion
        (let ((ref-or-link (if search-is-link "link" "reference")))
          (beginning-of-line)
          (if (org-favtable--update-line search)
              (setq message-text (format "Updated %s '%s'" ref-or-link search))
            (setq message-text (format "Did not find %s '%s'" ref-or-link search))))))


     ((eq what 'parse)

      ;; Just parse the reftable, which is already done, so nothing to do
      )


     ((memq what '(highlight unhighlight))

      (let ((where "buffer"))
        (save-excursion
          (save-restriction
            (when (and transient-mark-mode
                       mark-active)
              (narrow-to-region (region-beginning) (region-end))
              (setq where "region"))

            (if (eq what 'highlight)
                (progn
                  (highlight-regexp ref-regex)
                  (setq message-text (format "Highlighted references in %s" where)))
              (unhighlight-regexp ref-regex)
              (setq message-text (format "Removed highlights for references in %s" where)))))))


     ((memq what '(missing statistics))

      (org-favtable--goto-top)
      (let (missing
            ref-field
            ref
            min
            max
            (total 0))

        ;; start with list of all references
        (setq missing (mapcar (lambda (x) (format "%s%d%s" head x tail))
                              (number-sequence 1 maxref)))

        ;; go through table and remove all refs, that we see
        (while (and (forward-line)
                    (org-at-table-p))

          ;; get ref-field and number
          (setq ref-field (org-favtable--get-field 'ref))
          (if (and ref-field
                   (string-match ref-regex ref-field))
              (setq ref (string-to-number (match-string 1 ref-field))))

          ;; remove existing refs from list
          (if ref-field (setq missing (delete ref-field missing)))

          ;; record min and max
          (if (or (not min) (< ref min)) (setq min ref))
          (if (or (not max) (> ref max)) (setq max ref))

          ;; count
          (setq total (1+ total)))

        ;; insert them, if requested
        (forward-line -1)
        (if (eq what 'statistics)

            (setq message-text (format "Found %d references from %s to %s. %d references below highest do not appear in table. "
                                       total
                                       (format org-favtable--format min)
                                       (format org-favtable--format max)
                                       (length missing)))

          (if (y-or-n-p (format "Found %d missing references; do you wish to append them to the table of favorites"
                                (length missing)))
              (let (type)
                (setq type (org-icompleting-read
                            "Insert new lines for reuse by command \"new\" or just as missing ? " '("reuse" "missing")))
                (mapc (lambda (x)
                        (let (org-table-may-need-update) (org-table-insert-row t))
                        (org-favtable--get-field 'ref x)
                        (org-favtable--get-field 'count (format ":%s:" type)))
                      missing)
                (org-table-align)
                (setq message-text (format "Inserted %d new lines for missing refernces" (length missing))))
            (setq message-text (format "%d missing references." (length missing)))))))


     (t (error "This is a bug: unmatched case '%s'" what)))


    ;; remember what we have done for next time
    (setq org-favtable--last-action what)

    ;; tell, what we have done and what can be yanked
    (if kill-new-text (setq kill-new-text
                            (substring-no-properties kill-new-text)))
    (if (string= kill-new-text "") (setq kill-new-text nil))
    (let ((m (concat
              message-text
              (if (and message-text kill-new-text)
                  " and r"
                (if kill-new-text "R" ""))
              (if kill-new-text (format "eady to yank '%s'" kill-new-text) ""))))
      (unless (string= m "") (message m)))
    (if kill-new-text (kill-new kill-new-text))))



(defun org-favtable--parse-and-adjust-table ()

  (let ((maxref 0)
        top
        bottom
        ref-field
        link-field
        parts
        numcols
        head
        tail
        ref-regex
        has-reuse
        initial-point)

    (setq initial-point (point))
    (org-favtable--goto-top)
    (setq top (point))

    (goto-char top)

    ;; count columns
    (org-table-goto-column 100)
    (setq numcols (- (org-table-current-column) 1))

    ;; get contents of columns
    (forward-line -2)
    (unless (org-at-table-p)
      (org-favtable--report-setup-error
       "Table of favorites starts with a hline" t))

    ;; check for optional line consisting solely of width specifications
    (beginning-of-line)
    (if (looking-at "\\s *|\\(\\(\\s *|\\)\\|\\(\\s *<[0-9]+>\\s *|\\)\\)+\\s *$")
        (forward-line -1))
    (org-table-goto-column 1)

    (setq org-favtable--columns (org-favtable--parse-headings numcols))

    ;; Go beyond end of table
    (while (org-at-table-p) (forward-line 1))

    ;; Kill all empty rows at bottom
    (while (progn
             (forward-line -1)
             (org-table-goto-column 1)
             (and
              (not (org-favtable--get-field 'ref))
              (not (org-favtable--get-field 'link))))
      (org-table-kill-row))
    (forward-line)
    (setq bottom (point))
    (forward-line -1)

    ;; Retrieve any decorations around the number within the first nonempty ref-field
    (goto-char top)
    (while (and (org-at-table-p)
                (not (setq ref-field (org-favtable--get-field 'ref))))
      (forward-line))

    ;; Some Checking
    (unless ref-field
      (org-favtable--report-setup-error
       "No line of reference column contains a number" t))

    (unless (string-match "^\\([^0-9]*\\)\\([0-9]+\\)\\([^0-9]*\\)$" ref-field)
      (org-favtable--report-setup-error
       (format "First reference in table table of favorites ('%s') does not contain a number" ref-field) t))


    ;; These are the decorations used within the first ref of favtable
    (setq head (match-string 1 ref-field))
    (setq tail (match-string 3 ref-field))
    (setq ref-regex (concat (regexp-quote head)
                            "\\([0-9]+\\)"
                            (regexp-quote tail)))

    ;; Go through table to find maximum number and do some checking
    (let ((ref 0))

      (while (org-at-table-p)

        (setq ref-field (org-favtable--get-field 'ref))
        (setq link-field (org-favtable--get-field 'link))

        (if (and (not ref-field)
                 (not link-field))
            (throw 'content-error "Columns ref and link are both empty in this line"))

        (if ref-field
            (if (string-match ref-regex ref-field)
                ;; grab number
                (setq ref (string-to-number (match-string 1 ref-field)))
              (throw 'content-error "Column ref does not contain a number")))

        ;; check, if higher ref
        (if (> ref maxref) (setq maxref ref))

        ;; check if ref is ment for reuse
        (if (string= (org-favtable--get-field 'count) ":reuse:")
            (setq has-reuse 1))

        (forward-line 1)))

    ;; sort used to be here

    (setq parts (list head maxref tail numcols ref-regex has-reuse))

    ;; go back to top of table
    (goto-char top)

    parts))



(defun org-favtable--sort-table (sort-column)

  (unless sort-column (setq sort-column (org-favtable--column-num 'sort)))

  (let (top
        bottom
        ref-field
        count-field
        count-special)


    ;; get boundaries of table
    (org-favtable--goto-top)
    (forward-line 0)
    (setq top (point))
    (while (org-at-table-p) (forward-line))
    (setq bottom (point))

    (save-restriction
      (narrow-to-region top bottom)
      (goto-char top)
      (sort-subr t
                 'forward-line
                 'end-of-line
                 (lambda ()
                   (let (ref
                         (ref-field (or (org-favtable--get-field 'ref) ""))
                         (count-field (or (org-favtable--get-field 'count) ""))
                         (count-special 0))

                     ;; get reference with leading zeroes, so it can be
                     ;; sorted as text
                     (string-match org-favtable--ref-regex ref-field)
                     (setq ref (format
                                "%06d"
                                (string-to-number
                                 (or (match-string 1 ref-field)
                                     "0"))))

                     ;; find out, if special token in count-column
                     (setq count-special (format "%d"
                                                 (- 2
                                                    (length (member count-field '(":missing:" ":reuse:"))))))

                     ;; Construct different sort-keys according to
                     ;; requested sort column; prepend count-special to
                     ;; sort special entries at bottom of table, append ref
                     ;; as a secondary sort key
                     (cond

                      ((eq sort-column 'count)
                       (concat count-special
                               (format
                                "%08d"
                                (string-to-number (or (org-favtable--get-field 'count)
                                                      "")))
                               ref))

                      ((eq sort-column 'last-accessed)
                       (concat count-special
                               (org-favtable--get-field 'last-accessed)
                               " "
                               ref))

                      ((eq sort-column 'ref)
                       (concat count-special
                               ref))

                      (t (error "This is a bug: unmatched case '%s'" sort-column)))))

                 nil 'string<)))

  ;; align table
  (org-table-align))


(defun org-favtable--goto-top ()

  ;; go to heading of node
  (while (not (org-at-heading-p)) (forward-line -1))
  (forward-line 1)
  ;; go to table within node, but make sure we do not get into another node
  (while (and (not (org-at-heading-p))
              (not (org-at-table-p))
              (not (eq (point) (point-max))))
    (forward-line 1))

  ;; check, if there really is a table
  (unless (org-at-table-p)
    (org-favtable--report-setup-error
     (format "Cannot find favtable within node %s" org-favtable-id) t))

  ;; go to first hline
  (while (and (not (org-at-table-hline-p))
              (org-at-table-p))
    (forward-line 1))

  ;; and check
  (unless (org-at-table-hline-p)
    (org-favtable--report-setup-error
     "Cannot find hline within table of favorites" t))

  (forward-line 1)
  (org-table-goto-column 1))



(defun org-favtable--id-find ()
  "Find org-favtable-id"
  (let ((marker (org-id-find org-favtable-id 'marker))
        marker-and-buffer)

    (if marker
        (progn
          (setq marker-and-buffer (cons (marker-buffer marker) (marker-position marker)))
          (move-marker marker nil)
          marker-and-buffer)
      nil)))



(defun org-favtable--parse-headings (numcols)

  (let (columns)

    ;; Associate names of special columns with column-numbers
    (setq columns (copy-tree '((ref . 0) (link . 0) (created . 0) (last-accessed . 0)
                               (count . 0) (sort . nil) (copy . nil))))

    ;; For each column
    (dotimes (col numcols)
      (let* (field-flags ;; raw heading, consisting of file name and maybe
                         ;; flags (seperated by ";")
             field       ;; field name only
             field-symbol ;; and as a symbol
             flags       ;; flags from field-flags
             found)

        ;; parse field-flags into field and flags
        (setq field-flags (org-trim (org-table-get-field (+ col 1))))
        (if (string-match "^\\([^;]*\\);\\([a-z]+\\)$" field-flags)
            (progn
              (setq field (downcase (or (match-string 1 field-flags) "")))
              ;; get flags as list of characters
              (setq flags (mapcar 'string-to-char
                                  (split-string
                                   (downcase (match-string 2 field-flags))
                                   "" t))))
          ;; no flags
          (setq field field-flags))

        (unless (string= field "") (setq field-symbol (intern (downcase field))))

        ;; Check, that no flags appear twice
        (mapc (lambda (x)
                (when (memq (car x) flags)
                  (if (cdr (assoc (cdr x) columns))
                      (org-favtable--report-setup-error
                       (format "More than one heading is marked with flag '%c'" (car x)) t))))
              '((?s . sort)
                (?c . copy)))

        ;; Process flags
        (if (memq ?s flags)
            (setcdr (assoc 'sort columns) field-symbol))
        (if (memq ?c flags)
            (setcdr (assoc 'copy columns) (+ col 1)))

        ;; Store columns in alist
        (setq found (assoc field-symbol columns))
        (when found
          (if (> (cdr found) 0)
              (org-favtable--report-setup-error
               (format "'%s' appears two times as column heading" (downcase field)) t))
          (setcdr found (+ col 1)))))

    ;; check if all necessary informations have been specified
    (mapc (lambda (col)
            (unless (> (cdr (assoc col columns)) 0)
              (org-favtable--report-setup-error
               (format "column '%s' has not been set" col) t)))
          '(ref link count created last-accessed))

    ;; use ref as a default sort-column
    (unless (cdr (assoc 'sort columns))
      (setcdr (assoc 'sort columns) 'ref))
    columns))



(defun org-favtable--report-setup-error (text &optional switch-to-node)

  (when switch-to-node
    (org-id-goto org-favtable-id)
    (delete-other-windows))

  (when (y-or-n-p (concat
                   text
                   ";\n"
                   "the correct setup is explained in the documentation of 'org-favtable-id'.\n"
                   "Do you want to read it ? "))
    (org-favtable--show-help 'org-favtable-id))

  (error "")
  (setq org-favtable--last-action 'leave))



(defun org-favtable--show-help (function-or-variable)

  (let ((isfun (functionp function-or-variable)))
    ;; bring up help-buffer for function or variable
    (if isfun
        (describe-function function-or-variable)
      (describe-variable function-or-variable))


    ;; clean up help-buffer
    (pop-to-buffer "*Help*")
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (progn
               (kill-line 1)
               (not (looking-at
                     (if isfun
                         "("
                       "Documentation:")))))
      (kill-line (if isfun 2 3))
      (goto-char (point-max))
      (kill-line -2)
      (goto-char (point-min)))))



(defun org-favtable--update-line (ref-or-link)

  (let (initial
        found
        count-field
        (ref-node-buffer-and-point (org-favtable--id-find)))

    (with-current-buffer (car ref-node-buffer-and-point)

      ;; search reference or link, if given (or assume, that we are already positioned right)
      (when ref-or-link
        (setq initial (point))
        (goto-char (cdr ref-node-buffer-and-point))
        (org-favtable--goto-top)
        (while (and (org-at-table-p)
                    (not (or (string= ref-or-link (org-favtable--get-field 'ref))
                             (string= ref-or-link (org-favtable--get-field 'link)))))
          (forward-line)))

      (if (not (org-at-table-p))
          (error "Did not find reference or link '%s'" ref-or-link)
        (setq count-field (org-favtable--get-field 'count))

        ;; update count field only if number or empty; leave :missing: and :reuse: as is
        (if (or (not count-field)
                (string-match "^[0-9]+$" count-field))
            (org-favtable--get-field 'count
                                    (number-to-string
                                     (+ 1 (string-to-number (or count-field "0"))))))

        ;; update timestamp
        (org-table-goto-column (org-favtable--column-num 'last-accessed))
        (org-table-blank-field)
        (org-insert-time-stamp nil t t)

        (setq found t))

      (if initial (goto-char initial))

      found)))



(defun org-favtable--occur-helper (action)
  (let ((line-beg (line-beginning-position))
        key search link ref)

    ;; extract reference or link from text property (as put there before)
    (setq ref (get-text-property line-beg 'org-favtable--ref))
    (if (string= ref "") (setq ref nil))
    (setq link (get-text-property line-beg 'org-favtable--link))
    (if (string= link "") (setq link nil))

    (org-favtable action
                  (or link ref) ;; prefer link
                  (if link t nil))))


(defun org-favtable--get-field (key &optional value)
  (let (field)
    (setq field (org-trim (org-table-get-field (cdr (assoc key org-favtable--columns)) value)))
    (if (string= field "") (setq field nil))

    field))


(defun org-favtable--column-num (key)
  (cdr (assoc key org-favtable--columns)))


(defun org-favtable-version ()
  "Show version of org-favtable" (interactive)
  (message "org-favtable %s" org-favtable--version))


(defun org-favtable--make-guarded-search (ref &optional dont-quote)
  (concat "\\b" (if dont-quote ref (regexp-quote ref)) "\\b"))


(defun org-favtable-get-ref-regex-format ()
  "return cons-cell with regular expression and format for references"
  (unless org-favtable--ref-regex
    (org-favtable 'parse))
  (cons (org-favtable--make-guarded-search org-favtable--ref-regex 'dont-quote) org-favtable--ref-format))


(defadvice org-mark-ring-goto (after org-favtable--advice-text-to-yank activate)
  "Make text from the favtable available for yank."
  (when org-favtable--text-to-yank
      (kill-new org-favtable--text-to-yank)
      (message (format "Ready to yank '%s'" org-favtable--text-to-yank))
      (setq org-favtable--text-to-yank nil)))


(provide 'org-favtable)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; org-favtable.el ends here

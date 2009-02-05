;;; smarty-mode.el --- major mode for editing Smarty templates

;; Author:       Vincent DEBOUT <deboutv@free.fr>
;; Maintainer:  Vincent DEBOUT <deboutv@free.fr>
;; Keywords:    languages smarty templates
;; WWW:         http://deboutv.free.fr/lisp/smarty/

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


;;; Commentary:

;; 2.3 Installation
;; ================
;;
;; 2.3.1 Installation
;; ------------------
;;
;; To install Smarty Mode you need to choose an installation directory
;; (for example `/usr/local/share/lisp' or `c:\lisp'). The administrator
;; must have the write rights on this directory.
;;
;; With your favorite unzip software, unzip the archive in the
;; installation directory.
;;
;; Example:
;;      cd /usr/local/share/lisp
;;      tar zxvf smarty-0.0.4.tar.gz
;; Now you have a `smarty' directory in the installation directory. This
;; directory contains 2 files `smarty-mode.el' and `smarty-mode.elc' and
;; another directory `docs' containing the documentation.
;;
;; You need to configure XEmacs. open you initialization file `init.el'
;; (open the file or start XEmacs then choose the Options menu and Edit
;; Init File). Add the following lines (the installation directory in
;; this example is `/usr/local/share/lisp') :
;;
;;      (setq load-path
;;            (append (list \"/usr/local/share/lisp/\") load-path))
;;      (autoload 'smarty-mode \"smarty-mode\" \"Smarty Mode\" t)
;;
;; 2.3.2 Update
;; ------------
;;
;; The update is easy. You need to unzip the archive in the installation
;; directory to remove the old release.
;;
;; Example:
;;      cd /usr/local/share/lisp
;;      rm -rf smarty
;;      tar zxvf smarty-0.0.4.tar.gz
;;
;; 2.4 Invoke Smarty-Mode
;; ======================
;;
;; You have two possibilities to invoke the Smarty Mode.
;;
;;    - Manually: At each file opening you need to launch Smarty Mode
;;      with the following command:
;;
;;      `M-x smarty-mode'
;;
;;    - Automatically: Add the following linesin your initialization
;;      file `init.el' :
;;
;;           (setq auto-mode-alist
;;                 (append
;;                  '((\"\\.tpl$\" . smarty-mode))
;;               auto-mode-alist))


;;; History

;; $Log: smarty-mode.el,v $

;; Beta version 2008/01/16 Lennart Borgman
;; - Changed to work with Viper
;; - Changed to allow (expand-abbrev) to be called anywhere.
;; - Moved the installation instructions to the beginning of the file.
;; - Checked before insertion of { if it it feasable.
;; - Initialized smarty-template-map the standard way

;; Revision 1.6  2006/12/16 19:54:26  vincent
;; Update release version
;;
;; Revision 1.5  2006/12/16 19:53:00  vincent
;; Fix bug #15
;;
;; Revision 1.4  2006/12/16 14:59:46  vincent
;; Fix bugs for release
;;
;; Revision 1.3  2006/11/19 12:29:53  vincent
;; Fix highlight bug, add templates
;;
;; Revision 1.2  2006/11/12 11:44:18  vincent
;; First release commit
;;

(defconst smarty-version "0.0.4"
  "Smarty Mode version number.")

(defconst smarty-time-stamp "2006-12-16"
  "Smarty Mode time stamp for last update.")

(require 'font-lock)
(require 'cc-mode)
(require 'custom)
(require 'etags)
(eval-when-compile
(require 'regexp-opt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup smarty nil
  "Customizations for Smarty mode."
  :prefix "smarty-"
  :group 'languages)

(defgroup smarty-mode nil
  "Customizations for Smarty mode."
  :group 'smarty)

(defcustom smarty-electric-mode t
  "*Non-nil enables electrification (automatic template generation).
If nil, template generators can still be invoked through key bindings and
menu.  Is indicated in the modeline by \"/e\" after the mode name and can be
toggled by `\\[smarty-electric-mode]'."
  :type 'boolean
  :group 'smarty-mode)

(defcustom smarty-stutter-mode t
  "*Non-nil enables stuttering.
Is indicated in the modeline by \"/s\" after the mode name and can be toggled
by `\\[smarty-stutter-mode]'."
  :type 'boolean
  :group 'smarty-mode)

(defgroup smarty-menu nil
  "Customizations for menues."
  :group 'smarty)

(defcustom smarty-source-file-menu t
  "*Non-nil means add a menu of all source files in current directory."
  :type 'boolean
  :group 'smarty-menu)

(defgroup smarty-highlight nil
  "Customizations for highlight."
  :group 'smarty)

(defcustom smarty-highlight-plugin-functions t
  "*Non-nil means highlight the plugin functions in the buffer."
  :type 'boolean
  :group 'smarty-highlight)

(defgroup smarty-template nil
  "Customizations for templates."
  :group 'smarty)

(defgroup smarty-header nil
  "Customizations for header template."
  :group 'smarty-template)

(defcustom smarty-file-header ""
  "*String or file to insert as file header.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted as file header.
Type `C-j' for newlines.
If the header contains RCS keywords, they may be written as <RCS>Keyword<RCS>
if the header needs to be version controlled.

The following keywords for template generation are supported:
  <filename>    : replaced by the name of the buffer
  <author>      : replaced by the user name and email address
                  \(`user-full-name',`mail-host-address', `user-mail-address')
  <login>       : replaced by user login name (`user-login-name')
  <company>     : replaced by contents of option `smarty-company-name'
  <date>        : replaced by the current date
  <year>        : replaced by the current year
  <copyright>   : replaced by copyright string (`smarty-copyright-string')
  <cursor>      : final cursor position."
  :type 'string
  :group 'smarty-header)

(defcustom smarty-file-footer ""
  "*String or file to insert as file footer.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted as file footer (i.e. at
the end of the file).
Type `C-j' for newlines.
The same keywords as in option `smarty-file-header' can be used."
  :type 'string
  :group 'smarty-header)

(defcustom smarty-company-name ""
  "*Name of company to insert in file header.
See option `smarty-file-header'."
  :type 'string
  :group 'smarty-header)

(defcustom smarty-copyright-string ""
  "*Copyright string to insert in file header.
Can be multi-line string (type `C-j' for newline) and contain other file
header keywords (see option `smarty-file-header')."
  :type 'string
  :group 'smarty-header)

(defcustom smarty-date-format "%Y-%m-%d"
  "*Specifies the date format to use in the header.
This string is passed as argument to the command `format-time-string'.
For more information on format strings, see the documentation for the
`format-time-string' command (C-h f `format-time-string')."
  :type 'string
  :group 'smarty-header)

(defcustom smarty-modify-date-prefix-string ""
  "*Prefix string of modification date in Smarty file header.
If actualization of the modification date is called (menu,
`\\[smarty-template-modify]'), this string is searched and the rest
of the line replaced by the current date."
  :type 'string
  :group 'smarty-header)

(defcustom smarty-modify-date-on-saving nil
  "*Non-nil means update the modification date when the buffer is saved.
Calls function `\\[smarty-template-modify]').

NOTE: Activate the new setting in a Smarty buffer by using the menu entry
      \"Activate Options\"."
  :type 'boolean
  :group 'smarty-header)

(defgroup smarty-misc nil
  "Miscellaneous customizations."
  :group 'smarty)

(defcustom smarty-left-delimiter "{"
  "Left escaping delimiter."
  :type 'string
  :group 'smarty-misc)

(defcustom smarty-right-delimiter "}"
  "Right escaping delimiter."
  :type 'string
  :group 'smarty-misc)

(defcustom smarty-intelligent-tab t
  "*Non-nil means `TAB' does indentation, word completion and tab insertion.
That is, if preceding character is part of a word then complete word,
else if not at beginning of line then insert tab,
else if last command was a `TAB' or `RET' then dedent one step,
else indent current line (i.e. `TAB' is bound to `smarty-electric-tab').
If nil, TAB always indents current line (i.e. `TAB' is bound to
`indent-according-to-mode').

NOTE: Activate the new setting in a Smarty buffer by using the menu entry
      \"Activate Options\"."
  :type 'boolean
  :group 'smarty-misc)

(defcustom smarty-word-completion-in-minibuffer t
  "*Non-nil enables word completion in minibuffer (for template prompts).

NOTE: Activate the new setting by restarting Emacs."
  :type 'boolean
  :group 'smarty-misc)

(defcustom smarty-word-completion-case-sensitive nil
  "*Non-nil means word completion using `TAB' is case sensitive.
That is, `TAB' completes words that start with the same letters and case.
Otherwise, case is ignored."
  :type 'boolean
  :group 'smarty-misc)

;; Functions

(defun smarty-customize ()
  "Call the customize function with `smarty' as argument."
  (interactive)
  (customize-browse 'smarty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar smarty-menu-max-size 20
  "*Specifies the maximum size of a menu before splitting it into submenues.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu tools functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarty-menu-split (list title)
  "Split menu LIST into several submenues, if number of
elements > `smarty-menu-max-size'."
  (if (> (length list) smarty-menu-max-size)
      (let ((remain list)
            (result '())
            (sublist '())
            (menuno 1)
            (i 0))
        (while remain
          (setq sublist (cons (car remain) sublist))
          (setq remain (cdr remain))
          (setq i (+ i 1))
          (if (= i smarty-menu-max-size)
              (progn
                (setq result (cons (cons (format "%s %s" title menuno)
                                         (nreverse sublist)) result))
                (setq i 0)
                (setq menuno (+ menuno 1))
                (setq sublist '()))))
        (and sublist
             (setq result (cons (cons (format "%s %s" title menuno)
                                      (nreverse sublist)) result)))
        (nreverse result))
    list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source file menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar smarty-sources-menu nil)

;; Create the source menu
(defun smarty-add-source-files-menu ()
  "Scan directory for all Smarty source files and generate menu.
The directory of the current source file is scanned."
  (interactive)
  (message "Scanning directory for source files ...")
  (let ((newmap (current-local-map))
        (file-list (smarty-get-source-files))
        menu-list found)
    ;; Create list for menu
    (setq found nil)
    (while file-list
      (setq found t)
      (setq menu-list (cons (vector (car file-list)
                                   (list 'find-file (car file-list)) t)
                           menu-list))
      (setq file-list (cdr file-list)))
    (setq menu-list (smarty-menu-split menu-list "Sources"))
    (when found (setq menu-list (cons "--" menu-list)))
    (setq menu-list (cons ["*Rescan*" smarty-add-source-files-menu t] menu-list))
    (setq menu-list (cons "Sources" menu-list))
    ;; Create menu
    (easy-menu-add menu-list)
    (easy-menu-define smarty-sources-menu newmap
                      "Smarty source files menu" menu-list))
  (message ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smarty menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarty-create-mode-menu ()
  "Create Smarty Mode menu."
  `("Smarty"
    ("Templates"
     ("Built-in Functions"
      ["capture" smarty-template-capture t]
      ["config_load" smarty-template-config-load t]
      ["else" smarty-template-else t]
      ["elseif" smarty-template-elseif t]
      ["foreach" smarty-template-foreach t]
      ["foreachelse" smarty-template-foreachelse t]
      ["if" smarty-template-if t]
      ["include" smarty-template-include t]
      ["include_php" smarty-template-include-php t]
      ["insert" smarty-template-insert t]
      ["ldelim" smarty-template-ldelim t]
      ["literal" smarty-template-literal t]
      ["php" smarty-template-php t]
      ["rdelim" smarty-template-rdelim t]
      ["section" smarty-template-section t]
      ["sectionelse" smarty-template-sectionelse t]
      ["strip" smarty-template-strip t])
     ("Custom Functions"
      ["assign" smarty-template-assign t]
      ["counter" smarty-template-counter t]
      ["cycle" smarty-template-cycle t]
      ["debug" smarty-template-debug t]
      ["eval" smarty-template-eval t]
      ["fetch"  smarty-template-fetch t]
      ["html_checkboxes" smarty-template-html-checkboxes t]
      ["html_image" smarty-template-html-image t]
      ["html_options" smarty-template-html-options t]
      ["html_radios" smarty-template-html-radios t]
      ["html_select_date" smarty-template-html-select-date t]
      ["html_select_time" smarty-template-html-select-time t]
      ["html_table" smarty-template-html-table t]
      ["mailto" smarty-template-mailto t]
      ["math" smarty-template-math t]
      ["popup" smarty-template-popup t]
      ["popup_init" smarty-template-popup-init t]
      ["textformat" smarty-template-textformat t])
     ("Variable Modifiers"
      ["capitalize" smarty-template-capitalize t]
      ["cat" smarty-template-cat t]
      ["count_characters" smarty-template-count-characters t]
      ["count_paragraphs" smarty-template-count-paragraphs t]
      ["count_sentences" smarty-template-count-sentences t]
      ["count_words" smarty-template-count-words t]
      ["date_format" smarty-template-date-format t]
      ["default" smarty-template-default t]
      ["escape" smarty-template-escape t]
      ["indent" smarty-template-indent t]
      ["lower" smarty-template-lower t]
      ["nl2br" smarty-template-nl2br t]
      ["regex_replace" smarty-template-regex-replace t]
      ["replace" smarty-template-replace t]
      ["spacify" smarty-template-spacify t]
      ["string_format" smarty-template-string-format t]
      ["strip" smarty-template-vstrip t]
      ["strip_tags" smarty-template-strip-tags t]
      ["truncate" smarty-template-truncate t]
      ["upper" smarty-template-upper t]
      ["wordwrap" smarty-template-wordwrap t])
     ("Plugins (Functions)"
      ("SmartyFormtool"
       ["formtool_checkall" smarty-template-formtool-checkall t]
       ["formtool_copy" smarty-template-formtool-copy t]
       ["formtool_count_chars" smarty-template-formtool-count-chars t]
       ["formtool_init" smarty-template-formtool-init t]
       ["formtool_move" smarty-template-formtool-move t]
       ["formtool_moveall" smarty-template-formtool-moveall t]
       ["formtool_movedown" smarty-template-formtool-movedown t]
       ["formtool_moveup" smarty-template-formtool-moveup t]
       ["formtool_remove" smarty-template-formtool-remove t]
       ["formtool_rename" smarty-template-formtool-rename t]
       ["formtool_save" smarty-template-formtool-save t]
       ["formtool_selectall" smarty-template-formtool-selectall t])
      ("SmartyPaginate"
       ["paginate_first" smarty-template-paginate-first t]
       ["paginate_last" smarty-template-paginate-last t]
       ["paginate_middle" smarty-template-paginate-middle t]
       ["paginate_next" smarty-template-paginate-next t]
       ["paginate_prev" smarty-template-paginate-prev t])
      ("SmartyValidate"
       ["validate" smarty-template-validate t]))
     ("Plugins (Variable Modifiers)"
      ("AlternativeDateModifierPlugin"
       ["date_format2" smarty-template-date-formatto t])
      ("B2Smilies"
       ["B2Smilies" smarty-template-btosmilies t])
      ("BBCodePlugin"
       ["bbcode2html" smarty-template-bbcodetohtml t])
      )
     "--"
     ["Insert Header" smarty-template-header t]
     ["Insert Footer" smarty-template-footer t]
     ["Insert Date" smarty-template-insert-date t]
     ["Modify Date" smarty-template-modify t])
    "--"
    ["Show Messages" smarty-show-messages]
    ["Smarty Mode Documentation" smarty-doc-mode :keys "C-c C-h"]
    ["Version" smarty-version :keys "C-c C-v"]
    "--"
    ("Options"
     ("Mode"
      ["Electric Mode"
       (progn (customize-set-variable 'smarty-electric-mode
                                      (not smarty-electric-mode))
              (smarty-mode-line-update))
       :style toggle :selected smarty-electric-mode :keys "C-c C-m C-e"]
      ["Stutter Mode"
       (progn (customize-set-variable 'smarty-stutter-mode
                                      (not smarty-stutter-mode))
              (smarty-mode-line-update))
       :style toggle :selected smarty-stutter-mode :keys "C-c C-m C-s"]
      "--"
      ["Customize Group..." (customize-group 'smarty-mode) t])
     ("Menu"
      ["Source Menu"
       (customize-set-variable 'smarty-source-file-menu
                               (not smarty-source-file-menu))
       :style toggle :selected smarty-source-file-menu]
      "--"
      ["Customize Group..." (customize-group 'smarty-menu) t])
     ("Highlight"
      ["Highlight plugin functions"
       (progn (customize-set-variable 'smarty-highlight-plugin-functions
                                      (not smarty-highlight-plugin-functions)))
       :style toggle :selected smarty-highlight-plugin-functions]
      "--"
      ["Customize Group..." (customize-group 'smarty-highlight) t])
     ("Template"
      ("Header"
       ["Header template..."
        (customize-option 'smarty-file-header) t]
       ["Footer template..."
        (customize-option 'smarty-file-footer) t]
       ["Company..."
        (customize-option 'smarty-company-name) t]
       ["Copyright..."
        (customize-option 'smarty-copyright-string) t]
       ["Date format..."
        (customize-option 'smarty-date-format) t]
       ["Modify date prefix..."
        (customize-option 'smarty-modify-date-prefix-string) t]
       ["Modify date on saving"
        (customize-set-variable 'smarty-modify-date-on-saving
                                (not smarty-modify-date-on-saving))
        :style toggle :selected smarty-modify-date-on-saving]
       "--"
       ["Customize Group..." (customize-group 'smarty-header) t])
      "--"
      ["Customize Group..." (customize-group 'smarty-template) t])
     ("Miscellaneous"
      ["Left delimiter..."
       (customize-option 'smarty-left-delimiter) t]
      ["Right delimiter..."
       (customize-option 'smarty-right-delimiter) t]
      ["Use Intelligent Tab"
       (progn (customize-set-variable 'smarty-intelligent-tab
                                      (not smarty-intelligent-tab))
              (smarty-activate-customizations))
       :style toggle :selected smarty-intelligent-tab]
      ["Word Completion in Minibuffer"
       (progn (customize-set-variable 'smarty-word-completion-in-minibuffer
                                      (not smarty-word-completion-in-minibuffer))
              (message "Activate new setting by saving options and restarting Emacs"))
       :style toggle :selected smarty-word-completion-in-minibuffer]
      ["Completion is case sensitive"
       (customize-set-variable 'smarty-word-completion-case-sensitive
                               (not smarty-word-completion-case-sensitive))
       :style toggle :selected smarty-word-completion-case-sensitive]
      "--"
      ["Customize Group..." (customize-group 'smarty-misc) t])
     "--"
     ["Save Options" customize-save-customized t]
     ["Activate Options" smarty-activate-customizations t]
     ["Browse Options..." smarty-customize t])))

(defvar smarty-mode-menu-list (smarty-create-mode-menu)
  "Smarty Mode menu.")

(defvar smarty-mode-map nil
  "Keymap for Smarty Mode.")

(defun smarty-update-mode-menu ()
  "Update Smarty Mode menu."
  (interactive)
  (easy-menu-remove smarty-mode-menu-list)
  (setq smarty-mode-menu-list (smarty-create-mode-menu))
  (easy-menu-add smarty-mode-menu-list)
  (easy-menu-define smarty-mode-menu smarty-mode-map
                    "Menu keymap for Smarty Mode." smarty-mode-menu-list))




(defvar smarty-mode-hook nil)

(defvar smarty-functions nil
  "List of Smarty functions.")

(defvar smarty-functions-regexp nil
  "Regexp for Smarty functions.")

(defconst smarty-01-functions
  '("capture" "config_load" "foreach" "foreachelse" "include"
    "include_php" "insert" "if" "elseif" "else" "ldelim" "rdelim"
    "literal" "php" "section" "sectionelse" "strip" "assign" "counter"
    "cycle" "debug" "eval" "fetch" "html_checkboxes" "html_image"
    "html_options" "html_radios" "html_select_date" "html_select_time"
    "html_table" "math" "mailto" "popup_init" "popup" "textformat")
  "Smarty built-in & custom functions.")

(defvar smarty-modifiers nil
  "List of Smarty variable modifiers.")

(defvar smarty-modifiers-regexp nil
  "Regexp for Smarty variable modifiers.")

(defconst smarty-01-modifiers
  '("capitalize" "cat" "count_characters" "count_paragraphs"
    "count_sentences" "count_words" "date_format" "default"
    "escape" "indent" "lower" "nl2br" "regex_replace" "replace"
    "spacify" "string_format" "strip" "strip_tags" "truncate"
    "upper" "wordwrap")
  "Smarty variable modifiers.")

(defvar smarty-plugins-functions nil
  "List of Smarty functions.")

(defvar smarty-plugins-functions-regexp nil
  "Regexp for Smarty functions.")

(defconst smarty-01-plugins-functions
  '("validate" "formtool_checkall" "formtool_copy" "formtool_count_chars"
    "formtool_init" "formtool_move" "formtool_moveall"
    "formtool_movedown" "formtool_moveup" "formtool_remove"
    "formtool_rename" "formtool_save" "formtool_selectall"
    "paginate_first" "paginate_last" "paginate_middle"
    "paginate_next" "paginate_prev")
  "Smarty plugins functions.")

(defvar smarty-plugins-modifiers nil
  "List of Smarty variable modifiers.")

(defvar smarty-plugins-modifiers-regexp nil
  "Regexp for Smarty functions.")

(defconst smarty-01-plugins-modifiers
  '("B2Smilies" "bbcode2html" "date_format2")
  "Smarty plugins modifiers.")

(defconst smarty-constants
  (eval-when-compile
        (regexp-opt
         '("TRUE" "FALSE" "NULL") t))
  "Smarty constants.")


;; Syntax table creation
(defvar smarty-mode-syntax-table nil
  "Syntax table for smarty-mode.")

(defvar smarty-mode-ext-syntax-table nil
  "Syntax table extended by `_' used in `smarty-mode' buffers.")

(defun smarty-create-syntax-table ()
  (if smarty-mode-syntax-table
      ()
    (setq smarty-mode-syntax-table (make-syntax-table))

    ;; Make | a punctuation character
    (modify-syntax-entry ?| "." smarty-mode-syntax-table)
    ;; Make " a punctuation character so highlighing works withing html strings
    (modify-syntax-entry ?\" "." smarty-mode-syntax-table)
    ;; define parentheses to match
    (modify-syntax-entry ?\( "()"   smarty-mode-syntax-table)
    (modify-syntax-entry ?\) ")("   smarty-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]"   smarty-mode-syntax-table)
    (modify-syntax-entry ?\] ")["   smarty-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}"   smarty-mode-syntax-table)
    (modify-syntax-entry ?\} "){"   smarty-mode-syntax-table)
    )
  (set-syntax-table smarty-mode-syntax-table)
  ;; extended syntax table including '_' (for simpler search regexps)
  (setq smarty-mode-ext-syntax-table (copy-syntax-table smarty-mode-syntax-table))
  (modify-syntax-entry ?_ "w" smarty-mode-ext-syntax-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File/directory manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarty-directory-files (directory &optional full match)
  "Call `directory-files' if DIRECTORY exists, otherwise generate error
message."
  (if (not (file-directory-p directory))
      (smarty-warning-when-idle "No such directory: \"%s\"" directory)
    (let ((dir (directory-files directory full match)))
      (setq dir (delete "." dir))
      (setq dir (delete ".." dir))
      dir)))

(defun smarty-get-source-files (&optional full directory)
  "Get list of SMARTY source files in DIRECTORY or current directory."
  (let ((mode-alist auto-mode-alist)
        filename-regexp)
    ;; create regular expressions for matching file names
    (setq filename-regexp "\\`[^.].*\\(")
    (while mode-alist
      (when (eq (cdar mode-alist) 'smarty-mode)
        (setq filename-regexp
              (concat filename-regexp (caar mode-alist) "\\|")))
      (setq mode-alist (cdr mode-alist)))
    (setq filename-regexp
          (concat (substring filename-regexp 0
                             (string-match "\\\\|$" filename-regexp)) "\\)"))
    ;; find files
    (smarty-directory-files
     (or directory default-directory) full filename-regexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages reporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar smarty-warnings nil
  "Warnings to tell the user during start up.")

(defun smarty-run-when-idle (secs repeat function)
  "Wait until idle, then run FUNCTION."
  (if (fboundp 'start-itimer)
      (start-itimer "smarty-mode" function secs repeat t)
;    (run-with-idle-timer secs repeat function)))
    ;; explicitely activate timer (necessary when Emacs is already idle)
    (aset (run-with-idle-timer secs repeat function) 0 nil)))

(defun smarty-warning-when-idle (&rest args)
  "Wait until idle, then print out warning STRING and beep."
  (if noninteractive
      (smarty-warning (apply 'format args) t)
    (unless smarty-warnings
      (smarty-run-when-idle .1 nil 'smarty-print-warnings))
    (setq smarty-warnings (cons (apply 'format args) smarty-warnings))))

(defun smarty-warning (string &optional nobeep)
  "Print out warning STRING and beep."
  (message (concat "WARNING:  " string))
  (unless (or nobeep noninteractive) (beep)))

(defun smarty-print-warnings ()
  "Print out messages in variable `smarty-warnings'."
  (let ((no-warnings (length smarty-warnings)))
    (setq smarty-warnings (nreverse smarty-warnings))
    (while smarty-warnings
      (message (concat "WARNING:  " (car smarty-warnings)))
      (setq smarty-warnings (cdr smarty-warnings)))
    (beep)
    (when (> no-warnings 1)
      (message "WARNING:  See warnings in message buffer (type `C-c M-m')."))))

(defun smarty-show-messages ()
  "Get *Messages* buffer to show recent messages."
  (interactive)
  (display-buffer " *Message-Log*"))

(defun smarty-version ()
  "Echo the current version of Smarty Mode in the minibuffer."
  (interactive)
  (message "Smarty Mode %s (%s)" smarty-version smarty-time-stamp)
  (smarty-keep-region-active))

;; active regions
(defun smarty-keep-region-active ()
  "Do whatever is necessary to keep the region active in XEmacs.
Ignore byte-compiler warnings you might see."
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defmacro smarty-prepare-search-1 (&rest body)
  "Enable case insensitive search and switch to syntax table that includes '_',
then execute BODY, and finally restore the old environment.  Used for
consistent searching."
  `(let ((case-fold-search t)           ; case insensitive search
         (current-syntax-table (syntax-table))
         result
         (restore-prog                  ; program to restore enviroment
          '(progn
             ;; restore syntax table
             (set-syntax-table current-syntax-table))))
     ;; use extended syntax table
     (set-syntax-table smarty-mode-ext-syntax-table)
     ;; execute BODY safely
     (setq result
           (condition-case info
               (progn ,@body)
             (error (eval restore-prog) ; restore environment on error
                    (error (cadr info))))) ; pass error up
     ;; restore environment
     (eval restore-prog)
     result))

(defmacro smarty-prepare-search-2 (&rest body)
  "Enable case insensitive search, switch to syntax table that includes '_',
and remove `intangible' overlays, then execute BODY, and finally restore the
old environment.  Used for consistent searching."
  `(let ((case-fold-search t)           ; case insensitive search
         (current-syntax-table (syntax-table))
         result overlay-all-list overlay-intangible-list overlay
         (restore-prog                  ; program to restore enviroment
          '(progn
             ;; restore syntax table
             (set-syntax-table current-syntax-table)
             ;; restore `intangible' overlays
             (when (fboundp 'overlay-lists)
               (while overlay-intangible-list
                 (overlay-put (car overlay-intangible-list) 'intangible t)
                 (setq overlay-intangible-list
                       (cdr overlay-intangible-list)))))))
     ;; use extended syntax table
     (set-syntax-table smarty-mode-ext-syntax-table)
     ;; remove `intangible' overlays
     (when (fboundp 'overlay-lists)
       (setq overlay-all-list (overlay-lists))
       (setq overlay-all-list
             (append (car overlay-all-list) (cdr overlay-all-list)))
       (while overlay-all-list
         (setq overlay (car overlay-all-list))
         (when (memq 'intangible (overlay-properties overlay))
           (setq overlay-intangible-list
                 (cons overlay overlay-intangible-list))
           (overlay-put overlay 'intangible nil))
         (setq overlay-all-list (cdr overlay-all-list))))
     ;; execute BODY safely
     (setq result
           (condition-case info
               (progn ,@body)
             (error (eval restore-prog) ; restore environment on error
                    (error (cadr info))))) ; pass error up
     ;; restore environment
     (eval restore-prog)
     result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Enabling/disabling

(defun smarty-mode-line-update ()
  "Update the modeline string for Smarty major mode."
  (setq mode-name (concat "Smarty"
                          (and (or smarty-electric-mode smarty-stutter-mode) "/")
                          (and smarty-electric-mode "e")
                          (and smarty-stutter-mode "s")))
  (force-mode-line-update t))

(defun smarty-electric-mode (arg)
  "Toggle Smarty electric mode.
Turn on if ARG positive, turn off if ARG negative, toggle if ARG zero or nil."
  (interactive "P")
  (setq smarty-electric-mode
        (cond ((or (not arg) (zerop arg)) (not smarty-electric-mode))
              ((> arg 0) t) (t nil)))
  (smarty-mode-line-update))

(defun smarty-stutter-mode (arg)
  "Toggle Smarty stuttering mode.
Turn on if ARG positive, turn off if ARG negative, toggle if ARG zero or nil."
  (interactive "P")
  (setq smarty-stutter-mode
        (cond ((or (not arg) (zerop arg)) (not smarty-stutter-mode))
              ((> arg 0) t) (t nil)))
  (smarty-mode-line-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Smarty code delimitation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarty-in-literal ()
  "Determine if point is in a Smarty literal."
  (save-excursion
    (let ((here (point))
          start state)
      (beginning-of-line)
      (setq start (point))
      (goto-char here)
      (setq state (parse-partial-sexp start (point)))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

(defun smarty-in-comment-p ()
  "Check if point is in a comment."
  (let ((result nil) (here (point-marker)) found)
    (save-excursion
      (setq found (re-search-backward (regexp-quote (concat smarty-left-delimiter "*")) nil t))
      (when found
        (setq result (re-search-forward (regexp-quote (concat "*" smarty-right-delimiter)) here t))
        (setq result (not result))))
    result))

(defun smarty-after-ldelim ()
  "Check that the previous character is the left delimiter."
  (let ((here (point-marker)) ldelim-found ldelim-point)
    (save-excursion
      (setq ldelim-found (re-search-backward (regexp-quote smarty-left-delimiter) nil t))
      (re-search-forward (regexp-quote smarty-left-delimiter) here t)
      (setq ldelim-point (point-marker))
      (goto-char here)
      (if (and (= here ldelim-point) ldelim-found)
          t
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Words to expand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarty-words-init ()
  "Initialize reserved words."
  (setq smarty-functions smarty-01-functions)
  (setq smarty-modifiers smarty-01-modifiers)
  (setq smarty-plugins-functions smarty-01-plugins-functions)
  (setq smarty-plugins-modifiers smarty-01-plugins-modifiers)
  (setq smarty-functions-regexp (concat "\\<\\(" (regexp-opt smarty-functions) "\\)\\>"))
  (setq smarty-modifiers-regexp (concat "\\<\\(" (regexp-opt smarty-modifiers) "\\)\\>"))
  (setq smarty-plugins-functions-regexp (concat "\\<\\(" (regexp-opt smarty-plugins-functions) "\\)\\>"))
  (setq smarty-plugins-modifiers-regexp (concat "\\<\\(" (regexp-opt smarty-plugins-modifiers) "\\)\\>"))
  (smarty-abbrev-list-init))

(defvar smarty-abbrev-list nil
  "Predefined abbreviations for Smarty.")

(defun smarty-abbrev-list-init ()
  (setq smarty-abbrev-list
        (append
         (list nil) smarty-functions
         (list nil) smarty-modifiers
         (list nil) smarty-plugins-functions
         (list nil) smarty-plugins-modifiers)))

(defvar smarty-expand-upper-case nil)

(defun smarty-try-expand-abbrev (old)
  "Try expanding abbreviations from `smarty-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
          (let ((abbrev-list smarty-abbrev-list)
                (sel-abbrev-list '()))
            (while abbrev-list
           ;   (if (stringp (car abbrev-list))
                ;  (insert (concat " " (car abbrev-list))))
              (when (or (not (stringp (car abbrev-list)))
                        (string-match
                         (concat "^" he-search-string) (car abbrev-list)))
                (setq sel-abbrev-list
                      (cons (car abbrev-list) sel-abbrev-list)))
              (setq abbrev-list (cdr abbrev-list)))
            (nreverse sel-abbrev-list))))
  (while (and he-expand-list
              (or (not (stringp (car he-expand-list)))
                  (he-string-member (car he-expand-list) he-tried-table t)))
    (unless (stringp (car he-expand-list))
      (setq smarty-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
             nil)
    (he-substitute-string
     (if smarty-expand-upper-case
         (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

;; initialize reserved words for Smarty Mode
(smarty-words-init)

;; function for expanding abbrevs and dabbrevs
(defun smarty-expand-abbrev (arg))
(fset 'smarty-expand-abbrev (make-hippie-expand-function
                           '(try-expand-dabbrev
                             try-expand-dabbrev-all-buffers
                             smarty-try-expand-abbrev)))

;; function for expanding parenthesis
(defun smarty-expand-paren (arg))
(fset 'smarty-expand-paren (make-hippie-expand-function
                          '(try-expand-list
                            try-expand-list-all-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuttering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarty-electric-tab (&optional prefix-arg)
  "If preceding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then insert tab,
else if last command was a tab or return then dedent one step or if a comment
toggle between normal indent and inline comment indent,
else indent `correctly'."
  (interactive "*P")
  (smarty-prepare-search-2
   (cond
    ;; expand word
    ((= (char-syntax (preceding-char)) ?w)
     (let ((case-fold-search (not smarty-word-completion-case-sensitive))
           (case-replace nil)
           (hippie-expand-only-buffers
            (or (and (boundp 'hippie-expand-only-buffers)
                     hippie-expand-only-buffers)
                '(smarty-mode))))
       (smarty-expand-abbrev prefix-arg)))
    ;; expand parenthesis
    ((or (= (preceding-char) ?\() (= (preceding-char) ?\)))
     (let ((case-fold-search (not smarty-word-completion-case-sensitive))
           (case-replace nil))
       (smarty-expand-paren prefix-arg))))
   (setq this-command 'smarty-electric-tab)))

(defun smarty-electric-space (count)
  "Expand abbreviations and self-insert space(s)."
  (interactive "p")
  (let ((here (point-marker)) ldelim-found ldelim-point rdelim-found rdelim-point
        delete-a)
    (setq ldelim-found (re-search-backward (regexp-quote smarty-left-delimiter) nil t))
    (re-search-forward (regexp-quote smarty-left-delimiter) here t)
    (setq ldelim-point (point-marker))
    (goto-char here)
    (setq rdelim-found (re-search-backward (regexp-quote (concat " " smarty-right-delimiter)) nil t))
    (re-search-forward (regexp-quote (concat " " smarty-right-delimiter)) here t)
    (setq rdelim-point (point-marker))
    (goto-char here)
  (cond ((and (= here ldelim-point) ldelim-found) (insert (concat "ldelim" smarty-right-delimiter)))
        ((and (= here rdelim-point) rdelim-found)
         (re-search-backward (regexp-quote (concat " " smarty-right-delimiter)) nil t)
         (delete-char 1)
         (insert (concat " " smarty-left-delimiter "rdelim"))
         (goto-char here))
        ((smarty-in-comment-p)
         (self-insert-command count)
         (cond ((>= (current-column) (+ 2 end-comment-column))
                (backward-char 1)
                (skip-chars-backward "^ \t\n")
                (indent-new-comment-line)
                (skip-chars-forward "^ \t\n")
                (forward-char 1))
               ((>= (current-column) end-comment-column)
                (indent-new-comment-line))
               (t nil)))
        ((or (and (>= (preceding-char) ?a) (<= (preceding-char) ?z))
             (and (>= (preceding-char) ?A) (<= (preceding-char) ?Z))
             (and (>= (preceding-char) ?0) (<= (preceding-char) ?9)))
         (progn
           (setq here (point-marker))
           (insert " ")
           (setq delete-a t)
           (if (re-search-backward "|" nil t)
               (progn
                 (setq found (re-search-forward (regexp-quote "B2Smilies") here t))
                 (if (and found (= here (point-marker)))
                     (replace-match "btosmilies")
                   (setq found (re-search-forward (regexp-quote "bbcode2html") here t))
                   (if (and found (= here (point-marker)))
                       (replace-match "bbcodetohtml")
                     (setq found (re-search-forward (regexp-quote "date_format2") here t))
                     (if (and found (= here (point-marker)))
                         (replace-match "date_formatto")
                       (goto-char here)
                       (setq delete-a nil)
                       (delete-char 1)))))
             (goto-char here)
             (setq delete-a nil)
             (delete-char 1)))
         (smarty-prepare-search-1 (expand-abbrev))
         (self-insert-command count)
         (if (and delete-a (looking-at " "))
             (delete-char 1)))
        (t (self-insert-command count)))))

(defun smarty-electric-open-bracket (count)
  "'(' --> '(', '((' --> '[', '[(' --> '{'"
  (interactive "p")
  (if (and smarty-stutter-mode (= count 1) (not (smarty-in-literal)))
      (if (= (preceding-char) ?\()
          (progn (delete-char -1) (insert-char ?\[ 1))
        (if (= (preceding-char) ?\[)
            (progn (delete-char -1) (insert-char ?\{ 1))
          (insert-char ?\( 1)))
    (self-insert-command count)))

(defun smarty-electric-close-bracket (count)
  "')' --> ')', '))' --> ']', '])' --> '}'"
  (interactive "p")
  (if (and smarty-stutter-mode (= count 1) (not (smarty-in-literal)))
      (progn
        (if (= (preceding-char) ?\))
            (progn (delete-char -1) (insert-char ?\] 1))
          (if (= (preceding-char) ?\])
              (progn (delete-char -1) (insert-char ?} 1))
            (insert-char ?\) 1)))
        (blink-matching-open))
    (self-insert-command count)))

(defun smarty-electric-star (count)
  "After a left delimiter add a right delemiter to close the comment"
  (interactive "p")
  (let ((here (point-marker)) found)
    (if (and smarty-stutter-mode (= count 1) (not (smarty-in-literal)))
        (progn
          (setq found (re-search-backward (regexp-quote smarty-left-delimiter) nil t))
          (re-search-forward (regexp-quote smarty-left-delimiter) here t)
          (if (not (and (= here (point-marker)) found))
              (progn (goto-char here)
                     (self-insert-command count))
            (self-insert-command count)
            (insert " ")
            (setq here (point-marker))
            (insert " *")
            (insert smarty-right-delimiter)
            (goto-char here)))
      (self-insert-command count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Electrification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst smarty-template-prompt-syntax "[^ =<>][^<>@.\n]*[^ =<>]"
  "Syntax of prompt inserted by template generators.")

(defvar smarty-template-invoked-by-hook nil
  "Indicates whether a template has been invoked by a hook or by key or menu.
Used for undoing after template abortion.")

(defun smarty-minibuffer-tab (&optional prefix-arg)
  "If preceding character is part of a word or a paren then hippie-expand,
else insert tab (used for word completion in Smarty minibuffer)."
  (interactive "P")
  (cond
   ;; expand word
   ((= (char-syntax (preceding-char)) ?w)
    (let ((case-fold-search (not smarty-word-completion-case-sensitive))
          (case-replace nil)
          (hippie-expand-only-buffers
           (or (and (boundp 'hippie-expand-only-buffers)
                    hippie-expand-only-buffers)
               '(smarty-mode))))
      (smarty-expand-abbrev prefix-arg)))
   ;; expand parenthesis
   ((or (= (preceding-char) ?\() (= (preceding-char) ?\)))
    (let ((case-fold-search (not smarty-word-completion-case-sensitive))
          (case-replace nil))
      (smarty-expand-paren prefix-arg)))
   ;; insert tab
   (t (insert-tab))))

;; correct different behavior of function `unread-command-events' in XEmacs
(defun smarty-character-to-event (arg))
(defalias 'smarty-character-to-event
  (if (fboundp 'character-to-event) 'character-to-event 'identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev ook bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst smarty-mode-abbrev-table nil
  "Abbrev table to use in `smarty-mode' buffers.")

(defun smarty-mode-abbrev-table-init ()
  "Initialize `smarty-mode-abbrev-table'."
  (when smarty-mode-abbrev-table (clear-abbrev-table smarty-mode-abbrev-table))
  (define-abbrev-table 'smarty-mode-abbrev-table
    (append
     ;; I changed the second element of the records below to be same
     ;; as first instead of "". Using "" gives strange behaviour when
     ;; (expand-abbrev) is called and point is after a space.
     '(("capture" "capture" smarty-template-capture-hook 0)
       ("config_load" "config_load" smarty-template-config-load-hook 0)
       ("else" "else" smarty-template-else-hook 0)
       ("elseif" "elseif" smarty-template-elseif-hook 0)
       ("foreach" "foreach" smarty-template-foreach-hook 0)
       ("foreachelse" "foreachelse" smarty-template-foreachelse-hook 0)
       ("if" "if" smarty-template-if-hook 0)
       ("include" "include" smarty-template-include-hook 0)
       ("include_php" "include_php" smarty-template-include-php-hook 0)
       ("insert" "insert" smarty-template-insert-hook 0)
       ("ldelim" "ldelim" smarty-template-ldelim-hook 0)
       ("literal" "literal" smarty-template-literal-hook 0)
       ("php" "php" smarty-template-php-hook 0)
       ("rdelim" "rdelim" smarty-template-rdelim-hook 0)
       ("section" "section" smarty-template-section-hook 0)
       ("sectionelse" "sectionelse" smarty-template-sectionelse-hook 0)
       ("strip" "strip" smarty-template-strip-hook 0)
       ("assign" "assign" smarty-template-assign-hook 0)
       ("counter" "counter" smarty-template-counter-hook 0)
       ("cycle" "cycle" smarty-template-cycle-hook 0)
       ("debug" "debug" smarty-template-debug-hook 0)
       ("eval" "eval" smarty-template-eval-hook 0)
       ("fetch" "fetch"  smarty-template-fetch-hook 0)
       ("html_checkboxes" "html_checkboxes" smarty-template-html-checkboxes-hook 0)
       ("html_image" "html_image" smarty-template-html-image-hook 0)
       ("html_options" "html_options" smarty-template-html-options-hook 0)
       ("html_radios" "html_radios" smarty-template-html-radios-hook 0)
       ("html_select_date" "html_select_date" smarty-template-html-select-date-hook 0)
       ("html_select_time" "html_select_time" smarty-template-html-select-time-hook 0)
       ("html_table" "html_table" smarty-template-html-table-hook 0)
       ("mailto" "mailto" smarty-template-mailto-hook 0)
       ("math" "math" smarty-template-math-hook 0)
       ("popup" "popup" smarty-template-popup-hook 0)
       ("popup_init" "popup_init" smarty-template-popup-init-hook 0)
       ("textformat" "textformat" smarty-template-textformat-hook 0)
       ("capitalize" "capitalize" smarty-template-capitalize-hook 0)
       ("cat" "cat" smarty-template-cat-hook 0)
       ("count_characters" "count_characters" smarty-template-count-characters-hook 0)
       ("count_paragraphs" "count_paragraphs" smarty-template-count-paragraphs-hook 0)
       ("count_sentences" "count_sentences" smarty-template-count-sentences-hook 0)
       ("count_words" "count_words" smarty-template-count-words-hook 0)
       ("date_format" "date_format" smarty-template-date-format-hook 0)
       ("default" "default" smarty-template-default-hook 0)
       ("escape" "escape" smarty-template-escape-hook 0)
       ("indent" "indent" smarty-template-indent-hook 0)
       ("lower" "lower" smarty-template-lower-hook 0)
       ("nl2br" "nl2br" smarty-template-nl2br-hook 0)
       ("regex_replace" "regex_replace" smarty-template-regex-replace-hook 0)
       ("replace" "replace" smarty-template-replace-hook 0)
       ("spacify" "spacify" smarty-template-spacify-hook 0)
       ("string_format" "string_format" smarty-template-string-format-hook 0)
       ("strip" "strip" smarty-template-vstrip-hook 0)
       ("strip_tags" "strip_tags" smarty-template-strip-tags-hook 0)
       ("truncate" "truncate" smarty-template-truncate-hook 0)
       ("upper" "upper" smarty-template-upper-hook 0)
       ("wordwrap" "wordwrap" smarty-template-wordwrap-hook 0)
       ("validate" "validate" smarty-template-validate-hook 0)
       ("formtool_checkall" "formtool_checkall" smarty-template-formtool-checkall-hook 0)
       ("formtool_copy" "formtool_copy" smarty-template-formtool-copy-hook 0)
       ("formtool_count_chars" "formtool_count_chars" smarty-template-formtool-count-chars-hook 0)
       ("formtool_init" "formtool_init" smarty-template-formtool-init-hook 0)
       ("formtool_move" "formtool_move" smarty-template-formtool-move-hook 0)
       ("formtool_moveall" "formtool_moveall" smarty-template-formtool-moveall-hook 0)
       ("formtool_movedown" "formtool_movedown" smarty-template-formtool-movedown-hook 0)
       ("formtool_moveup" "formtool_moveup" smarty-template-formtool-moveup-hook 0)
       ("formtool_remove" "formtool_remove" smarty-template-formtool-remove-hook 0)
       ("formtool_rename" "formtool_rename" smarty-template-formtool-rename-hook 0)
       ("formtool_save" "formtool_save" smarty-template-formtool-save-hook 0)
       ("formtool_selectall" "formtool_selectall" smarty-template-formtool-selectall-hook 0)
       ("paginate_first" "paginate_first" smarty-template-paginate-first-hook 0)
       ("paginate_last" "paginate_last" smarty-template-paginate-last-hook 0)
       ("paginate_middle" "paginate_middle" smarty-template-paginate-middle-hook 0)
       ("paginate_next" "paginate_next" smarty-template-paginate-next-hook 0)
       ("paginate_prev" "paginate_prev" smarty-template-paginate-prev-hook 0)
       ("btosmilies" "btosmilies" smarty-template-btosmilies-hook 0)
       ("bbcodetohtml" "bbcodetohtml" smarty-template-bbcodetohtml-hook 0)
       ("date_formatto" "date_formatto" smarty-template-date-formatto-hook 0)))))

;; initialize abbrev table for Smarty Mode
(smarty-mode-abbrev-table-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarty-hooked-abbrev (func)
  "Do function, if syntax says abbrev is a keyword, invoked by hooked abbrev,
but not if inside a comment or quote)."
  (if (or (smarty-in-literal)
          (smarty-in-comment-p))
      (progn
        (insert " ")
        (unexpand-abbrev)
        (delete-char -1))
    (if (not smarty-electric-mode)
        (progn
          (insert " ")
          (unexpand-abbrev)
          (backward-word 1)
          (delete-char 1))
      ;; last-command-event (alias last-command-char) is not always a
      ;; char. An example is that Viper calls expand-abbrev when
      ;; escape is pressed. The event is then 'escape.
      (let* ((base-event (event-basic-type last-command-event))
             (invoke-char (when (char-valid-p base-event) base-event))
             (abbrev-mode -1)
             (smarty-template-invoked-by-hook t))
        ;;(setq invoke-char last-command-char) ;; to get the orig err
        (let ((caught (catch 'abort
                        (funcall func))))
          (when (stringp caught) (message caught)))
        (when invoke-char
          (when (= invoke-char ?-) (setq abbrev-start-location (point))))
        ;; delete CR which is still in event queue
        (if (fboundp 'enqueue-eval-event)
            (enqueue-eval-event 'delete-char -1)
          (delete-char -1))))))
;;           (setq unread-command-events        ; push back a delete char
;;                 (list (smarty-character-to-event ?\177))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fontification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar smarty-font-lock-keywords-1
  (list

   ;; Fontify built-in functions
   (cons
        (concat (regexp-quote smarty-left-delimiter) "[/]*" smarty-functions-regexp)
        '(1 font-lock-keyword-face))

   (cons
        (concat "\\<\\(" smarty-constants "\\)\\>")
        'font-lock-constant-face)

   (cons (concat "\\(" (regexp-quote (concat smarty-left-delimiter "*")) "\\(\\s-\\|\\w\\|\\s.\\|\\s_\\|\\s(\\|\\s)\\|\\s\\\\)*" (regexp-quote (concat "*" smarty-right-delimiter)) "\\)")
         'font-lock-comment-face)

   )
  "Subdued level highlighting for Smarty mode.")

;; I believe default is defined in Xemacs but not in GNU Emacs.
(unless (boundp 'default)
  (defvar default 'default))

(defconst smarty-font-lock-keywords-2
  (append
   smarty-font-lock-keywords-1
   (list

        ;; Fontify variable names (\\sw\\|\\s_\\) matches any word character +
        ;; underscore
        '("\\$\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face)) ; $variable
        '("->\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face t t)) ; ->variable
        '("\\.\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face t t)) ; .variable
        '("->\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" (1 font-lock-function-name-face t t)) ; ->function_call
        '("\\<\\(\\(?:\\sw\\|\\s_\\)+\\s-*\\)(" (1 font-lock-function-name-face)) ; word(
        '("\\<\\(\\(?:\\sw\\|\\s_\\)+\\s-*\\)[[]" (1 font-lock-variable-name-face)) ; word[
        '("\\<[0-9]+" . default)                        ; number (also matches word)

        ;; Fontify strings
        ;;'("\"\\([^\"]*\\)\"[^\"]+" (1 font-lock-string-face t t))
        ))

   "Medium level highlighting for Smarty mode.")

(defconst smarty-font-lock-keywords-3
  (append
   smarty-font-lock-keywords-2
   (list
    ;; Fontify modifiers
    (cons (concat "|\\(" smarty-modifiers-regexp "\\)[:|]+") '(1 font-lock-function-name-face))
    (cons (concat "|\\(" smarty-modifiers-regexp "\\)" (regexp-quote smarty-right-delimiter)) '(1 font-lock-function-name-face))

    ;; Fontify config vars
    (cons (concat (regexp-quote smarty-left-delimiter) "\\(#\\(?:\\sw\\|\\s_\\)+#\\)") '(1 font-lock-constant-face))))
  "Balls-out highlighting for Smarty mode.")

(defconst smarty-font-lock-keywords-4
  (append
   smarty-font-lock-keywords-3
   (list
    ;; Fontify plugin functions
    (cons
     (concat (regexp-quote smarty-left-delimiter) "[/]*" smarty-plugins-functions-regexp)
     '(1 font-lock-keyword-face))

    (cons (concat "|\\(" smarty-plugins-modifiers-regexp "\\)[:|]+") '(1 font-lock-function-name-face))
    (cons (concat "|\\(" smarty-plugins-modifiers-regexp "\\)" (regexp-quote smarty-right-delimiter)) '(1 font-lock-function-name-face)))))

(defvar smarty-font-lock-keywords smarty-font-lock-keywords-3
  "Default highlighting level for Smarty mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar smarty-template-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ba" 'smarty-template-capture)
    (define-key map "\C-bc" 'smarty-template-config-load)
    (define-key map "\C-b\M-e" 'smarty-template-else)
    (define-key map "\C-b\C-e" 'smarty-template-elseif)
    (define-key map "\C-b\C-f" 'smarty-template-foreach)
    (define-key map "\C-b\M-f" 'smarty-template-foreachelse)
    (define-key map "\C-bf" 'smarty-template-if)
    (define-key map "\C-b\C-i" 'smarty-template-include)
    (define-key map "\C-b\M-i" 'smarty-template-include-php)
    (define-key map "\C-bi" 'smarty-template-insert)
    (define-key map "\C-bl" 'smarty-template-ldelim)
    (define-key map "\C-b\C-l" 'smarty-template-literal)
    (define-key map "\C-bp" 'smarty-template-php)
    (define-key map "\C-br" 'smarty-template-rdelim)
    (define-key map "\C-b\C-s" 'smarty-template-section)
    (define-key map "\C-b\M-s" 'smarty-template-sectionelse)
    (define-key map "\C-bs" 'smarty-template-strip)
    (define-key map "\C-ca" 'smarty-template-assign)
    (define-key map "\C-co" 'smarty-template-counter)
    (define-key map "\C-cc" 'smarty-template-cycle)
    (define-key map "\C-cd" 'smarty-template-debug)
    (define-key map "\C-ce" 'smarty-template-eval)
    (define-key map "\C-cf" 'smarty-template-fetch)
    (define-key map "\C-c\C-hc" 'smarty-template-html-checkboxes)
    (define-key map "\C-c\C-hi" 'smarty-template-html-image)
    (define-key map "\C-c\C-ho" 'smarty-template-html-options)
    (define-key map "\C-c\C-hr" 'smarty-template-html-radios)
    (define-key map "\C-c\C-hd" 'smarty-template-html-select-date)
    (define-key map "\C-c\C-hm" 'smarty-template-html-select-time)
    (define-key map "\C-c\C-ht" 'smarty-template-html-table)
    (define-key map "\C-ci" 'smarty-template-mailto)
    (define-key map "\C-ch" 'smarty-template-math)
    (define-key map "\C-c\C-p" 'smarty-template-popup)
    (define-key map "\C-c\M-p" 'smarty-template-popup-init)
    (define-key map "\C-ct" 'smarty-template-textformat)
    (define-key map "\C-vp" 'smarty-template-capitalize)
    (define-key map "\C-vc" 'smarty-template-cat)
    (define-key map "\C-v\C-cc" 'smarty-template-count-characters)
    (define-key map "\C-v\C-cp" 'smarty-template-count-paragraphs)
    (define-key map "\C-v\C-cs" 'smarty-template-count-sentences)
    (define-key map "\C-v\C-cw" 'smarty-template-count-words)
    (define-key map "\C-vf" 'smarty-template-date-format)
    (define-key map "\C-vd" 'smarty-template-default)
    (define-key map "\C-ve" 'smarty-template-escape)
    (define-key map "\C-vi" 'smarty-template-indent)
    (define-key map "\C-vl" 'smarty-template-lower)
    (define-key map "\C-vn" 'smarty-template-nl2br)
    (define-key map "\C-vx" 'smarty-template-regex-replace)
    (define-key map "\C-v\C-p" 'smarty-template-replace)
    (define-key map "\C-vy" 'smarty-template-spacify)
    (define-key map "\C-vs" 'smarty-template-string-format)
    (define-key map "\C-v\C-s" 'smarty-template-vstrip)
    (define-key map "\C-v\M-s" 'smarty-template-strip-tags)
    (define-key map "\C-vt" 'smarty-template-truncate)
    (define-key map "\C-vu" 'smarty-template-upper)
    (define-key map "\C-vw" 'smarty-template-wordwrap)
    (define-key map "\C-h" 'smarty-template-header)
    (define-key map "\C-f" 'smarty-template-footer)
    (define-key map "\C-di" 'smarty-template-insert-date)
    (define-key map "\C-dm" 'smarty-template-modify)
    map)
  "Keymap for Smarty templates.")

(defun smarty-mode-map-init ()
  "Initialize `smarty-mode-map'."
  (setq smarty-mode-map (make-sparse-keymap))
  ;; template key bindings
  (define-key smarty-mode-map "\C-c\C-t"   smarty-template-map)
  ;; mode specific key bindings
  (define-key smarty-mode-map "\C-c\C-m\C-e"  'smarty-electric-mode)
  (define-key smarty-mode-map "\C-c\C-m\C-s"  'smarty-stutter-mode)
  (define-key smarty-mode-map "\C-c\C-s\C-u"  'smarty-add-source-files-menu)
  (define-key smarty-mode-map "\C-c\M-m"   'smarty-show-messages)
  (define-key smarty-mode-map "\C-c\C-h"   'smarty-doc-mode)
  (define-key smarty-mode-map "\C-c\C-v"   'smarty-version)
  ;; electric key bindings
  (when smarty-intelligent-tab
    (define-key smarty-mode-map "\t" 'smarty-electric-tab))
  (define-key smarty-mode-map " " 'smarty-electric-space)
  (define-key smarty-mode-map "(" 'smarty-electric-open-bracket)
  (define-key smarty-mode-map ")" 'smarty-electric-close-bracket)
  (define-key smarty-mode-map "*" 'smarty-electric-star))

;; initialize mode map for Smarty Mode
(smarty-mode-map-init)

(defvar smarty-minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (when smarty-word-completion-in-minibuffer
      (define-key map "\t" 'smarty-minibuffer-tab))
    map)
  "Keymap for minibuffer used in Smarty Mode.")

(mapcar
 (function
  (lambda (sym)
    (put sym 'delete-selection t)    ; for `delete-selection-mode' (Emacs)
    (put sym 'pending-delete t)))    ; for `pending-delete-mode' (XEmacs)
 '(smarty-electric-space
   smarty-electric-tab
   smarty-electric-open-bracket
   smarty-electric-close-bracket
   smarty-electric-star))

;;;###autoload
(defun smarty-mode ()
  "Mode for editing php smarty files.
Smarty Mode is a GNU XEmacs major mode for editing Smarty templates.

1 Introduction
**************

Smarty-Mode is a mode allowing easy edit of Smarty templates:
highlight, templates, navigation into source files...



Features (new features in bold) :

   * Completion

   * Customizable

   * Highlight

   * Menu

   * Stuttering

   * Templates
        - Built-in Functions

        - User Functions

        - Variable Modifiers

        - Plugin (Functions)
             * Smarty Formtool

             * Smarty Paginate

             * Smarty Validate

        - Plugin (Variable Modifiers)
             * AlternativeDateModifierPlugin

             * B2Smilies

             * BBCodePlugin

        - Fonctions Non-Smarty



This manual describes Smarty Mode version 0.0.4.

3 Customization
***************

This chapter describes the differents parameters and functions that
you can change to customize Smarty Mode.  To do that, open a Smarty
file, click on the Smarty menu and choose Options then Browse
Options....

3.1 Parameters
==============

3.1.1 Mode
----------

Smarty Mode has 2 modes allowing to simplify the writing of Smarty
templates. You can enable/disable each mode individually.

`smarty-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `smarty-electric-mode'.

`smarty-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `smarty-stutter-mode'.

3.1.2 Menu
----------

Smarty Mode has also 1 menu that you can enable/disable. The menu
Sources is specific to each Smarty files opened.

`smarty-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of Smarty file located in the current
     directory. The Sources menu scans the directory when a file is
     opened.

3.1.3 Menu
----------

`smarty-highlight-plugin-functions'
     Type: boolean
     Default value: `t'
     Description: If `t'; the functions described in the smarty
     plugins are highlighted.

3.1.4 Templates
---------------

3.1.4.1 Header
..............

`smarty-file-header'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file header. If the
     string specifies an existing file name the contents of the file
     is inserted; otherwise the string itself is inserted as file
     header.
     Type `C-j' for newlines.
     The follonwing keywords are supported:
     <filename>: replaced by the file name.
     <author>: replaced by the user name and email address.
     <login>: replaced by `user-login-name'.
     <company>: replaced by `smarty-company-name' content.
     <date>: replaced by the current date.
     <year>: replaced by the current year.
     <copyright>: replaced by `smarty-copyright-string' content.
     <cursor>: final cursor position.

`smarty-file-footer'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file footer.  See
     `smarty-file-header'

`smarty-company-name'
     Type: string
     Default value: `\"\"'
     Description: Name of the company to insert in file header.

`smarty-copyright-string'
     Type: string
     Default value: `\"\"'
     Description: Coryright string to insert in file header.

`smarty-date-format'
     Type: string
     Default value: `\"%Y-%m-%d\"'
     Description: Date format.

`smarty-modify-date-prefix-string'
     Type: string
     Default value: `\"\"'
     Description: Prefix string of modification date in Smarty file
     header.

`smarty-modify-date-on-saving'
     Type: bool
     Default value: `nil'
     Description: If `t'; update the modification date when the
     buffer is saved.

3.1.5 Miscellaneous
-------------------

`smarty-left-delimiter'
     Type: string
     Default value: `\"\"'
     Description: Left escaping delimiter for Smarty templates.

`smarty-right-delimiter'
     Type: string
     Default value: `\"\"'
     Description: Right escaping delimiter for Smarty templates.

`smarty-intelligent-tab'
     Type: bool
     Default value: `t'
     Description: If `t'; TAB does indentation; completion and insert
     tabulations. If `nil'; TAB does only indentation.

`smarty-word-completion-in-minibuffer'
     Type: bool
     Default value: `t'
     Description: If `t'; enable completion in the minibuffer.

`smarty-word-completion-case-sensitive'
     Type: bool
     Default value: `nil'
     Description: If `t'; completion is case sensitive.

3.2 Functions
=============

3.2.1 Mode
----------

`smarty-electric-mode'
     Menu: Smarty -> Options -> Mode -> Electric Mode
     Keybinding: `C-c C-m C-e'
     Description: This functions is used to enable/disable the
     electric mode.

`smarty-stutter-mode'
     Menu: Smarty -> Options -> Mode -> Stutter Mode
     Keybinding: `C-c C-m C-s'
     Description: This function is used to enable/disable the stutter
     mode.

4 Menus
*******

There are 2 menus: Smarty and Sources. All theses menus can be
accessed from the menubar or from the right click. This chapter
describes each menus.

4.1 Smarty
==========

This is the main menu of Smarty Mode. It allows an easy access to the
main features of the Smarty Mode: Templates (see *Note Templates::)
and Options (see *Note Customization::).

This menu contains also 3 functions that are discussed in the next
part.

4.1.1 Functions
---------------
\\<smarty-mode-map>
`smarty-show-messages'
     Menu: Smarty -> Show Messages
     Keybinding: `C-c M-m' \\[smarty-show-messages]
     Description: This function opens the *Messages* buffer to
     display previous error messages.

`smarty-doc-mode'
     Menu: Smarty -> Smarty Mode Documentation
     Keybinding: `C-c C-h' \\[smarty-doc-mode]
     Description: This function opens the *Help* buffer and prints in
     it the Smarty Mode documentation.

`smarty-version'
     Menu: Smarty -> Version
     Keybinding: `C-c C-v'
     Description: This function displays in the minibuffer the
     current Smarty Mode version with the timestamp.

4.2 Sources
===========

The Sources menu shows the Smarty files in the current directory. If
you add or delete a file in the current directory, you need to
refresh the menu.

4.2.1 Customization
-------------------

`smarty-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of Smarty file located in the current
     directory. The Sources menu scans the directory when a file is
     opened.

4.2.2 Functions
---------------

`smarty-add-source-files-menu'
     Menu: Sources -> *Rescan*
     Keybinding: `C-c C-s C-u'
     Description: This function is used to refresh the Sources menu.

5 Stuttering
************

The stutter mode is a mode that affects a function to a key. For
example, when you use the `ENTER' key, the associated function will
create a new line and indent it.

5.1 Customization
=================

`smarty-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `smarty-stutter-mode'.

5.2 Functions
=============

`SPACE'
     If in comment, indent the comment and add new line if necessary.
     In other case, add a space.

`('
     If the previous character is a `(', the `((' will be replaced by
     `['.
     If the previous character is a `[', the `[(' will be replaced by
     `{'.
     In other case, insert a `('.

`)'
     If the previous character is a `)', the `))' will be replaced by
     `]'.
     If the previous character is a `]', the `])' will be replaced by
     `}'.
     In other case, insert a `)'.

6 Templates
***********

In the Smarty Mode, the Smarty functions (like if, while, for, fopen,
fclose) are predefined in functions called \"Templates\".

Each template can be invoked by the function name or by using the
<SPACE> key after the Smarty function name in the buffer (Note, using
`M-<SPACE>' disable the template).

A template can be aborted by using the `C-g' or by lefting empty the
tempate prompt (in the minibuffer).

6.1 Customization
=================

`smarty-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `smarty-electric-mode'.

For a complete description of the template customizable variables,
see *Note Cu01-Pa01-Template::

6.2 Functions
=============

6.2.1 Smarty Functions
----------------------

For Smarty functions, see PDF or HTML documentation.

6.2.2 Non-Smarty Functions
--------------------------

`smarty-template-header'
     Menu: Smarty -> Templates -> Insert Header
     Keybinding: `C-c C-t C-h'
     Description: This function is used to insert a header in the
     current buffer.

`smarty-template-footer'
     Menu: Smarty -> Templates -> Insert Footer
     Keybinding: `C-c C-t C-f'
     Description: This function is used to insert a footer in the
     current buffer.

`smarty-template-insert-date'
     Menu: Smarty -> Templates -> Insert Date
     Keybinding: `C-c C-t C-d i'
     Description: This function is used to insert the date in the
     current buffer.

`smarty-template-modify'
     Menu: Smarty -> Templates -> Modify Date
     Keybinding: `C-c C-t C-d m'
     Description: This function is used to modify the last
     modification date in the current buffer.

7 Bugs, Help
************

   * To report bugs: Bugtracker
     (http://bugtracker.morinie.fr/lisp/set_project.php?project_id=2)

   * To obtain help you can post on the dedicated forum: Forum
     (http://forum.morinie.fr/lisp/)

8 Key bindings
**************

\\{smarty-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'smarty-mode)
  (setq mode-name "Smarty")

  (smarty-create-syntax-table)

  ;; set maps and tables
  (use-local-map smarty-mode-map)
  (set-syntax-table smarty-mode-syntax-table)
  (setq local-abbrev-table smarty-mode-abbrev-table)

  (set (make-local-variable 'comment-start) (concat smarty-left-delimiter "*"))
  (set (make-local-variable 'comment-end) (concat "*" smarty-right-delimiter))
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'end-comment-column) 80)

  (make-local-variable 'font-lock-defaults)
  (if smarty-highlight-plugin-functions
      (setq smarty-font-lock-keywords smarty-font-lock-keywords-4)
    (setq smarty-font-lock-keywords smarty-font-lock-keywords-3))
  (setq font-lock-defaults
                '((smarty-font-lock-keywords)
                nil ; Keywords only (i.e. no comment or string highlighting
                t   ; case fold
                nil ; syntax-alist
                nil ; syntax-begin
                ))

  (setq font-lock-maximum-decoration t
                case-fold-search t)

  ;; add source file menu
  (if smarty-source-file-menu (smarty-add-source-files-menu))
  ;; add Smarty menu
  (easy-menu-add smarty-mode-menu-list)
  (easy-menu-define smarty-mode-menu smarty-mode-map
                    "Menu keymap for Smarty Mode." smarty-mode-menu-list)

  (message "Smarty Mode %s.%s" smarty-version
           (if noninteractive "" "  See menu for documentation and release notes."))
  (smarty-mode-line-update)
  (run-hooks 'smarty-mode-hook))

(defun smarty-doc-mode ()
  "Display Smarty Mode documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer
      (if (fboundp 'help-buffer) (help-buffer) "*Help*")
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation 'smarty-mode))
    (with-current-buffer standard-output
      (help-mode))
    (print-help-return-message)))

(defun smarty-activate-customizations ()
  "Activate all customizations on local variables."
  (interactive)
  (smarty-mode-map-init)
  (use-local-map smarty-mode-map)
  (set-syntax-table smarty-mode-syntax-table)
  (smarty-update-mode-menu)
  (run-hooks 'menu-bar-update-hook)
  (smarty-mode-line-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarty-template-field (prompt &optional follow-string optional
                                   begin end is-string string-char default)
  "Prompt for string and insert it in buffer with optional FOLLOW-STRING.
If OPTIONAL is nil, the prompt is left if an empty string is inserted.  If
an empty string is inserted, return nil and call `smarty-template-undo' for
the region between BEGIN and END.  IS-STRING indicates whether a string
with double-quotes is to be inserted.  DEFAULT specifies a default string."
  (let ((position (point))
        string)
    (insert "<" prompt ">")
    (if (not (> (length string-char) 0))
        (setq string-char "\""))
    (setq string
          (condition-case ()
              (read-from-minibuffer (concat prompt ": ")
                                    (or (and is-string (cons (concat string-char string-char) 1)) default)
                                    smarty-minibuffer-local-map)
            (quit (if (and optional begin end)
                      (progn (beep) "")
                    (keyboard-quit)))))
    (when (or (not (equal string "")) optional)
      (delete-region position (point)))
    (when (and (equal string "") optional begin end)
      (smarty-template-undo begin end)
      (message "Template aborted"))
    (unless (equal string "")
      (insert string))
    (when (or (not (equal string "")) (not optional))
      (insert (or follow-string "")))
    (if (equal string "") nil string)))

(defun smarty-template-undo (begin end)
  "Undo aborted template by deleting region and unexpanding the keyword."
  (cond (smarty-template-invoked-by-hook
         (goto-char end)
         (insert " ")
         (delete-region begin end)
         (unexpand-abbrev))
        (t (delete-region begin end))))

(defun smarty-template-generic-function (label close-label field mandatory-count &optional infinite special-field)
  "Generic function template 'label field1= field2=..."
  (interactive)
  (let ((start (point)) found here result-value elt continue field-count stop prompt)
    (if smarty-template-invoked-by-hook
        (setq found (smarty-after-ldelim))
      (let ((here (point)))
        (let ((last-left (search-backward smarty-left-delimiter nil t)))
          (when last-left
            (goto-char here)
            (unless (search-backward smarty-right-delimiter last-left t)
              (goto-char here)
              ;;(error "Can't insert left delimiter '%s' here" smarty-left-delimiter)
              (lwarn t :warning "Can't insert left delimiter '%s' here" smarty-left-delimiter)
              )))
        (goto-char here))
      (insert smarty-left-delimiter)
      (setq found t))
    ;;(insert label) ;; Already done
    (setq here (point-marker))
    (insert " ")
    (when found
      (setq elt field)
      (setq continue t)
      (setq field-count 0)
      (setq stop nil)
      (while (and elt continue)
        (setq prompt (car elt))
        (when (not special-field)
          (insert prompt "="))
        (setq result-value (smarty-template-field prompt nil t))
        (if (and (not result-value)
                 (< field-count mandatory-count))
            (progn (setq continue nil)
                   (delete-region start (point))
                   (insert (concat label " "))
                   (setq stop t))
          (if (not result-value)
              (setq continue nil)
            (setq here (point-marker))
            (insert " ")))
        (setq field-count (+ 1 field-count))
        (setq elt (cdr elt)))
      (when (and infinite continue)
        (while continue
          (setq result-value (smarty-template-field "var_name" "=" t here))
          (if (not result-value)
              (setq continue nil)
            (setq continue (smarty-template-field "var_value" nil t here))
            (setq here (point-marker))
            (insert " "))))
      (when (not stop)
        (delete-region here (point))
        (if (> 0 mandatory-count)
            (delete-char -1))
        (if special-field
            (delete-char -1))
        (insert smarty-right-delimiter)
        (setq here (point-marker))
        (if close-label
            (insert smarty-left-delimiter "/" label smarty-right-delimiter))
        (goto-char here)))))

(defun smarty-template-generic-modifier (label field mandatory-count)
  "Generic modifier template '|label:field1:field2..."
  (interactive)
  (let ((start (point)) found here result-value elt continue field-count stop prompt)
    (setq found (re-search-backward (concat (regexp-quote smarty-left-delimiter) "\\$\\(\\w+\\)" (regexp-quote "|")) nil t))
    (if found
        (progn
          (setq found (re-search-forward (regexp-quote smarty-right-delimiter) start t))
          (if (not found)
              (progn
                (goto-char start)
                (insert label)
                (setq here (point-marker))
                (setq elt field)
                (setq continue t)
                (setq field-count 0)
                (setq stop nil)
                (while (and elt continue)
                  (setq prompt (car elt))
                  (insert ":")
                  (setq result-value (smarty-template-field prompt nil t))
                  (if (and (not result-value)
                           (< field-count mandatory-count))
                      (progn (setq continue nil)
                             (delete-region start (point))
                             (insert (concat label " "))
                             (setq stop t))
                    (if (not result-value)
                        (setq continue nil)
                      (setq here (point-marker))
                      (insert ":")))
                  (setq field-count (+ 1 field-count))
                  (setq elt (cdr elt)))
                (when (not stop)
                  (delete-region here (point))
                  (if (not (or (looking-at smarty-right-delimiter)
                               (looking-at "|")))
                      (insert smarty-right-delimiter))))
            (goto-char start)
            (insert label " ")))
      (goto-char start)
      (insert label " "))))

(defun smarty-template-capture-hook ()
  (smarty-hooked-abbrev 'smarty-template-capture))
(defun smarty-template-config-load-hook ()
  (smarty-hooked-abbrev 'smarty-template-config-load))
(defun smarty-template-else-hook ()
  (smarty-hooked-abbrev 'smarty-template-else))
(defun smarty-template-elseif-hook ()
  (smarty-hooked-abbrev 'smarty-template-elseif))
(defun smarty-template-foreach-hook ()
  (smarty-hooked-abbrev 'smarty-template-foreach))
(defun smarty-template-foreachelse-hook ()
  (smarty-hooked-abbrev 'smarty-template-foreachelse))
(defun smarty-template-if-hook ()
  (smarty-hooked-abbrev 'smarty-template-if))
(defun smarty-template-include-hook ()
  (smarty-hooked-abbrev 'smarty-template-include))
(defun smarty-template-include-php-hook ()
  (smarty-hooked-abbrev 'smarty-template-include-php))
(defun smarty-template-insert-hook ()
  (smarty-hooked-abbrev 'smarty-template-insert))
(defun smarty-template-ldelim-hook ()
  (smarty-hooked-abbrev 'smarty-template-ldelim))
(defun smarty-template-literal-hook ()
  (smarty-hooked-abbrev 'smarty-template-literal))
(defun smarty-template-php-hook ()
  (smarty-hooked-abbrev 'smarty-template-php))
(defun smarty-template-rdelim-hook ()
  (smarty-hooked-abbrev 'smarty-template-rdelim))
(defun smarty-template-section-hook ()
  (smarty-hooked-abbrev 'smarty-template-section))
(defun smarty-template-sectionelse-hook ()
  (smarty-hooked-abbrev 'smarty-template-sectionelse))
(defun smarty-template-strip-hook ()
  (smarty-hooked-abbrev 'smarty-template-strip))

(defun smarty-template-assign-hook ()
  (smarty-hooked-abbrev 'smarty-template-assign))
(defun smarty-template-counter-hook ()
  (smarty-hooked-abbrev 'smarty-template-counter))
(defun smarty-template-cycle-hook ()
  (smarty-hooked-abbrev 'smarty-template-cycle))
(defun smarty-template-debug-hook ()
  (smarty-hooked-abbrev 'smarty-template-debug))
(defun smarty-template-eval-hook ()
  (smarty-hooked-abbrev 'smarty-template-eval))
(defun smarty-template-fetch-hook ()
  (smarty-hooked-abbrev 'smarty-template-fetch))
(defun smarty-template-html-checkboxes-hook ()
  (smarty-hooked-abbrev 'smarty-template-html-checkboxes))
(defun smarty-template-html-image-hook ()
  (smarty-hooked-abbrev 'smarty-template-html-image))
(defun smarty-template-html-options-hook ()
  (smarty-hooked-abbrev 'smarty-template-html-options))
(defun smarty-template-html-radios-hook ()
  (smarty-hooked-abbrev 'smarty-template-html-radios))
(defun smarty-template-html-select-date-hook ()
  (smarty-hooked-abbrev 'smarty-template-html-select-date))
(defun smarty-template-html-select-time-hook ()
  (smarty-hooked-abbrev 'smarty-template-html-select-time))
(defun smarty-template-html-table-hook ()
  (smarty-hooked-abbrev 'smarty-template-html-table))
(defun smarty-template-mailto-hook ()
  (smarty-hooked-abbrev 'smarty-template-mailto))
(defun smarty-template-math-hook ()
  (smarty-hooked-abbrev 'smarty-template-math))
(defun smarty-template-popup-hook ()
  (smarty-hooked-abbrev 'smarty-template-popup))
(defun smarty-template-popup-init-hook ()
  (smarty-hooked-abbrev 'smarty-template-popup-init))
(defun smarty-template-textformat-hook ()
  (smarty-hooked-abbrev 'smarty-template-textformat))

(defun smarty-template-capitalize-hook ()
  (smarty-hooked-abbrev 'smarty-template-capitalize))
(defun smarty-template-cat-hook ()
  (smarty-hooked-abbrev 'smarty-template-cat))
(defun smarty-template-count-characters-hook ()
  (smarty-hooked-abbrev 'smarty-template-count-characters))
(defun smarty-template-count-paragraphs-hook ()
  (smarty-hooked-abbrev 'smarty-template-count-paragraphs))
(defun smarty-template-count-sentences-hook ()
  (smarty-hooked-abbrev 'smarty-template-count-sentences))
(defun smarty-template-count-words-hook ()
  (smarty-hooked-abbrev 'smarty-template-count-words))
(defun smarty-template-date-format-hook ()
  (smarty-hooked-abbrev 'smarty-template-date-format))
(defun smarty-template-default-hook ()
  (smarty-hooked-abbrev 'smarty-template-default))
(defun smarty-template-escape-hook ()
  (smarty-hooked-abbrev 'smarty-template-escape))
(defun smarty-template-indent-hook ()
  (smarty-hooked-abbrev 'smarty-template-indent))
(defun smarty-template-lower-hook ()
  (smarty-hooked-abbrev 'smarty-template-lower))
(defun smarty-template-nl2br-hook ()
  (smarty-hooked-abbrev 'smarty-template-nl2br))
(defun smarty-template-regex-replace-hook ()
  (smarty-hooked-abbrev 'smarty-template-regex-replace))
(defun smarty-template-replace-hook ()
  (smarty-hooked-abbrev 'smarty-template-replace))
(defun smarty-template-spacify-hook ()
  (smarty-hooked-abbrev 'smarty-template-spacify))
(defun smarty-template-string-format-hook ()
  (smarty-hooked-abbrev 'smarty-template-string-format))
(defun smarty-template-vstrip-hook ()
  (smarty-hooked-abbrev 'smarty-template-vstrip))
(defun smarty-template-strip-tags-hook ()
  (smarty-hooked-abbrev 'smarty-template-strip-tags))
(defun smarty-template-truncate-hook ()
  (smarty-hooked-abbrev 'smarty-template-truncate))
(defun smarty-template-upper-hook ()
  (smarty-hooked-abbrev 'smarty-template-upper))
(defun smarty-template-wordwrap-hook ()
  (smarty-hooked-abbrev 'smarty-template-wordwrap))

(defun smarty-template-validate-hook ()
  (smarty-hooked-abbrev 'smarty-template-validate))
(defun smarty-template-formtool-checkall-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-checkall))
(defun smarty-template-formtool-copy-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-copy))
(defun smarty-template-formtool-count-chars-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-count-chars))
(defun smarty-template-formtool-init-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-init))
(defun smarty-template-formtool-move-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-move))
(defun smarty-template-formtool-moveall-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-moveall))
(defun smarty-template-formtool-movedown-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-movedown))
(defun smarty-template-formtool-moveup-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-moveup))
(defun smarty-template-formtool-remove-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-remove))
(defun smarty-template-formtool-rename-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-rename))
(defun smarty-template-formtool-save-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-save))
(defun smarty-template-formtool-selectall-hook ()
  (smarty-hooked-abbrev 'smarty-template-formtool-selectall))
(defun smarty-template-paginate-first-hook ()
  (smarty-hooked-abbrev 'smarty-template-paginate-first))
(defun smarty-template-paginate-last-hook ()
  (smarty-hooked-abbrev 'smarty-template-paginate-last))
(defun smarty-template-paginate-middle-hook ()
  (smarty-hooked-abbrev 'smarty-template-paginate-middle))
(defun smarty-template-paginate-next-hook ()
  (smarty-hooked-abbrev 'smarty-template-paginate-next))
(defun smarty-template-paginate-prev-hook ()
  (smarty-hooked-abbrev 'smarty-template-paginate-prev))

(defun smarty-template-btosmilies-hook ()
  (smarty-hooked-abbrev 'smarty-template-btosmilies))
(defun smarty-template-bbcodetohtml-hook ()
  (smarty-hooked-abbrev 'smarty-template-bbcodetohtml))
(defun smarty-template-date-formatto-hook ()
  (smarty-hooked-abbrev 'smarty-template-date-formatto))

(defun smarty-template-capture ()
  "Insert a capture statement."
  (interactive)
  (smarty-template-generic-function "capture" t '("name" "assign") 0))

(defun smarty-template-config-load ()
  "Insert a config_load statement."
  (interactive)
  (smarty-template-generic-function "config_load" nil '("file" "section" "scope" "global") 1))

(defun smarty-template-else ()
  "Insert a else statement."
  (interactive)
  (smarty-template-generic-function "else" nil '() 0))

(defun smarty-template-elseif ()
  "Insert a elseif statement."
  (interactive)
  (smarty-template-generic-function "elseif" nil '("condition") 1 nil t))

(defun smarty-template-foreach ()
  "Insert a foreach statement."
  (interactive)
  (smarty-template-generic-function "foreach" t '("from" "item" "key" "name") 2))

(defun smarty-template-foreachelse ()
  "Insert a foreachelse statement."
  (interactive)
  (smarty-template-generic-function "foreachelse" nil '() 0))

(defun smarty-template-if ()
  "Insert a if statement."
  (interactive)
  (smarty-template-generic-function "if" t '("condition") 1 nil t))

(defun smarty-template-include ()
  "Insert a include statement."
  (interactive)
  (smarty-template-generic-function "include" nil '("file" "assign") 1 t))

(defun smarty-template-include-php ()
  "Insert a include_php statement."
  (interactive)
  (smarty-template-generic-function "include_php" nil '("file" "once" "assign") 1))

(defun smarty-template-insert ()
  "Insert a insert statement."
  (interactive)
  (smarty-template-generic-function "insert" nil '("name" "assign" "script") 1 t))

(defun smarty-template-ldelim ()
  "Insert a ldelim statement."
  (interactive)
  (smarty-template-generic-function "ldelim" nil '() 0))

(defun smarty-template-literal ()
  "Insert a literal statement."
  (interactive)
  (smarty-template-generic-function "literal" t '() 0))

(defun smarty-template-php ()
  "Insert a php statement."
  (interactive)
  (smarty-template-generic-function "php" t '() 0))

(defun smarty-template-rdelim ()
  "Insert a rdelim statement."
  (interactive)
  (smarty-template-generic-function "rdelim" nil '() 0))

(defun smarty-template-section ()
  "Insert a section statement."
  (interactive)
  (smarty-template-generic-function "section" t '("name" "loop" "start" "step" "max" "show") 2))

(defun smarty-template-sectionelse ()
  "Insert a sectionelse statement."
  (interactive)
  (smarty-template-generic-function "sectionelse" nil '() 0))

(defun smarty-template-strip ()
  "Insert a strip statement."
  (interactive)
  (smarty-template-generic-function "strip" t '() 0))


(defun smarty-template-assign ()
  "Insert a assign statement."
  (interactive)
  (smarty-template-generic-function "assign" nil '("var" "value") 2))

(defun smarty-template-counter ()
  "Insert a counter statement."
  (interactive)
  (smarty-template-generic-function "counter" nil '("name" "start" "skip" "direction" "print" "assign") 0))

(defun smarty-template-cycle ()
  "Insert a cycle statement."
  (interactive)
  (smarty-template-generic-function "cycle" nil '("values" "name" "print" "advance" "delimiter" "assign" "reset") 1))

(defun smarty-template-debug ()
  "Insert a debug statement."
  (interactive)
  (smarty-template-generic-function "debug" nil '("output") 0))

(defun smarty-template-eval ()
  "Insert a eval statement."
  (interactive)
  (smarty-template-generic-function "eval" nil '("var" "assign") 1))

(defun smarty-template-fetch ()
  "Insert a fetch statement."
  (interactive)
  (smarty-template-generic-function "fetch" nil '("file" "assign") 1))

(defun smarty-template-html-checkboxes ()
  "Insert a html_checkboxes statement."
  (interactive)
  (smarty-template-generic-function "html_checkboxes" nil '("name" "values" "output" "selected" "options" "separator" "assign" "labels") 0))

(defun smarty-template-html-image ()
  "Insert a html_image statement."
  (interactive)
  (smarty-template-generic-function "html_image" nil '("file" "height" "width" "basedir" "alt" "href" "path_prefix") 1))

(defun smarty-template-html-options ()
  "Insert a html_options statement."
  (interactive)
  (smarty-template-generic-function "html_options" nil '("name" "values" "output" "selected" "options") 0))

(defun smarty-template-html-radios ()
  "Insert a html_radios statement."
  (interactive)
  (smarty-template-generic-function "html_radios" nil '("name" "values" "output" "selected" "options" "separator" "assign") 0))

(defun smarty-template-html-select-date ()
  "Insert a html_select_date statement."
  (interactive)
  (smarty-template-generic-function "html_select_date" nil '("prefix" "time" "start_year" "end_year" "display_days" "display_months" "display_years" "month_format" "day_format" "day_value_format" "year_as_text" "reverse_years" "field_array" "day_size" "month_size" "year_size" "all_extra" "day_extra" "month_extra" "year_extra" "field_order" "field_separator" "month_value_format" "year_empty" "month_empty" "day_empty") 0))

(defun smarty-template-html-select-time ()
  "Insert a html_select_time statement."
  (interactive)
  (smarty-template-generic-function "html_select_time" nil '("prefix" "time" "display_hours" "display_minutes" "display_seconds" "display_meridian" "use_24_hours" "minute_interval" "second_interval" "field_array" "all_extra" "hour_extra" "minute_extra" "second_extra" "meridian_extra") 0))

(defun smarty-template-html-table ()
  "Insert a html_table statement."
  (interactive)
  (smarty-template-generic-function "html_table" nil '("loop" "cols" "rows" "inner" "caption" "table_attr" "th_attr" "tr_attr" "td_attr" "trailpad" "hdir" "vdir") 1))

(defun smarty-template-mailto ()
  "Insert a mailto statement."
  (interactive)
  (smarty-template-generic-function "mailto" nil '("address" "text" "encode" "cc" "bcc" "subject" "newsgroups" "followupto" "extra") 1))

(defun smarty-template-math ()
  "Insert a math statement."
  (interactive)
  (smarty-template-generic-function "math" nil '("equation" "var" "format" "assign") 2 t))

(defun smarty-template-popup ()
  "Insert a popup statement."
  (interactive)
  (smarty-template-generic-function "popup" nil '("text" "trigger" "sticky" "caption" "fgcolor" "bgcolor" "textcolor" "capcolor" "closecolor" "textfont" "captionfont" "closefont" "textsize" "captionsize" "closesize" "width" "height" "left" "right" "center" "above" "below" "border" "offsetx" "offsety" "fgbackground" "bgbackground" "closetext" "noclose" "status" "autostatus" "autostatuscap" "inarray" "caparray" "capicon" "snapx" "snapy" "fixx" "fixy" "background" "padx" "pady" "fullhtml" "frame" "function" "delay" "hauto" "vauto") 1))

(defun smarty-template-popup-init ()
  "Insert a popup_init statement."
  (interactive)
  (smarty-template-generic-function "popup_init" nil '("src") 1))

(defun smarty-template-textformat ()
  "Insert a textformat statement."
  (interactive)
  (smarty-template-generic-function "textformat" t '("style" "indent" "indent_first" "indent_char" "wrap" "wrap_char" "wrap_cut" "assign") 0))

(defun smarty-template-capitalize ()
  "Insert a capitalize statement."
  (interactive)
  (smarty-template-generic-modifier "capitalize" '("upcase_numeric") 0))

(defun smarty-template-cat ()
  "Insert a cat statement."
  (interactive)
  (smarty-template-generic-modifier "cat" '("value") 0))

(defun smarty-template-count-characters ()
  "Insert a count_characters statement."
  (interactive)
  (smarty-template-generic-modifier "count_characters" '("include_whitespace") 0))

(defun smarty-template-count-paragraphs ()
  "Insert a count_paragraphs statement."
  (interactive)
  (smarty-template-generic-modifier "count_paragraphs" '() 0))

(defun smarty-template-count-sentences ()
  "Insert a count_sentences statement."
  (interactive)
  (smarty-template-generic-modifier "count_sentences" '() 0))

(defun smarty-template-count-words ()
  "Insert a count_words statement."
  (interactive)
  (smarty-template-generic-modifier "count_words" '() 0))

(defun smarty-template-date-format ()
  "Insert a date_format statement."
  (interactive)
  (smarty-template-generic-modifier "date_format" '("format" "default") 0))

(defun smarty-template-default ()
  "Insert a default statement."
  (interactive)
  (smarty-template-generic-modifier "default" '("value") 0))

(defun smarty-template-escape ()
  "Insert a escape statement."
  (interactive)
  (smarty-template-generic-modifier "escape" '("html|htmlall|url|urlpathinfo|quotes|hex|hexentity|javascript|mail" "charset") 0))

(defun smarty-template-indent ()
  "Insert a indent statement."
  (interactive)
  (smarty-template-generic-modifier "indent" '("value" "character") 0))

(defun smarty-template-lower ()
  "Insert a lower statement."
  (interactive)
  (smarty-template-generic-modifier "lower" '() 0))

(defun smarty-template-nl2br ()
  "Insert a nl2br statement."
  (interactive)
  (smarty-template-generic-modifier "nl2br" '() 0))

(defun smarty-template-regex-replace ()
  "Insert a regex_replace statement."
  (interactive)
  (smarty-template-generic-modifier "regex_replace" '("regexp" "string_to_replace") 2))

(defun smarty-template-replace ()
  "Insert a replace statement."
  (interactive)
  (smarty-template-generic-modifier "replace" '("string" "string_to_replace_with") 2))

(defun smarty-template-spacify ()
  "Insert a spacify statement."
  (interactive)
  (smarty-template-generic-modifier "spacify" '("character") 0))

(defun smarty-template-string-format ()
  "Insert a string_format statement."
  (interactive)
  (smarty-template-generic-modifier "string_format" '("format") 1))

(defun smarty-template-vstrip ()
  "Insert a strip statement."
  (interactive)
  (smarty-template-generic-modifier "strip" '() 0))

(defun smarty-template-strip-tags ()
  "Insert a strip_tags statement."
  (interactive)
  (smarty-template-generic-modifier "strip_tags" '("replace_by_space") 0))

(defun smarty-template-truncate ()
  "Insert a truncate statement."
  (interactive)
  (smarty-template-generic-modifier "truncate" '("count" "text_to_replace" "character_boundary" "middle_string") 0))

(defun smarty-template-upper ()
  "Insert a upper statement."
  (interactive)
  (smarty-template-generic-modifier "upper" '() 0))

(defun smarty-template-wordwrap ()
  "Insert a wordwrap statement."
  (interactive)
  (smarty-template-generic-modifier "wordwrap" '("count" "string" "character_boundary") 0))


(defun smarty-template-validate ()
  "Insert a validate statement."
  (interactive)
  (smarty-template-generic-function "validate" nil '("field" "criteria" "message" "form" "transform" "trim" "empty" "halt" "assign" "append" "page") 3))

(defun smarty-template-formtool-checkall ()
  "Insert a formtool_checkall statement."
  (interactive)
  (smarty-template-generic-function "formtool_checkall" nil '("name" "class" "style") 1))

(defun smarty-template-formtool-copy ()
  "Insert a formtool_copy statement."
  (interactive)
  (smarty-template-generic-function "formtool_copy" nil '("from" "to" "save" "button_text" "all" "counter" "class" "style") 3))

(defun smarty-template-formtool-count-chars ()
  "Insert a formtool_count_chars statement."
  (interactive)
  (smarty-template-generic-function "formtool_count_chars" nil '("name" "limit" "alert") 3))

(defun smarty-template-formtool-init ()
  "Insert a formtool_init statement."
  (interactive)
  (smarty-template-generic-function "formtool_init" nil '("src") 1))

(defun smarty-template-formtool-move ()
  "Insert a formtool_move statement."
  (interactive)
  (smarty-template-generic-function "formtool_move" nil '("from" "to" "save_from" "save_to" "all" "count_to" "count_from" "class" "style") 4))

(defun smarty-template-formtool-moveall ()
  "Insert a formtool_moveall statement."
  (interactive)
  (smarty-template-generic-function "formtool_moveall" nil '("from" "to" "save_from" "save_to" "all" "count_to" "count_from" "class" "style") 4))

(defun smarty-template-formtool-movedown ()
  "Insert a formtool_movedown statement."
  (interactive)
  (smarty-template-generic-function "formtool_movedown" nil '("save" "name" "class" "style") 2))

(defun smarty-template-formtool-moveup ()
  "Insert a formtool_moveup statement."
  (interactive)
  (smarty-template-generic-function "formtool_moveup" nil '("save" "name" "class" "style") 2))

(defun smarty-template-formtool-remove ()
  "Insert a formtool_remove statement."
  (interactive)
  (smarty-template-generic-function "formtool_remove" nil '("from" "save" "all" "counter" "class" "style") 2))

(defun smarty-template-formtool-rename ()
  "Insert a formtool_rename statement."
  (interactive)
  (smarty-template-generic-function "formtool_rename" nil '("name" "from" "save" "class" "style") 3))

(defun smarty-template-formtool-save ()
  "Insert a formtool_save statement."
  (interactive)
  (smarty-template-generic-function "formtool_save" nil '("from" "name" "save") 3))

(defun smarty-template-formtool-selectall ()
  "Insert a formtool_selectall statement."
  (interactive)
  (smarty-template-generic-function "formtool_selectall" nil '("name" "class" "style") 1))

(defun smarty-template-paginate-first ()
  "Insert a paginate_first statement."
  (interactive)
  (smarty-template-generic-function "paginate_first" nil '("id" "text") 0))

(defun smarty-template-paginate-last ()
  "Insert a paginate_last statement."
  (interactive)
  (smarty-template-generic-function "paginate_last" nil '("id" "text") 0))

(defun smarty-template-paginate-middle ()
  "Insert a paginate_middle statement."
  (interactive)
  (smarty-template-generic-function "paginate_middle" nil '("id" "format" "prefix" "page_limit" "link_prefix" "link_suffix") 0))

(defun smarty-template-paginate-next ()
  "Insert a paginate_next statement."
  (interactive)
  (smarty-template-generic-function "paginate_next" nil '("id" "text") 0))

(defun smarty-template-paginate-prev ()
  "Insert a paginate_prev statement."
  (interactive)
  (smarty-template-generic-function "paginate_prev" nil '("id" "text") 0))


(defun smarty-template-btosmilies ()
  "Insert a B2Smilies statement."
  (interactive)
  (smarty-template-generic-modifier "B2Smilies" '() 0))

(defun smarty-template-bbcodetohtml ()
  "Insert a bbcode2html statement."
  (interactive)
  (smarty-template-generic-modifier "bbcode2html" '() 0))

(defun smarty-template-date-formatto ()
  "Insert a date_format2 statement."
  (interactive)
  (smarty-template-generic-modifier "date_format2" '("format" "default") 0))

;;

(defun smarty-resolve-env-variable (string)
  "Resolve environment variables in STRING."
  (while (string-match "\\(.*\\)${?\\(\\(\\w\\|_\\)+\\)}?\\(.*\\)" string)
    (setq string (concat (match-string 1 string)
                         (getenv (match-string 2 string))
                         (match-string 4 string))))
  string)

(defun smarty-insert-string-or-file (string)
  "Insert STRING or file contents if STRING is an existing file name."
  (unless (equal string "")
    (let ((file-name
           (progn (string-match "^\\([^\n]+\\)" string)
                  (smarty-resolve-env-variable (match-string 1 string)))))
      (if (file-exists-p file-name)
           (forward-char (cadr (insert-file-contents file-name)))
        (insert string)))))

(defun smarty-template-insert-date ()
  "Insert date in appropriate format."
  (interactive)
  (insert
   (cond
    ;; 'american, 'european, 'scientific kept for backward compatibility
    ((eq smarty-date-format 'american) (format-time-string "%m/%d/%Y" nil))
    ((eq smarty-date-format 'european) (format-time-string "%d.%m.%Y" nil))
    ((eq smarty-date-format 'scientific) (format-time-string "%Y/%m/%d" nil))
    (t (format-time-string smarty-date-format nil)))))

(defun smarty-template-header (&optional file-title)
  "Insert a Smarty file header."
  (interactive)
  (unless (equal smarty-file-header "")
    (let (pos)
      (save-excursion
        (smarty-insert-string-or-file smarty-file-header)
        (setq pos (point-marker)))
      (smarty-template-replace-header-keywords
       (point-min-marker) pos file-title))))

(defun smarty-template-footer ()
  "Insert a Smarty file footer."
  (interactive)
  (unless (equal smarty-file-footer "")
    (let (pos)
      (save-excursion
        (setq pos (point-marker))
        (smarty-insert-string-or-file smarty-file-footer)
        (unless (= (preceding-char) ?\n)
          (insert "\n")))
      (smarty-template-replace-header-keywords pos (point-max-marker)))))

(defun smarty-template-replace-header-keywords (beg end &optional file-title is-model)
  "Replace keywords in header and footer."
  (let ()
    (smarty-prepare-search-2
     (save-excursion
       (goto-char beg)
       (while (search-forward "<filename>" end t)
         (replace-match (buffer-name) t t))
       (goto-char beg)
       (while (search-forward "<copyright>" end t)
         (replace-match smarty-copyright-string t t))
       (goto-char beg)
       (while (search-forward "<author>" end t)
         (replace-match "" t t)
         (insert (user-full-name))
         (when user-mail-address (insert "  <" user-mail-address ">")))
       (goto-char beg)
       (while (search-forward "<login>" end t)
         (replace-match (user-login-name) t t))
       (goto-char beg)
       (while (search-forward "<company>" end t)
         (replace-match smarty-company-name t t))
       (goto-char beg)
       ;; Replace <RCS> with $, so that RCS for the source is
       ;; not over-enthusiastic with replacements
       (while (search-forward "<RCS>" end t)
         (replace-match "$" nil t))
       (goto-char beg)
       (while (search-forward "<date>" end t)
         (replace-match "" t t)
         (smarty-template-insert-date))
       (goto-char beg)
       (while (search-forward "<year>" end t)
         (replace-match (format-time-string "%Y" nil) t t))
       (goto-char beg)
       (let (string)
         (while
             (re-search-forward "<\\(\\(\\w\\|\\s_\\)*\\) string>" end t)
           (setq string (read-string (concat (match-string 1) ": ")))
           (replace-match string t t)))
       (goto-char beg)
       (when (and (not is-model) (search-forward "<cursor>" end t))
         (replace-match "" t t))))))

(provide 'smarty-mode)
;;; smarty-mode.el ends here

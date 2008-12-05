;;; anything-config.el --- predefined configurations for anything

;; Copyright (C) 2007 Tassilo Horn

;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>

;; Contributors:
;;     Tamas Patrovics
;;     Tassilo Horn <tassilo@member.fsf.org>
;;     Vagn Johansen <gonz808@hotmail.com>
;;     Mathias Dahl <mathias.dahl@gmail.com>
;;     Bill Clementson <billclem@gmail.com>
;;     Stefan Kamphausen (see http://www.skamphausen.de for more informations)
;;     Drew Adams <drew.adams@oracle.com>
;;     Jason McBrayer <jmcbray@carcosa.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; This package provides predefined configurations for anything.el.  To enable
;; it, put this into your `user-init-file'.
;;
;;   (require 'anything-config) ; loads anything.el too
;;
;; To configure anything you should tweak the variables `anything-sources' and
;; `anything-type-attributes'.  See the default configuration at the bottom of
;; this file.  Copy and modify it to your needs.
;;
;;   (setq anything-sources ...)
;;   (setq anything-type-attributes ...)
;;

;;; Bugs and TODOs:

;;
;; - TODO: anything-c-adaptive stores infos for sources/types that don't have
;;   set it as `filtered-candidate-transformer'.
;;
;; - TODO: The Imenu source is broken for very recent emacsen.  It seems to
;;   work with 22.1, though.
;;

;;; Startup

(require 'anything)

;;; Version

(defvar anything-c-version "<2008-08-13 Wed 14:49>"
  "The version of anything-config.el, or better the date of the
last change.")

;;; Keymaps

(defvar anything-c-use-standard-keys t
  "If non-nil the keybindings of anything will be the standard
bindings used in most parts of emacs, e.g. M-p/M-n for minibuffer
history, C-s for isearch, etc.

If it's nil anything uses some bindings that don't conflict with
`iswitchb', e.g. C-p/C-n for the minibuffer history.  If you use
`iswitchb' you probably should say nil here.")

(when anything-c-use-standard-keys
  (setq anything-map
        (let ((map (copy-keymap minibuffer-local-map)))
          (define-key map (kbd "<down>")  'anything-next-line)
          (define-key map (kbd "<up>")    'anything-previous-line)
          (define-key map (kbd "C-n")     'anything-next-line)
          (define-key map (kbd "C-p")     'anything-previous-line)
          (define-key map (kbd "<prior>") 'anything-previous-page)
          (define-key map (kbd "<next>")  'anything-next-page)
          (define-key map (kbd "M-v")     'anything-previous-page)
          (define-key map (kbd "C-v")     'anything-next-page)
          (define-key map (kbd "<right>") 'anything-next-source)
          (define-key map (kbd "<left>")  'anything-previous-source)
          (define-key map (kbd "<RET>")   'anything-exit-minibuffer)
          (define-key map (kbd "C-1")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-2")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-3")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-4")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-5")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-6")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-7")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-8")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-9")     'anything-select-with-digit-shortcut)
          (define-key map (kbd "<tab>")   'anything-select-action)
          (defalias 'anything-next-history-element     'next-history-element)
          (defalias 'anything-previous-history-element 'previous-history-element)
          (define-key map (kbd "M-p")     'anything-previous-history-element)
          (define-key map (kbd "M-n")     'anything-next-history-element)
          (define-key map (kbd "C-s")     'anything-isearch)
          (define-key map (kbd "C-r")     'undefined)
          map))

  (setq anything-isearch-map
        (let ((map (copy-keymap (current-global-map))))
          (define-key map (kbd "<return>")    'anything-isearch-default-action)
          (define-key map (kbd "<tab>")       'anything-isearch-select-action)
          (define-key map (kbd "C-g")         'anything-isearch-cancel)
          (define-key map (kbd "C-s")         'anything-isearch-again)
          (define-key map (kbd "C-r")         'undefined)
          (define-key map (kbd "<backspace>") 'anything-isearch-delete)
          ;; add printing chars
          (let ((i ?\s))
            (while (< i 256)
              (define-key map (vector i) 'anything-isearch-printing-char)
              (setq i (1+ i))))
          map)))

;;; Sources

;;;; Match functions

(defun anything-c-match-on-file-name (candidate)
  "Return non-nil if `anything-pattern' matches the
filename (without directory part) of CANDIDATE."
  (string-match anything-pattern (file-name-nondirectory candidate)))

(defun anything-c-match-on-directory-name (candidate)
  "Return non-nil if `anything-pattern' matches the directory
part of CANDIDATE (a file)."
  (let ((dir (file-name-directory candidate)))
    (when dir
      (string-match anything-pattern dir))))

(defun anything-c-string-match (candidate)
  "Return non-nil if `anything-pattern' matches CANDIDATE.
The match is done with `string-match'."
  (string-match anything-pattern candidate))

;;;; Buffers

(defun anything-c-buffer-list ()
  "Return the list of names of buffers with the `anything-buffer'
and hidden buffers filtered out.  The first buffer in the list
will be the last recently used buffer that is not the current
buffer."
  (let ((buffers (remove-if (lambda (name)
                              (or (equal name anything-buffer)
                                  (eq ?\  (aref name 0))))
                            (mapcar 'buffer-name (buffer-list)))))
    (append (cdr buffers) (list (car buffers)))))

(defvar anything-c-source-buffers
  '((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (volatile)
    (type . buffer)))

(defun anything-c-define-dummy-source (name func &rest other-attrib)
  `((name . ,name)
    (candidates "dummy")
    ,@other-attrib
    (filtered-candidate-transformer
     . (lambda (candidates source)
         (funcall ',func)))
    (requires-pattern . 1)
    (volatile)
    (category create)))

(defun anything-c-dummy-candidate ()
  ;; `source' is defined in filtered-candidate-transformer
  (list (cons (concat (assoc-default 'name source) 
                      " '" anything-input "'")
              anything-input)))

(defvar anything-c-source-buffer-not-found
  (anything-c-define-dummy-source
   "Create buffer"
   (lambda () (unless (get-buffer anything-input)
                (anything-c-dummy-candidate)))
   '(type . buffer)))

;;;; File name history

(defvar anything-c-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file)))

;;;; Recentf files

(defvar anything-c-source-recentf
  '((name . "Recentf")
    (candidates . recentf-list)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file))
  "See (info \"(emacs)File Conveniences\").")

;;;; Files in current dir

(defvar anything-c-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (init . (lambda ()
              (setq anything-c-default-directory
                    default-directory)))
    (candidates . (lambda ()
                    (directory-files
                     anything-c-default-directory)))
    (volatile)
    (type . file)))

;;;; Man Pages

(defvar anything-c-man-pages nil
  "All man pages on system.
Will be calculated the first time you invoke anything with this
source.")

(defvar anything-c-source-man-pages
  `((name . "Manual Pages")
    (candidates . (lambda ()
                    (if anything-c-man-pages
                        anything-c-man-pages
                      ;; XEmacs doesn't have a woman :)
                      (setq anything-c-man-pages
                            (condition-case nil
                                (progn
                                  (require 'woman)
                                  (woman-file-name "")
                                  (sort (mapcar 'car
                                                woman-topic-all-completions)
                                        'string-lessp))
                              (error nil))))))
    (action . (("Show with Woman" . woman)))
    (requires-pattern . 2)))

;;;; Info pages

(defvar anything-c-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke anything with this
source.")

(defvar anything-c-source-info-pages
  `((name . "Info Pages")
    (candidates
     . (lambda ()
         (if anything-c-info-pages
             anything-c-info-pages
           (setq anything-c-info-pages
                 (save-window-excursion
                   (save-excursion
                     (require 'info)
                     (Info-find-node "dir" "top")
                     (goto-char (point-min))
                     (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
                           topics)
                       (while (re-search-forward info-topic-regexp nil t)
                         (add-to-list 'topics (match-string-no-properties 1)))
                       (goto-char (point-min))
                       (Info-exit)
                       topics)))))))
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string "^[^:]+: "
                                                                    ""
                                                                    node-str))))))
    (requires-pattern . 2)))

;;;; Complex command history

(defvar anything-c-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda ()
                    (mapcar 'prin1-to-string
                            command-history)))
    (volatile)
    (type . sexp)))

;;;; Emacs commands

(defvar anything-c-source-emacs-commands
  '((name . "Emacs Commands")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (commandp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (volatile)
    (type . command)
    (requires-pattern . 2))
  "Source for completing and invoking Emacs commands.  A command
is a function with interactive spec that can be invoked with
`M-x'.

To get non-interactive functions listed, use
`anything-c-source-emacs-functions'.")

;;;; Emacs functions

;;;;; Normal

(defvar anything-c-source-emacs-functions
  '((name . "Emacs Functions")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (functionp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (volatile)
    (type . function)
    (requires-pattern . 2))
  "Source for completing Emacs functions.")

;;;;; With abbrev expansion similar to my exec-abbrev-cmd.el

;; See http://www.tsdh.de/cgi-bin/wiki.pl/exec-abbrev-cmd.el

(defvar anything-c-function-abbrev-regexp nil
  "Regexp built from the current `anything-pattern' interpreting
it as abbreviation.  Only for internal use.")

(defun anything-c-match-function-by-abbrev (candidate)
  "Return non-nil if `anything-pattern' is an abbreviation of the
function CANDIDATE.

Abbreviations are made by taking the first character from each
word in the function's name, e.g. \"bb\" is an abbrev for
`bury-buffer', \"stb\" is an abbrev for `switch-to-buffer'."
  (string-match anything-c-function-abbrev-regexp candidate))

(defvar anything-c-source-emacs-functions-with-abbrevs
  (append anything-c-source-emacs-functions
          '((match . (anything-c-match-function-by-abbrev
                      anything-c-string-match)))
          '((init . (lambda ()
                      (defadvice anything-update
                        (before anything-c-update-function-abbrev-regexp activate)
                        (let ((char-list (append anything-pattern nil))
                              (str "^"))
                          (dolist (c char-list)
                            (setq str (concat str (list c) "[^-]*-")))
                          (setq str (concat (substring str 0 (1- (length str))) "$"))
                          (setq anything-c-function-abbrev-regexp str))))))))

;;;; Bookmarks

(defvar anything-c-source-bookmarks
  '((name . "Bookmarks")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . bookmark-all-names)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

;;;; Picklist

(defvar anything-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda ()
                    (mapcar 'car picklist-list)))
    (volatile)
    (type . file)))

;;;; Imenu

(defvar anything-c-source-imenu
  '((name . "Imenu")
    (init . (lambda ()
              (setq anything-c-imenu-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (condition-case nil
                        (with-current-buffer anything-c-imenu-current-buffer
                          (mapcar (lambda (x)
                                    (cons (car x) x))
                                  ;; leave only top level completions for
                                  ;; simplicity (could be more sophisticated)
                                  (remove-if-not (lambda (x)
                                                   (markerp (cdr x)))
                                                 (imenu--make-index-alist))))
                      (error nil))))
    (volatile)
    (action . imenu)))

;;;; File Cache

(defvar anything-c-source-file-cache-initialized nil)

(defvar anything-c-file-cache-files nil)

(defvar anything-c-source-file-cache
  '((name . "File Cache")
    (init . (lambda ()
              (unless anything-c-source-file-cache-initialized
                (setq anything-c-file-cache-files
                      (loop for item in file-cache-alist append
                            (destructuring-bind (base &rest dirs) item
                              (loop for dir in dirs collect
                                    (concat dir base)))))
                (defadvice file-cache-add-file (after file-cache-list activate)
                  (add-to-list 'anything-c-file-cache-files (expand-file-name file)))
                (setq anything-c-source-file-cache-initialized t))))
    (candidates . anything-c-file-cache-files)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file)))

;;;; Locate

(defvar anything-c-locate-options (if (eq system-type 'darwin)
                                      '("locate")
                                    '("locate" "-i" "-r"))
  "A list where the `car' is the name of the locat program
followed by options.  The search pattern will be appended, so the
\"-r\" option should be the last option.")

(defvar anything-c-source-locate
  '((name . "Locate")
    (candidates . (lambda ()
                    (apply 'start-process "locate-process" nil
                           (append anything-c-locate-options
                                   (list anything-pattern)))))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with locate.")

;;;; Tracker desktop search

(defvar anything-c-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;;;; Spotlight (MacOS X desktop search)

(defvar anything-c-source-mac-spotlight
  '((name . "mdfind")
    (candidates . (lambda ()
                    (start-process "mdfind-process" nil
                                   "mdfind" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

;;;; BBDB

(defun anything-c-bbdb-candidates ()
  "Return a list of all names in the bbdb database.  The format
is \"Firstname Lastname\"."
  (mapcar (lambda (bbdb-record)
            (concat (aref bbdb-record 0) " " (aref bbdb-record 1)))
          (bbdb-records)))

(defun anything-c-bbdb-create-contact (actions candidate)
  "Action transformer that returns only an entry to add the
current `anything-pattern' as new contact.  All other actions are
removed."
  (if (string= candidate "*Add to contacts*")
      '(("Add to contacts"
         . (lambda (actions)
             (bbdb-create-internal
              (read-from-minibuffer "Name: " anything-c-bbdb-name)
              (read-from-minibuffer "Company: ")
              (read-from-minibuffer "Email: ")
              nil
              nil
              (read-from-minibuffer "Note: ")))))
    actions))

(defvar anything-c-bbdb-name nil
  "Only for internal use.")

(defvar anything-c-source-bbdb
  '((name . "BBDB")
    (candidates . anything-c-bbdb-candidates)
    (volatile)
    (action ("View person's data" . (lambda (candidate)
                                      (bbdb candidate nil)
                                      (set-buffer "*BBDB*")
                                      (bbdb-redisplay-one-record (bbdb-current-record))))
            ("Send a mail" . (lambda (candidate)
                               (let ((rec (progn
                                            (bbdb candidate nil)
                                            (set-buffer "*BBDB*")
                                            (bbdb-current-record))))
                                 (bbdb-send-mail rec)))))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (setq anything-c-bbdb-name anything-pattern)
                                        (if (not candidates)
                                            (list "*Add to contacts*")
                                          candidates)))
    (action-transformer . (lambda (actions candidate)
                            (anything-c-bbdb-create-contact actions candidate)))))

;;;; Evaluation Result

(defvar anything-c-source-evaluation-result
  '((name . "Evaluation Result")
    (requires-pattern)
    (match (lambda (candidate) t))
    (candidates  "dummy")
    (filtered-candidate-transformer
     . (lambda (candidates source)
         (list
          (condition-case nil
              (prin1-to-string
               (eval (read anything-pattern)))
            (error "error")))))
    (volatile)
    (action ("Do Nothing" . ignore))))

;;;; Calculation Result

(defvar anything-c-source-calculation-result
  '((name . "Calculation Result")
    (requires-pattern)
    (match (lambda (candidate) t))
    (candidates  "dummy")
    (filtered-candidate-transformer
     . (lambda (candidates source)
         (list
          (condition-case nil
              (calc-eval anything-pattern)
            (error "error")))))
    (volatile)
    (action ("Do Nothing" . ignore))))

;;;; Google Suggestions

(defvar anything-c-source-google-suggest
  '((name . "Google Suggest")
    (candidates
     . (lambda ()
         (let ((suggestions (anything-c-google-suggest-fetch anything-input)))
           (if (some (lambda (suggestion)
                       (equal (cdr suggestion) anything-input))
                     suggestions)
               suggestions
             ;; if there is no suggestion exactly matching the input then
             ;; prepend a Search on Google item to the list
             (append (list (cons (concat "Search for "
                                         "'" anything-input "'"
                                         " on Google")
                                 anything-input))
                     suggestions)))))
    (action . (("Google Search" .
                (lambda (candidate)
                  (browse-url (concat anything-c-google-suggest-search-url
                                      (url-hexify-string candidate)))))))
    (volatile)
    (requires-pattern . 3)
    (delayed)))

(defvar anything-c-google-suggest-url
  "http://www.google.com/complete/search?hl=en&js=true&qu="
  "URL used for looking up suggestions.")

(defvar anything-c-google-suggest-search-url
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
  "URL used for searching.")

(defun anything-c-google-suggest-fetch (input)
  "Fetch suggestions for INPUT."
  (let* ((result (with-current-buffer
                     (url-retrieve-synchronously
                      (concat anything-c-google-suggest-url
                              (url-hexify-string input)))
                   (buffer-substring (point-min) (point-max))))
         (split (split-string result "new Array("))
         (suggestions (anything-c-google-suggest-get-items (second split)))
         (numbers (anything-c-google-suggest-get-items (third split)))
         (longest (+ (apply 'max 0 (let (lengths)
                                     (dotimes (i (length suggestions))
                                       (push (+ (length (nth i suggestions))
                                                (length (nth i numbers)))
                                             lengths))
                                     lengths))
                     10))
         items)
    (dotimes (i (length suggestions))
      (let ((suggestion (nth i suggestions))
            (number (nth i numbers)))
        (push (cons (concat suggestion
                            (make-string (- longest
                                            (length suggestion)
                                            (length number))
                                         32)
                            number)
                    suggestion)
              items)))
    items))

(defun anything-c-google-suggest-get-items (str)
  "Extract items from STR returned by Google Suggest."
  (let ((start nil)
        items)
    (while (string-match "\"\\([^\"]+?\\)\"" str start)
      (push (match-string 1 str) items)
      (setq start (1+ (match-end 1))))
    items))

;;;; Jabber Contacts (jabber.el)

(defun anything-c-jabber-online-contacts ()
  "List online Jabber contacts."
  (let (jids)
    (dolist (item (jabber-concat-rosters) jids)
      (when (get item 'connected)
        (push (if (get item 'name)
                  (cons (get item 'name) item)
                (cons (symbol-name item) item)) jids)))))

(defvar anything-c-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda ()
                    (mapcar
                     'car
                     (anything-c-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (anything-c-jabber-online-contacts)))))))))

;;; Type Action helpers

;;;; Files

(defvar anything-c-external-commands-list nil
  "A list of all external commands the user can execute.  If this
variable is not set by the user, it will be calculated
automatically.")

(defun anything-c-external-commands-list-1 ()
  "Returns a list of all external commands the user can execute.

If `anything-c-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `anything-c-external-commands-list'.

The code is ripped out of `eshell-complete-commands-list'."
  (if anything-c-external-commands-list
      anything-c-external-commands-list
    (setq anything-c-external-commands-list
          (let* ((paths (split-string (getenv "PATH") path-separator))
                 (cwd (file-name-as-directory
                       (expand-file-name default-directory)))
                 (path "") (comps-in-path ())
                 (file "") (filepath "") (completions ()))
            ;; Go thru each path in the search path, finding completions.
            (while paths
              (setq path (file-name-as-directory
                          (expand-file-name (or (car paths) ".")))
                    comps-in-path
                    (and (file-accessible-directory-p path)
                         (file-name-all-completions "" path)))
              ;; Go thru each completion found, to see whether it should be
              ;; used, e.g. see if it's executable.
              (while comps-in-path
                (setq file (car comps-in-path)
                      filepath (concat path file))
                (if (and (not (member file completions))
                         (or (string-equal path cwd)
                             (not (file-directory-p filepath)))
                         (file-executable-p filepath))
                    (setq completions (cons file completions)))
                (setq comps-in-path (cdr comps-in-path)))
              (setq paths (cdr paths)))
            completions))))

(defun anything-c-file-buffers (filename)
  "Returns a list of those buffer names which correspond to the
file given by FILENAME."
  (let (name ret)
    (dolist (buf (buffer-list) ret)
      (let ((bfn (buffer-file-name buf)))
        (when (and bfn
                   (string= filename bfn))
          (push (buffer-name buf) ret)))
      ret)))

(defun anything-c-delete-file (file)
  "Delete the given file after querying the user.  Ask to kill
buffers associated with that file, too."
  (if (y-or-n-p (format "Really delete file %s? " file))
      (progn
        (let ((buffers (anything-c-file-buffers file)))
          (delete-file file)
          (dolist (buf buffers)
            (when (y-or-n-p (format "Kill buffer %s, too? " buf))
              (kill-buffer buf)))))
    (message "Nothing deleted.")))

(defun anything-c-open-file-externally (file)
  "Open FILE with an external tool.  Query the user which tool to
use."
  (start-process "anything-c-open-file-externally"
                 nil
                 (completing-read "Program: "
                                  (anything-c-external-commands-list-1))
                 file))

(defun w32-shell-execute-open-file (file)
  (interactive "fOpen file:")
  (w32-shell-execute "open" (replace-regexp-in-string ;for UNC paths
                             "/" "\\"
                             (replace-regexp-in-string ; strip cygdrive paths
                              "/cygdrive/\\(.\\)" "\\1:" file nil nil) nil t)))
(defun anything-c-open-file-with-default-tool (file)
  "Open FILE with the default tool on this platform."
  (if (eq system-type 'windows-nt)
      (w32-shell-execute-open-file file)
    (start-process "anything-c-open-file-with-default-tool"
                   nil
                   (cond ((eq system-type 'gnu/linux)
                          "xdg-open")
                         ((or (eq system-type 'darwin)  ;; Mac OS X
                              (eq system-type 'macos))  ;; Mac OS 9
                          "open"))
                   file)))

(defun anything-c-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
    (dired (file-name-directory file))
    (dired-goto-file file)))

;;; Action Transformers

;;;; Files

(defun anything-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "el")
          (string= (file-name-extension candidate) "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
    actions))

(defun anything-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file.  Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "htm")
          (string= (file-name-extension candidate) "html"))
      (append actions '(("Browse with Browser" . browse-url)))
    actions))

;;;; Function

(defun anything-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern candidate))
      (append actions '(("Call Interactively"
                         .
                         (lambda (c)
                           (call-interactively (intern c))))))
    actions))

;;;; S-Expressions

(defun anything-c-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' is a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" .
              (lambda (sexp)
                (let ((sym (read sexp)))
                  (eval sym)
                  (setq command-history
                        (cons sym command-history)))))
            actions)
    actions))

;;; Candidate Transformers

;;;; Files

(defvar anything-c-boring-file-regexp
  (rx (or
       ;; Boring directories
       (and "/" (or ".svn" "CVS" "_darcs" ".git" ".hg") (or "/" eol))
       ;; Boring files
       (and (or ".class" ".la" ".o" "~") eol)))
  "File candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-files' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-files' is used.")

(defun anything-c-shadow-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
displayed with the `file-name-shadow' face if available."
  (mapcar (lambda (file)
            ;; Add shadow face property to boring files.
            (let ((face (if (facep 'file-name-shadow)
                            'file-name-shadow
                          ;; fall back to default on XEmacs
                          'default)))
              (if (string-match anything-c-boring-file-regexp file)
                  (setq file (propertize file 'face face))))
            file)
          files))

(defun anything-c-skip-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
skipped."
  (let (filtered-files)
    (loop for file in files
          do (when (not (string-match anything-c-boring-file-regexp file))
               (push file filtered-files))
          finally (return (nreverse filtered-files)))))

(defun anything-c-w32-pathname-transformer (args)
  "Change undesirable features of windows pathnames to ones more acceptable to
other candidate transformers."
  (if (eq system-type 'windows-nt)
          (mapcar (lambda (x)
                    (replace-regexp-in-string "/cygdrive/\\(.\\)" "\\1:" x))
                  (mapcar (lambda (y)
                            (replace-regexp-in-string "\\\\" "/" y)) args))
    args))
            

(defun anything-c-shorten-home-path (files)
  "Replaces /home/user with ~."
  (mapcar (lambda (file)
            (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                                  (getenv "HOME"))))
              (if (string-match home file)
                  (cons (replace-match "~" nil nil file) file)
                file)))
          files))

;;;; Functions

(defun anything-c-mark-interactive-functions (functions)
  "Mark interactive functions (commands) with (i) after the function name."
  (let (list)
    (loop for function in functions
          do (push (cons (concat function
                                 (when (commandp (intern function)) " (i)"))
                         function)
                   list)
          finally (return (nreverse list)))))

;;; Filtered Candidate Transformers

;;;; Adaptive Sorting of Candidates

;; User config

(defvar anything-c-adaptive-history-file "~/.emacs.d/anything-c-adaptive-history"
  "Path of file where history information is stored.")

(defvar anything-c-adaptive-history-length 50
  "Maximum number of candidates stored for a source.")

;;----------------------------------------------------------------------

(defvar anything-c-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar anything-c-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

(defadvice anything-initialize (before anything-c-adaptive-initialize activate)
  "Advise `anything-initialize' to reset `anything-c-adaptive-done'
when anything is started."
  (setq anything-c-adaptive-done nil))

(defadvice anything-exit-minibuffer (before anything-c-adaptive-exit-minibuffer activate)
  "Advise `anything-exit-minibuffer' to store history information
when a candidate is selected with RET."
  (anything-c-adaptive-store-selection))

(defadvice anything-select-action (before anything-c-adaptive-select-action activate)
  "Advise `anything-select-action' to store history information
when the user goes to the action list with TAB."
  (anything-c-adaptive-store-selection))

(defun anything-c-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless anything-c-adaptive-done
    (setq anything-c-adaptive-done t)
    (let* ((source (anything-get-current-source))
           (source-name (or (assoc-default 'type source)
                            (assoc-default 'name source)))
           (source-info (or (assoc source-name anything-c-adaptive-history)
                            (progn
                              (push (list source-name) anything-c-adaptive-history)
                              (car anything-c-adaptive-history))))
           (selection (anything-get-selection))
           (selection-info (progn
                             (setcdr source-info
                                     (cons
                                      (let ((found (assoc selection (cdr source-info))))
                                        (if (not found)
                                            ;; new entry
                                            (list selection)

                                          ;; move entry to the beginning of the
                                          ;; list, so that it doesn't get
                                          ;; trimmed when the history is
                                          ;; truncated
                                          (setcdr source-info
                                                  (delete found (cdr source-info)))
                                          found))
                                      (cdr source-info)))
                             (cadr source-info)))
           (pattern-info (progn
                           (setcdr selection-info
                                   (cons
                                    (let ((found (assoc anything-pattern (cdr selection-info))))
                                      (if (not found)
                                          ;; new entry
                                          (cons anything-pattern 0)

                                        ;; move entry to the beginning of the
                                        ;; list, so if two patterns used the
                                        ;; same number of times then the one
                                        ;; used last appears first in the list
                                        (setcdr selection-info
                                                (delete found (cdr selection-info)))
                                        found))
                                    (cdr selection-info)))
                           (cadr selection-info))))

      ;; increase usage count
      (setcdr pattern-info (1+ (cdr pattern-info)))

      ;; truncate history if needed
      (if (> (length (cdr selection-info)) anything-c-adaptive-history-length)
          (setcdr selection-info
                  (subseq (cdr selection-info) 0 anything-c-adaptive-history-length))))))

(if (file-readable-p anything-c-adaptive-history-file)
    (load-file anything-c-adaptive-history-file))
(add-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

(defun anything-c-adaptive-save-history ()
  "Save history information to file given by
`anything-c-adaptive-history-file'."
  (interactive)
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; History entries used for anything adaptive display.\n")
    (prin1 `(setq anything-c-adaptive-history ',anything-c-adaptive-history)
           (current-buffer))
    (insert ?\n)
    (write-region (point-min) (point-max) anything-c-adaptive-history-file nil
                  (unless (interactive-p) 'quiet))))

(defun anything-c-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use for the
attribute `filtered-candidate-transformer' of a source in
`anything-sources' or a type in `anything-type-attributes'."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name anything-c-adaptive-history)))
    (if (not source-info)
        ;; if there is no information stored for this source then do nothing
        candidates
      ;; else...
      (let ((usage
             ;; ... assemble a list containing the (CANIDATE . USAGE-COUNT)
             ;; pairs
             (mapcar (lambda (candidate-info)
                       (let ((count 0))
                         (dolist (pattern-info (cdr candidate-info))
                           (if (not (equal (car pattern-info)
                                           anything-pattern))
                               (incf count (cdr pattern-info))

                             ;; if current pattern is equal to the previously
                             ;; used one then this candidate has priority
                             ;; (that's why its count is boosted by 10000) and
                             ;; it only has to compete with other candidates
                             ;; which were also selected with the same pattern
                             (setq count (+ 10000 (cdr pattern-info)))
                             (return)))
                         (cons (car candidate-info) count)))
                     (cdr source-info)))
            sorted)

        ;; sort the list in descending order, so candidates with highest
        ;; priorty come first
        (setq usage (sort usage (lambda (first second)
                                  (> (cdr first) (cdr second)))))

        ;; put those candidates first which have the highest usage count
        (dolist (info usage)
          (when (member* (car info) candidates
                         :test 'anything-c-adaptive-compare)
            (push (car info) sorted)
            (setq candidates (remove* (car info) candidates
                                      :test 'anything-c-adaptive-compare))))

        ;; and append the rest
        (append (reverse sorted) candidates nil)))))

(defun anything-c-adaptive-compare (x y)
  "Compare candidates X and Y taking into account that the
candidate can be in (DISPLAY . REAL) format."
  (equal (if (listp x)
             (cdr x)
           x)
         (if (listp y)
             (cdr y)
           y)))

;;; Default Configuration

;;;; Helper Functions

(defun anything-c-compose (arg-lst func-lst)
  "Call each function in FUNC-LST with the arguments specified in
ARG-LST.  The result of each function will be the new `car' of
ARG-LST.

This function allows easy sequencing of transformer functions."
  (dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

;;;; Sources

(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-buffer-not-found
            anything-c-source-file-name-history
            anything-c-source-info-pages
            anything-c-source-man-pages
            anything-c-source-locate
            anything-c-source-emacs-commands))

;;;; Type Attributes

(setq anything-type-attributes
      `((buffer
         (action
          ,@(if pop-up-frames
                '(("Switch to buffer other window" . switch-to-buffer-other-window)
                  ("Switch to buffer" . switch-to-buffer))
              '(("Switch to buffer" . switch-to-buffer)
                ("Switch to buffer other window" . switch-to-buffer-other-window)
                ("Switch to buffer other frame" . switch-to-buffer-other-frame)))
          ("Display buffer"   . display-buffer)
          ("Kill buffer"      . kill-buffer)))
        (file
         (action
          ,@(if pop-up-frames
                '(("Find file other window" . find-file-other-window)
                  ("Find file" . find-file))
              '(("Find file" . find-file)
                ("Find file other window" . find-file-other-window)
                ("Find file other frame" . find-file-other-frame)))
          ("Open dired in file's directory" . anything-c-open-dired)
          ("Delete file" . anything-c-delete-file)
          ("Open file externally" . anything-c-open-file-externally)
          ("Open file with default tool" . anything-c-open-file-with-default-tool))
         (action-transformer . (lambda (actions candidate)
                                 (anything-c-compose
                                  (list actions candidate)
                                  '(anything-c-transform-file-load-el
                                    anything-c-transform-file-browse-url))))
         (candidate-transformer . (lambda (candidates)
                                    (anything-c-compose
                                     (list candidates)
                                     '(anything-c-w32-pathname-transformer
                                       anything-c-skip-boring-files
                                       anything-c-shorten-home-path)))))
        (command (action ("Call interactively" . (lambda (command-name)
                                                   (call-interactively (intern command-name))))
                         ("Describe command" . (lambda (command-name)
                                                 (describe-function (intern command-name))))
                         ("Add command to kill ring" . kill-new)
                         ("Go to command's definition" . (lambda (command-name)
                                                           (find-function
                                                            (intern command-name)))))
                 ;; Sort commands according to their usage count.
                 (filtered-candidate-transformer . anything-c-adaptive-sort))
        (function (action ("Describe function" . (lambda (function-name)
                                                   (describe-function (intern function-name))))
                          ("Add function to kill ring" . kill-new)
                          ("Go to function's definition" . (lambda (function-name)
                                                             (find-function
                                                              (intern function-name)))))
                  (action-transformer . (lambda (actions candidate)
                                          (anything-c-compose
                                           (list actions candidate)
                                           '(anything-c-transform-function-call-interactively))))
                  (candidate-transformer . (lambda (candidates)
                                             (anything-c-compose
                                              (list candidates)
                                              '(anything-c-mark-interactive-functions)))))
        (sexp (action ("Eval s-expression" . (lambda (c)
                                               (eval (read c))))
                      ("Add s-expression to kill ring" . kill-new))
              (action-transformer . (lambda (actions candidate)
                                      (anything-c-compose
                                       (list actions candidate)
                                       '(anything-c-transform-sexp-eval-command-sexp)))))
        (bookmark (action ("Jump to bookmark" . bookmark-jump)
                          ("Delete bookmark" . bookmark-delete)))))

;;; Provide anything-config

(provide 'anything-config)

;; Local Variables:
;; mode: outline-minor
;; End:

;;; anything-config.el ends here

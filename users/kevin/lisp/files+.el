;;; files+.el --- Enhancements of standard library `files.el'.
;;
;; Filename: files+.el
;; Description: Enhancements of standard library `files.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2015, Drew Adams, all rights reserved.
;; Created: Fri Aug 11 14:24:13 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jan  1 10:41:22 2015 (-0800)
;;           By: dradams
;;     Update #: 730
;; URL: http://www.emacswiki.org/files+.el
;; Keywords: internal, extensions, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Enhancements of standard library `files.el'.
;;
;;  Commands defined here:
;;
;;    `dired-describe-listed-directory',
;;    `dired-mouse-describe-listed-directory',
;;
;;
;;  ***** NOTE: The following macro defined in `files.el' has been
;;              COPIED HERE:
;;
;;    `minibuffer-with-setup-hook' (Emacs 22 & 23.1 only).
;;
;;  ***** NOTE: The following functions defined in `files.el' have been
;;              REDEFINED HERE:
;;
;;    `display-buffer-other-frame' - Use `read-buffer'.
;;                                   Do not select the buffer.
;;                                   (Emacs 20-23 only)
;;    `find-file-read-args' - In Dired, use file at cursor as default
;;                            (Emacs 22 & 23.1 only).
;;    `insert-directory' - Add file count in Dired for each dir.
;;    `switch-to-buffer-other-frame'  - Use `read-buffer'.
;;                                      Return the buffer switched to.
;;                                      (Emacs 20-23 only)
;;    `switch-to-buffer-other-window' -
;;       Use `read-buffer'.
;;       Raise frame of selected window (for non-nil `pop-up-frames').
;;       (Emacs 20-23 only)
;;
;;  Load this library after loading the standard library `files.el'.
;;  However, if you use MS Windows, MS-DOS, or MacOS, then you will
;;  likely want to use library `ls-lisp+.el' together with
;;  `files+.el', to use an Emacs Lisp-only definition of
;;  `insert-directory'.
;;
;;  In that case, do *NOT* load `files+.el' directly.  Instead, just
;;  load `ls-lisp+.el' - it will load `ls-lisp.el' and `files+.el'.
;;  That is, do only this in your init file:
;;
;;   (require 'ls-lisp+)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/09/07 dadams
;;     update-dired-files-count: Corrected wrt dired-hide-details:
;;       Test inside loop, and use beginning of match, not (line-beginning-position 2).
;; 2013/11/11 dadams
;;     switch-to-buffer-other-(frame|window), display-buffer-other-frame:
;;       Do not redefine for Emacs 24+.  Removed autoload cookies.
;; 2013/07/13 dadams
;;     insert-directory: Add mouse-face to file count text starting at bol, not +2.
;;     update-dired-files-count: Do nothing if the files-count line should be hidden.
;; 2012/05/01 dadams
;;     minibuffer-with-setup-hook:
;;       Define only for Emacs 22 and 23.1.
;;       Updated wrt Emacs 23: (funcall ,fun), not (,fun).
;;     find-file-read-args: Redefine only for Emacs 22 and 23.1.
;; 2011/12/19 dadams
;;     insert-directory: Use line-end-position, not end-of-line + point.
;; 2011/09/21 dadams
;;     update-dired-files-count: Ignore any error when adding mouse-face.
;; 2011/04/25 dadams
;;     Removed: describe-file, dired(-mouse)-describe-file.
;;     dired-list-directory: raise error if describe-file not defined.
;; 2011/01/04 dadams
;;     Added autoload cookies for defmacro and commands.
;; 2010/09/29 dadams
;;     insert-directory: Updated per Emacs 24: Add -d switch if not full-directory-p.
;; 2009/10/23 dadams
;;     count-dired-files: Return 0 if search finds no file.
;;     update-dired-files-count: DTRT if count-dired-files returns 0.
;; 2008/12/17 dadams
;;     Added defvar of directory-listing-before-filename-regexp, for Emacs 22.
;; 2008/03/04 dadams
;;     insert-directory, update-dired-files-count: Use two separate tooltips.
;;     update-dired-files-count: Widen temporarily.
;; 2008/03/02 dadams
;;     insert-directory:
;;       Add total files in dir: shown/total.  Added tooltip, mouse-face.
;;       Bind RET, mouse-2 locally to dired(-mouse)-describe-listed-directory.
;;     update-dired-files-count: Add text properties here too.
;;     Added: dired(-mouse)-describe-listed-directory,
;;            dired(-mouse)-describe-file, describe-file.
;; 2008/02/29 dadams
;;     Added: count-dired-files, update-dired-files-count, insert-directory.
;;     Use update-dired-files-count on dired-after-readin-hook.
;; 2007/07/10 dadams
;;     find-file-read-args:
;;       Fix for cursor not being on a file line in Dired. Thx to Juri Linkov.
;; 2006/11/25 dadams
;;     switch-to-buffer-other-*: Update for Emacs 22.
;;     switch-to-buffer-other-frame: Return the buffer switched to.  Raise frame.
;;     switch-to-buffer-other-window: Raise frame of selected window.
;; 2006/10/15 dadams
;;     Copied macro minibuffer-with-setup-hook here from files.el.
;; 2006/08/20 dadams
;;     Added redefinition of find-file-read-args (Emacs 22 or later).
;;     No longer use defsubst instead of defun.
;; 2005/05/28 dadams
;;     switch-to-buffer-other-*: Provide second arg to read-buffer.
;; 2004/09/21 dadams
;;     Updated signatures of switch-to-buffer-other-*
;;     switch-to-buffer-other-frame: Removed call to raise-frame.
;; 1999/03/17 dadams
;;     switch-to-buffer-other-frame: Removed call to raise-frame.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/01/12 dadams
;;     switch-to-buffer-other-frame: No longer raise-frame,
;;       (see my pop-to-buffer).
;; 1995/10/24 dadams
;;     Added corrected (?) version of set-auto-mode, but commented it out
;;     since original (bugged?) version is depended on in other places.
;; 1995/08/11  12:40:30  dadams
;;     interactive "B..." -> use read-buffer instead.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

 ;; Cannot do (require 'files), because `files.el' does no `provide'.
 ;; Don't want to do a (load-library "files") either, because it wouldn't
 ;; allow doing (eval-after-load "files" '(progn (require 'files+)))

(require 'strings nil t) ;; (no error if not found) read-buffer
(require 'misc-fns nil t) ;; (no error if not found) another-buffer

;; Quiet byte compiler.
(defvar insert-directory-ls-version)

;;;;;;;;;;;;;;;;;;;;;

;; Used below, in `count-dired-files'.
(defvar directory-listing-before-filename-regexp
  (let* ((l              "\\([A-Za-z]\\|[^\0-\177]\\)")
         (l-or-quote     "\\([A-Za-z']\\|[^\0-\177]\\)")
         ;; In some locales, month abbreviations are as short as 2 letters,
         ;; and they can be followed by ".".
         ;; In Breton, a month name  can include a quote character.
         (month          (concat l-or-quote l-or-quote "+\\.?"))
         (s              " ")
         (yyyy           "[0-9][0-9][0-9][0-9]")
         (dd             "[ 0-3][0-9]")
         (HH:MM          "[ 0-2][0-9][:.][0-5][0-9]")
         (seconds        "[0-6][0-9]\\([.,][0-9]+\\)?")
         (zone           "[-+][0-2][0-9][0-5][0-9]")
         (iso-mm-dd      "[01][0-9]-[0-3][0-9]")
         (iso-time       (concat HH:MM "\\(:" seconds "\\( ?" zone "\\)?\\)?"))
         (iso            (concat "\\(\\(" yyyy "-\\)?" iso-mm-dd "[ T]" iso-time
                                 "\\|" yyyy "-" iso-mm-dd "\\)"))
         (western        (concat "\\(" month s "+" dd "\\|" dd "\\.?" s month "\\)"
                                 s "+" "\\(" HH:MM "\\|" yyyy "\\)"))
         (western-comma  (concat month s "+" dd "," s "+" yyyy))
         ;; Japanese MS-Windows ls-lisp has one-digit months, and
         ;; omits the Kanji characters after month and day-of-month.
         ;; On Mac OS X 10.3, the date format in East Asian locales is
         ;; day-of-month digits followed by month digits.
         (mm             "[ 0-1]?[0-9]")
         (east-asian     (concat "\\(" mm l "?" s dd l "?" s "+"
                                 "\\|" dd s mm s "+" "\\)"
                                 "\\(" HH:MM "\\|" yyyy l "?" "\\)")))
    ;; The "[0-9]" below requires the previous column to end in a digit.
    ;; This avoids recognizing `1 may 1997' as a date in the line:
    ;; -r--r--r--   1 may      1997        1168 Oct 19 16:49 README

    ;; The "[BkKMGTPEZY]?" below supports "ls -alh" output.
    ;; The ".*" below finds the last match if there are multiple matches.
    ;; This avoids recognizing `jservice  10  1024' as a date in the line:
    ;; drwxr-xr-x  3 jservice  10  1024 Jul  2  1997 esg-host

    ;; vc dired listings provide the state or blanks between file
    ;; permissions and date.  The state is always surrounded by
    ;; parantheses:
    ;; -rw-r--r-- (modified) 2005-10-22 21:25 files.el
    ;; This is not supported yet.
    (concat ".*[0-9][BkKMGTPEZY]?" s
            "\\(" western "\\|" western-comma "\\|" east-asian "\\|" iso "\\)"
            s "+"))
  "Regular expression to match up to the file name in a directory listing.
The default value is designed to recognize dates and times
regardless of the language.")


;; Copied here from `files.el', for use by `find-file-read-args'.
(when (and (> emacs-major-version 21)
           (or (< emacs-major-version 23)
               (and (= emacs-major-version 23)  (= emacs-minor-version 1))))
  (defmacro minibuffer-with-setup-hook (fun &rest body)
    "Temporarily add FUN to `minibuffer-setup-hook' while executing BODY.
BODY should use the minibuffer at most once.
Recursive uses of the minibuffer are unaffected (FUN is not
called additional times).

This macro actually adds an auxiliary function that calls FUN,
rather than FUN itself, to `minibuffer-setup-hook'."
    (declare (indent 1) (debug t))
    (let ((hook  (make-symbol "setup-hook")))
      `(let (,hook)
        (setq ,hook  (lambda ()
                       ;; Clear out this hook so it does not interfere
                       ;; with any recursive minibuffer usage.
                       (remove-hook 'minibuffer-setup-hook ,hook)
                       (funcall ,fun)))
        (unwind-protect
             (progn (add-hook 'minibuffer-setup-hook ,hook) ,@body)
          (remove-hook 'minibuffer-setup-hook ,hook))))))


;; REPLACES ORIGINAL in `files.el':
;; In Dired, use file under cursor as default.
;; Note: This function is not used before Emacs 22.
;;
(when (and (> emacs-major-version 21)
           (or (< emacs-major-version 23)
               (and (= emacs-major-version 23)  (= emacs-minor-version 1))))
  (defun find-file-read-args (prompt mustmatch)
    (list (let ((find-file-default
                 (if (eq major-mode 'dired-mode)
                     (let ((this-file  (condition-case nil
                                           (dired-get-file-for-visit)
                                         (error nil))))
                       (if this-file
                           (abbreviate-file-name this-file)
                         (and buffer-file-name  (abbreviate-file-name
                                                 buffer-file-name))))
                   (and buffer-file-name  (abbreviate-file-name buffer-file-name)))))
            (minibuffer-with-setup-hook
             (lambda () (setq minibuffer-default  find-file-default))
             (read-file-name prompt nil default-directory mustmatch)))
          t)))


;; REPLACES ORIGINAL in `files.el' (Emacs < 24):
;; Use `read-buffer' (not "B...") in the interactive spec.
;; Raise frame of selected window. This has an effect for non-nil `pop-up-frames'.
;;
(when (< emacs-major-version 24)
  (defun switch-to-buffer-other-window (buffer &optional norecord)
    "Select buffer BUFFER in another window.
If BUFFER does not identify an existing buffer, then this function
creates a buffer with that name.

When called from Lisp, BUFFER can be a buffer, a string \(a buffer name),
or nil.  If BUFFER is nil, then this function chooses another buffer
using `other-buffer' (or `another-buffer', if it is defined).

Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones.

Returns the buffer switched to.

This uses function `display-buffer' as a subroutine; see its
documentation for additional customization information."
    (interactive
     (list (read-buffer "Switch to buffer in other window: "
                        (if (fboundp 'another-buffer) ; Defined in `misc-fns.el'.
                            (another-buffer nil t)
                          (other-buffer (current-buffer))))
           nil))
    (let ((pop-up-windows           t)
          ;; Don't let these interfere.
          (same-window-buffer-names ())
          (same-window-regexps      ()))
      (prog1 (pop-to-buffer buffer t norecord)
        (raise-frame (window-frame (selected-window)))))))


;; REPLACES ORIGINAL in `files.el' (Emacs < 24):
;; Use `read-buffer' (not "B...") in the interactive spec.
;; Return the buffer switched to.
;;
(when (< emacs-major-version 24)
  (defun switch-to-buffer-other-frame (buffer &optional norecord)
    "Switch to buffer BUFFER in another frame.
The same frame will be used only if there is no other choice.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones.

Returns the buffer switched to.

This uses function `display-buffer' as a subroutine; see its
documentation for additional customization information."
    (interactive (list (read-buffer "Switch to buffer in other frame: "
                                    (if (fboundp 'another-buffer)
                                        (another-buffer nil t)
                                      (other-buffer (current-buffer))))))
    (let ((pop-up-frames t)
          ;; Don't let these interfere.
          (same-window-buffer-names ())
          (same-window-regexps      ()))
      (prog1 (pop-to-buffer buffer t norecord)
        (raise-frame (window-frame (selected-window)))))))


;; REPLACES ORIGINAL in `files.el' (Emacs 20-23):
;; Use `read-buffer' (not "B...") in the interactive spec.
;; Rewrote, using `switch-to-buffer-other-frame' and `select-frame-set-input-focus'.
;; Return the window displaying the buffer.
;;
(when (< emacs-major-version 24)
  (defun display-buffer-other-frame (buffer)
    "Display BUFFER in another frame.
See documentation of `display-buffer' for more information."
    (interactive (list (read-buffer "Display buffer in other frame: "
                                    (if (fboundp 'another-buffer)
                                        (another-buffer nil t)
                                      (other-buffer (current-buffer))))))
    (let ((win                       (selected-window))
          (same-window-buffer-names  ())
          (same-window-regexps       ()))
      (switch-to-buffer-other-frame buffer)
      (select-window win)
      (select-frame-set-input-focus (window-frame win))
      win)))


;; REPLACES ORIGINAL in `files.el':
;;
;; Add number of files in directory to `total' header entry.
;;
(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings
representing individual options.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This works by running a directory listing program
whose name is in the variable `insert-directory-program'.
If WILDCARD, it also runs the shell specified by `shell-file-name'.

When SWITCHES contains the long `--dired' option, this function
treats it specially, for the sake of dired.  However, the
normally equivalent short `-D' option is just passed on to
`insert-directory-program', as any other option."
;;; insert-directory
;;; - must insert _exactly_one_line_ describing FILE if WILDCARD and
;;;   FULL-DIRECTORY-P is nil.
;;;   The single line of output must display FILE's name as it was
;;;   given, namely, an absolute path name.
;;; - must insert exactly one line for each file if WILDCARD or
;;;   FULL-DIRECTORY-P is t, plus one optional "total" line
;;;   before the file lines, plus optional text after the file lines.
;;;   Lines are delimited by "\n", so filenames containing "\n" are not
;;;   allowed.
;;;   File lines should display the basename.
;;; - must be consistent with
;;;   - functions dired-move-to-filename, (these two define what a file line is)
;;;              dired-move-to-end-of-filename,
;;;              dired-between-files, (shortcut for (not (dired-move-to-filename)))
;;;              dired-insert-headerline
;;;              dired-after-subdir-garbage (defines what a "total" line is)
;;;   - variable dired-subdir-regexp
;;; - may be passed "--dired" as the first argument in SWITCHES.
;;;   Filename handlers might have to remove this switch if their
;;;   "ls" command does not support it.

  ;; We need the directory in order to find the right handler.
  (let ((handler  (find-file-name-handler (expand-file-name file)
                                          'insert-directory)))
    (if handler
        (funcall handler 'insert-directory file switches
                 wildcard full-directory-p)
      (if (eq system-type 'vax-vms)
          (vms-read-directory file switches (current-buffer))
        (let (result  (beg (point)))

          ;; Read the actual directory using `insert-directory-program'.
          ;; RESULT gets the status code.
          (let* (;; We at first read by no-conversion, then after
                 ;; putting text property `dired-filename, decode one
                 ;; bunch by one to preserve that property.
                 (coding-system-for-read   'no-conversion)
                 ;; This is to control encoding the arguments in call-process.
                 (coding-system-for-write  (and enable-multibyte-characters
                                                (or file-name-coding-system
                                                    default-file-name-coding-system))))
            (setq result
                  (if wildcard
                      ;; Run `ls' in the directory part of the file pattern
                      ;; using the last component as argument.
                      (let ((default-directory  (if (file-name-absolute-p file)
                                                    (file-name-directory file)
                                                  (file-name-directory
                                                   (expand-file-name file))))
                            (pattern            (file-name-nondirectory file)))
                        (call-process
                         shell-file-name nil t nil "-c"
                         (concat (if (memq system-type '(ms-dos windows-nt))
                                     ""
                                   "\\") ; Disregard Unix shell aliases!
                                 insert-directory-program " -d "
                                 (if (stringp switches)
                                     switches
                                   (mapconcat 'identity switches " "))
                                 " -- "
                                 ;; Quote some characters that have
                                 ;; special meanings in shells; but
                                 ;; don't quote the wildcards--we want
                                 ;; them to be special.  We also
                                 ;; currently don't quote the quoting
                                 ;; characters in case people want to
                                 ;; use them explicitly to quote
                                 ;; wildcard characters.
                                 (shell-quote-wildcard-pattern pattern))))
                    ;; SunOS 4.1.3, SVr4 and others need the "." to list the
                    ;; directory if FILE is a symbolic link.
                    (unless full-directory-p
                      (setq switches
                            (if (stringp switches)
                                (concat switches " -d")
                              ;; Vanilla Emacs uses this here, but no good for older
                              ;; Emacs since uses 3rd arg:
                              ;; (add-to-list 'switches "-d" 'append))))
                              (setq switches  (append switches (list "-d"))))))
                    (apply 'call-process
                           insert-directory-program nil t nil
                           (append (if (listp switches)
                                       switches
                                     (unless (equal switches "")
                                       ;; Split the switches at any spaces so we can
                                       ;; pass separate options as separate args.
                                       (split-string switches)))
                                   ;; Avoid lossage if FILE starts with `-'.
                                   '("--")
                                   (progn
                                     (when (string-match "\\`~" file)
                                       (setq file  (expand-file-name file)))
                                     (list (if full-directory-p
                                               (concat (file-name-as-directory file)
                                                       ".")
                                             file))))))))

          ;; If we got "//DIRED//" in the output, it means we got a real
          ;; directory listing, even if `ls' returned nonzero.
          ;; So ignore any errors.
          (when (if (stringp switches)
                    (string-match "--dired\\>" switches)
                  (member "--dired" switches))
            (save-excursion (forward-line -2)
                            (when (looking-at "//SUBDIRED//") (forward-line -1))
                            (when (looking-at "//DIRED//") (setq result  0))))

          ;; DADAMS: Added boundp test.
          (when (and (not (eq 0 result))
                     (boundp 'insert-directory-ls-version)
                     (eq insert-directory-ls-version 'unknown))
            ;; The first time ls returns an error,
            ;; find the version numbers of ls,
            ;; and set insert-directory-ls-version
            ;; to > if it is more than 5.2.1, < if it is less, nil if it
            ;; is equal or if the info cannot be obtained.
            ;; (That can mean it isn't GNU ls.)
            (let ((version-out  (with-temp-buffer
                                  (call-process "ls" nil t nil "--version")
                                  (buffer-string))))
              (if (string-match "ls (.*utils) \\([0-9.]*\\)$" version-out)
                  (let* ((version     (match-string 1 version-out))
                         (split       (split-string version "[.]"))
                         (numbers     (mapcar 'string-to-number split))
                         (min         '(5 2 1))
                         (comparison  nil))
                    (while (and (not comparison)  (or numbers  min))
                      (cond ((null min)                  (setq comparison  '>))
                            ((null numbers)              (setq comparison '<))
                            ((> (car numbers) (car min)) (setq comparison  '>))
                            ((< (car numbers) (car min)) (setq comparison  '<))
                            (t (setq numbers  (cdr numbers)
                                     min      (cdr min)))))
                    (setq insert-directory-ls-version  (or comparison  '=)))
                (setq insert-directory-ls-version  nil))))

          ;; For GNU ls versions 5.2.2 and up, ignore minor errors.
          ;; DADAMS: Added boundp test.
          (when (and (eq 1 result)  (boundp 'insert-directory-ls-version)
                     (eq insert-directory-ls-version '>))
            (setq result  0))

          ;; If `insert-directory-program' failed, signal an error.
          (unless (eq 0 result)
            ;; Delete the error message it may have output.
            (delete-region beg (point))
            ;; On non-Posix systems, we cannot open a directory, so
            ;; don't even try, because that will always result in
            ;; the ubiquitous "Access denied".  Instead, show the
            ;; command line so the user can try to guess what went wrong.
            (if (and (file-directory-p file)  (memq system-type '(ms-dos windows-nt)))
                (error "Reading directory: \"%s %s -- %s\" exited with status %s"
                       insert-directory-program
                       (if (listp switches) (concat switches) switches)
                       file
                       result)
              ;; Unix.  Access the file to get a suitable error.
              (access-file file "Reading directory")
              (error "Listing directory failed but `access-file' worked")))

          (when (if (stringp switches)
                    (string-match "--dired\\>" switches)
                  (member "--dired" switches))
            ;; The following overshoots by one line for an empty
            ;; directory listed with "--dired", but without "-a"
            ;; switch, where the ls output contains a
            ;; "//DIRED-OPTIONS//" line, but no "//DIRED//" line.
            ;; We take care of that case later.
            (forward-line -2)
            (when (looking-at "//SUBDIRED//")
              (delete-region (point) (progn (forward-line 1) (point)))
              (forward-line -1))
            (if (looking-at "//DIRED//")
                (let ((end          (line-end-position))
                      (linebeg      (point))
                      (error-lines  ()))
                  ;; Find all the lines that are error messages,
                  ;; and record the bounds of each one.
                  (goto-char beg)
                  (while (< (point) linebeg)
                    (or (eql (following-char) ?  ) ; DADAMS: replaced ?\s with ? .
                        (push (list (point) (line-end-position)) error-lines))
                    (forward-line 1))
                  (setq error-lines  (nreverse error-lines))
                  ;; Now read the numeric positions of file names.
                  (goto-char linebeg)
                  (forward-word 1)
                  (forward-char 3)
                  (while (< (point) end)
                    (let ((start  (insert-directory-adj-pos
                                   (+ beg (read (current-buffer)))
                                   error-lines))
                          (end    (insert-directory-adj-pos
                                   (+ beg (read (current-buffer)))
                                   error-lines)))
                      ;; DADAMS: Replaced ?\s by ? .
                      (if (memq (char-after end) '(?\n ? ))
                          ;; End is followed by \n or by " -> ".
                          (put-text-property start end 'dired-filename t)
                        ;; It seems that we can't trust ls's output as to
                        ;; byte positions of filenames.
                        (put-text-property beg (point) 'dired-filename nil)
                        (end-of-line))))
                  (goto-char end)
                  (beginning-of-line)
                  (delete-region (point) (progn (forward-line 1) (point))))
              ;; Take care of the case where the ls output contains a
              ;; "//DIRED-OPTIONS//"-line, but no "//DIRED//"-line
              ;; and we went one line too far back (see above).
              (forward-line 1))
            (when (looking-at "//DIRED-OPTIONS//")
              (delete-region (point) (progn (forward-line 1) (point)))))

          ;; Now decode what read if necessary.
          (let ((coding  (or coding-system-for-read
                             file-name-coding-system
                             default-file-name-coding-system
                             'undecided))
                coding-no-eol val pos)
            (when (and enable-multibyte-characters
                       (not (memq (coding-system-base coding)
                                  '(raw-text no-conversion))))
              ;; If no coding system is specified or detection is
              ;; requested, detect the coding.
              (when (eq (coding-system-base coding) 'undecided)
                (setq coding  (detect-coding-region beg (point) t)))
              (unless (eq (coding-system-base coding) 'undecided)
                (save-restriction
                  (setq coding-no-eol  (coding-system-change-eol-conversion
                                        coding 'unix))
                  (narrow-to-region beg (point))
                  (goto-char (point-min))
                  (while (not (eobp))
                    (setq pos  (point)
                          val  (get-text-property (point) 'dired-filename))
                    (goto-char (next-single-property-change
                                (point) 'dired-filename nil (point-max)))
                    ;; Force no eol conversion on a file name, so CR is preserved.
                    (decode-coding-region pos (point) (if val coding-no-eol coding))
                    (if val (put-text-property pos (point) 'dired-filename t)))))))
          (when full-directory-p
            (save-excursion
              (goto-char beg)
              (while (re-search-forward "^ *\\(total\\)" nil t)
                (beginning-of-line)
                (insert "files " (number-to-string (save-match-data
                                                     (count-dired-files)))
                        "/" (number-to-string
                             (- (length (directory-files default-directory
                                                         nil nil t)) 2))
                        " ")
                (goto-char beg)
                (re-search-forward "^files [0-9]+/[0-9]+ \\(total\\)" nil t)
                (replace-match "space used" nil nil nil 1)
                (let ((available  (and (fboundp 'get-free-disk-space)
                                       (get-free-disk-space ".")))
                      (map        (make-sparse-keymap)))
                  (define-key map [mouse-2] 'dired-mouse-describe-listed-directory)
                  (define-key map "\r" 'dired-describe-listed-directory)
                  (when available (end-of-line) (insert " available " available))
                  (add-text-properties (line-beginning-position)
                                       (1- (match-beginning 1))
                                       `(mouse-face highlight keymap ,map
                                         help-echo "Files shown / total files in \
directory \[RET, mouse-2: more info]"))
                  (add-text-properties (match-beginning 1) (line-end-position)
                                       `(mouse-face highlight keymap ,map
                                         help-echo "Kbytes used in directory, Kbytes \
available on disk [RET, mouse-2: more info]")))))))))))

(defun count-dired-files ()
  "Returns the number of files in the current Dired directory listing.
This includes directory entries, as well as files, but it excludes `.'
and `..'."
  ;; $$$$ Should we skip `#' files also, as in `dired-trivial-filenames'?
  (save-excursion
    (re-search-backward "^$" nil 'to-bob)
    (if (not (re-search-forward dired-move-to-filename-regexp nil t))
        0
      (let* ((beg     (line-beginning-position))
             (end     (save-excursion (re-search-forward "^$" nil t)))
             (dots-p  (save-excursion   ; Is `..' present?
                        (goto-char beg)
                        (re-search-forward
                         (concat directory-listing-before-filename-regexp
                                 "\\.\\./?$")
                         end t))))
        (if dots-p (- (count-lines beg end) 2) (count-lines beg end))))))


(add-hook 'dired-after-readin-hook 'update-dired-files-count)
(defun update-dired-files-count ()
  "Update file count in Dired header for each directory listed."
  (save-restriction
    (widen)
    (let* ((num-files      (count-dired-files))
           (str-num-files  (number-to-string num-files)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^  files \\([0-9]+\\)/\\([0-9]+\\)" nil t)
          ;; No-op if the line should be hidden.
          (unless (eq (get-text-property (match-beginning 0) 'invisible)
                      'dired-hide-details-information)
            (let ((buffer-read-only  nil)
                  (map               (make-sparse-keymap)))
              (define-key map [mouse-2] 'dired-mouse-describe-listed-directory)
              (define-key map "\r" 'dired-describe-listed-directory)
              (replace-match str-num-files nil nil nil 1)
              (replace-match (if (zerop num-files)
                                 str-num-files
                               (number-to-string (- (length (directory-files
                                                             default-directory
                                                             nil nil t))
                                                    2)))
                             nil nil nil 2)
              ;; Ignore any error, e.g. from `dired-details.el' hiding text.
              (condition-case nil
                  (add-text-properties
                   (save-excursion (beginning-of-line) (+ 2 (point))) (match-end 2)
                   `(mouse-face highlight keymap ,map
                                help-echo "Files shown / total files in directory \
\[RET, mouse-2: more info]"))
                (error nil)))))
        (set-buffer-modified-p nil)))))


;;;###autoload
(defun dired-describe-listed-directory ()
  "In Dired, describe the current listed directory."
  (interactive)
  (unless (fboundp 'describe-file)
    (error "This command needs `describe-file' from library `help-fns+.el'"))
  (let ((dirname  (save-excursion (forward-line -1)
                                  (skip-syntax-forward " ")
                                  (buffer-substring (point)
                                                    (save-excursion
                                                      (end-of-line)
                                                      (1- (point))))))) ; Up to colon.
    (describe-file dirname)))

;;;###autoload
(defun dired-mouse-describe-listed-directory (event)
  "Describe the current listed directory."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event)))
    (dired-describe-listed-directory)))






;;; `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
;; 1995 -
;; D. Adams: It seems to me that the original `set-auto-mode' is bugged.
;; It doesn't seem to correspond to the doc string of
;; `auto-mode-alist': One should be able to use (REGEXP FN), but it
;; appears that (REGEXP FN FN) is needed instead.  I would correct it
;; as follows, but there appear to be other programs (e.g.
;; `jka-compr.el') that now require the form (REGEXP FN FN), or
;; (REGEXP nil FN).

;; Anyway, here is my correction (commented out for now):
;; The original has (nth 2 alist), below, instead of (nth 1 alist).

;;(defun set-auto-mode ()
;;  "Select major mode appropriate for current buffer.
;;This checks for a -*- mode tag in the buffer's text, compares the filename
;;against the entries in `auto-mode-alist', or checks the interpreter that
;;runs this file against `interpreter-mode-alist'.

;;It does not check for the `mode:' local variable in the
;;Local Variables section of the file; for that, use `hack-local-variables'.

;;If `enable-local-variables' is nil, this function does not check for a
;;-*- mode tag."
;;  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
;;  (let (beg end done)
;;    (save-excursion
;;      (goto-char (point-min))
;;      (skip-chars-forward " \t\n")
;;      (and enable-local-variables
;;         ;; Don't look for -*- if this file name matches any
;;         ;; of the regexps in inhibit-first-line-modes-regexps.
;;         (let ((temp inhibit-first-line-modes-regexps))
;;           (while (and temp
;;                       (not (string-match (car temp)
;;                                          buffer-file-name)))
;;             (setq temp (cdr temp)))
;;           (not temp))
;;         (search-forward "-*-" (save-excursion
;;                                 ;; If the file begins with "#!"
;;                                 ;; (exec interpreter magic), look
;;                                 ;; for mode frobs in the first two
;;                                 ;; lines.  You cannot necessarily
;;                                 ;; put them in the first line of
;;                                 ;; such a file without screwing up
;;                                 ;; the interpreter invocation.
;;                                 (end-of-line (and (looking-at "^#!") 2))
;;                                 (point)) t)
;;         (progn
;;           (skip-chars-forward " \t")
;;           (setq beg (point))
;;           (search-forward "-*-" (line-end-position) t))
;;         (progn
;;           (forward-char -3)
;;           (skip-chars-backward " \t")
;;           (setq end (point))
;;           (goto-char beg)
;;           (if (save-excursion (search-forward ":" end t))
;;               ;; Find all specifications for the `mode:' variable
;;               ;; and execute them left to right.
;;               (while (let ((case-fold-search t))
;;                        (search-forward "mode:" end t))
;;                 (skip-chars-forward " \t")
;;                 (setq beg (point))
;;                 (if (search-forward ";" end t)
;;                     (forward-char -1)
;;                   (goto-char end))
;;                 (skip-chars-backward " \t")
;;                 (funcall (intern (concat (downcase
;;                                           (buffer-substring beg (point)))
;;                                          "-mode"))))
;;             ;; Simple -*-MODE-*- case.
;;             (funcall (intern (concat (downcase (buffer-substring beg end))
;;                                      "-mode"))))
;;           (setq done t)))
;;      ;; If we didn't find a mode from a -*- line, try using the file name.
;;      (if (and (not done) buffer-file-name)
;;        (let ((name buffer-file-name)
;;              (keep-going t))
;;          ;; Remove backup-suffixes from file name.
;;          (setq name (file-name-sans-versions name))
;;          (while keep-going
;;            (setq keep-going nil)
;;            (let ((alist auto-mode-alist)
;;                  (mode nil))
;;              ;; Find first matching alist entry.
;;              (let ((case-fold-search (eq system-type 'vax-vms)))
;;                (while (and (not mode) alist)
;;                  (if (string-match (car (car alist)) name)
;;                      (if (and (consp (cdr (car alist)))
;;                               (nth 1 (car alist)))
;;                          (progn
;;                            (setq mode (car (cdr (car alist)))
;;                                  name (substring name 0 (match-beginning 0))
;;                                  keep-going t))
;;                        (setq mode (cdr (car alist))
;;                              keep-going nil)))
;;                  (setq alist (cdr alist))))
;;              (if mode
;;                  (funcall mode)
;;                ;; If we can't deduce a mode from the file name,
;;                ;; look for an interpreter specified in the first line.
;;                (let ((interpreter
;;                       (save-excursion
;;                         (goto-char (point-min))
;;                         (if (looking-at "#! *\\([^ \t\n]+\\)")
;;                             (buffer-substring (match-beginning 1)
;;                                               (match-end 1))
;;                           "")))
;;                      elt)
;;                  ;; Map interpreter name to a mode.
;;                  (setq elt (assoc (file-name-nondirectory interpreter)
;;                                   interpreter-mode-alist))
;;                  (if elt
;;                      (funcall (cdr elt))))))))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;

(provide 'files+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; files+.el ends here

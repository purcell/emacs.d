;;; diredfl.el --- Extra font lock rules for a more colourful dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Author: Drew Adams
;; Keywords: faces
;; URL: https://github.com/purcell/diredfl
;; Package-Requires: ((emacs "24"))
;; Package-Version: 20171005.39
;; Package-X-Original-Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is adapted from the extra font lock rules provided by Drew
;; Adams' `dired+' package, but published via a modern means, and with
;; support for older Emacsen removed.

;; Enable in all dired buffers by calling or customising `diredfl-global-mode'.

;; Alternatively:

;;     (add-hook 'dired-mode-hook 'diredfl-mode)

;;; Code:

(require 'dired)

(defgroup diredfl ()
  "Extra font lock rules for a more colourful dired."
  :group 'dired)

(defcustom diredfl-compressed-extensions '(".tar" ".taz" ".tgz" ".arj" ".lzh"
                                           ".lzma" ".xz" ".zip" ".z" ".Z" ".gz" ".bz2")
  "*List of compressed-file extensions, for highlighting."
  :type '(repeat string) :group 'diredfl)

(defcustom diredfl-ignore-compressed-flag t
  "*Non-nil means to font-lock names of compressed files as ignored files.
This applies to filenames whose extensions are in
`diredfl-compressed-extensions'.  If nil they are highlighted using
face `diredfl-compressed-file-name'."
  :type 'boolean :group 'diredfl)

(defface diredfl-autofile-name
  '((((background dark)) (:background "#111313F03181")) ; Very dark blue
    (t                   (:background "#EEECEC0FCE7E"))) ; Very pale goldenrod
  "*Face used in Dired for names of files that are autofile bookmarks."
  :group 'diredfl)
(defvar diredfl-autofile-name 'diredfl-autofile-name)

(defface diredfl-compressed-file-name
  '((((background dark)) (:foreground "Blue"))
    (t                   (:foreground "Brown")))
  "*Face used for compressed file names."
  :group 'diredfl)
(defvar diredfl-compressed-file-name 'diredfl-compressed-file-name)

(defface diredfl-compressed-file-suffix
  '((((background dark)) (:foreground "Blue"))
    (t                   (:foreground "Yellow")))
  "*Face used for compressed file suffixes in Dired buffers.
This means the `.' plus the file extension.  Example: `.zip'."
  :group 'diredfl)
(defvar diredfl-compressed-file-suffix 'diredfl-compressed-file-suffix)

(defface diredfl-date-time
  '((((background dark)) (:foreground "#74749A9AF7F7")) ; ~ med blue
    (t                   (:foreground "DarkGoldenrod4")))
  "*Face used for date and time in Dired buffers."
  :group 'diredfl)
(defvar diredfl-date-time 'diredfl-date-time)

(defface diredfl-deletion
  '((t (:foreground "Yellow" :background "Red")))
  "*Face used for deletion flags (D) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-deletion 'diredfl-deletion)

(defface diredfl-deletion-file-name
  '((t (:foreground "Red")))
  "*Face used for names of deleted files in Dired buffers."
  :group 'diredfl)
(defvar diredfl-deletion-file-name 'diredfl-deletion-file-name)

(defface diredfl-dir-heading
  '((((background dark)) (:foreground "Yellow" :background "#00003F3F3434")) ; ~ dark green
    (t                   (:foreground "Blue" :background "Pink")))
  "*Face used for directory headings in Dired buffers."
  :group 'diredfl)
(defvar diredfl-dir-heading 'diredfl-dir-heading)

(defface diredfl-dir-name
  '((((background dark))
     (:foreground "#7474FFFFFFFF" :background "#2C2C2C2C2C2C")) ; ~ cyan, dark gray
    (t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory names."
  :group 'diredfl)
(defvar diredfl-dir-name 'diredfl-dir-name)

(defface diredfl-dir-priv
  '((((background dark))
     (:foreground "#7474FFFFFFFF" :background "#2C2C2C2C2C2C")) ; ~ cyan, dark gray
    (t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory privilege indicator (d) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-dir-priv 'diredfl-dir-priv)

(defface diredfl-exec-priv
  '((((background dark)) (:background "#4F4F3B3B2121")) ; ~ dark brown
    (t                   (:background "LightSteelBlue")))
  "*Face used for execute privilege indicator (x) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-exec-priv 'diredfl-exec-priv)

;; For this to show up, you need `F' among the options in `dired-listing-switches'.
;; For example, I use "-alF" for `dired-listing-switches'.
(defface diredfl-executable-tag
  '((t (:foreground "Red")))
  "*Face used for executable tag (*) on file names in Dired buffers."
  :group 'diredfl)
(defvar diredfl-executable-tag 'diredfl-executable-tag)

(defface diredfl-file-name
  '((((background dark)) (:foreground "Yellow"))
    (t                   (:foreground "Blue")))
  "*Face used for file names (without suffixes) in Dired buffers.
This means the base name.  It does not include the `.'."
  :group 'diredfl)
(defvar diredfl-file-name 'diredfl-file-name)

(defface diredfl-file-suffix
  '((((background dark)) (:foreground "#7474FFFF7474")) ; ~ light green
    (t                   (:foreground "DarkMagenta")))
  "*Face used for file suffixes in Dired buffers.
This means the `.' plus the file extension.  Example: `.elc'."
  :group 'diredfl)
(defvar diredfl-file-suffix 'diredfl-file-suffix)

(defface diredfl-flag-mark
  '((((background dark)) (:foreground "Blue" :background "#7575D4D41D1D")) ; ~ olive green
    (t                   (:foreground "Yellow" :background "Blueviolet")))
  "*Face used for flags and marks (except D) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-flag-mark 'diredfl-flag-mark)

(defface diredfl-flag-mark-line
  '((((background dark)) (:background "#787831311414")) ; ~ dark red brown
    (t                   (:background "Skyblue")))
  "*Face used for flagged and marked lines in Dired buffers."
  :group 'diredfl)
(defvar diredfl-flag-mark-line 'diredfl-flag-mark-line)

(defface diredfl-ignored-file-name
  '(;; (((background dark)) (:foreground "#FFFF921F921F")) ; ~ salmon
    ;; (((background dark)) (:foreground "#A71F5F645F64")) ; ~ dark salmon
    (((background dark)) (:foreground "#C29D6F156F15")) ; ~ salmon
    (t                   (:foreground "#00006DE06DE0")))                  ; ~ dark cyan
  "*Face used for ignored file names  in Dired buffers."
  :group 'diredfl)
(defvar diredfl-ignored-file-name 'diredfl-ignored-file-name)

(defface diredfl-link-priv
  '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
    (t                   (:foreground "DarkOrange")))
  "*Face used for link privilege indicator (l) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-link-priv 'diredfl-link-priv)

(defface diredfl-no-priv
  '((((background dark)) (:background "#2C2C2C2C2C2C")) ; ~ dark gray
    (t                   (:background "LightGray")))
  "*Face used for no privilege indicator (-) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-no-priv 'diredfl-no-priv)

(defface diredfl-number
  '((((background dark)) (:foreground "#FFFFFFFF7474")) ; ~ light yellow
    (t                   (:foreground "DarkBlue")))
  "*Face used for numerical fields in Dired buffers.
In particular, inode number, number of hard links, and file size."
  :group 'diredfl)
(defvar diredfl-number 'diredfl-number)

(defface diredfl-other-priv
  '((((background dark)) (:background "#111117175555")) ; ~ dark blue
    (t                   (:background "PaleGoldenrod")))
  "*Face used for l,s,S,t,T privilege indicators in Dired buffers."
  :group 'diredfl)
(defvar diredfl-other-priv 'diredfl-other-priv)

(defface diredfl-rare-priv
  '((((background dark)) (:foreground "Green" :background "#FFFF00008080")) ; ~ hot pink
    (t                   (:foreground "Magenta" :background "SpringGreen")))
  "*Face used for rare privilege indicators (b,c,s,m,p,S) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-rare-priv 'diredfl-rare-priv)

(defface diredfl-read-priv
  '((((background dark)) (:background "#999932325555")) ; ~ burgundy / dark magenta
    (t                   (:background "MediumAquamarine")))
  "*Face used for read privilege indicator (w) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-read-priv 'diredfl-read-priv)

(defface diredfl-symlink
  '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
    (t                   (:foreground "DarkOrange")))
  "*Face used for symbolic links in Dired buffers."
  :group 'diredfl)
(defvar diredfl-symlink 'diredfl-symlink)

(defface diredfl-tagged-autofile-name
  '((((background dark)) (:background "#328C0411328C")) ; Very dark magenta
    (t                   (:background "#CD73FBEECD73"))) ; Very pale green
  "*Face used in Dired for names of files that are autofile bookmarks."
  :group 'diredfl)
(defvar diredfl-tagged-autofile-name 'diredfl-tagged-autofile-name)

(defface diredfl-write-priv
  '((((background dark)) (:background "#25258F8F2929")) ; ~ dark green
    (t                   (:background "Orchid")))
  "*Face used for write privilege indicator (w) in Dired buffers."
  :group 'diredfl)
(defvar diredfl-write-priv 'diredfl-write-priv)

;;; Define second level of fontifying.
(defvar diredfl-font-lock-keywords-1
  (list
   '("^  \\(.+:\\)$" 1 diredfl-dir-heading) ; Directory headers
   '("^  wildcard.*$" 0 'default)       ; Override others, e.g. `l' for `diredfl-other-priv'.
   '("^  (No match).*$" 0 'default)     ; Override others, e.g. `t' for `diredfl-other-priv'.
   '("[^ .]\\(\\.[^. /]+\\)$" 1 diredfl-file-suffix) ; Suffix, including `.'.
   '("\\([^ ]+\\) -> .+$" 1 diredfl-symlink) ; Symbolic links

   ;; 1) Date/time and 2) filename w/o suffix.
   ;;    This is a bear, and it is fragile - Emacs can change `dired-move-to-filename-regexp'.
   `(,dired-move-to-filename-regexp
     (7 diredfl-date-time t t)         ; Date/time, locale (western or eastern)
     (2 diredfl-date-time t t)         ; Date/time, ISO
     (,(concat "\\(.+\\)\\(" (concat (funcall #'regexp-opt diredfl-compressed-extensions)
                                     "\\)[*]?$"))
      nil nil (0 diredfl-compressed-file-name keep t))) ; Compressed-file suffix
   `(,dired-move-to-filename-regexp
     (7 diredfl-date-time t t)         ; Date/time, locale (western or eastern)
     (2 diredfl-date-time t t)         ; Date/time, ISO
     ("\\(.+\\)$" nil nil (0 diredfl-file-name keep t))) ; Filename (not a compressed file)

   ;; Files to ignore
   (list (concat "^  \\(.*\\("
                 (mapconcat #'regexp-quote (or (and (boundp 'dired-omit-extensions)  dired-omit-extensions)
                                               completion-ignored-extensions)
                            "[*]?\\|")
                 (and diredfl-ignore-compressed-flag
                      (concat "\\|" (mapconcat #'regexp-quote diredfl-compressed-extensions "[*]?\\|")))
                 "[*]?\\)\\)$") ; Allow for executable flag (*).
         1 diredfl-ignored-file-name t)

   ;; Compressed-file (suffix)
   (list (concat "\\(" (concat (funcall #'regexp-opt diredfl-compressed-extensions) "\\)[*]?$"))
         1 diredfl-compressed-file-suffix t)
   '("\\([*]\\)$" 1 diredfl-executable-tag t) ; Executable (*)

   ;; Inode, hard-links, & file size (. and , are for the decimal point, depending on locale)
   ;; See comment for `directory-listing-before-filename-regexp' in `files.el' or `files+.el'.
   '("\\(\\([0-9]+\\([.,][0-9]+\\)?\\)[BkKMGTPEZY]?[ /]?\\)" 1 'diredfl-number)

   ;; Directory names - exclude d:/..., Windows drive letter in a dir heading.
   (list (concat dired-re-maybe-mark dired-re-inode-size "\\(d\\)[^:]")
         '(1 diredfl-dir-priv t) '(".+" (dired-move-to-filename) nil (0 diredfl-dir-name t)))

   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\(x\\)") ; o x
         '(1 diredfl-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\([lsStT]\\)") ; o misc
         '(1 diredfl-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].......\\(w\\).") ; o w
         '(1 diredfl-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]......\\(r\\)..") ; o r
         '(1 diredfl-read-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\(x\\)...") ; g x
         '(1 diredfl-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\([lsStT]\\)...") ; g misc
         '(1 diredfl-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]....\\(w\\)....") ; g w
         '(1 diredfl-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]...\\(r\\).....") ; g r
         '(1 diredfl-read-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\(x\\)...") ; u x
         '(1 diredfl-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\([lsStT]\\)...") ; u misc
         '(1 diredfl-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].\\(w\\)....") ; u w
         '(1 diredfl-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]\\(r\\).....") ; u r
         '(1 diredfl-read-priv))

   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\([-rwxlsStT]\\)") ; o -
         '(1 diredfl-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].......\\([-rwxlsStT]\\).") ; g -
         '(1 diredfl-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]......\\([-rwxlsStT]\\)..") ; u -
         '(1 diredfl-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\([-rwxlsStT]\\)...") ; o -
         '(1 diredfl-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]....\\([-rwxlsStT]\\)....") ; g -
         '(1 diredfl-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]...\\([-rwxlsStT]\\).....") ; u -
         '(1 diredfl-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\([-rwxlsStT]\\)......") ; o -
         '(1 diredfl-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].\\([-rwxlsStT]\\).......") ; g -
         '(1 diredfl-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]\\([-rwxlsStT]\\)........") ; u -
         '(1 diredfl-no-priv keep))

   (list (concat dired-re-maybe-mark dired-re-inode-size "\\([bcsmpS]\\)") ; (rare)
         '(1 diredfl-rare-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "\\(l\\)[-rwxlsStT]") ; l
         '(1 diredfl-rare-priv keep))

   (list (concat "^\\([^\n " (char-to-string dired-del-marker) "].*$\\)")
         1 diredfl-flag-mark-line t)     ; Flag/mark lines
   (list (concat "^\\([^\n " (char-to-string dired-del-marker) "]\\)") ; Flags, marks (except D)
         1 diredfl-flag-mark t)

   (list (concat "^\\([" (char-to-string dired-del-marker) "].*$\\)") ; Deletion-flagged lines
         1 diredfl-deletion-file-name t)
   (list (concat "^\\([" (char-to-string dired-del-marker) "]\\)") ; Deletion flags (D)
         1 diredfl-deletion t)

   ) "2nd level of Dired highlighting.  See `font-lock-maximum-decoration'.")


;;;###autoload
(define-minor-mode diredfl-mode
  "Enable additional font locking in `dired-mode'."
  nil
  :lighter ""
  (setq font-lock-defaults
        (if diredfl-mode
            '((dired-font-lock-keywords
               dired-font-lock-keywords
               diredfl-font-lock-keywords-1)
              t nil nil beginning-of-line)
          '(dired-font-lock-keywords t nil nil beginning-of-line)))
  (font-lock-refresh-defaults))

;;;###autoload
(define-globalized-minor-mode diredfl-global-mode diredfl-mode
  (lambda ()
    (when (derived-mode-p 'dired-mode)
      (diredfl-mode)))
  :require 'diredfl)


(provide 'diredfl)
;;; diredfl.el ends here

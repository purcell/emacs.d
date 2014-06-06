;;; helm-utils.el --- Utilities Functions for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'compile) ; Fixme: Is this needed?
(require 'dired)

(declare-function helm-find-files-1 "helm-files.el" (fname &optional preselect))


(defgroup helm-utils nil
  "Utilities routines for Helm."
  :group 'helm)

(defcustom helm-su-or-sudo "sudo"
  "What command to use for root access."
  :type 'string
  :group 'helm-utils)

(defcustom helm-yank-symbol-first nil
  "`helm-yank-text-at-point' yanks symbol at point on first
invocation if this is non-nil."
  :type  'boolean
  :group 'helm-utils)

(defcustom helm-default-kbsize 1024.0
  "Default Kbsize to use for showing files size.
It is a float, usually 1024.0 but could be 1000.0 on some systems."
  :group 'helm-utils
  :type 'float)

(defvar helm-goto-line-before-hook nil
  "Run before jumping to line.
This hook run when jumping from `helm-goto-line', `helm-etags-default-action',
and `helm-imenu-default-action'.")

(defvar helm-save-pos-before-jump-register ?_
  "The register where `helm-save-pos-to-register-before-jump' save position.")

(defface helm-selection-line
    '((t (:background "IndianRed4" :underline t)))
  "Face used in the `helm-current-buffer' when jumping to candidate."
  :group 'helm-utils)


;;; compatibility
;;
;;
(unless (fboundp 'window-system)
  (defun window-system (&optional _arg)
    window-system))

(unless (fboundp 'make-composed-keymap)
  (defun make-composed-keymap (maps &optional parent)
    "Construct a new keymap composed of MAPS and inheriting from PARENT.
When looking up a key in the returned map, the key is looked in each
keymap of MAPS in turn until a binding is found.
If no binding is found in MAPS, the lookup continues in PARENT, if non-nil.
As always with keymap inheritance, a nil binding in MAPS overrides
any corresponding binding in PARENT, but it does not override corresponding
bindings in other keymaps of MAPS.
MAPS can be a list of keymaps or a single keymap.
PARENT if non-nil should be a keymap."
    `(keymap
      ,@(if (keymapp maps) (list maps) maps)
      ,@parent)))

(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
    (let (found (tail alist) value)
      (while (and tail (not found))
        (let ((elt (car tail)))
          (when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
            (setq found t value (if (consp elt) (cdr elt) default))))
        (setq tail (cdr tail)))
      value)))

;; Function not available in XEmacs,
(unless (fboundp 'minibuffer-contents)
  (defun minibuffer-contents ()
    "Return the user input in a minbuffer as a string.
The current buffer must be a minibuffer."
    (field-string (point-max)))

  (defun delete-minibuffer-contents  ()
    "Delete all user input in a minibuffer.
The current buffer must be a minibuffer."
    (delete-field (point-max))))

;; Function not available in older Emacs (<= 22.1).
(unless (fboundp 'buffer-chars-modified-tick)
  (defun buffer-chars-modified-tick (&optional buffer)
    "Return BUFFER's character-change tick counter.
Each buffer has a character-change tick counter, which is set to the
value of the buffer's tick counter (see `buffer-modified-tick'), each
time text in that buffer is inserted or deleted.  By comparing the
values returned by two individual calls of `buffer-chars-modified-tick',
you can tell whether a character change occurred in that buffer in
between these calls.  No argument or nil as argument means use current
buffer as BUFFER."
    (with-current-buffer (or buffer (current-buffer))
      (if (listp buffer-undo-list)
          (length buffer-undo-list)
        (buffer-modified-tick)))))

;; Functions not available in versions < emacs-24.
;; Allow using helm-async.el in Emacs-23 among other things.
(unless (and (fboundp 'file-equal-p)
             (fboundp 'file-in-directory-p))
  (defun file-equal-p (file1 file2)
    "Return non-nil if files FILE1 and FILE2 name the same file.
If FILE1 or FILE2 does not exist, the return value is unspecified."
    (let ((handler (or (find-file-name-handler file1 'file-equal-p)
                       (find-file-name-handler file2 'file-equal-p))))
      (if handler
          (funcall handler 'file-equal-p file1 file2)
        (let (f1-attr f2-attr)
          (and (setq f1-attr (file-attributes (file-truename file1)))
               (setq f2-attr (file-attributes (file-truename file2)))
               (equal f1-attr f2-attr))))))

  ;; This is the original loop version, more readable, not the one of 24.1+.
  (defun file-in-directory-p (file dir)
    "Return non-nil if FILE is in DIR or a subdirectory of DIR.
A directory is considered to be \"in\" itself.
Return nil if DIR is not an existing directory."
    (let ((handler (or (find-file-name-handler file 'file-in-directory-p)
                       (find-file-name-handler dir 'file-in-directory-p))))
      (if handler
          (funcall handler 'file-in-directory-p file dir)
        (when (file-directory-p dir)
          (cl-loop with f1 = (file-truename file)
                with f2 = (file-truename dir)
                with ls1 = (or (split-string f1 "/" t) (list "/"))
                with ls2 = (or (split-string f2 "/" t) (list "/"))
                for p = (string-match "^/" f1)
                for i in ls1 for j in ls2
                when (string= i j)
                concat (if p (concat "/" i) (concat i "/")) into root
                finally return (file-equal-p (file-truename root) f2)))))))


;; CUA workaround
(defadvice cua-delete-region (around helm-avoid-cua activate)
  (ignore-errors ad-do-it))

(defadvice copy-region-as-kill (around helm-avoid-cua activate)
  (if cua-mode
      (ignore-errors ad-do-it)
    ad-do-it))


;;; Utils functions
;;
;;
(defun helm-ff-find-printers ()
  "Return a list of available printers on Unix systems."
  (when (executable-find "lpstat")
    (let ((printer-list (with-temp-buffer
                          (call-process "lpstat" nil t nil "-a")
                          (split-string (buffer-string) "\n"))))
      (cl-loop for p in printer-list
            for printer = (car (split-string p))
            when printer
            collect printer))))

;; Shut up byte compiler in emacs24*.
(defun helm-switch-to-buffer (buffer-or-name)
  "Same as `switch-to-buffer' whithout warnings at compile time."
  (with-no-warnings
    (switch-to-buffer buffer-or-name)))

(cl-defmacro helm-position (item seq &key (test 'eq) all)
  "A simple and faster replacement of CL `position'.
Return position of first occurence of ITEM found in SEQ.
Argument SEQ can be a string, in this case ITEM have to be a char.
Argument ALL, if non--nil specify to return a list of positions of
all ITEM found in SEQ."
  (let ((key (if (stringp seq) 'across 'in)))
    `(cl-loop for c ,key ,seq
           for index from 0
           when (funcall ,test c ,item)
           if ,all collect index into ls
           else return index
           finally return ls)))

(defun helm-substring (str width)
  "Return the substring of string STR from 0 to WIDTH.
Handle multibyte characters by moving by columns."
  (with-temp-buffer
    (save-excursion
      (insert str))
    (move-to-column width)
    (buffer-substring (point-at-bol) (point))))

(cl-defun helm-substring-by-width (str width &optional (endstr "..."))
  "Truncate string STR to end at column WIDTH.
Similar to `truncate-string-to-width'.
Add ENDSTR (default \"...\") at end of truncated STR.
Add spaces at end if needed to reach WIDTH when STR is shorter than WIDTH."
  (cl-loop for ini-str = str
        then (substring ini-str 0 (1- (length ini-str)))
        for sw = (string-width ini-str)
        when (<= sw width) return
        (concat ini-str endstr (make-string (- width sw) ? ))))

(defun helm-string-multibyte-p (str)
  "Check if string STR contains multibyte characters."
  (cl-loop for c across str
        thereis (> (char-width c) 1)))

(defun helm-get-pid-from-process-name (process-name)
  "Get pid from running process PROCESS-NAME."
  (cl-loop with process-list = (list-system-processes)
        for pid in process-list
        for process = (assoc-default 'comm (process-attributes pid))
        when (and process (string-match process-name process))
        return pid))

(cl-defun helm-current-buffer-narrowed-p (&optional
                                            (buffer helm-current-buffer))
  "Check if BUFFER is narrowed.
Default is `helm-current-buffer'."
  (with-current-buffer buffer
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun helm-region-active-p ()
  (and transient-mark-mode mark-active (/= (mark) (point))))

(defun helm-goto-char (loc)
  "Go to char, revealing if necessary."
  (goto-char loc)
  (when (or (eq major-mode 'org-mode)
            (and (boundp 'outline-minor-mode)
                 outline-minor-mode))
    (require 'org) ; On some old Emacs versions org may not be loaded.
    (org-reveal)))

(defun helm-goto-line (lineno &optional noanim)
  "Goto LINENO opening only outline headline if needed.
Animation is used unless NOANIM is non--nil."
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (goto-char (point-min))
  (helm-goto-char (point-at-bol lineno))
  (unless noanim
    (helm-highlight-current-line nil nil nil nil 'pulse)))

(defun helm-save-pos-to-register-before-jump ()
  "Save current buffer position to `helm-save-pos-before-jump-register'.
To use this add it to `helm-goto-line-before-hook'."
  (with-helm-current-buffer
    (unless helm-in-persistent-action
      (point-to-register helm-save-pos-before-jump-register))))

(defun helm-save-current-pos-to-mark-ring ()
  "Save current buffer position to mark ring.
To use this add it to `helm-goto-line-before-hook'."
  (with-helm-current-buffer
    (unless helm-in-persistent-action
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))))

;;;###autoload
(defun helm-show-all-in-this-source-only (arg)
  "Show only current source of this helm session with all its candidates.
With a numeric prefix arg show only the ARG number of candidates."
  (interactive "p")
  (with-helm-window
    (with-helm-default-directory helm-default-directory
        (let ((helm-candidate-number-limit (and (> arg 1) arg)))
          (save-window-excursion
            (helm-set-source-filter
             (list (assoc-default 'name (helm-get-current-source)))))))))

;;;###autoload
(defun helm-display-all-sources ()
  "Display all sources previously hidden by `helm-set-source-filter'."
  (interactive)
  (helm-set-source-filter nil))

(defun helm-displaying-source-names ()
  "Return the list of sources name for this helm session."
  (with-current-buffer helm-buffer
    (goto-char (point-min))
    (cl-loop with pos
          while (setq pos (next-single-property-change (point) 'helm-header))
          do (goto-char pos)
          collect (buffer-substring-no-properties (point-at-bol)(point-at-eol))
          do (forward-line 1))))

(defun helm-files-match-only-basename (candidate)
  "Allow matching only basename of file when \" -b\" is added at end of pattern.
If pattern contain one or more spaces, fallback to match-plugin
even is \" -b\" is specified."
  (let ((source (helm-get-current-source)))
    (if (string-match "\\([^ ]*\\) -b\\'" helm-pattern)
        (progn
          (helm-attrset 'no-matchplugin nil source)
          (string-match (match-string 1 helm-pattern)
                        (helm-basename candidate)))
      ;; Disable no-matchplugin by side effect.
      (helm-aif (assq 'no-matchplugin source)
          (setq source (delete it source)))
      (string-match
       (replace-regexp-in-string " -b\\'" "" helm-pattern)
       candidate))))

(defun helm--mapconcat-candidate (candidate)
  "Transform string CANDIDATE in regexp.
e.g helm.el$
    => \"[^h]*h[^e]*e[^l]*l[^m]*m[^.]*[.][^e]*e[^l]*l$\"
    ^helm.el$
    => \"helm[.]el$\"."
  (let ((ls (split-string candidate "" t)))
    (if (string= "^" (car ls))
        (mapconcat (lambda (c)
                     (if (string= c ".")
                         (concat "[" c "]") c))
                   (cdr ls) "")
      (mapconcat (lambda (c)
                   (cond ((string= c ".")
                          (concat "[^" c "]*" (concat "[" c "]")))
                         ((string= c "$") c)
                         (t (concat "[^" c "]*" (regexp-quote c)))))
                 ls ""))))

(defun helm-skip-entries (seq regexp-list)
  "Remove entries which matches one of REGEXP-LIST from SEQ."
  (cl-loop for i in seq
        unless (cl-loop for regexp in regexp-list
                     thereis (and (stringp i)
                                  (string-match regexp i)))
        collect i))

(defun helm-shadow-entries (seq regexp-list)
  "Put shadow property on entries in SEQ matching a regexp in REGEXP-LIST."
  (let ((face 'italic))
    (cl-loop for i in seq
          if (cl-loop for regexp in regexp-list
                   thereis (and (stringp i)
                                (string-match regexp i)))
          collect (propertize i 'face face)
          else collect i)))

(defun helm-stringify (str-or-sym)
  "Get string of STR-OR-SYM."
  (if (stringp str-or-sym)
      str-or-sym
    (symbol-name str-or-sym)))

(defun helm-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
    (intern str-or-sym)))

(defun helm-describe-function (func)
  "FUNC is symbol or string."
  (describe-function (helm-symbolify func))
  (message nil))

(defun helm-describe-variable (var)
  "VAR is symbol or string."
  (describe-variable (helm-symbolify var))
  (message nil))

(defun helm-find-function (func)
  "FUNC is symbol or string."
  (find-function (helm-symbolify func)))

(defun helm-find-variable (var)
  "VAR is symbol or string."
  (find-variable (helm-symbolify var)))

(defun helm-kill-new (candidate &optional replace)
  "CANDIDATE is symbol or string.
See `kill-new' for argument REPLACE."
  (kill-new (helm-stringify candidate) replace))

(cl-defun helm-fast-remove-dups (seq &key (test 'eq))
  "Remove duplicates elements in list SEQ.
This is same as `remove-duplicates' but with memoisation.
It is much faster, especially in large lists.
A test function can be provided with TEST argument key.
Default is `eq'."
  (cl-loop with cont = (make-hash-table :test test)
        for elm in seq
        unless (gethash elm cont)
        do (puthash elm elm cont)
        finally return
        (cl-loop for i being the hash-values in cont collect i)))

;;;###autoload
(defun helm-quit-and-find-file ()
  "Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory."
  (interactive)
  (require 'helm-grep)
  (helm-run-after-quit
   (lambda (f)
     (if (file-exists-p f)
         (helm-find-files-1 (file-name-directory f)
                            (regexp-quote
                             (if helm-ff-transformer-show-only-basename
                                 (helm-basename f) f)))
       (helm-find-files-1 f)))
   (let* ((sel       (helm-get-selection))
          (grep-line (and (stringp sel)
                          (helm-grep-split-line sel))))
     (if (stringp sel)
         (helm-aif (get-buffer (or (get-text-property
                                    (1- (length sel)) 'buffer-name sel)
                                   sel))
             (or (buffer-file-name it)
                 (car (rassoc it dired-buffers))
                 (and (with-current-buffer it
                        (eq major-mode 'org-agenda-mode))
                      org-directory
                      (expand-file-name org-directory))
                 (with-current-buffer it default-directory))
           (cond ((or (file-remote-p sel)
                      (file-exists-p sel))
                  (expand-file-name sel))
                 ((and grep-line (file-exists-p (car grep-line)))
                  (expand-file-name (car grep-line)))
                 ((and ffap-url-regexp (string-match ffap-url-regexp sel)) sel)
                 (t default-directory)))
       default-directory))))

;; Same as `vc-directory-exclusion-list'.
(defvar helm-walk-ignore-directories
  '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr"
    "_MTN" "_darcs" "{arch}"))

(cl-defun helm-walk-directory (directory &key path (directories t) match skip-subdirs)
  "Walk through DIRECTORY tree.
Argument PATH can be one of basename, relative, or full, default to basename.
Argument DIRECTORIES when non--nil (default) return also directories names,
otherwise skip directories names.
Argument MATCH can be a predicate or a regexp.
Argument SKIP-SUBDIRS when non--nil will skip `helm-walk-ignore-directories'
unless it is given as a list of directories, in this case this list will be used
instead of `helm-walk-ignore-directories'."
  (let* (result
         (fn (cl-case path
               (basename 'file-name-nondirectory)
               (relative 'file-relative-name)
               (full     'identity)
               (t        'file-name-nondirectory)))
         ls-R)
    (setq ls-R (lambda (dir)
                 (unless (and skip-subdirs
                              (member (helm-basename dir)
                                      (if (listp skip-subdirs)
                                          skip-subdirs
                                        helm-walk-ignore-directories)))
                   (cl-loop with ls = (directory-files
                                       dir t directory-files-no-dot-files-regexp)
                         for f in ls
                         if (file-directory-p f)
                         do (progn (when directories
                                     (push (funcall fn f) result))
                                   ;; Don't recurse in directory symlink.
                                   (unless (file-symlink-p f)
                                     (funcall ls-R f)))
                         else do
                         (if match
                             (and (if (functionp match)
                                      (funcall match f)
                                    (and (stringp match)
                                         (string-match
                                          match (file-name-nondirectory f))))
                                  (push (funcall fn f) result))
                           (push (funcall fn f) result))))))
    (funcall ls-R directory)
    (nreverse result)))

(defun helm-generic-sort-fn (s1 s2)
  "Sort predicate function for helm candidates.
Args S1 and S2 can be single or \(display . real\) candidates,
that is sorting is done against real value of candidate."
  (let* ((reg1  (concat "\\_<" helm-pattern "\\_>"))
         (reg2  (concat "\\_<" helm-pattern))
         (split (split-string helm-pattern))
         (str1  (if (consp s1) (cdr s1) s1))
         (str2  (if (consp s2) (cdr s2) s2))
         (score #'(lambda (str r1 r2 lst)
                    (cond ((string-match r1 str) 4)
                          ((and (string-match " " helm-pattern)
                                (string-match (concat "\\_<" (car lst)) str)
                                (cl-loop for r in (cdr lst)
                                      always (string-match r str))) 3)
                          ((and (string-match " " helm-pattern)
                                (cl-loop for r in lst always (string-match r str))) 2)
                          ((string-match r2 str) 1)
                          (t 0))))
         (sc1 (funcall score str1 reg1 reg2 split))
         (sc2 (funcall score str2 reg1 reg2 split)))
    (cond ((or (zerop (string-width helm-pattern))
               (and (zerop sc1) (zerop sc2)))
           (string-lessp str1 str2))
          ((= sc1 sc2)
           (< (length str1) (length str2)))
          (t (> sc1 sc2)))))

(defun helm-basename (fname &optional ext)
  "Print FNAME  with any  leading directory  components removed.
If specified, also remove filename extension EXT."
  (let ((non-essential t))
    (if (and ext (or (string= (file-name-extension fname) ext)
                     (string= (file-name-extension fname t) ext))
             (not (file-directory-p fname)))
        (file-name-sans-extension (file-name-nondirectory fname))
      (file-name-nondirectory (directory-file-name fname)))))

(defun helm-basedir (fname)
  "Return the base directory of filename."
  (helm-aif (and fname (file-name-directory fname))
      (file-name-as-directory it)))

(defun helm-ff-get-host-from-tramp-invalid-fname (fname)
  "Extract hostname from an incomplete tramp file name.
Return nil on valid file name remote or not."
  (let* ((str (helm-basename fname))
         (split (split-string str ":"))
         (meth (car (member (car split) (mapcar 'car tramp-methods))))) 
    (when (and meth (<= (length split) 2))
      (cadr split))))

(defun helm-file-human-size (size)
  "Return a string showing SIZE of a file in human readable form.
SIZE can be an integer or a float depending it's value.
`file-attributes' will take care of that to avoid overflow error.
KBSIZE if a floating point number, default value is 1024.0."
  (let ((M (cons "M" (/ size (expt helm-default-kbsize 2))))
        (G (cons "G" (/ size (expt helm-default-kbsize 3))))
        (K (cons "K" (/ size helm-default-kbsize)))
        (B (cons "B" size)))
    (cl-loop with result = B
          for (a . b) in
          (cl-loop for (x . y) in (list M G K B)
                unless (< y 1) collect (cons x y))
          when (< b (cdr result)) do (setq result (cons a b))
          finally return (if (string= (car result) "B")
                             (format "%s" size)
                           (format "%.1f%s" (cdr result) (car result))))))

(cl-defun helm-file-attributes
    (file &key type links uid gid access-time modif-time
            status size mode gid-change inode device-num dired human-size
            mode-type mode-owner mode-group mode-other (string t))
  "Return `file-attributes' elements of FILE separately according to key value.
Availables keys are:
- TYPE: Same as nth 0 `files-attributes' if STRING is nil
        otherwise return either symlink, directory or file (default).
- LINKS: See nth 1 `files-attributes'.
- UID: See nth 2 `files-attributes'.
- GID: See nth 3 `files-attributes'.
- ACCESS-TIME: See nth 4 `files-attributes', however format time
               when STRING is non--nil (the default).
- MODIF-TIME: See nth 5 `files-attributes', same as above.
- STATUS: See nth 6 `files-attributes', same as above.
- SIZE: See nth 7 `files-attributes'.
- MODE: See nth 8 `files-attributes'.
- GID-CHANGE: See nth 9 `files-attributes'.
- INODE: See nth 10 `files-attributes'.
- DEVICE-NUM: See nth 11 `files-attributes'.
- DIRED: A line similar to what 'ls -l' return.
- HUMAN-SIZE: The size in human form, see `helm-file-human-size'.
- MODE-TYPE, mode-owner,mode-group, mode-other: Split what
  nth 7 `files-attributes' return in four categories.
- STRING: When non--nil (default) `helm-file-attributes' return
          more friendly values.
If you want the same behavior as `files-attributes' ,
\(but with return values in proplist\) use a nil value for STRING.
However when STRING is non--nil, time and type value are different from what
you have in `file-attributes'."
  (let* ((all (cl-destructuring-bind
                    (type links uid gid access-time modif-time
                          status size mode gid-change inode device-num)
                  (file-attributes file string)
                (list :type        (if string
                                       (cond ((stringp type) "symlink") ; fname
                                             (type "directory")         ; t
                                             (t "file"))                ; nil
                                     type)
                      :links       links
                      :uid         uid
                      :gid         gid
                      :access-time (if string
                                       (format-time-string
                                        "%Y-%m-%d %R" access-time)
                                     access-time)
                      :modif-time  (if string
                                       (format-time-string
                                        "%Y-%m-%d %R" modif-time)
                                     modif-time)
                      :status      (if string
                                       (format-time-string
                                        "%Y-%m-%d %R" status)
                                     status)
                      :size        size
                      :mode        mode
                      :gid-change  gid-change
                      :inode       inode
                      :device-num  device-num)))
         (modes (helm-split-mode-file-attributes (cl-getf all :mode))))
    (cond (type (cl-getf all :type))
          (links (cl-getf all :links))
          (uid   (cl-getf all :uid))
          (gid   (cl-getf all :gid))
          (access-time (cl-getf all :access-time))
          (modif-time (cl-getf all :modif-time))
          (status (cl-getf all :status))
          (size (cl-getf all :size))
          (mode (cl-getf all :mode))
          (gid-change (cl-getf all :gid-change))
          (inode (cl-getf all :inode))
          (device-num (cl-getf all :device-num))
          (dired
           (concat
            (helm-split-mode-file-attributes (cl-getf all :mode) t) " "
            (number-to-string (cl-getf all :links)) " "
            (cl-getf all :uid) ":"
            (cl-getf all :gid) " "
            (if human-size
                (helm-file-human-size (cl-getf all :size))
              (int-to-string (cl-getf all :size))) " "
              (cl-getf all :modif-time)))
          (human-size (helm-file-human-size (cl-getf all :size)))
          (mode-type (cl-getf modes :mode-type))
          (mode-owner (cl-getf modes :user))
          (mode-group (cl-getf modes :group))
          (mode-other (cl-getf modes :other))
          (t (append all modes)))))

(defun helm-split-mode-file-attributes (str &optional string)
  "Split mode file attributes STR into a proplist.
If STRING is non--nil return instead a space separated string."
  (cl-loop with type = (substring str 0 1)
        with cdr = (substring str 1)
        for i across cdr
        for count from 1
        if (<= count 3)
        concat (string i) into user
        if (and (> count 3) (<= count 6))
        concat (string i) into group
        if (and (> count 6) (<= count 9))
        concat (string i) into other
        finally return
        (if string
            (mapconcat 'identity (list type user group other) " ")
          (list :mode-type type :user user :group group :other other))))

(defun helm-current-directory ()
  "Return current-directory name at point.
Useful in dired buffers when there is inserted subdirs."
  (if (eq major-mode 'dired-mode)
      (dired-current-directory)
    default-directory))

(defmacro with-helm-display-marked-candidates (buffer-or-name candidates &rest body)
  (declare (indent 0) (debug t))
  (let ((buffer (make-symbol "buffer"))
        (window (make-symbol "window")))
    `(let* ((,buffer (temp-buffer-window-setup ,buffer-or-name))
            ,window)
       (unwind-protect
            (with-current-buffer ,buffer
              (dired-format-columns-of-files ,candidates)
              (select-window
               (setq ,window (temp-buffer-window-show
                              ,buffer
                              '(display-buffer-below-selected
                                (window-height . fit-window-to-buffer)))))
              (progn ,@body))
         (quit-window 'kill ,window)))))

;;; Persistent Action Helpers
;;
;;
;; Internal
(defvar helm-match-line-overlay nil)

(defun helm-highlight-current-line (&optional start end buf face pulse)
  "Highlight and underline current position"
  (let* ((start (or start (line-beginning-position)))
         (end (or end (1+ (line-end-position))))
         (args (list start end buf)))
    (if (not helm-match-line-overlay)
        (setq helm-match-line-overlay (apply 'make-overlay args))
      (apply 'move-overlay helm-match-line-overlay args))
    (overlay-put helm-match-line-overlay
                 'face (or face 'helm-selection-line))
    (recenter)
    (when pulse
      (sit-for 0.3)
      (helm-match-line-cleanup))))

(defun helm-match-line-cleanup ()
  (when helm-match-line-overlay
    (delete-overlay helm-match-line-overlay)
    (setq helm-match-line-overlay nil)))

(defun helm-match-line-update ()
  (when helm-match-line-overlay
    (delete-overlay helm-match-line-overlay)
    (helm-highlight-current-line)))

(add-hook 'helm-cleanup-hook 'helm-match-line-cleanup)
(add-hook 'helm-after-persistent-action-hook 'helm-match-line-update)

(defun helm-w32-prepare-filename (file)
  "Convert filename FILE to something usable by external w32 executables."
  (replace-regexp-in-string ; For UNC paths
   "/" "\\"
   (replace-regexp-in-string ; Strip cygdrive paths
    "/cygdrive/\\(.\\)" "\\1:"
    file nil nil) nil t))

;;;###autoload
(defun helm-w32-shell-execute-open-file (file)
  (interactive "fOpen file:")
  (with-no-warnings
    (w32-shell-execute "open" (helm-w32-prepare-filename file))))

(defun helm-open-file-with-default-tool (file)
  "Open FILE with the default tool on this platform."
  (let (process-connection-type)
    (if (eq system-type 'windows-nt)
        (helm-w32-shell-execute-open-file file)
      (start-process "helm-open-file-with-default-tool"
                     nil
                     (cond ((eq system-type 'gnu/linux)
                            "xdg-open")
                           ((or (eq system-type 'darwin) ;; Mac OS X
                                (eq system-type 'macos)) ;; Mac OS 9
                            "open"))
                     file))))

(defun helm-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
    (dired (file-name-directory file))
    (dired-goto-file file)))

(defun helm-action-line-goto (lineno-and-content)
  (apply #'helm-goto-file-line
         (append lineno-and-content
                 (list (helm-interpret-value (helm-attr 'target-file))
                       (if (and (helm-attr-defined 'target-file)
                                (not helm-in-persistent-action))
                           'find-file-other-window
                         'find-file)))))

(cl-defun helm-action-file-line-goto (file-line-content)
  (apply #'helm-goto-file-line
         (if (stringp file-line-content)
             ;; Case: filtered-candidate-transformer is skipped
             (cdr (helm-filtered-candidate-transformer-file-line-1
                   file-line-content))
           file-line-content)))

(defun helm-require-or-error (feature function)
  (or (require feature nil t)
      (error "Need %s to use `%s'." feature function)))

(defun helm-filtered-candidate-transformer-file-line (candidates _source)
  (delq nil (mapcar 'helm-filtered-candidate-transformer-file-line-1
                    candidates)))

(defun helm-filtered-candidate-transformer-file-line-1 (candidate)
  (when (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" candidate)
    (let ((filename (match-string 1 candidate))
          (lineno (match-string 2 candidate))
          (content (match-string 3 candidate)))
      (cons (format "%s:%s\n %s"
                    (propertize filename 'face compilation-info-face)
                    (propertize lineno 'face compilation-line-face)
                    content)
            (list (string-to-number lineno) content
                  (expand-file-name
                   filename
                   (or (helm-interpret-value (helm-attr 'default-directory))
                       (and (helm-candidate-buffer)
                            (buffer-local-value
                             'default-directory (helm-candidate-buffer))))))))))

(cl-defun helm-goto-file-line (lineno &optional content file (find-file-function #'find-file))
  (helm-aif (helm-attr 'before-jump-hook)
      (funcall it))
  (when file (funcall find-file-function file))
  (if (helm-attr-defined 'adjust)
      (helm-goto-line-with-adjustment lineno content)
    (helm-goto-line lineno))
  (unless (helm-attr-defined 'recenter)
    (set-window-start (get-buffer-window helm-current-buffer) (point)))
  (helm-aif (helm-attr 'after-jump-hook)
      (funcall it))
  (when helm-in-persistent-action
    (helm-highlight-current-line)))

(defun helm-find-file-as-root (candidate)
  (let ((buf (helm-basename candidate))
        non-essential)
    (if (buffer-live-p (get-buffer buf))
        (progn
          (set-buffer buf)
          (find-alternate-file (concat "/" helm-su-or-sudo
                                       "::" (expand-file-name candidate))))
      (find-file (concat "/" helm-su-or-sudo "::" (expand-file-name candidate))))))

(defun helm-find-many-files (_ignore)
  (mapc 'find-file (helm-marked-candidates)))

(defun helm-goto-line-with-adjustment (line line-content)
  (let ((startpos)
        offset found pat)
    ;; This constant is 1/2 the initial search window.
    ;; There is no sense in making it too small,
    ;; since just going around the loop once probably
    ;; costs about as much as searching 2000 chars.
    (setq offset 1000
          found nil
          pat (concat (if (eq selective-display t)
                          "\\(^\\|\^m\\) *" "^ *") ;allow indent
                      (regexp-quote line-content)))
    ;; If no char pos was given, try the given line number.
    (setq startpos (progn (helm-goto-line line) (point)))
    (or startpos (setq startpos (point-min)))
    ;; First see if the tag is right at the specified location.
    (goto-char startpos)
    (setq found (looking-at pat))
    (while (and (not found)
                (progn
                  (goto-char (- startpos offset))
                  (not (bobp))))
      (setq found
            (re-search-forward pat (+ startpos offset) t)
            offset (* 3 offset)))       ; expand search window
    (or found
        (re-search-forward pat nil t)
        (error "not found")))
  ;; Position point at the right place
  ;; if the search string matched an extra Ctrl-m at the beginning.
  (and (eq selective-display t)
       (looking-at "\^m")
       (forward-char 1))
  (forward-line 0))

(defun helm-quit-and-execute-action (action)
  "Quit current helm session and execute ACTION."
  (setq helm-saved-action action)
  (helm-exit-minibuffer))

;; Yank text at point.
;;
;;
;; Internal
(defvar helm-yank-point nil)

;;;###autoload
(defun helm-yank-text-at-point ()
  "Yank text at point in `helm-current-buffer' into minibuffer.
If `helm-yank-symbol-first' is non--nil the first yank
grabs the entire symbol."
  (interactive)
  (with-helm-current-buffer
    (let ((fwd-fn (if helm-yank-symbol-first
                      'forward-symbol 'forward-word)))
      ;; Start to initial point if C-w have never been hit.
      (unless helm-yank-point (setq helm-yank-point (point)))
      (save-excursion
        (goto-char helm-yank-point)
        (funcall fwd-fn 1)
        (helm-set-pattern
         (concat
          helm-pattern (replace-regexp-in-string
                        "\\`\n" ""
                        (buffer-substring-no-properties
                         helm-yank-point (point)))))
        (setq helm-yank-point (point))))))

(defun helm-reset-yank-point ()
  (setq helm-yank-point nil))

(add-hook 'helm-after-persistent-action-hook 'helm-reset-yank-point)
(add-hook 'helm-cleanup-hook 'helm-reset-yank-point)
(add-hook 'helm-after-initialize-hook 'helm-reset-yank-point)

(defun helm-html-bookmarks-to-alist (file url-regexp bmk-regexp)
  "Parse html bookmark FILE and return an alist with (title . url) as elements."
  (let (bookmarks-alist url title)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "href=\\|^ *<DT><A HREF=" nil t)
        (forward-line 0)
        (when (re-search-forward url-regexp nil t)
          (setq url (match-string 0)))
        (when (re-search-forward bmk-regexp nil t)
          (setq title (match-string 1)))
        (push (cons title url) bookmarks-alist)
        (forward-line)))
    (nreverse bookmarks-alist)))


(provide 'helm-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-utils.el ends here

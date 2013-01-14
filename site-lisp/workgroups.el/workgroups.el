;;; workgroups.el --- workgroups for windows (for Emacs)

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:     workgroups.el
;; Author:   tlh <thunkout@gmail.com>
;; Created:  2010-07-22
;; Version:  0.2.0
;; Keywords: session management window-configuration persistence

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; See the file README.md in `workgroups.el's directory
;;
;;; Installation:
;;
;;; Usage:
;;

;;; Symbol naming conventions:
;;
;; W always refers to a Workgroups window or window tree.
;;
;; WT always refers to a Workgroups window tree.
;;
;; SW always refers to a sub-window or sub-window-tree of a wtree.
;;
;; WL always refers to the window list of a wtree.
;;
;; LN, TN, RN and BN always refer to the LEFT, TOP, RIGHT and BOTTOM edges of an
;; edge list, where N is a differentiating integer.
;;
;; LS, HS, LB and HB always refer to the LOW-SIDE, HIGH-SIDE, LOW-BOUND and
;; HIGH-BOUND of a bounds list.  See `wg-with-bounds'.
;;


;;; Code:

(require 'cl)


;;; consts

(defconst wg-version "0.2.0"
  "Current version of workgroups.")

(defconst wg-persisted-workgroups-tag 'workgroups
  "This should be the car of any list of persisted workgroups.")


;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version wg-version)

(defcustom workgroups-mode-hook nil
  "Hook run when workgroups-mode is turned on."
  :type 'hook
  :group 'workgroups)

;; FIXME: This complicates loading and byte-comp too much
(defcustom wg-prefix-key (kbd "C-z")
  "Workgroups' prefix key."
  :type 'string
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (when (and (boundp 'workgroups-mode) workgroups-mode)
           (wg-set-prefix-key))
         val))

(defcustom wg-switch-hook nil
  "Hook run by `wg-switch-to-workgroup'."
  :type 'hook
  :group 'workgroups)

(defcustom wg-no-confirm nil
  "Non-nil means don't request confirmation before various
destructive operations, like `wg-reset'.  This doesn't modify
query-for-save behavior.  Use
`wg-query-for-save-on-workgroups-mode-exit' and
`wg-query-for-save-on-emacs-exit' for that."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-on t
  "Toggles Workgroups' mode-line display."
  :type 'boolean
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defcustom wg-kill-ring-size 20
  "Maximum length of the `wg-kill-ring'."
  :type 'integer
  :group 'workgroups)

(defcustom wg-warning-timeout 0.7
  "Seconds to display minibuffer warning messages."
  :type 'float
  :group 'workgroups)


;; save and load customization

(defcustom wg-switch-on-load t
  "Non-nil means switch to the first workgroup in a file when it's loaded."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-query-for-save-on-emacs-exit t
  "Non-nil means query to save changes before exiting Emacs.
Exiting workgroups removes its `kill-emacs-query-functions' hook,
so if you set this to nil, you may want to set
`wg-query-for-save-on-workgroups-exit' to t."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-query-for-save-on-workgroups-mode-exit t
  "Non-nil means query to save changes before exiting `workgroups-mode'.
Exiting workgroups removes its `kill-emacs-query-functions' hook,
which is why this variable exists."
  :type 'boolean
  :group 'workgroups)


;; workgroup restoration customization

(defcustom wg-default-buffer "*scratch*"
  "Buffer switched to when a blank workgroup is created.
Also used when a window's buffer can't be restored."
  :type 'string
  :group 'workgroups)

(defcustom wg-restore-position nil
  "Non-nil means restore frame position on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-scroll-bars t
  "Non-nil means restore scroll-bar settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-fringes t
  "Non-nil means restore fringe settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-margins t
  "Non-nil means restore margin settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-mbs-window t
  "Non-nil means restore `minibuffer-scroll-window' on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point t
  "Non-nil means restore `point' on workgroup restore.
This is included mainly so point restoration can be suspended
during `wg-morph' -- you probably want this on."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point-max t
  "Controls point restoration when point is at `point-max'.
If `point' is at `point-max' when a wconfig is created, put
`point' back at `point-max' when the wconfig is restored, even if
`point-max' has increased in the meantime.  This is useful
in (say) irc buffers where `point-max' is constantly increasing."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-dedicated t
  "Non-nil means restore `window-dedicated-p' on workgroup restore."
  :type 'boolean
  :group 'workgroups)


;; morph customization

(defcustom wg-morph-on t
  "Non-nil means use `wg-morph' when restoring wconfigs."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-morph-hsteps 9
  "Columns/iteration to step window edges during `wg-morph'.
Values lower than 1 are invalid."
  :type 'integer
  :group 'workgroups)

(defcustom wg-morph-vsteps 3
  "Rows/iteration to step window edges during `wg-morph'.
Values lower than 1 are invalid."
  :type 'integer
  :group 'workgroups)

(defcustom wg-morph-terminal-hsteps 3
  "Used instead of `wg-morph-hsteps' in terminal frames.
If nil, `wg-morph-hsteps' is used."
  :type 'integer
  :group 'workgroups)

(defcustom wg-morph-terminal-vsteps 1
  "Used instead of `wg-morph-vsteps' in terminal frames.
If nil, `wg-morph-vsteps' is used."
  :type 'integer
  :group 'workgroups)

(defcustom wg-morph-sit-for-seconds 0
  "Seconds to `sit-for' between `wg-morph' iterations.
Should probably be zero unless `redisplay' is *really* fast on
your machine, and `wg-morph-hsteps' and `wg-morph-vsteps' are
already set as low as possible."
  :type 'float
  :group 'workgroups)

(defcustom wg-morph-truncate-partial-width-windows t
  "Bound to `truncate-partial-width-windows' during `wg-morph'.
Non-nil, this prevents weird-looking continuation line behavior,
and can speed up morphing a little.  Lines jump back to their
wrapped status when `wg-morph' is complete."
  :type 'boolean
  :group 'workgroups)


;; display customization

(defcustom wg-use-faces t
  "Nil means don't use faces in various displays."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-left-brace "("
  "String to the left of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-right-brace ")"
  "String to the right of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-divider ":"
  "String between workgroup position and name in the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-left-brace "( "
  "String to the left of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-right-brace " )"
  "String to the right of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-divider " | "
  "String between workgroup names in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-current-workgroup-left-decor "-<{ "
  "String to the left of the current workgroup name in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-current-workgroup-right-decor " }>-"
  "String to the right of the current workgroup name in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-previous-workgroup-left-decor "*"
  "String to the left of the previous workgroup name in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-previous-workgroup-right-decor "*"
  "String to the right of the previous workgroup name in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-time-format "%H:%M:%S %A, %B %d %Y"
  "Format string for time display.  Passed to `format-time-string'."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-battery t
  "Non-nil means include `battery', when available, in the time display."
  :type 'boolean
  :group 'workgroups)


;;; vars

(defvar wg-file nil
  "Current workgroups file.")

(defvar wg-list nil
  "List of currently defined workgroups.")

(defvar wg-frame-table (make-hash-table)
  "Hash table keyed on frame, storing each frame's state.")

(defvar wg-dirty nil
  "Non-nil when there are unsaved changes.")

(defvar wg-kill-ring nil
  "Ring of killed or kill-ring-saved wconfigs.")

(defvar wg-window-min-width 2
  "Bound to `window-min-width' when restoring wtrees. ")

(defvar wg-window-min-height 1
  "Bound to `window-min-height' when restoring wtrees.")

(defvar wg-window-min-pad 2
  "Added to `wg-window-min-foo' to produce the actual minimum window size.")

(defvar wg-actual-min-width (+ wg-window-min-width wg-window-min-pad)
  "Actual minimum window width when creating windows.")

(defvar wg-actual-min-height (+ wg-window-min-height wg-window-min-pad)
  "Actual minimum window height when creating windows.")

(defvar wg-min-edges `(0 0 ,wg-actual-min-width ,wg-actual-min-height)
  "Smallest allowable edge list of windows created by Workgroups.")

(defvar wg-null-edges '(0 0 0 0)
  "Null edge list.")

(defvar wg-morph-max-steps 200
  "Maximum `wg-morph' iterations before forcing exit.")

(defvar wg-morph-no-error t
  "Non-nil means ignore errors during `wg-morph'.
The error message is sent to *messages* instead.  This was added
when `wg-morph' was unstable, so that the screen wouldn't be left
in an inconsistent state.  It's unnecessary now, as `wg-morph' is
stable, but is left here for the time being.")

(defvar wg-last-message nil
  "Holds the last message Workgroups sent to the echo area.")

(defvar wg-selected-window nil
  "Used during wconfig restoration to hold the selected window.")

(defvar wg-face-abbrevs nil
  "Assoc list mapping face abbreviations to face names.")


;;; faces

(defmacro wg-defface (face key spec doc &rest args)
  "`defface' wrapper adding a lookup key used by `wg-fontify'."
  (declare (indent 2))
  `(progn
     (pushnew (cons ,key ',face) wg-face-abbrevs :test #'equal)
     (defface ,face ,spec ,doc ,@args)))

(wg-defface wg-current-workgroup-face :cur
  '((((class color)) (:foreground "white")))
  "Face used for the name of the current workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-previous-workgroup-face :prev
  '((((class color)) (:foreground "light sky blue")))
  "Face used for the name of the previous workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-other-workgroup-face :other
  '((((class color)) (:foreground "light slate grey")))
  "Face used for the names of other workgroups in the list display."
  :group 'workgroups)

(wg-defface wg-command-face :cmd
  '((((class color)) (:foreground "aquamarine")))
  "Face used for command/operation strings."
  :group 'workgroups)

(wg-defface wg-divider-face :div
  '((((class color)) (:foreground "light slate blue")))
  "Face used for dividers."
  :group 'workgroups)

(wg-defface wg-brace-face :brace
  '((((class color)) (:foreground "light slate blue")))
  "Face used for left and right braces."
  :group 'workgroups)

(wg-defface wg-message-face :msg
  '((((class color)) (:foreground "light sky blue")))
  "Face used for messages."
  :group 'workgroups)

(wg-defface wg-mode-line-face :mode
  '((((class color)) (:foreground "light sky blue")))
  "Face used for workgroup position and name in the mode-line display."
  :group 'workgroups)

(wg-defface wg-filename-face :file
  '((((class color)) (:foreground "light sky blue")))
  "Face used for filenames."
  :group 'workgroups)

(wg-defface wg-frame-face :frame
  '((((class color)) (:foreground "white")))
  "Face used for frame names."
  :group 'workgroups)


;;; utils


;; functions used in macros:
(eval-and-compile

  (defun wg-take (list n)
    "Return a list of the first N elts in LIST."
    (butlast list (- (length list) n)))

  (defun wg-partition (list n &optional step)
    "Return list of N-length sublists of LIST, offset by STEP.
Iterative to prevent stack overflow."
    (let (acc)
      (while list
        (push (wg-take list n) acc)
        (setq list (nthcdr (or step n) list)))
      (nreverse acc)))
  )

(defmacro wg-with-gensyms (syms &rest body)
  "Bind all symbols in SYMS to `gensym's, and eval BODY."
  (declare (indent 1))
  `(let (,@(mapcar (lambda (sym) `(,sym (gensym))) syms)) ,@body))

(defmacro wg-dbind (args expr &rest body)
  "Abbreviation of `destructuring-bind'."
  (declare (indent 2))
  `(destructuring-bind ,args ,expr ,@body))

(defmacro wg-dohash (spec &rest body)
  "do-style wrapper for `maphash'."
  (declare (indent 1))
  (wg-dbind (key val table &optional return) spec
    `(progn (maphash (lambda (,key ,val) ,@body) ,table) ,return)))

(defmacro wg-doconcat (spec &rest body)
  "do-style wrapper for `mapconcat'."
  (declare (indent 1))
  (wg-dbind (elt seq &optional sep) spec
    `(mapconcat (lambda (,elt) ,@body) ,seq (or ,sep ""))))

(defmacro wg-docar (spec &rest body)
  "do-style wrapper for `mapcar'."
  (declare (indent 1))
  `(mapcar (lambda (,(car spec)) ,@body) ,(cadr spec)))

(defmacro wg-get-some (spec &rest body)
  "do-style wrapper for `some'.
Returns the elt itself, rather than the return value of the form."
  (declare (indent 1))
  (wg-dbind (sym list) spec
    `(some (lambda (,sym) (when (progn ,@body) ,sym)) ,list)))

(defmacro wg-when-let (binds &rest body)
  "Like `let*', but only eval BODY when all BINDS are non-nil."
  (declare (indent 1))
  (wg-dbind (bind . binds) binds
    (when (consp bind)
      `(let (,bind)
         (when ,(car bind)
           ,(if (not binds) `(progn ,@body)
              `(wg-when-let ,binds ,@body)))))))

(defmacro wg-until (test &rest body)
  "`while' not."
  (declare (indent 1))
  `(while (not ,test) ,@body))

(defmacro wg-aif (test then &rest else)
  "Anaphoric `if'."
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro wg-awhen (test &rest body)
  "Anaphoric `when'."
  (declare (indent 1))
  `(wg-aif ,test (progn ,@body)))

(defmacro wg-aand (&rest args)
  "Anaphoric `and'."
  (declare (indent defun))
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defun wg-step-to (n m step)
  "Increment or decrement N toward M by STEP.
Return M when the difference between N and M is less than STEP."
  (cond ((= n m) n)
        ((< n m) (min (+ n step) m))
        ((> n m) (max (- n step) m))))

(defun wg-within (num lo hi &optional hi-inclusive)
  "Return t when NUM is within bounds LO and HI.
HI-INCLUSIVE non-nil means the HI bound is inclusive."
  (and (>= num lo) (if hi-inclusive (<= num hi) (< num hi))))

(defun wg-last1 (list)
  "Return the last element of LIST."
  (car (last list)))

(defun wg-leave (list n)
  "Return a list of the last N elts in LIST."
  (nthcdr (- (length list) n) list))

(defun wg-rnth (n list)
  "Return the Nth element of LIST, counting from the end."
  (nth (- (length list) n 1) list))

(defun wg-insert-elt (elt list &optional pos)
  "Insert ELT into LIST at POS or the end."
  (let* ((len (length list)) (pos (or pos len)))
    (when (wg-within pos 0 len t)
      (append (wg-take list pos) (cons elt (nthcdr pos list))))))

(defun wg-move-elt (elt list pos)
  "Move ELT to position POS in LIST."
  (when (member elt list)
    (wg-insert-elt elt (remove elt list) pos)))

(defun wg-cyclic-offset-elt (elt list n)
  "Cyclically offset ELT's position in LIST by N."
  (wg-when-let ((pos (position elt list)))
    (wg-move-elt elt list (mod (+ n pos) (length list)))))

(defun wg-cyclic-nth-from-elt (elt list n)
  "Return the elt in LIST N places cyclically from ELT.
If ELT is not present is LIST, return nil."
  (wg-when-let ((pos (position elt list)))
    (nth (mod (+ pos n) (length list)) list)))

(defun wg-util-swap (elt1 elt2 list)
  "Return a copy of LIST with ELT1 and ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (wg-when-let ((p1 (position elt1 list))
                (p2 (position elt2 list)))
    (wg-move-elt elt1 (wg-move-elt elt2 list p1) p2)))

(defun wg-aget (alist key)
  "Return the value of KEY in ALIST. Uses `assq'."
  (cdr (assq key alist)))

(defun wg-acopy (alist)
  "Return a copy of ALIST's toplevel list structure."
  (wg-docar (kvp alist) (cons (car kvp) (cdr kvp))))

(defun wg-aset (alist key val)
  "Set KEY's value to VAL in ALIST.
If KEY already exists in ALIST, destructively set its value.
Otherwise, cons a new key-value-pair onto ALIST."
  (wg-aif (assq key alist) (progn (setcdr it val) alist)
    (cons (cons key val) alist)))

(defun wg-aput (alist &rest key-value-pairs)
  "Add all KEY-VALUE-PAIRS to a copy of ALIST, and return the copy."
  (flet ((rec (alist kvps) (if (not kvps) alist
                             (wg-dbind (k v . rest) kvps
                               (wg-aset (rec alist rest) k v)))))
    (rec (wg-acopy alist) key-value-pairs)))

(defun wg-get-alist (key val alist-list)
  "Return the first alist in ALIST-LIST containing KEY and VAL."
  (catch 'res
    (dolist (alist alist-list)
      (when (equal val (cdr (assoc key alist)))
        (throw 'res alist)))))

(defmacro wg-abind (alist binds &rest body)
  "Bind values in ALIST to symbols in BINDS, then eval BODY.
If an elt of BINDS is a symbol, use it as both the bound variable
and the key in ALIST.  If it is a cons, use the car as the bound
variable, and the cadr as the key."
  (declare (indent 2))
  (wg-with-gensyms (asym)
    `(let* ((,asym ,alist)
            ,@(wg-docar (bind binds)
                (let ((c (consp bind)))
                  `(,(if c (car bind) bind)
                    (wg-aget ,asym ',(if c (cadr bind) bind))))))
       ,@body)))

(defmacro wg-fill-keymap (keymap &rest binds)
  "Return KEYMAP after defining in it all keybindings in BINDS."
  (declare (indent 1))
  (wg-with-gensyms (km)
    `(let ((,km ,keymap))
       ,@(wg-docar (b (wg-partition binds 2))
           `(define-key ,km (kbd ,(car b)) ,(cadr b)))
       ,km)))

(defun wg-write-sexp-to-file (sexp file)
  "Write the printable representation of SEXP to FILE."
  (with-temp-buffer
    (let (print-level print-length)
      (insert (format "%S" sexp))
      (write-file file))))

(defun wg-read-sexp-from-file (file)
  "Read and return an sexp from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun wg-read-object (prompt test warning &rest args)
  "PROMPT for an object that satisfies TEST, WARNING if necessary.
ARGS are `read-from-minibuffer's args, after PROMPT."
  (let ((obj (apply #'read-from-minibuffer prompt args)))
    (wg-until (funcall test obj)
      (message warning)
      (sit-for wg-warning-timeout)
      (setq obj (apply #'read-from-minibuffer prompt args)))
    obj))


;;; workgroups utils

(defun wg-type-of (obj)
  "Return workgroups' object type of OBJ."
  (wg-aget obj 'type))

(defun wg-type-p (type obj)
  "Return t if OBJ is of type TYPE, nil otherwise."
  (and (consp obj) (eq type (wg-type-of obj))))

(defun wg-type-check (type obj &optional noerror)
  "Throw an error if OBJ is not of type TYPE."
  (or (wg-type-p type obj)
      (unless noerror
        (error "%s is not of type %s" obj type))))

(defun wg-cyclic-nth-from-frame (&optional n frame)
  "Return the frame N places away from FRAME in `frame-list' cyclically.
N defaults to 1, and FRAME defaults to `selected-frame'."
  (wg-cyclic-nth-from-elt
   (or frame (selected-frame)) (frame-list) (or n 1)))

(defun wg-add-face (facekey str)
  "Return a copy of STR fontified according to FACEKEY.
FACEKEY must be a key in `wg-face-abbrevs'."
  (let ((face (wg-aget wg-face-abbrevs facekey))
        (str  (copy-seq str)))
    (unless face (error "No face with key %s" facekey))
    (if (not wg-use-faces) str
      (put-text-property 0 (length str) 'face face str)
      str)))

(defmacro wg-fontify (&rest specs)
  "A small fontification DSL. *WRITEME*"
  (declare (indent defun))
  `(concat
    ,@(wg-docar (spec specs)
        (typecase spec
          (cons (if (keywordp (car spec))
                    `(wg-add-face
                      ,(car spec)
                      ,(if (stringp (cadr spec))
                           (cadr spec)
                         `(format "%s" ,(cadr spec))))
                  `(progn ,spec)))
          (string `(progn ,spec))
          (atom `(format "%s" ,spec))))))

(defun wg-error-on-active-minibuffer ()
  "Throw an error when the minibuffer is active."
  (when (active-minibuffer-window)
    (error "Workgroup operations aren't permitted while the \
minibuffer is active.")))


;;; type predicates

(defun wg-window-p (obj)
  "Return t if OBJ is a Workgroups window, nil otherwise."
  (wg-type-p 'window obj))

(defun wg-wtree-p (obj)
  "Return t if OBJ is a Workgroups window tree, nil otherwise."
  (wg-type-p 'wtree obj))

(defun wg-wconfig-p (obj)
  "Return t if OBJ is a Workgroups window config, nil otherwise."
  (wg-type-p 'wconfig obj))

(defun wg-workgroup-p (obj)
  "Return t if OBJ is a workgroup, nil otherwise."
  (wg-type-p 'workgroup obj))


;; window config utils

;; Accessors for common fields:
(defun wg-dir   (w) (wg-aget w 'dir))
(defun wg-edges (w) (wg-aget w 'edges))
(defun wg-wlist (w) (wg-aget w 'wlist))
(defun wg-wtree (w) (wg-aget w 'wtree))

(defun wg-min-size (dir)
  "Return the minimum window size in split direction DIR."
  (if dir wg-window-min-height wg-window-min-width))

(defun wg-actual-min-size (dir)
  "Return the actual minimum window size in split direction DIR."
  (if dir wg-actual-min-height wg-actual-min-width))

(defmacro wg-with-edges (w spec &rest body)
  "Bind W's edge list to SPEC and eval BODY."
  (declare (indent 2))
  `(wg-dbind ,spec (wg-edges ,w) ,@body))

(defun wg-put-edges (w left top right bottom)
  "Return a copy of W with an edge list of LEFT TOP RIGHT and BOTTOM."
  (wg-aput w 'edges (list left top right bottom)))

(defmacro wg-with-bounds (w dir spec &rest body)
  "Bind SPEC to W's bounds in DIR, and eval BODY.
\"Bounds\" are a direction-independent way of dealing with edge lists."
  (declare (indent 3))
  (wg-with-gensyms (dir-sym l1 t1 r1 b1)
    (wg-dbind (ls1 hs1 lb1 hb1) spec
      `(wg-with-edges ,w (,l1 ,t1 ,r1 ,b1)
         (cond (,dir (let ((,ls1 ,l1) (,hs1 ,r1) (,lb1 ,t1) (,hb1 ,b1))
                       ,@body))
               (t    (let ((,ls1 ,t1) (,hs1 ,b1) (,lb1 ,l1) (,hb1 ,r1))
                       ,@body)))))))

(defun wg-put-bounds (w dir ls hs lb hb)
  "Set W's edges in DIR with bounds LS HS LB and HB."
  (if dir (wg-put-edges w ls lb hs hb) (wg-put-edges w lb ls hb hs)))

(defun wg-step-edges (edges1 edges2 hstep vstep)
  "Return W1's edges stepped once toward W2's by HSTEP and VSTEP."
  (wg-dbind (l1 t1 r1 b1) edges1
    (wg-dbind (l2 t2 r2 b2) edges2
      (let ((left (wg-step-to l1 l2 hstep))
            (top  (wg-step-to t1 t2 vstep)))
        (list left top
              (+ left (wg-step-to (- r1 l1) (- r2 l2) hstep))
              (+ top  (wg-step-to (- b1 t1) (- b2 t2) vstep)))))))

(defun wg-w-edge-operation (w edges op)
  "Return a copy of W with its edges mapped against EDGES through OP."
  (wg-aput w 'edges (mapcar* op (wg-aget w 'edges) edges)))

(defun wg-first-win (w)
  "Return the first actual window in W."
  (if (wg-window-p w) w (wg-first-win (car (wg-wlist w)))))

(defun wg-last-win (w)
  "Return the last actual window in W."
  (if (wg-window-p w) w (wg-last-win (wg-last1 (wg-wlist w)))))

(defun wg-minify-win (w)
  "Return a copy of W with the smallest allowable dimensions."
  (let* ((edges (wg-edges w))
         (left (car edges))
         (top (cadr edges)))
    (wg-put-edges w left top
                  (+ left wg-actual-min-width)
                  (+ top  wg-actual-min-height))))

(defun wg-minify-last-win (w)
  "Minify the last actual window in W."
  (wg-minify-win (wg-last-win w)))

(defun wg-wsize (w &optional height)
  "Return the width or height of W, calculated from its edge list."
  (wg-with-edges w (l1 t1 r1 b1)
    (if height (- b1 t1) (- r1 l1))))

(defun wg-adjust-wsize (w width-fn height-fn &optional new-left new-top)
  "Adjust W's width and height with WIDTH-FN and HEIGHT-FN."
  (wg-with-edges w (left top right bottom)
    (let ((left (or new-left left)) (top (or new-top top)))
      (wg-put-edges w left top
                    (+ left (funcall width-fn  (- right  left)))
                    (+ top  (funcall height-fn (- bottom top)))))))

(defun wg-scale-wsize (w width-scale height-scale)
  "Scale W's size by WIDTH-SCALE and HEIGHT-SCALE."
  (flet ((wscale (width)  (truncate (* width  width-scale)))
         (hscale (height) (truncate (* height height-scale))))
    (wg-adjust-wsize w #'wscale #'hscale)))

(defun wg-equal-wtrees (w1 w2)
  "Return t when W1 and W2 have equal structure."
  (cond ((and (wg-window-p w1) (wg-window-p w2))
         (equal (wg-edges w1) (wg-edges w2)))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (and (eq (wg-dir w1) (wg-dir w2))
              (equal (wg-edges w1) (wg-edges w2))
              (every #'wg-equal-wtrees (wg-wlist w1) (wg-wlist w2))))))

;; FIXME: Require a minimum size to fix wscaling
(defun wg-normalize-wtree (wtree)
  "Clean up and return a new wtree from WTREE.
Recalculate the edge lists of all subwins, and remove subwins
outside of WTREE's bounds.  If there's only one element in the
new wlist, return it instead of a new wtree."
  (if (wg-window-p wtree) wtree
    (wg-abind wtree (dir wlist)
      (wg-with-bounds wtree dir (ls1 hs1 lb1 hb1)
        (let* ((min-size (wg-min-size dir))
               (max (- hb1 1 min-size))
               (lastw (wg-last1 wlist)))
          (flet ((mapwl
                  (wl)
                  (wg-dbind (sw . rest) wl
                    (cons (wg-normalize-wtree
                           (wg-put-bounds
                            sw dir ls1 hs1 lb1
                            (setq lb1 (if (eq sw lastw) hb1
                                        (let ((hb2 (+ lb1 (wg-wsize sw dir))))
                                          (if (>= hb2 max) hb1 hb2))))))
                          (when (< lb1 max) (mapwl rest))))))
            (let ((new (mapwl wlist)))
              (if (cdr new) (wg-aput wtree 'wlist new)
                (car new)))))))))

(defun wg-scale-wtree (wtree wscale hscale)
  "Return a copy of WTREE with its dimensions scaled by WSCALE and HSCALE.
All WTREE's subwins are scaled as well."
  (let ((scaled (wg-scale-wsize wtree wscale hscale)))
    (if (wg-window-p wtree) scaled
      (wg-aput scaled
               'wlist (wg-docar (sw (wg-wlist scaled))
                        (wg-scale-wtree sw wscale hscale))))))

(defun wg-scale-wconfigs-wtree (wconfig new-width new-height)
  "Scale WCONFIG's wtree with NEW-WIDTH and NEW-HEIGHT.
Return a copy WCONFIG's wtree scaled with `wg-scale-wtree' by the
ratio or NEW-WIDTH to WCONFIG's width, and NEW-HEIGHT to
WCONFIG's height."
  (wg-normalize-wtree
   (wg-scale-wtree
    (wg-wtree wconfig)
    (/ (float new-width)  (wg-aget wconfig 'width))
    (/ (float new-height) (wg-aget wconfig 'height)))))

(defun w-set-frame-size-and-scale-wtree (wconfig &optional frame)
  "Set FRAME's size to WCONFIG's, returning a possibly scaled wtree.
If the frame size was set correctly, return WCONFIG's wtree
unchanged.  If it wasn't, return a copy of WCONFIG's wtree scaled
with `wg-scale-wconfigs-wtree' to fit the frame as it exists."
  (let ((frame (or frame (selected-frame))))
    (wg-abind wconfig ((wcwidth width) (wcheight height))
      (when window-system (set-frame-size frame wcwidth wcheight))
      (let ((fwidth  (frame-parameter frame 'width))
            (fheight (frame-parameter frame 'height)))
        (if (and (= wcwidth fwidth) (= wcheight fheight))
            (wg-wtree wconfig)
          (wg-scale-wconfigs-wtree wconfig fwidth fheight))))))

(defun wg-reverse-wlist (w &optional dir)
  "Reverse W's wlist and those of all its sub-wtrees in direction DIR.
If DIR is nil, reverse WTREE horizontally.
If DIR is 'both, reverse WTREE both horizontally and vertically.
Otherwise, reverse WTREE vertically."
  (flet ((inner (w) (if (wg-window-p w) w
                      (wg-abind w ((d1 dir) edges wlist)
                        (wg-make-wtree
                         d1 edges
                         (let ((wl2 (mapcar #'inner wlist)))
                           (if (or (eq dir 'both)
                                   (and (not dir) (not d1))
                                   (and dir d1))
                               (nreverse wl2) wl2)))))))
    (wg-normalize-wtree (inner w))))

(defun wg-reverse-wconfig (&optional dir wconfig)
  "Reverse WCONFIG's wtree's wlist in direction DIR."
  (let ((wc (or wconfig (wg-make-wconfig))))
    (wg-aput wc 'wtree (wg-reverse-wlist (wg-aget wc 'wtree) dir))))

(defun wg-wtree-move-window (wtree offset)
  "Offset `selected-window' OFFSET places in WTREE."
  (flet ((inner
          (w)
          (if (wg-window-p w) w
            (wg-abind w ((d1 dir) edges wlist)
              (wg-make-wtree
               d1 edges
               (wg-aif (wg-get-some (sw wlist) (wg-aget sw 'selwin))
                   (wg-cyclic-offset-elt it wlist offset)
                 (mapcar #'inner wlist)))))))
    (wg-normalize-wtree (inner wtree))))

(defun wg-wconfig-move-window (offset &optional wconfig)
  "Offset `selected-window' OFFSET places in WCONFIG."
  (let ((wc (or wconfig (wg-make-wconfig))))
    (wg-aput wc 'wtree (wg-wtree-move-window (wg-aget wc 'wtree) offset))))


;;; wconfig making

(defun wg-window-point (ewin)
  "Return `point' or :max.  See `wg-restore-point-max'.
EWIN should be an Emacs window object."
  (let ((p (window-point ewin)))
    (if (and wg-restore-point-max (= p (point-max))) :max p)))

(defun wg-ewin->window (ewin)
  "Return a new workgroups window from EWIN.
EWIN should be an Emacs window object."
  (with-current-buffer (window-buffer ewin)
    `((type      .   window)
      (edges     .  ,(window-edges ewin))
      (bname     .  ,(buffer-name))
      (fname     .  ,(buffer-file-name))
      (point     .  ,(wg-window-point ewin))
      (mark      .  ,(mark))
      (markx     .  ,mark-active)
      (wstart    .  ,(window-start ewin))
      (hscroll   .  ,(window-hscroll ewin))
      (sbars     .  ,(window-scroll-bars ewin))
      (margins   .  ,(window-margins ewin))
      (fringes   .  ,(window-fringes ewin))
      (selwin    .  ,(eq ewin (selected-window)))
      (mbswin    .  ,(eq ewin minibuffer-scroll-window))
      (dedicated .  ,(window-dedicated-p ewin)))))

(defun wg-make-wtree (dir edges wlist)
  "Return a new Workgroups wtree from DIR EDGES and WLIST."
  `((type   .   wtree)
    (dir    .  ,dir)
    (edges  .  ,edges)
    (wlist  .  ,wlist)))

(defun wg-ewtree->wtree (&optional ewtree)
  "Return a new Workgroups wtree from EWTREE or `window-tree'.
If specified, EWTREE should be an Emacs `window-tree'."
  (wg-error-on-active-minibuffer)
  (flet ((inner (ewt) (if (windowp ewt) (wg-ewin->window ewt)
                        (wg-dbind (dir edges . wins) ewt
                          (wg-make-wtree
                           dir edges (mapcar #'inner wins))))))
    (let ((ewt (car (or ewtree (window-tree)))))
      (when (and (windowp ewt) (window-minibuffer-p ewt))
        (error "Workgroups can't operate on minibuffer-only frames."))
      (inner ewt))))

(defun wg-make-wconfig ()
  "Return a new Workgroups window config from `selected-frame'."
  (message nil)
  `((type    .   wconfig)
    (left    .  ,(frame-parameter nil 'left))
    (top     .  ,(frame-parameter nil 'top))
    (width   .  ,(frame-parameter nil 'width))
    (height  .  ,(frame-parameter nil 'height))
    (sbars   .  ,(frame-parameter nil 'vertical-scroll-bars))
    (sbwid   .  ,(frame-parameter nil 'scroll-bar-width))
    (wtree   .  ,(wg-ewtree->wtree))))

(defun wg-make-blank-wconfig (&optional buffer)
  "Return a new blank wconfig.
BUFFER or `wg-default-buffer' is visible in the only window."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer wg-default-buffer))
    (wg-make-wconfig)))


;;; wconfig restoring

(defun wg-switch-to-window-buffer (win)
  "Switch to a buffer determined from WIN's fname and bname.
Return the buffer if it was found, nil otherwise."
  (wg-abind win (fname bname)
    (cond ((and fname (file-exists-p fname))
           (find-file fname)
           (rename-buffer bname)
           (current-buffer))
          ((wg-awhen (get-buffer bname) (switch-to-buffer it)))
          (t (switch-to-buffer wg-default-buffer) nil))))

(defun wg-restore-window (win)
  "Restore WIN in `selected-window'."
  (wg-abind win (point mark markx wstart hscroll sbars
                       fringes margins selwin mbswin dedicated)
    (let ((sw (selected-window)))
      (when selwin (setq wg-selected-window sw))
      (when (wg-switch-to-window-buffer win)
        (when (and wg-restore-mbs-window mbswin)
          (setq minibuffer-scroll-window sw))
        (when wg-restore-scroll-bars
          (set-window-scroll-bars
           sw (nth 0 sbars) (nth 2 sbars) (nth 3 sbars)))
        (when wg-restore-fringes
          (apply #'set-window-fringes sw fringes))
        (when wg-restore-margins
          (set-window-margins sw (car margins) (cdr margins)))
        (when wg-restore-dedicated
          (set-window-dedicated-p sw dedicated))
        (set-window-hscroll sw hscroll)
        (set-mark mark)
        (unless markx (deactivate-mark))
        (let ((pm (point-max)))
          (set-window-start sw wstart t)
          (goto-char (cond ((not wg-restore-point) wstart)
                           ((eq point :max) pm)
                           (t point)))
          (when (>= wstart pm) (recenter)))))))

(defun wg-restore-wtree (wtree)
  "Restore WTREE in `selected-frame'."
  (flet ((inner (w) (if (wg-wtree-p w)
                        (wg-abind w ((d dir) wlist)
                          (let ((lastw (wg-last1 wlist)))
                            (dolist (sw wlist)
                              (unless (eq sw lastw)
                                (split-window nil (wg-wsize sw d) (not d)))
                              (inner sw))))
                      (wg-restore-window w)
                      (other-window 1))))
    (let ((window-min-width  wg-window-min-width)
          (window-min-height wg-window-min-height))
      (delete-other-windows)
      (set-window-dedicated-p nil nil)
      (setq wg-selected-window nil)
      (inner wtree)
      (wg-awhen wg-selected-window (select-window it)))))

(defun wg-restore-wconfig (wconfig)
  "Restore WCONFIG in `selected-frame'."
  (wg-error-on-active-minibuffer)
  (let ((frame (selected-frame)) wtree)
    (wg-abind wconfig (left top sbars sbwid)
      (setq wtree (w-set-frame-size-and-scale-wtree wconfig frame))
      (when (and wg-restore-position left top)
        (set-frame-position frame left top))
      (when (and wg-morph-on after-init-time)
        (wg-morph (wg-ewtree->wtree) wtree wg-morph-no-error))
      (wg-restore-wtree wtree)
      (when wg-restore-scroll-bars
        (set-frame-parameter frame 'vertical-scroll-bars sbars)
        (set-frame-parameter frame 'scroll-bar-width sbwid)))))

(defun wg-restore-blank-wconfig ()
  "Restore a new blank wconfig in `selected-frame'."
  (wg-restore-wconfig (wg-make-blank-wconfig)))


;;; morph

(defun wg-morph-step-edges (w1 w2)
  "Step W1's edges toward W2's by `wg-morph-hsteps' and `wg-morph-vsteps'."
  (wg-step-edges (wg-edges w1) (wg-edges w2)
                 wg-morph-hsteps wg-morph-vsteps))

(defun wg-morph-determine-steps (gui-steps &optional term-steps)
  (max 1 (if (and (not window-system) term-steps) term-steps gui-steps)))

(defun wg-morph-match-wlist (wt1 wt2)
  "Return a wlist by matching WT1's wlist to WT2's.
When wlist1's and wlist2's lengths are equal, return wlist1.
When wlist1 is shorter than wlist2, add a window at the front of wlist1.
When wlist1 is longer than wlist2, package up wlist1's excess windows
into a wtree, so it's the same length as wlist2."
  (let* ((wl1 (wg-wlist wt1)) (l1 (length wl1)) (d1 (wg-dir wt1))
         (wl2 (wg-wlist wt2)) (l2 (length wl2)))
    (cond ((= l1 l2) wl1)
          ((< l1 l2)
           (cons (wg-minify-last-win (wg-rnth (1+ l1) wl2))
                 (if (< (wg-wsize (car wl1) d1)
                        (* 2 (wg-actual-min-size d1)))
                     wl1
                   (cons (wg-w-edge-operation (car wl1) wg-min-edges #'-)
                         (cdr wl1)))))
          ((> l1 l2)
           (append (wg-take wl1 (1- l2))
                   (list (wg-make-wtree d1 wg-null-edges
                                        (nthcdr (1- l2) wl1))))))))

(defun wg-morph-win->win (w1 w2 &optional swap)
  "Return a copy of W1 with its edges stepped once toward W2.
When SWAP is non-nil, return a copy of W2 instead."
  (wg-aput (if swap w2 w1) 'edges (wg-morph-step-edges w1 w2)))

(defun wg-morph-win->wtree (win wt)
  "Return a new wtree with WIN's edges and WT's last two windows."
  (wg-make-wtree
   (wg-dir wt)
   (wg-morph-step-edges win wt)
   (let ((wg-morph-hsteps 2) (wg-morph-vsteps 2))
     (wg-docar (w (wg-leave (wg-wlist wt) 2))
       (wg-morph-win->win (wg-minify-last-win w) w)))))

(defun wg-morph-wtree->win (wt win &optional noswap)
  "Grow the first window of WT and its subtrees one step toward WIN.
This eventually wipes WT's components, leaving only a window.
Swap WT's first actual window for WIN, unless NOSWAP is non-nil."
  (if (wg-window-p wt) (wg-morph-win->win wt win (not noswap))
    (wg-make-wtree
     (wg-dir wt)
     (wg-morph-step-edges wt win)
     (wg-dbind (fwin . wins) (wg-wlist wt)
       (cons (wg-morph-wtree->win fwin win noswap)
             (wg-docar (sw wins)
               (if (wg-window-p sw) sw
                 (wg-morph-wtree->win sw win t))))))))

(defun wg-morph-wtree->wtree (wt1 wt2)
  "Return a new wtree morphed one step toward WT2 from WT1.
Mutually recursive with `wg-morph-dispatch' to traverse the
structures of WT1 and WT2 looking for discrepancies."
  (let ((d1 (wg-dir wt1)) (d2 (wg-dir wt2)))
    (wg-make-wtree
     d2 (wg-morph-step-edges wt1 wt2)
     (if (not (eq (wg-dir wt1) (wg-dir wt2)))
         (list (wg-minify-last-win wt2) wt1)
       (mapcar* #'wg-morph-dispatch
                (wg-morph-match-wlist wt1 wt2)
                (wg-wlist wt2))))))

(defun wg-morph-dispatch (w1 w2)
  "Return a wtree morphed one step toward W2 from W1.
Dispatches on each possible combination of types."
  (cond ((and (wg-window-p w1) (wg-window-p w2))
         (wg-morph-win->win w1 w2 t))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (wg-morph-wtree->wtree w1 w2))
        ((and (wg-window-p w1) (wg-wtree-p w2))
         (wg-morph-win->wtree w1 w2))
        ((and (wg-wtree-p w1) (wg-window-p w2))
         (wg-morph-wtree->win w1 w2))))

(defun wg-morph (from to &optional noerror)
  "Morph from wtree FROM to wtree TO.
Assumes both FROM and TO fit in `selected-frame'."
  (let ((wg-morph-hsteps
         (wg-morph-determine-steps wg-morph-hsteps wg-morph-terminal-hsteps))
        (wg-morph-vsteps
         (wg-morph-determine-steps wg-morph-vsteps wg-morph-terminal-vsteps))
        (wg-restore-scroll-bars nil)
        (wg-restore-fringes nil)
        (wg-restore-margins nil)
        (wg-restore-point nil)
        (truncate-partial-width-windows
         wg-morph-truncate-partial-width-windows)
        (watchdog 0))
    (condition-case err
        (wg-until (wg-equal-wtrees from to)
          (when (> (incf watchdog) wg-morph-max-steps)
            (error "`wg-morph-max-steps' exceeded"))
          (setq from (wg-normalize-wtree (wg-morph-dispatch from to)))
          (wg-restore-wtree from)
          (redisplay)
          (unless (zerop wg-morph-sit-for-seconds)
            (sit-for wg-morph-sit-for-seconds t)))
      (error (if noerror (message "%S" err) (error "%S" err))))))


;;; global error wrappers

(defun wg-file (&optional noerror)
  "Return `wg-file' or error."
  (or wg-file
      (unless noerror
        (error "Workgroups isn't visiting a file"))))

(defun wg-list (&optional noerror)
  "Return `wg-list' or error."
  (or wg-list
      (unless noerror
        (error "No workgroups are defined."))))

(defun wg-get-workgroup (key val &optional noerror)
  "Return the workgroup whose KEY equals VAL or error."
  (or (wg-get-alist key val (wg-list noerror))
      (unless noerror
        (error "There is no workgroup with an %S of %S" key val))))


;;; frame-table ops

(defmacro wg-with-frame-state (frame state &rest body)
  "Bind FRAME and STATE and eval BODY.
FRAME is bound to `selected-frame', and STATE is bound to FRAME's
value in `wg-frame-table'."
  (declare (indent 2))
  `(let* ((,frame (selected-frame))
          (,state (or (gethash ,frame wg-frame-table)
                      (puthash ,frame (make-hash-table)
                               wg-frame-table))))
     ,@body))

(defun wg-frame-val (key)
  "Return KEY's value in `selected-frame's state in `wg-frame-table'."
  (wg-with-frame-state frame state
                       (gethash key state)))

(defun wg-set-frame-val (key val)
  "Set KEY to VAL in `selected-frame's state in `wg-frame-table'."
  (wg-with-frame-state frame state
                       (puthash key val state)))

(defun wg-delete-frame-key (key)
  "Remove KEY from `selected-frame's state in `wg-frame-table'."
  (wg-with-frame-state frame state
                       (remhash key state)))

(defun wg-delete-frame (frame)
  "Remove FRAME from `wg-frame-table'."
  (remhash frame wg-frame-table))


;;; workgroup property ops

(defun wg-get-workgroup-prop (prop workgroup)
  "Return PROP's value in WORKGROUP."
  (wg-type-check 'workgroup workgroup)
  (wg-aget workgroup prop))

(defun wg-set-workgroup-prop (prop val workgroup &optional nodirty)
  "Set PROP to VAL in WORKGROUP, setting `wg-dirty' unless NODIRTY."
  (wg-type-check 'workgroup workgroup)
  (setcdr (assq prop workgroup) val)
  (unless nodirty (setq wg-dirty t)))

(defun wg-uid (workgroup)
  "Return WORKGROUP's uid."
  (wg-get-workgroup-prop 'uid workgroup))

(defun wg-set-uid (workgroup uid)
  "Set the uid of WORKGROUP to UID."
  (wg-set-workgroup-prop 'uid uid workgroup))

(defun wg-uids (&optional noerror)
  "Return a list of workgroups uids."
  (mapcar 'wg-uid (wg-list noerror)))

(defun wg-new-uid ()
  "Return a uid greater than any in `wg-list'."
  (let ((uids (wg-uids t)) (new -1))
    (dolist (uid uids (1+ new))
      (setq new (max uid new)))))

(defun wg-name (workgroup)
  "Return the name of WORKGROUP."
  (wg-get-workgroup-prop 'name workgroup))

(defun wg-set-name (workgroup name)
  "Set the name of WORKGROUP to NAME."
  (wg-set-workgroup-prop 'name name workgroup))

(defun wg-names (&optional noerror)
  "Return a list of workgroup names."
  (mapcar 'wg-name (wg-list noerror)))


;;; current and previous workgroup ops

(defun wg-get-frame-workgroup (key &optional noerror)
  "Return the workgroup under KEY in `wg-frame-table'."
  (or (wg-frame-val key)
      (unless noerror
        (error "There's no %s in the frame" key))))

(defun wg-current-workgroup (&optional noerror)
  "Return the current workgroup."
  (wg-get-frame-workgroup 'current-workgroup noerror))

(defun wg-set-current-workgroup (workgroup)
  "Set the current workgroup to WORKGROUP."
  (wg-set-frame-val 'current-workgroup workgroup))

(defun wg-previous-workgroup (&optional noerror)
  "Return the previous workgroup."
  (wg-get-frame-workgroup 'previous-workgroup noerror))

(defun wg-set-previous-workgroup (workgroup)
  "Set the previous workgroup to WORKGROUP."
  (wg-set-frame-val 'previous-workgroup workgroup))


;;; base and working configs

(defun wg-set-base-config (workgroup config)
  "Set the base config of WORKGROUP to CONFIG."
  (wg-set-workgroup-prop 'wconfig config workgroup))

(defun wg-base-config (workgroup)
  "Return the base config of WORKGROUP."
  (wg-get-workgroup-prop 'wconfig workgroup))

(defun wg-set-working-config (workgroup config)
  "Set the working config of WORKGROUP to CONFIG."
  (wg-set-frame-val (wg-uid workgroup) config))

(defun wg-update-working-config (workgroup)
  "Set WORKGROUP's working config to the current window config."
  (wg-set-working-config workgroup (wg-make-wconfig)))

(defun wg-working-config (workgroup)
  "Return the working config of WORKGROUP.
If WORKGROUP is the current workgroup, update it first."
  (when (eq workgroup (wg-current-workgroup t))
    (wg-update-working-config workgroup))
  (or (wg-frame-val (wg-uid workgroup))
      (wg-base-config workgroup)))


;;; workgroup making and restoring

(defun wg-make-workgroup (uid name wconfig)
  "Return a new workgroup from UID, NAME and WCONFIG."
  `((type     .   workgroup)
    (uid      .  ,uid)
    (name     .  ,name)
    (wconfig  .  ,wconfig)))

(defun wg-make-default-workgroup (name)
  "Return a new workgroup named NAME with wconfig `wg-make-wconfig'."
  (wg-make-workgroup nil name (wg-make-wconfig)))

(defun wg-make-blank-workgroup (name &optional buffer)
  "Return a new blank workgroup named NAME, optionally viewing BUFFER."
  (wg-make-workgroup nil name (wg-make-blank-wconfig buffer)))

(defun wg-restore-workgroup (workgroup &optional base)
  "Restore WORKGROUP's working config, or base config is BASE is non-nil."
  (wg-restore-wconfig (if base (wg-base-config workgroup)
                        (wg-working-config workgroup))))


;;; workgroups list ops

(defun wg-delete (workgroup)
  "Remove WORKGROUP from `wg-list'.
Also delete all references to it in `wg-frame-table'."
  (wg-dohash (frame state wg-frame-table)
    (with-selected-frame frame
      (wg-delete-frame-key (wg-uid workgroup))
      (when (eq workgroup (wg-current-workgroup t))
        (wg-set-current-workgroup nil))
      (when (eq workgroup (wg-previous-workgroup t))
        (wg-set-previous-workgroup nil))))
  (setq wg-dirty t wg-list (remove workgroup (wg-list))))

(defun wg-add (new &optional pos)
  "Add WORKGROUP to `wg-list'.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-get-workgroup 'name (wg-name new) t)
    (unless pos (setq pos (position it wg-list)))
    (wg-delete it))
  (wg-set-uid new (wg-new-uid))
  (setq wg-dirty t wg-list (wg-insert-elt new wg-list pos)))

(defun wg-check-and-add (workgroup)
  "Add WORKGROUP to `wg-list'.
Query to overwrite if a workgroup with the same name exists."
  (let ((name (wg-name workgroup)))
    (when (wg-get-workgroup 'name name t)
      (unless (or wg-no-confirm
                  (y-or-n-p (format "%S exists. Overwrite? " name)))
        (error "Cancelled"))))
  (wg-add workgroup))

(defun wg-cyclic-offset-workgroup (workgroup n)
  "Offset WORKGROUP's position in `wg-list' by N."
  (wg-aif (wg-cyclic-offset-elt workgroup (wg-list) n)
      (setq wg-list it wg-dirty t)
    (error "Workgroup isn't present in `wg-list'.")))

(defun wg-list-swap (w1 w2)
  "Swap the positions of W1 and W2 in `wg-list'."
  (when (eq w1 w2) (error "Can't swap a workgroup with itself"))
  (wg-aif (wg-util-swap w1 w2 (wg-list))
      (setq wg-list it wg-dirty t)
    (error "Both workgroups aren't present in `wg-list'.")))


;;; buffer list ops

(defun wg-wtree-buffer-list (wtree)
  "Return a list of unique buffer names visible in WTREE."
  (flet ((rec (w) (if (wg-window-p w) (list (wg-aget w 'bname))
                    (mapcan #'rec (wg-wlist w)))))
    (remove-duplicates (rec wtree) :test #'equal)))

(defun wg-workgroup-buffer-list (workgroup)
  "Call `wg-wconfig-buffer-list' on WORKGROUP's working config."
  (wg-wtree-buffer-list (wg-wtree (wg-working-config workgroup))))

(defun wg-buffer-list ()
  "Call `wg-workgroup-buffer-list' on all workgroups in `wg-list'."
  (remove-duplicates
   (mapcan #'wg-workgroup-buffer-list (wg-list t))
   :test #'equal))

(defun wg-find-buffer (bname)
  "Return the first workgroup in which a buffer named BNAME is visible."
  (wg-get-some (wg (wg-list))
               (member bname (wg-workgroup-buffer-list wg))))


;;; mode-line

(defun wg-mode-line-string ()
  "Return the string to be displayed in the mode-line."
  (let ((cur (wg-current-workgroup t)))
    (cond (cur (wg-fontify " "
                 (:div wg-mode-line-left-brace)
                 (:mode (position cur (wg-list t)))
                 (:div wg-mode-line-divider)
                 (:mode (wg-name cur))
                 (:div wg-mode-line-right-brace)))
          (t   (wg-fontify " "
                 (:div wg-mode-line-left-brace)
                 (:mode "No workgroups")
                 (:div wg-mode-line-right-brace))))))

(defun wg-mode-line-add-display ()
  "Add Workgroups' mode-line format to `mode-line-format'."
  (if wg-mode-line-on
      (unless (assq 'wg-mode-line-on mode-line-format)
        (let ((format `(wg-mode-line-on (:eval (wg-mode-line-string))))
              (pos (1+ (position 'mode-line-position mode-line-format))))
          (set-default 'mode-line-format
                       (wg-insert-elt format mode-line-format pos))))))

(defun wg-mode-line-remove-display ()
  "Remove Workgroups' mode-line format from `mode-line-format'."
  (wg-awhen (assq 'wg-mode-line-on mode-line-format)
    (set-default 'mode-line-format (remove it mode-line-format))
    (force-mode-line-update)))


;;; minibuffer reading

(defun wg-completing-read (prompt choices &rest args)
  "Call `completing-read' or `ido-completing-read'."
  (apply (if (and (boundp 'ido-mode) ido-mode)
             #'ido-completing-read
           #'completing-read) prompt choices args))

(defun wg-read-workgroup (&optional noerror)
  "Read a workgroup with `wg-completing-read'."
  (wg-get-workgroup
   'name (wg-completing-read "Workgroup: " (wg-names))
   noerror))

(defun wg-read-buffer-name ()
  "Read and return a buffer-name from `wg-buffer-list'."
  (wg-completing-read "Workgroup buffers: " (wg-buffer-list)))

(defun wg-read-new-workgroup-name (&optional prompt)
  "Read a non-empty name string from the minibuffer."
  (wg-read-object
   (or prompt "Name: ")
   (lambda (obj) (and (stringp obj) (not (equal obj ""))))
   "Please enter a unique, non-empty name"))

(defun wg-read-workgroup-index ()
  "Prompt for the index of a workgroup."
  (let ((max (1- (length (wg-list)))))
    (wg-read-object
     (format "%s\n\nEnter [0-%d]: " (wg-disp) max)
     (lambda (obj) (and (integerp obj) (wg-within obj 0 max t)))
     (format "Please enter an integer [%d-%d]" 0 max)
     nil nil t)))


;;; messaging

(defun wg-msg (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS.
Also save the msg to `wg-last-message'."
  (setq wg-last-message (apply #'message format-string args)))

(defmacro wg-fontified-msg (&rest format)
  "`wg-fontify' FORMAT and call `wg-msg' on it."
  (declare (indent defun))
  `(wg-msg (wg-fontify ,@format)))


;;; command utils

(defun wg-arg (&optional reverse noerror)
  "Return a workgroup one way or another.
For use in interactive forms.  If `current-prefix-arg' is nil,
return the current workgroup.  Otherwise read a workgroup from
the minibuffer.  If REVERSE is non-nil, `current-prefix-arg's
begavior is reversed."
  (wg-list noerror)
  (if (if reverse (not current-prefix-arg) current-prefix-arg)
      (wg-read-workgroup noerror)
    (wg-current-workgroup noerror)))

(defun wg-add-to-kill-ring (config)
  "Add CONFIG to `wg-kill-ring'."
  (push config wg-kill-ring)
  (setq wg-kill-ring (wg-take wg-kill-ring wg-kill-ring-size)))

(defun wg-disp ()
  "Return the Workgroups list display string.
The string contains the names of all workgroups in `wg-list',
decorated with faces, dividers and strings identifying the
current and previous workgroups."
  (let ((wl    (wg-list t))
        (cur   (wg-current-workgroup  t))
        (prev  (wg-previous-workgroup t))
        (div   (wg-add-face :div wg-display-divider))
        (cld   wg-display-current-workgroup-left-decor)
        (crd   wg-display-current-workgroup-right-decor)
        (pld   wg-display-previous-workgroup-left-decor)
        (prd   wg-display-previous-workgroup-right-decor)
        (i     -1))
    (wg-fontify
      (:brace wg-display-left-brace)
      (if (not wl) (wg-fontify (:msg "No workgroups are defined"))
        (wg-doconcat (w wl div)
          (let ((str (format "%d: %s" (incf i) (wg-name w))))
            (cond ((eq w cur)
                   (wg-fontify (:cur (concat cld str crd))))
                  ((eq w prev)
                   (wg-fontify (:prev (concat pld str prd))))
                  (t (wg-fontify (:other str)))))))
      (:brace wg-display-right-brace))))

(defun wg-cyclic-nth-from-workgroup (&optional workgroup n)
  "Return the workgroup N places from WORKGROUP in `wg-list'."
  (wg-when-let ((wg (or workgroup (wg-current-workgroup t))))
    (wg-cyclic-nth-from-elt wg (wg-list) (or n 1))))


;;; commands

(defun wg-switch-to-workgroup (workgroup &optional base)
  "Switch to WORKGROUP.
BASE nil means restore WORKGROUP's working config.
BASE non-nil means restore WORKGROUP's base config."
  (interactive (list (wg-read-workgroup) current-prefix-arg))
  (wg-awhen (wg-current-workgroup t)
    (when (eq it workgroup) (error "Already on: %s" (wg-name it)))
    (wg-update-working-config it))
  (wg-restore-workgroup workgroup base)
  (wg-set-previous-workgroup (wg-current-workgroup t))
  (wg-set-current-workgroup workgroup)
  (run-hooks 'wg-switch-hook)
  (wg-fontified-msg (:cmd "Switched:  ") (wg-disp)))

(defun wg-create-workgroup (name)
  "Create and add a workgroup named NAME.
If workgroups already exist, create a blank workgroup.  If no
workgroups exist yet, create a workgroup from the current window
configuration."
  (interactive (list (wg-read-new-workgroup-name)))
  (let ((w (if (wg-current-workgroup t) (wg-make-blank-workgroup name)
             (wg-make-default-workgroup name))))
    (wg-check-and-add w)
    (wg-switch-to-workgroup w)
    (wg-fontified-msg (:cmd "Created: ") (:cur name) "  " (wg-disp))))

(defun wg-clone-workgroup (workgroup name)
  "Create and add a clone of WORKGROUP named NAME."
  (interactive (list (wg-arg) (wg-read-new-workgroup-name)))
  (let ((new (wg-make-workgroup nil name (wg-base-config workgroup))))
    (wg-check-and-add new)
    (wg-set-working-config new (wg-working-config workgroup))
    (wg-switch-to-workgroup new)
    (wg-fontified-msg
     (:cmd "Cloned: ") (:cur (wg-name workgroup))
     (:msg " to ") (:cur name) "  " (wg-disp))))

(defun wg-kill-workgroup (workgroup)
  "Kill WORKGROUP, saving its working config to the kill ring."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-working-config workgroup))
  (let ((to (or (wg-previous-workgroup t)
                (wg-cyclic-nth-from-workgroup workgroup))))
    (wg-delete workgroup)
    (if (eq to workgroup) (wg-restore-blank-wconfig)
      (wg-switch-to-workgroup to))
    (wg-fontified-msg
     (:cmd "Killed: ") (:cur (wg-name workgroup)) "  " (wg-disp))))

(defun wg-kill-ring-save-base-config (workgroup)
  "Save WORKGROUP's base config to `wg-kill-ring'."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-base-config workgroup))
  (wg-fontified-msg
   (:cmd "Saved: ") (:cur (wg-name workgroup))
   (:cur "'s ") (:msg "base config to the kill ring")))

(defun wg-kill-ring-save-working-config (workgroup)
  "Save WORKGROUP's working config to `wg-kill-ring'."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-working-config workgroup))
  (wg-fontified-msg
   (:cmd "Saved: ") (:cur (wg-name workgroup))
   (:cur "'s ") (:msg "working config to the kill ring")))

(defun wg-yank-config ()
  "Restore a wconfig from `wg-kill-ring'.
Successive yanks restore wconfigs sequentially from the kill
ring, starting at the front."
  (interactive)
  (unless wg-kill-ring (error "The kill-ring is empty"))
  (let ((pos (if (not (eq real-last-command 'wg-yank-config)) 0
               (mod (1+ (or (get 'wg-yank-config :position) 0))
                    (length wg-kill-ring)))))
    (put 'wg-yank-config :position pos)
    (wg-restore-wconfig (nth pos wg-kill-ring))
    (wg-fontified-msg (:cmd "Yanked: ") (:msg pos) "  " (wg-disp))))

(defun wg-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working config."
  (interactive (list (wg-arg)))
  (let ((bufs (save-window-excursion
                (wg-restore-workgroup workgroup)
                (mapcar #'window-buffer (window-list)))))
    (wg-kill-workgroup workgroup)
    (mapc #'kill-buffer bufs)
    (wg-fontified-msg
     (:cmd "Killed: ") (:cur (wg-name workgroup))
     (:msg " and its buffers ") "\n" (wg-disp))))

(defun wg-delete-other-workgroups (workgroup)
  "Delete all workgroups but WORKGROUP."
  (interactive (list (wg-arg)))
  (unless (or wg-no-confirm (y-or-n-p "Really delete all other workgroups? "))
    (error "Cancelled"))
  (let ((cur (wg-current-workgroup)))
    (mapc #'wg-delete (remove workgroup (wg-list)))
    (unless (eq workgroup cur) (wg-switch-to-workgroup workgroup))
    (wg-fontified-msg
     (:cmd "Deleted: ") (:msg "All workgroups but ")
     (:cur (wg-name workgroup)))))

(defun wg-update-workgroup (workgroup)
  "Set the base config of WORKGROUP to its working config in `selected-frame'."
  (interactive (list (wg-arg)))
  (wg-set-base-config workgroup (wg-working-config workgroup))
  (wg-fontified-msg
   (:cmd "Updated: ") (:cur (wg-name workgroup))))

(defun wg-update-all-workgroups ()
  "Update all workgroups' base configs.
Worgroups are updated with their working configs in the
`selected-frame'."
  (interactive)
  (mapc #'wg-update-workgroup (wg-list))
  (wg-fontified-msg (:cmd "Updated: ") (:msg "All")))

(defun wg-revert-workgroup (workgroup)
  "Set the working config of WORKGROUP to its base config in `selected-frame'."
  (interactive (list (wg-arg)))
  (wg-set-working-config
   workgroup (wg-base-config workgroup))
  (when (eq workgroup (wg-current-workgroup))
    (wg-restore-workgroup workgroup t))
  (wg-fontified-msg (:cmd "Reverted: ") (:cur (wg-name workgroup))))

(defun wg-revert-all-workgroups ()
  "Revert all workgroups to their base configs."
  (interactive)
  (mapc #'wg-revert-workgroup (wg-list))
  (wg-fontified-msg (:cmd "Reverted: ") (:msg "All")))

(defun wg-switch-to-index (n)
  "Switch to Nth workgroup in `wg-list'."
  (interactive (list (or current-prefix-arg (wg-read-workgroup-index))))
  (let ((wl (wg-list)))
    (wg-switch-to-workgroup
     (or (nth n wl) (error "There are only %d workgroups" (length wl))))))

;; Define wg-switch-to-index-[0-9]:
(macrolet
    ((defi (n)
       `(defun ,(intern (format "wg-switch-to-index-%d" n)) ()
          ,(format "Switch to the workgroup at index %d in the list." n)
          (interactive) (wg-switch-to-index ,n))))
  (defi 0) (defi 1) (defi 2) (defi 3) (defi 4)
  (defi 5) (defi 6) (defi 7) (defi 8) (defi 9))

(defun wg-switch-left (&optional workgroup n)
  "Switch to the workgroup left of WORKGROUP in `wg-list'."
  (interactive (list (wg-arg nil t) current-prefix-arg))
  (wg-switch-to-workgroup
   (or (wg-cyclic-nth-from-workgroup workgroup (or n -1))
       (car (wg-list)))))

(defun wg-switch-right (&optional workgroup n)
  "Switch to the workgroup right of WORKGROUP in `wg-list'."
  (interactive (list (wg-arg nil t) current-prefix-arg))
  (wg-switch-to-workgroup
   (or (wg-cyclic-nth-from-workgroup workgroup n)
       (car (wg-list)))))

(defun wg-switch-left-other-frame (&optional n)
  "Like `wg-switch-left', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-cyclic-nth-from-frame (or n 1))
    (wg-switch-left)))

(defun wg-switch-right-other-frame (&optional n)
  "Like `wg-switch-right', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-cyclic-nth-from-frame (or n -1))
    (wg-switch-right)))

(defun wg-switch-to-previous-workgroup ()
  "Switch to the previous workgroup."
  (interactive)
  (wg-switch-to-workgroup (wg-previous-workgroup)))

(defun wg-swap-workgroups ()
  "Swap the previous and current workgroups."
  (interactive)
  (wg-list-swap (wg-current-workgroup) (wg-previous-workgroup))
  (wg-fontified-msg (:cmd "Swapped ") (wg-disp)))

(defun wg-offset-left (workgroup &optional n)
  "Offset WORKGROUP leftward in `wg-list' cyclically."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-cyclic-offset-workgroup workgroup (or n -1))
  (wg-fontified-msg (:cmd "Offset left: ") (wg-disp)))

(defun wg-offset-right (workgroup &optional n)
  "Offset WORKGROUP rightward in `wg-list' cyclically."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-cyclic-offset-workgroup workgroup (or n 1))
  (wg-fontified-msg (:cmd "Offset right: ") (wg-disp)))

(defun wg-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list (wg-arg) (wg-read-new-workgroup-name "New name: ")))
  (let ((oldname (wg-name workgroup)))
    (wg-set-name workgroup newname)
    (wg-fontified-msg
     (:cmd "Renamed: ") (:cur oldname) (:msg " to ")
     (:cur (wg-name workgroup)))))

(defun wg-reset (&optional force)
  "Reset workgroups.
Deletes saved state in `wg-frame-table' and nulls out `wg-list',
`wg-file' and `wg-kill-ring'."
  (interactive "P")
  (unless (or force wg-no-confirm (y-or-n-p "Are you sure? "))
    (error "Canceled"))
  (clrhash wg-frame-table)
  (setq wg-list nil wg-file nil wg-dirty nil)
  (wg-fontified-msg (:cmd "Reset: ") (:msg "Workgroups")))


;;; file commands

(defun wg-save (file)
  "Save workgroups to FILE.
Called interactively with a prefix arg, or if `wg-file'
is nil, read a filename.  Otherwise use `wg-file'."
  (interactive
   (list (if (or current-prefix-arg (not (wg-file t)))
             (read-file-name "File: ") (wg-file))))
  (wg-write-sexp-to-file
   (cons wg-persisted-workgroups-tag (wg-list)) file)
  (setq wg-dirty nil wg-file file)
  (wg-fontified-msg (:cmd "Wrote: ") (:file file)))

(defun wg-load (file)
  "Load workgroups from FILE.
Called interactively with a prefix arg, and if `wg-file'
is non-nil, use `wg-file'. Otherwise read a filename."
  (interactive
   (list (if (and current-prefix-arg (wg-file t))
             (wg-file) (read-file-name "File: "))))
  (wg-dbind (tag . workgroups) (wg-read-sexp-from-file file)
    (unless (or (eq tag wg-persisted-workgroups-tag)
                ;; Added for compatibility with old save files.  This tag had to
                ;; be changed because it's formatted like a file-local variable,
                ;; causing workgroups-mode to toggle on or off when a file of
                ;; saved workgroups is visited (even though the symbol
                ;; `workgroups' denotes nothing in Workgroups except its
                ;; customization group -- yow!
                (eq tag '-*-workgroups-*-))
      (error "%S is not a workgroups file." file))
    (wg-reset t)
    (setq wg-list workgroups wg-file file))
  (when wg-switch-on-load
    (wg-awhen (wg-list t)
      (wg-switch-to-workgroup (car it))))
  (wg-fontified-msg (:cmd "Loaded: ") (:file file)))

(defun wg-find-file (file)
  "Create a new workgroup and find file FILE in it."
  (interactive "FFile: ")
  (wg-create-workgroup (file-name-nondirectory file))
  (find-file file))

(defun wg-find-file-read-only (file)
  "Create a new workgroup and find FILE read-only in it."
  (interactive "FFile: ")
  (wg-create-workgroup (file-name-nondirectory file))
  (find-file-read-only file))

(defun wg-get-by-buffer (buf)
  "Switch to the first workgroup in which BUF is visible."
  (interactive (list (wg-read-buffer-name)))
  (wg-aif (wg-find-buffer buf) (wg-switch-to-workgroup it)
    (error "No workgroup contains %S" buf)))

(defun wg-dired (dir &optional switches)
  "Create a workgroup and open DIR in dired with SWITCHES."
  (interactive (list (read-directory-name "Dired: ") current-prefix-arg))
  (wg-create-workgroup dir)
  (dired dir switches))

(defun wg-update-all-workgroups-and-save ()
  "Call `wg-update-all-workgroups', the `wg-save'.
Keep in mind that workgroups will be updated with their
working-config in the current frame."
  (interactive)
  (wg-update-all-workgroups)
  (call-interactively 'wg-save))


;;; mode-line commands

(defun wg-toggle-mode-line ()
  "Toggle Workgroups' mode-line display."
  (interactive)
  (setq wg-mode-line-on (not wg-mode-line-on))
  (force-mode-line-update)
  (wg-fontified-msg
   (:cmd "mode-line: ") (:msg (if wg-mode-line-on "on" "off"))))


;;; morph commands

(defun wg-toggle-morph ()
  "Toggle `wg-morph', Workgroups' morphing animation."
  (interactive)
  (setq wg-morph-on (not wg-morph-on))
  (wg-fontified-msg
   (:cmd "Morph: ") (:msg (if wg-morph-on "on" "off"))))


;;; Window movement commands

(defun wg-move-window-backward (offset)
  "Move `selected-window' backward by OFFSET in its wlist."
  (interactive (list (or current-prefix-arg -1)))
  (wg-restore-wconfig (wg-wconfig-move-window offset)))

(defun wg-move-window-forward (offset)
  "Move `selected-window' forward by OFFSET in its wlist."
  (interactive (list (or current-prefix-arg 1)))
  (wg-restore-wconfig (wg-wconfig-move-window offset)))

(defun wg-reverse-frame-horizontally ()
  "Reverse the order of all horizontally split wtrees."
  (interactive)
  (wg-restore-wconfig (wg-reverse-wconfig)))

(defun wg-reverse-frame-vertically ()
  "Reverse the order of all vertically split wtrees."
  (interactive)
  (wg-restore-wconfig (wg-reverse-wconfig t)))

(defun wg-reverse-frame-horizontally-and-vertically ()
  "Reverse the order of all wtrees."
  (interactive)
  (wg-restore-wconfig (wg-reverse-wconfig 'both)))


;;; echo commands

(defun wg-echo-current-workgroup ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (wg-fontified-msg
   (:cmd "Current: ") (:cur (wg-name (wg-current-workgroup)))))

(defun wg-echo-all-workgroups ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (wg-fontified-msg (:cmd "Workgroups: ") (wg-disp)))

(defun wg-echo-time ()
  "Echo the current time.  Optionally includes `battery' info."
  (interactive)
  (wg-msg ;; Pass through format to escape the % in `battery'
   "%s" (wg-fontify
          (:cmd "Current time: ")
          (:msg (format-time-string wg-time-format))
          (when (and wg-display-battery (fboundp 'battery))
            (wg-fontify "\n" (:cmd "Battery: ") (:msg (battery)))))))

(defun wg-echo-version ()
  "Echo Workgroups' current version number."
  (interactive)
  (wg-fontified-msg
   (:cmd "Workgroups version: ") (:msg wg-version)))

(defun wg-echo-last-message ()
  "Echo the last message Workgroups sent to the echo area.
The string is passed through a format arg to escape %'s."
  (interactive)
  (message "%s" wg-last-message))


;;; help

(defvar wg-help
  '("\\[wg-switch-to-workgroup]"
    "Switch to a workgroup"
    "\\[wg-create-workgroup]"
    "Create a new workgroup and switch to it"
    "\\[wg-clone-workgroup]"
    "Create a clone of the current workgroug and switch to it"
    "\\[wg-kill-workgroup]"
    "Kill a workgroup"
    "\\[wg-kill-ring-save-base-config]"
    "Save the current workgroup's base config to the kill ring"
    "\\[wg-kill-ring-save-working-config]"
    "Save the current workgroup's working config to the kill ring"
    "\\[wg-yank-config]"
    "Yank a config from the kill ring into the current frame"
    "\\[wg-kill-workgroup-and-buffers]"
    "Kill a workgroup and all buffers visible in it"
    "\\[wg-delete-other-workgroups]"
    "Delete all but the specified workgroup"
    "\\[wg-update-workgroup]"
    "Update a workgroup's base config with its working config"
    "\\[wg-update-all-workgroups]"
    "Update all workgroups' base configs with their working configs"
    "\\[wg-revert-workgroup]"
    "Revert a workgroup's working config to its base config"
    "\\[wg-revert-all-workgroups]"
    "Revert all workgroups' working configs to their base configs"
    "\\[wg-switch-to-index]"
    "Jump to a workgroup by its index in the workgroups list"
    "\\[wg-switch-to-index-0]"
    "Switch to the workgroup at index 0"
    "\\[wg-switch-to-index-1]"
    "Switch to the workgroup at index 1"
    "\\[wg-switch-to-index-2]"
    "Switch to the workgroup at index 2"
    "\\[wg-switch-to-index-3]"
    "Switch to the workgroup at index 3"
    "\\[wg-switch-to-index-4]"
    "Switch to the workgroup at index 4"
    "\\[wg-switch-to-index-5]"
    "Switch to the workgroup at index 5"
    "\\[wg-switch-to-index-6]"
    "Switch to the workgroup at index 6"
    "\\[wg-switch-to-index-7]"
    "Switch to the workgroup at index 7"
    "\\[wg-switch-to-index-8]"
    "Switch to the workgroup at index 8"
    "\\[wg-switch-to-index-9]"
    "Switch to the workgroup at index 9"
    "\\[wg-switch-left]"
    "Switch to the workgroup leftward cyclically in the workgroups list"
    "\\[wg-switch-right]"
    "Switch to the workgroup rightward cyclically in the workgroups list"
    "\\[wg-switch-left-other-frame]"
    "Like `wg-switch-left', but operates in the next frame"
    "\\[wg-switch-right-other-frame]"
    "Like `wg-switch-right', but operates in the next frame"
    "\\[wg-switch-to-previous-workgroup]"
    "Switch to the previously selected workgroup"
    "\\[wg-swap-workgroups]"
    "Swap the positions of the current and previous workgroups"
    "\\[wg-offset-left]"
    "Offset a workgroup's position leftward cyclically in the workgroups list"
    "\\[wg-offset-right]"
    "Offset a workgroup's position rightward cyclically in the workgroups list"
    "\\[wg-rename-workgroup]"
    "Rename a workgroup"
    "\\[wg-reset]"
    "Reset Workgroups' entire state."
    "\\[wg-save]"
    "Save the workgroup list to a file"
    "\\[wg-load]"
    "Load a workgroups list from a file"
    "\\[wg-find-file]"
    "Create a new blank workgroup and find a file in it"
    "\\[wg-find-file-read-only]"
    "Create a new blank workgroup and find a file read-only in it"
    "\\[wg-get-by-buffer]"
    "Switch to the workgroup and config in which the specified buffer is visible"
    "\\[wg-dired]"
    "Create a new blank workgroup and open a dired buffer in it"
    "\\[wg-move-window-backward]"
    "Move `selected-window' backward in its wlist"
    "\\[wg-move-window-forward]"
    "Move `selected-window' forward in its wlist"
    "\\[wg-reverse-frame-horizontally]"
    "Reverse the order of all horizontall window lists."
    "\\[wg-reverse-frame-vertically]"
    "Reverse the order of all vertical window lists."
    "\\[wg-reverse-frame-horizontally-and-vertically]"
    "Reverse the order of all window lists."
    "\\[wg-toggle-mode-line]"
    "Toggle Workgroups' mode-line display"
    "\\[wg-toggle-morph]"
    "Toggle the morph animation on any wconfig change"
    "\\[wg-echo-current-workgroup]"
    "Display the name of the current workgroup in the echo area"
    "\\[wg-echo-all-workgroups]"
    "Display the names of all workgroups in the echo area"
    "\\[wg-echo-time]"
    "Display the current time in the echo area"
    "\\[wg-echo-version]"
    "Display the current version of Workgroups in the echo area"
    "\\[wg-echo-last-message]"
    "Display the last message Workgroups sent to the echo area in the echo area."
    "\\[wg-help]"
    "Show this help message")
  "List of commands and their help messages. Used by `wg-help'.")

(defun wg-help ()
  "Display Workgroups' help buffer."
  (interactive)
  (with-output-to-temp-buffer "*workroups help*"
    (princ  "Workgroups' keybindings:\n\n")
    (dolist (elt (wg-partition wg-help 2))
      (wg-dbind (cmd help-string) elt
        (princ (format "%15s   %s\n"
                       (substitute-command-keys cmd)
                       help-string))))))


;;; keymap

(defvar wg-map
  (wg-fill-keymap (make-sparse-keymap)

                  ;; workgroup creation

                  "C-c"        'wg-create-workgroup
                  "c"          'wg-create-workgroup
                  "C"          'wg-clone-workgroup


                  ;; killing and yanking

                  "C-k"        'wg-kill-workgroup
                  "k"          'wg-kill-workgroup
                  "M-W"        'wg-kill-ring-save-base-config
                  "M-w"        'wg-kill-ring-save-working-config
                  "C-y"        'wg-yank-config
                  "y"          'wg-yank-config
                  "M-k"        'wg-kill-workgroup-and-buffers
                  "K"          'wg-delete-other-workgroups


                  ;; updating and reverting

                  "C-u"        'wg-update-workgroup
                  "u"          'wg-update-workgroup
                  "C-S-u"      'wg-update-all-workgroups
                  "U"          'wg-update-all-workgroups
                  "C-r"        'wg-revert-workgroup
                  "r"          'wg-revert-workgroup
                  "C-S-r"      'wg-revert-all-workgroups
                  "R"          'wg-revert-all-workgroups


                  ;; workgroup switching

                  "C-'"        'wg-switch-to-workgroup
                  "'"          'wg-switch-to-workgroup
                  "C-v"        'wg-switch-to-workgroup
                  "v"          'wg-switch-to-workgroup
                  "C-j"        'wg-switch-to-index
                  "j"          'wg-switch-to-index
                  "0"          'wg-switch-to-index-0
                  "1"          'wg-switch-to-index-1
                  "2"          'wg-switch-to-index-2
                  "3"          'wg-switch-to-index-3
                  "4"          'wg-switch-to-index-4
                  "5"          'wg-switch-to-index-5
                  "6"          'wg-switch-to-index-6
                  "7"          'wg-switch-to-index-7
                  "8"          'wg-switch-to-index-8
                  "9"          'wg-switch-to-index-9
                  "C-p"        'wg-switch-left
                  "p"          'wg-switch-left
                  "C-n"        'wg-switch-right
                  "n"          'wg-switch-right
                  "M-p"        'wg-switch-left-other-frame
                  "M-n"        'wg-switch-right-other-frame
                  "C-a"        'wg-switch-to-previous-workgroup
                  "a"          'wg-switch-to-previous-workgroup


                  ;; workgroup movement

                  "C-x"        'wg-swap-workgroups
                  "C-,"        'wg-offset-left
                  "C-."        'wg-offset-right


                  ;; file and buffer

                  "C-s"        'wg-save
                  "C-l"        'wg-load
                  "S"          'wg-update-all-workgroups-and-save
                  "C-f"        'wg-find-file
                  "S-C-f"      'wg-find-file-read-only
                  "C-b"        'wg-get-by-buffer
                  "b"          'wg-get-by-buffer
                  "d"          'wg-dired


                  ;; window moving and frame reversal

                  "<"          'wg-move-window-backward
                  ">"          'wg-move-window-forward
                  "|"          'wg-reverse-frame-horizontally
                  "-"          'wg-reverse-frame-vertically
                  "+"          'wg-reverse-frame-horizontally-and-vertically


                  ;; toggling

                  "C-i"        'wg-toggle-mode-line
                  "C-w"        'wg-toggle-morph


                  ;; echoing

                  "S-C-e"      'wg-echo-current-workgroup
                  "E"          'wg-echo-current-workgroup
                  "C-e"        'wg-echo-all-workgroups
                  "e"          'wg-echo-all-workgroups
                  "C-t"        'wg-echo-time
                  "t"          'wg-echo-time
                  "V"          'wg-echo-version
                  "C-m"        'wg-echo-last-message
                  "m"          'wg-echo-last-message


                  ;; misc

                  "A"          'wg-rename-workgroup
                  "!"          'wg-reset
                  "?"          'wg-help

                  )
  "Workgroups' keymap.")


;;; mode definition

(defun wg-unset-prefix-key ()
  "Restore the original definition of `wg-prefix-key'."
  (wg-awhen (get 'wg-prefix-key :original)
    (wg-dbind (key . def) it
      (when (eq wg-map (lookup-key global-map key))
        (global-set-key key def))
      (put 'wg-prefix-key :original nil))))

(defun wg-set-prefix-key ()
  "Define `wg-prefix-key' as `wg-map' in `global-map'."
  (wg-unset-prefix-key)
  (let ((key wg-prefix-key))
    (put 'wg-prefix-key :original (cons key (lookup-key global-map key)))
    (global-set-key key wg-map)))

(defun wg-query-for-save ()
  "Query for save when `wg-dirty' is non-nil."
  (or (not wg-dirty)
      (not (y-or-n-p "Save modified workgroups? "))
      (call-interactively 'wg-save)
      t))

(defun wg-emacs-exit-query ()
  "Conditionally call `wg-query-for-save'.
Call `wg-query-for-save' when `wg-query-for-save-on-emacs-exit'
is non-nil."
  (or (not wg-query-for-save-on-emacs-exit)
      (wg-query-for-save)))

(defun wg-workgroups-mode-exit-query ()
  "Conditionally call `wg-query-for-save'.
Call `wg-query-for-save' when
`wg-query-for-save-on-workgroups-mode-exit' is non-nil."
  (or (not wg-query-for-save-on-workgroups-mode-exit)
      (wg-query-for-save)))

(define-minor-mode workgroups-mode
  "This turns `workgroups-mode' on and off.
If ARG is null, toggle `workgroups-mode'.
If ARG is an integer greater than zero, turn on `workgroups-mode'.
If ARG is an integer less one, turn off `workgroups-mode'.
If ARG is anything else, turn on `workgroups-mode'."
  :lighter     " wg"
  :init-value  nil
  :global      t
  :group       'workgroups
  (cond (workgroups-mode
         (add-hook 'kill-emacs-query-functions 'wg-emacs-exit-query)
         (add-hook 'delete-frame-functions 'wg-delete-frame)
         (wg-set-prefix-key)
         (wg-mode-line-add-display))
        (t
         (wg-workgroups-mode-exit-query)
         (remove-hook 'kill-emacs-query-functions 'wg-emacs-exit-query)
         (remove-hook 'delete-frame-functions 'wg-delete-frame)
         (wg-unset-prefix-key)
         (wg-mode-line-remove-display))))


;;; provide

(provide 'workgroups)


;;; workgroups.el ends here

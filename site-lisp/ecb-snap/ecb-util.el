;;; ecb-util.el --- utility functions for ECB

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2000

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-util.el,v 1.136 2006/03/10 15:40:35 berndl Exp $

;;; Commentary:
;;
;; Contains misc utility functions for ECB.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile (require 'cl))

;;; ----- Silentcomp-Defs ----------------------------------

;; XEmacs
(silentcomp-defun button-release-event-p)
(silentcomp-defun button-press-event-p)
(silentcomp-defun event-key)
(silentcomp-defun frame-property)
(silentcomp-defun point-at-bol)
(silentcomp-defun point-at-eol)
(silentcomp-defun frame-parameter)
(silentcomp-defun line-beginning-position)
(silentcomp-defun line-end-position)
(silentcomp-defun window-pixel-edges)
(silentcomp-defun noninteractive)
;; Emacs
(silentcomp-defun event-basic-type)
(silentcomp-defvar noninteractive)
(silentcomp-defun window-edges)
(silentcomp-defun buffer-local-value)
(silentcomp-defun posn-point)
(silentcomp-defun posn-window)
(silentcomp-defun event-start)
;; XEmacs
(silentcomp-defun make-dialog-box)
(silentcomp-defun display-message)
(silentcomp-defun clear-message)
(silentcomp-defun make-event)
;; Emacs
(silentcomp-defvar message-log-max)
(silentcomp-defvar message-truncate-lines)
(silentcomp-defun x-popup-dialog)
(silentcomp-defun display-images-p)
(silentcomp-defvar tar-subfile-mode)
(silentcomp-defvar archive-subfile-mode)

;; timer stuff for Xemacs
(silentcomp-defun delete-itimer)
(silentcomp-defun start-itimer)
;; thing stuff for XEmacs
(silentcomp-defun thing-boundaries)
(silentcomp-defun thing-symbol)

(silentcomp-defun custom-file)

;;; ----- Some constants -----------------------------------

;;;###autoload
;;(defconst ecb-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(defconst ecb-running-xemacs (featurep 'xemacs))

(defconst ecb-running-unsupported-emacs (condition-case nil
                                            (<= emacs-major-version 20)
                                          (error t))
  "True if running XEmacs or Emacs < 21.")

(defconst ecb-running-version-22 (and (not ecb-running-unsupported-emacs)
                                      (>= emacs-major-version 22))
  "True if running \(X)Emacs >= version 22")

(defconst ecb-temp-dir
  (file-name-as-directory
   (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP")
       (if (eq system-type 'windows-nt) "c:/temp/" "/tmp/")))
  "A directory where ECB can store temporary files.")

(defconst ecb-ecb-dir
  (expand-file-name (file-name-directory (locate-library "ecb"))))
(defconst ecb-semantic-dir
  (if (locate-library "semantic")
      (expand-file-name (file-name-directory (locate-library "semantic")))))

(defconst ecb-ecb-parent-dir (expand-file-name (concat ecb-ecb-dir "../")))

;; we assume that current loaded ECB is a regular XEmacs-package if and only
;; if `ecb-ecb-dir' contains the files "_pkg.el" and "auto-autoloads.el" and
;; we are running XEmacs
(defconst ecb-regular-xemacs-package-p
  (and ecb-running-xemacs
       (file-exists-p (expand-file-name (concat ecb-ecb-dir "_pkg.el")))
       (file-exists-p (expand-file-name (concat ecb-ecb-dir "auto-autoloads.el")))))
(defconst ecb-semantic-regular-xemacs-package-p
  (and ecb-running-xemacs
       ecb-semantic-dir
       (file-exists-p (expand-file-name (concat ecb-semantic-dir "_pkg.el")))
       (file-exists-p (expand-file-name (concat ecb-semantic-dir "auto-autoloads.el")))))

(defconst ecb-images-can-be-used
  (and (or (fboundp 'defimage)
           (fboundp 'make-image-specifier))
       (if (fboundp 'display-images-p)
           (display-images-p)
         window-system)))

;;; ----- Tracing ------------------------------------------

;; we use the trace.el library!

;;; ----- Compatibility between GNU Emacs and XEmacs -------

;; miscellaneous differences

(defmacro when-ecb-running-xemacs (&rest body)
  "Evaluates BODY when `ecb-running-xemacs' is true. Use this macro when you
want the BODY being parsed by semantic!. If not use the variable
`ecb-running-xemacs'."
  `(when ecb-running-xemacs
     ,@body))

(defmacro when-ecb-running-emacs (&rest body)
  "Evaluates BODY when `ecb-running-xemacs' is false. Use this macro when you
want the BODY being parsed by semantic!. If not use the form
\(unless ecb-running-xemacs)."
  `(unless ecb-running-xemacs
     ,@body))

;; I do not want all this compatibitly stuff being parsed by semantic,
;; therefore i do not use the macro `when-ecb-running-xemacs'!

(when ecb-running-xemacs
  (defun ecb-event-to-key (event)
    (typecase event
      (button-release-event 'mouse-release)
      (button-press-event 'mouse-press)
      (otherwise
       ;; the ignore-errors is a little hack because i don't know all
       ;; events of XEmacs so sometimes event-key produces a
       ;; wrong-type-argument error.
       (ignore-errors (event-key event)))))
  (defun ecb-facep (face)
    (memq face (face-list)))
  (defun ecb-noninteractive ()
    "Return non-nil if running non-interactively, i.e. in batch mode."
    (noninteractive))
  (defun ecb-subst-char-in-string (fromchar tochar string &optional inplace)
    "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((i (length string))
          (newstr (if inplace string (copy-sequence string))))
      (while (> i 0)
        (setq i (1- i))
        (if (eq (aref newstr i) fromchar)
            (aset newstr i tochar)))
      newstr))
  (defun ecb-derived-mode-p (&rest modes)
    "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
    (let ((parent major-mode))
      (while (and (not (memq parent modes))
                  (setq parent (get parent 'derived-mode-parent))))
      parent))
  (defalias 'ecb-frame-parameter 'frame-property)
  (defalias 'ecb-line-beginning-pos 'point-at-bol)
  (defalias 'ecb-line-end-pos 'point-at-eol)
  (defalias 'ecb-event-window 'event-window)
  (defalias 'ecb-event-point 'event-point)
  (defalias 'ecb-event-buffer 'event-buffer)
  (defalias 'ecb-window-full-width 'window-full-width)
  (defalias 'ecb-window-full-height 'window-height)
  (defalias 'ecb-window-display-height 'window-displayed-height)
  (defun ecb-frame-char-width (&optional frame)
    (/ (frame-pixel-width frame) (frame-width frame)))
  (defun ecb-frame-char-height (&optional frame)
    (/ (frame-pixel-height frame) (frame-height frame)))
  (defun ecb-window-edges (&optional window)
    (let ((pix-edges (window-pixel-edges window)))
      (list (/ (nth 0 pix-edges) (ecb-frame-char-width))
            (/ (nth 1 pix-edges) (ecb-frame-char-height))
            (/ (nth 2 pix-edges) (ecb-frame-char-width))
            (/ (nth 3 pix-edges) (ecb-frame-char-height))))))

(unless ecb-running-xemacs
  (defun ecb-event-to-key (event)
    (let ((type (event-basic-type event)))
      (case type
        ((mouse-1 mouse-2 mouse-3) 'mouse-release)
        ((down-mouse-1 down-mouse-2 down-mouse-3) 'mouse-press)
        (otherwise (event-basic-type event)))))
  (defalias 'ecb-facep 'facep)
  (defun ecb-noninteractive ()
    "Return non-nil if running non-interactively, i.e. in batch mode."
    noninteractive)
  (defalias 'ecb-subst-char-in-string 'subst-char-in-string)
  (defalias 'ecb-derived-mode-p 'derived-mode-p)
  (defalias 'ecb-frame-parameter 'frame-parameter)
  (defalias 'ecb-line-beginning-pos 'line-beginning-position)
  (defalias 'ecb-line-end-pos 'line-end-position)
  (defun ecb-event-window (event)
    (posn-window (event-start event)))
  (defun ecb-event-point (event)
    (posn-point (event-start event)))
  (defun ecb-event-buffer (event)
    (window-buffer (ecb-event-window event)))
  (defun ecb-window-full-width (&optional window)
    (let ((edges (window-edges window)))
      (- (nth 2 edges) (nth 0 edges))))
  (defalias 'ecb-window-display-height 'window-text-height)
  (defalias 'ecb-window-full-height 'window-height)
  (defalias 'ecb-frame-char-width 'frame-char-width)
  (defalias 'ecb-frame-char-height 'frame-char-height)
  (defalias 'ecb-window-edges 'window-edges))

;; thing at point stuff

(if (not ecb-running-xemacs)
    (progn
      (require 'thingatpt)
      (defalias 'ecb-thing-at-point 'thing-at-point)
      (defalias 'ecb-end-of-thing 'end-of-thing)
      (defalias 'ecb-beginning-of-thing 'beginning-of-thing))
  ;; Xemacs
  (require 'thing)
  (defun ecb-thing-at-point (thing)
    (let ((bounds (if (eq 'symbol thing)
                      (thing-symbol (point))
                    (thing-boundaries (point)))))
      (buffer-substring (car bounds) (cdr bounds))))
  (defun ecb-end-of-thing (thing)
    (goto-char (cdr (if (eq 'symbol thing)
                        (thing-symbol (point))
                      (thing-boundaries (point))))))
  (defun ecb-beginning-of-thing (thing)
    (goto-char (car (if (eq 'symbol thing)
                        (thing-symbol (point))
                      (thing-boundaries (point)))))))

;; overlay- and extend-stuff

(if (not ecb-running-xemacs)
    (progn
      (defalias 'ecb-make-overlay            'make-overlay)
      (defalias 'ecb-overlay-put             'overlay-put)
      (defalias 'ecb-overlay-move            'move-overlay)
      (defalias 'ecb-overlay-delete          'delete-overlay)
      (defalias 'ecb-overlay-kill            'delete-overlay))
  ;; XEmacs
  (defalias 'ecb-make-overlay            'make-extent)
  (defalias 'ecb-overlay-put             'set-extent-property)
  (defalias 'ecb-overlay-move            'set-extent-endpoints)
  (defalias 'ecb-overlay-delete          'detach-extent)
  (defalias 'ecb-overlay-kill            'delete-extent))

;; timer stuff

(if (not ecb-running-xemacs)
    (progn
      (defalias 'ecb-run-with-timer 'run-with-timer)
      (defalias 'ecb-run-with-idle-timer 'run-with-idle-timer)
      (defalias 'ecb-cancel-timer 'cancel-timer))
  ;; XEmacs
  (defun ecb-run-with-timer (secs repeat function &rest args)
    (start-itimer "ecb-timer" function secs repeat
                  nil (if args t nil) args))
  (defun ecb-run-with-idle-timer (secs repeat function &rest args)
    (start-itimer "ecb-idle-timer"
                  function secs (if repeat secs nil)
                  t (if args t nil) args))
  (defun ecb-cancel-timer (timer)
    (delete-itimer timer))
  )

;;; ----- advice stuff -------------------------------------

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Attention. Current mechanism of
;; (de)activating the basic advices and the intelligent window advices of
;; `ecb-advice-window-functions' independent from each other works only if
;; both sets of functions are disjunct (because ad-activate always activates
;; ALL advices of a function if they are not disabled!)
(defconst ecb-basic-adviced-functions (if ecb-running-xemacs
                                          '((delete-frame . around)
                                            (compilation-set-window-height . around)
                                            (shrink-window-if-larger-than-buffer . around)
                                            (show-temp-buffer-in-current-frame . around)
                                            (pop-to-buffer . around)
                                            (current-window-configuration . after)
                                            (set-window-configuration . after)
                                            (scroll-other-window . around)
                                            (custom-save-all . around)
                                            (count-windows . around)
                                            (scroll-all-mode . after))
                                        '((delete-frame . around)
                                          (compilation-set-window-height . around)
                                          (resize-temp-buffer-window . around)
                                          (shrink-window-if-larger-than-buffer . around)
                                          (mouse-drag-vertical-line . around)
                                          (mouse-drag-mode-line . around)
                                          (pop-to-buffer . around)
                                          (current-window-configuration . after)
                                          (set-window-configuration . after)
                                          (enlarge-window . around)
                                          (shrink-window . around)
                                          (tmm-prompt . around)
                                          (scroll-other-window . around)
                                          (custom-save-all . around)
                                          (count-windows . around)
                                          (scroll-all-mode . after)))
  "These functions are always adviced if ECB is active. Each element of the
list is a cons-cell where the car is the function-symbol and the cdr the
advice-class \(before, around or after). If a function should be adviced with
more than one class \(e.g. with a before and an after-advice) then for every
class a cons must be added to this list.

Every basic advice of ECB must be registered in this constant but can be
implemented in another file!")

(defun ecb-enable-advices (advice-list)
  "Enable all advices of ADVICE-LIST. ADVICE-LIST must have the format of
`ecb-basic-adviced-functions'."
  (dolist (elem advice-list)
    (ad-enable-advice (car elem) (cdr elem) 'ecb)
    (ad-activate (car elem))))
  
(defun ecb-disable-advices (advice-list)
  "Disable all advices of ADVICE-LIST. ADVICE-LIST must have the format of
`ecb-basic-adviced-functions'."
  (dolist (elem advice-list)
    (ad-disable-advice (car elem) (cdr elem) 'ecb)
    (ad-activate (car elem))))
  

(defmacro ecb-with-original-basic-functions (&rest body)
  "Evaluates BODY with all adviced basic-functions of ECB deactivated \(means
with their original definition). Restores always the previous state of the ECB
adviced basic-functions, means after evaluating BODY it activates the advices
of exactly the functions in `ecb-basic-adviced-functions'!"
  `(unwind-protect
       (progn
         (ecb-disable-advices ecb-basic-adviced-functions)
         ,@body)
     (ecb-enable-advices ecb-basic-adviced-functions)))

(defun ecb-enable-ecb-advice (function-symbol advice-type arg)
  "If ARG is greater or equal zero then enable the adviced version of
FUNCTION-SYMBOL. The advice must be of type of the ADVICE-TYPE which can be
'around, 'before or 'after."
  (if (< arg 0)
      (progn
        (ad-disable-advice function-symbol advice-type 'ecb)
        (ad-activate function-symbol))
    (ad-enable-advice function-symbol advice-type 'ecb)
    (ad-activate function-symbol)))

(defmacro ecb-with-ecb-advice (function-symbol advice-type &rest body)
  "Evaluates BODY with the adviced version of FUNCTION-SYMBOL. The advice must
be of type of the ADVICE-TYPE which can be 'around, 'before or 'after. Such an
advice has to ensure that it behaves as its original version when called for
another frame than the `ecb-frame'."
  `(unwind-protect
       (progn
         (ecb-enable-ecb-advice ,function-symbol ,advice-type 1)
         ,@body)
     (ecb-enable-ecb-advice ,function-symbol ,advice-type -1)))

(put 'ecb-with-ecb-advice 'lisp-indent-function 2)


;;; ----- Customize stuff ----------------------------------

(defun ecb-custom-file ()
  "Filename of that file which is used by \(X)Emacs to store the
customize-options. If no custom-file can be computed or if Emacs reports an
error \(e.g. GNU Emacs complains when calling `custom-file' and Emacs has been
started with -q) nil is returned."
  (if ecb-running-xemacs
      custom-file
    (require 'cus-edit)
    (ignore-errors (custom-file))))

(defadvice custom-save-all (around ecb)
  "Save the customized options completely in the background, i.e. the
file-buffer where the value is saved \(see option `custom-file') is not parsed
by semantic and also killed afterwards."
  (if ecb-minor-mode
      (let (;; XEmacs 21.4 does not set this so we do it here, to ensure that
            ;; the custom-file is loadede in an emacs-lisp-mode buffer, s.b.
            (default-major-mode 'emacs-lisp-mode)
            (ecb-window-sync nil)
            (kill-buffer-hook nil)
            ;; we prevent parsing the custom-file
            (semantic-before-toplevel-bovination-hook (lambda ()
                                                        nil))
            (semantic--before-fetch-tags-hook (lambda ()
                                                nil))
            (semantic-after-toplevel-cache-change-hook nil)
            (semantic-after-partial-cache-change-hook nil))
        ;; Klaus Berndl <klaus.berndl@sdm.de>: we must ensure that the
        ;; current-buffer has a lisp major-mode when the kernel of
        ;; `custom-save-all' is called because cause of a bug (IMHO) in the
        ;; `custom-save-delete' of GNU Emacs (which loads the file returned by
        ;; `custom-file' with `default-major-mode' set to nil which in turn
        ;; causes that new buffer will get the major-mode of the
        ;; current-buffer) the file `custom-file' will get the major-mode of
        ;; the current-buffer. So when the current-buffer has for example
        ;; major-mode `c++-mode' then the file `custom-file' will be loaded
        ;; into a buffer with major-mode c++-mode. The function
        ;; `custom-save-delete' then parses this buffer with (forward-sexp
        ;; (buffer-size)) which of course fails because forward-sexp tries to
        ;; parse the custom-file (which is an emacs-lisp-file) as a c++-file
        ;; with c++-paren-syntax.
        ;; Solution: Ensure that the buffer *scratch* is current when calling
        ;; custom-save-all so we have surely a lispy-buffer and therefore we
        ;; can be sure that custom-file is loaded as lispy-buffer.
        (save-excursion
          (set-buffer (get-buffer-create "*scratch*"))
          ;; now we do the standard task
          ad-do-it)
        ;; now we have to kill the custom-file buffer otherwise semantic would
        ;; parse the buffer of custom-file and the method-buffer would be
        ;; updated with the contents of custom-file which is definitely not
        ;; desired.
        (ignore-errors
          (kill-buffer (find-file-noselect (ecb-custom-file)))))
    ad-do-it))

(defun ecb-option-get-value (option &optional type)
  "Return the value of a customizable ECB-option OPTION with TYPE, where TYPE
can either be 'standard-value \(the default-value of the defcustom) or
'saved-value \(the value stored persistent by the user via customize) or
'customized-value \(the value set but not saved in the customize buffer).
If TYPE is nil then the most recent set value is returned, means it
tries the customized-value, then the saved-value and then the standard-value
in exactly this sequence."
  (let ((val (car (if type
                      (get option type)
                    (or (get option 'customized-value)
                        (get option 'saved-value)
                        (get option 'standard-value))))))
    (cond ((not (listp val)) val)
          ((equal 'quote (car val)) (car (cdr val)))
;;          (t (car val)))))
          (t (eval val)))))

;;; ----- Assoc helpers ------------------------------------

(defun ecb-remove-assoc (key list)
  (delete nil
          (mapcar (function (lambda (elem)
                              (if (equal (car elem) key)
                                  nil
                                elem)))
                  list)))


(defun ecb-add-assoc (key-value list)
  (cons key-value list))

(defun ecb-find-assoc-value (key list)
  (cdr (assoc key list)))

(defun ecb-find-assoc (key list)
  (assoc key list))

;;; ----- Some function from cl ----------------------------

(defun ecb-filter (seq pred)
  "Filter out those elements of SEQUENCE for which PREDICATE returns nil."
  (let ((res))
    (dolist (elem seq)
      (if (if pred (funcall pred elem) elem)
          (setq res (append res (list elem)))))
    res))

(defun ecb-some (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of any element of SEQ or SEQs.
If so, return the true (non-nil) value returned by PREDICATE."
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-some
	(apply 'map nil
	       (function (lambda (&rest cl-x)
			   (let ((cl-res (apply cl-pred cl-x)))
			     (if cl-res (throw 'cl-some cl-res)))))
	       cl-seq cl-rest) nil)
    (let ((cl-x nil))
      (while (and cl-seq (not (setq cl-x (funcall cl-pred (pop cl-seq))))))
      cl-x)))

(defun ecb-copy-list (list)
  "Return a copy of a LIST, which may be a dotted list.
The elements of the list are not copied, just the list structure itself."
  (if (fboundp 'copy-sequence)
      (copy-sequence list)
    (if (consp list)
        (let ((res nil))
          (while (consp list) (push (pop list) res))
          (prog1 (nreverse res) (setcdr res list)))
      (car list))))

(defun ecb-set-difference (list1 list2)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2."
  (if (or (null list1) (null list2)) list1
    (let ((res nil))
      (while list1
        (or (if (numberp (car list1))
                (apply 'ecb-member (car list1) list2)
              (memq (car list1) list2))
            (push (car list1) res))
        (pop list1))
      res)))

(defun ecb-member (item list &optional test-fcn)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM. Comparison is done via `equal'
unless TEST-FCN is not nil: In this case TEST-FCN will be used to compare ITEM
with the elements of LIST. If TEST-FCN is `eq' then `memq' is called for
optimization."
  (if test-fcn
      (if (eq test-fcn 'eq)
          ;; some optimization
          (memq item list)
        (progn
          (while (and list (not (funcall test-fcn item (car list))))
            (setq list (cdr list)))
          list))
    (member item list)))

(defun ecb-position (seq elem &optional test-fcn)
  "Return the position of ELEM within SEQ counting from 0. Comparison is done
with `equal' unless TEST-FCN is not nil: In this case TEST-FCN will be used to
compare ITEM with the elements of SEQ."
  (if (listp seq)
      (let ((pos (- (length seq) (length (ecb-member elem seq test-fcn)))))
        (if (= pos (length seq))
            nil
          pos))
    (catch 'found
      (dotimes (i (length seq))
        (if (funcall (or test-fcn 'equal) elem (aref seq i))
            (throw 'found i)))
      nil)))

(defun ecb-set-elt (seq n val)
  "Set VAL as new N-th element of SEQ. SEQ can be any sequence. SEQ will be
changed because this is desctructive function. SEQ is returned."
  (if (listp seq)
      (setcar (nthcdr n seq) val)
    (aset seq n val))
  seq)

(defun ecb-remove-elt (seq n)
  "Remove N-th element from SEQ. SEQ can be any sequence. SEQ will be
changed because this is desctructive function. SEQ is returned."
  (delq 'ecb-util-remove-marker (ecb-set-elt seq n 'ecb-util-remove-marker)))

(defun ecb-replace-first-occurence (seq old-elem new-elem)
  "Replace in SEQ the first occurence of OLD-ELEM with NEW-ELEM. Comparison is
done by `equal'. This is desctructive function. SEQ is returned."
  (let ((pos (ecb-position seq old-elem)))
    (if pos
        (ecb-set-elt seq pos new-elem)))
  seq)

(defun ecb-replace-all-occurences (seq old-elem new-elem)
  "Replace in SEQ all occurences of OLD-ELEM with NEW-ELEM. Comparison is
done by `equal'. This is desctructive function. SEQ is returned."
  (while (ecb-position seq old-elem)
    (setq seq (ecb-replace-first-occurence seq old-elem new-elem)))
  seq)

(defun ecb-remove-first-occurence-from-list (list elem)
  "Replace first occurence of ELEM from LIST. Comparison is done by `equal'.
This is desctructive function. LIST is returned."
  (delq 'ecb-util-remove-marker
        (ecb-replace-first-occurence list elem 'ecb-util-remove-marker)))

(defun ecb-remove-all-occurences-from-list (list elem)
  "Replace all occurences of ELEM from LIST. Comparison is done by `equal'.
This is desctructive function. LIST is returned."
  (delq 'ecb-util-remove-marker
        (progn          
          (while (ecb-position list elem)
            (setq list (ecb-replace-first-occurence list elem
                                                    'ecb-util-remove-marker)))
          list)))

(defun ecb-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (typecase seq
        (list (if (> start 0) (setq seq (nthcdr start seq)))
              (if end
                  (let ((res nil))
                    (while (>= (setq end (1- end)) start)
                      (push (pop seq) res))
                    (nreverse res))
                (copy-sequence seq)))
        (otherwise (or end (setq end (or len (length seq))))
                   (let ((res (make-vector (max (- end start) 0) nil))
                         (i 0))
                     (while (< start end)
                       (aset res i (aref seq start))
                       (setq i (1+ i) start (1+ start)))
                     res))))))

(defun ecb-concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCES.
TYPE can be 'string, 'vector or 'list."
  (case type
    (vector (apply 'vconcat seqs))
    (string (apply 'concat seqs))
    (list (apply 'append (append seqs '(nil))))
    (otherwise (ecb-error "Not a sequence type name: %s" type))))

(defun ecb-rotate (seq start-elem)
  "Rotate SEQ so START-ELEM is the new first element of SEQ. SEQ is an
arbitrary sequence. Example: \(ecb-rotate '\(a b c d e f) 'c) results in \(c d
e f a b). If START-ELEM is not contained in SEQ then nil is returned."
  (let ((start-pos (ecb-position seq start-elem)))
    (when start-pos
      (ecb-concatenate (typecase seq
                         (list 'list)
                         (string 'string)
                         (vector 'vector))
                       (ecb-subseq seq start-pos)
                       (ecb-subseq seq 0 start-pos)))))

(defun ecb-last (seq)
  "Return the last elem of the sequence SEQ."
  (if (listp seq)
      (car (last seq))
    (if (> (length seq) 0)
        (aref seq (1- (length seq)))
      nil)))

(defun ecb-first (seq)
  "Return the first elem of the sequence SEQ."
  (if (listp seq)
      (car seq)
    (if (> (length seq) 0)
        (aref seq 0)
      nil)))
  

(defun ecb-next-listelem (list elem &optional nth-next)
  "Return that element of LIST which follows directly ELEM when ELEM is an
element of LIST. If ELEM is the last element of LIST then return the first
element of LIST. If ELEM is not an element of LIST nil is returned. Elements
are compared with `equal'.

If NTH-NEXT is an integer then the NTH-NEXT element of LIST in the meaning
described above is returned, i.e. the algorithm above is applied NTH-NEXT
times. Example: Suppose LIST = '\(a b c d), ELEM is 'c and NTH-NEXT = 3 then
'b is returned - same result for NTH-NEXT = 7, 11... It works also for
negative integers, so when NTH-NEXT is -1 in the example above then 'b is
returned."
  (let ((elem-pos (ecb-position list elem))
        (next (or nth-next 1)))
    (and elem-pos
         (nth (mod (+ elem-pos next)
                   (length list))
              list))))

;; Maybe we should enhance this docstring ;-)
(defun ecb-member-of-symbol/value-list (value list &optional elem-accessor
                                              return-accessor)
  "Returns not nil when VALUE is a member of that list which is build from
LIST by using the symbol-value if a list-member is a symbol and otherwise the
list-member itself. If a member then the matching elem of LIST is returned.
But if ELEM-ACCESSOR is a function then it is used to get that part of a elem
of LIST for which the rule above should be applied. If RETURN-ACCESSOR is a
function then it is used to get that part of that list-elem which is equal
according to the rules above."
  (let ((elem-acc (or elem-accessor 'identity))
        (return-acc (or return-accessor 'identity)))
    (catch 'exit
      (dolist (elem list)
        (let ((case-fold-search t)
              (e (funcall elem-acc elem)))
          (if (equal value (if (symbolp e)
                               (symbol-value e)
                             e))
              (throw 'exit (funcall return-acc elem)))
          nil)))))

;; tests

;; (ecb-member-of-symbol/value-list ecb-directories-buffer-name
;;                                  ecb-tree-do-not-leave-window-after-select--internal)

;; (ecb-member-of-symbol/value-list ecb-directories-buffer-name
;;                                  (cdr '("adgasgd" .
;;                                         ((ecb-directories-buffer-name . "dir")
;;                                          (ecb-sources-buffer-name . "sou")
;;                                          (ecb-history-buffer-name . "hist"))))
;;                                  'car
;;                                  'cdr)

;;; ----- Some regexp stuff  -------------------------------

(defsubst ecb-match-regexp-list (str regexp-list &optional elem-accessor
                                     return-accessor)
  "Return not nil if STR matches one of the regexps in REGEXP-LIST. If
ELEM-ACCESSOR is a function then it is used to get the regexp from the
processed elem of REGEXP-LIST. If nil the elem itself is used. If
RETURN-ACCESSOR is a function then it is used to get the object to return from
the matching elem. If nil then the matching elem itself is returned."
  (let ((elem-acc (or elem-accessor 'identity))
        (return-acc (or return-accessor 'identity)))
    (catch 'exit
      (dolist (elem regexp-list)
        (let ((case-fold-search t))
          (save-match-data
            (if (string-match (funcall elem-acc elem) str)
                (throw 'exit (funcall return-acc elem))))
          nil)))))

;;; ----- Multicache ---------------------------------------

;; internal functions
(defsubst ecb-multicache-init (cache-var)
  "Initialize the ecb-multicache of CACHE-VAR. If CACHE-VAR contains already
a valid cache then nothing is done otherwise a new cache is created."
  (or (ecb-multicache-p cache-var)
      (set cache-var (make-hash-table :size (get cache-var 'ecb-multicache-size)
                                      :test (get cache-var 'ecb-multicache-test)))))

(defun ecb-multicache-add-empty-key (cache-var key)
  "Checks if KEY is already cached in the cache of CACHE-VAR. If yes nothing
is done otherwise a new cache-element with empty subcaches is added to the
cache. All subcaches defined via `defecb-multicache' are created with a
value nil. CACHE-VAR has to be a symbol for which an assoc cache has been
defined with `defecb-multicache'!"
  (ecb-multicache-init cache-var)
  (or (gethash key (symbol-value cache-var))
      ;; now we add as value an assoc-list with an element for each registered
      ;; subcache-element
      (puthash key (mapcar (function (lambda (sc)
                                       (cons sc nil)))
                           (get cache-var
                                'ecb-multicache-subcache-list))
               (symbol-value cache-var))))

(defun ecb-multicache-get-subcache (cache-var key subcache)
  "Return that cons-cell which is associated with KEY in the cache of
CACHE-VAR and which has the symbol SUBCACHE as its car. The cdr of this
cons-cell is the currently stored SUBCACHE-value for KEY. If KEY is not cached
then nil is returned."
  (ecb-multicache-init cache-var)
  (let ((hash-val (gethash key (symbol-value cache-var))))
    (and hash-val
         (assoc subcache hash-val))))

;; public interface for the multi-cache

(defmacro defecb-multicache (name size test subcache docstring)
  "Defines NAME as variable and makes it an ecb-multicache.
This means that for each cache-item of the cache NAME informations can be
associated to different subcaches. SUBCACHE is either a symbol or a list of
symbols. For each symbol in SUBCACHE a subcache is reserved in the cache NAME.

Such a cache is especially senseful if different informations should be
associated to one key.

SIZE is a hint as to how many elements will be put in the cache. If SIZE is
nil then the default is 100. If the cache exceeds SIZE it will be increased
automatically.

TEST must be a symbol that specifies how to compare keys. If TEST is nil then
the default is `equal'.

After defining the cache with this macro the cache can be used immediately\;
there is no need for special initialization. The following functions are
available for setting and accessing values in such a cache:

  `ecb-multicache-put-value'
  `ecb-multicache-apply-to-value'
  `ecb-multicache-get-value'
  `ecb-multicache-mapsubcache'
  `ecb-multicache-clear-value'
  `ecb-multicache-clear-subcache'
  `ecb-multicache-remove'
  `ecb-multicache-clear'
  `ecb-multicache-print-subcache'
  `ecb-multicache-p'

The lookup in this multi-cache is really fast because the time required is
essentially _independent_ of how many elements are stored in the cache."
  `(progn
     (eval-and-compile
       (defvar ,name nil ,docstring))
     (unless (get ',name 'ecb-multicache-p)
       (setq ,name nil)
       (put ',name 'ecb-multicache-subcache-list
            (if (listp ,subcache)
                ,subcache
              (list ,subcache)))
       (put ',name 'ecb-multicache-p t)
       (put ',name 'ecb-multicache-size ,(or size 100))
       (put ',name 'ecb-multicache-test ,(or test (quote 'equal)))
       )))
(put 'defecb-multicache 'lisp-indent-function 4)

;; (insert (pp (macroexpand '(defecb-multicache klaus nil 'equal '(A B C) "docstring"))))



(defun ecb-multicache-p (cache-var)
  "Return not nil if the value of CACHE-VAR is a cache defined with
`defecb-multicache'."
  (and (hash-table-p (symbol-value cache-var))
       (get cache-var 'ecb-multicache-p)))

(defun ecb-multicache-get-value (cache-var key subcache)
  "Return the currently associated value for KEY in the subcache SUBCACHE of
the cache of CACHE-VAR. CACHE-VAR has to be a symbol for which an assoc cache
has been defined with `defecb-multicache'!

Be aware that the semantic of nil is not unique because nil can have the
following meanings:
- There is no cached item with KEY at all
- There is an item with KEY in the cache but there is no assigned value for
  SUBCACHE.
- nil has been set as value for KEY and SUBCACHE \(via
  `ecb-multicache-put-value' or `ecb-multicache-apply-to-value') - but this
  is not recommended, see `ecb-multicache-apply-to-value'."
  (cdr (ecb-multicache-get-subcache cache-var key subcache)))

(defun ecb-multicache-get-values (cache-var key &optional subcache-list)
  "Return an assoc-list with the subcaches listed in SUBCACHE-LIST. If
SUBCACHE-LIST is nil then all currently registered subcaches of CACHE-VAR are
returned. The result is an assoc-list where each element is a cons-cell:
- car: subcache-symbol.
- cdr: The currenty cached value for the subcache in the car.

So apply `assoc' and `cdr' to the result of this function.to get the value of
a certain subcache.

This function is useful when the values of more than one subcache for a key are
needed at the same time, i.e. with one cache-lookup."
  (ecb-multicache-init cache-var)
  (let ((cache-val (gethash key (symbol-value cache-var))))
    (when cache-val
      (if (null subcache-list)
          cache-val
        (mapcar (function (lambda (s)
                            (assoc s cache-val)))
                subcache-list)))))

(defun ecb-multicache-apply-to-value (cache-var key subcache apply-fcn
                                                &optional only-if-key-exist)
  "Apply the function APPLY-FCN to the old SUBCACHE-value of the cached item
with key KEY. APPLY-FCN is called with the old SUBCACHE-value as argument and
should return the new value which is then set as new SUBCACHE-value of the
cached-item. If optional argument ONLY-IF-KEY-EXIST is not nil then nothing
will be done if no cached item with key KEY exists. Otherwise a new item with
KEY will be added to the cache and APPLY-FCN will be called with nil.
CACHE-VAR has to be a symbol for which an assoc cache has been defined with
`defecb-multicache'!

With this function an already cached SUBCACHE-value for KEY can be evaluated
and then modified with only one cache-lookup because APPLY-FCN gets the
old-value as argument and has to return the new value which is then set as new
SUBCACHE-value of the cached item. This is more efficient than a call-sequence
of `ecb-multicache-get-value' \(to get the old-value) and then
`ecb-multicache-put-value' to set a new value.

It is recommended that APPLY-FCN doesn't return nil \(unless the SUBCACHE for
KEY should be cleared within APPLY-FCN) because then this will be set as new
value and then the returned value of next call to `ecb-multicache-get-value'
can have an ambiguous semantic - see documentation of
`ecb-multicache-get-value'. nil should be reserved to indicate that either no
item with KEY is cached or that no value has been put for SUBCACHE."
  (let ((subcache-conscell
         (or (ecb-multicache-get-subcache cache-var key subcache)
             ;; key is currently not cached
             (unless only-if-key-exist
               (ecb-multicache-add-empty-key cache-var key)
               (ecb-multicache-get-subcache cache-var key subcache)))))
    (when subcache-conscell
      (setcdr subcache-conscell
              (funcall apply-fcn (cdr subcache-conscell))))))
        
(defun ecb-multicache-put-value (cache-var key subcache value)
  "Put VALUE as SUBCACHE-value of the cached item with key KEY. If there is
already a value for this subcache and key then it will be replaced with VALUE.
CACHE-VAR has to be a symbol for which an assoc cache has been defined with
`defecb-multicache'!

Return VALUE.

It is recommended not to put nil as value - see
`ecb-multicache-apply-to-value' for an explanation. If the SUBCACHE for KEY
should be cleared use `ecb-multicache-clear-value'."
  (ecb-multicache-apply-to-value cache-var key subcache
                                 (function (lambda (old-val)
                                             value))))

(defun ecb-multicache-clear-value (cache-var key subcache)
  "Put nil as value of the cached item with key KEY under the subcache
SUBCACHE. This clears in fact the subcache SUBCACHE for a cached item with key
KEY. CACHE-VAR has to be a symbol for which an assoc cache has been defined
with `defecb-multicache'!"
  (ecb-multicache-put-value cache-var key subcache nil))

(defun ecb-multicache-remove (cache-var key)
  "Remove the cache item with key KEY from the cache of CACHE-VAR. CACHE-VAR
has to be a symbol for which an assoc cache has been defined with
`defecb-multicache'!"
  (ecb-multicache-init cache-var)
  (remhash key (symbol-value cache-var)))

(defun ecb-multicache-mapsubcache (cache-var subcache mapfcn)
  "Iterate over all item of the cache of CACHE-VAR and call the function
MAPFCN for each item for the subcache SUBCACHE. MAPFCN is called with two
arguments, the key and the SUBCACHE-value of the currently processed
cache-item. The SUBCACHE-value of this cache-item will be set to the
return-value of MAPFCN. So if MAPFCN is not intended to change the
SUBCACHE-value it should return the value of its second argument! CACHE-VAR
has to be a symbol for which an assoc cache has been defined with
`defecb-multicache'!"
  (ecb-multicache-init cache-var)
  (maphash (function (lambda (key value)
                       (let ((cache (assoc subcache value)))
                         (and cache
                              (setcdr cache
                                      (funcall mapfcn
                                               key (cdr cache)))))))
           (symbol-value cache-var)))

(defun ecb-multicache-clear-subcache (cache-var subcache)
  "Put nil as SUBCACHE-value for each cached item. This clears in fact the
whole SUBCACHE. CACHE-VAR has to be a symbol for which an assoc cache has been
defined with `defecb-multicache'!"
  (ecb-multicache-mapsubcache cache-var subcache
                              (function (lambda (key value)
                                          nil))))

(defun ecb-multicache-clear (cache-var)
  "Clears the whole cache of CACHE-VAR, i.e. remove all items. CACHE-VAR has
to be a symbol for which an assoc cache has been defined with
`defecb-multicache'!"
  (ecb-multicache-init cache-var)
  (clrhash (symbol-value cache-var)))

(defun ecb-multicache-print-subcache (cache-var subcache &optional no-nil-value)
  "Print the contents of SUBCACHE of the cache of CACHE-VAR in another window
in a special buffer. This is mostly for debugging the cache-contents.
CACHE-VAR has to be a symbol for which an assoc cache has been defined with
`defecb-multicache'!

The output has the following form:

Key: <the key of a cached element>
     Value: <the associated value in the subcache SUBCACHE>
Key: <the key of a cached element>
     Value: <the associated value in the subcache SUBCACHE>
...
Key: <the key of a cached element>
     Value: <the associated value in the subcache SUBCACHE>

If NO-NIL-VALUE is not nil then Keys with a SUBCACHE-value nil will be
excluded from the output."
  (let ((dump-buffer-name (format "*ecb-multicache - subcache: %s*"
                                  subcache))
        (key-str "Key:")
        (value-str "Value:")
        ;; Because XEmacs is not able to get a face-attributes-plist as value
        ;; for the special property 'face we have to create two temporary
        ;; faces here :-(
	(key-face (copy-face 'default 'ecb-multicache-print-key-face))
	(value-str-face (copy-face 'italic
                                   'ecb-multicache-print-value-str-face)))
    (set-face-foreground key-face "blue")
    (set-face-foreground value-str-face "forest green")    
    (put-text-property 0 (length key-str) 'face 'bold key-str)
    (put-text-property 0 (length value-str) 'face value-str-face value-str)
    (save-selected-window
      (set-buffer (get-buffer-create dump-buffer-name))
      (erase-buffer)
      (ecb-multicache-mapsubcache
       cache-var subcache
       (function (lambda (key value)
                   ;; if key is a string we colorize it blue but we must du
                   ;; this with a copy of key because otherwise we would
                   ;; colorize the key-object itself which maybe is not what
                   ;; we want if key is displayed somewhere else (e.g. in a
                   ;; tree-buffer).
                   (let ((key-cp (and (stringp key)
                                      (concat key))))
                     (and key-cp (put-text-property 0 (length key-cp)
                                                    'face key-face key-cp))
                     (unless (and no-nil-value (null value))
                       (insert (concat key-str " "
                                       (if key-cp
                                           key-cp
                                         (format "%s" key))
                                       "\n     "
                                       value-str " "
                                       (format "%s" value)
                                       "\n")))
                     value))))
      (switch-to-buffer-other-window (get-buffer-create dump-buffer-name))
      (goto-char (point-min)))))
  

;;; ----- User-interaction ---------------------------------

(defun ecb-confirm (text)
  (yes-or-no-p text))

;; stolen from query.el and slightly enhanced
;; This is for a small number of choices each of them a short string
(defun ecb-query-string (prompt choices &optional other-prompt)
  "Prints PROMPT and returns a string which must be one of CHOICES.
CHOICES is either a list of strings whereas the first choice is the default
\(which is returned if the user simply types RET) or nil \(then only a simple
RET quits the query and returns nil). If OTHER-PROMPT is not nil and a string
then the choice \"other\" is added to CHOICES and after selecting this choice
the user is prompted with OTHER-PROMPT to insert any arbitrary string."
  (let* ((new-choices (if other-prompt
                          ;; Emacs 20.X add-to-list can not append at the end
                          (append choices (list "other"))
                        choices))
         (default (car new-choices))
         answer)
    (setq prompt (concat prompt
                         " ["
                         (if new-choices
                             (mapconcat (function (lambda (x) x))
                                        new-choices ", ")
                           "RET")
                         "] "))
    (setq new-choices (nconc (mapcar (function (lambda (x) (list x t)))
                                     new-choices)
                             '('("" t))))
    (setq answer (completing-read prompt new-choices nil t))
    (cond ((ecb-string= answer "")
           (setq answer default))
          ((ecb-string= answer "other")
           (setq answer (read-string (concat other-prompt ": ")))))
    answer))

;; This is for any number of string-choices without any length restriction -
;; see also `ecb-query-string'.
(defun ecb-offer-choices (prompt choices)
  "Prints PROMPT and returns a string which must be one of CHOICES.
CHOICES is a list of strings whereas the first choice is the default. All
choices are immediately displayed as if completion does it so a selection can
be made either with the mouse or with the keyboard."
  (let* ((minibuffer-setup-hook
          (append minibuffer-setup-hook
                  (list (lambda ()
                          (with-output-to-temp-buffer "*Completions*"
                            (display-completion-list (all-completions "" minibuffer-completion-table)))))))
         (completion-list (mapcar (function (lambda (x) (list x t)))
                                  choices))
         (answer (completing-read prompt
                                  completion-list
                                  nil t
                                  (try-completion "" completion-list))))
    (if (ecb-string= answer "")
        (car choices)
      answer)))


;; ecb-offer-choices-1 and ecb-offer-choices-2 are two other approaches for
;; ecb-offer-choices - but IMHO not as good and clean as the current one and
;; therefore not used in ECB
;; (defun ecb-offer-choices-1 (prompt choices)
;;   "Prints PROMPT and returns a string which must be one of CHOICES.
;; CHOICES is a list of strings whereas the first choice is the default. All
;; choices are immediately displayed as if completion does it so a selection can
;; be made either with the mouse or with the keyboard."
;;   (let* ((minibuffer-setup-hook
;;           (append minibuffer-setup-hook
;;                   '(minibuffer-complete
;;                     minibuffer-complete
;;                     minibuffer-complete)))
;;          (answer (completing-read
;;                   prompt
;;                   (mapcar (function (lambda (x) (list x t)))
;;                           choices)
;;                   nil t)))
;;     (if (ecb-string= answer "")
;;         (car choices)
;;       answer)))

;; (defun ecb-offer-choices-2 (prompt choices)
;;   "Prints PROMPT and returns a string which must be one of CHOICES.
;; CHOICES is a list of strings whereas the first choice is the default. All
;; choices are immediately displayed as if completion does it so a selection can
;; be made either with the mouse or with the keyboard."
;;   ;; First we create a TAB-event
;;   (let ((event (if ecb-running-xemacs
;;                    (make-event 'key-press '(key tab))
;;                  9)))
;;     ;; With these 3 TAB-events we ensure that
;;     ;; 1. The longest possible common substring is display in the minibuffer
;;     ;; 2. All possible completions are displayed
;;     (dotimes (i 3)
;;       (setq unread-command-events (cons event unread-command-events))))
;;   (let ((answer (completing-read
;;                  prompt
;;                  (mapcar (function (lambda (x) (list x t)))
;;                          choices)
;;                  nil t)))
;;     (if (ecb-string= answer "")
;;         (car choices)
;;       answer)))

(defun ecb-read-number (prompt &optional init-value)
  "Ask in the minibuffer for a number with prompt-string PROMPT. Optional
INIT-VALUE can be either a number or a string-representation of a number."
  (let ((init (typecase init-value
                (number (number-to-string init-value))
                (string
                 (if (ecb-string= init-value "0")
                     init-value
                   (if (not (= 0 (string-to-number init-value)))
                       init-value
                     (ecb-error "ecb-read-number: init-value not a valid number!"))))
                (otherwise nil)))
        result)
    (while (progn
             (setq result (read-string prompt init))
             (not (or (ecb-string= "0" result)
                      (not (= 0 (string-to-number result)))))))
    (string-to-number result)))

(defun ecb-message-box (message-str &optional title-text button-text)
  "Display a message-box with message MESSAGE-STR and title TITLE-TEXT if
TITLE-TEXT is not nil - otherwise \"Message-box\" is used as title. The title
gets always the prefix \"ECB - \". Second optional argument BUTTON-TEXT
specifies the text of the message-box button; if nil then \"OK\" is used.

Remark: BUTTON-TEXT is currently only used with XEmacs. With GNU Emacs the
message itself is the button because GNU Emacs currently does not support
dialog-boxes very well.

If `window-system' is nil then a simple message is displayed in the echo-area."
  (let ((button (if (stringp button-text)
                    button-text
                  "OK"))
        (title (concat "ECB"
                       (if (stringp title-text)
                           (concat " - " title-text)
                         " Message"))))
    (if window-system
        (progn
          (if ecb-running-xemacs
              (make-dialog-box 'question
                               :title title
                               :modal t
                               :question message-str
                               :buttons (list (vector button '(identity nil) t)))
            (x-popup-dialog t (list title (cons message-str t))))
          t)
      (message (concat title " " message-str)))))

;; some first approaches to display informations in a temp-window

;; (defvar ecb-window-config-before-msg-display nil)

;; (defun ecb-display-temp-message-1 (msg-title msg-content)
;;   (require 'wid-edit)
;;   (setq ecb-window-config-before-msg-display
;;         (ecb-current-window-configuration))
;;   (with-current-buffer (get-buffer-create msg-title)
;;     (switch-to-buffer-other-window (current-buffer))
;;     (kill-all-local-variables)
;;     (let ((inhibit-read-only t))
;;       (erase-buffer))
;;     (widget-insert msg-content)
;;     (widget-insert "\n\n")
;;     ;; Insert the Save button
;;     (widget-create 'push-button
;;                    :button-keymap ecb-upgrade-button-keymap ; XEmacs
;;                    :keymap ecb-upgrade-button-keymap ; Emacs
;;                    :notify (lambda (&rest ignore)
;;                              (when ecb-window-config-before-msg-display
;;                                (ignore-errors
;;                                  (ecb-set-window-configuration
;;                                   ecb-window-config-before-msg-display))
;;                                (setq ecb-window-config-before-msg-display nil)))
;;                    "OK")
;;     (widget-setup)
;;     (goto-char (point-min))))


;; (defun ecb-display-temp-message-2 (msg-title msg-content)
;;   (require 'wid-edit)
;;   (setq ecb-window-config-before-msg-display
;;         (ecb-current-window-configuration))
;;   (with-output-to-temp-buffer msg-title
;;     (widget-insert msg-content)
;;     (widget-insert "\n\n")
;;     ;; Insert the Save button
;;     (widget-create 'push-button
;;                    :button-keymap ecb-upgrade-button-keymap ; XEmacs
;;                    :keymap ecb-upgrade-button-keymap ; Emacs
;;                    :notify (lambda (&rest ignore)
;;                              (when ecb-window-config-before-msg-display
;;                                (ignore-errors
;;                                  (ecb-set-window-configuration
;;                                   ecb-window-config-before-msg-display))
;;                                (setq ecb-window-config-before-msg-display nil)))
;;                    "OK")
;;     (widget-setup)
;;     (goto-char (point-min))))

;; (defvar ecb-user-information-msg-buffer nil)

;; (defun ecb-display-temp-message (msg-content)
;;   (require 'wid-edit)
;;   (progn
;;     (setq ecb-user-information-msg-buffer
;;           (get-buffer-create "*ECB User-Information*"))
;;     (cond
;;      ((not (get-buffer-window ecb-user-information-msg-buffer))
;;       (let ((split-window-keep-point nil)
;;             (window-min-height 2))
;;         ;; maybe leave two lines for our window because of the normal
;;         ;; `raised' modeline in Emacs 21
;;         ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: adjust this for Xemacs!
;;         (select-window
;;          (split-window-vertically
;;           (if (and (fboundp 'face-attr-construct)
;;                    (plist-get (face-attr-construct 'modeline) :box))
;;               -3 -2)))
;;         (switch-to-buffer ecb-user-information-msg-buffer)))
;;      ((not (eq (current-buffer) ecb-user-information-msg-buffer))
;;       (select-window (get-buffer-window ecb-user-information-msg-buffer))))
;;     ;; insert now the msg-content
;;     (let ((inhibit-read-only t))
;;       (erase-buffer))
;;     (widget-insert msg-content)
;;     (widget-insert "\n\n")
;;     ;; Insert the Save button
;;     (widget-create 'push-button
;;                    :button-keymap ecb-upgrade-button-keymap ; XEmacs
;;                    :keymap ecb-upgrade-button-keymap ; Emacs
;;                    :notify (lambda (&rest ignore)
;;                              (set-buffer ecb-user-information-msg-buffer)
;;                              (condition-case nil
;;                                  (while (get-buffer-window ecb-user-information-msg-buffer)
;;                                    (delete-window (get-buffer-window ecb-user-information-msg-buffer)))
;;                                (error nil))
;;                              (kill-buffer ecb-user-information-msg-buffer)
;;                              (setq ecb-user-information-msg-buffer nil))
                             
;;                    "OK")
;;     (widget-setup)
;;     ;; (setq buffer-read-only t)
;;     (message "Click [OK] or hit q for restoring previous window-layout.")))

;; ----- Information-display - errors, warnings, infos ----

(defun ecb-nolog-message (&rest args)
  "Works exactly like `message' but does not log the message"
  (let ((msg (cond ((or (null args)
                        (null (car args)))
                    nil)
                   ((null (cdr args))
                    (car args))
                   (t
                    (apply 'format args)))))
    ;; Now message is either nil or the formated string.
    (if ecb-running-xemacs
        ;; XEmacs way of preventing log messages.
        (if msg
            (display-message 'no-log msg)
          (clear-message 'no-log))
      ;; Emacs way of preventing log messages.
      (let ((message-log-max nil)
            (message-truncate-lines nil))
        (if msg
            (message "%s" msg)
          (message nil))))
    msg))

(defun ecb-error (&rest args)
  "Signals an error but prevents it from entering the debugger. This is
useful if an error-message should be signaled to the user and evaluating
should stopped but no debugging is senseful."
  (let ((debug-on-error nil))
    (error (concat "ECB " ecb-version " - Error: "
                   (apply 'format args)))))

(defun ecb-warning (&rest args)
  "Displays a warning."
  (message (concat "ECB " ecb-version " - Warning: " (apply 'format args))))

(defun ecb-info-message (&rest args)
  "Displays an information."
  (message (concat "ECB " ecb-version " - Info: " (apply 'format args))))

;;; ----- Text and string-stuff ----------------------------

(defun ecb-merge-face-into-text (text face)
  "Merge FACE to the already precolored TEXT so the values of all
face-attributes of FACE take effect and but the values of all face-attributes
of TEXT which are not set by FACE are preserved."
  (if (null face)
      text
    (if ecb-running-xemacs
        (put-text-property 0 (length text) 'face
                           (let* ((current-face (get-text-property 0
                                                                   'face
                                                                   text))
                                  (cf
                                   (typecase current-face
                                     (ecb-face (list current-face))
                                     (list current-face)
                                     (otherwise nil)))
                                  (nf
                                   (typecase face
                                     (ecb-face (list face))
                                     (list face)
                                     (otherwise nil))))
                             ;; we must add the new-face in front of
                             ;; current-face to get the right merge!
                             (append nf cf))
                           text)
      (alter-text-property 0 (length text) 'face
                           (lambda (current-face)
                             (let ((cf
                                    (typecase current-face
                                      (ecb-face (list current-face))
                                      (list current-face)
                                      (otherwise nil)))
                                   (nf
                                    (typecase face
                                      (ecb-face (list face))
                                      (list face)
                                      (otherwise nil))))
                               ;; we must add the new-face in front of
                               ;; current-face to get the right merge!
                               (append nf cf)))
                           text))
    text))

(if (fboundp 'compare-strings)
    (defalias 'ecb-compare-strings 'compare-strings)
  (defun ecb-compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
    "Compare the contents of two strings.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings.

Case is significant in this comparison if IGNORE-CASE is nil.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning."
    (or start1 (setq start1 0))
    (or start2 (setq start2 0))
    (setq end1 (if end1
                   (min end1 (length str1))
                 (length str1)))
    (setq end2 (if end2
                   (min end2 (length str2))
                 (length str2)))
    (let ((i1 start1)
          (i2 start2)
          result c1 c2)
      (while (and (not result) (< i1 end1) (< i2 end2))
        (setq c1 (aref str1 i1)
              c2 (aref str2 i2)
              i1 (1+ i1)
              i2 (1+ i2))
        (if ignore-case
            (setq c1 (upcase c1)
                  c2 (upcase c2)))
        (setq result (cond ((< c1 c2) (- i1))
                           ((> c1 c2) i1))))
      (or result
          (cond ((< i1 end1) (1+ (- i1 start1)))
                ((< i2 end2) (1- (- start1 i1)))
                (t)))
      )))

(defsubst ecb-string= (str1 str2 &optional ignore-case)
  (let ((s1 (or (and (stringp str1) str1) (symbol-name str1)))
        (s2 (or (and (stringp str2) str2) (symbol-name str2))))
    (eq (ecb-compare-strings s1 nil nil s2 nil nil ignore-case) t)))

(defsubst ecb-string< (str1 str2 &optional ignore-case)
  (let ((s1 (or (and (stringp str1) str1) (symbol-name str1)))
        (s2 (or (and (stringp str2) str2) (symbol-name str2)))
        (result nil))
    (setq result (ecb-compare-strings s1 nil nil s2 nil nil ignore-case))
    (and (numberp result) (< result 0))))

(defun ecb-excessive-trim (str)
  "Return a string where all double-and-more whitespaces in STR are replaced
with a single space-character."
  (let ((s str))
    (save-match-data
      (while (string-match "[ \t][ \t]+" s)
        (setq s (concat (substring s 0 (match-beginning 0))
                        " "
                        (substring s (match-end 0))))))
    s))

;; Klaus Berndl <klaus.berndl@sdm.de>: we have to take account that GNU Emacs
;; > 21.3 has changed its split-string function! For the new split-string is
;; (cdr (split-string ...)) not nil (at least in our context below), for GNU
;; Emacs <= 21.3 nil!
(defun ecb-left-trim (str)
  "Return a string stripped of all leading whitespaces of STR."
  (let ((split-result (split-string str "^[\n\t ]*")))
    (or (or (and (cdr split-result) ;; GNU Emacs > 21.3
                 (car (cdr split-result)))
            (car split-result))
        "")))

(defun ecb-right-trim (str)
  "Return a string stripped of all trailing whitespaces of STR."
  (or (car (split-string str "[\n\t ]*$")) ""))

(defun ecb-trim (str)
  "Applies `ecb-right-trim' and `ecb-left-trim' to STR."
  (ecb-left-trim (ecb-right-trim str)))

(defun ecb-full-trim (str)
  "Applies `ecb-trim' and `ecb-middle-trim' to STR."
  (ecb-excessive-trim (ecb-trim str)))

(defun ecb-fit-str-to-width (str width from)
  "If STR is longer than WIDTH then fit it to WIDTH by stripping from left or
right \(depends on FROM which can be 'left or 'right) and prepend \(rsp.
append) \"...\" to signalize that the string is stripped. If WIDTH >= length
of STR the always STR is returned. If either WIDTH or length of STR is < 5
then an empty string is returned because stripping makes no sense here."
  (let ((len-str (length str)))
    (if (>= width len-str)
        str
      (if (or (< len-str 5) ;; we want at least two characters visible of str
              (< width 5))
          ""
        (if (equal from 'left)
            (concat "..." (substring str (* -1 (- width 3))))
          (concat (substring str 0 (- width 3)) "..."))))))

;;; ----- Number-stuff -------------------------------------

(defun ecb-normalize-number (value &optional ref-value)
  "Normalize VALUE in the following manner and return:
* VALUE > -1.0 and < +1.0 and REF-VALUE a number: `floor' of VALUE * REF-VALUE
* all other cases: `floor' of VALUE"
  (floor (if (and (< value 1.0)
                  (> value -1.0)
                  (numberp ref-value))
             (* ref-value value)
           value)))


;;; ----- Working-display ----------------------------------

;; code for a working display - complete stolen from the semantic-package.
;; ECB has thrown away all code which is not needed by ECB
;; The original code is written by Eric M. Ludlam <zappo@gnu.org>

;; we need this here so we are independent of the semantic-package so we can
;; download eieio and semantic even if the user has not installed any version
;; of semantic.

;; Variables used in stages
(defvar ecb-working-message nil
  "Message stored when in a status loop.")
(defvar ecb-working-donestring nil
  "Done string stored when in a status loop.")
(defvar ecb-working-ref1 nil
  "A reference number used in a status loop.")
(defvar ecb-working-last-percent 0
  "A reference number used in a status loop.")

(defun ecb-working-frame-animation-display (length number frames)
  "Manage a simple frame-based animation for working functions.
LENGTH is the number of characters left.  NUMBER is a passed in
number (which happens to be ignored.).  While coders pass t into
NUMBER, functions using this should convert NUMBER into a vector
describing how to render the done message.
Argument FRAMES are the frames used in the animation."
  (typecase number
    (vector
     (let ((zone (- (length (aref frames 0)) (length (aref number 0))
                    (length (aref number 1)))))
       (if (< (length ecb-working-donestring) zone)
           (concat " " (aref number 0)
                   (make-string
                    (ceiling (/ (- (float zone)
                                   (length ecb-working-donestring)) 2)) ? )
                   ecb-working-donestring
                   (make-string
                    (floor (/ (- (float zone)
                                 (length ecb-working-donestring)) 2)) ? )
                   (aref number 1))
         (concat " " (aref frames (% ecb-working-ref1 (length frames)))
                 " " ecb-working-donestring))))
    (otherwise
     (concat " " (aref frames (% ecb-working-ref1 (length frames)))))))

(defvar ecb-working-celeron-strings
  [ "[O     ]" "[oO    ]" "[-oO   ]" "[ -oO  ]" "[  -oO ]" "[   -oO]"
    "[    -O]" "[     O]" "[    Oo]" "[   Oo-]"  "[  Oo- ]" "[ Oo-  ]"
    "[Oo-   ]" "[O-    ]"]
  "Strings representing a silly celeron.")

(defun ecb-working-celeron-display (length number)
  "Return a string displaying a celeron as things happen.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (case number
    ((t)
     (ecb-working-frame-animation-display length [ "[" "]" ]
					  ecb-working-celeron-strings))
    ;; All the % signs because it then gets passed to message.
    (otherwise
     (ecb-working-frame-animation-display length number
                                          ecb-working-celeron-strings))))



(defun ecb-working-dynamic-status (&optional number)
  "show the status. If NUMBER is nil, then increment a local NUMBER from 0
with each call. If it is a number or float, use it as the raw percentile."
  (let* ((n (or number ecb-working-ref1))
         (m1 (funcall 'format ecb-working-message))
         (m2 (ecb-working-celeron-display (length m1) n)))
    (ecb-nolog-message "%s%s" m1 m2)
    (setq ecb-working-ref1 (1+ ecb-working-ref1))))

(defmacro ecb-working-status-timeout (timeout message donestr &rest forms)
  "Contain a block of code during which working status is shown.
The code may call `sit-for' or `accept-process-output', so a timer
is needed to update the message.
TIMEOUT is the length of time to wait between message updates.
MESSAGE is the message string to use and DONESTR is the completed text
to use when the functions `ecb-working-status' is called from FORMS."
  (let ((current-message (make-symbol "ecb-working-current-message")))
    `(let* ((,current-message (current-message))
            (ecb-working-message ,message)
            (ecb-working-donestring ,donestr)
            (ecb-working-ref1 0)
            (time ,timeout)
            (ecb-working-timer
             (ecb-run-with-timer time time 'ecb-working-dynamic-status)))
       (unwind-protect
           (progn ,@forms)
         (ecb-cancel-timer ecb-working-timer)
         (ecb-working-dynamic-status t)
         (message ,current-message)))))


(defun ecb-working-status-call-process
  (timeout message donestr program &optional infile buffer display &rest args)
  "Display working messages while running a process.
TIMEOUT is how fast to display the messages.
MESSAGE is the message to show, and DONESTR is the string to add when done.
CALLPROCESSARGS are the same style of args as passed to `call-process'.
The are: PROGRAM, INFILE, BUFFER, DISPLAY, and ARGS.
Since it actually calls `start-process', not all features will work.
It returns the exit-status of the called PROGRAM."
  (ecb-working-status-timeout timeout message donestr
    (let* ((process-environment (cons "LC_ALL=C" process-environment))
           (proc (apply 'start-process "ecb-working"
                        (if (listp buffer) (car buffer) buffer)
                        program args)))
      (set-process-sentinel proc 'list)
      (while (eq (process-status proc) 'run)
	(accept-process-output proc)
	;; accept-process-output caused my Solaris Emacs 20.3 to crash.
	;; If this is unreliable for you, use the below which will work
	;; in that situation.
	;; (if (not (sit-for timeout)) (read-event))
	)
      (process-exit-status proc))))

;;; ----- Buffers and files --------------------------------

(defsubst ecb-current-line ()
  "Return the current line-number - the first line in a buffer has number 1."
  (+ (count-lines 1 (point)) (if (= (current-column) 0) 1 0)))

(defmacro ecb-with-readonly-buffer (buffer &rest body)
  "Make buffer BUFFER current but do not display it. Evaluate BODY in buffer
BUFFER \(not read-only an evaluation-time of BODY) and make afterwards BUFFER
read-only. Note: All this is done with `save-excursion' so after BODY that
buffer is current which was it before calling this macro."
  `(if (buffer-live-p ,buffer)
       (save-excursion
         (set-buffer ,buffer)
         (unwind-protect
             (progn
               (setq buffer-read-only nil)
               ,@body)
           (setq buffer-read-only t)))
     (ecb-error "Try to set a not existing buffer.")))

(put 'ecb-with-readonly-buffer 'lisp-indent-function 1)

(defmacro ecb-do-if-buffer-visible-in-ecb-frame (buffer-name-symbol &rest body)
  "Evaluate BODY if a buffer is visible in the ECB-frame.

This means in fact if the following conditions are all true:
- The symbol BUFFER-NAME-SYMBOL is bound
- The value of BUFFER-NAME-SYMBOL is a name of a living buffer B
- The buffer B is visible and displayed in a window of the `ecb-frame'
- ECB is active
- The current frame is the `ecb-frame'
- The window of buffer B is not a window in the edit-area.
If one of these conditions is false then nothing will be done.

During the evaluation of BODY the following local variables are bound:
- visible-buffer: The buffer-object which name is the value of
  BUFFER-NAME-SYMBOL.
- visible-window: The window which displays visible-buffer"
  `(let* ((visible-buffer (if (and (boundp ,buffer-name-symbol)
                                   (stringp (symbol-value ,buffer-name-symbol)))
                              (get-buffer (symbol-value ,buffer-name-symbol))))
          (visible-window (if (bufferp visible-buffer)
                              (get-buffer-window visible-buffer))))
     (when (and ecb-minor-mode
                (equal (selected-frame) ecb-frame)
                visible-window
                (window-live-p visible-window)
                (not (member visible-window (ecb-canonical-edit-windows-list))))
       ,@body)))
(put 'ecb-do-if-buffer-visible-in-ecb-frame 'lisp-indent-function 1)

(defun ecb-buffer-substring (minpoint maxpoint &optional buffer)
  "Return the contents of part of BUFFER as a string.
If BUFFER is nil then the current-buffer is used. BUFFER can be a buffer-name
or a buffer-object."
  (with-current-buffer (or buffer (current-buffer))
    (buffer-substring minpoint maxpoint)))

(defun ecb-delete-file (file)
  "Delete FILE if it eexists."
  (let ((exp-file (expand-file-name file)))
    (if (file-exists-p exp-file)
        (delete-file exp-file))))

(defun ecb-buffer-name (buffer-or-name)
  "Return the buffer-name of BUFFER-OR-NAME."
  (typecase buffer-or-name
    (string buffer-or-name)
    (buffer (buffer-name buffer-or-name))
    (otherwise nil)))

(defun ecb-buffer-obj (buffer-or-name)
  "Return the buffer-object of BUFFER-OR-NAME."
  (typecase buffer-or-name
    (string (get-buffer buffer-or-name))
    (buffer buffer-or-name)
    (otherwise nil)))

(defun ecb-buffer-local-value (sym buffer)
  "Get the buffer-local value of variable SYM in BUFFER. If there is no
buffer-local value in BUFFER then the global value of SYM is used."
  (if (fboundp 'buffer-local-value)
      (buffer-local-value sym buffer)
    (or (cdr (assoc sym (buffer-local-variables buffer)))
        (save-excursion
          (set-buffer buffer)
          (symbol-value sym)))))


(defun ecb-file-content-as-string (file)
  "If FILE exists and is readable returns the contents as a string otherwise
return nil.
Note: No major/minor-mode is activated and no local variables are evaluated
for FILE, but proper EOL-conversion and character interpretation is done!"
  (let ((exp-filename (expand-file-name file)))
    (if (and (file-exists-p exp-filename)
             (file-readable-p exp-filename))
        (with-temp-buffer
          (insert-file-contents exp-filename)
          (buffer-string)))))

(defun ecb-current-buffer-archive-extract-p ()
  "Return not nil if current buffer was extracted of an archive which is in
`tar-mode' or `archive-mode'. For this the current buffer has either to be in
minor-mode `tar-subfile-mode' or `archive-subfile-mode'."
  (or (and (boundp 'tar-subfile-mode)
           tar-subfile-mode)
      (and (boundp 'archive-subfile-mode)
           archive-subfile-mode)))

(defun ecb-buffer-or-file-readable-p (&optional filename)
  "Checks if a buffer or a file is a readable file in the sense of ECB which
means either a real physical file or an auto-extracted file from an archive.
See `ecb-current-buffer-archive-extract-p'. FILENAME is either a filename or
nil whereas in the latter case the current-buffer is assumed."
  (let* ((file (or filename (buffer-file-name (current-buffer)))))
    (or (and file (file-readable-p file))
        (and (not ecb-running-xemacs)
             (if filename
                 (save-excursion
                   (set-buffer (find-file-noselect filename))
                   (ecb-current-buffer-archive-extract-p))
               (ecb-current-buffer-archive-extract-p))))))


;;; ----- Windows ------------------------------------------

;; Emacs 20 has no window-list function and the XEmacs and Emacs 21 one has no
;; specified ordering. The following one is stolen from XEmacs and has fixed
;; this lack of a well defined order. We preserve also point of current
;; buffer! IMPORTANT: When the window-ordering is important then currently
;; these function should only be used with WINDOW = (frame-first-window
;; ecb-frame)!
(defun ecb-window-list (&optional frame minibuf window)
  "Return a list of windows on FRAME, beginning with WINDOW. The
windows-objects in the result-list are in the same canonical windows-ordering
of `next-window'. If omitted, WINDOW defaults to the selected window. FRAME and
WINDOW default to the selected ones. Optional second arg MINIBUF t means count
the minibuffer window even if not active. If MINIBUF is neither t nor nil it
means not to count the minibuffer even if it is active."
  (if (not ecb-running-xemacs)
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: There seems to be
      ;; mysterious behavior when running our own window-list version with
      ;; GNU Emacs >= 21.3 - especially when running an igrep when the
      ;; igrep-buffer is already in another window. We can here savely use the
      ;; function `window-list' because it returns an ordered list
      (window-list frame minibuf window)
    (setq window (or window (selected-window))
          frame (or frame (selected-frame)))
    (if (not (eq (window-frame window) frame))
        (error "Window must be on frame."))
    (let ((current-frame (selected-frame))
          (current-point (point))
          list)
      (unwind-protect
          (save-window-excursion
            (select-frame frame)
            ;; this is needed for correct start-point
            (select-window window)
            (walk-windows
             (function (lambda (cur-window)
                         (if (not (eq window cur-window))
                             (setq list (cons cur-window list)))))
             minibuf
             'selected)
            ;; This is needed to get the right canonical windows-order, i.e. the
            ;; same order of windows than `walk-windows' walks through!
            (setq list (nreverse list))
            (setq list (cons window list)))
        (select-frame current-frame)
        ;; we must reset the point of the buffer which was current at call-time
        ;; of this function
        (goto-char current-point)))))

(defun ecb-canonical-windows-list ()
  "Return a list of all current visible windows in the `ecb-frame' \(starting
from the left-most top-most window) in the order `other-window' would walk
through these windows."
  (ecb-window-list ecb-frame 0 (frame-first-window ecb-frame)))

(defun ecb-enlarge-window (window &optional val)
  "Enlarge the given window.
If VAL is nil then WINDOW is enlarged so that it is 1/2 of the current frame.
If VAL is a positive integer then WINDOW is enlarged so that its new height is
VAL lines. If VAL is > 0 and < 1 then WINDOW is enlarged so that its new
height is that fraction of the frame."
  (if (and window (window-live-p window))
      (let* ((norm-val (if val
                           (ecb-normalize-number val (1- (frame-height)))
                         (/ (1- (frame-height)) 2)))
             (enlargement (- norm-val (ecb-window-full-height window))))
        (save-selected-window
          (select-window window)          
          (if (> enlargement 0)
              (enlarge-window enlargement))))
    (error "Window is not alive!")))

(defun ecb-scroll-window (point window-start)
  "Scrolls window of current buffer. The window will start at WINDOW-START and
point will stay on POINT."
  (goto-char point)
  (set-window-start (get-buffer-window (current-buffer)) window-start))

(defun ecb-window-select (buffer-or-name)
  "Select that window which displays in the `ecb-frame' the buffer
BUFFER-OR-NAME which can be either a buffer-object or a buffer-name. Return
the window-object. If that buffer is not displayed in the `ecb-frame' then
nothing happens and nil is returned."
  (let ((window (get-buffer-window buffer-or-name ecb-frame)))
    (if window
	(select-window window)
      nil)))

(defmacro ecb-exec-in-window (buffer-or-name &rest body)
  "Evaluates BODY in that window which displays the buffer BUFFER-OR-NAME
which can be either a buffer-object or a buffer-name. If that window is not
visible then BODY is not evaluated and the symbol 'window-not-visible is
returned. Otherwise the return value of BODY is returned. Runs encapsulated in
`save-selected-window'."
  `(save-selected-window
     (if (not (ecb-window-select ,buffer-or-name))
         'window-not-visible
       ,@body)))

(put 'ecb-exec-in-window 'lisp-indent-function 1)

(defun ecb-make-windows-not-dedicated (&optional frame)
  "Make all windows of FRAME not dedicated."
  (mapc (function (lambda (w)
                    (set-window-dedicated-p w nil)))
        (ecb-window-list (or frame (selected-frame)))))

(defun ecb-set-windows-dedicated-state (buf-list state)
  "For every buffer in BUF-LIST set its windows dedicated-state to STATE if
visible in the `ecb-frame'."
  (mapc (function (lambda (b)
                    (when (get-buffer-window b ecb-frame)
                      (set-window-dedicated-p
                       (get-buffer-window b ecb-frame) state))))
        buf-list))


(defun ecb-window-number (&optional window)
  "Return the number of WINDOW or - if nil - of the current selected window.
The left-top-most window of the ecb-frame has number 0. The other windows have
the same ordering as `other-window' would walk through the frame."
  (1- (length (memq (or window (selected-window))
                    (nreverse (if ecb-running-xemacs
                                  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
                                  ;; There seems to be a mysterious behavior of
                                  ;; `ecb-window-list' when changing one buffer,
                                  ;; immediately opening another buffer and
                                  ;; scrolling sown to bottom of the new buffer
                                  ;; - then if this function is added to the
                                  ;;   modeline via (:eval...) the new buffer
                                  ;;   scrolls autom. back to beginng of
                                  ;;   buffer.
                                  ;; With the original window-list all is
                                  ;; ok...
                                  (ecb-canonical-windows-list)
                                (window-list)))))))

;;; ----- Time  stuff -----------------------------------------

;; next three functions stolen from gnus
(defun ecb-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (nth 2 time) 0) 1000000.0)))

(defun ecb-seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to an Emacs time structure."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(defun ecb-subtract-time (t1 t2)
  "Subtract two internal times and return the result as internal time."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun ecb-time-diff (t1 t2 &optional rounded)
  "Return the difference between time T1 and T2 in seconds \(can be a
floating-point number). If optional arg ROUNDED is not nil the result is a
rounded integer."
  (funcall (if rounded 'round 'identity)
           (ecb-time-to-seconds (ecb-subtract-time t1 t2))))
  
;; (let ((t1 nil)
;;       (t2 nil))
;;   (setq t1 (current-time))
;;   (sit-for 5)
;;   (setq t2 (current-time))
;;   (ecb-time-diff t2 t1 t))

(defun ecb-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

;;; ----- Ringstuff ----------------------------------------

(require 'ring)
(defalias 'ecb-make-ring 'make-ring)
(defalias 'ecb-ring-p 'ring-p)
(defalias 'ecb-ring-empty-p 'ring-empty-p)
(defalias 'ecb-ring-insert 'ring-insert)
(defalias 'ecb-ring-ref 'ring-ref)
;; at least XEmacs does not have this function.
(defun ecb-ring-elements (ring)
  "Return a list of the lements of RING."
  (mapcar #'identity (cddr ring)))

;;; ----- Menu stuff ---------------------------------------

(defvar ecb-max-submenu-depth 4
  "The maximum depth of nesting submenus for the tree-buffers.")

(defun ecb-create-menu-user-ext-type (curr-level max-level)
  "Creates the :type-definition for the *-menu-user-extension options.
This allows nested submenus for the popup-menus of the tree-buffers up to a
maximum level of MAX-LEVEL. CURR-LEVEL must be 1 when used in a
defcustom-clause and has to be <= MAX-LEVEL."
  (list 'repeat (delq nil
                      (list 'choice ':tag "Menu-entry" ':menu-tag "Menu-entry"
                            ':value '(ignore "")
                            (list 'const ':tag "Separator" ':value '("---"))
                            (list 'list ':tag "Menu-command"
                                  (list 'function ':tag "Function" ':value 'ignore)
                                  (list 'string ':tag "Entry-name"))
                            (if (= curr-level max-level)
                                nil
                              (list 'cons ':tag "Submenu"
                                    (list 'string ':tag "Submenu-title")
                                    (ecb-create-menu-user-ext-type (1+ curr-level)
                                                                   max-level)))))))

;;; ----- byte-compiling stuff ----------------------------

(defun ecb-is-byte-compiling ()
  "Return non-nil if eval'ed during compilation.  Don't use outside
`eval-when-compile'."
  (and (boundp 'byte-compile-dest-file)
       (stringp byte-compile-dest-file)))

(defun ecb-load-in-progress-p ()
  load-in-progress)

;;; ----- User Interrupt handling -------------------------

;; KB: stolen from semantic.....

(defvar ecb-current-input-throw-symbol nil
  "The current throw symbol for `ecb-exit-on-input'.")

(defmacro ecb-exit-on-input (symbol &rest forms)
  "Using SYMBOL as an argument to `throw', execute FORMS.
If FORMS includes a call to `ecb-thow-on-input', then if a user presses any
key during execution, this form macro will exit with the value passed to
`ecb-throw-on-input'. If FORMS completes, then the return value is the same as
`progn'."
  `(let ((ecb-current-input-throw-symbol ,symbol))
     (catch ,symbol
       ,@forms)))
(put 'ecb-exit-on-input 'lisp-indent-function 1)

(defmacro ecb-throw-on-input (from &optional value)
  "Exit with `throw' when in `ecb-exit-on-input' on user input.
FROM is an indication of where this function is called. Optional arg VALUE is
what should be thrown out and both are are combined in a cons-cell and passed
to `throw'. It is recommended to add as FROM the name of the function calling
this one or a descriptive symbol which indicates part of a code has been
interrupted..

Example: \(ecb-throw-on-input 'test-inner-loop \"test\") would throw a
cons-cell \('test-inner-loop . \"test\")"
  `(when (and ecb-current-input-throw-symbol (input-pending-p))
     (throw ecb-current-input-throw-symbol (cons ,from ,value))))


(defun ecb-test-throw-on-input ()
  "Test that throw on input will work."
  (interactive)
  (ecb-throw-on-input 'done-die)
  (message "Exit Code: %s"
	   (ecb-exit-on-input 'testing
	     (let ((inhibit-quit nil)
		   (message-log-max nil))
	       (while t
		 (message "Looping ...")
		 (ecb-throw-on-input 'test-inner-loop "test")
                 )
	       'exit))))


;;; ----- Provide ------------------------------------------

(silentcomp-provide 'ecb-util)

;;; Local Variables: ***
;;; mode:outline-minor ***
;;; End: ***

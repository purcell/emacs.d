;;; mmm-compat.el --- MMM Hacks for compatibility with other Emacsen

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: $Id: mmm-compat.el,v 1.9 2003/03/09 17:04:03 viritrilbia Exp $

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file provides a number of hacks that are necessary for MMM
;; Mode to function in different Emacsen.  MMM Mode is designed for
;; FSF Emacs 20 and 21, but these hacks usually enable it to work
;; almost perfectly in Emacs 19 and XEmacs 20 or 21.

;;; Code:

(require 'cl)

;;{{{ Emacsen Detection

(defvar mmm-xemacs (featurep 'xemacs)
  "Whether we are running XEmacs.")

;;}}}
;;{{{ Keywords (Emacs 19)

;; Emacs 19 doesn't automatically set keyword variables to themselves.
;; We shouldn't have to do any more than these, since CL automatically
;; defines all keywords used for function arguments.
(defvar mmm-keywords-used
  '(:group :regexp :region :function :insert :classes :private)
  "List of extra keywords used by MMM Mode.")

(dolist (keyword mmm-keywords-used)
  (set keyword keyword))

;;}}}
;;{{{ Customization (Emacs 19)

(condition-case ()
    (require 'custom)
  (error nil))

(unless (and (featurep 'custom)
             (fboundp 'custom-declare-variable))
  (defmacro defgroup (&rest args)
    nil)
  (defmacro defface (var values doc &rest args)
    (` (make-face (quote (, var)))))
  (defmacro defcustom (var value doc &rest args) 
    (` (defvar (, var) (, value) (, doc)))))

;;}}}
;;{{{ Regexp-Opt (Emacs 19)

(condition-case ()
    (require 'regexp-opt)
  (error nil))

(unless (and (featurep 'regexp-opt)
             (fboundp 'regexp-opt))
  ;; No regexp-opt; create one
  (defun regexp-opt (strings &optional paren)
    (concat (if paren "\\(" "")
            (mapconcat 'regexp-quote strings "\\|")
            (if paren "\\)" ""))))

;;}}}
;;{{{ Regexp-Opt (XEmacs)

(defmacro mmm-regexp-opt (strings paren)
  "Act like FSF Emacs' `regexp-opt', whichever Emacs we're in.
XEmacs' `regexp-opt' requires an extra parameter to do grouping."
  (if (featurep 'xemacs)
      `(regexp-opt ,strings ,paren t)
    `(regexp-opt ,strings ,paren)))

;;}}}
;;{{{ Overlays (XEmacs)

;; The main thing we use from FSF Emacs that XEmacs doesn't support
;; are overlays. XEmacs uses extents instead, but comes with a package
;; to emulate overlays.
(when mmm-xemacs
  ;; This does almost everything we need.
  (require 'overlay))

;; We also use a couple "special" overlay properties which have
;; different names for XEmacs extents.
(defvar mmm-evaporate-property
  (if (featurep 'xemacs) 'detachable 'evaporate)
  "The name of the overlay property controlling evaporation.")

;; We don't use this any more, since its behavior is different in FSF
;; and XEmacs: in the one it replaces the buffer's local map, but in
;; the other it gets stacked on top of it. Instead we just set the
;; buffer's local map temporarily.
;;;(defvar mmm-keymap-property
;;;  (if (featurep 'xemacs) 'keymap 'local-map)
;;;  "The name of the overlay property controlling keymaps.")

;;}}}
;;{{{ Keymaps and Events (XEmacs)

;; In XEmacs, keymaps are a primitive type, while in FSF Emacs, they
;; are a list whose car is the symbol `keymap'. Among other things,
;; this means that they handle default bindings differently.
(defmacro mmm-set-keymap-default (keymap binding)
  (if (featurep 'xemacs)
      `(set-keymap-default-binding ,keymap ,binding)
    `(define-key ,keymap [t] ,binding)))

;; In XEmacs, events are a primitive type, while in FSF Emacs, they
;; are represented by characters or vectors. We treat them as vectors.
;; We can use `event-modifiers' in both Emacsen to extract the
;; modifiers, but the function to extract the basic key is different.
(defmacro mmm-event-key (event)
  (if (featurep 'xemacs)
      `(event-key ,event)
    `(event-basic-type ,event)))

;;}}}
;;{{{ Skeleton (XEmacs)

;; XEmacs' `skeleton' package doesn't provide `@' to record positions.
(defvar skeleton-positions ())
(defun mmm-fixup-skeleton ()
  "Add `@' to `skeleton-further-elements' if XEmacs and not there.
This makes `@' in skeletons act approximately like it does in FSF."
  (and (featurep 'xemacs)
       (defvar skeleton-further-elements ())
       (not (assoc '@ skeleton-further-elements))
       (add-to-list 'skeleton-further-elements
                    '(@ ''(push (point) skeleton-positions)))))

;;}}}
;;{{{ Make Temp Buffers (XEmacs)

(defmacro mmm-make-temp-buffer (buffer name)
  "Return a buffer called NAME including the text of BUFFER.
This text should not be modified."
  (if (fboundp 'make-indirect-buffer)
      `(make-indirect-buffer ,buffer ,name)
    `(save-excursion
       (set-buffer (get-buffer-create ,name))
       (insert-buffer ,buffer)
       (current-buffer))))

;;}}}
;;{{{ Font Lock Available (Emacs w/o X)

(defvar mmm-font-lock-available-p (or window-system mmm-xemacs)
  "Whether font-locking is available.
Emacs 19 and 20 only provide font-lock with a window system in use.")

;;}}}
;;{{{ Font Lock Defaults (XEmacs)

(defmacro mmm-set-font-lock-defaults ()
  "Set font-lock defaults without trying to turn font-lock on.
In XEmacs, `font-lock-set-defaults' calls `font-lock-set-defaults-1'
to do the real work but then `turn-on-font-lock', which in turn calls
`font-lock-mode', which unsets the defaults if running in a hidden
buffer \(name begins with a space).  So in XEmacs, we just call
`font-lock-set-defaults-1' directly."
  (if mmm-xemacs
      `(font-lock-set-defaults-1)
    `(font-lock-set-defaults)))

;;}}}

(provide 'mmm-compat)

;;; mmm-compat.el ends here
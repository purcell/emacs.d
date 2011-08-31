;;; ecb-layout.el --- layout for ECB

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
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

;; $Id: ecb-layout.el,v 1.276 2009/05/16 13:24:19 berndl Exp $

;;; Commentary:
;;
;; Contains functions for settings the ECB layout.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;; This file has been re-implemented by Klaus Berndl <klaus.berndl@sdm.de>.
;; What has been done:
;; Completely rewritten the layout mechanism for better customizing, adding
;; new layouts, better redrawing and more straightforward code.
;; 1. Now all user-layouting is done by customizing the new option
;;    `ecb-layout-name' or by the command `ecb-change-layout'. The function
;;    `ecb-redraw-layout' (formally known as 'ecb-set-layout) can still be
;;    called interactively. But it just redraws the layout specified in
;;    `ecb-layout-name'. All changes to the layout must be made by customizing
;;    this new option. Please read the very detailed comment of
;;    `ecb-layout-name'!
;; 2. Adding new layouts is now much easier and more straightforward: We have
;;    now a main core-layout function (`ecb-redraw-layout-full') which is the
;;    "environment" for the specific "layout-functions". The core function
;;    does first some layout independent actions, then calls the
;;    "layout-function" for the name which has been set in `ecb-layout-name'
;;    and after that it does some layout independent actions again (see the
;;    comments in this function). See the macro `ecb-layout-define' and the
;;    command `ecb-create-new-layout'!
;;
;; Background-info: For each layout-type (ecb-windows left, right, top and
;; left-right) there is one function:
;; 'ecb-delete-window-ecb-windows-[left|right|top|leftright]'.
;; These functions follow these guide-lines:
;; - Preconditions for these functions:
;;   + the edit-area is splitted - at least in two edit-windows
;;   + The function gets two arguments: The window to delete (if nil then the
;;     current window has to be deleted) and the list of all current
;;     edit-windows.
;;   + These functions are always(!) called with deactivated advices of
;;     `delete-window' function.
;;   + These functions can only use `delete-window' of the set of maybe
;;     adviced window functions, because of a bug in advice.el only one
;;     function´s advice can be deactivated within a advice itself!
;; - What must they do: Doing the appropriate action (e.g.
;;   `ecb-delete-window-ecb-windows-left' must delete the window. This action
;;   must be done appropriate for the current ECB-layout type (see
;;   postconditions)
;; - Postcondition of these functions:
;;   + The named edit-window must be deleted and all ecb-windows in the
;;     ecb-frame must have the layout like before the delete.
;;   + If the current window has been deleted then point must reside after
;;     deletion in the next edit-window in a circular meaning (i.e. if the
;;     last edit-window has been deleted, point must stay afterwards in the
;;     first edit-window). If a window unequal the current window has been
;;     deleted point must stay in the window before deletion at the same
;;     place.
;;
;; New adviced intelligent window-functions as replacement for these originals:
;; - `other-window'
;; - `delete-window'
;; - `delete-other-windows'
;; - `delete-windows-on'
;; - `split-window-horizontally'
;; - `split-window-vertically'
;; - `split-window'
;; - `display-buffer'
;; - `switch-to-buffer'
;; - `switch-to-buffer-other-window'
;; - `other-window-for-scrolling'
;; - `balance-windows'
;; The behavior of the adviced functions is:
;; - All these function behaves exactly like their corresponding original
;;   functions but they always act as if the edit-window(s) of ECB would be the
;;   only window(s) of the ECB-frame. So the edit-window(s) of ECB seems to be
;;   a normal Emacs-frame to the user.
;; - If a persistent compile-window is used all buffers for which
;;   `ecb-compilation-buffer-p' returns not nil are handled in the
;;   compile-window!
;;
;; IMPORTANT: A note for programming Elisp for packages which work during
;; activated ECB (for ECB itself too :-): ECB offers three macros for easy
;; temporally (regardless of the settings in `ecb-advice-window-functions'!)
;; using all original-functions, all adviced functions or only some adviced
;; functions:
;; - `ecb-with-original-functions'
;; - `ecb-with-adviced-functions'
;; - `ecb-with-some-adviced-functions'
;;

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)
(require 'ecb-common-browser)
(require 'ecb-speedbar)
(require 'ecb-compilation)
(require 'ecb-create-layout)

;; XEmacs
(silentcomp-defvar scrollbars-visible-p)
(silentcomp-defun window-displayed-height)
(silentcomp-defvar pre-display-buffer-function)
(silentcomp-defvar split-width-threshold)
(silentcomp-defvar split-width-threshold)
(silentcomp-defun popup-menu-and-execute-in-window)
(silentcomp-defvar modeline-map)
(silentcomp-defun modeline-menu)
;; for the display-buffer stuff of XEmacs
(silentcomp-defun last-nonminibuf-frame)
(silentcomp-defun check-argument-type)
(silentcomp-defun buffer-dedicated-frame)
(silentcomp-defun display-buffer-1)
(silentcomp-defun frame-property)
(silentcomp-defun window-leftmost-p)
(silentcomp-defun window-rightmost-p)
(silentcomp-defun window-parent)
(silentcomp-defun window-previous-child)
(silentcomp-defun window-next-child)
(silentcomp-defun window-pixel-edges)
(silentcomp-defun window-pixel-height)
(silentcomp-defun record-buffer)
(silentcomp-defun push-window-configuration)
(silentcomp-defun set-frame-property)
(silentcomp-defvar temp-buffer-shrink-to-fit)

;; Emacs
(silentcomp-defvar scroll-bar-mode)
;; only Emacs 21 has this
(silentcomp-defvar window-size-fixed)
;;(silentcomp-defun fit-window-to-buffer)
(silentcomp-defvar temp-buffer-resize-mode)
(silentcomp-defun temp-buffer-resize-mode)
(silentcomp-defun modify-frame-parameters)
;; Emacs 21
(silentcomp-defvar grep-window-height)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(defvar ecb-layouts-reload-needed t)
(defun ecb-load-layouts ()
  "Load all defined layouts"
  (when ecb-layouts-reload-needed
    (require 'ecb-layout-defs)
    (if (file-readable-p ecb-create-layout-file)
        (load-file ecb-create-layout-file))
    (setq ecb-layouts-reload-needed nil)))

(defgroup ecb-layout nil
  "Settings for the screen-layout of the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-compilation nil
  "Settings for the compile window of ECB."
  :group 'ecb-layout
  :prefix "ecb-")


(defconst ecb-layout-option-set-function
  (function (lambda (symbol value)
	      (set symbol value)
             ;; we must check this because otherwise the layout would be drawn
	      ;; if we have changed the initial value regardless if ECB is
	      ;; activated or not.
	      (when (and (boundp 'ecb-minor-mode)
                         ecb-minor-mode
                         (frame-live-p ecb-frame))
                (let ((curr-frame (selected-frame)))
                  (unwind-protect
                      (progn
                        (select-frame ecb-frame)
                        (ecb-redraw-layout-full))
                    (select-frame curr-frame)))))))
                    

(defcustom ecb-select-edit-window-on-redraw nil
  "*Select the first edit window on `ecb-redraw-layout'."
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-new-ecb-frame nil
  "*Create a new frame at activation time of ECB."
  :group 'ecb-layout
  :group 'ecb-most-important
  :type 'boolean)

(defcustom ecb-activate-before-new-frame-created-hook nil
  "*Hook run before the new ECB-frame is created.
This has only an effect if `ecb-new-ecb-frame' is not nil \(otherwise this
hook is not evaluated)."
  :group 'ecb-layout
  :type 'hook)

(defvar ecb-last-selected-layout nil
  "Name of that layout which was current direct before switching to another
layout.")

(defcustom ecb-layout-name "left8"
  "*Select a window layout of ECB.
Value is any arbitrary string. There are four different types of layouts:
left, right, top and left-right, which means the location of the
ECB-tree-windows in the ECB-frame. Currently there are 20 predefined layouts;
names see below. You can savely try out any of them by changing this value and
saving it only for the current session. If you are sure which layout you want
you can save it for future sessions. To get a picture of the layout for name
<name> call `ecb-show-layout-help'.

Currently available layouts:

+ Left layouts:
  left1 left2 left3 left4 left5 left6 left7 left8 left9 left10 left11 left12
  left13 left14 left15

+ Right layouts:
  right1

+ Top layouts:
  top1 top2

+ Left-right layouts:
  leftright1 leftright2 leftright3

Regardless of the settings you define here: If you have destroyed or
changed the ECB-screen-layout by any action you can always go back to this
layout with `ecb-redraw-layout'"
  :group 'ecb-layout
  :group 'ecb-most-important
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   (ecb-load-layouts)
                   (if (fboundp (intern (format "ecb-layout-function-%s"
                                                value)))
                       (progn
                         (setq ecb-last-selected-layout ecb-layout-name)
                         (funcall ecb-layout-option-set-function
                                  symbol value))
                     (ecb-error "There is no layout with name %s available!"
                                value))))
  :type 'string)

(defun ecb-enable-temp-buffer-shrink-to-fit (arg)
  "Enables `temp-buffer-resize-mode' \(GNU Emacs) rsp.
`temp-buffer-shrink-to-fit' \(XEmacs) when a comile-window is used. When the
compile-window is disabled or when ECB is deactivated then the old state of
these modes/variables is restored."
  (if arg
      (progn
        ;; store old value if not already done
        (or (get 'ecb-enable-temp-buffer-shrink-to-fit
                 'ecb-old-temp-buffer-shrink-to-fit)
            (put 'ecb-enable-temp-buffer-shrink-to-fit
                 'ecb-old-temp-buffer-shrink-to-fit
                 (cons 'stored
                       (if ecb-running-xemacs
                           temp-buffer-shrink-to-fit
                         temp-buffer-resize-mode))))
        ;; now we activate temp-buffer-shrinking
        (if ecb-running-xemacs
            (setq temp-buffer-shrink-to-fit t)
          (temp-buffer-resize-mode 1))
        )
    ;; reset to the original value
    (and (get 'ecb-enable-temp-buffer-shrink-to-fit
              'ecb-old-temp-buffer-shrink-to-fit)
         (if ecb-running-xemacs
             (setq temp-buffer-shrink-to-fit
                   (cdr (get 'ecb-enable-temp-buffer-shrink-to-fit
                             'ecb-old-temp-buffer-shrink-to-fit)))
           (temp-buffer-resize-mode
            (if (cdr (get 'ecb-enable-temp-buffer-shrink-to-fit
                          'ecb-old-temp-buffer-shrink-to-fit))
                1
              -1))))
    (put 'ecb-enable-temp-buffer-shrink-to-fit
         'ecb-old-temp-buffer-shrink-to-fit
         nil)))

(defcustom ecb-compile-window-height nil
  "*Height of the persistent compilation-window of ECB.
If you want a compilation window shown at the bottom of the ECB-layout
then set here the height of it \(Default is a height of 5). If you redraw the
current layout with `ecb-redraw-layout' then the compilation window (if any)
has the height you set here. If the number is less than 1.0 the height is a
fraction of the frame height.

If you do not set a persistent compilation window then doing a compilation or
displaying temp-buffers \(e.g. *Help*-buffers) splits temporally the edit
window vertically if the edit window is not splitted already or uses another
edit window temporally for compilation output if the edit window is already
splitted. This is the recommended value for this option because this is the
standard-behavior of Emacs.

Beware: If you set a persistent compilation window then ECB displays all buffers
for which `ecb-compilation-buffer-p' returns not nil in that persistent
compilation window. If a buffer which should being displayed there is not
displayed there then try to modify the options `ecb-compilation-buffer-names',
`ecb-compilation-major-modes' or `ecb-compilation-predicates' \(in this
sequence).

See also the options `ecb-compile-window-temporally-enlarge' and
`ecb-enlarged-compilation-window-max-height' and also the command
`ecb-toggle-compile-window-height'!

ECB offers the functionality of such a persistent compile-window regardless if
the special ECB-windows are visible or not \(see the command
`ecb-toggle-ecb-windows').

Regardless of the settings you define here: If you have destroyed or
changed the ECB-screen-layout by any action you can always go back to this
layout with `ecb-redraw-layout'"
  :group 'ecb-compilation
  :group 'ecb-most-important
  :initialize 'custom-initialize-default
  ;; we can not use here ' ecb-layout-option-set-function' because here we
  ;; must call `ecb-redraw-layout-full' with NO-ECB-WINDOWS depending on the
  ;; value of `ecb-windows-hidden'! Same for `ecb-compile-window-width'. If
  ;; this is necessary for other options too then we should
  ;; `ecb-layout-option-set-function' to a function with an additional
  ;; parameter which decides if ecb-window-hidden should be used for
  ;; NO-ECB-WINDOWS or not.
  :set (function (lambda (symbol value)
                   ;; Emacs < 22 has some bugs concerning `windows-size-fixed'
                   ;; so we must disable window-fixing.
                   (and (not ecb-running-version-22) (ecb-set-window-size-fixed nil))
                   (set symbol value)
                   ;; we must check this because otherwise the layout would be
                   ;; drawn if we have changed the initial value regardless if
                   ;; ECB is activated or not.
                   (when (and (boundp 'ecb-minor-mode)
                              ecb-minor-mode
                              (frame-live-p ecb-frame))
                     (ecb-enable-temp-buffer-shrink-to-fit value)
                     (let ((curr-frame (selected-frame)))
                       (unwind-protect
                           (progn
                             (select-frame ecb-frame)
                             (ecb-redraw-layout-full nil nil nil
                                                     ecb-windows-hidden))
                         (select-frame curr-frame))))))
  :type '(radio (const :tag "No compilation window" nil)
                (number :tag "Window height" :value 6)))



(defcustom ecb-compile-window-width 'frame
  "*Width of the compile-window.

Possible values are 'frame and 'edit-window.
With 'frame the compile-window looks like:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|            edit-window(s)            |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------


With 'edit-window the compile-window looks like:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|            edit-window(s)            |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |---------------------------------------
   |              |                                      |
   |              |            Compilation               |
   |              |                                      |
   -------------------------------------------------------

This option takes only effect if `ecb-compile-window-height' is not nil!"
  :group 'ecb-compilation
  :group 'ecb-most-important
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   ;; Emacs < 22 has some bugs concerning `windows-size-fixed'
                   ;; so we must disable window-fixing.
                   (and (not ecb-running-version-22) (ecb-set-window-size-fixed nil))
                   (set symbol value)
                   ;; we must check this because otherwise the layout would be
                   ;; drawn if we have changed the initial value regardless if
                   ;; ECB is activated or not.
                   (when (and (boundp 'ecb-minor-mode)
                              ecb-minor-mode
                              (frame-live-p ecb-frame))
                     (let ((curr-frame (selected-frame)))
                       (unwind-protect
                           (progn
                             (select-frame ecb-frame)
                             (ecb-redraw-layout-full nil nil nil
                                                     ecb-windows-hidden))
                         (select-frame curr-frame))))))
  :type '(radio (const :tag "Width of ECB-frame" :value frame)
                (const :tag "Width of edit-window" :value edit-window)))

(defcustom ecb-change-layout-preserves-compwin-state t
  "*Changing the layout preserves the state of the compile-window.
This is for example useful if the user toggles between several layouts \(see
`ecb-toggle-layout') and wants to preserve the hidden-state of the
compile-window."
  :group 'ecb-compilation
  :type 'boolean)

(defcustom ecb-compile-window-temporally-enlarge 'after-display
  "*Let Emacs temporally enlarge the compile-window of the ECB-layout.
This option has only an effect if `ecb-compile-window-height' is not nil!

The following values are possible:
- 'after-display: After displaying a \"compilation-buffer\" \(in the sense of
  `ecb-compilation-buffer-p'!) in the compile-window of ECB. For the max.
  height of the enlarged compile-window see the option
  `ecb-enlarged-compilation-window-max-height'.

- 'after-selection: selecting the `ecb-compile-window' auto. enlarges it and
  de-selecting \(means leaving `ecb-compile-window') auto. shrinks it.
  Enlarging and shrinking the `ecb-compile-window' is done with
  `ecb-toggle-compile-window-height'. See also the documentation of this
  function!

- 'both: The combination of 'after-display and 'after-selection.

- nil: ECB fixes always the height of the `ecb-compile-window' at the value of
  `ecb-compile-window-height'.

To restore the ECB-layout after such a buffer-enlarge just call
`ecb-toggle-compile-window-height' or `ecb-redraw-layout'."
  :group 'ecb-compilation
  :type '(radio (const :tag "After displaying a buffer in the compile-window"
                       :value after-display)
                (const :tag "After selecting the compile window"
                       :value after-selection)
                (const :tag "Both of them" :value both)
                (const :tag "Never" :value nil)))

(defcustom ecb-maximize-ecb-window-after-selection nil
  "*If not nil maximize current tree-window after selection.
When selecting another not-tree-window after such an automatic maximizing all
tree-windows of current layout are displayed again. But a tree-window is not
maximized if either a node has been selected via primary- oder secondarc
mouse-button or the popup-menu of that tree-buffer has been opened."
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-maximize-next-after-maximized-select nil
  "*Maximizes the next logical tree-window after a maximized node-selection.
Selecting a node in a maximized tree-window is handled very smart by ECB:

If a tree-buffer-name is not contained in this option then selecting a node in
this maximized tree-buffer automatically \"minimizes\" that tree-window \(i.e.
displays all windows of the current layout) so the user can perform the next
logical step \(e.g. the next logical step after selecting a directory in the
directories-buffer is to select a source-file - therefore the sources-buffer
of current layout has to be displayed - if the current layout contains one;
the next logical step of selecting a source-file is probably to jump to a
certain tag via the methods-buffer).

If a tree-buffer-name is contained in this option then selecting a node in
this tree-buffer automatically maximizes the next logical tree-window \(e.g.
directories --> sources, sources/history --> methods). But if the current
maximized tree-buffer is also contained in the option
`ecb-tree-do-not-leave-window-after-select' \(see also the tree-buffer-command
`ecb-toggle-do-not-leave-window-after-select' which is bound to `C-t' in each
tree-buffer) then ECB does *not* maximize the next logical tree-window but
point stays in the currently maximized tree-buffer so for example the user can
select more than one node \(e.g. more than one source-file from the
sources-buffer.

The tree-buffer-name can either be defined as plain string or with a symbol
which contains the buffer-name as value. The latter one is recommended for the
builtin ECB-tree-buffers because then simply the related option-symbol can be
used \(e.g. `ecb-directories-buffer-name', `ecb-sources-buffer-name' or
`ecb-history-buffer-name').

In future versions this option will probably also allow to define the next
logical tree-buffer for a tree-buffer - currently this is hard-coded as
follows:
- directories --next-logical--> sources
- sources     --next-logical--> methods
- history     --next-logical--> methods."
  :group 'ecb-layout
  :type '(repeat (choice :menu-tag "Buffer-name"
                         (string :tag "Buffer-name as string")
                         (symbol :tag "Symbol holding buffer-name"))))

;; A value of never makes no sense because it is not much effort to prevent
;; all interactive shrinking commands (incl. mouse-commands) from shrinking it
;; below ecb-compile-window-height and it is also not worth. IMHO preventing
;; in non-interactive calls and allowing interactively is the best choice.
;; Allowing always is also possible and easy to implement.
(defcustom ecb-compile-window-prevent-shrink-below-height t
  "*Allow the compile-window to be shrunken below its height.
A non nil value means ECB prevents the compile-window from being shrunken
below the threshold of `ecb-compile-window-height' by displaying temp-buffers
\(e.g. *Help* etc.) or after running compilations or greps. But interactively
it is always allowed to shrink it to every height!

If nil then ECB does nothing to prevent being shrunken below the value of
`ecb-compile-window-height'.

Default is t."
  :group 'ecb-compilation
  :type 'boolean)


(defcustom ecb-enlarged-compilation-window-max-height 'best
  "*The max height of the compile-window after enlarging it.
The max height of the compilation window after enlarged by
`ecb-toggle-compile-window-height'. The following values are allowed:

'best:

ECB fits the height of the compile-window exactly to the size of its current
contents but never shrinks below the value of `ecb-compile-window-height' or
enlarges over the half of the frame-height of the ECB-frame. The values of the
options `compilation-window-height' and `temp-buffer-max-height' are taken
into account dependent of the current `major-mode' of the buffer in the
compile-window: If `compilation-mode' then `compilation-window-height' is used
otherwise `temp-buffer-max-height'.

'half:

1/2 the frame-height of the ECB-frame

Any number:

Max height in lines. If the number is less than 1.0 the height is a fraction
of the frame height \(e.g. 0.33 results in a max-height of 1/3 the
frame-height)."
  :group 'ecb-compilation
  :type '(radio (const :tag "Compute best height"
                       :value best)
                (const :tag "1/2 the frame height)"
                       :value half)
                (number :tag "Height" :value 0.3)))

(defcustom ecb-scroll-other-window-scrolls-compile-window nil
  "*`scroll-other-window' scrolls always the compile-window.
For all details about the scroll-behavior of `scroll-other-window' see the
advice documentation of `other-window-for-scrolling'."
  :group 'ecb-compilation
  :type 'boolean)

(defcustom ecb-ignore-special-display 'compile-window
  "*Ignore special-display-handling.
This means, that all values of `special-display-function',
`special-display-buffer-names' and `special-display-regexps' are ignored
- only when persistent compile window is used - i.e. if
  `ecb-compile-window-height' is not nil - this is the default value.
- always when ECB is active - that means no special-display-handling of
  buffers when ECB is active
- never, i.e. special-dislay-handling depends on the values of the options
  `special-display-function', `special-display-buffer-names' and
  `special-display-regexps'."
  :group 'ecb-layout
  :type '(radio (const :tag "When a persistent compile-window is used"
                       :value compile-window)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)))

(defsubst ecb-ignore-special-display ()
  (or (equal ecb-ignore-special-display 'always)
      (and (equal ecb-ignore-special-display 'compile-window)
           ecb-compile-window-height)))

(defcustom ecb-ignore-pop-up-frames 'compile-window
  "*Ignore setting of option `pop-up-frames'.
This means, that a value of not nil for `pop-up-frames' is ignored
- only when persistent compile window is used - i.e. if
  `ecb-compile-window-height' is not nil - this is the default value.
- always when ECB is active - that means no pop-up-frames when ECB is active
- never, i.e. pop-up-frames is fully active when set."
  :group 'ecb-layout
  :type '(radio (const :tag "When a persistent compile-window is used"
                       :value compile-window)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)))

(defsubst ecb-ignore-pop-up-frames ()
  (or (equal ecb-ignore-pop-up-frames 'always)
      (and (equal ecb-ignore-pop-up-frames 'compile-window)
           ecb-compile-window-height)))

(defcustom ecb-ignore-display-buffer-function 'always
  "*Adviced `display-buffer' ignores `display-buffer-function'.
This means, that the adviced version of `display-buffer' ignores the value of
`display-buffer-function' when called for the `ecb-frame'. If this variable
should not be ignored then the function of `display-buffer-function' is
completely responsible which window is used for the buffer to display - no
smart ECB-logic will help to deal best with the ECB-window-layout! You can
define if and when `display-buffer-function' should be ignored:
- only when persistent compile window is used - i.e. if
  `ecb-compile-window-height' is not nil
- always when ECB is active - that means ignore when ECB is active otherwise
  not - this is the default value
- never, the adviced version of `display-buffer' always uses the value of
  `display-buffer-function' if the value is a function."
  :group 'ecb-layout
  :type '(radio (const :tag "When a persistent compile-window is used"
                       :value compile-window)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)))

(defsubst ecb-ignore-display-buffer-function ()
  (or (equal ecb-ignore-display-buffer-function 'always)
      (and (equal ecb-ignore-display-buffer-function 'compile-window)
           ecb-compile-window-height)))

(defcustom ecb-split-edit-window-after-start 'before-deactivation
  "*Sets how and if the edit window should be splitted after ECB-start.
But be aware: This option determines only if and how the edit-window should be
splitted at start-time of ECB. There are five different values allowed for
this option:
- nil: Do not split the edit-area of ECB after activation, i.e. there will be
  only one edit-window after starting ECB.
- 'horizontal: Split the edit-area in 2 edit-windows side by side.
- 'vertical: Split the edit-area in 2 edit-windows, one above the other.
- 'before-activation: Split the edit-area as before the ECB-start, i.e. the
  edit-area will have after start a window-layout as the whole frame had
  before the start of ECB.
- 'before-deactivation: Split the edit-area into a window-layout ECB had in
  its edit-area direct before the ECB-deactivation. This value preserves the
  full state between activations of ECB, i.e. the visibility of the
  ECB-windows, the visibility of a compile-window and also the full
  split-state of the edit-area. But this can only be done if important
  layout-options have not been changed in the meanwhile. These are the options
  `ecb-layout-name', `ecb-compile-window-height', `ecb-compile-window-width',
  `ecb-windows-width' and `ecb-windows-height'.

Default value is 'before-deactivation.

Some remarks to the value 'before-activation: If this value has been set then
ECB needs four permanent adivces even when ECB is deactivated: `split-window',
`delete-window', `delete-other-windows' and `set-window-configuration'. But
these advices do not change any behavior of these functions but only storing
in an internal ECB-variable the facts that a window has been splitted or
deleted etc. In addition to this these advices are 100% error-save, means the
functionality of the original functions will be performed in every\(!) case
even if within the advice an error occurs \(but normally there can no errors
occur in these advices because they are very simple). Conclusion: If you want
really all ECB-advices being disabled after deactivating ECB then you have to
set this option to other values then 'before-activation. But setting this
variable to this value is really completely save."
  :group 'ecb-layout
  :type '(radio (const :tag "Split as before ECB-start"
                       :value before-activation)
                (const :tag "Split as before last ECB-deactivation"
                       :value before-deactivation)
                (const :tag "Split horizontally"
                       :value horizontal)
                (const :tag "Split vertically"
                       :value vertical)
                (const :tag "Do not split"
                       :value nil)))

(defcustom ecb-windows-width 0.33
  "*The width of the ECB windows in columns for left- and right layouts.
If the number is less than 1.0 the width is a fraction of the frame width."
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type 'number)

(defcustom ecb-windows-height 0.33
  "*The height of the ECB windows in lines for top layouts.
If the number is less than 1.0 the width is a fraction of the frame height."
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type 'number)


(defcustom ecb-fix-window-size nil
  "*Fix size of the ECB-windows/buffers even after frame-resizing.
The fix type \(valid values are nil, t, width and height) can either be set on
a layout-basis \(means a different value for each layout) or one value can be
set for all layouts. For the latter case there is an additional value 'auto
which choose autom. the senseful fix-type depending on the current
layout-type: For top-layouts the fix-type 'height and for all other
layout-types the fix-type 'width.

For a detailed description of the valid values see documentation of
`window-size-fixed' which is newly introduced in GNU Emacs 21 and is only
available there. Therefore this option takes only effect with GNU Emacs >= 21.
This option has no effect with XEmacs because it does not support the feature
`window-size-fixed'.

Note1: Manually resizing the ECB-windows via `enlarge-window',
`shrink-window', `mouse-drag-vertical-line' and `mouse-drag-mode-line' is
still possible even if the window-sizes are fixed for frame-resizing!

Note2: The description of `window-size-fixed' in the elisp-info-manual is more
detailed than the description offered by \[C-h v]!

Note3: With Emacs < 22 there seems to be no distinction between 'width,
'height and t. Therefore this option takes no effect \(means all ecb-windows
have always unfixed sizes) with Emacs < 22 if `ecb-compile-window-height' is
not nil.

Per default no window-size fixing has been done."
  :group 'ecb-directories
  :initialize 'custom-initialize-default
  :set (function (lambda (sym value)
                   (set sym value)
                   (ecb-set-window-size-fixed
                    (ecb-get-window-fix-type ecb-layout-name))))
  :type '(radio (choice :tag "Fix type for all layouts"
                        :menu-tag "Fix type for all layouts"
                        (const :tag "Automatic" :value auto)
                        (const :tag "Fix only width" :value width)
                        (const :tag "Fix only height" :value height)
                        (const :tag "Fix both" :value t)
                        (const :tag "No fixing" :value nil))
                (repeat :tag "With these layouts"
                        (cons (string :tag "Layout name")
                              (choice :tag "Fix type"
                                      :menu-tag "Fix type for all layouts"
                                      (const :tag "Fix only width" :value width)
                                      (const :tag "Fix only height" :value height)
                                      (const :tag "Fix both" :value t)
                                      (const :tag "No fixing" :value nil))))))

(defun ecb-get-window-fix-type (layout-name)
  "Determine which value of `window-size-fixed' we must set in all ecb-buffers
of layout LAYOUT-NAME."
  (if (symbolp ecb-fix-window-size)
      (if (equal ecb-fix-window-size 'auto)
          (if (equal (ecb-get-layout-type ecb-layout-name) 'top)
              'height
            'width)
        ecb-fix-window-size)
    (cdr (assoc layout-name ecb-fix-window-size))))

(defun ecb-set-window-size-fixed (fix)
  "Set the buffer-local value of `window-size-fixed' in each visible
ecb-window to FIX. For Emacs < 22: If `ecb-compile-window-height' is not nil
then set always nil!"
  (unless ecb-running-xemacs
    (let ((l (ecb-canonical-ecb-windows-list)))
      (dolist (w l)
        (save-excursion
          (set-buffer (window-buffer w))
          (setq window-size-fixed (if (and (not ecb-running-version-22)
                                           ecb-compile-window-height)
                                      nil
                                    fix)))))))


(defmacro ecb-do-with-unfixed-ecb-buffers (&rest body)
  "Evaluate BODY with unfixed size of all current-visible ecb-buffers and
ensure that at the end \(either after finishing of BODY or after an error
occurs during BODY) all now current visible ecb-buffers get the value of their
buffer-local `window-size-fixed' from the setting in `ecb-fix-window-size'."
  `(unwind-protect
       (progn
         (ecb-set-window-size-fixed nil)
         ,@body)
     (ecb-set-window-size-fixed (ecb-get-window-fix-type ecb-layout-name))))

(defmacro ecb-do-with-fixed-ecb-buffers (&rest body)
  "Evaluate BODY with fixed size of all current-visible ecb-buffers and
ensure that at the end \(either after finishing of BODY or after an error
occurs during BODY) all now current visible ecb-buffers get the value of their
buffer-local `window-size-fixed' from the setting in `ecb-fix-window-size'."
  `(unwind-protect
       (progn
         (ecb-set-window-size-fixed t)
         ,@body)
     (ecb-set-window-size-fixed (ecb-get-window-fix-type ecb-layout-name))))



(defcustom ecb-other-window-behavior 'smart
  "*The behavior of ECB concerning getting an \"other window\".

The following settings are possible:

'all:

ECB will cycle through all windows of the ECB-frame or scroll simply
the next window in the ECB-frame, means it behaves like the original
`other-window' rsp. the original `other-window-for-scrolling'.

'only-edit:

ECB will only cycle through the edit-windows of ECB or only
scroll another edit-window. If the selected window is not an edit-window
then it behaves like with value 'all.

'edit-and-compile:

Like 'only-edit plus the compile window if any. If the
selected window is neither an edit-window nor the compile-window then it
behaves like with value 'all.

'smart:

With this setting ECB tries to choose the `other-window'-destination or the
\"other window\" to scroll in a smart and intuitive way: If point is in one of
the edit-windows and if the edit-area is splitted then always the \"next\"
edit-window is choosen \(whereas the next edit-window of the last edit-window
is the first edit-window)- if the edit-area is unsplitted then the
compile-window is used if there is one. In the context of an
`other-window'-call the ARG of `other-window' will be taken into account.

If one of the special ecb-windows is selected then always the \"next\"
ecb-window is choosen \(whereas the next ecb-window of the last ecb-window is
the first ecb-window). In the context of an `other-window'-call the ARG of
`other-window' will be taken into account. If there is only one ecb-window
then ECB considers also the edit-windows!

If the compile-window is selected then always the last selected edit-window
will be used unless `other-window' has been called with a prefix-argument
unequal 1.

If there is an active minibuffer:

Regardless of the allowed values above ECB handles the situation of an active
minibuffer during a call to `other-window' or `scroll-other-window' like
follows:

If the minibuffer-window is selected then ECB always chooses the window
`minibuffer-scroll-window' points to \(when this variable is set, otherwise
the compile-window or the last selected edit-window is choosen) when the
called command is called to choose the 1. next window \(always true for
scrolling another window or true when `other-window' called without prefix-arg
or with prefix-arg equal 1). Otherwise the window ARG steps away is choosen
\(in case of `other-window).

If there is an active minibuffer but the minibuffer-window is not selected
then `other-window' and `scroll-other-window' behave like the original
version.

In addition to the allowed values above the value of this option can also be a
function:

This function gets seven arguments:
1. A canonical list of all currently visible windows of the `ecb-frame'
2. A canonical list of all currently visible edit-windows
3. A canonical list of all currently visible ecb-windows
4. The window-object of the compile-window if there is any.
5. The minibuffer-window of the ECB-frame if there is an active minibuffer.
5. The result of the function `ecb-where-is-point' - see the documentation
   of this function for details.
6. An integer which indicates how many steps away from the current selected
   window the \"other-window\ is. Is nil when this function is called in
   another context then for `other-window'.
The function has to return a window-object which is then used as \"other
window\" for the command `other-window' or for scrolling another window
\(e.g. with `scroll-other-window').

This function has to handle all properly situations for itself.
`ecb-get-other-window-smart' is an example for such a function."
  :group 'ecb-layout
  :group 'ecb-most-important
  :type '(radio (const :tag "Smart" :value smart)
                (const :tag "All windows" all)
                (const :tag "Only edit windows" only-edit)
                (const :tag "Edit + compile window" edit-and-compile)
                (function :tag "User defined" :value ignore)))


(defcustom ecb-advice-window-functions-signal-error nil
  "*Signal an error if an adviced function can not do its job.
If not nil then an error is signaled if one of the adviced functions can not
do its job. So for example if the user tries to split the compile-window or an
ecb-tree-window or if one tries to switch to another buffer in one of the
ecb-tree-windows. For details see the documentation of each of the adviced
functions to get info when an error is signaled.

If this option is nil then no error is signaled but the called adviced
function does simply nothing.

Default is nil but it can also be useful to signal errors - so you see when
call a function in a situation which is not supported by this function."
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-layout-always-operate-in-edit-window
  '(switch-to-buffer)
  "*Adviced window functions work always in the edit-window.
If we are in an ECB special buffer (methods, directories, etc), and any of the
adviced windowing functions is called interactively, we will select first an
edit-window according to the value of `ecb-mouse-click-destination'. This is
useful if you have any functions that use such functions and you don't want
them to fail with an error complaining that the current buffer can not be
split, or something similar.

Because this may not be desirable in all situations and for all adviced
functions this can be enabled separately for function where it is senseful. If
the symbol of an adviced function is contained in the value of this option,
then the edit-window is first selected otherwise either an error is reported
or some other special reaction (depends on
`ecb-advice-window-functions-signal-error'); see the documentation of the
adviced functions for this.

Per default this is only enabled for `switch-to-buffer'."
  :group 'ecb-layout
  :type '(set (const :tag "delete-window"
                     :value delete-window)
              (const :tag "delete-other-windows"
                     :value delete-other-windows)
              (const :tag "split-window-horizontally"
                     :value split-window-horizontally)
              (const :tag "split-window-vertically"
                     :value split-window-vertically)
              (const :tag "split-window"
                     :value split-window)
              (const :tag "display-buffer"
                     :value display-buffer)
              (const :tag "switch-to-buffer"
                     :value switch-to-buffer)))

(defun ecb-canonical-ecb-windows-list (&optional winlist)
  "Return a list of all visible ECB-windows.

Such a window must be dedicated to its ecb-buffer and for the related buffer
a dedicator-function must be defined with `defecb-window-dedicator' so this
dedicator is registered for that ecb-buffer.
The list starts from the left-most top-most window in the order `other-window'
would walk through these windows."
  (let ((windows-list (or winlist (ecb-canonical-windows-list)))
        (registered-ecb-buffers (ecb-dedicated-special-buffers))
        )
    (delete nil (mapcar (function (lambda (elem)
                                    (if (and (not (memq elem
                                                        ecb-layout-temporary-dedicated-windows))
                                             (window-dedicated-p elem)
                                             (memq (window-buffer elem) registered-ecb-buffers)
                                             )
                                        elem)))
                        windows-list))))

(defun ecb-canonical-edit-windows-list (&optional winlist)
  "Return a list of all current edit-windows \(starting from the left-most
top-most window) in the order `other-window' would walk through these windows.
These are all windows in the `ecb-frame' which are not identical to the
compile-window and not identical to one of the visible ECB-windows."
  (let ((comp-win-state (ecb-compile-window-state))
        (windows-list (or winlist (ecb-canonical-windows-list))))
    (delete nil (mapcar (function (lambda (elem)
                                    (if (and (or (member elem
                                                         ecb-layout-temporary-dedicated-windows)
                                                 (not (window-dedicated-p elem)))
                                             (or (not (equal comp-win-state 'visible))
                                                 (not (equal elem ecb-compile-window))))
                                        elem)))
                        windows-list))))

(defcustom ecb-layout-window-sizes nil
  "*Specifies the sizes of the ECB windows for each layout.
The easiest way \(and also the strongly recommended way) to change this
variable is to change the window sizes by dragging the window borders using
the mouse and then store the window sizes by calling the command
`ecb-store-window-sizes'. Next time the layout is redrawn the values stored in
this option will be used.

If `ecb-store-window-sizes' is used then the windows sizes are stored per
default as fractions of current frame-width and -height of the ecb-frame, so
the stored values will \"work\" for other frame sizes too. But if you call
`ecb-store-window-sizes' with a prefix-argument then the fixed values of
current width and height are stored!

If this option is set \"by hand\" \(i.e. not by `ecb-store-window-sizes') then
the following is important:
- It is recommended to use fractions of frame-width and -height!.
- The order of the sequence of the inserted window sizes must be the same as
  `other-window' \(the not-adviced version!) would walk!"
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type '(repeat (cons :tag "Window layout"
                       (string :tag "Layout name")
                       (repeat :tag "Window sizes"
                               (cons (choice :tag "Width"
                                             :menu-tag "Width"
                                             :value 0.0
                                             (const :tag "Default value"
                                                    :value nil)
                                             (number :tag "Custom size"))
                                     (choice :tag "Height"
                                             :menu-tag "Height"
                                             (const :tag "Default value"
                                                    :value nil)
                                             (number :tag "Custom size")))))))

(defcustom ecb-redraw-layout-quickly nil
  "If non-nil, we will attempt to redraw the layout quickly.
Please read also carefully the documentation of `ecb-redraw-layout'."
  :type 'boolean
  :group 'ecb-layout)

(defcustom ecb-major-modes-show-or-hide (cons nil nil)
  "*List of major-modes which show or hide the ecb-windows.
The value is a cons-cell where the car contains all major-mode-symbols which
should show the special ecb-windows and the cdr contains all
major-mode-symbols which should hide the special ecb-windows. If the symbol of
a major-mode is neither contained in the car-\"show-list\" nor in the
cdr-\"hide-list\" then the visibility-state of the ecb-windows does not
change."
  :group 'ecb-layout
  :group 'ecb-most-important
  :type '(cons (repeat :tag "Modes for visible ecb-windows"
                       (symbol :tag "Major-mode"))
               (repeat :tag "Modes for invisible ecb-windows"
                       (symbol :tag "Major-mode"))))

(defcustom ecb-toggle-layout-sequence '("left9" "left14")
  "*Toggle sequence for layout toggling with `ecb-toggle-layout'.
Every element of this list has to be a valid layout-name \(a string) i.e.
either one of the predefined layouts or one of the user-defined layouts \(see
`ecb-create-new-layout').

You can add here as many layouts as you want but to use this option most
effective you should not add more than 2 or 3 layouts so every layout can be
accessed very fast by toggling with `ecb-toggle-layout'. It is also senseful
to add layouts which have the same principal outline, i.e. all their
tree-buffers are on the same side of the frame and the
tree-buffer-\"column\" \(or -\"row\") has identical size for the layouts.

Recommended values are for example:
- \(\"left10\" \"left15\"), toggles between methods and directories/history
- \(\"left10\" \"left13\"), toggles between methods and directories
- \(\"left10\" \"left14\"), toggles between methods and history
- \(\"left10\" \"left13\" \"left14\"), toggles between methods, history and
  directories

See also option `ecb-show-sources-in-directories-buffer'.

This option makes only sense if the value is a list with more than 1 element!"
  :group 'ecb-layout
  :type '(repeat (string :tag "Layout name."))
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   (ecb-load-layouts)
                   (dolist (name value)
                     (if (and (boundp 'ecb-minor-mode)
                              ecb-minor-mode
                              (not (fboundp (intern
                                             (format "ecb-layout-function-%s"
                                                     name)))))
                         (ecb-error "There is no layout available with name %s!"
                                    name)))
                   (set symbol value))))

(defcustom ecb-hide-ecb-windows-before-hook nil
  "*Hook run direct before the ECB windows will be hidden.
Hiding is done either by `ecb-toggle-ecb-windows' or `ecb-hide-ecb-windows'.
This means that at runtime of this hook all the ECB-tree-windows of current
layout are visible.

IMPORTANT: Showing the hidden ECB-windows is internally done by calling
`ecb-redraw-layout' and therefore also the hooks
`ecb-redraw-layout-before-hook' and `ecb-redraw-layout-after-hook' are
evaluated. The hook-sequence is analogous to that described in
`ecb-show-ecb-windows-before-hook'."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-hide-ecb-windows-after-hook nil
  "*Hooks run direct after the ECB windows have been hidden.
Hiding was done either by `ecb-toggle-ecb-windows' or `ecb-hide-ecb-windows'.

IMPORTANT: Showing the hidden ECB-windows is internally done by calling
`ecb-redraw-layout' and therefore also the hooks
`ecb-redraw-layout-before-hook' and `ecb-redraw-layout-after-hook' are
evaluated. The hook-sequence is analogous to that described in
`ecb-show-ecb-windows-after-hook'."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-show-ecb-windows-before-hook nil
  "*Hooks run direct before the ECB windows will be shown.
Showing is done either by `ecb-toggle-ecb-windows' or `ecb-show-ecb-windows'.
This means that at runtime of this hook the ECB-windows are still hidden.

IMPORTANT: Showing the hidden ECB-windows is internally done by calling
`ecb-redraw-layout' and therefore also the hooks
`ecb-redraw-layout-before-hook' and `ecb-redraw-layout-after-hook' are
evaluated. So there is the following sequence of hooks during the process of
showing the hidden ECB-windows:
1. `ecb-show-ecb-windows-before-hook'
2. `ecb-redraw-layout-before-hook'
3. <redrawing the layout to show the hidden ECB-windows>
4. `ecb-redraw-layout-after-hook'
5. `ecb-show-ecb-windows-after-hook'
So be aware which code you add to which hook!"
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-show-ecb-windows-after-hook nil
  "*Hooks run direct before the ECB windows will be shown.
Showing has been done either by `ecb-toggle-ecb-windows' or
`ecb-show-ecb-windows'. This means that at runtime of this hook the
ECB-windows are already visible.

IMPORTANT: Showing the hidden ECB-windows is internally done by calling
`ecb-redraw-layout' and therefore also the hooks
`ecb-redraw-layout-before-hook' and `ecb-redraw-layout-after-hook' are
evaluated. So there is the following sequence of hooks during the process of
showing the hidden ECB-windows:
1. `ecb-show-ecb-windows-before-hook'
2. `ecb-redraw-layout-before-hook'
3. <redrawing the layout to show the hidden ECB-windows>
4. `ecb-redraw-layout-after-hook'
5. `ecb-show-ecb-windows-after-hook'
So be aware which code you add to which hook!"
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-redraw-layout-after-hook '(ecb-eshell-recenter)
  "*Hooks run direct after the ECB-layout has been redrawn.
If you use the eshell-integration of ECB then the function
`ecb-eshell-recenter' should be in this hook."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-redraw-layout-before-hook nil
  "*Hooks run direct before the ECB-layout will be redrawn."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-layout-debug-mode nil
  "*Write debug-information about layout-operations in the Messages-buffer.
Normally there should be no need to set this option to true but if there are
problems to display buffers in the compile-window of ECB \(e.g. buffers which
should be displayed there aren't or the temporally enlarging-mechanism does
not do what you think it should do etc...) then please do the following steps:
1. Set `ecb-layout-debug-mode' to not nil
2. Reproduce the wrong behavior exactly by repeating all the operations which
   lead to the problem.
3. Now send immediately a bug report with `ecb-submit-problem-report'.
4. Set `ecb-layout-debug-mode' back to nil if you do not want further
   debugging output in the *Messages* buffer"
  :group 'ecb-layout
  :type 'boolean)


;; ====== internal variables ====================================

(defvar ecb-frame nil
  "Frame where ECB runs. This frame is only set if this variable is nil or the
value points to a dead frame. Deactivation and activation of ECB does not set
this variable to nil!")

(defvar ecb-edit-window nil
  "Only used internally by `ecb-redraw-layout-full'. Do not refer to this
variable because the value is not predictable!")

(defvar ecb-last-edit-window-with-point nil
  "The edit-window of ECB which had the point before an emacs-command is
done.")

(defvar ecb-last-source-buffer nil
  "The source-buffer of `ecb-last-edit-window-with-point'.")

(defvar ecb-last-compile-buffer-in-compile-window nil
  "The buffer in the compile-window before an emacs-command is done.")

(defvar ecb-compile-window nil
  "Window to display compile-output in.")

;; We need this absolute # of lines of the compile-window-height because if it
;; is specified by a fraction of the frame (e.g. 0.1) we need this information
;; at all places where the compile-window will be shrunken back to its
;; specified height and computing the height from the value of
;; `ecb-compile-window-height' will often be unequal to that height drawn by
;; `ecb-redraw-layout-full' ==> we store and use the height drawn by this
;; function.
(defvar ecb-compile-window-height-lines nil
  "Contains always the absolute number of lines \(incl. modeline) of the
compile-window direct after a full redraw. Only set by
`ecb-redraw-layout-full' and evaluated and used by *all* mechanisms of ECB
which shrink back the compile-window to its specified height without doing a
full redraw!!")

(defvar ecb-compile-window-was-selected-before-command nil
  "Not nil only if the `ecb-compile-window' was selected before most recent
command.")

(defvar ecb-layout-default-window-sizes nil
  "Contains the the sizes of the ecb-windows of the current layout exactly as
drawn by the layout-function \(see `ecb-redraw-layout-full').")

(defvar ecb-windows-hidden nil
  "Used with `ecb-toggle-ecb-windows'. If true the ECB windows are hidden. Do
not change this variable!")

(defvar ecb-special-ecb-buffers-of-current-layout nil
  "The list of special ecb-buffers of current-layout.")

(defvar ecb-ecb-buffer-name-selected-before-command nil
  "Not nil only if a special ecb-window was selected before most recent
command. If not nil it contains the buffer-name of this special ecb-buffer.")

(defvar ecb-layout-prevent-handle-ecb-window-selection nil
  "If not nil ECB will ignore in the post-command-hook auto. maximizing.")


(defvar ecb-last-major-mode nil)

(defecb-autocontrol/sync-function ecb-handle-major-mode-visibilty nil nil nil
  "Added to `post-command-hook' after loading the ecb-library. Handles the
value `ecb-major-modes-show-or-hide'. Because this hook of `post-command-hook'
does nothing if the major-mode has not changed there should be no
performance-problem!"
  ;; Klaus: I think we need this to prevent doing here (de)activation
  ;; immediately after the button-pressed event (which is a command) because
  ;; then a mysterious window-live-p error for the minibuffer-window occurs if
  ;; we click onto a file which deactivates ECB.
  ;; With this the (de)activation is first done after the button-released
  ;; event which is created by Emacs for every tree-buffer click and is bound
  ;; to a nop.
  ;; At least this is my current interpretation and it works :-)
  ;; TODO: detecting the real reason why this happens and fixing it.
  (if (and ecb-item-in-tree-buffer-selected
           (equal ecb-tree-mouse-action-trigger 'button-press))
      (setq ecb-item-in-tree-buffer-selected nil)
    ;; do nothing if major-mode has not been changed or if a minibuffer is
    ;; active or if now one of the ecb-buffers is active
    (when (and (not (> (minibuffer-depth) 0))
               (not (equal ecb-last-major-mode major-mode))
               (not (member (current-buffer)
                            (ecb-get-current-visible-ecb-buffers))))
      (let ((last-mode ecb-last-major-mode))
        (setq ecb-last-major-mode major-mode)
        (ignore-errors
          (cond ((member major-mode (car ecb-major-modes-show-or-hide))
                 (let ((edit-win-list (ecb-canonical-edit-windows-list)))
                   ;; the window must not be splitted or if splitted the last
                   ;; major-mode must be dired-mode
                   (when (or (not (ecb-edit-window-splitted edit-win-list))
                             (equal last-mode 'dired-mode))
                     (and (ecb-point-in-edit-window-number edit-win-list)
                          ecb-windows-hidden
                          (ecb-show-ecb-windows)))))
                ((member major-mode (cdr ecb-major-modes-show-or-hide))
                 (let ((edit-win-list (ecb-canonical-edit-windows-list)))
                   ;; the window must not be splitted or if splitted the last
                   ;; major-mode must be dired-mode
                   (when (or (not (ecb-edit-window-splitted edit-win-list))
                             (equal last-mode 'dired-mode))
                     (and (ecb-point-in-edit-window-number edit-win-list)
                          (not ecb-windows-hidden)
                          (ecb-hide-ecb-windows))))))))))
  )

(defun ecb-initialize-layout ()
  ;; We do not initialize the `ecb-frame'!
  (setq ecb-edit-window nil
        ecb-last-edit-window-with-point nil
        ecb-last-source-buffer nil
        ecb-last-compile-buffer-in-compile-window nil
        ecb-current-maximized-ecb-buffer-name nil
        ecb-cycle-ecb-buffer-state nil
        ecb-special-ecb-buffers-of-current-layout nil
        ecb-windows-hidden nil
        ecb-compile-window nil
        ecb-layout-prevent-handle-compile-window-selection nil
        ecb-layout-prevent-handle-ecb-window-selection nil
        ecb-ecb-buffer-name-selected-before-command nil
        ecb-compile-window-was-selected-before-command nil
        ecb-compile-window-height-lines nil)
  (ecb-window-config-cache-clear))

(defun ecb-layout-debug-error (&rest args)
  "Run ARGS through `format' and write it to the *Messages*-buffer."
  (when ecb-layout-debug-mode
    (message (concat (format "ECB %s layout debug [%s] " ecb-version
                             (format-time-string "%H:%M:%S"))
                     (apply 'format args)))))


(defun ecb-compile-window-live-p (&optional display-msg)
  "Return not nil when a compile-window is live and visible.
If optional arg DISPLAY-MSG is not nil then an informational message for the
user is displayed in the echo-area if no compile-window is visible."
  (if (and ecb-compile-window-height
           ecb-compile-window
           (window-live-p ecb-compile-window))
      t
    (if display-msg
        (message "No compile-window visible in current ECB-layout!"))
    nil))

(defun ecb-get-compile-window-buffer ()
  "Return the buffer currently displayed in the compile-window or nil if there
is no compile-window displayed."
  (if (ecb-compile-window-live-p)
      (window-buffer ecb-compile-window)))

;; Klaus Berndl <klaus.berndl@sdm.de>: This function is only there for
;; backward compatibility and is not needed for ECB-versions > 2.11
(defun ecb-edit-window-live-p ()
  "At least one edit-window is always alive."
  t)

(defun ecb-window-live-p (buffer-name)
  "Return not nil if buffer BUFFER-NAME is displayed in an active window."
  (and buffer-name (window-live-p (get-buffer-window buffer-name))))

;; ====== basic advices ======================================================

(defecb-advice-set ecb-layout-basic-adviced-functions
  "All functions  needed to be adviced for the layout-engine of ECB.")

(defecb-advice delete-frame around ecb-layout-basic-adviced-functions
  "If FRAME is equal to the ECB frame then the user will be asked if he want
to proceed. If yes then ECB will be deactivated before deleting FRAME. If ECB
is not activated or FRAME is not equal the ECB-frame then this advice is
either not activated or it behaves exactly like the original version!"
  (let ((frame (or (ad-get-arg 0) (selected-frame))))
    (if (and ecb-minor-mode
             (equal frame ecb-frame))
        (when (ecb-confirm "Attempt to delete the ECB-frame. ECB will be deactivated! Proceed? ")
	  (ecb-deactivate-internal) ;; deletes also the ecb-frame if not the only frame
	  ad-do-it)
      ad-do-it)))

(require 'compile)

;; GNU Emacs 21.4 has has a new variable `grep-window-height'. Cause of the
;; smart mechanism of the new compile-library of Emacs 21.4 which sets autom.
;; for all compilation-* variables local values in the derived modes (like
;; `grep-mode) according to the value of the equivalent variable in the
;; derived mode (e.g. if `grep-window-height' has a special value in
;; `grep-mode' then the new macro `define-compilation-mode' sets the local
;; value of `compilation-window-height' to the value of `grep-window-height').
;; we have not to deal with special variables like `grep-window-height' (see
;; `define-compilation-mode') in the functions `compilation-set-window-height'
;; and `ecb-toggle-compile-window-height'!
(defecb-advice compilation-set-window-height around ecb-layout-basic-adviced-functions
  "Makes the function compatible with ECB."
  (if (not (equal (window-frame (ad-get-arg 0)) ecb-frame))
      (ecb-with-original-basic-functions
       ad-do-it)
    (if (and (equal (ad-get-arg 0) ecb-compile-window)
             (member ecb-compile-window-temporally-enlarge
                     '(after-selection nil)))
        (ecb-toggle-compile-window-height -1)
      ;; we prevent to shrink the compile-window below
      ;; `ecb-compile-window-height'
      (let* ((comp-win-height-value (ecb-buffer-local-value
                                     'compilation-window-height
                                     (window-buffer (ad-get-arg 0))))
             (compilation-window-height (if (and ecb-compile-window-prevent-shrink-below-height
                                                 comp-win-height-value
                                                 ecb-compile-window-height-lines
                                                 (< comp-win-height-value
                                                    ecb-compile-window-height-lines))
                                            ecb-compile-window-height-lines
                                          comp-win-height-value)))
        (and compilation-window-height
             ;; Klaus Berndl <klaus.berndl@sdm.de>: we do nothing if an unsplitted
             ;; edit-window should be resized because this would fail (e.g. if
             ;; `pop-up-windows' is nil).
             (or (equal (ad-get-arg 0) ecb-compile-window)
                 (ecb-edit-window-splitted))
             ;; If window is alone in its frame, aside from a minibuffer,
             ;; don't change its height.
             (not (eq (ad-get-arg 0) (frame-root-window (window-frame (ad-get-arg 0)))))
             ;; This save-excursion prevents us from changing the current buffer,
             ;; which might not be the same as the selected window's buffer.
             (save-excursion
               (let ((w (selected-window)))
                 (ecb-layout-debug-error "compilation-set-window-height: window: %s, cur-win: %s, cur-height: %d"
                                         (ad-get-arg 0) w
                                         (ecb-window-full-height (ad-get-arg 0)))
                 (unwind-protect
                     (progn
                       (select-window (ad-get-arg 0))
                       (enlarge-window (- compilation-window-height
                                          (ecb-window-full-height))))
                   ;; The enlarge-window above may have deleted W, if
                   ;; compilation-window-height is large enough.
                   (when (window-live-p w)
                     (select-window w))))))))))

;; We need this advice only because the ugly implementation of Emacs:
;; `scroll-other-window' uses per default not the function
;; `other-window-for-scrolling'.
(defecb-advice scroll-other-window around ecb-layout-basic-adviced-functions
  "See the advice-documentation of `other-window-for-scrolling' to get all
details which window will be scrolled."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame))
          (and (equal (selected-window) (minibuffer-window ecb-frame))
               minibuffer-scroll-window))
      (ecb-with-original-basic-functions
       ad-do-it)
    (let* ((o-w (other-window-for-scrolling))
           (o-w-s-b (window-buffer o-w))
           (other-window-scroll-buffer (if (or (equal (current-buffer) o-w-s-b)
                                               (equal (minibuffer-window ecb-frame)
                                                      o-w))
                                           nil
                                         o-w-s-b)))
      ad-do-it)))


;; The following two advices are currently not activated because with XEmacs
;; we now set `progress-feedback-use-echo-area' to t which prevents this
;; window-resizing!
;; (defadvice find-file (around ecb)
;;   "Workaround for the annoying \(X)Emacs-behavior to resize some of the
;; special ecb-windows after opening a file. This advices restores the sizes of
;; the ecb-windows exactly as before this command."
;;   (if (and ecb-minor-mode
;;            (equal (selected-frame) ecb-frame)
;;            (not ecb-windows-hidden))
;;       (let ((ecb-sizes-before (ecb-get-ecb-window-sizes t)))
;;         ad-do-it
;;         ;; this seems to be necessary - otherwise the reszing seems not to
;;         ;; take effect...
;;         (sit-for 0)
;;         (ignore-errors (ecb-set-ecb-window-sizes ecb-sizes-before)))
;;     ad-do-it))
      
;; (defadvice find-file-other-window (around ecb)
;;   "Workaround for the annoying \(X)Emacs-behavior to resize some of the
;; special ecb-windows after opening a file. This advices restores the sizes of
;; the ecb-windows exactly as before this command."
;;   (if (and ecb-minor-mode
;;            (equal (selected-frame) ecb-frame)
;;            (not ecb-windows-hidden))
;;       (let ((ecb-sizes-before (ecb-get-ecb-window-sizes t)))
;;         ad-do-it
;;         ;; this seems to be necessary - otherwise the reszing seems not to
;;         ;; take effect...
;;         (sit-for 0)
;;         (ignore-errors (ecb-set-ecb-window-sizes ecb-sizes-before)))
;;     ad-do-it))

(defun ecb-toggle-scroll-other-window-scrolls-compile (&optional arg)
  "Toggle the state of `ecb-scroll-other-window-scrolls-compile-window'.
With prefix argument ARG, set it to t, otherwise to nil. For all details about
the scroll-behavior of `scroll-other-window' see the advice documentation of
`other-window-for-scrolling'."
  (interactive "P")
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (setq ecb-scroll-other-window-scrolls-compile-window
          (if (null arg)
              (not ecb-scroll-other-window-scrolls-compile-window)
            (>= (prefix-numeric-value arg) 0)))
    (message "Scrolling the other-window scrolls compile-window is now %s."
             (if ecb-scroll-other-window-scrolls-compile-window "ON" "OFF"))))
      

  
(defun ecb-edit-window-splitted (&optional edit-windows-list)
  "Returns either nil if the ECB edit-window is not splitted or 'vertical or
'horizontal when splitted in two windows \(depending on the splitting) or
'splitted if splitted in more than two windows. If EDIT-WINDOWS-LIST is not
nil then it must be a current list of edit-windows \(got by
`ecb-canonical-edit-windows-list'). If EDIT-WINDOWS-LIST is nil then a new
edit-window-list is computed via `ecb-canonical-edit-windows-list'."
  (let ((edit-win-list (or edit-windows-list (ecb-canonical-edit-windows-list))))
    (cond ((null edit-win-list)
           (ecb-error "Internal error - redraw the layout!"))
          ((= (length edit-win-list) 1)
           nil)
          ((= (length edit-win-list) 2)
           (if (= (car (ecb-window-edges (car edit-win-list)))
                  (car (ecb-window-edges (cadr edit-win-list))))
               'vertical
             'horizontal))
          (t 'splitted))))

(defvar ecb-temp-buffer-shrink-to-fit nil
  "Workaround for XEmacs-version which have a `display-buffer' with only 3
arguments. Do never set this variable; it is only set by
`show-temp-buffer-in-current-frame'!")

(when-ecb-running-xemacs
 ;; We advice this function to exactly that version of XEmacs 21.4.13.
 ;; For that XEmacs-version (and higher) this would not be necessary but
 ;; we need this advice for versions of XEmacs which do not have the
 ;; 4-argument-version of `display-buffer'. With this advice we give
 ;; older XEmacsen the newest display-buffer- and
 ;; shrink-to-fit-mechanism. How this is done is described at beginning
 ;; of `ecb-display-buffer-xemacs'.

 ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: XEmacs crashes in the following
 ;; scenario:
 ;; 1. acticate ECB
 ;; 2. open a file
 ;; 3. no compile-window active
 ;; 4. set temp-buffer-shrink-to-fit to t
 ;; 5. Split edit-window vertically
 ;; 6. call from the above window help (e.g. for function insert)
 ;; 7. insert help is displayed in the bottom window with shrinked window,
 ;;    point stay in the help-window
 ;; 8. press q --> the previous window-layout is restored (both window equally
 ;;    high); point stays in the top-window.
 ;; 9. now call C-x 1 (delete-other-windows) --> XEmacs crashes
 ;; It crashes not if temp-buffer-shrink-to-fit is nil (see step 4 above)
 ;; Solution: seems that with current shrink-window-if-larger-than-buffer
 ;; (s.b.) the crash has been gone.......
 
 (defecb-advice shrink-window-if-larger-than-buffer around ecb-layout-basic-adviced-functions
   "Makes the function compatible with ECB."
   (or (ad-get-arg 0) (ad-set-arg 0 (selected-window)))
   (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: window: %s"
                           (ad-get-arg 0))
   (if (or (not ecb-minor-mode)
           (not (equal (window-frame (ad-get-arg 0)) ecb-frame))
           (member (ad-get-arg 0) (ecb-canonical-ecb-windows-list)))
       (ecb-with-original-basic-functions
        ad-do-it)

     ;; now:
     ;; - the window to shrink is in the ecb-frame
     ;; - this window is not a special ecb-window

     ;; we handle only the edit-windows and the compile-window of the
     ;; ecb-frame in a special manner.

     ;; if called non-interactively (e.g. by `display-buffer's forth
     ;; argument SHRINK-TO-FIT) and if called for the compile-window of
     ;; ECB and if `ecb-compile-window-temporally-enlarge' is either
     ;; after-selection or nil then we shrink to the
     ;; ecb-compile-window-height! Otherwise we run the normal job!
     (if (and (not (interactive-p))
              (equal (ad-get-arg 0) ecb-compile-window)
              (member ecb-compile-window-temporally-enlarge
                      '(after-selection nil))
              ;; The *Completions*-buffer must always being enlarged!
              (not (ecb-string= (buffer-name (window-buffer (ad-get-arg 0)))
                                "*Completions*")))
         (ecb-toggle-compile-window-height -1)
       (save-excursion
         (set-buffer (window-buffer (ad-get-arg 0)))
         (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: buffer to shrink: %s"
                                 (current-buffer))
         ;; we prevent to shrink the compile-window below
         ;; `ecb-compile-window-height'
         (let ((window-min-height (if (and (equal (ad-get-arg 0) ecb-compile-window)
                                           (and ecb-compile-window-prevent-shrink-below-height
                                                (not (interactive-p)))
                                           window-min-height
                                           ecb-compile-window-height-lines
                                           (< window-min-height
                                              ecb-compile-window-height-lines))
                                      ecb-compile-window-height-lines
                                    window-min-height))
               (mini (frame-property (window-frame (ad-get-arg 0)) 'minibuffer)))
           (when (and (let ((frame (selected-frame)))
                        (select-frame (window-frame (ad-get-arg 0)))
                        (unwind-protect
                            (not (one-window-p t))
                          (select-frame frame)))
                      (ecb-window-safely-shrinkable-p (ad-get-arg 0))
                      (pos-visible-in-window-p (point-min) (ad-get-arg 0))
                      (not (eq mini 'only)))
             (ecb-fit-window-to-buffer (ad-get-arg 0)
                                       (ecb-window-full-height (ad-get-arg 0)))))))))
      
 (defecb-advice pop-to-buffer around ecb-layout-basic-adviced-functions
   "Chooses the window with the ECB-adviced version of `display-buffer'."
   ad-do-it
   (when (and (equal (selected-frame) ecb-frame)
              (ecb-point-in-compile-window))
     ;; we set the height of the compile-window according to
     ;; `ecb-enlarged-compilation-window-max-height'
     (ecb-set-compile-window-height)))
      
 ) ;; end of if-ecb-running-xemacs

(when-ecb-running-emacs
 ;; only GNU Emacs basic advices
 (defecb-advice mouse-drag-vertical-line around ecb-layout-basic-adviced-functions
   "Allows manually window-resizing even if `ecb-fix-window-size' is not nil
for current layout."
   (if (and ecb-minor-mode
            (equal (selected-frame) ecb-frame)
            (not ecb-windows-hidden)
            (ecb-get-window-fix-type ecb-layout-name))
       (ecb-do-with-unfixed-ecb-buffers ad-do-it)
     ad-do-it))


 (defecb-advice mouse-drag-mode-line around ecb-layout-basic-adviced-functions
   "Allows manually window-resizing even if `ecb-fix-window-size' is not nil
for current layout."
   (if (and ecb-minor-mode
            (equal (selected-frame) ecb-frame)
            (not ecb-windows-hidden)
            (ecb-get-window-fix-type ecb-layout-name)
            (member (car (car (cdr (ad-get-arg 0)))) ;; the window of the event
                    (ecb-canonical-ecb-windows-list)))
       (ecb-do-with-unfixed-ecb-buffers ad-do-it)
     ad-do-it))

 (defecb-advice enlarge-window around ecb-layout-basic-adviced-functions
   "Allows manually window-resizing even if `ecb-fix-window-size' is not nil
for current layout."
   (if (and ecb-minor-mode
            (equal (selected-frame) ecb-frame)
            (not ecb-windows-hidden)
            (ecb-get-window-fix-type ecb-layout-name)
            (member (selected-window) (ecb-canonical-ecb-windows-list)))
       (ecb-do-with-unfixed-ecb-buffers ad-do-it)
     ad-do-it))

 (defecb-advice shrink-window around ecb-layout-basic-adviced-functions
   "Allows manually window-resizing even if `ecb-fix-window-size' is not nil
for current layout."
   (if (and ecb-minor-mode
            (equal (selected-frame) ecb-frame)
            (not ecb-windows-hidden)
            ;; See comment of defecb-advice for mouse-drag-mode-line
            (ecb-get-window-fix-type ecb-layout-name)
            (member (selected-window) (ecb-canonical-ecb-windows-list)))
       (ecb-do-with-unfixed-ecb-buffers ad-do-it)
     ad-do-it))

 (defecb-advice shrink-window-if-larger-than-buffer around ecb-layout-basic-adviced-functions
   "Makes the function compatible with ECB."
   (or (ad-get-arg 0) (ad-set-arg 0 (selected-window)))
   (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: window: %s"
                           (ad-get-arg 0))
   (if (or (not ecb-minor-mode)
           (not (equal (window-frame (ad-get-arg 0)) ecb-frame))
           (member (ad-get-arg 0) (ecb-canonical-ecb-windows-list)))
       (ecb-with-original-basic-functions
        ad-do-it)
     (save-selected-window
       (select-window (ad-get-arg 0))
       (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: buffer to shrink: %s"
                               (current-buffer))
       (let* ((params (frame-parameters))
              (mini (cdr (assq 'minibuffer params)))
              (edges (ecb-window-edges))
              ;; we prevent to shrink the compile-window below
              ;; `ecb-compile-window-height'
              (window-min-height (if (and (equal (ad-get-arg 0) ecb-compile-window)
                                          (and ecb-compile-window-prevent-shrink-below-height
                                               (not (interactive-p)))
                                          window-min-height
                                          ecb-compile-window-height-lines
                                          (< window-min-height
                                             ecb-compile-window-height-lines))
                                     ecb-compile-window-height-lines
                                   window-min-height)))
         (if (and (let ((frame (selected-frame)))
                    (select-frame (window-frame (ad-get-arg 0)))
                    (unwind-protect
                        (not (one-window-p t))
                      (select-frame frame)))
                  (ecb-window-safely-shrinkable-p (ad-get-arg 0))
                  (pos-visible-in-window-p (point-min) (ad-get-arg 0))
                  (not (eq mini 'only))
                  (or (not mini)
                      (< (nth 3 edges) (nth 1 (ecb-window-edges mini)))
                      (> (nth 1 edges) (cdr (assq 'menu-bar-lines params)))))
             (ecb-fit-window-to-buffer (ad-get-arg 0)
                                       (ecb-window-full-height (ad-get-arg 0))))))))

 (defecb-advice resize-temp-buffer-window around ecb-layout-basic-adviced-functions
   "Makes the function compatible with ECB."
   (ecb-layout-debug-error "resize-temp-buffer-window: buffer: %s, window: %s, frame: %s"
                           (current-buffer) (selected-window) (selected-frame))
   (if (or (not ecb-minor-mode)
           (not (equal (selected-frame) ecb-frame))
           (equal (car (ecb-where-is-point)) 'ecb))
       (ecb-with-original-basic-functions
        ad-do-it)
     (if (and (equal (selected-window) ecb-compile-window)
              (member ecb-compile-window-temporally-enlarge
                      '(after-selection nil))
              ;; The *Completions* buffer must always being enlarged!!
              (not (ecb-string= (buffer-name (current-buffer)) "*Completions*")))
         (progn
           (ecb-layout-debug-error "resize-temp-buffer-window: buffer: shrink to comp-win-height")
           (ecb-toggle-compile-window-height -1))
       ;; we prevent to shrink the compile-window below
       ;; `ecb-compile-window-height'
       (let ((window-min-height (if (and window-min-height
                                         ecb-compile-window-height-lines
                                         ecb-compile-window-prevent-shrink-below-height
                                         (< window-min-height
                                            ecb-compile-window-height-lines))
                                    ecb-compile-window-height-lines
                                  window-min-height)))
         (ecb-layout-debug-error "resize-temp-buffer-window: within let - window: %s"
                                 (selected-window))
         (ecb-layout-debug-error "resize-temp-buffer-window: check unless %s, %s, %s"
                                 (one-window-p 'nomini)
                                 (and (not (equal (selected-window) ecb-compile-window))
                                      (not (ecb-edit-window-splitted)))
                                 (not (pos-visible-in-window-p (point-min))))
         (unless (or (one-window-p 'nomini)
                     ;; Klaus Berndl <klaus.berndl@sdm.de>: we do nothing if an unsplitted
                     ;; edit-window should be resized because this would fail (e.g. if
                     ;; `pop-up-windows' is nil)
                     (and (not (equal (selected-window) ecb-compile-window))
                          (not (ecb-edit-window-splitted)))
                     (not (pos-visible-in-window-p (point-min)))
                     )
           (ecb-layout-debug-error "resize-temp-buffer-window: resize buffer: %s"
                                   (current-buffer))
           (ecb-fit-window-to-buffer
            (selected-window)
            (if (functionp temp-buffer-max-height)
                (funcall temp-buffer-max-height (current-buffer))
              temp-buffer-max-height)))))))
 
 (defecb-advice pop-to-buffer around ecb-layout-basic-adviced-functions
   "Chooses the window with the ECB-adviced version of `display-buffer'."
   (if (or (not ecb-minor-mode)
           (null (ad-get-arg 0)))
       (ecb-with-original-basic-functions
        ad-do-it)
     (condition-case nil
         (progn
           (ecb-layout-debug-error "pop-to-buffer: buffer: %s, %s"
                                   (ad-get-arg 0) (ad-get-arg 1))
           (select-window (display-buffer (ad-get-arg 0)
                                          (ad-get-arg 1)))
           ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Do we need this?!
           (set-buffer (ad-get-arg 0))
           (if (ad-get-arg 2)
               ;; not the best solution but for now....
               (bury-buffer (ad-get-arg 0))))
       ;; This is if the call to the adviced `display-buffer' fails (seems
       ;; to make problems with C-h i and the *info*-buffer). Then we run the
       ;; orginal version.
       (error
        (ecb-layout-debug-error "pop-to-buffer: adviced version failed for buffer: %s, %s"
                                (ad-get-arg 0) (ad-get-arg 1))
        (if (ecb-buffer-is-dedicated-special-buffer-p (ad-get-arg 0))
            (ecb-error "Can not go to a invisible special ECB-buffer!")
          ad-do-it)))
     (when (ecb-point-in-compile-window)
       ;; we set the height of the compile-window according to
       ;; `ecb-enlarged-compilation-window-max-height'
       (ecb-set-compile-window-height)))))
  

;; Klaus Berndl <klaus.berndl@sdm.de>: Fixes a bug with ths
;; shrink-to-fit stuff: (set-window-buffer ...) has to be called BEFORE
;; `shrink-window-if-larger-than-buffer'! The rest is identical with the
;; version from window-xemacs.el of XEmacs 21.4.13. Shrinking in an
;; after advice does not work - fails for the first call for
;; `display-buffer' in case of an temp-buffer!

;; Klaus Berndl <klaus.berndl@sdm.de>: Some older versions of XEmacs
;; 21.4 does not support a four-argument display-buffer. We handle this case
;; properly within `ecb-display-buffer-xemacs' and
;; `show-temp-buffer-in-current-frame'; see the comments in both of these
;; functions.
(defun ecb-display-buffer-xemacs (buffer &optional not-this-window-p
                                         override-frame
                                         shrink-to-fit)
  "Make BUFFER appear in some window on the current frame, but don't select it.
BUFFER can be a buffer or a buffer name.
If BUFFER is shown already in some window in the current frame,
just uses that one, unless the window is the selected window and
NOT-THIS-WINDOW-P is non-nil (interactively, with prefix arg).

If BUFFER has a dedicated frame, display on that frame instead of
the current frame, unless OVERRIDE-FRAME is non-nil.

If OVERRIDE-FRAME is non-nil, display on that frame instead of
the current frame (or the dedicated frame).

If SHRINK-TO-FIT is non-nil and splitting the window is appropriate, give
the new buffer less than half the space if it is small enough to fit.

If `pop-up-windows' is non-nil, always use the
current frame and create a new window regardless of whether the
buffer has a dedicated frame, and regardless of whether
OVERRIDE-FRAME was specified.

If `pop-up-frames' is non-nil, make a new frame if no window shows BUFFER.

Returns the window displaying BUFFER."
  (interactive "BDisplay buffer:\nP")

  (ecb-layout-debug-error "ecb-display-buffer-xemacs for %s %s %s %s [%s]"
                          buffer not-this-window-p override-frame
                          shrink-to-fit ecb-temp-buffer-shrink-to-fit)
  ;; Here we make the shrink-to-fit workaround for these old XEmacs 21.4
  ;; versions which do not have a display-buffer-command with 4 arguments:
  ;; `ecb-temp-buffer-shrink-to-fit' is only set by
  ;; `show-temp-buffer-in-current-frame' (an XEmacs-only function) to the
  ;; value of `temp-buffer-shrink-to-fit' and can only be set to not nil
  ;; there. This is done be a let-binding so `ecb-temp-buffer-shrink-to-fit'
  ;; has always nil after finishing this function! Only if
  ;; `ecb-temp-buffer-shrink-to-fit' is not nil here we override the value of
  ;; SHRINK-TO-FIT. And because  `ecb-temp-buffer-shrink-to-fit' is only set
  ;; to not nil in case of a wrong-number-of-arguments error for
  ;; `display-buffer' (see `show-temp-buffer-in-current-frame') this will
  ;; never take place for XEmacs-versions with the 4-arg version of
  ;; `display-buffer'!
  (setq shrink-to-fit (or ecb-temp-buffer-shrink-to-fit shrink-to-fit))
  (let ((wconfig (current-window-configuration))
        (result
         ;; We just simulate a `return' in C.  This function is way ugly
         ;; and does `returns' all over the place and there's no sense
         ;; in trying to rewrite it to be more Lispy.
         (catch 'done
           (let (window old-frame target-frame explicit-frame shrink-it)
             (setq old-frame (or (last-nonminibuf-frame) (selected-frame)))
             (setq buffer (get-buffer buffer))
             (check-argument-type 'bufferp buffer)

             ;; KB: For pre-display-buffer-function and
             ;; display-buffer-function we have to check for
             ;; wrong-number-of-arguments errors because older XEmacs 21.4
             ;; does not support the SHRINK-TO-FIT-argument and therefore
             ;; these functions probably don't too. Therefore in case of this
             ;; error we call these functions again with only the first three
             ;; arguments.
             
             (setq explicit-frame
                   (if pre-display-buffer-function
                       (condition-case oops
                           (funcall pre-display-buffer-function buffer
                                    not-this-window-p
                                    override-frame
                                    shrink-to-fit)
                         (wrong-number-of-arguments
                          (funcall pre-display-buffer-function buffer
                                   not-this-window-p
                                   override-frame))
                         (error (signal (car oops) (cdr oops)))
                         (quit (signal 'quit nil)))))

             ;; Give the user the ability to completely reimplement
             ;; this function via the `display-buffer-function'.
             (if display-buffer-function
                 (throw 'done
                        (condition-case oops
                            (funcall display-buffer-function buffer
                                     not-this-window-p
                                     override-frame
                                     shrink-to-fit)
                          (wrong-number-of-arguments
                           (funcall display-buffer-function buffer
                                    not-this-window-p
                                    override-frame))
                          (error (signal (car oops) (cdr oops)))
                          (quit (signal 'quit nil)))))

             ;; If the buffer has a dedicated frame, that takes
             ;; precedence over the current frame, and over what the
             ;; pre-display-buffer-function did.
             (let ((dedi (buffer-dedicated-frame buffer)))
               (if (frame-live-p dedi) (setq explicit-frame dedi)))

             ;; if override-frame is supplied, that takes precedence over
             ;; everything.  This is gonna look bad if the
             ;; pre-display-buffer-function raised some other frame
             ;; already.
             (if override-frame
                 (progn
                   (check-argument-type 'frame-live-p override-frame)
                   (setq explicit-frame override-frame)))

             (setq target-frame
                   (or explicit-frame
                       (last-nonminibuf-frame)
                       (selected-frame)))

             ;; If we have switched frames, then set not-this-window-p
             ;; to false.  Switching frames means that selected-window
             ;; is no longer the same as it was on entry -- it's the
             ;; selected-window of target_frame instead of old_frame,
             ;; so it's a fine candidate for display.
             (if (not (eq old-frame target-frame))
                 (setq not-this-window-p nil))

             ;; if it's in the selected window, and that's ok, then we're done.
             (if (and (not not-this-window-p)
                      (eq buffer (window-buffer (selected-window))))
                 (throw 'done (display-buffer-1 (selected-window))))

             ;; See if the user has specified this buffer should appear
             ;; in the selected window.

             (if not-this-window-p
                 nil

               (if (or (member (buffer-name buffer) same-window-buffer-names)
                       (assoc (buffer-name buffer) same-window-buffer-names))
                   (progn
                     (switch-to-buffer buffer)
                     (throw 'done (display-buffer-1 (selected-window)))))

               (let ((tem same-window-regexps))
                 (while tem
                   (let ((car (car tem)))
                     (if (save-match-data
                           (or
                            (and (stringp car)
                                 (string-match car (buffer-name buffer)))
                            (and (consp car) (stringp (car car))
                                 (string-match (car car) (buffer-name buffer)))))
                         (progn
                           (switch-to-buffer buffer)
                           (throw 'done (display-buffer-1
                                         (selected-window))))))
                   (setq tem (cdr tem)))))

             ;; If pop-up-frames, look for a window showing BUFFER on
             ;; any visible or iconified frame.  Otherwise search only
             ;; the current frame.
             (if (and (not explicit-frame)
                      (or pop-up-frames (not (last-nonminibuf-frame))))
                 (setq target-frame 0))

             ;; Otherwise, find some window that it's already in, and
             ;; return that, unless that window is the selected window
             ;; and that isn't ok.  What a contorted mess!
             (setq window (or (if (not explicit-frame)
                                  ;; search the selected frame
                                  ;; first if the user didn't
                                  ;; specify an explicit frame.
                                  (get-buffer-window buffer nil))
                              (get-buffer-window buffer target-frame)))
             (if (and window
                      (or (not not-this-window-p)
                          (not (eq window (selected-window)))))
                 (throw 'done (display-buffer-1 window)))

             ;; Certain buffer names get special handling.
             (if special-display-function
                 (progn
                   (if (member (buffer-name buffer)
                               special-display-buffer-names)
                       (throw 'done (funcall special-display-function buffer)))

                   (let ((tem (assoc (buffer-name buffer)
                                     special-display-buffer-names)))
                     (if tem
                         (throw 'done (funcall special-display-function
                                               buffer (cdr tem)))))

                   (let ((tem special-display-regexps))
                     (while tem
                       (let ((car (car tem)))
                         (if (and (stringp car)
                                  (save-match-data (string-match car (buffer-name buffer))))
                             (throw 'done
                                    (funcall special-display-function buffer)))
                         (if (and (consp car)
                                  (stringp (car car))
                                  (save-match-data
                                    (string-match (car car)
                                                  (buffer-name buffer))))
                             (throw 'done (funcall
                                           special-display-function buffer
                                           (cdr car)))))
                       (setq tem (cdr tem))))))

             ;; If there are no frames open that have more than a minibuffer,
             ;; we need to create a new frame.
             (if (or pop-up-frames
                     (null (last-nonminibuf-frame)))
                 (progn
                   (setq window (frame-selected-window
                                 (funcall pop-up-frame-function)))
                   (set-window-buffer window buffer)
                   (throw 'done (display-buffer-1 window))))

             ;; Otherwise, make it be in some window, splitting if
             ;; appropriate/possible.  Do not split a window if we are
             ;; displaying the buffer in a different frame than that which
             ;; was current when we were called.  (It is already in a
             ;; different window by virtue of being in another frame.)
             (if (or (and pop-up-windows (eq target-frame old-frame))
                     (eq 'only (frame-property (selected-frame) 'minibuffer))
                     ;; If the current frame is a special display frame,
                     ;; don't try to reuse its windows.
                     (window-dedicated-p (frame-root-window (selected-frame))))
                 (progn
                   (if (eq 'only (frame-property (selected-frame) 'minibuffer))
                       (setq target-frame (last-nonminibuf-frame)))

                   ;; Don't try to create a window if would get an error with
                   ;; height.
                   (if (< split-height-threshold (* 2 window-min-height))
                       (setq split-height-threshold (* 2 window-min-height)))

                   ;; Same with width.
                   (if (< split-width-threshold (* 2 window-min-width))
                       (setq split-width-threshold (* 2 window-min-width)))

                   ;; If the frame we would try to split cannot be split,
                   ;; try other frames.
                   (if (frame-property (if (null target-frame)
                                           (selected-frame)
                                         (last-nonminibuf-frame))
                                       'unsplittable)
                       (setq window
                             ;; Try visible frames first.
                             (or (get-largest-window 'visible)
                                 ;; If that didn't work, try iconified frames.
                                 (get-largest-window 0)
                                 (get-largest-window t)))
                     (setq window (get-largest-window target-frame)))

                   ;; If we got a tall enough full-width window that
                   ;; can be split, split it.
                   (if (and window
                            (not (frame-property (window-frame window)
                                                 'unsplittable))
                            (>= (window-height window) split-height-threshold)
                            (or (>= (window-width window)
                                    split-width-threshold)
                                (and (window-leftmost-p window)
                                     (window-rightmost-p window))))
                       (setq window (split-window window))
                     (let (upper
                           ;;			   lower
                           other)
                       (setq window (get-lru-window target-frame))
                       ;; If the LRU window is selected, and big enough,
                       ;; and can be split, split it.
                       (if (and window
                                (not (frame-property (window-frame window)
                                                     'unsplittable))
                                (or (eq window (selected-window))
                                    (not (window-parent window)))
                                (>= (window-height window)
                                    (* 2 window-min-height)))
                           (setq window (split-window window)))
                       ;; If get-lru-window returned nil, try other approaches.
                       ;; Try visible frames first.
                       (or window
                           (setq window (or (get-largest-window 'visible)
                                            ;; If that didn't work, try
                                            ;; iconified frames.
                                            (get-largest-window 0)
                                            ;; Try invisible frames.
                                            (get-largest-window t)
                                            ;; As a last resort, make
                                            ;; a new frame.
                                            (frame-selected-window
                                             (funcall
                                              pop-up-frame-function)))))
                       ;; If window appears above or below another,
                       ;; even out their heights.
                       (if (window-previous-child window)
                           (setq other (window-previous-child window)
                                 ;;				 lower window
                                 upper other))
                       (if (window-next-child window)
                           (setq other (window-next-child window)
                                 ;;				 lower other
                                 upper window))
                       ;; Check that OTHER and WINDOW are vertically arrayed.
                       (if (and other
                                (not (= (nth 1 (window-pixel-edges other))
                                        (nth 1 (window-pixel-edges window))))
                                (> (window-pixel-height other)
                                   (window-pixel-height window)))
                           ;; Klaus Berndl <klaus.berndl@sdm.de>: here we
                           ;; write this as somehow clumsy code to silence the
                           ;; byte-compiler because GNU Emacs <= 21.3. knows
                           ;; only 2 args for `enlarge-window'
                           (funcall (symbol-function 'enlarge-window)
                                    (- (/ (+ (window-height other)
                                             (window-height window))
                                          2)
                                       (window-height upper))
                                    nil upper))
                       ;; Klaus Berndl <klaus.berndl@sdm.de>: Only in
                       ;; this situation we shrink-to-fit but we can do
                       ;; this first after we have displayed buffer in
                       ;; window (s.b. (set-window-buffer window buffer))
                       (setq shrink-it shrink-to-fit))))

               (setq window (get-lru-window target-frame)))

             ;; Bring the window's previous buffer to the top of the MRU chain.
             (if (window-buffer window)
                 (save-excursion
                   (save-selected-window
                     (select-window window)
                     (record-buffer (window-buffer window)))))

             (set-window-buffer window buffer)

             ;; Now window's previous buffer has been brought to the top
             ;; of the MRU chain and window displays buffer - now we can
             ;; shrink-to-fit if necessary
             (if shrink-it
                 (shrink-window-if-larger-than-buffer window))
                   
             (display-buffer-1 window)))))
    (or (equal wconfig (current-window-configuration))
        (push-window-configuration wconfig))
    result))

(defun ecb-temp-buffer-show-function-emacs (buf)
  ;; cause of running the hooks in `temp-buffer-show-hook' we must use
  ;; save-selected-window (s.b.). But maybe `display-buffer' calls
  ;; `ecb-toggle-compile-window' which completely destroy all windows and
  ;; redraw the layout. This conflicts with the save-selected-window.
  ;; Therefore we toggle the compile-window before the save-selected-window!
  (when (ecb-compilation-buffer-p buf)
    (ecb-layout-debug-error "ecb-temp-buffer-show-function-emacs: comp-buffer: %s"
                            buf)
    (when (and (equal (selected-frame) ecb-frame)
               (equal 'hidden (ecb-compile-window-state))
               ;; calling this from minibuffer (e.g. completions)
               ;; seems to cause problems
               (not (equal (minibuffer-window ecb-frame) (selected-window))))
      (ecb-layout-debug-error "ecb-temp-buffer-show-function-emacs: comp-win will toggled")
      (ecb-toggle-compile-window 1)))
  (save-selected-window
    (save-excursion
      ;; this call to `display-buffer' runs the adviced version of ECB which
      ;; always handles all the compile-window stuff if buf is a
      ;; compile-buffer in the sense of `ecb-compilation-buffer-p'.
      (let ((win (display-buffer buf)))
        (ecb-layout-debug-error "ecb-temp-buffer-show-function-emacs: win: %s, buf: %s,dedi:%s"
                                win buf (window-dedicated-p win))
        (select-window win)
        (setq minibuffer-scroll-window win)
        (set-buffer buf)
        (run-hooks 'temp-buffer-show-hook)
        (ecb-layout-debug-error "ecb-temp-buffer-show-function-emacs: sel-win: %s, win-buf: %s,dedi%s"
                                (selected-window)
                                (window-buffer (selected-window))
                                (window-dedicated-p (selected-window)))
;;         (if (and (boundp 'help-window)
;;                  (equal (selected-window) ecb-compile-window))
;;             (setq help-window ecb-compile-window))
        ))))

(defvar ecb-temp-buffer-show-function-old nil)

(defun ecb-enable-own-temp-buffer-show-function (arg)
  "If ARG then set `temp-buffer-show-function' to the right function so ECB
works correct. Store the old value. If ARG is nil then restore the old value
of `temp-buffer-show-function'."
  (if arg
      (progn
        (setq ecb-temp-buffer-show-function-old
              temp-buffer-show-function)
        (setq temp-buffer-show-function
              (if ecb-running-xemacs
                  'show-temp-buffer-in-current-frame
                'ecb-temp-buffer-show-function-emacs)))
    (setq temp-buffer-show-function
          ecb-temp-buffer-show-function-old)))


;; =========== intelligent window function advices ===================

(defmacro ecb-with-original-basic-functions (&rest body)
  "Evaluates BODY with all adviced basic-functions of ECB deactivated \(means
with their original definition). Restores always the previous state of the ECB
adviced basic-functions, means after evaluating BODY it activates the advices
of exactly the functions in `ecb-layout-basic-adviced-functions'!"
  `(ecb-with-original-adviced-function-set 'ecb-layout-basic-adviced-functions
     ,@body))

(defecb-advice-set ecb-permanent-adviced-layout-functions
  "Permanent adviced layout functions.
This means these function will remain adviced after deactivating ecb!"
  t)

(defmacro ecb-with-original-permanent-layout-functions (&rest body)
  "Evaluates BODY with all adviced permanent-functions of ECB deactivated
\(means with their original definition). Restores always the previous state of
the ECB adviced permanent-functions, means after evaluating BODY it activates
the advices of exactly the functions in `ecb-permanent-adviced-layout-functions'!"
  `(ecb-with-original-adviced-function-set 'ecb-permanent-adviced-layout-functions
                                           ,@body))

(defun ecb-where-is-point (&optional win-list)
  "Return a cons-cell with the exact location of point in the ecb-frame.
The car is one of 'ecb \(point in a special ecb-window), 'edit
\(point in an edit-window of the edit-area), 'compile \(point in
the compile window), 'minibuf \(the minibuffer is the selected
window) or 'other-dedicated \(point is in a dedicated window
which is not one of the special ecb-windows - some packages like
ediff could use dedicated windows for their own needs). The cdr
is the number of the window in the related window-list in
canonical order \(means top-left-most window in the related
window-list has number 1 and so on...). In case of 'other-dedicated the cdr is
the number of the window in the full window-list of the ecb-frame \(without
minibuffer-window).

If the compile-window or the minibuffer is the selected window then the window
number is always 1.

If the ecb-frame is not the selected frame then nil is returned.
This means the result is undefined.

If WIN-LIST is nil then a new window-list is computed via
`ecb-canonical-windows-list'.

Examples:

If point stays in the second special-window list, then \('ecb . 2) is
returned, even in layouts of type 'right'.

If there are 3 edit windows and point stays in the third one, then \(edit . 3)
is returned, even in layouts of type 'left'.

If the compile-window or the minibuffer is the selected window then
\('compile . 1) rsp. \(minibuf . 1) is returned."
  (when (equal (selected-frame) ecb-frame)
    (let ((win-list (or win-list (ecb-canonical-windows-list)))
          (type (cond ((memq (selected-window)
                             (ecb-canonical-ecb-windows-list win-list))
                       'ecb)
                      ((memq (selected-window)
                             (ecb-canonical-edit-windows-list win-list))
                       'edit)
                      ((equal (selected-window) (minibuffer-window ecb-frame))
                       'minibuf)
                      ((ecb-point-in-compile-window)
                       'compile)
                      (t 'other-dedicated))))
           (cons type (case type
                        (ecb (ecb-point-in-ecb-window-number
                              (ecb-canonical-ecb-windows-list win-list)))
                        (edit (ecb-point-in-edit-window-number
                               (ecb-canonical-edit-windows-list win-list)))
                        (compile 1)
                        (minibuf 1)
                        (otherwise (ecb-window-in-window-list-number win-list)))))))

;; Do not use the following two functions in pre- or post-command hook because
;; XEmacs has no builtin c-function but only an elisp one for this and
;; therefore using it within post-command-hook or pre-command-hook would
;; dramatically slow down XEmacs.

(defun ecb-point-in-ecb-window-number (&optional ecb-windows-list)
  "Return nil if point stays not in an special ecb-window otherwise return 1
if point is in the left/topmost ecb-window or 2 if in the next ecb-window and
so on. Return the number of the ecb-window \(if point is in an ecb-window) in
the order `walk-windows' would go through the ecb-windows. If ECB-WINDOWS-LIST
is not nil then it must be a current list of ecb-windows \(got by
`ecb-canonical-ecb-windows-list'). If ECB-WINDOWS-LIST is nil then a new
ecb-window-list is computed via `ecb-canonical-ecb-windows-list'."
  (when (equal (selected-frame) ecb-frame)
    (ignore-errors (ecb-window-in-window-list-number
                    (or ecb-windows-list (ecb-canonical-ecb-windows-list))))))


(defun ecb-point-in-edit-window-number (&optional edit-windows-list)
  "Return nil if point stays not in an edit-window otherwise return 1 if point
is in the left/topmost edit-window or 2 if in the next edit-window and so on.
Return the number of the edit-window \(if point is in an edit-window) in the
order `walk-windows' would go through the edit-windows. If EDIT-WINDOWS-LIST
is not nil then it must be a current list of edit-windows \(got by
`ecb-canonical-edit-windows-list'). If EDIT-WINDOWS-LIST is nil then a new
edit-window-list is computed via `ecb-canonical-edit-windows-list'."
  (when (equal (selected-frame) ecb-frame)
    (ignore-errors (ecb-window-in-window-list-number
                    (or edit-windows-list (ecb-canonical-edit-windows-list))))))

(defmacro ecb-when-point-in-edit-window-ecb-windows-visible (&rest body)
  "Evaluate BODY if an edit-window is selected and ecb-windows are visible."
  `(when (and ecb-minor-mode
              (not ecb-windows-hidden)
              (ecb-point-in-edit-window-number))
     ,@body))


(defun ecb-get-edit-window-by-number (edit-win-nr &optional edit-win-list)
  "Return that edit-window with number EDIT-WIN-NR. If EDIT-WIN-LIST is set
then get it from that list otherwise from `ecb-canonical-edit-windows-list'.
EDIT-WIN-NR must be an integer between 1 and length of EDIT-WIN-LIST \(rsp.
`ecb-canonical-edit-windows-list')."
  (nth (1- edit-win-nr) (or edit-win-list (ecb-canonical-edit-windows-list))))

(defun ecb-get-window-by-number (win-nr &optional win-list)
  "Return that window with number WIN-NR. If WIN-LIST is set
then get it from that list otherwise from `ecb-canonical-windows-list'.
WIN-NR must be an integer between 1 and length of WIN-LIST \(rsp.
`ecb-canonical-windows-list')."
  (nth (1- win-nr) (or win-list (ecb-canonical-windows-list))))

(defun ecb-get-ecb-window-by-number (ecb-win-nr &optional ecb-win-list)
  "Return that ecb-window with number ECB-WIN-NR. If ECB-WIN-LIST is set
then get it from that list otherwise from `ecb-canonical-ecb-windows-list'.
ECB-WIN-NR must be an integer between 1 and length of ECB-WIN-LIST \(rsp.
`ecb-canonical-ecb-windows-list')."
  (nth (1- ecb-win-nr) (or ecb-win-list (ecb-canonical-ecb-windows-list))))

(defun ecb-point-in-compile-window ()
  "Return not nil iff point is in the compile-window of ECB"
  (and (equal (selected-frame) ecb-frame)
       (ecb-compile-window-live-p)
       (equal (selected-window) ecb-compile-window)))

(defun ecb-point-in-ecb-tree-buffer ()
  "Return not nil if point is in any of the standard tree-buffers \(see
function `ecb-tree-buffers-name-list') of ECB and if the current buffer is
displayed in the currently selected window."
  (when (and (equal (selected-frame) ecb-frame)
             (member (buffer-name (current-buffer))
                     (ecb-tree-buffers-name-list))
             (eq (selected-window) (get-buffer-window (current-buffer)
                                                      ecb-frame)))
    (current-buffer)))

;; This function should not use any call to `ecb-window-list' because XEmacs
;; has no builtin c-function but only an elisp one for this and therefore
;; using it within post-command-hook or pre-command-hook would dramatically
;; slow down XEmacs.
(defun ecb-point-in-dedicated-special-buffer ()
  "Return not nil if point is in any of the special dedicated buffers which
are registrated via the macro `defecb-window-dedicator' \(see
`ecb-dedicated-special-buffers') and if the current buffer is displayed in the
currently selected window."
  (when (equal (selected-frame) ecb-frame)
    (if (and (member (current-buffer) (ecb-dedicated-special-buffers))
             (eq (selected-window) (get-buffer-window (current-buffer) ecb-frame)))
        (current-buffer))))
          
(defun ecb-buffer-is-dedicated-special-buffer-p (buffer-or-name)
  "Return not nil if BUFFER-OR-NAME is a member of
`ecb-dedicated-special-buffers'. BUFFER-OR-NAME ca be either a
buffer-object or a buffer-name."
  (let ((buffer (ecb-buffer-obj buffer-or-name)))
    (member buffer (ecb-dedicated-special-buffers))))


(defun ecb-goto-ecb-window (ecb-buffer-name)
  "Select that ecb-window displaying the special ecb-buffer ECB-BUFFER-NAME.
Only buffer-names defined for the current layout \(see function
`ecb-buffer-is-ecb-buffer-of-current-layout-p') or the buffer-name of the
integrated speedbar are accepted. If such a window can not be selected then
probably because another ecb-window of current layout is currently maximized;
therefore in such a case the layout has been redrawn and then tried to select
the window again. This function does nothing if NAME fulfills not the
described conditions or if the ecb-windows are hidden or ECB is not active. If
necessary the `ecb-frame' will be first raised."
  (when (and ecb-minor-mode
             (not ecb-windows-hidden)
             (or (equal ecb-buffer-name ecb-speedbar-buffer-name)
                 (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-buffer-name)))
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (or (ecb-window-select ecb-buffer-name)
        ;; the window is not visible because another one is maximized;
        ;; therefore we first redraw the layout
        (progn
          (ecb-redraw-layout-full nil nil nil nil)
          ;; now we can go to the window
          (ecb-window-select ecb-buffer-name)))))

(defun ecb-goto-window-edit-last ()
  "Make the last selected edit-window window the current window. This is the
same as if `ecb-mouse-click-destination' is set to 'last-point."
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (let ((ecb-mouse-click-destination 'last-point))
      (ecb-select-edit-window))))

(defun ecb-goto-window-edit1 ()
  "Make the \(first) edit-window window the current window."
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (ecb-select-edit-window 1)))

(defun ecb-goto-window-edit2 ()
  "Make the second edit-window \(if available) window the current window."
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (ecb-select-edit-window t)))

(defun ecb-goto-window-edit-by-smart-selection (&optional use-immediate-completion)
  "Selects an edit-window by smart selection.
The command offers a list of all edit-windows by buffer-name. Selecting the
buffer-name will select the displaying window. Which edit-windows are offered
depends on the currently selected window: If not an edit-window is the
currently selected-window then all edit-windows are offered in canonical order
\(means from top-left to button-right). If an edit-window is the currently
selected-window then all other\(!) edit-windows are offered, beginning with
the next edit-window to the current edit-window \(the following edit-windows
also in canonical order). If there is only one senseful
destination-edit-window then this window is immediately selected, without
confirmation \(e.g.: There are two edit-windows and point stays in one of
them. Or there is only one edit-window and point stays either in one of the
special ecb-windows or in the compile-window).

If optional argument USE-IMMEDIATE-COMPLETION is nil then all possible
destination-windows are displayed in the message-area and only hitting TAB
offers completion. If USE-IMMEDIATE-COMPLETION is not nil then all possible
destinations are immediately shown in a completion-buffer."
  (interactive "P")
  (ecb-goto-window-by-smart-selection--internal
   (ecb-canonical-edit-windows-list)
   use-immediate-completion))

(defun ecb-goto-window-ecb-by-smart-selection (&optional use-immediate-completion)
  "Selects a special ecb-browsing-window by smart selection.
The command offers a list of all special ecb-windows by buffer-name. Selecting
the buffer-name will select the displaying window. Which special ecb-windows
are offered depends on the currently selected window: If not a special
ecb-window is the currently selected-window then all edit-windows are offered
in canonical order \(means from top-left to button-right). If an ecb-window is
the currently selected-window then all other\(!) special ecb-windows are
offered, beginning with the next special ecb-window to the current ecb-window
\(the following special ecb-windows also in canonical order). If there is only
one senseful destination-ecb-window then this window is immediately selected,
without confirmation \(e.g.: There are two special ecb-windows and point stays
in one of them. Or there is only one ecb-window and point stays either in one
of the edit-windows or in the compile-window).

If optional argument USE-IMMEDIATE-COMPLETION is nil then all possible
destination-windows are displayed in the message-area and only hitting TAB
offers completion. If USE-IMMEDIATE-COMPLETION is not nil then all possible
destinations are immediately shown in a completion-buffer."
  (interactive "P")
  (ecb-goto-window-by-smart-selection--internal
   (ecb-canonical-ecb-windows-list)
   use-immediate-completion))

(defun ecb-goto-window-by-smart-selection--internal (win-list &optional use-immediate-completion)
  "Perform smart window-selection depending on WIN-LIST.
Possible window-destinations are offered by the buffer-name displayed in a
window. Selecting the buffer-name will make the displaying window the
selected-window. Which windows are offered depends on the currently selected
window at call-time: If not a window contained in WIN-LIST is currently the
selected-window then all windows of WIN-LIST are offered in canonical order
\(means from top-left to button-right). If a window contained in WIN-LIST is
currently the selected-window then all other\(!) windows of WIN-LIST are
offered, beginning with the next edit-window to the currently selected window
\(the following windows also in canonical order). If there is only one
senseful destination-window then this window is immediately selected, without
confirmation \(e.g. if WIN-LIST is the list of all edit-windows: There are two
edit-windows and point stays in one of them. Or there is only one edit-window
and point stays either in one of the special ecb-windows or in the
compile-window).

If optional argument USE-IMMEDIATE-COMPLETION is nil then all possible
destination-windows are displayed in the message-area and only hitting TAB
offers completion. If USE-IMMEDIATE-COMPLETION is not nil then all
possible destinations are immediately shown in a completion-buffer."
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (let* ((destination-list (if (member (selected-window) win-list)
                                 ;; point already in a window of the win-list ==> all
                                 ;; windows of this list but the current one
                                 (cdr (ecb-rotate win-list (selected-window)))
                               ;; point elsewhere ==> all windows of the list in
                               ;; canonical order
                               win-list))
           ;; build an alist with car is a left-trimmed buffer-name (usefull
           ;; for the special ECB-buffers which all start with a blank) and
           ;; cdr ist the real buffer-name (used for selecting the buffer)
           (choices-buffer-name-alist
            (mapcar (function (lambda (w)
                                (cons (ecb-left-trim (buffer-name (window-buffer w)))
                                      (buffer-name (window-buffer w)))))
                    destination-list)))
      (when choices-buffer-name-alist
        (if (= (length choices-buffer-name-alist) 1)
            ;; no user-interaction necessary because only one senseful destination
            (ecb-window-select (cdar choices-buffer-name-alist))
          ;; we ask the user which window/buffer should be selected. For conveniance
          ;; the left-trimmed buffer-names are offered - s.a.
          (if use-immediate-completion
              (ecb-window-select
               (cdr (assoc (ecb-offer-choices "Select a window: "
                                              (mapcar 'car choices-buffer-name-alist))
                           choices-buffer-name-alist)))
            (ecb-window-select
             (cdr (assoc (ecb-query-string "Select a window: "
                                           (mapcar 'car choices-buffer-name-alist))
                         choices-buffer-name-alist)))))))))

(defun ecb-goto-window-compilation ()
  "Goto the ecb compilation window `ecb-compile-window'."
  (interactive)
  (when (and ecb-minor-mode
             (equal 'visible (ecb-compile-window-state)))
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (select-window ecb-compile-window)))


(defun ecb-select-ecb-frame ()
  "Selects the `ecb-frame' if ECB is activated - otherwise reports an error."
  (interactive)
  (if ecb-minor-mode
      (progn
        (raise-frame ecb-frame)
        (select-frame ecb-frame))
    (ecb-error "ECB is not avtive in any frame!")))

(defun ecb-select-edit-window (&optional edit-window-number)
  "Moves point into the edit-window. If optional EDIT-WINDOW-NUMBER
is an integer then move point into the EDIT-WINDOW-NUMBER-nth edit-window
\(beginning from 1). If EDIT-WINDOW-NUMBER is not nil but not an integer or an
integer < 0 or > number of current available edit-windows then move point to
the second edit-window if there is more than 1. If EDIT-WINDOW-NUMBER is nil
then select the edit-window according to the value of the option
`ecb-mouse-click-destination'. If point already stays in the right edit-window
nothing is done."
  (if (and (null edit-window-number)
           (equal ecb-mouse-click-destination 'last-point))
      (condition-case nil
          (select-window ecb-last-edit-window-with-point)
        (error (ecb-select-edit-window 1)))
    (let* ((edit-win-list (ecb-canonical-edit-windows-list))
           (edit-win (or (and (not edit-window-number)
                              (car edit-win-list))
                         (and (integerp edit-window-number)
                              (> edit-window-number 0)
                              (<= edit-window-number (length edit-win-list))
                              (nth (1- edit-window-number) edit-win-list))
                         (and (> (length edit-win-list) 1)
                              (nth 1 edit-win-list))
                         (car edit-win-list))))
      (select-window edit-win))))

;; VERY IMPORTANT: pre-command- and the post-command-hook must NOT use any
;; function which calls `ecb-window-list' because this would slow-down the
;; performance of all Emacs-versions unless GNU Emacs >= 21 because they have no
;; builtin `window-list'-function.
(defecb-autocontrol/sync-function ecb-layout-pre-command-hook nil nil nil
  "During activated ECB this function is added to `pre-command-hook' to set
always `ecb-last-edit-window-with-point', `ecb-last-source-buffer',
`ecb-compile-window-was-selected-before-command' and
`ecb-last-compile-buffer-in-compile-window' correct so other functions
can use these variables."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    ;; We MUST not use here `ecb-point-in-edit-window-number' because this
    ;; would slow-down the performance of all Emacs-versions unless GNU Emacs
    ;; >= 21 because they have no builtin `window-list'-function.
    (when (and (not (ecb-point-in-dedicated-special-buffer))
               (not (equal (minibuffer-window ecb-frame)
                           (selected-window)))
               (not (ecb-point-in-compile-window)))
      (setq ecb-last-edit-window-with-point (selected-window))
      (setq ecb-last-source-buffer (current-buffer)))
    (if (ecb-point-in-compile-window)
        (progn
          (setq ecb-compile-window-was-selected-before-command t)
          (setq ecb-last-compile-buffer-in-compile-window
                (current-buffer)))
      (setq ecb-compile-window-was-selected-before-command nil)
      (setq ecb-last-compile-buffer-in-compile-window nil))
    (if (ecb-point-in-dedicated-special-buffer)
        (setq ecb-ecb-buffer-name-selected-before-command (buffer-name))
      (setq ecb-ecb-buffer-name-selected-before-command nil))))
      

(defvar ecb-layout-prevent-handle-compile-window-selection nil)
(defvar ecb-last-edit-area-creators nil)
(defecb-autocontrol/sync-function ecb-layout-post-command-hook nil nil nil
  "During activated ECB this function is added to `post-command-hook' to do
some special tasks:
- handling of `ecb-compile-window-temporally-enlarge'
- handling of `ecb-maximize-ecb-window-after-selection'"
  (if ecb-layout-prevent-handle-compile-window-selection
      (setq ecb-layout-prevent-handle-compile-window-selection nil)
    (when (and ecb-minor-mode
               (equal (selected-frame) ecb-frame)
               (member ecb-compile-window-temporally-enlarge
                       '(after-selection both))
               (ecb-compile-window-live-p)
               (= (minibuffer-depth) 0))
      (cond ((and (ecb-point-in-compile-window)
                  (not ecb-compile-window-was-selected-before-command))
             (ecb-layout-debug-error "ecb-layout-post-command-hook: enlarge")
             (ecb-toggle-compile-window-height 1)
             ;; now we change the window-start, so we see autom. more text
             ;; after the enlargement of the window.
;;              (let ((height-before (ecb-window-full-height))
;;                    (height-after (ecb-toggle-compile-window-height 1)))
;;                (set-window-start ecb-compile-window
;;                                  (save-excursion
;;                                    (goto-char (window-start))
;;                                    (forward-line (* -1 (- height-after height-before)))
;;                                    (ecb-line-beginning-pos))
;;                                  t))
             )
            ((and ecb-compile-window-was-selected-before-command
                  (not (ecb-point-in-compile-window)))
             (ecb-layout-debug-error "ecb-layout-post-command-hook: shrink")
             (ecb-toggle-compile-window-height -1)))))
  (if ecb-layout-prevent-handle-ecb-window-selection
      (progn
        (setq ecb-layout-prevent-handle-ecb-window-selection nil))
    (when (and ecb-minor-mode
               ecb-maximize-ecb-window-after-selection
               ;; Klaus Berndl <klaus.berndl@sdm.de>: There is a very
               ;; mysterious behavior of GNU Emacs 21: When reaching this
               ;; code-part then after the full-redraw Emacs runs
               ;; automatically the command bound to the button-release-event
               ;; of mouse-1 with an event which contains as event-window the
               ;; top/left-most window which is in left- and top-layouts the
               ;; first ecb-window - so it seems that Emacs automatically
               ;; "clicks" into the first ecb-window which then maximizes this
               ;; window which is definitively very annoying and a bug (with
               ;; right-layouts there is no problem, because the top/left-most
               ;; window is not a ecb-tree-window and has therefore no special
               ;; mouse-1 binding!). But it's currently not sure if it is a
               ;; bug somewhere in ECB or in GNU Emacs - but for the moment i
               ;; assume Emacs because with XEmacs all is working fine, i.e.
               ;; the full window redraw is done and nothing afterwards this
               ;; post-command-hook.

               ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: We should identify
               ;; the underlying problem and if it is an Emacs-bug we should
               ;; report it - maybe we have to write a small ECB-independent
               ;; code-example which reproduces this bug/behavior?!!

               ;; In the meanwhile we allow automatic maximizing only when
               ;; `ecb-tree-mouse-action-trigger' is 'button-press!
               (or ecb-running-xemacs
                   ecb-running-version-22
                   (equal ecb-tree-mouse-action-trigger 'button-press))
               (equal (selected-frame) ecb-frame)
               (= (minibuffer-depth) 0))
      (cond ((and (not (equal ecb-ecb-buffer-name-selected-before-command
                              (buffer-name)))
                  (ecb-point-in-dedicated-special-buffer))
             (ecb-maximize-ecb-buffer (buffer-name) t))
            ((and ecb-ecb-buffer-name-selected-before-command
                  (not (ecb-point-in-dedicated-special-buffer)))
             (ecb-redraw-layout-full nil nil nil nil)))))
  )


;; here come the advices

;; *** `special-display-buffer-names' and `special-display-regexps' now
;; understand two new boolean pseudo-frame-parameters `same-frame' and
;; `same-buffer'. This is new in Emacs 22 so we have to integrate it!
;; The format is: (same-buffer . VALUE) or (same-frame . VALUE). But this is
;; automatically taken into account by ecb-check-for-special-buffer because
;; the car of an element of special-display-buffer-names must be a buffer-name
;; and we must only check if the buffer-name matches... if yes our advice of
;; display-buffer calls et the end by ad-do-it the display-buffer-code which
;; itself evaluates these options... so we are fine with the following
;; implementation!

(defun ecb-check-for-special-buffer (buffer-or-name)
  "Return  not nil if and only if `special-display-function' is not nil and
BUFFER-OR-NAME is contained or matches `special-display-buffer-names' or
`special-display-regexps'."
  (let ((result
         (if (ecb-ignore-special-display)
             nil
           (catch 'done
             (let ((buf-name (ecb-buffer-name buffer-or-name)))
               (when (and buf-name special-display-function)
                 (if (member buf-name
                             special-display-buffer-names)
                     (throw 'done t))
      
                 (let ((tem (assoc buf-name
                                   special-display-buffer-names)))
                   (if tem
                       (throw 'done t)))

                 (let ((tem special-display-regexps))
                   (while tem
                     (let ((car (car tem)))
                       (if (and (stringp car)
                                (save-match-data (string-match car buf-name)))
                           (throw 'done t))
                       (if (and (consp car)
                                (stringp (car car))
                                (save-match-data (string-match (car car) buf-name)))
                           (throw 'done t)))
                     (setq tem (cdr tem))))))))))
    (ecb-layout-debug-error "ecb-check-for-special-buffer for %s: %s"
                            buffer-or-name result)
    result))
    
(defun ecb-check-for-same-window-buffer (buffer-or-name)
  "Return not nil if BUFFER-OR-NAME is contained or matches
`same-window-buffer-names' or `same-window-regexps'."
  (let ((result
          (catch 'done
            (let ((buf-name (ecb-buffer-name buffer-or-name)))
              (when buf-name
                (if (or (member buf-name same-window-buffer-names)
                        (assoc buf-name same-window-buffer-names))
                    (throw 'done t))
                (let ((tem same-window-regexps))
                  (while tem
                    (let ((car (car tem)))
                      (if (or
                           (and (stringp car)
                                (save-match-data (string-match car buf-name)))
                           (and (consp car) (stringp (car car))
                                (save-match-data (string-match (car car) buf-name))))
                          (throw 'done t)))
                    (setq tem (cdr tem)))))))))
     (ecb-layout-debug-error "ecb-check-for-same-window-buffer for %s: %s"
                             buffer-or-name result)
     result))

(if ecb-running-xemacs
    (defmacro ecb-with-unsplittable-ecb-frame (&rest body)
      "Runs BODY with `ecb-frame' temporally unsplittable."
      `(unwind-protect
           (progn
             (set-frame-property ecb-frame 'unsplittable t)
             ,@body)
         (set-frame-property ecb-frame 'unsplittable nil)))
    
  (defmacro ecb-with-unsplittable-ecb-frame (&rest body)
    "Runs BODY with `ecb-frame' temporally unsplittable."
    `(unwind-protect
         (progn
           (modify-frame-parameters ecb-frame '((unsplittable . t)))
           ,@body)
       (modify-frame-parameters ecb-frame '((unsplittable . nil)))))
  )
     


(defvar ecb-layout-temporary-dedicated-windows nil
  "List of windows temporary made dedicated by ECB.
Only set by the adviced `display-buffer' and only evaluated by
`ecb-canonical-edit-windows-list' and `ecb-canonical-ecb-windows-list'. This
variable is strictly only for internal usage!")

;; The XEmacs-versions never choose dedicated windows (so the function don't
;; have a DEDICATED argument and so we don't need advice....
(when-ecb-running-emacs
 (defecb-advice get-largest-window before ecb-layout-basic-adviced-functions
   "When called from within the `ecb-frame' then DEDICATED is always set to nil.
So never a dedicated window is returned during activated ECB."
   (ecb-layout-debug-error "get-largest-window for frame:%s, dedicated:%s"
                           (ad-get-arg 0) (ad-get-arg 1))
   (and (boundp 'ecb-minor-mode)
        ecb-minor-mode
        (eq (selected-frame) ecb-frame)
        ;; caller wants dedicated windows also taken into account
        (ad-get-arg 1)
        ;; we forbid dedicated windows
        (ad-set-arg 1 nil)))
      

 (defecb-advice get-lru-window before ecb-layout-basic-adviced-functions
   "When called from within the `ecb-frame' then DEDICATED is always set to nil.
So never a dedicated window is returned during activated ECB."
   (ecb-layout-debug-error "get-lru-window for frame:%s, dedicated:%s"
                           (ad-get-arg 0) (ad-get-arg 1))
   (and (boundp 'ecb-minor-mode)
        ecb-minor-mode
        (eq (selected-frame) ecb-frame)
        ;; caller wants dedicated windows also taken into account
        (ad-get-arg 1)
        ;; we forbid dedicated windows
        (ad-set-arg 1 nil)))
 )


;; This advice is the heart of the mechanism which displays all buffer in the
;; compile-window if they are are "compilation-buffers" in the sense of
;; `ecb-compilation-buffer-p'!
;; We do not use `display-buffer-function' but we just handle it within the
;; advice, because otherwise we would have to implement all window-choosing
;; for ourself and with our advice we just "restrict" the windows
;; `display-buffer' can use (by setting the not choosable windows temporarly
;; dedicated) but the real choosing-task is done by the function
;; itself - this is much better and smarter than implementing the whole stuff.

(defecb-advice display-buffer around ecb-layout-basic-adviced-functions
  "Makes this function compatible with ECB if called in or for the ecb-frame.
It displays all buffers which are \"compilation-buffers\" in the sense of
`ecb-compilation-buffer-p' in the compile-window of ECB. If the compile-window
is temporally hidden then it will be displayed first.

If there is no compile-window \(`ecb-compile-window-height' is nil) then it
splits the edit-window if unsplitted and displays BUFFER in the other
edit-window but only if `pop-up-windows' is not nil \(otherwise the
edit-window will not splitted).

All buffers which are not \"compilation-buffers\" in the sense of
`ecb-compilation-buffer-p' will be displayed in one of the edit-area and
`display-buffer' behaves as if the edit-windows would be the only windows in
the frame.

If BUFFER is contained in `special-display-buffer-names' or matches
`special-display-regexps' then `special-display-function' will be called \(if
not nil). But this behavior depends on the value of the option
`ecb-ignore-special-display'. The values of `same-window-buffer-names' and
`same-window-regexps' are supported as well.

See the value of the option `ecb-ignore-display-buffer-function'!

If called for other frames it works like the original version."
  (if ecb-running-xemacs
      (ecb-layout-debug-error "display-buffer entered with: %s %s %s %s"
                              (ad-get-arg 0)
                              (ad-get-arg 1)
                              (ad-get-arg 2)
                              (ad-get-arg 3))
    (ecb-layout-debug-error "display-buffer entered with: %s %s %s"
                            (ad-get-arg 0)
                            (ad-get-arg 1)
                            (ad-get-arg 2)))
  (if (and ecb-minor-mode
           (or (and (ad-get-arg 2)
                    (framep (ad-get-arg 2))
                    (equal (ad-get-arg 2) ecb-frame))
               (and (or (null (ad-get-arg 2))
                        (equal (ad-get-arg 2) t)
                        (equal (ad-get-arg 2) 0))
                    (equal (selected-frame) ecb-frame)))
           (not (ecb-check-for-special-buffer (ad-get-arg 0)))
           (not (and (boundp 'display-buffer-function)
                     (fboundp display-buffer-function)
                     (not (ecb-ignore-display-buffer-function)))))
      (let ((special-display-function (if (ecb-ignore-special-display)
                                          nil
                                        special-display-function)))
        (cond ((ecb-compilation-buffer-p (ad-get-arg 0))
               (ecb-layout-debug-error "display-buffer for a comp-buffer: %s"
                                       (ad-get-arg 0))
               ;; we have to display the buffer in the compile-window if a
               ;; compile-window was set but currently hidden --> then we have
               ;; to show it now. `ecb-toggle-compile-window' preserves always
               ;; the selected window!
               (when (and (equal 'hidden (ecb-compile-window-state))
                          ;; calling this from minibuffer (e.g. completions)
                          ;; seems to cause problems
                          (not (equal (minibuffer-window ecb-frame) (selected-window))))
                 (ecb-layout-debug-error "display-buffer: comp-win will be toggled.")
                 (save-excursion (ecb-toggle-compile-window 1)))
               (if (ecb-compile-window-live-p)
                   ;; now we have to make the edit-window(s) dedicated
                   (let ((edit-window-list (ecb-canonical-edit-windows-list))
                         (pop-up-frames (if (ecb-ignore-pop-up-frames)
                                            nil
                                          pop-up-frames)))
                     (ecb-layout-debug-error "display-buffer: buffer %s has to be displayed in the alive compile-window: %s"
                                             (ad-get-arg 0) ecb-compile-window)
                     (unwind-protect
                         (progn
                           (mapc (function (lambda (w)
                                             (set-window-dedicated-p w t)))
                                 edit-window-list)
                           (setq ecb-layout-temporary-dedicated-windows
                                 edit-window-list)
                           ;; now we perform the original `display-buffer' but
                           ;; now the only not dedicated window is the compile
                           ;; window so `display-buffer' MUST use this.

                           ;; `display-buffer' sometimes tries to split the
                           ;; compile-window (e.g. if it is called from the
                           ;; compile-window - e.g. calling help from the
                           ;; compile-window - or with Emacs 23 also when
                           ;; using a frame-width compile-window and do
                           ;; completions). To avoid this we must temporally
                           ;; make the ecb-frame unsplittable - but here we
                           ;; can do this savely because here we have a live
                           ;; compile-window and a buffer which should be
                           ;; displayed there ==> display-buffer MUST
                           ;; (because all other windows are temporally
                           ;; dedicated) use exactly this window and there
                           ;; is no need to split it
                           (ecb-with-unsplittable-ecb-frame
                            (if ecb-running-xemacs
                                ;; XEmacs does not shrink to fit if
                                ;; `pop-up-windows' is nil so we must set it
                                ;; here temporally to t
                                (let ((pop-up-windows t))
                                  ad-do-it)
                              ad-do-it)))
                       ;; making the edit-window(s) not dedicated
                       (mapc (function (lambda (w)
                                         (set-window-dedicated-p w nil)))
                             edit-window-list)
                       ;;(modify-frame-parameters ecb-frame '((unsplittable . nil)))
                       (setq ecb-layout-temporary-dedicated-windows nil)) ;; end unwind-protect

                     ;; if called interactively we run now our
                     ;; `ecb-toggle-compile-window-height' to set the height of
                     ;; the compile-window according to the value of
                     ;; `ecb-enlarged-compilation-window-max-height'. If called
                     ;; non-interactively (e.g. by `compile-internal',
                     ;; `with-output-to-temp-buffer' etc...) then all the
                     ;; resizing or shrinking stuff is handled by
                     ;; `compilation-set-window-height',
                     ;; `resize-temp-buffer-window' (GNU Emacs) or
                     ;; `shrink-window-if-larger-than-buffer' (called by the
                     ;; SHRINK-TO-FIT arg of the XEmacs `display-buffer').

                     ;; We can not do this in the non-interactive case because
                     ;; here often after the call of display-buffer the buffer
                     ;; to display does not contain its final contents so the
                     ;; algorithm of `ecb-toggle-compile-window-height' fails
                     ;; (e.g. during `compile-internal'!).
                     (unless pop-up-frames
                       (if (interactive-p)
                           (ecb-set-compile-window-height)
                         (if (save-excursion
                               (set-buffer (ad-get-arg 0))
                               (= (point-min) (point-max)))
                             ;; Klaus Berndl <klaus.berndl@sdm.de>: If this
                             ;; makes trouble we remove it.
                             (ecb-toggle-compile-window-height -1)))
                       
                       (if (member ecb-compile-window-temporally-enlarge
                                   '(after-selection both))
                           (setq ecb-layout-prevent-handle-compile-window-selection t)))
                     ) ;; end of let...

                 ;; OK, we have really no compile-window...
               
                 ;; needed for TAB-completion if this offers the completions in
                 ;; a temp-buffer. Without this manually split the whole
                 ;; edit-window would be used for the completions which is not
                 ;; the default-behavior of Emacs.
                 (let ((pop-up-frames (if (ecb-ignore-pop-up-frames)
                                          nil
                                        pop-up-frames)))
                   ;; emacs 23 splits automatically when window-size allows
                   ;; this (see split-width-threshold and
                   ;; split-height-threshold)... 
                   (when (and (not ecb-running-version-23)
                              (not ecb-windows-hidden)
                              (not (ecb-layout-top-p))
                              pop-up-windows
                              (not pop-up-frames)
                              (not (ecb-edit-window-splitted))
                              (not (ecb-check-for-same-window-buffer (ad-get-arg 0))))
                     (ecb-layout-debug-error "display-buffer for comp-buffer %s - split edit-window:"
                                             (ad-get-arg 0))
                     (split-window (car (ecb-canonical-edit-windows-list))))
                   ;; Here the values of temp-buffer-max-height and
                   ;; compilation-window-height take effect.
                   ad-do-it)))
            
              ((not (ecb-buffer-is-dedicated-special-buffer-p (ad-get-arg 0)))
               (ecb-layout-debug-error "display-buffer for normal buffer: %s"
                                       (ad-get-arg 0))
               (let ((edit-win-list (ecb-canonical-edit-windows-list))
                     (pop-up-frames (if (ecb-ignore-pop-up-frames)
                                        nil
                                      pop-up-frames)))
                 ;; maybe we have to split the edit-area here
                 (when (and (or pop-up-windows (ad-get-arg 1))
                            (not pop-up-frames)
                            (not (ecb-edit-window-splitted edit-win-list))
                            ;; if the BUFFER is already displayed in an
                            ;; edit-window and NOT-THIS-WINDOW is nil then
                            ;; we must not split the edit-window because
                            ;; display-buffer then just uses this window for
                            ;; displaying BUFFER.
                            (not (and (not (ad-get-arg 1))
                                      (member (get-buffer-window (ad-get-arg 0) ecb-frame)
                                              edit-win-list)))
                            ;; if we display a "normal" buffer from outside
                            ;; the edit-windows then we have per se another
                            ;; window because the buffer will displayed in
                            ;; an edit-window ==> we only split if we are
                            ;; already in an edit-window.
                            (ecb-point-in-edit-window-number edit-win-list)
                            (not (ecb-check-for-same-window-buffer (ad-get-arg 0))))
                   (ecb-layout-debug-error "display-buffer for normal-buffer %s - split edit-window:"
                                           (ad-get-arg 0))
                   (split-window (car edit-win-list)))
                 (if (ecb-compile-window-live-p)
                     (unwind-protect
                         (progn
                           (set-window-dedicated-p ecb-compile-window t)
                           ;; now we perform the original `display-buffer' but
                           ;; now the only not dedicated window(s) are the
                           ;; edit-window(s)
                           (setq ecb-layout-temporary-dedicated-windows
                                 (list ecb-compile-window))
                           ad-do-it)
                       ;; making the compile-window not dedicated
                       (set-window-dedicated-p ecb-compile-window nil)
                       (setq ecb-layout-temporary-dedicated-windows nil))
                   ad-do-it)))
            
              (t ;; buffer is a special ecb-buffer
               (ecb-layout-debug-error "display-buffer for special ecb-buffer: %s" (ad-get-arg 0))
               (or (setq ad-return-value (get-buffer-window (ad-get-arg 0) ecb-frame))
                   (ecb-error "display-buffer can not display not visible ecb-buffers!")))))

    (ecb-layout-debug-error "display-buffer - just run original version.")
    (ecb-with-original-basic-functions
     ad-do-it)))

;; (defecb-advice display-buffer around ecb-layout-basic-adviced-functions
;;   "Makes this function compatible with ECB if called in or for the ecb-frame.
;; It displays all buffers which are \"compilation-buffers\" in the sense of
;; `ecb-compilation-buffer-p' in the compile-window of ECB. If the compile-window
;; is temporally hidden then it will be displayed first.

;; If there is no compile-window \(`ecb-compile-window-height' is nil) then it
;; splits the edit-window if unsplitted and displays BUFFER in the other
;; edit-window but only if `pop-up-windows' is not nil \(otherwise the
;; edit-window will not splitted).

;; All buffers which are not \"compilation-buffers\" in the sense of
;; `ecb-compilation-buffer-p' will be displayed in one of the edit-area and
;; `display-buffer' behaves as if the edit-windows would be the only windows in
;; the frame.

;; If BUFFER is contained in `special-display-buffer-names' or matches
;; `special-display-regexps' then `special-display-function' will be called \(if
;; not nil). But this behavior depends on the value of the option
;; `ecb-ignore-special-display'. The values of `same-window-buffer-names' and
;; `same-window-regexps' are supported as well.

;; See the value of the option `ecb-ignore-display-buffer-function'!

;; If called for other frames it works like the original version."
;;   (if ecb-running-xemacs
;;       (ecb-layout-debug-error "display-buffer entered with: %s %s %s %s"
;;                               (ad-get-arg 0)
;;                               (ad-get-arg 1)
;;                               (ad-get-arg 2)
;;                               (ad-get-arg 3))
;;     (ecb-layout-debug-error "display-buffer entered with: %s %s %s"
;;                             (ad-get-arg 0)
;;                             (ad-get-arg 1)
;;                             (ad-get-arg 2)))
;;   (if (and ecb-minor-mode
;;            (or (and (ad-get-arg 2)
;;                     (framep (ad-get-arg 2))
;;                     (equal (ad-get-arg 2) ecb-frame))
;;                (and (or (null (ad-get-arg 2))
;;                         (equal (ad-get-arg 2) t)
;;                         (equal (ad-get-arg 2) 0))
;;                     (equal (selected-frame) ecb-frame)))
;;            (not (ecb-check-for-special-buffer (ad-get-arg 0)))
;;            (not (and (boundp 'display-buffer-function)
;;                      (fboundp display-buffer-function)
;;                      (not (ecb-ignore-display-buffer-function)))))
;;       (let ((special-display-function (if (ecb-ignore-special-display)
;;                                           nil
;;                                         special-display-function)))
;;         (cond ((ecb-compilation-buffer-p (ad-get-arg 0))
;;                (ecb-layout-debug-error "display-buffer for a comp-buffer: %s"
;;                                        (ad-get-arg 0))
;;                ;; we have to display the buffer in the compile-window if a
;;                ;; compile-window was set but currently hidden --> then we have
;;                ;; to show it now. `ecb-toggle-compile-window' preserves always
;;                ;; the selected window!
;;                (when (and (equal 'hidden (ecb-compile-window-state))
;;                           ;; calling this from minibuffer (e.g. completions)
;;                           ;; seems to cause problems
;;                           (not (equal (minibuffer-window ecb-frame) (selected-window))))
;;                  (ecb-layout-debug-error "display-buffer: comp-win will be toggled.")
;;                  (save-excursion (ecb-toggle-compile-window 1)))
;;                (if (ecb-compile-window-live-p)
;;                    ;; now we have to make the edit-window(s) dedicated
;;                    (let ((edit-window-list (ecb-canonical-edit-windows-list))
;;                          (pop-up-frames (if (ecb-ignore-pop-up-frames)
;;                                             nil
;;                                           pop-up-frames)))
;;                      (ecb-layout-debug-error "display-buffer: buffer %s has to be displayed in the alive compile-window: %s"
;;                                              (ad-get-arg 0) ecb-compile-window)
;;                      (unwind-protect
;;                          (progn
;;                            (mapc (function (lambda (w)
;;                                              (set-window-dedicated-p w t)))
;;                                  edit-window-list)
;;                            (setq ecb-layout-temporary-dedicated-windows
;;                                  edit-window-list)
;;                            ;; now we perform the original `display-buffer' but
;;                            ;; now the only not dedicated window is the compile
;;                            ;; window so `display-buffer' MUST use this.

;;                            ;; `display-buffer' sometimes tries to split the
;;                            ;; compile-window (e.g. if it is called from the
;;                            ;; compile-window - e.g. calling help from the
;;                            ;; compile-window - or with Emacs 23 also when
;;                            ;; using a frame-width compile-window and do
;;                            ;; completions). To avoid this we must temporally
;;                            ;; make the ecb-frame unsplittable - but here we
;;                            ;; can do this savely because here we have a live
;;                            ;; compile-window and a buffer which should be
;;                            ;; displayed there ==> display-buffer MUST
;;                            ;; (because all other windows are temporally
;;                            ;; dedicated) use exactly this window and there
;;                            ;; is no need to split it
;;                            (ecb-with-unsplittable-ecb-frame
;;                             (if ecb-running-xemacs
;;                                 ;; XEmacs does not shrink to fit if
;;                                 ;; `pop-up-windows' is nil so we must set it
;;                                 ;; here temporally to t
;;                                 (let ((pop-up-windows t))
;;                                   (setq ad-return-value
;;                                         (ecb-display-buffer-xemacs (ad-get-arg 0)
;;                                                                    (ad-get-arg 1)
;;                                                                    (ad-get-arg 2)
;;                                                                    (ad-get-arg 3))))
;;                               ad-do-it)))
;;                        ;; making the edit-window(s) not dedicated
;;                        (mapc (function (lambda (w)
;;                                          (set-window-dedicated-p w nil)))
;;                              edit-window-list)
;;                        ;;(modify-frame-parameters ecb-frame '((unsplittable . nil)))
;;                        (setq ecb-layout-temporary-dedicated-windows nil)) ;; end unwind-protect

;;                      ;; if called interactively we run now our
;;                      ;; `ecb-toggle-compile-window-height' to set the height of
;;                      ;; the compile-window according to the value of
;;                      ;; `ecb-enlarged-compilation-window-max-height'. If called
;;                      ;; non-interactively (e.g. by `compile-internal',
;;                      ;; `with-output-to-temp-buffer' etc...) then all the
;;                      ;; resizing or shrinking stuff is handled by
;;                      ;; `compilation-set-window-height',
;;                      ;; `resize-temp-buffer-window' (GNU Emacs) or
;;                      ;; `shrink-window-if-larger-than-buffer' (called by the
;;                      ;; SHRINK-TO-FIT arg of the XEmacs `display-buffer').

;;                      ;; We can not do this in the non-interactive case because
;;                      ;; here often after the call of display-buffer the buffer
;;                      ;; to display does not contain its final contents so the
;;                      ;; algorithm of `ecb-toggle-compile-window-height' fails
;;                      ;; (e.g. during `compile-internal'!).
;;                      (unless pop-up-frames
;;                        (if (interactive-p)
;;                            (ecb-set-compile-window-height)
;;                          (if (save-excursion
;;                                (set-buffer (ad-get-arg 0))
;;                                (= (point-min) (point-max)))
;;                              ;; Klaus Berndl <klaus.berndl@sdm.de>: If this
;;                              ;; makes trouble we remove it.
;;                              (ecb-toggle-compile-window-height -1)))
                       
;;                        (if (member ecb-compile-window-temporally-enlarge
;;                                    '(after-selection both))
;;                            (setq ecb-layout-prevent-handle-compile-window-selection t)))
;;                      ) ;; end of let...

;;                  ;; OK, we have really no compile-window...
               
;;                  ;; needed for TAB-completion if this offers the completions in
;;                  ;; a temp-buffer. Without this manually split the whole
;;                  ;; edit-window would be used for the completions which is not
;;                  ;; the default-behavior of Emacs.
;;                  (let ((pop-up-frames (if (ecb-ignore-pop-up-frames)
;;                                           nil
;;                                         pop-up-frames)))
;;                    ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: emacs 23
;;                    ;; splits automatically when window-size allows this (see
;;                    ;; split-width-threshold and split-height-threshold)...
;;                    ;; Test and ev. modify (not a serious problem but not nice)
;;                    (when (and (not ecb-windows-hidden)
;;                               (not (ecb-layout-top-p))
;;                               pop-up-windows
;;                               (not pop-up-frames)
;;                               (not (ecb-edit-window-splitted))
;;                               (not (ecb-check-for-same-window-buffer (ad-get-arg 0))))
;;                      (ecb-layout-debug-error "display-buffer for comp-buffer %s - split edit-window:"
;;                                              (ad-get-arg 0))
;;                      (split-window (car (ecb-canonical-edit-windows-list))))
;;                    ;; Here the values of temp-buffer-max-height and
;;                    ;; compilation-window-height take effect.
;;                    (if ecb-running-xemacs
;;                        (setq ad-return-value
;;                              (ecb-display-buffer-xemacs (ad-get-arg 0)
;;                                                         (ad-get-arg 1)
;;                                                         (ad-get-arg 2)
;;                                                         (ad-get-arg 3)))
;;                      ad-do-it))))
            
;;               ((not (ecb-buffer-is-dedicated-special-buffer-p (ad-get-arg 0)))
;;                (ecb-layout-debug-error "display-buffer for normal buffer: %s"
;;                                        (ad-get-arg 0))
;;                (let ((edit-win-list (ecb-canonical-edit-windows-list))
;;                      (pop-up-frames (if (ecb-ignore-pop-up-frames)
;;                                         nil
;;                                       pop-up-frames)))
;;                  ;; maybe we have to split the edit-area here
;;                  (when (and (or pop-up-windows (ad-get-arg 1))
;;                             (not pop-up-frames)
;;                             (not (ecb-edit-window-splitted edit-win-list))
;;                             ;; if the BUFFER is already displayed in an
;;                             ;; edit-window and NOT-THIS-WINDOW is nil then
;;                             ;; we must not split the edit-window because
;;                             ;; display-buffer then just uses this window for
;;                             ;; displaying BUFFER.
;;                             (not (and (not (ad-get-arg 1))
;;                                       (member (get-buffer-window (ad-get-arg 0) ecb-frame)
;;                                               edit-win-list)))
;;                             ;; if we display a "normal" buffer from outside
;;                             ;; the edit-windows then we have per se another
;;                             ;; window because the buffer will displayed in
;;                             ;; an edit-window ==> we only split if we are
;;                             ;; already in an edit-window.
;;                             (ecb-point-in-edit-window-number edit-win-list)
;;                             (not (ecb-check-for-same-window-buffer (ad-get-arg 0))))
;;                    (ecb-layout-debug-error "display-buffer for normal-buffer %s - split edit-window:"
;;                                            (ad-get-arg 0))
;;                    (split-window (car edit-win-list)))
;;                  (if (ecb-compile-window-live-p)
;;                      (unwind-protect
;;                          (progn
;;                            (set-window-dedicated-p ecb-compile-window t)
;;                            ;; now we perform the original `display-buffer' but
;;                            ;; now the only not dedicated window(s) are the
;;                            ;; edit-window(s)
;;                            (setq ecb-layout-temporary-dedicated-windows
;;                                  (list ecb-compile-window))
;;                            (if ecb-running-xemacs
;;                                (setq ad-return-value
;;                                      (ecb-display-buffer-xemacs (ad-get-arg 0)
;;                                                                 (ad-get-arg 1)
;;                                                                 (ad-get-arg 2)
;;                                                                 (ad-get-arg 3)))
;;                              ad-do-it))
;;                        ;; making the compile-window not dedicated
;;                        (set-window-dedicated-p ecb-compile-window nil)
;;                        (setq ecb-layout-temporary-dedicated-windows nil))
;;                    (if ecb-running-xemacs
;;                        (setq ad-return-value
;;                              (ecb-display-buffer-xemacs (ad-get-arg 0)
;;                                                         (ad-get-arg 1)
;;                                                         (ad-get-arg 2)
;;                                                         (ad-get-arg 3)))
;;                      ad-do-it))))
            
;;               (t ;; buffer is a special ecb-buffer
;;                (ecb-layout-debug-error "display-buffer for special ecb-buffer: %s" (ad-get-arg 0))
;;                (or (setq ad-return-value (get-buffer-window (ad-get-arg 0) ecb-frame))
;;                    (ecb-error "display-buffer can not display not visible ecb-buffers!")))))

;;     (ecb-layout-debug-error "display-buffer - just run original version.")
;;     (ecb-with-original-basic-functions
;;      (if ecb-running-xemacs
;;          (setq ad-return-value
;;                (ecb-display-buffer-xemacs (ad-get-arg 0)
;;                                           (ad-get-arg 1)
;;                                           (ad-get-arg 2)
;;                                           (ad-get-arg 3)))
;;        ad-do-it))))


(defun ecb-get-other-window-minibuf-active (win-list
                                            edit-win-list
                                            ecb-win-list
                                            comp-win
                                            minibuf-win
                                            point-loc
                                            nth-window)
  "Implements the situation of an active minibuffer, see
`ecb-other-window-behavior'."
  (let ((nth-win (or nth-window 1)))
    (if (equal (car point-loc) 'minibuf)
        (if (= nth-win 1)
            (or (if (and minibuffer-scroll-window
                         (window-live-p minibuffer-scroll-window)
                         (equal (window-frame minibuffer-scroll-window)
                                ecb-frame))
                    minibuffer-scroll-window)
                comp-win
                ecb-last-edit-window-with-point)
          (ecb-next-listelem (append edit-win-list
                                     (if comp-win
                                         (list comp-win))
                                     (if minibuf-win
                                         (list minibuf-win)))
                             (selected-window) nth-win))
      (ecb-next-listelem (append win-list (if minibuf-win (list minibuf-win)))
                         (selected-window) nth-win))))
  


(defun ecb-get-other-window-smart (win-list
                                   edit-win-list
                                   ecb-win-list
                                   comp-win
                                   minibuf-win
                                   point-loc
                                   nth-window)
  "Implements the smart-setting of `ecb-other-window-behavior'."
  (if minibuf-win
      ;; if we have an active mini-buffer we delegate this to
      ;; `ecb-get-other-window-minibuf-active'
      (ecb-get-other-window-minibuf-active win-list
                                           edit-win-list
                                           ecb-win-list
                                           comp-win
                                           minibuf-win
                                           point-loc
                                           nth-window)
    ;; here we have no active minibuffer!
    (let ((nth-win (or nth-window 1)))
      (case (car point-loc)
        (ecb
         (ecb-next-listelem (if (and ecb-win-list
                                     (= 1 (length ecb-win-list)))
                                (append ecb-win-list edit-win-list)
                              ecb-win-list)
                            (selected-window) nth-win))
        (compile
         (if (= nth-win 1)
             (or (and ecb-last-edit-window-with-point
                      (window-live-p ecb-last-edit-window-with-point)
                      ecb-last-edit-window-with-point)
                 (car edit-win-list))
           (ecb-next-listelem (append edit-win-list
                                      (list (selected-window)))
                              (selected-window)
                              nth-win)))
        (other-dedicated
         (ecb-next-listelem win-list
                            (selected-window)
                            nth-win))
        (otherwise ;; must be an edit-window
         (ecb-next-listelem (append edit-win-list
                                    (if (and comp-win
                                             (= (length edit-win-list)
                                                1))
                                        (list comp-win)))
                            (selected-window)
                            nth-win))))))

(defun ecb-get-other-window (nth-window)
  "Return the \"other window\" according to `ecb-other-window-behavior'.
Returns the window NTH-WINDOW steps away from the current window. If
NTH-WINDOW is nil then it is treated as 1."
  (let* ((nth-win (or nth-window 1))
         (windows-list (ecb-canonical-windows-list))
         (edit-win-list (ecb-canonical-edit-windows-list windows-list))
         (ecb-win-list (ecb-canonical-ecb-windows-list windows-list))
         (point-loc (ecb-where-is-point windows-list))
         (compwin-state (ecb-compile-window-state))
         (minibuf-win (if (> (minibuffer-depth) 0)
                          (minibuffer-window ecb-frame))))
    (if (functionp ecb-other-window-behavior)
        (let ((other-win (funcall ecb-other-window-behavior
                                  windows-list
                                  edit-win-list
                                  ecb-win-list
                                  (if (equal compwin-state 'visible)
                                      ecb-compile-window)
                                  minibuf-win
                                  point-loc
                                  nth-window)))
          (if (and other-win (window-live-p other-win))
              other-win
            (ecb-next-listelem (append windows-list (list minibuf-win))
                               (selected-window) nth-win)))
      (if minibuf-win
          (ecb-get-other-window-minibuf-active windows-list
                                               edit-win-list
                                               ecb-win-list
                                               (if (equal compwin-state 'visible)
                                                   ecb-compile-window)
                                               minibuf-win
                                               point-loc
                                               nth-window)
        ;; in the following there is no minibuffer active...
        (case ecb-other-window-behavior
          (all
           (ecb-next-listelem windows-list
                              (selected-window) nth-win))
          (only-edit
           (if (not (equal 'edit (car point-loc))) ;; point not in an edit-window
               (if (= nth-win 1)
                   (or (and ecb-last-edit-window-with-point
                            (window-live-p ecb-last-edit-window-with-point)
                            ecb-last-edit-window-with-point)
                       (car edit-win-list))
                 (ecb-next-listelem windows-list
                                    (selected-window) nth-win))
             (ecb-next-listelem edit-win-list
                                (selected-window) nth-win)))
          (edit-and-compile
           (if (member (car point-loc) '(ecb other-dedicated))
               (ecb-next-listelem windows-list
                                  (selected-window) nth-win)
             ;; point stays either in edit- or compile-window
             (ecb-next-listelem (append edit-win-list
                                        (if (equal compwin-state 'visible)
                                            (list ecb-compile-window)))
                                (selected-window)
                                nth-win)))
          (otherwise ;; = 'smart
           (ecb-get-other-window-smart windows-list
                                       edit-win-list
                                       ecb-win-list
                                       (if (equal compwin-state 'visible)
                                           ecb-compile-window)
                                       minibuf-win
                                       point-loc
                                       nth-window)))))))

;; (ecb-get-other-window-smart (ecb-canonical-windows-list) (ecb-canonical-edit-windows-list) (ecb-canonical-ecb-windows-list) nil nil '(other-dedicated . 2) 1)


(defecb-advice other-window around ecb-layout-basic-adviced-functions
  "The ECB-version of `other-window'. Works exactly like the original function
with the following ECB-adjustment: The behavior depends on
`ecb-other-window-behavior'."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       ad-do-it)
    (let* ((count (if (ad-get-arg 0)
                      (ad-get-arg 0)
                    1))
           (o-w (ecb-get-other-window count)))
      (select-window o-w))))


(defecb-advice delete-windows-on around ecb-layout-basic-adviced-functions
  "The ECB-version of `delete-windows-on'. Works exactly like the original
function with the following ECB-adjustment:

An error is reported if BUFFER is an ECB-tree-buffer. These windows are not
allowed to be deleted."
  (let ((curr-frame (selected-frame))
        (buf-name (or (and (stringp (ad-get-arg 0))
                           (ad-get-arg 0))
                      (and (bufferp (ad-get-arg 0))
                           (buffer-name (ad-get-arg 0)))))
        (frames (case (ad-get-arg 1)
                  (0 ;; visible or iconified frames
                   (delete nil (mapcar (lambda (f)
                                         (if (frame-visible-p f) f))
                                       (frame-list))))
                  (visible ;; visible frames
                   (delete nil (mapcar (lambda (f)
                                         (if (equal (frame-visible-p f) t)
                                             f))
                                       (frame-list))))
                  ((nil) ;; all frames
                   (frame-list))
                  ((t) ;; current frame
                   (list (selected-frame)))
                  (otherwise ;; a certain frame
                       (if (frame-live-p (ad-get-arg 1))
                           (list (ad-get-arg 1)))))))
    (if (member (get-buffer buf-name) (ecb-get-current-visible-ecb-buffers))
        (if ecb-advice-window-functions-signal-error
            (ecb-error "delete-windows-on is not allowed for the special ECB-buffers!"))
      (dolist (f frames)
        (if (not (equal f ecb-frame))
            (progn
              (ad-set-arg 1 f)
              ad-do-it)
          (when (get-buffer-window buf-name ecb-frame)
            (select-frame ecb-frame)
            ;; first we must delete the window
            (delete-window (get-buffer-window buf-name ecb-frame))
            ;; to get exactly the same behavior like the original version
            ;; we must check if the current-buffer in the edit-window is
            ;; the same as the buffer argument for the current call and if
            ;; yes we must switch to the buffer returned by `other-buffer'.
            (if (ecb-string= buf-name
                             (buffer-name (window-buffer (car (ecb-canonical-edit-windows-list)))))
                (switch-to-buffer (other-buffer buf-name
                                                nil ecb-frame)))
            (select-frame curr-frame)))))))

(defvar ecb-edit-area-creators nil)

(defsubst ecb-edit-area-creators-init ()
  (setq ecb-edit-area-creators nil))

(defsubst ecb-edit-area-creators-add (creator)
  (setq ecb-edit-area-creators (append ecb-edit-area-creators (list creator))))

(defun ecb-restore-edit-area ()
  (dolist (elem ecb-edit-area-creators)
    (let ((edit-win-list (ecb-canonical-edit-windows-list)))
      (select-window (nth (car elem) edit-win-list))
      (funcall (cdr elem)))))

(defun ecb-edit-area-creators-number-of-edit-windows ()
  (let ((dels (length (delq nil (mapcar (function (lambda (e)
                                                    (if (equal (cdr e)
                                                               'delete-window)
                                                        e
                                                      nil)))
                                        ecb-edit-area-creators)))))
    (- (1+ (- (length ecb-edit-area-creators) dels))
       dels)))
                              

(defecb-advice delete-window before ecb-permanent-adviced-layout-functions
  "Does nothing special but only storing the fact that the edit-window has
been deleted. This is done even when ECB is deactivated so ECB can later
restore the window-layout as if before activation. Normally there can not
occur an error in this advice but nevertheless this advice is constructed
error-save, i.e. if an error occurs during this before-advice then it will be
reported but `delete-window' will be executed correctly."
  (when (equal (window-frame (or (ad-get-arg 0) (selected-window)))
               ecb-frame)
    (condition-case oops
        (let* ((edit-win-list (ecb-canonical-edit-windows-list))
               (window (or (ad-get-arg 0) (selected-window)))
               (edit-win-number (ecb-position edit-win-list window)))
          (when edit-win-number
            (if (or (= (length edit-win-list) 1)
                    (/= (length edit-win-list)
                        (ecb-edit-area-creators-number-of-edit-windows)))
                (ecb-edit-area-creators-init)
              (if (= (length edit-win-list) 2)
                  ;; After the deletion of WINDOW there will be only one
                  ;; edit-window.
                  (ecb-edit-area-creators-init)
                ;; After the deletion of WINDOW there will be still more than
                ;; one edit-window.
                (ecb-edit-area-creators-add (cons edit-win-number
                                                  'delete-window))))))
      (error (ecb-warning "Before-advice delete-window (error-type: %S, error-data: %S)"
                          (car oops) (cdr oops))))))

(defecb-advice delete-window around ecb-layout-basic-adviced-functions
  "The ECB-version of `delete-window'. Works exactly like the original
function with the following ECB-adjustment:

If optional argument WINDOW is nil \(i.e. probably called interactively): If
called in a splitted edit-window then it works like as if all the edit-windows
would be the only windows in the frame. This means the current edit-window
which contains the point will be destroyed and its place will be occupied from
another one. If called in the compile-window of ECB then the compile-window
will be hidden \(like with `ecb-toggle-compile-window'). If called in an
unsplitted edit-window then nothing is done. If called in an ecb-window of the
current ECB-layout there are two alternatives:
- If the function is contained in `ecb-layout-always-operate-in-edit-window'
  the right edit-window is selected \(depends on the value of the option
  `ecb-mouse-click-destination') and does then it´s job.
- Otherwise the behavior depends on the value of the option
  `ecb-advice-window-functions-signal-error'.

If optional argument WINDOW is a living window \(i.e. called from program): If
WINDOW is an edit-window then this window is deleted, if WINDOW is the
compile-window then it will be hidden and otherwise the behavior depends on
`ecb-advice-window-functions-signal-error'."
  (if (or (not ecb-minor-mode)
          (not (equal (window-frame (or (ad-get-arg 0) (selected-window)))
                      ecb-frame))
          ;; we are in the ecb-frame but neither a compile-window nor the
          ;; ecb-windows are visible, so we have no windows to protect against
          ;; deletion.
          (and ecb-windows-hidden
               (not (ecb-compile-window-live-p)))
          ;; if all windows are dedicated (i.e. there is no edit-window left)
          ;; we allow deletion of all other windows (incl. ecb-windows and
          ;; compile-window
          ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: not perfect but...
          (null (ecb-canonical-edit-windows-list))
          )
      (ecb-with-original-basic-functions
       ad-do-it)
    (ecb-do-with-unfixed-ecb-buffers
     (when (and (interactive-p)
                (null (ad-get-arg 0))
                (member 'delete-window ecb-layout-always-operate-in-edit-window)
                (not (ecb-point-in-compile-window))
                ;; this is needed because otherwise we would also select
                ;; the 1. edit-window if point stays in another one!
                (not (ecb-point-in-edit-window-number)))
       (ecb-select-edit-window))
     
     (let* ((edit-win-list (ecb-canonical-edit-windows-list))
            (window (or (ad-get-arg 0) (selected-window)))
            (edit-win-number (ecb-position edit-win-list window))
            (curr-window-before (selected-window)))
       (cond ((equal window ecb-compile-window)
              (ecb-toggle-compile-window -1))
             ((null edit-win-number)
              (if ecb-advice-window-functions-signal-error
                  (ecb-error "Only an edit-window can be deleted!")))
             (t
              (ad-with-originals 'delete-window
                (when (> (length edit-win-list) 1)
                  (if ecb-windows-hidden
                      (delete-window window)
                    (funcall (intern (format "ecb-delete-window-in-editwindow-%s"
                                             ecb-layout-name))
                             window edit-win-list))
                  ;; If we have deleted that window which was current at call-time
                  ;; we have to ensure that point stays in the next edit-window
                  (when (equal curr-window-before window)
                    (let ((edit-win-list-after (ecb-canonical-edit-windows-list)))
                      (if (not (member (selected-window) edit-win-list-after))
                          (select-window (car edit-win-list-after)))))))))))))

(defecb-advice delete-other-windows before ecb-permanent-adviced-layout-functions
  "Does nothing special but only storing the fact that the other edit-windows
have been deleted. This is done even when ECB is deactivated so ECB can later
restore the window-layout as if before activation. Normally there can not
occur an error in this advice but nevertheless this advice is constructed
error-save, i.e. if an error occurs during this before-advice then it will be
reported but `delete-window' will be executed correctly."
  (when (and (equal (window-frame (or (ad-get-arg 0) (selected-window)))
                    ecb-frame)
             ;; The case when ecb is active is handled completely by
             ;; delete-window!
             (not ecb-minor-mode))
    (condition-case oops
        (let* ((edit-win-list (ecb-canonical-edit-windows-list))
               (window (or (ad-get-arg 0) (selected-window)))
               (edit-win-number (ecb-position edit-win-list window)))
          (when edit-win-number
            ;; After the deletion of the other edit-windows there will be only
            ;; one edit-window. We can init the edit-area-creators always
            ;; regardless of current number of edit-windows.
            (ecb-edit-area-creators-init)))
      (error (ecb-warning "Before-advice delete-other-windows (error-type: %S, error-data: %S)"
                          (car oops) (cdr oops))))))

(defecb-advice delete-other-windows around ecb-layout-basic-adviced-functions
  "The ECB-version of `delete-other-windows'. Works exactly like the
original function with the following ECB-adjustment:

If optional argument WINDOW is nil \(i.e. probably called interactively): If
called in a splitted edit-window then it works like as if all the edit-windows
would be the only windows in the frame. This means all other edit-windows
besides the current edit-window which contains the point will be destroyed and
the current edit-window fills the whole edit-area. Neither the special
ecb-windows nor the compile-window will be destroyed!
- If called in an unsplitted edit-window then either the compile-window will
  be hidden \(if there is one) or nothing is done.
- If called in one of the ecb-windows then the current one is maximized, i.e.
  the other ecb-windows \(not the edit-windows!) are deleted.
- If called in the compile window there are two
  alternatives:
  + If the function is contained in `ecb-layout-always-operate-in-edit-window'
    the right edit-window is selected \(depends on the value of the option
    `ecb-mouse-click-destination') and then it does it´s job.
  + Otherwise the behavior depends on the value of the option
  `ecb-advice-window-functions-signal-error'.

If optional argument WINDOW is a living window \(i.e. called from program): If
WINDOW is an edit-window then this window is maximized \(i.e. the other
edit-window is deleted) if there are more at least 2 edit-windows otherwise
the compile-window is deleted \(if there is one). If WINDOW is an ecb-window
then only the other ecb-windows are deleted and in all other cases the
behavior depends on `ecb-advice-window-functions-signal-error'."
  
  (if (or (not ecb-minor-mode)
          (not (equal (window-frame (or (ad-get-arg 0) (selected-window)))
                                    ecb-frame))
          ;; we are in the ecb-frame but neither a compile-window nor the
          ;; ecb-windows are visible, so we have no windows to protect against
          ;; deletion.
          (and ecb-windows-hidden
               (not (ecb-compile-window-live-p)))
          ;; if all windows are dedicated (i.e. there is no edit-window left)
          ;; we allow deletion of all other windows (incl. ecb-windows and
          ;; compile-window
          ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: not perfect but...
          (null (ecb-canonical-edit-windows-list))
          )
      (ecb-with-original-basic-functions
       ad-do-it)
    (ecb-do-with-unfixed-ecb-buffers
     (when (and (interactive-p)
                (null (ad-get-arg 0))
                (member 'delete-other-windows
                        ecb-layout-always-operate-in-edit-window)
                ;; this is needed because otherwise we would also select
                ;; the 1. edit-window if point stays in another one!
                ;; if in an ecb-window then we stay there because then we
                ;; want to maximize the current ecb-window
                (ecb-point-in-compile-window))
       (ecb-select-edit-window))
     
     (let ((edit-win-list (ecb-canonical-edit-windows-list))
           (window (or (ad-get-arg 0) (selected-window))))
       (cond ((equal window ecb-compile-window)
              (if ecb-advice-window-functions-signal-error
                  (ecb-error "The compile window can not be maximized!")))
             ((integerp (ecb-position edit-win-list window))
              ;; we run the adviced version of delete-window for each "other"
              ;; edit-window
              (if (= (length edit-win-list) 1)
                  (ecb-toggle-compile-window -1)
                (dolist (ew (delete window edit-win-list))
                  (delete-window ew))))
             (t ;; must be one of the special ecb-windows
              (ecb-maximize-ecb-buffer (buffer-name (window-buffer window)) t)))))))
            
  
(defecb-advice split-window-horizontally around ecb-layout-basic-adviced-functions
  "The ECB-version of `split-window-horizontally'. Works exactly like the
original function with the following ECB-adjustment:

If called in any other window of the current ECB-layout it stops with an error
if this `split-window-horizontally' is not contained in the option
`ecb-layout-always-operate-in-edit-window'!"
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       ad-do-it)
    (when (and (member 'split-window-horizontally
                       ecb-layout-always-operate-in-edit-window)
               ;; this is needed because otherwise we would also select the 1.
               ;; edit-window if point stays in another one!
               (not (ecb-point-in-edit-window-number)))
      (ecb-select-edit-window))
    ad-do-it))

(defecb-advice split-window-vertically around ecb-layout-basic-adviced-functions
  "The ECB-version of `split-window-vertically'. Works exactly like the
original function with the following ECB-adjustment:

If called in any other window of the current ECB-layout it stops with an error
if this `split-window-vertically' is not contained in the option
`ecb-layout-always-operate-in-edit-window'!"
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       ad-do-it)
    (when (and (member 'split-window-vertically
                       ecb-layout-always-operate-in-edit-window)
               (not (ecb-point-in-edit-window-number)))
      (ecb-select-edit-window))
    ad-do-it))

(defecb-advice split-window before ecb-permanent-adviced-layout-functions
  "Does nothing special but only storing the fact that the window is splitted.
This is done even when ECB is deactivated so ECB can later restore the
window-layout as if before activation. Normally there can not occur an error
in this advice but nevertheless this advice is constructed error-save, i.e. if
an error occurs during this before-advice then it will be reported but
`split-window' will be executed correctly."
  (when (equal (window-frame (or (ad-get-arg 0) (selected-window)))
               ecb-frame)
    (condition-case oops
        (let* ((edit-win-list (ecb-canonical-edit-windows-list))
               (window (or (ad-get-arg 0) (selected-window)))
               (edit-win-number (ecb-position edit-win-list window)))
          (when (and edit-win-number
                     (or (= (length edit-win-list) 1)
                         (/= (length edit-win-list)
                             (ecb-edit-area-creators-number-of-edit-windows))))
            (ecb-edit-area-creators-init))
          (when edit-win-number
            (ecb-edit-area-creators-add (cons edit-win-number
                                              (if (ad-get-arg 2)
                                                  'split-window-horizontally
                                                'split-window-vertically)))))
      (error (ecb-warning "Before-advice split-window (error-type: %S, error-data: %S)"
                          (car oops) (cdr oops))))))
      

(defecb-advice split-window around ecb-layout-basic-adviced-functions
  "The ECB-version of `split-window'. Works exactly like the original function
with the following ECB-adjustment:

If called for a not-edit-window in the `ecb-frame' it stops with an error if
`split-window' is not contained in the option
`ecb-layout-always-operate-in-edit-window'! Besides this \(e.g. called for a
window in another frame than the `ecb-frame') it behaves like the original
version."
  (ecb-layout-debug-error "split-window entered for window: %s" (ad-get-arg 0))
  (if (or (not ecb-minor-mode)
          (not (equal (window-frame (or (ad-get-arg 0) (selected-window)))
                      ecb-frame)))
      (ecb-with-original-basic-functions
       ad-do-it)
    ;; if called interactively and WINDOW is nil (i.e. selected window is
    ;; used) then we maybe must first go to the edit-window.
    ;; The check for interactiv-p prevents that we jump to the edit-window if
    ;; called from within `split-window-vertically' for example.
    (when (and (interactive-p)
               (null (ad-get-arg 0))
               (member 'split-window
                       ecb-layout-always-operate-in-edit-window)
               (not (ecb-point-in-edit-window-number)))
      (ecb-select-edit-window))

    ;; now perform the splitting task
    (let* ((window (or (ad-get-arg 0) (selected-window)))
           (edit-win-number (ecb-position (ecb-canonical-edit-windows-list)
                                          window)))
      (if edit-win-number
          ad-do-it
        (if ecb-advice-window-functions-signal-error
            (ecb-error "Only the edit-windows of ECB are split-able!")
          (setq ad-return-value (selected-window)))))))

(defecb-advice switch-to-buffer-other-window around ecb-layout-basic-adviced-functions
  "The ECB-version of `switch-to-buffer-other-window'. Works exactly like the
original but with some adaptions for ECB so this function works in a
\"natural\" way:

If called in any special ecb-window of the current ECB-layout then it goes
always to an edit-window \(which one depends on the setting in
`ecb-mouse-click-destination') and then goes on as if called from this
edit-window.

If a compile-window is used \(i.e. `ecb-compile-window-height' is not nil)
then \"compilation-buffers\" in the sense of `ecb-compilation-buffer-p' are
always displayed in the compile-window. If the compile-window is temporally
hidden then it will be displayed first. If no compile-window is used it
behaves like the original.

If called from within the compile-window then \"compilation-buffers\" will be
displayed still there and all other buffers are displayed in one of the
edit-windows - if the destination-buffer is already displayed in one of the
edit-windows then this one is used otherwise it behaves like the original.

If called within an edit-window it behaves like the original function except
for compilation-buffers \(if a compile-window is used, see above)."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       ad-do-it)
    (if (equal (car (ecb-where-is-point)) 'ecb)
        (ecb-select-edit-window))
    (let ((pop-up-windows t)
          ;; Don't let these interfere...
          same-window-buffer-names same-window-regexps)          
      (pop-to-buffer (ad-get-arg 0) t
                     (if ecb-running-xemacs
                         (selected-frame)
                       (ad-get-arg 1))))))


;; Klaus Berndl <klaus.berndl@sdm.de>: We can not use pop-to-buffer here
;; because with XEmacs there is an error max-lisp-eval-depth exceeded. XEmacs
;; implements display-buffer (which is called by pop-to-buffer) internally
;; with switch-to-buffer so there is an bidirectional dependency - ugly :-(
(defecb-advice switch-to-buffer around ecb-layout-basic-adviced-functions
  "The ECB-version of `switch-to-buffer'. Works exactly like the original but
with the following enhancements for ECB:

\"compilation-buffers\" in the sense of `ecb-compilation-buffer-p' will be
displayed always in the compile-window of ECB \(if `ecb-compile-window-height'
is not nil) - if the compile-window is temporally hidden then it will be
displayed first. If you do not want this you have to modify the options
`ecb-compilation-buffer-names', `ecb-compilation-major-modes' or
`ecb-compilation-predicates'.

If called for non \"compilation-buffers\" \(s.a.) from outside the edit-area
of ECB it behaves as if called from an edit-window if `switch-to-buffer' is
contained in the option `ecb-layout-always-operate-in-edit-window'. Otherwise
an error is reported."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       ad-do-it)
    (ecb-layout-debug-error "switch-to-buffer buffer: %s" (ad-get-arg 0))
    (cond ((ecb-compilation-buffer-p (ad-get-arg 0))
           (when (equal 'hidden (ecb-compile-window-state))
             (ecb-toggle-compile-window 1))
           (if (ecb-compile-window-live-p)
               (select-window ecb-compile-window))
           ;; now we must handle if there is still no compile-window and
           ;; therefore point can still stay in an ecb-window
           (if (equal (car (ecb-where-is-point)) 'ecb)
               (if (member 'switch-to-buffer ecb-layout-always-operate-in-edit-window)
                   (ecb-select-edit-window)
                 (ecb-error "switch-to-buffer: Can not switch to %s in an ecb-window!"
                            (ad-get-arg 0)))))
          ((ecb-buffer-is-dedicated-special-buffer-p (ad-get-arg 0))
           (if (get-buffer-window (ad-get-arg 0) ecb-frame)
               (select-window (get-buffer-window (ad-get-arg 0) ecb-frame))
             (ecb-error "switch-to-buffer: Can only switch to visible special ecb-buffers!")))
          (t ;; normal buffers
           (if (member (car (ecb-where-is-point)) '(ecb compile))
               (if (member 'switch-to-buffer ecb-layout-always-operate-in-edit-window)
                   (ecb-select-edit-window)
                 (ecb-error "switch-to-buffer: Can only switch to %s in an edit-window!"
                            (ad-get-arg 0))))))
    ;; now we stay in the correct window
    (ecb-with-original-basic-functions
     ad-do-it)
    (ecb-layout-debug-error "switch-to-buffer curr-buffer: %s" (current-buffer))
    (when (ecb-point-in-compile-window)
      (ecb-layout-debug-error "switch-to-buffer curr-buffer: %s, curr window %s"
                              (current-buffer) (selected-window))
      ;; we set the height of the compile-window according to
      ;; `ecb-enlarged-compilation-window-max-height'
      (ecb-set-compile-window-height))))

(defecb-advice other-window-for-scrolling around ecb-layout-basic-adviced-functions
  "This function determines the window which is scrolled if any of the
\"other-window-scrolling-functions\" is called \(e.g. `scroll-other-window'):

If the option `ecb-scroll-other-window-scrolls-compile-window' is not nil
\(maybe set by `ecb-toggle-scroll-other-window-scrolls-compile') and a
compile-window is visible then always the current buffer in the compile-window
is scrolled!

Otherwise it depends completely on the setting in `ecb-other-window-behavior'."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame))
          (and (equal (selected-window) (minibuffer-window ecb-frame))
               minibuffer-scroll-window))
      (ecb-with-original-basic-functions
       ad-do-it)
    (let* ((o-w-s-b 
            (if (and ecb-scroll-other-window-scrolls-compile-window
                     (not (ecb-point-in-compile-window))
                     (equal 'visible (ecb-compile-window-state)))
                (window-buffer ecb-compile-window)
              (window-buffer (ecb-get-other-window nil))))
           (other-window-scroll-buffer (if (equal (current-buffer) o-w-s-b)
                                           nil
                                         o-w-s-b)))
      ad-do-it)))

(defecb-advice walk-windows around ecb-always-disabled-advices
  "Walk only through the edit-windows of ECB. When ECB is not active or
called for other frames than for the `ecb-frame' then act like the original.
This adviced version of `walk-windows' is not for direct usage therefore it is
added to `ecb-always-disabled-advices' and therefore always disabled; use the
macro `ecb-with-ecb-advice' instead if you need this adviced version of
`walk-windows'!"
  (if (and ecb-minor-mode
           (or (equal (ad-get-arg 2) ecb-frame)
               (and (null (ad-get-arg 2))
                    (equal (selected-frame) ecb-frame))))
      (progn
        (let ((ecb-walk-windows-advice-proc (ad-get-arg 0)))
          (ad-with-originals 'walk-windows
            (walk-windows (function (lambda (w)
                                      (if (or (ecb-buffer-is-ecb-buffer-of-current-layout-p
                                               (window-buffer w))
                                              (equal w ecb-compile-window))
                                          nil
                                        ;; for an edit-window we call the
                                        ;; original PROC
                                        (funcall ecb-walk-windows-advice-proc w))))
                          (ad-get-arg 1)
                          (ad-get-arg 2)))))
    ad-do-it))

(defecb-advice balance-windows around ecb-layout-basic-adviced-functions
  "When called in the `ecb-frame' then only the edit-windows are balanced."
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame)
           (not ecb-windows-hidden))
      (if ecb-running-version-22
          ;; Emacs 22 has reimplemented balance-windows so it is not longer based on
          ;; walk-windows but uses a completely new mechanism based on a
          ;; c-level-function `window-tree'! Therefore we have to use another
          ;; mechanism which just restore the size of the ecb-windows as
          ;; before belance-windows...
          (let ((ecb-sizes-before (ecb-get-ecb-window-sizes t)))
            (ecb-do-with-fixed-ecb-buffers ad-do-it)
            ;; this seems to be necessary - otherwise the reszing seems not to
            ;; take effect...
            (sit-for 0)
            (ignore-errors (ecb-set-ecb-window-sizes ecb-sizes-before)))
        ;; with Emacs 21 running the adviced version of walk-windows is
        ;; sufficient
        (ecb-with-ecb-advice 'walk-windows 'around
          ad-do-it))
    ad-do-it))


;;======= Helper-functions ===========================================

(defun ecb-split-hor (amount &optional dont-switch-window use-frame)
  "Splits the current-window horizontally and returns the absolute amount in
columns. If AMOUNT is greater than -1.0 and lower than +1.0 then the value is
multiplied with the current window-width \(frame-width if USE-FRAME is not nil)."
  (let ((abs-amout (ecb-normalize-number amount (if use-frame
                                                    (frame-width)
                                                  (window-width)))))
    (ecb-split-hor-abs abs-amout dont-switch-window)
    abs-amout))

(defun ecb-split-hor-abs (amount &optional dont-switch-window)
  (split-window-horizontally amount)
  (if (not dont-switch-window)
      (select-window (next-window))))

(defun ecb-split-ver (amount &optional dont-switch-window use-frame)
  "Splits the current-window and returns the absolute amount in lines. If
AMOUNT is greater than -1.0 and lower than +1.0 then the value is multiplied
with the current window-height \(frame-height if USE-FRAME is not nil)."
  (let ((abs-amout (ecb-normalize-number amount (if use-frame
                                                    (1- (frame-height))
                                                  (ecb-window-full-height)))))
    (ecb-split-ver-abs abs-amout dont-switch-window)
    abs-amout))

(defun ecb-split-ver-abs (amount &optional dont-switch-window)
  (split-window-vertically amount)
  (if (not dont-switch-window)
      (select-window (next-window))))

;;======= The new layout mechanism========================================

;; Klaus: Completely rewritten the layout mechanism to make it more
;; straightforward, more customizable by users and slightly more
;; convenient.

(defvar ecb-buffer-setfunction-registration nil
  "An alist where for each `buffer-name' of a special ecb-buffer - displayed
in a dedicated window - a function must be registered which displays that
buffer in current window and makes this window dedicated to this buffer. So
for every ecb-buffer a cons cell must be added to this alist where car is
`buffer-name' and cdr is the symbol of the setting-function.

The setting function of such a buffer must be defined with the macro
`defecb-window-dedicator' and do:

1. switch to that buffer in current window
2. all things necessary for this buffer - e.g. making it read-only

The setting function must ensure that the current window is still current at
the end and that the related ecb-buffer is displayed in this window at the
end.

One examples of such a setting function is `ecb-set-history-buffer' for the
buffer with name `ecb-history-buffer-name'.")

(defun ecb-dedicated-special-buffers ()
  "Return a list of the special dedicated buffers which are registrated via
the macro `defecb-window-dedicator' \(these are normally only the standard
tree-buffers of ECB plus the integrated speedbar-buffer, but in general it can
be more if there are additional buffers registrated, e.g. by other
applications). The value returned is independend from the currently *visible*
special ecb-buffers and therefore also from the current layout. If the
currently visible ECB-buffers are needed then use the function
`ecb-get-current-visible-ecb-buffers'. "
  (delq nil (mapcar (function (lambda (e)
                                (get-buffer (car e))))
                    ecb-buffer-setfunction-registration)))

(defun ecb-get-current-visible-ecb-buffers ()
  "Return a list of all buffer-objects displayed in a currently visible and
dedicated special ecb-window. The superset of all possible \(because
registered) special ecb-buffers are available by
`ecb-dedicated-special-buffers'."
  (mapcar (function (lambda (window)
                      (window-buffer window)))
          (ecb-canonical-ecb-windows-list)))

(defun ecb-buffer-is-visible-ecb-buffer-p (buffer-or-name)
  "Return not nil if BUFFER-OR-NAME is a member of
`ecb-get-current-visible-ecb-buffers'. BUFFER-OR-NAME ca be either a
buffer-object or a buffer-name."
  (let ((buffer (ecb-buffer-obj buffer-or-name)))
    (member buffer (ecb-get-current-visible-ecb-buffers))))

(defun ecb-buffer-is-ecb-buffer-of-current-layout-p (buffer-or-name)
  "Return not nil if BUFFER-OR-NAME is a member of
`ecb-special-ecb-buffers-of-current-layout', means BUFFER-OR-NAME is one of
that buffers which build up the current-layout as it is defined.
BUFFER-OR-NAME ca be either a buffer-object or a buffer-name."
  (let ((buff-name (ecb-buffer-name buffer-or-name)))
    (member buff-name ecb-special-ecb-buffers-of-current-layout)))

(defun ecb-buffer-is-the-only-visible-ecb-buffer-p (buffer-or-name)
  "Return not nil if BUFFER-OR-NAME is currently the only visible ecb-buffer."
  (let ((buffer (ecb-buffer-obj buffer-or-name))
        (current-ecb-buffers (ecb-get-current-visible-ecb-buffers)))
    (and (= (length current-ecb-buffers) 1)
         (equal buffer (car current-ecb-buffers)))))

(defun ecb-set-minor-mode-text ()
  (setq ecb-minor-mode-text
        (if ecb-windows-hidden
            (or (ecb-option-get-value 'ecb-minor-mode-text 'saved-value)
                (ecb-option-get-value 'ecb-minor-mode-text 'standard-value))
          "")))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: For left-right-layouts: Not only
;; hiding all the ecb-windows but offering to hide only one of the left or the
;; right column. Maybe toggling in the sequence "Hide left" --> "Hide all" -->
;; Hide right" --> "Show all". But i (Klaus) think this is not so easy........
(defun ecb-toggle-ecb-windows (&optional arg)
  "Toggle visibility of the ECB-windows.
With prefix argument ARG, make visible if positive, otherwise invisible.
This has nothing to do with \(de)activating ECB but only affects the
visibility of the ECB windows. ECB minor mode remains active!"
  (interactive "P")
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))

    (let ((new-state (if (null arg)
                         (not ecb-windows-hidden)
                       (<= (prefix-numeric-value arg) 0))))
      (if (not new-state)
          (progn
            (run-hooks 'ecb-show-ecb-windows-before-hook)
            (if (ecb-show-any-node-info-by-mouse-moving-p)
                (tree-buffer-activate-follow-mouse))
            ;; if `ecb-buffer-is-maximized-p' is not nil then this means we
            ;; should only restore this one maximized buffer!
            (let ((compwin-hidden (equal 'hidden
                                         (ecb-compile-window-state))))
              (if (ecb-buffer-is-maximized-p)
                   (ecb-maximize-ecb-buffer ecb-current-maximized-ecb-buffer-name)
                (ecb-redraw-layout-full))
              (if compwin-hidden
                  (ecb-toggle-compile-window -1)))
            (run-hooks 'ecb-show-ecb-windows-after-hook)
            (message "ECB windows are now visible."))
        (unless ecb-windows-hidden
          (run-hooks 'ecb-hide-ecb-windows-before-hook)
          (tree-buffer-deactivate-follow-mouse)
            (let ((compwin-hidden (equal 'hidden
                                         (ecb-compile-window-state))))
              (ecb-redraw-layout-full nil nil nil t)
              (if compwin-hidden
                  (ecb-toggle-compile-window -1)))
          (run-hooks 'ecb-hide-ecb-windows-after-hook)
          (message "ECB windows are now hidden."))))))

(defun ecb-hide-ecb-windows ()
  "Hide the ECB windows if not already hidden."
  (interactive)
  (ecb-toggle-ecb-windows 0))

(defun ecb-show-ecb-windows ()
  "Make the ECB windows visible."
  (interactive)
  (ecb-toggle-ecb-windows 1))


(defvar ecb-current-maximized-ecb-buffer-name nil
  "If not nil then it contains the buffer-name of the current maximized
ecb-buffer. If nil then this means currently there is no ecb-buffer maximized.

Do not set this variable. It is only set by `ecb-redraw-layout-full' and
`ecb-maximize-ecb-buffer'.")

(defun ecb-redraw-layout-preserving-compwin-state ()
  "Redraw current layout with all ECB-windows visible."
  (interactive)
  (let ((compwin-state (ecb-compile-window-state)))
    (ecb-redraw-layout-full)
    (if (equal compwin-state 'hidden)
        (ecb-toggle-compile-window -1))))

(defun ecb-toggle-maximize-ecb-window-with-mouse ()
  "Mouse-wrapper for `ecb-toggle-maximize-ecb-window'."
  (interactive "@")
  (ecb-toggle-maximize-ecb-window))

(defun ecb-toggle-maximize-ecb-window (&optional ecb-buffer-name)
  "Toggle maximizing the special ecb-window of ECB-BUFFER-NAME.
If there is a maximizied ecb-window then it will be \"minimized\", i.e. all
ecb-windows of current layout will be displayed. If there is no ecb-window
maximized then that of ECB-BUFFER-NAME will be maximized. If ECB-BUFFER-NAME
is nil then the current buffer-name is used. Nothing will be done if the
caller tries to maximize a non-ecb-window."
  (if (ecb-buffer-is-maximized-p)
      (ecb-undo-maximize-ecb-buffer)
    (let ((buf-name (or ecb-buffer-name (buffer-name))))
      (when (ecb-buffer-is-ecb-buffer-of-current-layout-p buf-name)
        (ecb-maximize-ecb-buffer buf-name t)))))

(defun ecb-undo-maximize-ecb-buffer (&optional preserve-selected-window)
  "Undo the maximizing of an ecb-buffer.
If optional arg PRESERVE-SELECTED-WINDOW is not nil then the currently
selected window does not change. Otherwise after displaying all ecb-windows the
current edit-window is selected."
  (when (equal (selected-frame) ecb-frame)
    (let ((curr-loc (ecb-where-is-point))
          (prev-buffer-name (buffer-name)))
      (ecb-redraw-layout-preserving-compwin-state)
      ;; point is now in the edit-buffer so maybe we have to move point to the
      ;; buffer where it was before.
      (when preserve-selected-window
        (case (car curr-loc)
          (ecb
           (ecb-window-select prev-buffer-name))
          (compile
           (ecb-window-select ecb-compile-window))))
      (ecb-info-message "Maximizing has been undone."))))

(defun ecb-maximized-tree-buffer-name ()
  "Return the currently maximized tree-buffer-name or nil if there is none."
  ecb-current-maximized-ecb-buffer-name)

(defun ecb-buffer-is-maximized-p (&optional ecb-buffer-name)
  "Not nil if ECB-BUFFER-NAME is currently maximized.
If ECB-BUFFER-NAME is not nil and not part of the current layout then nil is
returned. If ECB-BUFFER-NAME is nil then not nil is returned if any special
buffer of current layout is maximized otherwise nil."
  (if ecb-buffer-name
      (and (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-buffer-name)
           (equal ecb-buffer-name
                  ecb-current-maximized-ecb-buffer-name))
    ecb-current-maximized-ecb-buffer-name))

(defun ecb-maximize-ecb-buffer (ecb-buffer-name &optional preserve-selected-window)
  "Maximize that window which displays the special ECB-buffer ECB-BUFFER-NAME.
Afterwards ECB-BUFFER-NAME is the only visible special ECB-buffer. If optional
arg PRESERVE-SELECTED-WINDOW is nil then after maximizing always the current
edit-window is selected and if not nil then the currently selected window-type
does not change which means: If any ecb-window was selected before maximizing
then after maximizing the maximized ecb-window is selected \(regardless if its
the same as before the maximizing). If the compile window was selected before
then it will be selected also after. If an edit-window was selected before it
will be selected also after."
  (when (equal (selected-frame) ecb-frame)
    (let ((curr-point (ecb-where-is-point))
          (compwin-hidden (equal 'hidden (ecb-compile-window-state))))
      ;; maximize the window if ECB-BUFFER-NAME is one of the special
      ;; ecb-buffers of current layout
      (when (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-buffer-name)
        (ecb-redraw-layout-full
         t ;; no buffer synchronisation!
         (cdr (assoc ecb-buffer-name
                     ecb-buffer-setfunction-registration)))
        (if compwin-hidden (ecb-toggle-compile-window -1))
        (setq ecb-current-maximized-ecb-buffer-name ecb-buffer-name)
        ;; point is now in the edit-buffer so maybe we have to move point to the
        ;; buffer where it was before.
        (when preserve-selected-window
          (case (car curr-point)
            (ecb
             (ecb-window-select ecb-buffer-name))
            (compile
             (ecb-window-select ecb-compile-window))))))))

(defvar ecb-cycle-ecb-buffer-state nil
  "State of ecb-buffer-cycling. An alist where the car is the list of all
buffer-names of the ecb-buffers of current layout and the cdr the index which
buffer-name is the next one in the cycle-sequence. Is only set by
`ecb-redraw-layout-full' and `ecb-cycle-maximized-ecb-buffers'.")

(defun ecb-cycle-maximized-ecb-buffers ()
  "Cycles through all ecb-buffers by maximizing one at each step."
  (interactive)
  (when (null ecb-cycle-ecb-buffer-state)
    ;; we have redrawn the complete layout and therefore the cycle-state
    ;; has been reset. If we start the cycling with an already maximized
    ;; ecb-window we have first redraw fully so we get a correct
    ;; initialization!
    (ecb-redraw-layout-full)
    (setq ecb-cycle-ecb-buffer-state
          (cons (mapcar (function (lambda (w)
                                    (buffer-name (window-buffer w))))
                        (ecb-canonical-ecb-windows-list))
                0)))
  ;; now we have a valid cycle-state so we can display the next ecb-buffer
  (ecb-maximize-ecb-buffer (nth (cdr ecb-cycle-ecb-buffer-state)
                                (car ecb-cycle-ecb-buffer-state))
                              t)
  ;; now we have to move forward the cycle-state
  (setcdr ecb-cycle-ecb-buffer-state
          (if (= (cdr ecb-cycle-ecb-buffer-state)
                 (1- (length (car ecb-cycle-ecb-buffer-state))))
              0
            (1+ (cdr ecb-cycle-ecb-buffer-state)))))


(defun ecb-window-configuration-data ()
  "Return current window configuration of the ecb-frame as a list with the
following structure:
1. The number of the edit-window if point is one of the edit-windows, nil
   otherwise
2. current point if one of the edit-windows is selected, nil otherwise.
3. Data of all edit-windows in form of a list: Everey listelement is a list
   again with first subelement is the buffer of an edit-window, second
   subelement is the `window-start' of this window, third is the
   `window-point' and fourth subelement is the result of `ecb-get-window-size'
   for this window. This data-list has the same ordering as
   `ecb-canonical-edit-windows-list'.
4. Data of the compile window or nil \(if there is no compile-window visible):
   List with first elem is the buffer of the compile-window, second elem is
   current point of the compile-buffer if the compile-window is selected
   \(otherwise nil) and third elem is the current height of the
   compile-window.
5. The window sizes of the ecb-windows as returned by
   `ecb-get-ecb-window-sizes'"
  (let* ((win-list (ecb-canonical-windows-list))
         (edit-win-list (ecb-canonical-edit-windows-list win-list))
         (ecb-win-list (ecb-canonical-ecb-windows-list win-list))
         (point-pos (ecb-where-is-point win-list))
         (edit-area-size (ecb-get-edit-area-size win-list)))
    (list (if (equal 'edit (car point-pos)) (cdr point-pos))
          (if (equal 'edit (car point-pos)) (point))
          (mapcar (function (lambda (win)
                              (list (window-buffer win)
                                    (window-start win)
                                    (window-point win)
                                    (ecb-get-window-size win nil edit-area-size))))
                  edit-win-list)
          (if (equal 'visible (ecb-compile-window-state))
              (list (window-buffer ecb-compile-window)
                    (if (equal (car point-pos) 'compile) (point))
                    (ecb-window-full-height ecb-compile-window)))
          (ecb-get-ecb-window-sizes nil ecb-win-list)
          )))

;; =================== Helper functions ==================================

(defmacro ecb-with-dedicated-window (&rest body)
  "Make current selected window not dedicated, evaluate BODY in current
window and make this window dedicated at the end. Even if an error occurs
during evaluating BODY the current window is always dedicated at the end!"
  `(unwind-protect
       (progn
         (set-window-dedicated-p (selected-window) nil)
         ,@body)
     (set-window-dedicated-p (selected-window) t)))

(defmacro defecb-window-dedicator (creator buffer-name docstring &rest body)
  "Define a function CREATOR which makes the selected window dedicated to the
BUFFER-NAME. Do not quote CREATOR. DOCSTRING is the docstring for CREATOR.
BODY is all the program-code of CREATOR which will be run encapsulated within
a call to `ecb-with-dedicated-window'.

Example:

\(defecb-window-dedicator ecb-set-history-buffer ecb-history-buffer-name
  \"Display the History-buffer in current window and make window
dedicated.\"
  \(switch-to-buffer ecb-history-buffer-name))

This defines a function `ecb-set-history-buffer' registered as
\"window-dedicator\" for the buffer with name `ecb-history-buffer-name'. The
BODY \(in this example only a call to switch-to-buffer) will run within the
macro `ecb-with-dedicated-window'!"
  `(eval-and-compile
     (defun ,creator ()
       ,docstring
       (add-to-list 'ecb-buffer-setfunction-registration
                    (cons ,buffer-name (quote ,creator)))
       (ecb-with-dedicated-window
        ,@body))))

(put 'defecb-window-dedicator 'lisp-indent-function 2)

(defecb-window-dedicator ecb-set-speedbar-buffer ecb-speedbar-buffer-name
  "Display in current window the speedbar-buffer and make window dedicated."
  (ecb-speedbar-set-buffer))

(defecb-window-dedicator ecb-set-default-ecb-buffer " *ECB-default-buffer*"
  "Set in the current window the default ecb-buffer which is useless but is
used if a layout calls within its creation body a non bound
ecb-buffer-setting-function."
  (switch-to-buffer (get-buffer-create " *ECB-default-buffer*"))
  (when (= (buffer-size) 0)
    (insert " This is the default\n")
    (insert " ecb-buffer which is\n")
    (insert " useless. Probably this\n")
    (insert " buffer is displayed\n")
    (insert " because the layout uses\n")
    (insert " an unbound window-\n")
    (insert " dedicator!"))
  (setq buffer-read-only t))


;; ======== Delete-window-functions for the different layout-types ==========

;; There are three different types of layouts:
;; 1. Ecb-windows on the left side (include layouts with left and right side
;;    ECB-windows)
;; 2. Ecb-windows on the right side
;; 3. Ecb-windows on the top
;; For each type we have a special replacement for `delete-window' which
;; operates correctly as if all the edit-windows would be in an extra frame.

;; 1. Ecb-windows on the left side

(defun ecb-delete-window-ecb-windows-left (window edit-win-list)
  (let ((ecb-win-width-before (window-width (frame-first-window ecb-frame)))
        (curr-edit-win-width (ecb-window-full-width window)))
    (delete-window window)
    (when (/= (window-width (frame-first-window ecb-frame))
              ecb-win-width-before)
      ;; We have to select here the "next" edit-window explicitly because if
      ;; WINDOW is not the selected-window `delete-window' does not select the
      ;; next window!
      (save-selected-window
        (select-window (ecb-next-listelem edit-win-list window))
        (enlarge-window (+ curr-edit-win-width
                           (if ecb-running-xemacs
                               (if scrollbars-visible-p 3 1)
                             0))
                        t)))))

(defalias 'ecb-delete-window-ecb-windows-left-right
  'ecb-delete-window-ecb-windows-left)

;; 2. Ecb-windows on the right side

(defun ecb-delete-window-ecb-windows-right (window edit-win-list)
  (delete-window window))

;; 3. Ecb-windows on the top

(defun ecb-delete-window-ecb-windows-top (window edit-win-list)
  (let ((ecb-win-height-before (ecb-window-full-height (frame-first-window ecb-frame)))
        (curr-edit-win-height (ecb-window-full-height window)))
    (delete-window window)
    (when (/= (ecb-window-full-height (frame-first-window ecb-frame))
              ecb-win-height-before)
      (save-selected-window
        (select-window (ecb-next-listelem edit-win-list window))
        (enlarge-window curr-edit-win-height)))))


(defconst ecb-layout-types '(left right top left-right))

(defun ecb-layout-type-p (type &optional err)
  (if (not (member type ecb-layout-types))
      (if err
          (error "Only left, right, top and left-right are allowed as types!")
        nil)
    t))

(defvar ecb-available-layouts nil
  "List of all current available layout names. Do not change this variable!
This variable is only modified by `ecb-available-layouts-add' and
`ecb-available-layouts-remove'. These functions are only called by
`ecb-layout-define' and `ecb-layout-undefine'!")

;; Accessors for `ecb-available-layouts':
(defun ecb-available-layouts-of-type (type)
  "Return a list of all layout-names for given type TYPE. Type must be an
element of `ecb-layout-types' or nil \(then return all layout-names
regardless of the type)."
  (if type (ecb-layout-type-p type t))
  (delete nil (mapcar (function (lambda (elem)
                                  (if (or (not type)
                                          (equal (cdr elem) type))
                                      (car elem))))
                      ecb-available-layouts)))

(defun ecb-available-layouts-member-p (layout-name)
  "Return a non nil value iff LAYOUT-NAME is the name of a layout of
`ecb-available-layouts'."
  (member layout-name (ecb-available-layouts-of-type nil)))

(defun ecb-available-layouts-add (name type)
  "Add layout with NAME and TYPE to `ecb-available-layouts'. NAME is a string
and TYPE must be an element of `ecb-layout-types'."
  (add-to-list 'ecb-available-layouts (cons name type))
  (setq ecb-available-layouts
        (sort ecb-available-layouts
              (function (lambda (l r)
                          (ecb-string< (car l) (car r)))))))

(defun ecb-available-layouts-remove (name)
  "Remove layout with NAME from `ecb-available-layouts'."
  (let ((elem (assoc name ecb-available-layouts)))
    (when elem
      (setq ecb-available-layouts
            (sort (delete elem ecb-available-layouts)
                  (function (lambda (l r)
                              (ecb-string< (car l) (car r)))))))))

(defun ecb-get-layout-type (&optional name)
  "Return the type of current layout or of layout NAME."
  (let ((n (or name ecb-layout-name)))
    (cdr (assoc n ecb-available-layouts))))

(defun ecb-layout-left-p (&optional name)
  "Return not nil if current layout or layout NAME is of type left."
  (equal 'left (ecb-get-layout-type name)))

(defun ecb-layout-leftright-p (&optional name)
  "Return not nil if current layout or layout NAME is of type left-right."
  (equal 'left-right (ecb-get-layout-type name)))

(defun ecb-layout-right-p (&optional name)
  "Return not nil if current layout or layout NAME is of type right."
  (equal 'right (ecb-get-layout-type name)))

(defun ecb-layout-top-p (&optional name)
  "Return not nil if current layout or layout NAME is of type top."
  (equal 'top (ecb-get-layout-type name)))

;; Macro for easy defining new layouts
(defmacro ecb-layout-define (name type doc &rest create-code)
  "Creates a new ECB-layout with name NAME which must be a string. TYPE is the
type of the new layout and is literal, i.e. not evaluated. It can be left,
right, top or left-right. DOC is the docstring for the new layout-function
\"ecb-layout-function-<name>\". CREATE-CODE is all the lisp code which is
necessary to define the ECB-windows/buffers. This macro adds the layout with
NAME and TYPE to the internal variable `ecb-available-layouts'.

Preconditions for CREATE-CODE:
1. Current frame is splitted at least in one edit-window and the column\(s)
   (for layout types left, right and left-right) rsp. row \(for a top layout)
   for the special ECB-windows/buffers. Depending on the value of the option
   `ecb-compile-window-height' there is also a compile window at the bottom of
   the frame which is stored in `ecb-compile-window'.

2. All windows are not dedicated.

3. Neither the edit-window nor the compile-window \(if there is one) are
   selected for types left, right and top. For type left-right the left
   column-window is selected.

4. All ECB-advices for the functions in `ecb-layout-basic-adviced-functions' are
   disabled!

Things CREATE-CODE has to do:
1. Splitting the ECB-windows-column\(s)/row \(s.a.) in all the ECB-windows the
   layout should contain \(e.g. directories, sources, methods and history).
   The split must not be done with other functions than `ecb-split-hor' and
   `ecb-split-ver'! It is recommended not to to use a \"hard\" number of
   split-lines or -columns but using fractions between -0.9 and +0.9! Tip: It
   is recommended to spilt from right to left and from bottom to top or with
   other words: First create the right-most and bottom-most special windows!

2. Making each special ECB-window a dedicated window. This can be done with
   one of the following functions:
   + `ecb-set-directories-buffer'
   + `ecb-set-sources-buffer'
   + `ecb-set-methods-buffer'
   + `ecb-set-history-buffer'
   + `ecb-set-speedbar-buffer'
   Each layout can only contain one of each tree-buffer-type!

   In addition to these functions there is a general macro:
   + `defecb-window-dedicator':
   This macro defines a so called \"window-dedicator\" which is a function
   registered at ECB and called by ECB to perform any arbitrary code in
   current window and makes the window autom. dedicated at the end. This can
   be used by third party packages like JDEE to create arbitrary ECB-windows
   besides the standard tree-windows.

   To make a special ECB-window a dedicated window either one of the five
   functions above must be used or a new \"window-dedicator\"-function has to
   be defined with `defecb-window-dedicator' and must be used within the
   layout-definition.

3. Every\(!) special ECB-window must be dedicated as described in 2.

4. CREATE-CODE must work correctly regardless if there is already a
   compile-window \(stored in `ecb-compile-window') or not
   \(`ecb-compile-window' is nil)

Things CREATE-CODE can do or can use:
1. Using the values of `ecb-compile-window-height', `ecb-windows-width',
   `ecb-windows-height' and `ecb-compile-window-width'.

Things CREATE-CODE must NOT do:
1. Splitting the edit-window
2. Creating a compile-window
3. Deleting the edit-window, the compile-window \(if there is any) or the
   ECB-windows-column\(s)/row \(see Precondition 1.)
4. Referring to the values of `ecb-edit-window' or `ecb-compile-window'
   because these values are always nil or undefined during CREATE-CODE.
5. Using the function `ecb-compile-window-live-p'.

Postconditions for CREATE-CODE:
1. The edit-window must be the selected window and must not be dedicated.
2. Every window besides the edit-window \(and the compile-window) must be
   a dedicated window \(e.g. a ECB-tree-window)."
  `(progn
     (ecb-layout-type-p (quote ,type) t)
     (eval-and-compile
       (defun ,(intern (format "ecb-layout-function-%s" name)) (&optional create-code-fcn)
         ,doc
         ;; Klaus Berndl <klaus.berndl@sdm.de>: creating the compile-window is
         ;; now done in `ecb-redraw-layout-full'!
         ;; (when (and ecb-compile-window-height
         ;;            (or (equal ecb-compile-window-width 'frame)
         ;;                (equal (ecb-get-layout-type ecb-layout-name) 'top)))
         ;;   (ecb-split-ver (- ecb-compile-window-height) t t)
         ;;   (setq ecb-compile-window (next-window)))
         ,(cond ((equal type 'left)
                 '(ecb-split-hor ecb-windows-width t))
                ((equal type 'right)
                 '(ecb-split-hor (- ecb-windows-width) nil))
                ((equal type 'top)
                 '(ecb-split-ver ecb-windows-height t))
                ((equal type 'left-right)
                 '(progn
                    (ecb-split-hor (- ecb-windows-width) t)
                    (ecb-split-hor ecb-windows-width t t))))
         ;; if create-code-fcn is not nil and we have not a left-right layout
         ;; then we call this function instead of create-code - afterwards we
         ;; have to select the edit-window. If create-code-fcn is nil then the
         ;; leftmost-topmost ecb-window-column/bar is selected.
         (if (and create-code-fcn
                  (not (equal (ecb-get-layout-type ecb-layout-name) 'left-right)))
             (progn
               (funcall create-code-fcn)
               (select-window (next-window)))
           ,@create-code)
         ;; Klaus Berndl <klaus.berndl@sdm.de>: creating the compile-window is
         ;; now done in `ecb-redraw-layout-full'!
         ;; (when (and ecb-compile-window-height
         ;;            (equal ecb-compile-window-width 'edit-window)
         ;;            (not (equal (ecb-get-layout-type ecb-layout-name) 'top)))
         ;;   (ecb-split-ver (- ecb-compile-window-height) t t)
         ;;   (setq ecb-compile-window (next-window)))
         (setq ecb-edit-window (selected-window)))
       (defalias (quote ,(intern
                          (format "ecb-delete-window-in-editwindow-%s"
                                  name)))
         (quote ,(intern
                  (format "ecb-delete-window-ecb-windows-%s" type)))))
     (ecb-available-layouts-add ,name (quote ,type))))

;; we want proper editing with ecb-layout-define like follows:
;; (ecb-layout-define "name" left
;;   "documentation" or nil
;;   ;; here comes the creation code
;;   )
(put 'ecb-layout-define 'lisp-indent-function 2)

(defun ecb-layout-undefine (name)
  "Unbind ecb-layout-function-<NAME>, ecb-delete-window-ecb-windows-<NAME>,
ecb-delete-other-windows-ecb-windows-<NAME> and remove NAME from
`ecb-available-layouts'."
  (fmakunbound (intern (format "ecb-layout-function-%s" name)))
  (fmakunbound (intern (format "ecb-delete-window-ecb-windows-%s" name)))
  (ecb-available-layouts-remove name))


(defun ecb-choose-layout-name (layout-list require-match)
  "Calls `completing-read' for LAYOUT-LIST which is a list of layout-names.
For REQUIRE-MATCH see documentation of `completing-read'. For a null input the
first element of LAYOUT-LIST is returned."
  (let ((result (completing-read "Insert a layout name: "
                                 (mapcar (function (lambda (x) (list x t)))
                                         layout-list)
                                 nil require-match)))
    (if (= (length result) 0)
        (car layout-list)
      result)))

(defun ecb-layout-switch (name)
  "Switch to layout with layout-name NAME."
  (let ((comp-win-state (ecb-compile-window-state)))
    (customize-set-variable 'ecb-layout-name name)
    (if (and ecb-change-layout-preserves-compwin-state
             (equal comp-win-state 'hidden))
        (ecb-toggle-compile-window -1))))
  

(defun ecb-change-layout (&optional preselect-type)
  "Change to one of current available layouts.
For this TAB-completion is offered. If optional argument PRESELECT-TYPE is not
nil then you can preselect a layout-type \(TAB-completion is offered too) and
then will be asked only for layouts of that preselected type.

Note: Do not use this function from within elisp-programs; use
`ecb-layout-switch'!"
  (interactive "P")
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (let ((type (if preselect-type
                    (intern (ecb-query-string
                             "Insert a layout type:"
                             (mapcar (function (lambda (elem)
                                                 (symbol-name elem)))
                                     ecb-layout-types))))))
      (ecb-layout-switch (ecb-choose-layout-name
                          (ecb-available-layouts-of-type type) t)))))

(defun ecb-show-layout-help ()
  "Select a layout-name and shows the documentation of the layout-function.
At least for the built-in layouts the documentation contains a picture of the
outline of the chosen layout."
  (interactive)
  ;; ensure we have load all layouts defined until now
  (ecb-load-layouts)
  (describe-function
   (intern (format "ecb-layout-function-%s"
                   (ecb-choose-layout-name (ecb-available-layouts-of-type nil)
                                           t)))))

;; the window-configuration cache with some accessors

;; TODO: Make this advices permanent.
(defvar ecb-window-config-cache-size 200)
(defvar ecb-window-config-cache nil
  "A ring-cache for ecb-window-configurations. Max. size is
`ecb-window-config-cache-size'. If a new element is added and the cache is
already full then the oldest item is dumped to make room.")

(defun ecb-window-config-cache-add (ecb-window-config)
  "Add ECB-WINDOW-CONFIG to the `ecb-window-config-cache'-ring. The new config
will be added as the newest \(last) item. If the cache is full \(see
`ecb-window-config-cache-size'), dump the oldest item to make room."
  (ecb-window-config-cache-init)
  (ecb-ring-insert ecb-window-config-cache ecb-window-config))

(defun ecb-window-config-cache-get (emacs-window-config)
  "Get the cache-element for the EMACS-WINDOW-CONFIG which is
window-configuration-object like returned by `current-window-configuration'.
If the cache does not contain such an element then nil is returned."
  (if (and (ecb-ring-p ecb-window-config-cache)
           (not (ecb-ring-empty-p ecb-window-config-cache)))
      (let ((configs (ecb-ring-elements ecb-window-config-cache)))
        (assq emacs-window-config configs))))

(defun ecb-window-config-cache-get-latest ()
  "Get the newest \(last) cache-item."
  (if (and (ecb-ring-p ecb-window-config-cache)
           (not (ecb-ring-empty-p ecb-window-config-cache)))
      (ecb-ring-ref ecb-window-config-cache 0)))

(defun ecb-window-config-cache-init ()
  "Initialize the cache as a ring of size `ecb-window-config-cache-size' if
not already initialized."
  (or (ecb-ring-p ecb-window-config-cache)
      (setq ecb-window-config-cache
            (ecb-make-ring ecb-window-config-cache-size))))

(defun ecb-window-config-cache-clear ()
  "Clear the cache."
  (setq ecb-window-config-cache nil))

;; handling window-configurations 

(defun ecb-window-configuration-invalidp (window-config)
  "Return non nil when WINDOW-CONFIG is probably not valid anymore.
WINDOW-CONFIG must be got from the adviced version of
`current-window-configuration'."
  (not (equal (nth 3 window-config)
              (list ecb-frame ecb-layout-name ecb-compile-window-height
                    ecb-compile-window-width
                    ecb-windows-width ecb-windows-height))))


(defecb-advice current-window-configuration after ecb-layout-basic-adviced-functions
  "Stores some additional informations about the window-configurations needed
by ECB."
  (condition-case oops
      (let ((f (or (ad-get-arg 0) (selected-frame))))
        (when (equal f ecb-frame)
          (ecb-window-config-cache-add
           (list ad-return-value
                 (if ecb-windows-hidden
                     nil
                   (ecb-get-current-visible-ecb-buffers))
                 (if (ecb-compile-window-live-p)
                     (ecb-position (ecb-canonical-windows-list)
                                   ecb-compile-window))
                 ;; We add here as first element `ecb-frame' and also in the
                 ;; check of `ecb-window-configuration-invalidp'! Then a
                 ;; ecb-window-config made from a frame which is now deleted
                 ;; would be always invalid, which would be more consistency
                 ;; with the return-value of `set-window-configuration' (see
                 ;; docstring)
                 ;;
                 ;; This element is only used in
                 ;; `ecb-window-configuration-invalidp'!
                 (list ecb-frame ecb-layout-name ecb-compile-window-height
                       ecb-compile-window-width
                       ecb-windows-width ecb-windows-height)
                 ecb-edit-area-creators
                 ecb-windows-hidden
                 (ecb-window-configuration-data)))))
    (error
     (ecb-layout-debug-error "advice of current-window-configuration failed: (error-type: %S, error-data: %S)"
                             (car oops) (cdr oops))))
  ad-return-value)


(defecb-advice set-window-configuration after ecb-layout-basic-adviced-functions
  "Resets some internal window-configuration-states needed by ECB. These
internal ECB-states were stored by `current-window-configuration' in a
ring-cache as add-on to CONFIGURATION."
  (condition-case oops
      (when (equal (selected-frame) ecb-frame)
        (let ((config (ecb-window-config-cache-get (ad-get-arg 0))))
          (when (and config
                     (not (ecb-window-configuration-invalidp config)))
            (ecb-make-windows-not-dedicated ecb-frame)
            ;; we have to reset the dedicated state because it is not
            ;; preserved by `current-window-configuration' and
            ;; `set-window-configuration'! At least not with GNU Emacs 21.X,
            ;; In addition we have to reset ecb-compile-window and also to set
            ;; ecb-windows-hidden correctly
            (and (nth 1 config)
                 (ecb-set-windows-dedicated-state (nth 1 config) t))
            (when (nth 2 config)
              (let ((win-list (ecb-canonical-windows-list)))
                (and ecb-compile-window-height
                     (setq ecb-compile-window (nth (nth 2 config) win-list)))))
            ;; (nth 3 config) is not used and needed within this function!
            (setq ecb-edit-area-creators (nth 4 config))
            (setq ecb-windows-hidden (nth 5 config))
            (ecb-set-minor-mode-text))))
    (error
     (ecb-layout-debug-error "advice of set-window-configuration failed: (error-type: %S, error-data: %S)"
                             (car oops) (cdr oops))))
  ad-return-value)

(when-ecb-running-xemacs
 (defecb-advice set-window-configuration/mapping after ecb-layout-basic-adviced-functions
   "If `set-window-configuration' changes the values of `ecb-edit-window',
`ecb-last-edit-window-with-point' or `ecb-compile-window', this advice reset
them to the new values to allow ecb to run at all in XEmacs 21.5"
   (let ((edit-window-changed (assq ecb-edit-window ad-return-value))
         (last-edit-window-with-point-changed (assq ecb-last-edit-window-with-point ad-return-value))
         (compile-window-changed (assq ecb-compile-window ad-return-value)))
     (if edit-window-changed
         (setq ecb-edit-window (cdr edit-window-changed)))
     (if last-edit-window-with-point-changed
         (setq ecb-last-edit-window-with-point (cdr last-edit-window-with-point-changed)))
     (if compile-window-changed
         (setq ecb-compile-window (cdr compile-window-changed)))))
 )


(defun ecb-current-window-configuration ()
  "Return the current ecb-window-configuration"
  (progn
    (current-window-configuration)
    (ecb-window-config-cache-get-latest)))

(defun ecb-set-window-configuration (ecb-window-config)
  "Sets the window-configuration of ECB-WINDOW-CONFIG. The additional
informations needed by ECB will be set by the adviced version of
`set-window-configuration'."
  (set-window-configuration (car ecb-window-config)))

(defmacro ecb-save-window-excursion (&rest body)
  "Same as `save-window-excursion' but it takes care of the ECB-needs."
  (let ((current-window-config (make-symbol "curr-win-conf")))
    `(let ((,current-window-config (ecb-current-window-configuration)))
       (unwind-protect
           (progn
             ,@body)
         (ecb-set-window-configuration ,current-window-config)))))

;; test of the advices of set-window-configuration and
;; current-window-configuration.

;; Show a compile-window and split the edit-area and then run this code. At
;; the end the layout has to be as before.
;;   (let ((config (current-window-configuration)))
;;     (ecb-with-original-basic-functions
;;      (delete-other-windows))
;;     ;; set ecb-compile-window and ecb-edit-area-creators to some crap. If the
;;     ;; test is ok, then the adviced set-window-configuration must reset these
;;     ;; variables correctly!
;;     (setq ecb-compile-window nil)
;;     (setq ecb-edit-area-creators '(1 2 3 4))
;;     (set-window-configuration config))

;; run `ecb-test-store' when some splitted layout is active. Then do anything
;; what you want to change the split-state of the layout and/or the
;; visibility-state of the compile-window. Then run `ecb-test-restore': This
;; has to bring back completely the stored configuration!
;;   (defvar ecb-test-config nil)
;;   (defun ecb-test-store ()
;;     (interactive)
;;     (setq ecb-test-config (current-window-configuration)))
;;   (defun ecb-test-restore ()
;;     (interactive)
;;     (set-window-configuration ecb-test-config))


;; redrawing the layout

(defun ecb-redraw-layout (&optional arg)
  "Redraw the ECB screen.

Do not call this command from elisp-program but only interactively!

Called without a prefix-argument the state of the ECB-frame-layout will
preserved. This means:
- The state of compile-window \(hidden or visible) will be preserved but if
  visible then the height will be as specified in `ecb-compile-window-height'.
- The state of the ECB-windows will be preserved \(hidden or visible) but if
  visible then the sizes will be as specified in the layout \(and with the
  options `ecb-windows-width' and `ecb-windows-height') or as stored with
  `ecb-store-window-sizes'.

If called with ONE prefix-argument \(\[C-u]) then the layout will be drawn
with all ECB-windows and also with a visible compile-window \(when
`ecb-compile-window-height' is not nil). The splitting-state of the edit-area
will be preserved.

If called with TWO prefix-arguments \(i.e. hitting \[C-u] twice: \[C-u]
\[C-u]) then an emergency-redraw will be performed. This means the same as if
called with one prefix-argument \(s.a.) but the splitting-state of the
edit-area will NOT be preserved but all edit-windows besides the current one
will be deleted. Use this only if there are some anomalies after standard
redraws!

If the variable `ecb-redraw-layout-quickly' is not nil then the redraw is done
by the `ecb-redraw-layout-quickly' function, otherwise by
`ecb-redraw-layout-full'.

Please not: It's strongly recommended to use the quick redraw only if you have
really slow machines where a full redraw takes several seconds because the
quick redraw is not really safe and has some annoying drawbacks! On normal
machines the full redraw should be done in << 1s so there should be no need
for the quick version!"
  (interactive "P")

  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (let ((compwin-hidden (equal (ecb-compile-window-state) 'hidden)))
      (message "ECB redrawing layout...")
    
      (if (and ecb-redraw-layout-quickly
               ecb-activated-window-configuration)
          (condition-case nil
              (ecb-redraw-layout-quickly)
            (error (message "ECB: Quick redraw failed...full redraw will be performed!")
                   (ecb-redraw-layout-full nil nil nil (and (not arg)
                                                            ecb-windows-hidden)
                                           (equal arg '(16)))))
        (ecb-redraw-layout-full nil nil nil (and (not arg)
                                                 ecb-windows-hidden)
                                (equal arg '(16))))
      
      (if (and (not arg) compwin-hidden)
          (ecb-toggle-compile-window -1)))

    ;;make sure we are in the edit window if necessary.
    (when ecb-select-edit-window-on-redraw
      (ecb-select-edit-window))
  
    (message "ECB redrawing layout...done")))


(defecb-autocontrol/sync-function ecb-repair-only-ecb-window-layout nil nil nil
  "Repair the ecb-window layout if it has been destroyed."
  ;; In the following situation repairing the layout with preserving all
  ;; states of all edit-windows and the compile-window (incl. all sizes) makes
  ;; sense:
  ;; 1. There is a permanent compile-window visible
  ;; 2. The ecb-windows are not hidden
  ;; 3. there is no ecb-window maximized
  ;; 4. There is no active minibuffer
  ;; 5. we have less ecb-windows than we should have
  ;; 6. Emacs does not wait for output of a running process where the
  ;;    associated process-buffer is visible in the ecb-frame

  ;; Then we redraw the layout with the current window-configuration-data
  ;; (i.e. all data of all edit-windows and all data of the compile window) so
  ;; we get back all ecb-windows of current lyout but preserve the
  ;; edit-windows and also the compile-window (incl. its height).
  (if (and (ecb-compile-window-live-p)
           (not ecb-windows-hidden)
           (not (ecb-buffer-is-maximized-p))
           (not (minibuffer-window-active-p (minibuffer-window ecb-frame)))
           (not (equal (mapcar 'buffer-name
                               (ecb-get-current-visible-ecb-buffers))
                       ecb-special-ecb-buffers-of-current-layout))
           ;; Klaus Berndl <klaus.berndl@sdm.de>: Currently this is not
           ;; perfect, because for example with a *shell* buffer visible this
           ;; returns nil even if the shell-buffer will probably not be in use
           ;; by Emacs - but for savety we have to incl. *shell* because a
           ;; long running shell-job could run where the job inserts output in
           ;; the shell-buffer.
           (not (delq nil (mapcar (function (lambda (p)
                                              (and (process-buffer p)
                                                   (get-buffer-window (process-buffer p)
                                                                      ecb-frame))))
                                  (process-list)))))
      (let ((config-data (ecb-window-configuration-data))
            ;; (win-config-before (ecb-current-window-configuration))
            (success nil))
        (ecb-layout-debug-error "ecb-repair-ecb-window-layout: Current ecb-windows: %s"
                                (ecb-get-current-visible-ecb-buffers))
        (ecb-layout-debug-error "ecb-repair-ecb-window-layout: We repair with data: %s"
                                config-data)
        (setq success (condition-case oops
                          (progn
                            (ecb-redraw-layout-full nil nil config-data)
                            (equal (mapcar 'buffer-name
                                           (ecb-get-current-visible-ecb-buffers))
                                   ecb-special-ecb-buffers-of-current-layout))
                        (error
                         (ecb-layout-debug-error "ecb-repair-only-ecb-window-layout failed (error-type: %S, error-data: %S)"
                                                 (car oops) (cdr oops))
                         nil)))
        (when (not success)
          ;; Reseting to the window-config before is good when done
          ;; interactively but not with an idle-times because then this reset
          ;; would be done until the user creates a window-config where no
          ;; repair is necessary or at least the repair doesn't fail. So we
          ;; have to implement a smarter mechanism..............
          nil ;; (ecb-set-window-configuration win-config-before)
          ))))

                        

(defun ecb-draw-compile-window (&optional height)
  "Draws the compile-window during `ecb-redraw-layout-full'. This function
does not change the selected window! It sets `ecb-compile-window' and
`ecb-compile-window-height-lines'. If HEIGHT is not nil then the
compile-window will drawn with height HEIGHT otherwise
`ecb-compile-window-height' is used."
  (ecb-split-ver (- ecb-compile-window-height) t t)
  (setq ecb-compile-window (next-window))
  ;; now we store the absolut height of the compile-window - if there is
  ;; any - we need this height to always have the absolut height in
  ;; lines when we want shrink back the compile-window to its
  ;; specified height.
  (setq ecb-compile-window-height-lines
        (ecb-window-full-height ecb-compile-window))
  ;; now we change the height of the compile-window if
  ;; WINDOW-CONFIGURATION-DATA is not nil and contains data for a
  ;; compile-window. This must be done before the layout-function
  ;; is called so the ecb-windows will be created also with correct
  ;; sizes when the compile-window is enlarged.
  (when height
    (save-selected-window
      (select-window ecb-compile-window)
      (enlarge-window (- height ecb-compile-window-height-lines)))))
  

;; the main layout core-function. This function is the "environment" for a
;; special layout function (l.b.)
(defun ecb-redraw-layout-full (&optional no-buffer-sync ecb-windows-creator
                                         window-configuration-data
                                         no-ecb-windows emergency)
  "Redraw the ECB screen according to the layout set in `ecb-layout-name'. After
this function the edit-window is selected which was current before redrawing.
If NO-BUFFER-SYNC is not nil then the ecb-buffers will not be synchronized. If
ECB-WINDOWS-CREATOR is not nil then it will be used to draw the layout instead
of the standard layout. If WINDOW-CONFIGURATION-DATA is not nil it must be an
object returned by `ecb-window-configuration-data' and will be used for
restoring the layout. If EMERGENCY is not nil then all other args will be
ignored and the layout will be redrawn like defined in the current layout and
the edit-area will be unsplitted and will just contain the buffer before the
emergency-redraw."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    ;; this functions are only needed at runtime!
    (ecb-load-layouts)
    (run-hooks 'ecb-redraw-layout-before-hook)
    (let* ((config (or window-configuration-data (ecb-window-configuration-data)))
           (window-before-redraw (nth 0 config))
           (pos-before-redraw (nth 1 config))
           (edit-win-data-before-redraw (nth 2 config))
           (compile-window-config (nth 3 config))
           (compile-buffer-before-redraw (nth 0 compile-window-config))
           (compile-buffer-pos-before-redraw (nth 1 compile-window-config))
           (ecb-windows-before-redraw (ecb-get-current-visible-ecb-buffers))
           (edit-area-size nil)
           (edit-win-list-after-redraw nil))

      (ecb-layout-debug-error "ecb-redraw-layout-full: config: %s, hidden-state: %s, curr-buff: %s, last-source-buff: %s"
                              config ecb-windows-hidden (current-buffer)
                              ecb-last-source-buffer)
      
      ;; The following code runs with deactivated adviced functions, so the
      ;; layout-functions can use the original function-definitions.
      (ecb-with-original-basic-functions
       (ecb-with-original-permanent-layout-functions
        ;; first we go to the edit-window/buffer
        (ecb-select-edit-window)

        (ecb-do-with-unfixed-ecb-buffers
         ;; Do some actions regardless of the chosen layout

         ;; first we make all windows of ecb-frame not dedicated and then we
         ;; delete all other windows so we have a clean frame with only one
         ;; window where we can draw our layout. We restore later the
         ;; frame-state (splits, buffers, points etc.)
         (ecb-make-windows-not-dedicated ecb-frame)
         (delete-other-windows)
         ;; some paranoia...
         (set-window-dedicated-p (selected-window) nil)

         ;; we force a layout-function to set both of these windows
         ;; correctly.
         (setq ecb-edit-window nil
               ecb-compile-window nil)

         ;; Now we draw the compile-window and also the ecb-windows - the
         ;; latter ones by calling the layout-function
         (if (and (not emergency) no-ecb-windows)
             ;; we want a layout-redraw without ecb-windows
             (progn
               (when ecb-compile-window-height
                 (ecb-draw-compile-window (and window-configuration-data
                                               compile-window-config
                                               (nth 2 compile-window-config)))
                 )
               (setq ecb-edit-window (selected-window)))

           ;; we have to redraw with ecb-windows
           ;; 1. Drawing the compile-window when it has frame-width
           (when (and ecb-compile-window-height
                      (or (equal ecb-compile-window-width 'frame)
                          (equal (ecb-get-layout-type ecb-layout-name) 'top)))
             (ecb-draw-compile-window (and window-configuration-data
                                           compile-window-config
                                           (nth 2 compile-window-config))))

           ;; 2. Drawing the ecb-windows with the layout-function
           (funcall (intern (format "ecb-layout-function-%s" ecb-layout-name))
                    ecb-windows-creator)

           ;; 3. Drawing the compile-window when it has edit-area-width
           (when (and ecb-compile-window-height
                      (equal ecb-compile-window-width 'edit-window)
                      (not (equal (ecb-get-layout-type ecb-layout-name) 'top)))
             (ecb-draw-compile-window (and window-configuration-data
                                           compile-window-config
                                           (nth 2 compile-window-config)))))

         ;; Now we store the window-sizes of the ecb-windows but only if we
         ;; have drawn them either without a compile-window or with a
         ;; compile-window with height as specified in
         ;; `ecb-compile-window-height'
         (if (or (not ecb-compile-window-height-lines)
                 (not (and window-configuration-data
                           compile-window-config)))
             (setq ecb-layout-default-window-sizes (ecb-get-ecb-window-sizes)))

         ;; Here all needed windows are created

         ;; selecting the edit-window (this is an implicit-check if the
         ;; layout-function has set `ecb-edit-window'
         (select-window
          (if ecb-edit-window
              ecb-edit-window
            (error "Edit-window not set in function 'ecb-layout-function-%s"
                   ecb-layout-name)))

         ;; resetting some states if we have a full layout
         (when (or emergency
                   (and (null ecb-windows-creator) (not no-ecb-windows)))
           (setq ecb-current-maximized-ecb-buffer-name nil)
           (setq ecb-cycle-ecb-buffer-state nil))

         ;; now regardless of the value of ECB-WINDOWS-CREATOR and
         ;; NO-ECB-WINDOWS the selected window is the edit-window and
         ;; ecb-edit-window is set to this window.

         ) ;; end ecb-do-with-unfixed-ecb-buffers

        (if emergency
            (setq ecb-windows-hidden nil)
          (setq ecb-windows-hidden no-ecb-windows))
        (ecb-set-minor-mode-text)

        ;; Now all the windows must be created and the editing window must not
        ;; be splitted! In addition the variables `ecb-edit-window' and
        ;; `ecb-compile-window' must be set to the correct windows.

        ;; The following when-expression is added for better relayouting the
        ;; chosen layout if we have a compilation-window.
        (when ecb-compile-window-height
          (if (not (ecb-compile-window-live-p))
              (error "Compilation-window not set in the layout-function"))
          (set-window-buffer
           ecb-compile-window
           (or (and compile-buffer-before-redraw
                    (ecb-compilation-buffer-p compile-buffer-before-redraw))
               (ecb-some (function (lambda (buf)
                                     (and (not (ecb-string= "*Completions*"
                                                            (ecb-buffer-name buf)))
                                          (not (ecb-check-for-special-buffer buf))
                                          (ecb-compilation-buffer-p buf))))
                         (buffer-list ecb-frame))
               (get-buffer-create "*scratch*"))))

        ;; Only the function `ecb-redraw-layout-full' is allowed to refer and
        ;; set the variable `ecb-edit-window' but not after the following
        ;; code-line. The only exception is that every layout-function must set
        ;; `ecb-edit-window' and `ecb-compile-window' at its end (but because
        ;; most layout-function will be generated with `ecb-layout-define' this
        ;; is not really an exception).
        (select-window ecb-edit-window))) ;; end of ecb-with-original-basic-functions,
                                          ;; ecb-with-original-permanent-layout-functions

      ;; now we restore the edit-windows as before the redraw
      (if (and (not emergency)
               (= (length edit-win-data-before-redraw)
                  (ecb-edit-area-creators-number-of-edit-windows)))
          (ecb-with-original-basic-functions
           (ecb-with-original-permanent-layout-functions
            (ecb-restore-edit-area)))
        (ecb-edit-area-creators-init))
      
      (when (not emergency)
        (setq edit-win-list-after-redraw (ecb-canonical-edit-windows-list))
        (setq edit-area-size (ecb-get-edit-area-size)))

      ;; a safety-check if we have now after redrawing at least as many
      ;; edit-windows as before redrawing. If yes we restore all window-data
      ;; as before redraw.
      (when (and (not emergency)
                 (= (length edit-win-list-after-redraw)
                    (length edit-win-data-before-redraw)))
        (dotimes (i (length edit-win-data-before-redraw))
          (let ((win (nth i edit-win-list-after-redraw))
                (data (nth i edit-win-data-before-redraw)))
            (when (buffer-live-p (nth 0 data))
              (set-window-buffer win (nth 0 data))
              (set-window-start win (nth 1 data))
              (set-window-point win (nth 2 data)))))

        ;; Klaus Berndl <klaus.berndl@sdm.de>: It seems that resizing the
        ;; windows is not reliable in some constellations with more than 3
        ;; edit-windows - but with max. 3 edit-windows there are not known any
        ;; problems - and 3 edit-windows should e sufficient for most
        ;; situations.
        (when (< (length edit-win-data-before-redraw) 4)
          (dotimes (i (length edit-win-data-before-redraw))
            (let ((win (nth i edit-win-list-after-redraw))
                  (data (nth i edit-win-data-before-redraw)))
              (if (> (length edit-win-list-after-redraw) 1)
                  (ecb-set-window-size win (nth 3 data) edit-area-size))
              )))

        ;; at the end of the edit-area-redraw we always stay in that edit-window
        ;; as before the redraw
        (when (integerp window-before-redraw)
          (ecb-select-edit-window window-before-redraw))       
        ;; if we were in an edit-window before redraw let us go to the old
        ;; place if the buffer is still alive.
        (when (and pos-before-redraw
                   (buffer-live-p (nth 0 (nth (1- window-before-redraw)
                                              edit-win-data-before-redraw))))
          (goto-char pos-before-redraw)))

      ;; Restore saved window sizes
      (when (or emergency
                (and (null ecb-windows-creator) (not no-ecb-windows)))
        (ecb-restore-window-sizes))

      (setq ecb-last-source-buffer (current-buffer))
      (setq ecb-last-edit-window-with-point (selected-window))

      ;; updating and synchronizing of the ecb-windows but only when we have a
      ;; full redraw incl. the ecb-windows.
      (when (or emergency (not no-ecb-windows))
        (let ((current-ecb-buffers (ecb-get-current-visible-ecb-buffers)))
          (when (or emergency (null ecb-windows-creator))
            (setq ecb-special-ecb-buffers-of-current-layout
                  (mapcar 'buffer-name current-ecb-buffers)))
          ;; fill-up the history new with all buffers if the history buffer was
          ;; not shown before the redisplay but now (means if the layout has
          ;; changed)
          (when (and (not (member (get-buffer ecb-history-buffer-name)
                                  ecb-windows-before-redraw))
                     (member (get-buffer ecb-history-buffer-name)
                             current-ecb-buffers))
            (ecb-add-buffers-to-history-new))
          ;; update the directories buffer if the directories buffer was not
          ;; shown before the redisplay but now (means if the layout has
          ;; changed)
          (when (and (not (member (get-buffer ecb-directories-buffer-name)
                                  ecb-windows-before-redraw))
                     (member (get-buffer ecb-directories-buffer-name)
                             current-ecb-buffers))
            (ecb-update-directories-buffer))
          ;; deactivate the speedbar stuff if the speedbar-integration-buffer
          ;; was shown before but not now
          (when (and (member (get-buffer ecb-speedbar-buffer-name)
                             ecb-windows-before-redraw)
                     (not (member (get-buffer ecb-speedbar-buffer-name)
                                  current-ecb-buffers)))
            (ignore-errors (ecb-speedbar-deactivate)))
          ;; synchronize the special ecb-buffers if necessary (means if not all
          ;; ecb-windows of current layout were visible before redraw) and
          (when (and (not (equal ecb-windows-before-redraw current-ecb-buffers))
                     (not no-buffer-sync))
            ;; maybe we have to deal with the other special buffers too but
            ;; maybe this is not necessary because the idle-stuff runs...
            (ecb-basic-buffer-sync t))
          ))

      ;; if the compile-window was selected before redraw we go back to it
      (when (and (ecb-compile-window-live-p)
                 compile-buffer-pos-before-redraw)
        (select-window ecb-compile-window)
        (goto-char compile-buffer-pos-before-redraw))

      ;; after a full redraw the stored window-configuration for a quick
      ;; redraw should be actualized
      (if ecb-redraw-layout-quickly
          (setq ecb-activated-window-configuration
                (ecb-current-window-configuration))))
    (run-hooks 'ecb-redraw-layout-after-hook)))
    

(defun ecb-redraw-layout-quickly ()
  "Redraw the layout quickly using the cached window configuration
`ecb-activated-window-configuration'."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (run-hooks 'ecb-redraw-layout-before-hook)
    (ecb-set-window-configuration ecb-activated-window-configuration)
    (run-hooks 'ecb-redraw-layout-after-hook)))


(defvar ecb-toggle-layout-state 0
  "Internal state of `ecb-toggle-layout'. Do not change it!")
(defun ecb-toggle-layout (&optional last-one)
  "Toggles between the layouts defined in `ecb-toggle-layout-sequence'.
See also option `ecb-show-sources-in-directories-buffer'.

If optional argument LAST-ONE is not nil \(e.g. called with a prefix-arg) then
always the last selected layout was choosen regardless of the setting in
`ecb-toggle-layout-sequence'. The last selected layout is always that layout
which was current direct before the most recent layout-switch. So now a user
can switch to another layout via `ecb-change-layout' and always come back to
his previous layout via \[C-u] `ecb-toggle-layout'."
  (interactive "P")
  (if (and last-one
           (stringp ecb-last-selected-layout))
      (ecb-layout-switch ecb-last-selected-layout)
    (let ((layout-name (nth ecb-toggle-layout-state ecb-toggle-layout-sequence))
          (next-index (if (< (1+ ecb-toggle-layout-state)
                             (length ecb-toggle-layout-sequence))
                          (1+ ecb-toggle-layout-state)
                        0)))
      (when (and layout-name (not (= ecb-toggle-layout-state next-index)))
        (setq ecb-toggle-layout-state next-index)
        (ecb-layout-switch layout-name)))))

(defun ecb-store-window-sizes (&optional fix)
  "Stores the sizes of the ECB windows for the current layout.
The size of the ECB windows will be set to their stored values when
`ecb-redraw-layout' or `ecb-restore-window-sizes' is called. To reset the
window sizes to their default values call `ecb-restore-default-window-sizes'.
Please read also the documentation of `ecb-layout-window-sizes'!

The windows sizes are stored per default as fractions of current frame-width
and -height of the ecb-frame, so the stored values will \"work\" for other
frame sizes too. If a permanent compile-window is visible then ECB will tell
you that window-sizes should be stored with hidden compile-window and ask you
if you want proceed; if you proceed then the window-heights will be stored as
fractions of current \(frame-height minus current visible
compile-window-height) so you should ensure that the current compile-window
has its standard-height as specified in `ecb-compile-window-height'!. If FIX
is not nil \(means called with a prefix argument) then always the fixed values
of current width and height are stored!"
  (interactive "P")
  (when (equal (selected-frame) ecb-frame)
    (if (ecb-buffer-is-maximized-p)
         (ecb-error "Sizes can not be stored when an ECB-window is maximized!")
      (if (or (not (ecb-compile-window-live-p))
              (y-or-n-p "Window-sizes should be stored with hidden compile-window! Proceed? "))
          (let ((a (ecb-find-assoc ecb-layout-name ecb-layout-window-sizes)))
            (unless a
              (setq a (cons ecb-layout-name nil))
              (setq ecb-layout-window-sizes
                    (ecb-add-assoc a ecb-layout-window-sizes)))
            (setcdr a (ecb-get-ecb-window-sizes fix))
            (ecb-customize-save-variable 'ecb-layout-window-sizes
                                         ecb-layout-window-sizes))))))


(defun ecb-restore-window-sizes ()
  "Sets the sizes of the ECB windows to their stored values."
  (interactive)
  (when (equal (selected-frame) ecb-frame)
    (ecb-set-ecb-window-sizes (ecb-find-assoc-value ecb-layout-name
                                                    ecb-layout-window-sizes))))

(defun ecb-restore-default-window-sizes ()
  "Resets the sizes of the ECB windows to their default values."
  (interactive)
  (when (equal (selected-frame) ecb-frame)
    (setq ecb-layout-window-sizes
	  (ecb-remove-assoc ecb-layout-name ecb-layout-window-sizes))
    (ecb-customize-save-variable 'ecb-layout-window-sizes ecb-layout-window-sizes)))

;; Now per default returns fractions of the ecb-frame; thanks to Geert Ribbers
;; [geert.ribbers@realworld.nl] for a first implementation.
(defun ecb-get-window-size (window &optional fix ref-size)
  "Return the sizes of WINDOW as a cons where the car is the width and the cdr
is the height. Per default both values are fractions of the frame-width (rsp.
height) of the `ecb-frame' unless FIX is not nil. Optional third argument
REF-SIZE is a cons where car is the referencial width and the cdr is the
referencial height when the sizes should be computed as fractionals of the
REF-SIZE; then the resulting sizes are floating-point-numbers) \(if FIX is
nil). Default referencial width rsp. height are `frame-width' rsp.
`frame-height' of the `ecb-frame'."
  (when window
    (let ((ref-width (or (car ref-size) (frame-width ecb-frame)))
          (ref-height (or (cdr ref-size) (frame-height ecb-frame))))
      (cons (/ (ecb-window-full-width window)
               (if fix
                   1
                 (* 1.0 ref-width)))
            (/ (ecb-window-full-height window)
               (if fix
                   1
                 (* 1.0 ref-height)))))))


(defun ecb-get-ecb-window-sizes (&optional fix ecb-win-list)
  "Get all window-sizes of current visible ecb-windows. If FIX is not nil then
fixed sizes are used otherwise fractions of current frame-width rsp. -height.
If a permanent compile-window is visible then window-heights will be computed
as fractions of current \(frame-height minus current visible
compile-window-height)!
Uses ECB-WIN-LIST or - if nil - computes it with the function
`ecb-canonical-ecb-windows-list'."
  (let ((ref-height (if (ecb-compile-window-live-p)
                        (- (frame-height ecb-frame)
                           (ecb-window-full-height ecb-compile-window))
                      (frame-height ecb-frame)))
        (ref-width (frame-width ecb-frame)))
    (mapcar (function (lambda (window)
                        (ecb-get-window-size window
                                             fix
                                             (cons ref-width ref-height))))
            (or ecb-win-list (ecb-canonical-ecb-windows-list)))))

;; Now possible to set fractional sizes; thanks to Geert Ribbers
;; [geert.ribbers@realworld.nl] for a first implementation.
(defun ecb-set-window-size (window size &optional ref-size)
  "Enlarge/shrink WINDOW to SIZE where SIZE is a cons with new width as car
and new height as cdr. New width and height can be floating-point-numbers.
Optional third argument REF-SIZE is a cons too where car is the referencial
width and the cdr is the referencial height when the car and cdr of SIZE are
floating-point-numbers. Default referencial width rsp. height are
`frame-width' rsp. `frame-height' of the `ecb-frame'."
  (when (and window (window-live-p window) (consp size))
    (let* ((ref-width (or (car ref-size) (frame-width ecb-frame)))
           (ref-height (or (cdr ref-size) (frame-height ecb-frame)))
           (absolut-width (if (and (numberp (car size))
                                   (floatp (car size))) 
                              (* (car size) ref-width)
                            (car size)))
           (absolut-height (if (and (numberp (car size))
                                    (floatp (cdr size))) 
                               (* (cdr size) ref-height)
                             (cdr size)))
           (enlarge-width (if (numberp absolut-width)
                              (- (round absolut-width)
                                 (ecb-window-full-width window))))
           (enlarge-height (if (numberp absolut-height)
                               (- (round absolut-height)
                                  (ecb-window-full-height window)))))
      (save-selected-window
        (select-window window)
        (if (and (numberp enlarge-width) (/= enlarge-width 0))
            (ignore-errors (enlarge-window enlarge-width t)))
        (if (and (numberp enlarge-height) (/= enlarge-height 0))
            (ignore-errors (enlarge-window enlarge-height)))))))

(defun ecb-set-ecb-window-sizes (window-sizes)
  (unless ecb-windows-hidden
    (ecb-do-with-unfixed-ecb-buffers
     (let ((sizes (or window-sizes ecb-layout-default-window-sizes))
           (windows (ecb-canonical-ecb-windows-list))
           (ref-width (frame-width ecb-frame))
           (ref-height (if (ecb-compile-window-live-p)
                           (- (frame-height ecb-frame)
                              (ecb-window-full-height ecb-compile-window))
                         (frame-height ecb-frame))))
       (ecb-layout-debug-error "ecb-set-ecb-window-sizes: window-sizes: %s, sizes: %s, windows: %s, length-s: %d, length-w: %d"
                               window-sizes sizes windows
                               (length sizes) (length windows))
       (mapc (lambda (win)
               (ecb-layout-debug-error "ecb-set-ecb-window-sizes: win %s, ded: %s"
                                       win (window-dedicated-p win)))
             windows)
       (when sizes
         (if (= (length windows) (length sizes))
             (dolist (size sizes)
               (ecb-set-window-size (car windows) size (cons ref-width ref-height))
               (setq windows (cdr windows)))
           (when (interactive-p)
             (ecb-error "Stored sizes of layout %s not applicable for current window layout!"
                        ecb-layout-name))))))))

;; Klaus Berndl <klaus.berndl@sdm.de>: frame-width is smaller than
;; ecb-window-full-width for only one window in the frame. But for now this
;; doesn't matter because it is only important that for getting and setting
;; the edit-windows-sizes the same reference-sizes are used.
(defun ecb-get-edit-area-size (&optional win-list)
  (let ((layout-type (ecb-get-layout-type ecb-layout-name))
        (ecb-win-list (ecb-canonical-ecb-windows-list win-list))
        (comp-win-height (if (equal (ecb-compile-window-state) 'visible)
                             (ecb-window-full-height ecb-compile-window)
                           0)))
    (if (null ecb-win-list)
        (cons (frame-width ecb-frame)
              (- (frame-height ecb-frame) comp-win-height))
      (case layout-type
        (top
         (cons (frame-width ecb-frame)
               (- (frame-height ecb-frame)
                  (ecb-window-full-height (ecb-first ecb-win-list))
                  comp-win-height)))
        (left-right
         (cons (- (frame-width ecb-frame)
                  (ecb-window-full-width (ecb-first ecb-win-list))
                  (ecb-window-full-width (ecb-last ecb-win-list)))
               (- (frame-height ecb-frame)
                  comp-win-height)))
        (otherwise
         (cons (- (frame-width ecb-frame)
                  (ecb-window-full-width (ecb-first ecb-win-list)))
               (- (frame-height ecb-frame)
                  comp-win-height)))))))


;; Klaus Berndl <klaus.berndl@sdm.de>: Cause of a much better repair-mechanism
;; (see `ecb-repair-only-ecb-window-layout') the following code and mechanism
;; is not used - but who knows, maybe we can need it later.................

;; Klaus Berndl <klaus.berndl@sdm.de>:
;; - this var must be set with the `current-window-configuration' in
;;   + `ecb-restore-window-sizes' (`ecb-store-window-sizes' and
;;     `ecb-restore-default-window-sizes' uses the :set-function of
;;     `ecb-layout-window-sizes' which in turn calles
;;     `ecb-redraw-layout-full' - see below).
;;   + What about the `enlarge-window', `shrink-window',
;;     `mouse-drag-mode-line' and `mouse-drag-vertical-line' (rsp.
;;     `drag-window-divider' for XEmacs) (new after advices)??
;;   + `delete-window' (new after advice)
;;   + `split-window' (new after advice)
;;   + `ecb-redraw-layout-full' (should cover all siutuation like
;;     hidden-ecb-windows, visible-ecb-windows, maximized-ecb-windows etc...
;;   + `ecb-toggle-compile-window-height' when the window has to be enlarged.
;;   Whenever a compile-window is visible and has its specified height
;;   (`ecb-compile-window-height-lines') then the current window-config has to
;;   be stored into this variable. Otherwise this variable must be set to nil!
;; - This stored config is used by `ecb-toggle-compile-window-height' to
;;   shrink back a compile-window to its specified height - so eventually
;;   be the compile-window-enlargement destroyed or resized ecb-windows will
;;   be redrawn - we can not shrink the compile-window by a full redraw with
;;   ecb-redraw-layout-full because this would damage some
;;   save-selected-window calls with `ecb-toggle-compile-window-height
(defvar ecb-compile-window-specified-height-config nil
  "Contains the most recent and valid ecb-window-configuration \(a object
returned by `ecb-current-window-configuration') or nil. If not nil then this
config contains always a visible compile-window of height
`ecb-compile-window-height!")

;; Klaus Berndl <klaus.berndl@sdm.de>: Not really necessary but a
;; fallback-mechanism to disable in an easy way the new
;; window-config-shrinking if there occur problems.
(defvar ecb-use-window-config-for-compwin-shrink nil)

(defun ecb-store-compile-window-specified-height-config ()
  "Store the current ecb-window-configuration in
`ecb-compile-window-specified-height-config' but do this only if a
compile-window is visible and if this compile-window has height
`ecb-compile-window-height-lines'! Otherwise set this variable to nil."
  (setq ecb-compile-window-specified-height-config
        (if (and ecb-use-window-config-for-compwin-shrink
                 (ecb-compile-window-live-p)
                 (equal (ecb-window-full-height ecb-compile-window)
                        ecb-compile-window-height-lines))
            (ecb-current-window-configuration))))

;; Klaus Berndl <klaus.berndl@sdm.de>: returns curently always nil because
;; currently ecb-store-compile-window-specified-height-config is never called
;; - see comment above!
(defun ecb-reset-compile-window-specified-height-config ()
  "Set the ecb-window-configuration of
`ecb-compile-window-specified-height-config' if it is a still valid
ecb-window-configuration. If this had success return t otherwise nil."
  (let ((result
         (and ecb-compile-window-specified-height-config
              (not (ecb-window-configuration-invalidp ecb-compile-window-specified-height-config))
              (ecb-set-window-configuration ecb-compile-window-specified-height-config))))
    (ecb-layout-debug-error "ecb-reset-compile-window-specified-height-config: %s"
                            result)
    result))

;; For backward-compatibility
(defalias 'ecb-toggle-enlarged-compilation-window
  'ecb-toggle-compile-window-height)

(defun ecb-toggle-compile-window-height (&optional arg)
  "Toggle whether the `ecb-compile-window' is enlarged or not.
If ARG > 0 then shrink or enlarge the the compile-window according to the
value of `ecb-enlarged-compilation-window-max-height'. But never shrink below
the value of `ecb-compile-window-height'. If ARG <= 0 then shrink
`ecb-compile-window' to `ecb-compile-window-height' and if ARG is nil then
toggle the enlarge-state. Returns the new height of the compile-window or nil
if no compile-window is visible."
  (interactive "P")
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame)
           (equal 'visible (ecb-compile-window-state)))
      (let* ((height-before (ecb-window-full-height ecb-compile-window))
             (should-shrink (if (null arg)
                               (> height-before ecb-compile-window-height-lines)
                             (<= (prefix-numeric-value arg) 0)))
;;             (compile-window-selected-p (equal (selected-window)
;;                                               ecb-compile-window))
            (max-height nil)
            (number-of-lines nil))
        ;; we must use save-excursion because otherwise this command behaves
        ;; wrong when called from program within a (save-excursion (set-buffer
        ;; ...)) call.
        (save-excursion
          (save-selected-window
            (select-window ecb-compile-window)
            (setq number-of-lines (+ (if ecb-running-xemacs 2 1) ; XEmacs hor. scrollb.
                                     (count-lines (point-min) (point-max))))
            (ecb-layout-debug-error "ecb-toggle-compile-window-height: buffer: %s, lines: %d"
                                    (current-buffer) number-of-lines)
            (if should-shrink
                (progn
                  (ecb-layout-debug-error "ecb-toggle-compile-window-height: buffer: %s, lines: %d shrink down to compile-window-height"
                                          (current-buffer) number-of-lines)
                  (when (not (ecb-reset-compile-window-specified-height-config))
                    (ecb-layout-debug-error "ecb-toggle-compile-window-height: call shrink-window!")
                    (shrink-window (max 0 (- (ecb-window-full-height)
                                             ecb-compile-window-height-lines)))
                    ;; we restore the window-sizes (either the default or the
                    ;; stored sizes. because this function is often called
                    ;; during display-buffer (e.g. when completions, help-buffers,
                    ;; choosing a completion are performed) and XEmacs often
                    ;; destroyes the window-layout (e.g. the topmost
                    ;; ecb-window disappears, when doing completion etc..) we
                    ;; hav to ignore errors here.... it's not easy to find out
                    ;; what is precisely happening here but with this error
                    ;; ignoring all seems to work...
                    (ignore-errors (ecb-restore-window-sizes))
                    ))
              (if (equal ecb-enlarged-compilation-window-max-height 'best)
                  ;; With GNU Emacs we could use `ecb-fit-window-to-buffer' but
                  ;; XEmacs doesn't have such a function; Therefore...
                  ;; We fit the window to exactly this height:
                  ;; The minimum MIN of
                  ;; - half of frame-height
                  ;; - number of lines +1 in current buffer
                  ;; - temp-buffer-max-height or compilation-window-height (in
                  ;;   lines) - dependent on the mode of current buffer.
                  ;; Then we take the maximum of this MIN and the height of the
                  ;; compile-window as defined in `ecb-compile-window-height'
                  ;; (in lines).
                  (progn
                    (setq max-height
                          (max (min (floor (/ (1- (frame-height)) 2))
                                    (or (if (ecb-derived-mode-p 'compilation-mode)
                                            compilation-window-height
                                          (if ecb-running-xemacs
                                              (ignore-errors ; if temp-buffer-... is nil!
                                                (ecb-normalize-number
                                                 temp-buffer-max-height
                                                 (1- (frame-height))))
                                            (if (functionp temp-buffer-max-height)
                                                (funcall temp-buffer-max-height
                                                         (current-buffer))
                                              temp-buffer-max-height)))
                                        1000) ; 1000 is surely > then half of the frame
                                    number-of-lines)
                               ecb-compile-window-height-lines))
                    (ecb-layout-debug-error "ecb-toggle-compile-window-height: max-height: %s, curr-win-height: %s"
                                            max-height (ecb-window-full-height))
                    (enlarge-window (- max-height (ecb-window-full-height))))
                (setq max-height
                      (cond ((equal ecb-enlarged-compilation-window-max-height
                                    'half)
                             (floor (/ (1- (frame-height)) 2)))
                            ((numberp ecb-enlarged-compilation-window-max-height)
                             (ecb-normalize-number
                              ecb-enlarged-compilation-window-max-height
                              (1- (frame-height))))))
                (enlarge-window (- (max max-height ecb-compile-window-height-lines)
                                   (ecb-window-full-height))))
              ;; now we set the window-start
              (set-window-start ecb-compile-window
                                (save-excursion
                                  (goto-char (window-start))
                                  (forward-line (* -1
                                                   (- (ecb-window-full-height)
                                                      height-before)))
                                  (ecb-line-beginning-pos))
                                t)
;;               (when (and (not (equal major-mode 'compilation-mode))
;;                          (not compile-window-selected-p))
;;                 (set-window-start ecb-compile-window (point-min)))
              )
            ;; return the new compile-window height
            (ecb-window-full-height))))
    (if (interactive-p)
        (ecb-info-message "No compile-window in current ECB-layout!"))
    nil))

;; This function takes into account the value of of
;; `temp-buffer-shrink-to-fit' (XEmacs) and `temp-buffer-resize-mode' (GNU
;; Emacs) so all the callers can profit: pop-to-buffer (==>
;; switch-to-buffer-other-window too), display-buffer (if interactive) and
;; switch-to-buffer.
;;
;; The case for calling help via `with-output-to-temp-buffer' is automatically
;; taken into account because in this case XEmacs calls display-buffer with
;; SHRINK-TO-FIT = nil (called by show-temp-buffer-in-current-frame which is
;; value of temp-buffer-show-function) and GNU Emacs doesn't add
;; resize-temp-buffer-window to the temp-buffer-show-hook.
(defun ecb-set-compile-window-height ()
  "Set the height of the compile-window according to
`ecb-enlarged-compilation-window-max-height' and the value of
`temp-buffer-shrink-to-fit' \(XEmacs) and `temp-buffer-resize-mode' \(GNU
Emacs)."
  (if (and (ecb-compile-window-live-p)
           (member ecb-compile-window-temporally-enlarge
                   '(after-display both))
           (or (save-excursion
                 (set-buffer (window-buffer ecb-compile-window))
                 (equal major-mode 'compilation-mode))
               (if ecb-running-xemacs
                   temp-buffer-shrink-to-fit
                 temp-buffer-resize-mode)))
      (progn
        (ecb-layout-debug-error "ecb-set-compile-window-height: enlarge/fit")
        (ecb-toggle-compile-window-height 1))
    (ecb-layout-debug-error "ecb-set-compile-window-height: shrink")
    (ecb-toggle-compile-window-height -1)))


(defun ecb-compile-window-state ()
  "Returns the state of the compile-window:
- 'no: No persistent compile-window, i.e. `ecb-compile-window-height' is nil.
- 'visible: The compile-window is visible.
- 'hidden: A persistent compile-window is set but it is currently hidden."
  (if (null ecb-compile-window-height)
      'no
    (if (ecb-compile-window-live-p)
        'visible
      'hidden)))

(defun ecb-toggle-compile-window (&optional arg)
  "Toggle the visibility of the compile-window of ECB.
With prefix argument ARG, make visible if positive, otherwise invisible. The
height of the compile-window is always the current value of
`ecb-compile-window-height'! If called and `ecb-compile-window-height' is nil
then ECB asks for the height of the compile-window, sets this height as new
value of `ecb-compile-window-height' and displays the compile-window \(so if
you have called this command by mistake and you do not want a compile-window
you have to quit with `C-g')."
  (interactive "P")
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (let ((new-state (if (null arg)
                         (not (ecb-compile-window-live-p))
                       (>= (prefix-numeric-value arg) 0)))
          (ecb-buf (if (member (current-buffer)
                                (ecb-get-current-visible-ecb-buffers))
                       (current-buffer)))
          (new-win nil))
      (if new-state
          (let ((height (or ecb-compile-window-height
                            (and (interactive-p)
                                 (or (ecb-option-get-value 'ecb-compile-window-height
                                                           'saved-value)
                                     (ecb-read-number "Insert height of the compile-window: " 6))))))
            (when height
              (customize-set-variable 'ecb-compile-window-height height)
              ;; ecb-redraw-layout-full only preserves point and selected window
              ;; if called from an edit- or compile-window. If called from an
              ;; ECB-window we have to restore it here.
              (when ecb-buf
                (setq new-win (get-buffer-window ecb-buf))
                (if (and new-win (window-live-p new-win)
                         (equal (window-frame new-win) ecb-frame))
                    (select-window new-win)))))
        (when (ecb-compile-window-live-p)
          (let ((point-location (ecb-where-is-point)))
            (ecb-with-original-basic-functions
             (ecb-with-original-permanent-layout-functions
              (delete-window ecb-compile-window)))
            (ecb-restore-window-sizes)
            ;; If point was in the compile-window we move it back to the first
            ;; edit-window
            (if (equal (car point-location) 'compile)
                (ecb-select-edit-window))))))))


(silentcomp-provide 'ecb-layout)

;;; ecb-layout.el ends here

;; LocalWords:  ecb Jesper Nordenberg Berndl java splitted Elisp ChangeLog CVS
;; LocalWords:  berndl Exp eval silentcomp util speedbar defvar defun cl defs

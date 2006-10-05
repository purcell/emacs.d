;;; ecb-common-browser.el --- common browsing stuff for  Emacs

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2004

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

;; $Id: ecb-common-browser.el,v 1.25 2006/05/12 16:03:11 berndl Exp $


;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))


(require 'ecb-util)

(require 'tree-buffer)
;; (require 'ecb-layout) ;; causes cyclic dependencies!
(require 'ecb-mode-line)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(silentcomp-defvar modeline-map)

(defgroup ecb-tree-buffer nil
  "General settings related to the tree-buffers of ECB."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-bucket-node-display '("" "" ecb-bucket-node-face)
  "*How ECB displays bucket-nodes in a ECB tree-buffer.
Bucket-nodes have only one job: Nodes with similar properties will be dropped
into one bucket for such a common property and all these nodes will be added
as children to the bucket-node. Besides being expandable and collapsable a
bucket-node has no senseful action assigned. Examples for bucket-nodes are
\"[+] Variables\", \"[+] Dependencies\" etc. in the Methods-buffer or buckets
which combine filenames with same extension under a bucket-node with name this
extension.

This option defines how bucket-node should be displayed. The name of the
bucket-node is computed by ECB but you can define a prefix, a suffix and a
special face for the bucket-node

The default are empty prefix/suffix-strings and 'ecb-bucket-node-face'. But
an alternative can be for example '\(\"[\" \"]\" nil) which means no special
face and a display like \"[+] [<bucket-name>]\"."
  :group 'ecb-general
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(list (string :tag "Bucket-prefix" :value "[")
               (string :tag "Bucket-suffix" :value "]")
               (choice :tag "Bucket-face" :menu-tag "Bucket-face"
                       (const :tag "No special face" :value nil)
                       (face :tag "Face" :value ecb-bucket-node-face)))
  :initialize 'custom-initialize-default)

(defcustom ecb-use-speedbar-instead-native-tree-buffer nil
  "*If true then uses speedbar for directories, sources or methods.
This means that speedbar is integrated in the ECB-frame and is displayed in
that window normally displaying the standard ECB-directories-buffer,
ECB-sources-buffer or ECB-methods-buffer.

This option takes effect in all layouts which contain either a directory
window, a sources window or a method window.

This option can have four valid values:
- nil: Do not use speedbar \(default)
- dir: Use speedbar instead of the standard directories-buffer
- source: Use speedbar instead of the standard sources-buffer
- method: Use speedbar instead of the standard methods-buffer

Note: For directories and sources a similar effect and usability is available
by setting this option to nil \(or 'method) and setting
`ecb-show-sources-in-directories-buffer' to not nil, because this combination
displays also directories and sources in one window.

`ecb-use-speedbar-instead-native-tree-buffer' is for people who like the
speedbar way handling directories and source-files or methods and want it in
conjunction with ECB."
  :group 'ecb-general
  :group 'ecb-directories
  :group 'ecb-sources
  :group 'ecb-methods
  :type '(radio (const :tag "Do not use speedbar" :value nil)
                (const :tag "For directories" :value dir)
                (const :tag "For sources" :value source)
                (const :tag "For methods" :value method))
  :initialize 'custom-initialize-default
  :set (function (lambda (sym val)
                   (set sym val)
                   (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
                       (ecb-redraw-layout-full)))))

(defvar ecb-tree-do-not-leave-window-after-select--internal nil
  "Only set by customizing `ecb-tree-do-not-leave-window-after-select' or
calling `ecb-toggle-do-not-leave-window-after-select'! Do not set this
variable directly, it is only for internal uses!")

(defcustom ecb-tree-do-not-leave-window-after-select nil
  "*Tree-buffers which stay selected after a key- or mouse-selection.
If a buffer \(either its name or the variable-symbol which holds the name) is
contained in this list then selecting a tree-node either by RET or by a
mouse-click doesn't leave that tree-buffer after the node-selection but
performes only the appropriate action \(opening a new source, selecting a
method etc.) but point stays in the tree-buffer. In tree-buffers not contained
in this option normaly a node-selection selects as \"last\" action the right
edit-window or maximizes the next senseful tree-buffer in case of a currently
maximized tree-buffer \(see `ecb-maximize-next-after-maximized-select').

The buffer-name can either be defined as plain string or with a symbol which
contains the buffer-name as value. The latter one is recommended for the
builtin ECB-tree-buffers because then simply the related option-symbol can be
used.

A special remark for the `ecb-directories-buffer-name': Of course here the
value of this option is only relevant if the name of the current layout is
contained in `ecb-show-sources-in-directories-buffer' or if the value of
`ecb-show-sources-in-directories-buffer' is 'always and the clicked ot hitted
node represents a sourcefile \(otherwise this would not make any sense)!

The setting in this option is only the default for each tree-buffer. With the
command `ecb-toggle-do-not-leave-window-after-select' the behavior of a
node-selection can be changed fast and easy in a tree-buffer without
customizing this option, but of course not for future Emacs sessions!"
  :group 'ecb-tree-buffer
  :set (function (lambda (sym val)
                   (set sym val)
                   (setq ecb-tree-do-not-leave-window-after-select--internal
                         (ecb-copy-list val))))
  :type '(repeat (choice :menu-tag "Buffer-name"
                        (string :tag "Buffer-name as string")
                        (symbol :tag "Symbol holding buffer-name"))))


(defcustom ecb-tree-indent 4
  "*Indent size for tree buffer.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type 'integer)

(defcustom ecb-tree-expand-symbol-before t
  "*Show the expand symbol before the items in a tree.
When the expand-symbol is located before the items then the tree looks like:

\[-] ECB
    \[+] code-save
    \[-] ecb-images
        \[-] directories

When located after then the tree looks like:

ECB \[-]
  code-save \[+]
  ecb-images \[-]
    directories \[-]

The after-example above use a value of 2 for `ecb-tree-indent' whereas the
before-example uses a value of 4.

It is recommended to display the expand-symbol before because otherwise it
could be that with a deep nested item-structure with and/or with long
item-names \(e.g. a deep directory-structure with some long
subdirectory-names) the expand-symbol is not visible in the tree-buffer and
the tree-buffer has to be horizontal scrolled to expand an item."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type 'boolean)


(defcustom ecb-tree-buffer-style (if ecb-images-can-be-used
                                     'image
                                   'ascii-guides)
  "*The style of the tree-buffers.
There are three different styles available:

Image-style \(value 'image):
Very nice and modern - just try it. For this style the options
`ecb-tree-indent' and `ecb-tree-expand-symbol-before' have no effect!
Note: GNU Emacs <= 21.3.X for Windows does not support image-display so ECB
uses always 'ascii-guides even when here 'image is set!

Ascii-style with guide-lines \(value 'ascii-guides):
\[-] ECB
 |  \[+] code-save
 `- \[-] ecb-images
     |  \[-] directories
     |   |  \[-] height-15
     |   |   |  * close.xpm
     |   |   |  * empty.xpm
     |   |   |  * leaf.xpm
     |   |   `- * open.xpm
     |   |  \[+] height-17
     |   |  \[+] height-19
     |   `- \[+] height-21
     |  \[x] history
     |  \[x] methods
     `- \[x] sources

Ascii-style without guide-lines \(value 'ascii-no-guides) - this is the style
used by ECB <= 1.96:
\[-] ECB
    \[+] code-save
    \[-] ecb-images
        \[-] directories
            \[-] height-15
                * close.xpm
                * empty.xpm
                * leaf.xpm
                * open.xpm
            \[+] height-17
            \[+] height-19
            \[+] height-21
        \[x] history
        \[x] methods
        \[x] sources

With both ascii-styles the tree-layout can be affected with the options
`ecb-tree-indent' and `ecb-tree-expand-symbol-before'."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type '(radio (const :tag "Images-style" :value image)
                (const :tag "Ascii-style with guide-lines" :value ascii-guides)
                (const :tag "Ascii-style w/o guide-lines" :value ascii-no-guides)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: add here the analyse buffer if
;; additonal images are necessary - but currently i don't think we need
;; special images for this analyse-stuff.
(defcustom ecb-tree-image-icons-directories
  (let ((base (concat (if ecb-regular-xemacs-package-p
                          (format "%s" (locate-data-directory "ecb"))
                        ecb-ecb-dir)
                      "ecb-images/")))
    (cons (concat base "default/height-17")
          (mapcar (function (lambda (i)
                              (cons (car i) (concat base (cdr i)))))
                  '((ecb-directories-buffer-name . "directories/height-17")
                    (ecb-sources-buffer-name . "sources/height-14_to_21")
                    (ecb-methods-buffer-name . "methods/height-14_to_21")))))
  "*Directories where the images for the tree-buffer can be found.
This is a cons cell where:

car: Default directory where the default images for the tree-buffer can be
found. It should contain an image for every name of
`tree-buffer-tree-image-names'. The name of an image-file must be:
\"ecb-<NAME of TREE-BUFFER-TREE-IMAGE-NAMES>.<ALLOWED EXTENSIONS>\".

cdr: This is a list where each element is a cons again with: car is the buffer
name of the tree-buffer for which a special image-path should be used. The
buffer-name can either be defined as plain string or with a symbol which
contains the buffer-name as value. The latter one is recommended for the
builtin ECB-tree-buffers because then simply the related option-symbol can be
used \(e.g. the symbol `ecb-directories-buffer-name'). The cdr is the the
full-path of an additional image-directorie which is searched first for images
needed for the related tree-buffer. If the image can not be found in this
directory then the default-directory \(see above) is searched. If the
image can't even be found there the related ascii-symbol is used - which is
defined in `tree-buffer-tree-image-names'. If a tree-buffer is not contained
in this list then there is no additional special image-directory for it.

ECB comes with predefined images in several different heights - so for the
most senseful font-heights of a tree-buffer a fitting image-size should be
available. The images reside either in the subdirectory \"ecb-images\" of the
ECB-installation or - if ECB is installed as regular XEmacs-package - in the
ECB-etc data-directory \(the directory returned by \(locate-data-directory
\"ecb\")."
  :group 'ecb-tree-buffer
  :type '(cons (directory :tag "Full default image-path")
               (repeat (cons (choice :menu-tag "Buffer-name"
                                     (string :tag "Buffer-name as string")
                                     (symbol :tag "Symbol holding
                                     buffer-name"))                             
                             (directory :tag "Full image-path for this tree-buffer")))))

(defcustom ecb-tree-truncate-lines '(ecb-directories-buffer-name
                                     ecb-sources-buffer-name
                                     ecb-methods-buffer-name
                                     ecb-history-buffer-name
                                     ecb-analyse-buffer-name)
  "*Truncate lines in ECB buffers.
If a buffer \(either its name or the variable-symbol which holds the name) is
contained in this list then line-truncation is switched on for this buffer
otherwise it is off.

The buffer-name can either be defined as plain string or with a symbol which
contains the buffer-name as value. The latter one is recommended to switch on
line-truncation for one of the builtin ECB-tree-buffers because then simply
the related option-symbol can be used. To truncate lines in the builtin
directories tree-buffer just add the symbol `ecb-directories-buffer-name' to
this option.

If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type '(repeat (choice :menu-tag "Buffer-name"
                         (string :tag "Buffer-name as string")
                         (symbol :tag "Symbol holding buffer-name"))))

(defcustom ecb-tree-easy-hor-scroll 5
  "*Scroll step for easy hor. scrolling via mouse-click in tree-buffers.
XEmacs has horizontal scroll-bars so invisible parts beyond the right
window-border of a tree-buffer can always made visible very easy.

GNU Emacs does not have hor. scroll-bars so especially with the mouse it is
quite impossible to scroll smoothly right and left. The functions
`scroll-left' and `scroll-right' can be annoying and are also not bound to
mouse-buttons.

If this option is a positive integer S then in all ECB-tree-buffers the keys
\[M-mouse-1] and \[M-mouse-3] are bound to scrolling left rsp. right with
scroll-step S - clicking with mouse-1 or mouse-2 onto the edge of the modeline
has the same effect, i.e. if you click with mouse-1 onto the left \(rsp.
right) edge of the modeline you will scroll left \(rsp. right). Additionally
\[C-M-mouse-1] and \[C-M-mouse-3] are bound to scrolling left rsp. right with
scroll-step `window-width' - 2. Default is a scroll-step of 5. If the value is
nil then no keys for horizontal scrolling are bound."
  :group 'ecb-tree-buffer
  :type '(radio :value 5
                (const :tag "No hor. mouse scrolling" :value nil)
                (integer :tag "Scroll step")))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should change this to a
;; type analogous to ecb-tree-truncate-lines
(defcustom ecb-truncate-long-names t
  "*Truncate long names that don't fit in the width of the ECB windows.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type 'boolean)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should change this to a
;; type analogous to ecb-tree-truncate-lines
(defcustom ecb-tree-incremental-search 'prefix
  "*Enable incremental search in the ECB-tree-buffers.
For a detailed explanation see the online help section \"Working with the
keyboard in the ECB buffers\". If you change this during ECB is activated you
must deactivate and activate ECB again to take effect."
  :group 'ecb-tree-buffer
  :type '(radio (const :tag "Match only prefix"
                       :value prefix)
                (const :tag "Match every substring"
                       :value substring)
                (const :tag "No incremental search"
                       :value nil)))

(defcustom ecb-tree-navigation-by-arrow t
  "*Enable smart navigation in the tree-windows by horizontal arrow-keys.
If not nil then the left- and right-arrow keys work in the ECB tree-window in
the following smart way if onto an expandable node:
+ Left-arrow: If node is expanded then it will be collapsed otherwise point
  jumps to the next \"higher\" node in the hierarchical tree \(higher means
  the next higher tree-level or - if no higher level available - the next
  higher node on the same level).
+ Right-arrow: If node is not expanded then it will be expanded.
Onto a not expandable node the horizontal arrow-keys go one character in the
senseful correct direction.

If this option is changed the new value takes first effect after deactivating
ECB and then activating it again!"
  :group 'ecb-tree-buffer
  :type 'boolean)

(defun ecb-show-any-node-info-by-mouse-moving-p ()
  "Return not nil if for at least one tree-buffer showing node info only by
moving the mouse over a node is activated. See
`ecb-directories-show-node-info' etc...."
  (let ((when-list (mapcar (function (lambda (elem)
                                       (car (symbol-value elem))))
                           '(ecb-directories-show-node-info
                             ecb-sources-show-node-info
                             ecb-methods-show-node-info
                             ecb-history-show-node-info
                             ecb-analyse-show-node-info
                             ))))
    (or (member 'if-too-long when-list)
        (member 'always when-list))))

(defcustom ecb-primary-secondary-mouse-buttons 'mouse-2--C-mouse-2
  "*Primary- and secondary mouse button for using the ECB-buffers.
A click with the primary button causes the main effect in each ECB-buffer:
- ECB Directories: Expanding/collapsing nodes and displaying files in the ECB
  Sources buffer.
- ECB sources/history: Opening the file in that edit-window specified by the
  option `ecb-mouse-click-destination'.
- ECB Methods: Jumping to the method in that edit-window specified by the
  option `ecb-mouse-click-destination'.

A click with the primary mouse-button while the SHIFT-key is pressed called
the POWER-click and does the following \(depending on the ECB-buffer where the
POWER-click occurs):
+ Directory-buffer: Refreshing the directory-contents-cache \(see
  `ecb-cache-directory-contents').
+ Sources- and History-buffer: Only displaying the source-contents in the
  method-buffer but not displaying the source-file in the edit-window.
+ Methods-buffer: Narrowing to the clicked method/variable/ect... \(see
  `ecb-tag-visit-post-actions'). This works only for sources supported by
  semantic!

In addition always the whole node-name is displayed in the minibuffer after a
POWER-click \(for this see `ecb-directories-show-node-info' etc...).

The secondary mouse-button is for opening \(jumping to) the file in another
edit-window \(see the documentation `ecb-mouse-click-destination').

The following combinations are possible:
- primary: mouse-2, secondary: C-mouse-2 \(means mouse-2 while CTRL-key is
  pressed). This is the default setting.
- primary: mouse-1, secondary: C-mouse-1
- primary: mouse-1, secondary: mouse-2

Note: If the tree-buffers are used with the keyboard instead with the mouse
then [RET] is interpreted as primary mouse-button and [C-RET] as secondary
mouse-button!

If you change this during ECB is activated you must deactivate and activate
ECB again to take effect!"
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type '(radio (const :tag "Primary: mouse-2, secondary: Ctrl-mouse-2"
                       :value mouse-2--C-mouse-2)
                (const :tag "Primary: mouse-1, secondary: Ctrl-mouse-1"
                       :value mouse-1--C-mouse-1)
                (const :tag "Primary: mouse-1, secondary: mouse-2"
                       :value mouse-1--mouse-2)))

(defcustom ecb-tree-mouse-action-trigger 'button-release
  "*When the tree-buffer mouse-action should be triggered.
This option determines the moment a mouse-action in a tree-buffer is
triggered. This can be either direct after pressing a mouse-button \(value
'button-press) or not until releasing the mouse-button \(value:
'button-release).

If you change this during ECB is activated you must deactivate and activate
ECB again to take effect!"
  :group 'ecb-tree-buffer
  :type '(radio (const :tag "After button release" :value button-release)
                (const :tag "After button press" :value button-press)))

(defcustom ecb-mouse-click-destination 'last-point
  "*Destination of a mouse-button click.
Defines in which edit-window \(if splitted) ECB does the \"right\" action
\(opening a source, jumping to a method/variable etc.) after clicking with a
mouse-button \(see `ecb-primary-secondary-mouse-buttons') onto a node. There
are two possible choices:
- left-top: Does the \"right\" action always in the left/topmost edit-window.
- last-point: Does the \"right\" action always in that edit-window which had
  the point before.
This is if the user has clicked either with the primary mouse-button or
has activated a popup-menu in the tree-buffer.

A click with the secondary mouse-button \(see again
`ecb-primary-secondary-mouse-buttons') does the \"right\" action always in
another edit-window related to the setting in this option: If there are two
edit-windows then the \"other\" edit-window is used and for more than 2
edit-windows the \"next\" edit-window is used \(whereas the next edit-window
of the last edit-window is the first edit-window).

If the edit-window is not splitted this setting has no effect.

Note: If the tree-buffers are used with the keyboard instead with the mouse
then this option takes effect too because [RET] is interpreted as primary
mouse-button and [C-RET] as secondary mouse-button!"
  :group 'ecb-general
  :group 'ecb-most-important
  :type '(radio (const :tag "Left/topmost edit-window"
                       :value left-top)
                (const :tag "Last edit-window with point"
                       :value last-point)))


(defcustom ecb-common-tree-buffer-after-create-hook nil
  "*Local hook running at the end of each tree-buffer creation.
Every function of this hook is called once without arguments direct after
creating a tree-buffer of ECB and it's local key-map. So for example a function
could be added which performs calls of `local-set-key' to define new
key-bindings for EVERY tree-buffer.

The following keys must not be rebind in all tree-buffers:
- <RET> and all combinations with <Shift> and <Ctrl>
- <TAB>
- `C-t'"
  :group 'ecb-tree-buffer
  :type 'hook)
  
;;====================================================
;; Internals
;;====================================================

;; all defined tree-buffer creators

(defvar ecb-tree-buffer-creators nil
  "The tree-buffer creators of ECB.
An alist where each element is a cons where the car is a symbol which contains
the name of a tree-buffer \(e.g. `ecb-sources-buffer-name') and the cdr is the
associated function-symbol which creates the tree-buffer with that name.")

(defun ecb-tree-buffer-creators-init ()
  "Initialize `ecb-tree-buffer-creators'.
Removes all creators and set it to nil."
  (setq ecb-tree-buffer-creators nil))

(defun ecb-tree-buffer-creators-register (name-symbol fn)
  "Register the creator-function FN for the tree-buffer NAME-SYMBOL."
  (add-to-list 'ecb-tree-buffer-creators (cons name-symbol fn)))

(defun ecb-tree-buffer-creators-run ()
  "Run all currently registered creator-functions."
  (dolist (creator-elem ecb-tree-buffer-creators)
    ;; create all the tree-buffers if they don't already exist
    (funcall (cdr creator-elem))))
  

(defmacro defecb-tree-buffer-creator (creator
                                      tree-buffer-name-symbol
                                      docstring &rest body)
  "Define a creator-function CREATOR for a tree-buffer which name is hold in
the symbol TREE-BUFFER-NAME-SYMBOL. Do not quote CREATOR and
TREE-BUFFER-NAME-SYMBOL. DOCSTRING is the docstring for CREATOR. BODY is all
the program-code of CREATOR \(must contain a call to `tree-buffer-create'). It
makes sense that BODY returns the created tree-buffer.

When creating a tree-buffer with this macro then this tree-buffer will be
automatically created \(i.e. its creator-function defined with this macro will
be called) when activating ECB and the tree-buffer will automatically
registered at ECB. This means that some features of ECB will work
automatically out of the box with this tree-buffer.

When creating a tree-buffer for ECB then it MUST be created with this macro
and not with `tree-buffer-create'!"
  `(eval-and-compile
     (ecb-tree-buffer-creators-register (quote ,tree-buffer-name-symbol)
                                        (quote ,creator))
     (defun ,creator ()
       ,docstring
       (unless (ecb-tree-buffers-get-symbol ,tree-buffer-name-symbol)
         (ecb-tree-buffers-add ,tree-buffer-name-symbol
                               (quote ,tree-buffer-name-symbol))
         ,@body))))

(put 'defecb-tree-buffer-creator 'lisp-indent-function 2)

;; all created tree-buffers 

(defvar ecb-tree-buffers nil
  "The tree-buffers of ECB.
An alist with a cons for each created \(do not confuse created with visible!)
tree-buffer where the car is the name of the tree-buffer and the cdr is the
associated symbol which contains this name.")

(defsubst ecb-tree-buffers-init ()
  (setq ecb-tree-buffers nil))

(defsubst ecb-tree-buffers-add (name name-symbol)
  (unless (ecb-find-assoc name ecb-tree-buffers)
    (setq ecb-tree-buffers
          (ecb-add-assoc (cons name name-symbol) ecb-tree-buffers))))

(defsubst ecb-tree-buffers-name-list ()
  (mapcar (function (lambda (e) (car e))) ecb-tree-buffers))

(defsubst ecb-tree-buffers-symbol-list ()
  (mapcar (function (lambda (e) (cdr e))) ecb-tree-buffers))

(defsubst ecb-tree-buffers-buffer-list ()
  (mapcar (function (lambda (e) (get-buffer (car e)))) ecb-tree-buffers))

(defsubst ecb-tree-buffers-get-symbol (name)
  (ecb-find-assoc-value name ecb-tree-buffers))


  
;; the filename/path cache

(defecb-multicache ecb-filename-cache 500 nil '(FILES-AND-SUBDIRS
                                                EMPTY-DIR-P
                                                SOURCES
                                                VC
                                                FIXED-FILENAMES
                                                REMOTE-PATH
                                                HOST-ACCESSIBLE)
  "Cache used for the filebrowser to cache all necessary informations
associated to file- or directory-names.

Currently there are three subcaches managed within this cache:

  FILES-AND-SUBDIRS:
  
  Cache for every directory all subdirs and files. This is a cache with
     key:   <directory>
     value: \(<file-list> . <subdirs-list>)
  
  EMPTY-DIR-P:
  
  Cache for every directory if it is empty or not. This is a cache with
     key:   <directory>
     value: \(\[nil|t] . <checked-with-show-sources>)
  
  SOURCES:
  
  Cache for the contents of the buffer `ecb-sources-buffer-name'. This is a
  cache with
     key:   <directory>
     value: \(<full-content> . <filtered-content>)
  whereas <full-content> is a 3-elem list \(tree-buffer-root <copy of
  tree-buffer-displayed-nodes> buffer-string) for a full \(i.e. all files)
  cache and <filtered-content> is a 4-elem list \(tree-buffer-root <copy of
  tree-buffer-displayed-nodes> sources-buffer-string <filter>) for a filtered
  cache where <filter> is a cons-cell \(<filter-regexp> . <filter-display>).

  VC:

  Cache necessary informations for the version-control-support. This is a
  cache for filenames and directories. In case of a file with
     key: <filename> of a sourcefile
     value: \(<state> <check-timestamp> <checked-buffers>)
  whereas <state> is the that VC-state the file had at time <check-timestamp>.
  <checked-buffers> is a list of tree-buffer-names for which <state> was
  checked.
  In case of a directory with
     key: <dirname> of a directory
     value: <vc-state-fcn> or 'NO-VC
  <vc-state-fcn> is the function used to get the VC-state if <check-timestamp>
  is older than the most recent modification-timestamp of <filename>.

  FIXED-FILENAMES:

  Cache for fixed filenames which can speedup handling-remote-paths \(like
  tramp-paths)
     key: The concatenation of the args PATH and FILENAME of `ecb-fix-filename'.
     value: The result of `ecb-fix-filename' for these args.

  REMOTE-PATH:

  Cache if a path is a remote path and store its components if yes.
     key: a path
     value: 'NOT-REMOTE if not a remote path otherwise the result of
     `ecb-remote-path'.

  HOST-ACCESSIBLE:

  Cache if a host is accessible or not.
     key: a host \(e.g. ecb.sourceforge.net)
     value: \(<timestamp> . <value>) whereas <timestamp> is the cache time of
     <value> and <value> is either 'NOT-ACCESSIBLE if host is not accessible
     or t if accessible.
")

(defun ecb-filename-cache-init ()
  "Initialize the whole cache for file- and directory-names"
  (if (ecb-multicache-p 'ecb-filename-cache)
      (ecb-multicache-clear 'ecb-filename-cache)))

;; directory separator

(defconst ecb-directory-sep-char
  (if ecb-running-xemacs directory-sep-char ?/))

(defsubst ecb-directory-sep-char (&optional refdir)
  (if (or (null refdir)
          (not (ecb-remote-path refdir)))
      ecb-directory-sep-char
    ?/))

(defsubst ecb-directory-sep-string (&optional refdir)
  (char-to-string (ecb-directory-sep-char refdir)))   


;;; ----- Wrappers for file- and directory-operations ------

(dolist (f '(file-name-nondirectory
             file-exists-p
             file-name-directory
             file-readable-p
             file-attributes
             file-name-extension
             file-directory-p
             file-accessible-directory-p
             file-name-sans-extension
             file-writable-p
             file-name-as-directory
             directory-files))
  (fset (intern (format "ecb-%s" f))
        `(lambda (file-or-dir-name &rest args)
           ,(format "Delegate all args to `%s' but call first `ecb-fix-path' for FILE-OR-DIR-NAME." f)
           (apply (quote ,f) (ecb-fix-path file-or-dir-name) args))))

(defun ecb-expand-file-name (name &optional default-dir)
  "Delegate all args to `expand-file-name' but call first `ecb-fix-path'
for both args."
  (expand-file-name (ecb-fix-path name) (ecb-fix-path default-dir)))

;;; ----- Canonical filenames ------------------------------

(defun ecb-fix-path (path)
  "Fixes an annoying behavior of the native windows-version of XEmacs:
When PATH contains only a drive-letter and a : then `expand-file-name' does
not interpret this PATH as root of that drive. So we add a trailing
`directory-sep-char' and return this new path because then `expand-file-name'
treats this as root-dir of that drive. For all \(X)Emacs-version besides the
native-windows-XEmacs PATH is returned."
  (if (and ecb-running-xemacs
           (equal system-type 'windows-nt))
      (if (and (= (length path) 2)
               (equal (aref path 1) ?:))
          (concat path (ecb-directory-sep-string))
        path)
    path))

;; accessors for the FIXED-FILENAMES-cache

(defsubst ecb-fixed-filename-cache-put (path filename fixed-filename)
  "Add FIXED-FILENAME for PATH and FILENAME to the FIXED-FILENAMES-cache
of `ecb-filename-cache'."
  (ecb-multicache-put-value 'ecb-filename-cache
                            (concat path filename)
                            'FIXED-FILENAMES
                            fixed-filename))

(defsubst ecb-fixed-filename-cache-get (path filename)
  "Get the cached value for PATH and FILENAME from the FIXED-FILENAMES-cache
in `ecb-filename-cache'. If no vaue is cached for PATH and FILENAME then nil
is returned."
  (ecb-multicache-get-value 'ecb-filename-cache
                            (concat path filename)
                            'FIXED-FILENAMES))

(defun ecb-fixed-filename-cache-dump (&optional no-nil-value)
  "Dump the whole FIXED-FILENAMES-cache. If NO-NIL-VALUE is not nil then these
cache-entries are not dumped. This command is not intended for end-users of ECB."
  (interactive "P")
  (ecb-multicache-print-subcache 'ecb-filename-cache
                                 'FIXED-FILENAMES
                                 no-nil-value))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: What about the new cygwin-version
;; of GNU Emacs 21? We have to test if this function and all locations where
;; `ecb-fix-path' is used work correctly with the cygwin-port of GNU Emacs.
(silentcomp-defun mswindows-cygwin-to-win32-path)
(defun ecb-fix-filename (path &optional filename substitute-env-vars)
  "Normalizes path- and filenames for ECB. If FILENAME is not nil its pure
filename \(i.e. without directory part) will be concatenated to PATH. The
result will never end with the directory-separator! If SUBSTITUTE-ENV-VARS is
not nil then in both PATH and FILENAME env-var substitution is done. If the
`system-type' is 'cygwin32 then the path is converted to win32-path-style!"
  (when (stringp path)
    (or (ecb-fixed-filename-cache-get path filename)
        (let ((remote-path (ecb-remote-path path))
              (norm-path nil)
              (result nil))
          (if (or (not remote-path)
                  (ecb-host-accessible-p (nth 1 remote-path)))
              (progn
                (setq norm-path (if ecb-running-xemacs
                                    (case system-type
                                      (cygwin32
                                       (mswindows-cygwin-to-win32-path
                                        (expand-file-name path)))
                                      (windows-nt
                                       (expand-file-name (ecb-fix-path path)))
                                      (otherwise
                                       (expand-file-name path)))
                                  (expand-file-name path)))
                ;; substitute environment-variables
                (setq norm-path (expand-file-name (if substitute-env-vars
                                                      (substitute-in-file-name norm-path)
                                                    norm-path))))
            (setq norm-path path))
          ;; For windows systems we normalize drive-letters to downcase
          (setq norm-path (if (and (member system-type '(windows-nt cygwin32))
                                   (> (length norm-path) 1)
                                   (equal (aref norm-path 1) ?:))
                              (concat (downcase (substring norm-path 0 2))
                                      (substring norm-path 2))
                            norm-path))
          ;; delete a trailing directory-separator if there is any
          (setq norm-path (if (and (> (length norm-path) 1)
                                   (= (aref norm-path (1- (length norm-path)))
                                      (ecb-directory-sep-char path)))
                              (substring norm-path 0 (1- (length norm-path)))
                            norm-path))
          (setq result
                (concat norm-path
                        (if (stringp filename)
                            (concat (when (> (length norm-path) 1)
                                      ;; currently all protocols like tramp,
                                      ;; ange-ftp or efs support only not
                                      ;; windows-remote-hosts ==> we must not
                                      ;; add a backslash here (would be done
                                      ;; in case of a native Windows-XEmacs)
                                      (ecb-directory-sep-string path))
                                    (file-name-nondirectory (if substitute-env-vars
                                                                (substitute-in-file-name filename)
                                                              filename))))))
          (ecb-fixed-filename-cache-put path filename result)
          result))))

;; -- end of canonical filenames

;; -- interactors synchronizers
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Has to be done!!!

(defvar ecb-interactor-synchronizers nil)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Only called from within
;; `defecb-interactor-synchronizer'.
(defun ecb-interactor-synchronizer-register (buffer-name-symbol
                                             synchronizer-fcn)
  ""
  )

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: This function can only be used if
;; a synchronizer is already registered.
(defun ecb-interactor-synchronizer-activate (&optional arg)
  "Activate if ARG >= 0, deactivate if ARG < 0. Toggle if ARG is nil."
  )

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: We do not change the
;; synchronizing of the basic-interactors! eshell and speedbar synchonizing
;; won't be changed too. So the internal-hook still remains for this stuff! But
;; all add-on-interactors (currently analyse and symboldef) MUST use the new
;; macro `defecb-interactor-synchronizer' for their synchronizers! For these
;; we introduce also a new option `ecb-add-on-interactor-sync-delay' (if a
;; buffer is not contained it will not be synced anymore!) which allows adding
;; delays for these interactors. In addition we rename `ecb-window-sync' to
;; `ecb-interactor-sync' and `ecb-window-sync-delay' to
;; `ecb-basic-interactor-sync-delay'. Maybe there are further
;; options/functions/commands to rename!?

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Hmmmmmmm, maybe we should ensure
;; that the add-on synchonizers will always being called AFTER the basic ones?
;; For this we should name the new option
;; `ecb-add-on-interactor-sync-delay-plus' which means each add-on interactor
;; has at elast the basic delay of `ecb-basic-interactor-sync-delay' plus
;; eventually some delay on top (this addon-delay must be realized with
;; `ecb-run-with-timer'). Hmm, i think i have to make some brainstorming what
;; is the better approach!
(defmacro defecb-interactor-synchronizer (synchronizer
                                          buffer-name-symbol
                                          docstring &rest body)
  "Define a creator-function CREATOR for a tree-buffer which name is hold in
the symbol TREE-BUFFER-NAME-SYMBOL. Do not quote CREATOR and
TREE-BUFFER-NAME-SYMBOL. DOCSTRING is the docstring for CREATOR. BODY is all
the program-code of CREATOR \(must contain a call to `tree-buffer-create'). It
makes sense that BODY returns the created tree-buffer.

When creating a tree-buffer with this macro then this tree-buffer will be
automatically created \(i.e. its creator-function defined with this macro will
be called) when activating ECB and the tree-buffer will automatically
registered at ECB. This means that some features of ECB will work
automatically out of the box with this tree-buffer.

When creating a tree-buffer for ECB then it MUST be created with this macro
and not with `tree-buffer-create'!"
  `(eval-and-compile
     (ecb-interactor-synchronizer-register (quote ,buffer-name-symbol)
                                           (quote ,synchronizer))
     (defun ,synchronizer ()
       ,docstring
       (interactive)
       (ecb-do-if-buffer-visible-in-ecb-frame (quote ,buffer-name-symbol)
         ,@body))))

;; (insert (pp (macroexpand
;;              '(defecb-interactor-synchronizer ecb-analyse-buffer-sync-test
;;                 ecb-analyse-buffer-name
;;                 "testdoctsirng"
;;                 (let ((analysis nil)
;;                       (completions nil)
;;                       (fnargs nil)
;;                       (cnt nil)
;;                       )
;;                   ;; Try and get some sort of analysis
;;                   (ignore-errors
;;                     (save-excursion
;;                       (setq analysis (ecb--semantic-analyze-current-context (point)))
;;                       (setq cnt (ecb--semantic-find-tag-by-overlay))
;;                       (when analysis
;;                         (setq completions (ecb--semantic-analyze-possible-completions analysis))
;;                         (setq fnargs (ecb--semantic-get-local-arguments (point)))
;;                         )))
;;                   (ecb-exec-in-window ecb-analyse-buffer-name
;;                     ;; we must remove the old nodes
;;                     (tree-buffer-set-root (tree-node-new-root))
;;                     (when analysis
;;                       ;; Now insert information about the context
;;                       (when cnt
;;                         (ecb-analyse-add-nodes "Context" "Context"
;;                                                cnt ecb-analyse-nodetype-context))
;;                       (when fnargs
;;                         (ecb-analyse-add-nodes "Arguments" "Arguments" fnargs
;;                                                ecb-analyse-nodetype-arguments))
;;                       ;; Let different classes draw more nodes.
;;                       (ecb-analyse-more-nodes analysis)
;;                       (when completions
;;                         (ecb-analyse-add-nodes "Completions" "Completions" completions
;;                                                ecb-analyse-nodetype-completions)))
;;                     (tree-buffer-update)))))))

;; -- end of interactors synchronizers

(defun ecb-format-bucket-name (name)
  "Format NAME as a bucket-name according to `ecb-bucket-node-display'."
  (let ((formatted-name (concat (nth 0 ecb-bucket-node-display)
				name
				(nth 1 ecb-bucket-node-display))))
    (ecb-merge-face-into-text formatted-name (nth 2 ecb-bucket-node-display))
    formatted-name))

(defun ecb-toggle-do-not-leave-window-after-select ()
  "Toggles if a node-selection in a tree-buffer leaves the tree-window.
See also the option `ecb-tree-do-not-leave-window-after-select'."
  (interactive)
  (let ((tree-buffer (ecb-point-in-ecb-tree-buffer)))
    (if tree-buffer
        (let ((tree-buf-name (buffer-name tree-buffer)))
          (if (ecb-member-of-symbol/value-list
               tree-buf-name
               ecb-tree-do-not-leave-window-after-select--internal)
              (progn
                (setq ecb-tree-do-not-leave-window-after-select--internal
                      ;; we must try both - the symbol of the tree-buffer-name
                      ;; and the tree-buffer-name because we do not know what
                      ;; the user has specified in
                      ;; `ecb-tree-do-not-leave-window-after-select'!
                      (delete (ecb-tree-buffers-get-symbol tree-buf-name)
                              (delete tree-buf-name
                                      ecb-tree-do-not-leave-window-after-select--internal)))
                (message "Selections leave the tree-window of %s" tree-buf-name))
            (setq ecb-tree-do-not-leave-window-after-select--internal
                  (append ecb-tree-do-not-leave-window-after-select--internal
                          (list (ecb-tree-buffers-get-symbol tree-buf-name))))
            (message "Selections don't leave the tree-window of %s." tree-buf-name)))
      (message "Point must stay in an ECB tree-buffer!"))))

(defun ecb-common-tree-buffer-modeline-menu-creator (buf-name)
  "Return a menu for the modeline of all ECB-tree-buffers."
  '((delete-other-windows "Maximize Window Above")
    (ecb-redraw-layout-preserving-compwin-state "Display All ECB-windows")))

(defun ecb-common-after-tree-buffer-create-actions ()
  "Things which should be performed after creating a tree-buffer.
The tree-buffer is the current buffer."
  (local-set-key (kbd "C-t") 'ecb-toggle-do-not-leave-window-after-select)
  (if ecb-running-xemacs
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Is it necessary to make
      ;; modeline-map buffer-local for current buffer first?!
      (define-key modeline-map
        '(button2up)
        'ecb-toggle-maximize-ecb-window-with-mouse)
    (local-set-key [mode-line mouse-2]
                   'ecb-toggle-maximize-ecb-window-with-mouse)))
  

(defun ecb-combine-ecb-button/edit-win-nr (ecb-button edit-window-nr)
  "Depending on ECB-BUTTON and EDIT-WINDOW-NR return one value:
- nil if ECB-BUTTON is 1.
- t if ECB-BUTTON is 2 and the edit-area of ECB is splitted.
- EDIT-WINDOW-NR if ECB-BUTTON is 3."
  (case ecb-button
    (1 nil)
    (2 (ecb-edit-window-splitted))
    (3 edit-window-nr)))

(defun ecb-get-edit-window (other-edit-window)
  "Get the correct edit-window. Which one is the correct one depends on the
value of OTHER-EDIT-WINDOW \(which is a value returned by
`ecb-combine-ecb-button/edit-win-nr') and `ecb-mouse-click-destination'.
- OTHER-EDIT-WINDOW is nil: Get the edit-window according to the option
  `ecb-mouse-click-destination'.
- OTHER-EDIT-WINDOW is t: Get the next edit-window in the cyclic list of
  current edit-windows starting either from the left-top-most one or from the
  last edit-window with point (depends on
  `ecb-mouse-click-destination').
- OTHER-EDIT-WINDOW is an integer: Get exactly the edit-window with that
  number > 0."
  (let ((edit-win-list (ecb-canonical-edit-windows-list)))
    (typecase other-edit-window
      (null
       (if (eq ecb-mouse-click-destination 'left-top)
           (car edit-win-list)
         ecb-last-edit-window-with-point))
      (integer
       (ecb-get-edit-window-by-number other-edit-window edit-win-list))
      (otherwise
       (ecb-next-listelem edit-win-list
                          (if (eq ecb-mouse-click-destination 'left-top)
                              (car edit-win-list)
                            ecb-last-edit-window-with-point))))))

;;====================================================
;; Mouse callbacks
;;====================================================

(defun ecb-tree-buffer-node-select-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
                                             meta-pressed
					     tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node has been selected. This function does nothing if the click
combination is invalid \(see `ecb-interpret-mouse-click'."
  (let* ((ecb-button-list (ecb-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
	 (ecb-button (nth 0 ecb-button-list))
	 (shift-mode (nth 1 ecb-button-list))
         (meta-mode (nth 2 ecb-button-list))
         (keyboard-p (equal (nth 3 ecb-button-list) 'keyboard))
         (maximized-p (ecb-buffer-is-maximized-p tree-buffer-name)))
    ;; we need maybe later that something has clicked in a tree-buffer, e.g.
    ;; in `ecb-handle-major-mode-visibilty'.
    (setq ecb-item-in-tree-buffer-selected t)
    (if (not keyboard-p)
        (setq ecb-layout-prevent-handle-ecb-window-selection t))
    ;; first we dispatch to the right action
    (when ecb-button-list
      (cond ((ecb-string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-directory-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-history-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-methods-buffer-name)
	     (ecb-method-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-analyse-buffer-name)
	     (ecb-analyse-node-clicked node ecb-button nil shift-mode meta-mode))
	    (t nil)))

    ;; now we go back to the tree-buffer but only if all of the following
    ;; conditions are true:
    ;; 1. The ecb-windows are now not hidden
    ;; 2. The tree-buffer-name is contained in
    ;;    ecb-tree-do-not-leave-window-after-select--internal
    ;; 3. Either it is not the ecb-directories-buffer-name or
    ;;    at least `ecb-show-sources-in-directories-buffer-p' is true and the
    ;;    hitted node is a sourcefile
    (when (and (not ecb-windows-hidden)
               (ecb-member-of-symbol/value-list
                tree-buffer-name
                ecb-tree-do-not-leave-window-after-select--internal)
               (or (not (ecb-string= tree-buffer-name ecb-directories-buffer-name))
                   (and (ecb-show-sources-in-directories-buffer-p)
                        (= ecb-directories-nodetype-sourcefile
                           (tree-node->type node)))))
      ;; If there is currently no maximized window then we can savely call
      ;; `ecb-goto-ecb-window'. If we have now a maximized window then there
      ;; are two possibilities:
      ;; - if it is not equal to the maximzed tree-buffer before the selection
      ;;   then we must maximi
      (if (and maximized-p
               (not (ecb-buffer-is-maximized-p tree-buffer-name)))
          (ecb-maximize-ecb-buffer tree-buffer-name t)
        (ecb-goto-ecb-window tree-buffer-name))
      (tree-buffer-remove-highlight))))
 
(defun ecb-tree-buffer-node-collapsed-callback (node
                                                mouse-button
                                                shift-pressed
                                                control-pressed
                                                meta-pressed
                                                tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node has been collapsed."
  (let* ((ecb-button-list (ecb-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
         (keyboard-p (equal (nth 3 ecb-button-list) 'keyboard)))
    (if (not keyboard-p)
        (setq ecb-layout-prevent-handle-ecb-window-selection t))))

(defun ecb-tree-buffer-node-expand-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
                                             meta-pressed
					     tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node should be expanded. This function does nothing if the click
combination is invalid \(see `ecb-interpret-mouse-click')."
  (let* ((ecb-button-list (ecb-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
	 (ecb-button (nth 0 ecb-button-list))
	 (shift-mode (nth 1 ecb-button-list))
         (meta-mode (nth 2 ecb-button-list))
         (keyboard-p (equal (nth 3 ecb-button-list) 'keyboard)))
    (if (not keyboard-p)
        (setq ecb-layout-prevent-handle-ecb-window-selection t))
    (when ecb-button-list
      (cond ((ecb-string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-update-directory-node node))
	    ((ecb-string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-history-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-methods-buffer-name)
	     nil)
	    (t nil)))))

(defun ecb-interpret-mouse-click (mouse-button
                                  shift-pressed
                                  control-pressed
                                  meta-pressed
                                  tree-buffer-name)
  "Converts the physically pressed MOUSE-BUTTON \(1 = mouse-1, 2 = mouse-2, 0 =
no mouse-button but the keys RET or TAB) to ECB-mouse-buttons: either primary
or secondary mouse-button depending on the value of CONTROL-PRESSED and the
setting in `ecb-primary-secondary-mouse-buttons'. Returns a list
'\(<ECB-button> <shift-mode> <meta-mode> <device>) where <ECB-button> is
either 1 \(= primary) or 2 \(= secondary) and <shift-mode> and <meta-mode> are
non nil if SHIFT-PRESSED rsp. META-PRESSED is non nil. <device> is either
'mouse or 'keyboard dependent if the uses has used the mouse rsp. the keyboard
in the tree-buffer. For an invalid and not accepted click combination nil is
returned.

Note: If MOUSE-BUTTON is 0 \(means no mouse-button but a key like RET or TAB
was hitted) then CONTROL-PRESSED is interpreted as ECB-button 2.

Currently the fourth argument TREE-BUFFER-NAME is not used here."
  (if (eq mouse-button 0)
      (list (if control-pressed 2 1) shift-pressed meta-pressed 'keyboard)
    (if (and (not (eq mouse-button 1)) (not (eq mouse-button 2)))
	nil
      (case ecb-primary-secondary-mouse-buttons
        (mouse-1--mouse-2
         (if control-pressed
             nil
           (list mouse-button shift-pressed meta-pressed 'mouse)))
        (mouse-1--C-mouse-1
         (if (not (eq mouse-button 1))
             nil
           (list (if control-pressed 2 1) shift-pressed meta-pressed 'mouse)))
        (mouse-2--C-mouse-2
         (if (not (eq mouse-button 2))
             nil
           (list (if control-pressed 2 1) shift-pressed meta-pressed 'mouse)))
        (otherwise nil)))))

(defun ecb-show-minibuffer-info (node window when-spec)
  "Checks if any info about the current node in the ECB-window WINDOW should
be displayed. WHEN-SPEC must have the same format as the car of
`ecb-directories-show-node-info'."
  (or (eq when-spec 'always)
      (and (eq when-spec 'if-too-long)
           window
           (>= (tree-node-linelength node)
               (window-width window)))))


(tree-buffer-defpopup-command ecb-maximize-ecb-window-menu-wrapper
  "Expand the current ECB-window from popup-menu."
  (ecb-maximize-ecb-buffer (buffer-name (current-buffer)) t))

;; stealthy mechanism

(defvar ecb-stealthy-function-list nil
  "List of functions which ECB runs stealthy. Do not modify this variable!
This variable is autom. set by the macro `defecb-stealthy'!")

(defvar ecb-stealthy-function-state-alist nil
  "Alist which stores the state of each function of
`ecb-stealthy-function-list'. Do not add new items to this variable because
this is autom. done by the macro `defecb-stealthy'!")

(defun ecb-stealthy-function-list-add (fcn)
  (add-to-list 'ecb-stealthy-function-list fcn))

(defun ecb-stealthy-function-state-alist-add (fcn)
  (add-to-list 'ecb-stealthy-function-state-alist
               (cons fcn 'done)))

(defun ecb-stealthy-function-state-get (fcn)
  "Getter for `ecb-stealthy-function-state-alist'. Return state for the
stealthy function FCN."
  (cdr (assoc fcn ecb-stealthy-function-state-alist)))

(defun ecb-stealthy-function-state-set (fcn state)
  "Setter for `ecb-stealthy-function-state-alist'. Set STATE for the
stealthy function FCN. Return STATE."
  (setcdr (assoc fcn ecb-stealthy-function-state-alist) state))

(defun ecb-stealthy-function-p (fcn)
  "Return not nil if FCN is a stealthy function defined with
`defecb-stealthy'."
  (member fcn ecb-stealthy-function-list))

(defun ecb-stealthy-function-state-init (&optional fcn state)
  "Reset the state of stealthy functions. If first optional arg FCN is a
stealthy function then only the state of this function is reset - otherwise
all stealthy functions of `ecb-stealthy-function-list' are reset. If second
optional arg STATE is nil then the state will be reset to the special state
'restart - otherwise to the value STATE."
  (if (and fcn (ecb-stealthy-function-p fcn))
      (ecb-stealthy-function-state-set fcn (or state 'restart))
    (dolist (f ecb-stealthy-function-list)
      (ecb-stealthy-function-state-set f (or state 'restart)))))

(defmacro defecb-stealthy (name docstring &rest body)
  "Define a so called stealthy function with NAME. This function will be
registered by this macro in `ecb-stealthy-function-list' and
`ecb-stealthy-function-state-alist'. During the evaluation of BODY the
variable `state' will be bound and initialized with the stealthy state. BODY
can use and modify `state'. After evaluating BODY `state' will be
automatically saved so its available at the runtime of this stealthy function.
BODY will only be evaluated if `state' is not 'done. BODY should be designed
to be interruptable by the user, so its recommended to use for this
`ecb-exit-on-input' ans `ecb-throw-on-input' \(see example in
`ecb-test-throw-on-input'). If BODY completes then BODY has to set `state' to
the special value 'done! If BODY has been interrupted then `state' can have an
arbitrary value which will be autom. stored and at next runtime of the
stealthy function NAME `state' will be initialized with this stored value. If
`state' is initialized with the special value 'restart then this means the
stealthy function should start from scratch because an eventually stored state
is not longer valid. If the stealthy function sets `state' to 'done then this
function will first being called after the state for this function has been
reset to something else than 'done \(mostly to 'restart)\; such a reset of the
state for a stealthy function can be done by any code and must be done via
`ecb-stealthy-function-state-init'!"
  `(progn
     (unless (fboundp (quote ,name))
       (ecb-stealthy-function-list-add (quote ,name))
       (ecb-stealthy-function-state-alist-add (quote ,name)))
     (eval-and-compile
       (unless (fboundp (quote ,name))
         (defun ,name nil
           ,docstring
           (let ((state (ecb-stealthy-function-state-get (quote ,name))))
             (unless (equal state 'done)
               ,@body)
             (ecb-stealthy-function-state-set (quote ,name) state)))))))
  
(put 'defecb-stealthy 'lisp-indent-function 1)

(defvar ecb-stealthy-update-running nil
  "Recursion avoidance variable for stealthy performance.")

(defun ecb-stealthy-updates ()
  "Run all functions in the stealthy function list.
Each function returns 'done if it completes successfully, or something else if
interrupted by the user \(i.e. the function has been interrupted by the
user). If a function is interrupted then `ecb-stealthy-function-list' is
rotated so the interrupted function is the first element so the next stealthy
run starts with this interrupted function."
  (ecb-debug-autocontrol-fcn-error 'ecb-stealthy-updates
                                   "Begin: Cur-buf: %s" (current-buffer))
  (unless ecb-stealthy-update-running
    (let ((l ecb-stealthy-function-list)
          (ecb-stealthy-update-running t)
          ;; necessary because timers set this locally to t to prevent
          ;; timer-actions from being quitted by C-g. Our potentially long
          ;; lasting actions must be quit-able!
          (inhibit-quit nil))
      (while (and l (equal 'done (funcall (car l))))
        (setq l (cdr l)))
      ;; if l is nil this means all functions have successfully completed -
      ;; otherwise we ensure that next time we start with the interrupted
      ;; function.
      (when l
        (setq ecb-stealthy-function-list
              (ecb-rotate ecb-stealthy-function-list (car l))))))
  (ecb-debug-autocontrol-fcn-error 'ecb-stealthy-updates
                                   "End: Cur-buf: %s" (current-buffer)))


;; generation of nodes rsp. of attributes of nodes

(defun ecb-generate-node-name (text-name first-chars icon-name name-of-buffer)
  "Generate a new name from TEXT-NAME by adding an appropriate image according
to ICON-NAME to the first FIRST-CHARS of TEXT-NAME. If FIRST-CHARS is < 0 then
a string with length abs\(FIRST-CHARS) is created, the image is applied to
this new string and this \"image\"-string is added to the front of TEXT-NAME.
If no image can be found for ICON-NAME then the original TEXT-NAME is
returned. NAME-OF-BUFFER is the name of the tree-buffer where the resulting
node-name will be displayed.

If an image is added then two text-properties are added to the FIRST-CHARS of
the returned string: 'tree-buffer-image-start which holds 0 as value and
'tree-buffer-image-length which holds the absolute value of FIRST-CHARS
value."
  (let ((image nil))
    (save-excursion
      (set-buffer name-of-buffer)
      (setq image (and icon-name (tree-buffer-find-image icon-name)))
      (if image
          (if (> first-chars 0)
              (tree-buffer-add-image-icon-maybe
               0 first-chars text-name image)
            (concat (tree-buffer-add-image-icon-maybe
                     0 (- first-chars)
                     (make-string (- first-chars) ? ) image)
                    text-name))
        text-name))))


(silentcomp-provide 'ecb-common-browser)

;;; ecb-common-browser.el ends here

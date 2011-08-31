;;; ecb-analyse.el --- ECB analysis display window

;;; Copyright (C) 2004 - 2005 Klaus Berndl

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, analyse
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

;; $Id: ecb-analyse.el,v 1.25 2009/05/08 14:05:55 berndl Exp $


;;; Commentary:
;;
;; Displays the analysing informations of semantic-analyze in a special
;; tree-buffer.
;;

;;; Code:

(require 'semantic-analyze)
(require 'ecb-layout)
(require 'ecb-common-browser)
(require 'ecb-method-browser)

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))


(defgroup ecb-analyse nil
  "Settings for the analyse-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")


(defcustom ecb-analyse-buffer-name " *ECB Analyse*"
  "*Name of the ECB analyse buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Analyse*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-analyse-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-analyse
  :type 'string)

(defcustom ecb-analyse-buffer-after-create-hook nil
  "*Local hook running after the creation of the analyse-buffer.
Every function of this hook is called once without arguments direct after
creating the analyse-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the analyse-buffer of ECB."
  :group 'ecb-analyse
  :type 'hook)

(defcustom ecb-analyse-show-node-info '(if-too-long . name)
  "*When to display which node-info in the analyse-buffer.
Define which node info should displayed after moving the mouse over a node
\(or after a shift click onto the node) in the analyse-buffer.

You can define \"when\" a node-info should be displayed:
See `ecb-directories-show-node-info' for the possible choices.

You can define what info should be displayed:
- name: The full name of the node
- full-info: All infos available to a node.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-analyse
  :type '(cons (choice :tag "When"
                       (const :tag "Always" :value always)
                       (const :tag "If too long" :value if-too-long)
                       (const :tag "After shift click" :value shift-click)
                       (const :tag "Never" :value never))
               (choice :tag "What"
                       (const :tag "Node-name" :value name)
                       (const :tag "Full info" :value full-info))))


(defcustom ecb-analyse-collapsed-buckets nil
  "*Buckets collapsed when displaying the current semantic analysis.
The semantic analyse-modul offers several categories of analysis which are
called buckets here. These are for example:

Context: The current context, which is the current function/method, variable,
class etc. \(what exactly depends on the programming language) point is in.
This means not the current function/method/variable/class-name point stand on
but the current surrounding context. Example: If point stays somewhere within
a defun-definition in emacs-lisp or within a java-method then this defun rsp.
method is the context. In object oriented languages this can be the full
hierachy, i.e. not only the current method, but the current method, the class
of this method, the superclass of this class and so on!

Local Variables: All accessible and bound local variables visible at current
point.

Prefix: The currently parsed prefix, which is mostly the current identifier
point stands on.

Assignee: See the semantic manual

Function: Current function-name point stands on.

Argument #: When point is located within a function-call then this is the
number of the argument point stands on.

Completions: All possible completions for current prefix \(see above). This is
probably the most helpful bucket.

If one of these categories/buckets are not needed per default then add the
bucket-name \(s.a.) to this option and ECB will per default collapse this
bucket. So most needed buckets are better visible in the analyse-buffer."
  :group 'ecb-analyse
  :type '(repeat (choice :tag "Bucket" :menu-tag "Bucket"
                         (const :tag "Context" :value "Context")
                         (const :tag "Arguments" :value "Arguments")
                         (const :tag "Local Variables" :value "Local Variables")
                         (const :tag "Prefix" :value "Prefix")
                         (const :tag "Assignee" :value "Assignee")
                         (const :tag "Function" :value "Function")
                         (const :tag "Argument #" :value "Argument #")
                         (const :tag "Completions" :value "Completions")
                         (string :tag "Other bucketname"))))

(defcustom ecb-analyse-fontified-buckets '("Context" "Function")
  "*Buckets whose elements should be fontified as in the methods-buffer.
If the name of a category/bucket is contained in this option then
all elements of this bucket will be displayed as in the
methods-buffer - at least if an element is a semantic-tag. This
means if `ecb-font-lock-tags' is not nil and the font-lock
feature is loaded into Emacs these elements will be fontified and
also displayed with an appropriate icon if possible. The default
value does this only for the Context-bucket because for most of
the other buckets this makes not really much sense.

For available buckets see `ecb-analyse-collapsed-buckets'.

For the faces used to display a bucket-node itself or bucket-elements not
fontified see the options `ecb-analyse-bucket-node-face' rsp.
`ecb-analyse-bucket-element-face'."
  :group 'ecb-analyse
  :type '(repeat (choice :tag "Bucket" :menu-tag "Bucket"
                         (const :tag "Context" :value "Context")
                         (const :tag "Arguments" :value "Arguments")
                         (const :tag "Local Variables" :value "Local Variables")
                         (const :tag "Prefix" :value "Prefix")
                         (const :tag "Assignee" :value "Assignee")
                         (const :tag "Function" :value "Function")
                         (const :tag "Argument #" :value "Argument #")
                         (const :tag "Completions" :value "Completions")
                         (string :tag "Other bucketname"))))

(defcustom ecb-analyse-gen-tag-info-fn nil
  "*Which info should be displayed for a tag of the analyse-buffer.
If nil then the default information about a tag will be displayed. If a
function then this function gets as argument the tag for which tag-information
should be displayed. This function has to return a string which will be then
display as tag-info. This string has to be fully formatted \(e.g. must already
include line-breaks if the tag-info should be displayed in several lines).

See `ecb-analyse-show-tag-info-fn' how the tag-info is displayed."
  :group 'ecb-analyse
  :type '(radio (const :tag "Default info" :value nil)
                (function :tag "")))

(defcustom ecb-analyse-show-tag-info-fn 'message
  "*How to display the tag-info for a tag of the analyse-buffer.
The value of this option is a function which will be called with the
info-string generated for the current tag of the analyse-buffer. This function
must do all things necessary for displaying this info. When this function is
called the window stored in `ecb-last-edit-window-with-point' is the selected
window!

ECB offers two builtin ways: Display the info in the echo-area \(via the
function `message') or in a temp-buffer in the edit-area \(via the function
`ecb-analyse-show-tag-info-in-temp-buffer'). Default is echo-area-display.

See also `ecb-analyse-gen-tag-info-fn'."
  :group 'ecb-analyse
  :type '(radio (const :tag "Display in the echo-area" :value message)
                (const :tag "Display in a temp-buffer"
                       :value ecb-analyse-show-tag-info-in-temp-buffer)
                (function :tag "Info display-function")))

(defcustom ecb-analyse-buffer-sync 'basic
  "*Synchronize the analyse buffer automatically with current edit buffer.

If 'always then the synchronization takes place always a buffer changes in the
edit window, if nil then never. If a list of major-modes then only if the
`major-mode' of the new buffer belongs NOT to this list.

Normally it's not necessary to exclude some major-modes because with
not-semantic supported major-modes simply nothing happens. But maybe it can be
helpful for certain situations...

If the special value 'basic is set then ECB uses the setting of the option
`ecb-basic-buffer-sync'.

IMPORTANT NOTE: Every time the synchronization is done the hook
`ecb-analyse-buffer-sync-hook' is evaluated."
    :group 'ecb-analyse
    :type '(radio :tag "Synchronize ECBs analyse buffer"
                  (const :tag "use basic value" :value basic)
                  (const :tag "Always" :value always)
                  (const :tag "Never" nil)
                  (repeat :tag "Not with these modes"
                          (symbol :tag "mode"))))

(defcustom ecb-analyse-buffer-sync-delay 2
  "*Time Emacs must be idle before the analyse-buffer is synchronized.
Synchronizing is done with the current source displayed in the edit window. If
nil then there is no delay, means synchronization takes place immediately.

CAUTION: With analysing a value not too small is strongly recommended because
it can be very annoying if more or less after each typing the current context
is analysed. If set to nil then *each* keyboard hit refreshes the
analyse-buffer which will make ECB quite unusable!

If the special value 'basic is set then ECB uses the setting of the option
`ecb-basic-buffer-sync-delay'"
  :group 'ecb-analyse
  :type '(radio (const :tag "Use basic value" :value basic)
                (const :tag "No synchronizing delay" :value nil)
                (number :tag "Idle time before synchronizing" :value 2))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (ecb-activate-ecb-autocontrol-function
                        value 'ecb-analyse-buffer-sync))))
  :initialize 'custom-initialize-default)

(defcustom ecb-analyse-buffer-sync-hook nil
  "Hook run at the end of `ecb-analyse-buffer-sync'.
See documentation of `ecb-analyse-buffer-sync' for conditions when
synchronization takes place and so in turn these hooks are evaluated.

Preconditions for such a hook:
- Current buffer is the buffer of the currently selected
  edit-window.
- The analyse-buffer is displayed in a visible window of the
  ecb-frame \(so no check for visibilty of the analyse-buffer in
  the ecb-frame is necessary in a hook function)

Postcondition for such a hook:
Point must stay in the same edit-window as before evaluating the hook.

Important note: If the option `ecb-analyse-buffer-sync' is not
nil the function `ecb-analyse-buffer-sync' is running either
every time Emacs is idle or even after every command \(see
`ecb-analyse-buffer-sync-delay'). So if the anaylse-buffer is
displayed in a window of the ecb-frame \(see preconditions above)
these hooks can be really called very often! Therefore each
function of this hook should/must check in an efficient way at
beginning if its task have to be really performed and then do
them only if really necessary! Otherwise performance of Emacs
could slow down dramatically!"
    :group 'ecb-analyse
    :type 'hook)

(defconst ecb-analyse-nodedata-tag-with-pos 0)
(defconst ecb-analyse-nodedata-tag-without-pos 1)
(defconst ecb-analyse-nodedata-no-tag 2)

(defconst ecb-analyse-nodetype-bucket 0)
(defconst ecb-analyse-nodetype-context 1)
(defconst ecb-analyse-nodetype-arguments 2)
(defconst ecb-analyse-nodetype-completions 3)
(defconst ecb-analyse-nodetype-localvars 4)
(defconst ecb-analyse-nodetype-prefix 5)
(defconst ecb-analyse-nodetype-assignee 6)
(defconst ecb-analyse-nodetype-function 7)
(defconst ecb-analyse-nodetype-function-arg 8)

(defecb-autocontrol/sync-function ecb-analyse-buffer-sync
    ecb-analyse-buffer-name ecb-analyse-buffer-sync t
  "Synchronize the analyse buffer with the current buffer and point.
This means in fact display the current analysis for current point."
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: make interruptable. Necessary
  ;; e.g. when typing: "(e" then scanning all elisp stuff beginning with e is
  ;; really annoying....
  (let ((analysis nil)
        (scope nil)
        (completions nil)
        (cnt nil)
        (mode-local-active-mode nil)
        )
    ;; Try and get some sort of analysis
    (condition-case nil
        (progn
          (setq mode-local-active-mode major-mode)
          (save-excursion
            ;; Get the current scope
            (setq scope (semantic-calculate-scope (point)))
            ;; Get the analysis
            (setq analysis (ecb--semantic-analyze-current-context (point)))
            (setq cnt (ecb--semantic-find-tag-by-overlay))
            (when analysis
              (setq completions (ecb--semantic-analyze-possible-completions analysis)))))
      (error nil))
    (ecb-exec-in-window ecb-analyse-buffer-name
      ;; we must remove the old nodes
      (tree-buffer-set-root (tree-node-new-root))
      (when cnt
        (ecb-analyse-add-nodes "Context" "Context"
                               cnt ecb-analyse-nodetype-context))
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should adopt this
      ;; for ecb-analyse..
      ;;     (when analysis
      ;;       ;; If this analyzer happens to point at a complete symbol, then
      ;;       ;; see if we can dig up some documentation for it.
      ;;       (semantic-ia-sb-show-doc analysis))

      ;; Show local variables
      (when scope
        (ecb-analyse-show-scope scope))

      (when analysis
        ;; Let different classes draw more buttons.
        (ecb-analyse-more-nodes analysis)
        (when completions
          (ecb-analyse-add-nodes "Completions" "Completions" completions
                                 ecb-analyse-nodetype-completions)))
      (tree-buffer-update)))
  (run-hooks 'ecb-analyse-buffer-sync-hook))
        
(defun ecb-analyse-show-scope (scope)
  "Show SCOPE information."
  (let ((localvars (when scope
		     (oref scope localvar))))
    (when localvars
      (ecb-analyse-add-nodes "Local Variables" "Local Variables" localvars
                             ecb-analyse-nodetype-localvars))))

(defmethod ecb-analyse-more-nodes ((context semantic-analyze-context))
  "Show a set of ecb-nodes specific to CONTEXT."
  (let ((prefix (oref context prefix)))
    (when prefix
      (ecb-analyse-add-nodes "Prefix" "Prefix" prefix ecb-analyse-nodetype-prefix))))

(defmethod ecb-analyse-more-nodes ((context semantic-analyze-context-assignment))
  "Show a set of ecb-nodes specific to CONTEXT."
  (call-next-method)
  (let ((assignee (oref context assignee)))
    (when assignee
      (ecb-analyse-add-nodes "Assignee" "Assignee" assignee
                             ecb-analyse-nodetype-assignee))))

(defmethod ecb-analyse-more-nodes ((context semantic-analyze-context-functionarg))
  "Show a set of ecb-nodes specific to CONTEXT."
  (call-next-method)
  (let ((func (oref context function)))
    (when func
      (ecb-analyse-add-nodes "Function" "Function" func ecb-analyse-nodetype-function)
      ;; An index for the argument the prefix is in:
      (let ((arg (oref context argument)))
	(when arg
	  (ecb-analyse-add-nodes "Argument #"
                                 (format "Argument # %d" (oref context index))
                                 arg
                                 ecb-analyse-nodetype-function-arg))))))

;; Each category of nodes gets its own nodetype, so we can offer different
;; popup-menus for different categories (e.g. completions have other senseful
;; popup-menu-entries than the rest. The date of a node will always be a cons
;; where the car is the analyse-elem and the cdr is a const if it is a
;; semantic-tag (positionless or with position) or not.

(defun ecb-analyse-add-nodes (bucket bucket-name list nodetype)
  "Create ecb-nodes from LIST. BUCKET is one of the categories/buckets
mentioned in `ecb-analyse-collapsed-buckets'. BUCKET-NAME is the name a bucket
should be displayed with. LIST is a list of tags for this bucket. NODETYPE is
an integer which will be added as type to the nodes created for the elements
of LIST."
  (when list
    (save-excursion
      (set-buffer ecb-analyse-buffer-name)
      (let* ((bucket-name-formatted
              (ecb-merge-face-into-text (ecb-format-bucket-name bucket-name)
                                        ecb-analyse-bucket-node-face))
             (bucket-node (tree-node-new bucket-name-formatted
                                         ecb-analyse-nodetype-bucket
                                         (list 'ecb-bucket-node
                                               ecb-analyse-nodetype-bucket)
                                         nil
                                         (tree-buffer-get-root))))
        (setf (tree-node->expanded bucket-node)
              (not (member bucket ecb-analyse-collapsed-buckets)))
        (ecb-exit-on-input 'ecb-analyse
          (dolist (elem list)
            (ecb-throw-on-input 'ecb-analyse-tree-buffer-build)
            (let* ((fontify-tags (member bucket ecb-analyse-fontified-buckets))
                   (string-1 (typecase elem
                               (string elem)
                               (ecb--semantic-tag
                                (if fontify-tags
                                    (ecb-displayed-tag-name elem)
                                  (ecb--semantic-format-tag-uml-concise-prototype elem)))
                               (otherwise "foo")))
                   (string (concat string-1)))
              (unless fontify-tags
                (ecb-merge-face-into-text string ecb-analyse-bucket-element-face))
              (if (ecb--semantic-tag-p elem)
                  (tree-node-new string nodetype
                                 (list elem 
                                       (if (ecb--semantic-tag-with-position-p elem)
                                           ecb-analyse-nodedata-tag-with-pos
                                         ecb-analyse-nodedata-tag-without-pos)
                                       nodetype)
                                 t bucket-node nil)
                (tree-node-new string nodetype
                               (list elem ecb-analyse-nodedata-no-tag nodetype)
                               t bucket-node nil)))))))))
  
(defun ecb-analyse-compare-node-data (left right)
  "Return not nil when LEFT and RIGHT are identical node-datas."
  (and (equal (nth 2 left) (nth 2 right))
       (ecb-compare-methods-buffer-node-data (car left) (car right))))

(tree-buffer-defpopup-command ecb-analyse-jump-to-tag
  "Jump to the definition of current tag of the analyse-buffer.
If first arg of the REST-ARG-LIST is not nil then it must be a window and then
ECB jumps to that window. If nil then `ecb-last-edit-window-with-point' is
used as window."
  (let ((window (nth 0 rest-arg-list)))
    (when node
      (let* ((data (tree-node->data node))
             (tag (nth 0 data)))
        ;; if we have a positioned tag we jump to it
        (when (and tag (= (nth 1 data) ecb-analyse-nodedata-tag-with-pos))
          ;; We must highlight the tag
          (tree-buffer-highlight-node-by-data/name data)
          ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: what about tags without
          ;; buffer but onlxy with start- and end-pos?!
          (ecb-display-tag (ecb-source-make (ecb-buffer-file-name
                                             (ecb-semantic-tag-buffer tag))
                                            (ecb-semantic-tag-buffer tag))
                           tag
                           (or window (ecb-get-edit-window nil))
                           t nil))))))

(tree-buffer-defpopup-command ecb-analyse-complete/insert
  "Complete/insert at current point the selected completion/localvar."
  ;; We must highlight the tag
  (let* ((data (tree-node->data node))
         (tag (nth 0 data))
         (type (tree-node->type node)))
    (when (or (= type ecb-analyse-nodetype-completions)
              (= type ecb-analyse-nodetype-localvars))
      (tree-buffer-highlight-node-by-data/name data)
      (ecb-display-source ecb-path-selected-source nil)
      (let* ((a (ecb--semantic-analyze-current-context (point)))
             (bounds (if a (oref a bounds)))
             (movepoint nil))
        (if (null bounds)
            (insert (ecb--semantic-tag-name tag))
          (save-excursion
            (if (and (<= (point) (cdr bounds)) (>= (point) (car bounds)))
                (setq movepoint t))
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert (ecb--semantic-tag-name tag))
            (if movepoint (setq movepoint (point))))
          (if movepoint
              (goto-char movepoint)))))))

(defecb-tree-buffer-callback ecb-analyse-node-clicked ecb-analyse-buffer-name select nil
  "Handles clicking onto any of the nodes in the analyse-buffer of ECB."
  (if shift-mode
      (ecb-mouse-over-analyse-node node nil nil 'force))
  (let* ((data (tree-node->data node))
         (tag (nth 0 data))
         (type (tree-node->type node)))
    ;; we handle hiding the ecb-windows for ourself
    (setq no-meta-hiding t)
    (cond
     ((= type ecb-analyse-nodetype-bucket)
      (tree-node-toggle-expanded node)
      (tree-buffer-update node))
     ((= type ecb-analyse-nodetype-completions)
      (ecb-analyse-complete/insert node))
     ((= type ecb-analyse-nodetype-localvars)
      (ecb-analyse-complete/insert node))
     (t
      (ecb-analyse-jump-to-tag node (ecb-get-edit-window
                                     ;; `ecb-analyse-jump-to-tag' expects all
                                     ;; args beyond NODE as one list.
                                     `(,(ecb-combine-ecb-button/edit-win-nr
                                         ecb-button edit-window-nr))))
      (when (and tag (= (nth 1 data) ecb-analyse-nodedata-tag-with-pos))
        (when meta-mode
          (ecb-run-with-idle-timer 0.001 nil 'ecb-hide-ecb-windows)))))))

(defecb-window-dedicator ecb-set-analyse-buffer ecb-analyse-buffer-name
  "Display the analyse buffer in current window and make window dedicated."
  (ecb-activate-ecb-autocontrol-function ecb-analyse-buffer-sync-delay
                                         'ecb-analyse-buffer-sync)
  (switch-to-buffer ecb-analyse-buffer-name))

(defun ecb-maximize-window-analyse ()
  "Maximize the ECB-analyse-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-analyse-window is not visible in current layout."
  (interactive)
  (ecb-maximize-ecb-buffer ecb-analyse-buffer-name t))

(defun ecb-goto-window-analyse ()
  "Make the ECB-analyse window the current window."
  (interactive)
  (ecb-goto-ecb-window ecb-analyse-buffer-name))

(defun ecb-analyse-show-tag-info-in-temp-buffer (info-string)
  "Display INFO-STRING in a temp-buffer in the edit-area." 
  (with-output-to-temp-buffer "*Tag Information*"
    (save-excursion
      (set-buffer "*Tag Information*")
      (insert info-string)))
  ;; Make it small
  (shrink-window-if-larger-than-buffer
   (get-buffer-window "*Tag Information*")))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: we could use the function
;; `semantic-documentation-for-tag' here to display more infos?!

(defun ecb-analyse-gen-tag-info (tag)
  "Return the info-string for TAG."
  (or (and (functionp ecb-analyse-gen-tag-info-fn)
           (or (funcall ecb-analyse-gen-tag-info-fn tag)
               (format "No info generated by `%s'." ecb-analyse-gen-tag-info-fn)))
      (concat (ecb-displayed-tag-name tag)
              "\n"
              (let ((typetag
                     (condition-case nil
                         (save-excursion
                           (ecb--semantic-analyze-tag-type tag))
                       (error nil))))
                (if typetag
                    (ecb-displayed-tag-name typetag)
                  ;; No type found by the analyzer The below used
                  ;; to try and select the buffer from the last
                  ;; analysis, but since we are already in the
                  ;; correct buffer, I don't think that is needed.
                  (when (fboundp 'semantic-lex-keyword-p)
                    (let ((type (ecb--semantic-tag-type tag)))
                      (typecase type
                        (ecb--semantic-tag
                         (setq type (ecb--semantic-tag-name type)))
                        (list
                         (setq type (car type))))
                      (if (semantic-lex-keyword-p type)
                          (setq typetag
                                (semantic-lex-keyword-get type 'summary))))
                    (if typetag
                        typetag))
                  )))))


(tree-buffer-defpopup-command ecb-analyse-show-tag-info
  "Display as much information as possible about current tag.
Show the information in a shrunk split-buffer and expand out as many details
as possible."
  (let* ((data (tree-node->data node))
         (tag (car data)))
    (when (ecb--semantic-tag-p tag)
      (save-selected-window
        (select-window ecb-last-edit-window-with-point)
        (funcall ecb-analyse-show-tag-info-fn (ecb-analyse-gen-tag-info tag))))))

(defun ecb-mouse-over-analyse-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the analyse buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-analyse-show-node-info'. NODE is the node for which help text should be
displayed, WINDOW is the related window, NO-MESSAGE defines if the help-text
should be printed here."
  (let ((str (when (or click-force
                       (ecb-show-minibuffer-info node window
                                                 (car
                                                 ecb-analyse-show-node-info)))
               (if (equal (cdr ecb-analyse-show-node-info) 'full-info)
                   (ecb-analyse-gen-tag-info (car (tree-node->data node)))
                 (tree-node->name node)))))
    (prog1 str
      (unless no-message
        (ecb-nolog-message str)))))

(defun ecb-analyse-node-mouse-highlighted-p (node)
  "Return not nil when NODE has a positioned tag as data or belongs to the
completions. This means that this node should be highlighted when mouse is
moved over it."
  (or (equal ecb-analyse-nodedata-tag-with-pos
             (nth 1 (tree-node->data node)))
      (member (tree-node->type node)
              (list ecb-analyse-nodetype-completions
                    ecb-analyse-nodetype-localvars))))

(defun ecb-analyse-create-menu (node)
  "Return a popup-menu suitable for NODE."
  (let* ((data (tree-node->data node))
         (tag-p (not (equal (nth 1 data) ecb-analyse-nodedata-no-tag)))
         (tag-with-pos-p (equal (nth 1 data) ecb-analyse-nodedata-tag-with-pos))
         (nodetype (nth 2 data)))
    (delq nil (list (if (member nodetype (list
                                          ecb-analyse-nodetype-completions
                                          ecb-analyse-nodetype-localvars))
                        '(ecb-analyse-complete/insert "Complete/insert"))
                    (if tag-p 
                        '(ecb-analyse-show-tag-info "Show tag info"))
                    (if tag-with-pos-p
                        '(ecb-analyse-jump-to-tag "Jump to tag"))))))
    
(defun ecb-analyse-menu-creator (tree-buffer-name node)
  "Creates the popup-menus for the analyse-buffer."
  (setq ecb-layout-prevent-handle-ecb-window-selection t)
  (let ((nodetype (tree-node->type node)))
    (unless (equal nodetype ecb-analyse-nodetype-bucket)
      (mapcar (function (lambda (type)
                          (cons type (ecb-analyse-create-menu node))))
              `(,ecb-analyse-nodetype-context
                ,ecb-analyse-nodetype-arguments
                ,ecb-analyse-nodetype-completions
                ,ecb-analyse-nodetype-localvars
                ,ecb-analyse-nodetype-prefix
                ,ecb-analyse-nodetype-assignee
                ,ecb-analyse-nodetype-function
                ,ecb-analyse-nodetype-function-arg)))))


(defun ecb-analyse-gen-menu-title-creator ()
  "Returns a menu-title-create-function for the nodetypes of the
analyse-buffer."
  (mapcar (function (lambda (nodetype)
                      (cons nodetype
                            (function (lambda (node)
                                        (tree-node->name node))))))
          `(,ecb-analyse-nodetype-context
            ,ecb-analyse-nodetype-arguments
            ,ecb-analyse-nodetype-completions
            ,ecb-analyse-nodetype-localvars
            ,ecb-analyse-nodetype-prefix
            ,ecb-analyse-nodetype-assignee
            ,ecb-analyse-nodetype-function
            ,ecb-analyse-nodetype-function-arg)))


(defecb-tree-buffer-creator ecb-create-analyse-tree-buffer ecb-analyse-buffer-name
  "Create the tree-buffer for analyse-display."
  (tree-buffer-create
   ecb-analyse-buffer-name
   :frame ecb-frame
   :mouse-action-trigger ecb-tree-mouse-action-trigger
   :is-click-valid-fn 'ecb-interpret-mouse-click
   :node-selected-fn 'ecb-tree-buffer-node-select-callback
   :node-expanded-fn 'ecb-tree-buffer-node-expand-callback
   :node-collapsed-fn 'ecb-tree-buffer-node-collapsed-callback
   :node-mouse-over-fn 'ecb-mouse-over-analyse-node
   :mouse-highlight-fn 'ecb-analyse-node-mouse-highlighted-p
   :node-data-equal-fn 'ecb-analyse-compare-node-data
   :maybe-empty-node-types nil
   :leaf-node-types nil
   :menu-creator 'ecb-analyse-menu-creator
   :menu-titles (ecb-analyse-gen-menu-title-creator)
   :modeline-menu-creator 'ecb-common-tree-buffer-modeline-menu-creator
   :sticky-parent-p ecb-tree-make-parent-node-sticky
   :sticky-indent-string ecb-tree-stickynode-indent-string
   :sticky-parent-fn nil
   :trunc-lines (ecb-member-of-symbol/value-list ecb-analyse-buffer-name
                                                 ecb-tree-truncate-lines)
   :read-only t
   :tree-indent ecb-tree-indent
   :incr-search-p nil ;; ecb-tree-incremental-search
   :incr-search-additional-pattern nil ;; ecb-methods-incr-searchpattern-node-prefix
   :arrow-navigation ecb-tree-navigation-by-arrow
   :hor-scroll-step ecb-tree-easy-hor-scroll
   :default-images-dir (car ecb-tree-image-icons-directories)
   :additional-images-dir (ecb-member-of-symbol/value-list ecb-analyse-buffer-name
                                                           (cdr ecb-tree-image-icons-directories)
                                                           'car 'cdr)
   :image-file-prefix "ecb-"
   :tree-style ecb-tree-buffer-style
   :ascii-guide-face ecb-tree-guide-line-face
   :type-facer nil
   :expand-symbol-before-p ecb-tree-expand-symbol-before
   :highlight-node-face ecb-analyse-face
   :general-face ecb-analyse-general-face
   :after-create-hook (append
                       (list (function (lambda ()
                                         (ecb-common-after-tree-buffer-create-actions))))
                       ecb-common-tree-buffer-after-create-hook
                       ecb-analyse-buffer-after-create-hook)
   :after-update-hook nil))



(silentcomp-provide 'ecb-analyse)

;;; ecb-anaylse.el ends here

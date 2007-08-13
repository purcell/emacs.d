;;; semantic-tag-folding.el --- semantic decoration style to enable folding of semantic tags
;; Time-stamp: <2005-04-28 17:03:44 sacharya>

;;; Copyright (C) 2005 Suraj Acharya

;; Author: Suraj Acharya <sacharya@cs.indiana.edu>

;; This file is not part of GNU Emacs.

;; semantic-tag-folding.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.:

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;;
;;; Defines a `semantic-decoration-mode' style which allows semantic
;;; tags to be expanded or collapsed in the style of folding mode and
;;; hideshow mode.  In addition to regular semantic tag, comments
;;; preceeding tags can also be folded, and consecutive 'include tags
;;; are folded as a single unit.  A semantic minor mode
;;; `semantic-tag-folding-mode' is also created.  So M-x
;;; semantic-tag-folding-mode can be used to turn this mode on and
;;; off, it will also turn on `semantic-deocration-mode' if required.
;;;
;;; To use feature, add this file to your load path and put the
;;; following line in your .emacs: (require 'semantic-tag-folding)
;;; 
;;; Customize `semantic-tag-folding-allow-folding-of' to choose which
;;; tags you want to be able to fold.  You can also choose which tags
;;; types are folded by default when semantic-decoration-mode is
;;; enabled.
;;;
;;; M-x semantic-tag-folding-mode to enable tag folding in a buffer,
;;; M-x global-semantic-tag-folding-mode turns on folding in all
;;; semantic enabled buffers.
;;;
;; Features:
;; 1. Indicators in the fringe to show which tags which can be
;;    expanded or hidden, clicking on the fringe symbols toggles the
;;    associated block's state
;; 2. `semantic-tag-folding-allow-folding-of' lets you customize which
;;    tags can be folded, and which of those tags are folded by
;;    default when this mode is first activated.
;; 3. semantic-tag-folding-mode which toggles this mode, without
;;    having to turn on semantic-decoration-mode
;; 4. hs-mode style commands to fold and show all tags, all child tags
;;    or only the current tag.
;;    
;; TODO:
;; * semantic-tag-folding-tag and semantic-tag-folding-comment attributes should be ignored
;; when calling fold-all or show-all (or the yet to be implemented show-children functions)
;; * make tooltips behave well (turn them off when the region is
;; expanded, consistent location, no truncation, update tooltips when
;; the text changes)
;; * make the ellipsis clickable
;; * investigate occasional windows cvs Emacs crashes



(require 'semantic-decorate-mode)
(require 'cl)

;;; Code:

;; xemacs compatibility
;; http://www.opensource.apple.com/darwinsource/10.3/emacs-56/emacs/lisp/progmodes/hideshow.el
(when (or (not (fboundp 'add-to-invisibility-spec))
          (not (fboundp 'remove-from-invisibility-spec)))
  ;; `buffer-invisibility-spec' mutators snarfed from Emacs 20.3 lisp/subr.el
  (defun add-to-invisibility-spec (arg)
    (cond
     ((or (null buffer-invisibility-spec) (eq buffer-invisibility-spec t))
      (setq buffer-invisibility-spec (list arg)))
     (t
      (setq buffer-invisibility-spec
            (cons arg buffer-invisibility-spec)))))
  (defun remove-from-invisibility-spec (arg)
    (when buffer-invisibility-spec
      (setq buffer-invisibility-spec
            (delete arg buffer-invisibility-spec)))))

;; http://list-archive.xemacs.org/xemacs-patches/200206/msg00144.html
;; `propertize' is a builtin in GNU Emacs 21.
(when (not (fboundp 'propertize))
  (defun propertize (string &rest properties)
    "Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result."
    (let ((str (copy-sequence string)))
      (add-text-properties 0 (length str)
                           properties
                           str)
      str)))

;;;###autoload
(defcustom global-semantic-tag-folding-mode nil
  "*If non-nil enable global use of variable `semantic-tag-folding-mode'.
With this mode enabled, a new folding decoration mode is added.
Clicking on a + or - in the fringe will fold that tag."
  :group 'semantic
  :group 'semantic-modes
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-tag-folding-mode (if val 1 -1))))

;;;###autoload
(defun global-semantic-tag-folding-mode (&optional arg)
  "Toggle global use of option `semantic-tag-folding-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-tag-folding-mode
        (semantic-toggle-minor-mode-globally
         'semantic-tag-folding-mode arg)))

(defcustom semantic-tag-folding-mode-hook nil
  "*Hook run at the end of function `semantic-tag-folding-mode'."
  :group 'semantic
  :type 'hook)
  
(defvar semantic-tag-folding-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [left-fringe mouse-1] 'semantic-tag-folding-click)
    km)
  "Keymap for folding minor mode.")

(defvar semantic-tag-folding-mode nil
  "Non-nil if folding minor mode is enabled.
Use the command `semantic-tag-folding-mode' to change this variable.")

(make-variable-buffer-local 'semantic-tag-folding-mode)

(defvar semantic-tag-folding-decoration-mode-hook-enabled t
  "Used to disable `semantic-tag-folding-decoration-mode-hook'.
This is done when semantic-tag-folding mode turns on semantic-decoration mode.")


(defvar semantic-tag-folding-saved-decoration-styles nil
"The saved value of `semantic-decoration-styles'.")
(make-variable-buffer-local 'semantic-tag-folding-saved-decoration-styles)

(defvar semantic-tag-folding-decoration-style
  '(("semantic-tag-folding" . t))
  "Only turn on semantic-tag-folding decorations.
A value for variable `semantic-decoration-styles'.")

(defun semantic-tag-folding-mode-setup ()
  "Setup option `semantic-tag-folding-mode'.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  In addition,
`semantic-tag-folding-mode' is only available when fringe images are available
in Emacs 20.4."
  (if semantic-tag-folding-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)
                    ))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-tag-folding-mode nil)
            (error "Buffer %s cannot be folded by semantic"
                   (buffer-name)))
        ;; Enable decoration mode
        (add-to-invisibility-spec '(semantic-tag-fold . t))
        (if (featurep 'xemacs)
            (set (make-local-variable 'line-move-ignore-invisible) t))
        (setq semantic-tag-folding-saved-decoration-styles semantic-decoration-styles)
        (if semantic-decoration-mode
            ;; if decoration mode is already on, ensure that semantic-tag-folding is enabled
            (let ((style (assoc "semantic-tag-folding" semantic-decoration-styles)))
              (when (not (cdr style))
                (setcdr style t)
                (semantic-decoration-mode-setup)))
          ;; else, turn on decoration mode with only semantic-tag-folding on
          (setq semantic-tag-folding-saved-decoration-styles semantic-decoration-styles)
          (setq semantic-decoration-styles semantic-tag-folding-decoration-style)
          (let ((semantic-tag-folding-decoration-mode-hook-enabled nil))
            (semantic-decoration-mode 1))))
    ;; Remove hooks
    ;; Disable the decoration.
    (when semantic-decoration-mode
      (if (eq semantic-decoration-styles semantic-tag-folding-decoration-style)
          ;; if no calls were made to
          (progn     (semantic-decoration-mode -1)
                     (setq  semantic-decoration-styles semantic-tag-folding-saved-decoration-styles))
        ;; else
        (setq  semantic-decoration-styles semantic-tag-folding-saved-decoration-styles)
        (semantic-decoration-mode 1))))
  semantic-tag-folding-mode)

(add-hook 'semantic-decoration-mode-hook 'semantic-tag-folding-decoration-mode-hook)

(defun semantic-tag-folding-decoration-mode-hook ()
  "Hook function used to manage folding icons in decoration-mode."
  (when semantic-tag-folding-decoration-mode-hook-enabled
    (cond
     ((and semantic-decoration-mode semantic-tag-folding-mode)
      ;; when turning on decoration-mode with tag folding already on,
      ;; use the saved value of `semantic-decoration-styles' and ensure
      ;; that tag folding decorations are turned on
      (setq semantic-decoration-styles semantic-tag-folding-saved-decoration-styles)
      (let ((style (assoc "semantic-tag-folding" semantic-decoration-styles)))
        (when (not (cdr style))
          (setcdr style t)
          (semantic-decoration-mode-setup)
          )))
     ((and semantic-decoration-mode (not semantic-tag-folding-mode))
      ;; when turning on decorations with out tag folding, ensure that
      ;; tag-folding decorations are not enabled
      (let ((style (assoc "semantic-tag-folding" semantic-decoration-styles)))
        (when (cdr style)
          (setcdr style nil)
          (semantic-decoration-mode-setup)
          )))
     ((and (not semantic-decoration-mode) semantic-tag-folding-mode)
      ;; if turning off decoration mode with semantic tag folding on,
      ;; turn off semantic tag foldng mode
      (if (eq semantic-decoration-styles semantic-tag-folding-decoration-style)
          ;; M-x tag-folding -> M-x decoration , turn on all the deocration mode styles
          (semantic-decoration-mode 1)
        ;; M-x tag-folding -> M-x decoration M-x decoration, only keep
        ;; the semantic-tag-folding-decoration-style active
       (semantic-tag-folding-mode 1))))))

;;;###autoload
(defun semantic-tag-folding-mode (&optional arg)
  "Minor mode mark semantic tags for folding.
This mode will display +/- icons in the fringe.  Clicking on them
will fold the current tag.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (interactive
   (list (or current-prefix-arg
             (if semantic-tag-folding-mode 0 1))))
  (setq semantic-tag-folding-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-tag-folding-mode)))
  (semantic-tag-folding-mode-setup)
  (run-hooks 'semantic-tag-folding-mode-hook)
  (if (interactive-p)
      (message "folding minor mode %sabled"
               (if semantic-tag-folding-mode "en" "dis")))
  semantic-tag-folding-mode)

(semantic-add-minor-mode 'semantic-tag-folding-mode "" semantic-tag-folding-mode-map)


(define-semantic-decoration-style semantic-tag-folding "Enables folding of tags.")

;; this  needs  to  go  after  defining the  decoration  style,  until
;; define-semantic-decoration-style   uses  setq-default   instead  of
;; add-to-list when setting the value of semantic-decoration-styles
(make-variable-buffer-local 'semantic-decoration-styles)


(defcustom semantic-tag-folding-allow-folding-of
  '((type . nil) (function . nil) (variable . nil) (include . nil)
    (comment . nil) (package . nil))
  "A set of semantic classes.  Tags of these classes will be allowed to be folded and unfolded by this mode."
  :group 'semantic
  :type ;;    '(alist    :key-type  symbol   :value-type boolean :options (type function variable include package code))
  '(set (cons :format "%v" (const :tag "Types" type)
	      (choice :tag "Fold by default"
		      (const :tag "Outer type(s) as well as inner types" all)
		      (const :tag "Only inner types" inner)
		      (const :tag "Neither" )))
        (cons :format "%v"
	      (const :tag "Function/method declarations" function)
	      (boolean :tag "Fold by default"))
        (cons :format "%v"
	      (const :tag "Varible declarations" variable)
	      (boolean :tag "Fold by default"))
        (cons :format "%v"
	      (const :tag "Blocks of consecutive include/import statements" include)
	      (boolean :tag "Fold by default"))
        (cons :format "%v"
	      (const :tag "Comment blocks preceeding tags" comment)
	      (boolean :tag "Fold by default"))
        (cons :format "%v"
	      (const :tag "Package declarations" package)
	      (boolean :tag "Fold by default"))
        (cons :format "%v"
	      (const :tag "Code regions" code)
	      (boolean :tag "Fold by default"))
        (cons :format "%v"
	      (const :tag "Code regions" block)
	      (boolean :tag "Fold by default"))
        (repeat :tag "Other Semantic classes"
		(cons :format "%v" (symbol :tag "Semantic class" code)
		      (boolean :tag "Fold by default")))
	))
(make-variable-buffer-local 'semantic-tag-folding-allow-folding-of)

(defcustom semantic-tag-folding-tag-higlight-time 1
  "The time in seconds for which a fringe highlight appears.
This higlight shows extent of the tag body when a tag is
expanded.  Set this to nil for no extent indication."
  :group 'semantic :type 'number)

(defcustom semantic-tag-folding-highlight-tags-shown-by-reveal-mode nil
  "If non-nil the extent tags unfolded by reveal mode is not highlighted."
  :group 'semantic
  :type 'boolean)

(defcustom semantic-tag-folding-show-tooltips nil
  "Display tooltips for folded tag bodies..
If set to t, the body of a hidden tag is shown as a tooltip
  when the mouse hovers over the first line of the tag.  This is
  not very pretty because the tooltip sometimes appears above the
  cursor and not below where the tag body is, and the tootltip
  text is truncated at some limit so large tag bodies are often
  cut short."
  :group 'semantic :type 'boolean)

(defvar semantic-tag-folding-function 'semantic-tag-folding-function-default
  "Default folding of tags.
Function which determines whether a tag should be folded by
default when `semantic-tag-folding' is activated."  )

(defun semantic-tag-folding-set-fringe-image-style  (&optional symbol value)
  "Set the bitmaps for this folding \"fringe style\".
This function is called when customizing
`semantic-tag-folding-fringe-image-style'. SYMBOL is
`semantic-tag-folding-fringe-image-style' and VALUE is the fringe
style selected.
Five bitmaps are needed for each style:
* semantic-tag-folding-folded - the image in the fringe which
indicates that there is a folded tag on this line
* semantic-tag-folding-unfolded - this image indicates that the
tag starting on this line can be folded
* semantic-tag-folding-highlight-{top,middle,bottom} - when
`semantic-tag-folding-tag-higlight-time' is non-nil these three
bitmaps are used to indicate the extent of a tag when it is
unfolded."
  (if symbol (set-default symbol value))
  (cond
   ((not (functionp 'define-fringe-bitmap)) nil)
   ((eq value 'plusminus)

    (define-fringe-bitmap  'semantic-tag-folding-folded
      ;; a plus sign
      [#b00011000
       #b00011000
       #b00011000
       #b11111111
       #b11111111
       #b00011000
       #b00011000
       #b00011000])
    
    (define-fringe-bitmap  'semantic-tag-folding-unfolded
      ;; a minus sign
      [#b11111111
       #b11111111])

    (define-fringe-bitmap  'semantic-tag-folding-highlight-top
      ;; a minus sign
      [#b11111111
       #b11111111])

    (define-fringe-bitmap  'semantic-tag-folding-highlight-middle
      ;; a vertical bar
      [#b00011000] nil nil '(center t))

    (define-fringe-bitmap  'semantic-tag-folding-highlight-bottom
      ;; a minus sign
      [#b11111111
       #b11111111]))

   ((eq value 'triangles)

    (define-fringe-bitmap  'semantic-tag-folding-unfolded
      ;; a triangle pointing downwards
      [#b11111110
       #b01000100
       #b00101000
       #b00010000])

    (define-fringe-bitmap  'semantic-tag-folding-folded
      ;; a filled triangle pointing to the right
      [#b100000
       #b110000
       #b111000
       #b111100
       #b111100
       #b111000
       #b110000
       #b100000])

    (define-fringe-bitmap  'semantic-tag-folding-highlight-top
      ;; a triangle pointing downwards
      [#b11111110
       #b01000100
       #b00101000
       #b00010000])

    (define-fringe-bitmap  'semantic-tag-folding-highlight-middle
      ;; a vertical bar
      [#b00010000] nil nil '(center t))

    (define-fringe-bitmap  'semantic-tag-folding-highlight-bottom
      ;; a triangle pointing upwards
      [#b00010000
       #b00101000
       #b01000100
       #b11111110])
    )
   ))

(defcustom semantic-tag-folding-fringe-image-style 'triangles
  "Fringe image style.
This variable determines the bitmaps drawn in the fringe to
  indicate folded or unfolded (expanded) tags."
  :group 'semantic
  :type '(choice (const triangles)
                 (const plusminus))
  :set 'semantic-tag-folding-set-fringe-image-style)

(defun semantic-tag-folding-allow-folding-of (class)
  "Is folding of tags of semantic class CLASS allowed?"
  (or
   (assq class semantic-tag-folding-allow-folding-of)
   (assq class (car (last semantic-tag-folding-allow-folding-of)))
   ))

(defun semantic-tag-folding-hidden-by-default (class)
"Are tags of semantic class CLASS to be hidden by default?"
  (cdr (semantic-tag-folding-allow-folding-of class)))

(defun semantic-tag-folding-function-default (tag comment)
  "The default `semantic-tag-folding-function'.
Returns non-nil if the body of TAG is to be hidden when the mode
is started.  COMMENT is non-nil to indicate that the comment above
TAG is what is being hidden, not the body of TAG."
  (if comment
      (semantic-tag-folding-hidden-by-default 'comment)
    (let* ((c (semantic-tag-class tag))
          (default (semantic-tag-folding-hidden-by-default c)))
      ;; `default' is the value to be returned, unless TAG is a type
      ;; and only inner types are to  be hidden
      (if (and default (eq c 'type) (eq default 'inner))
          ;; the outermost type has no parent
          (semantic-find-tag-parent-by-overlay tag)
        default))))

(defun semantic-tag-folding-p-default (tag)
  "Return non-nil if TAG is to be considered for folding.
TAG has to have valid start and end locations in the
buffer.  Customize variable `semantic-tag-folding-allow-folding-of' to
influence the output of this function."
  (let ((c (semantic-tag-class tag)))
    (and
     (semantic-tag-with-position-p tag)
     (or (semantic-tag-folding-allow-folding-of 'comment)
	 (semantic-tag-folding-allow-folding-of c))
     ;; we only want the first include from a block of includes
     (or (not (eq c 'include))
         (not (semantic-find-tag-by-overlay-prev (semantic-tag-start tag)))
         (not (eq (semantic-tag-class
		   (semantic-find-tag-by-overlay-prev (semantic-tag-start tag))) 'include)))
     )))

(defun semantic-tag-folding-highlight-default (tag)
  "Create decoration overlays for TAG.
Also put a marker in the fringe for each thing that can be
collapsed."
  (when (semantic-tag-buffer tag)
    (with-current-buffer (semantic-tag-buffer tag)
      (let ((point (point))
            (tag-start (semantic-tag-start tag))
            (tag-end (semantic-tag-end tag)))
        ;; fold the comment preceding this tag
        (if (semantic-tag-folding-allow-folding-of 'comment)
            (let ((start (progn
                           (goto-char tag-start)
                           (when (forward-comment -1)
                             (do ((ret (point-at-eol) (point-at-eol)))
                                 ( ;; until we see an empty line, or there are
                                  ;; no more comments, or we reach the
                                  ;; beginning of the buffer
                                  (or (re-search-backward "\n\n" (- (point) 2) t)
                                      (not (forward-comment -1))
                                      (bobp))
                                  ;; return
                                  ret)))))
                  (end (progn
                         (goto-char tag-start)
                         (- (point-at-bol) 1))))
              (semantic-tag-folding-create-folding-overlays tag start end point t)))
        ;; Fold the body of this tag.
        ;; If folding comments is enabled all tags are passed into this
        ;; function, so we need to check if folding is enabled for this
        ;; tag type
        (if (or (not (semantic-tag-folding-allow-folding-of 'comment)) (semantic-tag-folding-allow-folding-of (semantic-tag-class tag)))
            (let ((start (progn
                           (goto-char tag-start)
                           (point-at-eol)))
                  (end (if (eq (semantic-tag-class tag) 'include)
                           (progn
                             (let ((tag-cursor tag) (last-tag-cursor tag))
                               (while (eq (semantic-tag-class tag-cursor) 'include)
                                 (setq last-tag-cursor tag-cursor)
                                 (setq tag-cursor (semantic-find-tag-by-overlay-next (semantic-tag-end tag-cursor))))
                               (semantic-tag-end last-tag-cursor)))
                         tag-end)))
              (semantic-tag-folding-create-folding-overlays tag start end point nil)))
        (goto-char point)))))


(defun semantic-tag-folding-get-attribute-overlay (tag create-if-null)
  "Get the overlay used to store the fold state for TAG.
Create the overlay if CREATE-IF-NULL is non-nil."
  (let* ((pos (semantic-tag-start tag))
        (ov (car (remove-if-not
                 (lambda (ov)
                   (semantic-overlay-get ov 'semantic-tag-folding-attributes))
                 (semantic-overlays-at pos)))))
    (when (and create-if-null (null ov))
      (setq ov (semantic-make-overlay (- pos 1) (+ 1 pos)))
      (semantic-overlay-put ov 'semantic-tag-folding-attributes t))
    ov))

(defun semantic-tag-folding-get-folding-attribute (comment)
  "Return the symbol used to store the fold state.
The symbol returned is for a tag (COMMENT is nil) or the comment
preceeding a tag (COMMENT is non-nil)"
  (if comment
      'semantic-tag-folding-comment
    'semantic-tag-folding-tag))

(defun semantic-tag-folding-get-fold-state (tag comment)
  "Return the fold state for TAG.
If COMMENT is non-nil return the fold state for the comment preceeding TAG."
  (let* ((attr (semantic-tag-folding-get-folding-attribute comment))
         (ov (semantic-tag-folding-get-attribute-overlay tag nil)))
    (and ov (semantic-overlay-get ov attr))))

(defun semantic-tag-folding-set-fold-state (tag comment state)
  "Set the fold state for TAG to STATE.
If COMMENT is non-nil set the fold state for the comment preceeding TAG."
  (let* ((attr (semantic-tag-folding-get-folding-attribute comment))
         (ov (semantic-tag-folding-get-attribute-overlay tag t)))
    (semantic-overlay-put ov attr state)))


(defun semantic-tag-folding-create-folding-overlays (tag start end point comment)
"Create an overlay for `semantic-tag-overlay'.
Create an overlay associated TAG.  START and END are buffer
positions, usually inside TAG, but can be outside for comment and
include block overlays.  POINT is the saved location of point,
this is used to unfold any TAGS around point by default.  COMMENT
is non-nil if the fold region is a comment."
  (let ((fold (if (functionp semantic-tag-folding-function)
                  (apply semantic-tag-folding-function (list tag comment))
                semantic-tag-folding-function)))
    (when (and start end (< start end) (> (count-lines start end) 1))
      (let* ((ov (semantic-decorate-tag tag start end))
             (start2 (if comment
                         (save-excursion
                           (goto-char start)
                           (backward-char)
                           (point-at-bol))
                       (semantic-tag-start tag)))
             (ov2 (semantic-decorate-tag tag start2 (+ start2 1)))
             (marker-string "+"))
        (semantic-overlay-put ov 'semantic-tag-folding t)
        (semantic-overlay-put ov 'isearch-open-invisible
		     'semantic-tag-folding-show-block)

        ;; check for fold state attributes
        (if (functionp semantic-tag-folding-function)
            (let ((state (semantic-tag-folding-get-fold-state tag comment)))
              (if state
                  (setq fold (eq state 'fold)))))

        ;; don't fold this region if point is inside it
        (if (and (> end point) (< start point))
            (setq fold nil))

        (if (not fold)
            ;; just display the unfolded bitmap in the fringe
            (setq marker-string (propertize
				 marker-string 'display
				 '((left-fringe semantic-tag-folding-unfolded)
				   "-")))
          ;; fold the body and display a + in the fringe
          (semantic-overlay-put ov 'invisible 'semantic-tag-fold)
          (setq marker-string (propertize
			       marker-string
			       'display
			       '((left-fringe semantic-tag-folding-folded)
				 "+" ))))
        
        ;; store the marker string and tag as a property of the
        ;; overlay so we use it to change the displayed fold state
        ;; later (in semantic-tag-folding-set-overlay-visibility)
        (semantic-overlay-put ov 'semantic-tag-folding-marker-string marker-string)
        (semantic-overlay-put ov 'semantic-tag-folding-tag tag)
        (semantic-overlay-put ov 'semantic-tag-folding-comment-overlay comment)

        (semantic-overlay-put ov2 'before-string marker-string)
        
        ;; store fold state as a function of the tag (unless the default state is being set)
        (unless (functionp semantic-tag-folding-function)
          (semantic-tag-folding-set-fold-state tag comment fold))

        ;; tooltips
        (when semantic-tag-folding-show-tooltips
          (semantic-overlay-put ov2 'mouse-face 'highlight)
          (semantic-overlay-put ov2 'help-echo (buffer-substring (+ 1 start) end)))))))

(defun semantic-tag-folding-fold-block ()
  "Fold the smallest enclosing tag at point."
  (interactive)
 (semantic-tag-folding-set-overlay-visibility
  (semantic-tag-folding-get-overlay) t))

(defun semantic-tag-folding-show-block (&optional ov)
  "Unfold overlay OV, or the smallest enclosing tag at point."
  (interactive)
  (semantic-tag-folding-set-overlay-visibility
   (or ov (semantic-tag-folding-get-overlay)) nil))

(defun semantic-tag-folding-show-all ()
  "Unfold all the tags in this buffer."
  (interactive)
    (semantic-tag-folding-fold-or-show-tags
     (semantic-fetch-available-tags) nil))

(defun semantic-tag-folding-fold-all ()
  "Fold all the tags in this buffer."
  (interactive)
    (semantic-tag-folding-fold-or-show-tags
     (semantic-fetch-available-tags) t))

(defun semantic-tag-folding-show-children ()
  "Unfold all the tags in this buffer."
  (interactive)
  (semantic-tag-folding-fold-or-show-tags
   (cons (semantic-current-tag)
	 (semantic-tag-components (semantic-current-tag)))
   nil))

(defun semantic-tag-folding-fold-children ()
  "Unfold all the tags in this buffer."
  (interactive)
  (semantic-tag-folding-fold-or-show-tags
   (cons (semantic-current-tag)
	 (semantic-tag-components (semantic-current-tag)))
   t))

(defun semantic-tag-folding-fold-or-show-tags (tags fold)
"Change the fold state of TAGS to FOLD."
  (lexical-let ((fold fold))
    (when semantic-decoration-mode
      (semantic-decorate-clear-decorations tags)
      (let ((semantic-tag-folding-function fold))
        (semantic-decorate-add-decorations tags)))))

(defun semantic-tag-folding-get-overlay ()
  "Return the innermost semantic-tag-folding-folding overlay at point."
  (labels ((semantic-overlay-size (ov)
             (- (semantic-overlay-end ov) (semantic-overlay-start ov))))
    (car
     (sort
      (remove-if-not (lambda (ov) (semantic-overlay-get ov 'semantic-tag-folding))
                     (semantic-overlays-at (point-at-eol)))
      (lambda (x y)
        (< (semantic-overlay-size x) (semantic-overlay-size y)))))))

(defun semantic-tag-folding-set-overlay-visibility (ov fold &optional called-by-reveal-mode)
  "Change the visibility of overlay OV.
If FOLD is non-nil OV is hidden.  Also changes the fringe bitmap
to indcate the new state.  CALLED-BY-REVEAL-MODE is t when this
overlay is folded or expanded by reveal mode."
  (when (and (semantic-overlay-p ov)
             ;; if reveal mode is hiding an overlay, it should've been folded by reveal mode
             (or (not called-by-reveal-mode) (not fold) (semantic-overlay-get ov 'semantic-tag-reveal-mode)))
    (semantic-overlay-put ov 'invisible (if fold 'semantic-tag-fold))
    (let ((tag (semantic-overlay-get ov 'semantic-tag-folding-tag)))
      
      (when tag
        (semantic-tag-folding-set-fold-state tag (semantic-overlay-get ov 'semantic-tag-folding-comment-overlay) (if fold 'fold 'show))
        (if fold
            (put-text-property 0 1 'display '((left-fringe semantic-tag-folding-folded) "+")
                               (semantic-overlay-get ov 'semantic-tag-folding-marker-string))
          ;; show
          (put-text-property 0 1 'display '((left-fringe semantic-tag-folding-unfolded) "-")
                             (semantic-overlay-get ov 'semantic-tag-folding-marker-string))
          (semantic-overlay-put ov 'semantic-tag-reveal-mode called-by-reveal-mode)
          (semantic-tag-folding-highlight-overlay ov))))))

;; set the function to be called when regions are revealed and hidden by reveal-mode.
(put  'semantic-tag-fold 'reveal-toggle-invisible 'semantic-tag-folding-set-overlay-visibility-for-reveal-mode)

(defun semantic-tag-folding-set-overlay-visibility-for-reveal-mode (ov fold)
"Fold/unfold function called from reveal mode.
OV is the overlay whose state must change, FOLD is non-nil to fold the overlay."
  (let ((semantic-tag-folding-tag-higlight-time
         (if semantic-tag-folding-highlight-tags-shown-by-reveal-mode
             semantic-tag-folding-tag-higlight-time
           nil)))
    (semantic-tag-folding-set-overlay-visibility ov fold t)))

(defun semantic-tag-folding-highlight-overlay (ov)
  "Temporarily draw attention to the overlay OV.
This is done by drawing a vertical bar in the fringe for the
lines that OV extends over for
`semantic-tag-folding-tag-higlight-time' seconds."
  (when semantic-tag-folding-tag-higlight-time
    (let ((overlays nil))
      (labels ((make-fringe (fringe string)
                 (setq overlays (cons (semantic-make-overlay (point-at-bol) (+ 1(point-at-bol))) overlays) )
                 (semantic-overlay-put (car overlays) 'before-string
                              (propertize string 'display `(left-fringe ,fringe)))))
        (save-excursion
          (goto-char (semantic-overlay-start ov))
          (make-fringe 'semantic-tag-folding-highlight-top "+")
          (forward-line)
          (while (< (point-at-eol) (semantic-overlay-end ov))
            (make-fringe 'semantic-tag-folding-highlight-middle "|")
            (forward-line))
          (make-fringe 'semantic-tag-folding-highlight-bottom "+"))
        (sit-for semantic-tag-folding-tag-higlight-time)
        (mapc 'semantic-overlay-delete overlays)))))

(defun semantic-tag-folding-click (event)
  "Handle fringe click EVENT by folding/unfolding blocks."
    (interactive "e")
    (when (event-start event)
      (let* ((start (event-start event))
            (point (posn-point start))
            (window (posn-window start)))
        (select-window window)
        (goto-char point)
        (let ((bitmaps (fringe-bitmaps-at-pos point)))
          (if (member 'semantic-tag-folding-folded bitmaps)
              (semantic-tag-folding-set-overlay-visibility (semantic-tag-folding-get-overlay) nil))
          (if (member 'semantic-tag-folding-unfolded bitmaps)
              (semantic-tag-folding-set-overlay-visibility (semantic-tag-folding-get-overlay) t))))))

(provide 'semantic-tag-folding)
;;; semantic-tag-folding.el ends here
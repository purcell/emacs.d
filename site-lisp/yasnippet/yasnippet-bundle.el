;;; yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid
;; 
;; Author: pluskid <pluskid@gmail.com>
;; Version: 0.5.6
;; X-URL: http://code.google.com/p/yasnippet/

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

;;; Commentary:

;; Basic steps to setup:
;;   1. Place `yasnippet.el' in your `load-path'.
;;   2. In your .emacs file:
;;        (require 'yasnippet)
;;   3. Place the `snippets' directory somewhere. E.g: ~/.emacs.d/snippets
;;   4. In your .emacs file
;;        (yas/initialize)
;;        (yas/load-directory "~/.emacs.d/snippets")
;;
;; For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/dont-activate nil
  "If set to t, don't activate yas/minor-mode automatically.")
(make-variable-buffer-local 'yas/dont-activate)

(defvar yas/key-syntaxes (list "w" "w_" "w_." "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/root-directory nil
  "The (list of) root directory that stores the snippets for each 
major modes.")

(defvar yas/indent-line t
  "Each (except the 1st) line of the snippet template is indented to
current column if this variable is non-`nil'.")
(make-variable-buffer-local 'yas/indent-line)

(defvar yas/trigger-key (kbd "TAB")
  "The key to bind as a trigger of snippet.")
(defvar yas/next-field-key (kbd "TAB")
  "The key to navigate to next field.")

(defvar yas/keymap (make-sparse-keymap)
  "The keymap of snippet.")
(define-key yas/keymap yas/next-field-key 'yas/next-field-group)
(define-key yas/keymap (kbd "S-TAB") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-iso-lefttab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-tab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<backtab>") 'yas/prev-field-group)

(defvar yas/show-all-modes-in-menu nil
  "Currently yasnippet only all \"real modes\" to menubar. For
example, you define snippets for \"cc-mode\" and make it the
parent of `c-mode', `c++-mode' and `java-mode'. There's really
no such mode like \"cc-mode\". So we don't show it in the yasnippet
menu to avoid the menu becoming too big with strange modes. The
snippets defined for \"cc-mode\" can still be accessed from
menu-bar->c-mode->parent (or c++-mode, java-mode, all are ok).
However, if you really like to show all modes in the menu, set
this variable to t.")
(defvar yas/use-menu t
  "If this is set to `t', all snippet template of the current
mode will be listed under the menu \"yasnippet\".")
(defvar yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger.")

(defface yas/field-highlight-face
  '((((class color) (background light)) (:background "DarkSeaGreen2"))
    (t (:background "DimGrey")))
  "The face used to highlight a field of snippet.")
(defface yas/mirror-highlight-face
  '((((class color) (background light)) (:background "LightYellow2"))
    (t (:background "gray22")))
  "The face used to highlight mirror fields of a snippet.")

(defvar yas/window-system-popup-function #'yas/dropdown-list-popup-for-template
  "When there's multiple candidate for a snippet key. This function
is called to let user select one of them. `yas/text-popup-function'
is used instead when not in a window system.")
(defvar yas/text-popup-function #'yas/dropdown-list-popup-for-template
  "When there's multiple candidate for a snippet key. If not in a
window system, this function is called to let user select one of
them. `yas/window-system-popup-function' is used instead when in
a window system.")

(defvar yas/extra-mode-hooks
  '()
  "A list of mode-hook that should be hooked to enable yas/minor-mode.
Most modes need no special consideration. Some mode (like ruby-mode)
doesn't call `after-change-major-mode-hook' need to be hooked explicitly.")
(mapc '(lambda (x)
	 (add-to-list 'yas/extra-mode-hooks
		      x))
      '(ruby-mode-hook actionscript-mode-hook ox-mode-hook python-mode-hook))

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.
The hooks will be run in an environment where some variables bound to 
proper values:
 * yas/snippet-beg : The beginning of the region of the snippet.
 * yas/snippet-end : Similar to beg.")

(defvar yas/before-expand-snippet-hook
  '()
  "Hooks to run after a before expanding a snippet.")

(defvar yas/buffer-local-condition 
  '(if (and (not (bobp))
	    (or (equal "font-lock-comment-face"
		       (get-char-property (1- (point))
					  'face))
		(equal "font-lock-string-face"
		       (get-char-property (1- (point))
					  'face))))
       '(require-snippet-condition . force-in-comment)
     t)
  "Condition to yasnippet local to each buffer.

    * If yas/buffer-local-condition evaluate to nil, snippet
      won't be expanded.

    * If it evaluate to the a cons cell where the car is the
      symbol require-snippet-condition and the cdr is a
      symbol (let's call it requirement):
       * If the snippet has no condition, then it won't be
         expanded.
       * If the snippet has a condition but evaluate to nil or
         error occured during evaluation, it won't be expanded.
       * If the snippet has a condition that evaluate to
         non-nil (let's call it result):
          * If requirement is t, the snippet is ready to be
            expanded.
          * If requirement is eq to result, the snippet is ready
            to be expanded.
          * Otherwise the snippet won't be expanded.
    * If it evaluate to other non-nil value:
       * If the snippet has no condition, or has a condition that
         evaluate to non-nil, it is ready to be expanded.
       * Otherwise, it won't be expanded.

Here's an example:

 (add-hook 'python-mode-hook
           '(lambda ()
              (setq yas/buffer-local-condition
                    '(if (python-in-string/comment)
                         '(require-snippet-condition . force-in-comment)
                       t))))")

(defvar yas/fallback-behavior 'call-other-command
  "The fall back behavior of YASnippet when it can't find a snippet
to expand. 

 * 'call-other-command means try to temporarily disable
    YASnippet and call other command bound to `yas/trigger-key'.
 * 'return-nil means return nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/version "0.5.6")

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major-mode.")
(defvar yas/menu-table (make-hash-table)
  "A hash table of menus of corresponding major-mode.")
(defvar yas/menu-keymap (make-sparse-keymap "YASnippet"))
;; empty menu will cause problems, so we insert some items
(define-key yas/menu-keymap [yas/about]
  '(menu-item "About" yas/about))
(define-key yas/menu-keymap [yas/reload]
  '(menu-item "Reload all snippets" yas/reload-all))
(define-key yas/menu-keymap [yas/load]
  '(menu-item "Load snippets..." yas/load-directory))
(define-key yas/menu-keymap [yas/separator]
  '(menu-item "--"))

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")
(defconst yas/escape-backslash
  (concat "YASESCAPE" "BACKSLASH" "PROTECTGUARD"))
(defconst yas/escape-dollar
  (concat "YASESCAPE" "DOLLAR" "PROTECTGUARD"))
(defconst yas/escape-backquote
  (concat "YASESCAPE" "BACKQUOTE" "PROTECTGUARD"))

(defconst yas/field-regexp
  (concat "$\\([0-9]+\\)" "\\|"
	  "${\\(?:\\([0-9]+\\):\\)?\\([^}]*\\)}"))

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet")
(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))

(defvar yas/overlay-modification-hooks
  (list 'yas/overlay-modification-hook)
  "The list of hooks to the overlay modification event.")
(defvar yas/overlay-insert-in-front-hooks
  (list 'yas/overlay-insert-in-front-hook)
  "The list of hooks of the overlay inserted in front event.")
(defvar yas/keymap-overlay-modification-hooks
  (list 'yas/overlay-maybe-insert-behind-hook)
  "The list of hooks of the big keymap overlay modification event.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/minor-mode-map (make-sparse-keymap)
  "The keymap of yas/minor-mode")
(defvar yas/minor-mode-on-hook nil
  "Hook to call when yas/minor-mode is on.")
(defvar yas/minor-mode-off-hook nil
  "Hook to call when yas/minor-mode is off.")
(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.
With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When YASnippet mode is enabled, the TAB key
expands snippets of code depending on the mode.

You can customize the key through `yas/trigger-key'."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'editing
  (define-key yas/minor-mode-map yas/trigger-key 'yas/expand))

(defun yas/minor-mode-auto-on ()
  "Turn on YASnippet minor mode unless `yas/dont-activate' is
set to t."
  (unless yas/dont-activate
    (yas/minor-mode-on)))
(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode."
  (interactive)
  (yas/minor-mode 1))
(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (yas/template (:constructor yas/make-template
				       (content name condition)))
  "A template for a snippet."
  content
  name
  condition)
(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet."
  (groups nil)
  (exit-marker nil)
  (id (yas/snippet-next-id) :read-only t)
  (overlay nil))
(defstruct (yas/group (:constructor yas/make-group (primary-field snippet)))
  "A group contains a list of field with the same number."
  primary-field
  (fields (list primary-field))
  (next nil)
  (prev nil)
  snippet)
(defstruct (yas/field 
	    (:constructor yas/make-field (overlay number value transform)))
  "A field in a snippet."
  overlay
  number
  transform
  value)
(defstruct (yas/snippet-table (:constructor yas/make-snippet-table ()))
  "A table to store snippets for a perticular mode."
  (hash (make-hash-table :test 'equal))
  (parent nil))

(defun yas/snippet-valid? (snippet)
  "See if snippet is valid (ie. still alive)."
  (and (not (null snippet))
       (not (null (yas/snippet-overlay snippet)))
       (not (null (overlay-start (yas/snippet-overlay snippet))))))

(defun yas/snippet-add-field (snippet field)
  "Add FIELD to SNIPPET."
  (let ((group (find field
		     (yas/snippet-groups snippet)
		     :test
		     '(lambda (field group)
			(and (not (null (yas/field-number field)))
			     (not (null (yas/group-number group)))
			     (= (yas/field-number field)
				(yas/group-number group)))))))
    (if group
	(yas/group-add-field group field)
      (push (yas/make-group field snippet)
	    (yas/snippet-groups snippet)))))

(defun yas/group-value (group)
  "Get the default value of the field group."
  (or (yas/field-value
       (yas/group-primary-field group))
      ""))
(defun yas/group-number (group)
  "Get the number of the field group."
  (yas/field-number
   (yas/group-primary-field group)))
(defun yas/group-add-field (group field)
  "Add a field to the field group. If the value of the primary 
field is nil and that of the field is not nil, the field is set
as the primary field of the group."
  (push field (yas/group-fields group))
  (when (and (null (yas/field-value (yas/group-primary-field group)))
	     (yas/field-value field))
    (setf (yas/group-primary-field group) field)))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the start point of the overlay."
  (let ((n1 (yas/field-number field1))
	(n2 (yas/field-number field2)))
    (if n1
	(if n2
	    (< n1 n2)
	  t)
      (if n2
	  nil
	(< (overlay-start (yas/field-overlay field1))
	   (overlay-start (yas/field-overlay field2)))))))

(defun yas/template-condition-predicate (condition)
  (condition-case err
      (save-excursion
	(save-restriction
	  (save-match-data
	    (eval condition))))
    (error (progn
	     (message (format "[yas]error in condition evaluation: %s"
			      (error-message-string err)))
	     nil))))

(defun yas/filter-templates-by-condition (templates)
  "Filter the templates using the condition. The rules are:

 * If the template has no condition, it is kept.
 * If the template's condition eval to non-nil, it is kept.
 * Otherwise (eval error or eval to nil) it is filtered."
  (remove-if-not '(lambda (pair)
		    (let ((condition (yas/template-condition (cdr pair))))
		      (if (null condition)
			  (if yas/require-template-condition
			      nil
			    t)
			(let ((result 
			       (yas/template-condition-predicate condition)))
			  (if yas/require-template-condition
			      (if (eq yas/require-template-condition t)
				  result
				(eq result yas/require-template-condition))
			    result)))))
		 templates))

(defun yas/snippet-table-fetch (table key)
  "Fetch a snippet binding to KEY from TABLE. If not found,
fetch from parent if any."
  (let ((templates (yas/filter-templates-by-condition
		    (gethash key (yas/snippet-table-hash table)))))
    (when (and (null templates)
	       (not (null (yas/snippet-table-parent table))))
      (setq templates (yas/snippet-table-fetch
		       (yas/snippet-table-parent table)
		       key)))
    templates))
(defun yas/snippet-table-store (table full-key key template)
  "Store a snippet template in the table."
  (puthash key
	   (yas/modify-alist (gethash key
				      (yas/snippet-table-hash table))
			     full-key
			     template)
	   (yas/snippet-table-hash table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/ensure-minor-mode-priority ()
  "Ensure that the key binding of yas/minor-mode takes priority."
  (unless (eq 'yas/minor-mode
	      (caar minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons
	   (cons 'yas/minor-mode yas/minor-mode-map)
	   (assq-delete-all 'yas/minor-mode
			    minor-mode-map-alist)))))

(defun yas/real-mode? (mode)
  "Try to find out if MODE is a real mode. The MODE bound to
a function (like `c-mode') is considered real mode. Other well
known mode like `ruby-mode' which is not part of Emacs might
not bound to a function until it is loaded. So yasnippet keeps
a list of modes like this to help the judgement."
  (or (fboundp mode)
      (find mode yas/known-modes)))

(defun yas/eval-string (string)
  "Evaluate STRING and convert the result to string."
  (condition-case err
      (save-excursion
	(save-restriction
	  (save-match-data
	    (widen)
	    (format "%s" (eval (read string))))))
    (error (format "(error in elisp evaluation: %s)" 
		   (error-message-string err)))))
(defun yas/calculate-field-value (field value)
  "Calculate the value of the field. If there's a transform
for this field, apply it. Otherwise, the value is returned
unmodified."
  (let ((text value)
	(transform (yas/field-transform field)))
    (if transform
	(yas/eval-string transform)
      text)))
(defsubst yas/replace-all (from to)
  "Replace all occurance from FROM to TO."
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t t)))

(defun yas/snippet-table (mode)
  "Get the snippet table corresponding to MODE."
  (let ((table (gethash mode yas/snippet-tables)))
    (unless table
      (setq table (yas/make-snippet-table))
      (puthash mode table yas/snippet-tables))
    table))
(defsubst yas/current-snippet-table ()
  "Get the snippet table for current major-mode."
  (yas/snippet-table major-mode))

(defun yas/menu-keymap-for-mode (mode)
  "Get the menu keymap correspondong to MODE."
  (let ((keymap (gethash mode yas/menu-table)))
    (unless keymap
      (setq keymap (make-sparse-keymap))
      (puthash mode keymap yas/menu-table))
    keymap))

(defun yas/current-key ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
	(end (point))
	(syntaxes yas/key-syntaxes)
	syntax done templates)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
	(skip-syntax-backward syntax)
	(setq start (point)))
      (setq templates
	    (yas/snippet-table-fetch
	     (yas/current-snippet-table)
	     (buffer-substring-no-properties start end)))
      (if templates
	  (setq done t)
	(setq start end)))
    (list templates
	  start
	  end)))

(defun yas/synchronize-fields (field-group)
  "Update all fields' text according to the primary field."
  (when (yas/snippet-valid? (yas/group-snippet field-group))
    (save-excursion
      (let* ((inhibit-modification-hooks t)
	     (primary (yas/group-primary-field field-group))
	     (primary-overlay (yas/field-overlay primary))
	     (text (buffer-substring-no-properties (overlay-start primary-overlay)
						   (overlay-end primary-overlay))))
	(dolist (field (yas/group-fields field-group))
	  (let* ((field-overlay (yas/field-overlay field))
		 (original-length (- (overlay-end field-overlay)
				     (overlay-start field-overlay))))
	    (unless (eq field-overlay primary-overlay)
	      (goto-char (overlay-start field-overlay))
	      (insert (yas/calculate-field-value field text))
	      (if (= (overlay-start field-overlay)
		     (overlay-end field-overlay))
		  (move-overlay field-overlay
				(overlay-start field-overlay)
				(point))
		(delete-char original-length)))))))))
  
(defun yas/overlay-modification-hook (overlay after? beg end &optional length)
  "Modification hook for snippet field overlay."
  (when (and after? (not undo-in-progress))
    (yas/synchronize-fields (overlay-get overlay 'yas/group))))
(defun yas/overlay-insert-in-front-hook (overlay after? beg end &optional length)
  "Hook for snippet overlay when text is inserted in front of a snippet field."
  (when after?
    (let ((field-group (overlay-get overlay 'yas/group))
	  (inhibit-modification-hooks t))
      (when (not (overlay-get overlay 'yas/modified?))
	(overlay-put overlay 'yas/modified? t)
	(when (> (overlay-end overlay) end)
	  (save-excursion
	    (goto-char end)
	    (delete-char (- (overlay-end overlay) end)))))
     (yas/synchronize-fields field-group))))
(defun yas/overlay-maybe-insert-behind-hook (overlay after? beg end &optional length)
  "Insert behind hook sometimes doesn't get called. I don't know why.
So I add modification hook in the big overlay and try to detect `insert-behind'
event manually."
  (when after?
    (cond ((and (= beg end)
		(> length 0)
		(= (overlay-start overlay)
		   (overlay-end overlay)))
	   (yas/exit-snippet (overlay-get overlay 'yas/snippet-reference)))
	  ((and (= length 0)
		(> end beg)
		(null (yas/current-snippet-overlay beg))
		(not (bobp)))
	   (let ((field-overlay (yas/current-snippet-overlay (1- beg))))
	     (if field-overlay
		 (when (= beg (overlay-end field-overlay))
		   (move-overlay field-overlay
				 (overlay-start field-overlay)
				 end)
		   (yas/synchronize-fields (overlay-get field-overlay 'yas/group)))
	       (let ((snippet (yas/snippet-of-current-keymap))
		     (done nil))
		 (if snippet
		     (do* ((groups (yas/snippet-groups snippet) (cdr groups))
			   (group (car groups) (car groups)))
			 ((or (null groups)
			      done))
		       (setq field-overlay (yas/field-overlay 
					    (yas/group-primary-field group)))
		       (when (and (= (overlay-start field-overlay)
				     (overlay-end field-overlay))
				  (= beg
				     (overlay-start field-overlay)))
			 (move-overlay field-overlay beg end)
			 (yas/synchronize-fields group)
			 (setq done t)))))))))))

(defun yas/undo-expand-snippet (start end key snippet)
  "Undo a snippet expansion. Delete the overlays. This undo can't be
redo-ed."
  (let ((undo (car buffer-undo-list)))
    (while (null undo)
      (setq buffer-undo-list (cdr buffer-undo-list))
      (setq undo (car buffer-undo-list)))
    ;; Remove this undo operation record
    (setq buffer-undo-list (cdr buffer-undo-list))
  (let ((inhibit-modification-hooks t)
	(buffer-undo-list t))
    (yas/exit-snippet snippet)
    (goto-char start)
    (delete-char (- end start))
    (insert key))))

(defun yas/expand-snippet (start end template)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (run-hooks 'yas/before-expand-snippet-hook)

  (goto-char start)

  (let ((key (buffer-substring-no-properties start end))
	(original-undo-list buffer-undo-list)
	(inhibit-modification-hooks t)
	(length (- end start))
	(column (current-column)))
    (save-restriction
      (narrow-to-region start start)

      (setq buffer-undo-list t)
      (insert template)

      ;; Step 1: do necessary indent
      (when yas/indent-line
	(let* ((indent (if indent-tabs-mode
			   (concat (make-string (/ column tab-width) ?\t)
				   (make-string (% column tab-width) ?\ ))
			 (make-string column ?\ ))))
	  (goto-char (point-min))
	  (while (and (zerop (forward-line))
		      (= (current-column) 0))
	    (insert indent))))

      ;; Step 2: protect backslash and backquote
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)

      ;; Step 3: evaluate all backquotes
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`]*\\)`" nil t)
	;; go back so that (current-column) in elisp code evaluation
	;; will calculate to a meaningful value
	(goto-char (match-beginning 0))
	(replace-match (yas/eval-string (match-string-no-properties 1))
		       t t))

      ;; Step 4: protect all escapes, including backslash and backquot
      ;; which may be produced in Step 3
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)
      (yas/replace-all "\\$" yas/escape-dollar)

      (let ((snippet (yas/make-snippet)))
	;; Step 5: Create fields
	(goto-char (point-min))
	(while (re-search-forward yas/field-regexp nil t)
	  (let ((number (or (match-string-no-properties 1)
			    (match-string-no-properties 2)))
		(transform nil)
		(value (match-string-no-properties 3)))
	    (when (eq (elt value 0) ?\$)
	      (setq transform (substring value 1))
	      (setq value nil))
	    (if (and number
		     (string= "0" number))
		(progn
		  (replace-match "")
		  (setf (yas/snippet-exit-marker snippet)
			(copy-marker (point) t)))
	      (yas/snippet-add-field
	       snippet
	       (yas/make-field
		(make-overlay (match-beginning 0) (match-end 0))
		(and number (string-to-number number))
		value
		transform)))))

	;; Step 6: Sort and link each field group
	(setf (yas/snippet-groups snippet)
	      (sort (yas/snippet-groups snippet)
		    '(lambda (group1 group2)
		       (yas/snippet-field-compare
			(yas/group-primary-field group1)
			(yas/group-primary-field group2)))))
	(let ((prev nil))
	  (dolist (group (yas/snippet-groups snippet))
	    (setf (yas/group-prev group) prev)
	    (when prev
	      (setf (yas/group-next prev) group))
	    (setq prev group)))

	;; Step 7: Create keymap overlay for snippet
	(let ((overlay (make-overlay (point-min)
				     (point-max)
				     nil
				     nil
				     t)))
	  (overlay-put overlay 
		       'modification-hooks
		       yas/keymap-overlay-modification-hooks)
	  (overlay-put overlay 
		       'insert-behind-hooks
		       yas/keymap-overlay-modification-hooks)
	  (overlay-put overlay 'keymap yas/keymap)
	  (overlay-put overlay 'yas/snippet-reference snippet)
	  (setf (yas/snippet-overlay snippet) overlay))
	
	;; Step 8: Replace fields with default values
	(dolist (group (yas/snippet-groups snippet))
	  (let ((value (yas/group-value group)))
	    (dolist (field (yas/group-fields group))
	      (let* ((overlay (yas/field-overlay field))
		     (start (overlay-start overlay))
		     (end (overlay-end overlay))
		     (length (- end start)))
		(goto-char start)
		(insert (yas/calculate-field-value field value))
		(delete-char length)))))

	;; Step 9: restore all escape characters
	(yas/replace-all yas/escape-dollar "$")
	(yas/replace-all yas/escape-backquote "`")
	(yas/replace-all yas/escape-backslash "\\")

	;; Step 10: Set up properties of overlays
	(dolist (group (yas/snippet-groups snippet))
	  (let ((overlay (yas/field-overlay
			  (yas/group-primary-field group))))
	    (overlay-put overlay 'yas/snippet snippet)
	    (overlay-put overlay 'yas/group group)
	    (overlay-put overlay 'yas/modified? nil)
	    (overlay-put overlay 'modification-hooks yas/overlay-modification-hooks)
	    (overlay-put overlay 'insert-in-front-hooks yas/overlay-insert-in-front-hooks)
	    (overlay-put overlay 'face 'yas/field-highlight-face)
	    (dolist (field (yas/group-fields group))
	      (unless (equal overlay (yas/field-overlay field))
		(overlay-put (yas/field-overlay field)
			     'face 
			     'yas/mirror-highlight-face)))))

	;; Step 11: move to end and make sure exit-marker exist
	(goto-char (point-max))
	(unless (yas/snippet-exit-marker snippet)
	  (setf (yas/snippet-exit-marker snippet) (copy-marker (point) t)))

	;; Step 12: Construct undo information
	(unless (eq original-undo-list t)
	  (add-to-list 'original-undo-list
		       `(apply yas/undo-expand-snippet
			       ,(point-min)
			       ,(point-max)
			       ,key
			       ,snippet)))

	;; Step 13: remove the trigger key
	(widen)
	(delete-char length)

	(setq buffer-undo-list original-undo-list)

	;; Step 14: place the cursor at a proper place
	(let ((groups (yas/snippet-groups snippet))
	      (exit-marker (yas/snippet-exit-marker snippet)))
	  (if groups
	      (goto-char (overlay-start 
			  (yas/field-overlay
			   (yas/group-primary-field
			    (car groups)))))
	    ;; no need to call exit-snippet, since no overlay created.
	    (yas/exit-snippet snippet)))))))

(defun yas/current-snippet-overlay (&optional point)
  "Get the most proper overlay which is belongs to a snippet."
  (let ((point (or point (point)))
	(snippet-overlay nil))
    (dolist (overlay (overlays-at point))
      (when (overlay-get overlay 'yas/snippet)
	(if (null snippet-overlay)
	    (setq snippet-overlay overlay)
	  (when (> (yas/snippet-id (overlay-get overlay 'yas/snippet))
		   (yas/snippet-id (overlay-get snippet-overlay 'yas/snippet)))
	    (setq snippet-overlay overlay)))))
    snippet-overlay))

(defun yas/snippet-of-current-keymap (&optional point)
  "Get the snippet holding the snippet keymap under POINT."
  (let ((point (or point (point)))
	(keymap-snippet nil)
	(snippet nil))
    (dolist (overlay (overlays-at point))
      (setq snippet (overlay-get overlay 'yas/snippet-reference))
      (when snippet
	(if (null keymap-snippet)
	    (setq keymap-snippet snippet)
	  (when (> (yas/snippet-id snippet)
		   (yas/snippet-id keymap-snippet))
	    (setq keymap-snippet snippet)))))
    keymap-snippet))

(defun yas/current-overlay-for-navigation ()
  "Get current overlay for navigation. Might be overlay at current or previous point."
  (let ((overlay1 (yas/current-snippet-overlay))
	(overlay2 (if (bobp)
		      nil
		    (yas/current-snippet-overlay (- (point) 1)))))
    (if (null overlay1)
	overlay2
      (if (or (null overlay2)
	      (eq (overlay-get overlay1 'yas/snippet) 
		  (overlay-get overlay2 'yas/snippet)))
	  overlay1
	(if (> (yas/snippet-id (overlay-get overlay2 'yas/snippet))
	       (yas/snippet-id (overlay-get overlay1 'yas/snippet)))
	    overlay2
	  overlay1)))))

(defun yas/navigate-group (group next?)
  "Go to next of previous field group. Exit snippet if none."
  (let ((target (if next?
		    (yas/group-next group)
		  (yas/group-prev group))))
    (if target
	(goto-char (overlay-start
		    (yas/field-overlay
		     (yas/group-primary-field target))))
      (yas/exit-snippet (yas/group-snippet group)))))

(defun yas/parse-template (&optional file-name)
  "Parse the template in the current buffer.
If the buffer contains a line of \"# --\" then the contents
above this line are ignored. Variables can be set above this
line through the syntax:

#name : value

Here's a list of currently recognized variables:

 * name
 * contributor
 * condition

#name: #include \"...\"
# --
#include \"$1\""
  (goto-char (point-min))
  (let ((name file-name) template bound condition)
    (if (re-search-forward "^# --\n" nil t)
	(progn (setq template 
		     (buffer-substring-no-properties (point) 
						     (point-max)))
	       (setq bound (point))
	       (goto-char (point-min))
	       (while (re-search-forward "^#\\([^ ]+\\) *: *\\(.*\\)$" bound t)
		 (when (string= "name" (match-string-no-properties 1))
		   (setq name (match-string-no-properties 2)))
		 (when (string= "condition" (match-string-no-properties 1))
		   (setq condition (read (match-string-no-properties 2))))))
      (setq template
	    (buffer-substring-no-properties (point-min) (point-max))))
    (list template name condition)))

(defun yas/directory-files (directory file?)
  "Return directory files or subdirectories in full path."
  (remove-if (lambda (file)
	       (or (string-match "^\\."
				 (file-name-nondirectory file))
		   (if file?
		       (file-directory-p file)
		     (not (file-directory-p file)))))
	     (directory-files directory t)))

(defun yas/make-menu-binding (template)
  (lexical-let ((template template))
    (lambda ()
      (interactive)
      (yas/expand-snippet (point) 
			  (point)
			  template))))

(defun yas/modify-alist (alist key value)
  "Modify ALIST to map KEY to VALUE. return the new alist."
  (let ((pair (assoc key alist)))
    (if (null pair)
	(cons (cons key value)
	      alist)
      (setcdr pair value)
      alist)))

(defun yas/fake-keymap-for-popup (templates)
  "Create a fake keymap for popup menu usage."
  (cons 'keymap 
	(mapcar (lambda (pair)
		  (let* ((template (cdr pair))
			 (name (yas/template-name template))
			 (content (yas/template-content template)))
		    (list content 'menu-item name t)))
		templates)))

(defun yas/point-to-coord (&optional point)
  "Get the xoffset/yoffset information of POINT.
If POINT is not given, default is to current point.
If `posn-at-point' is not available (like in Emacs 21.3),
t is returned simply."
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (or point (point))))))
	(list (list (+ (car x-y) 10)
		    (+ (cdr x-y) 20))
	      (selected-window)))
    t))
 
(defun yas/x-popup-menu-for-template (templates)
  "Show a popup menu listing templates to let the user select one."
  (car (x-popup-menu (yas/point-to-coord)
		     (yas/fake-keymap-for-popup templates))))
(defun yas/text-popup-for-template (templates)
  "Can't display popup menu in text mode. Just select the first one."
  (yas/template-content (cdar templates)))
(defun yas/dropdown-list-popup-for-template (templates)
  "Use dropdown-list.el to popup for templates. Better than the 
default \"select first\" behavior of `yas/text-popup-for-template'.
You can also use this in window-system.

NOTE: You need to download and install dropdown-list.el to use this."
  (if (fboundp 'dropdown-list)
      (let ((n (dropdown-list (mapcar (lambda (i)
					(yas/template-name
					 (cdr i)))
				      templates))))
	(if n
	    (yas/template-content
	     (cdr (nth n templates)))
	  nil))
    (error "Please download and install dropdown-list.el to use this")))

(defun yas/popup-for-template (templates)
  (if window-system
      (funcall yas/window-system-popup-function templates)
    (funcall yas/text-popup-function templates)))

(defun yas/load-directory-1 (directory &optional parent)
  "Really do the job of loading snippets from a directory 
hierarchy."
  (let ((mode-sym (intern (file-name-nondirectory directory)))
	(snippets nil))
    (with-temp-buffer
      (dolist (file (yas/directory-files directory t))
	(when (file-readable-p file)
	  (insert-file-contents file nil nil nil t)
	  (let ((snippet-file-name (file-name-nondirectory file)))
	    (push (cons snippet-file-name
			(yas/parse-template snippet-file-name))
		  snippets)))))
    (yas/define-snippets mode-sym
			 snippets
			 parent)
    (dolist (subdir (yas/directory-files directory nil))
      (yas/load-directory-1 subdir mode-sym))))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
	  (replace-regexp-in-string "[\\\"]"
				    "\\\\\\&"
				    string
				    t)
	  "\""))

(defun yas/compile-bundle
  (&optional yasnippet yasnippet-bundle snippet-roots code)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.
SNIPPET-ROOTS is a list of root directories that contains the snippets
definition. YASNIPPET is the yasnippet.el file path. YASNIPPET-BUNDLE
is the output file of the compile result. CODE is the code you would
like to used to initialize yasnippet. Here's the default value for
all the parameters:

 (yas/compile-bundle \"yasnippet.el\"
                     \"./yasnippet-bundle.el\"
                     '(\"snippets\")
                     \"(yas/initialize)\")"
  (when (null yasnippet)
    (setq yasnippet "yasnippet.el"))
  (when (null yasnippet-bundle)
    (setq yasnippet-bundle "./yasnippet-bundle.el"))
  (when (null snippet-roots)
    (setq snippet-roots '("snippets")))
  (when (null code)
    (setq code "(yas/initialize)"))

  (let ((dirs (or (and (listp snippet-roots) snippet-roots)
		  (list snippet-roots)))
	(bundle-buffer nil))
    (with-temp-buffer
      (setq bundle-buffer (current-buffer))
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert code "\n")
      (flet ((yas/define-snippets 
	      (mode snippets &optional parent)
	      (with-current-buffer bundle-buffer
		(insert ";;; snippets for " (symbol-name mode) "\n")
		(insert "(yas/define-snippets '" (symbol-name mode) "\n")
		(insert "'(\n")
		(dolist (snippet snippets)
		  (insert "  (" 
			  (yas/quote-string (car snippet))
			  " "
			  (yas/quote-string (cadr snippet))
			  " "
			  (if (caddr snippet)
			      (yas/quote-string (caddr snippet))
			    "nil")
			  " "
			  (if (nth 3 snippet)
			      (format "'%s" (nth 3 snippet))
			    "nil")
			  ")\n"))
		(insert "  )\n")
		(insert (if parent
			    (concat "'" (symbol-name parent))
			  "nil")
			")\n\n"))))
	    (dolist (dir dirs)
	      (dolist (subdir (yas/directory-files dir nil))
		(yas/load-directory-1 subdir nil))))
      (insert "(provide '"
	      (file-name-nondirectory
	       (file-name-sans-extension
		yasnippet-bundle))
	      ")\n")
      (setq buffer-file-name yasnippet-bundle)
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
		   yas/version
		   ") -- pluskid <pluskid@gmail.com>")))
(defun yas/reload-all ()
  "Reload all snippets."
  (interactive)
  (if yas/root-directory
      (if (listp yas/root-directory)
	  (dolist (directory yas/root-directory)
	    (yas/load-directory directory))
	(yas/load-directory yas/root-directory))
    (call-interactively 'yas/load-directory))
  (message "done."))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.
Below the top-level directory, each directory is a mode
name. And under each subdirectory, each file is a definition
of a snippet. The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (when (file-directory-p directory)
    (add-to-list 'yas/root-directory directory))
  (dolist (dir (yas/directory-files directory nil))
    (yas/load-directory-1 dir))
  (when (interactive-p)
    (message "done.")))

(defun yas/initialize ()
  "Do necessary initialization."
  (add-hook 'after-change-major-mode-hook
	    'yas/minor-mode-auto-on)
  (dolist (hook yas/extra-mode-hooks)
    (add-hook hook
	      'yas/minor-mode-auto-on))
  (add-hook 'yas/minor-mode-on-hook
	    'yas/ensure-minor-mode-priority)
  (when yas/use-menu
    (define-key-after 
      (lookup-key global-map [menu-bar])
      [yasnippet]
      (cons "YASnippet" yas/menu-keymap)
      'buffer)))

(defun yas/define-snippets (mode snippets &optional parent-mode)
  "Define snippets for MODE. SNIPPETS is a list of
snippet definition, of the following form:

 (KEY TEMPLATE NAME CONDITION)

or the NAME and CONDITION may be omitted. The optional 3rd
parameter can be used to specify the parent mode of MODE. That
is, when looking a snippet in MODE failed, it can refer to its
parent mode. The PARENT-MODE may not need to be a real mode."
  (let ((snippet-table (yas/snippet-table mode))
	(parent-table (if parent-mode
			  (yas/snippet-table parent-mode)
			nil))
	(keymap (if yas/use-menu
		    (yas/menu-keymap-for-mode mode)
		  nil)))
    (when parent-table
      (setf (yas/snippet-table-parent snippet-table)
	    parent-table)
      (when yas/use-menu
	(define-key keymap (vector 'parent-mode)
	  `(menu-item "parent mode"
		      ,(yas/menu-keymap-for-mode parent-mode)))))
    (when (and yas/use-menu
	       (yas/real-mode? mode))
      (define-key yas/menu-keymap (vector mode)
	`(menu-item ,(symbol-name mode) ,keymap)))
    (dolist (snippet snippets)
      (let* ((full-key (car snippet))
	     (key (file-name-sans-extension full-key))
	     (name (or (caddr snippet) (file-name-extension full-key)))
	     (condition (nth 3 snippet))
	     (template (yas/make-template (cadr snippet)
					  (or name key)
					  condition)))
	(yas/snippet-table-store snippet-table
				 full-key
				 key
				 template)
	(when yas/use-menu
	  (define-key keymap (vector (make-symbol full-key))
	    `(menu-item ,(yas/template-name template)
			,(yas/make-menu-binding (yas/template-content template))
			:keys ,(concat key yas/trigger-symbol))))))))

(defun yas/set-mode-parent (mode parent)
  "Set parent mode of MODE to PARENT."
  (setf (yas/snippet-table-parent
	 (yas/snippet-table mode))
	(yas/snippet-table parent))
  (when yas/use-menu
    (define-key (yas/menu-keymap-for-mode mode) (vector 'parent-mode)
      `(menu-item "parent mode"
		  ,(yas/menu-keymap-for-mode parent)))))

(defun yas/define (mode key template &optional name condition)
  "Define a snippet. Expanding KEY into TEMPLATE.
NAME is a description to this template. Also update
the menu if `yas/use-menu' is `t'. CONDITION is the
condition attached to this snippet. If you attach a
condition to a snippet, then it will only be expanded
when the condition evaluated to non-nil."
  (yas/define-snippets mode
		       (list (list key template name condition))))
    

(defun yas/hippie-try-expand (first-time?)
  "Integrate with hippie expand. Just put this function in
`hippie-expand-try-functions-list'."
  (if (not first-time?)
      (let ((yas/fallback-behavior 'return-nil))
	(yas/expand))
    (when (and (null (car buffer-undo-list))
	       (eq 'apply
		   (car (cadr buffer-undo-list)))
	       (eq 'yas/undo-expand-snippet
		   (cadr (cadr buffer-undo-list))))
      (undo 1))
    nil))

(defun yas/expand ()
  "Expand a snippet."
  (interactive)
  (let ((local-condition (yas/template-condition-predicate
			  yas/buffer-local-condition)))
    (if local-condition
	(let ((yas/require-template-condition 
	       (if (and (consp local-condition)
			(eq 'require-snippet-condition (car local-condition))
			(symbolp (cdr local-condition)))
		   (cdr local-condition)
		 nil)))
	  (multiple-value-bind (templates start end) (yas/current-key)
	    (if templates
		(let ((template (if (null (cdr templates)) ; only 1 template
				    (yas/template-content (cdar templates))
				  (yas/popup-for-template templates))))
		  (if template
		    (progn (yas/expand-snippet start end template)
			   'expanded)	; expanded successfully
		    'interruptted))	; interrupted by user
	      (if (eq yas/fallback-behavior 'return-nil)
		  nil			; return nil
		(let* ((yas/minor-mode nil)
		       (command (key-binding yas/trigger-key)))
		  (when (commandp command)
		    (call-interactively command))))))))))
      
(defun yas/next-field-group ()
  "Navigate to next field group. If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-overlay-for-navigation)))
    (if overlay
	(yas/navigate-group (overlay-get overlay 'yas/group) t)
      (let ((snippet (yas/snippet-of-current-keymap))
	    (done nil))
	(if snippet
	    (do* ((groups (yas/snippet-groups snippet) (cdr groups))
		  (group (car groups) (car groups)))
		((or (null groups)
		     done)
		 (unless done 
		   (let* ((overlay (yas/snippet-overlay snippet))
			  (keymap (overlay-get overlay 'keymap))
			  (command nil))
		     (overlay-put overlay 'keymap nil)
		     (overlay-put overlay 'yas/snippet-reference nil)
		     (setq command (key-binding yas/next-field-key))
		     (when (commandp command)
		       (call-interactively command))
		     (overlay-put overlay 'keymap keymap)
		     (overlay-put overlay 'yas/snippet-reference snippet))))
	      (when (= (point)
		       (overlay-start
			(yas/field-overlay
			 (yas/group-primary-field group))))
		(setq done t)
		(yas/navigate-group group t))))))))

(defun yas/prev-field-group ()
  "Navigate to prev field group. If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-overlay-for-navigation)))
    (if overlay
	(yas/navigate-group (overlay-get overlay 'yas/group) nil)
      (let ((snippet (yas/snippet-of-current-keymap))
	    (done nil))
	(if snippet
	  (do* ((groups (yas/snippet-groups snippet) (cdr groups))
		(group (car groups) (car groups)))
	      ((or (null groups)
		   done)
	       (unless done (message "Not in a snippet field.")))
	    (when (= (point)
		     (overlay-start
		      (yas/field-overlay
		       (yas/group-primary-field group))))
	      (setq done t)
	      (yas/navigate-group group nil)))
	  (message "Not in a snippet field."))))))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET and delete the snippet."
  (interactive)
  (let ((overlay (yas/snippet-overlay snippet)))
    (let ((yas/snippet-beg (overlay-start overlay))
	  (yas/snippet-end (overlay-end overlay)))
      (goto-char (yas/snippet-exit-marker snippet))
      (delete-overlay overlay)
      (dolist (group (yas/snippet-groups snippet))
	(dolist (field (yas/group-fields group))
	  (delete-overlay (yas/field-overlay field))))

      (run-hooks 'yas/after-exit-snippet-hook))))

(provide 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey patching for other functions that's causing
;; problems to yasnippet. For details on why I patch
;; those functions, refer to
;;   http://code.google.com/p/yasnippet/wiki/MonkeyPatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-neutralize-syntax-in-CPP
  (around yas-mp/c-neutralize-syntax-in-CPP activate)
  "Adviced `c-neutralize-syntax-in-CPP' to properly 
handle the end-of-buffer error fired in it by calling
`forward-char' at the end of buffer."
  (condition-case err
      ad-do-it
    (error (message (error-message-string err)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contents of dropdown-list.el
;;
;; dropdown-list.el is used by yasnippet to select multiple
;; candidate snippets.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropdown-list.el --- Drop-down menu interface
;;
;; Filename: dropdown-list.el
;; Description: Drop-down menu interface
;; Author: Jaeyoun Chung [jay.chung@gmail.com]
;; Maintainer:
;; Copyright (C) 2008 Jaeyoun Chung
;; Created: Sun Mar 16 11:20:45 2008 (Pacific Daylight Time)
;; Version: 
;; Last-Updated: Sun Mar 16 12:19:49 2008 (Pacific Daylight Time)
;;           By: dradams
;;     Update #: 43
;; URL: http://www.emacswiki.org/cgi-bin/wiki/dropdown-list.el
;; Keywords: convenience menu
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  According to Jaeyoun Chung, "overlay code stolen from company-mode.el."
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2008/03/16 dadams
;;     Clean-up - e.g. use char-to-string for control chars removed by email posting.
;;     Moved example usage code (define-key*, command-selector) inside the library.
;;     Require cl.el at byte-compile time.
;;     Added GPL statement.
;; 2008/01/06 Jaeyoun Chung
;;     Posted to gnu-emacs-sources@gnu.org at 9:10 p.m.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; decf, fourth, incf, loop, mapcar*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface dropdown-list-face
    '((t :inherit default :background "lightyellow" :foreground "black"))
  "*Bla." :group 'dropdown-list)

(defface dropdown-list-selection-face
    '((t :inherit dropdown-list-face :background "purple"))
  "*Bla." :group 'dropdown-list)

(defvar dropdown-list-overlays nil)

(defun dropdown-list-hide ()
  (while dropdown-list-overlays
    (delete-overlay (pop dropdown-list-overlays))))

(defun dropdown-list-put-overlay (beg end &optional prop value prop2 value2)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'window t)
    (when prop
      (overlay-put ov prop value)
      (when prop2 (overlay-put ov prop2 value2)))
    ov))

(defun dropdown-list-line (start replacement &optional no-insert)
  ;; start might be in the middle of a tab, which means we need to hide the
  ;; tab and add spaces
  (let ((end (+ start (length replacement)))
        beg-point end-point
        before-string after-string)
    (goto-char (point-at-eol))
    (if (< (current-column) start)
        (progn (setq before-string (make-string (- start (current-column)) ? ))
               (setq beg-point (point)))
      (goto-char (point-at-bol)) ;; Emacs bug, move-to-column is wrong otherwise
      (move-to-column start)
      (setq beg-point (point))
      (when (> (current-column) start)
        (goto-char (1- (point)))
        (setq beg-point (point))
        (setq before-string (make-string (- start (current-column)) ? ))))
    (move-to-column end)
    (setq end-point (point))
    (let ((end-offset (- (current-column) end)))
      (when (> end-offset 0) (setq after-string (make-string end-offset ?b))))
    (when no-insert
      ;; prevent inheriting of faces
      (setq before-string (when before-string (propertize before-string 'face 'default)))
      (setq after-string (when after-string (propertize after-string 'face 'default))))
    (let ((string (concat before-string replacement after-string)))
      (if no-insert
          string
        (push (dropdown-list-put-overlay beg-point end-point 'invisible t
                                         'after-string string)
              dropdown-list-overlays)))))

(defun dropdown-list-start-column (display-width)
  (let ((column (mod (current-column) (window-width)))
        (width (window-width)))
    (cond ((<= (+ column display-width) width) column)
          ((> column display-width) (- column display-width))
          ((>= width display-width) (- width display-width))
          (t nil))))

(defun dropdown-list-move-to-start-line (candidate-count)
  (decf candidate-count)
  (let ((above-line-count (save-excursion (- (vertical-motion (- candidate-count)))))
        (below-line-count (save-excursion (vertical-motion candidate-count))))
    (cond ((= below-line-count candidate-count)
           t)
          ((= above-line-count candidate-count)
           (vertical-motion (- candidate-count))
           t)
          ((>= (+ below-line-count above-line-count) candidate-count)
           (vertical-motion (- (- candidate-count below-line-count)))
           t)
          (t nil))))

(defun dropdown-list-at-point (candidates &optional selidx)
  (dropdown-list-hide)
  (let* ((lengths (mapcar #'length candidates))
         (max-length (apply #'max lengths))
         (start (dropdown-list-start-column (+ max-length 3)))
         (i -1)
         (candidates (mapcar* (lambda (candidate length)
                                (let ((diff (- max-length length)))
                                  (propertize
                                   (concat (if (> diff 0)
                                               (concat candidate (make-string diff ? ))
                                             (substring candidate 0 max-length))
                                           (format "%3d" (+ 2 i)))
                                   'face (if (eql (incf i) selidx)
                                             'dropdown-list-selection-face
                                           'dropdown-list-face))))
                              candidates
                              lengths)))
    (save-excursion
      (and start
           (dropdown-list-move-to-start-line (length candidates))
           (loop initially (vertical-motion 0)
              for candidate in candidates
              do (dropdown-list-line (+ (current-column) start) candidate)
              while (/= (vertical-motion 1) 0)
              finally return t)))))

(defun dropdown-list (candidates)
  (let ((selection)
        (temp-buffer))
    (save-window-excursion
      (unwind-protect
           (let ((candidate-count (length candidates))
                 done key selidx)
             (while (not done)
               (unless (dropdown-list-at-point candidates selidx)
                 (switch-to-buffer (setq temp-buffer (get-buffer-create "*selection*"))
                                   'norecord)
                 (delete-other-windows)
                 (delete-region (point-min) (point-max))
                 (insert (make-string (length candidates) ?\n))
                 (goto-char (point-min))
                 (dropdown-list-at-point candidates selidx))
               (setq key (read-key-sequence ""))
               (cond ((and (stringp key)
                           (>= (aref key 0) ?1)
                           (<= (aref key 0) (+ ?0 (min 9 candidate-count))))
                      (setq selection (- (aref key 0) ?1)
                            done      t))
                     ((member key `(,(char-to-string ?\C-p) [up]))
                      (setq selidx (mod (+ candidate-count (1- (or selidx 0)))
                                        candidate-count)))
                     ((member key `(,(char-to-string ?\C-n) [down]))
                      (setq selidx (mod (1+ (or selidx -1)) candidate-count)))
                     ((member key `(,(char-to-string ?\f))))
                     ((member key `(,(char-to-string ?\r) [return]))
                      (setq selection selidx
                            done      t))
                     (t (setq done t)))))
        (dropdown-list-hide)
        (and temp-buffer (kill-buffer temp-buffer)))
      ;;     (when selection
      ;;       (message "your selection => %d: %s" selection (nth selection candidates))
      ;;       (sit-for 1))
      selection)))

(defun define-key* (keymap key command)
  "Add COMMAND to the multiple-command binding of KEY in KEYMAP.
Use multiple times to bind different COMMANDs to the same KEY."
  (define-key keymap key (combine-command command (lookup-key keymap key))))

(defun combine-command (command defs)
  "$$$$$ FIXME - no doc string"
  (cond ((null defs) command)
        ((and (listp defs)
              (eq 'lambda (car defs))
              (= (length defs) 4)
              (listp (fourth defs))
              (eq 'command-selector (car (fourth defs))))
         (unless (member `',command (cdr (fourth defs)))
           (setcdr (fourth defs) (nconc (cdr (fourth defs)) `(',command))))
         defs)
        (t
         `(lambda () (interactive) (command-selector ',defs ',command)))))

(defvar command-selector-last-command nil "$$$$$ FIXME - no doc string")

(defun command-selector (&rest candidates)
  "$$$$$ FIXME - no doc string"
  (if (and (eq last-command this-command) command-selector-last-command)
      (call-interactively command-selector-last-command)
    (let* ((candidate-strings
            (mapcar (lambda (candidate)
                      (format "%s" (if (symbolp candidate)
                                       candidate
                                     (let ((s (format "%s" candidate)))
                                       (if (>= (length s) 7)
                                           (concat (substring s 0 7) "...")
                                         s)))))
                    candidates))
           (selection (dropdown-list candidate-strings)))
      (when selection
        (let ((cmd (nth selection candidates)))
          (call-interactively cmd)
          (setq command-selector-last-command cmd))))))

;;;;;;;;;;;;;;;;;;;;

(provide 'dropdown-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropdown-list.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas/initialize)
;;; snippets for text-mode
(yas/define-snippets 'text-mode
'(
  ("time" "`(current-time-string)`" "(current time)" nil)
  ("email" "`user-mail-address`" "(user's email)" nil)
  )
nil)

;;; snippets for cc-mode
(yas/define-snippets 'cc-mode
'(
  ("struct" "struct ${1:name}
{
    $0
};" "struct ... { ... }" nil)
  ("once" "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
#define $1

$0

#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil)
  ("main" "int main(int argc, char *argv[])
{
    $0
    return 0;
}
" "int main(argc, argv) { ... }" nil)
  ("inc.1" "#include <$1>
" "#include <...>" nil)
  ("inc" "#include \"$1\"
" "#include \"...\"" nil)
  ("if" "if (${1:condition})
{
    $0
}" "if (...) { ... }" nil)
  ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:++i})
{
    $0
}" "for (...; ...; ...) { ... }" nil)
  ("do" "do
{
    $0
} while (${1:condition});" "do { ... } while (...)" nil)
  )
'text-mode)

;;; snippets for c++-mode
(yas/define-snippets 'c++-mode
'(
  ("using" "using namespace ${std};
$0" "using namespace ... " nil)
  ("ns" "namespace " "namespace ..." nil)
  ("class" "class ${1:Name}
{
public:
    $1($2);
    virtual ~$1();
};" "class ... { ... }" nil)
  ("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil)
  )
'cc-mode)

;;; snippets for c-mode
(yas/define-snippets 'c-mode
'(
  ("fopen" "FILE *${fp} = fopen(${\"file\"}, \"${r}\");
" "FILE *fp = fopen(..., ...);" nil)
  )
'cc-mode)

;;; snippets for css-mode
(yas/define-snippets 'css-mode
'(
  ("pad.top" "padding-top: $1;
" "padding-top: ..." nil)
  ("pad.right" "padding-right: $1;
" "padding-right: ..." nil)
  ("pad.padding" "padding: ${top} ${right} ${bottom} ${left};
" "padding: top right bottom left" nil)
  ("pad.pad" "padding: $1;
" "padding: ..." nil)
  ("pad.left" "padding-left: $1;
" "padding-left: ..." nil)
  ("pad.bottom" "padding-bottom: $1;
" "padding-bottom: ..." nil)
  ("mar.top" "margin-top: $1;
" "margin-top: ..." nil)
  ("mar.right" "margin-right: $1;
" "margin-right: ..." nil)
  ("mar.margin" "margin: ${top} ${right} ${bottom} ${left};
" "margin top right bottom left" nil)
  ("mar.mar" "margin: $1;
" "margin: ..." nil)
  ("mar.left" "margin-left: $1;
" "margin-left: ..." nil)
  ("mar.bottom" "margin-bottom: $1;
" "margin-bottom: ..." nil)
  ("fs" "font-size: ${12px};
" "font-size: ..." nil)
  ("ff" "font-family: $1;
" "font-family: ..." nil)
  ("disp.none" "dislpay: none;
" "display: none" nil)
  ("disp.inline" "dislpay: inline;
" "display: inline" nil)
  ("disp.block" "dislpay: block;
" "display: block" nil)
  ("cl" "clear: $1;
" "clear: ..." nil)
  ("bor" "border: ${1:1px} ${2:solid} #${3:999};" "border size style color" nil)
  ("bg.1" "background-image: url($1);" "background-image: ..." nil)
  ("bg" "background-color: #${1:DDD};" "background-color: ..." nil)
  )
'text-mode)

;;; snippets for f90-mode
(yas/define-snippets 'f90-mode
'(
  ("wr" "write (${1:*},${2:*}) $0
" "write (*,*)" nil)
  ("su" "subroutine $0
" "subroutine" nil)
  ("st" "structure $0
" "structure" nil)
  ("re" "read (${1:*},${2:*}) $0
" "read (*,*)" nil)
  ("pr" "program ${1:name}
  $0
end program ${1:name}
" "program ... end program ..." nil)
  ("pa" "parameter $0
" "parameter" nil)
  ("l" "logical $0
" "logical" nil)
  ("ir" "implicit real $0
" "implicit real" nil)
  ("intr" "intrinsic $0
" "intrinsic" nil)
  ("inc" "include $0
" "include" nil)
  ("in" "implicit none
" "implicit none" nil)
  ("il" "implicit logical $0
" "implicit logical" nil)
  ("ii" "implicit integer $0
" "implicit integer " nil)
  ("if" "if ( ${1:condition} ) then
   $0
end if
" "if then end if" nil)
  ("ich" "implicit character $0
" "implicit character" nil)
  ("ic" "implicit complex $0
" "implicit complex" nil)
  ("ib" "implicit byte $0
" "implicit byte" nil)
  ("eq" "equivalence $0
" "equivalence" nil)
  ("dp" "double precision $0
" "double precision" nil)
  ("do" "do while (${1:condition})
   $0
end do
" "do while (...) end do" nil)
  ("dc" "double complex $0
" "double complex" nil)
  ("cx" "complex $0
" "complex" nil)
  ("ch" "character $0
" "character" nil)
  ("c" "continue $0
" "continue" nil)
  ("bd" "block data $0
" "block data" nil)
  ("au" "automatic $0 
" "automatic" nil)
  )
'text-mode)

;;; snippets for html-mode
(yas/define-snippets 'html-mode
'(
  ("ul.id" "<ul id=\"$1\">
  $0
</ul>" "<ul id=\"...\">...</ul>" nil)
  ("ul.class" "<ul class=\"$1\">
  $0
</ul>" "<ul class=\"...\">...</ul>" nil)
  ("ul" "<ul>
  $0
</ul>" "<ul>...</ul>" nil)
  ("tr" "<tr>
  $0
</tr>" "<tr>...</tr>" nil)
  ("title" "<title>$1</title>" "<title>...</title>" nil)
  ("th" "<th$1>$2</th>" "<th>...</th>" nil)
  ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil)
  ("td" "<td$1>$2</td>" "<td>...</td>" nil)
  ("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">
  $0
</table>" "<table ...>...</table>" nil)
  ("style" "<style type=\"text/css\" media=\"${1:screen}\">
  $0
</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil)
  ("span.id" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil)
  ("span.class" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil)
  ("span" "<span>$1</span>" "<span>...</span>" nil)
  ("script.javascript-src" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script> " nil)
  ("script.javascript" "<script type=\"text/javascript\">
  $0
</script>" "<script type=\"text/javascript\">...</script> " nil)
  ("quote" "<blockquote>
  $1
</blockquote>" "<blockquote>...</blockquote>" nil)
  ("pre" "<pre>
  $0
</pre>" "<pre>...</pre>" nil)
  ("p" "<p>$1</p>" "<p>...</p>" nil)
  ("ol.id" "<ol id=\"$1\">
  $0
</ol>" "<ol id=\"...\">...</ol>" nil)
  ("ol.class" "<ol class=\"$1\">
  $0
</ol>" "<ol class=\"...\">...</ol>" nil)
  ("ol" "<ol>
  $0
</ol>" "<ol>...</ol>" nil)
  ("meta.http-equiv" "<meta name=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil)
  ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil)
  ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil)
  ("link.stylesheet-ie" "<!--[if IE]>
<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />
<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil)
  ("link.stylesheet" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil)
  ("li.class" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil)
  ("li" "<li>$1</li>" "<li>...</li>" nil)
  ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil)
  ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil)
  ("html.xmlns" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">
  $0
</html>
" "<html xmlns=\"...\">...</html>" nil)
  ("html" "<html>
  $0
</html>
" "<html>...</html>" nil)
  ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil)
  ("hr" "<hr />
" "<hr />" nil)
  ("head" "<head>
  $0
</head>" "<head>...</head>" nil)
  ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil)
  ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil)
  ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil)
  ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil)
  ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil)
  ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil)
  ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">
  $0
</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil)
  ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil)
  ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil)
  ("doctype.xhtml1_1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil)
  ("doctype.xhml1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil)
  ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil)
  ("div.id-class" "<div id=\"$1\" class=\"$2\">
  $0
</div>" "<div id=\"...\" class=\"...\">...</div>" nil)
  ("div.id" "<div id=\"$1\">
  $0
</div>" "<div id=\"...\">...</div>" nil)
  ("div.class" "<div class=\"$1\">
  $0
</div>" "<div class=\"...\">...</div>" nil)
  ("div" "<div$1>$0</div>" "<div...>...</div>" nil)
  ("code.class" "<code class=\"$1\">
  $0
</code>" "<code class=\"...\">...</code>" nil)
  ("code" "<code>
  $0
</code>" "<code>...</code>" nil)
  ("br" "<br />" "<br />" nil)
  ("body" "<body$1>
  $0
</body>" "<body>...</body>" nil)
  )
'text-mode)

;;; snippets for markdown-mode
(yas/define-snippets 'markdown-mode
'(
  ("rlink" "[${1:Link Text}][$2] $0
" "Reference Link" nil)
  ("rlb" "[${1:Reference}]: ${2:URL} $3
$0
" "Reference Label" nil)
  ("rimg" "![${1:Alt Text}][$2] $0
" "Referenced Image" nil)
  ("ol" "${1:1}. ${2:Text}
${1:$(number-to-string (1+ (string-to-number text)))}. $0
" "Ordered List" nil)
  ("link" "[${1:Link Text}](${2:URL} $3) $0
" "Link" nil)
  ("img" "![${1:Alt Text}](${2:URL} $3) $0
" "Image" nil)
  ("hr.2" "
*******

$0
" "Horizontal Rule (*)" nil)
  ("hr.1" "
----------

$0
" "Horizontal Rule (-)" nil)
  ("h6" "###### ${1:Header 6} ######

$0
" "Header 6" nil)
  ("h5" "##### ${1:Header 5} #####

$0
" "Header 5" nil)
  ("h4" "#### ${1:Header 4} ####

$0
" "Header 4" nil)
  ("h3" "### ${1:Header 3} ###

$0
" "Header 3" nil)
  ("h2.2" "${1:Header 2}
${1:$(make-string (string-width text) ?\\-)}

$0
" "Header 2 (-)" nil)
  ("h2.1" "## ${1:Header 1} ##

$0
" "Header 2 (##)" nil)
  ("h1.2" "${1:Header 1}
${1:$(make-string (string-width text) ?\\=)}

$0
" "Header 1 (=)" nil)
  ("h1.1" "# ${1:Header 1} #

$0
" "Header 1 (#)" nil)
  ("`" "\\`${1:Code}\\` $0
" "Inline Code" nil)
  ("__" "**${1:Text}** $0
" "Strong" nil)
  ("_" "_${1:Text}_ $0
" "Emphasis" nil)
  ("-" "- ${1:Text}
-$0
" "Unordered List" nil)
  ("+" "+ ${1:Text}
+$0
" "Unordered List" nil)
  )
'text-mode)

;;; snippets for objc-mode
(yas/define-snippets 'objc-mode
'(
  ("prop" "- (${1:id})${2:foo}
{
    return $2;
}

- (void)set${2:$(capitalize text)}:($1)aValue
{
    [$2 autorelease];
    $2 = [aValue retain];
}
$0" "foo { ... } ; setFoo { ... }" nil)
  )
'text-mode)

;;; snippets for perl-mode
(yas/define-snippets 'perl-mode
'(
  ("xwhile" "${1:expression} while ${2:condition};" "... while ..." nil)
  ("xunless" "${1:expression} unless ${2:condition}" "... unless ..." nil)
  ("xif" "${1:expression} if ${2:condition}" "... if ..." nil)
  ("xfore" "${1:expression} foreach @${2:array};" "... foreach ..." nil)
  ("while" "while ($1) {
    $0
}" "while (...) { ... }" nil)
  ("unless" "unless ($1) {
    $0
}" "unless (...) { ... }" nil)
  ("sub" "sub ${1:function_name} {
    $0
}" "sub ... { ... }" nil)
  ("ifee" "if ($1) {
	${2:# body...}
} elsif ($3) {
	${4:# elsif...}
} else {
	${5:# else...}
}" "if, elsif, else ..." nil)
  ("ife" "if ($1) {
    $2
} else {
    $3
}" "if (...) { ... } else { ... }" nil)
  ("if" "if ($1) {
    $0
}" "if (...) { ... }" nil)
  ("fore" "foreach my \\$${1:x} (@${2:array}) {
    ${3:# body...}
}" "foreach ... { ... }" nil)
  ("for" "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {
    ${3:# body...}
}" "for (...) { ... }" nil)
  ("eval" "eval {
    ${1:# do something risky...}
};
if (\\$@) {
    ${2:# handle failure...}
}" "eval { ... } if ($@) { ... }" nil)
  )
'text-mode)

;;; snippets for cperl-mode
(yas/define-snippets 'cperl-mode
'(
  )
'perl-mode)

;;; snippets for python-mode
(yas/define-snippets 'python-mode
'(
  ("while" "while ${condition}:
    $0" "while ... : ..." nil)
  ("propsg" "def _set_${1:foo}(self, value):
    self._$1 = value

def _get_$1(self):
    return self._$1

$1 = property(_get_$1, _set_$1)

$0
" "_get_foo ... _set_foo ... foo=property(...)" nil)
  ("propg" "def _get_${1:foo}(self):
    return self._$1

$1 = property(_get_$1)

$0
" "_get_foo ... foo=property(...)" nil)
  ("ifmain" "if __name__ == '__main__':
    $0" "if __name__ == '__main__': ..." nil)
  ("for" "for ${var} in ${collection}:
    $0" "for ... in ... : ..." nil)
  ("defm" "def ${1:name}(self, $2):
    \"\"\"$3
    ${2:$
    (let* ((indent
            (concat \"\\n\" (make-string (current-column) 32)))
           (args
            (mapconcat
             '(lambda (x)
                (if (not (string= (nth 0 x) \"\"))
                    (concat \"- \" (char-to-string 96) (nth 0 x)
                            (char-to-string 96) \":\")))
             (mapcar
              '(lambda (x)
                 (mapcar
                  '(lambda (x)
                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                  x))
              (mapcar '(lambda (x) (split-string x \"=\"))
                      (split-string text \",\")))
             indent)))
      (if (string= args \"\")
          (make-string 3 34)
        (mapconcat
         'identity
         (list \"\" \"Arguments:\" args (make-string 3 34))
         indent)))
    }
    $0
" "defm" nil)
  ("def" "def ${1:name}($2):
    \"\"\"$3
    ${2:$
    (let* ((indent
            (concat \"\\n\" (make-string (current-column) 32)))
           (args
            (mapconcat
             '(lambda (x)
                (if (not (string= (nth 0 x) \"\"))
                    (concat \"- \" (char-to-string 96) (nth 0 x)
                            (char-to-string 96) \":\")))
             (mapcar
              '(lambda (x)
                 (mapcar
                  '(lambda (x)
                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                  x))
              (mapcar '(lambda (x) (split-string x \"=\"))
                      (split-string text \",\")))
             indent)))
      (if (string= args \"\")
          (make-string 3 34)
        (mapconcat
         'identity
         (list \"\" \"Arguments:\" args (make-string 3 34))
         indent)))
    }
    $0
" "def" nil)
  ("class" "class ${1:ClassName}(${2:object}):
    \"\"\"$3
    \"\"\"

    def __init__(self, $4):
        \"\"\"$5
        ${4:$
        (let* ((indent
                (concat \"\\n\" (make-string (current-column) 32)))
               (args
                (mapconcat
                 '(lambda (x)
                    (if (not (string= (nth 0 x) \"\"))
                        (concat \"- \" (char-to-string 96) (nth 0 x)
                                (char-to-string 96) \":\")))
                 (mapcar
                  '(lambda (x)
                     (mapcar
                      (lambda (x)
                        (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                         (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x))) x))
                  (mapcar '(lambda (x) (split-string x \"=\"))
                          (split-string text \",\")))
                 indent)))
          (if (string= args \"\")
              (make-string 3 34)
            (mapconcat
             'identity
             (list \"\" \"Arguments:\" args (make-string 3 34))
             indent)))
        }
        ${4:$
        (mapconcat
         '(lambda (x)
            (if (not (string= (nth 0 x) \"\"))
                (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))
         (mapcar
          '(lambda (x)
             (mapcar
              '(lambda (x)
                 (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                  (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
              x))
          (mapcar '(lambda (x) (split-string x \"=\"))
                  (split-string text \",\")))
         (concat \"\\n\" (make-string (current-column) 32)))
        }
        $0
" "class" nil)
  ("__" "__${init}__" "__...__" nil)
  )
'text-mode)

;;; snippets for rst-mode
(yas/define-snippets 'rst-mode
'(
  ("tit" "${1:$(make-string (string-width text) ?\\=)}
${1:Title}
${1:$(make-string (string-width text) ?\\=)}

$0" "Document title" nil)
  ("sec" "${1:Section}
${1:$(make-string (string-width text) ?\\-)}

$0" "Section title" nil)
  ("chap" "${1:Chapter}
${1:$(make-string (string-width text) ?\\=)}

$0" "Chapter title" nil)
  )
'text-mode)

;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
'(
  ("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil)
  ("y" ":yields: $0" ":yields: arguments (rdoc)" nil)
  ("w" "attr_writer :" "attr_writer ..." nil)
  ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil)
  ("rw" "attr_accessor :" "attr_accessor ..." nil)
  ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil)
  ("req" "require \"$0\"" "require \"...\"" nil)
  ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil)
  ("rb" "#!/usr/bin/ruby -wKU
" "/usr/bin/ruby -wKU" nil)
  ("r" "attr_reader :" "attr_reader ..." nil)
  ("mm" "def method_missing(method, *args)
  $0
end" "def method_missing ... end" nil)
  ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil)
  ("ife" "if ${1:condition}
  $2
else
  $3
end" "if ... else ... end" nil)
  ("if" "if ${1:condition}
  $0
end" "if ... end" nil)
  ("forin" "for ${1:element} in ${2:collection}
  $0
end" "for ... in ...; ... end" nil)
  ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil)
  ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil)
  ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil)
  ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil)
  ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil)
  ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil)
  ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil)
  ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil)
  ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil)
  ("cls" "class ${Name}
  $0
end" "class ... end" nil)
  ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil)
  ("cla" "class << ${self}
  $0
end" "class << self ... end" nil)
  ("case" "case ${1:object}
when ${2:condition}
  $0
end" "case ... end" nil)
  ("bm" "Benchmark.bmbm(${1:10}) do |x|
  $0
end" "Benchmark.bmbm(...) do ... end" nil)
  ("app" "if __FILE__ == $PROGRAM_NAME
  $0
end" "if __FILE__ == $PROGRAM_NAME ... end" nil)
  ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil)
  ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil)
  ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil)
  ("Comp" "include Comparable

def <=> other
  $0
end" "include Comparable; def <=> ... end" nil)
  ("=b" "=begin rdoc
  $0
=end" "=b" nil)
  ("#" "# => " "# =>" nil)
  )
'text-mode)

(provide 'yasnippet-bundle)

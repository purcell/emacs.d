;;; mc-mark-more.el

;; Copyright (C) 2012-2016 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: editing cursors

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

;;; Commentary:

;; This file contains functions to mark more parts of the buffer.
;; See ./features/mark-more.feature for examples.

;; Please see multiple-cursors.el for more commentary.

;;; Code:

(require 'multiple-cursors-core)
(require 'thingatpt)

(defun mc/cursor-end (cursor)
  (if (overlay-get cursor 'mark-active)
      (max (overlay-get cursor 'point)
           (overlay-get cursor 'mark))
    (overlay-get cursor 'point)))

(defun mc/cursor-beg (cursor)
  (if (overlay-get cursor 'mark-active)
      (min (overlay-get cursor 'point)
           (overlay-get cursor 'mark))
    (overlay-get cursor 'point)))

(defun mc/furthest-region-end ()
  (let ((end (max (mark) (point))))
    (mc/for-each-fake-cursor
     (setq end (max end (mc/cursor-end cursor))))
    end))

(defun mc/first-region-start ()
  (let ((beg (min (mark) (point))))
    (mc/for-each-fake-cursor
     (setq beg (min beg (mc/cursor-beg cursor))))
    beg))

(defun mc/furthest-cursor-before-point ()
  (let ((beg (if mark-active (min (mark) (point)) (point)))
	furthest)
    (mc/for-each-fake-cursor
     (when (< (mc/cursor-beg cursor) beg)
       (setq beg (mc/cursor-beg cursor))
       (setq furthest cursor)))
    furthest))

(defun mc/furthest-cursor-after-point ()
  (let ((end (if mark-active (max (mark) (point)) (point)))
	furthest)
    (mc/for-each-fake-cursor
     (when (> (mc/cursor-end cursor) end)
       (setq end (mc/cursor-end cursor))
       (setq furthest cursor)))
    furthest))

(defun mc/fake-cursor-at-point (&optional point)
  "Return the fake cursor with its point right at POINT (defaults
to (point)), or nil."
  (setq point (or point (point)))
  (let ((cursors (mc/all-fake-cursors))
        (c nil))
    (catch 'found
      (while (setq c (pop cursors))
        (when (eq (marker-position (overlay-get c 'point))
                  point)
          (throw 'found c))))))

(defun mc/region-strings ()
  (let ((strings (list (buffer-substring-no-properties (point) (mark)))))
    (mc/for-each-fake-cursor
     (add-to-list 'strings (buffer-substring-no-properties
                            (mc/cursor-beg cursor)
                            (mc/cursor-end cursor))))
    strings))

(defvar mc/enclose-search-term nil
  "How should mc/mark-more-* search for more matches?

Match everything: nil
Match only whole words: 'words
Match only whole symbols: 'symbols

Use like case-fold-search, don't recommend setting it globally.")

(defun mc/mark-more-like-this (skip-last direction)
  (let ((case-fold-search nil)
        (re (regexp-opt (mc/region-strings) mc/enclose-search-term))
        (point-out-of-order (cl-ecase direction
                              (forwards       (< (point) (mark)))
                              (backwards (not (< (point) (mark))))))
        (furthest-cursor (cl-ecase direction
                           (forwards  (mc/furthest-cursor-after-point))
                           (backwards (mc/furthest-cursor-before-point))))
        (start-char (cl-ecase direction
                      (forwards  (mc/furthest-region-end))
                      (backwards (mc/first-region-start))))
        (search-function (cl-ecase direction
                           (forwards  'search-forward-regexp)
                           (backwards 'search-backward-regexp)))
        (match-point-getter (cl-ecase direction
                              (forwards 'match-beginning)
                              (backwards 'match-end))))
    (if (and skip-last (not furthest-cursor))
        (error "No cursors to be skipped")
      (mc/save-excursion
       (goto-char start-char)
       (when skip-last
         (mc/remove-fake-cursor furthest-cursor))
       (if (funcall search-function re nil t)
           (progn
             (push-mark (funcall match-point-getter 0))
             (when point-out-of-order
               (exchange-point-and-mark))
             (mc/create-fake-cursor-at-point))
         (error "no more matches found."))))))

;;;###autoload
(defun mc/mark-next-like-this (arg)
  "Find and mark the next part of the buffer matching the currently active region
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-after-point)))
	(if cursor
	    (mc/remove-fake-cursor cursor)
	  (error "No cursors to be unmarked")))
    (if (region-active-p)
        (mc/mark-more-like-this (= arg 0) 'forwards)
      (mc/mark-lines arg 'forwards)))
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun mc/mark-next-like-this-word (arg)
  "Find and mark the next part of the buffer matching the currently active region
If no region is active, mark the word at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-after-point)))
	(if cursor
	    (mc/remove-fake-cursor cursor)
	  (error "No cursors to be unmarked")))
    (if (region-active-p)
        (mc/mark-more-like-this (= arg 0) 'forwards)
      (mc--select-thing-at-point 'word)
      (mc/mark-more-like-this (= arg 0) 'forwards)))
  (mc/maybe-multiple-cursors-mode))

(defun mc/mark-next-like-this-symbol (arg)
  "Find and mark the next part of the buffer matching the currently active region
If no region is active, mark the symbol at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-after-point)))
	(if cursor
	    (mc/remove-fake-cursor cursor)
	  (error "No cursors to be unmarked")))
    (if (region-active-p)
        (mc/mark-more-like-this (= arg 0) 'forwards)
      (mc--select-thing-at-point 'symbol)
      (mc/mark-more-like-this (= arg 0) 'forwards)))
  (mc/maybe-multiple-cursors-mode))


;;;###autoload
(defun mc/mark-next-word-like-this (arg)
  "Find and mark the next word of the buffer matching the currently active region
The matching region must be a whole word to be a match
If no region is active, mark the symbol at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (let ((mc/enclose-search-term 'words))
    (mc/mark-next-like-this arg)))

;;;###autoload
(defun mc/mark-next-symbol-like-this (arg)
  "Find and mark the next symbol of the buffer matching the currently active region
The matching region must be a whole symbol to be a match
If no region is active, mark the symbol at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (let ((mc/enclose-search-term 'symbols))
    (mc/mark-next-like-this arg)))

;;;###autoload
(defun mc/mark-previous-like-this (arg)
  "Find and mark the previous part of the buffer matching the currently active region
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-before-point)))
	(if cursor
	    (mc/remove-fake-cursor cursor)
	  (error "No cursors to be unmarked")))
    (if (region-active-p)
        (mc/mark-more-like-this (= arg 0) 'backwards)
      (mc/mark-lines arg 'backwards)))
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun mc/mark-previous-like-this-word (arg)
  "Find and mark the previous part of the buffer matching the currently active region
If no region is active, mark the word at the point and find the previous match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark previous."
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-after-point)))
	(if cursor
	    (mc/remove-fake-cursor cursor)
	  (error "No cursors to be unmarked")))
    (if (region-active-p)
        (mc/mark-more-like-this (= arg 0) 'backwards)
      (mc--select-thing-at-point 'word)
      (mc/mark-more-like-this (= arg 0) 'backwards)))
  (mc/maybe-multiple-cursors-mode))

(defun mc/mark-previous-like-this-symbol (arg)
  "Find and mark the previous part of the buffer matching the currently active region
If no region is active, mark the symbol at the point and find the previous match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark previous."
  (interactive "p")
  (if (< arg 0)
      (let ((cursor (mc/furthest-cursor-after-point)))
	(if cursor
	    (mc/remove-fake-cursor cursor)
	  (error "No cursors to be unmarked")))
    (if (region-active-p)
        (mc/mark-more-like-this (= arg 0) 'backwards)
      (mc--select-thing-at-point 'symbol)
      (mc/mark-more-like-this (= arg 0) 'backwards)))
  (mc/maybe-multiple-cursors-mode))


;;;###autoload
(defun mc/mark-previous-word-like-this (arg)
  "Find and mark the previous part of the buffer matching the currently active region
The matching region must be a whole word to be a match
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (let ((mc/enclose-search-term 'words))
    (mc/mark-previous-like-this arg)))

;;;###autoload
(defun mc/mark-previous-symbol-like-this (arg)
  "Find and mark the previous part of the buffer matching the currently active region
The matching region must be a whole symbol to be a match
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (let ((mc/enclose-search-term 'symbols))
    (mc/mark-previous-like-this arg)))

(defun mc/mark-lines (num-lines direction)
  (dotimes (i (if (= num-lines 0) 1 num-lines))
    (mc/save-excursion
     (let ((furthest-cursor (cl-ecase direction
			      (forwards  (mc/furthest-cursor-after-point))
			      (backwards (mc/furthest-cursor-before-point)))))
       (when (overlayp furthest-cursor)
         (goto-char (overlay-get furthest-cursor 'point))
         (when (= num-lines 0)
           (mc/remove-fake-cursor furthest-cursor))))
     (cl-ecase direction
       (forwards (next-logical-line 1 nil))
       (backwards (previous-logical-line 1 nil)))
     (mc/create-fake-cursor-at-point))))

;;;###autoload
(defun mc/mark-next-lines (arg)
  (interactive "p")
  (mc/mark-lines arg 'forwards)
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun mc/mark-previous-lines (arg)
  (interactive "p")
  (mc/mark-lines arg 'backwards)
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun mc/unmark-next-like-this ()
  "Deselect next part of the buffer matching the currently active region."
  (interactive)
  (mc/mark-next-like-this -1))

;;;###autoload
(defun mc/unmark-previous-like-this ()
  "Deselect prev part of the buffer matching the currently active region."
  (interactive)
  (mc/mark-previous-like-this -1))

;;;###autoload
(defun mc/skip-to-next-like-this ()
  "Skip the current one and select the next part of the buffer matching the currently active region."
  (interactive)
  (mc/mark-next-like-this 0))

;;;###autoload
(defun mc/skip-to-previous-like-this ()
  "Skip the current one and select the prev part of the buffer matching the currently active region."
  (interactive)
  (mc/mark-previous-like-this 0))

;;;###autoload
(defun mc/mark-all-like-this ()
  "Find and mark all the parts of the buffer matching the currently active region"
  (interactive)
  (unless (region-active-p)
    (error "Mark a region to match first."))
  (mc/remove-fake-cursors)
  (let ((master (point))
        (case-fold-search nil)
        (point-first (< (point) (mark)))
        (re (regexp-opt (mc/region-strings) mc/enclose-search-term)))
    (mc/save-excursion
     (goto-char 0)
     (while (search-forward-regexp re nil t)
       (push-mark (match-beginning 0))
       (when point-first (exchange-point-and-mark))
       (unless (= master (point))
         (mc/create-fake-cursor-at-point))
       (when point-first (exchange-point-and-mark)))))
  (if (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)
    (multiple-cursors-mode 0)))

(defun mc--select-thing-at-point (thing)
  (let ((bound (bounds-of-thing-at-point thing)))
    (when bound
      (set-mark (car bound))
      (goto-char (cdr bound))
      bound)))

(defun mc--select-thing-at-point-or-bark (thing)
  (unless (or (region-active-p) (mc--select-thing-at-point thing))
    (error "Mark a region or set cursor on a %s." thing)))

;;;###autoload
(defun mc/mark-all-words-like-this ()
  (interactive)
  (mc--select-thing-at-point-or-bark 'word)
  (let ((mc/enclose-search-term 'words))
    (mc/mark-all-like-this)))

;;;###autoload
(defun mc/mark-all-symbols-like-this ()
  (interactive)
  (mc--select-thing-at-point-or-bark 'symbol)
  (let ((mc/enclose-search-term 'symbols))
    (mc/mark-all-like-this)))

;;;###autoload
(defun mc/mark-all-in-region (beg end &optional search)
  "Find and mark all the parts in the region matching the given search"
  (interactive "r")
  (let ((search (or search (read-from-minibuffer "Mark all in region: ")))
        (case-fold-search nil))
    (if (string= search "")
        (message "Mark aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (while (search-forward search end t)
          (push-mark (match-beginning 0))
          (mc/create-fake-cursor-at-point))
        (let ((first (mc/furthest-cursor-before-point)))
          (if (not first)
              (error "Search failed for %S" search)
            (mc/pop-state-from-overlay first)))
        (if (> (mc/num-cursors) 1)
            (multiple-cursors-mode 1)
          (multiple-cursors-mode 0))))))

;;;###autoload
(defun mc/mark-all-in-region-regexp (beg end)
  "Find and mark all the parts in the region matching the given regexp."
  (interactive "r")
  (let ((search (read-regexp "Mark regexp in region: "))
        (case-fold-search nil))
    (if (string= search "")
        (message "Mark aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (let ((lastmatch))
          (while (and (< (point) end) ; can happen because of (forward-char)
                      (search-forward-regexp search end t))
            (push-mark (match-beginning 0))
            (mc/create-fake-cursor-at-point)
            (setq lastmatch (point))
            (when (= (point) (match-beginning 0))
              (forward-char)))
          (unless lastmatch
            (error "Search failed for %S" search)))
          (goto-char (match-end 0))
        (if (< (mc/num-cursors) 3)
            (multiple-cursors-mode 0)
          (mc/pop-state-from-overlay (mc/furthest-cursor-before-point))
          (multiple-cursors-mode 1))))))

(when (not (fboundp 'set-temporary-overlay-map))
  ;; Backport this function from newer emacs versions
  (defun set-temporary-overlay-map (map &optional keep-pred)
    "Set a new keymap that will only exist for a short period of time.
The new keymap to use must be given in the MAP variable. When to
remove the keymap depends on user input and KEEP-PRED:

- if KEEP-PRED is nil (the default), the keymap disappears as
  soon as any key is pressed, whether or not the key is in MAP;

- if KEEP-PRED is t, the keymap disappears as soon as a key *not*
  in MAP is pressed;

- otherwise, KEEP-PRED must be a 0-arguments predicate that will
  decide if the keymap should be removed (if predicate returns
  nil) or kept (otherwise). The predicate will be called after
  each key sequence."

    (let* ((clearfunsym (make-symbol "clear-temporary-overlay-map"))
           (overlaysym (make-symbol "t"))
           (alist (list (cons overlaysym map)))
           (clearfun
            `(lambda ()
               (unless ,(cond ((null keep-pred) nil)
                              ((eq t keep-pred)
                               `(eq this-command
                                    (lookup-key ',map
                                                (this-command-keys-vector))))
                              (t `(funcall ',keep-pred)))
                 (remove-hook 'pre-command-hook ',clearfunsym)
                 (setq emulation-mode-map-alists
                       (delq ',alist emulation-mode-map-alists))))))
      (set overlaysym overlaysym)
      (fset clearfunsym clearfun)
      (add-hook 'pre-command-hook clearfunsym)

      (push alist emulation-mode-map-alists))))

;;;###autoload
(defun mc/mark-more-like-this-extended ()
  "Like mark-more-like-this, but then lets you adjust with arrows key.
The adjustments work like this:

   <up>    Mark previous like this and set direction to 'up
   <down>  Mark next like this and set direction to 'down

If direction is 'up:

   <left>  Skip past the cursor furthest up
   <right> Remove the cursor furthest up

If direction is 'down:

   <left>  Remove the cursor furthest down
   <right> Skip past the cursor furthest down

The bindings for these commands can be changed. See `mc/mark-more-like-this-extended-keymap'."
  (interactive)
  (mc/mmlte--down)
  (set-temporary-overlay-map mc/mark-more-like-this-extended-keymap t))

(defvar mc/mark-more-like-this-extended-direction nil
  "When using mc/mark-more-like-this-extended are we working on the next or previous cursors?")

(make-variable-buffer-local 'mc/mark-more-like-this-extended)

(defun mc/mmlte--message ()
  (if (eq mc/mark-more-like-this-extended-direction 'up)
      (message "<up> to mark previous, <left> to skip, <right> to remove, <down> to mark next")
    (message "<down> to mark next, <right> to skip, <left> to remove, <up> to mark previous")))

(defun mc/mmlte--up ()
  (interactive)
  (mc/mark-previous-like-this 1)
  (setq mc/mark-more-like-this-extended-direction 'up)
  (mc/mmlte--message))

(defun mc/mmlte--down ()
  (interactive)
  (mc/mark-next-like-this 1)
  (setq mc/mark-more-like-this-extended-direction 'down)
  (mc/mmlte--message))

(defun mc/mmlte--left ()
  (interactive)
  (if (eq mc/mark-more-like-this-extended-direction 'down)
      (mc/unmark-next-like-this)
    (mc/skip-to-previous-like-this))
  (mc/mmlte--message))

(defun mc/mmlte--right ()
  (interactive)
  (if (eq mc/mark-more-like-this-extended-direction 'up)
      (mc/unmark-previous-like-this)
    (mc/skip-to-next-like-this))
  (mc/mmlte--message))

(defvar mc/mark-more-like-this-extended-keymap (make-sparse-keymap))

(define-key mc/mark-more-like-this-extended-keymap (kbd "<up>") 'mc/mmlte--up)
(define-key mc/mark-more-like-this-extended-keymap (kbd "<down>") 'mc/mmlte--down)
(define-key mc/mark-more-like-this-extended-keymap (kbd "<left>") 'mc/mmlte--left)
(define-key mc/mark-more-like-this-extended-keymap (kbd "<right>") 'mc/mmlte--right)

(defvar mc--restrict-mark-all-to-symbols nil)

;;;###autoload
(defun mc/mark-all-like-this-dwim (arg)
  "Tries to guess what you want to mark all of.
Can be pressed multiple times to increase selection.

With prefix, it behaves the same as original `mc/mark-all-like-this'"
  (interactive "P")
  (if arg
      (mc/mark-all-like-this)
    (if (and (not (use-region-p))
             (derived-mode-p 'sgml-mode)
             (mc--on-tag-name-p))
        (mc/mark-sgml-tag-pair)
      (let ((before (mc/num-cursors)))
        (unless (eq last-command 'mc/mark-all-like-this-dwim)
          (setq mc--restrict-mark-all-to-symbols nil))
        (unless (use-region-p)
          (mc--mark-symbol-at-point)
          (setq mc--restrict-mark-all-to-symbols t))
        (if mc--restrict-mark-all-to-symbols
            (mc/mark-all-symbols-like-this-in-defun)
          (mc/mark-all-like-this-in-defun))
        (when (<= (mc/num-cursors) before)
          (if mc--restrict-mark-all-to-symbols
              (mc/mark-all-symbols-like-this)
            (mc/mark-all-like-this)))
        (when (<= (mc/num-cursors) before)
          (mc/mark-all-like-this))))))

;;;###autoload
(defun mc/mark-all-dwim (arg)
  "Tries even harder to guess what you want to mark all of.

If the region is active and spans multiple lines, it will behave
as if `mc/mark-all-in-region'. With the prefix ARG, it will call
`mc/edit-lines' instead.

If the region is inactive or on a single line, it will behave like
`mc/mark-all-like-this-dwim'."
  (interactive "P")
  (if (and (use-region-p)
           (not (> (mc/num-cursors) 1))
           (not (= (line-number-at-pos (region-beginning))
                   (line-number-at-pos (region-end)))))
      (if arg
          (call-interactively 'mc/edit-lines)
       (call-interactively 'mc/mark-all-in-region))
    (progn
      (setq this-command 'mc/mark-all-like-this-dwim)
      (mc/mark-all-like-this-dwim arg))))

(defun mc--in-defun ()
  (bounds-of-thing-at-point 'defun))

;;;###autoload
(defun mc/mark-all-like-this-in-defun ()
  "Mark all like this in defun."
  (interactive)
  (if (mc--in-defun)
      (save-restriction
        (widen)
        (narrow-to-defun)
        (mc/mark-all-like-this))
    (mc/mark-all-like-this)))

;;;###autoload
(defun mc/mark-all-words-like-this-in-defun ()
  "Mark all words like this in defun."
  (interactive)
  (mc--select-thing-at-point-or-bark 'word)
  (if (mc--in-defun)
      (save-restriction
        (widen)
        (narrow-to-defun)
        (mc/mark-all-words-like-this))
    (mc/mark-all-words-like-this)))

;;;###autoload
(defun mc/mark-all-symbols-like-this-in-defun ()
  "Mark all symbols like this in defun."
  (interactive)
  (mc--select-thing-at-point-or-bark 'symbol)
  (if (mc--in-defun)
      (save-restriction
        (widen)
        (narrow-to-defun)
        (mc/mark-all-symbols-like-this))
    (mc/mark-all-symbols-like-this)))

(defun mc--mark-symbol-at-point ()
  "Select the symbol under cursor"
  (interactive)
  (when (not (use-region-p))
    (let ((b (bounds-of-thing-at-point 'symbol)))
      (goto-char (car b))
      (set-mark (cdr b)))))

(defun mc--get-nice-sgml-context ()
  (car
   (last
    (progn
      (when (looking-at "<") (forward-char 1))
      (when (looking-back ">") (forward-char -1))
      (sgml-get-context)))))

(defun mc--on-tag-name-p ()
  (let* ((context (save-excursion (mc--get-nice-sgml-context)))
         (tag-name-len (length (aref context 4)))
         (beg (aref context 2))
         (end (+ beg tag-name-len (if (eq 'open (aref context 1)) 1 3))))
    (and context
         (>= (point) beg)
         (<= (point) end))))

;;;###autoload
(defun mc/toggle-cursor-on-click (event)
  "Add a cursor where you click, or remove a fake cursor that is
already there."
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((position (event-end event)))
    (if (not (windowp (posn-window position)))
        (error "Position not in text area of window"))
    (select-window (posn-window position))
    (let ((pt (posn-point position)))
      (if (numberp pt)
          ;; is there a fake cursor with the actual *point* right where we are?
          (let ((existing (mc/fake-cursor-at-point pt)))
            (if existing
                (mc/remove-fake-cursor existing)
              (save-excursion
                (goto-char pt)
                (mc/create-fake-cursor-at-point))))))
    (mc/maybe-multiple-cursors-mode)))

;;;###autoload
(defalias 'mc/add-cursor-on-click 'mc/toggle-cursor-on-click)

;;;###autoload
(defun mc/mark-sgml-tag-pair ()
  "Mark the tag we're in and its pair for renaming."
  (interactive)
  (when (not (mc--inside-tag-p))
    (error "Place point inside tag to rename."))
  (let ((context (mc--get-nice-sgml-context)))
    (if (looking-at "</")
        (setq context (car (last (sgml-get-context)))))
    (goto-char (aref context 2))
    (let* ((tag-name (aref context 4))
           (num-chars (length tag-name))
           (master-start (1+ (point)))
           (mirror-end (save-excursion
                         (sgml-skip-tag-forward 1)
                         (1- (point)))))
      (goto-char (- mirror-end num-chars))
      (set-mark mirror-end)
      (mc/create-fake-cursor-at-point)
      (goto-char master-start)
      (set-mark (+ (point) num-chars))))
  (mc/maybe-multiple-cursors-mode))

(defun mc--inside-tag-p ()
  (save-excursion
    (not (null (sgml-get-context)))))

(provide 'mc-mark-more)

;;; mc-mark-more.el ends here

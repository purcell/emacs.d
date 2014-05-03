;;; visual-regexp.el --- A regexp/replace command for Emacs with interactive visual feedback

;; Copyright (C) 2013 Marko Bencun

;; Author: Marko Bencun <mbencun@gmail.com>
;; URL: https://github.com/benma/visual-regexp.el/
;; Version: 0.6
;; Package-Requires: ((cl-lib "0.2"))
;; Keywords: regexp, replace, visual, feedback

;; This file is part of visual-regexp.

;; visual-regexp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; visual-regexp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with visual-regexp.  If not, see <http://www.gnu.org/licenses/>.

;;; WHAT'S NEW
;; 0.6: Distinguish prompts in vr/replace, vr/query-replace, vr/mc-mark.
;; 0.5: emulate case-conversion of replace-regexp.
;; 0.4: vr/mc-mark: interface to multiple-cursors.
;; 0.3: Use the same history as the regular Emacs replace commands; 
;; 0.2: Support for lisp expressions in the replace string, same as in (query-)replace-regexp
;; 0.1: initial release

;;; Tip Jar
;; If you found this useful, please consider donating.
;; BTC: 1BxauiLGMQPb2pavkkQkuFe5CgrGMrUat2

;;; What's This?

;; visual-regexp for Emacs is like `replace-regexp`, but with live  visual feedback directly in the buffer.
;; While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches.
;; While constructing the replacement in the minibuffer, you get live visual feedback for the replacements.
;; It can be used to replace all matches in one go (like `replace-regexp`), or a decision can be made on each match (like `query-replace-regexp`).
;; Thanks to Detlev Zundel for his re-builder.

;;; Where does visual-regexp come from?
;;
;; I was not happy with the way I used emacs' replace-regexp before. Constructing the regular expression is error prone and emacs' regular expressions are limited
;; (for example, no lookaheads, named groups, etc.).
;; Using re-builder to interactively build regular expressions was a step into the right direction, but manually copying over the regexp
;; to the minibuffer is cumbersome.
;; Using the idea of interactive feedback of re-builder, this package makes it possible to use just the minibuffer to construct (with live visual feedback) the regexp and replacement,
;; using Emacs style regular expressions, or optionally, regular expressions powered by other (mode modern) engines, for the replacement. For the latter part, see the package visual-regexp-steroids.

;;; Installation

;; If you are using Emacs 24, you can get visual-regexp from [melpa](http://melpa.milkbox.net/) with the package manager.
;; Add the following code to your init file. Of course you can select your own key bindings.
;; ----------------------------------------------------------
;; (add-to-list 'load-path "folder-in-which-visual-regexp-files-are-in/") ;; if the files are not already in the load path
;; (require 'visual-regexp)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
;; ;; if you use multiple-cursors, this is for you:
;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; ----------------------------------------------------------
;; To customize, use `M-x customize-group [RET] visual-regexp`.

;;; Code:
(unless (fboundp 'make-overlay)
  (require 'overlay))

;; cl is used for the (loop ...) macro
(require 'cl-lib)

;;; faces

(defface vr/match-0
  '((((class color) (background light))
     :background "lightblue")
    (((class color) (background dark))
     :background "steelblue4")
    (t
     :inverse-video t))
  "First face for displaying a whole match."
  :group 'visual-regexp)

(defface vr/match-1
  '((((class color) (background light))
     :background "pale turquoise")
    (((class color) (background dark))
     :background "dodgerblue4")
    (t
     :inverse-video t))
  "Second face for displaying a whole match."
  :group 'visual-regexp)

(defface vr/group-0
  '((((class color) (background light))
     :background "aquamarine")
    (((class color) (background dark))
     :background "blue3")
    (t
     :inverse-video t))
  "First face for displaying a matching group."
  :group 'visual-regexp)

(defface vr/group-1
  '((((class color) (background light))
     :background "springgreen")
    (((class color) (background dark))
     :background "chartreuse4")
    (t
     :inverse-video t))
  "Second face for displaying a matching group."
  :group 'visual-regexp)

(defface vr/group-2
  '((((min-colors 88) (class color) (background light))
     :background "yellow1")
    (((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "sienna4")
    (t
     :inverse-video t))
  "Third face for displaying a matching group."
  :group 'visual-regexp)

;;; variables

(defcustom vr/auto-show-help t
  "Show help message automatically when the minibuffer is entered."
  :type 'boolean
  :group 'visual-regexp)

(defcustom vr/default-feedback-limit 50
  "Limit number of matches shown in visual feedback.
If nil, don't limit the number of matches shown in visual feedback."
  :type 'integer
  :group 'visual-regexp)

(defcustom vr/default-replace-preview nil
  "Preview of replacement activated by default? If activated, the original is not shown alongside the replacement."
  :type 'boolean
  :group 'visual-regexp)

(defcustom vr/query-replace-from-history-variable query-replace-from-history-variable
  "History list to use for the FROM argument. The default is to use the same history as Emacs' query-replace commands."
  :type 'symbol
  :group 'visual-regexp)

(defcustom vr/query-replace-to-history-variable query-replace-to-history-variable
  "History list to use for the TO argument. The default is to use the same history as Emacs' query-replace commands."
  :type 'symbol
  :group 'visual-regexp)

(defvar vr/initialize-hook nil
  "Hook called before vr/replace and vr/query-replace")

;;; private variables

(defconst vr--match-faces '(vr/match-0 vr/match-1)
  "Faces in list for convenience")

(defconst vr--group-faces '(vr/group-0 vr/group-1 vr/group-2)
  "Faces in list for convenience")

(defconst vr--overlay-priority 1001
  "Starting priority of visual-regexp overlays.")

(defvar vr--in-minibuffer nil
  "Is visual-regexp currently being used?")

(defvar vr--calling-func nil
  "Which function invoked vr--interactive-get-args?")

(defvar vr--last-minibuffer-contents nil
  "Keeping track of minibuffer changes")

(defvar vr--target-buffer-start nil
  "Starting position in target buffer.")

(defvar vr--target-buffer-end nil
  "Ending position in target buffer.")

(defvar vr--regexp-string nil
  "Entered regexp.")

(defvar vr--replace-string nil
  "Entered replacement.")

(defvar vr--feedback-limit nil
  "Feedback limit currently in use.")

(defvar vr--replace-preview nil
  "Preview of replacement activated?")

(defvar vr--target-buffer nil
  "Buffer to which visual-regexp is applied to.")

(defvar vr--overlays (make-hash-table :test 'equal)
  "Overlays used in target buffer.")

(defvar vr--visible-overlays (list)
  "Overlays currently visible.")

(defvar vr--minibuffer-message-overlay nil)

;;; keymap

(defvar vr/minibuffer-regexp-keymap
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") 'vr--minibuffer-help)

    (define-key map (kbd "C-c a") 'vr--shortcut-toggle-limit)
    map)
  "Keymap used while using visual-regexp,")

(defvar vr/minibuffer-replace-keymap
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") 'vr--minibuffer-help)
    (define-key map (kbd "C-c m") 'vr--shortcut-show-matches)
    (define-key map (kbd "C-c p") 'vr--shortcut-toggle-preview)

    (define-key map (kbd "C-c a") 'vr--shortcut-toggle-limit)
    map)
  "Keymap used while using visual-regexp,")

;;; helper functions

(defun vr--shortcut-show-matches ()
  (interactive)
  (vr--delete-overlay-displays)
  ;; wait for any input to redisplay replacements
  (sit-for 100000000 t)
  (vr--do-replace-feedback))

(defun vr--shortcut-toggle-preview ()
  (interactive)
  (setq vr--replace-preview (not vr--replace-preview))
  (vr--update-minibuffer-prompt)
  (vr--do-replace-feedback))

(defun vr--shortcut-toggle-limit ()
  "Toggle the limit of overlays shown (default limit / no limit)"
  (interactive)
  (if vr--feedback-limit
      (setq vr--feedback-limit nil)
    (setq vr--feedback-limit vr/default-feedback-limit))
  (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
         (vr--feedback))
        ((equal vr--in-minibuffer 'vr--minibuffer-replace)
         (vr--feedback t) ;; update overlays
         (vr--do-replace-feedback))))


(defun vr--get-regexp-string ()
  (concat (if (equal vr--in-minibuffer 'vr--minibuffer-regexp)
              (minibuffer-contents-no-properties)
            vr--regexp-string)))

(defun vr--format-error (err)
  (if (eq (car err) 'error)
      (car (cdr err))
    (format "%s" err)))

;;; minibuffer functions

(defun vr--minibuffer-set-prompt (prompt)
  "Updates minibuffer prompt. Call when minibuffer is active."
  (let ((inhibit-read-only t))
    (put-text-property (point-min) (minibuffer-prompt-end) 'display prompt)))

(defun vr--set-minibuffer-prompt-regexp ()
  (cond ((equal vr--calling-func 'vr--calling-func-query-replace)
         "Query regexp: ")
        ((equal vr--calling-func 'vr--calling-func-mc-mark)
         "Mark regexp: ")
	(t
         "Regexp: ")))

(defun vr--set-minibuffer-prompt-replace ()
  (let (prefix)
    (setq prefix (cond ((equal vr--calling-func 'vr--calling-func-query-replace)
			"Query replace: ")
		       (t
			"Replace: ")))
    
    (concat prefix
          (let ((flag-infos (mapconcat 'identity
                                       (delq nil (list (when vr--replace-preview "preview")))
                                       ", ")))
            (when (not (string= "" flag-infos ))
              (format " (%s)" flag-infos)))
          (format " (%s)" (vr--get-regexp-string))
          ": ")))

(defun vr--update-minibuffer-prompt ()
  (when (and vr--in-minibuffer (minibufferp))
    (vr--minibuffer-set-prompt
     (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
            (vr--set-minibuffer-prompt-regexp))
           ((equal vr--in-minibuffer 'vr--minibuffer-replace)
            (vr--set-minibuffer-prompt-replace))))))


(defun vr--minibuffer-message (message &rest args)
  "Adaptation of minibuffer-message that does not use sit-for
to make the message disappear. The problem with this was that during sit-for,
 the cursor was shown at the beginning of the message regardless of whether
 the point was actually there or not. Workaround: we let the message stay
visible all the time in the minibuffer."
  (if (not (and vr--in-minibuffer (minibufferp (current-buffer))))
      ;; fallback
      (apply 'minibuffer-message message args)
    ;; Clear out any old echo-area message to make way for our new thing.
    (message nil)
    (setq message (concat " [" message "]"))
    (when args (setq message (apply 'format message args)))
    (unless (zerop (length message))
      ;; The current C cursor code doesn't know to use the overlay's
      ;; marker's stickiness to figure out whether to place the cursor
      ;; before or after the string, so let's spoon-feed it the pos.
      (put-text-property 0 1 'cursor t message))
    (unless (overlayp vr--minibuffer-message-overlay)
      (setq vr--minibuffer-message-overlay (make-overlay (point-max) (point-max) nil t t)))
    (move-overlay vr--minibuffer-message-overlay (point-max) (point-max))
    (overlay-put vr--minibuffer-message-overlay 'after-string message)))

(defun vr/minibuffer-help-regexp ()
  (vr--minibuffer-message (format (substitute-command-keys "\\<vr/minibuffer-regexp-keymap>\\[vr--minibuffer-help]: helps \\[vr--shortcut-toggle-limit]: toggle show all"))))

(defun vr/minibuffer-help-replace ()
  (vr--minibuffer-message (format (substitute-command-keys "\\<vr/minibuffer-replace-keymap>\\[vr--minibuffer-help]: help \\[vr--shortcut-show-matches]: show matches/groups, \\[vr--shortcut-toggle-preview]: toggle preview, \\[vr--shortcut-toggle-limit]: toggle show all"))))

(defun vr--minibuffer-help ()
  (interactive)
  (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
         (vr/minibuffer-help-regexp))
        ((equal vr--in-minibuffer 'vr--minibuffer-replace)
         (vr/minibuffer-help-replace))))

;;; overlay functions

(defun vr--get-overlay (i j)
  "i: match index, j: submatch index"
  (let (overlay)
    (setq overlay (gethash (list i j) vr--overlays))
    (unless overlay ;; create new one if overlay does not exist yet
      (setq overlay (make-overlay 0 0))
      (if (= 0 j)
          (overlay-put overlay 'face (nth (mod i (length vr--match-faces)) vr--match-faces))
        (overlay-put overlay 'face (nth (mod j (length vr--group-faces)) vr--group-faces)))
      (overlay-put overlay 'priority (+ vr--overlay-priority (if (= j 0) 0 1)))
      (overlay-put overlay 'vr-ij (list i j))
      (puthash (list i j) overlay vr--overlays))
    overlay))

(defun vr--delete-overlays ()
  "Delete all visible overlays."
  (mapc (lambda (overlay)
	  (delete-overlay overlay))
	vr--visible-overlays)
  (setq vr--visible-overlays (list)))

(defun vr--delete-overlay-display (overlay)
  (overlay-put overlay 'display nil)
  (overlay-put overlay 'after-string nil)
  (overlay-put overlay 'priority vr--overlay-priority))

(defun vr--delete-overlay-displays ()
  "Delete the display of all visible overlays. Call before vr--delete-overlays."
  (mapc (lambda (overlay)
          (cl-multiple-value-bind (i j) (overlay-get overlay 'vr-ij)
            (when (= 0 j)
              (vr--delete-overlay-display overlay))))
        vr--visible-overlays))

;;; hooks

(defun vr--update (beg end len)
  (when (and vr--in-minibuffer (minibufferp))
    ;; minibuffer-up temporarily deletes minibuffer contents before inserting new one.
    ;; don't do anything then as the messages shown by visual-regexp are irritating while browsing the history.
    (unless (and (string= "" (minibuffer-contents-no-properties))
                 (equal last-command 'previous-history-element))
      ;; do something when minibuffer contents changes
      (unless (string= vr--last-minibuffer-contents (minibuffer-contents-no-properties))
        (setq vr--last-minibuffer-contents (minibuffer-contents-no-properties))
        ;; minibuffer contents has changed, update visual feedback.
        ;; not using after-change-hook because this hook applies to the whole minibuffer, including minibuffer-messages
        ;; that disappear after a while.
        (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
               (vr--feedback))
              ((equal vr--in-minibuffer 'vr--minibuffer-replace)
               (vr--do-replace-feedback)))))))
(add-hook 'after-change-functions 'vr--update)

(defun vr--minibuffer-setup ()
  "Setup prompt and help when entering minibuffer."
  (when vr--in-minibuffer
    (progn
      (vr--update-minibuffer-prompt)
      (when vr/auto-show-help (vr--minibuffer-help)))))
(add-hook 'minibuffer-setup-hook 'vr--minibuffer-setup)

;;; helper functions

(defun vr--target-window ()
  (if vr--target-buffer
      (get-buffer-window vr--target-buffer)
    nil))

(defun vr--compose-messages (&rest msgs)
  (mapconcat 'identity (delq nil (mapcar (lambda (msg) (if (or (not msg) (string= "" msg)) nil msg)) msgs)) " - "))

;;; show feedback functions
(defun vr--feedback-function (forward feedback-limit callback)
  "Feedback function for emacs-style regexp search"
  (let ((message-line "")
        (err))
    (with-current-buffer vr--target-buffer
      (save-excursion
        (goto-char (if forward vr--target-buffer-start vr--target-buffer-end))
        (let ((i 0)
              (looping t))
          (while (and looping
                      (condition-case err
                          (if forward
                              (re-search-forward regexp-string vr--target-buffer-end t)
                            (re-search-backward regexp-string vr--target-buffer-start t))
                        ('invalid-regexp (progn (setq message-line (car (cdr err))) nil))))
            (when (or (not feedback-limit) (< i feedback-limit)) ;; let outer loop finish so we can get the matches count
              (cl-loop for (start end) on (match-data) by 'cddr
                       for j from 0 do
                       (funcall callback i j start end)))
            (when (= (match-beginning 0) (match-end 0))
              (cond ;; don't get stuck on zero-width matches
               ((and forward (> vr--target-buffer-end (point))) (forward-char))
               ((and (not forward) (< vr--target-buffer-start (point))) (backward-char))
               (t (setq looping nil))))
            (setq i (1+ i)))
          (setq message-line (format "%s matches" i)))))
    message-line))

(defun vr--feedback-match-callback (i j begin end)
  (with-current-buffer vr--target-buffer
    (save-excursion
      (when (= 0 i) ;; make first match visible
	(with-selected-window (vr--target-window)
	  (goto-char end)))
      (let ((overlay (vr--get-overlay i j)))
	(move-overlay overlay begin end vr--target-buffer)
	(if (and (= 0 j) (= begin end)) ;; empty match; indicate by a pipe
	    (overlay-put overlay 'after-string (propertize "|" 'face (nth (mod i (length vr--match-faces)) vr--match-faces) 'help-echo "empty match"))
	  (overlay-put overlay 'after-string nil))
	(setq vr--visible-overlays (cons overlay vr--visible-overlays)))
      ;; mark if we have reached the specified feedback limit
      (when (and vr--feedback-limit (= vr--feedback-limit (1+ i)) )
	(setq limit-reached t)))))

(defun vr--feedback (&optional inhibit-message)
  "Show visual feedback for matches."
  (vr--delete-overlays)
  (let ((limit-reached nil)
        message-line)
    (setq message-line
          (let ((regexp-string (vr--get-regexp-string)))
            (vr--feedback-function t vr--feedback-limit 'vr--feedback-match-callback)))
    (unless inhibit-message
      (let ((msg (vr--compose-messages message-line (when limit-reached (format "%s matches shown, hit C-c a to show all" vr--feedback-limit)))))
        (unless (string= "" msg)
          (vr--minibuffer-message msg))))))

(defun vr--get-replacement (replacement match-data i)
  (with-current-buffer vr--target-buffer
    (set-match-data match-data)
    (let*
	;; emulate case-conversion of (perform-replace)
	((case-fold-search (if (and case-fold-search search-upper-case)
			       (isearch-no-upper-case-p (vr--get-regexp-string) t)
			     case-fold-search))
	 (nocasify (not (and case-replace case-fold-search))))
      (if (stringp replacement)
	  (match-substitute-replacement replacement nocasify)
	(match-substitute-replacement (funcall (car replacement) (cdr replacement) i) nocasify)))))

(defun vr--do-replace-feedback-match-callback (replacement match-data i)
  (let ((begin (cl-first match-data))
	(end (cl-second match-data))
	(replacement (vr--get-replacement replacement match-data i)))
  (let* ((overlay (vr--get-overlay i 0))
         (empty-match (= begin end)))
    (move-overlay overlay begin end vr--target-buffer)
    (vr--delete-overlay-display overlay)
    (if (or empty-match vr--replace-preview)
        (progn
          (overlay-put overlay (if empty-match 'after-string 'display) (propertize replacement 'face (nth (mod i (length vr--match-faces)) vr--match-faces)))
          (overlay-put overlay 'priority (+ vr--overlay-priority 2)))
      (progn
        (overlay-put overlay 'after-string
                     (propertize (format " => %s" replacement) 'face (nth (mod i (length vr--match-faces)) vr--match-faces)))
        (overlay-put overlay 'priority (+ vr--overlay-priority 0)))))))

(defun vr--get-replacements (feedback feedback-limit)
  "Get replacements using emacs-style regexp."
  (let ((regexp-string (vr--get-regexp-string))
        (message-line "")
        (replacements (list))
        (err)
        (buffer-contents (with-current-buffer vr--target-buffer
                           (buffer-substring-no-properties (point-min) (point-max)))))
    (with-current-buffer vr--target-buffer
      (goto-char vr--target-buffer-start)
      (let ((i 0)
	    (looping t)
	    (limit-reached nil))
	(while (and
		looping
		(condition-case err
		    (re-search-forward regexp-string vr--target-buffer-end t)
		  ('invalid-regexp (progn (setq message-line (car (cdr err))) nil))))
	  (condition-case err
	      (progn
		(if (or (not feedback) (not feedback-limit) (< i feedback-limit))
		    (setq replacements (cons
					(let ((match-data (mapcar 'marker-position (match-data))))
					  (list (query-replace-compile-replacement replace-string t) match-data i))
					replacements))
		  (setq limit-reached t))
		(when (= (match-beginning 0) (match-end 0))
		  (if (> vr--target-buffer-end (point))
		      (forward-char) ;; don't get stuck on zero-width matches
		    (setq looping nil)))
		(setq i (1+ i)))
	    ('error (progn
		      (setq message-line (vr--format-error err))
		      (setq replacements (list))
		      (setq looping nil)))))
	(if (string= "" message-line)
	  (if feedback
	      (setq message-line (vr--compose-messages (format "%s matches" i) (when limit-reached (format "%s matches shown, hit C-c a to show all" feedback-limit))))
	    (setq message-line (format "replaced %d matches" i))))))
    (list replacements message-line)))

(defun vr--do-replace-feedback ()
  "Show visual feedback for replacements."
  (vr--feedback t) ;; only really needed when regexp has not been changed from default (=> no overlays have been created)
  (let ((replace-string (minibuffer-contents-no-properties)))
        (cl-multiple-value-bind (replacements message-line) (vr--get-replacements t vr--feedback-limit)
      ;; visual feedback for matches
      (condition-case err
	  (mapc (lambda (replacement-info) (apply 'vr--do-replace-feedback-match-callback replacement-info)) replacements)
	('error (setq message-line (vr--format-error err))))
      (unless (string= "" message-line)
        (vr--minibuffer-message message-line)))))

;;; vr/replace

(defun vr--do-replace (&optional silent)
  "Replace matches."
  (vr--delete-overlay-displays)
  (vr--delete-overlays)
  (let ((replace-string vr--replace-string))
    (cl-multiple-value-bind (replacements message-line) (vr--get-replacements nil nil)
      (let ((replace-count 0)
            (cumulative-offset 0)
            last-match-data)
        (cl-loop for replacement-info in replacements
                 for counter from 0 do
                 (setq replace-count (1+ replace-count))
                 (cl-multiple-value-bind (replacement match-data i) replacement-info
                   ;; replace match
                   (let* ((replacement (vr--get-replacement replacement match-data i))
			  (begin (cl-first match-data))
			  (end (cl-second match-data)))
                     (with-current-buffer vr--target-buffer
                       (save-excursion
                         ;; first insert, then delete
                         ;; this ensures that if we had an active region before, the replaced match is still part of the region
                         (goto-char begin)
                         (insert replacement)
                         (setq cumulative-offset (+ cumulative-offset (- (point) end)))
                         (delete-char (- end begin))))
                   (when (= 0 counter)
                     (setq last-match-data match-data))
		   )))
        (unless (or silent (string= "" message-line))
          (vr--minibuffer-message message-line))
	;; needed to correctly position the mark after query replace (finished with 'automatic ('!'))
        (set-match-data (mapcar (lambda (el) (+ cumulative-offset el)) last-match-data))
	replace-count))))

(defun vr--interactive-get-args (mode calling-func)
  "Get interactive args for the vr/replace and vr/query-replace functions."
  (unwind-protect
      (progn
        (let ((buffer-read-only t)) ;; make target buffer
          (when vr--in-minibuffer (error "visual-regexp already in use."))
	  (setq vr--calling-func calling-func)
          (setq vr--target-buffer (current-buffer))
          (setq vr--target-buffer-start (if (region-active-p)
					    (region-beginning)
					  (point)))
          (setq vr--target-buffer-end (if (region-active-p)
					  (region-end)
					(point-max)))

          (run-hooks 'vr/initialize-hook)
          (setq vr--feedback-limit vr/default-feedback-limit)

          (setq vr--replace-preview vr/default-replace-preview)

          (save-excursion
            ;; deactivate mark so that we can see our faces instead of region-face.
            (deactivate-mark)
            (progn
              (setq vr--in-minibuffer 'vr--minibuffer-regexp)
              (setq vr--last-minibuffer-contents "")
              (setq vr--regexp-string
                    (read-from-minibuffer
                     " " ;; prompt will be  set in vr--minibuffer-setup
                     nil vr/minibuffer-regexp-keymap
		     nil vr/query-replace-from-history-variable))
              ;;(setq vr--regexp-string (format "%s%s" (vr--get-regexp-modifiers-prefix) vr--regexp-string))

	      (when (equal mode 'vr--mode-regexp-replace)
		(setq vr--in-minibuffer 'vr--minibuffer-replace)
		(setq vr--last-minibuffer-contents "")
		(setq vr--replace-string
		      (read-from-minibuffer
		       " " ;; prompt will be  set in vr--minibuffer-setup
		       nil vr/minibuffer-replace-keymap
		       nil vr/query-replace-to-history-variable)))))
          ;; Successfully got the args, deactivate mark now. If the command was aborted (C-g), the mark (region) would remain active.
          (deactivate-mark)
	  (cond ((equal mode 'vr--mode-regexp-replace)
		 (list vr--regexp-string
		       vr--replace-string
		       vr--target-buffer-start
		       vr--target-buffer-end))
		((equal mode 'vr--mode-regexp)
		 (list vr--regexp-string
		       vr--target-buffer-start
		       vr--target-buffer-end)))))
    (progn ;; execute on finish
      (setq vr--in-minibuffer nil)
      (setq vr--calling-func nil)
      (unless (overlayp vr--minibuffer-message-overlay)
	(delete-overlay vr--minibuffer-message-overlay))
      (vr--delete-overlay-displays)
      (vr--delete-overlays))))

(add-hook 'multiple-cursors-mode-enabled-hook
	  ;; run vr/mc-mark once per cursor by default (do not ask the user)
	  (lambda ()
	    (when (boundp 'mc--default-cmds-to-run-once)
	      (add-to-list 'mc--default-cmds-to-run-once 'vr/mc-mark))))

;;;###autoload
(defun vr/mc-mark (regexp start end)
  "Convert regexp selection to multiple cursors."
  (interactive
   (vr--interactive-get-args 'vr--mode-regexp 'vr--calling-func-mc-mark))
  (with-current-buffer vr--target-buffer
    (mc/remove-fake-cursors)
    (activate-mark)
    (let ((regexp-string (vr--get-regexp-string))
	  ;; disable deactivating of mark after buffer-editing commands
	  ;; (which happens for example in visual-regexp-steroids/vr--parse-matches
	  ;; during the callback).
	  (deactivate-mark nil)
	  (first-fake-cursor nil))
      (vr--feedback-function t nil (lambda (i j begin end)
				     (with-current-buffer vr--target-buffer
				       (goto-char end)
				       (push-mark begin)
				       ;; temporarily enable transient mark mode
				       (activate-mark)
				       (let ((fc (mc/create-fake-cursor-at-point)))
					 (unless first-fake-cursor
					   (setq first-fake-cursor fc))))))
      
      ;; one fake cursor too many, replace first one with
      ;; the regular cursor.
      (when first-fake-cursor
	(mc/pop-state-from-overlay first-fake-cursor)))
    (mc/maybe-multiple-cursors-mode)))

;;;###autoload
(defun vr/replace (regexp replace start end)
  "Regexp-replace with live visual feedback."
  (interactive
   (vr--interactive-get-args 'vr--mode-regexp-replace 'vr--calling-func-replace))
  (unwind-protect
      (progn
        (when vr--in-minibuffer (error "visual-regexp already in use."))
        (setq vr--target-buffer (current-buffer)
              vr--target-buffer-start start
              vr--target-buffer-end end
              vr--regexp-string regexp
              vr--replace-string replace)
        ;; do replacement
        (vr--do-replace))
    ;; execute on finish
    (setq vr--in-minibuffer nil)))

;; query-replace-regexp starts here

(defvar vr--query-replacements nil)
;; we redefine the help text from replace.el to remove the commands we don't support.

(defconst vr--query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
p to preview the replacement (like 'C-c p' during construction of the regexp),
C-r [not supported in visual-regexp],
C-w [not supported in visual-regexp],
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ [not supported in visual-regexp],
E [not supported in visual-regexp]"
  "Help message while in `vr/query-replace'.")

(defvar vr--query-replace-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map query-replace-map)
    ;; the following replace.el commands are not supported by visual-regexp.
    (define-key map "e" nil)
    (define-key map "E" nil)
    (define-key map "\C-r" nil)
    (define-key map "\C-w" nil)
    (define-key map "^" nil)
    (define-key map "p" 'toggle-preview)
    map
    ))

;;;###autoload
(defun vr/query-replace (regexp replace start end)
  "Use vr/query-replace like you would use query-replace-regexp."
  (interactive
   (vr--interactive-get-args 'vr--mode-regexp-replace 'vr--calling-func-query-replace))
  (unwind-protect
      (progn
        (when vr--in-minibuffer (error "visual-regexp already in use."))
        (setq vr--target-buffer (current-buffer)
              vr--target-buffer-start start
              vr--target-buffer-end end
              vr--regexp-string regexp
              vr--replace-string replace)
        (let ((replace-string vr--replace-string))
          (setq vr--query-replacements (nreverse (car (vr--get-replacements nil nil)))))
        (vr--perform-query-replace regexp nil))
    ;; execute on finish
    (setq vr--in-minibuffer nil)))

(defun vr--perform-query-replace (from-string &optional map)
  ;; This function is a heavily modified version of (perform-replace) from replace.el.
  ;; The original plan was to use the original perform-replace, but various issues stood in the way.
  (or map (setq map vr--query-replace-map))
  (and minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let* ((next-replacement nil) ;; replacement string for current match
         (keep-going t)
         (replace-count 0)
         ;; a match can be replaced by a longer/shorter replacement. cumulate the difference
         (cumulative-offset 0)
         (recenter-last-op nil)	; Start cycling order with initial position.
         (message
          (apply 'propertize
                 (substitute-command-keys
                  "Query replacing %s with %s: (\\<vr--query-replace-map>\\[help] for help) ")
                 minibuffer-prompt-properties)))

    ;; show visual feedback for all matches
    (mapc (lambda (replacement-info)
            (cl-multiple-value-bind (replacement match-data i) replacement-info
              (vr--feedback-match-callback i 0 (cl-first match-data) (cl-second match-data))))
          vr--query-replacements)

    (goto-char vr--target-buffer-start)
    (push-mark)
    (undo-boundary)
    (unwind-protect
        ;; Loop finding occurrences that perhaps should be replaced.
        (while (and keep-going vr--query-replacements)
          ;; Advance replacement list
          (cl-multiple-value-bind (replacement match-data i) (car vr--query-replacements)
	    (setq match-data (mapcar (lambda (el) (+ cumulative-offset el)) match-data))
	    (let ((begin (cl-first match-data))
		  (end (cl-second match-data)))
	      (setq next-replacement-orig replacement)
	      (setq next-replacement (vr--get-replacement replacement match-data replace-count))
	      (goto-char begin)
	      (setq vr--query-replacements (cdr vr--query-replacements))
	      
	      ;; default for new occurrence: no preview
	      (setq vr--replace-preview nil)

	      (undo-boundary)
	      (let (done replaced key def)
		;; Loop reading commands until one of them sets done,
		;; which means it has finished handling this
		;; occurrence.
		(while (not done)
		  ;; show replacement feedback for current occurrence
		  (unless replaced
		    (vr--do-replace-feedback-match-callback next-replacement-orig match-data i))
		  ;; Bind message-log-max so we don't fill up the message log
		  ;; with a bunch of identical messages.
		  (let ((message-log-max nil))
		    (message message from-string next-replacement))
		  (setq key (read-event))
		  (setq key (vector key))
		  (setq def (lookup-key map key))

		  ;; can use replace-match afterwards
		  (set-match-data match-data)

		  ;; Restore the match data while we process the command.
		  (cond ((eq def 'help)
			 (with-output-to-temp-buffer "*Help*"
			   (princ
			    (concat "Query replacing visual-regexp "
				    from-string " with "
				    next-replacement ".\n\n"
				    (substitute-command-keys
				     vr--query-replace-help)))
			   (with-current-buffer standard-output
			     (help-mode))))
			((eq def 'exit)
			 (setq keep-going nil
			       done t))
			((eq def 'act)
			 (unless replaced
			   (replace-match next-replacement t t)
			   (setq replace-count (1+ replace-count)))
			 (setq done t
			       replaced t))
			((eq def 'act-and-exit)
			 (unless replaced
			   (replace-match next-replacement t t)
			   (setq replace-count (1+ replace-count)))
			 (setq keep-going nil
			       done t
			       replaced t))
			((eq def 'act-and-show)
			 (unless replaced
			   (replace-match next-replacement t t)
			   (setq replace-count (1+ replace-count))
			   (setq replaced t)))
			((eq def 'toggle-preview)
			 (setq vr--replace-preview (not vr--replace-preview)))
			((eq def 'automatic)
			 (setq vr--target-buffer-start (match-beginning 0)
			       vr--target-buffer-end (+ cumulative-offset vr--target-buffer-end))
			 (setq replace-count (+ replace-count (vr--do-replace t)))
			 (setq done t
			       replaced t
			       keep-going nil))
			((eq def 'skip)
			 (setq done t))
			((eq def 'recenter)
			 ;; `this-command' has the value `query-replace',
			 ;; so we need to bind it to `recenter-top-bottom'
			 ;; to allow it to detect a sequence of `C-l'.
			 (let ((this-command 'recenter-top-bottom)
			       (last-command 'recenter-top-bottom))
			   (recenter-top-bottom)))
			(t
			 (setq this-command 'mode-exited)
			 (setq keep-going nil)
			 (setq unread-command-events
			       (append (listify-key-sequence key)
				       unread-command-events))
			 (setq done t)))
		  (when replaced
		    (setq cumulative-offset (+ cumulative-offset (- (length next-replacement) (- end begin)))))
		  (unless (eq def 'recenter)
		    ;; Reset recenter cycling order to initial position.
		    (setq recenter-last-op nil))
		  ;; in case of 'act-and-show: delete overlay display or it will still be
		  ;; visible even though the replacement has been made
		  (when replaced (vr--delete-overlay-display (vr--get-overlay i 0)))))

	      ;; occurrence has been handled
	      ;; delete feedback overlay
	      (delete-overlay (vr--get-overlay i 0)))))

      ;; unwind
      (progn
        (vr--delete-overlay-displays)
        (vr--delete-overlays)
        ;; (replace-dehighlight)
        ))
    (unless unread-command-events
      ;; point is set to the end of the last occurrence.
      (goto-char (match-end 0))
      (message "Replaced %d occurrence%s"
               replace-count
               (if (= replace-count 1) "" "s")))))

(provide 'visual-regexp)

;;; visual-regexp.el ends here

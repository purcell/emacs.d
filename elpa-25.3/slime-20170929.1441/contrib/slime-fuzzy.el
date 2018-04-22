(require 'slime)
(require 'slime-repl)
(require 'slime-c-p-c)
(require 'cl-lib)

(define-slime-contrib slime-fuzzy
  "Fuzzy symbol completion."
  (:authors "Brian Downing <bdowning@lavos.net>"
            "Tobias C. Rittweiler <tcr@freebits.de>"
            "Attila Lendvai <attila.lendvai@gmail.com>")
  (:license "GPL")
  (:swank-dependencies swank-fuzzy)
  (:on-load
   (define-key slime-mode-map "\C-c\M-i" 'slime-fuzzy-complete-symbol)
   (when (featurep 'slime-repl)
     (define-key slime-repl-mode-map "\C-c\M-i"
       'slime-fuzzy-complete-symbol))))

(defcustom slime-fuzzy-completion-in-place t
  "When non-NIL the fuzzy symbol completion is done in place as
opposed to moving the point to the completion buffer."
  :group 'slime-mode
  :type 'boolean)

(defcustom slime-fuzzy-completion-limit 300
  "Only return and present this many symbols from swank."
  :group 'slime-mode
  :type 'integer)

(defcustom slime-fuzzy-completion-time-limit-in-msec 1500
  "Limit the time spent (given in msec) in swank while gathering
comletitions."
  :group 'slime-mode
  :type 'integer)

(defcustom slime-when-complete-filename-expand nil
  "Use comint-replace-by-expanded-filename instead of
comint-filename-completion to complete file names"
  :group 'slime-mode
  :type 'boolean)


(defvar slime-fuzzy-target-buffer nil
  "The buffer that is the target of the completion activities.")
(defvar slime-fuzzy-saved-window-configuration nil
  "The saved window configuration before the fuzzy completion
buffer popped up.")
(defvar slime-fuzzy-start nil
  "The beginning of the completion slot in the target buffer.
This is a non-advancing marker.")
(defvar slime-fuzzy-end nil
  "The end of the completion slot in the target buffer.
This is an advancing marker.")
(defvar slime-fuzzy-original-text nil
  "The original text that was in the completion slot in the
target buffer.  This is what is put back if completion is
aborted.")
(defvar slime-fuzzy-text nil
  "The text that is currently in the completion slot in the
target buffer.  If this ever doesn't match, the target buffer has
been modified and we abort without touching it.")
(defvar slime-fuzzy-first nil
  "The position of the first completion in the completions buffer.
The descriptive text and headers are above this.")
(defvar slime-fuzzy-last nil
    "The position of the last completion in the completions buffer.
If the time limit has exhausted during generation possible completion
choices inside SWANK, an indication is printed below this.")
(defvar slime-fuzzy-current-completion nil
  "The current completion object.  If this is the same before and
after point moves in the completions buffer, the text is not
replaced in the target for efficiency.")
(defvar slime-fuzzy-current-completion-overlay nil
  "The overlay representing the current completion in the completion
buffer. This is used to hightlight the text.")

;;;;;;; slime-target-buffer-fuzzy-completions-mode
;; NOTE: this mode has to be able to override key mappings in slime-mode

(defvar slime-target-buffer-fuzzy-completions-map
  (let ((map (make-sparse-keymap)))
    (cl-labels ((def (keys command)
                   (unless (listp keys)
                     (setq keys (list keys)))
                   (dolist (key keys)
                     (define-key map key command))))
      (def `([remap keyboard-quit]
             ,(kbd "C-g"))
           'slime-fuzzy-abort)
      (def `([remap slime-fuzzy-indent-and-complete-symbol]
             [remap slime-indent-and-complete-symbol]
             ,(kbd "<tab>"))
           'slime-fuzzy-select-or-update-completions)
      (def `([remap previous-line]
             ,(kbd "<up>"))
           'slime-fuzzy-prev)
      (def `([remap next-line]
             ,(kbd "<down>"))
           'slime-fuzzy-next)
      (def `([remap isearch-forward]
             ,(kbd "C-s"))
           'slime-fuzzy-continue-isearch-in-fuzzy-buffer)
      ;; some unconditional direct bindings
      (def (list (kbd "<return>") (kbd "RET") (kbd "<SPC>") "(" ")" "[" "]")
           'slime-fuzzy-select-and-process-event-in-target-buffer))
    map)
  "Keymap for slime-target-buffer-fuzzy-completions-mode.
This will override the key bindings in the target buffer
temporarily during completion.")

;; Make sure slime-fuzzy-target-buffer-completions-mode's map is
;; before everything else.
(setf minor-mode-map-alist
      (cl-stable-sort minor-mode-map-alist
                      (lambda (a b)
                        (eq a 'slime-fuzzy-target-buffer-completions-mode))
                      :key #'car))

(defun slime-fuzzy-continue-isearch-in-fuzzy-buffer ()
  (interactive)
  (select-window (get-buffer-window (slime-get-fuzzy-buffer)))
  (call-interactively 'isearch-forward))

(define-minor-mode slime-fuzzy-target-buffer-completions-mode
  "This minor mode is intented to override key bindings during
fuzzy completions in the target buffer. Most of the bindings will
do an implicit select in the completion window and let the
keypress be processed in the target buffer."
  nil
  nil
  slime-target-buffer-fuzzy-completions-map)

(add-to-list 'minor-mode-alist
             '(slime-fuzzy-target-buffer-completions-mode
               " Fuzzy Target Buffer Completions"))

(defvar slime-fuzzy-completions-map
  (let ((map (make-sparse-keymap)))
    (cl-labels ((def (keys command)
                     (unless (listp keys)
                       (setq keys (list keys)))
                     (dolist (key keys)
                       (define-key map key command))))
      (def `([remap keyboard-quit]
             "q"
             ,(kbd "C-g"))
           'slime-fuzzy-abort)
      (def `([remap previous-line]
             "p"
             "\M-p"
             ,(kbd "<up>"))
           'slime-fuzzy-prev)
      (def `([remap next-line]
             "n"
             "\M-n"
             ,(kbd "<down>"))
           'slime-fuzzy-next)
      (def "\d" 'scroll-down)
      (def `([remap slime-fuzzy-indent-and-complete-symbol]
             [remap slime-indent-and-complete-symbol]
             ,(kbd "<tab>"))
           'slime-fuzzy-select)
      (def (kbd "<mouse-2>") 'slime-fuzzy-select/mouse)
      (def `(,(kbd "RET")
             ,(kbd "<SPC>"))
           'slime-fuzzy-select))
    map)
  "Keymap for slime-fuzzy-completions-mode when in the completion buffer.")

(define-derived-mode slime-fuzzy-completions-mode
  fundamental-mode "Fuzzy Completions"
  "Major mode for presenting fuzzy completion results.

When you run `slime-fuzzy-complete-symbol', the symbol token at
point is completed using the Fuzzy Completion algorithm; this
means that the token is taken as a sequence of characters and all
the various possibilities that this sequence could meaningfully
represent are offered as selectable choices, sorted by how well
they deem to be a match for the token. (For instance, the first
choice of completing on \"mvb\" would be \"multiple-value-bind\".)

Therefore, a new buffer (*Fuzzy Completions*) will pop up that
contains the different completion choices. Simultaneously, a
special minor-mode will be temporarily enabled in the original
buffer where you initiated fuzzy completion (also called the
``target buffer'') in order to navigate through the *Fuzzy
Completions* buffer without leaving.

With focus in *Fuzzy Completions*:
  Type `n' and `p' (`UP', `DOWN') to navigate between completions.
  Type `RET' or `TAB' to select the completion near point.
  Type `q' to abort.

With focus in the target buffer:
  Type `UP' and `DOWN' to navigate between completions.
  Type a character that does not constitute a symbol name
  to insert the current choice and then that character (`(', `)',
  `SPACE', `RET'.) Use `TAB' to simply insert the current choice.
  Use C-g to abort.

Alternatively, you can click <mouse-2> on a completion to select it.


Complete listing of keybindings within the target buffer:

\\<slime-target-buffer-fuzzy-completions-map>\
\\{slime-target-buffer-fuzzy-completions-map}

Complete listing of keybindings with *Fuzzy Completions*:

\\<slime-fuzzy-completions-map>\
\\{slime-fuzzy-completions-map}"
  (use-local-map slime-fuzzy-completions-map)
  (set (make-local-variable 'slime-fuzzy-current-completion-overlay)
       (make-overlay (point) (point) nil t nil)))

(defun slime-fuzzy-completions (prefix &optional default-package)
  "Get the list of sorted completion objects from completing
`prefix' in `package' from the connected Lisp."
  (let ((prefix (cl-etypecase prefix
                  (symbol (symbol-name prefix))
                  (string prefix))))
    (slime-eval `(swank:fuzzy-completions ,prefix
                                          ,(or default-package
                                               (slime-current-package))
                  :limit ,slime-fuzzy-completion-limit
                  :time-limit-in-msec
                  ,slime-fuzzy-completion-time-limit-in-msec))))

(defun slime-fuzzy-selected (prefix completion)
  "Tell the connected Lisp that the user selected completion
`completion' as the completion for `prefix'."
  (let ((no-properties (copy-sequence prefix)))
    (set-text-properties 0 (length no-properties) nil no-properties)
    (slime-eval `(swank:fuzzy-completion-selected ,no-properties
                                                  ',completion))))

(defun slime-fuzzy-indent-and-complete-symbol ()
  "Indent the current line and perform fuzzy symbol completion.  First
indent the line. If indenting doesn't move point, complete the
symbol. If there's no symbol at the point, show the arglist for the
most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'slime-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (slime-fuzzy-complete-symbol))
            ((memq (char-before) '(?\t ?\ ))
             (slime-echo-arglist))))))

(cl-defun slime-fuzzy-complete-symbol ()
  "Fuzzily completes the abbreviation at point into a symbol."
  (interactive)
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\=" nil t))
    (cl-return-from slime-fuzzy-complete-symbol
      ;; don't add space after completion
      (let ((comint-completion-addsuffix '("/" . "")))
        (if slime-when-complete-filename-expand
            (comint-replace-by-expanded-filename)
          ;; FIXME: use `comint-filename-completion' when dropping emacs23
          (funcall (if (>= emacs-major-version 24)
                       'comint-filename-completion
                     'comint-dynamic-complete-as-filename))))))
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end)))
    (cl-destructuring-bind (completion-set interrupted-p)
        (slime-fuzzy-completions prefix)
      (if (null completion-set)
          (progn (slime-minibuffer-respecting-message
                  "Can't find completion for \"%s\"" prefix)
                 (ding)
                 (slime-fuzzy-done))
          (goto-char end)
          (cond ((slime-length= completion-set 1)
                 ;; insert completed string
                 (insert-and-inherit (caar completion-set))
                 (delete-region beg end)
                 (goto-char (+ beg (length (caar completion-set))))
                 (slime-minibuffer-respecting-message "Sole completion")
                 (slime-fuzzy-done))
                ;; Incomplete
                (t
                 (slime-fuzzy-choices-buffer completion-set interrupted-p
                                             beg end)
                 (slime-minibuffer-respecting-message
                  "Complete but not unique")))))))


(defun slime-get-fuzzy-buffer ()
  (get-buffer-create "*Fuzzy Completions*"))

(defvar slime-fuzzy-explanation
  "For help on how the use this buffer, see `slime-fuzzy-completions-mode'.

Flags: boundp fboundp generic-function class macro special-operator package
\n"
  "The explanation that gets inserted at the beginning of the
*Fuzzy Completions* buffer.")

(defun slime-fuzzy-insert-completion-choice (completion max-length)
  "Inserts the completion object `completion' as a formatted
completion choice into the current buffer, and mark it with the
proper text properties."
  (cl-destructuring-bind (symbol-name score chunks classification-string)
      completion
    (let ((start (point))
          (end))
      (insert symbol-name)
      (setq end (point))
      (dolist (chunk chunks)
        (put-text-property (+ start (cl-first chunk))
                           (+ start (cl-first chunk)
                              (length (cl-second chunk)))
                           'face 'bold))
      (put-text-property start (point) 'mouse-face 'highlight)
      (dotimes (i (- max-length (- end start)))
        (insert " "))
      (insert (format " %s %s\n"
                      classification-string
                      score))
      (put-text-property start (point) 'completion completion))))

(defun slime-fuzzy-insert (text)
  "Inserts `text' into the target buffer in the completion slot.
If the buffer has been modified in the meantime, abort the
completion process.  Otherwise, update all completion variables
so that the new text is present."
  (with-current-buffer slime-fuzzy-target-buffer
    (cond
     ((not (string-equal slime-fuzzy-text
                         (buffer-substring slime-fuzzy-start
                                           slime-fuzzy-end)))
      (slime-fuzzy-done)
      (beep)
      (message "Target buffer has been modified!"))
     (t
      (goto-char slime-fuzzy-start)
      (delete-region slime-fuzzy-start slime-fuzzy-end)
      (insert-and-inherit text)
      (setq slime-fuzzy-text text)
      (goto-char slime-fuzzy-end)))))

(defun slime-minibuffer-p (buffer)
  (if (featurep 'xemacs)
      (eq buffer (window-buffer (minibuffer-window)))
      (minibufferp buffer)))

(defun slime-fuzzy-choices-buffer (completions interrupted-p start end)
  "Creates (if neccessary), populates, and pops up the *Fuzzy
Completions* buffer with the completions from `completions' and
the completion slot in the current buffer bounded by `start' and
`end'.  This saves the window configuration before popping the
buffer so that it can possibly be restored when the user is
done."
  (let ((new-completion-buffer (not slime-fuzzy-target-buffer))
        (connection (slime-connection)))
    (when new-completion-buffer
      (setq slime-fuzzy-saved-window-configuration
            (current-window-configuration)))
    (slime-fuzzy-enable-target-buffer-completions-mode)
    (setq slime-fuzzy-target-buffer (current-buffer))
    (setq slime-fuzzy-start (move-marker (make-marker) start))
    (setq slime-fuzzy-end (move-marker (make-marker) end))
    (set-marker-insertion-type slime-fuzzy-end t)
    (setq slime-fuzzy-original-text (buffer-substring start end))
    (setq slime-fuzzy-text slime-fuzzy-original-text)
    (slime-fuzzy-fill-completions-buffer completions interrupted-p)
    (pop-to-buffer (slime-get-fuzzy-buffer))
    (slime-fuzzy-next)
    (setq slime-buffer-connection connection)
    (when new-completion-buffer
      ;; Hook to nullify window-config restoration if the user changes
      ;; the window configuration himself.
      (when (boundp 'window-configuration-change-hook)
        (add-hook 'window-configuration-change-hook
                  'slime-fuzzy-window-configuration-change))
      (add-hook 'kill-buffer-hook 'slime-fuzzy-abort 'append t)
      (set (make-local-variable 'cursor-type) nil)
      (setq buffer-quit-function 'slime-fuzzy-abort)) ; M-Esc Esc
    (when slime-fuzzy-completion-in-place
      ;; switch back to the original buffer
      (if (slime-minibuffer-p slime-fuzzy-target-buffer)
          (select-window (minibuffer-window))
          (switch-to-buffer-other-window slime-fuzzy-target-buffer)))))

(defun slime-fuzzy-fill-completions-buffer (completions interrupted-p)
  "Erases and fills the completion buffer with the given completions."
  (with-current-buffer (slime-get-fuzzy-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (slime-fuzzy-completions-mode)
    (insert slime-fuzzy-explanation)
    (let ((max-length 12))
      (dolist (completion completions)
        (setf max-length (max max-length (length (cl-first completion)))))

      (insert "Completion:")
      (dotimes (i (- max-length 10)) (insert " "))
      ;;     Flags:   Score:
      ;; ... -------  --------
      ;;     bfgctmsp
      (let* ((example-classification-string (cl-fourth (cl-first completions)))
             (classification-length (length example-classification-string))
             (spaces (- classification-length (length "Flags:"))))
        (insert "Flags:")
        (dotimes (i spaces) (insert " "))
        (insert " Score:\n")
        (dotimes (i max-length) (insert "-"))
        (insert " ")
        (dotimes (i classification-length) (insert "-"))
        (insert " --------\n")
        (setq slime-fuzzy-first (point)))

      (dolist (completion completions)
        (setq slime-fuzzy-last (point)) ; will eventually become the last entry
        (slime-fuzzy-insert-completion-choice completion max-length))

      (when interrupted-p
        (insert "...\n")
        (insert "[Interrupted: time limit exhausted]"))

      (setq buffer-read-only t))
    (setq slime-fuzzy-current-completion
          (caar completions))
    (goto-char 0)))

(defun slime-fuzzy-enable-target-buffer-completions-mode ()
  "Store the target buffer's local map, so that we can restore it."
  (unless slime-fuzzy-target-buffer-completions-mode
;    (slime-log-event "Enabling target buffer completions mode")
    (slime-fuzzy-target-buffer-completions-mode 1)))

(defun slime-fuzzy-disable-target-buffer-completions-mode ()
  "Restores the target buffer's local map when completion is finished."
  (when slime-fuzzy-target-buffer-completions-mode
;    (slime-log-event "Disabling target buffer completions mode")
    (slime-fuzzy-target-buffer-completions-mode 0)))

(defun slime-fuzzy-insert-from-point ()
  "Inserts the completion that is under point in the completions
buffer into the target buffer.  If the completion in question had
already been inserted, it does nothing."
  (with-current-buffer (slime-get-fuzzy-buffer)
    (let ((current-completion (get-text-property (point) 'completion)))
      (when (and current-completion
                 (not (eq slime-fuzzy-current-completion
                          current-completion)))
        (slime-fuzzy-insert
         (cl-first (get-text-property (point) 'completion)))
        (setq slime-fuzzy-current-completion
              current-completion)))))

(defun slime-fuzzy-post-command-hook ()
  "The post-command-hook for the *Fuzzy Completions* buffer.
This makes sure the completion slot in the target buffer matches
the completion that point is on in the completions buffer."
  (condition-case err
      (when slime-fuzzy-target-buffer
        (slime-fuzzy-insert-from-point))
    (error
     ;; Because this is called on the post-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slime-fuzzy-post-command-hook: %S" err))))

(defun slime-fuzzy-next ()
  "Moves point directly to the next completion in the completions
buffer."
  (interactive)
  (with-current-buffer (slime-get-fuzzy-buffer)
    (let ((point (next-single-char-property-change
                  (point) 'completion nil slime-fuzzy-last)))
      (set-window-point (get-buffer-window (current-buffer)) point)
      (goto-char point))
    (slime-fuzzy-highlight-current-completion)))

(defun slime-fuzzy-prev ()
  "Moves point directly to the previous completion in the
completions buffer."
  (interactive)
  (with-current-buffer (slime-get-fuzzy-buffer)
    (let ((point (previous-single-char-property-change
                  (point)
                  'completion nil slime-fuzzy-first)))
      (set-window-point (get-buffer-window (current-buffer)) point)
      (goto-char point))
    (slime-fuzzy-highlight-current-completion)))

(defun slime-fuzzy-highlight-current-completion ()
  "Highlights the current completion,
so that the user can see it on the screen."
  (let ((pos (point)))
    (when (overlayp slime-fuzzy-current-completion-overlay)
      (move-overlay slime-fuzzy-current-completion-overlay
                    (point) (1- (search-forward " ")))
      (overlay-put slime-fuzzy-current-completion-overlay
                   'face 'secondary-selection))
    (goto-char pos)))

(defun slime-fuzzy-abort ()
  "Aborts the completion process, setting the completions slot in
the target buffer back to its original contents."
  (interactive)
  (when slime-fuzzy-target-buffer
    (slime-fuzzy-done)))

(defun slime-fuzzy-select ()
  "Selects the current completion, making sure that it is inserted
into the target buffer.  This tells the connected Lisp what completion
was selected."
  (interactive)
  (when slime-fuzzy-target-buffer
    (with-current-buffer (slime-get-fuzzy-buffer)
      (let ((completion (get-text-property (point) 'completion)))
        (when completion
          (slime-fuzzy-insert (cl-first completion))
          (slime-fuzzy-selected slime-fuzzy-original-text
                                completion)
          (slime-fuzzy-done))))))

(defun slime-fuzzy-select-or-update-completions ()
  "If there were no changes since the last time fuzzy completion was started
this function will select the current completion.
Otherwise refreshes the completion list based on the changes made."
  (interactive)
;  (slime-log-event "Selecting or updating completions")
  (if (string-equal slime-fuzzy-original-text
                    (buffer-substring slime-fuzzy-start
                                      slime-fuzzy-end))
      (slime-fuzzy-select)
      (slime-fuzzy-complete-symbol)))

(defun slime-fuzzy-process-event-in-completions-buffer ()
  "Simply processes the event in the target buffer"
  (interactive)
  (with-current-buffer (slime-get-fuzzy-buffer)
    (push last-input-event unread-command-events)))

(defun slime-fuzzy-select-and-process-event-in-target-buffer ()
 "Selects the current completion, making sure that it is inserted
into the target buffer and processes the event in the target buffer."
 (interactive)
; (slime-log-event "Selecting and processing event in target buffer")
 (when slime-fuzzy-target-buffer
   (let ((buff slime-fuzzy-target-buffer))
     (slime-fuzzy-select)
     (with-current-buffer buff
       (slime-fuzzy-disable-target-buffer-completions-mode)
       (push last-input-event unread-command-events)))))

(defun slime-fuzzy-select/mouse (event)
  "Handle a mouse-2 click on a completion choice as if point were
on the completion choice and the slime-fuzzy-select command was
run."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (when (get-text-property (point) 'mouse-face)
        (slime-fuzzy-insert-from-point)
        (slime-fuzzy-select)))))

(defun slime-fuzzy-done ()
  "Cleans up after the completion process.  This removes all hooks,
and attempts to restore the window configuration.  If this fails,
it just burys the completions buffer and leaves the window
configuration alone."
  (when slime-fuzzy-target-buffer
    (set-buffer slime-fuzzy-target-buffer)
    (slime-fuzzy-disable-target-buffer-completions-mode)
    (if (slime-fuzzy-maybe-restore-window-configuration)
        (bury-buffer (slime-get-fuzzy-buffer))
        ;; We couldn't restore the windows, so just bury the fuzzy
        ;; completions buffer and let something else fill it in.
        (pop-to-buffer (slime-get-fuzzy-buffer))
        (bury-buffer))
    (if (slime-minibuffer-p slime-fuzzy-target-buffer)
        (select-window (minibuffer-window))
        (pop-to-buffer slime-fuzzy-target-buffer))
    (goto-char slime-fuzzy-end)
    (setq slime-fuzzy-target-buffer nil)
    (remove-hook 'window-configuration-change-hook
                 'slime-fuzzy-window-configuration-change)))

(defun slime-fuzzy-maybe-restore-window-configuration ()
  "Restores the saved window configuration if it has not been
nullified."
  (when (boundp 'window-configuration-change-hook)
    (remove-hook 'window-configuration-change-hook
                 'slime-fuzzy-window-configuration-change))
  (if (not slime-fuzzy-saved-window-configuration)
      nil
    (set-window-configuration slime-fuzzy-saved-window-configuration)
    (setq slime-fuzzy-saved-window-configuration nil)
    t))

(defun slime-fuzzy-window-configuration-change ()
  "Called on window-configuration-change-hook.  Since the window
configuration was changed, we nullify our saved configuration."
  (setq slime-fuzzy-saved-window-configuration nil))

(provide 'slime-fuzzy)

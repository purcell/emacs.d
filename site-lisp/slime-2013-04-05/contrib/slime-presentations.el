
(define-slime-contrib slime-presentations
  "Imitate LispM presentations."
  (:authors "Alan Ruttenberg  <alanr-l@mumble.net>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-presentations)
  (:on-load
   (add-hook 'slime-repl-mode-hook
             (lambda ()
               ;; Respect the syntax text properties of presentation.
               (set (make-local-variable 'parse-sexp-lookup-properties) t)
               (slime-add-local-hook 'after-change-functions 
                                     'slime-after-change-function)))
   (add-hook 'slime-event-hooks 'slime-dispatch-presentation-event)
   (setq slime-write-string-function 'slime-presentation-write)
   (add-hook 'slime-repl-return-hooks 'slime-presentation-on-return-pressed)
   (add-hook 'slime-repl-current-input-hooks 'slime-presentation-current-input)
   (add-hook 'slime-open-stream-hooks 'slime-presentation-on-stream-open)
   (add-hook 'slime-repl-clear-buffer-hook 'slime-clear-presentations)
   (add-hook 'slime-edit-definition-hooks 'slime-edit-presentation)
   (setq slime-inspector-insert-ispec-function 'slime-presentation-inspector-insert-ispec)
   (setq sldb-insert-frame-variable-value-function 
         'slime-presentation-sldb-insert-frame-variable-value)
   (slime-presentation-init-keymaps)
   (slime-presentation-add-easy-menu)))

(defface slime-repl-output-mouseover-face
  (if (featurep 'xemacs)
      '((t (:bold t)))
    (if (slime-face-inheritance-possible-p)
        '((t
           (:box
            (:line-width 1 :color "black" :style released-button)
            :inherit
            slime-repl-inputed-output-face)))
      '((t (:box (:line-width 1 :color "black"))))))
  "Face for Lisp output in the SLIME REPL, when the mouse hovers over it"
  :group 'slime-repl)

(defface slime-repl-inputed-output-face
  '((((class color) (background light)) (:foreground "Red"))
    (((class color) (background dark)) (:foreground "Red"))
    (t (:slant italic)))
  "Face for the result of an evaluation in the SLIME REPL."
  :group 'slime-repl)

;; FIXME: This conditional is not right - just used because the code
;; here does not work in XEmacs.
(when (boundp 'text-property-default-nonsticky)
  (pushnew '(slime-repl-presentation . t) text-property-default-nonsticky
	   :test 'equal)
  (pushnew '(slime-repl-result-face . t) text-property-default-nonsticky
	   :test 'equal))

(make-variable-buffer-local
 (defvar slime-presentation-start-to-point (make-hash-table)))

(defun slime-mark-presentation-start (id &optional target)
  "Mark the beginning of a presentation with the given ID.
TARGET can be nil (regular process output) or :repl-result."
  (setf (gethash id slime-presentation-start-to-point) 
        ;; We use markers because text can also be inserted before this presentation.
        ;; (Output arrives while we are writing presentations within REPL results.)
        (copy-marker (slime-output-target-marker target) nil)))

(defun slime-mark-presentation-start-handler (process string)
  (if (and string (string-match "<\\([-0-9]+\\)" string))
      (let* ((match (substring string (match-beginning 1) (match-end 1)))
             (id (car (read-from-string match))))
        (slime-mark-presentation-start id))))

(defun slime-mark-presentation-end (id &optional target)
  "Mark the end of a presentation with the given ID.
TARGET can be nil (regular process output) or :repl-result."
  (let ((start (gethash id slime-presentation-start-to-point)))
    (remhash id slime-presentation-start-to-point)
    (when start
      (let* ((marker (slime-output-target-marker target))
             (buffer (and marker (marker-buffer marker))))
        (with-current-buffer buffer
          (let ((end (marker-position marker)))
            (slime-add-presentation-properties start end
                                               id nil)))))))

(defun slime-mark-presentation-end-handler (process string)
  (if (and string (string-match ">\\([-0-9]+\\)" string))
      (let* ((match (substring string (match-beginning 1) (match-end 1)))
             (id (car (read-from-string match))))
        (slime-mark-presentation-end id))))

(defstruct slime-presentation text id)

(defvar slime-presentation-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This allows to use C-M-k, C-M-SPC,
    ;; etc. to deal with a whole presentation.  (For Lisp mode, this
    ;; is not desirable, since we do not wish to get a mismatched
    ;; paren highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    table)
  "Syntax table for presentations.")

(defun slime-add-presentation-properties (start end id result-p)
  "Make the text between START and END a presentation with ID.
RESULT-P decides whether a face for a return value or output text is used."
  (let* ((text (buffer-substring-no-properties start end))
         (presentation (make-slime-presentation :text text :id id)))
    (let ((inhibit-modification-hooks t))
      (add-text-properties start end
                           `(modification-hooks (slime-after-change-function)
                             insert-in-front-hooks (slime-after-change-function)
                             insert-behind-hooks (slime-after-change-function)
                             syntax-table ,slime-presentation-syntax-table
                             rear-nonsticky t))
      ;; Use the presentation as the key of a text property
      (case (- end start)
        (0)
        (1
         (add-text-properties start end
                              `(slime-repl-presentation ,presentation
                                ,presentation :start-and-end)))
        (t
         (add-text-properties start (1+ start) 
                              `(slime-repl-presentation ,presentation
                                ,presentation :start))
         (when (> (- end start) 2)
           (add-text-properties (1+ start) (1- end)
                                `(,presentation :interior)))
         (add-text-properties (1- end) end
                              `(slime-repl-presentation ,presentation
                                ,presentation :end))))
      ;; Also put an overlay for the face and the mouse-face.  This enables
      ;; highlighting of nested presentations.  However, overlays get lost
      ;; when we copy a presentation; their removal is also not undoable.
      ;; In these cases the mouse-face text properties need to take over ---
      ;; but they do not give nested highlighting.
      (slime-ensure-presentation-overlay start end presentation))))

(defun slime-ensure-presentation-overlay (start end presentation)
  (unless (find presentation (overlays-at start)
                :key (lambda (overlay) 
                       (overlay-get overlay 'slime-repl-presentation)))
    (let ((overlay (make-overlay start end (current-buffer) t nil)))
      (overlay-put overlay 'slime-repl-presentation presentation)
      (overlay-put overlay 'mouse-face 'slime-repl-output-mouseover-face)
      (overlay-put overlay 'help-echo 
                   (if (eq major-mode 'slime-repl-mode)
                       "mouse-2: copy to input; mouse-3: menu"
                     "mouse-2: inspect; mouse-3: menu"))
      (overlay-put overlay 'face 'slime-repl-inputed-output-face)
      (overlay-put overlay 'keymap slime-presentation-map))))
  
(defun slime-remove-presentation-properties (from to presentation)
  (let ((inhibit-read-only t)) 
    (remove-text-properties from to
                            `(,presentation t syntax-table t rear-nonsticky t))
    (when (eq (get-text-property from 'slime-repl-presentation) presentation)
      (remove-text-properties from (1+ from) `(slime-repl-presentation t)))
    (when (eq (get-text-property (1- to) 'slime-repl-presentation) presentation)
      (remove-text-properties (1- to) to `(slime-repl-presentation t)))
    (dolist (overlay (overlays-at from))
      (when (eq (overlay-get overlay 'slime-repl-presentation) presentation)
        (delete-overlay overlay)))))

(defun slime-insert-presentation (string output-id &optional rectangle)
  "Insert STRING in current buffer and mark it as a presentation 
corresponding to OUTPUT-ID.  If RECTANGLE is true, indent multi-line
strings to line up below the current point."
  (flet ((insert-it ()
                    (if rectangle 
                        (slime-insert-indented string)
                      (insert string))))
    (let ((start (point)))
      (insert-it)
      (slime-add-presentation-properties start (point) output-id t))))

(defun slime-presentation-whole-p (presentation start end &optional object)
  (let ((object (or object (current-buffer))))
    (string= (etypecase object
               (buffer (with-current-buffer object
                         (buffer-substring-no-properties start end)))
               (string (substring-no-properties object start end)))
             (slime-presentation-text presentation))))

(defun slime-presentations-around-point (point &optional object)
  (let ((object (or object (current-buffer))))
    (loop for (key value . rest) on (text-properties-at point object) by 'cddr
          when (slime-presentation-p key)
          collect key)))

(defun slime-presentation-start-p (tag)
  (memq tag '(:start :start-and-end)))

(defun slime-presentation-stop-p (tag)
  (memq tag '(:end :start-and-end)))

(defun* slime-presentation-start (point presentation
                                        &optional (object (current-buffer)))
  "Find start of `presentation' at `point' in `object'.
Return buffer index and whether a start-tag was found."
  (let* ((this-presentation (get-text-property point presentation object)))
    (while (not (slime-presentation-start-p this-presentation))
      (let ((change-point (previous-single-property-change 
                           point presentation object)))
        (unless change-point
          (return-from slime-presentation-start
            (values (etypecase object
                      (buffer (with-current-buffer object 1))
                      (string 0))
                    nil)))
        (setq this-presentation (get-text-property change-point 
                                                   presentation object))
        (unless this-presentation
          (return-from slime-presentation-start 
            (values point nil)))
        (setq point change-point)))
    (values point t)))

(defun* slime-presentation-end (point presentation
                                      &optional (object (current-buffer)))
  "Find end of presentation at `point' in `object'.  Return buffer
index (after last character of the presentation) and whether an
end-tag was found."
  (let* ((this-presentation (get-text-property point presentation object)))
    (while (not (slime-presentation-stop-p this-presentation))
      (let ((change-point (next-single-property-change 
                           point presentation object)))
        (unless change-point
          (return-from slime-presentation-end
            (values (etypecase object
                      (buffer (with-current-buffer object (point-max)))
                      (string (length object))) 
                    nil)))
        (setq point change-point)
        (setq this-presentation (get-text-property point 
                                                   presentation object))))
    (if this-presentation 
        (let ((after-end (next-single-property-change point
                                                      presentation object)))
          (if (not after-end)
              (values (etypecase object
                        (buffer (with-current-buffer object (point-max)))
                        (string (length object))) 
                      t)
              (values after-end t)))
        (values point nil))))

(defun* slime-presentation-bounds (point presentation 
                                         &optional (object (current-buffer)))
  "Return start index and end index of `presentation' around `point'
in `object', and whether the presentation is complete."
  (multiple-value-bind (start good-start)
      (slime-presentation-start point presentation object)
    (multiple-value-bind (end good-end)
        (slime-presentation-end point presentation object)
      (values start end 
              (and good-start good-end
                   (slime-presentation-whole-p presentation 
                                               start end object))))))

(defun slime-presentation-around-point (point &optional object)
  "Return presentation, start index, end index, and whether the
presentation is complete."
  (let ((object (or object (current-buffer)))
        (innermost-presentation nil)
        (innermost-start 0)
        (innermost-end most-positive-fixnum))
    (dolist (presentation (slime-presentations-around-point point object))
      (multiple-value-bind (start end whole-p)
          (slime-presentation-bounds point presentation object)
        (when whole-p 
          (when (< (- end start) (- innermost-end innermost-start))
            (setq innermost-start start
                  innermost-end end
                  innermost-presentation presentation)))))
    (values innermost-presentation
            innermost-start innermost-end)))

(defun slime-presentation-around-or-before-point (point &optional object)
  (let ((object (or object (current-buffer))))
    (multiple-value-bind (presentation start end whole-p)
        (slime-presentation-around-point point object)
      (if (or presentation (= point (point-min)))
          (values presentation start end whole-p)
        (slime-presentation-around-point (1- point) object)))))

(defun slime-presentation-around-or-before-point-or-error (point)
  (multiple-value-bind (presentation start end whole-p)
      (slime-presentation-around-or-before-point point)
    (unless presentation
      (error "No presentation at point"))
    (values presentation start end whole-p)))

(defun* slime-for-each-presentation-in-region (from to function &optional (object (current-buffer)))
  "Call `function' with arguments `presentation', `start', `end',
`whole-p' for every presentation in the region `from'--`to' in the
string or buffer `object'."
  (flet ((handle-presentation (presentation point)
                              (multiple-value-bind (start end whole-p)
                                  (slime-presentation-bounds point presentation object)
                                (funcall function presentation start end whole-p))))
    ;; Handle presentations active at `from'.
    (dolist (presentation (slime-presentations-around-point from object))
      (handle-presentation presentation from))
    ;; Use the `slime-repl-presentation' property to search for new presentations.
    (let ((point from))
      (while (< point to)
        (setq point (next-single-property-change point 'slime-repl-presentation object to))
        (let* ((presentation (get-text-property point 'slime-repl-presentation object))
               (status (get-text-property point presentation object)))
          (when (slime-presentation-start-p status)
            (handle-presentation presentation point)))))))

;; XEmacs compatibility hack, from message by Stephen J. Turnbull on
;; xemacs-beta@xemacs.org of 18 Mar 2002
(unless (boundp 'undo-in-progress)
  (defvar undo-in-progress nil
   "Placeholder defvar for XEmacs compatibility from SLIME.")
  (defadvice undo-more (around slime activate)
     (let ((undo-in-progress t)) ad-do-it)))

(defun slime-after-change-function (start end &rest ignore)
  "Check all presentations within and adjacent to the change.
When a presentation has been altered, change it to plain text."
  (let ((inhibit-modification-hooks t))
    (let ((real-start (max 1 (1- start)))
          (real-end   (min (1+ (buffer-size)) (1+ end)))
          (any-change nil))
      ;; positions around the change
      (slime-for-each-presentation-in-region 
       real-start real-end
       (lambda (presentation from to whole-p)
         (cond
          (whole-p
           (slime-ensure-presentation-overlay from to presentation))
          ((not undo-in-progress)
           (slime-remove-presentation-properties from to 
                                                 presentation)
           (setq any-change t)))))
      (when any-change
        (undo-boundary)))))

(defun slime-presentation-around-click (event)
  "Return the presentation around the position of the mouse-click EVENT.
If there is no presentation, signal an error.
Also return the start position, end position, and buffer of the presentation."
  (when (and (featurep 'xemacs) (not (button-press-event-p event)))
    (error "Command must be bound to a button-press-event"))
  (let ((point (if (featurep 'xemacs) (event-point event) (posn-point (event-end event))))
        (window (if (featurep 'xemacs) (event-window event) (caadr event))))
    (with-current-buffer (window-buffer window)
      (multiple-value-bind (presentation start end)
          (slime-presentation-around-point point)
        (unless presentation
          (error "No presentation at click"))
        (values presentation start end (current-buffer))))))

(defun slime-check-presentation (from to buffer presentation)
  (unless (slime-eval `(cl:nth-value 1 (swank:lookup-presented-object
                                        ',(slime-presentation-id presentation))))
    (with-current-buffer buffer
      (slime-remove-presentation-properties from to presentation))))

(defun slime-copy-or-inspect-presentation-at-mouse (event)
  (interactive "e") ; no "@" -- we don't want to select the clicked-at window
  (multiple-value-bind (presentation start end buffer)
      (slime-presentation-around-click event)
    (slime-check-presentation start end buffer presentation)
    (if (with-current-buffer buffer
          (eq major-mode 'slime-repl-mode))
        (slime-copy-presentation-at-mouse-to-repl event)
        (slime-inspect-presentation-at-mouse event))))

(defun slime-inspect-presentation (presentation start end buffer)
  (let ((reset-p 
	 (with-current-buffer buffer
	   (not (eq major-mode 'slime-inspector-mode)))))
    (slime-eval-async `(swank:inspect-presentation ',(slime-presentation-id presentation) ,reset-p)
		      'slime-open-inspector)))

(defun slime-inspect-presentation-at-mouse (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slime-presentation-around-click event)
    (slime-inspect-presentation presentation start end buffer)))

(defun slime-inspect-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slime-presentation-around-or-before-point-or-error point)
    (slime-inspect-presentation presentation start end (current-buffer))))


(defun slime-M-.-presentation (presentation start end buffer &optional where)
  (let* ((id (slime-presentation-id presentation))
	 (presentation-string (format "Presentation %s" id))
	 (location (slime-eval `(swank:find-definition-for-thing
				 (swank:lookup-presented-object
				  ',(slime-presentation-id presentation))))))
    (slime-edit-definition-cont
     (and location (list (make-slime-xref :dspec `(,presentation-string)
					  :location location)))
     presentation-string
     where)))

(defun slime-M-.-presentation-at-mouse (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slime-presentation-around-click event)
    (slime-M-.-presentation presentation start end buffer)))

(defun slime-M-.-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slime-presentation-around-or-before-point-or-error point)
    (slime-M-.-presentation presentation start end (current-buffer))))

(defun slime-edit-presentation (name &optional where)
  (if (or current-prefix-arg (not (equal (slime-symbol-at-point) name)))
      nil ; NAME came from user explicitly, so decline.
      (multiple-value-bind (presentation start end whole-p)
	  (slime-presentation-around-or-before-point (point))
	(when presentation
	  (slime-M-.-presentation presentation start end (current-buffer) where)))))


(defun slime-copy-presentation-to-repl (presentation start end buffer)
  (let ((presentation-text 
	 (with-current-buffer buffer
	   (buffer-substring start end))))
    (unless (eql major-mode 'slime-repl-mode)
      (slime-switch-to-output-buffer))
    (flet ((do-insertion ()
	     (unless (looking-back "\\s-" (- (point) 1))
	       (insert " "))
	     (insert presentation-text)
	     (unless (or (eolp) (looking-at "\\s-"))
	       (insert " "))))
      (if (>= (point) slime-repl-prompt-start-mark)
	  (do-insertion)
	(save-excursion
	  (goto-char (point-max))
	  (do-insertion))))))

(defun slime-copy-presentation-at-mouse-to-repl (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slime-presentation-around-click event)
    (slime-copy-presentation-to-repl presentation start end buffer)))

(defun slime-copy-presentation-at-point-to-repl (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slime-presentation-around-or-before-point-or-error point)
    (slime-copy-presentation-to-repl presentation start end (current-buffer))))

(defun slime-copy-presentation-at-mouse-to-point (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slime-presentation-around-click event)
    (let ((presentation-text 
           (with-current-buffer buffer
             (buffer-substring start end))))
      (when (not (string-match "\\s-"
                               (buffer-substring (1- (point)) (point))))
        (insert " "))
      (insert presentation-text)
      (slime-after-change-function (point) (point))
      (when (and (not (eolp)) (not (looking-at "\\s-")))
        (insert " ")))))

(defun slime-copy-presentation-to-kill-ring (presentation start end buffer)
  (let ((presentation-text 
           (with-current-buffer buffer
             (buffer-substring start end))))
    (kill-new presentation-text)
    (message "Saved presentation \"%s\" to kill ring" presentation-text)))

(defun slime-copy-presentation-at-mouse-to-kill-ring (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer) 
      (slime-presentation-around-click event)
    (slime-copy-presentation-to-kill-ring presentation start end buffer)))

(defun slime-copy-presentation-at-point-to-kill-ring (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slime-presentation-around-or-before-point-or-error point)
    (slime-copy-presentation-to-kill-ring presentation start end (current-buffer))))
  
(defun slime-describe-presentation (presentation)
  (slime-eval-describe 
     `(swank::describe-to-string
       (swank:lookup-presented-object ',(slime-presentation-id presentation)))))

(defun slime-describe-presentation-at-mouse (event)
  (interactive "@e")
  (multiple-value-bind (presentation) (slime-presentation-around-click event)
    (slime-describe-presentation presentation)))

(defun slime-describe-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation) 
      (slime-presentation-around-or-before-point-or-error point)
    (slime-describe-presentation presentation)))

(defun slime-pretty-print-presentation (presentation)
  (slime-eval-describe 
     `(swank::swank-pprint
       (cl:list
        (swank:lookup-presented-object ',(slime-presentation-id presentation))))))

(defun slime-pretty-print-presentation-at-mouse (event)
  (interactive "@e")
  (multiple-value-bind (presentation) (slime-presentation-around-click event)
    (slime-pretty-print-presentation presentation)))

(defun slime-pretty-print-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation) 
      (slime-presentation-around-or-before-point-or-error point)
    (slime-pretty-print-presentation presentation)))

(defun slime-mark-presentation (point)
  (interactive "d")
  (multiple-value-bind (presentation start end) 
      (slime-presentation-around-or-before-point-or-error point)
    (goto-char start)
    (push-mark end nil t)))

(defun slime-previous-presentation (&optional arg)
  "Move point to the beginning of the first presentation before point.
With ARG, do this that many times.
A negative argument means move forward instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (slime-next-presentation (- arg)))

(defun slime-next-presentation (&optional arg)
  "Move point to the beginning of the next presentation after point.
With ARG, do this that many times.
A negative argument means move backward instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((plusp arg)
    (dotimes (i arg)
      ;; First skip outside the current surrounding presentation (if any)
      (multiple-value-bind (presentation start end) 
	  (slime-presentation-around-point (point))
	(when presentation
	  (goto-char end)))
      (let ((p (next-single-property-change (point) 'slime-repl-presentation)))
	(unless p 
	  (error "No next presentation"))
	(multiple-value-bind (presentation start end) 
	    (slime-presentation-around-or-before-point-or-error p)
	  (goto-char start)))))
   ((minusp arg)
    (dotimes (i (- arg))
      ;; First skip outside the current surrounding presentation (if any)
      (multiple-value-bind (presentation start end)
	  (slime-presentation-around-point (point))
	(when presentation
	  (goto-char start)))
      (let ((p (previous-single-property-change (point) 'slime-repl-presentation)))
	(unless p 
	  (error "No previous presentation"))
	(multiple-value-bind (presentation start end) 
	    (slime-presentation-around-or-before-point-or-error p)
	  (goto-char start)))))))

(defvar slime-presentation-map (make-sparse-keymap))

(define-key  slime-presentation-map [mouse-2] 'slime-copy-or-inspect-presentation-at-mouse)
(define-key  slime-presentation-map [mouse-3] 'slime-presentation-menu)

(when (featurep 'xemacs)
  (define-key  slime-presentation-map [button2] 'slime-copy-or-inspect-presentation-at-mouse)
  (define-key  slime-presentation-map [button3] 'slime-presentation-menu))

;; protocol for handling up a menu.
;; 1. Send lisp message asking for menu choices for this object. 
;;    Get back list of strings.
;; 2. Let used choose
;; 3. Call back to execute menu choice, passing nth and string of choice

(defun slime-menu-choices-for-presentation (presentation buffer from to choice-to-lambda)
  "Return a menu for `presentation' at `from'--`to' in `buffer', suitable for `x-popup-menu'."
  (let* ((what (slime-presentation-id presentation))
         (choices (with-current-buffer buffer
                    (slime-eval 
                     `(swank::menu-choices-for-presentation-id ',what)))))
    (flet ((savel (f) ;; IMPORTANT - xemacs can't handle lambdas in x-popup-menu. So give them a name
            (let ((sym (gensym)))
              (setf (gethash sym choice-to-lambda) f)
              sym)))
    (etypecase choices
      (list
       `(,(format "Presentation %s" what)
         ("" 
	  ("Find Definition" . ,(savel 'slime-M-.-presentation-at-mouse))
          ("Inspect" . ,(savel 'slime-inspect-presentation-at-mouse))
          ("Describe" . ,(savel 'slime-describe-presentation-at-mouse))
          ("Pretty-print" . ,(savel 'slime-pretty-print-presentation-at-mouse))
          ("Copy to REPL" . ,(savel 'slime-copy-presentation-at-mouse-to-repl))
          ("Copy to kill ring" . ,(savel 'slime-copy-presentation-at-mouse-to-kill-ring))
          ,@(unless buffer-read-only 
              `(("Copy to point" . ,(savel 'slime-copy-presentation-at-mouse-to-point))))
          ,@(let ((nchoice 0))
              (mapcar 
               (lambda (choice)
                 (incf nchoice)
                 (cons choice 
                       (savel `(lambda ()
                          (interactive)
                          (slime-eval 
                           '(swank::execute-menu-choice-for-presentation-id
                             ',what ,nchoice ,(nth (1- nchoice) choices)))))))
               choices)))))
      (symbol                           ; not-present
       (with-current-buffer buffer
         (slime-remove-presentation-properties from to presentation))
       (sit-for 0)                      ; allow redisplay
       `("Object no longer recorded" 
         ("sorry" . ,(if (featurep 'xemacs) nil '(nil)))))))))

(defun slime-presentation-menu (event)
  (interactive "e")
  (let* ((point (if (featurep 'xemacs) (event-point event) 
                  (posn-point (event-end event))))
         (window (if (featurep 'xemacs) (event-window event) (caadr event)))
         (buffer (window-buffer window))
         (choice-to-lambda (make-hash-table)))
    (multiple-value-bind (presentation from to)
        (with-current-buffer buffer
          (slime-presentation-around-point point))
      (unless presentation
        (error "No presentation at event position"))
      (let ((menu (slime-menu-choices-for-presentation 
                   presentation buffer from to choice-to-lambda)))
        (let ((choice (x-popup-menu event menu)))
          (when choice
            (call-interactively (gethash choice choice-to-lambda))))))))

(defun slime-presentation-expression (presentation)
  "Return a string that contains a CL s-expression accessing 
the presented object."
  (let ((id (slime-presentation-id presentation)))
    (etypecase id
      (number
       ;; Make sure it works even if *read-base* is not 10.
       (format "(swank:lookup-presented-object-or-lose %d.)" id))
      (list
       ;; for frame variables and inspector parts
       (format "(swank:lookup-presented-object-or-lose '%s)" id)))))

(defun slime-buffer-substring-with-reified-output (start end)
  (let ((str-props (buffer-substring start end))
        (str-no-props (buffer-substring-no-properties start end)))
    (slime-reify-old-output str-props str-no-props)))

(defun slime-reify-old-output (str-props str-no-props)
  (let ((pos (slime-property-position 'slime-repl-presentation str-props)))
    (if (null pos)
        str-no-props
        (multiple-value-bind (presentation start-pos end-pos whole-p)
            (slime-presentation-around-point pos str-props)
          (if (not presentation)
              str-no-props
              (concat (substring str-no-props 0 pos)
                      ;; Eval in the reader so that we play nice with quote.
                      ;; -luke (19/May/2005)
                      "#." (slime-presentation-expression presentation)
                      (slime-reify-old-output (substring str-props end-pos)
                                              (substring str-no-props end-pos))))))))



(defun slime-repl-grab-old-output (replace)
  "Resend the old REPL output at point.  
If replace it non-nil the current input is replaced with the old
output; otherwise the new input is appended."
  (multiple-value-bind (presentation beg end) 
      (slime-presentation-around-or-before-point (point))
    (slime-check-presentation beg end (current-buffer) presentation)
    (let ((old-output (buffer-substring beg end))) ;;keep properties
      ;; Append the old input or replace the current input
      (cond (replace (goto-char slime-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (let ((inhibit-read-only t))
        (insert old-output)))))

;;; Presentation-related key bindings, non-context menu

(defvar slime-presentation-command-map nil
  "Keymap for presentation-related commands. Bound to a prefix key.")

(defvar slime-presentation-bindings
  '((?i slime-inspect-presentation-at-point)
    (?d slime-describe-presentation-at-point)
    (?w slime-copy-presentation-at-point-to-kill-ring)
    (?r slime-copy-presentation-at-point-to-repl)
    (?p slime-previous-presentation)
    (?n slime-next-presentation)
    (?\  slime-mark-presentation)))

(defun slime-presentation-init-keymaps ()
  (slime-init-keymap 'slime-presentation-command-map nil t 
		     slime-presentation-bindings)
  (define-key slime-presentation-command-map "\M-o" 'slime-clear-presentations)
  ;; C-c C-v is the prefix for the presentation-command map.
  (define-key slime-prefix-map "\C-v" slime-presentation-command-map))

(defun slime-presentation-around-or-before-point-p ()
  (multiple-value-bind (presentation beg end) 
      (slime-presentation-around-or-before-point (point))
    presentation))

(defvar slime-presentation-easy-menu
  (let ((P '(slime-presentation-around-or-before-point-p)))
    `("Presentations"
      [ "Find Definition" slime-M-.-presentation-at-point ,P ]
      [ "Inspect" slime-inspect-presentation-at-point ,P ]
      [ "Describe" slime-describe-presentation-at-point ,P ]
      [ "Pretty-print" slime-pretty-print-presentation-at-point ,P ]
      [ "Copy to REPL" slime-copy-presentation-at-point-to-repl ,P ]
      [ "Copy to kill ring" slime-copy-presentation-at-point-to-kill-ring ,P ]
      [ "Mark" slime-mark-presentation ,P ]
      "--"
      [ "Previous presentation" slime-previous-presentation ]
      [ "Next presentation" slime-next-presentation ]
      "--"
      [ "Clear all presentations" slime-clear-presentations ])))

(defun slime-presentation-add-easy-menu ()
  (easy-menu-define menubar-slime-presentation slime-mode-map "Presentations" slime-presentation-easy-menu)
  (easy-menu-define menubar-slime-presentation slime-repl-mode-map "Presentations" slime-presentation-easy-menu)
  (easy-menu-define menubar-slime-presentation sldb-mode-map "Presentations" slime-presentation-easy-menu)
  (easy-menu-define menubar-slime-presentation slime-inspector-mode-map "Presentations" slime-presentation-easy-menu)
  (easy-menu-add slime-presentation-easy-menu 'slime-mode-map)
  (easy-menu-add slime-presentation-easy-menu 'slime-repl-mode-map)
  (easy-menu-add slime-presentation-easy-menu 'sldb-mode-map)
  (easy-menu-add slime-presentation-easy-menu 'slime-inspector-mode-map))

;;; hook functions (hard to isolate stuff)

(defun slime-dispatch-presentation-event (event)
  (destructure-case event
    ((:presentation-start id &optional target)
     (slime-mark-presentation-start id target)
     t)
    ((:presentation-end id &optional target)
     (slime-mark-presentation-end id target)
     t)
    (t nil)))

(defun slime-presentation-write-result (string)
  (with-current-buffer (slime-output-buffer)
    (let ((marker (slime-output-target-marker :repl-result)))
      (goto-char marker)
      (slime-propertize-region `(face slime-repl-result-face
                                      rear-nonsticky (face))
        (insert string))
      ;; Move the input-start marker after the REPL result.
      (set-marker marker (point)))
    (slime-repl-show-maximum-output)))

(defun slime-presentation-write (string &optional target)
  (case target
    ((nil)                              ; Regular process output
     (slime-repl-emit string))
    (:repl-result                       
     (slime-presentation-write-result string))
    (t (slime-emit-to-target string target))))

(defun slime-presentation-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer. Presentations of old results are expanded into code."
  (slime-buffer-substring-with-reified-output  slime-repl-input-start-mark
					       (point-max)))

(defun slime-presentation-on-return-pressed ()
  (when (and (car (slime-presentation-around-or-before-point (point)))
             (< (point) slime-repl-input-start-mark))
    (slime-repl-grab-old-output end-of-input)
    (slime-repl-recenter-if-needed)
    t))

(defun slime-presentation-on-stream-open (stream)
  (require 'bridge)
  (defun bridge-insert (process output)
    (slime-output-filter process (or output "")))
  (install-bridge)
  (setq bridge-destination-insert nil)
  (setq bridge-source-insert nil)
  (setq bridge-handlers 
	(list* '("<" . slime-mark-presentation-start-handler) 
	       '(">" . slime-mark-presentation-end-handler)
	       bridge-handlers)))

(defun slime-clear-presentations ()
  "Forget all objects associated to SLIME presentations.
This allows the garbage collector to remove these objects
even on Common Lisp implementations without weak hash tables."
  (interactive)
  (slime-eval-async `(swank:clear-repl-results))
  (unless (eql major-mode 'slime-repl-mode)
    (slime-switch-to-output-buffer))
  (slime-for-each-presentation-in-region 1 (1+ (buffer-size)) 
					 (lambda (presentation from to whole-p)
					   (slime-remove-presentation-properties from to 
										 presentation))))

(defun slime-presentation-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (destructure-case ispec
      ((:value string id)
       (slime-propertize-region 
           (list 'slime-part-number id 
                 'mouse-face 'highlight
                 'face 'slime-inspector-value-face)
         (slime-insert-presentation string `(:inspected-part ,id) t)))
      ((:label string)
       (insert (slime-inspector-fontify label string)))
      ((:action string id)
       (slime-insert-propertized (list 'slime-action-number id
                                       'mouse-face 'highlight
                                       'face 'slime-inspector-action-face)
                                 string)))))

(defun slime-presentation-sldb-insert-frame-variable-value (value frame index)
  (slime-insert-presentation
   (in-sldb-face local-value value)
   `(:frame-var ,slime-current-thread ,(car frame) ,index) t))

(provide 'slime-presentations)

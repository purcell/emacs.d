;;; -*- coding: utf-8; lexical-binding: t -*-
;;;
;;; slime-trace-dialog.el -- a navigable dialog of inspectable trace entries
;;;
;;; TODO: implement better wrap interface for sbcl method, labels and such
;;; TODO: backtrace printing is very slow
;;;
(require 'slime)
(require 'slime-parse)
(require 'slime-repl)
(require 'cl-lib)

(define-slime-contrib slime-trace-dialog
  "Provide an interfactive trace dialog buffer for managing and
inspecting details of traced functions. Invoke this dialog with C-c T."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:swank-dependencies swank-trace-dialog)
  (:on-load (add-hook 'slime-mode-hook 'slime-trace-dialog-enable)
            (add-hook 'slime-repl-mode-hook 'slime-trace-dialog-enable))
  (:on-unload (remove-hook 'slime-mode-hook 'slime-trace-dialog-enable)
              (remove-hook 'slime-repl-mode-hook 'slime-trace-dialog-enable)))


;;;; Variables
;;;
(defvar slime-trace-dialog-flash t
  "Non-nil means flash the updated region of the SLIME Trace Dialog. ")

(defvar slime-trace-dialog--specs-overlay nil)

(defvar slime-trace-dialog--progress-overlay nil)

(defvar slime-trace-dialog--tree-overlay nil)

(defvar slime-trace-dialog--collapse-chars (cons "-" "+"))


;;;; Local trace entry model
(defvar slime-trace-dialog--traces nil)

(cl-defstruct (slime-trace-dialog--trace
               (:constructor slime-trace-dialog--make-trace))
  id
  parent
  spec
  args
  retlist
  depth
  beg
  end
  collapse-button-marker
  summary-beg
  children-end
  collapsed-p)

(defun slime-trace-dialog--find-trace (id)
  (gethash id slime-trace-dialog--traces))


;;;; Modes and mode maps
;;;
(defvar slime-trace-dialog-mode-map
  (let ((map (make-sparse-keymap))
        (remaps '((slime-inspector-operate-on-point . nil)
                  (slime-inspector-operate-on-click . nil)
                  (slime-inspector-reinspect
                   . slime-trace-dialog-fetch-status)
                  (slime-inspector-next-inspectable-object
                   . slime-trace-dialog-next-button)
                  (slime-inspector-previous-inspectable-object
                   . slime-trace-dialog-prev-button))))
    (set-keymap-parent map slime-inspector-mode-map)
    (cl-loop for (old . new) in remaps
             do (substitute-key-definition old new map))
    (set-keymap-parent map slime-parent-map)
    (define-key map (kbd "G") 'slime-trace-dialog-fetch-traces)
    (define-key map (kbd "C-k") 'slime-trace-dialog-clear-fetched-traces)
    (define-key map (kbd "g") 'slime-trace-dialog-fetch-status)
    (define-key map (kbd "M-RET") 'slime-trace-dialog-copy-down-to-repl)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode slime-trace-dialog-mode fundamental-mode
  "SLIME Trace Dialog" "Mode for controlling SLIME's Trace Dialog"
  (set-syntax-table lisp-mode-syntax-table)
  (read-only-mode 1)
  (add-to-list (make-local-variable 'slime-trace-dialog-after-toggle-hook)
               'slime-trace-dialog-fetch-status))

(define-derived-mode slime-trace-dialog--detail-mode slime-inspector-mode
  "SLIME Trace Detail"
  "Mode for viewing a particular trace from SLIME's Trace Dialog")

(setq slime-trace-dialog--detail-mode-map
      (let ((map (make-sparse-keymap))
            (remaps '((slime-inspector-next-inspectable-object
                       . slime-trace-dialog-next-button)
                      (slime-inspector-previous-inspectable-object
                       . slime-trace-dialog-prev-button))))
        (set-keymap-parent map slime-trace-dialog-mode-map)
        (cl-loop for (old . new) in remaps
                 do (substitute-key-definition old new map))
        map))

(defvar slime-trace-dialog-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c T") 'slime-trace-dialog)
    (define-key map (kbd "C-c M-t") 'slime-trace-dialog-toggle-trace)
    map))

(define-minor-mode slime-trace-dialog-minor-mode
  "Add keybindings for accessing SLIME's Trace Dialog.")

(defun slime-trace-dialog-enable ()
  (slime-trace-dialog-minor-mode 1))

(easy-menu-define slime-trace-dialog--menubar (list slime-trace-dialog-minor-mode-map
                                                    slime-trace-dialog-mode-map)
  "A menu for accessing some features of SLIME's Trace Dialog"
  (let* ((in-dialog '(eq major-mode 'slime-trace-dialog-mode))
         (dialog-live `(and ,in-dialog
                            (memq slime-buffer-connection slime-net-processes)))
         (connected '(slime-connected-p)))
    `("Trace"
      ["Toggle trace" slime-trace-dialog-toggle-trace ,connected]
      ["Trace complex spec" slime-trace-dialog-toggle-complex-trace ,connected]
      ["Open Trace dialog" slime-trace-dialog (and ,connected (not ,in-dialog))]
      "--"
      [ "Refresh traces and progress" slime-trace-dialog-fetch-status ,dialog-live]
      [ "Fetch next batch" slime-trace-dialog-fetch-traces ,dialog-live]
      [ "Clear all fetched traces" slime-trace-dialog-clear-fetched-traces ,dialog-live]
      [ "Toggle details" slime-trace-dialog-hide-details-mode ,in-dialog]
      [ "Toggle autofollow" slime-trace-dialog-autofollow-mode ,in-dialog])))

(define-minor-mode slime-trace-dialog-hide-details-mode
  "Hide details in `slime-trace-dialog-mode'"
  nil " Brief"    
  :group 'slime-trace-dialog
  (unless (derived-mode-p 'slime-trace-dialog-mode)
    (error "Not a SLIME Trace Dialog buffer"))
  (slime-trace-dialog--set-hide-details-mode))

(define-minor-mode slime-trace-dialog-autofollow-mode
  "Automatically open buffers with trace details from `slime-trace-dialog-mode'"
  nil " Autofollow"
  :group 'slime-trace-dialog
  (unless (derived-mode-p 'slime-trace-dialog-mode)
    (error "Not a SLIME Trace Dialog buffer")))


;;;; Helper functions
;;;
(defun slime-trace-dialog--call-refreshing (buffer
                                            overlay
                                            dont-erase
                                            recover-point-p
                                            fn)
  (with-current-buffer buffer
    (let ((inhibit-point-motion-hooks t)
          (inhibit-read-only t)
          (saved (point)))
      (save-restriction
        (when overlay
          (narrow-to-region (overlay-start overlay)
                            (overlay-end overlay)))
        (unwind-protect
            (if dont-erase
                (goto-char (point-max))
              (delete-region (point-min) (point-max)))
          (funcall fn)
          (when recover-point-p
            (goto-char saved)))
        (when slime-trace-dialog-flash
          (slime-flash-region (point-min) (point-max)))))
    buffer))

(cl-defmacro slime-trace-dialog--refresh ((&key
                                           overlay
                                           dont-erase
                                           recover-point-p
                                           buffer)
                                          &rest body)
  (declare (indent 1)
           (debug (sexp &rest form)))
  `(slime-trace-dialog--call-refreshing ,(or buffer
                                             `(current-buffer))
                                        ,overlay
                                        ,dont-erase
                                        ,recover-point-p
                                        #'(lambda () ,@body)))

(defmacro slime-trace-dialog--insert-and-overlay (string overlay)
  `(save-restriction
     (let ((inhibit-read-only t))
       (narrow-to-region (point) (point))
       (insert ,string "\n")
       (set (make-local-variable ',overlay)
            (let ((overlay (make-overlay (point-min)
                                         (point-max)
                                         (current-buffer)
                                         nil
                                         t)))
              (move-overlay overlay (overlay-start overlay)
                            (1- (overlay-end overlay)))
              ;; (overlay-put overlay 'face '(:background "darkslategrey"))
              overlay)))))

(defun slime-trace-dialog--buffer-name ()
  (format "*traces for %s*"
          (slime-connection-name slime-default-connection)))

(defun slime-trace-dialog--live-dialog (&optional buffer-or-name)
  (let ((buffer-or-name (or buffer-or-name
                            (slime-trace-dialog--buffer-name))))
    (and (buffer-live-p (get-buffer buffer-or-name))
       (with-current-buffer buffer-or-name
         (memq slime-buffer-connection slime-net-processes))
       buffer-or-name)))

(defun slime-trace-dialog--ensure-buffer ()
  (let ((name (slime-trace-dialog--buffer-name)))
    (or (slime-trace-dialog--live-dialog name)
        (with-current-buffer (get-buffer-create name)
          (let ((inhibit-read-only t))
            (erase-buffer))
          (slime-trace-dialog-mode)
          (save-excursion
            (buffer-disable-undo)
            (slime-trace-dialog--insert-and-overlay
             "[waiting for the traced specs to be available]"
             slime-trace-dialog--specs-overlay)
            (slime-trace-dialog--insert-and-overlay
             "[waiting for some info on trace download progress ]"
             slime-trace-dialog--progress-overlay)
            (slime-trace-dialog--insert-and-overlay
             "[waiting for the actual traces to be available]"
             slime-trace-dialog--tree-overlay)
            (current-buffer))
          (setq slime-buffer-connection slime-default-connection)
          (current-buffer)))))

(defun slime-trace-dialog--make-autofollow-fn (id)
  (let ((requested nil))
    #'(lambda (_before after)
        (let ((inhibit-point-motion-hooks t)
              (id-after (get-text-property after 'slime-trace-dialog--id)))
          (when (and (= after (point))
                     slime-trace-dialog-autofollow-mode
                     id-after
                     (= id-after id)
                     (not requested))
            (setq requested t)
            (slime-eval-async `(swank-trace-dialog:report-trace-detail
                                ,id-after)
              #'(lambda (detail)
                  (setq requested nil)
                  (when detail
                    (let ((inhibit-point-motion-hooks t))
                      (slime-trace-dialog--open-detail detail
                                                       'no-pop))))))))))

(defun slime-trace-dialog--set-collapsed (collapsed-p trace button)
  (save-excursion
    (setf (slime-trace-dialog--trace-collapsed-p trace) collapsed-p)
    (slime-trace-dialog--go-replace-char-at
     button
     (if collapsed-p
         (cdr slime-trace-dialog--collapse-chars)
       (car slime-trace-dialog--collapse-chars)))
    (slime-trace-dialog--hide-unhide
     (slime-trace-dialog--trace-summary-beg trace)
     (slime-trace-dialog--trace-end trace)
     (if collapsed-p 1 -1))
    (slime-trace-dialog--hide-unhide
     (slime-trace-dialog--trace-end trace)
     (slime-trace-dialog--trace-children-end trace)
     (if collapsed-p 1 -1))))

(defun slime-trace-dialog--hide-unhide (start-pos end-pos delta)
  (cl-loop with inhibit-read-only = t
           for pos = start-pos then next
           for next = (next-single-property-change
                       pos
                       'slime-trace-dialog--hidden-level
                       nil
                       end-pos)
           for hidden-level = (+ (or (get-text-property
                                      pos
                                      'slime-trace-dialog--hidden-level)
                                     0)
                                 delta)
           do (add-text-properties pos next
                                   (list 'slime-trace-dialog--hidden-level
                                         hidden-level
                                         'invisible
                                         (cl-plusp hidden-level)))
           while (< next end-pos)))

(defun slime-trace-dialog--set-hide-details-mode ()
  (cl-loop for trace being the hash-values of slime-trace-dialog--traces
           do (slime-trace-dialog--hide-unhide
               (slime-trace-dialog--trace-summary-beg trace)
               (slime-trace-dialog--trace-end trace)
               (if slime-trace-dialog-hide-details-mode 1 -1))))

(defun slime-trace-dialog--format-part (part-id part-text trace-id type)
  (slime-trace-dialog--button
   (format "%s" part-text)
   #'(lambda (_button)
       (slime-eval-async
           `(swank-trace-dialog:inspect-trace-part ,trace-id ,part-id ,type)
         #'slime-open-inspector))
   'mouse-face 'highlight
   'slime-trace-dialog--part-id part-id
   'slime-trace-dialog--type type
   'face 'slime-inspector-value-face))

(defun slime-trace-dialog--format-trace-entry (id external)
  (slime-trace-dialog--button
   (format "%s" external)
   #'(lambda (_button)
       (slime-eval-async
           `(swank::inspect-object (swank-trace-dialog::find-trace ,id))
         #'slime-open-inspector))
   'face 'slime-inspector-value-face))

(defun slime-trace-dialog--format (fmt-string &rest args)
  (let* ((string (apply #'format fmt-string args))
         (indent (make-string (max 2
                                   (- 50 (length string))) ? )))
    (format "%s%s" string indent)))

(defun slime-trace-dialog--button (title lambda &rest props)
  (let ((string (format "%s" title)))
    (apply #'make-text-button string nil
           'action     #'(lambda (button)
                           (funcall lambda button))
           'mouse-face 'highlight
           'face       'slime-inspector-action-face
           props)
    string))

(defun slime-trace-dialog--call-maintaining-properties (pos fn)
  (save-excursion
    (goto-char pos)
    (let* ((saved-props (text-properties-at pos))
           (saved-point (point))
           (inhibit-read-only t)
           (inhibit-point-motion-hooks t))
      (funcall fn)
      (add-text-properties saved-point (point) saved-props)
      (if (markerp pos) (set-marker pos saved-point)))))

(cl-defmacro slime-trace-dialog--maintaining-properties (pos
                                                         &body body)
  (declare (indent 1))
  `(slime-trace-dialog--call-maintaining-properties ,pos #'(lambda () ,@body)))

(defun slime-trace-dialog--go-replace-char-at (pos char)
  (slime-trace-dialog--maintaining-properties pos
    (delete-char 1)
    (insert char)))


;;;; Handlers for the *trace-dialog* and *trace-detail* buffers
;;;
(defun slime-trace-dialog--open-specs (traced-specs)
  (cl-labels ((make-report-spec-fn
               (&optional form)
               #'(lambda (_button)
                   (slime-eval-async
                       `(cl:progn
                         ,form
                         (swank-trace-dialog:report-specs))
                     #'(lambda (results)
                         (slime-trace-dialog--open-specs results))))))
    (slime-trace-dialog--refresh
        (:overlay slime-trace-dialog--specs-overlay
                  :recover-point-p t)
      (insert
       (slime-trace-dialog--format "Traced specs (%s)" (length traced-specs))
       (slime-trace-dialog--button "[refresh]"
                                   (make-report-spec-fn))
       "\n" (make-string 50 ? )
       (slime-trace-dialog--button
        "[untrace all]"
        (make-report-spec-fn `(swank-trace-dialog:dialog-untrace-all)))
       "\n\n")
      (cl-loop for spec in traced-specs
               do (insert
                   "  "
                   (slime-trace-dialog--button
                    "[untrace]"
                    (make-report-spec-fn
                     `(swank-trace-dialog:dialog-untrace ',spec)))
                   (format " %s" spec)
                   "\n")))))

(defvar slime-trace-dialog--fetch-key nil)

(defvar slime-trace-dialog--stop-fetching nil)

(defun slime-trace-dialog--update-progress (total &optional show-stop-p remaining-p)
  ;; `remaining-p' indicates `total' is the number of remaining traces.
  (slime-trace-dialog--refresh
      (:overlay slime-trace-dialog--progress-overlay
                :recover-point-p t)
    (let* ((done (hash-table-count slime-trace-dialog--traces))
           (total (if remaining-p (+ done total) total)))
      (insert
       (slime-trace-dialog--format "Trace collection status (%d/%s)"
                                   done
                                   (or total "0"))
       (slime-trace-dialog--button "[refresh]"
                                   #'(lambda (_button)
                                       (slime-trace-dialog-fetch-progress))))

      (when (and total (cl-plusp (- total done)))
        (insert "\n" (make-string 50 ? )
                (slime-trace-dialog--button
                 "[fetch next batch]"
                 #'(lambda (_button)
                     (slime-trace-dialog-fetch-traces nil)))
                "\n" (make-string 50 ? )
                (slime-trace-dialog--button
                 "[fetch all]"
                 #'(lambda (_button)
                     (slime-trace-dialog-fetch-traces t)))))
      (when total
        (insert "\n" (make-string 50 ? )
                (slime-trace-dialog--button
                 "[clear]"
                 #'(lambda (_button)
                     (slime-trace-dialog-clear-fetched-traces)))))
      (when show-stop-p
        (insert "\n" (make-string 50 ? )
                (slime-trace-dialog--button
                 "[stop]"
                 #'(lambda (_button)
                     (setq slime-trace-dialog--stop-fetching t)))))
      (insert "\n\n"))))

(defun slime-trace-dialog--open-detail (trace-tuple &optional no-pop)
  (slime-with-popup-buffer ("*trace-detail*" :select (not no-pop)
                            :mode 'slime-trace-dialog--detail-mode)
    (cl-destructuring-bind (id _parent-id _spec args retlist backtrace external)
        trace-tuple
      (let ((headline (slime-trace-dialog--format-trace-entry id external)))
        (setq headline (format "%s\n%s\n"
                               headline
                               (make-string (length headline) ?-)))
        (insert headline))
      (cl-loop for (type objects label)
               in `((:arg ,args   "Called with args:")
                    (:retval ,retlist "Returned values:"))
               do (insert (format "\n%s\n" label))
               (insert (cl-loop for object in objects
                                for i from 0
                                concat (format "   %s: %s\n" i
                                               (slime-trace-dialog--format-part
                                                (cl-first object)
                                                (cl-second object)
                                                id
                                                type)))))
      (when backtrace
        (insert "\nBacktrace:\n"
                (cl-loop for (i spec) in backtrace
                         concat (format "   %s: %s\n" i spec)))))))


;;;; Rendering traces
;;;
(defun slime-trace-dialog--draw-tree-lines (start offset direction)
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (goto-char start)
      (cl-loop with replace-set = (if (eq direction 'down)
                                      '(? )
                                    '(?  ?`))
               for line-beginning = (line-beginning-position
                                     (if (eq direction 'down)
                                         2 0))
               for pos = (+ line-beginning offset)
               while (and (< (point-min) line-beginning)
                          (< line-beginning (point-max))
                          (memq (char-after pos) replace-set))
               do
               (slime-trace-dialog--go-replace-char-at pos "|")
               (goto-char pos)))))

(defun slime-trace-dialog--make-indent (depth suffix)
  (concat (make-string (* 3 (max 0 (1- depth))) ? )
          (if (cl-plusp depth) suffix)))

(defun slime-trace-dialog--make-collapse-button (trace)
  (slime-trace-dialog--button (if (slime-trace-dialog--trace-collapsed-p trace)
                                  (cdr slime-trace-dialog--collapse-chars)
                                (car slime-trace-dialog--collapse-chars))
                              #'(lambda (button)
                                  (slime-trace-dialog--set-collapsed
                                   (not (slime-trace-dialog--trace-collapsed-p
                                         trace))
                                   trace
                                   button))))


(defun slime-trace-dialog--insert-trace (trace)
  (let* ((id (slime-trace-dialog--trace-id trace))
         (parent (slime-trace-dialog--trace-parent trace))
         (has-children-p (slime-trace-dialog--trace-children-end trace))
         (indent-spec (slime-trace-dialog--make-indent
                       (slime-trace-dialog--trace-depth trace)
                       "`--"))
         (indent-summary (slime-trace-dialog--make-indent
                          (slime-trace-dialog--trace-depth trace)
                          "   "))
         (autofollow-fn (slime-trace-dialog--make-autofollow-fn id))
         (id-string (slime-trace-dialog--button
                     (format "%4s" id)
                     #'(lambda (_button)
                         (slime-eval-async
                             `(swank-trace-dialog:report-trace-detail
                               ,id)
                           #'slime-trace-dialog--open-detail))))
         (spec (slime-trace-dialog--trace-spec trace))
         (summary (cl-loop for (type objects marker) in
                           `((:arg    ,(slime-trace-dialog--trace-args trace)
                                      " > ")
                             (:retval ,(slime-trace-dialog--trace-retlist trace)
                                      " < "))
                           concat (cl-loop for object in objects
                                           concat "      "
                                           concat indent-summary
                                           concat marker
                                           concat (slime-trace-dialog--format-part
                                                   (cl-first object)
                                                   (cl-second object)
                                                   id
                                                   type)
                                           concat "\n"))))
    (puthash id trace slime-trace-dialog--traces)
    ;; insert and propertize the text
    ;;
    (setf (slime-trace-dialog--trace-beg trace) (point-marker))
    (insert id-string " ")
    (insert indent-spec)
    (if has-children-p
        (insert (slime-trace-dialog--make-collapse-button trace))
      (setf (slime-trace-dialog--trace-collapse-button-marker trace)
            (point-marker))
      (insert "-"))
    (insert (format " %s\n" spec))
    (setf (slime-trace-dialog--trace-summary-beg trace) (point-marker))
    (insert summary)
    (setf (slime-trace-dialog--trace-end trace) (point-marker))
    (set-marker-insertion-type (slime-trace-dialog--trace-beg trace) t)

    (add-text-properties (slime-trace-dialog--trace-beg trace)
                         (slime-trace-dialog--trace-end trace)
                         (list 'slime-trace-dialog--id id
                               'point-entered autofollow-fn
                               'point-left autofollow-fn))
    ;; respect brief mode and collapsed state
    ;;
    (cl-loop for condition in (list slime-trace-dialog-hide-details-mode
                                    (slime-trace-dialog--trace-collapsed-p trace))
             when condition
             do (slime-trace-dialog--hide-unhide
                 (slime-trace-dialog--trace-summary-beg
                  trace)
                 (slime-trace-dialog--trace-end trace)
                 1))
    (cl-loop for tr = trace then parent
             for parent = (slime-trace-dialog--trace-parent tr)
             while parent
             when (slime-trace-dialog--trace-collapsed-p parent)
             do (slime-trace-dialog--hide-unhide
                 (slime-trace-dialog--trace-beg trace)
                 (slime-trace-dialog--trace-end trace)
                 (+ 1
                    (or (get-text-property (slime-trace-dialog--trace-beg parent)
                                           'slime-trace-dialog--hidden-level)
                        0)))
             (cl-return))
    ;; maybe add the collapse-button to the parent in case it didn't
    ;; have one already
    ;;
    (when (and parent
               (slime-trace-dialog--trace-collapse-button-marker parent))
      (slime-trace-dialog--maintaining-properties
          (slime-trace-dialog--trace-collapse-button-marker parent)
        (delete-char 1)
        (insert (slime-trace-dialog--make-collapse-button parent))
        (setf (slime-trace-dialog--trace-collapse-button-marker parent)
              nil)))
    ;; draw the tree lines
    ;;
    (when parent
      (slime-trace-dialog--draw-tree-lines (slime-trace-dialog--trace-beg trace)
                                           (+ 2 (length indent-spec))
                                           'up))
    (when has-children-p
      (slime-trace-dialog--draw-tree-lines (slime-trace-dialog--trace-beg trace)
                                           (+ 5 (length indent-spec))
                                           'down))
    ;; set the "children-end" slot
    ;;
    (unless (slime-trace-dialog--trace-children-end trace)
      (cl-loop for parent = trace
               then (slime-trace-dialog--trace-parent parent)
               while parent
               do
               (setf (slime-trace-dialog--trace-children-end parent)
                     (slime-trace-dialog--trace-end trace))))))

(defun slime-trace-dialog--render-trace (trace)
  ;; Render the trace entry in the appropriate place.
  ;;
  ;; A trace becomes a few lines of slightly propertized text in the
  ;; buffer, inserted by `slime-trace-dialog--insert-trace', bound by
  ;; point markers that we use here.
  ;;
  ;; The new trace might be replacing an existing one, or otherwise
  ;; must be placed under its existing parent which might or might not
  ;; be the last entry inserted.
  ;;
  (let ((existing (slime-trace-dialog--find-trace
                   (slime-trace-dialog--trace-id trace)))
        (parent (slime-trace-dialog--trace-parent trace)))
    (cond (existing
           ;; Other traces might already reference `existing' and with
           ;; need to maintain that eqness. Best way to do that is
           ;; destructively modify `existing' with the new retlist...
           ;;
           (setf (slime-trace-dialog--trace-retlist existing)
                 (slime-trace-dialog--trace-retlist trace))
           ;; Now, before deleting and re-inserting `existing' at an
           ;; arbitrary point in the tree, note that it's
           ;; "children-end" marker is already non-nil, and informs us
           ;; about its parenthood status. We want to 1. leave it
           ;; alone if it's already a parent, or 2. set it to nil if
           ;; it's a leaf, thus forcing the needed update of the
           ;; parents' "children-end" marker.
           ;;
           (when (= (slime-trace-dialog--trace-children-end existing)
                    (slime-trace-dialog--trace-end existing))
             (setf (slime-trace-dialog--trace-children-end existing) nil))
           (delete-region (slime-trace-dialog--trace-beg existing)
                          (slime-trace-dialog--trace-end existing))
           (goto-char (slime-trace-dialog--trace-end existing))
           ;; Remember to set `trace' to be `existing'
           ;;
           (setq trace existing))
          (parent
           (goto-char (1+ (slime-trace-dialog--trace-children-end parent))))
          (;; top level trace
           t
           (goto-char (point-max))))
    (goto-char (line-beginning-position))
    (slime-trace-dialog--insert-trace trace)))

(defun slime-trace-dialog--update-tree (tuples)
  (save-excursion
    (slime-trace-dialog--refresh
        (:overlay slime-trace-dialog--tree-overlay
                  :dont-erase t)
      (cl-loop for tuple in tuples
               for parent = (slime-trace-dialog--find-trace (cl-second tuple))
               for trace = (slime-trace-dialog--make-trace
                            :id (cl-first tuple)
                            :parent parent
                            :spec (cl-third tuple)
                            :args (cl-fourth tuple)
                            :retlist (cl-fifth tuple)
                            :depth (if parent
                                       (1+ (slime-trace-dialog--trace-depth
                                            parent))
                                     0))
               do (slime-trace-dialog--render-trace trace)))))

(defun slime-trace-dialog--clear-local-tree ()
  (set (make-local-variable 'slime-trace-dialog--fetch-key)
       (cl-gensym "slime-trace-dialog-fetch-key-"))
  (set (make-local-variable 'slime-trace-dialog--traces)
       (make-hash-table))
  (slime-trace-dialog--refresh
      (:overlay slime-trace-dialog--tree-overlay))
  (slime-trace-dialog--update-progress nil))

(defun slime-trace-dialog--on-new-results (results &optional recurse)
  (cl-destructuring-bind (tuples remaining reply-key)
      results
    (cond ((and slime-trace-dialog--fetch-key
                (string= (symbol-name slime-trace-dialog--fetch-key)
                         (symbol-name reply-key)))
           (slime-trace-dialog--update-tree tuples)
           (slime-trace-dialog--update-progress
            remaining
            (and recurse
                 (cl-plusp remaining))
            t)
           (when (and recurse
                      (not (prog1 slime-trace-dialog--stop-fetching
                             (setq slime-trace-dialog--stop-fetching nil)))
                      (cl-plusp remaining))
             (slime-eval-async `(swank-trace-dialog:report-partial-tree
                                 ',reply-key)
               #'(lambda (results) (slime-trace-dialog--on-new-results
                                    results
                                    recurse))))))))


;;;; Interactive functions
;;;
(defun slime-trace-dialog-fetch-specs ()
  "Refresh just list of traced specs."
  (interactive)
  (slime-eval-async `(swank-trace-dialog:report-specs)
    #'slime-trace-dialog--open-specs))

(defun slime-trace-dialog-fetch-progress ()
  (interactive)
  (slime-eval-async
      '(swank-trace-dialog:report-total)
    #'(lambda (total)
        (slime-trace-dialog--update-progress
         total))))

(defun slime-trace-dialog-fetch-status ()
  "Refresh just the status part of the SLIME Trace Dialog"
  (interactive)
  (slime-trace-dialog-fetch-specs)
  (slime-trace-dialog-fetch-progress))

(defun slime-trace-dialog-clear-fetched-traces (&optional interactive)
  "Clear local and remote traces collected so far"
  (interactive "p")
  (when (or (not interactive)
            (y-or-n-p "Clear all collected and fetched traces?"))
    (slime-eval-async
        '(swank-trace-dialog:clear-trace-tree)
      #'(lambda (_ignored)
          (slime-trace-dialog--clear-local-tree)))))

(defun slime-trace-dialog-fetch-traces (&optional recurse)
  (interactive "P")
  (setq slime-trace-dialog--stop-fetching nil)
  (slime-eval-async `(swank-trace-dialog:report-partial-tree
                      ',slime-trace-dialog--fetch-key)
    #'(lambda (results) (slime-trace-dialog--on-new-results results
                                                            recurse))))

(defun slime-trace-dialog-next-button (&optional goback)
  (interactive)
  (let ((finder (if goback
                    #'previous-single-property-change
                  #'next-single-property-change)))
    (cl-loop for pos = (funcall finder (point) 'action)
             while pos
             do (goto-char pos)
             until (get-text-property pos 'action))))

(defun slime-trace-dialog-prev-button ()
  (interactive)
  (slime-trace-dialog-next-button 'goback))

(defvar slime-trace-dialog-after-toggle-hook nil
  "Hooks run after toggling a dialog-trace")

(defun slime-trace-dialog-toggle-trace (&optional using-context-p)
  "Toggle the dialog-trace of the spec at point.

When USING-CONTEXT-P, attempt to decipher lambdas. methods and
other complicated function specs."
  (interactive "P")
  ;; Notice the use of "spec strings" here as opposed to the
  ;; proper cons specs we use on the swank side.
  ;;
  ;; Notice the conditional use of `slime-trace-query' found in
  ;; swank-fancy-trace.el
  ;;
  (let* ((spec-string (if using-context-p
                          (slime-extract-context)
                        (slime-symbol-at-point)))
         (spec-string (if (fboundp 'slime-trace-query)
                          (slime-trace-query spec-string)
                        spec-string)))
    (message "%s" (slime-eval `(swank-trace-dialog:dialog-toggle-trace
                                (swank::from-string ,spec-string))))
    (run-hooks 'slime-trace-dialog-after-toggle-hook)))

(defun slime-trace-dialog--update-existing-dialog ()
  (let ((existing (slime-trace-dialog--live-dialog)))
    (when existing
      (with-current-buffer existing
        (slime-trace-dialog-fetch-status)))))

(add-hook 'slime-trace-dialog-after-toggle-hook
          'slime-trace-dialog--update-existing-dialog)

(defun slime-trace-dialog-toggle-complex-trace ()
  "Toggle the dialog-trace of the complex spec at point.

See `slime-trace-dialog-toggle-trace'."
  (interactive)
  (slime-trace-dialog-toggle-trace t))

(defun slime-trace-dialog (&optional clear-and-fetch)
  "Show trace dialog and refresh trace collection status.

With optional CLEAR-AND-FETCH prefix arg, clear the current tree
and fetch a first batch of traces."
  (interactive "P")
  (with-current-buffer
      (pop-to-buffer (slime-trace-dialog--ensure-buffer))
    (slime-trace-dialog-fetch-status)
    (when (or clear-and-fetch
              (null slime-trace-dialog--fetch-key))
      (slime-trace-dialog--clear-local-tree))
    (when clear-and-fetch
      (slime-trace-dialog-fetch-traces nil))))

(defun slime-trace-dialog-copy-down-to-repl (id part-id type)
  "Eval the Trace Dialog entry under point in the REPL (to set *)"
  (interactive (cl-loop for prop in '(slime-trace-dialog--id
                                      slime-trace-dialog--part-id
                                      slime-trace-dialog--type)
                        collect (get-text-property (point) prop)))
  (unless (and id part-id type) (error "No trace part at point %s" (point)))
  (slime-repl-send-string
   (format "%s" `(nth-value 0
                            (swank-trace-dialog::find-trace-part
                             ,id ,part-id ,type))))
  (slime-repl))

(provide 'slime-trace-dialog)

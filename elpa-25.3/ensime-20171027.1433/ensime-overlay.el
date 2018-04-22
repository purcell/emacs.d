;;; ensime-overlay.el --- ENSIME overlay support

;; Copyright (C) 2003 - 20165 the SLIME and ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;; shameless copypasta from cider-overlays.el
;;
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'ensime-compat))

;;; Customization
(defface ensime-result-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")))
  "Face used to display evaluation results at the end of line.
If `ensime-overlays-use-font-lock' is non-nil, this face is
applied with lower priority than the syntax highlighting."
  :group 'ensime-ui)

(defcustom ensime-overlays-use-font-lock nil
  "If non-nil, results overlays are font-locked as Scala code.
If nil, apply `ensime-result-overlay-face' to the entire overlay instead of
font-locking it."
  :group 'ensime-ui
  :type 'boolean)

(defcustom ensime-eval-result-prefix "=> "
  "The prefix displayed in the minibuffer before a result value."
  :type 'string
  :group 'ensime-ui)

(defcustom ensime-eval-result-duration 'command
  "Duration, in seconds, of ENSIME's eval-result overlays.
If nil, overlays last indefinitely.
If the symbol `command', they're erased after the next command."
  :type '(choice (integer :tag "Duration in seconds")
                 (const :tag "Until next command" command)
                 (const :tag "Last indefinitely" nil))
  :group 'ensime-ui)

;;; Overlay logic
(defun ensime--delete-overlay (ov &rest _)
  "Safely delete overlay OV.
Never throws errors, and can be used in an overlay's modification-hooks."
  (ignore-errors (delete-overlay ov)))

(defun ensime--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.
TYPE is a symbol put on the overlay's category property.  It is used to
easily remove all overlays from a region with:
    (remove-overlays start end 'category TYPE)
PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category type)
    (overlay-put o 'ensime-temporary t)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'ensime--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun ensime--remove-result-overlay ()
  "Remove result overlay from current buffer.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'ensime--remove-result-overlay 'local)
  (remove-overlays nil nil 'category 'result))

(defun ensime--remove-result-overlay-after-command ()
  "Add `ensime--remove-result-overlay' locally to `post-command-hook'.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'ensime--remove-result-overlay-after-command 'local)
  (add-hook 'post-command-hook #'ensime--remove-result-overlay nil 'local))

(cl-defun ensime--make-result-overlay (value &rest props &key where duration (type 'result)
                                             (format (concat " " ensime-eval-result-prefix "%s "))
                                             (prepend-face 'ensime-result-overlay-face)
                                             &allow-other-keys)
  "Place an overlay displaying VALUE at the end of line.
VALUE is used as the overlay's after-string property, meaning it is
displayed at the end of the overlay.  The overlay itself is placed from
beginning to end of current line.
Return nil if the overlay was not placed or if it might not be visible, and
return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it failed.

This function takes some optional keyword arguments:

  If WHERE is a number or a marker, apply the overlay over
  the entire line at that place (defaulting to `point').  If
  it is a cons cell, the car and cdr determine the start and
  end of the overlay.
  DURATION takes the same possible values as the
  `ensime-eval-result-duration' variable.
  TYPE is passed to `ensime--make-overlay' (defaults to `result').
  FORMAT is a string passed to `format'.  It should have
  exactly one %s construct (for VALUE).

All arguments beyond these (PROPS) are properties to be used on the
overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cdr (cdr props))))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
                 ((markerp where) (marker-buffer where))
                 ((markerp (car-safe where)) (marker-buffer (car where)))
                 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (when (number-or-marker-p where)
          (goto-char where))
        ;; Make sure the overlay is actually at the end of the sexp.
        (skip-chars-backward "\r\n[:blank:]")
        (let* ((beg (if (consp where)
                        (car where)
                      (save-excursion
                        ;;(clojure-backward-logical-sexp 1)
                        (point))))
               (end (if (consp where)
                        (cdr where)
                      (line-end-position)))
               (display-string (format format value))
               (o nil))
          (remove-overlays beg end 'category type)
          (funcall (if ensime-overlays-use-font-lock
                       #'font-lock-prepend-text-property
                     #'put-text-property)
                   0 (length display-string)
                   'face prepend-face
                   display-string)
          ;; If the display spans multiple lines or is very long, display it at
          ;; the beginning of the next line.
          (when (or (string-match "\n." display-string)
                    (> (string-width display-string)
                       (- (window-width) (current-column))))
            (setq display-string (concat " \n" display-string)))
          ;; Put the cursor property only once we're done manipulating the
          ;; string, since we want it to be at the first char.
          (put-text-property 0 1 'cursor 0 display-string)
          (when (> (string-width display-string) (* 3 (window-width)))
            (setq display-string
                  (concat (substring display-string 0 (* 3 (window-width)))
                          (substitute-command-keys
                           "...\nResult truncated."))))
          ;; Create the result overlay.
          (setq o (apply #'ensime--make-overlay
                         beg end type
                         'after-string display-string
                         props))
          (pcase duration
            ((pred numberp) (run-at-time duration nil #'ensime--delete-overlay o))
            (`command
             ;; If inside a command-loop, tell `ensime--remove-result-overlay'
             ;; to only remove after the *next* command.
             (if this-command
                 (add-hook 'post-command-hook
                           #'ensime--remove-result-overlay-after-command
                           nil 'local)
               (ensime--remove-result-overlay-after-command))))
          (when-let ((win (get-buffer-window buffer)))
            ;; Left edge is visible.
            (when (and (<= (window-start win) (point))
                       ;; In 24.3 `<=' is still a binary perdicate.
                       (<= (point) (window-end win))
                       ;; Right edge is visible. This is a little conservative
                       ;; if the overlay contains line breaks.
                       (or (< (+ (current-column) (string-width value))
                              (window-width win))
                           (not truncate-lines)))
              o)))))))

(provide 'ensime-overlay)
;;; ensime-overlays.el ends here

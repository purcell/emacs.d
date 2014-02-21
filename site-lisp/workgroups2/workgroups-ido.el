;;; workgroups-ido --- ido and iswitchb compatibility
;;; Commentary:
;;; Code:

(require 'workgroups-variables)

(require 'cl-lib)
(eval-when-compile
  (require 'ido)
  (require 'iswitchb))

(defun wg-read-buffer-mode ()
  "Return the buffer switching package (ido or iswitchb) to use, or nil."
  (if (eq wg-current-buffer-list-filter-id 'fallback) 'fallback
    (cl-case (let (workgroups-mode) (command-remapping 'switch-to-buffer))
      (ido-switch-buffer 'ido)
      (iswitchb-buffer 'iswitchb)
      (otherwise 'fallback))))

(defun wg-read-buffer-function (&optional mode)
  "Return MODE's or `wg-read-buffer-mode's `read-buffer' function."
  (cl-case (or mode (wg-read-buffer-mode))
    (ido 'ido-read-buffer)
    (iswitchb 'iswitchb-read-buffer)
    (fallback (lambda (prompt &optional default require-match)
                (let (read-buffer-function)
                  (read-buffer prompt default require-match))))))

;; TODO: clean this up
(defun wg-completing-read
  (prompt choices &optional pred require-match initial-input history default)
  "Do a completing read.  The function called depends on what's on."
  (cl-ecase (wg-read-buffer-mode)
    (ido
     (ido-completing-read prompt choices pred require-match
                          initial-input history default))
    (iswitchb
     (let* ((iswitchb-use-virtual-buffers nil)
            (iswitchb-make-buflist-hook
             (lambda () (setq iswitchb-temp-buflist choices))))
       (iswitchb-read-buffer prompt default require-match)))
    (fallback
     (completing-read prompt choices pred require-match
                      initial-input history default))))

(defun wg-current-matches (&optional read-buffer-mode)
  "Return READ-BUFFER-MODE's current matches."
  (cl-ecase (or read-buffer-mode (wg-read-buffer-mode))
    (ido (wg-when-boundp (ido-cur-list) ido-cur-list))
    (iswitchb (wg-when-boundp (iswitchb-buflist) iswitchb-buflist))
    (fallback (list minibuffer-default))))

(defun wg-current-match (&optional read-buffer-mode)
  "Return READ-BUFFER-MODE's current match."
  (car (wg-current-matches read-buffer-mode)))

(defun wg-set-current-matches (match-list &optional read-buffer-mode)
  "Set READ-BUFFER-MODE's current matches, and flag a rescan."
  (cl-case (or read-buffer-mode (wg-read-buffer-mode))
    (ido
     (wg-when-boundp (ido-cur-list)
       (setq ido-cur-list match-list ido-rescan t)))
    (iswitchb
     (wg-when-boundp (iswitchb-buflist)
       (setq iswitchb-buflist match-list iswitchb-rescan t)))
    (fallback nil)))

(defun wg-iswitchb-internal (method &optional prompt default init)
  "This provides the buffer switching interface to
`iswitchb-read-buffer' (analogous to ido's `ido-buffer-internal')
that iswitchb *should* have had.  A lot of this code is
duplicated from `iswitchb', so is similarly shitty."
  (let ((iswitchb-method (if (memq method '(insert kill)) 'samewindow method))
        (iswitchb-invalid-regexp nil)
        (buffer (iswitchb-read-buffer (or prompt "iswitch ") default nil init)))
    (cond ((eq iswitchb-exit 'findfile)
           (call-interactively 'find-file))
          (iswitchb-invalid-regexp
           (message "Won't make invalid regexp named buffer"))
          ((not buffer) nil)
          ((not (wg-get-buffer buffer))
           (iswitchb-possible-new-buffer buffer))
          ((eq method 'insert)
           (insert-buffer-substring buffer))
          ((eq method 'kill)
           (kill-buffer buffer))
          (t (iswitchb-visit-buffer buffer)))))

(defun wg-buffer-internal (command &optional prompt default)
  "Buffer list filtration interface to the current remapping of COMMAND.
PROMPT non-nil specifies the prompt.
DEFAULT non-nil specifies the first completion candidate."
  (if (not (wg-filter-buffer-list-p))
      (call-interactively (wg-prior-mapping workgroups-mode command))
    (wg-with-buffer-list-filters command
      (let ((wg-buffer-internal-default-buffer default))
        (cl-ecase (wg-read-buffer-mode)
          (ido
           (ido-buffer-internal
            (wg-aget wg-ido-method-translations command) nil
            (wg-buffer-list-filter-prompt prompt)
            nil wg-previous-minibuffer-contents))
          (iswitchb
           (wg-iswitchb-internal
            (wg-aget wg-iswitchb-method-translations command)
            (wg-buffer-list-filter-prompt prompt)
            nil wg-previous-minibuffer-contents))
          (fallback
           (let (read-buffer-function)
             (call-interactively command))))
        (wg-message (wg-buffer-command-display))))))

(defun wg-get-sneaky-ido-entry-buffer-replacement (&optional regexp)
  "Return a live buffer to replace `ido-entry-buffer'.
This is a workaround for an ido misfeature.  IMHO, ido should
respect the value of `ido-temp-list' after
`ido-make-buffer-list-hook' has been run, since the user's
preference for the final value of `ido-temp-list', if any, has
been expressed in that hook.  But ido conditionally rotates the
first match to the end after the hook has been run, based on the
value of `ido-entry-buffer'.  So as a workaround, set
`ido-entry-buffer' to a buffer that will never be a completion
candidate under normal circumstances.  See
`wg-ido-entry-buffer-replacement-regexp'."
  (wg-get-first-buffer-matching-regexp
   (or regexp wg-ido-entry-buffer-replacement-regexp)))

(defun wg-adjust-buffer-list-default (buflist &optional default)
  "Adjust BUFLIST based on DEFAULT.
DEFAULT is the default completion candidate, and defaults to
`wg-buffer-internal-default-buffer'.  Non-nil, this gets placed
at the beginning of BUFLIST.  Otherwise rotate BUFLIST."
  (wg-aif (or default wg-buffer-internal-default-buffer)
      (wg-move-elt it buflist 0)
    (wg-rotate-list buflist)))

(defun wg-finalize-buffer-list (buflist)
  "Run `wg-buffer-list-finalization-hook' and return
`wg-temp-buffer-list'."
  (let ((wg-temp-buffer-list buflist))
    (run-hooks 'wg-buffer-list-finalization-hook)
    wg-temp-buffer-list))

(defun wg-set-buffer-list-symbol (symbol)
  "Set SYMBOL to the filtered buffer-list."
  (when (and wg-current-buffer-list-filter-id (boundp symbol))
    (set symbol
         (wg-finalize-buffer-list
          (wg-adjust-buffer-list-default
           (wg-filtered-buffer-list t))))))

(defun wg-set-ido-buffer-list ()
  "Set `ido-temp-list' with `wg-set-buffer-list-symbol'.
Added to `ido-make-buffer-list-hook'."
  (wg-set-buffer-list-symbol 'ido-temp-list)
  (wg-when-boundp (ido-entry-buffer)
    (setq ido-entry-buffer (wg-get-sneaky-ido-entry-buffer-replacement))))

(defun wg-set-iswitchb-buffer-list ()
  "Set `iswitchb-temp-buflist' with `wg-set-buffer-list-symbol'.
Added to `iswitchb-make-buflist-hook'."
  (wg-set-buffer-list-symbol 'iswitchb-temp-buflist))

(provide 'workgroups-ido)
;;; workgroups-ido.el ends here

(require 'slime)
(require 'slime-parse)

(define-slime-contrib slime-highlight-edits
  "Highlight edited, i.e. not yet compiled, code."
  (:authors "William Bland <doctorbill.news@gmail.com>")
  (:license "GPL")
  (:on-load   (add-hook 'slime-mode-hook 'slime-activate-highlight-edits))
  (:on-unload (remove-hook 'slime-mode-hook 'slime-activate-highlight-edits)))

(defun slime-activate-highlight-edits ()
 (slime-highlight-edits-mode 1))

(defface slime-highlight-edits-face
    `((((class color) (background light))
       (:background "lightgray"))
      (((class color) (background dark))
       (:background "dimgray"))
      (t (:background "yellow")))
  "Face for displaying edit but not compiled code."
  :group 'slime-mode-faces)

(define-minor-mode slime-highlight-edits-mode 
  "Minor mode to highlight not-yet-compiled code." nil)

(add-hook 'slime-highlight-edits-mode-on-hook
          'slime-highlight-edits-init-buffer)

(add-hook 'slime-highlight-edits-mode-off-hook
          'slime-highlight-edits-reset-buffer)

(defun slime-highlight-edits-init-buffer ()
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 
               'slime-highlight-edits)
  (add-to-list 'slime-before-compile-functions
               'slime-highlight-edits-compile-hook))

(defun slime-highlight-edits-reset-buffer ()
  (setq after-change-functions  
        (remove 'slime-highlight-edits after-change-functions))
  (slime-remove-edits (point-min) (point-max)))

;; FIXME: what's the LEN arg for?
(defun slime-highlight-edits (beg end &optional len) 
  (save-match-data
    (when (and (slime-connected-p)
               (not (slime-inside-comment-p))
               (not (slime-only-whitespace-p beg end)))
      (let ((overlay (make-overlay beg end)))
        (overlay-put overlay 'face 'slime-highlight-edits-face)
        (overlay-put overlay 'slime-edit t)))))

(defun slime-remove-edits (start end)
  "Delete the existing Slime edit hilights in the current buffer."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'slime-edit)
          (delete-overlay o)))
      (goto-char (next-overlay-change (point))))))

(defun slime-highlight-edits-compile-hook (start end)
  (when slime-highlight-edits-mode
    (let ((start (save-excursion (goto-char start) 
				 (skip-chars-backward " \t\n\r")
				 (point)))
	  (end (save-excursion (goto-char end) 
			       (skip-chars-forward " \t\n\r")
			       (point))))
      (slime-remove-edits start end))))

(defun slime-only-whitespace-p (beg end)
  "Contains the region from BEG to END only whitespace?"
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \n\t\r" end)
    (<= end (point))))

(provide 'slime-highlight-edits)

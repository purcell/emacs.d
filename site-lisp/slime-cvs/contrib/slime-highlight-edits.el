;;; slime-higlight-edits --- highlight edited, i.e. not yet compiled, code 
;;
;; Author: William Bland <doctorbill.news@gmail.com> and others
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation: 
;; 
;; Add something like this your .emacs: 
;;
;;   (add-to-list 'load-path "<contrib-dir>")
;;   (autoload 'slime-highlight-edits-mode "slime-highlight-edits")
;;   (add-hook 'slime-mode-hook (lambda () (slime-highlight-edits-mode 1)))

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
               (not (slime-inside-comment-p beg end))
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

(defun slime-inside-comment-p (beg end)
  "Is the region from BEG to END in a comment?"
  (save-excursion
    (goto-char beg)
    (let* ((hs-c-start-regexp ";\\|#|")
           (comment (hs-inside-comment-p)))
      (and comment
           (destructuring-bind (cbeg cend) comment
             (<= end cend))))))

(defun slime-only-whitespace-p (beg end)
  "Contains the region from BEG to END only whitespace?"
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \n\t\r" end)
    (<= end (point))))

(defun slime-highlight-edits-mode-on () (slime-highlight-edits-mode 1))

(defun slime-highlight-edits-init ()
  (add-hook 'slime-mode-hook 'slime-highlight-edits-mode-on))

(defun slime-highlight-edits-unload ()
  (remove-hook 'slime-mode-hook 'slime-highlight-edits-mode-on))

(provide 'slime-highlight-edits)
;;; slime-typeout-frame.el --- display some message in a dedicated frame
;;
;; Author: Luke Gorrie  <luke@synap.se>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-typeout-frame)))
;;


;;;; Typeout frame

;; When a "typeout frame" exists it is used to display certain
;; messages instead of the echo area or pop-up windows.

(defvar slime-typeout-window nil
  "The current typeout window.")

(defvar slime-typeout-frame-properties
  '((height . 10) (minibuffer . nil))
  "The typeout frame properties (passed to `make-frame').")

(defun slime-typeout-active-p ()
  (and slime-typeout-window
       (window-live-p slime-typeout-window)))

(defun slime-typeout-message-aux (format-string &rest format-args)
  (slime-ensure-typeout-frame)
  (with-current-buffer (window-buffer slime-typeout-window)
    (let ((msg (apply #'format format-string format-args)))
      (unless (string= msg "")
	(erase-buffer)
	(insert msg)))))

(defun slime-typeout-message (format-string &rest format-args)
  (apply #'slime-typeout-message-aux format-string format-args)
  ;; Disable the timer for autodoc temporarily, as it would overwrite
  ;; the current typeout message otherwise.
  (when (and (featurep 'slime-autodoc) slime-autodoc-mode)
    (slime-autodoc-stop-timer)
    (add-hook 'pre-command-hook #'slime-autodoc-start-timer)))

(defun slime-make-typeout-frame ()
  "Create a frame for displaying messages (e.g. arglists)."
  (interactive)
  (let ((frame (make-frame slime-typeout-frame-properties)))
    (save-selected-window
      (select-window (frame-selected-window frame))
      (switch-to-buffer "*SLIME-Typeout*")
      (setq slime-typeout-window (selected-window)))))

(defun slime-ensure-typeout-frame ()
  "Create the typeout frame unless it already exists."
  (interactive)
  (unless (slime-typeout-active-p)
    (slime-make-typeout-frame)))

(defun slime-typeout-autodoc-message (doc)
  ;; No need for refreshing per `slime-autodoc-pre-command-refresh-echo-area'.
  (setq slime-autodoc-last-message "")
  (slime-typeout-message-aux "%s" doc))

(defun slime-typeout-autodoc-dimensions ()
  (cond ((slime-typeout-active-p)
	 (list (window-width slime-typeout-window) nil))
	(t
	 (list 75 nil))))


;;; Initialization

(defvar slime-typeout-frame-unbind-stack ())

(defun slime-typeout-frame-init ()
  (add-hook 'slime-connected-hook 'slime-ensure-typeout-frame)
  (loop for (var value) in 
	'((slime-message-function slime-typeout-message)
	  (slime-background-message-function slime-typeout-message)
	  (slime-autodoc-message-function slime-typeout-autodoc-message)
	  (slime-autodoc-dimensions-function slime-typeout-autodoc-dimensions))
	do (slime-typeout-frame-init-var var value)))

(defun slime-typeout-frame-init-var (var value)
  (push (list var (if (boundp var) (symbol-value var) 'slime-unbound))
	slime-typeout-frame-unbind-stack)
  (set var value))

(defun slime-typeout-frame-unload ()
  (remove-hook 'slime-connected-hook 'slime-ensure-typeout-frame)
  (loop for (var value) in slime-typeout-frame-unbind-stack 
	do (cond ((eq var 'slime-unbound) (makunbound var))
		 (t (set var value))))
  (setq slime-typeout-frame-unbind-stack nil))
  
(provide 'slime-typeout-frame)

(require 'slime)
(require 'slime-autodoc)
(require 'cl-lib)

(defvar slime-typeout-frame-unbind-stack ())

(define-slime-contrib slime-typeout-frame
  "Display messages in a dedicated frame."
  (:authors "Luke Gorrie  <luke@synap.se>")
  (:license "GPL")
  (:on-load
   (unless (slime-typeout-tty-only-p)
     (add-hook 'slime-connected-hook 'slime-ensure-typeout-frame)
     (add-hook 'slime-autodoc-mode-hook 'slime-typeout-wrap-autodoc)
     (cl-loop for (var value) in 
              '((slime-message-function slime-typeout-message)
                (slime-background-message-function slime-typeout-message))
              do (slime-typeout-frame-init-var var value))))
  (:on-unload
   (remove-hook 'slime-connected-hook 'slime-ensure-typeout-frame)
   (remove-hook 'slime-autodoc-mode-hook 'slime-typeout-wrap-autodoc)
   (cl-loop for (var value) in slime-typeout-frame-unbind-stack 
            do (cond ((eq var 'slime-unbound) (makunbound var))
                     (t (set var value))))
   (setq slime-typeout-frame-unbind-stack nil)))

(defun slime-typeout-frame-init-var (var value)
  (push (list var (if (boundp var) (symbol-value var) 'slime-unbound))
	slime-typeout-frame-unbind-stack)
  (set var value))

(defun slime-typeout-tty-only-p ()
  (cond ((featurep 'xemacs)
	 (null (remove 'tty (mapcar #'device-type (console-device-list)))))
	(t (not (window-system)))))


;;;; Typeout frame

;; When a "typeout frame" exists it is used to display certain
;; messages instead of the echo area or pop-up windows.

(defvar slime-typeout-window nil
  "The current typeout window.")

(defvar slime-typeout-frame-properties
  '((height . 10) (minibuffer . nil))
  "The typeout frame properties (passed to `make-frame').")

(defun slime-typeout-buffer ()
  (with-current-buffer (get-buffer-create (slime-buffer-name :typeout))
    (setq buffer-read-only t)
    (current-buffer)))

(defun slime-typeout-active-p ()
  (and slime-typeout-window
       (window-live-p slime-typeout-window)))

(defun slime-typeout-message-aux (format-string &rest format-args)
  (slime-ensure-typeout-frame)
  (with-current-buffer (slime-typeout-buffer)
    (let ((inhibit-read-only t)
          (msg (apply #'format format-string format-args)))
      (unless (string= msg "")
	(erase-buffer)
	(insert msg)))))

(defun slime-typeout-message (format-string &rest format-args)
  (apply #'slime-typeout-message-aux format-string format-args))

(defun slime-make-typeout-frame ()
  "Create a frame for displaying messages (e.g. arglists)."
  (interactive)
  (let ((frame (make-frame slime-typeout-frame-properties)))
    (save-selected-window
      (select-window (frame-selected-window frame))
      (switch-to-buffer (slime-typeout-buffer))
      (setq slime-typeout-window (selected-window)))))

(defun slime-ensure-typeout-frame ()
  "Create the typeout frame unless it already exists."
  (interactive)
  (if (slime-typeout-active-p)
      (save-selected-window
        (select-window slime-typeout-window)
        (switch-to-buffer (slime-typeout-buffer)))
    (slime-make-typeout-frame)))

(defun slime-typeout-wrap-autodoc ()
  (setq eldoc-message-function 'slime-typeout-message-aux))

(provide 'slime-typeout-frame)

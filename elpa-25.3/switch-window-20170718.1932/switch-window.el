;;; switch-window.el --- A *visual* way to choose a window to switch to
;;
;; Copyright (C) 2010-2017  Dimitri Fontaine
;;               2016-2017  Feng Shu
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;;         Feng Shu <tumashu@163.com>
;; URL: https://github.com/dimitri/switch-window
;;      http://tapoueh.org/emacs/switch-window.html
;; Git-URL: https://github.com/dimitri/switch-window.git
;; Version: 1.5.0
;; Created: 2010-04-30
;; Keywords: window navigation
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;; Package-Requires: ((cl-lib "0.5"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; * What is switch-window                       :README:
;; switch-window is an emacs window switch tool, which offer a
;; *visual* way to choose a window to switch to, delete, split
;; or other operations.
;;
;; [[./snapshots/switch-window.png]]
;;
;; ** Installation
;;
;; 1. Config melpa source, please read: http://melpa.org/#/getting-started
;; 2. M-x package-install RET switch-window RET
;;
;; Note: User can install switch-window with [[http://github.com/dimitri/el-get][El-Get]] too.
;;
;; ** Configure and Usage
;;
;; #+BEGIN_EXAMPLE
;; (require 'switch-window)
;; (global-set-key (kbd "C-x o") 'switch-window)
;; (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
;; (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
;; (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
;; (global-set-key (kbd "C-x 0") 'switch-window-then-delete)
;; #+END_EXAMPLE
;;
;; When switch-window is enabled, user can use the below five keys:
;;
;; | key | command description   |
;; |-----+-----------------------|
;; | "i" | Move the border up    |
;; | "k" | Move the border down  |
;; | "j" | Move the border left  |
;; | "k" | Move the border right |
;; | "b" | Balance windows       |
;; |"SPC"| Resume auto-resize    |
;;
;; If you want to customize this feature, please see variable:
;; `switch-window-extra-map'.
;;
;; Note: if you use auto-resize window feature, you *must* know
;; that when you execute above window operate commands, auto-resize
;; feature will be disabled temporarily, you should use above "SPC"
;; key to resume.
;;
;; ** Tips
;;
;; *** I want to select a window with "a-z" instead of "1-9".
;; #+BEGIN_EXAMPLE
;; (setq switch-window-shortcut-style 'qwerty)
;; (setq switch-window-qwerty-shortcuts
;;       '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
;; #+END_EXAMPLE
;;
;; *** I want to let window to show bigger label.
;; #+BEGIN_EXAMPLE
;; (setq switch-window-increase 6) ;Increase or decrease this number.
;; #+END_EXAMPLE
;;
;; *** I want to *hide* window label when window's number < 3
;; #+BEGIN_EXAMPLE
;; (setq switch-window-threshold 2)
;; #+END_EXAMPLE
;;
;; *** I want to select minibuffer with label "z".
;; #+BEGIN_EXAMPLE
;; (setq switch-window-minibuffer-shortcut ?z)
;; #+END_EXAMPLE
;;
;; *** I want to auto resize a window when switch to it
;; #+BEGIN_EXAMPLE
;; (setq switch-window-auto-resize-window t)
;; (setq switch-window-default-window-size 0.8) ;80% of frame size
;; (switch-window-mouse-mode) ;auto resize when switch window with mouse
;; #+END_EXAMPLE
;;
;; Advanced usage:
;; #+BEGIN_EXAMPLE
;; (setq switch-window-auto-resize-window
;;       (lambda ()
;;         (equal (buffer-name) "*scratch*"))) ;when return t, run auto switch
;; (setq switch-window-default-window-size '(0.8 . 0.6)) ;80% width and 60% height of frame
;; #+END_EXAMPLE
;;
;; By the way, you can use package [[https://github.com/roman/golden-ratio.el][golden-ratio]] also.
;;
;; *** Switch-window seem to conflict with Exwm, how to do?
;; By default, switch-window get user's input with the help
;; of function `read-event', this approach does not work well
;; with [[https://github.com/ch11ng/exwm][Exwm]] (Emacs X window manager),
;; user should set the below variable and use minibuffer
;; to get input instead:
;;
;; #+BEGIN_EXAMPLE
;; (setq switch-window-input-style 'minibuffer)
;; #+END_EXAMPLE
;;
;; Note: if you use minibuffer to get input, the feature about
;; `switch-window-minibuffer-shortcut' will not work well.
;;
;; *** I use text terminal, but I want *bigger* label.
;; The only choice is using asciiart, which *draw* a bigger label
;; with *small* ascii char.
;;
;; #+BEGIN_EXAMPLE
;; (setq switch-window-shortcut-appearance 'asciiart)
;; #+END_EXAMPLE
;;
;; [[./snapshots/switch-window-3.png]]
;;
;; *** I want to use image or icon as label.
;; 1. Prepare your label images, rename them to:
;;    1.png ... 9.png, a.png ... z.png.
;;
;;    You can use other image types supported by
;;    emacs, please see: `image-types'.
;; 2. Put all above images to directory:
;;    `switch-window-image-directory'.
;; 3. Set variable: `switch-window-shortcut-appearance'
;;    #+BEGIN_EXAMPLE
;;    (setq switch-window-shortcut-appearance 'image)
;;    #+END_EXAMPLE
;;
;; [[./snapshots/switch-window-2.png]]
;;
;; *** `switch-window-shortcut-appearance' can't satisfy my need. how to do?
;; All you should do is hacking you own label buffer function,
;; for example: my-switch-window-label-buffer-function, and set
;; the below variable:
;;
;; #+BEGIN_EXAMPLE
;; (setq switch-window-label-buffer-function
;;       'my-switch-window-label-buffer-function)
;; #+END_EXAMPLE
;;
;; *** Have any other similar package exist?
;; - [[https://github.com/abo-abo/ace-window][ace-window]]
;;
;; ** Changelog
;;
;; *** 1.5.0 - 2017-04-29
;; - Implement commands:
;;   1. switch-window-then-maximize
;;   2. switch-window-then-delete
;;   3. switch-window-then-split-below
;;   4. switch-window-then-split-right
;;   5. switch-window-then-split-horizontally
;;   6. switch-window-then-split-vertically
;;   7. switch-window-then-swap-buffer
;; - Let switch-window work well with Exwm (Emacs X window manager).
;; - User can customize switch-window label's appearance.
;;
;; *** 1.0.0 - 2015-01-14
;; - Please fixme.
;;
;; *** 0.11 - 2013-09-14
;; - restore point to end-of-buffer for windows where it was the case after
;;   switching, fixing an anoying bug.
;;
;; *** 0.10 - 2011-06-19
;; - implement M-x delete-other-window (thanks developernotes on github)
;;
;; *** 0.9 - 2010-11-11 - emacs22 called, it wants some support
;; - implement a propertize based hack to support emacs22
;;
;; *** 0.8 - 2010-09-13 - 999
;; - Suport more than 9 windows (with a single key to type)
;; - Use quail-keyboard-layout to choose single key labels for windows
;;
;; *** 0.7 - 2010-08-23 - window-dedicated-p
;; - temporarily unset the window dedicated flag for displaying the
;;   numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
;; - fix timeout and RET handling wrt to not changing window selection
;;
;; *** 0.6 - 2010-08-12 - *Minibuf-1*
;; - add support for selecting the minibuffer when it's active
;; - some try at a better horizontal centering
;; - assorted cleanup
;;
;; *** 0.5 - 2010-08-08 - Polishing
;; - dim:switch-window-increase is now a maximum value

;;; Code:
;; * Switch-window's code

(require 'cl-lib) ; We use cl-loop and cl-subseq
(require 'quail)
(require 'switch-window-asciiart)
(require 'switch-window-mvborder)

(defgroup switch-window nil
  "switch-window customization group"
  :group 'convenience)

(defcustom switch-window-increase 12
  "How much to increase text size in the window numbering, maximum"
  :type 'integer
  :group 'switch-window)

(defcustom switch-window-timeout 5
  "After this many seconds, cancel the window switching"
  :type 'integer
  :group 'switch-window)

(defcustom switch-window-threshold 2
  "Only active switch-window after this many windows open"
  :type 'integer
  :group 'switch-window)

(defcustom switch-window-relative nil
  "Control the ordering of windows, when true this depends on current-window"
  :type 'boolean
  :group 'switch-window)

(defcustom switch-window-shortcut-style 'quail
  "Use either keyboard layout or alphabet shortcut style"
  :type '(choice (const :tag "Alphabet" 'alphabet)
                 (const :tag "Keyboard Layout" 'quail)
                 (const :tag "Qwerty Homekeys Layout" 'qwerty))
  :group 'switch-window)

(defcustom switch-window-qwerty-shortcuts
  '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o" "g" "h" "r" "q" "u" "v" "n")
  "The list of characters used when switch-window-shortcut-style is 'qwerty'"
  :type 'list
  :group 'switch-window)

(defcustom switch-window-shortcut-appearance 'text
  "Switch-window shortcut's appearance."
  :type '(choice (const :tag "Show shortcut with text" 'text)
                 (const :tag "Show shortcut with Ascii art." 'asciiart)
                 (const :tag "Show shortcut with image." 'image))
  :group 'switch-window)

(defcustom switch-window-image-directory (locate-user-emacs-file "switch-window/image")
  "If `switch-window-shortcut-appearance' set to 'image, image file
will be found in this directory."
  :type 'directory
  :group 'switch-window)

(defcustom switch-window-label-buffer-function
  'switch-window--create-label-buffer
  "The function is used to prepare a temp buffer to diplay
a window's label string, three arguments are required:
1. buffer  Label string will be inserted into this buffer.
2. label   The window's shortcut string.
3. scale   Use to increase or decrease label's size."
  :type 'function
  :group 'switch-window)

(defcustom switch-window-input-style 'default
  "Use `read-event' or `read-from-minibuffer' to get user's input."
  :type '(choice (const :tag "Get input by read-event" 'default)
                 (const :tag "Get input from minibuffer" 'minibuffer))
  :group 'switch-window)

(defcustom switch-window-minibuffer-shortcut nil
  "Whether to customize the minibuffer shortcut.
Default to no customisation (nil), which will make the minibuffer take whatever the last short is.
If a character is specified it will always use that key for the minibuffer shortcut.

Note: this feature only works when the value of `switch-window-input-style' is 'default ."
  :type '(choice (const :tag "Off" nil)
                 (character "m"))
  :group 'switch-window)

(defcustom switch-window-auto-resize-window nil
  "Auto resize window's size when switch to a window.
1. If its value is t, auto resize the selected window.
2. If its value is a function without arguments,
   when the returned value it non-nil, auto resize
   the selected window."
  :group 'switch-window)

(defcustom switch-window-default-window-size 0.7
  "The default auto resize window's size.
1. If its value is nil, disable auto resize feature.
2. If its value is a number (0<x<1), resize selected window to % of frame size.
3. If its value is a number (0<x<1) cons, resize selected window to
   car% of frame width and cdr% of frame height."
  :group 'switch-window)

(defcustom switch-window-finish-hook nil
  "A hook, run when `switch-window--then' is finishd.
Its hook function have no arguments."
  :group 'switch-window
  :type 'hook)

(defvar switch-window-extra-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'switch-window-mvborder-up)
    (define-key map (kbd "k") 'switch-window-mvborder-down)
    (define-key map (kbd "j") 'switch-window-mvborder-left)
    (define-key map (kbd "l") 'switch-window-mvborder-right)
    (define-key map (kbd "b") 'balance-windows)
    (define-key map (kbd "SPC") 'switch-window-resume-auto-resize-window)
    map)
  "Extra keymap for switch-window input.
Note: at the moment, it cannot bind commands, which will
increase or decrease window's number, for example:
`split-window-below' `split-window-right' `maximize'.")

(defcustom switch-window-configuration-change-hook-inhibit nil
  "Whether inhibit `window-configuration-change-hook' during switch-window."
  :type 'boolean
  :group 'switch-window)

(defvar switch-window--temp-disable-auto-resize nil
  "Disable auto resize window feature temporarily.")

;; Fix warn when compile switch-window with emacs-no-x
(defvar image-types)

(defun switch-window--list-keyboard-keys ()
  "Return a list of current keyboard layout keys"
  (cl-loop with layout = (split-string quail-keyboard-layout "")
           for row from 1 to 4
           nconc (cl-loop for col from 1 to 10
                          collect (nth (+ 1 (* 2 col) (* 30 row)) layout))))

(defun switch-window--list-keys ()
  "Return a list of keys to use depending on `switch-window-shortcut-style'"
  (cl-remove-if
   #'(lambda (key)
       (or (and switch-window-minibuffer-shortcut
                (equal key (char-to-string switch-window-minibuffer-shortcut)))
           (lookup-key switch-window-extra-map key)))
   (cond ((eq switch-window-shortcut-style 'qwerty)
          switch-window-qwerty-shortcuts)
         ((eq switch-window-shortcut-style 'alphabet)
          (cl-loop for i from 0 to 25
                   collect (byte-to-string (+ (string-to-char "a") i))))
         (t (switch-window--list-keyboard-keys)))))

(defun switch-window--enumerate ()
  "Return a list of one-letter strings to label current windows"
  (cl-loop for w in (switch-window--list)
           for x in (switch-window--list-keys)
           collect (if (and switch-window-minibuffer-shortcut
                            (minibuffer-window-active-p w))
                       (char-to-string switch-window-minibuffer-shortcut)
                     x)))

(defun switch-window--label (num)
  "Return the label to use for a given window number"
  (nth (- num 1) (switch-window--enumerate)))

(defun switch-window--list (&optional from-current-window)
  "list windows for current frame, starting at top left unless
from-current-window is not nil"
  (if (or from-current-window switch-window-relative)
      (window-list nil nil)
    (window-list nil nil (frame-first-window))))

(defun switch-window--display-number (win num)
  "prepare a temp buffer to diplay in the window while choosing"
  (let* ((label (switch-window--label num))
         (buffer (get-buffer-create
                  (format " *%s: %s*"
                          label (buffer-name (window-buffer win))))))
    (funcall switch-window-label-buffer-function
             buffer label switch-window-increase)
    (set-window-buffer win buffer)
    buffer))

(defun switch-window--create-label-buffer (buffer label scale)
  "The default label buffer create funcion."
  (with-current-buffer buffer
    (cond
     ((eq switch-window-shortcut-appearance 'asciiart)
      (setq line-spacing nil)
      (insert
       (replace-regexp-in-string
        "^\n" ""
        (nth (cl-position
              label
              (remove "" (split-string "123456789abcdefjhijklmnopqrstuvwxyz" ""))
              :test #'equal)
             switch-window-asciiart))))
     ((eq switch-window-shortcut-appearance 'text)
      (if (fboundp 'text-scale-increase)
          (progn (text-scale-increase scale)
                 (insert label))
        (insert (propertize
                 label 'face (list :height (* 1.0 scale))))))
     ((eq switch-window-shortcut-appearance 'image)
      (let ((types (cl-copy-seq image-types))
            file)
        (while types
          (let* ((type (pop types))
                 (file1 (format "%s%s.%S"
                                (file-name-as-directory switch-window-image-directory)
                                label type)))
            (when (file-exists-p file1)
              (setq file file1)
              (setq types nil))))
        (if (and file (display-images-p))
            (insert-image-file (expand-file-name file))
          (if (fboundp 'text-scale-increase)
              (progn (text-scale-increase scale)
                     (insert label))
            (insert label))))))
    (goto-char (point-min))
    buffer))

(defun switch-window--jump-to-window (index)
  "Jump to the window which index is `index'."
  (cl-loop for c from 1
           for win in (switch-window--list)
           until (= c index)
           finally (select-window win)))

(defun switch-window--list-eobp ()
  "Return a list of all the windows where `eobp' is currently
   true so that we can restore that important property (think
   auto scrolling) after switching."
  (cl-loop for win in (switch-window--list)
           when (with-current-buffer (window-buffer win) (eobp))
           collect win))

(defun switch-window--restore-eobp (eobp-window-list)
  "For each window in EOBP-WINDOW-LIST move the point to end of buffer."
  (cl-loop for win in eobp-window-list
           do (let ((buffer (window-buffer win)))
                (when buffer
                  (with-current-buffer buffer
                    (goto-char (point-max)))))))

;;;###autoload
(defun switch-window-then-delete ()
  "Display an overlay in each window showing a unique key, then
ask user which window to delete"
  (interactive)
  (switch-window--then
   "Delete window: "
   #'delete-window
   #'delete-window t))

(defalias 'delete-other-window 'switch-window-then-delete)
(make-obsolete 'delete-other-window 'switch-window-then-delete
               "switch-window version 0.2")

;;;###autoload
(defun switch-window-then-maximize ()
  "Display an overlay in each window showing a unique key, then
ask user which window to maximize"
  (interactive)
  (switch-window--then
   "Maximize window: "
   #'delete-other-windows
   #'delete-other-windows t))

;;;###autoload
(defun switch-window ()
  "Display an overlay in each window showing a unique key, then
ask user for the window where move to"
  (interactive)
  (switch-window--then
   "Move to window: "
   #'(lambda () (other-window 1))))

;;;###autoload
(defun switch-window-then-split-horizontally (arg)
  "Select a window then split it horizontally."
  (interactive "P")
  (switch-window--then
   "Horiz-split window: "
   #'split-window-horizontally
   #'split-window-horizontally arg 1))

;;;###autoload
(defun switch-window-then-split-vertically (arg)
  "Select a window then split it vertically."
  (interactive "P")
  (switch-window--then
   "Verti-split window: "
   #'split-window-vertically
   #'split-window-vertically arg 1))

;;;###autoload
(defun switch-window-then-split-below (arg)
  "Select a window then split it with split-window-below's mode."
  (interactive "P")
  (switch-window--then
   "Below-split window: "
   #'split-window-below
   #'split-window-below arg 1))

;;;###autoload
(defun switch-window-then-split-right (arg)
  "Select a window then split it with split-window-right's mode."
  (interactive "P")
  (switch-window--then
   "Right-split window: "
   #'split-window-right
   #'split-window-right arg 1))

;;;###autoload
(defun switch-window-then-swap-buffer (arg)
  "Select a window then swap it buffer with current window's buffer."
  (interactive "P")
  (let ((buffer1 (window-buffer))
        (window1 (get-buffer-window))
        buffer2 window2)
    (switch-window)
    (setq buffer2 (current-buffer))
    (setq window2 (get-buffer-window))
    (set-window-buffer window2 buffer1)
    (set-window-buffer window1 buffer2)
    (if arg
        (select-window window1)
      (select-window window2))))

(defun switch-window--then (prompt function1 &optional function2
                                   return-original-window threshold)
  "If the number of opened window is less than `threshold', call `function1'
in current window, otherwise, switch to the window assocated with the typed key,
then call `function2'.

1. `function1' and `function2' are functions with no arguments.
2. When `return-original-window' is t, switch to original window
   after `function2' is called.
3. When `threshold' is not a number, use the value of
   `switch-window-threshold' instead."
  (if (<= (length (window-list))
          (if (numberp threshold)
              threshold
            switch-window-threshold))
      (when (functionp function1)
        (funcall function1))
    (let ((orig-window (selected-window))
          (index (switch-window--prompt prompt))
          (eobps (switch-window--list-eobp)))
      (switch-window--jump-to-window index)
      (when (functionp function2)
        (funcall function2))
      (when (and return-original-window
                 (window-live-p orig-window))
        (select-window orig-window))
      (switch-window--restore-eobp eobps)))
  (switch-window--auto-resize-window)
  (run-hooks 'switch-window-finish-hook))

(defun switch-window--get-input (prompt-message minibuffer-num eobps)
  "Get user's input with the help of `read-event'."
  (let (key)
    (while (not key)
      (let ((input (event-basic-type
                    (read-event
                     (if minibuffer-num
                         (format "Move to window [minibuffer is %s]: "
                                 (if switch-window-minibuffer-shortcut
                                     (char-to-string switch-window-minibuffer-shortcut)
                                   (switch-window--label minibuffer-num)))
                       prompt-message)
                     nil switch-window-timeout))))
        (if (or (null input) (eq input 'return))
            (progn
              (switch-window--restore-eobp eobps)
              (keyboard-quit))	; timeout or RET
          (unless (symbolp input)
            (let* ((wchars (mapcar 'string-to-char
                                   (switch-window--enumerate)))
                   (pos (cl-position input wchars))
                   (extra-function
                    (lookup-key switch-window-extra-map
                                (char-to-string input))))
              (cond
               (extra-function
                (call-interactively extra-function)
                ;; Commands in `switch-window-extra-map' mainly are window-resize commands.
                ;; If we use these commands, theirs effects should not be override by
                ;; auto-resize feature.
                (unless (eq extra-function 'switch-window-resume-auto-resize-window)
                  (setq switch-window--temp-disable-auto-resize t)))
               (pos (setq key (1+ pos)))
               (t (switch-window--restore-eobp eobps)
                  (keyboard-quit))))))))
    key))

(defun switch-window--get-minibuffer-input (prompt-message minibuffer-num eobps)
  "Get user's input with the help of `read-from-minibuffer'."
  (let (key)
    (while (not key)
      (let ((input (read-from-minibuffer
                    (if minibuffer-num
                        (format "Move to window [minibuffer is %s]: "
                                (if switch-window-minibuffer-shortcut
                                    (char-to-string switch-window-minibuffer-shortcut)
                                  (switch-window--label minibuffer-num)))
                      prompt-message)
                    nil
                    (let ((map (copy-keymap minibuffer-local-map))
                          (i ?\ ))
                      (while (< i 127)
                        (define-key map (char-to-string i)
                          (lambda ()
                            (interactive)
                            (self-insert-command 1)
                            (exit-minibuffer)))
                        (setq i (1+ i)))
                      map))))
        (if (< (length input) 1)
            (switch-window--restore-eobp eobps)
          (let ((pos (cl-position input (switch-window--enumerate)
                                  :test #'equal))
                (extra-function
                 (lookup-key switch-window-extra-map input)))
            (cond
             (extra-function
              (call-interactively extra-function)
              ;; Commands in `switch-window-extra-map' mainly are window resize commands.
              ;; If we use these commands, theirs effects should not be override by
              ;; auto-resize feature.
              (unless (eq extra-function 'switch-window-resume-auto-resize-window)
                (setq switch-window--temp-disable-auto-resize t)))
             (pos (setq key (1+ pos)))
             (t (switch-window--restore-eobp eobps)))))))
    key))

(defun switch-window--prompt (prompt-message)
  "Display an overlay in each window showing a unique key, then
ask user for the window to select"
  (let ((window-configuration-change-hook
         (unless switch-window-configuration-change-hook-inhibit
           window-configuration-change-hook))
        (original-cursor (default-value 'cursor-type))
        (eobps (switch-window--list-eobp))
        (minibuffer-num nil)
        (num 1)
        key label-buffers
        window-buffers window-points dedicated-windows)

    ;; arrange so that C-g will get back to previous window configuration
    (unwind-protect
        (progn
          ;; hide cursor during window selection process
          (setq-default cursor-type nil)
          ;; save window's buffer, point and dedicate state.
          ;; then display label buffers in all window.
          (dolist (win (switch-window--list))
            (push (cons win (window-buffer win)) window-buffers)
            (push (cons win (window-point win)) window-points)
            (when (window-dedicated-p win)
              (push (cons win (window-dedicated-p win)) dedicated-windows)
              (set-window-dedicated-p win nil))
            (if (minibuffer-window-active-p win)
                (setq minibuffer-num num)
              (push (switch-window--display-number win num) label-buffers))
            (setq num (1+ num)))
          ;; get user's input
          (cond ((eq switch-window-input-style 'default)
                 (setq key (switch-window--get-input
                            prompt-message minibuffer-num eobps)))
                ((eq switch-window-input-style 'minibuffer)
                 (setq key (switch-window--get-minibuffer-input
                            prompt-message minibuffer-num eobps)))))
      ;; clean input-method-previous-message
      (setq input-method-previous-message nil)
      ;; restore original cursor
      (setq-default cursor-type original-cursor)
      ;; remove all label-buffers, useless now.
      (mapc 'kill-buffer label-buffers)
      ;; Restore window's buffer, point and dedicate state.
      (dolist (w window-buffers)
        (set-window-buffer (car w) (cdr w)))
      (dolist (w window-points)
        (set-window-point (car w) (cdr w)))
      (dolist (w dedicated-windows)
        (set-window-dedicated-p (car w) (cdr w))))
    key))

(define-minor-mode switch-window-mouse-mode
  "Enable auto resize window when switch window with mouse."
  :global t
  (if switch-window-mouse-mode
      (add-hook 'mouse-leave-buffer-hook
                #'switch-window--mouse-auto-resize-window)
    (remove-hook 'mouse-leave-buffer-hook
                 #'switch-window--mouse-auto-resize-window)))

(defun switch-window--mouse-auto-resize-window ()
  "Auto resize window when switch window with mouse."
  (run-at-time 0.1 nil #'switch-window--auto-resize-window))

(defun switch-window-resume-auto-resize-window ()
  "Resume auto resize window feature, which is temporarily
disabled by commands in `switch-window-extra-map'."
  (interactive)
  (setq switch-window--temp-disable-auto-resize nil))

(defun switch-window--auto-resize-window ()
  "Auto resize window's size when switch to window."
  (when (and (not switch-window--temp-disable-auto-resize)
             (if (functionp switch-window-auto-resize-window)
                 (funcall switch-window-auto-resize-window)
               switch-window-auto-resize-window)
             (not (window-minibuffer-p))
             (not (one-window-p)))
    (let ((arg switch-window-default-window-size)
          n1 n2)
      (cond ((and (numberp arg) (< 0 arg 1))
             (setq n1 arg)
             (setq n2 arg))
            ((and (consp arg)
                  (numberp (car arg))
                  (numberp (cdr arg))
                  (< 0 (car arg) 1)
                  (< 0 (cdr arg) 1))
             (setq n1 (car arg))
             (setq n2 (cdr arg)))
            (t (setq n1 nil)
               (setq n2 nil)
               (message "The value of `switch-window-default-window-size' is invalid.")))
      (when (and n1 n2)
        (with-selected-window (selected-window)
          (let ((ncol (floor (- (* (frame-width) n1)
                                (window-width))))
                (nrow (floor (- (* (frame-height) n2)
                                (window-height)))))
            (when (window-resizable-p (selected-window) nrow)
              (enlarge-window nrow))
            (when (window-resizable-p (selected-window) ncol t)
              (enlarge-window ncol t)))))))
  (when (and switch-window-auto-resize-window
             switch-window--temp-disable-auto-resize)
    (message
     (substitute-command-keys
      "Switch-window: resume auto-resize with `\\[switch-window-resume-auto-resize-window]'"))
    (message "")))


(provide 'switch-window)
;;; switch-window.el ends here

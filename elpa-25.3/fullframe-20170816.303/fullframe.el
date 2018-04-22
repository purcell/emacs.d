;;; fullframe.el --- Generalized automatic execution in a single frame

;; Copyright (C) 2013 Tom Regner

;; Author: Tom Regner <tom@goochesa.de>
;; Maintainer: Tom Regner <tom@goochesa.de>
;; Version: 0.5.0
;; Package-Version: 20170816.303
;; Keywords: fullscreen
;; Package-Requires: ((cl-lib "0.5"))

;;  This file is NOT part of GNU Emacs

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Generalized automatic execution in a single frame
;;
;; This is a library that package developers can use to provide user
;; friendly single window per frame execution of buffer exposing
;; commands, as well as to use in personal Emacs configurations to attain
;; the same goal for packages that don't use =fullframe= or the likes of
;; it themself.
;;
;;  Example: Setup =magit-status= to open in one window in the current
;;  frame when called
;; Example:
;; - Open magit-status in a single window in fullscreen
;;   (require 'fullframe)
;;   (fullframe magit-status magit-mode-quit-window nil)
;;
;;; Code:


(require 'cl-lib)

;; customization
;; 

(defcustom fullframe/advice-generic-quit-commands nil
  "If set to a non-nil value, on each call to fullframe ALL
functions in `fullframe/generic-exit-frame-commands' will be
adviced to restore the previous window configuration, not only
the one given as `exit-cmd' to the fullframe-call.

The default value is `nil'."
  :type '(boolean)
  :group 'fullframe)

(defcustom fullframe/generic-quit-commands
  '(kill-this-buffer kill-current-buffer kill-buffer-and-window kill-other-buffer-and-window bury-buffer)
  "List of functions that will adviced in addition to `command-off', iff `fullframe/advice-generic-quit-commands' is not `nil'"
  :type '(set :value-type function)
  :group 'fullframe)

;; variables
(defvar fullframe/previous-window-configuration nil
  "The window configuration to restore.")
(make-variable-buffer-local 'fullframe/previous-window-configuration)

;; internal functions

(defmacro fullframe/with-gensym (names &rest body)
  "Make macros relying on multiple `cl-gensym' calls more readable.
Takes a list of symbols NAMES and defines `cl-gensym' variables
  in a `let' that has BODY as body.  The symbol names generated
  are prefixed with \"fullframe/--\", the variable names are as
  given in NAMES.

Example:

\(fullframe/with-gensym (one two three)
  (progn
    `(let ((,one \"one\")
          (,two \"two\")
          (,three \"three\"))
    (message \"%s:%s:%s\\n\" ,one ,two ,three))\)

Instead of

\(let ((one (cl-gensym \"sym-one\"))
       (two (cl-gensym \"sym-two\"))
       (three (cl-gensym \"sym-three\")))
  `(let ((,one \"one\")
        (,two \"two\")
        (,three \"three\"))
    (message \"%s:%s:%s\\n\" ,one ,two ,three)))

Idea attributed to Peter Seibel where I found it."
  (declare (indent defun))
  `(let
       ,(cl-loop for n in names collect
                 `(,n (cl-gensym (concat "fullframe/--"
                                         (symbol-name (quote ,n))))))
     ,@body))

(defun fullframe/maybe-restore-configuration (config)
  "Restore CONFIG if non-nil."
  (when config
    (condition-case nil
        (set-window-configuration config)
      (error (message "Failed to restore all windows.")))))

;; API
;;;###autoload
(defun fullframe/current-buffer-window-config ()
  "Return the window-configuration stored for the current buffer."
  fullframe/previous-window-configuration)

;;;###autoload
(defun fullframe/erase-current-buffer-window-config ()
  "Forget the window config associated with the current buffer."
  (setq fullframe/previous-window-configuration nil))

;;;###autoload
(defun fullframe/set-current-buffer-window-config (wconf)
  "Associate the current buffer with the window-configuration WCONF."
  (setq fullframe/previous-window-configuration wconf))

;;;###autoload
(defmacro fullframe/split-screen (command-on command-off second-buffer &optional direction  switch-to-second-buffer size)
  "After COMMAND-ON is executed and only one window present in
  the current frame, split the frame in two windows ('below or
  'right, depending on DIRECTION being `horizontal' or
  `vertical') and switch the new window to the buffer
  SECOND-BUFFER (name or symbol). If SWITCH-TO-SECOND-BUFFER is
  not `nil', the window holding SECOND-BUFFER will be activated.
"
  `(fullframe ,command-on
              ,command-off
              nil
              (lambda ()
                (let ((wconf (fullframe/current-buffer-window-config))
                      (new-window (if (eq 'horizontal ,direction)(split-window-right)(split-window-below))))
                  (set-window-buffer new-window ,second-buffer)
                  (fullframe/erase-current-buffer-window-config)
                  (if ,switch-to-second-buffer (select-window new-window))
                  (with-current-buffer ,second-buffer
                    (fullframe/set-current-buffer-window-config wconf))))))

;;;###autoload
(defmacro fullframe (command-on command-off &optional kill-on-coff after-command-on-func)
  "Save window/frame state when executing COMMAND-ON.

Advises COMMAND-ON so that the buffer it displays will appear in
a full-frame window.  The previous window configuration will be
restored when COMMAND-OFF is executed in that buffer.  If
KILL-ON-COFF is non-nil, then the buffer will also be killed
after COMMAND-OFF has completed.

This function uses `defadvice' on versions of emacs < 24.4,
`advice-add' otherwise.

AFTER-COMMAND-ON-FUNC is called after COMMAND-ON was called and
the window it generated is the only one in in the frame.
"
  (when (keywordp kill-on-coff)
    (error "The register parameter for fullframe has been removed"))
  (fullframe/with-gensym (window-config window-config-post buf)
    (let ((on-code `(let ((,window-config-post (current-window-configuration)))
                      (delete-other-windows)
                      (unless (equal ,window-config-post (current-window-configuration))
                        (setq fullframe/previous-window-configuration ,window-config))
                      ,@(when after-command-on-func
                          (list `(funcall #',after-command-on-func)))))
          (off-code `(progn
                       (fullframe/maybe-restore-configuration ,window-config)
                       ,@(when kill-on-coff (list `(kill-buffer ,buf)))))
          (exit-cmds `(append (if fullframe/advice-generic-quit-commands fullframe/generic-quit-commands nil)
                              (if (and (not (functionp ',command-off)) (listp ',command-off)) ',command-off (list ',command-off)))))
      (if (version< emacs-version "24.4")
          `(progn
             (require 'fullframe)
             (defadvice ,command-on (around fullframe activate)
               (let ((,window-config (current-window-configuration)))
                 ad-do-it
                 ,on-code))
             (dolist (coff ,exit-cmds)
               (defadvice coff (around fullframe activate)
                 (let ((,window-config fullframe/previous-window-configuration)
                       (,buf (current-buffer)))
                   (prog1
                       ad-do-it
                     ,off-code)))))
        `(progn
           (require 'fullframe)
           (advice-add #',command-on :around
                       #'(lambda (orig-fun &rest args)
                           (let ((,window-config (current-window-configuration)))
                             (apply orig-fun args)
                             ,on-code))
                       '((name . "fullframe-command-on-advice")))
           (dolist (coff ,exit-cmds)
             (progn
               (advice-add coff :around
                           #'(lambda (orig-fun &rest args)
                               (let ((,window-config fullframe/previous-window-configuration)
                                     (,buf (current-buffer)))
                                 (prog1
                                     (apply orig-fun args)
                                   ,off-code)))
                           '((name . "fullframe-command-off-advice"))))))))))

;; interactive functions
;; - none

(provide 'fullframe)
;;; fullframe.el ends here

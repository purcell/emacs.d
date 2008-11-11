;;; js-comint.el --- Run javascript in an inferior process window.

;;; Copyright (C) 2008 Paul Huff
     
;;; Author: Paul Huff <paul.huff@gmail.com>
;;; Maintainer: Paul Huff <paul.huff@gmail.com>
;;; Created: 26 May 2008
;;; Version: 0.0.1
;;; Package-Requires: ()
;;; Keywords: javascript, inferior-mode, convenience


;; js-comint.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; {at your option} any later version.

;; js-comint.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:
;;; js-comint.el let's you run an inferior javascript process in emacs, 
;;; and defines a few functions for sending javascript input to it quickly.

;;  Usage: 
;;  Put js-comint.el in your load path
;;  Add (require 'js-comint) to your .emacs
;;  Set inferior-js-program-command to the execution command for running your javascript REPL
;;  (setq inferior-js-program-command "/path/to/executable <args>")
;;  Do: M-x run-js
;;  Away you go.

;;  I've added the following couple of lines to my .emacs to take advantage of 
;;  cool keybindings for sending things to the javascript interpreter inside
;;  of Steve Yegge's most excellent js2-mode.

;; (add-hook 'js2-mode-hook '(lambda () 
;;			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;;			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;;			    (local-set-key "\C-cb" 'js-send-buffer)
;;			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;;			    (local-set-key "\C-cl" 'js-load-file-and-go)
;;			    ))

;;  This is version 0.0.1, so I've only tested it on my own version of emacs which is currently:
;;  GNU Emacs 22.0.90.1 (i386-apple-darwin8.8.1, Carbon Version 1.6.0) of 2006-10-28
;;  Not sure if it'll work anywhere else, but it doesn't require anything apple-ish, just emacs-ish.

;; Additionally, I've only tested this with rhino.  I'm sure it'll probably work with spidermonkey, 
;; though if it barfs let me know, and I'll update it.

;; I'm a newbie elisper, so please let me know if I'm a. doing things the wrong way, b.
;; making things work they way they shouldn't in the elisp world.

;;; History:
;;

;;; Code:

(require 'comint)

(provide 'js-comint)

(defcustom inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main" "Path to the javascript interpreter")

(defgroup inferior-js nil
  "Run a javascript process in a buffer."
  :group 'inferior-js)

(defcustom inferior-js-mode-hook nil
  "*Hook for customizing inferior-js mode."
  :type 'hook
  :group 'inferior-js)

;;;###autoload
(defun run-js (cmd &optional dont-switch-p)
  "Run an inferior Javascript process, input and output via buffer `*js*'.
If there is a process already running in `*js*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-js-program-command').
Runs the hook `inferior-js-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run js: " inferior-js-program-command)
			 inferior-js-program-command)))
  (if (not (comint-check-proc "*js*"))
      (save-excursion (let ((cmdlist (split-string cmd)))
	(set-buffer (apply 'make-comint "js" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-js-mode))))
  (setq inferior-js-program-command cmd)
  (setq inferior-js-buffer "*js*")
  (if (not dont-switch-p)
      (pop-to-buffer "*js*")))

;;;###autoload
(defun js-send-region (start end)
  "Send the current region to the inferior Javascript process."
  (interactive "r")
  (run-js inferior-js-program-command t)
  (comint-send-region inferior-js-buffer start end)
  (comint-send-string inferior-js-buffer "\n"))

;;;###autoload
(defun js-send-region-and-go (start end)
  "Send the current region to the inferior Javascript process."
  (interactive "r")
  (run-js inferior-js-program-command t)
  (comint-send-region inferior-js-buffer start end)
  (comint-send-string inferior-js-buffer "\n")
  (switch-to-js inferior-js-buffer))

;;;###autoload
(defun js-send-last-sexp-and-go ()
  "Send the previous sexp to the inferior Js process."
  (interactive)
  (js-send-region-and-go (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun js-send-last-sexp ()
  "Send the previous sexp to the inferior Javascript process."
  (interactive)
  (js-send-region (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun js-send-buffer ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (js-send-region (point-min) (point-max)))


;;;###autoload
(defun js-send-buffer-and-go ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (js-send-region-and-go (point-min) (point-max)))

;;;###autoload
(defun js-load-file (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (run-js inferior-js-program-command t)
    (comint-send-string inferior-js-buffer (concat "load(\"" filename "\")\n"))))

;;;###autoload
(defun js-load-file-and-go (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (run-js inferior-js-program-command t)
    (comint-send-string inferior-js-buffer (concat "load(\"" filename "\")\n"))
    (switch-to-js inferior-js-buffer)))

;;;###autoload
(defun switch-to-js (eob-p)
  "Switch to the javascript process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (or (and inferior-js-buffer (get-buffer inferior-js-buffer))
          (js-interactively-start-process))
      (pop-to-buffer inferior-js-buffer)
    (error "No current process buffer.  See variable `inferior-js-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defvar inferior-js-buffer)

(defvar inferior-js-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-x\C-e" 'js-send-last-sexp)
    (define-key m "\C-cl" 'js-load-file)
    m))

;;;###autoload
(define-derived-mode inferior-js-mode comint-mode "Inferior Javascript"
  "Major mode for interacting with an inferior javascript process.

The following commands are available:
\\{inferior-js-mode-map}

A javascript process can be fired up with M-x run-js.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-js-mode-hook (in that order).

You can send text to the inferior Javascript process from othber buffers containing
Javascript source.
    switch-to-js switches the current buffer to the Javascript process buffer.
    js-send-region sends the current region to the Javascript process.


"
(use-local-map inferior-js-mode-map)
)
;;; disable-mouse.el --- Disable mouse commands globally

;; Copyright (C) 2016  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/disable-mouse
;; Package-Version: 20170929.1353
;; Package-X-Original-Version: 0
;; Keywords: mouse

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `disable-mouse-mode' and `global-disable-mouse-mode', pair
;; of minor modes which suppress all mouse events by intercepting them
;; and running a customisable handler command (`ignore' by default).

;;; Code:

(defgroup disable-mouse nil
  "Disable mouse commands globally."
  :prefix "disable-mouse-"
  :group 'mouse)

(defcustom disable-mouse-command 'ignore
  "The command to run when a mouse action is attempted."
  :group 'disable-mouse
  :type 'function)

(defcustom disable-mouse-mode-lighter " NoMouse"
  "Mode-line lighter for `disable-mouse-mode'."
  :group 'disable-mouse
  :type 'string)

(defcustom global-disable-mouse-mode-lighter " NoMouse!"
  "Mode-line lighter for `global-disable-mouse-mode'."
  :group 'disable-mouse
  :type 'string)

(defconst disable-mouse--bindings-modifier-combos
  '("C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "M-C-S-"))

(defconst disable-mouse--bindings-targets '("mode-line" "bottom-divider" "vertical-line"))

(defconst disable-mouse--multipliers '("double" "triple"))

(defconst disable-mouse--bindings
  '("mouse-1" "mouse-2" "mouse-3"
    "up-mouse-1" "up-mouse-2" "up-mouse-3"
    "down-mouse-1" "down-mouse-2" "down-mouse-3"
    "drag-mouse-1" "drag-mouse-2" "drag-mouse-3"
    "mouse-4" "mouse-5"
    "up-mouse-4" "up-mouse-5"
    "down-mouse-4" "down-mouse-5"
    "drag-mouse-4" "drag-mouse-5"
    "wheel-up" "wheel-down" "wheel-left" "wheel-right"
    ))

(defun disable-mouse--all-bindings (include-targets)
  "Return an extensive list of mouse-related keybindings.
When INCLUDE-TARGETS is non-nil, also return bindings that target
the elements in `disable-mouse--bindings-targets'."
  (let ((bindings))
    (dolist (target (append '(nil)
                            (when include-targets
                              disable-mouse--bindings-targets)))
      (dolist (mod (append '(nil) disable-mouse--bindings-modifier-combos))
        (dolist (mult (append '(nil) disable-mouse--multipliers))
          (dolist (binding disable-mouse--bindings)
            (push (read-kbd-macro
                   (concat (when target (concat "<" target "> "))
                           mod
                           "<"
                           (when mult (concat mult "-"))
                           binding
                           ">"))
                  bindings)))))
    bindings))

(defun disable-mouse--handle ()
  "Handle when a disabled mouse event is fired."
  (interactive)
  (call-interactively disable-mouse-command))

(defvar disable-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding (disable-mouse--all-bindings nil))
      (define-key map binding 'disable-mouse--handle))
    map)
  "Map containing no-op bindings for all mouse events.")

(defvar global-disable-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding (disable-mouse--all-bindings t))
      (define-key map binding 'disable-mouse--handle))
    map)
  "Map containing no-op bindings for all mouse events.")

;;;###autoload
(define-minor-mode disable-mouse-mode
  "Disable the mouse in the current buffer.
You can still use the mouse to click into other buffers or
interact with GUI elements such as divider lines."
  nil
  :lighter disable-mouse-mode-lighter)

;;;###autoload
(define-minor-mode global-disable-mouse-mode
  "Disable the mouse globally.
Interact with GUI elements such as divider lines will also be prevented."
  nil
  :lighter global-disable-mouse-mode-lighter
  :global t)

(provide 'disable-mouse)
;;; disable-mouse.el ends here

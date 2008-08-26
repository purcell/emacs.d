;;; rdebug-source.el --- Ruby debugger user interface.

;; Copyright (C) 2006, 2007, 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2007, 2008 Anders Lindgren

;; $Id: rdebug-source.el 821 2008-04-25 02:54:44Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the manual and the file `rdebug.el' for more information.

;; This file is loaded when a Ruby source buffer is loaded. It
;; contains, among else, the Debugger menu.

;;; Code:

;; -------------------------------------------------------------------
;; Consistency checks.
;;

(if (< emacs-major-version 22)
    (error
     "Rdebug needs at least Emacs 22 or greater - you have version %d."
     emacs-major-version))


;; -------------------------------------------------------------------
;; Dependencies.
;;

(require 'cl)

(require 'gud)                          ; For gud-key-prefix, sigh...
(require 'rdebug)
(require 'rdebug-vars)


;; -------------------------------------------------------------------
;; Key bindings
;;

(defun rdebug-populate-common-keys-standard (map)
  "Bind the basic debugger key layout used by many debuggers.

\\{rdebug-example-map-standard}"
  (define-key map [f5]    'gud-cont)
  (define-key map [S-f5]  'rdebug-quit)
  (define-key map [f9]    'rdebug-toggle-source-breakpoint)
  (define-key map [C-f9]  'rdebug-toggle-source-breakpoint-enabled)
  (define-key map [f10]   'rdebug-next)
  (define-key map [f11]   'rdebug-step)
  (define-key map [S-f11] 'gud-finish))


;; TODO: Verify and complement.
(defun rdebug-populate-common-keys-eclipse (map)
  "Bind the basic debugger key layout used by Eclipse.

\\{rdebug-example-map-eclipse}"
  ;;(define-key map []  'gud-cont)
  ;;(define-key map []  'rdebug-quit)
  (define-key map [S-C-b] 'rdebug-toggle-source-breakpoint)
  (define-key map [f6]    'rdebug-next)
  (define-key map [f5]    'rdebug-step)
  (define-key map [f7]    'gud-finish))


;; TODO: Verify and complement.
(defun rdebug-populate-common-keys-netbeans (map)
  "Bind the basic debugger key layout used by NetBeans.

\\{rdebug-example-map-netbeans}"
  ;;(define-key map []  'gud-cont)
  ;;(define-key map []  'rdebug-quit)
  ;; F4 - Run to cursor.
  (define-key map [S-f8]   'rdebug-toggle-source-breakpoint)
  (define-key map [f8]     'rdebug-next)
  (define-key map [f7]     'rdebug-step)
  (define-key map [M-S-f7] 'gud-finish))


;; Note: This is only used in doc-strings.
(defvar rdebug-example-map-standard
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys-standard map)
    map)
  "Rdebug Standard common keymap used only in doc-string.")


(defvar rdebug-example-map-eclipse
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys-eclipse map)
    map)
  "Rdebug Eclipse compatibility common keymap used only in doc-string.")


(defvar rdebug-example-map-netbeans
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys-netbeans map)
    map)
  "Rdebug NetBeans compatibility common keymap used only in doc-string.")


(defun rdebug-populate-common-keys (map)
  "Define the keys that are used by all debugger windows, even by the source.

The variable `rdebug-populate-common-keys-function' controls the layout."
  (define-key map "\C-x\C-a\C-q" 'rdebug-short-key-mode)
  (if rdebug-populate-common-keys-function
      (funcall rdebug-populate-common-keys-function map)))


(defun rdebug-populate-digit-keys (map)
  (define-key map "0" 'rdebug-goto-entry-n)
  (define-key map "1" 'rdebug-goto-entry-n)
  (define-key map "2" 'rdebug-goto-entry-n)
  (define-key map "3" 'rdebug-goto-entry-n)
  (define-key map "4" 'rdebug-goto-entry-n)
  (define-key map "5" 'rdebug-goto-entry-n)
  (define-key map "6" 'rdebug-goto-entry-n)
  (define-key map "7" 'rdebug-goto-entry-n)
  (define-key map "8" 'rdebug-goto-entry-n)
  (define-key map "9" 'rdebug-goto-entry-n))


;; -------------------------------------------------------------------
;; Menu support.
;;


;; Note: We want the key binding to show in the menu. However, our
;; situation is a little bit complex:
;;
;; 1) We want the binding of the `common' man (i.e. the function key
;;    the user has selected.)
;;
;; 2) We want this even when the menu is disabled and the key isn't
;;    bound, typically when the debugger isn't running.
;;
;; This has been solved by setting up an explicit ":keys" properly.
(defun rdebug-menu-item (common-map name cmd &rest args)
  "Return a menu item entry with the correct key bindings.

A command can be bound to a number of different key sequences. If
the rdebug common map contains a binding it is displayed in the
menu. (The common map typically contains function key bindings.)"
  (let ((key-binding (where-is-internal cmd (list common-map) t))
        (hint '()))
    (if key-binding
        (setq hint (list :keys (key-description key-binding))))
    (append (list 'menu-item name cmd)
            hint
            args)))


;; Note, we re-populate the menus of the different minor and major
;; modes. The reason is that Emacs caches the key bindings, which
;; means that wrong ones are shown when buffers are changed.

;; Remember, all menu items are added in the reverse order!

(defun rdebug-populate-debugger-menu (map)
  "Populate the Rdebug 'Debugger' menu."
  (let ((menu (make-sparse-keymap))
        (common-map (make-sparse-keymap)))
    ;; Use a simple common map to find the best key sequence to
    ;; display in menu.
    (rdebug-populate-common-keys common-map)

    (define-key map [menu-bar debugger] (cons "Debugger" menu))

    (define-key menu [break-delete]
      (rdebug-menu-item common-map "Enable/disable breakpoint"
                        'rdebug-toggle-source-breakpoint-enabled
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [break]
      (rdebug-menu-item common-map "Toggle breakpoint"
                        'rdebug-toggle-source-breakpoint
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [finish]
      (rdebug-menu-item common-map "Step out" 'gud-finish
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [step]
      (rdebug-menu-item common-map "Step into" 'rdebug-step
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [next]
      (rdebug-menu-item common-map "Step over" 'rdebug-next
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [cont]
      (rdebug-menu-item common-map "Continue" 'rdebug-continue
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key map [menu-bar debugger line1] '(menu-item "--"))

    (define-key menu [stop]
      (rdebug-menu-item
       common-map "Stop the debugger" 'rdebug-quit
       :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [start]
      (rdebug-menu-item common-map "Start the debugger" 'rdebug))

    (define-key map [menu-bar debugger line2] '(menu-item "--"))

    ;; --------------------
    ;; The "Options" submenu.

    (let ((submenu (make-sparse-keymap)))
      (define-key menu [options] (cons "Options" submenu)))

    (define-key map [menu-bar debugger options customize]
      (rdebug-menu-item common-map
                        "Customize Rdebug" 'rdebug-customize))

    (define-key map [menu-bar debugger options line1] '(menu-item "--"))



    ;; ----------------
    ;; The "short key" toggle.

    (define-key map [menu-bar debugger options short-key-mode]
      (rdebug-menu-item common-map
                        "Short keys in source" 'rdebug-short-key-mode
                        :button
                        '(:toggle
                          . rdebug-short-key-mode)))

    (define-key map [menu-bar debugger options line2] '(menu-item "--"))

    ;; ----------------
    ;; Separate I/O buffer.

    (define-key map [menu-bar debugger options io-buffer]
      (rdebug-menu-item common-map
                        "Separate I/O buffer"
                        'rdebug-toggle-use-separate-io-buffer
                        :button
                        '(:toggle
                          . rdebug-use-separate-io-buffer)))

    ;; --------------------
    ;; The optional secondary windows submenu.


    ;; Placeholder used when populating the menu of the secondary buffers.
    (define-key menu [placeholder] nil)

    ;; --------------------
    ;; The "Window Layout" submenu.
    (let ((submenu (make-sparse-keymap)))
      (define-key menu [layout] (cons "Window Layout" submenu)))

    ;; ----------------
    ;; The "Window Layout" submenu.


    ;; TODO: The following is a somewhat clumsy implementation. Maybe we can
    ;; automatically generate the entries, or use the `dynamic' menu kind?
    ;;
    ;; Also, there might be other situations where the list might be
    ;; handy, e.g. completion.
    (let ((predefined '(rdebug-window-layout-standard
                        rdebug-window-layout-no-shell
                        rdebug-window-layout-conservative
                        rdebug-window-layout-stack-of-windows
                        rdebug-window-layout-rocky
                        rdebug-window-layout-rocky2)))

      (define-key map [menu-bar debugger layout other]
        (rdebug-menu-item
         common-map
         "Other"
         'rdebug-set-window-layout
         :button
         `(:radio
           . (not (memq rdebug-window-layout-function (quote ,predefined))))))

      (define-key map [menu-bar debugger layout rocky]
        (rdebug-menu-item
         common-map
         "Rocky's Own"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-rocky))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-rocky))))

      (define-key map [menu-bar debugger layout rocky2]
        (rdebug-menu-item
         common-map
         "Rocky II"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-rocky2))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-rocky2))))

      (define-key map [menu-bar debugger layout stack]
        (rdebug-menu-item
         common-map
         "Stack of Windows"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-stack-of-windows))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-stack-of-windows))))

      (define-key map [menu-bar debugger layout conservative]
        (rdebug-menu-item
         common-map
         "Conservative"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-conservative))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-conservative))))

      (define-key map [menu-bar debugger layout no-shell]
        (rdebug-menu-item
         common-map
         "No Shell"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-no-shell))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-no-shell))))

      (define-key map [menu-bar debugger layout standard]
        (rdebug-menu-item
         common-map
         "Standard"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-standard))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-standard)))))

    (define-key map [menu-bar debugger layout line3] '(menu-item "--"))

    (define-key map [menu-bar debugger layout initial]
      (rdebug-menu-item common-map
                        "Restore Debugger Layout"
                        'rdebug-restore-debugger-window-layout
                        :enable '(fboundp 'rdebug-restore-debugger-window-layout)))

    (define-key map [menu-bar debugger layout line1] '(menu-item "--"))

    ;; Note: It seems as though :enable doesn't work when :button is used.
    (define-key map [menu-bar debugger layout debugger]
      (rdebug-menu-item common-map "Current Debugger Layout"
                        'rdebug-display-debugger-window-configuration
                        :button
                        '(:radio
                          . (eq rdebug-window-configuration-state 'debugger))))

    (define-key map [menu-bar debugger layout original]
      (rdebug-menu-item common-map "Original Layout"
                        'rdebug-display-original-window-configuration
                        :button
                        '(:radio
                          . (eq rdebug-window-configuration-state 'original))))

    ;; --------------------
    ;; The "View" submenu.
    (let ((submenu (make-sparse-keymap)))
      (define-key menu [view] (cons "View" submenu)))

    (define-key map [menu-bar debugger view output]
      (rdebug-menu-item common-map "Output" 'rdebug-display-output-buffer
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key map [menu-bar debugger view watch]
      (rdebug-menu-item common-map "Watch" 'rdebug-display-watch-buffer
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key map [menu-bar debugger view stack]
      (rdebug-menu-item common-map "Stack-Frame trace" 
			'rdebug-display-frame-buffer
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key map [menu-bar debugger view shell]
      (rdebug-menu-item common-map "Debugger Shell" 'rdebug-display-cmd-buffer
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key map [menu-bar debugger view variables]
      (rdebug-menu-item common-map "Variables" 'rdebug-display-variables-buffer
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key map [menu-bar debugger view breakpoints]
      (rdebug-menu-item common-map
                        "Breakpoints" 'rdebug-display-breakpoints-buffer
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key map [menu-bar debugger view source]
      (rdebug-menu-item common-map
                        "Source" 'rdebug-display-source-buffer
                        :enable '(get-buffer-process gud-comint-buffer)))
    menu))


;; -----------------------------------------------
;; Key bindings and menu for secondary buffers.
;;

(defun rdebug-populate-secondary-buffer-map-plain (map)
  "Bind the plain keys used in rdebug secondary buffers.

This does not menus or prefix keys."
  ;; Keys to view other buffers.
  (let ((prefix-map (make-sparse-keymap)))
    (define-key map "?" 'rdebug-display-secondary-window-help-buffer)
    (define-key map "B" 'rdebug-display-breakpoints-buffer)
    (define-key map "C" 'rdebug-display-cmd-buffer)
    (define-key map "E" 'rdebug-display-error-buffer)
    (define-key map "F" 'rdebug-display-frame-buffer)
    (define-key map "I" 'rdebug-display-info-buffer)
    (define-key map "O" 'rdebug-display-output-buffer)
    (define-key map "S" 'rdebug-display-source-buffer)
    (define-key map "V" 'rdebug-display-variables-buffer)
    (define-key map "W" 'rdebug-display-watch-buffer)
    ;; Common debugger commands.
    (define-key map " " 'rdebug-step)
    (define-key map "_" 'rdebug-set-stepping-prefix)
    (define-key map "+" 'rdebug-set-stepping-prefix)
    (define-key map "-" 'rdebug-set-stepping-prefix)
    (define-key map "<" 'rdebug-newer-frame)
    (define-key map ">" 'rdebug-older-frame)
    ;; (define-key map "a" 'gud-args)
    ;; (define-key map "b" 'gud-break)
    (define-key map "c" 'rdebug-continue)
    ;; (define-key map "d" 'gud-remove)
    (define-key map "f" 'gud-finish)
    (define-key map "n" 'rdebug-next)
    (define-key map "p" prefix-map)
    (define-key map "q" 'rdebug-quit)
    (define-key map "r" 'rdebug-restart)
    (define-key map "R" 'rdebug-restart)
    (define-key map "s" 'rdebug-step)
    (define-key map [M-down]   'rdebug-locring-newer)
    (define-key map [M-up]     'rdebug-locring-older)
    (define-key map [M-S-down] 'rdebug-locring-newest)
    (define-key map [M-S-up]   'rdebug-locring-oldest)
    (define-key map [mouse-3]  'rdebug-variables-pretty-print-mouse)
    (define-key prefix-map "l" 'rdebug-print-list-region)
    (define-key prefix-map "p" 'rdebug-pretty-print-region)
    (define-key prefix-map "s" 'rdebug-print-sorted-region)
    ))


;; -------------------------------------------------------------------
;; Window layout.
;;

;; This function is intended for the Options submenu.
(defun rdebug-set-window-layout (func)
  "Set and, if the debugger is running, display the window layout."
  (interactive "aWindow layout function: ")
  (setq rdebug-window-layout-function func)
  (if gud-comint-buffer
      (with-no-warnings
        (rdebug-setup-windows))))


;; -------------------------------------------------------------------
;; The source buffer rdebug support mode.
;;
;; This is a minor mode that is active in Ruby source buffers. It
;; provides the menu and, when the debugger is active, the debugger
;; key bindings.

(defvar rdebug-debugger-support-minor-mode-map-when-deactive
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (rdebug-populate-debugger-menu map)
    (rdebug-populate-secondary-buffer-map-plain prefix-map)
    (define-key map gud-key-prefix prefix-map)
    map)
  "Keymap used by rdebugs support minor mode when the debugger is active.")

(defvar rdebug-debugger-support-minor-mode-map-when-active
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (rdebug-populate-debugger-menu map)
    (rdebug-populate-secondary-buffer-map-plain prefix-map)
    (define-key prefix-map [insert] 'rdebug-short-key-mode)
    (define-key map gud-key-prefix prefix-map)
    (rdebug-populate-common-keys map)
    map)
  "Keymap used by rdebugs support minor mode when the debugger not active.")


(define-minor-mode rdebug-debugger-support-minor-mode
  "Minor mode active in source buffers that use the `rdebug' Ruby debugger."
  :group rdebug
  :global nil
  :init-value nil
  :keymap rdebug-debugger-support-minor-mode-map-when-deactive
  (setq mode-line-process (and rdebug-debugger-support-minor-mode
                               'rdebug-mode-line-process)))


;;;###autoload
(defun rdebug-turn-on-debugger-support ()
  "Enable extra source buffer support for the `rdebug' Ruby debugger.

This includes a 'Debugger' menu and special key bindings when the
debugger is active."
  (rdebug-debugger-support-minor-mode 1))


;; -------------------------------------------------------------------
;; Use separate I/O buffer
;;

(defun rdebug-toggle-use-separate-io-buffer ()
  "Toggle `rdebug-use-separate-io-buffer'.
This is used by the menu."
  (interactive)
  (setq rdebug-use-separate-io-buffer (not rdebug-use-separate-io-buffer))
  (if (interactive-p)
      (message "Issue M-x rdebug-restore-debugger-window-layout \
RET to update display.")))


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug-source)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-source.el ends here

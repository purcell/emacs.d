;;; ecb-create-layout.el --- creating new layouts

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2002

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-create-layout.el,v 1.32 2005/05/23 15:49:15 berndl Exp $

;;; Commentary:
;;
;; Contains code for easy creating new layouts

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.


;;; Code

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(require 'ecb-mode-line)
(require 'ecb-util)
(require 'ecb-compilation)

;; XEmacs stuff
(silentcomp-defvar vertical-divider-map)
(silentcomp-defvar modeline-map)
;; Emacs 21.X stuff
(silentcomp-defvar automatic-hscrolling)
(silentcomp-defvar before-make-frame-hook)
(silentcomp-defvar after-make-frame-functions)
;; First loaded during activated ECB
(silentcomp-defvar ecb-buildin-layouts)

(defgroup ecb-create-layout nil
  "Settings for creating new ECB-layouts."
  :group 'ecb-layout
  :prefix "ecb-create-layout-")

(defcustom ecb-create-layout-file "~/.ecb-user-layouts.el"
  "*File where all layouts created by `ecb-create-new-layout' are stored."
  :group 'ecb-create-layout
  :type 'file)

(defcustom ecb-create-layout-frame-width 110
  "*Frame width of the layout creation frame."
  :group 'ecb-create-layout
  :type 'integer)

(defcustom ecb-create-layout-frame-height 42
  "*Frame height of the layout creation frame."
  :group 'ecb-create-layout
  :type 'integer)


;; internal vars and consts


(defconst ecb-create-layout-buf-prefix " *ECB-LC-")
(defconst ecb-create-layout-frame-name "Creation of a new ECB-layout")
(defconst ecb-create-layout-all-buf-types
  '("directories" "history" "methods" "sources" "speedbar" "analyse"))

(defconst ecb-create-layout-help-text-left-right
  "
 ECB layout creation mode
 ========================

 This is the help-screen of this mode. The window
 displaying this help text is called the edit-window
 which is neither selectable nor delete-able nor
 split-able in this mode.

 <left/right/up/down-arrow>: Moving around in current
 window C-n, C-p: Go to next/previous window (beside
 the edit-window)

 C-s: Split current window. You will be asked:
      - If \"vertical\" or \"horizontal\" split
      - How to split: \"at-point\", \"half\" or
        \"other\" (i.e. you can specify any fraction
        between 0.1 and 0.9)
      - Which type the current window should be
        \(see description of C-t below).
 C-u: Unsplit, ie. delete current window
 C-t: Give the current window a built-in type
      (\"directories\", \"sources\", \"methods\",
      \"history\" etc.) or any arbitrary user-defined
      type (\"other\"). See the Online-manual!

 C-c: Cancel layout creation. This does not save the
      layout. Deletes this frame.
 C-q: Save current defined layout and quit the layout
      creation. You will be asked for a layout-name.
      With TAB-completion you can get the names already
      in use. You have to choose a new name!
      Deletes this frame.

 There are NO other commands or keys available. ALL
 other keys are disabled in this mode!
")

(defconst ecb-create-layout-help-text-top
  " ECB layout creation mode
 ========================

 This is the help-screen of this mode. The window displaying this help text is called
 the edit-window which is neither selectable nor delete-able nor split-able in this mode.

 <left/right/up/down-arrow>: Moving around in current window
 C-n, C-p: Go to next/previous window (beside the edit-window)

 C-s: Split current window. You will be asked:
      - If \"vertical\" or \"horizontal\" split
      - How to split: \"at-point\", \"half\" or \"other\" (i.e. you can specify any
        fraction between 0.1 and 0.9)
      - Which type the current window should be \(see description of C-t below).
 C-u: Unsplit, ie. delete current window
 C-t: Give the current window a built-in type (\"directories\", \"sources\", \"methods\",
      \"history\" etc.) or any arbitrary user-defined type (\"other\").

 C-c: Cancel layout creation. This does not save the layout. Deletes this frame.
 C-q: Save current defined layout and quit the layout creation. You will be asked for a
      layout-name. With TAB-completion you can get the names already in use.
      You have to choose a new name! Deletes this frame.

 There are NO other commands or keys available. ALL other keys are disabled in this mode!
")

(defconst ecb-create-layout-file-header
     "

;; Copyright (C) 2001 - 2005 Jesper Nordenberg
;; Copyright (C) 2001 - 2005 Free Software Foundation, Inc.
;; Copyright (C) 2001 - 2005 Klaus Berndl <klaus.berndl@sdm.de>

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: java, class, browser

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains all user-defined ECB-layouts created by the command
;; `ecb-create-new-layout'.

;; !!! DO NOT EDIT THIS FILE MANUALLY - IT IS GENERATED BY ECB !!!

")


(defvar ecb-create-layout-frame nil)
(defvar ecb-create-layout-edit-window nil)

(defvar ecb-create-layout-old-global-map nil)
(defvar ecb-create-layout-old-minor-mode-map-alist nil)
(defvar ecb-create-layout-old-hscroll nil)
(defvar ecb-create-layout-old-debug-on-error nil)
(defvar ecb-create-layout-old-frame nil)
(defvar ecb-create-layout-old-vertical-div-map nil)
(defvar ecb-create-layout-old-modeline-map nil)
(defvar ecb-create-layout-old-after-frame-h nil)
(defvar ecb-create-layout-old-before-frame-h nil)

(defvar ecb-create-layout-generated-lisp nil)
(defvar ecb-create-layout-gen-counter 0)

(defvar ecb-create-layout-buf-types nil)

;; can be 'left, 'right, 'top or 'left-right
(defvar ecb-create-layout-type 'left)

(defun ecb-create-layout-initilize ()
  (setq ecb-create-layout-buf-types
        (ecb-copy-list ecb-create-layout-all-buf-types))
  (setq ecb-create-layout-frame nil)
  (setq ecb-create-layout-edit-window nil)
  (setq ecb-create-layout-old-global-map nil)
  (setq ecb-create-layout-old-minor-mode-map-alist nil)
  (setq ecb-create-layout-old-hscroll nil)
  (setq ecb-create-layout-old-frame nil)

  (if ecb-running-xemacs
      (progn
        (setq ecb-create-layout-old-vertical-div-map nil)
        (setq ecb-create-layout-old-modeline-map nil))
    (setq ecb-create-layout-old-after-frame-h nil)
    (setq ecb-create-layout-old-before-frame-h nil))
  
  (setq ecb-create-layout-generated-lisp nil)
  (setq ecb-create-layout-gen-counter 0))

(defvar ecb-create-layout-frame-deleted nil)

(defadvice delete-frame (before ecb-create-layout)
  "Ensure calling `ecb-create-layout-cancel' during deleting the
layout-creation frame. Does nothing for any other other frame!"
  (let ((frame (or (ad-get-arg 0) (selected-frame))))
    (when (ecb-string= (ecb-frame-parameter frame 'name)
                       ecb-create-layout-frame-name)
      (setq ecb-create-layout-frame-deleted t)
      (ecb-create-layout-cancel))))

(defadvice delete-frame (after ecb-create-layout)
  "Ensures correct deleting of the layout-creation frame. Does nothing for any
other other frame!"
  (when ecb-create-layout-frame-deleted
    (setq ecb-create-layout-frame-deleted nil)
    (ecb-activate))
  (ad-disable-advice 'delete-frame 'after 'ecb-create-layout)
  (ad-activate 'delete-frame))
  

(defun ecb-create-layout-frame-ok ()
  "Return not nil if current frame is the `ecb-create-layout-frame'"
  (and ecb-create-layout-frame
       (frame-live-p ecb-create-layout-frame)
       (equal (selected-frame) ecb-create-layout-frame)))

(defun ecb-create-layout-cancel (&rest ignore)
  "Cancel layout-creation without saving the layout."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (ecb-create-layout-clear-all (interactive-p))
    (message "ECB Layout Creation canceled - the layout is not saved!")
    (and (interactive-p) (ecb-activate))))

(defun ecb-create-layout-clear-all (&optional delete-frame)
  "Resets all stuff to state before `ecb-create-new-layout' was called. If
DELETE-FRAME is not nil then the new created frame will be deleted and the
`ecb-create-layout-old-frame' will be selected."
  ;; disabling the advice
  (ad-disable-advice 'delete-frame 'before 'ecb-create-layout)
  (ad-activate 'delete-frame)
  ;; killing all white-space-filled layout-buffers
  (dolist (b (buffer-list ecb-create-layout-frame))
    (if (string-match "^ \\*ECB-LC-" (buffer-name b))
        (kill-buffer b)))
  ;; restore the global-map
  (if (keymapp ecb-create-layout-old-global-map)
      (use-global-map ecb-create-layout-old-global-map))
  ;; restore the minor-mode-maps
  (if ecb-create-layout-old-minor-mode-map-alist
      (setq minor-mode-map-alist
            ecb-create-layout-old-minor-mode-map-alist))
  ;; restore horiz. scrolling
  (unless ecb-running-xemacs
    (setq automatic-hscrolling ecb-create-layout-old-hscroll))
  ;; for XEmacs restore these maps
  (if ecb-running-xemacs
      (progn
        (setq vertical-divider-map ecb-create-layout-old-vertical-div-map)
        (setq modeline-map ecb-create-layout-old-modeline-map))
    ;; before and after making frame stuff
    (setq before-make-frame-hook ecb-create-layout-old-before-frame-h)
    (setq after-make-frame-functions ecb-create-layout-old-after-frame-h))
  ;; restore old debug-on-error
  (setq debug-on-error ecb-create-layout-old-debug-on-error)
  ;; delete the layout-frame and select the ecb-create-layout-old-frame
  (when delete-frame
    (when (and ecb-create-layout-old-frame
               (frame-live-p ecb-create-layout-old-frame))
      (raise-frame ecb-create-layout-old-frame)
      (select-frame ecb-create-layout-old-frame))
    (when (and ecb-create-layout-frame
               (frame-live-p ecb-create-layout-frame))
      (ad-with-originals 'delete-frame
        (delete-frame ecb-create-layout-frame))))
  (setq ecb-create-layout-frame nil))

(defun ecb-create-layout-save-and-quit (&rest ignore)
  "Quit the ECB Layout creation and save the defined layout."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (if (ecb-create-layout-ready-for-save-p)
        (let ((delete-frame (interactive-p)))
          ;; if an error occurs during `ecb-create-layout-save-layout' or the
          ;; user hits C-q we must clean the layout creation stuff!
          (unwind-protect
              (ecb-create-layout-save-layout)
            ;; clean the layout creation stuff
            (ecb-create-layout-clear-all delete-frame)
            (message "ECB Layout Creation finished.")
            (ecb-activate)))
      (ecb-error "You must give every ECB-tree-window a type (use C-t)!"))))


(defun ecb-create-layout-ready-for-save-p ()
  "Returns only not nil if all windows in current layout have a type."
  (let ((save-p t))
    (save-excursion
      (dolist (win (ecb-window-list (selected-frame) 0))
        (unless (equal win ecb-create-layout-edit-window)
          (set-buffer (window-buffer win))
          (setq save-p (ecb-create-layout-buffer-type)))))
    save-p))


(defmacro ecb-create-layout-insert-line (line)
  "Insert LINE in current-buffer and adds a newline."
  `(progn
     (insert ,line)
     (insert "\n")))

(defun ecb-create-layout-insert-file-header ()
  (insert (format ";;; %s --- user defined ECB-layouts" ;;
                  (file-name-nondirectory ecb-create-layout-file)))
  (insert ecb-create-layout-file-header))

(defun ecb-create-layout-save-layout ()
  "Saves current layout in `ecb-create-layout-file'."
  ;; make edit-window the current selected window
  (ecb-create-layout-select-edit-window)
  ;; we need the reversed sequence of the generated code
  (setq ecb-create-layout-generated-lisp
        (nreverse ecb-create-layout-generated-lisp))
  ;; ensure we have load all layouts defined until now
  (ecb-load-layouts)
  ;; now we have the create-code in the right sequence so we can save the new
  ;; layout in the user-layout file
  (let ((layout-name ""))
    ;; a repeat...until-loop
    (while (progn
             ;;the while body
             (setq layout-name
                   (ecb-choose-layout-name (ecb-available-layouts-of-type nil)
                                           nil))
             ;; the while condition
             (ecb-available-layouts-member-p layout-name)))
    (with-temp-file (expand-file-name ecb-create-layout-file)
      (erase-buffer)
      (if (file-readable-p (expand-file-name ecb-create-layout-file))
          (insert-file-contents (expand-file-name ecb-create-layout-file))
        (ecb-create-layout-insert-file-header))
      (goto-char (point-max))
      ;; insert header of the layout-define macro
      (ecb-create-layout-insert-line
       (format "(ecb-layout-define \"%s\" %s nil"
               layout-name
               (symbol-name ecb-create-layout-type)))
      ;; insert all the generated layout-code of the new layout
      (dolist (line ecb-create-layout-generated-lisp)
        (ecb-create-layout-insert-line
         (format "  %s" line)))
      ;; close the new layout-function
      (ecb-create-layout-insert-line "  )"))
    ;; now we load the new layout
    (load-file (expand-file-name ecb-create-layout-file))
    (message "The new layout is saved in %s, loaded and available!"
             ecb-create-layout-file)))

(defun ecb-create-layout-gen-lisp (lisp-statement)
  (setq ecb-create-layout-generated-lisp
        (cons lisp-statement ecb-create-layout-generated-lisp)))

(defun ecb-create-layout-split-ver (&optional fraction)
  (let ((factor (or fraction
                    (/ (float (count-lines (window-start) (point)))
                       (float (- (ecb-window-full-height) 2))))))
    (ecb-split-ver factor t)
    (ecb-create-layout-gen-lisp `(ecb-split-ver ,factor t))
    factor))

(defun ecb-create-layout-split-hor (&optional fraction)
  (let ((factor (or fraction
                    (/ (float (- (point) (ecb-line-beginning-pos)))
                       (float (- (window-width) 3))))))
    (ecb-split-hor factor t)
    (ecb-create-layout-gen-lisp `(ecb-split-hor ,factor t))
    (beginning-of-line)
    factor))

(defun ecb-create-layout-add-to-buf-types (type)
  (when (and (stringp type)
             (member type ecb-create-layout-all-buf-types))
    (add-to-list 'ecb-create-layout-buf-types type)
    (setq ecb-create-layout-buf-types
          (sort ecb-create-layout-buf-types 'ecb-string<))))

(defun ecb-create-layout-remove-from-buf-type (type)
  (when (stringp type)
    (setq ecb-create-layout-buf-types
          (sort (delete type ecb-create-layout-buf-types) 'ecb-string<))))

(defun ecb-create-layout-buffer-type ()
  (get-text-property (point-min) 'ecb-create-layout-type))

(defun ecb-create-layout-buffer-factor ()
  (get-text-property (point-min) 'ecb-create-layout-factor))

(defun ecb-create-layout-set-buffer-type (type)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (1+ (point-min)) 'ecb-create-layout-type
                       type)))

(defun ecb-create-layout-set-buffer-factor (factor)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (1+ (point-min)) 'ecb-create-layout-factor
                       factor)))


(defun ecb-create-layout-gen-lisp-for-buffer-type (type)
  (let ((func-sym (intern (format "ecb-set-%s-buffer" type))))
    (ecb-create-layout-gen-lisp
     `(if (fboundp (quote ,func-sym))
          (,func-sym)
        (ecb-set-default-ecb-buffer)))))
  

(defun ecb-create-layout-set-buffer-to-type (&optional type)
  "Give current ECB-buffer a type."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    ;; adding the old buffer type to the available-list
    (ecb-create-layout-add-to-buf-types (or type
                                            (ecb-create-layout-buffer-type)))
    (let ((new-type (or (and (stringp type) type)
                        (ecb-query-string "Type of current ECB-tree-buffer:"
                                          ecb-create-layout-buf-types
                                          "Insert the buffer type"))))
      ;; removing the new buffer type from the available-list
      (ecb-create-layout-remove-from-buf-type new-type)
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Fix this - it seems not
      ;; work anymore!!!
      (ecb-mode-line-set (buffer-name (current-buffer))
                         (selected-frame)
                         (concat "ECB " new-type) nil t)
      ;; setting the new buffer type in the buffer itself
      (ecb-create-layout-set-buffer-type new-type)
      (when (interactive-p)
        (ecb-create-layout-gen-lisp-for-buffer-type new-type)
        (ecb-create-layout-next-window))
      new-type)))

(defun ecb-create-layout-select-edit-window ()
  (let ((counter 0))
    (while (not (equal (selected-window) ecb-create-layout-edit-window))
      (other-window 1)
      (setq counter (1+ counter)))
    (ecb-create-layout-gen-lisp `(dotimes (i ,counter)
                                   (other-window 1)
                                   (if (equal (selected-window)
                                              ecb-compile-window)
                                       (other-window 1))))))

(defun ecb-create-layout-split ()
  "Split current window."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    ;; splitting
    (let* ((old-buf-type (ecb-create-layout-buffer-type))
           (split-choices (if (equal ecb-create-layout-type 'top)
                              '("horizontal" "vertical")
                            '("vertical" "horizontal")))
           (split-type (ecb-query-string "Split type:" split-choices))
           (split-method
            (ecb-query-string "Split method:"
                              '("at-point" "half")
                              "Insert a fraction between 0.1 and 0.9"))
           (fraction (cond ((ecb-string= split-method "at-point")
                            nil)
                           ((ecb-string= split-method "half")
                            0.5)
                           ((floatp (string-to-number split-method))
                            (string-to-number split-method))
                           (t 0.5)))
           (real-split-factor
            (if (ecb-string= split-type "horizontal")
                (ecb-create-layout-split-hor fraction)
              (ecb-create-layout-split-ver fraction))))
      ;; creating new fitting buffers
      (save-selected-window
        (ecb-create-layout-new-buffer)
        (select-window (next-window))
        (ecb-create-layout-new-buffer))
      ;; asking for the buffer type
      (ecb-create-layout-set-buffer-factor real-split-factor)
      (ecb-create-layout-gen-lisp-for-buffer-type
       (ecb-create-layout-set-buffer-to-type old-buf-type))
      (ecb-create-layout-next-window))))

(defun ecb-create-layout-forward-char ()
  "Move one character forward."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (unless (> (- (point) (ecb-line-beginning-pos)) (- (window-width)
                                                       (if ecb-running-xemacs
                                                           3
                                                         2)))
      (call-interactively 'forward-char))))

(defun ecb-create-layout-next-window ()
  "Go to the next window.
This command always goes to the next special ECB-window, i.e. it never selects
the edit-window."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (let ((steps (if (equal (next-window) ecb-create-layout-edit-window) 2 1)))
      (other-window steps)
      (ecb-create-layout-gen-lisp `(dotimes (i ,steps)
                                     (other-window 1)
                                     (if (equal (selected-window)
                                                ecb-compile-window)
                                         (other-window 1)))))))

(defun ecb-create-layout-previous-window ()
  "Go to the previous window.
This command always goes to the ECB-window preceding current window, i.e. it
never selects the edit-window."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (let ((steps (if (equal (previous-window (selected-window) 0)
                            ecb-create-layout-edit-window)
                     -2 -1)))
      (other-window steps)
      (ecb-create-layout-gen-lisp `(dotimes (i ,(abs steps))
                                     (other-window -1)
                                     (if (equal (selected-window)
                                                ecb-compile-window)
                                         (other-window -1)))))))

(defun ecb-create-layout-delete-window ()
  "Delete current window."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (unless (or (equal (selected-window) ecb-create-layout-edit-window)
                (= (length (ecb-window-list nil 0))
                   (if (equal ecb-create-layout-type 'left-right) 3 2)))
      (if (and (member ecb-create-layout-type '(right left-right))
               (equal (previous-window (selected-window) 0)
                      ecb-create-layout-edit-window)
               (> (nth 0 (ecb-window-edges (next-window))) (nth 0 (ecb-window-edges)))
               (= (nth 3 (ecb-window-edges ecb-create-layout-edit-window))
                  (nth 3 (ecb-window-edges))))
          ;; In exactly this window context we can not delete the current
          ;; window because otherwise the edit-window would enlarge and the
          ;; wrong window would be deleted!
          (ecb-error "This window can not be deleted! Delete another one.")
        ;; add the buffer type of the deleted window to the available-list
        (ecb-create-layout-add-to-buf-types (ecb-create-layout-buffer-type))
        (kill-buffer (current-buffer))
        (delete-window)
        (ecb-create-layout-gen-lisp '(delete-window))
        (if (equal (selected-window) ecb-create-layout-edit-window)
            (ecb-create-layout-previous-window))
        ;; add the buffer type of the new bigger window to the available-list
        (ecb-create-layout-add-to-buf-types (ecb-create-layout-buffer-type))
        (kill-buffer (current-buffer))
        (ecb-create-layout-new-buffer)))))

(defvar ecb-create-layout-mode-map nil
  "`ecb-create-layout-mode' key-map.")

(if ecb-create-layout-mode-map
    ()
  (setq ecb-create-layout-mode-map (make-sparse-keymap))
;;  (suppress-key-map ecb-create-layout-mode-map t)

  ;; for minibuffer insertion we need the following
  (dotimes (i 26)
    (define-key ecb-create-layout-mode-map
      (string (+ i 97)) 'self-insert-command))

  (dotimes (i 26)
    (define-key ecb-create-layout-mode-map
      (string (+ i 65)) 'self-insert-command))

  (dotimes (i 10)
    (define-key ecb-create-layout-mode-map
      (string (+ i 48)) 'self-insert-command))

  (define-key ecb-create-layout-mode-map "." 'self-insert-command)
  (define-key ecb-create-layout-mode-map "-" 'self-insert-command)
  
  (if ecb-running-xemacs
      (define-key ecb-create-layout-mode-map (kbd "<BS>")
        'delete-backward-char)
    (define-key ecb-create-layout-mode-map (kbd "<DEL>")
      'backward-delete-char-untabify))

  (define-key ecb-create-layout-mode-map (kbd "C-q")
    'ecb-create-layout-save-and-quit)
  (define-key ecb-create-layout-mode-map (kbd "C-c")
    'ecb-create-layout-cancel)
  (define-key ecb-create-layout-mode-map (kbd "C-u")
    'ecb-create-layout-delete-window)
  (define-key ecb-create-layout-mode-map (kbd "C-s") 'ecb-create-layout-split)
  (define-key ecb-create-layout-mode-map (kbd "C-t")
    'ecb-create-layout-set-buffer-to-type)
  (define-key ecb-create-layout-mode-map (kbd "<left>") 'backward-char)
  (define-key ecb-create-layout-mode-map (kbd "<right>")
    'ecb-create-layout-forward-char)
  (define-key ecb-create-layout-mode-map (kbd "<up>") 'previous-line)
  (define-key ecb-create-layout-mode-map (kbd "<down>") 'next-line)
  (define-key ecb-create-layout-mode-map (kbd "C-n")
    'ecb-create-layout-next-window)
  (define-key ecb-create-layout-mode-map (kbd "C-p")
    'ecb-create-layout-previous-window)
;;   (define-key ecb-create-layout-mode-map (kbd "C-h v")
;;     'describe-variable)
;;   (define-key ecb-create-layout-mode-map (kbd "C-h k")
;;     'describe-key)
;;   (define-key ecb-create-layout-mode-map (kbd "C-h d")
;;     'ecb-create-layout-debug)
;;   (define-key ecb-create-layout-mode-map (kbd "M-<down>")
;;     'scroll-other-window)
  (set-keymap-parent ecb-create-layout-mode-map nil))


(defun ecb-create-layout-new-buffer (&optional do-not-fill)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer (generate-new-buffer ecb-create-layout-buf-prefix))
  (erase-buffer)
  (unless do-not-fill
    (dotimes (i (ecb-window-full-height))
      (insert
       (format "%s\n"
               (make-string (- (window-width)
                               (if ecb-running-xemacs 3 1))
                            ?\ )))))
  (goto-char (point-min))
  (ecb-create-layout-mode)
  (set-window-dedicated-p (selected-window) t))



(defun ecb-create-layout-mode ()
  "Major mode for creating new ECB-layouts."
  (setq major-mode 'ecb-create-layout-mode)
  (setq mode-name "ECB Create-Layout")
  (use-local-map ecb-create-layout-mode-map)
  (make-variable-buffer-local 'buffer-read-only)
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Scheint nicht mehr zu
  ;; funktionieren.
  (ecb-mode-line-set (buffer-name (current-buffer))
                     (selected-frame) "" nil t)
  (setq buffer-read-only t))

(defun ecb-create-layout-init-layout (&optional new)
  (delete-other-windows)
  (ecb-create-layout-new-buffer)
  (when new
    (setq ecb-create-layout-type (intern (ecb-query-string
                                          "Location of the the ECB-tree-windows:"
                                          '("left" "right" "top" "left-right")))))
  (case ecb-create-layout-type
    (left
     (ecb-split-hor ecb-windows-width))
    (right
     (ecb-split-hor (- ecb-windows-width) t))
    (top
     (ecb-split-ver ecb-windows-height))
    (otherwise
     (ecb-split-hor (- (* 0.667 ecb-windows-width)) t)
     (ecb-split-hor (* 0.667 ecb-windows-width) nil t)))
  ;; we set the buffer in the big edit-window
  (ecb-create-layout-new-buffer t)
  ;; now we insert the help in the edit-window
  (let ((buffer-read-only nil))
    (insert (if (equal ecb-create-layout-type 'top)
                ecb-create-layout-help-text-top
              ecb-create-layout-help-text-left-right)))
  (setq ecb-create-layout-edit-window (selected-window))
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Scheint nicht mehr zu
  ;; funktionieren.
  (ecb-mode-line-set (buffer-name (current-buffer))
                     (selected-frame) "   ECB edit-window" nil t)
  ;; The edit window must not be dedicated
  (set-window-dedicated-p (selected-window) nil)
  ;; we set the buffer for the (currently unsplitted) ECB-window
  (other-window 1)
  (ecb-create-layout-new-buffer)
  ;; for the left-right type we have to set the other column too
  (when (equal ecb-create-layout-type 'left-right)
    (other-window 1)
    (ecb-create-layout-new-buffer)))


(defun ecb-create-layout-make-frame ()
  "Create a new frame for the layout creation process and return it."
  (if ecb-running-xemacs
      (make-frame `((name . ,ecb-create-layout-frame-name)
                    (minibuffer . t)
                    (user-position . t)
                    (width . ,ecb-create-layout-frame-width)
                    (height . ,ecb-create-layout-frame-height)
                    (default-toolbar-visible-p . nil)
                    (left-toolbar-visible-p . nil)
                    (right-toolbar-visible-p . nil)
                    (top-toolbar-visible-p . nil)
                    (bottom-toolbar-visible-p . nil)
                    (default-gutter-visible-p . nil)
                    (left-gutter-visible-p . nil)
                    (right-gutter-visible-p . nil)
                    (top-gutter-visible-p . nil)
                    (bottom-gutter-visible-p . nil)
                    (has-modeline-p . t)
                    (use-left-overflow . nil)
                    (vertical-scrollbar-visible-p . nil)
                    (horizontal-scrollbar-visible-p . nil)
                    (use-right-overflow . nil)
                    (menubar-visible-p . nil)))
    (make-frame `((name . ,ecb-create-layout-frame-name)
                  (minibuffer . t)
                  (user-position . t)
                  (width . ,ecb-create-layout-frame-width)
                  (height . ,ecb-create-layout-frame-height)
                  (vertical-scroll-bars . nil)
                  (horizontal-scroll-bars . nil)
                  (tool-bar-lines . 0)
                  (menu-bar-lines . 0)))))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Wir müssen ev. ECB vorher
;; deaktivieren, da sonst ein 2. ECB-menu entsteht. Beim C-c oder C-q eben
;; dann wieder aktivieren.
(defun ecb-create-new-layout ()
  "Start interactively layout creating."
  (interactive)

  (ecb-deactivate)
  (ecb-create-layout-initilize)

  ;; before- and after make frame stuff
  (when (not ecb-running-xemacs)
    (setq ecb-create-layout-old-after-frame-h after-make-frame-functions)
    (setq after-make-frame-functions nil)
    (setq ecb-create-layout-old-before-frame-h before-make-frame-hook)
    (setq before-make-frame-hook nil))
    
  ;; saving old frame
  (setq ecb-create-layout-old-frame (selected-frame))

  ;; creating new frame
  (setq ecb-create-layout-frame (ecb-create-layout-make-frame))
  (raise-frame ecb-create-layout-frame)
  (select-frame ecb-create-layout-frame)
  (ad-enable-advice 'delete-frame 'before 'ecb-create-layout)
  (ad-enable-advice 'delete-frame 'after 'ecb-create-layout)
  (ad-activate 'delete-frame)

  ;; global map
  (setq ecb-create-layout-old-global-map (current-global-map))
  (use-global-map ecb-create-layout-mode-map)

  ;; minor-modes map
  (setq ecb-create-layout-old-minor-mode-map-alist minor-mode-map-alist)
  (setq minor-mode-map-alist nil)

  ;; horiz. scrolling
  (unless ecb-running-xemacs
    (setq ecb-create-layout-old-hscroll automatic-hscrolling)
    (setq automatic-hscrolling nil))

  ;; for XEmacs modeline- and vertical-divider maps
  (when ecb-running-xemacs
    (setq ecb-create-layout-old-vertical-div-map vertical-divider-map)
    (setq vertical-divider-map nil)
    (setq ecb-create-layout-old-modeline-map modeline-map)
    (setq modeline-map nil))

  ;; debug on error
  (setq ecb-create-layout-old-debug-on-error debug-on-error)
  (setq debug-on-error nil)

  (ecb-create-layout-init-layout t))


(defun ecb-delete-new-layout ()
  "Select a layout-name and delete this layout.
This means the layout-definition is removed from the file
`ecb-create-layout-file' and the layout-function and associated aliases are
unbound."
  (interactive)
  ;; ensure we have load all layouts defined until now
  (ecb-load-layouts)
  (let ((new-layout-list
         (sort (ecb-set-difference (ecb-available-layouts-of-type nil)
                               (mapcar (function (lambda (elem)
                                                   (car elem)))
                                       ecb-buildin-layouts))
               'ecb-string<))
        (layout-name nil))
    (if (= (length new-layout-list) 0)
        (ecb-error "There are no layouts to delete!")
      (setq layout-name (ecb-choose-layout-name new-layout-list t)))
    (with-temp-file (expand-file-name ecb-create-layout-file)
      (erase-buffer)
      (if (file-readable-p (expand-file-name ecb-create-layout-file))
          (insert-file-contents (expand-file-name ecb-create-layout-file))
        (ecb-error "This layout is not defined in %s!" ecb-create-layout-file))
      (goto-char (point-min))
      (if (re-search-forward (concat "^(ecb-layout-define +"
                                     "\"" layout-name "\".+$")
                             nil t)
          (progn
            ;; Deleting the layout definition in the file
            ;; `ecb-create-layout-file'.
            (beginning-of-line)
            (delete-region (match-beginning 0)
                           (progn
                             (forward-sexp)
                             (point)))
            (kill-line)
            ;; undefining the function and aliases.
            (ecb-layout-undefine layout-name))
        (ecb-error "This layout is not defined in %s!" ecb-create-layout-file)))))

(defun ecb-create-layout-debug ()
  "Debugging command for the ECB-developers."
  (interactive)
  (message "Layout-Debug: Type: %s, Factor: %s"
           (ecb-create-layout-buffer-type)
           (ecb-create-layout-buffer-factor)))

;; Klaus Berndl <klaus.berndl@sdm.de>: Cause of the magic autostart stuff of
;; the advice-package we must disable at load-time all these advices!!
;; Otherwise would just loading ecb (not activating!) activate each advice
;; AFTER the FIRST usage of our advices!!
(ad-disable-advice 'delete-frame 'after 'ecb-create-layout)
(ad-disable-advice 'delete-frame 'before 'ecb-create-layout)
(ad-activate 'delete-frame)


(silentcomp-provide 'ecb-create-layout)

;; ecb-help.el ends here

;;; srecode-mode.el --- Minor-mode for inserting templates into other files.

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This uses a bunch of semantic conveniences for making a minor mode.

(require 'srecode)
(require 'senator)
(require 'wisent)

;;; Code:
(defcustom global-srecode-minor-mode nil
  "Non-nil in buffers with Semantic Recoder macro keybindings."
  :group 'srecode
  :type 'boolean
  :require 'srecode-mode
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-srecode-minor-mode (if val 1 -1))))

(defvar srecode-minor-mode nil
  "Non-nil in buffers with Semantic Recoder macro keybindings.")
(make-variable-buffer-local 'srecode-minor-mode)

(defcustom srecode-minor-mode-hook nil
  "Hook run at the end of the function `srecode-minor-mode'."
  :group 'srecode
  :type 'hook)

;; We don't want to waste space.  There is a menu after all.
;;(add-to-list 'minor-mode-alist '(srecode-minor-mode ""))

(defvar srecode-prefix-key [(control ?c) ?/]
  "The common prefix key in srecode minor mode.")

(defvar srecode-prefix-map
  (let ((km (make-sparse-keymap)))
    ;; Basic template codes
    (define-key km "/" 'srecode-insert)
    (define-key km "e" 'srecode-edit)
    ;; Template direct binding
    ;; Template applications
    (define-key km "G" 'srecode-insert-getset)
    km)
  "Keymap used behind the srecode prefix key in in srecode minor mode.")

(defvar srecode-menu-bar
  (list
   "SRecoder"
   (senator-menu-item
    ["Insert Template"
      srecode-insert
      :active t
      :help "Insert a template by name."
      ])
   (senator-menu-item
    ["Edit Template"
     srecode-edit
     :active t
     :help "Edit a template for this language by name."
     ])
   "---"
   (senator-menu-item
    ["Dump Tables"
      srecode-dump-templates
      :active t
      :help "Dump the current template table."
      ])
   )
  "Menu for srecode minor mode.")

(defvar srecode-minor-menu nil
  "Menu keymap build from `srecode-menu-bar'.")

(defvar srecode-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km srecode-prefix-key srecode-prefix-map)
    (easy-menu-define srecode-minor-menu km "Srecode Minor Mode Menu"
                      srecode-menu-bar)
    km)
  "Keymap for srecode minor mode.")

;;;###autoload
(defun srecode-minor-mode (&optional arg)
  "Toggle srecode minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{srecode-mode-map}"
  (interactive
   (list (or current-prefix-arg
             (if srecode-minor-mode 0 1))))
  (setq srecode-minor-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not srecode-minor-mode)))
  (run-hooks 'srecode-minor-mode-hook)
  srecode-minor-mode)

;;;###autoload
(defun global-srecode-minor-mode (&optional arg)
  "Toggle global use of srecode minor mode.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-srecode-minor-mode
        (semantic-toggle-minor-mode-globally
         'srecode-minor-mode arg)))

;; Use the semantic minor mode magic stuff.
(semantic-add-minor-mode 'srecode-minor-mode
			 ""
			 srecode-mode-map)


;;; Minor Mode commands
;;
(defun srecode-edit (template-name)
  "Switch to the template buffer for TEMPLATE-NAME.
Template is chosen based on the mode of the starting buffer."
  (interactive (list (srecode-read-template-name "Template Name: ")))
  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
    (let ((newdict (srecode-create-dictionary))
	  (temp (srecode-template-get-table (srecode-table) template-name)))
      (if (not temp)
	  (error "No Template named %s" template-name))
      ;; We need a template specific table, since tables chain.
      (let ((tab (oref temp :table))
	    (names nil)
	    (ctxt nil))
	(find-file (oref tab :file))
	(setq names (semantic-find-tags-by-name (oref temp :object-name)
						(current-buffer)))
	(cond ((= (length names) 1)
	       (semantic-go-to-tag (car names))
	       (semantic-momentary-highlight-tag (car names)))
	      ((> (length names) 1)
	       (let* ((ctxt (semantic-find-tags-by-name (oref temp :context)
							(current-buffer)))
		      (cls (semantic-find-tags-by-class 'context ctxt))
		      )
		 (while (and names
			     (< (semantic-tag-start (car names))
				(semantic-tag-start (car cls))))
		   (setq names (cdr names)))
		 (if names
		     (progn
		       (semantic-go-to-tag (car names))
		       (semantic-momentary-highlight-tag (car names)))
		   (error "Can't find template %s" template-name))
		 ))
	      (t (error "Can't find template %s" template-name)))
	)))

  

(provide 'srecode-mode)

;;; srecode-mode.el ends here

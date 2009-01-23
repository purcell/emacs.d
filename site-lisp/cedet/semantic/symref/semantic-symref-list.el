;;; semantic-symref-list.el --- Symref Output List UI.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-symref-list.el,v 1.6 2008/12/30 23:03:32 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Provide a simple user facing API to finding symbol references.
;;
;; This UI will is the base of some refactoring tools.  For any
;; refactor, the user will execture `semantic-symref' in a tag.  Once
;; that data is collected, the output will be listed in a buffer.  In
;; the output buffer, the user can then initiate different refactoring
;; operations.
;;
;; NOTE: Need to add some refactoring tools.

(require 'semantic-symref)
(require 'pulse)

;;; Code:
;;;###autoload
(defun semantic-symref ()
  "Find references to the current tag.
This command uses the currently configured references tool within the
current project to find references to the current tag. The
references are the organized by file and the name of the function
they are used in.
Display the references in`semantic-symref-results-mode'"
  (interactive)
  (semantic-fetch-tags)
  (let ((ct (semantic-current-tag))
	(res nil)
	)
    ;; Must have a tag...
    (when (not ct) (error "Place cursor inside tag to be searched for"))
    ;; Check w/ user.
    (when (not (y-or-n-p (format "Find references for %s? " (semantic-tag-name ct))))
      (error "Quit"))
    ;; Gather results and tags
    (message "Gathering References...")
    (setq res (semantic-symref-find-references-by-name (semantic-tag-name ct)))
    (semantic-symref-produce-list-on-results res (semantic-tag-name ct))))

;;;###autoload
(defun semantic-symref-symbol (sym)
  "Find references to the symbol SYM.
This command uses the currently configured references tool within the
current project to find references to the input SYM. The
references are the organized by file and the name of the function
they are used in.
Display the references in`semantic-symref-results-mode'"
  (interactive "sSymbol: ")
  (semantic-fetch-tags)
  (let ((res nil)
	)
    ;; Gather results and tags
    (message "Gathering References...")
    (setq res (semantic-symref-find-references-by-name sym))
    (semantic-symref-produce-list-on-results res sym)))


(defun semantic-symref-produce-list-on-results (res str)
  "Produce a symref list mode buffer on the results RES."
    (when (not res) (error "No references found"))
    (semantic-symref-result-get-tags res t)
    (message "Gathering References...done")
    ;; Build a refrences buffer.
    (let ((buff (get-buffer-create
		 (format "*Symref %s" str)))
	  )
      (switch-to-buffer-other-window buff)
      (set-buffer buff)
      (semantic-symref-results-mode res))
    )

;;; RESULTS MODE
;;
(defgroup semantic-symref-results-mode nil
  "Symref Results group."
  :group 'semantic)

(defvar semantic-symref-results-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-i" 'forward-button)
    (define-key km "\M-C-i" 'backward-button)
    (define-key km " " 'push-button)
    (define-key km "-" 'semantic-symref-list-toggle-showing)
    (define-key km "=" 'semantic-symref-list-toggle-showing)
    (define-key km "+" 'semantic-symref-list-toggle-showing)
    (define-key km "n" 'semantic-symref-list-next-line)
    (define-key km "p" 'semantic-symref-list-prev-line)
    (define-key km "q" 'semantic-symref-hide-buffer)
    km)
  "Keymap used in `semantic-symref-results-mode'.")

(defcustom semantic-symref-results-mode-hook nil
  "*Hook run when `semantic-symref-results-mode' starts."
  :group 'semantic-symref
  :type 'hook)

(defvar semantic-symref-current-results nil
  "The current results in a results mode buffer.")

;;;###autoload
(defun semantic-symref-results-mode (results)
  "Major-mode for displaying Semantic Symbol Reference RESULTS.
RESULTS is an object of class `semantic-symref-results'."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'semantic-symref-results-mode
        mode-name "Symref"
	)
  (use-local-map semantic-symref-results-mode-map)
  (set (make-local-variable 'semantic-symref-current-results)
       results)
  (semantic-symref-results-dump results)
  (goto-char (point-min))
  (run-hooks 'semantic-symref-results-mode-hook)
  )

(defun semantic-symref-hide-buffer ()
  "Hide buffer with sematinc-symref results"
  (interactive)
  (bury-buffer))

(defcustom semantic-symref-results-summary-function 'semantic-format-tag-prototype
  "*Function to use when creating items in Imenu.
Some useful functions are found in `semantic-format-tag-functions'."
  :group 'semantic-symref
  :type semantic-format-tag-custom-list)

(defun semantic-symref-results-dump (results)
  "Dump the RESULTS into the current buffer."
  ;; Get ready for the insert.
  (toggle-read-only -1)
  (erase-buffer)
  
  ;; Insert the contents.
  (let ((lastfile nil)
	)
    (dolist (T (oref results :hit-tags))

      (when (not (equal lastfile (semantic-tag-file-name T)))
	(setq lastfile (semantic-tag-file-name T))
	(insert-button lastfile
		       'mouse-face 'custom-button-pressed-face
		       'action 'semantic-symref-rb-goto-file
		       'tag T
		       )
	(insert "\n"))

      (insert "  ")
      (insert-button "[+]"
		     'mouse-face 'highlight
		     'face nil
		     'action 'semantic-symref-rb-toggle-expand-tag
		     'tag T
		     'state 'closed)
      (insert " ")
      (insert-button (funcall semantic-symref-results-summary-function
			      T nil t)
		     'mouse-face 'custom-button-pressed-face
		     'face nil
		     'action 'semantic-symref-rb-goto-tag
		     'tag T)
      (insert "\n")

      ))

  ;; Clean up the mess
  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  )

;;; Commands for semantic-symref-results
;;
(defun semantic-symref-list-toggle-showing ()
  "Toggle showing the contents below the current line."
  (interactive)
  (beginning-of-line)
  (when (re-search-forward "\\[[-+]\\]" (point-at-eol) t)
    (forward-char -1)
    (push-button)))

(defun semantic-symref-rb-toggle-expand-tag (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (interactive)
  (let* ((tag (button-get button 'tag))
	 (buff (semantic-tag-buffer tag))
	 (hits (semantic--tag-get-property tag :hit))
	 (state (button-get button 'state))
	 (text nil)
	 )
    (cond
     ((eq state 'closed)
      (toggle-read-only -1)
      (save-excursion
	(set-buffer buff)
	(dolist (H hits)
	  (goto-char (point-min))
	  (forward-line (1- H))
	  (beginning-of-line)
	  (back-to-indentation)
	  (setq text (cons (buffer-substring (point) (point-at-eol)) text)))
	(setq text (nreverse text))
	)
      (goto-char (button-start button))
      (forward-char 1)
      (delete-char 1)
      (insert "-")
      (button-put button 'state 'open)
      (save-excursion
	(end-of-line)
	(while text
	(insert "\n")
	  (insert "    ")
	  (insert-button (car text)
			 'mouse-face 'highlight
			 'face nil
			 'action 'semantic-symref-rb-goto-match
			 'tag tag
			 'line (car hits))
	  (setq text (cdr text)
		hits (cdr hits))))
      (toggle-read-only 1)
      )
     ((eq state 'open)
      (toggle-read-only -1)
      (button-put button 'state 'closed)
      ;; Delete the various bits.
      (goto-char (button-start button))
      (forward-char 1)
      (delete-char 1)
      (insert "+")
      (save-excursion
	(end-of-line)
	(forward-char 1)
	(delete-region (point)
		       (save-excursion
			 (forward-char 1)
			 (forward-line (length hits))
			 (point))))
      (toggle-read-only 1)
      )
     ))
  )

(defun semantic-symref-rb-goto-file (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (let* ((tag (button-get button 'tag))
	 (buff (semantic-tag-buffer tag))
	 (win (selected-window))
	 )
    (switch-to-buffer-other-window buff)
    (pulse-momentary-highlight-one-line (point))
    (when (eq last-command-char ? ) (select-window win))
    ))


(defun semantic-symref-rb-goto-tag (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (interactive)
  (let* ((tag (button-get button 'tag))
	 (buff (semantic-tag-buffer tag))
	 (win (selected-window))
	 )
    (switch-to-buffer-other-window buff)
    (semantic-go-to-tag tag)
    (pulse-momentary-highlight-one-line (point))
    (when (eq last-command-char ? ) (select-window win))
    )
  )

(defun semantic-symref-rb-goto-match (&optional button)
  "Go to the file specified in the symref results buffer.
BUTTON is the button that was clicked."
  (interactive)
  (let* ((tag (button-get button 'tag))
	 (line (button-get button 'line))
	 (buff (semantic-tag-buffer tag))
	 (win (selected-window))
	 )
    (switch-to-buffer-other-window buff)
    (goto-line line)
    (pulse-momentary-highlight-one-line (point))
    (when (eq last-command-char ? ) (select-window win))
    )
  )

(defun semantic-symref-list-next-line ()
  "Next line in `semantic-symref-results-mode'."
  (interactive)
  (forward-line 1)
  (back-to-indentation))

(defun semantic-symref-list-prev-line ()
  "Next line in `semantic-symref-results-mode'."
  (interactive)
  (forward-line -1)
  (back-to-indentation))

(provide 'semantic-symref-list)
;;; semantic-symref-list.el ends here

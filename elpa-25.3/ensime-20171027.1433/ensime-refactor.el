;;; ensime-refactor.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'diff-mode)

(defvar ensime-refactor-id-counter 0
  "Each refactoring is given a unique id.")

(defvar ensime-refactor-info-buffer-name "*ENSIME-Refactoring*")

(defvar ensime-refactor-info-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") (lambda()(interactive)
                               (funcall continue-refactor)
                               (ensime-popup-buffer-quit-function)
                               ))
    (define-key map (kbd "q") (lambda()(interactive)
                               (funcall cancel-refactor)
                               (ensime-popup-buffer-quit-function)
                               ))
   map)
  "Key bindings for the refactor confirmation popup.")

(defun ensime-refactor-organize-java-imports ()
  "Sort all import statements lexicographically and delete the duplicate imports."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^\\s-*package\\s-" nil t)
    (goto-char (point-at-eol))
    (let ((beg (point)) end)
      ;; Advance past all imports
      (while (looking-at "[\n\t ]*import\\s-\\(.+\\)\n")
        (search-forward-regexp "import" nil t)
        (goto-char (point-at-eol)))
      (setq end (point))
      (sort-lines nil beg end)
      (delete-duplicate-lines beg end nil t))))

(defun ensime-refactor-diff-rename (&optional new-name)
  "Rename a symbol, project-wide."
  (interactive)
  (let ((sym (ensime-sym-at-point)))
    (if sym
        (let* ((start (plist-get sym :start))
               (end (plist-get sym :end))
               (old-name (plist-get sym :name))
               (name (or new-name
                         (read-string (format "Rename '%s' to: " old-name)))))
          (ensime-refactor-diff
           'rename
           `(file ,buffer-file-name
                  start ,(ensime-externalize-offset start)
                  end ,(ensime-externalize-offset end)
                  newName ,name)))
      (message "Please place cursor on a symbol."))))

(defun ensime-refactor-diff-organize-imports ()
  "Do a syntactic organization of the imports in the current buffer."
  (interactive)
  (cond ((ensime-visiting-java-file-p)
         (ensime-refactor-organize-java-imports)
         (message "Organized."))
        (t
         (ensime-refactor-diff
          'organizeImports
          `(file ,buffer-file-name)))))

(defun ensime-refactor-diff-extract-local ()
  "Extract a range of code into a val."
  (interactive)
  (let ((name (read-string "Name of local value: ")))
    (destructuring-bind (start end)
        (ensime-computed-range)
      (ensime-refactor-diff
       'extractLocal
       `(file ,buffer-file-name
              start ,start
              end ,end
              name ,name)))))

(defun ensime-refactor-diff-extract-method ()
  "Extract a range of code into a method."
  (interactive)
  (let ((name (read-string "Name of method: ")))
    (destructuring-bind (start end)
        (ensime-computed-range)
      (ensime-refactor-diff
       'extractMethod
       `(file ,buffer-file-name
              start ,start
              end ,end
              methodName ,name)))))

(defun ensime-refactor-diff-inline-local ()
  "Get rid of an intermediate variable."
  (interactive)
  (let ((sym (ensime-sym-at-point)))
    (if sym
        (let* ((start (plist-get sym :start))
               (end (plist-get sym :end)))
          (ensime-refactor-diff
           'inlineLocal
           `(file ,buffer-file-name
                  start ,(ensime-externalize-offset start)
                  end ,(ensime-externalize-offset end))))
      (message "Please place cursor on a local value."))))

(defun ensime-refactor-expand-match-cases ()
  "Expand the cases for a match block on a sealed trait, case class or case object."
  (interactive)
  (destructuring-bind (start end)
      (ensime-computed-range)
    (ensime-refactor-diff
     'expandMatchCases
     `(file ,buffer-file-name
            start ,start
            end ,end
            tpe "expandMatchCases"
            ))))

(defun ensime-refactor-diff (refactor-type params &optional non-interactive blocking)
  (if (buffer-modified-p) (ensime-write-buffer nil t))
  (incf ensime-refactor-id-counter)
  (if (not blocking) (message "Please wait..."))
  (ensime-rpc-refactor-diff
   ensime-refactor-id-counter
   params
   non-interactive
   'ensime-refactor-diff-handler
   blocking))

(defun ensime-refactor-diff-handler (result)
  (let ((refactor-type (plist-get result :refactor-type))
        (id (plist-get result :procedure-id))
        (diff (plist-get result :diff)))
    (if ensime-refactor-preview
        (pcase (list
                (ensime--refactor-diff-preview-override-types-p refactor-type)
                (ensime--refactor-diff-preview-override-file-p diff)
                (ensime--refactor-diff-preview-override-hunk-p diff))
          (`(nil nil nil)   (ensime-refactor-diff-preview-popup diff))
          (_                (ensime-refactor-diff-apply-silently diff)))
      (pcase (list
              (ensime--refactor-diff-no-preview-override-types-p refactor-type)
              (ensime--refactor-diff-no-preview-override-file-p diff)
              (ensime--refactor-diff-no-preview-override-hunk-p diff))
        (`(nil nil nil)   (ensime-refactor-diff-apply-silently diff))
        (_                (ensime-refactor-diff-preview-popup diff))))
    (delete-file diff)
    (ensime-event-sig :refactor-diff-done diff)))

(defun ensime-refactor-diff-preview-popup (diff)
  (ensime-with-popup-buffer (ensime-refactor-info-buffer-name
                             nil t 'diff-mode)
                            (insert-file-contents diff)
                            (ensime-refactor-diff-buffer-local-key)))

(defun ensime-refactor-diff-preview-apply-popup (diff)
  (ensime-with-popup-buffer (ensime-refactor-info-buffer-name
                             nil nil 'diff-mode)
                            (insert-file-contents diff)
                            (ensime-refactor-diff-apply-hunks)
                            (ensime-refactor-diff-save-source-files)))

(defun ensime-refactor-diff-apply-silently (diff)
  (with-temp-buffer
    (insert-file-contents diff)
    (ensime-refactor-diff-apply-hunks)
    (ensime-refactor-diff-save-source-files)))

(defun ensime--refactor-diff-preview-override-types-p (refactor-type)
  (memq refactor-type ensime-refactor-preview-override-types))

(defun ensime--refactor-diff-preview-override-file-p (diff)
  (with-temp-buffer
    (insert-file-contents diff)
    (goto-char (point-min))
    (not (re-search-forward diff-file-header-re nil t
                            (+ 1 ensime-refactor-preview-override-file)))))

(defun ensime--refactor-diff-preview-override-hunk-p (diff)
  (with-temp-buffer
    (insert-file-contents diff)
    (goto-char (point-min))
    (not (re-search-forward diff-hunk-header-re nil t
                            (+ 1 ensime-refactor-preview-override-hunk)))))

(defun ensime--refactor-diff-no-preview-override-types-p (refactor-type)
  (memq refactor-type ensime-refactor-no-preview-override-types))

(defun ensime--refactor-diff-no-preview-override-file-p (diff)
  (with-temp-buffer
    (insert-file-contents diff)
    (goto-char (point-min))
    (not (re-search-forward diff-file-header-re nil t
                            (+ 1 ensime-refactor-no-preview-override-file)))))

(defun ensime--refactor-diff-no-preview-override-hunk-p (diff)
  (with-temp-buffer
    (insert-file-contents diff)
    (goto-char (point-min))
    (not (re-search-forward diff-hunk-header-re nil t
                            (+ 1 ensime-refactor-no-preview-override-hunk)))))


(defun ensime-refactor-diff-apply-hunks ()
  "Apply or undo all hunks in the diff contents of the current buffer."
  (interactive)
  (make-local-variable 'diff-advance-after-apply-hunk)
  (setq diff-advance-after-apply-hunk nil)
  (goto-char (point-min))
  (while (re-search-forward diff-hunk-header-re nil t)
    (diff-apply-hunk)))

(defun ensime-refactor-diff-save-source-files ()
  "Save all source files from the diff contents of the current buffer.
Do not asks user about each one if `ensime-refactor-save-with-no-questions' is non-nil."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward diff-file-header-re nil t)
    (-when-let (src-buffer-name (buffer-name (car (diff-find-source-location))))
      (save-some-buffers
       ensime-refactor-save-with-no-questions
       (-partial (lambda (src-buffer-name)
                   (equal src-buffer-name (buffer-name)))
                 src-buffer-name)))))

(defun ensime-refactor-add-type-annotation ()
  "Add type annotation to current symbol."
  (interactive)
  (let* ((type (ensime-rpc-get-type-at-point))
         (shortname (ensime-type-name-with-args type)))
    (save-excursion
      (forward-word)
      (while (let ((current-char (thing-at-point 'char)))
               (or (equal "(" current-char) (equal "[" current-char)))
        (forward-list))
      (insert (concat ": " shortname)))))

(defun ensime-refactor-diff-buffer-local-key ()
  "Define a buffer local key in a copy of `diff-mode-map'"
  (use-local-map (copy-keymap diff-mode-map))
  (local-set-key (kbd "a")
                 (lambda ()
                   (interactive)
                   (ensime-refactor-diff-apply-hunks)
                   (ensime-refactor-diff-save-source-files))))

(provide 'ensime-refactor)

;; Local Variables:
;; End:



(define-slime-contrib slime-compiler-notes-tree
  "Display compiler messages in tree layout.

M-x slime-list-compiler-notes display the compiler notes in a tree
grouped by severity.

  `slime-maybe-list-compiler-notes' can be used as
  `slime-compilation-finished-hook'.
"
  (:authors "Helmut Eller <heller@common-lisp.net>")
  (:license "GPL"))

(defun slime-maybe-list-compiler-notes (notes)
  "Show the compiler notes if appropriate."
  ;; don't pop up a buffer if all notes are already annotated in the
  ;; buffer itself
  (unless (every #'slime-note-has-location-p notes)
    (slime-list-compiler-notes notes)))

(defun slime-list-compiler-notes (notes)
  "Show the compiler notes NOTES in tree view."
  (interactive (list (slime-compiler-notes)))
  (with-temp-message "Preparing compiler note tree..."
    (slime-with-popup-buffer ((slime-buffer-name :notes)
                              :mode 'slime-compiler-notes-mode)
      (when (null notes)
        (insert "[no notes]"))
      (let ((collapsed-p))
        (dolist (tree (slime-compiler-notes-to-tree notes))
          (when (slime-tree.collapsed-p tree) (setf collapsed-p t))
          (slime-tree-insert tree "")
          (insert "\n"))
        (goto-char (point-min))))))

(defvar slime-tree-printer 'slime-tree-default-printer)

(defun slime-tree-for-note (note)
  (make-slime-tree :item (slime-note.message note)
                   :plist (list 'note note)
                   :print-fn slime-tree-printer))

(defun slime-tree-for-severity (severity notes collapsed-p)
  (make-slime-tree :item (format "%s (%d)" 
                                 (slime-severity-label severity)
                                 (length notes))
                   :kids (mapcar #'slime-tree-for-note notes)
                   :collapsed-p collapsed-p))

(defun slime-compiler-notes-to-tree (notes)
  (let* ((alist (slime-alistify notes #'slime-note.severity #'eq))
         (collapsed-p (slime-length> alist 1)))
    (loop for (severity . notes) in alist
          collect (slime-tree-for-severity severity notes 
                                           collapsed-p))))

(defvar slime-compiler-notes-mode-map)

(define-derived-mode slime-compiler-notes-mode fundamental-mode 
  "Compiler-Notes"
  "\\<slime-compiler-notes-mode-map>\
\\{slime-compiler-notes-mode-map}
\\{slime-popup-buffer-mode-map}
"
  (slime-set-truncate-lines))

(slime-define-keys slime-compiler-notes-mode-map
  ((kbd "RET") 'slime-compiler-notes-default-action-or-show-details)
  ([return] 'slime-compiler-notes-default-action-or-show-details)
  ([mouse-2] 'slime-compiler-notes-default-action-or-show-details/mouse))

(defun slime-compiler-notes-default-action-or-show-details/mouse (event)
  "Invoke the action pointed at by the mouse, or show details."
  (interactive "e")
  (destructuring-bind (mouse-2 (w pos &rest _) &rest __) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 
                                   'slime-compiler-notes-default-action)))
	(if fn (funcall fn) (slime-compiler-notes-show-details))))))

(defun slime-compiler-notes-default-action-or-show-details ()
  "Invoke the action at point, or show details."
  (interactive)
  (let ((fn (get-text-property (point) 'slime-compiler-notes-default-action)))
    (if fn (funcall fn) (slime-compiler-notes-show-details))))

(defun slime-compiler-notes-show-details ()
  (interactive)
  (let* ((tree (slime-tree-at-point))
         (note (plist-get (slime-tree.plist tree) 'note))
         (inhibit-read-only t))
    (cond ((not (slime-tree-leaf-p tree))
           (slime-tree-toggle tree))
          (t
           (slime-show-source-location (slime-note.location note) t)))))


;;;;;; Tree Widget

(defstruct (slime-tree (:conc-name slime-tree.))
  item
  (print-fn #'slime-tree-default-printer :type function)
  (kids '() :type list)
  (collapsed-p t :type boolean)
  (prefix "" :type string)
  (start-mark nil)
  (end-mark nil)
  (plist '() :type list))

(defun slime-tree-leaf-p (tree)
  (not (slime-tree.kids tree)))

(defun slime-tree-default-printer (tree)
  (princ (slime-tree.item tree) (current-buffer)))

(defun slime-tree-decoration (tree)
  (cond ((slime-tree-leaf-p tree) "-- ")
	((slime-tree.collapsed-p tree) "[+] ")
	(t "-+  ")))

(defun slime-tree-insert-list (list prefix)
  "Insert a list of trees."
  (loop for (elt . rest) on list 
	do (cond (rest
		  (insert prefix " |")
		  (slime-tree-insert elt (concat prefix " |"))
                  (insert "\n"))
		 (t
		  (insert prefix " `")
		  (slime-tree-insert elt (concat prefix "  "))))))

(defun slime-tree-insert-decoration (tree)
  (insert (slime-tree-decoration tree)))

(defun slime-tree-indent-item (start end prefix)
  "Insert PREFIX at the beginning of each but the first line.
This is used for labels spanning multiple lines."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (< start (point))
      (insert-before-markers prefix)
      (forward-line -1))))

(defun slime-tree-insert (tree prefix)
  "Insert TREE prefixed with PREFIX at point."
  (with-struct (slime-tree. print-fn kids collapsed-p start-mark end-mark) tree
    (let ((line-start (line-beginning-position)))
      (setf start-mark (point-marker))
      (slime-tree-insert-decoration tree)
      (funcall print-fn tree)
      (slime-tree-indent-item start-mark (point) (concat prefix "   "))
      (add-text-properties line-start (point) (list 'slime-tree tree))
      (set-marker-insertion-type start-mark t)
      (when (and kids (not collapsed-p))
        (terpri (current-buffer))
        (slime-tree-insert-list kids prefix))
      (setf (slime-tree.prefix tree) prefix)
      (setf end-mark (point-marker)))))

(defun slime-tree-at-point ()
  (cond ((get-text-property (point) 'slime-tree))
        (t (error "No tree at point"))))

(defun slime-tree-delete (tree)
  "Delete the region for TREE."
  (delete-region (slime-tree.start-mark tree)
                 (slime-tree.end-mark tree)))

(defun slime-tree-toggle (tree)
  "Toggle the visibility of TREE's children."
  (with-struct (slime-tree. collapsed-p start-mark end-mark prefix) tree
    (setf collapsed-p (not collapsed-p))
    (slime-tree-delete tree)
    (insert-before-markers " ") ; move parent's end-mark
    (backward-char 1)
    (slime-tree-insert tree prefix)
    (delete-char 1)
    (goto-char start-mark)))

(provide 'slime-compiler-notes-tree)

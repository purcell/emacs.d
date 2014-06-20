
(define-slime-contrib slime-references
  "Clickable references to documentation (SBCL only)."
  (:authors "Christophe Rhodes  <csr21@cantab.net>"
            "Luke Gorrie  <luke@bluetail.com>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:on-load
   (ad-enable-advice 'slime-note.message 'after 'slime-note.message+references)
   (ad-activate 'slime-note.message)
   (setq slime-tree-printer 'slime-tree-print-with-references)
   (add-hook 'sldb-extras-hooks 'sldb-maybe-insert-references))
  (:on-unload
   (ad-disable-advice 'slime-note.message 'after 'slime-note.message+references)
   (ad-deactivate 'slime-note.message)
   (setq slime-tree-printer 'slime-tree-default-printer)
   (remove-hook 'sldb-extras-hooks 'sldb-maybe-insert-references)))

(defcustom slime-sbcl-manual-root "http://www.sbcl.org/manual/"
  "*The base URL of the SBCL manual, for documentation lookup."
  :type 'string
  :group 'slime-mode)

(defface sldb-reference-face 
  (list (list t '(:underline t)))
  "Face for references."
  :group 'slime-debugger)


;;;;; SBCL-style references 

(defvar slime-references-local-keymap
  (let ((map (make-sparse-keymap "local keymap for slime references")))
    (define-key map [mouse-2] 'slime-lookup-reference-at-mouse)
    (define-key map [return] 'slime-lookup-reference-at-point)
    map))

(defun slime-reference-properties (reference)
  "Return the properties for a reference.
Only add clickability to properties we actually know how to lookup."
  (destructuring-bind (where type what) reference
    (if (or (and (eq where :sbcl) (eq type :node))
            (and (eq where :ansi-cl)
                 (memq type '(:function :special-operator :macro
			      :section :glossary :issue))))
        `(slime-reference ,reference
          font-lock-face sldb-reference-face
          follow-link t
          mouse-face highlight
          help-echo "mouse-2: visit documentation."
          keymap ,slime-references-local-keymap))))

(defun slime-insert-reference (reference)
  "Insert documentation reference from a condition.
See SWANK-BACKEND:CONDITION-REFERENCES for the datatype."
  (destructuring-bind (where type what) reference
    (insert "\n" (slime-format-reference-source where) ", ")
    (slime-insert-propertized (slime-reference-properties reference)
                              (slime-format-reference-node what))
    (insert (format " [%s]" type))))

(defun slime-insert-references (references)
  (when references
    (insert "\nSee also:")
    (slime-with-rigid-indentation 2
      (mapc #'slime-insert-reference references))))

(defun slime-format-reference-source (where)
  (case where
    (:amop    "The Art of the Metaobject Protocol")
    (:ansi-cl "Common Lisp Hyperspec")
    (:sbcl    "SBCL Manual")
    (t        (format "%S" where))))

(defun slime-format-reference-node (what)
  (if (listp what)
      (mapconcat #'prin1-to-string what ".")
    what))

(defun slime-lookup-reference-at-point ()
  "Browse the documentation reference at point."
  (interactive)
  (let ((refs (get-text-property (point) 'slime-reference)))
    (if (null refs)
        (error "No references at point")
        (destructuring-bind (where type what) refs
          (case where
            (:ansi-cl
               (case type
                 (:section
                    (browse-url (funcall common-lisp-hyperspec-section-fun what)))
                 (:glossary
                    (browse-url (funcall common-lisp-glossary-fun what)))
                 (:issue
                    (browse-url (funcall 'common-lisp-issuex what)))
                 (t
                    (hyperspec-lookup what))))
            (t
               (let ((url (format "%s%s.html" slime-sbcl-manual-root
                                  (subst-char-in-string ?\  ?\- what))))
                 (browse-url url))))))))

(defun slime-lookup-reference-at-mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (destructuring-bind (mouse-1 (w pos . _) . _) event
    (save-excursion
      (goto-char pos)
      (slime-lookup-reference-at-point))))

;;;;; Hook into *SLIME COMPILATION*

(defun slime-note.references (note)
  (plist-get note :references))

;;; FIXME: `compilation-mode' will swallow the `mouse-face'
;;; etc. properties.
(defadvice slime-note.message (after slime-note.message+references)
  (setq ad-return-value 
        (concat ad-return-value
                (with-temp-buffer
                  (slime-insert-references 
                   (slime-note.references (ad-get-arg 0)))
                  (buffer-string)))))

;;;;; Hook into slime-compiler-notes-tree

(defun slime-tree-print-with-references (tree)
  ;; for SBCL-style references
  (slime-tree-default-printer tree)
  (when-let (note (plist-get (slime-tree.plist tree) 'note))
    (when-let (references (slime-note.references note))
      (terpri (current-buffer))
      (slime-insert-references references))))

;;;;; Hook into SLDB

(defun sldb-maybe-insert-references (extra)
  (destructure-case extra
    ((:references references) (slime-insert-references references) t)
    (t nil)))

(provide 'slime-references)
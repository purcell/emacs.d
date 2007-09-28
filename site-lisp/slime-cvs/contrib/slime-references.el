;;; slime-references.el --- Clickable references to documentation (SBCL only)
;;
;; Authors: Christophe Rhodes  <csr21@cantab.net>
;;          Luke Gorrie  <luke@bluetail.com>
;;
;; License: GNU GPL (same license as Emacs)
;;
;;;

(defcustom slime-sbcl-manual-root "http://www.sbcl.org/manual/"
  "*The base URL of the SBCL manual, for documentation lookup."
  :type 'string
  :group 'slime-mode)

(defface sldb-reference-face 
  (list (list t '(:underline t)))
  "Face for references."
  :group 'slime-debugger)

(defun slime-note.references (note)
  (plist-get note :references))

(defun slime-tree-print-with-references (tree)
  ;; for SBCL-style references
  (slime-tree-default-printer tree)
  (when-let (note (plist-get (slime-tree.plist tree) 'note))
    (when-let (references (slime-note.references note))
      (terpri (current-buffer))
      (princ "See also:" (current-buffer))
      (terpri (current-buffer))
      (slime-tree-insert-references references))))

(defun slime-tree-insert-references (references)
  "Insert documentation references from a condition.
See SWANK-BACKEND:CONDITION-REFERENCES for the datatype."
  (loop for refs on references
        for ref = (car refs)
        do
        (destructuring-bind (where type what) ref
          ;; FIXME: this is poorly factored, and shares some code and
          ;; data with sldb that it shouldn't: notably
          ;; sldb-reference-face.  Probably the names of
          ;; sldb-reference-foo should be altered to be not sldb
          ;; specific.
          (insert "  " (sldb-format-reference-source where) ", ")
          (slime-insert-propertized (sldb-reference-properties ref)
                                    (sldb-format-reference-node what))
          (insert (format " [%s]" type))
          (when (cdr refs)
            (terpri (current-buffer))))))


;;;;; SLDB references (rather SBCL specific)

(defun sldb-insert-references (references)
  "Insert documentation references from a condition.
See SWANK-BACKEND:CONDITION-REFERENCES for the datatype."
  (dolist (ref references)
    (destructuring-bind (where type what) ref
      (insert "\n" (sldb-format-reference-source where) ", ")
      (slime-insert-propertized (sldb-reference-properties ref)
				(sldb-format-reference-node what))
      (insert (format " [%s]" type)))))

(defun sldb-reference-properties (reference)
  "Return the properties for a reference.
Only add clickability to properties we actually know how to lookup."
  (destructuring-bind (where type what) reference
    (if (or (and (eq where :sbcl) (eq type :node))
            (and (eq where :ansi-cl)
                 (memq type '(:function :special-operator :macro
			      :section :glossary :issue))))
        `(sldb-default-action
          sldb-lookup-reference
          ;; FIXME: this is a hack!  slime-compiler-notes and sldb are a
          ;; little too intimately entwined.
          slime-compiler-notes-default-action sldb-lookup-reference
          sldb-reference ,reference
          face sldb-reference-face
          mouse-face highlight))))

(defun sldb-format-reference-source (where)
  (case where
    (:amop    "The Art of the Metaobject Protocol")
    (:ansi-cl "Common Lisp Hyperspec")
    (:sbcl    "SBCL Manual")
    (t        (format "%S" where))))

(defun sldb-format-reference-node (what)
  (if (listp what)
      (mapconcat #'prin1-to-string what ".")
    what))

(defun sldb-lookup-reference ()
  "Browse the documentation reference at point."
  (destructuring-bind (where type what)
      (get-text-property (point) 'sldb-reference)
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
         (browse-url url))))))

(defun sldb-maybe-insert-references (extra)
  (destructure-case extra
    ((:references references)
     (when references
       (insert "\nSee also:")
       (slime-with-rigid-indentation 2
	 (sldb-insert-references references)))
     t)
    (t nil)))


;;; Initialization

(defun slime-references-init ()
  (setq slime-tree-printer 'slime-tree-print-with-references)
  (add-hook 'sldb-extras-hooks 'sldb-maybe-insert-references))

(defun slime-references-unload ()
  (setq slime-tree-printer 'slime-tree-default-printer)
  (remove-hook 'sldb-extras-hooks 'sldb-maybe-insert-references))
  
(provide 'slime-references)

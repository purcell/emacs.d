;;; ensime-model.el --- Data-structure accessors

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 's)
(require 'dash)

;; these accessors are all redundant in light of -let or
;; ensime-plist-bind
;;
;; also, we should consider creating a `defclass' mirror of the
;; ensime-server API https://github.com/ensime/ensime-emacs/issues/452
(defun ensime-search-sym-name (sym)
  (plist-get sym :name))

(defun ensime-search-sym-local-name (sym)
  (plist-get sym :local-name))

(defun ensime-search-sym-pos (sym)
  (plist-get sym :pos))

(defun ensime-search-sym-owner-name (sym)
  (plist-get sym :owner-name))

(defun ensime-search-sym-decl-as (sym)
  (plist-get sym :decl-as))

(defun ensime-symbol-decl-pos (sym)
  (plist-get sym :decl-pos))

(defun ensime-symbol-type (sym)
  (plist-get sym :type))

(defun ensime-package-name (info)
  (plist-get info :name))

(defun ensime-package-full-name (info)
  (plist-get info :full-name))

(defun ensime-package-members (info)
  (plist-get info :members))

(defun ensime-package-p (info)
  (equal 'package (plist-get info :info-type)))

(defun ensime-type-name (type)
  ;; legacy method
  (replace-regexp-in-string "\\[.*" ""
                            (plist-get type :name)))

(defun ensime-type-full-name (type)
  ;; legacy method
  (replace-regexp-in-string "\\[.*" ""
                            (plist-get type :full-name)))

(defun ensime-type-is-object-p (type)
  (equal (plist-get type :decl-as) 'object))

(defun ensime-type-full-name-with-args (type)
  (plist-get type :full-name))

(defun ensime-type-param-sections (type)
  (plist-get type :param-sections))

(defun ensime-type-name-with-args (type)
  (plist-get type :name))

(defun ensime-type-is-by-name-p (type)
  ;; These two patterns should match both the old representation of by-name parameters
  ;; from ensime and the new (standard) representation
  (string-match "\\(^scala.<byname>\\|^=>\s+\\)" (plist-get type :full-name)))

(defun ensime-declared-as (obj)
  (plist-get obj :decl-as))

(defun ensime-declared-as-str (obj)
  (case (plist-get obj :decl-as)
    (method "method")
    (trait "trait")
    (interface "interface")
    (class "class")
    (object "object")
    (otherwise "type")
    ))

(defun ensime-type-is-arrow-p (type)
  (plist-get type :arrow-type))

(defun ensime-type-param-types (type)
  "Return types of params in first section."
  (let ((section (car (plist-get type :param-sections))))
    (mapcar
     (lambda (p)
       (cadr p))
     (plist-get section :params)
     )))

(defun ensime-param-section-accepts-block-p (section)
  "Returns t if `SECTION' has a single functional parameter."
  (let* ((params (plist-get section :params))
         (arg-type (cadar params)))
    (and (= 1 (length params))
         (or
          (plist-get arg-type :arrow-type)
          (ensime-type-is-by-name-p arg-type)))))

(defun ensime-type-result-type (type)
  (plist-get type :result-type))

(defun ensime-type-type-args (type)
  (plist-get type :type-args))

(defun ensime-member-name (member)
  (plist-get member :name))

(defun ensime-member-type (member)
  (plist-get member :type))

(defun ensime-member-signature (member)
  (plist-get member :signature-string))

(defun ensime-member-pos (member)
  (plist-get member :pos))

(defun ensime-pos-file (pos)
  (plist-get pos :file))

(defun ensime-pos-archive (pos)
  (plist-get pos :archive))

(defun ensime-pos-effective-file (pos)
  (if (plist-get pos :archive)
      (concat
       (ensime-source-jars-dir)
       (file-name-as-directory (file-name-nondirectory (plist-get pos :archive)))
       (plist-get pos :file))
    (plist-get pos :file)))

(defun ensime-pos-offset (pos)
  (plist-get pos :offset))

(defun ensime-pos-line (pos)
  (plist-get pos :line))

(defun ensime-pos-available-p (pos)
  (or (ensime-pos-valid-local-p pos)
      (eq pos t)))

(defun ensime-pos-valid-local-p (pos)
  (and (stringp (ensime-pos-file pos))
       (or (file-exists-p (ensime-pos-file pos))
           (and (stringp (ensime-pos-archive pos))
                (file-exists-p (ensime-pos-archive pos))))
       (or (integerp (ensime-pos-line pos))
           (integerp (ensime-pos-offset pos)))))

(defun ensime-note-file (note)
  (plist-get note :file))

(defun ensime-note-beg (note)
  (plist-get note :beg))

(defun ensime-note-end (note)
  (plist-get note :end))

(defun ensime-note-line (note)
  (plist-get note :line))

(defun ensime-note-message (note)
  (plist-get note :msg))

(defun ensime-type-ancestors (type)
  (plist-get type :ancestors))

(defun ensime-type-inheritors (type)
  (plist-get type :inheritors))

(defun ensime-type-fqn (type)
  (plist-get type :fqn))

(defun ensime-type-source-position (type)
  (plist-get type :source-position))

(defun ensime-source-hint-position (hint)
  (plist-get hint :position))

(defun ensime-preview (hint)
  (plist-get hint :preview))



(provide 'ensime-model)

;; Local Variables:
;; End:

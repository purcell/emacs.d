;;; scala-mode-imenu.el - Major mode for editing scala
;;; Copyright (c) 2014 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

;;; Code:

(require 'scala-mode-syntax)

;; Make lambdas proper clousures (only in this file)
(make-local-variable 'lexical-binding)
(setq lexical-binding t)

(defcustom scala-imenu:should-flatten-index t
  "Controls whether or not the imenu index is flattened or hierarchical.")
(defcustom scala-imenu:build-imenu-candidate
  'scala-imenu:default-build-imenu-candidate
  "Controls whether or not the imenu index has definition type information.")
(defcustom scala-imenu:cleanup-hooks nil
  "Functions that will be run after the construction of each imenu")

(defun scala-imenu:flatten-list (incoming-list &optional predicate)
  (when (not predicate) (setq predicate 'listp))
  (cl-mapcan (lambda (x) (if (funcall predicate x)
			  (scala-imenu:flatten-list x predicate) (list x))) incoming-list))

(defun scala-imenu:flatten-imenu-index (index)
  (cl-mapcan (lambda (x) (if (listp (cdr x))
			  (scala-imenu:flatten-imenu-index (cdr x))
			(list x))) index))

(defun scala-imenu:create-imenu-index ()
  (let ((imenu-index (cl-mapcar 'scala-imenu:build-imenu-candidates
			     (scala-imenu:create-index))))
    (dolist (cleanup-hook scala-imenu:cleanup-hooks)
      (funcall cleanup-hook))
    (if scala-imenu:should-flatten-index
	(scala-imenu:flatten-imenu-index imenu-index)
      imenu-index)))

(defun scala-imenu:build-imenu-candidates (member-info &optional parents)
  (if (listp (car member-info))
      (let* ((current-member-info (car member-info))
	     (child-member-infos (cdr member-info))
	     (current-member-result
	      (scala-imenu:destructure-for-build-imenu-candidate
	       current-member-info parents))
	     (current-member-name (car current-member-result)))
	(if child-member-infos
	    (let ((current-member-members
		   (scala-imenu:build-child-members
		    (append parents `(,current-member-info))
		    (cdr member-info))))
	      `(,current-member-name .
	        ,(cons current-member-result current-member-members)))
	  current-member-result))
    (scala-imenu:destructure-for-build-imenu-candidate member-info parents)))

(defun scala-imenu:build-child-members (parents child-members)
  (cl-mapcar (lambda (child) (scala-imenu:build-imenu-candidates
			   child parents)) child-members))

(defun scala-imenu:destructure-for-build-imenu-candidate (member-info parents)
  (cl-destructuring-bind (member-name definition-type marker)
      member-info (funcall scala-imenu:build-imenu-candidate
			   member-name definition-type marker parents)))


(defun scala-imenu:default-build-imenu-candidate (member-name definition-type
							      marker parents)
  (let* ((all-names
	  (append (cl-mapcar (lambda (parent) (car parent)) parents)
		  `(,member-name)))
	 (member-string (mapconcat 'identity all-names ".")))
    `(,(format "(%s)%s" definition-type member-string) . ,marker)))

(defun scala-imenu:create-index ()
  (let ((class nil) (index nil))
    (goto-char (point-max))
    (while (setq class (scala-imenu:parse-nested-from-end))
      (setq index (cons class index)))
    index))

(defun scala-imenu:parse-nested-from-end ()
  (let ((last-point (point)) (class-name nil) (definition-type nil))
    (scala-syntax:beginning-of-definition)
    ;; We're done if scala-syntax:beginning-of-definition has no effect.
    (if (eq (point) last-point) nil
      (progn (looking-at scala-syntax:all-definition-re)
	     (setq class-name (match-string-no-properties 2))
	     (setq definition-type (match-string-no-properties 1)))
      `(,`(,class-name ,definition-type ,(point-marker)) .
	,(scala-imenu:nested-members)))))

(defun scala-imenu:parse-nested-from-beginning ()
  (scala-syntax:end-of-definition)
  (scala-imenu:parse-nested-from-end))

(defun scala-imenu:nested-members ()
  (let ((start-point (point)))
    (save-excursion
      (scala-syntax:end-of-definition)
      ;; This gets us inside of the class definition
      ;; It seems like there should be a better way
      ;; to do this.
      (backward-char)
      (reverse (scala-imenu:get-nested-members start-point)))))

(defvar scala-imenu:nested-definition-types '("class" "object" "trait"))

(defun scala-imenu:get-nested-members (parent-start-point)
  (scala-syntax:beginning-of-definition)
  (if (< parent-start-point (point))
      (cons (scala-imenu:get-member-info-at-point)
	    (scala-imenu:get-nested-members parent-start-point))
      nil))

(defun scala-imenu:get-member-info-at-point ()
  (looking-at scala-syntax:all-definition-re)
  (let* ((member-name (match-string-no-properties 2))
	 (definition-type (match-string-no-properties 1)))
    (if (member definition-type scala-imenu:nested-definition-types)
	(save-excursion (scala-imenu:parse-nested-from-beginning))
      `(,member-name ,definition-type ,(point-marker)))))


(provide 'scala-mode-imenu)
;;; scala-mode-imenu.el ends here

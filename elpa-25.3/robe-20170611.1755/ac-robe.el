(eval-when-compile (require 'robe))

(defun ac-robe-doc (symbol)
  "Return popup documentation for `auto-complete'."
  (when robe-running
    (let ((spec (car (robe-cached-specs symbol))))
      (when spec
        (concat (robe-signature spec)
                "\n\n"
                (cdr (assoc 'docstring (robe-doc-for spec))))))))

;;;###autoload
(defun ac-robe-available ()
  "Return t if `robe-mode' completions are available, otherwise nil."
  (and (boundp 'robe-mode) robe-mode))

(defun ac-robe-prefix ()
  (let ((bounds (robe-complete-bounds)))
    (when (and bounds (robe-complete-symbol-p (car bounds)))
      (car bounds))))

(defun ac-robe-candidates ()
  "Return completion candidates for `ac-prefix'."
  (require 'robe)
  (when (robe-running-p)
    (let (robe-highlight-capf-candidates)
      (robe-complete-thing ac-prefix))))

;;;###autoload
(defun ac-robe-setup ()
  (push 'ac-source-robe ac-sources))

(define-obsolete-function-alias 'robe-ac-setup 'ac-robe-setup "0.7.8")

;;;###autoload
(defconst ac-source-robe
  '((available . ac-robe-available)
    (prefix . ac-robe-prefix)
    (candidates . ac-robe-candidates)
    (document . ac-robe-doc)
    (symbol . "r"))
  "`auto-complete' completion source for Ruby using `robe-mode'.")

(provide 'ac-robe)

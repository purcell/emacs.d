;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

(require 'eproject)
(declare-function look-for "eproject")

(define-project-type python (generic)
  (or (look-for ".ropeproject")
      (look-for "setup.py"))
  :tasks
  (("build" :shell "python setup.py build")
   ("build_ext" :shell "python setup.py build_ext")
   ("build_ext --inplace" :shell "python setup.py build_ext --inplace")
   ("clean" :shell "python setup.py clean")
   ("install" :shell "python setup.py install" :confirm t)
   ("build document"
    :available eproject-python--find-doc
    :call eproject-python--doc-compile)
   ("tox" :shell "tox"
    :available (lambda () (file-exists-p (expand-file-name "tox.ini"))))))

(defvar eproject-python-doc-dirs '("doc" "docs" "Doc" "Docs"))
(defvar eproject-python-doc-makefiles '("Makefile" "make.py"))

(defun eproject-python--find-doc ()
  (loop named found
        for doc in eproject-python-doc-dirs
        for abs-doc-dir = (expand-file-name (file-name-as-directory doc))
        do (loop for makefile in eproject-python-doc-makefiles
                 when (file-exists-p (concat abs-doc-dir makefile))
                 do (return-from found (list abs-doc-dir makefile)))))

(defun eproject-python--doc-compile ()
  (destructuring-bind (default-directory makefile)
      (eproject-python--find-doc)
    (cond
     ((string-match-p "\\.py$" makefile)
      (let ((compile-command (format "python %s " makefile)))
        (call-interactively 'compile)))
     ((equal makefile "Makefile")
      (let ((compile-command "make html"))
        (call-interactively 'compile)))
     (t (call-interactively 'compile)))))

(provide 'eproject-python)

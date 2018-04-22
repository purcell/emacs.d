;;; ob-mathematica.el --- org-babel functions for Mathematica evaluation

;; Copyright (C) 2014 Yi Wang

;; Authors: Yi Wang
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/tririver/wy-els/blob/master/ob-mathematica.el
;; Distributed under the GNU GPL v2 or later

;; Org-Babel support for evaluating Mathematica source code.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(declare-function org-trim "org" (s &optional keep-lead))

;; Optionally require mma.el for font lock, etc
(require 'mma nil 'noerror)
(add-to-list 'org-src-lang-modes '("mathematica" . "mma"))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("mathematica" . "m"))

(defvar org-babel-default-header-args:mathematica '())

(defvar org-babel-mathematica-command "MathematicaScript -script"
  "Name of the command for executing Mathematica code.")

(defvar org-babel-mathematica-command-alt "math -noprompt"
  "Name of the command for executing Mathematica code.")

(defun org-babel-expand-body:mathematica (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
	(format "%s=%s;"
		(car pair)
		(org-babel-mathematica-var-to-mathematica (cdr pair))))
      vars "\n") "\nPrint[\n" body "\n]\n")))

(defun org-babel-execute:mathematica (body params)
  "Execute a block of Mathematica code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (let* ((result-params (cdr (assq :result-params params)))
	 (full-body (org-babel-expand-body:mathematica body params))
	 (tmp-script-file (org-babel-temp-file "mathematica-"))
	 (cmd org-babel-mathematica-command))
    ;; actually execute the source-code block
    (with-temp-file tmp-script-file (insert full-body))
    ;; (with-temp-file "/tmp/dbg" (insert full-body))
    ((lambda (raw)
       (if (or (member "code" result-params)
	       (member "pp" result-params)
	       (and (member "output" result-params)
		    (not (member "table" result-params))))
	   raw
	 (org-babel-script-escape (org-trim raw))))
    (org-babel-eval (concat cmd " " tmp-script-file) ""))))

(defun org-babel-prep-session:mathematica (session params)
  "This function does nothing so far"
  (error "Currently no support for sessions"))

(defun org-babel-prep-session:mathematica (session body params)
  "This function does nothing so far"
  (error "Currently no support for sessions"))

(defun org-babel-mathematica-var-to-mathematica (var)
  "Convert an elisp value to a Mathematica variable.
Convert an elisp value, VAR, into a string of Mathematica source code
specifying a variable of the same value."
  (if (listp var)
      (concat "{" (mapconcat #'org-babel-mathematica-var-to-mathematica var ", ") "}")
    (format "%S" var)))

(provide 'ob-mathematica)

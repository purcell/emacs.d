(eval-when-compile (require 'robe))

;;;###autoload
(defun company-robe (command &optional arg &rest ignore)
  "A `company-mode' completion back-end for `robe-mode'."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-robe))
    (prefix (and (boundp 'robe-mode)
                 robe-mode (robe-running-p)
                 (company-robe--prefix)))
    (candidates (robe-complete-thing arg))
    (duplicates t)
    (meta (company-robe--meta arg))
    (location (let ((spec (company-robe--choose-spec arg)))
                (cons (robe-spec-file spec)
                      (robe-spec-line spec))))
    (annotation (robe-complete-annotation arg))
    (doc-buffer (let ((spec (company-robe--choose-spec arg)))
                  (when spec
                    (save-window-excursion
                      (robe-show-doc spec)
                      (message nil)
                      (get-buffer "*robe-doc*")))))))

(defun company-robe--meta (completion)
  (or
   (get-text-property 0 'robe-type completion)
   (let ((spec (car (robe-cached-specs completion))))
     (when spec (robe-signature spec)))))

(defun company-robe--prefix ()
  (let ((bounds (robe-complete-bounds)))
    (when (and bounds
               (equal (point) (cdr bounds))
               (robe-complete-symbol-p (car bounds)))
      (buffer-substring (car bounds) (cdr bounds)))))

(defun company-robe--choose-spec (thing)
  (let ((specs (robe-cached-specs thing)))
    (when specs
      (if (cdr specs)
          (let ((alist (cl-loop for spec in specs
                             for module = (robe-spec-module spec)
                             when module
                             collect (cons module spec))))
            (cdr (assoc (robe-completing-read "Module: " alist nil t) alist)))
        (car specs)))))

(provide 'company-robe)

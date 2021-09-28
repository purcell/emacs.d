;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
                     '(("parameters"
                        (progn
                          (flet
                           ((parse-args
                             (count tmp-buf)
                             (while
                                 (and
                                  (< count 5)
                                  (= 1
                                     (call-process-shell-command
                                      (format
                                       (concat "python << ___EOF___\n" "%s\n" " pass\n" "print(%s.__code__.co_varnames)\n" "___EOF___")
                                       (replace-regexp-in-string "^ *def" "def"
                                                                 (buffer-substring-no-properties
                                                                  (line-beginning-position)
                                                                  (line-end-position
                                                                   (incf count))))
                                       (replace-regexp-in-string "^ *def *\\(.*?\\)(.*$" "\\1"
                                                                 (buffer-substring-no-properties
                                                                  (line-beginning-position)
                                                                  (line-end-position))))
                                      nil
                                      (list tmp-buf nil))))))
                            (form-template
                             (arg-list template)
                             (let
                                 ((count 0))
                               (dolist
                                   (narg arg-list template)
                                 (setq template
                                       (format
                                        (concat "%s${%d:%s: ${%d:type}\n"
                                                (make-string python-indent 32)
                                                "${%d:description}\n}")
                                        template
                                        (incf count)
                                        narg
                                        (incf count)
                                        (incf count)))))
                             (replace-regexp-in-string "}\\n}$" "}}" template))
                            (get-word-from-nline
                             (n)
                             (replace-regexp-in-string "[ 	-]*" ""
                                                       (buffer-substring-no-properties
                                                        (line-beginning-position n)
                                                        (line-end-position n)))))
                           (if
                               (and
                                (equal ""
                                       (get-word-from-nline 0))
                                (equal "parameters"
                                       (downcase
                                        (get-word-from-nline -1))))
                               (delete-region
                                (line-beginning-position -1)
                                (line-beginning-position 1)))
                           (let
                               ((indent
                                 (python-indent-calculate-indentation))
                                (match)
                                (template "Parameters\n----------\n")
                                (tmp-buf "*tmp sni-param*"))
                             (save-excursion
                               (while
                                   (and
                                    (re-search-backward "^[ 	]*\\(def\\|class\\)" nil t)
                                    (setq match
                                          (match-string-no-properties 1))
                                    (re-search-forward "[ 	]*[dc]" nil t)
                                    (not
                                     (=
                                      (-
                                       (point)
                                       (line-beginning-position)
                                       1)
                                      (- indent python-indent)))))
                               (cond
                                ((equal match "def")
                                 (parse-args 0 tmp-buf)
                                 (setq template
                                       (form-template
                                        (split-string
                                         (with-current-buffer tmp-buf
                                           (buffer-string))
                                         "[()', \n]+" t)
                                        template))
                                 (kill-buffer tmp-buf))
                                ((equal match "class")
                                 (while
                                     (and
                                      (re-search-forward "^[ 	]+def +__init__" nil t)
                                      (not
                                       (=
                                        (python-indent-calculate-levels)
                                        indent))))
                                 (parse-args 0 tmp-buf)
                                 (setq template
                                       (form-template
                                        (cdr
                                         (split-string
                                          (with-current-buffer tmp-buf
                                            (buffer-string))
                                          "[()', \n]+" t))
                                        template))
                                 (kill-buffer tmp-buf))))
                             (yas-expand-snippet template nil nil
                                                 '((yas-indent-line 'fixed))))))
                        "parameters" nil nil nil "/Users/marshall/.emacs.d/snippets/emacs-lisp-mode/parameters" nil nil)
                       ("defg" "def ${1:name}($2):\n    \\\"\\\"\\\"$3\n    ${2:$(python-args-to-google-docstring yas-text t)}\n    ${5:Returns:\n        $6\n}\n    \\\"\\\"\\\"\n    ${0:$$(let ((beg yas-snippet-beg)\n                (end yas-snippet-end))\n        (yas-expand-snippet\n          (buffer-substring-no-properties beg end) beg end\n              (quote ((yas-indent-line nil) (yas-wrap-around-region nil))))\n            (delete-trailing-whitespace beg (- end 1)))}\n" "Python Docstring" nil nil nil "/Users/marshall/.emacs.d/snippets/emacs-lisp-mode/defg" nil nil)))


;;; Do not edit! File generated at Tue Feb 23 12:53:24 2021

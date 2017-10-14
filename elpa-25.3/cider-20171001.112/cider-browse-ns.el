;;; cider-browse-ns.el --- CIDER namespace browser

;; Copyright © 2014-2017 John Andrews, Bozhidar Batsov and CIDER contributors

;; Author: John Andrews <john.m.andrews@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; M-x cider-browse-ns
;;
;; Display a list of all vars in a namespace.
;; Pressing <enter> will take you to the cider-doc buffer for that var.
;; Pressing ^ will take you to a list of all namespaces (akin to `dired-mode').

;; M-x cider-browse-ns-all
;;
;; Explore Clojure namespaces by browsing a list of all namespaces.
;; Pressing <enter> expands into a list of that namespace's vars as if by
;; executing the command (cider-browse-ns "my.ns").

;;; Code:

(require 'cider-interaction)
(require 'cider-client)
(require 'subr-x)
(require 'cider-compat)
(require 'cider-util)
(require 'nrepl-dict)

(defconst cider-browse-ns-buffer "*cider-ns-browser*")
(add-to-list 'cider-ancillary-buffers cider-browse-ns-buffer)

(defvar-local cider-browse-ns-current-ns nil)

;; Mode Definition

(defvar cider-browse-ns-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map "d" #'cider-browse-ns-doc-at-point)
    (define-key map "s" #'cider-browse-ns-find-at-point)
    (define-key map (kbd "RET") #'cider-browse-ns-operate-at-point)
    (define-key map "^" #'cider-browse-ns-all)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map))

(defvar cider-browse-ns-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-browse-ns-handle-mouse)
    map))

(define-derived-mode cider-browse-ns-mode special-mode "browse-ns"
  "Major mode for browsing Clojure namespaces.

\\{cider-browse-ns-mode-map}"
  (setq-local electric-indent-chars nil)
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t))
  (setq-local cider-browse-ns-current-ns nil))

(defun cider-browse-ns--text-face (var-meta)
  "Return font-lock-face for a var.
VAR-META contains the metadata information used to decide a face.
Presence of \"arglists-str\" and \"macro\" indicates a macro form.
Only \"arglists-str\" indicates a function. Otherwise, its a variable.
If the NAMESPACE is not loaded in the REPL, assume TEXT is a fn."
  (cond
   ((not var-meta) 'font-lock-function-name-face)
   ((and (nrepl-dict-contains var-meta "arglists")
         (string= (nrepl-dict-get var-meta "macro") "true"))
    'font-lock-keyword-face)
   ((nrepl-dict-contains var-meta "arglists") 'font-lock-function-name-face)
   (t 'font-lock-variable-name-face)))

(defun cider-browse-ns--properties (var var-meta)
  "Decorate VAR with a clickable keymap and a face.
VAR-META is used to decide a font-lock face."
  (let ((face (cider-browse-ns--text-face var-meta)))
    (propertize var
                'font-lock-face face
                'mouse-face 'highlight
                'keymap cider-browse-ns-mouse-map)))

(defun cider-browse-ns--list (buffer title items &optional ns noerase)
  "Reset contents of BUFFER.
Display TITLE at the top and ITEMS are indented underneath.
If NS is non-nil, it is added to each item as the
`cider-browse-ns-current-ns' text property.  If NOERASE is non-nil, the
contents of the buffer are not reset before inserting TITLE and ITEMS."
  (with-current-buffer buffer
    (cider-browse-ns-mode)
    (let ((inhibit-read-only t))
      (unless noerase (erase-buffer))
      (goto-char (point-max))
      (insert (cider-propertize title 'ns) "\n")
      (dolist (item items)
        (insert (propertize (concat "  " item "\n")
                            'cider-browse-ns-current-ns ns)))
      (goto-char (point-min)))))

(defun cider-browse-ns--first-doc-line (doc)
  "Return the first line of the given DOC string.
If the first line of the DOC string contains multiple sentences, only
the first sentence is returned.  If the DOC string is nil, a Not documented
string is returned."
  (if doc
      (let* ((split-newline (split-string doc "\n"))
             (first-line (car split-newline)))
        (cond
         ((string-match "\\. " first-line) (substring first-line 0 (match-end 0)))
         ((= 1 (length split-newline)) first-line)
         (t (concat first-line "..."))))
    "Not documented."))

(defun cider-browse-ns--items (namespace)
  "Return the items to show in the namespace browser of the given NAMESPACE.
Each item consists of a ns-var and the first line of its docstring."
  (let* ((ns-vars-with-meta (cider-sync-request:ns-vars-with-meta namespace))
         (propertized-ns-vars (nrepl-dict-map #'cider-browse-ns--properties ns-vars-with-meta)))
    (mapcar (lambda (ns-var)
              (let* ((doc (nrepl-dict-get-in ns-vars-with-meta (list ns-var "doc")))
                     ;; to avoid (read nil)
                     ;; it prompts the user for a Lisp expression
                     (doc (when doc (read doc)))
                     (first-doc-line (cider-browse-ns--first-doc-line doc)))
                (concat ns-var " " (propertize first-doc-line 'font-lock-face 'font-lock-doc-face))))
            propertized-ns-vars)))

;; Interactive Functions

;;;###autoload
(defun cider-browse-ns (namespace)
  "List all NAMESPACE's vars in BUFFER."
  (interactive (list (completing-read "Browse namespace: " (cider-sync-request:ns-list))))
  (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer t)
    (cider-browse-ns--list (current-buffer)
                           namespace
                           (cider-browse-ns--items namespace))
    (setq-local cider-browse-ns-current-ns namespace)))

;;;###autoload
(defun cider-browse-ns-all ()
  "List all loaded namespaces in BUFFER."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer t)
    (let ((names (cider-sync-request:ns-list)))
      (cider-browse-ns--list (current-buffer)
                             "All loaded namespaces"
                             (mapcar (lambda (name)
                                       (cider-browse-ns--properties name nil))
                                     names))
      (setq-local cider-browse-ns-current-ns nil))))

(defun cider-browse-ns--thing-at-point ()
  "Get the thing at point.
Return a list of the type ('ns or 'var) and the value."
  (let ((line (car (split-string (string-trim (thing-at-point 'line)) " "))))
    (if (string-match "\\." line)
        `(ns ,line)
      `(var ,(format "%s/%s"
                     (or (get-text-property (point) 'cider-browse-ns-current-ns)
                         cider-browse-ns-current-ns)
                     line)))))

(defun cider-browse-ns-doc-at-point ()
  "Show the documentation for the thing at current point."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (value (cadr thing)))
    ;; value is either some ns or a var
    (cider-doc-lookup value)))

(defun cider-browse-ns-operate-at-point ()
  "Expand browser according to thing at current point.
If the thing at point is a ns it will be browsed,
and if the thing at point is some var - its documentation will
be displayed."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (type (car thing))
         (value (cadr thing)))
    (if (eq type 'ns)
        (cider-browse-ns value)
      (cider-doc-lookup value))))

(defun cider-browse-ns-find-at-point ()
  "Find the definition of the thing at point."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (type (car thing))
         (value (cadr thing)))
    (if (eq type 'ns)
        (cider-find-ns nil value)
      (cider-find-var current-prefix-arg value))))

(defun cider-browse-ns-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (cider-browse-ns-operate-at-point))

(provide 'cider-browse-ns)

;;; cider-browse-ns.el ends here

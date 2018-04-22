;;; ensime-eldoc.el --- ELDoc Support for ensime

;;; Commentary:

;;; Code:

(require 'eldoc)
(require 'ensime-client)
(require 'ensime-model)
(require 'ensime-notes)

(defun ensime-eldoc-info ()
  "ELDoc backend for ensime."
  ;; The response from `ensime-rpc-symbol-at-point' has the type info but,
  ;; its sligthly different from the one obtained with `ensime-type-at-point'
  ;; Using the underlying `ensime-rpc-get-type-at-point' to maintain consistency
  (when (ensime-connected-p)
    (let ((msg (pcase ensime-eldoc-hints
                 (`error
                  (mapconcat 'identity (ensime-errors-at (point)) "\n"))
                 (`implicit
                  (mapconcat 'identity (ensime-implicit-notes-at (point)) "\n"))
                 (`type
                  (or (ensime-eldoc-type-info) ""))
                 (`all
                  (format "%s\n%s\n%s"
                          (or (ensime-eldoc-type-info) "")
                          (mapconcat 'identity (ensime-implicit-notes-at (point)) "\n")
                          (mapconcat 'identity (ensime-errors-at (point)) "\n"))))))
      (eldoc-message (s-trim msg)))))

(defun ensime-eldoc-type-info ()
  "Get type information at point in a format suitable for eldoc"
  (when (ensime-connected-p)
    (let* ((symbol (ensime-rpc-symbol-at-point))
           (type (ensime-rpc-get-type-at-point))
           (name (plist-get symbol :local-name))
           (type-name (ensime-type-name-with-args type)))
      (when (and name type (not (string= type-name "<none>")))
        (format "%s: %s"
                (propertize name 'face 'font-lock-variable-name-face)
                (propertize type-name 'face 'font-lock-type-face))))))

(provide 'ensime-eldoc)

;;; ensime-eldoc.el ends here

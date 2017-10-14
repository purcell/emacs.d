;;; cider-common.el --- Common use functions         -*- lexical-binding: t; -*-

;; Copyright © 2015-2017  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Common functions that are useful in both Clojure buffers and REPL
;; buffers.

;;; Code:

(require 'subr-x)
(require 'cider-compat)
(require 'nrepl-dict)
(require 'cider-util)
(require 'tramp)

(defcustom cider-prompt-for-symbol t
  "Controls when to prompt for symbol when a command requires one.

When non-nil, always prompt, and use the symbol at point as the default
value at the prompt.

When nil, attempt to use the symbol at point for the command, and only
prompt if that throws an error."
  :type '(choice (const :tag "always" t)
                 (const :tag "dwim" nil))
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-special-mode-truncate-lines t
  "If non-nil, contents of CIDER's special buffers will be line-truncated.
Should be set before loading CIDER."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defun cider--should-prompt-for-symbol (&optional invert)
  "Return the value of the variable `cider-prompt-for-symbol'.
Optionally invert the value, if INVERT is truthy."
  (if invert (not cider-prompt-for-symbol) cider-prompt-for-symbol))

(defun cider-prompt-for-symbol-function (&optional invert)
  "Prompt for symbol if funcall `cider--should-prompt-for-symbol' is truthy.
Otherwise attempt to use the symbol at point for the command, and only
prompt if that throws an error.

INVERT is used to invert the semantics of the function `cider--should-prompt-for-symbol'."
  (if (cider--should-prompt-for-symbol invert)
      #'cider-read-symbol-name
    #'cider-try-symbol-at-point))

(defun cider--kw-to-symbol (kw)
  "Convert the keyword KW to a symbol."
  (when kw
    (replace-regexp-in-string "\\`:+" "" kw)))

(declare-function cider-read-from-minibuffer "cider-interaction")

(defun cider-read-symbol-name (prompt callback)
  "Read a symbol name using PROMPT with a default of the one at point.
Use CALLBACK as the completing read var callback."
  (funcall callback (cider-read-from-minibuffer
                     prompt
                     ;; if the thing at point is a keyword we treat it as symbol
                     (cider--kw-to-symbol (cider-symbol-at-point 'look-back)))))

(defun cider-try-symbol-at-point (prompt callback)
  "Call CALLBACK with symbol at point.
On failure, read a symbol name using PROMPT and call CALLBACK with that."
  (condition-case nil (funcall callback (cider--kw-to-symbol (cider-symbol-at-point 'look-back)))
    ('error (funcall callback (cider-read-from-minibuffer prompt)))))

(declare-function cider-jump-to "cider-interaction")

(defun cider--find-buffer-for-file (file)
  "Return a buffer visiting FILE.
If FILE is a temp buffer name, return that buffer."
  (if (string-prefix-p "*" file)
      file
    (and file
         (not (cider--tooling-file-p file))
         (cider-find-file file))))

(defun cider--jump-to-loc-from-info (info &optional other-window)
  "Jump to location give by INFO.
INFO object is returned by `cider-var-info' or `cider-member-info'.
OTHER-WINDOW is passed to `cider-jump-to'."
  (let* ((line (nrepl-dict-get info "line"))
         (file (nrepl-dict-get info "file"))
         (name (nrepl-dict-get info "name"))
         ;; the filename might actually be a REPL buffer name
         (buffer (cider--find-buffer-for-file file)))
    (if buffer
        (cider-jump-to buffer (if line (cons line nil) name) other-window)
      (error "No source location"))))

(declare-function url-filename "url-parse" (cl-x) t)

(defun cider--url-to-file (url)
  "Return the filename from the resource URL.
Uses `url-generic-parse-url' to parse the url.  The filename is extracted and
then url decoded.  If the decoded filename has a Windows device letter followed
by a colon immediately after the leading '/' then the leading '/' is dropped to
create a valid path."
  (let ((filename (url-unhex-string (url-filename (url-generic-parse-url url)))))
    (if (string-match "^/\\([a-zA-Z]:/.*\\)" filename)
        (match-string 1 filename)
      filename)))

(defun cider-tramp-prefix (&optional buffer)
  "Use the filename for BUFFER to determine a tramp prefix.
Defaults to the current buffer.
Return the tramp prefix, or nil if BUFFER is local."
  (let* ((buffer (or buffer (current-buffer)))
         (name (or (buffer-file-name buffer)
                   (with-current-buffer buffer
                     default-directory))))
    (when (tramp-tramp-file-p name)
      (let ((vec (tramp-dissect-file-name name)))
        (tramp-make-tramp-file-name (tramp-file-name-method vec)
                                    (tramp-file-name-user vec)
                                    (tramp-file-name-host vec)
                                    nil)))))

(defun cider--client-tramp-filename (name &optional buffer)
  "Return the tramp filename for path NAME relative to BUFFER.
If BUFFER has a tramp prefix, it will be added as a prefix to NAME.
If the resulting path is an existing tramp file, it returns the path,
otherwise, nil."
  (let* ((buffer (or buffer (current-buffer)))
         (name (concat (cider-tramp-prefix buffer) name)))
    (if (tramp-handle-file-exists-p name)
        name)))

(defun cider--server-filename (name)
  "Return the nREPL server-relative filename for NAME."
  (if (tramp-tramp-file-p name)
      (with-parsed-tramp-file-name name nil
        localname)
    name))

(defvar cider-from-nrepl-filename-function
  (with-no-warnings
    (if (eq system-type 'cygwin)
        #'cygwin-convert-file-name-from-windows
      #'identity))
  "Function to translate nREPL namestrings to Emacs filenames.")

(defcustom cider-prefer-local-resources nil
  "Prefer local resources to remote (tramp) ones when both are available."
  :type 'boolean
  :group 'cider)

(defun cider--file-path (path)
  "Return PATH's local or tramp path using `cider-prefer-local-resources'.
If no local or remote file exists, return nil."
  (let* ((local-path (funcall cider-from-nrepl-filename-function path))
         (tramp-path (and local-path (cider--client-tramp-filename local-path))))
    (cond ((equal local-path "") "")
          ((and cider-prefer-local-resources (file-exists-p local-path))
           local-path)
          ((and tramp-path (file-exists-p tramp-path))
           tramp-path)
          ((and local-path (file-exists-p local-path))
           local-path))))

(declare-function archive-extract "arc-mode")
(declare-function archive-zip-extract "arc-mode")

(defun cider-find-file (url)
  "Return a buffer visiting the file URL if it exists, or nil otherwise.
If URL has a scheme prefix, it must represent a fully-qualified file path
or an entry within a zip/jar archive.  If URL doesn't contain a scheme
prefix and is an absolute path, it is treated as such.  Finally, if URL is
relative, it is expanded within each of the open Clojure buffers till an
existing file ending with URL has been found."
  (require 'arc-mode)
  (cond ((string-match "^file:\\(.+\\)" url)
         (when-let ((file (cider--url-to-file (match-string 1 url)))
                    (path (cider--file-path file)))
           (find-file-noselect path)))
        ((string-match "^\\(jar\\|zip\\):\\(file:.+\\)!/\\(.+\\)" url)
         (when-let ((entry (match-string 3 url))
                    (file  (cider--url-to-file (match-string 2 url)))
                    (path  (cider--file-path file))
                    (name  (format "%s:%s" path entry)))
           (or (find-buffer-visiting name)
               (if (tramp-tramp-file-p path)
                   (progn
                     ;; Use emacs built in archiving
                     (find-file path)
                     (goto-char (point-min))
                     ;; Make sure the file path is followed by a newline to
                     ;; prevent eg. clj matching cljs.
                     (search-forward (concat entry "\n"))
                     ;; moves up to matching line
                     (forward-line -1)
                     (archive-extract)
                     (current-buffer))
                 ;; Use external zip program to just extract the single file
                 (with-current-buffer (generate-new-buffer
                                       (file-name-nondirectory entry))
                   (archive-zip-extract path entry)
                   (set-visited-file-name name)
                   (setq-local default-directory (file-name-directory path))
                   (setq-local buffer-read-only t)
                   (set-buffer-modified-p nil)
                   (set-auto-mode)
                   (current-buffer))))))
        (t (if-let ((path (cider--file-path url)))
               (find-file-noselect path)
             (unless (file-name-absolute-p url)
               (let ((cider-buffers (cider-util--clojure-buffers))
                     (url (file-name-nondirectory url)))
                 (or (cl-loop for bf in cider-buffers
                              for path = (with-current-buffer bf
                                           (expand-file-name url))
                              if (and path (file-exists-p path))
                              return (find-file-noselect path))
                     (cl-loop for bf in cider-buffers
                              if (string= (buffer-name bf) url)
                              return bf))))))))

(defun cider--open-other-window-p (arg)
  "Test prefix value ARG to see if it indicates displaying results in other window."
  (let ((narg (prefix-numeric-value arg)))
    (pcase narg
      (-1 t) ; -
      (16 t) ; empty empty
      (_ nil))))

(defun cider-abbreviate-ns (namespace)
  "Return a string that abbreviates NAMESPACE."
  (when namespace
    (let* ((names (reverse (split-string namespace "\\.")))
           (lastname (car names)))
      (concat (mapconcat (lambda (s) (concat (substring s 0 1) "."))
                         (reverse (cdr names))
                         "")
              lastname))))

(defun cider-last-ns-segment (namespace)
  "Return the last segment of NAMESPACE."
  (when namespace
    (car (reverse (split-string namespace "\\.")))))


(provide 'cider-common)
;;; cider-common.el ends here

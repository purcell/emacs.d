;;; json-reformat.el --- Reformatting tool for JSON

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/json-reformat
;; Package-Version: 20160212.53
;; Version: 0.0.6
;; Keywords: json

;; Copyright (c) 2012 Wataru MIYAGUNI
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;; Commentary:

;; json-reformat.el is a reformatting tool for JSON (http://json.org/).
;;
;; ## Usage
;;
;;   1. Specify region
;;   2. Call 'M-x json-reformat-region'
;;
;; ## Customize
;;
;;   - `json-reformat:indent-width'
;;   - `json-reformat:pretty-string?'
;;

;;; Code:

(require 'json)
(eval-when-compile (require 'cl))

(unless (require 'subr-x nil t)
  ;; built-in subr-x from 24.4
  (defsubst hash-table-keys (hash-table)
    "Return a list of keys in HASH-TABLE."
    (let ((keys '()))
      (maphash (lambda (k _v) (push k keys)) hash-table)
      keys)))

(put 'json-reformat-error 'error-message "JSON Reformat error")
(put 'json-reformat-error 'error-conditions '(json-reformat-error error))

(defconst json-reformat:special-chars-as-pretty-string
  '((?\" . ?\")
    (?\\ . ?\\)))

(defcustom json-reformat:indent-width 4
  "How much indentation `json-reformat-region' should do at each level."
  :type 'integer
  :safe #'integerp
  :group 'json-reformat)

(defcustom json-reformat:pretty-string? nil
  "Whether to decode the string.

Example:

{\"name\":\"foobar\",\"nick\":\"foo \\u00e4 bar\",\"description\":\"<pre>\\nbaz\\n</pre>\"}

If nil:

    {
        \"name\": \"foobar\",
        \"nick\": \"foo \\u00e4 bar\",
        \"description\": \"<pre>\\nbaz\\n<\\/pre>\"
    }

Else t:

    {
        \"name\": \"foobar\",
        \"nick\": \"foo ä bar\",
        \"description\": \"<pre>
    baz
    </pre>\"
    }"
  :type 'boolean
  :safe #'booleanp
  :group 'json-reformat)

(defun json-reformat:indent (level)
  (make-string (* level json-reformat:indent-width) ? ))

(defun json-reformat:number-to-string (val)
  (number-to-string val))

(defun json-reformat:symbol-to-string (val)
  (cond ((equal 't val) "true")
        ((equal json-false val) "false")
        (t (symbol-name val))))

(defun json-reformat:encode-char-as-pretty (char)
  (setq char (encode-char char 'ucs))
  (let ((special-char (car (rassoc char json-reformat:special-chars-as-pretty-string))))
    (if special-char
        (format "\\%c" special-char)
      (format "%c" char))))

(defun json-reformat:string-to-string (val)
  (if json-reformat:pretty-string?
      (format "\"%s\"" (mapconcat 'json-reformat:encode-char-as-pretty val ""))
    (json-encode-string val)))

(defun json-reformat:vector-to-string (val level)
  (if (= (length val) 0) "[]"
    (concat "[\n"
            (mapconcat
             'identity
             (loop for v across val
                   collect (concat
                            (json-reformat:indent (1+ level))
                            (json-reformat:print-node v (1+ level))
                            ))
             (concat ",\n"))
            "\n" (json-reformat:indent level) "]"
            )))

(defun json-reformat:print-node (val level)
  (cond ((hash-table-p val) (json-reformat:tree-to-string (json-reformat:tree-sibling-to-plist val) level))
        ((numberp val)      (json-reformat:number-to-string val))
        ((vectorp val)      (json-reformat:vector-to-string val level))
        ((null val)         "null")
        ((symbolp val)      (json-reformat:symbol-to-string val))
        (t                  (json-reformat:string-to-string val))))

(defun json-reformat:tree-sibling-to-plist (root)
  (let (pl)
    (dolist (key (reverse (hash-table-keys root)) pl)
      (setq pl (plist-put pl key (gethash key root))))))

(defun json-reformat:tree-to-string (root level)
  (concat "{\n"
          (let (key val str)
            (while root
              (setq key (car root)
                    val (cadr root)
                    root (cddr root))
              (setq str
                    (concat str (json-reformat:indent (1+ level))
                            "\"" key "\""
                            ": "
                            (json-reformat:print-node val (1+ level))
                            (when root ",")
                            "\n"
                            )))
            str)
          (json-reformat:indent level)
          "}"))

(defun json-reformat-from-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (condition-case errvar
        (let ((json-key-type 'string)
              (json-object-type 'hash-table)
              json-tree)
          (setq json-tree (json-read))
          (json-reformat:print-node json-tree 0))
      (json-error
       (signal 'json-reformat-error
               (list (error-message-string errvar)
                     (line-number-at-pos (point))
                     (point)))))))

;;;###autoload
(defun json-reformat-region (begin end)
  "Reformat the JSON in the specified region.

If you want to customize the reformat style,
please see the documentation of `json-reformat:indent-width'
and `json-reformat:pretty-string?'."
  (interactive "*r")
  (let ((start-line (line-number-at-pos begin))
        (start-pos  begin))
    (save-excursion
      (save-restriction
        (narrow-to-region begin end)
        (goto-char (point-min))
        (let (reformatted)
          (condition-case errvar
              (progn
                (setq reformatted
                      (json-reformat-from-string
                       (buffer-substring-no-properties (point-min) (point-max))))
                (delete-region (point-min) (point-max))
                (insert reformatted))
            (json-reformat-error
             (let ((reason   (nth 1 errvar))
                   (line     (nth 2 errvar))
                   (position (nth 3 errvar)))
               (message
                "JSON parse error [Reason] %s [Position] In buffer, line %d (char %d)"
                reason
                (+ start-line line -1)
                (+ start-pos position -1))))))))))

(provide 'json-reformat)

;;; json-reformat.el ends here

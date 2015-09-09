;;; javascript

(defun js-method-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back ": ")))

(defun js-function-declaration-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back "^\\s *")))

(defun snippet--function-punctuation ()
  (if (js-method-p)
      (when (not (looking-at "[ \n\t\r]*[},]"))
        (insert ","))
    (unless (js-function-declaration-p)
      (if (looking-at "$") (insert ";")))))

(defun snippet--function-name ()
  (if (js-function-declaration-p) "name" ""))

;;; clojure

(defun snippet--clojure-namespace-from-buffer-file-name ()
  (replace-regexp-in-string "_" "-"
   (replace-regexp-in-string "/" "."
    (chop-prefix "test/"
    (chop-prefix "src/"
    (chop-suffix ".clj"
     (substring (buffer-file-name) (length eproject-root))))))))

(defun snippet--clojure-namespace-under-test ()
  (replace-regexp-in-string "-test" "" (snippet--clojure-namespace-from-buffer-file-name)))

;; snippet-helper-helpers

(defun chop-suffix (suffix s)
  "Remove string 'suffix' if it is at end of string 's'"
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun chop-prefix (prefix s)
  "Remove string 'prefix' if it is at start of string 's'"
  (let ((pos (length prefix)))
    (if (and (>= (length s) (length prefix))
             (string= prefix (substring s 0 pos)))
        (substring s pos)
      s)))

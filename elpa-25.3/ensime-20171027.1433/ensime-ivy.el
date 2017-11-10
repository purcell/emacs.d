;;; ensime-ivy.el -- Ivy integration

;; Copyright (C) 2016 ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;; Code:

(require 'ivy "ivy.el" t)
(require 'ensime-model)

(declare-function ensime-search-jump-to-item "ensime-search.el")
(declare-function ensime-rpc-public-symbol-search "ensime-client.el")
(declare-function ensime-format-source-position "ensime-editor.el")
(declare-function ensime-source-hint-position "ensime-model.el")

(defvar ensime-search-min-length)
(defvar ensime-search-max-results)
(defvar ensime-ivy--search-results '()
  "Temporary variable to hold search result when using ivy completion.")

(defun ensime-ivy--format-search-element (elem)
  "Format the search element ELEM."
  (let ((pos (ensime-search-sym-pos elem)))
    (let* ((maybe-line (ensime-pos-line pos)))
      (let ((line (if maybe-line (number-to-string maybe-line) "?"))
            (name (ensime-search-sym-name elem)))
        (add-to-list 'ensime-ivy--search-results (cons name elem))
        (if (s-ends-with? ";" name)
            (propertize name 'face 'font-lock-function-name-face)
          (if (s-contains? "$" name)
              (propertize name 'face 'font-lock-comment-face)
            (propertize name 'face 'font-lock-type-face)))))))

(defun ensime-ivy-jump-to-item (name)
  "Open the item associated with NAME, if it has a source location."
  (ensime-search-jump-to-item (cdr (assoc name ensime-ivy--search-results))))

(defun ensime-ivy-public-symbol-search (pattern)
  "Search for symbols with the given PATTERN."
  (when (>= (length pattern) ensime-search-min-length)
    (setq ensime-ivy--search-results '())
    (mapcar 'ensime-ivy--format-search-element
            (ensime-rpc-public-symbol-search (split-string pattern " ")
                                             ensime-search-max-results))))

;;;###autoload
(defun ensime-search-ivy ()
  "Search ensime with ivy."
  (interactive)
  (ivy-read "Pattern: "
            #'ensime-ivy-public-symbol-search
            :action #'ensime-ivy-jump-to-item
            :dynamic-collection t))

(defun ensime-ivy-select-source-position (positions name)
  "Select one source position from POSITIONS prompted by NAME."
  (let ((name-alist (mapcar (lambda (elem)
                              (cons (ensime-format-source-position
                                     (ensime-source-hint-position elem))
                                    elem))
                            positions)))
    (ivy-read name name-alist)))

(provide 'ensime-ivy)

;;; ensime-ivy.el ends here

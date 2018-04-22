(require 'slime)
(require 'cl-lib)

(define-slime-contrib slime-sbcl-exts
  "Misc extensions for SBCL"
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies slime-references)
  (:swank-dependencies swank-sbcl-exts))

(defun slime-sbcl-bug-at-point ()
  (save-excursion
    (save-match-data
      (unless (looking-at "#[0-9]\\{6\\}")
        (search-backward-regexp "#\\<" (line-beginning-position) t))
      (when (looking-at "#[0-9]\\{6\\}")
        (buffer-substring-no-properties (match-beginning 0) (match-end 0))))))

(defun slime-read-sbcl-bug (prompt &optional query)
  "Either read a sbcl bug or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (let ((bug (slime-sbcl-bug-at-point)))
    (cond ((or current-prefix-arg query (not bug))
           (slime-read-from-minibuffer prompt bug))
          (t bug))))

(defun slime-visit-sbcl-bug (bug)
  "Visit the Launchpad site that describes `bug' (#nnnnnn)."
  (interactive (list (slime-read-sbcl-bug "Bug number (#nnnnnn): ")))
  (browse-url (format "http://bugs.launchpad.net/sbcl/+bug/%s" 
                      (substring bug 1))))

(provide 'slime-sbcl-exts)

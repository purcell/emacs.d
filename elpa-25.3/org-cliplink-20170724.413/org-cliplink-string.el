(defun org-cliplink-straight-string (s)
  (when s
    (mapconcat #'identity (split-string s) " ")))

(defun org-cliplink-join-string (ss)
  (mapconcat #'identity ss " "))

(defun org-cliplink-elide-string (s max-length)
  (when s
    (if (> max-length 3)
        (if (> (length s) max-length)
            (concat (substring s 0 (- max-length 3)) "...")
          s)
      "...")))

(provide 'org-cliplink-string)

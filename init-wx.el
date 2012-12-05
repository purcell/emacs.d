(defun wx-root-dir ()
  (let ((rd (getenv "WXWIN")))
    (if (not rd)
        (setq rd (getenv "WXWIDGETS"))
        )
    rd
    )
  )

(defun wx-list-api ()
  "List wxWidgets API in its default HTML manual"
  (interactive)
  (let ((rd (wx-root-dir)))
    (when rd
      (w3m-browse-url (concat rd "/docs/doxygen/out/html/group__group__funcmacro.html"))
      )
    )
  )

(defun wx-list-class ()
  "List wxWidgets class in its default HTML manual"
  (interactive)
  (let ((rd (wx-root-dir)))
    (when rd
      (w3m-browse-url (concat rd "/docs/doxygen/out/html/group__group__class.html"))
      )
    )
  )

(defun wx-match-strs (s)
  (let ((cs case-fold-search) v r l (i 0))
    (setq case-fold-search nil) ;case sensitive search
    (while (setq i (string-match "\\([A-Z][a-z]*\\)" s i))
      (setq r (downcase (match-string 1 s)))
      (setq l (concat l "_" r))
      (setq i (+ i (length r) ))
      )
    ;restore
    (setq case-fold-search cs)
    l
    )
  )

(defun wx-browse-class (cls)
  (interactive "sClass Name: ")
  (let ((rd (wx-root-dir)))
    (when rd
      (w3m-browse-url (concat rd "/docs/doxygen/out/html/classwx" (wx-match-strs cls) ".html"))
      )
    )
  )
(provide 'init-wx)

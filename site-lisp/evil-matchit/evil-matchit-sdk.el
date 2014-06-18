(defvar evilmi-sdk-extract-keyword-howtos
  '(("^[ \t]*\\([a-z]+\!?\\)\\( .*\\| *\\)$" 1)
    ("^.* \\(do\\) |[a-z0-9A-Z,|]+|$" 1)
    )
  "The list of HOWTO on extracting keyword from current line.
Each howto is actually a pair. The first element of pair is the regular
expression to match the current line. The second is the index of sub-matches
to extract the keyword which starts from one. The sub-match is the match defined
between '\\(' and '\\)' in regular expression.
"
  )


(defun evilmi-sdk-tags-match (level orig-tag-info cur-tag-info)
  (let (rlt)
    (when (nth 2 cur-tag-info) ;; handle function exit point
      (setq level 1)
      )
    (setq rlt (and (= 1 level) (= (nth 0 orig-tag-info) (nth 0 cur-tag-info))))
    ))

;;;###autoload
(defun evilmi-sdk-member (KEYWORD LIST)
  "check if KEYWORD exist in LIST"
  (let (rlt)
    (cond
     ((not KEYWORD) nil)
     ((not LIST) nil)
     ((stringp (car LIST))
      (if (string-match (concat "^" (car LIST) "$") KEYWORD) t
        (evilmi-sdk-member KEYWORD (cdr LIST)))
      )
     ((listp (car LIST))
      (setq rlt (evilmi-sdk-member KEYWORD (car LIST)))
      (if rlt rlt (evilmi-sdk-member KEYWORD (cdr LIST))))
     (t
      ;; just ignore first element
      (evilmi-sdk-member KEYWORD (cdr LIST))))))


;;;###autoload
(defun evilmi-sdk-get-tag-info (tag match-tags)
  "return (row column is-function-exit-point),
the row and column marked position in evilmi-mylang-match-tags
is-function-exit-point could be t or nil
"
  (let (rlt elems elem tag-type
            found i j)

    (setq i 0)
    (while (and (< i (length match-tags)) (not found))
      (setq elems (nth i match-tags))
      (setq j 0)
      (while (and (not found) (< j (length elems)))
        (setq elem (nth j elems))
        (cond
         ((stringp elem)
          (if (string-match (concat "^" elem "$") tag)
              (setq found t)
            ))
         ((listp elem)
          (if (evilmi-sdk-member tag elem)
              (setq found t)
            ))
         )
        (if (not found) (setq j (1+ j)))
        )
      (if (not found) (setq i (1+ i)))
      )
    (when found
      (if (nth 3 (nth i match-tags))
          (setq rlt (list i j t))
        (setq rlt (list i j))
        ))
    rlt
    ))

(defun evilmi--sdk-extract-keyword (cur-line match-tags howtos)
  "extract keyword from cur-line. keyword should be defined in match-tags"
  (let (keyword howto i)
    (setq i 0)
    (while (and (not keyword) (< i (length howtos)))
      (setq howto (nth i howtos))
      (when (string-match (nth 0 howto) cur-line)
        (setq keyword (match-string (nth 1 howto) cur-line))
        ;; keep search keyword by using next howto (regex and match-string index)
        (if (not (evilmi-sdk-member keyword match-tags)) (setq keyword nil))
        )
      (setq i (1+ i))
      )
    keyword
    )
  )

;;;###autoload
(defun evilmi-sdk-get-tag (match-tags howtos)
  "return '(start-point tag-info)
"
  (let (rlt
        keyword
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
        tag-info)

    (when (setq keyword (evilmi--sdk-extract-keyword cur-line match-tags howtos))

      ;; since we mixed ruby and lua mode here
      ;; maybe we should be strict at the keyword
      (if (setq tag-info (evilmi-sdk-get-tag-info keyword match-tags))
          ;; 0 - open tag; 1 - middle tag; 2 - close tag;
          (setq rlt (list
                     (if (= 2 (nth 1 tag-info))
                         (line-end-position)
                       (line-beginning-position))
                     tag-info))
        )
      )
    rlt
    )
  )

;;;###autoload
(defun evilmi-sdk-jump (rlt NUM match-tags howtos)
  (let ((orig-tag-type (nth 1 (nth 1 rlt)))
        (orig-tag-info (nth 1 rlt))
        cur-tag-type
        cur-tag-info
        (level 1)
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
        keyword
        found
        where-to-jump-in-theory
        )

    (while (not found)
      (forward-line (if (= orig-tag-type 2) -1 1))
      (setq cur-line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))
            )

      (setq keyword (evilmi--sdk-extract-keyword cur-line match-tags howtos))
      ;; (message "keyword=%s cur-line=%s" keyword cur-line)

      (when keyword

        (setq cur-tag-info (evilmi-sdk-get-tag-info keyword match-tags))
        (setq cur-tag-type (nth 1 cur-tag-info))

        ;; key algorithm
        (cond
         ;; handle open tag
         ;; open (0) -> mid (1)  found when level is one else ignore
         ((and (= orig-tag-type 0) (= cur-tag-type 1))
          (when (evilmi-sdk-tags-match level orig-tag-info cur-tag-info)
            (back-to-indentation)
            (setq where-to-jump-in-theory (1- (line-beginning-position)))
            (setq found t)
            )
          )
         ;; open (0) -> closed (2) found when level is zero, level--
         ((and (= orig-tag-type 0) (= cur-tag-type 2))
          (when (evilmi-sdk-tags-match level orig-tag-info cur-tag-info)
            (goto-char (line-end-position))
            (setq where-to-jump-in-theory (line-end-position))
            (setq found t)
            )
          (setq level (1- level))
          )
         ;; open (0) -> open (0) level++
         ((and (= orig-tag-type 0) (= cur-tag-type 0))
          (setq level (1+ level))
          )

         ;; now handle mid tag
         ;; mid (1) -> mid (1) found if:
         ;;   1. level is one
         ;;   2. the open tag and middle tag are in the same row in evilmi-mylang-match-tags
         ;; else: just ignore
         ;; level is one means we are not in some embedded loop/conditional statements
         ((and (= orig-tag-type 1) (= cur-tag-type 1))

          (when (evilmi-sdk-tags-match level orig-tag-info cur-tag-info)
            (back-to-indentation)
            (setq where-to-jump-in-theory (1- (line-beginning-position)))
            (setq found t)
            )
          )
         ;; mid (1) -> closed (2) found when level is zero, level --
         ((and (= orig-tag-type 1) (= cur-tag-type 2))
          (when (evilmi-sdk-tags-match level orig-tag-info cur-tag-info)
            (goto-char (line-end-position))
            (setq where-to-jump-in-theory (line-end-position))
            (setq found t)
            )
          (setq level (1- level))
          )
         ;; mid (1) -> open (0) level++
         ((and (= orig-tag-type 1) (= cur-tag-type 0))
          (setq level (1+ level))
          )

         ;; now handle closed tag
         ;; closed (2) -> mid (1) ignore,impossible
         ((and (= orig-tag-type 2) (= cur-tag-type 1))
          )
         ;; closed (2) -> closed (2) level++
         ((and (= orig-tag-type 2) (= cur-tag-type 2))
          (setq level (1+ level))
          )
         ;; closed (2) -> open (0) found when level is zero, level--
         ((and (= orig-tag-type 2) (= cur-tag-type 0))
          (when (evilmi-sdk-tags-match level orig-tag-info cur-tag-info)
            (setq where-to-jump-in-theory (line-beginning-position))
            (back-to-indentation)
            (setq found t)
            )
          (setq level (1- level))
          )
         (t (message "why here?"))
         )

        )

      ;; we will stop at end or beginning of buffer anyway
      (if (or (= (line-end-position) (point-max))
              (= (line-beginning-position) (point-min))
              )
          (setq found t)
        )
      )
    where-to-jump-in-theory
    )
  )

(provide 'evil-matchit-sdk)

;;; ensime-util.el --- helper functions

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(defvar ensime-dir (file-name-directory load-file-name)
  "The root dir of the Ensime distribution.")

(defvar ensime-ch-fix 1
  "Single character offset to convert between emacs and
 0-based character indexing.")

(defvar ensime-message-function 'message)

(defvar ensime-background-message-function 'ensime-display-oneliner)

;;;;; Link/test insertion

(defun ensime-make-code-link (start end file-path offset &optional face line)
  "Make an emacs button, from start to end in current buffer,
 linking to file-path and offset."
  (make-button start end
	       'face (or face font-lock-keyword-face)
	       'action `(lambda (x)
			  (find-file-other-window ,file-path)
			  (if (integerp ,line)
			      (ensime-goto-line ,line)
			    (goto-char (if ,offset
                                           (ensime-internalize-offset ,offset)
                                         0))))))

(defun ensime-make-pos-link (start end pos &optional face)
  "Make an emacs button, from start to end in current buffer,
 linking to pos"
  (make-button start end
	       'face (or face font-lock-keyword-face)
	       'action `(lambda (x)
			  (ensime-goto-source-location ',pos t))))

(defun ensime-make-code-hyperlink (start end http-path &optional face)
  "Make an emacs button, from start to end in current buffer,
 hyperlinking to http-path."
  (make-button start end
	       'face (or face font-lock-keyword-face)
	       'action `(lambda (x)
			  (browse-url ,http-path)
			  (message "Opening documentation in browser..")
			  )))

(defun ensime-http-url-p (s)
  (and (stringp s) (or (string-match "http://" s) (string-match "https://" s) (string-match "file://" s))))

(defun ensime-insert-link (text pos-or-path &optional face)
  "Insert text in current buffer and make it into an emacs
 button, linking to file-path and offset. Intelligently decide
 whether to make a source link or an http link based on the file-path."
  (let ((start (point)))
    (cond
     ((and pos-or-path (ensime-http-url-p pos-or-path))
      (progn
	(insert text)
	(ensime-make-code-hyperlink start (point) pos-or-path face)))

     ((and (listp pos-or-path) (ensime-pos-valid-local-p pos-or-path))
      (progn
	(insert text)
        (ensime-make-pos-link start (point) pos-or-path face)))

     (t
      (insert text)))))


(defun ensime-insert-action-link (text action &optional face)
  "Insert text in current buffer and make it into an emacs
 button, linking to file-path and offset."
  (let ((start (point)))
    (insert text)
    (make-button start (point) 'face
		 (or face font-lock-variable-name-face)
		 'action action)))

(defun ensime-insert-with-face (text face)
  "Insert text in current buffer and color it
 with face"
  (let ((start (point)))
    (insert text)
    (set-text-properties start (point) `(face ,face))))

(defun ensime-kill-txt-props (str)
  "Remove all text-properties from str and return str."
  (set-text-properties 0 (length str) nil str)
  str)

(defun buffer-file-name-with-indirect ()
  "Return buffer name. Works both for regular buffers and indirect buffers."
  (buffer-file-name (buffer-base-buffer)))

;; File/path functions

(defun ensime-source-file-p (&optional filename)
  "Return t if the given filename (or the currently visited file if no
argument is supplied) is a .scala or .java file."
  (let ((file (or filename (buffer-file-name-with-indirect))))
    (when file
      (integerp (string-match "\\(?:\\.scala$\\|\\.java$\\)" file)))))

(defun ensime-java-file-p (f)
  (string-match "\\.java$" f))

(defun ensime-scala-file-p (f)
  (string-match "\\.scala$" f))

(defun ensime-visiting-java-file-p ()
  (ensime-java-file-p (buffer-file-name-with-indirect)))

(defun ensime-visiting-scala-file-p ()
  (ensime-scala-file-p (buffer-file-name-with-indirect)))

(defun ensime-path-prefix-p (file-name dir-name)
  "Expands both file-name and dir-name and returns t if dir-name is a
 prefix of file-name. Does not touch the file system."
  (let* ((dir (file-name-as-directory (expand-file-name dir-name)))
	 (file (expand-file-name file-name)))
    (string-prefix-p dir file)))

(defun ensime-path-includes-dir-p (file dir)
  "Return t if dir is found in file's absolute path, either directly or via
 symbolic link. This is in contrast to file-in-directory-p, which tests for
 physical containment. For example:
 If I have a buffer open, say /proj/src/X.scala, and src is a symlink to
 /tmp/sources, (file-in-directory-p \"/proj/src/X.scala\" \"/proj\") will answer
 nil, where ensime-path-includes-dir-p will answer t.
 Note: This function assumes both file and dir actually exist."
  (let ((phys-dir (file-truename dir))
        (d (file-name-directory (expand-file-name file))))
    (catch 'return
      (while d
        (let ((prev d))
          (when (string-prefix-p phys-dir (file-truename d))
            (throw 'return t))
          (setq d (file-name-directory (directory-file-name d)))
          (when (equal d prev)
            (throw 'return nil))
          )))))

; TODO deprecate and rewrite callers to use the cache-dir
(defun ensime-temp-directory ()
  "Return the directory name of the system's temporary file dump."
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	(t "/tmp/")))

(defun ensime-temp-file-name (name)
  "Return the path of a temp file with filename 'name'."
  (expand-file-name
   (ensime--join-paths (ensime-temp-directory) name)))

(defun ensime-assert-executable-on-path (name)
  (when (null (executable-find name))
    (error (concat name " not found on your emacs exec-path. "
		   "See Troubleshooting section of the ENSIME manual."))))

(defun ensime-relativise-path (path root)
  "Given a directory named root, and a path f, return f's path
relative to root. If f is not contained by root, return the
absolute path to f."
  (let* ((full-root (directory-file-name (expand-file-name root)))
	 (full-path (expand-file-name path))
	 (index (string-match (concat "^" full-root) full-path)))
    (if (equal index 0)
	(concat "." (substring full-path (length full-root)))
      path)))

(defun ensime-write-buffer (&optional filename clear-modtime set-unmodified)
  "Write the contents of buffer to its buffer-file-name.
Do not show 'Writing..' message."
  (let ((file (or filename (buffer-file-name-with-indirect)))
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (when clear-modtime
      (clear-visited-file-modtime))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) file nil 'nomessage))
    (when set-unmodified
      (set-buffer-modified-p nil))
    ))

(defun ensime-write-to-file (filename string)
  "Write the given string to FILENAME, creating parent directories as needed"
  (let ((dir (file-name-directory filename)))
    (make-directory dir t))
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) filename)))

(defun ensime-read-from-file (filename)
  "Return the contents of FILENAME as a string"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun ensime-src-info-for-current-buffer ()
  "Returns a source-file-info for the current state of the current buffer."
  (if (or (buffer-modified-p) (null buffer-file-number))
      (if (< (buffer-size) 1000) ;; TODO: find break-even point experimentally
	  (ensime-src-info-with-contents)
	(ensime-src-info-with-contents-in-temp))
    `(:file ,(buffer-file-name-with-indirect))
    ))

(defun ensime-src-info-with-contents-in-temp ()
  "Write the contents of current buffer to temp file, return a source-file-info
 with contents in temp file."
  (let* ((tmp-dir (concat (ensime--get-cache-dir (ensime-config-for-buffer))
			  "/scratch"))
	 (max-rpcs-in-flight 25) ;; random guess...
	 (tmp-file (format "%s/source_file_contents_%s_%s_%s"
			   tmp-dir
			   (emacs-pid)
			   ensime-connection-counter
			   (% (ensime-continuation-counter)
			      max-rpcs-in-flight))))
    (when (not (file-directory-p tmp-dir))
      (make-directory tmp-dir t))
    (ensime-write-buffer tmp-file nil nil)
    `(:file ,(buffer-file-name-with-indirect) :contents-in ,tmp-file)))

(defun ensime-src-info-with-contents ()
  "Returns a source-file-info with contents of current buffer as string."
  `(:file ,(buffer-file-name-with-indirect) :contents ,(ensime-get-buffer-as-string)))

(defun ensime--dependencies-newer-than-target-p (target-file dep-files-list)
  (if (file-exists-p target-file)
      (let ((target-mtime (nth 5 (file-attributes target-file))))
        (some (lambda (d)
                (time-less-p target-mtime (nth 5 (file-attributes d))))
              dep-files-list))
    t))

(defun ensime--join-paths (base &rest paths)
  (if paths
      (apply
       'ensime--join-paths
       (concat (file-name-as-directory base) (first paths))
       (rest paths))
    base))

(defun ensime--build-classpath (paths)
  "Build a classpath string from a list of paths"
  (mapconcat #'identity paths ensime--classpath-separator))

;; Commonly used functions

(defun ensime-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function.
This idiom is preferred over `lexical-let'."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun ensime-rcurry (fun &rest args)
  "Like `ensime-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))

(defun ensime-recompile-el ()
  "Byte-recompilation of all Emacs Lisp files."
  (interactive)
  (byte-recompile-directory ensime-dir 0))

(defun ensime-flatten-list (list)
  ;;(ensime-flatten-list '((a) b c (d e (q) f g)))
  (mapcan (lambda (x)
	    (if (listp x)
		(ensime-flatten-list x)
	      (list x))) list))

(defun ensime-escape-control-chars (s)
  "Return a copy of s with control characters
 escaped."
  ;; TODO:
  ;; Can't get this to work with single regexp - hence shitty
  ;; iterative version. Maybe Emacs bug.
  (let ((s s))
    (setq s (replace-regexp-in-string "\n" "\\n" s nil t))
    (setq s (replace-regexp-in-string "\t" "\\t" s nil t))
    (setq s (replace-regexp-in-string "\r" "\\r" s nil t))
    (setq s (replace-regexp-in-string "\b" "\\r" s nil t))
    s))

(defun ensime-replace-keywords (template proplist)
  "Replace keywords in the template list with the associated
 values in the provided proplist."
  (let* ((result '()))
    (dolist (ea template)
      (cond
       ((keywordp ea)
        (let ((val (plist-get proplist ea)))
          (setq result (if (listp val)
                           (append (reverse val) result)
                         (cons val result)))))
       (t
	(setq result (cons ea result)))))
    (reverse result)))

(defun ensime-last-name-component (str)
  (if (integerp (string-match "^.*?\\.\\([^\\.]+\\)$" str))
      (match-string 1 str)
    str))

(defun ensime-short-local-name (local-name)
  (if (integerp (string-match "^\\(.*\\$\\)?\\([^$]+\\)\\$?$" local-name))
      (match-string 2 local-name)
    local-name))

(defun ensime-strip-dollar-signs (str)
  (replace-regexp-in-string
   "\\$+" "."
   (replace-regexp-in-string "\\$$" "" str)))

;; Portability

(defun ensime-computed-point ()
  "Subtract one to convert to 0-indexed buffer offsets.
 Additionally, in buffers with windows-encoded line-endings,
 add the appropriate number of CRs to compensate for characters
 that are hidden by Emacs."
  (ensime-externalize-offset (point)))

(defun ensime-computed-range ()
  (if (and transient-mark-mode mark-active)
      (list
       (ensime-externalize-offset (min (mark) (point)))
       (ensime-externalize-offset (max (mark) (point))))
    (list (ensime-computed-point) (ensime-computed-point))))

(defun ensime-externalize-offset (offset)
  (+ offset (- ensime-ch-fix)
     (if (eq 1 (coding-system-eol-type buffer-file-coding-system))
         (save-restriction
           (widen)
           (1- (line-number-at-pos offset)))
       0)
     ))

(defun ensime-internalize-offset (offset)
  (if (eq 1 (coding-system-eol-type buffer-file-coding-system))
      (save-excursion
        (save-restriction
          (widen)
          (block nil
            (when (<= offset 0) (return 1))
            (when (>= offset (ensime-externalize-offset (point-max)))
              (return (point-max)))

            (goto-char offset)
            (while t
              (let* ((diff (- (ensime-externalize-offset (point)) offset))
                     (step (/ (abs diff) 2)))
                (cond
                 ((eql diff 0) (return (point)))

                 ;; Treat -1 and +1 specially: if offset matches a CR character
                 ;; we want to avoid an infinite loop
                 ((eql diff -1) (if (eql (char-after (point)) ?\n)
                                    (return (point))
                                  (return (1+ (point)))))
                 ((eql diff 1)  (return (1- (point))))

                 ((> diff 0) (backward-char step))
                 ((< diff 0) (forward-char step))))))))
    (+ offset ensime-ch-fix)))

(defun ensime-internalize-offset-at-line (offset line-end-offset line)
  "Return the internal offset of OFFSET, given that the offset is at line
number LINE, and the end of the line has external offset LINE-END-OFFSET.
This function may be faster than `ensime-internalize-offset'"
  (if (eq 1 (coding-system-eol-type buffer-file-coding-system))
      (if (eql offset line-end-offset)
          (- offset (- ensime-ch-fix) line)
        (- offset (- ensime-ch-fix) (1- line)))
    (+ offset ensime-ch-fix)))

(defun ensime-external-offsets-to-lines ()
  "For the current buffer, return an alist that associates the external
offset of each line's last character, to the line number"
  (save-excursion
    (save-restriction
      (widen)
      (let ((lines-map nil)
            (line-num 1)
            (line-factor
             (if (eq 1 (coding-system-eol-type buffer-file-coding-system)) 1 0)))
        (goto-char (point-min))
        (end-of-line)
        (while (< (point) (point-max))
          (push (cons (+ (point) (- ensime-ch-fix) (* line-factor line-num))
                      line-num)
                lines-map)
          (forward-line 1)
          (end-of-line)
          (incf line-num))
        (unless (eql (line-beginning-position) (line-end-position))
          (push (cons (+ (point) (- ensime-ch-fix) (* line-factor line-num))
                      line-num)
                lines-map))
        (nreverse lines-map)))))

(defun ensime-get-line-for-external-offset (offset-lines offset)
  (while (and offset-lines
              (> offset (caar offset-lines))
              (cdr offset-lines))
    (setf offset-lines (cdr offset-lines)))
  offset-lines)

(defun ensime-internalize-offset-for-file (file-name offset)
  (let ((buf (find-buffer-visiting file-name)))
    (if buf
        (with-current-buffer buf
           (ensime-internalize-offset offset))
      (with-temp-buffer
        (insert-file-contents file-name)
        (ensime-internalize-offset offset)))))

(defun ensime-internalize-offset-fields (plist &rest keys)
  (dolist (key keys)
    (setq plist (plist-put
		 plist key
		 (ensime-internalize-offset
		  (plist-get plist key)))))
  plist)

(defun ensime-get-buffer-as-string ()
  (save-restriction
    (widen)
    (let ((contents
           (buffer-substring-no-properties (point-min) (point-max))))
      (when (eq 1 (coding-system-eol-type buffer-file-coding-system))
        (setq contents (replace-regexp-in-string "\n" "\r\n" contents)))
      contents)))

;; Interface

(defun ensime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
	(if (fboundp 'temp-minibuffer-message) ;; XEmacs
	    (temp-minibuffer-message text)
	  (minibuffer-message text))
      (message "%s" text))))

(defun ensime-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply ensime-message-function format args))

(defun ensime-display-warning (message &rest args)
  (display-warning '(ensime warning) (apply #'format message args)))


(defun ensime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `ensime-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply ensime-background-message-function format-string format-args))

(defun ensime-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (ensime-oneliner msg)))))

(defun ensime-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
			   (or (position ?\n string) most-positive-fixnum)
			   (1- (frame-width)))))


;; Interface Helpers

(defun ensime-add-face (face string)
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

(defsubst ensime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (ensime-propertize-region props (apply #'insert args)))

(defun ensime-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
		  (progn
		    (insert-before-markers indent)
		    (zerop (forward-line -1))))))))

(defun ensime-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (ensime-with-rigid-indentation nil
    (apply #'insert strings)))

(defun ensime-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun ensime-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun ensime-in-comment-p (pos)
  "A helper to determine if the text at point is in comment.
   TODO: Currently this relies on font-lock-mode."
  (let ((face (plist-get (text-properties-at pos) 'face)))
    (and face
     (or
      (equal face 'font-lock-doc-face)
      (equal face 'font-lock-comment-face)))))

(defun ensime-at-bol-p ()
  (not (string-match "[^\s-]" (buffer-substring-no-properties
			       (point-at-bol)
			       (point)))))

(defun ensime-in-string-or-comment-p (pos)
  "A helper to determine if the text at point is in a string
   or comment, and therefore should not be considered as part
   of a paren-balancing calculation.

   TODO: Currently this relies on font-lock-mode. Could be
   better."
  (let ((face (plist-get (text-properties-at pos) 'face)))
    (and face
     (or
      (equal face 'font-lock-doc-face)
      (equal face 'font-lock-string-face)
      (equal face 'font-lock-comment-face)))))

(defun ensime-pt-at-end-of-prev-line ()
  (save-excursion (forward-line -1)
		  (min
		   (- (point) 1)
		   (point-at-eol))))

;; Testing helpers

(defun ensime-event-sig (event &optional value)
  "Signal an event. Send to testing harness if it exists.
   Used to drive asynchronous regression tests."
  (if (fboundp 'ensime-test-sig)
      (ensime-test-sig event value)))


(provide 'ensime-util)

;; Local Variables:
;; End:

;;; ensime-notes.el --- Compiler Notes (Error/Warning overlays)

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'dash)
(require 'cl-lib)
(require 'ensime-overlay)

;; Note: This might better be a connection-local variable, but
;; afraid that might lead to hanging overlays..

(defvar ensime-note-overlays '()
  "The overlay structures created to highlight notes.")

(defun ensime-all-notes ()
  (append (ensime-scala-compiler-notes (ensime-connection))
	  (ensime-java-compiler-notes (ensime-connection))))


(defun ensime-add-notes (lang result)
  (let ((is-full (plist-get result :is-full))
	(notes (plist-get result :notes)))
    (cond
     ((equal lang 'scala)
      (setf (ensime-scala-compiler-notes (ensime-connection))
	    (append
	     (ensime-scala-compiler-notes (ensime-connection))
	     notes)))

     ((equal lang 'java)
      (setf (ensime-java-compiler-notes (ensime-connection))
	    (append
	     (ensime-java-compiler-notes (ensime-connection))
	     notes))))

    (ensime-make-note-overlays notes)
    (ensime-update-note-counts)
    (ensime-event-sig :notes-added)
    ))


(defun ensime-clear-notes (lang)
  (cond
   ((equal lang 'scala)
    (setf (ensime-scala-compiler-notes (ensime-connection)) nil))
   ((equal lang 'java)
    (setf (ensime-java-compiler-notes (ensime-connection)) nil)))
  (ensime-clear-note-overlays lang)
  (ensime-update-note-counts))


(defun ensime-make-overlay-at (file line b e msg visuals)
  "Create an overlay highlighting the given line in
any buffer visiting the given file."
  (let ((beg b)
        (end e))
    (assert (or (integerp line)
                (and (integerp beg) (integerp end))))
    (-when-let (buf (find-buffer-visiting file))
      (with-current-buffer buf
        (if (and (integerp beg) (integerp end))
            (progn
              (setq beg (ensime-internalize-offset beg))
              (setq end (ensime-internalize-offset end)))
          ;; If line provided, use line to define region
          (save-excursion
            (goto-line line)
            (setq beg (point-at-bol))
            (setq end (point-at-eol)))))

      (ensime-make-overlay beg end msg visuals nil buf))
    ))


(defun ensime-make-note-overlays (notes)
  (dolist (note notes)
    (destructuring-bind
        (&key severity msg beg end line col file &allow-other-keys) note

      ;; No empty note overlays!
      (when (eq beg end)
        (setq end (+ end 1)))

      (let ((lang
             (cond
              ((ensime-java-file-p file) 'java)
              ((ensime-scala-file-p file) 'scala)
              (t 'scala)))
            (visuals
             (cond
              ((equal severity 'error)
               (list :face 'ensime-errline-highlight
		     :char "!"
		     :bitmap 'exclamation-mark
		     :fringe 'ensime-compile-errline))
              (t
               (list :face 'ensime-warnline-highlight
		     :char "?"
		     :bitmap 'question-mark
		     :fringe 'ensime-compile-warnline)))))

        (-when-let (ov (ensime-make-overlay-at file line beg end msg visuals))
          (overlay-put ov 'lang lang)
          (push ov ensime-note-overlays))

        ))))


(defun ensime-update-note-counts ()
  (let ((notes (ensime-all-notes))
	(num-err 0)
	(num-warn 0)
	(conn (ensime-connection)))
    (dolist (note notes)
      (let ((severity (plist-get note :severity)))
	(cond
	 ((equal severity 'error)
	  (incf num-err))
	 ((equal severity 'warn)
	  (incf num-warn))
	 (t))))
    (setf (ensime-num-errors conn) num-err)
    (setf (ensime-num-warnings conn) num-warn)))


(defun ensime-refresh-all-note-overlays ()
  (let ((notes (when (ensime-connected-p)
		   (append
		    (ensime-java-compiler-notes (ensime-connection))
		    (ensime-scala-compiler-notes (ensime-connection)))
		 )))
    (ensime-clear-note-overlays)
    (ensime-make-note-overlays notes)
    ))

(defface ensime-errline-highlight
  '((t (:inherit flymake-errline)))
  "Face used for marking the specific region of an error, if available."
  :group 'ensime-ui)

(defface ensime-warnline-highlight
  '((t (:inherit flymake-warnline)))
  "Face used for marking the specific region of an warning, if available."
  :group 'ensime-ui)

(defface ensime-implicit-highlight
  (if (facep 'flymake-infoline)
      '((t (:inherit flymake-infoline)))
  '((((supports :underline (:style line)))
     :underline (:style line :color "light gray"))
    (t :inherit flymake-warnline)))
  "Face used for marking a region where an implicit conversion was applied."
  :group 'ensime-ui)

(defun ensime-make-overlay (beg end tooltip-text visuals &optional mouse-face buf)
  "Allocate a ensime overlay in range BEG and END."
  (let ((ov (make-overlay beg end buf t t)))
    (overlay-put ov 'face           (plist-get visuals :face))
    (overlay-put ov 'mouse-face     mouse-face)
    (overlay-put ov 'help-echo      tooltip-text)
    (overlay-put ov 'ensime-overlay  t)
    (overlay-put ov 'priority 100)
    (let ((char (plist-get visuals :char)))
      (if (window-system)
          (when char
            (overlay-put ov 'before-string
                         (propertize char
                                     'display
                                     (list 'left-fringe
                                           (plist-get visuals :bitmap)
                                           (plist-get visuals :fringe)))))
        (when (and char ensime-left-margin-gutter)
          (ensime-show-sign-overlay char (plist-get visuals :fringe) ov))))
    ov))

(defun ensime-show-sign-overlay (sign face ov)
  (save-excursion
    (overlay-put ov 'before-string (ensime-before-string sign face))))

(defun ensime-before-string (sign face)
  (propertize " " 'display `((margin left-margin)
                             ,(propertize sign 'face
                                          (face-remap-add-relative face
                                                                   :underline nil
                                                                   :weight 'normal
                                                                   :slant 'normal)))))

(defun ensime-set-left-window-margin (width)
  (let ((curwin (get-buffer-window)))
    (set-window-margins curwin width (cdr (window-margins curwin)))))

(defun ensime-show-left-margin-hook ()
  "Shows the left margin. This function is called by
 window-configuration-change-hook."
  (when (and
         (not window-system)
         ensime-left-margin-gutter)
    (ensime-set-left-window-margin 1)))

(defun ensime-overlays-at (point)
  "Return list of overlays of type 'ensime-overlay at point."
  (let ((ovs (overlays-at point)))
    (remove-if-not
     (lambda (ov) (overlay-get ov 'ensime-overlay))
     ovs)
    ))

(defun ensime-clear-note-overlays (&optional lang)
  "Delete note overlays language. If lang is nil, delete all
 overlays."
  (let ((revised '()))
    (dolist (ov ensime-note-overlays)
      (if (or (null lang)
	      (equal lang (overlay-get ov 'lang)))
	  (delete-overlay ov)
	(setq revised (cons ov revised))))
    (setq ensime-note-overlays revised)))

(defun ensime-next-note-in-current-buffer (notes forward)
  (let ((best-note nil)
	(best-dist most-positive-fixnum)
        (external-offset (ensime-externalize-offset (point)))
        (max-external-offset (ensime-externalize-offset (point-max))))
    (dolist (note notes)
      (if (and (ensime-files-equal-p (ensime-note-file note)
				     (buffer-file-name-with-indirect))
	       (/= (ensime-note-beg note) external-offset))
	  (let ((dist (cond
		       (forward
			(if (< (ensime-note-beg note) external-offset)
			    (+ (ensime-note-beg note)
			       (- max-external-offset external-offset))
			  (- (ensime-note-beg note) external-offset)))

		       (t (if (> (ensime-note-beg note) external-offset)
			      (+ external-offset (- max-external-offset
                                                    (ensime-note-beg note)))
			    (- external-offset (ensime-note-beg note)))))))

	    (when (< dist best-dist)
	      (setq best-dist dist)
	      (setq best-note note))
	    )))
    best-note))

(defun ensime-goto-next-note (forward)
  "Helper to move point to next note. Go forward if forward is non-nil."
  (let* ((conn (ensime-connection))
	 (notes (append (ensime-java-compiler-notes conn)
			(ensime-scala-compiler-notes conn)))
	 (next-note (ensime-next-note-in-current-buffer notes forward)))
    (if next-note
	(progn
	  (goto-char (ensime-internalize-offset (ensime-note-beg next-note)))
	  (message (ensime-note-message next-note)))
      (message (concat
                "No more compilation issues in this buffer.")))))

(defun ensime-forward-note ()
  "Goto the next compilation note in this buffer"
  (interactive)
  (ensime-goto-next-note t))

(defun ensime-backward-note ()
  "Goto the prev compilation note in this buffer"
  (interactive)
  (ensime-goto-next-note nil))

(defun ensime-errors-at (point)
  (delq nil (mapcar (lambda (x) (overlay-get x 'help-echo)) (ensime-overlays-at point))))

(defun ensime-print-errors-at-point (&optional arg)
  (interactive "P")
  (let ((msgs (append (ensime-errors-at (point))
                      (ensime-implicit-notes-at (point)))))
    (when msgs
      (let ((msg (mapconcat 'identity msgs "\n")))
        (when (equal arg '(16))
          (ensime--make-result-overlay
              (format "%S" msg)
            :where (point)
            :duration 'command))
        (message "%s" msg)))
    (ensime-event-sig :errors-at-point-printed)))

(defun ensime-implicit-notes-at (point)
  (cl-labels
      ((format-body (s)
         (let ((lines (split-string s "\n")))
           (if (>= (length lines) 5)
               (mapconcat 'identity
                          (list* (first lines) (second lines) "..." (last lines 2))
                          "\n")
             s)))
       (format-param (p)
         (let ((name (propertize (plist-get p :name) 'face 'font-lock-variable-name-face))
               (type (propertize (ensime-type-full-name-with-args (plist-get p :type))
                                 'face 'font-lock-type-face)))
           (concat name ": " type)))
       (implicit-infos-ending-on-line (point)
         (save-excursion
           (goto-char point)
           (let ((infos (ensime-rpc-implicit-info-in-range (point-at-bol) (point-at-eol)))
                 (begin (ensime-externalize-offset (point-at-bol)))
                 (end (ensime-externalize-offset (point-at-eol))))
             (remove-if #'(lambda (info)
                            (or (< (plist-get info :end) begin)
                                (> (plist-get info :end) end)))
                        infos))))
       (info-string (i)
         (let* ((type (plist-get i :type))
                (substr (propertize (buffer-substring-no-properties
                                     (ensime-internalize-offset (plist-get i :start))
                                     (ensime-internalize-offset (plist-get i :end)))
                                    'face 'font-lock-variable-name-face))
                (body (format-body substr))
                (fun (plist-get i :fun))
                (fun-name (propertize (plist-get fun :name)
                                      'face 'font-lock-function-name-face))
                (fun-type (propertize
                           (ensime-type-full-name-with-args (plist-get fun :type))
                           'face 'font-lock-type-face)))
           (cond
            ((eq type 'conversion)
             (format "Implicit conversion of %s using %s: %s" body fun-name fun-type))

            ((eq type 'param)
             (let* ((params (mapcar #'format-param (plist-get i :params)))
                    (param-list (mapconcat #'identity params ", "))
                    (fun-is-implicit (plist-get i :fun-is-implicit)))
               (if fun-is-implicit
                   (format "Implicit parameters added to call of %s(%s): (%s)" fun-name body param-list)
                 (format "Implicit parameters added to call of %s: (%s)" body param-list))))))))

    (delq nil (mapcar #'info-string (implicit-infos-ending-on-line point)))))


(provide 'ensime-notes)

;; Local Variables:
;; End:

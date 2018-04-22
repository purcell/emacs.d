;;; skewer-html.el --- skewer support for live-interaction HTML -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This minor mode provides functionality for HTML like plain Skewer
;; does for JavaScript. There's no clean way to replace the body and
;; head elements of a live document, so "evaluating" these elements is
;; not supported.

;; * C-M-x   -- `skewer-html-eval-tag'

;; See also `skewer-html-fetch-selector-into-buffer' for grabbing the
;; page as it current exists.

;;; Code:

(require 'cl-lib)
(require 'sgml-mode)
(require 'skewer-mode)

;; Macros

(defmacro skewer-html--with-html-mode (&rest body)
  "Evaluate BODY as if in `html-mode', using a temp buffer if necessary."
  (declare (indent 0))
  (let ((orig-buffer (make-symbol "orig-buffer"))
        (temp-buffer (make-symbol "temp-buffer"))
        (orig-point  (make-symbol "orig-point")))
    `(let ((,temp-buffer (and (not (eq major-mode 'html-mode))
                              (generate-new-buffer " *skewer-html*")))
           (,orig-buffer (current-buffer))
           (,orig-point (point)))
       (unwind-protect
           (with-current-buffer (or ,temp-buffer ,orig-buffer)
             (when ,temp-buffer
               (insert-buffer-substring ,orig-buffer)
               (setf (point) ,orig-point)
               (html-mode))
             ,@body)
         (when ,temp-buffer
           (kill-buffer ,temp-buffer))))))

;; Selector computation

(defun skewer-html--cleanup (tag)
  "Cleanup TAG name from sgml-mode."
  (skewer-html--with-html-mode
    (replace-regexp-in-string "/$" "" (sgml-tag-name tag))))

(defun skewer-html--tag-after-point ()
  "Return the tag struct for the tag immediately following point."
  (skewer-html--with-html-mode
    (save-excursion
      (forward-char 1)
      (sgml-parse-tag-backward))))

(defun skewer-html--get-context ()
  "Like `sgml-get-context' but to the root, skipping close tags."
  (skewer-html--with-html-mode
    (save-excursion
      (cl-loop for context = (sgml-get-context)
               while context
               nconc (nreverse context) into tags
               finally return (cl-delete 'close tags :key #'sgml-tag-type)))))

(cl-defun skewer-html-compute-tag-nth (&optional (point (point)))
  "Compute the position of this tag within its parent."
  (skewer-html--with-html-mode
    (save-excursion
      (setf (point) point)
      (let ((context (skewer-html--get-context)))
        (when context
          (let ((tag-name (skewer-html--cleanup (car context)))
                (target-depth (1- (length context))))
            (cl-loop with n = 0
                     ;; If point doesn't move, we're at the root.
                     for point-start = (point)
                     do (sgml-skip-tag-backward 1)
                     until (= (point) point-start)
                     ;; If depth changed, we're done.
                     for current-depth = (length (skewer-html--get-context))
                     until (< current-depth target-depth)
                     ;; Examine the sibling tag.
                     for current-name = (save-excursion
                                          (forward-char)
                                          (sgml-parse-tag-name))
                     when (equal current-name tag-name)
                     do (cl-incf n)
                     finally return n)))))))

(defun skewer-html-compute-tag-ancestry ()
  "Compute the ancestry chain at point."
  (skewer-html--with-html-mode
    (nreverse
     (cl-loop for tag in (skewer-html--get-context)
              for nth = (skewer-html-compute-tag-nth (1+ (sgml-tag-start tag)))
              for name = (skewer-html--cleanup tag)
              unless (equal name "html")
              collect (list name nth)))))

(defun skewer-html-compute-selector ()
  "Compute the selector for exactly the tag around point."
  (let ((ancestry (skewer-html-compute-tag-ancestry)))
    (mapconcat (lambda (tag)
                 (format "%s:nth-of-type(%d)" (cl-first tag) (cl-second tag)))
               ancestry " > ")))

;; Fetching

(defun skewer-html-fetch-selector (selector)
  "Fetch the innerHTML of a selector."
  (let ((result (skewer-eval-synchronously selector :type "fetchselector")))
    (if (skewer-success-p result)
        (cdr (assoc 'value result))
      "")))

(defun skewer-html-fetch-selector-into-buffer (selector)
  "Fetch the innerHTML of a selector and insert it into the active buffer."
  (interactive "sSelector: ")
  (insert (skewer-html-fetch-selector selector)))

;; Evaluation

(defun skewer-html-eval (string ancestry &optional append)
  "Load HTML into a selector, optionally appending."
  (let ((ancestry* (cl-coerce ancestry 'vector)))  ; for JSON
    (skewer-eval string nil :type "html" :extra `((ancestry . ,ancestry*)
                                                  (append   . ,append)))))

(defun skewer-html-eval-tag ()
  "Load HTML from the immediately surrounding tag."
  (interactive)
  (let ((ancestry (skewer-html-compute-tag-ancestry)))
    (save-excursion
      ;; Move to beginning of opening tag
      (let* ((beg (skewer-html--with-html-mode
                    (sgml-skip-tag-forward 1) (point)))
             (end (skewer-html--with-html-mode
                    (sgml-skip-tag-backward 1) (point)))
             (region (buffer-substring-no-properties beg end)))
        (skewer-flash-region beg end)
        (if (= (length ancestry) 1)
            (error "Error: cannot eval body and head tags.")
          (skewer-html-eval region ancestry nil))))))

;; Minor mode definition

(defvar skewer-html-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-M-x") 'skewer-html-eval-tag)))
  "Keymap for skewer-html-mode")

;;;###autoload
(define-minor-mode skewer-html-mode
  "Minor mode for interactively loading new HTML."
  :lighter " skewer-html"
  :keymap skewer-html-mode-map
  :group 'skewer)

(provide 'skewer-html)

;;; skewer-html.el ends here

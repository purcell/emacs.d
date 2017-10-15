;;; tern.el --- Tern-powered JavaScript integration -*- lexical-binding: t -*-

;; Author: Marijn Haverbeke
;; URL: http://ternjs.net/
;; Package-Version: 20170925.1333
;; Version: 0.0.1
;; Package-Requires: ((json "1.2") (cl-lib "0.5") (emacs "24"))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(defvar tern-known-port nil)
(defvar tern-server nil)
(defvar tern-explicit-port nil)
(defvar tern-project-dir nil)
(defvar tern-last-file-name nil)
(defvar tern-last-project-dir nil)

(defun tern-message (fmt &rest objects)
  (apply 'message fmt objects))

(defun tern-req (port doc c)
  (let* ((url-mime-charset-string nil) ; Suppress huge, useless header
         (url-request-method "POST")
         (deactivate-mark nil) ; Prevents json-encode from interfering with shift-selection-mode
         (url-request-data (encode-coding-string (json-encode doc) 'utf-8))
         (url-show-status nil)
         (url (url-parse-make-urlobj "http" nil nil tern-server port "/" nil nil nil))
         (url-current-object url))
    (url-http url #'tern-req-finished (list c))))

(defun tern-req-finished (c)
  (defvar url-http-process)
  (defvar url-callback-function)
  (let ((is-error (and (consp c) (eq (car c) :error)))
        (found-body (search-forward "\n\n" nil t))
        (deactivate-mark nil))
    (set-buffer-multibyte t)
    (if (or is-error (not found-body))
        (let ((message (and found-body
                            (buffer-substring-no-properties (point) (point-max))))
              (url-callback-function #'ignore))
          (delete-process url-http-process)
          (kill-buffer (current-buffer))
          (funcall (if is-error (cddr c) c)
                   (cons (and is-error (cadr c)) message) nil))
      (let ((json (json-read)))
        (delete-process url-http-process)
        (kill-buffer (current-buffer))
        (funcall c nil json)))))

(defun tern-project-dir ()
  (or (and (equal tern-last-file-name (buffer-file-name)) tern-project-dir)
      (and (not (buffer-file-name)) (setf tern-project-dir ""))
      (let ((project-dir (file-name-directory (buffer-file-name))))
        (cl-loop for cur = project-dir then (let ((shorter (file-name-directory (directory-file-name cur))))
                                              (and (< (length shorter) (length cur)) shorter))
                 while cur do
                 (when (file-exists-p (expand-file-name ".tern-project" cur))
                   (cl-return (setf project-dir cur))))
        (setf tern-project-dir project-dir)))
  ;; Track the file name to detect if it changed, which means the project
  ;; directory needs to be found again.
  (setf tern-last-file-name (buffer-file-name))
  tern-project-dir)

(defun tern-known-port ()
  ;; Invalidate the port when the project directory changes, since a new
  ;; directory may yield a new .tern-port file.
  (if (equal tern-last-project-dir (tern-project-dir))
      tern-known-port
    (setf tern-last-project-dir (tern-project-dir)
          tern-known-port nil)))

(defun tern-find-server (c &optional ignore-port)
  (cl-block nil
    (when (tern-known-port)
      (cl-return (if (consp (tern-known-port))
                     (funcall c nil (cdr (tern-known-port)))
                   (funcall c (tern-known-port) nil))))
    (if tern-explicit-port
        (funcall c tern-explicit-port nil)
      (let ((deactivate-mark nil)
            (port-file (expand-file-name ".tern-port" (tern-project-dir))))
        (when (file-exists-p port-file)
          (let ((port (string-to-number (with-temp-buffer
                                          (insert-file-contents port-file)
                                          (buffer-string)))))
            (unless (eq port ignore-port)
              (setf tern-known-port port)
              (cl-return (funcall c port nil))))))
      (tern-start-server c))))

(defvar tern-command
  (let* ((script-file (or load-file-name
                          (and (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))
         (bin-file (expand-file-name "../bin/tern" (file-name-directory (file-truename script-file)))))
    (if (file-exists-p bin-file)
        (if (eq system-type 'windows-nt) (list "node" bin-file) (list bin-file))
      (list "tern")))
  "The command to be run to start the Tern server. Should be a
list of strings, giving the binary name and arguments.")

(defun tern-start-server (c)
  (let* ((default-directory (tern-project-dir))
         (cmd (if (member "--strip-crs" tern-command) tern-command (append tern-command '("--strip-crs"))))
         (proc (apply #'start-process "Tern" nil cmd))
         (all-output ""))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc (lambda (_proc _event)
                                 (delete-process proc)
                                 (setf tern-known-port (cons :failed (concat "Could not start Tern server\n" all-output)))
                                 (run-at-time "30 sec" nil
                                              (lambda (buf)
                                                (with-current-buffer buf
                                                  (when (consp (tern-known-port)) (setf tern-known-port nil))))
                                              (current-buffer))
                                 (funcall c nil (tern-known-port))))
    (set-process-filter proc (lambda (proc output)
                               (if (not (string-match "Listening on port \\([0-9][0-9]*\\)" output))
                                   (setf all-output (concat all-output output))
                                 (setf tern-known-port (string-to-number (match-string 1 output)))
                                 (set-process-sentinel proc (lambda (proc _event)
                                                              (delete-process proc)
                                                              (setf tern-known-port nil)))
                                 (set-process-filter proc nil)
                                 (funcall c (tern-known-port) nil))))))

(defvar tern-command-generation 0)
(defvar tern-activity-since-command -1)
(defvar tern-last-point-pos nil)

(defvar tern-last-completions nil)
(defvar tern-last-argument-hints nil)
(defvar tern-buffer-is-dirty nil)

(defun tern-project-relative-file ()
  (if (buffer-file-name)
      (substring (buffer-file-name) (length (tern-project-dir)))
    (buffer-name)))

(defun tern-get-partial-file (at)
  (let* (min-indent start-pos end-pos
                    (min-pos (max 0 (- at 2000))))
    (save-excursion
      (goto-char at)
      (cl-loop
       (unless (re-search-backward "\\bfunction\\b" min-pos t) (cl-return))
       (let ((indent (current-indentation))
             (pos (line-beginning-position)))
         (when (or (not min-indent) (< indent min-indent))
           (setf min-indent indent))
         (goto-char pos)
         (when (<= pos min-pos) (cl-return))))
      (unless start-pos (goto-char min-pos) (setf start-pos (line-beginning-position))))
    (save-excursion
      (goto-char (min (+ at 1000) (point-max)))
      (let ((line-beg (line-beginning-position)))
        (setf end-pos (if (<= line-beg at) (line-end-position) line-beg))))
    `((type . "part")
      (name . ,(tern-project-relative-file))
      (offset . ,(1- start-pos))
      (text . ,(buffer-substring-no-properties start-pos end-pos)))))

(defun tern-modified-sibling-buffers ()
  (let (found)
    (dolist (buf (buffer-list))
      (when (and (not (eq buf (current-buffer)))
                 (buffer-local-value 'tern-mode buf)
                 (buffer-local-value 'tern-buffer-is-dirty buf)
                 (equal (tern-project-dir) (with-current-buffer buf (tern-project-dir))))
        (with-current-buffer buf
          (push `((type . "full")
                  (name . ,(tern-project-relative-file))
                  (text . ,(buffer-string))) found))))
    (nreverse found)))

(defun tern-run-request (f doc)
  (let ((buffer (current-buffer))
        (retrying nil)
        callback runner)
    (setf callback (lambda (port err)
                     (if port
                         (condition-case err
                             (tern-req port doc runner)
                           (error (funcall runner (list err) nil)))
                       (funcall f err nil))))
    (setf runner (lambda (err data)
                   (with-current-buffer buffer
                     (cond ((and err (not retrying)
                                 (or (eq (cl-cadar err) 'connection-failed)
                                     (eq (caar err) 'file-error)))
                            (setf retrying t)
                            (let ((old-port (tern-known-port)))
                              (setf tern-known-port nil)
                              (if tern-explicit-port
                                  (funcall callback nil err)
                                (tern-find-server callback old-port))))
                           (t (funcall f err data))))))
    (tern-find-server callback)))

(defun tern-run-query (f query pos &optional mode)
  (when (stringp query) (setf query `((type . ,query))))
  (let ((generation (cl-incf tern-command-generation))
        (doc `((query . ,query)))
        (files (and (eq mode :full-file) (tern-modified-sibling-buffers)))
        file-name
        (pos pos))
    (cond
     ((not tern-buffer-is-dirty) (setf file-name (tern-project-relative-file)))
     ((and (not (eq mode :full-file)) (> (buffer-size) 8000))
      (push (tern-get-partial-file pos) files)
      (setf file-name "#0")
      (cl-decf pos (cdr (assq 'offset (car files)))))
     (t
      (push `((type . "full") (text . ,(buffer-string)) (name . ,(tern-project-relative-file))) files)
      (setf file-name (tern-project-relative-file))))
    (when files (push `(files . ,(apply #'vector files)) doc))
    (push `(file . ,file-name) (cdr (assq 'query doc)))
    (push `(end . ,(1- pos)) (cdr (assq 'query doc)))
    (tern-run-request
     (lambda (err data)
       (when (< tern-activity-since-command generation)
         (cond ((not err)
                (dolist (file files)
                  (when (equal (cdr (assq 'type file)) "full")
                    (with-current-buffer (find-file-noselect (expand-file-name (cdr (assq 'name file)) (tern-project-dir)))
                      (setf tern-buffer-is-dirty nil))))
                (funcall f data))
               ((not (eq mode :silent)) (tern-message "Request failed: %s" err)))))
     doc)))

(defun tern-send-buffer-to-server ()
  (when (buffer-file-name)
    (tern-run-request (lambda (_err _data))
                      `((files . [((type . "full")
                                   (name . ,(tern-project-relative-file))
                                   (text . ,(buffer-string)))])))))

;; Completion

(defun tern-completion-at-point-fn ()
  (tern-run-query #'tern-do-complete '((type . "completions") (includeKeywords . t)) (point)))

(defun tern-completion-at-point ()
  (or (tern-completion-matches-last)
      ;; Do not return a closure, as calling car-safe (e.g. in
      ;; completion-at-point) on such an object returns 'closure
      ;; instead of nil.
      'tern-completion-at-point-fn))

(defun tern-do-complete (data)
  (let ((cs (cl-loop for elt across (cdr (assq 'completions data)) collect elt))
        (start (+ 1 (cdr (assq 'start data))))
        (end (+ 1 (cdr (assq 'end data)))))
    (setf tern-last-completions (list (buffer-substring-no-properties start end) start end cs))
    (let ((completion-in-region-mode-predicate nil))
      (completion-in-region start end cs))))

(defun tern-completion-matches-last ()
  (when tern-last-completions
    (cl-destructuring-bind (word start end list) tern-last-completions
      (and (<= end (point-max))
           (equal word (buffer-substring-no-properties start end))
           (if (= (point) end)
               (cdr tern-last-completions)
             (and (>= (point) end)
                  (<= (point) (+ end 50))
                  (string-match-p "^[a-zA-Z0-9_$]*$" (buffer-substring-no-properties end (point)))
                  (let ((new-word (buffer-substring-no-properties start (point))))
                    (list start (point)
                          (cl-loop for elt in list
                                   when (eq (compare-strings word 0 (length word) new-word 0 (length word)) t)
                                   collect elt)))))))))

;; Argument hints

(defvar tern-update-argument-hints-timer 500 "millisecond.")

(defvar tern-update-argument-hints-async nil
  "[internal] If non-nil, `tern-update-argument-hints' will be called later.")

(defun tern-update-argument-hints-async ()
  (when tern-update-argument-hints-async
    (cancel-timer tern-update-argument-hints-async))
  (setq tern-update-argument-hints-async
        (run-at-time
         (* 0.001 tern-update-argument-hints-timer) nil
         (lambda ()
           (condition-case err
               (tern-update-argument-hints)
             (t (message "tern-update-argument-hints : %S" err)))
           (setq tern-update-argument-hints-async nil)))))

(defun tern-update-argument-hints ()
  (let ((opening-paren (cadr (syntax-ppss))))
    (when (and opening-paren (equal (char-after opening-paren) ?\())
      (if (and tern-last-argument-hints (eq (car tern-last-argument-hints) opening-paren))
          (tern-show-argument-hints)
        (tern-run-query (lambda (data)
                          (let ((type (tern-parse-function-type data)))
                            (when type
                              (setf tern-last-argument-hints (cons opening-paren type))
                              (tern-show-argument-hints))))
                        `((type . "type")
                          (preferFunction . t))
                        opening-paren
                        :silent)))))

(defun tern-skip-matching-brackets (end-chars)
  (let ((depth 0) (end (+ (point) 500)))
    (cl-loop while (< (point) (point-max)) do
             (let ((next (char-after (point))))
               (cond
                ((and (<= depth 0) (cl-find next end-chars)) (cl-return t))
                ((or (eq next ?\)) (eq next ?\]) (eq next ?\})) (cl-decf depth))
                ((or (eq next ?\() (eq next ?\[) (eq next ?\{)) (cl-incf depth))
                ((> (point) end) (cl-return nil)))
               (forward-char)))))

(defun tern-parse-function-type (data)
  (let ((type (cdr (assq 'type data)))
        (name (or (cdr (assq 'exprName data)) (cdr (assq 'name data)) "fn"))
        (deactivate-mark nil))
    (when (string-match-p "^fn(" type)
      (with-temp-buffer
        (insert type)
        (goto-char 4)
        (let (args retval)
          (cl-loop until (eq (char-after (point)) ?\)) do
                   (let ((name (when (looking-at "\\([a-zA-Z0-9_$?]*\\):\\s-*")
                                 (goto-char (match-end 0))
                                 (match-string 1)))
                         (typestart (point)))
                     (tern-skip-matching-brackets '(?\) ?\,))
                     (push (cons name (buffer-substring typestart (point))) args))
                   (when (eq (char-after (point)) ?\,) (forward-char 2)))
          (when (looking-at ") -> ")
            (setf retval (buffer-substring (+ (point) 5) (point-max))))
          (list name (nreverse args) retval))))))

(defun tern-find-current-arg (start)
  (when (< (point) (+ start 500))
    (save-excursion
      (let ((cur-point (point)))
        (goto-char (1+ start))
        (cl-loop for i from 0 do
                 (let ((found-end (tern-skip-matching-brackets '(?\) ?\,))))
                   (when (>= (point) cur-point) (cl-return i))
                   (when (or (not found-end) (looking-at ")")) (cl-return nil))
                   (forward-char 1)))))))

(defun tern-show-argument-hints ()
  (cl-destructuring-bind (paren . type) tern-last-argument-hints
    (let ((parts ())
          (current-arg (tern-find-current-arg paren)))
      (cl-destructuring-bind (name args ret) type
        (push (propertize name 'face 'font-lock-function-name-face) parts)
        (push "(" parts)
        (cl-loop for arg in args for i from 0 do
                 (unless (zerop i) (push ", " parts))
                 (let ((name (or (car arg) "?")))
                   (push (if (eq i current-arg) (propertize name 'face 'highlight) name) parts))
                 (unless (equal (cdr arg) "?")
                   (push ": " parts)
                   (push (propertize (cdr arg) 'face 'font-lock-type-face) parts)))
        (push ")" parts)
        (when ret
          (push " -> " parts)
          (push (propertize ret 'face 'font-lock-type-face) parts)))
      (let (message-log-max)
        (tern-message (apply #'concat (nreverse parts)))))))

;; Refactoring ops

(defun tern-do-refactor (data)
  (let ((per-file ())
        (orig-buffer (current-buffer)))
    (cl-loop for change across (cdr (assq 'changes data)) do
             (let ((found (assoc-string (cdr (assq 'file change)) per-file)))
               (unless found (setf found (list (cdr (assq 'file change)))) (push found per-file))
               (push change (cdr found))))
    (cl-loop for (file . changes) in per-file do
             (setf changes (sort changes (lambda (a b) (> (cdr (assq 'start a)) (cdr (assq 'start b))))))
             (find-file (expand-file-name file (tern-project-dir)))
             (cl-loop for change in changes do
                      (let ((start (1+ (cdr (assq 'start change))))
                            (end (1+ (cdr (assq 'end change)))))
                        (delete-region start end)
                        (save-excursion
                          (goto-char start)
                          (insert (cdr (assq 'text change)))))))
    (switch-to-buffer orig-buffer)))

(defun tern-rename-variable (new-name)
  (interactive "MNew variable name: ")
  (tern-run-query #'tern-do-refactor `((type . "rename") (newName . ,new-name)) (point) :full-file))

;; Highlight references in scope

(defvar tern-flash-timeout 0.5 "Delay before the highlight overlay disappears.")

(defun tern-flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer tern-flash-timeout nil 'delete-overlay overlay)))

(defun tern-do-highlight (data)
  (cl-loop for ref across (cdr (assq 'refs data)) do
           (let ((file (cdr (assq 'file ref))))
             (when (string= buffer-file-name (expand-file-name file (tern-project-dir)))
               (let ((start (1+ (cdr (assq 'start ref))))
                     (end (1+ (cdr (assq 'end ref)))))
                 (tern-flash-region start end))))))

(defun tern-highlight-refs ()
  (interactive)
  (tern-run-query #'tern-do-highlight "refs" (point)))

;; Jump-to-definition

(defvar tern-find-definition-stack ())

(defun tern-show-definition (data)
  (let* ((file (cdr (assq 'file data)))
         (found (and file (setf file (expand-file-name (cdr (assq 'file data)) (tern-project-dir)))
                     (tern-find-position file data))))
    (if found
        (progn
          (push (cons (buffer-file-name) (point)) tern-find-definition-stack)
          (let ((too-long (nthcdr 20 tern-find-definition-stack)))
            (when too-long (setf (cdr too-long) nil)))
          (tern-go-to-position file found))
      (let ((url (cdr (assq 'url data))))
        (if url
            (browse-url url)
          (tern-message "No definition found."))))))

(defun tern-at-interesting-expression ()
  (if (member (get-text-property (point) 'face)
              '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-string-face))
      nil
    (let ((around (buffer-substring-no-properties (max 1 (1- (point))) (min (1+ (point)) (point-max)))))
      (string-match "\\sw\\|)\\|]\\|_" around))))

(defun tern-find-definition (&optional prompt-var)
  (interactive)
  (let ((varname (and (or prompt-var (not (tern-at-interesting-expression)))
                      (read-from-minibuffer "Variable: "))))
    (push-mark)
    (tern-run-query #'tern-show-definition `((type . "definition") (variable . ,varname)) (point))))

(defun tern-find-definition-by-name ()
  (interactive)
  (tern-find-definition t))

(defun tern-find-position (file data)
  (with-current-buffer (find-file-noselect file)
    (let* ((start (1+ (cdr (assq 'start data))))
           (cx-start (- start (cdr (assq 'contextOffset data))))
           (cx (cdr (assq 'context data)))
           (cx-end (+ cx-start (length cx))))
      (if (and (<= (point-max) cx-end) (equal (buffer-substring-no-properties cx-start cx-end) cx))
          start
        (let (nearest nearest-dist)
          (save-excursion
            (goto-char (point-min))
            (cl-loop
             (unless (search-forward cx nil t) (cl-return))
             (let* ((here (- (point) (length cx)))
                    (dist (abs (- cx-start here))))
               (when (or (not nearest-dist) (< dist nearest-dist))
                 (setf nearest here nearest-dist dist)))))
          (when nearest
            (+ nearest (- start cx-start))))))))

(defun tern-pop-find-definition ()
  (interactive)
  (when tern-find-definition-stack
    (cl-destructuring-bind (file . pos) (pop tern-find-definition-stack)
      (tern-go-to-position file pos))))

(defun tern-go-to-position (file pos)
  (find-file file)
  (goto-char (min pos (point-max)))
  (setf tern-last-point-pos (point)))

;; Query type

(defun tern-get-type ()
  (interactive)
  (tern-run-query (lambda (data) (tern-message (or (cdr (assq 'type data)) "Not found")))
                  "type"
                  (point)))

;; Display docs

(defvar tern-last-docs-url nil)
(defun tern-get-docs ()
  (interactive)
  (if (and tern-last-docs-url (eq last-command 'tern-get-docs))
      (progn
        (browse-url tern-last-docs-url)
        (setf tern-last-docs-url nil))
    (tern-run-query (lambda (data)
                      (let ((url (cdr (assq 'url data))) (doc (cdr (assq 'doc data))))
                        (cond (doc
                               (setf tern-last-docs-url url)
                               (tern-message doc))
                              (url
                               (browse-url url))
                              (t (tern-message "Not found")))))
                    "documentation"
                    (point))))

;; Connection management

;;;###autoload
(defun tern-use-server (port server)
  (interactive "nPort to connect to: \nsServer: ")
  (setf tern-explicit-port port)
  (setf tern-known-port nil)
  (setf tern-server (if (string= server "") "127.0.0.1" server)))

;; Mode plumbing

(defun tern-after-change (start end prev-length)
  "Track changes to the buffer."
  (if tern-buffer-is-dirty
      (setf tern-buffer-is-dirty (cons (min start (car tern-buffer-is-dirty))
                                       (max end   (cdr tern-buffer-is-dirty))))
    (setf tern-buffer-is-dirty (cons start end)))
  ;; Set this here in addition to `tern-post-command', since the buffer may have
  ;; changed before a command completes (e.g. in a `query-replace' session).
  ;; See issue #786.
  (setf tern-activity-since-command tern-command-generation))

(defvar tern-idle-time 2.5
  "The time Emacs is allowed to idle before updating Tern's representation of the file.")
(defvar tern-idle-timer nil
  "The timer on which `tern-reparse-on-idle' runs.")
(defun tern-reparse-on-idle ()
  "Do some mode plumbing and refresh tern's representation of the buffer."
  (when tern-mode
    (setf tern-last-point-pos nil)
    (when (and tern-last-argument-hints tern-buffer-is-dirty
               (<= (car tern-buffer-is-dirty) (car tern-last-argument-hints)))
      (setf tern-last-argument-hints nil))
    (when (and tern-last-completions tern-buffer-is-dirty
               (<= (car tern-buffer-is-dirty) (cadr tern-last-completions)))
      (setf tern-last-completions nil))
    (tern-send-buffer-to-server)))

(defun tern-post-command ()
  (unless (eq (point) tern-last-point-pos)
    (setf tern-last-point-pos (point))
    (setf tern-activity-since-command tern-command-generation)
    (tern-update-argument-hints-async)))

(defun tern-left-buffer ()
  (when (and tern-buffer-is-dirty (not (buffer-file-name (car (buffer-list)))))
    (setf tern-buffer-is-dirty nil)
    (let ((buffer-list-update-hook ()))
      (tern-send-buffer-to-server))))

(defvar tern-mode-keymap (make-sparse-keymap))
(define-key tern-mode-keymap [(meta ?.)] 'tern-find-definition)
(define-key tern-mode-keymap [(control meta ?.)] 'tern-find-definition-by-name)
(define-key tern-mode-keymap [(meta ?,)] 'tern-pop-find-definition)
(define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)
(define-key tern-mode-keymap [(control ?c) (control ?c)] 'tern-get-type)
(define-key tern-mode-keymap [(control ?c) (control ?d)] 'tern-get-docs)

;;;###autoload
(define-minor-mode tern-mode
  "Minor mode binding to the Tern JavaScript analyzer"
  nil
  " Tern"
  tern-mode-keymap
  (if tern-mode (tern-mode-enable) (tern-mode-disable)))

(defun tern-mode-enable ()
  (set (make-local-variable 'tern-known-port) nil)
  (set (make-local-variable 'tern-server) "127.0.0.1")
  (set (make-local-variable 'tern-explicit-port) nil)
  (set (make-local-variable 'tern-project-dir) nil)
  (set (make-local-variable 'tern-last-file-name) nil)
  (set (make-local-variable 'tern-last-project-dir) nil)
  (set (make-local-variable 'tern-last-point-pos) nil)
  (set (make-local-variable 'tern-last-completions) nil)
  (set (make-local-variable 'tern-last-argument-hints) nil)
  (set (make-local-variable 'tern-buffer-is-dirty) (and (buffer-modified-p) (cons (point-min) (point-max))))
  (make-local-variable 'completion-at-point-functions)
  (push 'tern-completion-at-point completion-at-point-functions)
  (add-hook 'after-change-functions 'tern-after-change nil t)
  (when (null tern-idle-timer)
    (setq tern-idle-timer
          (run-with-idle-timer tern-idle-time t #'tern-reparse-on-idle)))
  (add-hook 'post-command-hook 'tern-post-command nil t)
  (add-hook 'buffer-list-update-hook 'tern-left-buffer nil t)
  (tern-send-buffer-to-server))

(defun tern-mode-disable ()
  (setf completion-at-point-functions
        (remove 'tern-completion-at-point completion-at-point-functions))
  (unless (null tern-idle-timer)
    (cancel-timer tern-idle-timer)
    (setq tern-idle-timer nil))
  (remove-hook 'after-change-functions 'tern-after-change t)
  (remove-hook 'post-command-hook 'tern-post-command t)
  (remove-hook 'buffer-list-update-hook 'tern-left-buffer t))

(provide 'tern)

;;; tern.el ends here

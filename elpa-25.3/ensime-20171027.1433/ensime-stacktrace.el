;;; ensime-stacktrace.el - Paste buffer for stack traces

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'hideshow)
(require 'dash)

(defconst ensime-stacktrace-buffer-name-base "*ensime-stacktrace*")

(defvar ensime-stacktrace-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'ensime-stacktrace-highlight)
    (define-key map (kbd "C-c C-f") 'ensime-stacktrace-fold-buffer)
    (define-key map (kbd "C-c C-p") 'hs-show-block)
    (define-key map (kbd "C-c C-q") 'quit-window)
    map)
  "Keymap for `ensime-stacktrace-buffer-mode'.")

(defvar ensime-stacktrace-foldable-stackframes '("at akka\\.*"
                                                 "at scala\\.*"
                                                 "at java\\.*"
                                                 "at jline\\.*"
                                                 "at sbt\\.*"
                                                 "at xsbt\\.*"))

(define-minor-mode ensime-stacktrace-buffer-mode
  "Mode for highlighting stack traces"
  nil
  nil
  ensime-stacktrace-buffer-map)

(defun ensime-stacktrace-build-buffer-name ()
  "Return the name of the project-specific stacktrace buffer."
  (format "%s<%s>"
          ensime-stacktrace-buffer-name-base
          (plist-get (ensime-config) :project-name)))

(defun ensime-stacktrace-switch ()
  "Switch to buffer containing the stack trace parser"
  (interactive)
  (let ((stacktrace-buf-name (ensime-stacktrace-build-buffer-name)))
    (unless (equal stacktrace-buf-name (buffer-name))
      (ensime-with-conn-interactive
       conn
       (let ((buf (get-buffer-create stacktrace-buf-name)))
         (switch-to-buffer-other-window buf)
         (setq ensime-buffer-connection conn)
         (ensime-stacktrace-buffer-mode 1)
         (setq comment-start ";")
         (setq comment-end "$")
         (hs-minor-mode 1)
         (font-lock-mode 1)
         (when (= (buffer-size buf) 0)
           (insert ";; Stack trace buffer\n")
           (insert ";; Paste a stack trace below and press `C-c C-c' to create links to source code.\n")
           (insert ";; Press `C-c C-f' to fold all defined stackframes\n")
           (insert ";; Press `C-c C-p' to unfold region that you're currently on\n")
           (insert ";; Press `C-c C-q' to leave this buffer\n")
           (insert "\n"))
        (local-set-key (kbd "C-c C-c") 'ensime-stacktrace-highlight)
        (local-set-key (kbd "C-c C-q") 'quit-window)
        (current-buffer))))))

(defun ensime-stacktrace-highlight ()
  (interactive)
  "Parse the current buffer and look for lines that looks for a stack trace.
Create links to the source code."
  (set-text-properties (point-min) (point-max) nil)
  (ensime-inf-highlight-stack-traces (point-min) (point-max)))


(defun ensime-stacktrace-pick-lines-to-fold (foldable-stackframes)
  (save-excursion
    (goto-char (point-min))
    (goto-char (point-at-bol))
    (let ((lines-to-fold '()))
      (while
          (search-forward-regexp
           "^[ \t]+at .+(.+)[ \t]*$"
           (point-max)
           t)
        (let ((stackframe (buffer-substring (point-at-bol) (point-at-eol))))
          (when (--any? (s-matches? it stackframe) foldable-stackframes)
            (add-to-list 'lines-to-fold (line-number-at-pos)))))
      lines-to-fold)))

(defun ensime-stacktrace-group-lines-to-fold (lines)
  (let (groups)
    (dolist (line lines groups)
      (let (current-group (car groups))
        (if (-contains-p (car groups) (+ line 1))
            (setq groups (append (list (append (list line) (car groups))) (cdr groups)))
          (setq groups (append (list (list line)) groups)))))))

(defun ensime-stacktrace-fold-lines (lines)
  (save-excursion
    (let ((first-line (car lines)))
      (goto-line first-line)
      (hs-hide-comment-region (point-at-bol) (point-at-eol (- (car (last lines)) (- first-line 1)))))))

(defun ensime-stacktrace-fold-buffer ()
  (interactive)
  "Folds all stackframe lines in buffer that starts with
package present in ENSIME-STACKFRAME-FOLDABLE-STACKFRAMES"
  (hs-show-all)
  (let* ((lines (ensime-stacktrace-pick-lines-to-fold ensime-stacktrace-foldable-stackframes))
         (groups (ensime-stacktrace-group-lines-to-fold lines)))
    (while groups
      (print (car groups))
      (ensime-stacktrace-fold-lines (car groups))
      (setq groups (cdr groups)))))

(provide 'ensime-stacktrace)

;; Local Variables:
;; End:

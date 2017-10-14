(require 'slime)
(require 'cl-lib)
(eval-when-compile (require 'cl)) ; lexical-let*

(define-slime-contrib slime-sprof
  "Integration with SBCL's sb-sprof."
  (:authors "Juho Snellman"
            "Stas Boukarev")
  (:license "MIT")
  (:swank-dependencies swank-sprof)
  (:on-load
   (let ((C '(and (slime-connected-p)
              (equal (slime-lisp-implementation-type) "SBCL"))))
     (setf (cdr (last (assoc "Profiling" slime-easy-menu)))
           `("--"
             [ "Start sb-sprof"  slime-sprof-start ,C ]
             [ "Stop sb-sprof"   slime-sprof-stop ,C ]
             [ "Report sb-sprof" slime-sprof-report ,C ])))))

(defvar slime-sprof-exclude-swank nil
  "*Display swank functions in the report.")

(define-derived-mode slime-sprof-browser-mode fundamental-mode
  "slprof"
  "Mode for browsing profiler data\
\\<slime-sprof-browser-mode-map>\
\\{slime-sprof-browser-mode-map}"
  :syntax-table lisp-mode-syntax-table
  (setq buffer-read-only t))

(set-keymap-parent slime-sprof-browser-mode-map slime-parent-map)

(slime-define-keys slime-sprof-browser-mode-map
  ("h" 'describe-mode)
  ("d" 'slime-sprof-browser-disassemble-function)
  ("g" 'slime-sprof-browser-go-to)
  ("v" 'slime-sprof-browser-view-source)
  ("s" 'slime-sprof-toggle-swank-exclusion)
  ((kbd "RET") 'slime-sprof-browser-toggle))

;; Start / stop profiling

(cl-defun slime-sprof-start (&optional (mode :cpu))
  (interactive)
  (slime-eval `(swank:swank-sprof-start :mode ,mode)))

(defun slime-sprof-start-alloc ()
  (interactive)
  (slime-sprof-start :alloc))

(defun slime-sprof-start-time ()
  (interactive)
  (slime-sprof-start :time))

(defun slime-sprof-stop ()
  (interactive)
  (slime-eval `(swank:swank-sprof-stop)))

;; Reporting

(defun slime-sprof-format (graph)
  (with-current-buffer (slime-buffer-name :sprof)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%4s %-54s %6s %6s %6s\n"
                      "Rank"
                      "Name"
                      "Self%"
                      "Cumul%"
                      "Total%"))
      (dolist (data graph)
        (slime-sprof-browser-insert-line data 54))))
  (forward-line 2))

(cl-defun slime-sprof-update (&optional (exclude-swank slime-sprof-exclude-swank))
  (slime-eval-async `(swank:swank-sprof-get-call-graph
                      :exclude-swank ,exclude-swank)
    'slime-sprof-format))

(defalias 'slime-sprof-browser 'slime-sprof-report)

(defun slime-sprof-report ()
  (interactive)
  (slime-with-popup-buffer ((slime-buffer-name :sprof)
                            :connection t
                            :select t
                            :mode 'slime-sprof-browser-mode)
    (slime-sprof-update)))

(defun slime-sprof-toggle-swank-exclusion ()
  (interactive)
  (setq slime-sprof-exclude-swank
        (not slime-sprof-exclude-swank))
  (slime-sprof-update))

(defun slime-sprof-browser-insert-line (data name-length)
  (cl-destructuring-bind (index name self cumul total)
      data
    (if index
        (insert (format "%-4d " index))
      (insert "     "))
    (slime-insert-propertized
     (slime-sprof-browser-name-properties)
     (format (format "%%-%ds " name-length)
             (slime-sprof-abbreviate-name name name-length)))
    (insert (format "%6.2f " self))
    (when cumul
      (insert (format "%6.2f " cumul))
      (when total
        (insert (format "%6.2f" total))))
    (when index
      (slime-sprof-browser-add-line-text-properties
       `(profile-index ,index expanded nil)))
    (insert "\n")))

(defun slime-sprof-abbreviate-name (name max-length)
  (cl-subseq name 0 (min (length name) max-length)))

;; Expanding / collapsing

(defun slime-sprof-browser-toggle ()
  (interactive)
  (let ((index (get-text-property (point) 'profile-index)))
    (when index
      (save-excursion
        (if (slime-sprof-browser-line-expanded-p)
            (slime-sprof-browser-collapse)
            (slime-sprof-browser-expand))))))

(defun slime-sprof-browser-collapse ()
  (let ((inhibit-read-only t))
    (slime-sprof-browser-add-line-text-properties '(expanded nil))
    (forward-line)
    (cl-loop until (or (eobp)
                       (get-text-property (point) 'profile-index))
             do
             (delete-region (point-at-bol) (point-at-eol))
             (unless (eobp)
               (delete-char 1)))))

(defun slime-sprof-browser-expand ()
  (lexical-let* ((buffer (current-buffer))
                 (point (point))
                 (index (get-text-property point 'profile-index)))
    (slime-eval-async `(swank:swank-sprof-expand-node ,index)
                      (lambda (data)
                        (with-current-buffer buffer
                          (save-excursion 
                            (destructuring-bind (&key callers calls)
                                data
                              (slime-sprof-browser-add-expansion callers
                                                                   "Callers"
                                                                   0)
                              (slime-sprof-browser-add-expansion calls
                                                                   "Calls"
                                                                   0))))))))

(defun slime-sprof-browser-add-expansion (data type nesting)
  (when data
    (let ((inhibit-read-only t))
      (slime-sprof-browser-add-line-text-properties '(expanded t))
      (end-of-line)
      (insert (format "\n     %s" type))
      (dolist (node data)
        (cl-destructuring-bind (index name cumul) node
          (insert (format (format "\n%%%ds" (+ 7 (* 2 nesting))) ""))
          (slime-insert-propertized
           (slime-sprof-browser-name-properties)
           (let ((len (- 59 (* 2 nesting))))
             (format (format "%%-%ds " len)
                     (slime-sprof-abbreviate-name name len))))
          (slime-sprof-browser-add-line-text-properties
           `(profile-sub-index ,index))
          (insert (format "%6.2f" cumul)))))))

(defun slime-sprof-browser-line-expanded-p ()
  (get-text-property (point) 'expanded))

(defun slime-sprof-browser-add-line-text-properties (properties)
  (add-text-properties (point-at-bol)
                       (point-at-eol)
                       properties))

(defun slime-sprof-browser-name-properties ()
  '(face sldb-restart-number-face))

;; "Go to function"

(defun slime-sprof-browser-go-to ()                                           
  (interactive)
  (let ((sub-index (get-text-property (point) 'profile-sub-index)))
    (when sub-index
      (let ((pos (text-property-any
                  (point-min) (point-max) 'profile-index sub-index)))
        (when pos (goto-char pos))))))

;; Disassembly

(defun slime-sprof-browser-disassemble-function ()
  (interactive)
  (let ((index (or (get-text-property (point) 'profile-index)
                   (get-text-property (point) 'profile-sub-index))))
    (when index
      (slime-eval-describe `(swank:swank-sprof-disassemble
                             ,index)))))

;; View source

(defun slime-sprof-browser-view-source ()
  (interactive)
  (let ((index (or (get-text-property (point) 'profile-index)
                   (get-text-property (point) 'profile-sub-index))))
    (when index
      (slime-eval-async
       `(swank:swank-sprof-source-location ,index)
       (lambda (source-location)
         (slime-dcase source-location
           ((:error message)
            (message "%s" message)
            (ding))
           (t
            (slime-show-source-location source-location))))))))

(provide 'slime-sprof)

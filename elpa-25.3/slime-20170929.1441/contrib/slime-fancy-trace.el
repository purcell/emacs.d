(eval-and-compile
  (require 'slime))

(define-slime-contrib slime-fancy-trace
  "Enhanced version of slime-trace capable of tracing local functions,
methods, setf functions, and other entities supported by specific
swank:swank-toggle-trace backends. Invoke via C-u C-t."
  (:authors "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies slime-parse))

(defun slime-trace-query (spec)
  "Ask the user which function to trace; SPEC is the default.
The result is a string."
  (cond ((null spec)
         (slime-read-from-minibuffer "(Un)trace: "))
        ((stringp spec)
         (slime-read-from-minibuffer "(Un)trace: " spec))
        ((symbolp spec)    ; `slime-extract-context' can return symbols.
         (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
        (t
         (slime-dcase spec
           ((setf n)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:defun n)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string n)))
           ((:defgeneric n)
            (let* ((name (prin1-to-string n))
                   (answer (slime-read-from-minibuffer "(Un)trace: " name)))
              (cond ((and (string= name answer)
                          (y-or-n-p (concat "(Un)trace also all "
                                            "methods implementing "
                                            name "? ")))
                     (prin1-to-string `(:defgeneric ,n)))
                    (t
                     answer))))
           ((:defmethod &rest _)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:call caller callee)
            (let* ((callerstr (prin1-to-string caller))
                   (calleestr (prin1-to-string callee))
                   (answer (slime-read-from-minibuffer "(Un)trace: "
                                                       calleestr)))
              (cond ((and (string= calleestr answer)
                          (y-or-n-p (concat "(Un)trace only when " calleestr
                                            " is called by " callerstr "? ")))
                     (prin1-to-string `(:call ,caller ,callee)))
                    (t
                     answer))))
           (((:labels :flet) &rest _)
            (slime-read-from-minibuffer "(Un)trace local function: "
                                        (prin1-to-string spec)))
           (t (error "Don't know how to trace the spec %S" spec))))))

(defun slime-toggle-fancy-trace (&optional using-context-p)
  "Toggle trace."
  (interactive "P")
  (let* ((spec (if using-context-p
                   (slime-extract-context)
                   (slime-symbol-at-point)))
         (spec (slime-trace-query spec)))
    (message "%s" (slime-eval `(swank:swank-toggle-trace ,spec)))))

;; override slime-toggle-trace-fdefinition
(define-key slime-prefix-map "\C-t" 'slime-toggle-fancy-trace)

(provide 'slime-fancy-trace)

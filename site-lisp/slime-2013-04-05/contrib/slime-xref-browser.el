
(define-slime-contrib slime-xref-browser
  "Xref browsing with tree-widget"
  (:authors "Rui Patrocínio <rui.patrocinio@netvisao.pt>")
  (:license "GPL"))


;;;; classes browser

(defun slime-expand-class-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (slime-eval `(swank:mop :subclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :expander slime-expand-class-node
				    :has-children t)))))

(defun slime-browse-classes (name)
  "Read the name of a class and show its subclasses."
  (interactive (list (slime-read-symbol-name "Class Name: ")))
  (slime-call-with-browser-setup 
   (slime-buffer-name :browser) (slime-current-package) "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name 
                    :expander 'slime-expand-class-node 
                    :has-echildren t))))

(defvar slime-browser-map nil
  "Keymap for tree widget browsers")

(require 'tree-widget)
(unless slime-browser-map
  (setq slime-browser-map (make-sparse-keymap))
  (set-keymap-parent slime-browser-map widget-keymap)
  (define-key slime-browser-map "q" 'bury-buffer))

(defun slime-call-with-browser-setup (buffer package title fn)
  (switch-to-buffer buffer)
  (kill-all-local-variables)
  (setq slime-buffer-package package)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert title "\n\n")
  (save-excursion
    (funcall fn))
  (lisp-mode-variables t)
  (slime-mode t)
  (use-local-map slime-browser-map)
  (widget-setup))


;;;; Xref browser

(defun slime-fetch-browsable-xrefs (type name)
  "Return a list ((LABEL DSPEC)).
LABEL is just a string for display purposes. 
DSPEC can be used to expand the node."
  (let ((xrefs '()))
    (loop for (_file . specs) in (slime-eval `(swank:xref ,type ,name)) do
          (loop for (dspec . _location) in specs do
                (let ((exp (ignore-errors (read (downcase dspec)))))
                  (cond ((and (consp exp) (eq 'flet (car exp)))
                         ;; we can't expand FLET references so they're useless
                         )
                        ((and (consp exp) (eq 'method (car exp)))
                         ;; this isn't quite right, but good enough for now
                         (push (list dspec (string (second exp))) xrefs))
                        (t
                         (push (list dspec dspec) xrefs))))))
    xrefs))

(defun slime-expand-xrefs (widget)
  (or (widget-get widget :args)
      (let* ((type (widget-get widget :xref-type))
             (dspec (widget-get widget :xref-dspec))
             (xrefs (slime-fetch-browsable-xrefs type dspec)))
        (loop for (label dspec) in xrefs
              collect `(tree-widget :tag ,label
                                    :xref-type ,type
                                    :xref-dspec ,dspec
                                    :expander slime-expand-xrefs
                                    :has-children t)))))

(defun slime-browse-xrefs (name type)
  "Show the xref graph of a function in a tree widget."
  (interactive 
   (list (slime-read-from-minibuffer "Name: "
                                     (slime-symbol-at-point))
         (read (completing-read "Type: " (slime-bogus-completion-alist
                                          '(":callers" ":callees" ":calls"))
                                nil t ":"))))
  (slime-call-with-browser-setup 
   (slime-buffer-name :xref) (slime-current-package) "Xref Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name :xref-type type :xref-dspec name 
                    :expander 'slime-expand-xrefs :has-echildren t))))

(provide 'slime-xref-browser)

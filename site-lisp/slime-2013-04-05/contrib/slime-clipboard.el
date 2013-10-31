
(define-slime-contrib slime-clipboard
  "This add a few commands to put objects into a clipboard and to
insert textual references to those objects.

The clipboard command prefix is C-c @.

 C-c @ +   adds an object to the clipboard
 C-c @ @   inserts a reference to an object in the clipboard
 C-c @ ?   displays the clipboard

This package also also binds the + key in the inspector and
debugger to add the object at point to the clipboard."
  (:authors "Helmut Eller  <heller@common-lisp.net>")
  (:license "GPL")
  (:swank-dependencies swank-clipboard))

(define-derived-mode slime-clipboard-mode fundamental-mode
  "Slime-Clipboard"
  "SLIME Clipboad Mode.

\\{slime-clipboard-mode-map}")

(slime-define-keys slime-clipboard-mode-map
  ("g" 'slime-clipboard-redisplay)
  ((kbd "C-k") 'slime-clipboard-delete-entry)
  ("i" 'slime-clipboard-inspect))

(defvar slime-clipboard-map (make-sparse-keymap))

(slime-define-keys slime-clipboard-map
  ("?" 'slime-clipboard-display)
  ("+" 'slime-clipboard-add)
  ("@" 'slime-clipboard-ref))

(define-key slime-mode-map (kbd "C-c @") slime-clipboard-map)
(define-key slime-repl-mode-map (kbd "C-c @") slime-clipboard-map)

(slime-define-keys slime-inspector-mode-map
  ("+" 'slime-clipboard-add-from-inspector))

(slime-define-keys sldb-mode-map
  ("+" 'slime-clipboard-add-from-sldb))

(defun slime-clipboard-add (exp package)
  "Add an object to the clipboard."
  (interactive (list (slime-read-from-minibuffer 
                      "Add to clipboard (evaluated): "
                      (slime-sexp-at-point))
		     (slime-current-package)))
  (slime-clipboard-add-internal `(:string ,exp ,package)))

(defun slime-clipboard-add-internal (datum)
  (slime-eval-async `(swank-clipboard:add ',datum) 
		    (lambda (result) (message "%s" result))))

(defun slime-clipboard-display ()
  "Display the content of the clipboard."
  (interactive)
  (slime-eval-async `(swank-clipboard:entries) 
		    #'slime-clipboard-display-entries))

(defun slime-clipboard-display-entries (entries)
  (slime-with-popup-buffer ((slime-buffer-name :clipboard)
                            :mode 'slime-clipboard-mode)
    (slime-clipboard-insert-entries entries)))

(defun slime-clipboard-insert-entries (entries)
  (let ((fstring "%2s %3s %s\n"))
    (insert (format fstring "Nr" "Id" "Value")
            (format fstring "--" "--" "-----" ))
    (save-excursion
      (loop for i from 0 for (ref . value) in entries do
	    (slime-insert-propertized `(slime-clipboard-entry ,i
					slime-clipboard-ref ,ref)
				      (format fstring i ref value))))))

(defun slime-clipboard-redisplay ()
  "Update the clipboard buffer."
  (interactive)
  (slime-eval-async 
   `(swank-clipboard:entries) 
   (lambda (entries) 
     (let ((inhibit-read-only t))
       (slime-save-coordinates (point)
	 (erase-buffer)
	 (slime-clipboard-insert-entries entries))))))

(defun slime-clipboard-entry-at-point ()
  (or (get-text-property (point) 'slime-clipboard-entry)
      (error "No clipboard entry at point")))

(defun slime-clipboard-ref-at-point ()
  (or (get-text-property (point) 'slime-clipboard-ref)
      (error "No clipboard ref at point")))

(defun slime-clipboard-inspect (&optional entry)
  "Inspect the current clipboard entry."
  (interactive (list (slime-clipboard-ref-at-point)))
  (slime-inspect (prin1-to-string `(swank-clipboard::clipboard-ref ,entry))))

(defun slime-clipboard-delete-entry (&optional entry)
  "Delete the current entry from the clipboard."
  (interactive (list (slime-clipboard-entry-at-point)))
  (slime-eval-async `(swank-clipboard:delete-entry ,entry)
		    (lambda (result) 
		      (slime-clipboard-redisplay)
		      (message "%s" result))))

(defun slime-clipboard-ref ()
  "Ask for a clipboard entry number and insert a reference to it."
  (interactive)
  (slime-clipboard-read-entry-number #'slime-clipboard-insert-ref))
  
;; insert a reference to clipboard entry ENTRY at point.  The text
;; receives a special 'display property to make it look nicer.  We
;; remove this property in a modification when a user tries to modify
;; he real text.
(defun slime-clipboard-insert-ref (entry)
  (destructuring-bind (ref . string) 
      (slime-eval `(swank-clipboard:entry-to-ref ,entry))
    (slime-insert-propertized
     `(display ,(format "#@%d%s" ref string)
	       modification-hooks (slime-clipboard-ref-modified)
	       rear-nonsticky t)
     (format "(swank-clipboard::clipboard-ref %d)" ref))))

(defun slime-clipboard-ref-modified (start end)
  (when (get-text-property start 'display)
    (let ((inhibit-modification-hooks t))
      (save-excursion
	(goto-char start)
	(destructuring-bind (dstart dend) (slime-property-bounds 'display)
	  (unless (and (= start dstart) (= end dend))
	    (remove-list-of-text-properties 
	     dstart dend '(display modification-hooks))))))))

;; Read a entry number.
;; Written in CPS because the display the clipboard before reading.
(defun slime-clipboard-read-entry-number (k)
  (slime-eval-async 
   `(swank-clipboard:entries) 
   (slime-rcurry
    (lambda (entries window-config k)
      (slime-clipboard-display-entries entries)
      (let ((entry (unwind-protect
		       (read-from-minibuffer "Entry number: " nil nil t)
		     (set-window-configuration window-config))))
	(funcall k entry)))
    (current-window-configuration)
    k)))

(defun slime-clipboard-add-from-inspector ()
  (interactive)
  (let ((part (or (get-text-property (point) 'slime-part-number)
		  (error "No part at point"))))
    (slime-clipboard-add-internal `(:inspector ,part))))

(defun slime-clipboard-add-from-sldb ()
  (interactive)
  (slime-clipboard-add-internal 
   `(:sldb ,(sldb-frame-number-at-point) 
	   ,(sldb-var-number-at-point))))

(provide 'slime-clipboard)

(require 'slime)
(require 'eldoc)
(require 'cl-lib)
(require 'slime-parse)

(define-slime-contrib slime-autodoc
  "Show fancy arglist in echo area."
  (:license "GPL")
  (:authors "Luke Gorrie  <luke@bluetail.com>"
            "Lawrence Mitchell  <wence@gmx.li>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler  <tcr@freebits.de>")
  (:slime-dependencies slime-parse)
  (:swank-dependencies swank-arglists)
  (:on-load (slime-autodoc--enable))
  (:on-unload (slime-autodoc--disable)))

(defcustom slime-autodoc-accuracy-depth 10
  "Number of paren levels that autodoc takes into account for
  context-sensitive arglist display (local functions. etc)"
  :type 'integer
  :group 'slime-ui)



(defun slime-arglist (name)
  "Show the argument list for NAME."
  (interactive (list (slime-read-symbol-name "Arglist of: " t)))
  (let ((arglist (slime-retrieve-arglist name)))
    (if (eq arglist :not-available)
        (error "Arglist not available")
        (message "%s" (slime-autodoc--fontify arglist)))))

;; used also in slime-c-p-c.el.
(defun slime-retrieve-arglist (name)
  (let ((name (cl-etypecase name
		(string name)
		(symbol (symbol-name name)))))
    (car (slime-eval `(swank:autodoc '(,name ,slime-cursor-marker))))))

(defun slime-autodoc-manually ()
  "Like autodoc informtion forcing multiline display."
  (interactive)
  (let ((doc (slime-autodoc t)))
    (cond (doc (eldoc-message doc))
	  (t (eldoc-message nil)))))

;; Must call eldoc-add-command otherwise (eldoc-display-message-p)
;; returns nil and eldoc clears the echo area instead.
(eldoc-add-command 'slime-autodoc-manually)

(defun slime-autodoc-space (n)
  "Like `slime-space' but nicer."
  (interactive "p")
  (self-insert-command n)
  (let ((doc (slime-autodoc)))
    (when doc
      (eldoc-message doc))))

(eldoc-add-command 'slime-autodoc-space)


;;;; Autodoc cache

(defvar slime-autodoc--cache-last-context nil)
(defvar slime-autodoc--cache-last-autodoc nil)

(defun slime-autodoc--cache-get (context)
  "Return the cached autodoc documentation for `context', or nil."
  (and (equal context slime-autodoc--cache-last-context)
       slime-autodoc--cache-last-autodoc))

(defun slime-autodoc--cache-put (context autodoc)
  "Update the autodoc cache for CONTEXT with AUTODOC."
  (setq slime-autodoc--cache-last-context context)
  (setq slime-autodoc--cache-last-autodoc autodoc))


;;;; Formatting autodoc

(defsubst slime-autodoc--canonicalize-whitespace (string)
  (replace-regexp-in-string "[ \n\t]+" " "  string))

(defun slime-autodoc--format (doc multilinep)
  (let ((doc (slime-autodoc--fontify doc)))
    (cond (multilinep doc)
	  (t (slime-oneliner (slime-autodoc--canonicalize-whitespace doc))))))

(defun slime-autodoc--fontify (string)
  "Fontify STRING as `font-lock-mode' does in Lisp mode."
  (with-current-buffer (get-buffer-create (slime-buffer-name :fontify 'hidden))
    (erase-buffer)
    (unless (eq major-mode 'lisp-mode)
      ;; Just calling (lisp-mode) will turn slime-mode on in that buffer,
      ;; which may interfere with this function
      (setq major-mode 'lisp-mode)
      (lisp-mode-variables t))
    (insert string)
    (let ((font-lock-verbose nil))
      (font-lock-fontify-buffer))
    (goto-char (point-min))
    (when (re-search-forward "===> \\(\\(.\\|\n\\)*\\) <===" nil t)
      (let ((highlight (match-string 1)))
        ;; Can't use (replace-match highlight) here -- broken in Emacs 21
        (delete-region (match-beginning 0) (match-end 0))
	(slime-insert-propertized '(face eldoc-highlight-function-argument) highlight)))
    (buffer-substring (point-min) (point-max))))

(define-obsolete-function-alias 'slime-fontify-string
  'slime-autodoc--fontify
  "SLIME 2.10")


;;;; Autodocs (automatic context-sensitive help)

(defun slime-autodoc (&optional force-multiline)
  "Returns the cached arglist information as string, or nil.
If it's not in the cache, the cache will be updated asynchronously."
  (save-excursion
    (save-match-data
      (let ((context (slime-autodoc--parse-context)))
	(when context
	  (let* ((cached (slime-autodoc--cache-get context))
		 (multilinep (or force-multiline
				 eldoc-echo-area-use-multiline-p)))
	    (cond (cached (slime-autodoc--format cached multilinep))
		  (t
		   (when (slime-background-activities-enabled-p)
		     (slime-autodoc--async context multilinep))
		   nil))))))))

;; Return the context around point that can be passed to
;; swank:autodoc.  nil is returned if nothing reasonable could be
;; found.
(defun slime-autodoc--parse-context ()
  (and (slime-autodoc--parsing-safe-p)
       (let ((levels slime-autodoc-accuracy-depth))
	 (slime-parse-form-upto-point levels))))

(defun slime-autodoc--parsing-safe-p ()
  (cond ((fboundp 'slime-repl-inside-string-or-comment-p)
	 (not (slime-repl-inside-string-or-comment-p)))
	(t
	 (not (slime-inside-string-or-comment-p)))))

(defun slime-autodoc--async (context multilinep)
  (slime-eval-async
      `(swank:autodoc ',context ;; FIXME: misuse of quote
		      :print-right-margin ,(window-width (minibuffer-window)))
    (slime-curry #'slime-autodoc--async% context multilinep)))

(defun slime-autodoc--async% (context multilinep doc)
  (cl-destructuring-bind (doc cache-p) doc
    (unless (eq doc :not-available)
      (when cache-p
	(slime-autodoc--cache-put context doc))
      ;; Now that we've got our information,
      ;; get it to the user ASAP.
      (when (eldoc-display-message-p)
	(eldoc-message (slime-autodoc--format doc multilinep))))))


;;; Minor mode definition

;; Compute the prefix for slime-doc-map, usually this is C-c C-d.
(defun slime-autodoc--doc-map-prefix ()
  (concat
   (car (rassoc '(slime-prefix-map) slime-parent-bindings))
   (car (rassoc '(slime-doc-map) slime-prefix-bindings))))

(define-minor-mode slime-autodoc-mode
  "Toggle echo area display of Lisp objects at point."
  :keymap (let ((prefix (slime-autodoc--doc-map-prefix)))
	    `((,(concat prefix "A") . slime-autodoc-manually)
	      (,(concat prefix (kbd "C-A")) . slime-autodoc-manually)
	      (,(kbd "SPC") . slime-autodoc-space)))
  (set (make-local-variable 'eldoc-documentation-function) 'slime-autodoc)
  (set (make-local-variable 'eldoc-minor-mode-string) " adoc")
  (setq slime-autodoc-mode (eldoc-mode arg))
  (when (called-interactively-p 'interactive)
    (message "Slime autodoc mode %s."
             (if slime-autodoc-mode "enabled" "disabled"))))


;;; Noise to enable/disable slime-autodoc-mode

(defun slime-autodoc--on  () (slime-autodoc-mode 1))
(defun slime-autodoc--off () (slime-autodoc-mode 0))

(defvar slime-autodoc--relevant-hooks
  '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))

(defun slime-autodoc--enable ()
  (dolist (h slime-autodoc--relevant-hooks)
    (add-hook h 'slime-autodoc--on))
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when slime-mode
	(slime-autodoc--on)))))

(defun slime-autodoc--disable ()
  (dolist (h slime-autodoc--relevant-hooks)
    (remove-hook h 'slime-autodoc--on))
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when slime-autodoc-mode
	(slime-autodoc--off)))))

(provide 'slime-autodoc)

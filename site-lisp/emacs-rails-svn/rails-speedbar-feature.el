(defvar rails-speedbar:roots
  '(("Controllers" rails-core:controllers rails-core:controller-file)
    ("Helpers"     rails-core:helpers     rails-core:helper-file)
    ("Models"      rails-core:models      rails-core:model-file)
    ("Observers"   rails-core:observers   rails-core:observer-file)
    ("Mailers"     rails-core:mailers     rails-core:mailer-file)
    ("Functional Tests" rails-core:functional-tests  rails-core:functional-test-file)
    ("Unit Tests"       rails-core:unit-tests        rails-core:unit-test-file)
    ("Fixtures"         rails-core:fixtures          rails-core:fixture-file)))

(defvar rails-speedbar:menu-items nil)
(defvar rails-speedbar:key-map
  (let ((map (speedbar-make-specialized-keymap)))
    (define-key map " " 'speedbar-toggle-line-expansion)
    (define-key map "+" 'speedbar-expand-line)
    (define-key map "=" 'speedbar-expand-line)
    (define-key map "-" 'speedbar-contract-line)
    (define-key map "e" 'speedbar-edit-line)
    (define-key map "\C-m" 'speedbar-edit-line)
    map))

(defun rails-speedbar:display (directory depth)
  (setq speedbar-update-flag nil)
  (dolist (i rails-speedbar:roots)
    (speedbar-make-tag-line 'angle
                            ?+
                            'rails-speedbar:expand-group
                            (car i)
                            (car i)
                            nil
                            nil
                            nil
                            depth)))

(defun rails-speedbar:expand-group (text token indent)
  (cond
   ((string-match "+" text)
    (speedbar-change-expand-button-char ?-)
    (let* ((fn (find-if #'(lambda(i) (string= token (car i)))
                        rails-speedbar:roots))
           (lst (apply (nth 1 fn) (list)))
           (find (nth 2 fn)))
      (speedbar-with-writable
        (save-excursion
          (end-of-line) (forward-char 1)
          (dolist (i lst)
            (speedbar-make-tag-line 'bracket
                                    ?+
                                    'rails-speedbar:expand-tags
                                    (rails-core:file (apply find (list i)))
                                    i
                                    'rails-speedbar:find-file
                                    (rails-core:file (apply find (list i)))
                                    nil
                                    (+ indent 1))
            )))))
   ((string-match "-" text)
    (speedbar-change-expand-button-char ?+)
    (speedbar-delete-subblock indent))))

(defun rails-speedbar:expand-tags (text token indent)
  (cond
   ((string-match "+" text)
    (let ((lst (speedbar-fetch-dynamic-tags token)))
      (if (not lst)
          (speedbar-change-expand-button-char ??)
        (speedbar-change-expand-button-char ?-)
        (speedbar-with-writable
          (save-excursion
            (end-of-line) (forward-char 1)
            (speedbar-insert-generic-list indent
                                          (cdr lst)
                                          'rails-speedbar:find-file
                                          'rails-speedbar:find-file))))))
   ((string-match "-" text)
    (speedbar-change-expand-button-char ?+)
    (speedbar-delete-subblock indent))))

(defun rails-speedbar:find-file (text token indent)
  (typecase token
    (string (speedbar-find-file-in-frame token))))

(defun rails-speedbar:get-focus ()
  (interactive)
  (speedbar-change-initial-expansion-list "Ruby On Rails")
  (let ((default-directory (rails-project:root)))
    (speedbar-get-focus)))

(defun rails-speedbar-feature:install ()
  (speedbar-add-expansion-list '("Ruby On Rails"
                                 rails-speedbar:menu-items
                                 rails-speedbar:key-map
                                 rails-speedbar:display))
  (define-key rails-minor-mode-map (kbd "<f11>") 'rails-speedbar:get-focus)
  (define-key-after
    (lookup-key rails-minor-mode-map [menu-bar rails])
    [speedbar] '("Toggle Speedbar" . rails-speedbar:get-focus)
    'svn-status))

(provide 'rails-speedbar-feature)
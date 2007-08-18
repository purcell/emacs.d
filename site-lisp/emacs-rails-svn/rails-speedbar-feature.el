(defvar rails-speedbar:roots
  '(("Controllers"      rails-core:controllers       rails-core:controller-file)
    ("Helpers"          rails-core:helpers           rails-core:helper-file)
    ("Models"           rails-core:models            rails-core:model-file)
    ("Observers"        rails-core:observers         rails-core:observer-file)
    ("Mailers"          rails-core:mailers           rails-core:mailer-file)
    ("Migrations"       rails-core:migrations        rails-core:migration-file)
    ("Functional Tests" rails-core:functional-tests  rails-core:functional-test-file)
    ("Unit Tests"       rails-core:unit-tests        rails-core:unit-test-file)
    ("Fixtures"         rails-core:fixtures          rails-core:fixture-file)
    ("Configuration"    rails-core:configuration-files rails-core:configuration-file)))

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
  (speedbar-with-writable
    (insert (rails-project:root) "\n"))
  (dolist (i rails-speedbar:roots)
    (speedbar-make-tag-line 'angle
                            ?+
                            'rails-speedbar:expand-group
                            (car i)
                            (car i)
                            nil
                            nil
                            nil
                            depth))
  (speedbar-make-tag-line 'angle
                          ?+
                          'rails-speedbar:expand-directory
                          (concat (rails-speedbar:root) "app/views")
                          "Views"
                          nil
                          nil
                          nil
                          depth))

(defun rails-speedbar:expand-directory (text token indent)
  (cond
   ((string-match "+" text)
    (speedbar-change-expand-button-char ?-)
    (let ((files (directory-files token nil "^[^.]")))
      (save-excursion
        (end-of-line) (forward-char 1)
        (speedbar-with-writable
        (dolist (i files)
          (if (file-directory-p (format "%s/%s" token i))
              (speedbar-make-tag-line 'curly
                                      ?+
                                      'rails-speedbar:expand-directory
                                      (format "%s/%s" token i)
                                      i
                                      nil nil nil
                                      (+ 1 indent))
            (speedbar-make-tag-line 'statictag
                                    ??
                                    nil
                                    nil
                                    i
                                    'rails-speedbar:find-file
                                    (format "%s/%s" token i)
                                    nil
                                    (+ 1 indent))))))))
   ((string-match "-" text)
    (speedbar-change-expand-button-char ?+)
    (speedbar-delete-subblock indent))))

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
                                    (rails-speedbar:in-root (rails-core:file (apply find (list i))))
                                    i
                                    'rails-speedbar:find-file
                                    (rails-speedbar:in-root (rails-core:file (apply find (list i))))
                                    nil
                                    (+ indent 1)))))))
   ((string-match "-" text)
    (speedbar-change-expand-button-char ?+)
    (speedbar-delete-subblock indent))))

(defun rails-speedbar:expand-tags (text token indent)
  (cond
   ((string-match "+" text)
    (let ((lst (speedbar-fetch-dynamic-tags token)))
      (if (not lst)
          (speedbar-change-expand-button-char ??)
        (progn
          (speedbar-change-expand-button-char ?-)
          (speedbar-with-writable
            (save-excursion
              (end-of-line) (forward-char 1)
              (speedbar-insert-generic-list indent
                                            (cdr lst)
                                            'speedbar-tag-expand
                                            'speedbar-tag-find)))))))
   ((string-match "-" text)
    (speedbar-change-expand-button-char ?+)
    (speedbar-delete-subblock indent))))

(defun rails-speedbar:line-directory (&optional depth)
  (save-excursion
    (end-of-line)
    (let ((start (point)))
      (when (search-backward "[-]" nil t)
        (end-of-line)
        (skip-syntax-backward "w")
        (get-text-property (point) 'speedbar-token)))))

(defun rails-speedbar:find-file (text token indent)
  (typecase token
    (string (speedbar-find-file-in-frame token))))

(defun rails-speedbar:root ()
  (save-excursion
    (goto-char (point-min))
    (let* ((root (current-line-string))
           (root (if (file-directory-p root)
                     root
                   (rails-project:root))))
      root)))

(defmacro rails-speedbar:in-root (&rest body)
  `(flet ((rails-project:root () ,(rails-speedbar:root)))
     ,@body))

(defun rails-speedbar:get-focus ()
  (interactive)
  (speedbar-change-initial-expansion-list "Ruby On Rails")
  (let ((default-directory (rails-project:root)))
    (speedbar-get-focus)))

(defun rails-speedbar-feature:install ()
  (speedbar-add-expansion-list
   '("Ruby On Rails"
     rails-speedbar:menu-items
     rails-speedbar:key-map
     rails-speedbar:display))
  (speedbar-add-mode-functions-list
   '("Ruby On Rails"
     (speedbar-line-directory . rails-speedbar:line-directory)))

  (define-key rails-minor-mode-map (kbd "<f11>") 'rails-speedbar:get-focus)
  (define-key-after
    (lookup-key rails-minor-mode-map [menu-bar rails])
    [speedbar] '("Toggle Speedbar" . rails-speedbar:get-focus)
    'svn-status))

(provide 'rails-speedbar-feature)
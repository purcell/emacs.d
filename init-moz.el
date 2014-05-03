(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun moz-reload-browser ()
  (interactive)
  (comint-send-string (inferior-moz-process)
                      "setTimeout(function(){content.document.location.reload(true);}, '500');"))

(defun moz-custom-setup ()
  (message "moz-custom-setup called")
  (moz-minor-mode 1)
  ;; @see  http://www.emacswiki.org/emacs/MozRepl
  ;; Example - you may want to add hooks for your own modes.
  ;; I also add this to python-mode when doing django development.
  (add-hook 'after-save-hook
            '(lambda () (interactive)
               (when (memq major-mode '(web-mode html-mode nxml-mode nxhml-mode php-mode))
                 (moz-reload-browser)
                 ))
            'append 'local))

;; (add-hook 'js2-mode-hook 'moz-custom-setup)
(add-hook 'html-mode-hook 'moz-custom-setup)
(add-hook 'nxml-mode-hook 'moz-custom-setup)
(add-hook 'web-mode-hook 'moz-custom-setup)

(eval-after-load 'moz
  '(progn
     (global-set-key (kbd "C-x p") 'moz-reload-browser)
     ))

(defun moz-goto-content-and-run-cmd (cmd)
  (comint-send-string (inferior-moz-process)
                      (concat "repl.enter(content);"
                              cmd
                              "repl.back();")))

(setq moz-repl-js-dir (expand-file-name "~/moz-repl-js-dir"))

(defun moz--read-file (js-file)
  (with-temp-buffer
    (insert-file-contents js-file)
    (buffer-string)))

(defun moz--load-js-file (js-file)
  (let (cmd )
    (when (file-exists-p js-file)
      ;; make moz API usable in any major-mode
      (moz-minor-mode 1)
      ;; flush mozrepl at first
      (moz-goto-content-and-run-cmd "console.log('hello');")
      ;; read the content of js-file
      (setq cmd (moz--read-file js-file))
      (moz-goto-content-and-run-cmd cmd))))

(defun moz-load-js-file-and-send-it ()
  "load js file from specific directory and send it to mozrepl"
  (interactive)
  (let ((js-file (read-file-name "js file:" moz-repl-js-dir)))
    (moz--load-js-file js-file)
    ))

(defun moz-console-clear ()
  (interactive)
  (moz-minor-mode 1)
  (moz-goto-content-and-run-cmd "console.log('clearing ...');")
  (moz-goto-content-and-run-cmd "console.clear();"))

(defun moz-console-log-var ()
  "guess variable to console.log, support both html and javascript"
  (interactive)
  (let (attr-faces
        var
        cmd)
    (cond
     ((region-active-p)
      (setq var (buffer-substring-no-properties (region-beginning) (region-end)))
      )
     ((memq major-mode '(js-mode js2-mode js3-mode javascript-mode))
      (setq var (thing-at-point 'symbol)))
     (t
      (cond
       ((memq major-mode '(web-mode))
        (setq attr-faces '(web-mode-html-attr-value-face)))
       ((memq major-mode '(nxml-mode))
        (setq attr-faces '(nxml-attribute-value)))
       ((memq major-mode '(html-mode))
        (setq attr-faces '(font-lock-string-face)))
       )

      (let ((f (get-text-property (- (point) 1) 'face))
            attr-name)
        (when (memq f attr-faces)
          (save-excursion
            (search-backward-regexp "=['\"]" (line-beginning-position) t)
            (backward-char)
            (setq attr-name (thing-at-point 'word))
            )
          (setq var (thing-at-point 'symbol))

          (when (or (string= attr-name "id") (string= attr-name "for"))
            (setq var (concat "$('#" var "')"))
            )

          (when (string= attr-name "class")
            (setq var (concat "$('." var "')"))
            )

          ))
      ))

    (when var
      (setq cmd (concat "console.log(\"" var "=\"" "," var ");"))
      (moz-goto-content-and-run-cmd cmd))

    ))

(provide 'init-moz)

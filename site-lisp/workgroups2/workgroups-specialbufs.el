;;; workgroups-specialbufs --- special buffers serialization
;;; Commentary:
;;
;; TODO:
;;  1. Add more special buffers support (that you use)
;;  2. Improve existing
;;
;;; Code:

(require 'workgroups-variables)
(require 'workgroups-support-macro)

;; Dired
(wg-support 'dired-mode 'dired
            `((deserialize . ,(lambda (buffer vars)
                                (if (or wg-restore-remote-buffers
                                        (not (file-remote-p default-directory)))
                                    ;; TODO: try to restore parent dir if not exist
                                    (if (file-directory-p default-directory)
                                        (dired default-directory)))))))

;; Info-mode
(wg-support 'Info-mode 'info
            `((save . (Info-current-file Info-current-node))
              (deserialize . ,(lambda (buffer vars)
                                (wg-aif vars
                                    (if (fboundp 'Info-find-node)
                                        (apply #'Info-find-node it))
                                  (info))))))

;; help-mode
;; Bug: https://github.com/pashinin/workgroups2/issues/29
(if nil
(wg-support 'help-mode 'help-mode
            `((save . (help-xref-stack-item help-xref-stack help-xref-forward-stack))
              (deserialize . ,(lambda (buffer vars)
                               (wg-dbind (item stack forward-stack) vars
                                 (condition-case err
                                     (apply (car item) (cdr item))
                                   (error (message "%s" err)))
                                 (wg-awhen (get-buffer "*Help*")
                                   (set-buffer it)
                                   (wg-when-boundp (help-xref-stack help-xref-forward-stack)
                                     (setq help-xref-stack stack
                                           help-xref-forward-stack forward-stack))))))))
)

;; ielm
(wg-support 'inferior-emacs-lisp-mode 'ielm
            `((deserialize . ,(lambda (buffer vars) (ielm)))))

;; Magit status
(wg-support 'magit-status-mode 'magit
            `((deserialize . ,(lambda (buffer vars)
                                (when (file-directory-p default-directory)
                                  (magit-status default-directory))))))

;; Shell
(wg-support 'shell-mode 'shell
            `((deserialize . ,(lambda (buffer vars)
                                (shell (wg-buf-name buffer))))))

;; org-agenda buffer
(defun wg-get-org-agenda-view-commands ()
  "Return commands to restore the state of Agenda buffer.
Can be restored using \"(eval commands)\"."
  (interactive)
  (when (boundp 'org-agenda-buffer-name)
    (if (get-buffer org-agenda-buffer-name)
        (with-current-buffer org-agenda-buffer-name
          (let* ((p (or (and (looking-at "\\'") (1- (point))) (point)))
                 (series-redo-cmd (get-text-property p 'org-series-redo-cmd)))
            (if series-redo-cmd
                (get-text-property p 'org-series-redo-cmd)
              (get-text-property p 'org-redo-cmd)))))))

(defun wg-run-agenda-cmd (f)
  "Run commands F in Agenda buffer.
You can get these commands using `wg-get-org-agenda-view-commands'."
  (when (and (boundp 'org-agenda-buffer-name)
             (fboundp 'org-current-line)
             (fboundp 'org-goto-line))
    (if (get-buffer org-agenda-buffer-name)
        (save-window-excursion
          (with-current-buffer org-agenda-buffer-name
            (let* ((line (org-current-line)))
              (if f (eval f))
              (org-goto-line line)))))))

(wg-support 'org-agenda-mode 'org-agenda
            '((serialize . (lambda (buffer)
                             (wg-get-org-agenda-view-commands)))
              (deserialize . (lambda (buffer vars)
                               (org-agenda-list)
                               (wg-awhen (get-buffer org-agenda-buffer-name)
                                 (set-buffer it)
                                 (wg-run-agenda-cmd vars))
                               ))))

;; eshell
(wg-support 'eshell-mode 'esh-mode
            '((deserialize . (lambda (buffer vars)
                               (prog1 (eshell t)
                                 (rename-buffer (wg-buf-name buffer) t))))))

;; term-mode
;;
;; This should work for `ansi-term's, too, as there doesn't seem to
;; be any difference between the two except how the name of the
;; buffer is generated.
;;
(wg-support 'term-mode 'term
            `((serialize . ,(lambda (buffer)
                              (if (get-buffer-process buffer)
                                  (wg-last1 (process-command (get-buffer-process buffer)))
                                "/bin/bash")))
              (deserialize . ,(lambda (buffer vars)
                                (cl-labels ((term-window-width () 80)
                                            (window-height () 24))
                                  (prog1 (term vars)
                                    (rename-buffer (wg-buf-name buffer) t)))))))

;; Python
(wg-support 'inferior-python-mode 'python
            `((save . (python-shell-interpreter python-shell-interpreter-args))
              (deserialize . ,(lambda (buffer vars)
                                (wg-dbind (pythoncmd pythonargs) vars
                                  (save-window-excursion
                                    (run-python (concat pythoncmd " " pythonargs)))
                                  (wg-awhen (get-buffer (process-buffer (python-shell-get-or-create-process)))
                                    (set-buffer it)
                                    (switch-to-buffer (process-buffer (python-shell-get-or-create-process)))
                                    (goto-char (point-max)))
                                  )))))

;; Sage shell
(wg-support 'inferior-sage-mode 'sage-mode
            `((deserialize . ,(lambda (buffer vars)
                                (save-window-excursion
                                  (if (boundp' sage-command)
                                      (run-sage t sage-command t)))
                                (if (boundp 'sage-buffer)
                                    (wg-awhen (and
                                               sage-buffer)
                                      (set-buffer it)
                                      (switch-to-buffer sage-buffer)
                                      (goto-char (point-max))))))))

;; inferior-ess-mode   (ess-inf.el)
;; R shell, M-x R
(wg-support 'inferior-ess-mode 'ess-inf
            `((save . (inferior-ess-program))
              (deserialize . ,(lambda (buffer vars)
                                (wg-dbind (cmd) vars
                                  (let ((ess-ask-about-transfile nil)
                                        (ess-ask-for-ess-directory nil)
                                        (ess-history-file nil))
                                    (R)))))))

;; inferior-octave-mode
(wg-support 'inferior-octave-mode 'octave
            `((deserialize . ,(lambda (buffer vars)
                                (prog1 (run-octave)
                                  (rename-buffer (wg-buf-name buffer) t))))))

;; Prolog shell
(wg-support 'prolog-inferior-mode 'prolog
            `((deserialize . ,(lambda (buffer vars)
                                (save-window-excursion
                                  (run-prolog nil))
                                (switch-to-buffer "*prolog*")
                                (goto-char (point-max))))))

;; compilation-mode
;;
;; I think it's not a good idea to compile a program just to switch
;; workgroups. So just restoring a buffer name.
(wg-support 'compilation-mode 'compile
            `((serialize . ,(lambda (buffer)
                              (if (boundp' compilation-arguments) compilation-arguments)))
              (deserialize . ,(lambda (buffer vars)
                                (save-window-excursion
                                  (get-buffer-create (wg-buf-name buffer)))
                                (with-current-buffer (wg-buf-name buffer)
                                  (when (boundp' compilation-arguments)
                                    (make-local-variable 'compilation-arguments)
                                    (setq compilation-arguments vars)))
                                (switch-to-buffer (wg-buf-name buffer))
                                (goto-char (point-max))))))

;; grep-mode
;; see grep.el - `compilation-start' - it is just a compilation buffer
;; local variables:
;; `compilation-arguments' == (cmd mode nil nil)
(wg-support 'grep-mode 'grep
            `((serialize . ,(lambda (buffer)
                              (if (boundp' compilation-arguments) compilation-arguments)))
              (deserialize . ,(lambda (buffer vars)
                                (compilation-start (car vars) (nth 1 vars))
                                (switch-to-buffer "*grep*")))))


;; speedbar-mode
(defun wg-deserialize-speedbar-buffer (buf)
  "Deserialize speedbar-buffer BUF."
  (when (and (require 'speedbar nil 'noerror)
             (require 'dframe nil 'noerror))
    (wg-dbind (this-function args) (wg-buf-special-data buf)
      (let ((default-directory (car args))
            bufname)
        (if (boundp 'sr-speedbar-buffer-name)
            (setq bufname sr-speedbar-buffer-name)
          (setq bufname "*SPEEDBAR*"))
        (when (and (fboundp 'speedbar-mode)
                   (fboundp 'speedbar-reconfigure-keymaps)
                   (fboundp 'speedbar-update-contents)
                   (fboundp 'speedbar-set-timer))
          (with-no-warnings
            (setq speedbar-buffer (get-buffer-create bufname))
            (setq speedbar-frame (selected-frame)
                  dframe-attached-frame (selected-frame)
                  speedbar-select-frame-method 'attached
                  speedbar-verbosity-level 0
                  speedbar-last-selected-file nil)
            (set-buffer speedbar-buffer)
            (speedbar-mode)
            (speedbar-reconfigure-keymaps)
            (speedbar-update-contents)
            (speedbar-set-timer 1)
            (set-window-dedicated-p (get-buffer-window bufname) t)
            (switch-to-buffer bufname)))
        (current-buffer)
        ))))


(defun wg-serialize-speedbar-buffer (buffer)
  "Serialize speedbar BUFFER."
  (with-current-buffer buffer
    (if (fboundp 'speedbar-mode)
        (when (eq major-mode 'speedbar-mode)
          (list 'wg-deserialize-speedbar-buffer
                (wg-take-until-unreadable (list default-directory))
                )))))


(defun wg-deserialize-slime-buffer (buf)
  "Deserialize `slime' buffer BUF."
  (when (require 'slime nil 'noerror)
    (wg-dbind (this-function args) (wg-buf-special-data buf)
      (let ((default-directory (car args))
            (arguments (nth 1 args)))
        (when (and (fboundp 'slime-start*)
                   (fboundp 'slime-process))
          (save-window-excursion
            (slime-start* arguments))
          (switch-to-buffer (process-buffer (slime-process)))
          (current-buffer))))))

;; `comint-mode'  (general mode for all shells)
;;
;; It may have different shells. So we need to determine which shell is
;; now in `comint-mode' and how to restore it.
;;
;; Just executing `comint-exec' may be not enough because we can miss
;; some hooks or any other stuff that is executed when you run a
;; specific shell.
(defun wg-serialize-comint-buffer (buffer)
  "Serialize comint BUFFER."
  (with-current-buffer buffer
    (if (fboundp 'comint-mode)
        (when (eq major-mode 'comint-mode)
          ;; `slime-inferior-lisp-args' var is used when in `slime'
          (when (and (boundp 'slime-inferior-lisp-args)
                     slime-inferior-lisp-args)
            (list 'wg-deserialize-slime-buffer
                  (wg-take-until-unreadable (list default-directory
                                                  slime-inferior-lisp-args))
                ))))))

;; inf-mongo
;; https://github.com/tobiassvn/inf-mongo
;; `mongo-command' - command used to start inferior mongo
(wg-support 'inf-mongo-mode 'inf-mongo
            `((serialize . ,(lambda (buffer)
                              (if (boundp 'inf-mongo-command) inf-mongo-command)))
              (deserialize . ,(lambda (buffer vars)
                                (save-window-excursion
                                  (when (fboundp 'inf-mongo)
                                    (inf-mongo vars)))
                                (when (get-buffer "*mongo*")
                                  (switch-to-buffer "*mongo*")
                                  (goto-char (point-max)))))))

(defun wg-temporarily-rename-buffer-if-exists (buffer)
  "Rename BUFFER if it exists."
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (rename-buffer "*wg--temp-buffer*" t))))

;; SML shell
;; Functions to serialize deserialize inferior sml buffer
;; `inf-sml-program' is the program run as inferior sml, is the
;; `inf-sml-args' are the extra parameters passed, `inf-sml-host'
;; is the host on which sml was running when serialized
(wg-support 'inferior-sml-mode 'sml-mode
            `((serialize . ,(lambda (buffer)
                              (list (if (boundp 'sml-program-name) sml-program-name)
                                    (if (boundp 'sml-default-arg) sml-default-arg)
                                    (if (boundp 'sml-host-name) sml-host-name))))
              (deserialize . ,(lambda (buffer vars)
                                (wg-dbind (program args host) vars
                                  (save-window-excursion
                                    ;; If a inf-sml buffer already exists rename it temporarily
                                    ;; otherwise `run-sml' will simply switch to the existing
                                    ;; buffer, however we want to create a separate buffer with
                                    ;; the serialized name
                                    (let* ((inf-sml-buffer-name (concat "*"
                                                                        (file-name-nondirectory program)
                                                                        "*"))
                                           (existing-sml-buf (wg-temporarily-rename-buffer-if-exists
                                                              inf-sml-buffer-name)))
                                      (with-current-buffer (run-sml program args host)
                                        ;; Rename the buffer
                                        (rename-buffer (wg-buf-name buffer) t)

                                        ;; Now we can re-rename the previously renamed buffer
                                        (when existing-sml-buf
                                          (with-current-buffer existing-sml-buf
                                            (rename-buffer inf-sml-buffer-name t))))))
                                  (switch-to-buffer (wg-buf-name buffer))
                                  (goto-char (point-max)))))))

;; Geiser repls
;; http://www.nongnu.org/geiser/
(wg-support 'geiser-repl-mode 'geiser
            `((save . (geiser-impl--implementation))
              (deserialize . ,(lambda (buffer vars)
                                (save-window-excursion
                                  (when (fboundp 'run-geiser)
                                    (wg-dbind (impl) vars
                                      (run-geiser impl)
                                      (goto-char (point-max)))))
                                (switch-to-buffer (wg-buf-name buffer))))))

;; w3m-mode
(wg-support 'w3m-mode 'w3m
            `((save . (w3m-current-url))
              (deserialize . ,(lambda (buffer vars)
                                (wg-dbind (url) vars
                                  (w3m-goto-url url))))))


;; Wanderlust modes:
;; WL - folders
;;(defun wg-deserialize-wl-folders-buffer (buf)
;;  ""
;;  (if (fboundp 'wl)
;;      (wg-dbind (this-function) (wg-buf-special-data buf)
;;        ;;(when (not (eq major-mode 'wl-folder-mode))
;;        (wl)
;;        (goto-char (point-max))
;;        (current-buffer)
;;        )))
;;
;;(defun wg-serialize-wl-folders-buffer (buffer)
;;  ""
;;  (if (fboundp 'wl)
;;      (with-current-buffer buffer
;;        (when (eq major-mode 'wl-folder-mode)
;;          (list 'wg-deserialize-wl-folders-buffer
;;                )))))

;; WL - summary mode (list of mails)
;;(defun wg-deserialize-wl-summary-buffer (buf)
;;  ""
;;  (interactive)
;;  (if (fboundp 'wl)
;;      (wg-dbind (this-function param-list) (wg-buf-special-data buf)
;;        (when (not (eq major-mode 'wl-summary-mode))
;;          (let ((fld-name (car param-list)))
;;            ;;(switch-to-buffer "*scratch*")
;;            ;;(wl)
;;            ;;(wl-folder-jump-folder fld-name)
;;            ;;(message fld-name)
;;            ;;(goto-char (point-max))
;;            ;;(insert fld-name)
;;            (current-buffer)
;;          )))))
;;
;;(defun wg-serialize-wl-summary-buffer (buffer)
;;  ""
;;  (if (fboundp 'wl)
;;      (with-current-buffer buffer
;;        (when (eq major-mode 'wl-summary-mode)
;;          (list 'wg-deserialize-wl-summary-buffer
;;                (wg-take-until-unreadable (list wl-summary-buffer-folder-name))
;;                )))))
;;
;;
;;;; mime-view-mode
;;
;;(defun wg-deserialize-mime-view-buffer (buf)
;;  ""
;;  (wg-dbind (this-function) (wg-buf-special-data buf)
;;    (when (not (eq major-mode 'mime-view-mode))
;;      ;;(wl-summary-enter-handler 3570)     ; only in wl-summary-mode
;;      ;;(wl-summary-enter-handler)     ; only in wl-summary-mode
;;      (current-buffer)
;;      )))
;;
;;(defun wg-serialize-mime-view-buffer (buffer)
;;  ""
;;  (with-current-buffer buffer
;;    (when (eq major-mode 'mime-view-mode)
;;      (list 'wg-deserialize-mime-view-buffer
;;            ))))


;; emms-playlist-mode
;;
;; Help me on this one:
;; 1. How to start emms without any user interaction?
;;
;;(defun wg-deserialize-emms-buffer (buf)
;;  "Deserialize emms-playlist buffer BUF."
;;  (when (require 'emms-setup nil 'noerror)
;;    (require 'emms-player-mplayer)
;;    (emms-standard)
;;    (emms-default-players)
;;    (if (fboundp 'emms-playlist-mode)
;;        (wg-dbind (this-function args) (wg-buf-special-data buf)
;;          (let ((default-directory (car args)))
;;            (save-window-excursion
;;              ;;(emms)
;;              (if (or (null emms-playlist-buffer)
;;                      (not (buffer-live-p emms-playlist-buffer)))
;;                  ;;(call-interactively 'emms-add-file)
;;                  (emms-source-directory "/usr/data/disk_3/Music/SORT/")
;;                ))
;;            ;; (emms)
;;            ;;(with-current-buffer emms-playlist-buffer-name
;;            ;;(emms-source-playlist-directory-tree "/usr/data/disk_3/Music/SORT/")
;;            ;;(emms-source-directory "/usr/data/disk_3/Music/SORT")
;;            ;;(switch-to-buffer emms-playlist-buffer-name)
;;            (emms-playlist-mode-go)
;;            (current-buffer)
;;            )))))
;;
;;(defun wg-serialize-emms-buffer (buffer)
;;  "Serialize emms BUFFER."
;;  (with-current-buffer buffer
;;    (if (fboundp 'emms-playlist-mode)
;;        (when (eq major-mode 'emms-playlist-mode)
;;          (list 'wg-deserialize-emms-buffer
;;                (wg-take-until-unreadable (list default-directory))
;;                )))))


;;; buffer-local variable serdes

(defun wg-serialize-buffer-mark-ring ()
  "Return a new list of the positions of the marks in `mark-ring'."
  (mapcar 'marker-position mark-ring))

(defun wg-deserialize-buffer-mark-ring (positions)
  "Set `mark-ring' to a new list of markers created from POSITIONS."
  (setq mark-ring
        (mapcar (lambda (pos) (set-marker (make-marker) pos))
                positions)))

(defun wg-deserialize-buffer-major-mode (major-mode-symbol)
  "Conditionally retore MAJOR-MODE-SYMBOL in `current-buffer'."
  (and (fboundp major-mode-symbol)
       (not (eq major-mode-symbol major-mode))
       (funcall major-mode-symbol)))

(defun wg-deserialize-buffer-local-variables (buf)
  "Restore BUF's buffer local variables in `current-buffer'."
  (cl-loop for ((var . val) . rest) on (wg-buf-local-vars buf)
           do (wg-awhen (assq var wg-buffer-local-variables-alist)
                (wg-dbind (var ser des) it
                  (if des (funcall des val)
                    (set var val))))))

(provide 'workgroups-specialbufs)
;;; workgroups-specialbufs.el ends here

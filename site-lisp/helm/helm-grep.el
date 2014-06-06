;;; helm-grep.el --- Helm Incremental Grep. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'grep)
(require 'helm-regexp)

;;; load wgrep proxy if it's available
(require 'wgrep-helm nil t)

(declare-function helm-buffer-list "helm-buffers")
(declare-function helm-elscreen-find-file "helm-elscreen" (file))
(declare-function View-quit "view")
(declare-function doc-view-goto-page "doc-view" (page))


(defgroup helm-grep nil
  "Grep related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-grep-default-command
  "grep -a -d skip %e -n%cH -e %p %f"
  "Default grep format command for `helm-do-grep-1'.
Where:
'%e' format spec is for --exclude or --include grep options or
     ack-grep --type option.               (Not mandatory)

'%c' format spec is for case-fold-search,
     whether to use the -i option of grep. (Not mandatory)
     When you specify this spec, helm grep will use smartcase
     that is when a upcase character is found in pattern case will
     be respected and no '-i' option will be used, otherwise, when
     no upcase character is found in pattern always use '-i'.
     If you don't want this behavior, don't use this spec and
     specify or not the '-i' option.
     Note that with ack-grep this is not needed, just specify
     the '--smart-case' option.

'%p' format spec is for pattern.           (Mandatory)

'%f' format spec is for filenames.         (Mandatory)

If your grep version doesn't support the --exclude/include args
don't specify the '%e' format spec.

Helm also support ack-grep and git-grep ,
here a default command example for ack-grep:

\(setq helm-grep-default-command \"ack-grep -Hn --no-group --no-color %e %p %f\"
       helm-grep-default-recurse-command \"ack-grep -H --no-group --no-color %e %p %f\")

You can ommit the %e spec if you don't want to be prompted for types.
`helm-grep-default-command' and `helm-grep-default-recurse-command'are
independents, so you can enable `helm-grep-default-command' with ack-grep
and `helm-grep-default-recurse-command' with grep if you want to be faster
on recursive grep.
NOTE: remote grepping is not available with ack-grep."
  :group 'helm-grep
  :type  'string)

(defcustom helm-grep-default-recurse-command
  "grep -a -d recurse %e -n%cH -e %p %f"
  "Default recursive grep format command for `helm-do-grep-1'.
See `helm-grep-default-command' for format specs and infos about ack-grep."
  :group 'helm-grep
  :type  'string)

(defcustom helm-default-zgrep-command
  "zgrep -a -n%cH -e %p %f"
  "Default command for Zgrep.
See `helm-grep-default-command' for infos on format specs."
  :group 'helm-grep
  :type  'string)

(defcustom helm-ack-grep-executable "ack-grep"
  "Default ack-grep command."
  :group 'helm-grep
  :type  'string)

(defcustom helm-pdfgrep-default-command
  "pdfgrep --color never -niH %s %s"
  "Default command for pdfgrep."
  :group 'helm-grep
  :type  'string)

(defcustom helm-grep-use-ioccur-style-keys t
  "Use Arrow keys to jump to occurences."
  :group 'helm-grep
  :type  'boolean)

(defcustom helm-pdfgrep-default-read-command nil
  "Default command to read pdf files from pdfgrep.
Where '%f' format spec is filename and '%p' is page number.
e.g In Ubuntu you can set it to:

    \"evince --page-label=%p '%f'\"

If set to nil `doc-view-mode' will be used instead of an external command."
  :group 'helm-grep
  :type  'string)

(defcustom helm-grep-max-length-history 100
  "Max number of elements to save in `helm-grep-history'."
  :group 'helm-grep
  :type 'integer)

(defcustom helm-zgrep-file-extension-regexp
  ".*\\(\\.gz\\|\\.bz\\|\\.xz\\|\\.lzma\\)$"
  "Default file extensions zgrep will search in."
  :group 'helm-grep
  :type 'string)

(defcustom helm-do-grep-preselect-candidate nil
  "When non--nil the file name of current buffer will be selected."
  :group 'helm-grep
  :type 'boolean)

(defcustom helm-grep-preferred-ext nil
  "This file extension will be preselected for grep."
  :group 'helm-grep
  :type  'string)


;;; Faces
;;
;;
(defface helm-grep-match
    '((t (:inherit match)))
  "Face used to highlight grep matches."
  :group 'helm-grep)

(defface helm-grep-file
    '((t (:foreground "BlueViolet"
          :underline t)))
  "Face used to highlight grep results filenames."
  :group 'helm-grep)

(defface helm-grep-lineno
    '((t (:foreground "Darkorange1")))
  "Face used to highlight grep number lines."
  :group 'helm-grep)

(defface helm-grep-running
    '((t (:foreground "Red")))
  "Face used in mode line when grep is running."
  :group 'helm-grep)

(defface helm-grep-finish
    '((t (:foreground "Green")))
  "Face used in mode line when grep is finish."
  :group 'helm-grep)

(defface helm-grep-cmd-line
    '((t (:inherit diff-added)))
  "Face used to highlight grep command line when no results."
  :group 'helm-grep)


;;; Keymaps
;;
;;
(defvar helm-grep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-c o")    'helm-grep-run-other-window-action)
    (define-key map (kbd "C-c C-o")  'helm-grep-run-other-frame-action)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-x C-s")  'helm-grep-run-save-buffer)
    (when helm-grep-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'helm-grep-run-persistent-action)
      (define-key map (kbd "<left>")  'helm-grep-run-default-action))
    (define-key map (kbd "C-c ?")    'helm-grep-help)
    (delq nil map))
  "Keymap used in Grep sources.")

(defvar helm-pdfgrep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'helm-pdfgrep-help)
    map)
  "Keymap used in pdfgrep.")

(defvar helm-grep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'helm-grep-mode-jump)
    (define-key map (kbd "C-o")      'helm-grep-mode-jump-other-window)
    (define-key map (kbd "<C-down>") 'helm-grep-mode-jump-other-window-forward)
    (define-key map (kbd "<C-up>")   'helm-grep-mode-jump-other-window-backward)
    (define-key map (kbd "<M-down>") 'helm-gm-next-file)
    (define-key map (kbd "<M-up>")   'helm-gm-precedent-file)
    map))


;;; Internals vars
;;
;;
(defvar helm-rzgrep-cache (make-hash-table :test 'equal))
(defvar helm-grep-default-function 'helm-grep-init)
(defvar helm-zgrep-recurse-flag nil)
(defvar helm-grep-history nil)
(defvar helm-grep-last-targets nil)
(defvar helm-grep-include-files nil)
(defvar helm-grep-in-recurse nil)
(defvar helm-grep-use-zgrep nil)
(defvar helm-grep-last-default-directory nil)
(defvar helm-grep-default-directory-fn nil
  "A function that should return a directory to expand candidate to.
It is intended to use as a let-bound variable, DON'T set this globaly.")
(defvar helm-pdfgrep-targets nil)
(defvar helm-grep-last-cmd-line nil)
(defvar helm-grep-split-line-regexp "^\\([a-zA-Z]?:?.*?\\):\\([0-9]+\\):\\(.*\\)")


;;; Init
;;
;;
(defun helm-grep-prepare-candidates (candidates)
  "Prepare filenames and directories CANDIDATES for grep command line."
  ;; If one or more candidate is a directory, search in all files
  ;; of this candidate (e.g /home/user/directory/*).
  ;; If r option is enabled search also in subdidrectories.
  ;; We need here to expand wildcards to support crap windows filenames
  ;; as grep doesn't accept quoted wildcards (e.g "dir/*.el").
  (setq candidates (if (file-remote-p default-directory)
                       ;; Grep don't understand tramp filenames
                       ;; use the local name.
                       (mapcar #'(lambda (x)
                                   (file-remote-p x 'localname))
                               candidates)
                     candidates))
  (if helm-zgrep-recurse-flag
      (mapconcat 'shell-quote-argument candidates " ")
    ;; When candidate is a directory, search in all its files.
    ;; NOTE that `file-expand-wildcards' will return also
    ;; directories, they will be ignored by grep but not
    ;; by ack-grep that will grep all files of this directory
    ;; without recursing in their subdirs though, see that as a one
    ;; level recursion with ack-grep.
    ;; So I leave it as it is, considering it is a feature. [1]
    (cl-loop for i in candidates append
          (cond ((string-match "^git" helm-grep-default-command)
                 (list i))
                ;; Candidate is a directory and we use recursion or ack.
                ((and (file-directory-p i)
                      (or helm-grep-in-recurse
                          ;; ack-grep accept directory [1].
                          (helm-grep-use-ack-p)))
                 (list (expand-file-name i)))
                ;; Grep doesn't support directory only when not in recurse.
                ((file-directory-p i)
                 (file-expand-wildcards
                  (concat (file-name-as-directory (expand-file-name i)) "*") t))
                ;; Candidate is a file or wildcard and we use recursion, use the
                ;; current directory instead of candidate.
                ((and (or (file-exists-p i) (string-match "[*]" i))
                      helm-grep-in-recurse)
                 (list (expand-file-name
                        (directory-file-name ; Needed for windoze.
                         (file-name-directory (directory-file-name i))))))
                ;; Else should be one or more file/directory
                ;; possibly marked.
                ;; When real is a normal filename without wildcard
                ;; file-expand-wildcards returns a list of one file.
                ;; wildcards should have been already handled by
                ;; helm-read-file-name or helm-find-files but do it from
                ;; here too in case we are called from elsewhere.
                (t (file-expand-wildcards i t))) into all-files ; [1]
          finally return
          (if (string-match "^git" helm-grep-default-command)
              (mapconcat 'identity all-files " ")
            (mapconcat 'shell-quote-argument all-files " ")))))

(defun helm-grep-command (&optional recursive)
  (let ((com (car (split-string (if recursive
                                    helm-grep-default-recurse-command
                                  helm-grep-default-command) " "))))
    (if (string= com "git") "git-grep" com)))

(cl-defun helm-grep-use-ack-p (&key where)
  (cl-case where
    (default (string= (helm-grep-command) helm-ack-grep-executable))
    (recursive (string= (helm-grep-command t) helm-ack-grep-executable))
    (strict (and (string= (helm-grep-command t) helm-ack-grep-executable)
                 (string= (helm-grep-command) helm-ack-grep-executable)))
    (t (and (not (string= (helm-grep-command) "git-grep"))
            (or (string= (helm-grep-command) helm-ack-grep-executable)
                (string= (helm-grep-command t) helm-ack-grep-executable))))))

(defun helm-grep--prepare-cmd-line (only-files &optional include zgrep)
  (let* ((default-directory (or helm-default-directory
                                (expand-file-name helm-ff-default-directory)))
         (fnargs            (helm-grep-prepare-candidates only-files))
         (ignored-files     (unless (helm-grep-use-ack-p)
                              (mapconcat
                               #'(lambda (x)
                                   (concat "--exclude="
                                           (shell-quote-argument x)))
                               grep-find-ignored-files " ")))
         (ignored-dirs      (unless (helm-grep-use-ack-p)
                              (mapconcat
                               ;; Need grep version >=2.5.4
                               ;; of Gnuwin32 on windoze.
                               #'(lambda (x)
                                   (concat "--exclude-dir="
                                           (shell-quote-argument x)))
                               grep-find-ignored-directories " ")))
         (exclude           (unless (helm-grep-use-ack-p)
                              (if helm-grep-in-recurse
                                  (concat (or include ignored-files)
                                          " " ignored-dirs)
                                ignored-files)))
         (types             (and (helm-grep-use-ack-p)
                                 ;; When %e format spec is not specified
                                 ;; in `helm-grep-default-command'
                                 ;; we need to pass an empty string
                                 ;; to types to avoid error.
                                 (or include "")))
         (smartcase         (if (helm-grep-use-ack-p) ""
                              (unless (let ((case-fold-search nil))
                                        (string-match-p
                                         "[A-Z]" helm-pattern)) "i"))))
    (format-spec
     helm-grep-default-command
     (delq nil
           (list (unless zgrep
                   (if types
                       (cons ?e types)
                     (cons ?e exclude)))
                 (cons ?c (or smartcase ""))
                 (cons ?p (shell-quote-argument helm-pattern))
                 (cons ?f fnargs))))))

(defun helm-grep-init (cmd-line)
  "Start an asynchronous grep process with CMD-LINE using ZGREP if non--nil."
  (let* ((zgrep (string-match "\\`zgrep" cmd-line))
         ;; Use pipe only with grep, zgrep or git-grep.
         (process-connection-type (and (not zgrep) (helm-grep-use-ack-p)))
         (tramp-verbose helm-tramp-verbose))
    ;; Start grep process.
    (helm-log "Starting Grep process in directory `%s'" default-directory)
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd-line 'face 'helm-grep-cmd-line) "\n\n"))
    (prog1            ; This function should return the process first.
        (start-file-process-shell-command
         "grep" helm-buffer cmd-line)
      ;; Init sentinel.
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (process event)
           (let ((noresult (= (process-exit-status process) 1)))
             (unless noresult
               (helm-process-deferred-sentinel-hook
                process event helm-ff-default-directory))
             (cond ((and noresult
                         ;; [FIXME] This is a workaround for zgrep
                         ;; that exit with code 1
                         ;; after a certain amount of results.
                         (not (with-helm-buffer helm-grep-use-zgrep)))
                    (with-current-buffer helm-buffer
                      (insert (concat "* Exit with code 1, no result found,"
                                      " Command line was:\n\n "
                                      (propertize helm-grep-last-cmd-line
                                                  'face 'helm-grep-cmd-line)))
                      (setq mode-line-format
                            '(" " mode-line-buffer-identification " "
                              (line-number-mode "%l") " "
                              (:eval (propertize
                                      (format
                                       "[%s process finished - (no results)] "
                                       (if helm-grep-use-zgrep
                                           "Zgrep"
                                         (capitalize
                                          (if helm-grep-in-recurse
                                              (helm-grep-command t)
                                            (helm-grep-command)))))
                                      'face 'helm-grep-finish))))))
                   ((string= event "finished\n")
                    (with-helm-window
                      (setq mode-line-format
                            '(" " mode-line-buffer-identification " "
                              (line-number-mode "%l") " "
                              (:eval (propertize
                                      (format
                                       "[%s process finished - (%s results)] "
                                       (if helm-grep-use-zgrep
                                           "Zgrep"
                                         (capitalize
                                          (if helm-grep-in-recurse
                                              (helm-grep-command t)
                                            (helm-grep-command))))
                                       (max (1- (count-lines
                                                 (point-min)
                                                 (point-max)))
                                            0))
                                      'face 'helm-grep-finish))))
                      (force-mode-line-update)))
                   ;; Catch error output in log.
                   (t (helm-log
                       "Error: %s %s"
                       (if helm-grep-use-zgrep "Zgrep" "Grep")
                       (replace-regexp-in-string "\n" "" event))))))))))

(defun helm-grep-collect-candidates ()
  (let ((cmd-line (helm-grep--prepare-cmd-line
                   helm-grep-last-targets
                   helm-grep-include-files
                   helm-grep-use-zgrep)))
    (set (make-local-variable 'helm-grep-last-cmd-line) cmd-line)
    (funcall helm-grep-default-function cmd-line)))


;;; Actions
;;
;;
(defun helm-grep-action (candidate &optional where mark)
  "Define a default action for `helm-do-grep' on CANDIDATE.
WHERE can be one of other-window, elscreen, other-frame."
  (let* ((split        (helm-grep-split-line candidate))
         (lineno       (string-to-number (nth 1 split)))
         (loc-fname    (or (with-current-buffer
                               (if (eq major-mode 'helm-grep-mode)
                                   (current-buffer)
                                 helm-buffer)
                             (get-text-property (point-at-bol) 'help-echo))
                           (car split)))
         (tramp-method (file-remote-p (or helm-ff-default-directory
                                          default-directory) 'method))
         (tramp-host   (file-remote-p (or helm-ff-default-directory
                                          default-directory) 'host))
         (tramp-prefix (concat "/" tramp-method ":" tramp-host ":"))
         (fname        (if tramp-host
                           (concat tramp-prefix loc-fname) loc-fname)))
    (cl-case where
      (other-window (find-file-other-window fname))
      (elscreen     (helm-elscreen-find-file fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-grep-save-results-1))
      (pdf          (if helm-pdfgrep-default-read-command
                        (helm-pdfgrep-action-1 split lineno (car split))
                      (find-file (car split)) (doc-view-goto-page lineno)))
      (t            (find-file fname)))
    (unless (or (eq where 'grep) (eq where 'pdf))
      (helm-goto-line lineno))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))
    ;; Save history
    (unless (or helm-in-persistent-action
                (eq major-mode 'helm-grep-mode)
                (string= helm-pattern ""))
      (setq helm-grep-history
            (cons helm-pattern
                  (delete helm-pattern helm-grep-history)))
      (when (> (length helm-grep-history)
               helm-grep-max-length-history)
        (setq helm-grep-history
              (delete (car (last helm-grep-history))
                      helm-grep-history))))))

(defun helm-grep-persistent-action (candidate)
  "Persistent action for `helm-do-grep'.
With a prefix arg record CANDIDATE in `mark-ring'."
  (if current-prefix-arg
      (helm-grep-action candidate nil 'mark)
    (helm-grep-action candidate))
  (helm-highlight-current-line))

(defun helm-grep-other-window (candidate)
  "Jump to result in other window from helm grep."
  (helm-grep-action candidate 'other-window))

(defun helm-grep-other-frame (candidate)
  "Jump to result in other frame from helm grep."
  (helm-grep-action candidate 'other-frame))

(defun helm-grep-jump-elscreen (candidate)
  "Jump to result in elscreen from helm grep."
  (helm-grep-action candidate 'elscreen))

(defun helm-goto-next-or-prec-file (n &optional type)
  "Go to next or precedent candidate file in helm grep/etags buffers.
If N is positive go forward otherwise go backward."
  (let* ((allow-mode (or (eq major-mode 'helm-grep-mode)
                         (eq major-mode 'helm-moccur-mode)))
         (sel (if allow-mode
                  (buffer-substring (point-at-bol) (point-at-eol))
                (helm-get-selection nil t)))
         (current-line-list  (if (eq type 'etags)
                                 (split-string sel ": +" t)
                               (helm-grep-split-line sel)))
         (current-fname      (nth 0 current-line-list))
         (bob-or-eof         (if (eq n 1) 'eobp 'bobp))
         (mark-maybe #'(lambda ()
                         (if allow-mode
                             (ignore)
                           (helm-mark-current-line)))))
    (catch 'break
      (while (not (funcall bob-or-eof))
        (forward-line n) ; Go forward or backward depending of n value.
        ;; Exit when current-fname is not matched or in `helm-grep-mode'
        ;; the line is not a grep line i.e 'fname:num:tag'.
        (setq sel (buffer-substring (point-at-bol) (point-at-eol)))
        (unless (or (string= current-fname
                             (car (if (eq type 'etags)
                                      (split-string sel ": +" t)
                                    (helm-grep-split-line sel))))
                    (and (eq major-mode 'helm-grep-mode)
                         (not (get-text-property (point-at-bol) 'help-echo))))
          (funcall mark-maybe)
          (throw 'break nil))))
    (cond ((and (> n 0) (eobp))
           (re-search-backward ".")
           (forward-line 0)
           (funcall mark-maybe))
          ((and (< n 0) (bobp))
           (helm-aif (next-single-property-change (point-at-bol) 'help-echo)
               (goto-char it)
             (forward-line 1))
           (funcall mark-maybe)))))

;;;###autoload
(defun helm-goto-precedent-file ()
  "Go to precedent file in helm grep/etags buffers."
  (interactive)
  (let ((etagp (when (string= (assoc-default
                               'name (helm-get-current-source)) "Etags")
                 'etags)))
    (with-helm-window
      (helm-goto-next-or-prec-file -1 etagp))))

;;;###autoload
(defun helm-goto-next-file ()
  "Go to precedent file in helm grep/etags buffers."
  (interactive)
  (let ((etagp (when (string= (assoc-default
                               'name (helm-get-current-source)) "Etags")
                 'etags)))
    (with-helm-window
      (helm-goto-next-or-prec-file 1 etagp))))

(defun helm-grep-run-persistent-action ()
  "Run grep persistent action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'jump-persistent 'helm-grep-persistent-action)
    (helm-execute-persistent-action 'jump-persistent)))

(defun helm-grep-run-default-action ()
  "Run grep default action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-grep-action)))

(defun helm-grep-run-other-window-action ()
  "Run grep goto other window action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-grep-other-window)))

(defun helm-grep-run-other-frame-action ()
  "Run grep goto other frame action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-grep-other-frame)))

;;;###autoload
(defun helm-grep-run-save-buffer ()
  "Run grep save results action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-grep-save-results)))


;;; helm-grep-mode
;;
;;
(defun helm-grep-save-results (candidate)
  (helm-grep-action candidate 'grep))

(defun helm-grep-save-results-1 ()
  "Save helm grep result in a `helm-grep-mode' buffer."
  (let ((buf "*hgrep*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (read-string "GrepBufferName: " buf))
      (cl-loop for b in (helm-buffer-list)
            when (and (string= new-buf b)
                      (not (y-or-n-p
                            (format "Buffer `%s' already exists overwrite? "
                                    new-buf))))
            do (setq new-buf (read-string "GrepBufferName: " "*hgrep ")))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "-*- mode: helm-grep -*-\n\n"
                (format "Grep Results for `%s':\n\n" helm-pattern))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max))))))
      (helm-grep-mode) (pop-to-buffer buf))
    (message "Helm Grep Results saved in `%s' buffer" buf)))

;;;###autoload
(define-derived-mode helm-grep-mode
    special-mode "helm-grep"
    "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-grep-mode-map}"
    (set (make-local-variable 'helm-grep-last-cmd-line)
         (with-helm-buffer helm-grep-last-cmd-line))
    (set (make-local-variable 'revert-buffer-function)
         #'helm-grep-mode--revert-buffer-function))

(defun helm-grep-mode--revert-buffer-function (&optional _ignore-auto _noconfirm)
  (goto-char (point-min))
  (re-search-forward "^Grep Results for" nil t)
  (forward-line 0)
  (when (re-search-forward "^$" nil t)
    (forward-line 1))
  (let ((inhibit-read-only t))
    (delete-region (point) (point-max)))
  (message "Reverting buffer...")
  (set-process-sentinel
   (start-file-process-shell-command
    "hgrep"  (generate-new-buffer "*hgrep revert*") helm-grep-last-cmd-line)
   (lambda (process event)
     (when (string= event "finished\n")
       (with-current-buffer (current-buffer)
         (let ((inhibit-read-only t))
           (save-excursion
             (cl-loop for line in (with-current-buffer (process-buffer process)
                                    (prog1 (split-string (buffer-string) "\n")
                                      (kill-buffer)))
                      when (string-match helm-grep-split-line-regexp line)
                      do (insert (propertize
                                  (car (helm-grep-filter-one-by-one line))
                                  ;; needed for wgrep.
                                  'helm-realvalue line)
                                 "\n"))))
         (message "Reverting buffer done"))))))

;;;###autoload
(defun helm-gm-next-file ()
  (interactive)
  (helm-goto-next-or-prec-file 1))

;;;###autoload
(defun helm-gm-precedent-file ()
  (interactive)
  (helm-goto-next-or-prec-file -1))

;;;###autoload
(defun helm-grep-mode-jump ()
  (interactive)
  (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
    (condition-case nil
        (progn (helm-grep-action candidate) (delete-other-windows))
      (error nil))))

(defun helm-grep-mode-jump-other-window-1 (arg)
  (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
    (condition-case nil
        (progn
          (save-selected-window
            (helm-grep-action candidate 'other-window)
            (recenter))
          (forward-line arg))
      (error nil))))

;;;###autoload
(defun helm-grep-mode-jump-other-window-forward ()
  (interactive)
  (helm-grep-mode-jump-other-window-1 1))

;;;###autoload
(defun helm-grep-mode-jump-other-window-backward ()
  (interactive)
  (helm-grep-mode-jump-other-window-1 -1))

;;;###autoload
(defun helm-grep-mode-jump-other-window ()
  (interactive)
  (let ((candidate (buffer-substring (point-at-bol) (point-at-eol))))
    (condition-case nil
        (helm-grep-action candidate 'other-window)
      (error nil))))


;;; ack-grep types
;;
;;
(defun helm-grep-hack-types ()
  "Return a list of known ack-grep types."
  (with-temp-buffer
    ;; "--help-types" works with both 1.96 and 2.1+, while
    ;; "--help types" works only with 1.96 Issue #422
    (call-process helm-ack-grep-executable nil t nil "--help-types")
    (goto-char (point-min))
    (cl-loop while (re-search-forward
                    "^ *--\\(\\[no\\]\\)\\([^. ]+\\) *\\(.*\\)" nil t)
          collect (cons (concat (match-string 2)
                                " [" (match-string 3) "]")
                        (match-string 2))
          collect (cons (concat "no" (match-string 2)
                                " [" (match-string 3) "]")
                        (concat "no" (match-string 2))))))

(defun helm-grep-ack-types-transformer (candidates _source)
  (cl-loop for i in candidates
        if (stringp i)
        collect (rassoc i helm-grep-ack-types-cache)
        else
        collect i))

(defvar helm-grep-ack-types-cache nil)
(defun helm-grep-read-ack-type ()
  "Select types for the '--type' argument of ack-grep."
  (require 'helm-mode)
  (require 'helm-adaptive)
  (setq helm-grep-ack-types-cache (helm-grep-hack-types))
  (let ((types (helm-comp-read
                "Types: " helm-grep-ack-types-cache
                :name "*Ack-grep types*"
                :marked-candidates t
                :must-match t
                :fc-transformer '(helm-adaptive-sort
                                  helm-grep-ack-types-transformer)
                :buffer "*helm ack-types*")))
    (mapconcat #'(lambda (type) (concat "--type=" type)) types " ")))


;;; grep extensions
;;
;;
(defun helm-grep-guess-extensions (files)
  "Try to guess file extensions in FILES list when using grep recurse.
These extensions will be added to command line with --include arg of grep."
  (cl-loop with ext-list = (list helm-grep-preferred-ext "*")
        with lst = (if (file-directory-p (car files))
                       (directory-files
                        (car files) nil
                        directory-files-no-dot-files-regexp)
                     files)
        for i in lst
        for ext = (file-name-extension i 'dot)
        for glob = (and ext (not (string= ext ""))
                        (concat "*" ext))
        unless (or (not glob)
                   (and glob-list (member glob glob-list))
                   (and glob-list (member glob ext-list))
                   (and glob-list (member glob grep-find-ignored-files)))
        collect glob into glob-list
        finally return (delq nil (append ext-list glob-list))))

(defun helm-grep-get-file-extensions (files)
  "Try to return a list of file extensions to pass to include arg of grep."
  (let* ((all-exts (helm-grep-guess-extensions
                    (mapcar 'expand-file-name files)))
         (extensions (helm-comp-read "Search Only in: " all-exts
                                     :marked-candidates t
                                     :fc-transformer 'helm-adaptive-sort
                                     :buffer "*helm grep exts*"
                                     :name "*helm grep extensions*")))
    (if (listp extensions) ; Otherwise it is empty string returned by C-RET.
        ;; If extensions is a list of one string containing spaces,
        ;; assume user entered more than one glob separated by space(s) and
        ;; split this string to pass it later to mapconcat.
        ;; e.g '("*.el *.py")
        (cl-loop for i in extensions
              append (split-string-and-unquote i " "))
      (list "*"))))


;;; Set up source
;;
;;
(defvar helm-source-grep nil)
(defun helm-do-grep-1 (targets &optional recurse zgrep exts)
  "Launch grep on a list of TARGETS files.
When RECURSE is given use -r option of grep and prompt user
to set the --include args of grep.
You can give more than one arg separated by space.
e.g *.el *.py *.tex.
If you are using ack-grep, you will be prompted for --type
instead.
If prompt is empty '--exclude `grep-find-ignored-files' is used instead.
ZGREP when non--nil use zgrep instead, without prompting for a choice
in recurse, search being made on `helm-zgrep-file-extension-regexp'."
  (when (and (helm-grep-use-ack-p)
             helm-ff-default-directory
             (file-remote-p helm-ff-default-directory))
    (error "Error: Remote operation not supported with ack-grep."))
  (let* ((exts (and recurse
                    ;; [FIXME] I could handle this from helm-walk-directory.
                    (not zgrep) ; zgrep doesn't handle -r opt.
                    (not (helm-grep-use-ack-p :where 'recursive))
                    (or exts (helm-grep-get-file-extensions targets))))
         (include-files (and exts
                             (mapconcat #'(lambda (x)
                                            (concat "--include="
                                                    (shell-quote-argument x)))
                                        (if (> (length exts) 1)
                                            (remove "*" exts)
                                          exts) " ")))
         (types (and (not include-files)
                     (not zgrep)
                     recurse
                     ;; When %e format spec is not specified
                     ;; ignore types and do not prompt for choice.
                     (string-match "%e" helm-grep-default-command)
                     (helm-grep-read-ack-type)))
         (follow (and helm-follow-mode-persistent
                      (assoc-default 'follow helm-source-grep))))
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    ;; If `helm-find-files' haven't already started,
    ;; give a default value to `helm-ff-default-directory'.
    (unless helm-ff-default-directory
      (setq helm-ff-default-directory default-directory))
    ;; We need to store these vars locally
    ;; to pass infos later to `helm-resume'.
    (with-helm-temp-hook 'helm-after-initialize-hook
      (with-helm-buffer
        (set (make-local-variable 'helm-zgrep-recurse-flag)
             (and recurse zgrep))
        (set (make-local-variable 'helm-grep-last-targets) targets)
        (set (make-local-variable 'helm-grep-include-files)
             (or include-files types))
        (set (make-local-variable 'helm-grep-in-recurse) recurse)
        (set (make-local-variable 'helm-grep-use-zgrep) zgrep)
        (set (make-local-variable 'helm-grep-last-default-directory)
             helm-ff-default-directory)
        (set (make-local-variable 'helm-grep-default-command)
             (cond (helm-grep-use-zgrep helm-default-zgrep-command)
                   (helm-grep-in-recurse helm-grep-default-recurse-command)
                   ;; When resuming the local value of
                   ;; `helm-grep-default-command' is used, only git-grep
                   ;; should need this.
                   (t helm-grep-default-command)))))
    ;; Setup the source.
    (setq helm-source-grep
          `((name . ,(if zgrep "Zgrep" (capitalize (if recurse
                                                       (helm-grep-command t)
                                                     (helm-grep-command)))))
            (header-name . (lambda (name)
                             (concat name "(C-c ? Help)")))
            (candidates-process . helm-grep-collect-candidates)
            (filter-one-by-one . helm-grep-filter-one-by-one)
            (candidate-number-limit . 9999)
            (no-matchplugin)
            (nohighlight)
            (mode-line . helm-grep-mode-line-string)
            ;; We need to specify keymap here and as :keymap arg [1]
            ;; to make it available in further resuming.
            (keymap . ,helm-grep-map)
            (history . ,'helm-grep-history)
            (action . ,(delq
                        nil
                        `(("Find File" . helm-grep-action)
                          ("Find file other frame" . helm-grep-other-frame)
                          ,(and (locate-library "elscreen")
                                '("Find file in Elscreen"
                                  . helm-grep-jump-elscreen))
                          ("Save results in grep buffer" . helm-grep-save-results)
                          ("Find file other window" . helm-grep-other-window))))
            (persistent-action . helm-grep-persistent-action)
            (persistent-help . "Jump to line (`C-u' Record in mark ring)")
            (requires-pattern . 2)))
    (and follow (helm-attrset 'follow follow helm-source-grep))
    (helm
     :sources '(helm-source-grep)
     :buffer (format "*helm %s*" (if zgrep "zgrep" (helm-grep-command recurse)))
     :default-directory helm-grep-last-default-directory
     :keymap helm-grep-map ; [1]
     :history 'helm-grep-history
     :truncate-lines t)))


;;; zgrep
;;
;;
(defun helm-ff-zgrep-1 (flist recursive)
  (unwind-protect
       (let* ((def-dir (or helm-ff-default-directory
                           default-directory))
              (only    (if recursive
                           (or (gethash def-dir helm-rzgrep-cache)
                               (puthash
                                def-dir
                                (helm-walk-directory
                                 def-dir
                                 :directories nil
                                 :path 'full
                                 :match helm-zgrep-file-extension-regexp)
                                helm-rzgrep-cache))
                         flist)))
         (helm-do-grep-1 only recursive 'zgrep))
    (setq helm-zgrep-recurse-flag nil)))


;;; transformers
;;
;;
(defun helm-grep-split-line (line)
  "Split a grep output line."
  ;; The output of grep may send a truncated line in this chunk,
  ;; so don't split until grep line is valid, that is
  ;; once the second part of the line comes with next chunk
  ;; send by process.
  (when (string-match helm-grep-split-line-regexp line)
    ;; Don't use split-string because buffer/file name or string
    ;; may contain a ":".
    (cl-loop for n from 1 to 3 collect (match-string n line))))

(defun helm-grep--filter-candidate-1 (candidate &optional dir)
  (let* ((root   (or dir (and helm-grep-default-directory-fn
                              (funcall helm-grep-default-directory-fn))))
         (split  (helm-grep-split-line candidate))
         (fname  (if (and root split)
                     (expand-file-name (car split) root)
                   (car-safe split)))
         (lineno (nth 1 split))
         (str    (nth 2 split)))
    (when (and fname lineno str)
      (cons (concat (propertize (file-name-nondirectory fname)
                                'face 'helm-grep-file
                                'help-echo fname) ":"
                                (propertize lineno 'face 'helm-grep-lineno) ":"
                                (helm-grep-highlight-match str))
            candidate))))

(defun helm-grep-filter-one-by-one (candidate)
  "`filter-one-by-one' transformer function for `helm-do-grep'."
  (let ((helm-grep-default-directory-fn
         (or helm-grep-default-directory-fn
             (lambda () (or helm-ff-default-directory
                            helm-default-directory
                            default-directory)))))
    (helm-grep--filter-candidate-1 candidate)))

(defun helm-grep-highlight-match (str &optional multi-match)
  "Highlight in string STR all occurences matching `helm-pattern'."
  (require 'helm-match-plugin)
  (let (beg end)
    (condition-case nil
        (with-temp-buffer
          (insert str)
          (goto-char (point-min))
          (cl-loop for reg in (if multi-match
                                  (cl-loop for r in (helm-mp-split-pattern
                                                     helm-pattern)
                                        unless (string-match "\\`!" r)
                                        collect r)
                                (list helm-pattern))
                do
                (while (and (re-search-forward reg nil t)
                            (> (- (setq end (match-end 0))
                                  (setq beg (match-beginning 0))) 0))
                  (add-text-properties beg end '(face helm-grep-match)))
                do (goto-char (point-min))) 
          (buffer-string))
      (error nil))))


;;; Grep from buffer list
;;
;;
(defun helm-grep-buffers-1 (candidate &optional zgrep)
  "Run grep on all file--buffers or CANDIDATE if it is a file--buffer.
If one of selected buffers is not a file--buffer,
it is ignored and grep will run on all others file--buffers.
If only one candidate is selected and it is not a file--buffer,
switch to this buffer and run `helm-occur'.
If a prefix arg is given run grep on all buffers ignoring non--file-buffers."
  (let* ((prefarg (or current-prefix-arg helm-current-prefix-arg))
         (helm-ff-default-directory
          (if (and helm-ff-default-directory
                   (file-remote-p helm-ff-default-directory))
              default-directory
            helm-ff-default-directory))
         (cands (if prefarg
                    (buffer-list)
                  (helm-marked-candidates)))
         (win-conf (current-window-configuration))
         ;; Non--fname and remote buffers are ignored.
         (bufs (cl-loop for buf in cands
                     for fname = (buffer-file-name (get-buffer buf))
                     when (and fname (not (file-remote-p fname)))
                     collect (expand-file-name fname))))
    (if bufs
        (if zgrep
            (helm-do-grep-1 bufs nil 'zgrep)
          (helm-do-grep-1 bufs))
      ;; bufs is empty, thats mean we have only CANDIDATE
      ;; and it is not a buffer-filename, fallback to occur.
      (helm-switch-to-buffer candidate)
      (when (get-buffer helm-action-buffer)
        (kill-buffer helm-action-buffer))
      (helm-occur)
      (when (eq helm-exit-status 1)
        (set-window-configuration win-conf)))))

(defun helm-grep-buffers (candidate)
  "Action to grep buffers."
  (helm-grep-buffers-1 candidate))

(defun helm-zgrep-buffers (candidate)
  "Action to zgrep buffers."
  (helm-grep-buffers-1 candidate 'zgrep))


;;; Helm interface for pdfgrep
;;  pdfgrep program <http://pdfgrep.sourceforge.net/>
;;  and a pdf-reader (e.g xpdf) are needed.
;;
(defvar helm-pdfgrep-default-function 'helm-pdfgrep-init)
(defun helm-pdfgrep-init (only-files)
  "Start an asynchronous pdfgrep process in ONLY-FILES list."
  (let* ((default-directory (or helm-ff-default-directory
                                default-directory))
         (fnargs   (helm-grep-prepare-candidates
                    (if (file-remote-p default-directory)
                        (mapcar #'(lambda (x)
                                    (file-remote-p x 'localname))
                                only-files)
                      only-files)))
         (cmd-line (format helm-pdfgrep-default-command
                           helm-pattern
                           fnargs))
         process-connection-type)
    ;; Start pdf grep process.
    (helm-log "Starting Pdf Grep process in directory `%s'" default-directory)
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd-line 'face 'helm-grep-cmd-line) "\n\n"))
    (prog1
        (start-file-process-shell-command
         "pdfgrep" helm-buffer cmd-line)
      (message nil)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (_process event)
           (if (string= event "finished\n")
               (with-helm-window
                 (setq mode-line-format
                       '(" " mode-line-buffer-identification " "
                         (line-number-mode "%l") " "
                         (:eval (propertize
                                 (format "[Pdfgrep Process Finish - %s result(s)] "
                                         (max (1- (count-lines
                                                   (point-min) (point-max))) 0))
                                 'face 'helm-grep-finish))))
                 (force-mode-line-update))
             (helm-log "Error: Pdf grep %s"
                       (replace-regexp-in-string "\n" "" event))))))))

(defun helm-do-pdfgrep-1 (only)
  "Launch pdfgrep with a list of ONLY files."
  (unless (executable-find "pdfgrep")
    (error "Error: No such program `pdfgrep'."))
  (let* (helm-grep-in-recurse) ; recursion is never used in pdfgrep.
    ;; When called as action from an other source e.g *-find-files
    ;; we have to kill action buffer.
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))
    (setq helm-pdfgrep-targets only)
    (helm
     :sources
     `(((name . "PdfGrep")
        (init . (lambda ()
                  ;; If `helm-find-files' haven't already started,
                  ;; give a default value to `helm-ff-default-directory'.
                  (setq helm-ff-default-directory (or helm-ff-default-directory
                                                      default-directory))))
        (candidates-process
         . (lambda ()
             (funcall helm-pdfgrep-default-function helm-pdfgrep-targets)))
        (filter-one-by-one . helm-grep-filter-one-by-one)
        (candidate-number-limit . 9999)
        (no-matchplugin)
        (nohighlight)
        (history . ,'helm-grep-history)
        (keymap . ,helm-pdfgrep-map)
        (mode-line . helm-pdfgrep-mode-line-string)
        (action . helm-pdfgrep-action)
        (persistent-help . "Jump to PDF Page")
        (requires-pattern . 2)))
     :buffer "*helm pdfgrep*"
     :history 'helm-grep-history)))

(defun helm-pdfgrep-action (candidate)
  (helm-grep-action candidate 'pdf))

(defun helm-pdfgrep-action-1 (_split pageno fname)
  (save-selected-window
    (start-file-process-shell-command
     "pdf-reader" nil
     (format-spec helm-pdfgrep-default-read-command
                  (list (cons ?f fname) (cons ?p pageno))))))

;;;###autoload
(defun helm-do-grep ()
  "Preconfigured helm for grep.
Contrarily to Emacs `grep', no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can also use wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep (recurse).
The prefix arg can be passed before or after start file selection.
See also `helm-do-grep-1'."
  (interactive)
  (require 'helm-mode)
  (let* ((preselection (or (dired-get-filename nil t)
                           (buffer-file-name (current-buffer))))
         (only    (helm-read-file-name
                   "Search in file(s): "
                   :marked-candidates t
                   :preselect (and helm-do-grep-preselect-candidate
                                   (if helm-ff-transformer-show-only-basename
                                       (helm-basename preselection)
                                     preselection))))
         (prefarg (or current-prefix-arg helm-current-prefix-arg)))
    (helm-do-grep-1 only prefarg)))

;;;###autoload
(defun helm-do-zgrep ()
  "Preconfigured helm for zgrep."
  (interactive)
  (require 'helm-mode)
  (let* ((prefarg (or current-prefix-arg helm-current-prefix-arg))
         (preselection (or (dired-get-filename nil t)
                           (buffer-file-name (current-buffer))))
         (ls (helm-read-file-name
              "Search in file(s): "
              :marked-candidates t
              :preselect (and helm-do-grep-preselect-candidate
                              (if helm-ff-transformer-show-only-basename
                                  (helm-basename preselection)
                                preselection)))))
    (helm-ff-zgrep-1 ls prefarg)))

;;;###autoload
(defun helm-do-pdfgrep ()
  "Preconfigured helm for pdfgrep."
  (interactive)
  (require 'helm-mode)
  (let* ((preselection (or (dired-get-filename nil t)
                           (buffer-file-name (current-buffer))))
         (only (helm-read-file-name
                "Search in file(s): "
                :marked-candidates t
                :test #'(lambda (file)
                          (or (string= (file-name-extension file) "pdf")
                              (string= (file-name-extension file) "PDF")
                              (file-directory-p file)))
                :preselect (and helm-do-grep-preselect-candidate
                                (if helm-ff-transformer-show-only-basename
                                    (helm-basename preselection)
                                  preselection))))
         (helm-grep-default-function 'helm-pdfgrep-init))
    (helm-do-pdfgrep-1 only)))


(provide 'helm-grep)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-grep.el ends here

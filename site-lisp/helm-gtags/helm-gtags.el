;;; helm-gtags.el --- GNU GLOBAL helm interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags
;; Version: 1.2.6
;; Package-Requires: ((helm "1.5.6") (cl-lib "0.5"))

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

;;; Commentary:

;; `helm-gtags.el' is a `helm' interface of GNU Global.
;; `helm-gtags.el' is not compatible `anything-gtags.el', but `helm-gtags.el'
;; is designed for fast search.

;;
;; To use this package, add these lines to your init.el or .emacs file:
;;
;;     ;; Enable helm-gtags-mode
;;     (add-hook 'c-mode-hook 'helm-gtags-mode)
;;     (add-hook 'c++-mode-hook 'helm-gtags-mode)
;;     (add-hook 'asm-mode-hook 'helm-gtags-mode)
;;
;;     ;; Set key bindings
;;     (eval-after-load "helm-gtags"
;;       '(progn
;;          (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;          (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;          (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;          (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;          (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;          (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;          (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))
;;

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-files)
(require 'which-func)
(require 'pulse)

(defgroup helm-gtags nil
  "GNU GLOBAL for helm"
  :group 'helm)

(defcustom helm-gtags-path-style 'root
  "Style of file path"
  :type '(choice (const :tag "Root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute Path" absolute))
  :group 'helm-gtags)

(defcustom helm-gtags-ignore-case nil
  "Ignore case in each search."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-read-only nil
  "Gtags read only mode."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-auto-update nil
  "*If non-nil, tag files are updated whenever a file is saved."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-pulse-at-cursor t
  "If non-nil, pulse at point after jumping"
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-cache-select-result nil
  "*If non-nil, results of helm-gtags-select and helm-gtags-select-path are cached."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-cache-max-result-size (* 10 1024 1024) ;10M
  "Max size(bytes) to cache for each select result."
  :type 'integer
  :group 'helm-gtags)

(defcustom helm-gtags-update-interval-second 60
  "Tags are updated in `after-save-hook' if this seconds is passed from last update.
Always update if value of this variable is nil."
  :type '(choice (integer :tag "Update interval seconds")
                 (boolean :tag "Update every time" nil))
  :group 'helm-gtags)

(defcustom helm-gtags-maximum-candidates 9999
  "Maximum number of helm candidates"
  :type 'integer
  :group 'helm-gtags)

(defcustom helm-gtags-highlight-candidate t
  "Highlight candidate or not"
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-use-input-at-cursor nil
  "Use input at cursor"
  :type 'boolean
  :group 'helm-gtags)

(defface helm-gtags-file
  '((t :inherit font-lock-keyword-face))
  "Face for line numbers in the error list."
  :group 'helm-gtags)

(defface helm-gtags-lineno
  '((t :inherit font-lock-doc-face))
  "Face for line numbers in the error list."
  :group 'helm-gtags)

(defface helm-gtags-match
  '((t :inherit helm-match))
  "Face for word matched against tagname"
  :group 'helm-gtags)

(defvar helm-gtags-tag-location nil
  "GNU global tag `GTAGS' location")

(defvar helm-gtags--last-update-time 0)

(defvar helm-gtags-buffer "*helm gtags*")

(defvar helm-gtags-prompt-alist
  '((:tag    . "Find Definition: ")
    (:pattern . "Find Pattern: ")
    (:rtag   . "Find Reference: ")
    (:symbol . "Find Symbol: ")
    (:file   . "Find File: ")))

(defvar helm-gtags-completing-history nil)
(defvar helm-gtags-context-stack (make-hash-table :test 'equal))
(defvar helm-gtags-result-cache (make-hash-table :test 'equal))
(defvar helm-gtags-saved-context nil)
(defvar helm-gtags-use-otherwin nil)
(defvar helm-gtags-local-directory nil)
(defvar helm-gtags-parsed-file nil)
(defvar helm-gtags--update-tags-buffer " *helm-gtags-update-tag*")
(defvar helm-gtags--current-position nil)
(defvar helm-gtags--real-tag-location nil)
(defvar helm-gtags--remote-p nil)
(defvar helm-gtags--last-input nil)

(defmacro helm-declare-obsolete-variable (old new version)
  `(progn
     (defvaralias ,old ,new)
     (make-obsolete-variable ,old ,new ,version)))

(helm-declare-obsolete-variable
 'helm-c-gtags-path-style 'helm-gtags-path-style "0.8")
(helm-declare-obsolete-variable
 'helm-c-gtags-ignore-case 'helm-gtags-ignore-case  "0.8")
(helm-declare-obsolete-variable
 'helm-c-gtags-read-only 'helm-gtags-read-only "0.8")

;; completsion function for completing-read.
(defun helm-gtags-completing-gtags (string predicate code)
  (helm-gtags-complete :tag string predicate code))
(defun helm-gtags-completing-pattern (string predicate code)
  (helm-gtags-complete :pattern string predicate code))
(defun helm-gtags-completing-grtags (string predicate code)
  (helm-gtags-complete :rtag string predicate code))
(defun helm-gtags-completing-gsyms (string predicate code)
  (helm-gtags-complete :symbol string predicate code))
(defun helm-gtags-completing-files (string predicate code)
  (helm-gtags-complete :file string predicate code))

(defvar helm-gtags-comp-func-alist
  '((:tag    . helm-gtags-completing-gtags)
    (:pattern . helm-gtags-completing-pattern)
    (:rtag   . helm-gtags-completing-grtags)
    (:symbol . helm-gtags-completing-gsyms)
    (:file   . helm-gtags-completing-files)))

(defun helm-gtags-construct-completion-command (type input)
  (let ((option (helm-gtags-construct-option type t)))
    (format "global %s %s" option input)))

(defun helm-gtags--execute-command (cmd)
  (if helm-gtags--remote-p
      (process-file-shell-command cmd nil t)
    (call-process-shell-command cmd nil t)))

(defun helm-gtags-complete (type string predicate code)
  (let ((candidates-list nil)
        (cmd (helm-gtags-construct-completion-command type string)))
    (with-temp-buffer
      (helm-gtags--execute-command cmd)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string 1) candidates-list)))
    (if (not code)
        (try-completion string candidates-list predicate)
      (all-completions string candidates-list predicate))))

(defsubst helm-gtags-token-at-point ()
  (thing-at-point 'symbol))

(defsubst helm-gtags-type-is-not-file-p (type)
  (not (eq type :file)))

(defun helm-gtags-input (type)
  (let ((tagname (helm-gtags-token-at-point))
        (prompt (assoc-default type helm-gtags-prompt-alist))
        (comp-func (assoc-default type helm-gtags-comp-func-alist)))
    (if (and tagname helm-gtags-use-input-at-cursor)
        tagname
      (when (and tagname (helm-gtags-type-is-not-file-p type))
        (setq prompt (format "%s(default \"%s\") " prompt tagname)))
      (let ((completion-ignore-case helm-gtags-ignore-case)
            (completing-read-function 'completing-read-default))
        (completing-read prompt comp-func nil nil nil
                         'helm-gtags-completing-history tagname)))))

(defun helm-gtags--path-libpath-p (tagroot)
  (let ((gtags-libpath (getenv "GTAGSLIBPATH")))
    (when gtags-libpath
      (cl-loop for path in (parse-colon-path gtags-libpath)
               thereis (string= tagroot path)))))

(defun helm-gtags-find-tag-directory ()
  (setq helm-gtags--real-tag-location nil
        helm-gtags--remote-p (file-remote-p default-directory))
  (with-temp-buffer
    (let ((status (helm-gtags--execute-command "global -p")))
      (unless (zerop status)
        (error "GTAGS not found"))
      (goto-char (point-min))
      (let ((tagroot (file-name-as-directory
                      (buffer-substring-no-properties (point) (line-end-position)))))
        (if (and (helm-gtags--path-libpath-p tagroot) helm-gtags-tag-location)
            (progn
              (setq helm-gtags--real-tag-location tagroot)
              helm-gtags-tag-location)
          (setq helm-gtags-tag-location tagroot))))))

(defun helm-gtags-base-directory ()
  (let ((dir (or helm-gtags-local-directory
                 (cl-case helm-gtags-path-style
                   (root (or helm-gtags--real-tag-location
                             helm-gtags-tag-location))
                   (otherwise default-directory))))
        (remote (file-remote-p default-directory)))
    (if (and remote (not (file-remote-p dir)))
        (concat remote dir)
      dir)))

(defsubst helm-gtags--new-context-info (index stack)
  (list :index index :stack stack))

(defun helm-gtags--put-context-stack (tag-location index stack)
  (puthash tag-location (helm-gtags--new-context-info index stack)
           helm-gtags-context-stack))

(defsubst helm-gtags--current-context ()
  (let ((file (buffer-file-name (current-buffer))))
    (list :file file :position (point) :readonly buffer-file-read-only)))

(defun helm-gtags-save-current-context ()
  (setq helm-gtags-saved-context (helm-gtags--current-context)))

(defun helm-gtags-open-file (file readonly)
  (if readonly
      (find-file-read-only file)
    (find-file file)))

(defun helm-gtags-open-file-other-window (file readonly)
  (setq helm-gtags-use-otherwin nil)
  (if readonly
      (find-file-read-only-other-window file)
    (find-file-other-window file)))

(defun helm-gtags--get-context-info ()
  (let* ((tag-location (helm-gtags-find-tag-directory))
         (context-info (gethash tag-location helm-gtags-context-stack))
         (context-stack (plist-get context-info :stack)))
    (if (null context-stack)
        (error "Context stack is empty(TAG at %s)" tag-location)
      context-info)))

(defun helm-gtags--get-or-create-context-info ()
  (or (gethash helm-gtags-tag-location helm-gtags-context-stack)
      (helm-gtags--new-context-info -1 nil)))

;;;###autoload
(defun helm-gtags-clear-all-cache ()
  (interactive)
  (clrhash helm-gtags-result-cache))

;;;###autoload
(defun helm-gtags-clear-cache ()
  (interactive)
  (helm-gtags-find-tag-directory)
  (let ((gtags-path (concat (or helm-gtags--real-tag-location
                                helm-gtags-tag-location)
                            "GTAGS"))
        (gpath-path (concat (or helm-gtags--real-tag-location
                                helm-gtags-tag-location)
                            "GPATH")))
    (remhash gtags-path helm-gtags-result-cache)
    (remhash gpath-path helm-gtags-result-cache)))

(defun helm-gtags--move-to-context (context)
  (let ((file (plist-get context :file))
        (curpoint (plist-get context :position))
        (readonly (plist-get context :readonly)))
    (helm-gtags-open-file file readonly)
    (goto-char curpoint)
    (recenter)))

;;;###autoload
(defun helm-gtags-next-history ()
  "Jump to next position on context stack"
  (interactive)
  (let* ((context-info (helm-gtags--get-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack))
         context)
    (when (<= current-index -1)
      (error "This context is latest in context stack"))
    (setf (nth current-index context-stack) (helm-gtags--current-context))
    (cl-decf current-index)
    (if (= current-index -1)
        (setq context helm-gtags--current-position
              helm-gtags--current-position nil)
      (setq context (nth current-index context-stack)))
    (helm-gtags--put-context-stack helm-gtags-tag-location
                                   current-index context-stack)
    (helm-gtags--move-to-context context)))

;;;###autoload
(defun helm-gtags-previous-history ()
  "Jump to previous position on context stack"
  (interactive)
  (let* ((context-info (helm-gtags--get-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack))
         (context-length (length context-stack)))
    (cl-incf current-index)
    (when (>= current-index context-length)
      (error "This context is last in context stack"))
    (if (= current-index 0)
        (setq helm-gtags--current-position (helm-gtags--current-context))
      (setf (nth (- current-index 1) context-stack) (helm-gtags--current-context)))
    (let ((prev-context (nth current-index context-stack)))
      (helm-gtags--move-to-context prev-context))
    (helm-gtags--put-context-stack helm-gtags-tag-location
                                   current-index context-stack)))

(defun helm-gtags-get-result-cache (file)
  (helm-gtags-find-tag-directory)
  (let* ((file-path (concat (or helm-gtags--real-tag-location
                                helm-gtags-tag-location)
                            file))
         (file-mtime (nth 5 (file-attributes file-path)))
         (hash-value (gethash file-path helm-gtags-result-cache))
         (cached-file-mtime (nth 0 hash-value)))
    (if (and cached-file-mtime (equal cached-file-mtime file-mtime))
        (nth 1 hash-value)
      nil)))

(defun helm-gtags-put-result-cache (file cache)
  (helm-gtags-find-tag-directory)
  (let* ((file-path (concat (or helm-gtags--real-tag-location
                                helm-gtags-tag-location)
                            file))
         (file-mtime (nth 5 (file-attributes file-path)))
         (hash-value (list file-mtime cache)))
    (puthash file-path hash-value helm-gtags-result-cache)))

(defun helm-gtags-pop-context ()
  (let* ((context-info (helm-gtags--get-context-info))
         (context-stack (plist-get context-info :stack))
         (context (pop context-stack)))
    (helm-gtags--put-context-stack helm-gtags-tag-location -1 context-stack)
    (helm-gtags--move-to-context context)))

(defun helm-gtags-exec-global-command (cmd)
  (helm-gtags-find-tag-directory)
  (helm-gtags-save-current-context)
  (let ((buf-coding buffer-file-coding-system))
    (with-current-buffer (helm-candidate-buffer 'global)
      (let ((default-directory (helm-gtags-base-directory))
            (input (car (last (split-string cmd))))
            (coding-system-for-read buf-coding)
            (coding-system-for-write buf-coding))
        (let ((status (helm-gtags--execute-command cmd)))
          (unless (zerop status)
            (error (format "%s: not found" input))))))))

(defvar helm-gtags-command-option-alist
  '((:tag    . "")
    (:pattern . "-g")
    (:rtag   . "-r")
    (:symbol . "-s")
    (:file   . "-Poa")))

(defun helm-gtags-construct-option (type &optional comp)
  (let ((type-opt (assoc-default type helm-gtags-command-option-alist))
        (result-opt (or (and (eq type :file) "") "--result=grep"))
        (abs-opt (or (and (eq helm-gtags-path-style 'absolute) "-a") ""))
        (case-opt (or (and helm-gtags-ignore-case "-i") ""))
        (comp-opt (or (and comp "-c") ""))
        (local-opt (or (and current-prefix-arg
                            (helm-gtags-type-is-not-file-p type) "-l") "")))
    (format "%s %s %s %s %s %s"
            result-opt comp-opt type-opt abs-opt case-opt local-opt)))

(defun helm-gtags-construct-command (type &optional in)
  (setq helm-gtags-local-directory nil)
  (let ((dir (helm-attr 'helm-gtags-base-directory (helm-get-current-source))))
    (when (and dir (helm-gtags-type-is-not-file-p type))
      (setq helm-gtags-local-directory dir)))
  (let ((input (or in (helm-gtags-input type)))
        (option (helm-gtags-construct-option type)))
    (setq helm-gtags--last-input input)
    (when (string= input "")
      (error "Input is empty!!"))
    (format "global %s %s" option input)))

(defun helm-gtags-tags-init (&optional input)
  (let ((cmd (helm-gtags-construct-command :tag input)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-pattern-init (&optional input)
  (let ((cmd (helm-gtags-construct-command :pattern input)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-rtags-init (&optional input)
  (let ((cmd (helm-gtags-construct-command :rtag input)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-gsyms-init ()
  (let ((cmd (helm-gtags-construct-command :symbol)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-files-init ()
  (let ((cmd (helm-gtags-construct-command :file)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-find-tag-from-here-init ()
  (helm-gtags-find-tag-directory)
  (helm-gtags-save-current-context)
  (let* ((token (helm-gtags-token-at-point))
         (filename (buffer-file-name))
         (cmd (format "global --result=grep --from-here=%d:%s %s"
                      (line-number-at-pos)
                      (shell-quote-argument filename)
                      token)))
    (setq helm-gtags--last-input token)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let* ((default-directory (helm-gtags-base-directory))
             (status (helm-gtags--execute-command cmd)))
        (cond ((= status 1)
               (error "%s%s" (buffer-string) filename))
              ((= status 3)
               (error "%s" (buffer-string)))
              ((/= status 0)
               (error "%s: not found" token)))))))

(defun helm-gtags-parse-file-init ()
  (let ((cmd (concat "global --result cscope -f "
                     (shell-quote-argument helm-gtags-parsed-file))))
    (with-current-buffer (helm-candidate-buffer 'global)
      (unless (zerop (helm-gtags--execute-command cmd))
        (error "Failed: %s" cmd)))))

(defun helm-gtags--push-context (context)
  (let* ((context-info (helm-gtags--get-or-create-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack)))
    (unless (= current-index -1)
      (setq context-stack (nthcdr (1+ current-index) context-stack)))
    (setq helm-gtags--current-position nil)
    (push context context-stack)
    (helm-gtags--put-context-stack helm-gtags-tag-location -1 context-stack)))

(defun helm-gtags-select-find-file-func ()
  (if helm-gtags-use-otherwin
      'helm-gtags-open-file-other-window
    'helm-gtags-open-file))

(defun helm-gtags-do-open-file (open-func file line)
  (funcall open-func file helm-gtags-read-only)
  (goto-char (point-min))
  (forward-line (1- line))
  (back-to-indentation)
  (recenter)
  (helm-gtags--push-context helm-gtags-saved-context)
  (when helm-gtags-pulse-at-cursor
    (pulse-momentary-highlight-one-line (point))))

(defun helm-gtags-parse-file-action (cand)
  (let ((line (when (string-match "\\s-+\\([1-9][0-9]+\\)\\s-+" cand)
                (string-to-number (match-string-no-properties 1 cand))))
        (open-func (helm-gtags-select-find-file-func)))
    (helm-gtags-do-open-file open-func helm-gtags-parsed-file line)))

(defun helm-gtags-action-openfile (elm)
  (let* ((elems (split-string elm ":"))
         (filename (cl-first elems))
         (line (string-to-number (cl-second elems)))
         (open-func (helm-gtags-select-find-file-func))
         (default-directory (helm-gtags-base-directory)))
    (helm-gtags-do-open-file open-func filename line)))

(defun helm-gtags-file-content-at-pos (file pos)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (let ((curfunc (which-function))
            (line (line-number-at-pos))
            (content (or (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         "")))
        (format "%s:%d%s:%s"
                file line
                (helm-aif curfunc (format "[%s]" it) "")
                content)))))

(defun helm-gtags-show-stack-init ()
  (cl-loop with context-stack = (plist-get (helm-gtags--get-context-info) :stack)
           with stack-length = (length context-stack)
           for context in (reverse context-stack)
           for file = (plist-get context :file)
           for pos  = (plist-get context :position)
           for index = (1- stack-length) then (1- index)
           for line = (helm-gtags-file-content-at-pos file pos)
           collect (cons (helm-gtags-files-candidate-transformer line) index)))

(defun helm-gtags-tags-persistent-action (cand)
  (let* ((elems (split-string cand ":"))
         (filename (cl-first elems))
         (line (string-to-number (cl-second elems)))
         (default-directory (helm-gtags-base-directory)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))

(defvar helm-source-gtags-tags
  `((name . "GNU GLOBAL")
    (init . helm-gtags-tags-init)
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (real-to-display . helm-gtags--candidate-transformer)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-pattern
  `((name . "GNU GLOBAL")
    (init . helm-gtags-pattern-init)
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (real-to-display . helm-gtags--candidate-transformer)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-rtags
  `((name . "GNU GLOBAL")
    (init . helm-gtags-rtags-init)
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (real-to-display . helm-gtags--candidate-transformer)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-gsyms
  `((name . "GNU GLOBAL")
    (init . helm-gtags-gsyms-init)
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (real-to-display . helm-gtags--candidate-transformer)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defun helm-gtags--highlight-candidate (candidate)
  (let ((regexp (concat "\\_<" helm-gtags--last-input "\\_>"))
        (limit (1- (length candidate)))
        (last-pos 0))
    (while (and (< last-pos limit)
                (string-match regexp candidate last-pos))
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'helm-gtags-match
                         candidate)
      (setq last-pos (1+ (match-end 0))))
    candidate))

(defun helm-gtags--candidate-transformer (candidate)
  (if (not helm-gtags-highlight-candidate)
      candidate
    (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):\\(.*\\)" candidate)
      (format "%s:%s:%s"
              (propertize (match-string 1 candidate) 'face 'helm-gtags-file)
              (propertize (match-string 2 candidate) 'face 'helm-gtags-lineno)
              (helm-gtags--highlight-candidate (match-string 3 candidate))))))

(defun helm-gtags-files-candidate-transformer (file)
  (let ((removed-regexp (format "^%s" helm-gtags-tag-location)))
    (replace-regexp-in-string removed-regexp "" file)))

(defun helm-gtags-parse-file-candidate-transformer (file)
  (let ((removed-file (replace-regexp-in-string "\\`\\S-+ " "" file)))
    (when (string-match "\\`\\(\\S-+\\) \\(\\S-+\\) \\(.+\\)\\'" removed-file)
      (format "%-25s %-5s %s"
              (match-string-no-properties 1 removed-file)
              (match-string-no-properties 2 removed-file)
              (match-string-no-properties 3 removed-file)))))

(defvar helm-source-gtags-files
  `((name . "GNU GLOBAL")
    (init . helm-gtags-files-init)
    (candidates-in-buffer)
    (real-to-display . helm-gtags-files-candidate-transformer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (type . file)))

(defvar helm-source-gtags-find-tag-from-here
  `((name . "GNU GLOBAL")
    (init . helm-gtags-find-tag-from-here-init)
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (real-to-display . helm-gtags--candidate-transformer)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-parse-file
  `((name . "Parsed File")
    (init . helm-gtags-parse-file-init)
    (candidates-in-buffer)
    (real-to-display . helm-gtags-parse-file-candidate-transformer)
    (action . helm-gtags-parse-file-action)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)))

(defun helm-gtags--show-stack-action (index)
  (let* ((context-info (helm-gtags--get-context-info))
         (context-stack (plist-get context-info :stack)))
    (helm-gtags--put-context-stack helm-gtags-tag-location
                                   index context-stack)
    (helm-gtags--move-to-context (nth index context-stack))))

(defvar helm-source-gtags-show-stack
  `((name . "Show Context Stack")
    (candidates . helm-gtags-show-stack-init)
    (volatile)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags--show-stack-action)))

;;;###autoload
(defun helm-gtags-select ()
  (interactive)
  (helm-gtags-common '(helm-source-gtags-select)))

;;;###autoload
(defun helm-gtags-select-path ()
  (interactive)
  (helm-gtags-common '(helm-source-gtags-select-path)))

(defun helm-source-gtags-select-tag (candidate)
  `((name . "GNU GLOBAL")
    (init . (lambda ()
              (helm-gtags-tags-init ,candidate)))
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defun helm-source-gtags-select-rtag (candidate)
  `((name . "GNU GLOBAL")
    (init . (lambda ()
              (helm-gtags-rtags-init ,candidate)))
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defun helm-source-gtags-select-tag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-gtags-common (list (helm-source-gtags-select-tag ,c))))))

(defun helm-source-gtags-select-rtag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-gtags-common (list (helm-source-gtags-select-rtag ,c))))))

(defun helm-source-gtags-select-cache-init-common (command tagfile)
  (let ((cache (helm-gtags-get-result-cache tagfile)))
    (if cache
        (insert cache)
      (helm-gtags--execute-command command)
      (let* ((cache (buffer-string))
             (cache-size (length cache)))
        (when (<= cache-size helm-gtags-cache-max-result-size)
          (helm-gtags-put-result-cache tagfile cache))))))

(defun helm-source-gtags-select-init ()
  (setq helm-gtags--remote-p (file-remote-p default-directory))
  (with-current-buffer (helm-candidate-buffer 'global)
    (if (not helm-gtags-cache-select-result)
        (helm-gtags--execute-command "global -c")
      (helm-source-gtags-select-cache-init-common "global -c" "GTAGS"))))

(defvar helm-source-gtags-select
  `((name . "GNU GLOBAL SELECT")
    (init . helm-source-gtags-select-init)
    (candidates-in-buffer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (action . (("Goto the location" . helm-source-gtags-select-tag-action)
               ("Goto the location(other buffer)" .
                (lambda (c)
                  (setq helm-gtags-use-otherwin t)
                  (helm-source-gtags-select-tag-action c)))
               ("Move to the referenced point" .
                helm-source-gtags-select-rtag-action)))))

(defun helm-source-gtags-select-path-init ()
  (setq helm-gtags--remote-p (file-remote-p default-directory))
  (with-current-buffer (helm-candidate-buffer 'global)
    (if (not helm-gtags-cache-select-result)
        (helm-gtags--execute-command "global -Poa")
      (helm-source-gtags-select-cache-init-common "global -Poa" "GPATH"))))

(defvar helm-source-gtags-select-path
  `((name . "GNU GLOBAL PATH")
    (init . helm-source-gtags-select-path-init)
    (candidates-in-buffer)
    (real-to-display . helm-gtags-files-candidate-transformer)
    (candidate-number-limit . ,helm-gtags-maximum-candidates)
    (type . file)))

(defun helm-gtags-searched-directory ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 (let ((dir (read-directory-name "Input Directory: ")))
         (setq helm-gtags-local-directory (file-name-as-directory dir))))
    (16 (file-name-directory (buffer-file-name)))))

(defsubst helm-gtags--using-other-window-p ()
  (< (prefix-numeric-value current-prefix-arg) 0))

(defun helm-gtags-common (srcs)
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (dir (helm-gtags-searched-directory))
        (src (car srcs)))
    (when (symbolp src)
      (setq src (symbol-value src)))
    (when (helm-gtags--using-other-window-p)
      (setq helm-gtags-use-otherwin t))
    (helm-attrset 'helm-gtags-base-directory dir src)
    (helm-attrset 'name
                  (format "Searched at %s" (or dir default-directory))
                  src)
    (helm :sources srcs :buffer helm-gtags-buffer)))

;;;###autoload
(defun helm-gtags-find-tag ()
  "Jump to definition"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-tags)))

;;;###autoload
(defun helm-gtags-find-rtag ()
  "Jump to referenced point"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-rtags)))

;;;###autoload
(defun helm-gtags-find-symbol ()
  "Jump to the symbol location"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-gsyms)))

;;;###autoload
(defun helm-gtags-find-pattern ()
  "Jump to pattern"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-pattern)))

;;;###autoload
(defun helm-gtags-find-files ()
  "Find file with gnu global"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-files)))

;;;###autoload
(defun helm-gtags-find-tag-from-here ()
  "Find from here with gnu global"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-find-tag-from-here)))

(defun helm-gtags-set-parsed-file ()
  (let* ((this-file (file-name-nondirectory (buffer-file-name)))
         (file (if current-prefix-arg
                   (read-file-name "Parsed File: " nil this-file)
                 this-file)))
    (setq helm-gtags-parsed-file (expand-file-name file))))

;;;###autoload
(defun helm-gtags-parse-file ()
  "Find file with gnu global"
  (interactive)
  (helm-gtags-find-tag-directory)
  (helm-gtags-save-current-context)
  (when (helm-gtags--using-other-window-p)
    (setq helm-gtags-use-otherwin t))
  (helm-gtags-set-parsed-file)
  (helm-attrset 'name
                (format "Parsed File: %s"
                        (file-relative-name helm-gtags-parsed-file
                                            helm-gtags-tag-location))
                helm-source-gtags-parse-file)
  (helm :sources '(helm-source-gtags-parse-file) :buffer helm-gtags-buffer))

;;;###autoload
(defun helm-gtags-pop-stack ()
  "Jump to previous point on the stack"
  (interactive)
  (helm-gtags-pop-context))

;;;###autoload
(defun helm-gtags-show-stack ()
  "Show context stack"
  (interactive)
  (helm-other-buffer 'helm-source-gtags-show-stack
                     (get-buffer-create helm-gtags-buffer)))

;;;###autoload
(defun helm-gtags-clear-stack ()
  "Clear jumped point stack"
  (interactive)
  (let ((tag-location (helm-gtags-find-tag-directory)))
    (puthash tag-location nil helm-gtags-context-stack)))

;;;###autoload
(defun helm-gtags-clear-all-stacks ()
  "Clear all jumped point stacks"
  (interactive)
  (setq helm-gtags-context-stack (make-hash-table :test 'equal)))

(defun helm-gtags--update-tags-sentinel (process _state)
  (when (eq (process-status process) 'exit)
    (if (zerop (process-exit-status process))
        (message "Update TAGS successfully")
      (message "Failed to update TAGS"))
    (kill-buffer helm-gtags--update-tags-buffer)))

(defun helm-gtags--read-tag-directory ()
  (let ((dir (read-directory-name "Directory tag generated: " nil nil t)))
    ;; On Windows, "gtags d:/tmp" work, but "gtags d:/tmp/" doesn't
    (directory-file-name (expand-file-name dir))))

(defsubst helm-gtags--how-to-update-tags ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 'entire-update)
    (16 'generate-other-directory)
    (t 'single-update)))

(defun helm-gtags--update-tags-command (how-to)
  (cl-case how-to
    (entire-update "global -u")
    (generate-other-directory (concat "gtags " (helm-gtags--read-tag-directory)))
    (single-update (concat "global -u --single-update "
                           (shell-quote-argument
                            (expand-file-name (buffer-file-name)))))))

(defsubst helm-gtags--update-tags-process-live-p ()
  (get-buffer helm-gtags--update-tags-buffer))

(defun helm-gtags--check-from-last-update (current-time)
  (let ((delta (- current-time helm-gtags--last-update-time)))
    (> delta helm-gtags-update-interval-second)))

(defun helm-gtags--update-tags-p (how-to interactive-p current-time)
  (unless (helm-gtags--update-tags-process-live-p)
    (or interactive-p
        (and (eq how-to 'single-update)
             (buffer-file-name)
             (or (not helm-gtags-update-interval-second)
                 (helm-gtags--check-from-last-update current-time))))))

(defsubst helm-gtags--start-update-tags-process (cmd)
  (start-process-shell-command "helm-gtags-update-tag"
                               helm-gtags--update-tags-buffer
                               cmd))

;;;###autoload
(defun helm-gtags-update-tags ()
  "Update TAG file. Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'"
  (interactive)
  (let ((how-to (helm-gtags--how-to-update-tags))
        (interactive-p (called-interactively-p 'interactive))
        (current-time (float-time (current-time))))
    (when (helm-gtags--update-tags-p how-to interactive-p current-time)
      (let* ((cmd (helm-gtags--update-tags-command how-to))
             (proc (helm-gtags--start-update-tags-process cmd)))
        (if (not proc)
            (progn
              (message "Failed: %s" cmd)
              (kill-buffer helm-gtags--update-tags-buffer))
          (set-process-query-on-exit-flag proc nil)
          (set-process-sentinel proc 'helm-gtags--update-tags-sentinel)
          (setq helm-gtags--last-update-time current-time))))))

(defvar helm-gtags-mode-name " Helm Gtags")
(defvar helm-gtags-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-gtags-mode ()
  "Enable for helm-gtags"
  :group      'helm-gtags
  :init-value nil
  :global     nil
  :keymap     helm-gtags-mode-map
  :lighter    helm-gtags-mode-name
  (if helm-gtags-mode
      (progn
        (run-hooks 'helm-gtags-mode-hook)
        (when helm-gtags-auto-update
          (add-hook 'after-save-hook 'helm-gtags-update-tags nil t)))
    (progn
      (when helm-gtags-auto-update
        (remove-hook 'after-save-hook 'helm-gtags-update-tags t)))))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here

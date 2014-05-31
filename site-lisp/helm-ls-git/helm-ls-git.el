;;; helm-ls-git.el --- list git files. -*- lexical-binding: t -*-

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

;;; Code

(require 'cl-lib)
(require 'vc)
(require 'helm-locate)
(require 'helm-files)

(defvaralias 'helm-c-source-ls-git 'helm-source-ls-git)
(make-obsolete-variable 'helm-c-source-ls-git 'helm-source-ls-git "1.5.1")
(defvaralias 'helm-c-source-ls-git-status 'helm-source-ls-git-status)
(make-obsolete-variable 'helm-c-source-ls-git-status 'helm-source-ls-git-status "1.5.1")


(defgroup helm-ls-git nil
  "Helm completion for git repos."
  :group 'helm)

(defcustom helm-ls-git-show-abs-or-relative 'absolute
  "Show full path or relative path to repo when using `helm-ff-toggle-basename'.
Valid values are symbol 'abs (default) or 'relative."
  :group 'helm-ls-git
  :type  '(radio :tag "Show full path or relative path to Git repo when toggling"
           (const :tag "Show full path" absolute)
           (const :tag "Show relative path" relative)))

(defcustom helm-ls-git-status-command 'vc-dir
  "Favorite git-status command for emacs."
  :group 'helm-ls-git
  :type 'symbol)

(defface helm-ls-git-modified-not-staged-face
    '((t :foreground "yellow"))
  "Files which are modified but not yet staged."
  :group 'helm-ls-git)

(defface helm-ls-git-modified-and-staged-face
    '((t :foreground "Gold"))
  "Files which are modified and already staged."
  :group 'helm-ls-git)

(defface helm-ls-git-renamed-modified-face
    '((t :foreground "Gold"))
  "Files which are renamed or renamed and modified."
  :group 'helm-ls-git)

(defface helm-ls-git-untracked-face
    '((t :foreground "red"))
  "Files which are not yet tracked by git."
  :group 'helm-ls-git)

(defface helm-ls-git-added-copied-face
    '((t :foreground "green"))
  "Files which are newly added or copied."
  :group 'helm-ls-git)

(defface helm-ls-git-added-modified-face
    '((t :foreground "blue"))
  "Files which are newly added and have unstaged modifications."
  :group 'helm-ls-git)

(defface helm-ls-git-deleted-not-staged-face
    '((t :foreground "Darkgoldenrod3"))
  "Files which are deleted but not staged."
  :group 'helm-ls-git)

(defface helm-ls-git-deleted-and-staged-face
    '((t :foreground "DimGray"))
  "Files which are deleted and staged."
  :group 'helm-ls-git)

(defface helm-ls-git-conflict-face
    '((t :foreground "MediumVioletRed"))
  "Files which contain rebase/merge conflicts."
  :group 'helm-ls-git)

;; Append visited files from `helm-source-ls-git' to `file-name-history'.
(add-to-list 'helm-file-completion-sources "Git files")


(defvar helm-ls-git-log-file nil) ; Set it for debugging.


(defun helm-ls-git-list-files ()
  (when (and helm-ls-git-log-file
             (file-exists-p helm-ls-git-log-file))
    (delete-file helm-ls-git-log-file))
  ;; `helm-resume' will use the value of `helm-default-directory'
  ;; as value for `default-directory'.
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
          (with-output-to-string
              (with-current-buffer standard-output
                (apply #'process-file
                       "git"
                       nil (list t helm-ls-git-log-file) nil
                       (list "ls-files" "--full-name" "--")))))))

(cl-defun helm-ls-git-root-dir (&optional (directory default-directory))
  (let ((root (locate-dominating-file directory ".git")))
    (and root (file-name-as-directory root))))

(defun helm-ls-git-not-inside-git-repo ()
  (not (helm-ls-git-root-dir)))

(defun helm-ls-git-transformer (candidates)
  (cl-loop with root = (helm-ls-git-root-dir helm-default-directory)
        for i in candidates
        for abs = (expand-file-name i root)
        for disp = (if (and helm-ff-transformer-show-only-basename
                            (not (string-match "[.]\\{1,2\\}$" i)))
                       (helm-basename i)
                     (cl-case helm-ls-git-show-abs-or-relative
                       (absolute abs)
                       (relative i)))
        collect
        (cons (propertize disp 'face 'helm-ff-file) abs)))

(defun helm-ls-git-sort-fn (candidates)
  "Transformer for sorting candidates."
  (helm-ff-sort-candidates candidates nil))

(defun helm-ls-git-init ()
  (let ((data (helm-ls-git-list-files)))
    (when (string= data "")
      (setq data
            (if helm-ls-git-log-file
                (with-current-buffer
                    (find-file-noselect helm-ls-git-log-file)
                  (prog1
                      (buffer-substring-no-properties
                       (point-min) (point-max))
                    (kill-buffer)))
              data)))
    (helm-init-candidates-in-buffer 'global data)))

(defun helm-ls-git-header-name (name)
  (let ((refs   (shell-command-to-string "git rev-parse --branches"))
        (branch (shell-command-to-string
                 "git rev-parse --abbrev-ref HEAD")))
    (format "%s (%s)"
            name
            (replace-regexp-in-string
             "\n" ""
             ;; Check REFS to avoid message error in header
             ;; when repo is just initialized and there is
             ;; no branches yet.
             (if (or (null refs) (string= refs "")) "--" branch)))))

(defvar helm-source-ls-git
  `((name . "Git files")
    (header-name . helm-ls-git-header-name)
    (init . helm-ls-git-init)
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (match-part . (lambda (candidate)
                    (if helm-ff-transformer-show-only-basename
                        (helm-basename candidate)
                      candidate)))
    (candidate-transformer . (helm-ls-git-transformer
                              helm-ls-git-sort-fn))
    (action-transformer helm-transform-file-load-el)
    (action . ,(cdr (helm-get-actions-from-type helm-source-locate)))))


(defun helm-ls-git-grep (candidate)
  (let* ((helm-grep-default-command "git grep -n%cH --full-name -e %p %f")
         helm-grep-default-recurse-command
         (exts (helm-grep-guess-extensions (helm-marked-candidates)))
         (globs (format "'%s'" (mapconcat 'identity exts " ")))
         (files (cond ((equal helm-current-prefix-arg '(4))
                       (list "--" (read-string "OnlyExt(*.[ext]): " globs)))
                      ((equal helm-current-prefix-arg '(16))
                       '("--"))
                      (t (helm-marked-candidates))))
         ;; Expand filename of each candidate with the git root dir.
         ;; The filename will be in the help-echo prop.
         (helm-grep-default-directory-fn 'helm-ls-git-root-dir)
         ;; `helm-grep-init' initialize `default-directory' to this value,
         ;; So set this value (i.e `helm-ff-default-directory') to
         ;; something else.
         (helm-ff-default-directory (file-name-directory candidate)))
    (helm-do-grep-1 files)))

(helm-add-action-to-source
 "Git grep files (`C-u' only, `C-u C-u' all)"
 'helm-ls-git-grep helm-source-ls-git 3)

(helm-add-action-to-source
 "Search in Git log (C-u show patch)"
 'helm-ls-git-search-log
 helm-source-ls-git 4)


(defun helm-ls-git-search-log (_candidate)
  (let* ((query (read-string "Search log: "))
         (coms (if helm-current-prefix-arg
                   (list "log" "-p" "--grep" query)
                 (list "log" "--grep" query))))
    (with-current-buffer (get-buffer-create "*helm ls log*")
      (set (make-local-variable 'buffer-read-only) nil)
      (erase-buffer)
      (apply #'process-file "git" nil (list t nil) nil coms)))
  (pop-to-buffer "*helm ls log*")
  (goto-char (point-min))
  (diff-mode)
  (set (make-local-variable 'buffer-read-only) t))


(defun helm-ls-git-status ()
  (when (and helm-ls-git-log-file
             (file-exists-p helm-ls-git-log-file))
    (delete-file helm-ls-git-log-file))
  (with-output-to-string
      (with-current-buffer standard-output
        (apply #'process-file
               "git"
               nil (list t helm-ls-git-log-file) nil
               (list "status" "--porcelain")))))

(defun helm-ls-git-status-transformer (candidates _source)
  (cl-loop with root = (helm-ls-git-root-dir helm-default-directory)
        for i in candidates
        collect
        (cond ((string-match "^\\( M \\)\\(.*\\)" i) ; modified.
               (cons (propertize i 'face 'helm-ls-git-modified-not-staged-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(M+ *\\)\\(.*\\)" i) ; modified and staged.
               (cons (propertize i 'face 'helm-ls-git-modified-and-staged-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\([?]\\{2\\} \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-untracked-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\([AC] +\\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-added-copied-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\( [D] \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-deleted-not-staged-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(RM?\\).* -> \\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-renamed-modified-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\([D] \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-deleted-and-staged-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(UU \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-conflict-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(AM \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-added-modified-face)
                     (expand-file-name (match-string 2 i) root)))
              (t i))))

(defvar helm-source-ls-git-status
  `((name . "Git status")
    (header-name . helm-ls-git-header-name)
    (init . (lambda ()
              (helm-init-candidates-in-buffer
                  'global
                (helm-ls-git-status))))
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (filtered-candidate-transformer . helm-ls-git-status-transformer)
    (persistent-action . helm-ls-git-diff)
    (persistent-help . "Diff")
    (action-transformer . helm-ls-git-status-action-transformer)
    (action . (("Find file" . helm-find-many-files)
               ("Git status" . (lambda (_candidate)
                                 (with-current-buffer helm-buffer
                                   (funcall helm-ls-git-status-command
                                            helm-default-directory))))))))

(defun helm-ls-git-status-action-transformer (actions _candidate)
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^[?]\\{2\\}" disp)
           (append actions
                   (list '("Add file(s)"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (vc-call-backend 'Git 'register marked))))
                         '("Delete file(s)" . helm-delete-marked-files)
                         '("Copy bnames to .gitignore"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (with-current-buffer (find-file-noselect
                                                       (expand-file-name
                                                        ".gitignore"
                                                        (helm-ls-git-root-dir)))
                                   (goto-char (point-max))
                                   (cl-loop with last-bname 
                                         for f in marked
                                         for bname = (helm-basename f)
                                         unless (string= bname last-bname)
                                         do (insert (concat bname "\n"))
                                         do (setq last-bname bname))
                                   (save-buffer))))))))
          ((string-match "^ ?M+ *" disp)
           (append actions (list '("Diff file" . helm-ls-git-diff)
                                 '("Commit file(s)"
                                   . (lambda (candidate)
                                       (let* ((marked (helm-marked-candidates))
                                              (default-directory
                                               (file-name-directory (car marked))))
                                         (vc-checkin marked 'Git))))
                                 '("Revert file(s)" . (lambda (candidate)
                                                     (let ((marked (helm-marked-candidates)))
                                                       (cl-loop for f in marked do
                                                             (progn
                                                               (vc-git-revert f)
                                                               (helm-aif (get-file-buffer f)
                                                                   (with-current-buffer it
                                                                     (revert-buffer t t)))))))))))
          ((string-match "^ D " disp)
           (append actions (list '("Git delete" . vc-git-delete-file))))
          (t actions))))

(defun helm-ls-git-diff (candidate)
  (let (helm-persistent-action-use-special-display)
    (with-current-buffer (find-file-noselect candidate)
      (when (buffer-live-p (get-buffer "*vc-diff*"))
        (kill-buffer "*vc-diff*")
        (balance-windows))
      (call-interactively #'vc-diff))))


;;;###autoload
(defun helm-ls-git-ls ()
  (interactive)
  (helm :sources '(helm-source-ls-git-status
                   helm-source-ls-git)
        ;; When `helm-ls-git-ls' is called from lisp
        ;; `default-directory' is normally let-bounded,
        ;; to some other value;
        ;; we now set this new let-bounded value local
        ;; to `helm-default-directory'.
        :default-directory default-directory
        :buffer "*helm lsgit*"))


;;; Helm-find-files integration.
;;
(defun helm-ff-ls-git-find-files (_candidate)
  (let ((default-directory helm-ff-default-directory))
    (helm-run-after-quit
     #'(lambda (d)
         (let ((default-directory d))
           (helm-ls-git-ls)))
     default-directory)))

(defun helm-ls-git-ff-dir-git-p (file)
  (when (or (file-exists-p file)
            (file-directory-p file))
    (stringp (condition-case nil
                 (helm-ls-git-root-dir
                  helm-ff-default-directory)
               (error nil)))))

(when (require 'helm-files)
  (helm-add-action-to-source-if
   "Git ls-files"
   'helm-ff-ls-git-find-files
   helm-source-find-files
   'helm-ls-git-ff-dir-git-p
   4))

(provide 'helm-ls-git)

;;; helm-ls-git.el ends here

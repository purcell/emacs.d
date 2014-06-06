;;; helm-locate.el --- helm interface for locate. -*- lexical-binding: t -*-

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

;; NOTE for WINDOZE users:
;; You have to install Everything with his command line interface here:
;; http://www.voidtools.com/download.php

;;; Code:

(require 'cl-lib)
(require 'helm)


(defgroup helm-locate nil
  "Locate related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-locate-db-file-regexp "m?locate\.db$"
  "Default regexp to match locate database.
If nil Search in all files."
  :type  'string
  :group 'helm-locate)

(defcustom helm-ff-locate-db-filename "locate.db"
  "The basename of the locatedb file you use locally in your directories.
When this is set and `helm' find such a file in the directory from
where you launch locate, it will use this file and will not prompt you
for a db file.
Note that this happen only when locate is launched with a prefix arg."
  :group 'helm-locate
  :type 'string)

(defcustom helm-locate-command nil
  "A list of arguments for locate program.
Normally you should not have to modify this yourself.

If nil it will be calculated when `helm-locate' startup
with these default values for different systems:

Gnu/linux: \"locate %s -e -A %s\"
berkeley-unix: \"locate %s %s\"
windows-nt: \"es %s %s\"
Others: \"locate %s %s\"

This string will be passed to format so it should end with `%s'.
The first format spec is used for the \"-i\" value of locate/es,
So don't set it directly but use `helm-locate-case-fold-search'
for this.
The \"-r\" option must be the last option, however if not specified you will
be able to specify it during helm invocation by prefixing the pattern
you enter with \"-r\"."
  :type 'string
  :group 'helm-locate)

(defcustom helm-locate-create-db-command
  "updatedb -l 0 -o %s -U %s"
  "Command used to create a locale locate db file."
  :type 'string
  :group 'helm-locate)

(defcustom helm-locate-case-fold-search helm-case-fold-search
  "It have the same meaning as `helm-case-fold-search'.
The -i option of locate will be used depending of value of
`helm-pattern' when this is set to 'smart.
When nil \"-i\" will not be used at all.
and when non--nil it will always be used.
NOTE: the -i option of the \"es\" command used on windows does
the opposite of \"locate\" command."
  :group 'helm-locate
  :type 'symbol)


(defvar helm-generic-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-]")     'helm-ff-run-toggle-basename)
    (define-key map (kbd "C-s")     'helm-ff-run-grep)
    (define-key map (kbd "M-g s")   'helm-ff-run-grep)
    (define-key map (kbd "M-g z")   'helm-ff-run-zgrep)
    (define-key map (kbd "M-g p")   'helm-ff-run-pdfgrep)
    (define-key map (kbd "M-D")     'helm-ff-run-delete-file)
    (define-key map (kbd "C-=")     'helm-ff-run-ediff-file)
    (define-key map (kbd "C-c =")   'helm-ff-run-ediff-merge-file)
    (define-key map (kbd "C-c o")   'helm-ff-run-switch-other-window)
    (define-key map (kbd "M-i")     'helm-ff-properties-persistent)
    (define-key map (kbd "C-c C-x") 'helm-ff-run-open-file-externally)
    (define-key map (kbd "C-c X")   'helm-ff-run-open-file-with-default-tool)
    (define-key map (kbd "M-.")     'helm-ff-run-etags)
    (define-key map (kbd "C-w")     'helm-yank-text-at-point)
    (define-key map (kbd "C-c @")   'helm-ff-run-insert-org-link)
    (define-key map (kbd "C-c ?")   'helm-generic-file-help)
    map)
  "Generic Keymap for files.")


(defface helm-locate-finish
    '((t (:foreground "Green")))
  "Face used in mode line when locate process is finish."
  :group 'helm-locate)


(defun helm-ff-find-locatedb (&optional from-ff)
  "Try to find if a local locatedb file is available.
The search is done in `helm-ff-default-directory' or
fall back to `default-directory' if FROM-FF is nil."
  (when helm-ff-locate-db-filename
    (cond ((and helm-ff-default-directory
                from-ff
                (file-exists-p (expand-file-name
                                helm-ff-locate-db-filename
                                helm-ff-default-directory))
                (expand-file-name
                 helm-ff-locate-db-filename
                 helm-ff-default-directory)))
          ((and (not from-ff)
                (file-exists-p (expand-file-name
                                helm-ff-locate-db-filename
                                default-directory))
                (expand-file-name
                 helm-ff-locate-db-filename
                 default-directory))))))

(defun helm-locate-1 (&optional localdb init from-ff default)
  "Generic function to run Locate.
Prefix arg LOCALDB when (4) search and use a local locate db file when it
exists or create it, when (16) force update of existing db file
even if exists.
It have no effect when locate command is 'es'.
INIT is a string to use as initial input in prompt.
See `helm-locate-with-db' and `helm-locate'."
  (require 'helm-mode)
  (let ((pfn #'(lambda (candidate)
                 (if (file-directory-p candidate)
                     (message "Error: The locate Db should be a file")
                   (if (= (shell-command
                           (format helm-locate-create-db-command
                                   candidate
                                   helm-ff-default-directory))
                          0)
                       (message "New locatedb file `%s' created" candidate)
                     (error "Failed to create locatedb file `%s'" candidate)))))
        (locdb (and localdb
                    (not (string-match "^es" helm-locate-command))
                    (or (and (equal '(4) localdb)
                             (helm-ff-find-locatedb from-ff))
                        (helm-read-file-name
                         "Create Locate Db file: "
                         :initial-input (expand-file-name "locate.db"
                                                          (or helm-ff-default-directory
                                                              default-directory))
                         :preselect helm-locate-db-file-regexp
                         :test #'(lambda (x)
                                   (if helm-locate-db-file-regexp
                                       ;; Select only locate db files and directories
                                       ;; to allow navigation.
                                       (or (string-match
                                            helm-locate-db-file-regexp x)
                                           (file-directory-p x))
                                     x)))))))
    (when (and locdb (or (equal localdb '(16))
                         (not (file-exists-p locdb))))
      (funcall pfn locdb))
    (helm-locate-with-db (and localdb locdb) init default)))

(defun helm-locate-set-command ()
  "Setup `helm-locate-command' if not already defined."
  (unless helm-locate-command
    (setq helm-locate-command
          (cl-case system-type
            (gnu/linux "locate %s -e -r %s")
            (berkeley-unix "locate %s %s")
            (windows-nt "es %s %s")
            (t "locate %s %s")))))

(defvar helm-file-name-history nil)
(defun helm-locate-with-db (&optional db initial-input default)
  "Run locate -d DB.
If DB is not given or nil use locate without -d option.
Argument DB can be given as a string or list of db files.
Argument INITIAL-INPUT is a string to use as initial-input.
See also `helm-locate'."
  (when (and db (stringp db)) (setq db (list db)))
  (helm-locate-set-command)
  (let ((helm-ff-transformer-show-only-basename nil)
        (helm-locate-command
         (if db
             (replace-regexp-in-string
              "locate"
              (format "locate -d %s"
                      (mapconcat 'identity
                                 ;; Remove eventually
                                 ;; marked directories by error.
                                 (cl-loop for i in db
                                       unless (file-directory-p i)
                                       collect i) ":"))
              helm-locate-command)
           helm-locate-command)))
    (setq helm-file-name-history (mapcar 'helm-basename file-name-history))
    (helm :sources 'helm-source-locate
          :buffer "*helm locate*"
          :input initial-input
          :default default
          :history 'helm-file-name-history)))

(defun helm-locate-init ()
  "Initialize async locate process for `helm-source-locate'."
  (let* ((locate-is-es (string-match "\\`es" helm-locate-command))
         (real-locate (string-match "\\`locate" helm-locate-command))
         (case-sensitive-flag (if locate-is-es "-i" ""))
         (ignore-case-flag (if (or locate-is-es
                                   (not real-locate)) "" "-i"))
         process-connection-type
         (args (split-string helm-pattern " ")))
    (prog1
        (start-process-shell-command
         "locate-process" helm-buffer
         (format helm-locate-command
                 (cl-case helm-locate-case-fold-search
                   (smart (let ((case-fold-search nil))
                            (if (string-match "[A-Z]" helm-pattern)
                                case-sensitive-flag
                              ignore-case-flag)))
                   (t (if helm-locate-case-fold-search
                          ignore-case-flag
                        case-sensitive-flag)))
                 (concat
                  ;; The pattern itself.
                  (shell-quote-argument (car args)) " "
                  ;; Possible locate args added
                  ;; after pattern, don't quote them.
                  (mapconcat 'identity (cdr args) " "))))
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (_process event)
           (if (string= event "finished\n")
               (with-helm-window
                 (setq mode-line-format
                       '(" " mode-line-buffer-identification " "
                         (line-number-mode "%l") " "
                         (:eval (propertize
                                 (format "[Locate Process Finish- (%s results)]"
                                         (max (1- (count-lines
                                                   (point-min) (point-max)))
                                              0))
                                 'face 'helm-locate-finish))))
                 (force-mode-line-update))
             (helm-log "Error: Locate %s"
                       (replace-regexp-in-string "\n" "" event))))))))

(defvar helm-source-locate
  `((name . "Locate")
    (init . helm-locate-set-command)
    (candidates-process . helm-locate-init)
    (type . file)
    (requires-pattern . 3)
    (history . ,'helm-file-name-history)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (candidate-number-limit . 9999)
    (no-matchplugin)
    (mode-line . helm-generic-file-mode-line-string))
  "Find files matching the current input pattern with locate.")

;;;###autoload
(defun helm-locate-read-file-name (prompt)
  (let* (helm-ff-transformer-show-only-basename
         (src `((name . "Locate read fname")
                (init . helm-locate-set-command)
                (candidates-process . helm-locate-init)
                (action . identity)
                (requires-pattern . 3)
                (history . ,'helm-file-name-history)
                (candidate-transformer . (helm-skip-boring-files
                                          helm-highlight-files))
                (candidate-number-limit . 9999)
                (no-matchplugin))))
    (or (helm :sources src
              :prompt prompt
              :buffer "*helm locate read fname*"
              :resume 'noresume)
        (keyboard-quit))))

;;;###autoload
(defun helm-locate (arg)
  "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it
if it doesn't exists.
Many databases can be used: navigate and mark them.
See also `helm-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'."
  (interactive "P")
  (setq helm-ff-default-directory default-directory)
  (helm-locate-1 arg))

(provide 'helm-locate)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-locate.el ends here

;;; helm-tags.el --- Helm for Etags. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-help)
(require 'helm-utils)
(require 'helm-grep)


(defgroup helm-tags nil
  "Tags related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-etags-tag-file-name "TAGS"
  "Etags tag file name."
  :type  'string
  :group 'helm-tags)

(defcustom helm-etags-tag-file-search-limit 10
  "The limit level of directory to search tag file.
Don't search tag file deeply if outside this value."
  :type  'number
  :group 'helm-tags)

(defcustom helm-etags-match-part-only 'tag
  "Allow choosing the tag part of CANDIDATE in `helm-source-etags-select'.
A tag looks like this:
    filename: \(defun foo
You can choose matching against the tag part (i.e \"(defun foo\"),
or against the whole candidate (i.e \"(filename:5:(defun foo\")."
  :type '(choice
          (const :tag "Match only tag" tag)
          (const :tag "Match all file+tag" all))
  :group 'helm-tags)

(defcustom helm-etags-execute-action-at-once-if-one t
  "Whether to jump straight to the selected tag if there's only
one match."
  :type 'boolean
  :group 'helm-tags)


(defgroup helm-tags-faces nil
  "Customize the appearance of helm-tags faces."
  :prefix "helm-"
  :group 'helm-tags
  :group 'helm-faces)

(defface helm-etags-file
    '((t (:foreground "Lightgoldenrod4"
          :underline t)))
  "Face used to highlight etags filenames."
  :group 'helm-tags-faces)


;;; Etags
;;
;;
(defun helm-etags-run-switch-other-window ()
  "Run switch to other window action from `helm-source-etags-select'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (c)
       (helm-etags-action-goto 'find-file-other-window c)))))
(put 'helm-etags-run-switch-other-window 'helm-only t)

(defun helm-etags-run-switch-other-frame ()
  "Run switch to other frame action from `helm-source-etags-select'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (c)
       (helm-etags-action-goto 'find-file-other-frame c)))))
(put 'helm-etags-run-switch-other-frame 'helm-only t)

(defvar helm-etags-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-c o")    'helm-etags-run-switch-other-window)
    (define-key map (kbd "C-c C-o")  'helm-etags-run-switch-other-frame)
    map)
  "Keymap used in Etags.")

(defvar helm-etags-mtime-alist nil
  "Store the last modification time of etags files here.")
(defvar helm-etags-cache (make-hash-table :test 'equal)
  "Cache content of etags files used here for faster access.")

(defun helm-etags-get-tag-file (&optional directory)
  "Return the path of etags file if found.
Lookes recursively in parents directorys for a
`helm-etags-tag-file-name' file."
  ;; Get tag file from `default-directory' or upper directory.
  (let ((current-dir (helm-etags-find-tag-file-directory
                      (or directory default-directory))))
    ;; Return nil if not find tag file.
    (when current-dir
      (expand-file-name helm-etags-tag-file-name current-dir))))

(defun helm-etags-all-tag-files ()
  "Return files from the following sources;
  1) An automatically located file in the parent directories, by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command."
  (helm-fast-remove-dups
   (delq nil
         (append (list (helm-etags-get-tag-file)
                       tags-file-name)
                 tags-table-list))
   :test 'equal))

(defun helm-etags-find-tag-file-directory (current-dir)
  "Try to find the directory containing tag file.
If not found in CURRENT-DIR search in upper directory."
  (let ((file-exists? (lambda (dir)
                          (let ((tag-path (expand-file-name
                                           helm-etags-tag-file-name dir)))
                            (and (stringp tag-path)
                                 (file-regular-p tag-path)
                                 (file-readable-p tag-path))))))
    (cl-loop with count = 0
          until (funcall file-exists? current-dir)
          ;; Return nil if outside the value of
          ;; `helm-etags-tag-file-search-limit'.
          if (= count helm-etags-tag-file-search-limit)
          do (cl-return nil)
          ;; Or search upper directories.
          else
          do (cl-incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
          finally return current-dir)))

(defun helm-etags-get-header-name (_x)
  "Create header name for this helm etags session."
  (concat "Etags in "
          (with-helm-current-buffer
            (helm-etags-get-tag-file))))

(defun helm-etags-create-buffer (file)
  "Create the `helm-buffer' based on contents of etags tag FILE."
  (let* (max
         (split (with-temp-buffer
                  (insert-file-contents file)
                  (prog1
                      (split-string (buffer-string) "\n" 'omit-nulls)
                    (setq max (line-number-at-pos (point-max))))))
         (progress-reporter (make-progress-reporter "Loading tag file..." 0 max)))
    (cl-loop
          with fname
          with cand
          for i in split for count from 0
          for elm = (unless (string-match "^\x0c" i)    ;; "^L"
                      (helm-aif (string-match "\177" i) ;; "^?"
                          (substring i 0 it)
                        i))
          for linum = (when (string-match "[0-9]+,?[0-9]*$" i)
                        (car (split-string (match-string 0 i) ",")))
          do (cond ((and elm (string-match "^\\([^,]+\\),[0-9]+$" elm))
                    (setq fname (propertize (match-string 1 elm)
                                            'face 'helm-etags-file)))
                   (elm (setq cand (format "%s:%s:%s" fname linum elm)))
                   (t (setq cand nil)))
          when cand do (progn
                         (insert (propertize (concat cand "\n") 'linum linum))
                         (progress-reporter-update progress-reporter count)))))

(defun helm-etags-init ()
  "Feed `helm-buffer' using `helm-etags-cache' or tag file.
If no entry in cache, create one."
  (let ((tagfiles (helm-etags-all-tag-files)))
    (when tagfiles
      (with-current-buffer (helm-candidate-buffer 'global)
        (dolist (f tagfiles)
          (helm-aif (gethash f helm-etags-cache)
              ;; An entry is present in cache, insert it.
              (insert it)
            ;; No entry, create a new buffer using content of tag file (slower).
            (helm-etags-create-buffer f)
            ;; Store content of buffer in cache.
            (puthash f (buffer-string) helm-etags-cache)
            ;; Store or set the last modification of tag file.
            (helm-aif (assoc f helm-etags-mtime-alist)
                ;; If an entry exists modify it.
                (setcdr it (helm-etags-mtime f))
              ;; No entry create a new one.
              (cl-pushnew (cons f (helm-etags-mtime f))
                          helm-etags-mtime-alist
                          :test 'equal))))))))

(defvar helm-source-etags-select nil
  "Helm source for Etags.")

(defun helm-etags-build-source ()
  (helm-build-in-buffer-source "Etags"
    :header-name 'helm-etags-get-header-name
    :init 'helm-etags-init
    :get-line 'buffer-substring
    :match-part (lambda (candidate)
                  ;; Match only the tag part of CANDIDATE
                  ;; and not the filename.
                  (cl-case helm-etags-match-part-only
                      (tag (cl-caddr (helm-grep-split-line candidate)))
                      (t   candidate)))
    :fuzzy-match helm-etags-fuzzy-match
    :help-message 'helm-etags-help-message
    :keymap helm-etags-map
    :action '(("Go to tag" . (lambda (c)
                               (helm-etags-action-goto 'find-file c)))
              ("Go to tag in other window" . (lambda (c)
                                               (helm-etags-action-goto
                                                'find-file-other-window
                                                c)))
              ("Go to tag in other frame" . (lambda (c)
                                              (helm-etags-action-goto
                                               'find-file-other-frame
                                               c))))
    :group 'helm-tags
    :persistent-help "Go to line"
    :persistent-action (lambda (candidate)
                         (helm-etags-action-goto 'find-file candidate)
                         (helm-highlight-current-line))))

(defcustom helm-etags-fuzzy-match nil
  "Use fuzzy matching in `helm-etags-select'."
  :group 'helm-tags
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (setq helm-source-etags-select
                (helm-etags-build-source))))

(defvar find-tag-marker-ring)

(defsubst helm-etags--file-from-tag (fname)
  (cl-loop for ext in
           (cons "" (remove "" tags-compression-info-list))
           for file = (concat fname ext)
           when (file-exists-p file)
           return file))

(defun helm-etags-action-goto (switcher candidate)
  "Helm default action to jump to an etags entry in other window."
  (require 'etags)
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (let* ((split (helm-grep-split-line candidate))
         (fname (cl-loop for tagf being the hash-keys of helm-etags-cache
                         for f = (expand-file-name
                                  (car split) (file-name-directory tagf))
                         ;; Try to find an existing file, possibly compressed.
                         when (helm-etags--file-from-tag f)
                         return it))
         (elm   (cl-caddr split))
         (linum (string-to-number (cadr split))))
    (if (null fname)
        (error "file %s not found" fname)
      (ring-insert find-tag-marker-ring (point-marker))
      (funcall switcher fname)
      (helm-goto-line linum t)
      (when (search-forward elm nil t)
        (goto-char (match-beginning 0))))))

(defun helm-etags-mtime (file)
  "Last modification time of etags tag FILE."
  (cadr (nth 5 (file-attributes file))))

(defun helm-etags-file-modified-p (file)
  "Check if tag FILE have been modified in this session.
If FILE is nil return nil."
  (let ((last-modif (and file
                         (assoc-default file helm-etags-mtime-alist))))
    (and last-modif
         (/= last-modif (helm-etags-mtime file)))))

;;;###autoload
(defun helm-etags-select (reinit)
  "Preconfigured helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command."
  (interactive "P")
  (let ((tag-files (helm-etags-all-tag-files))
        (helm-execute-action-at-once-if-one
         helm-etags-execute-action-at-once-if-one)
        (str (if (region-active-p)
                 (buffer-substring-no-properties
                  (region-beginning) (region-end))
                 ;; Use a raw syntax-table to determine tap.
                 ;; This may be wrong when calling etags
                 ;; with hff from a buffer that use
                 ;; a different syntax, but most of the time it
                 ;; should be better.
                 (with-syntax-table (standard-syntax-table)
                   (thing-at-point 'symbol)))))
    (if (cl-notany 'file-exists-p tag-files)
        (message "Error: No tag file found.\
Create with etags shell command, or visit with `find-tag' or `visit-tags-table'.")
        (cl-loop for k being the hash-keys of helm-etags-cache
                 unless (member k tag-files)
                 do (remhash k helm-etags-cache))
        (mapc (lambda (f)
                (when (or (equal reinit '(4))
                          (and helm-etags-mtime-alist
                               (helm-etags-file-modified-p f)))
                  (remhash f helm-etags-cache)))
              tag-files)
        (unless helm-source-etags-select
          (setq helm-source-etags-select
                (helm-etags-build-source)))
        (helm :sources 'helm-source-etags-select
              :keymap helm-etags-map
              :default (if helm-etags-fuzzy-match
                           str
                           (list (concat "\\_<" str "\\_>") str))
              :buffer "*helm etags*"))))

(provide 'helm-tags)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-tags.el ends here

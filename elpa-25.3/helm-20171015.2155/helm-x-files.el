;;; helm-x-files.el --- helm auxiliary functions and sources. -*- lexical-binding: t -*-

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

(require 'helm-for-files)


;;; List of files gleaned from every dired buffer
;;
;;
(defun helm-files-in-all-dired-candidates ()
  (save-excursion
    (cl-loop for (f . b) in dired-buffers
          when (buffer-live-p b)
          append (let ((dir (with-current-buffer b dired-directory)))
                   (if (listp dir) (cdr dir)
                     (directory-files f t dired-re-no-dot))))))

;; (dired '("~/" "~/.emacs.d/.emacs-custom.el" "~/.emacs.d/.emacs.bmk"))

(defclass helm-files-dired-source (helm-source-sync helm-type-file)
  ((candidates :initform #'helm-files-in-all-dired-candidates)))

(defvar helm-source-files-in-all-dired
  (helm-make-source "Files in all dired buffer." 'helm-files-dired-source))

;;; session.el files
;;
;;  session (http://emacs-session.sourceforge.net/) is an alternative to
;;  recentf that saves recent file history and much more.
(defvar session-file-alist)
(defclass helm-source-session-class (helm-source-sync)
  ((candidates :initform (lambda ()
                           (cl-delete-if-not
                            (lambda (f)
                              (or (string-match helm-tramp-file-name-regexp f)
                                  (file-exists-p f)))
                            (mapcar 'car session-file-alist))))
   (keymap       :initform helm-generic-files-map)
   (help-message :initform helm-generic-file-help-message)
   (action       :initform 'helm-type-file-actions)))

(defvar helm-source-session nil
  "File list from emacs-session.")

(defcustom helm-session-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-session' when non--nil."
  :group 'helm-files
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (setq helm-source-session
               (helm-make-source "Session" 'helm-source-session-class
                 :fuzzy-match val))))


;;; External searching file tools.
;;
;; Tracker desktop search
(defvar helm-source-tracker-cand-incomplete nil "Contains incomplete candidate")
(defun helm-source-tracker-transformer (candidates _source)
  (helm-log "received: %S" candidates)
  (cl-loop for cand in candidates
           for path = (when (stringp helm-source-tracker-cand-incomplete)
                        (caar (helm-highlight-files
                               (list helm-source-tracker-cand-incomplete)
                               nil)))
           for built = (if (not (stringp cand)) cand
                         (let ((snippet cand))
                           (unless (or (null path)
                                      (string= "" path)
                                      (not (string-match-p
                                          "\\`[[:space:]]*\\.\\.\\."
                                          snippet)))
                             (let ((complete-candidate
                                    (cons (concat path "\n" snippet) path)))
                               (setq helm-source-tracker-cand-incomplete nil)
                               (helm-log "built: %S" complete-candidate)
                               complete-candidate))))
           when (and (stringp cand)
                   (string-match "\\`[[:space:]]*file://" cand))
           do (setq helm-source-tracker-cand-incomplete ; save path
                    (replace-match "" t t cand)) end
           collect built))

(defvar helm-source-tracker-search
  (helm-build-async-source "Tracker Search"
    :candidates-process
     (lambda ()
       (start-process "tracker-search-process" nil
                      "tracker-search"
                      "--disable-color"
                      "--limit=512"
                      helm-pattern))
    :filtered-candidate-transformer #'helm-source-tracker-transformer
    ;;(multiline) ; https://github.com/emacs-helm/helm/issues/529
    :keymap helm-generic-files-map
    :action 'helm-type-file-actions
    :action-transformer '(helm-transform-file-load-el
                          helm-transform-file-browse-url)
    :requires-pattern 3)
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;; Spotlight (MacOS X desktop search)
(defclass helm-mac-spotlight-source (helm-source-async helm-type-file)
  ((candidates-process :initform
                       (lambda ()
                         (start-process
                          "mdfind-process" nil "mdfind" helm-pattern)))
   (requires-pattern :initform 3)))

(defvar helm-source-mac-spotlight
  (helm-make-source "mdfind" 'helm-mac-spotlight-source)
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

(provide 'helm-x-files)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-x-files.el ends here

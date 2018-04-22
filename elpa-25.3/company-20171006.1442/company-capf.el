;;; company-capf.el --- company-mode completion-at-point-functions backend -*- lexical-binding: t -*-

;; Copyright (C) 2013-2016  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(defvar company--capf-cache nil)

(defun company--capf-data ()
  (let ((cache company--capf-cache))
    (if (and (equal (current-buffer) (car cache))
             (equal (point) (car (setq cache (cdr cache))))
             (equal (buffer-chars-modified-tick) (car (setq cache (cdr cache)))))
        (cadr cache)
      (let ((data (company--capf-data-real)))
        (setq company--capf-cache
              (list (current-buffer) (point) (buffer-chars-modified-tick) data))
        data))))

(defun company--capf-data-real ()
  (cl-letf* (((default-value 'completion-at-point-functions)
              ;; Ignore tags-completion-at-point-function because it subverts
              ;; company-etags in the default value of company-backends, where
              ;; the latter comes later.
              (remove 'tags-completion-at-point-function
                      (default-value 'completion-at-point-functions)))
             (completion-at-point-functions (company--capf-workaround))
             (data (run-hook-wrapped 'completion-at-point-functions
                                     ;; Ignore misbehaving functions.
                                     #'completion--capf-wrapper 'optimist)))
    (when (and (consp (cdr data)) (integer-or-marker-p (nth 1 data))) data)))

(declare-function python-shell-get-process "python")

(defun company--capf-workaround ()
  ;; For http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18067
  (if (or (not (listp completion-at-point-functions))
          (not (memq 'python-completion-complete-at-point completion-at-point-functions))
          (python-shell-get-process))
      completion-at-point-functions
    (remq 'python-completion-complete-at-point completion-at-point-functions)))

(defun company-capf (command &optional arg &rest _args)
  "`company-mode' backend using `completion-at-point-functions'."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-capf))
    (`prefix
     (let ((res (company--capf-data)))
       (when res
         (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
               (prefix (buffer-substring-no-properties (nth 1 res) (point))))
           (cond
            ((> (nth 2 res) (point)) 'stop)
            (length (cons prefix length))
            (t prefix))))))
    (`candidates
     (let ((res (company--capf-data)))
       (when res
         (let* ((table (nth 3 res))
                (pred (plist-get (nthcdr 4 res) :predicate))
                (meta (completion-metadata
                      (buffer-substring (nth 1 res) (nth 2 res))
                      table pred))
                (sortfun (cdr (assq 'display-sort-function meta)))
                (candidates (completion-all-completions arg table pred (length arg)))
                (last (last candidates))
                (base-size (and (numberp (cdr last)) (cdr last))))
           (when base-size
             (setcdr last nil))
           (when sortfun
             (setq candidates (funcall sortfun candidates)))
           (if (not (zerop (or base-size 0)))
               (let ((before (substring arg 0 base-size)))
                 (mapcar (lambda (candidate)
                           (concat before candidate))
                         candidates))
             candidates)))))
    (`sorted
     (let ((res (company--capf-data)))
       (when res
         (let ((meta (completion-metadata
                      (buffer-substring (nth 1 res) (nth 2 res))
                      (nth 3 res) (plist-get (nthcdr 4 res) :predicate))))
           (cdr (assq 'display-sort-function meta))))))
    (`match
     ;; Can't just use 0 when base-size (see above) is non-zero.
     (let ((start (if (get-text-property 0 'face arg)
                      0
                    (next-single-property-change 0 'face arg))))
       (when start
         ;; completions-common-part comes first, but we can't just look for this
         ;; value because it can be in a list.
         (or
          (let ((value (get-text-property start 'face arg)))
            (text-property-not-all start (length arg)
                                   'face value arg))
          (length arg)))))
    (`duplicates t)
    (`no-cache t)   ;Not much can be done here, as long as we handle
                    ;non-prefix matches.
    (`meta
     (let ((f (plist-get (nthcdr 4 (company--capf-data)) :company-docsig)))
       (when f (funcall f arg))))
    (`doc-buffer
     (let ((f (plist-get (nthcdr 4 (company--capf-data)) :company-doc-buffer)))
       (when f (funcall f arg))))
    (`location
     (let ((f (plist-get (nthcdr 4 (company--capf-data)) :company-location)))
       (when f (funcall f arg))))
    (`annotation
     (save-excursion
       ;; FIXME: `company-begin' sets `company-point' after calling
       ;; `company--begin-new'.  We shouldn't rely on `company-point' here,
       ;; better to cache the capf-data value instead.  However: we can't just
       ;; save the last capf-data value in `prefix', because that command can
       ;; get called more often than `candidates', and at any point in the
       ;; buffer (https://github.com/company-mode/company-mode/issues/153).
       ;; We could try propertizing the returned prefix string, but it's not
       ;; passed to `annotation', and `company-prefix' is set only after
       ;; `company--strip-duplicates' is called.
       (when company-point
         (goto-char company-point))
       (let ((f (plist-get (nthcdr 4 (company--capf-data)) :annotation-function)))
         (when f (funcall f arg)))))
    (`require-match
     (plist-get (nthcdr 4 (company--capf-data)) :company-require-match))
    (`init nil)      ;Don't bother: plenty of other ways to initialize the code.
    (`post-completion
     (let* ((res (company--capf-data))
            (exit-function (plist-get (nthcdr 4 res) :exit-function))
            (table (nth 3 res))
            (pred (plist-get (nthcdr 4 res) :predicate)))
       (if exit-function
           ;; Follow the example of `completion--done'.
           (funcall exit-function arg
                    (if (eq (try-completion arg table pred) t)
                        'finished 'sole)))))
    ))

(provide 'company-capf)

;;; company-capf.el ends here

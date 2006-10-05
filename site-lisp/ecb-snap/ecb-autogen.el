;;; ecb-autogen.el --- Auto load statement generator

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2003

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-autogen.el,v 1.13 2005/02/28 11:31:59 berndl Exp $

;;; Commentary:
;;
;; Automatically generate autoloads for ECB
;;
;; This code is based onto semantic-autogen.el, the autoload generator of
;; semantic.
;;

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code
;;

(require 'autoload)

(require 'ecb-util)

(eval-when-compile
  (require 'silentcomp))


(when (ecb-noninteractive)
  ;; If the user is doing this non-interactively, we need to set up
  ;; these conveniences.
  (add-to-list 'load-path nil)
  (set (if (boundp 'find-file-hook)
           'find-file-hook
         'find-file-hooks) nil)
  (setq find-file-suppress-same-file-warnings t)
  )


(defconst ecb-autogen-header
  "Auto-generated ecb autoloads"
  "Header of the auto-generated autoloads file.")

(defconst ecb-autogen-file "ecb-autoloads.el"
  "Name of the auto-generated autoloads file.")

(defconst ecb-autoload-feature "ecb-autoloads"
  "Feature-name of the autoloads")

(defvar ecb-autogen-subdirs nil
  "Sub-directories to scan for autoloads.")

(defun ecb-autogen-update-header ()
  "Update header of the auto-generated autoloads file.
Run as `write-contents-hooks'."
  (when (ecb-string= generated-autoload-file (buffer-file-name))
    (let ((tag (format ";;; %s ---" (file-name-nondirectory
                                     (buffer-file-name)))))
      (message "Updating header...")
      (goto-char (point-min))
      (cond
       ;; Replace existing header line
       ((re-search-forward (concat "^" (regexp-quote tag)) nil t)
        (beginning-of-line)
        (kill-line 1)
        )
       ;; Insert header before first ^L encountered (XEmacs)
       ((re-search-forward "^" nil t)
        (beginning-of-line)
        ))
      (insert tag " " ecb-autogen-header)
      (newline)
      (message "Updating header...done")
      nil ;; Say not already written.
      )))

;; We code this so clumsy to silence the bytecompiler of GNU Emacs >= 21.4 not
;; to complain about obsoleteness of `write-contents-hooks'.
(defun ecb-batch-update-autoloads ()
  (let ((old-val (symbol-value (if (boundp 'write-contents-functions)
                                   'write-contents-functions
                                 'write-contents-hooks))))
    (unwind-protect
        (progn
          (set (if (boundp 'write-contents-functions)
                   'write-contents-functions
                 'write-contents-hooks)
               '(ecb-autogen-update-header))
          (batch-update-autoloads))
      (set (if (boundp 'write-contents-functions)
               'write-contents-functions
             'write-contents-hooks)
           old-val)))) 

(defun ecb-update-autoloads ()
  "Update ecb autoloads from sources.
Autoloads file name is defined in variable `ecb-autogen-file'. If ECB is
installed as regular XEmacs-package then this function reports an error and
does nothing."
  (interactive)
  (if ecb-regular-xemacs-package-p
      (ecb-error "Updating autoloads not possible for regular XEmacs-packages!")
    (if (file-exists-p (expand-file-name ecb-autogen-file))
        (delete-file (expand-file-name ecb-autogen-file)))
    (when (not ecb-running-xemacs)
      ;; generate a new one but do this not for XEmacs because XEmacs must(!)
      ;; handle this itself
      (with-temp-file (expand-file-name ecb-autogen-file)
        (insert "")))
    (let* ((default-directory (file-name-directory (locate-library "ecb")))
           (generated-autoload-file (expand-file-name ecb-autogen-file))
           ;; needed for XEmacs to ensure that always a feature 'ecb-autoloads
           ;; is provided and not a feature like 'ecb-1.91.2-autoloads (XEmacs
           ;; uses the installation-directory of ECB as feature prefix if
           ;; autoload-package-name is not provided.
           (autoload-package-name "ecb")
           (subdirs (mapcar 'expand-file-name ecb-autogen-subdirs))
           (command-line-args-left (cons default-directory subdirs))
           )
      (ecb-batch-update-autoloads))
    ;; XEmacs adds autom. the provide statement but for GNU Emacs we must do
    ;; this:
    (when (not ecb-running-xemacs)
      (save-excursion
        (set-buffer (find-file-noselect (expand-file-name ecb-autogen-file)))
        (goto-char (point-min))
        (when (not (re-search-forward (format "^(provide '%s)"
                                              ecb-autoload-feature) nil t))
          (goto-char (point-max))
          (insert (format "\n(provide '%s)\n" ecb-autoload-feature))
          (save-buffer)
          (kill-buffer (current-buffer)))))))

(silentcomp-provide 'ecb-autogen)

;;; ecb-autogen.el ends here

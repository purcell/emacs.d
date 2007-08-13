;;; cedet-autogen.el --- Generate autoloads for CEDET libraries

;; Copyright (C) 2003, 2004 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Created: 22 Aug 2003
;; Keywords: maint
;; X-CVS: $Id: cedet-autogen.el,v 1.6 2005/09/30 20:07:14 zappo Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Automatically generate autoloads for CEDET libraries.
;;

;;; History:
;;

;;; Code:
;;

(require 'autoload)
(eval-when-compile (require 'cl))

;;; Compatibility
(defun cedet-autogen-noninteractive ()
  "Return non-nil if running non-interactively."
  (if (featurep 'xemacs)
      (noninteractive)
    noninteractive))

(if (fboundp 'keywordp)
    (defalias 'cedet-autogen-keywordp 'keywordp)
  (defun cedet-autogen-keywordp (object)
    "Return t if OBJECT is a keyword.
This means that it is a symbol with a print name beginning with `:'
interned in the initial obarray."
    (and (symbolp object)
         (char-equal ?: (aref 0 (symbol-name object)))))
  )

(when (cedet-autogen-noninteractive)
  ;; If the user is doing this non-interactively, we need to set up
  ;; these conveniences.
  (add-to-list 'load-path nil)
  (setq find-file-hooks nil
        find-file-suppress-same-file-warnings t)
  )

(defadvice make-autoload (before cedet-make-autoload activate)
  "Extend `make-autoload' with support for particular CEDET forms.
When a such form, like defclass, defmethod, etc., is recognized, it is
replaced with side effect by an equivalent known form before calling
the true `make-autoload' function."
  (if (consp (ad-get-arg 0))
      (let* ((form (ad-get-arg 0))
             (car (car-safe form))
             name args doc
             )
        (cond
         ((eq car 'define-overload)
          (setcar form 'defun)
          )
         ((eq car 'defmethod)
          (setq name (nth 1 form)
                args (nthcdr 2 form))
          (if (cedet-autogen-keywordp (car args))
              (setq args (cdr args)))
          (setq doc  (nth 1 args)
                args (car args))
          (setcar form 'defun)
          (setcdr form (list name args (if (stringp doc) doc)))
          )
         ((eq car 'defclass)
          (setq name (nth 1 form)
                args (nth 2 form)
                doc  (nth 4 form))
          (setcar form 'defun)
          (setcdr form (list name args (if (stringp doc) doc)))
          ))
        )))

(defconst cedet-autogen-header
  "Auto-generated CEDET autoloads"
  "Header of the auto-generated autoloads file.")

(defconst cedet-autogen-tagfile ".cedet-lisp"
  "Dummy file that indicates to scan this directory for autoloads.")

(defun cedet-autogen-kill-xemacs-autoloads-feature ()
  "Remove Xemacs autoloads feature from this buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "(\\(featurep\\|provide\\) '\\sw+-autoloads" nil t)
      (condition-case nil
          (while t (up-list -1))
        (error nil))
      (kill-region (point) (save-excursion (forward-list) (point)))
      )))

(defun cedet-autogen-update-header ()
  "Update header of the auto-generated autoloads file.
Run as `write-contents-hooks'."
  (when (string-equal generated-autoload-file (buffer-file-name))
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
      (insert tag " " cedet-autogen-header)
      (newline)
      (when (featurep 'xemacs)
        (cedet-autogen-kill-xemacs-autoloads-feature))
      (message "Updating header...done")
      nil ;; Say not already written.
      )))

(defun cedet-autogen-subdirs (root-dir)
  "Return autoload candidate sub directories of ROOT-DIR.
That is, those where a `cedet-autogen-tagfile' file is found.
Return a list of directory names, relative to ROOT-DIR."
  (let (dirs)
    (dolist (dir (directory-files default-directory))
      (and (file-directory-p dir) (not (string-match dir "\\`..?\\'"))
           (let* ((default-directory (expand-file-name dir))
                  (subdirs (cedet-autogen-subdirs root-dir)))
             (when (file-exists-p cedet-autogen-tagfile)
               (push (file-relative-name default-directory root-dir)
                     subdirs))
             (setq dirs (nconc dirs subdirs)))))
    dirs))

(defun cedet-autogen-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  ;; If file don't exist, and is not automatically created...
  (unless (or (file-exists-p file)
              (fboundp 'autoload-ensure-default-file))
    ;; Create a file buffer.
    (find-file file)
    ;; Use Unix EOLs, so that the file is portable to all platforms.
    (setq buffer-file-coding-system 'raw-text-unix)
    (unless (featurep 'xemacs)
      ;; Insert a GNU Emacs loaddefs skeleton.
      (insert ";;; " (file-name-nondirectory file)
              " --- automatically extracted autoloads\n"
              ";;\n"
              ";;; Code:\n\n"
              "\n;; Local" " Variables:\n"
              ";; version-control: never\n"
              ";; no-byte-compile: t\n"
              ";; no-update-autoloads: t\n"
              ";; End:\n"
              ";;; " (file-name-nondirectory file)
              " ends here\n"))
    ;; Insert the header so that the buffer is not empty.
    (cedet-autogen-update-header))
  file)

;;;###autoload
(defun cedet-update-autoloads (loaddefs &optional directory &rest directories)
  "Update autoloads in file LOADDEFS from sources.
Optional argument DIRECTORY, specifies the directory to scan for
autoloads.  It defaults to the current directory.
DIRECTORIES is a list of extra directory to scan.  Those directory
names are relative to DIRECTORY.  If DIRECTORIES is nil try to scan
sub directories of DIRECTORY where a `cedet-autogen-tagfile' file
exists."
  (interactive "FLoaddefs file: \nDDirectory: ")
  (let* ((generated-autoload-file (expand-file-name loaddefs))
         (default-directory
           (file-name-as-directory
            (expand-file-name (or directory default-directory))))
         (extra-dirs (or directories
                         (cedet-autogen-subdirs default-directory)))
         (write-contents-hooks '(cedet-autogen-update-header))
         (command-line-args-left (cons default-directory extra-dirs))
         )
    (cedet-autogen-ensure-default-file generated-autoload-file)
    (batch-update-autoloads)))

(defun cedet-batch-update-autoloads ()
  "Update autoloads in batch mode.
Usage: emacs -batch -f cedet-batch-update-autoloads LOADDEFS [DIRECTORY]
See the command `cedet-update-autoloads' for the meaning of the
LOADDEFS and DIRECTORY arguments."
  (unless (cedet-autogen-noninteractive)
    (error "\
`cedet-batch-update-autoloads' is to be used only with -batch"))
  (condition-case err
      (apply 'cedet-update-autoloads command-line-args-left)
    (error
     (error "%S\n\
Usage: emacs -batch -f cedet-batch-update-autoloads LOADDEFS [DIRECTORY]"
	    err))
    ))

(provide 'cedet-autogen)

;;; cedet-autogen.el ends here

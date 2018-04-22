;;; async-bytecomp.el --- Compile elisp files asynchronously -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 Free Software Foundation, Inc.

;; Authors: John Wiegley <jwiegley@gmail.com>
;;          Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Keywords: dired async byte-compile
;; X-URL: https://github.com/jwiegley/dired-async

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  This package provide the `async-byte-recompile-directory' function
;;  which allows, as the name says to recompile a directory outside of
;;  your running emacs.
;;  The benefit is your files will be compiled in a clean environment without
;;  the old *.el files loaded.
;;  Among other things, this fix a bug in package.el which recompile
;;  the new files in the current environment with the old files loaded, creating
;;  errors in most packages after upgrades.
;;
;;  NB: This package is advicing the function `package--compile'.

;;; Code:

(require 'cl-lib)
(require 'async)

(defcustom async-bytecomp-allowed-packages
  '(async helm helm-core helm-ls-git helm-ls-hg magit)
  "Packages in this list will be compiled asynchronously by `package--compile'.
All the dependencies of these packages will be compiled async too,
so no need to add dependencies to this list.
The value of this variable can also be a list with a single element,
the symbol `all', in this case packages are always compiled asynchronously."
  :group 'async
  :type '(repeat (choice symbol)))

(defvar async-byte-compile-log-file "~/.emacs.d/async-bytecomp.log")

;;;###autoload
(defun async-byte-recompile-directory (directory &optional quiet)
  "Compile all *.el files in DIRECTORY asynchronously.
All *.elc files are systematically deleted before proceeding."
  (cl-loop with dir = (directory-files directory t "\\.elc\\'")
           unless dir return nil
           for f in dir
           when (file-exists-p f) do (delete-file f))
  ;; Ensure async is reloaded when async.elc is deleted.
  ;; This happen when recompiling its own directory.
  (load "async")
  (let ((call-back
         (lambda (&optional _ignore)
           (if (file-exists-p async-byte-compile-log-file)
               (let ((buf (get-buffer-create byte-compile-log-buffer))
                     (n 0))
                 (with-current-buffer buf
                   (goto-char (point-max))
                   (let ((inhibit-read-only t))
                     (insert-file-contents async-byte-compile-log-file)
                     (compilation-mode))
                   (display-buffer buf)
                   (delete-file async-byte-compile-log-file)
                   (unless quiet
                     (save-excursion
                       (goto-char (point-min))
                       (while (re-search-forward "^.*:Error:" nil t)
                         (cl-incf n)))
                     (if (> n 0)
                         (message "Failed to compile %d files in directory `%s'" n directory)
                         (message "Directory `%s' compiled asynchronously with warnings" directory)))))
               (unless quiet
                 (message "Directory `%s' compiled asynchronously with success" directory))))))
    (async-start
     `(lambda ()
        (require 'bytecomp)
        ,(async-inject-variables "\\`\\(load-path\\)\\|byte\\'")
        (let ((default-directory (file-name-as-directory ,directory))
              error-data)
          (add-to-list 'load-path default-directory)
          (byte-recompile-directory ,directory 0 t)
          (when (get-buffer byte-compile-log-buffer)
            (setq error-data (with-current-buffer byte-compile-log-buffer
                               (buffer-substring-no-properties (point-min) (point-max))))
            (unless (string= error-data "")
              (with-temp-file ,async-byte-compile-log-file
                (erase-buffer)
                (insert error-data))))))
     call-back)
    (unless quiet (message "Started compiling asynchronously directory %s" directory))))

(defvar package-archive-contents)
(defvar package-alist)
(declare-function package-desc-reqs "package.el" (cl-x))

(defun async-bytecomp--get-package-deps (pkg &optional only)
  ;; Same as `package--get-deps' but parse instead `package-archive-contents'
  ;; because PKG is not already installed and not present in `package-alist'.
  ;; However fallback to `package-alist' in case PKG no more present
  ;; in `package-archive-contents' due to modification to `package-archives'.
  ;; See issue #58.
  (let* ((pkg-desc (cadr (or (assq pkg package-archive-contents)
                             (assq pkg package-alist))))
         (direct-deps (cl-loop for p in (package-desc-reqs pkg-desc)
                               for name = (car p)
                               when (or (assq name package-archive-contents)
                                        (assq name package-alist))
                               collect name))
         (indirect-deps (unless (eq only 'direct)
                          (delete-dups
                           (cl-loop for p in direct-deps append
                                    (async-bytecomp--get-package-deps p))))))
    (cl-case only
      (direct   direct-deps)
      (separate (list direct-deps indirect-deps))
      (indirect indirect-deps)
      (t        (delete-dups (append direct-deps indirect-deps))))))

(defun async-bytecomp-get-allowed-pkgs ()
  (when (and async-bytecomp-allowed-packages
             (listp async-bytecomp-allowed-packages))
    (if package-archive-contents
        (cl-loop for p in async-bytecomp-allowed-packages
                 when (assq p package-archive-contents)
                 append (async-bytecomp--get-package-deps p) into reqs
                 finally return
                 (delete-dups
                  (append async-bytecomp-allowed-packages reqs)))
        async-bytecomp-allowed-packages)))

(defadvice package--compile (around byte-compile-async)
  (let ((cur-package (package-desc-name pkg-desc))
        (pkg-dir (package-desc-dir pkg-desc)))
    (if (or (equal async-bytecomp-allowed-packages '(all))
            (memq cur-package (async-bytecomp-get-allowed-pkgs)))
        (progn
          (when (eq cur-package 'async)
            (fmakunbound 'async-byte-recompile-directory))
          ;; Add to `load-path' the latest version of async and
          ;; reload it when reinstalling async.
          (when (string= cur-package "async")
            (cl-pushnew pkg-dir load-path)
            (load "async-bytecomp"))
          ;; `async-byte-recompile-directory' will add directory
          ;; as needed to `load-path'.
          (async-byte-recompile-directory (package-desc-dir pkg-desc) t))
        ad-do-it)))

;;;###autoload
(define-minor-mode async-bytecomp-package-mode
    "Byte compile asynchronously packages installed with package.el.
Async compilation of packages can be controlled by
`async-bytecomp-allowed-packages'."
  :group 'async
  :global t
  (if async-bytecomp-package-mode
      (ad-activate 'package--compile)
      (ad-deactivate 'package--compile)))

(provide 'async-bytecomp)

;;; async-bytecomp.el ends here

;;; f.el --- Modern API for working with files and directories -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.19.0
;; Package-Version: 20170404.1039
;; Keywords: files, directories
;; URL: http://github.com/rejeep/f.el
;; Package-Requires: ((s "1.7.0") (dash "2.2.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:



(require 's)
(require 'dash)

(put 'f-guard-error 'error-conditions '(error f-guard-error))
(put 'f-guard-error 'error-message "Destructive operation outside sandbox")

(defvar f--guard-paths nil
  "List of allowed paths to modify when guarded.

Do not modify this variable.")

(defmacro f--destructive (path &rest body)
  "If PATH is allowed to be modified, yield BODY.

If PATH is not allowed to be modified, throw error."
  (declare (indent 1))
  `(if f--guard-paths
       (if (--any? (or (f-same? it ,path)
                       (f-ancestor-of? it ,path)) f--guard-paths)
           (progn ,@body)
         (signal 'f-guard-error (list ,path f--guard-paths)))
     ,@body))


;;;; Paths

(defun f-join (&rest args)
  "Join ARGS to a single path."
  (let (path (relative (f-relative? (car args))))
    (-map
     (lambda (arg)
       (setq path (f-expand arg path)))
     args)
    (if relative (f-relative path) path)))

(defun f-split (path)
  "Split PATH and return list containing parts."
  (let ((parts (s-split (f-path-separator) path 'omit-nulls)))
    (if (f-absolute? path)
        (push (f-path-separator) parts)
      parts)))

(defun f-expand (path &optional dir)
  "Expand PATH relative to DIR (or `default-directory')."
  (let (file-name-handler-alist)
    (directory-file-name (expand-file-name path dir))))

(defun f-filename (path)
  "Return the name of PATH."
  (file-name-nondirectory (directory-file-name path)))

(defalias 'f-parent 'f-dirname)
(defun f-dirname (path)
  "Return the parent directory to PATH."
  (let ((parent (file-name-directory (f-expand path default-directory))))
    (unless (f-same? path parent)
      (if (f-relative? path)
          (f-relative parent)
        (directory-file-name parent)))))

(defun f-common-parent (paths)
  "Return the deepest common parent directory of PATHS."
  (cond
   ((not paths) nil)
   ((not (cdr paths)) (f-parent (car paths)))
   (:otherwise
    (let* ((paths (-map 'f-split paths))
           (common (caar paths))
           (re nil))
      (while (and (not (null (car paths))) (--all? (equal (car it) common) paths))
        (setq paths (-map 'cdr paths))
        (push common re)
        (setq common (caar paths)))
      (cond
       ((null re) "")
       ((and (= (length re) 1) (f-root? (car re)))
        (f-root))
       (:otherwise
        (concat (apply 'f-join (nreverse re)) "/")))))))

(defun f-ext (path)
  "Return the file extension of PATH.

The extension, in a file name, is the part that follows the last
'.', excluding version numbers and backup suffixes."
  (file-name-extension path))

(defun f-no-ext (path)
  "Return everything but the file extension of PATH."
  (file-name-sans-extension path))

(defun f-swap-ext (path ext)
  "Return PATH but with EXT as the new extension.
EXT must not be nil or empty."
  (if (s-blank? ext)
      (error "Extension cannot be empty or nil")
    (concat (f-no-ext path) "." ext)))

(defun f-base (path)
  "Return the name of PATH, excluding the extension of file."
  (f-no-ext (f-filename path)))

(defun f-relative (path &optional dir)
  "Return PATH relative to DIR."
  (file-relative-name path dir))

(defalias 'f-abbrev 'f-short)
(defun f-short (path)
  "Return abbrev of PATH.  See `abbreviate-file-name'."
  (abbreviate-file-name path))

(defun f-long (path)
  "Return long version of PATH."
  (f-expand path))

(defun f-canonical (path)
  "Return the canonical name of PATH."
  (file-truename path))

(defun f-slash (path)
  "Append slash to PATH unless one already.

Some functions, such as `call-process' requires there to be an
ending slash."
  (if (f-dir? path)
      (file-name-as-directory path)
    path))

(defun f-full (path)
  "Return absolute path to PATH, with ending slash."
  (f-slash (f-long path)))

(defun f--uniquify (paths)
  "Helper for `f-uniquify' and `f-uniquify-alist'."
  (let* ((files-length (length paths))
         (uniq-filenames (--map (cons it (f-filename it)) paths))
         (uniq-filenames-next (-group-by 'cdr uniq-filenames)))
    (while (/= files-length (length uniq-filenames-next))
      (setq uniq-filenames-next
            (-group-by 'cdr
                       (--mapcat
                        (let ((conf-files (cdr it)))
                          (if (> (length conf-files) 1)
                              (--map (cons (car it) (concat (f-filename (s-chop-suffix (cdr it) (car it))) (f-path-separator) (cdr it))) conf-files)
                            conf-files))
                        uniq-filenames-next))))
    uniq-filenames-next))

(defun f-uniquify (files)
  "Return unique suffixes of FILES.

This function expects no duplicate paths."
  (-map 'car (f--uniquify files)))

(defun f-uniquify-alist (files)
  "Return alist mapping FILES to unique suffixes of FILES.

This function expects no duplicate paths."
  (-map 'cadr (f--uniquify files)))


;;;; I/O

(defun f-read-bytes (path)
  "Read binary data from PATH.

Return the binary data as unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defalias 'f-read 'f-read-text)
(defun f-read-text (path &optional coding)
  "Read text with PATH, using CODING.

CODING defaults to `utf-8'.

Return the decoded text as multibyte string."
  (decode-coding-string (f-read-bytes path) (or coding 'utf-8)))

(defalias 'f-write 'f-write-text)
(defun f-write-text (text coding path)
  "Write TEXT with CODING to PATH.

TEXT is a multibyte string.  CODING is a coding system to encode
TEXT with.  PATH is a file name to write to."
  (f-write-bytes (encode-coding-string text coding) path))

(defun f-unibyte-string-p (s)
  "Determine whether S is a unibyte string."
  (not (multibyte-string-p s)))

(defun f-write-bytes (data path)
  "Write binary DATA to PATH.

DATA is a unibyte string.  PATH is a file name to write to."
  (f--destructive path
    (unless (f-unibyte-string-p data)
      (signal 'wrong-type-argument (list 'f-unibyte-string-p data)))
    (let ((file-coding-system-alist nil)
          (coding-system-for-write 'binary))
      (with-temp-file path
        (setq buffer-file-coding-system 'binary)
        (set-buffer-multibyte nil)
        (insert data)))))

(defalias 'f-append 'f-append-text)
(defun f-append-text (text coding path)
  "Append TEXT with CODING to PATH.

If PATH does not exist, it is created."
  (f-append-bytes (encode-coding-string text coding) path))

(defun f-append-bytes (data path)
  "Append binary DATA to PATH.

If PATH does not exist, it is created."
  (let ((content
         (if (f-file? path)
             (f-read-bytes path)
           "")))
    (f-write-bytes (concat content data) path)))


;;;; Destructive

(defun f-mkdir (&rest dirs)
  "Create directories DIRS."
  (let (path)
    (-each
        dirs
      (lambda (dir)
        (setq path (f-expand dir path))
        (unless (f-directory? path)
          (f--destructive path (make-directory path)))))))

(defun f-delete (path &optional force)
  "Delete PATH, which can be file or directory.

If FORCE is t, a directory will be deleted recursively."
  (f--destructive path
    (if (or (f-file? path) (f-symlink? path))
        (delete-file path)
      (delete-directory path force))))

(defun f-symlink (source path)
  "Create a symlink to SOURCE from PATH."
  (f--destructive path (make-symbolic-link source path)))

(defun f-move (from to)
  "Move or rename FROM to TO."
  (f--destructive to (rename-file from to t)))

(defun f-copy (from to)
  "Copy file or directory FROM to TO."
  (f--destructive to
    (if (f-file? from)
        (copy-file from to)
      ;; The behavior of `copy-directory' differs between Emacs 23 and
      ;; 24 in that in Emacs 23, the contents of `from' is copied to
      ;; `to', while in Emacs 24 the directory `from' is copied to
      ;; `to'. We want the Emacs 24 behavior.
      (if (> emacs-major-version 23)
          (copy-directory from to)
        (if (f-dir? to)
            (progn
              (apply 'f-mkdir (f-split to))
              (let ((new-to (f-expand (f-filename from) to)))
                (copy-directory from new-to)))
          (copy-directory from to))))))

(defun f-copy-contents (from to)
  "Copy contents in directory FROM, to directory TO."
  (unless (f-exists? to)
    (error "Cannot copy contents to non existing directory %s" to))
  (unless (f-dir? from)
    (error "Cannot copy contents as %s is a file" from))
  (--each (f-entries from)
    (f-copy it to)))

(defun f-touch (path)
  "Update PATH last modification date or create if it does not exist."
  (f--destructive path
    (if (f-file? path)
        (set-file-times path)
      (f-write-bytes "" path))))


;;;; Predicates

(defun f-exists? (path)
  "Return t if PATH exists, false otherwise."
  (file-exists-p path))

(defalias 'f-exists-p 'f-exists?)

(defalias 'f-dir? 'f-directory?)
(defalias 'f-dir-p 'f-dir?)

(defun f-directory? (path)
  "Return t if PATH is directory, false otherwise."
  (file-directory-p path))

(defalias 'f-directory-p 'f-directory?)

(defun f-file? (path)
  "Return t if PATH is file, false otherwise."
  (file-regular-p path))

(defalias 'f-file-p 'f-file?)

(defun f-symlink? (path)
  "Return t if PATH is symlink, false otherwise."
  (not (not (file-symlink-p path))))

(defalias 'f-symlink-p 'f-symlink?)

(defun f-readable? (path)
  "Return t if PATH is readable, false otherwise."
  (file-readable-p path))

(defalias 'f-readable-p 'f-readable?)

(defun f-writable? (path)
  "Return t if PATH is writable, false otherwise."
  (file-writable-p path))

(defalias 'f-writable-p 'f-writable?)

(defun f-executable? (path)
  "Return t if PATH is executable, false otherwise."
  (file-executable-p path))

(defalias 'f-executable-p 'f-executable?)

(defun f-absolute? (path)
  "Return t if PATH is absolute, false otherwise."
  (file-name-absolute-p path))

(defalias 'f-absolute-p 'f-absolute?)

(defun f-relative? (path)
  "Return t if PATH is relative, false otherwise."
  (not (f-absolute? path)))

(defalias 'f-relative-p 'f-relative?)

(defun f-root? (path)
  "Return t if PATH is root directory, false otherwise."
  (not (f-parent path)))

(defalias 'f-root-p 'f-root?)

(defun f-ext? (path &optional ext)
  "Return t if extension of PATH is EXT, false otherwise.

If EXT is nil or omitted, return t if PATH has any extension,
false otherwise.

The extension, in a file name, is the part that follows the last
'.', excluding version numbers and backup suffixes."
  (if ext
      (string= (f-ext path) ext)
    (not (eq (f-ext path) nil))))

(defalias 'f-ext-p 'f-ext?)

(defalias 'f-equal? 'f-same?)
(defalias 'f-equal-p 'f-equal?)

(defun f-same? (path-a path-b)
  "Return t if PATH-A and PATH-B are references to same file."
  (when (and (f-exists? path-a)
             (f-exists? path-b))
    (equal
     (f-canonical (f-expand path-a))
     (f-canonical (f-expand path-b)))))

(defalias 'f-same-p 'f-same?)

(defun f-parent-of? (path-a path-b)
  "Return t if PATH-A is parent of PATH-B."
  (--when-let (f-parent path-b)
    (f-same? path-a it)))

(defalias 'f-parent-of-p 'f-parent-of?)

(defun f-child-of? (path-a path-b)
  "Return t if PATH-A is child of PATH-B."
  (--when-let (f-parent path-a)
    (f-same? it path-b)))

(defalias 'f-child-of-p 'f-child-of?)

(defun f-ancestor-of? (path-a path-b)
  "Return t if PATH-A is ancestor of PATH-B."
  (unless (f-same? path-a path-b)
    (s-starts-with? (f-full path-a)
                    (f-full path-b))))

(defalias 'f-ancestor-of-p 'f-ancestor-of?)

(defun f-descendant-of? (path-a path-b)
  "Return t if PATH-A is desendant of PATH-B."
  (unless (f-same? path-a path-b)
    (s-starts-with? (f-full path-b)
                    (f-full path-a))))

(defalias 'f-descendant-of-p 'f-descendant-of?)

(defun f-hidden? (path)
  "Return t if PATH is hidden, nil otherwise."
  (unless (f-exists? path)
    (error "Path does not exist: %s" path))
  (string= (substring path 0 1) "."))

(defalias 'f-hidden-p 'f-hidden?)


;;;; Stats

(defun f-size (path)
  "Return size of PATH.

If PATH is a file, return size of that file.  If PATH is
directory, return sum of all files in PATH."
  (if (f-directory? path)
      (-sum (-map 'f-size (f-files path nil t)))
    (nth 7 (file-attributes path))))

(defun f-depth (path)
  "Return the depth of PATH.

At first, PATH is expanded with `f-expand'.  Then the full path is used to
detect the depth.
'/' will be zero depth,  '/usr' will be one depth.  And so on."
  (- (length (f-split (f-expand path))) 1))


;;;; Misc

(defun f-this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

(defun f-path-separator ()
  "Return path separator."
  (substring (f-join "x" "y") 1 2))

(defun f-glob (pattern &optional path)
  "Find PATTERN in PATH."
  (file-expand-wildcards
   (f-join (or path default-directory) pattern)))

(defun f--collect-entries (path recursive)
  (let (result
        (entries
         (-reject
          (lambda (file)
            (or
             (equal (f-filename file) ".")
             (equal (f-filename file) "..")))
          (directory-files path t))))
    (cond (recursive
           (-map
            (lambda (entry)
              (if (f-file? entry)
                  (setq result (cons entry result))
                (when (f-directory? entry)
                  (setq result (cons entry result))
                  (setq result (append result (f--collect-entries entry recursive))))))
            entries))
          (t (setq result entries)))
    result))

(defmacro f--entries (path body &optional recursive)
  "Anaphoric version of `f-entries'."
  `(f-entries
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-entries (path &optional fn recursive)
  "Find all files and directories in PATH.

FN - called for each found file and directory.  If FN returns a thruthy
value, file or directory will be included.
RECURSIVE - Search for files and directories recursive."
  (let ((entries (f--collect-entries path recursive)))
    (if fn (-select fn entries) entries)))

(defmacro f--directories (path body &optional recursive)
  "Anaphoric version of `f-directories'."
  `(f-directories
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-directories (path &optional fn recursive)
  "Find all directories in PATH.  See `f-entries'."
  (let ((directories (-select 'f-directory? (f--collect-entries path recursive))))
    (if fn (-select fn directories) directories)))

(defmacro f--files (path body &optional recursive)
  "Anaphoric version of `f-files'."
  `(f-files
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-files (path &optional fn recursive)
  "Find all files in PATH.  See `f-entries'."
  (let ((files (-select 'f-file? (f--collect-entries path recursive))))
    (if fn (-select fn files) files)))

(defmacro f--traverse-upwards (body &optional path)
  "Anaphoric version of `f-traverse-upwards'."
  `(f-traverse-upwards
    (lambda (dir)
      (let ((it dir))
        ,body))
    ,path))

(defun f-traverse-upwards (fn &optional path)
  "Traverse up as long as FN return nil, starting at PATH.

If FN returns a non-nil value, the path sent as argument to FN is
returned.  If no function callback return a non-nil value, nil is
returned."
  (unless path
    (setq path default-directory))
  (when (f-relative? path)
    (setq path (f-expand path)))
  (if (funcall fn path)
      path
    (unless (f-root? path)
      (f-traverse-upwards fn (f-parent path)))))

(defun f-root ()
  "Return absolute root."
  (f-traverse-upwards 'f-root?))

(defmacro f-with-sandbox (path-or-paths &rest body)
  "Only allow PATH-OR-PATHS and decendants to be modified in BODY."
  (declare (indent 1))
  `(let ((paths (if (listp ,path-or-paths)
                    ,path-or-paths
                  (list ,path-or-paths))))
     (unwind-protect
         (let ((f--guard-paths paths))
           ,@body)
       (setq f--guard-paths nil))))

(provide 'f)

;;; f.el ends here

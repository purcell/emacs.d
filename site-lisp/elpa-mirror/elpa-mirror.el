;;; elpa-mirror.el --- ELPA mirror from locally installed packages is easy

;; Copyright (C) 2014 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/elpa-mirror
;; Version: 1.1.2
;; Keywords: cloud mirror elpa
;;
;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of elpa-mirror
;;
;; elpa-mirror is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; elpa-mirror is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  `M-x elpamr-create-mirror-for-installed` is the only command you need run.
;;

;;; Code:
(require 'package)

(defvar elpamr-default-output-directory
  nil
  "The output directory. If nil, user will be required provide one when running `elpamr-create-mirror-for-installed`")

(defvar elpamr-repository-name
  "myelpa"
  "repository name to be displayed in index.html.")

(defvar elpamr-repository-path
  "http://myelpa.mydomain.com"
  "Repository path to be displayed in index.html")

(defvar elpamr-email
  "name@mydomain.com"
  "Email to be displayed in index.html")

(defvar elpamr-exclude-package-from-repositories
  '("myelpa")
  "From certain repositories, we include NO package")

(defun elpamr--create-one-item-for-archive-contents (pkg)
  "We can use package-alist directly. This API will append some meta info into package-alist."
  (let ((name (car pkg))
        item
        package-content
        repo
        found
        (i 0))

    (while (and (not found)
                (< i (length package-archive-contents)))
      (setq package-content (nth i package-archive-contents))
      ;; well, all we need do it to write the actual version into package-content
      (when (string= name (car package-content))
        ;; we try to provide more information from archive-contents if possible
        (aset (cdr package-content) 0 (elt (cdr pkg) 0))
        (setq item package-content)
        (setq found t)
        )
      (setq i (1+ i)))

    (unless found
      ;; make do with installed package, looks it's deleted in archive-contents
      (setq item pkg))

    (setq repo (elt (cdr package-content) 4))
    (if (listp repo)  (setq repo (elt (cdr package-content) 5)))
    ;; (message "repo=%s" repo)
    (cond
     ((member repo elpamr-exclude-package-from-repositories)
      (setq item nil))
     (t
      (let ((a (cdr item)) na)
        (cond
         (found
          (when (>= (length a) 4)
            ;; only need first four
            (setq na (vector (elt a 0)
                             (elt a 1)
                             (elt a 2)
                             (elt a 3)))
            (setq item (cons (car item) na))))
         (t
          ;; we assume it's tar format
          (when (>= (length a) 3)
            ;; only need first three
            (setq na (vector (elt a 0)
                             (elt a 1)
                             (elt a 2)
                             'tar))
            (setq item (cons (car item) na))))
         ))
      ))
    item
    ))


(defun elpamr--package-info (dirname)
  "return '(package-name integer-version-number) or nil"
  (interactive)
  (let (rlt name version)
    (when (string-match "\\(.*\\)-\\([0-9.]+\\)$" dirname)
      (setq name (match-string 1 dirname))
      (setq version (split-string (match-string 2 dirname) "\\."))
      (setq rlt (list name version)))
    rlt
    ))

(defun elpamr--output-fullpath (file)
  "return full path of output file give the FILE"
  (file-truename (concat
                  (file-name-as-directory elpamr-default-output-directory)
                  file)))

(defun elpamr--get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun elpamr--clean-package-description (descr)
  (replace-regexp-in-string "-\*-.*-\*-" "" descr t))

(defun elpamr--create-complete-package-name (item)
  (concat (symbol-name (car item))
          "-"
          (mapconcat (lambda (arg) (format "%d" arg)) (elt (cdr item) 0) ".")))

(defun elpamr--format-package-list-into-json (list)
  (let (pkg-name)
    (mapconcat
     (lambda (item)
       (setq pkg-name (elpamr--create-complete-package-name item))
       (format "'%s'" pkg-name)
       ) list ",\n")
    ))

(defun elpamr--is-single-el (item)
  (let ((a (cdr item)))
    (and (> (length a) 3) (string= "single" (elt a 3)))
    ))

(defun elpamr--format-package-list-into-html (list)
  (let (tar-name (cnt 0))
    (mapconcat
     (lambda (item)
       (setq cnt (1+ cnt))
       (setq tar-name (concat (elpamr--create-complete-package-name item)
                              (if (elpamr--is-single-el item) ".el" ".tar")
                              ))
       (format "<div id='n%d' class='name'><a href='%s'>%s</a></div><div id='d%d' class='descr'>%s</div>\n"
               cnt
               tar-name
               tar-name
               cnt
               (elpamr--clean-package-description (elt (cdr item) 2)))
       ) list "\n")
    ))

(defun elpamr--format-email ()
  (format "<a href='mailto:%s'>%s</a>" elpamr-email elpamr-email))

(defun elpamr--output-html (rlt)
  (let ((js-file (elpamr--output-fullpath "elpa-mirror.js"))
        (js-tmpl (concat
                  (file-name-directory (if load-file-name load-file-name (symbol-file 'elpamr--output-html)))
                  "elpa-mirror.js"))
        (html-file (elpamr--output-fullpath "index.html"))
        ;; @see http://stackoverflow.com/questions/145291/smart-home-in-emacs/145359#145359
        (html-tmpl (concat
                    (file-name-directory (if load-file-name load-file-name (symbol-file 'elpamr--output-html)))
                    "index.html")))

    ;; index.html
    (with-temp-buffer
      (let ((print-level nil)  (print-length nil) str)
        (setq str (replace-regexp-in-string
                 "elpamr-package-list-html"
                 (elpamr--format-package-list-into-html rlt)
                 (elpamr--get-string-from-file html-tmpl)
                 t))
        (setq str (replace-regexp-in-string
                   "elpamr-package-list-json"
                   (elpamr--format-package-list-into-json rlt)
                   str
                   t))
        (setq str (replace-regexp-in-string
                   "elpamr-email"
                   (elpamr--format-email)
                   str
                   t))
        (setq str (replace-regexp-in-string
                   "elpamr-repository-name"
                   elpamr-repository-name
                   str
                   t))
        (setq str (replace-regexp-in-string
                   "elpamr-repository-path"
                   elpamr-repository-path
                   str
                   t))
        (insert str))
      (write-file html-file))

    ;; js file
    (with-temp-buffer
      (let ((print-level nil)  (print-length nil))
        (insert (elpamr--get-string-from-file js-tmpl)))
      (write-file js-file))
    ))

(defun elpamr--is-single-el-by-name (name pkglist)
  (interactive)
  (let (rlt)
    (dolist (pkg pkglist)
      (if (string= (car pkg) name)
          (setq rlt (elpamr--is-single-el pkg))
        ))
    rlt))

;;;###autoload
(defun elpamr-create-mirror-for-installed ()
  "Export INSTALLED packages into a new directory. Create html files for the mirror site.
If elpamr-default-output-directory is not nil, it's assumed that is output directory. Or else, user will be asked to provide the output directory."
  (interactive)
  (let (item rlt pkg-dirname pkg-info tar-cmd len dirs cnt)
    (dolist (pkg package-alist)
      (setq item (elpamr--create-one-item-for-archive-contents pkg))
      (if item (push item rlt))
      )

    (unless (and elpamr-default-output-directory (file-directory-p elpamr-default-output-directory))
      (setq elpamr-default-output-directory (read-directory-name "Output directory:"))
      )

    (when (and (> (length rlt) 0)
               elpamr-default-output-directory
               (file-directory-p elpamr-default-output-directory))
      (setq dirs (directory-files package-user-dir))
      (setq cnt 0)
      (setq len (length dirs))
      (dolist (dir dirs)
        (unless (or (member dir '("archives" "." ".."))
                    (not (setq pkg-info (elpamr--package-info dir))))

          (cond
           ;; copy single el
           ((elpamr--is-single-el-by-name (car pkg-info) rlt)
            (setq tar-cmd (concat "cd " package-user-dir
                                  "; cp "
                                  (file-name-as-directory dir) (car pkg-info) ".el"
                                  " "
                                  (elpamr--output-fullpath dir)
                                  ".el ")))
           ;; create tar using GNU tar or BSD tar
           (t
            (setq tar-cmd (concat "cd " package-user-dir "; COPYFILE_DISABLE=\"\" tar cf " (elpamr--output-fullpath dir) ".tar --exclude=*.elc --exclude=*~ " dir))
            ))
          (shell-command tar-cmd)
          (setq cnt (1+ cnt))
          (message "Creating *.tar and *.el ... %d%%" (/ (* cnt 100) len))
          ))

      ;; output archive-contents
      (with-temp-buffer
        (let ((print-level nil)  (print-length nil))
          (insert (format "%S" (cons 1 rlt))))
        (write-file (elpamr--output-fullpath "archive-contents")))
      (elpamr--output-html rlt)
      (message "DONE! Output into %s" elpamr-default-output-directory))
    ))

(provide 'elpa-mirror)
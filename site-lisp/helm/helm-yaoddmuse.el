;;; helm-yaoddmuse.el --- Helm extension for yaoddmuse -*- lexical-binding: t -*-

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

;;; Code:

(require 'cl-lib)
(require 'helm)

(declare-function yaoddmuse-update-pagename "ext:yaoddmuse.el" (&optional unforced))
(declare-function yaoddmuse-get-library-list "ext:yaoddmuse.el" (&optional dirs string))

;; Be sure to have yaoddmuse.el installed
;; install-elisp may be required if you want to install elisp file from here.
(defvar helm-yaoddmuse-use-cache-file nil)
(defvar helm-yaoddmuse-cache-file "~/.emacs.d/yaoddmuse-cache.el")
(defvar helm-yaoddmuse-ew-cache nil)
(defvar yaoddmuse-pages-hash)

(defun helm-yaoddmuse-get-candidates ()
  (if helm-yaoddmuse-use-cache-file
      (ignore-errors
        (unless helm-yaoddmuse-ew-cache
          (load helm-yaoddmuse-cache-file)
          (setq helm-yaoddmuse-ew-cache
                (gethash "EmacsWiki" yaoddmuse-pages-hash)))
        helm-yaoddmuse-ew-cache)
    (yaoddmuse-update-pagename t)
    (gethash "EmacsWiki" yaoddmuse-pages-hash)))

(defvar helm-source-yaoddmuse-emacswiki-edit-or-view
  '((name . "Yaoddmuse Edit or View (EmacsWiki)")
    (candidates . helm-yaoddmuse-get-candidates)
    (action . (("Edit page" . (lambda (candidate)
                                (yaoddmuse-edit "EmacsWiki" candidate)))
               ("Browse page"
                . (lambda (candidate)
                    (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse page other window"
                . (lambda (candidate)
                    (if (one-window-p)
                        (split-window-vertically))
                    (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse diff"
                . (lambda (candidate)
                    (yaoddmuse-browse-page-diff "EmacsWiki" candidate)))
               ("Copy URL"
                . (lambda (candidate)
                    (kill-new (yaoddmuse-url "EmacsWiki" candidate))
                    (message "Have copy page %s's URL to yank." candidate)))
               ("Create page"
                . (lambda (candidate)
                    (yaoddmuse-edit "EmacsWiki" helm-input)))
               ("Update cache"
                . (lambda (candidate)
                    (if helm-yaoddmuse-use-cache-file
                        (progn
                          (helm-yaoddmuse-cache-pages t)
                          (setq helm-yaoddmuse-ew-cache
                                (gethash "EmacsWiki" yaoddmuse-pages-hash)))
                      (yaoddmuse-update-pagename))))))
    (action-transformer helm-yaoddmuse-action-transformer))
  "Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el")


(defvar helm-source-yaoddmuse-emacswiki-post-library
  '((name . "Yaoddmuse Post library (EmacsWiki)")
    (init . (helm-yaoddmuse-init))
    (candidates-in-buffer)
    (action . (("Post library and Browse"
                . (lambda (candidate)
                    (yaoddmuse-post-file
                     (find-library-name candidate)
                     "EmacsWiki"
                     (file-name-nondirectory (find-library-name candidate))
                     nil t)))
               ("Post library"
                . (lambda (candidate)
                    (yaoddmuse-post-file
                     (find-library-name candidate)
                     "EmacsWiki"
                     (file-name-nondirectory
                      (find-library-name candidate))))))))
  "Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el")


(defun helm-yaoddmuse-action-transformer (actions candidate)
  "Allow the use of `install-elisp' only on elisp files."
  (if (string-match "\.el$" candidate)
      (append actions '(("Install Elisp"
                         . (lambda (elm)
                             (install-elisp-from-emacswiki elm)))))
    actions))

;;;###autoload
(defun helm-yaoddmuse-cache-pages (&optional load)
  "Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'."
  (interactive)
  (yaoddmuse-update-pagename)
  (save-excursion
    (find-file helm-yaoddmuse-cache-file)
    (erase-buffer)
    (insert "(puthash \"EmacsWiki\" '(")
    (cl-loop for i in (gethash "EmacsWiki" yaoddmuse-pages-hash)
          do
          (insert (concat "(\"" (car i) "\") ")))
    (insert ") yaoddmuse-pages-hash)\n")
    (save-buffer)
    (kill-buffer (current-buffer))
    (when (or current-prefix-arg
              load)
      (load helm-yaoddmuse-cache-file))))

(defun helm-yaoddmuse-init ()
  "Init helm buffer status."
  (let ((helm-buffer (helm-candidate-buffer 'global))
        (library-list (yaoddmuse-get-library-list)))
    (with-current-buffer helm-buffer
      ;; Insert library name.
      (cl-dolist (library library-list)
        (insert (format "%s\n" library)))
      ;; Sort lines.
      (sort-lines nil (point-min) (point-max)))))

;;;###autoload
(defun helm-yaoddmuse-emacswiki-edit-or-view ()
  "Preconfigured `helm' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el"
  (interactive)
  (helm :sources 'helm-source-yaoddmuse-emacswiki-edit-or-view))

;;;###autoload
(defun helm-yaoddmuse-emacswiki-post-library ()
  "Preconfigured `helm' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el"
  (interactive)
  (helm :sources 'helm-source-yaoddmuse-emacswiki-post-library))

(provide 'helm-yaoddmuse)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-yaoddmuse.el ends here

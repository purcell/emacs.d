;;; js2-build.el --- Build script for js2-mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code:

(defvar js2-mode-directory
  (expand-file-name "./"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      command-line-default-directory))
  "Directory where js2-mode is installed. ")

(defconst js2-build-directory
  (concat js2-mode-directory "build/")
  "Where `js2-build-js2-mode' build artifacts are deposited.")
  
(defun js2-build-js2-mode ()
  "Byte-compile js2-mode.el.
Rewrites some of the code on the fly."
  (interactive)
  (when (get-buffer "js2-mode.el")
    (save-excursion
      (set-buffer "js2-mode.el")
      (set-buffer-modified-p nil)
      (kill-buffer "js2-mode.el")))
  (save-some-buffers)
  (unless (file-exists-p js2-build-directory)
    (make-directory js2-build-directory))
  (let ((infile (concat js2-mode-directory "js2-mode.el"))
        (outfile (concat js2-build-directory "js2-mode.el")))
    (copy-file infile outfile 'overwrite)
    (copy-file outfile (concat js2-build-directory "js2-mode-"
                               (js2-build-version-number)
                               ".el")
               'overwrite)
    (if (get-buffer "*Compile-Log*")
        (kill-buffer "*Compile-Log*"))
    (byte-compile-file outfile)
    (let ((dest (format "%sjs2-emacs%d.elc" js2-build-directory
                       emacs-major-version)))
      (copy-file (format "%sc" outfile) dest 'overwrite)
      (message "Wrote %s" dest))
    (pop-to-buffer "js2-mode.el")
    (toggle-read-only 1)
    (pop-to-buffer "*Compile-Log*")
    (message "finished compilation")))

(defun js2-build-version-number ()
  (let* ((s (split-string (current-time-string)))  ; "Mon Mar 24 19:09:09 2008"
         (month (nth 1 s))
         (day (nth 2 s))
         (year (nth 4 s)))
    (setq month (cdr (assoc month '(("Jan" . "01")
                                    ("Feb" . "02")
                                    ("Mar" . "03")
                                    ("Apr" . "04")
                                    ("May" . "05")
                                    ("Jun" . "06")
                                    ("Jul" . "07")
                                    ("Aug" . "08")
                                    ("Sep" . "09")
                                    ("Oct" . "10")
                                    ("Nov" . "11")
                                    ("Dec" . "12")))))
    (format "%s%s%02d" year month (string-to-number day))))

(provide 'js2-build)

;;; js2-build.el ends here

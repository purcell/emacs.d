;;; helm-firefox.el --- Firefox bookmarks -*- lexical-binding: t -*-

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
(require 'helm-utils)
(require 'helm-adaptive)
;;
;; You will have to set firefox to import bookmarks in his html file bookmarks.html.
;; (only for firefox versions >=3)
;; To achieve that, open about:config in firefox and double click on this line to enable value
;; to true:
;; user_pref("browser.bookmarks.autoExportHTML", false);
;; You should have now:
;; user_pref("browser.bookmarks.autoExportHTML", true);
;; NOTE: This is also working in the same way for mozilla aka seamonkey.


(defgroup helm-firefox nil
  "Helm libraries and applications for Firefox navigator."
  :group 'helm)

(defcustom helm-firefox-default-directory "/.mozilla/firefox/"
  "The root directory containing firefox config."
  :group 'helm-firefox
  :type 'string)

(defvar helm-firefox-bookmark-url-regexp "\\(https\\|http\\|ftp\\|about\\|file\\)://[^ \"]*")
(defvar helm-firefox-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")

(defun helm-get-firefox-user-init-dir ()
  "Guess the default Firefox user directory name."
  (let* ((moz-dir (concat (getenv "HOME") helm-firefox-default-directory))
         (moz-user-dir
          (with-current-buffer (find-file-noselect (concat moz-dir "profiles.ini"))
            (goto-char (point-min))
            (prog1
                (when (search-forward "Path=" nil t)
                  (buffer-substring-no-properties (point) (point-at-eol)))
              (kill-buffer)))))
    (file-name-as-directory (concat moz-dir moz-user-dir))))

(defun helm-guess-firefox-bookmark-file ()
  "Return the path of the Firefox bookmarks file."
  (concat (helm-get-firefox-user-init-dir) "bookmarks.html"))

(defvar helm-firefox-bookmarks-alist nil)
(defvar helm-source-firefox-bookmarks
  '((name . "Firefox Bookmarks")
    (init . (lambda ()
              (setq helm-firefox-bookmarks-alist
                    (helm-html-bookmarks-to-alist
                     (helm-guess-firefox-bookmark-file)
                     helm-firefox-bookmark-url-regexp
                     helm-firefox-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car helm-firefox-bookmarks-alist)))
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-firefox-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (helm-browse-url
                     (helm-firefox-bookmarks-get-value candidate))))
               ("Copy Url"
                . (lambda (candidate)
                    (let ((url (helm-firefox-bookmarks-get-value
                                candidate))) 
                      (kill-new url)
                      (message "`%s' copied to kill-ring" url))))))))

(defun helm-firefox-bookmarks-get-value (elm)
  (assoc-default elm helm-firefox-bookmarks-alist))

(defun helm-highlight-firefox-bookmarks (bookmarks _source)
  (cl-loop for i in bookmarks
        collect (propertize
                 i 'face '((:foreground "YellowGreen"))
                 'help-echo (helm-firefox-bookmarks-get-value i))))

;;;###autoload
(defun helm-firefox-bookmarks ()
  "Preconfigured `helm' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value \
to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.
"
  (interactive)
  (helm-other-buffer 'helm-source-firefox-bookmarks
                     "*Helm Firefox*"))


(provide 'helm-firefox)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-firefox.el ends here

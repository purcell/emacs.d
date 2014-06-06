;;; helm-emms.el --- Emms for Helm. -*- lexical-binding: t -*-

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

(declare-function emms-streams "ext:emms-streams")
(declare-function emms-stream-delete-bookmark "ext:emms-streams")
(declare-function emms-stream-add-bookmark "ext:emms-streams" (name url fd type))
(declare-function emms-stream-save-bookmarks-file "ext:emms-streams")
(declare-function emms-stream-quit "ext:emms-streams")
(declare-function with-current-emms-playlist "ext:emms" (&rest body))
(declare-function emms-playlist-tracks-in-region "ext:emms" (beg end))
(declare-function emms-playlist-first "ext:emms")
(declare-function emms-playlist-mode-play-smart "ext:emms-playlist-mode")


(defgroup helm-emms nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defface helm-emms-playlist
    '((t (:foreground "Springgreen4" :underline t)))
  "Face used for tracks in current emms playlist."
  :group 'helm-emms)


(defvar emms-stream-list)
(defun helm-emms-stream-edit-bookmark (elm)
  "Change the information of current emms-stream bookmark from helm."
  (let* ((cur-buf helm-current-buffer)
         (bookmark (assoc elm emms-stream-list))
         (name     (read-from-minibuffer "Description: "
                                         (nth 0 bookmark)))
         (url      (read-from-minibuffer "URL: "
                                         (nth 1 bookmark)))
         (fd       (read-from-minibuffer "Feed Descriptor: "
                                         (int-to-string (nth 2 bookmark))))
         (type     (read-from-minibuffer "Type (url, streamlist, or lastfm): "
                                         (format "%s" (car (last bookmark))))))
    (save-window-excursion
      (emms-streams)
      (when (re-search-forward (concat "^" name) nil t)
        (forward-line 0)
        (emms-stream-delete-bookmark)
        (emms-stream-add-bookmark name url (string-to-number fd) type)
        (emms-stream-save-bookmarks-file)
        (emms-stream-quit)
        (helm-switch-to-buffer cur-buf)))))

(defun helm-emms-stream-delete-bookmark (_candidate)
  "Delete emms-streams bookmarks from helm."
  (let* ((cands   (helm-marked-candidates))
         (bmks    (cl-loop for bm in cands collect
                        (car (assoc bm emms-stream-list))))
         (bmk-reg (mapconcat 'regexp-quote bmks "\\|^")))
    (when (y-or-n-p (format "Really delete radios\n -%s: ? "
                            (mapconcat 'identity bmks "\n -")))
      (save-window-excursion
        (emms-streams)
        (goto-char (point-min))
        (cl-loop while (re-search-forward bmk-reg nil t)
              do (progn (forward-line 0)
                        (emms-stream-delete-bookmark))
              finally do (progn
                           (emms-stream-save-bookmarks-file)
                           (emms-stream-quit)))))))

(defvar helm-source-emms-streams
  '((name . "Emms Streams")
    (init . (lambda ()
              (emms-stream-init)))
    (candidates . (lambda ()
                    (mapcar 'car emms-stream-list)))
    (action . (("Play" . (lambda (elm)
                           (let* ((stream (assoc elm emms-stream-list))
                                  (fn (intern (concat "emms-play-" (symbol-name (car (last stream))))))
                                  (url (cl-second stream)))
                             (funcall fn url))))
               ("Delete" . helm-emms-stream-delete-bookmark)
               ("Edit" . helm-emms-stream-edit-bookmark)))
    (filtered-candidate-transformer . helm-adaptive-sort)))

;; Don't forget to set `emms-source-file-default-directory'
(defvar helm-source-emms-dired
  '((name . "Music Directory")
    (candidates . (lambda ()
                    (cddr (directory-files emms-source-file-default-directory))))
    (action .
     (("Play Directory" . (lambda (item)
                            (emms-play-directory
                             (expand-file-name
                              item
                              emms-source-file-default-directory))))
      ("Open dired in file's directory" . (lambda (item)
                                            (helm-open-dired
                                             (expand-file-name
                                              item
                                              emms-source-file-default-directory))))))
    (filtered-candidate-transformer . helm-adaptive-sort)))

(defvar helm-emms-current-playlist nil)
(defun helm-emms-files-modifier (candidates _source)
  (cl-loop for i in candidates
        if (member (cdr i) helm-emms-current-playlist)
        collect (cons (propertize (car i)
                                  'face 'helm-emms-playlist)
                      (cdr i))
        into lis
        else collect i into lis
        finally return (reverse lis)))

(defun helm-emms-play-current-playlist ()
  "Play current playlist."
  (emms-playlist-first)
  (emms-playlist-mode-play-smart))

(defvar helm-source-emms-files
  '((name . "Emms files")
    (init . (lambda ()
              (setq helm-emms-current-playlist
                    (with-current-emms-playlist
                      (cl-loop with cur-list = (emms-playlist-tracks-in-region
                                                (point-min) (point-max))
                            for i in cur-list
                            for name = (assoc-default 'name i)
                            when name
                            collect name)))))
    (candidates . (lambda ()
                    (cl-loop for v being the hash-values in emms-cache-db
                          for name      = (assoc-default 'name v)
                          for artist    = (or (assoc-default 'info-artist v) "unknown")
                          for genre     = (or (assoc-default 'info-genre v) "unknown")
                          for tracknum  = (or (assoc-default 'info-tracknumber v) "unknown")
                          for song      = (or (assoc-default 'info-title v) "unknown")
                          for info      = (concat artist " - " genre " - " tracknum ": " song)
                          unless (string-match "^\\(http\\|mms\\):" name)
                          collect (cons info name))))
    (filtered-candidate-transformer . helm-emms-files-modifier)
    (candidate-number-limit . 9999)
    (action . (("Play file" . emms-play-file)
               ("Add to Playlist and play (C-u clear current)"
                . (lambda (candidate)
                    (with-current-emms-playlist
                      (when helm-current-prefix-arg
                        (emms-playlist-current-clear))
                      (emms-playlist-new)
                      (mapc 'emms-add-playlist-file (helm-marked-candidates))
                      (unless emms-player-playing-p
                        (helm-emms-play-current-playlist)))))))))

;;;###autoload
(defun helm-emms ()
  "Preconfigured `helm' for emms sources."
  (interactive)
  (helm :sources '(helm-source-emms-streams
                   helm-source-emms-files
                   helm-source-emms-dired)
        :buffer "*Helm Emms*"))


(provide 'helm-emms)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-emms ends here

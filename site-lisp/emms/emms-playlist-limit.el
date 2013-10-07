;;; emms-playlist-limit.el --- Limit playlist by various info

;; Copyright (C) 2007, 2008, 2009 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: emms, limit

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'emms-playlist-mode)

;;; User Interfaces

(defgroup emms-playlist-limit nil
  "Playlist limit module for EMMS."
  :group 'emms)

(defcustom emms-playlist-limit-hook nil
  "Hooks to run after each limit operations."
  :type 'symbol
  :group 'emms-playing-limit)

(defvar emms-playlist-limit-enabled-p nil
  "If non-nil, emms playlist limit is enabled.")

(defun emms-playlist-limit (arg)
  "Turn on emms playlist limit if ARG is positive, off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (progn
        (setq emms-playlist-limit-enabled-p t)
        (add-hook 'emms-playlist-source-inserted-hook
                  'emms-playlist-limit-insert))
    (setq emms-playlist-limit-enabled-p nil)
    (remove-hook 'emms-playlist-source-inserted-hook
                 'emms-playlist-limit-insert)))

;;;###autoload
(defun emms-playlist-limit-enable ()
  "Turn on emms playlist limit."
  (interactive)
  (emms-playlist-limit 1)
  (message "emms playlist limit enabled"))

;;;###autoload
(defun emms-playlist-limit-disable ()
  "Turn off emms playlist limit."
  (interactive)
  (emms-playlist-limit -1)
  (message "emms playlist limit disabled"))

;;;###autoload
(defun emms-playlist-limit-toggle ()
  "Toggle emms playlist limit."
  (interactive)
  (if emms-playlist-limit-enabled-p
      (emms-playlist-limit-disable)
    (emms-playlist-limit-enable)))

(defmacro define-emms-playlist-limit (attribute)
  "Macro for defining emms playlist limit functions."
  `(defun ,(intern (format "emms-playlist-limit-to-%s" attribute)) (regexp)
     ,(format "Limit to playlists that have %s that matches REGEXP." attribute)
     (interactive
      (list
       (let* ((curr
               (or (emms-track-get
                    (emms-playlist-track-at) (quote ,attribute))
                   (emms-track-get
                    (emms-playlist-selected-track) (quote ,attribute))))
              (attr-name ,(emms-replace-regexp-in-string
                           "info-" "" (symbol-name attribute)))
              (fmt (if curr
                       (format "Limit to %s (regexp = %s): " attr-name curr)
                     (format "Limit to %s (regexp): " attr-name))))
         (read-string fmt))))
     (when (string= regexp "")
       (setq regexp (emms-track-get (emms-playlist-track-at) (quote ,attribute))))
     (emms-playlist-limit-do (quote ,attribute) regexp)))

(define-emms-playlist-limit info-artist)
(define-emms-playlist-limit info-composer)
(define-emms-playlist-limit info-performer)
(define-emms-playlist-limit info-title)
(define-emms-playlist-limit info-album)
(define-emms-playlist-limit info-year)
(define-emms-playlist-limit info-genre)
(define-emms-playlist-limit name)

(defun emms-playlist-limit-to-all ()
  "Show all tracks again."
  (interactive)
  (emms-playlist-limit-do nil nil))

(define-key emms-playlist-mode-map (kbd "/ n") 'emms-playlist-limit-to-name)
(define-key emms-playlist-mode-map (kbd "/ a") 'emms-playlist-limit-to-info-artist)
(define-key emms-playlist-mode-map (kbd "/ c") 'emms-playlist-limit-to-info-composer)
(define-key emms-playlist-mode-map (kbd "/ p") 'emms-playlist-limit-to-info-performer)
(define-key emms-playlist-mode-map (kbd "/ t") 'emms-playlist-limit-to-info-title)
(define-key emms-playlist-mode-map (kbd "/ b") 'emms-playlist-limit-to-info-album)
(define-key emms-playlist-mode-map (kbd "/ y") 'emms-playlist-limit-to-info-year)
(define-key emms-playlist-mode-map (kbd "/ g") 'emms-playlist-limit-to-info-genre)
(define-key emms-playlist-mode-map (kbd "/ /") 'emms-playlist-limit-to-all)


;;; Low Level Functions

(defvar emms-playlist-limit-tracks nil
  "All tracks in playlist buffer(unlimited).")

(defun emms-playlist-limit-insert ()
  "Run in `emms-playlist-source-inserted-hook'."
  (with-current-emms-playlist
    (emms-playlist-ensure-playlist-buffer)
    (setq emms-playlist-limit-tracks
          (emms-with-widened-buffer
           (emms-playlist-tracks-in-region (point-min) (point-max))))))

;; FIXME: When user deletes some tracks, `emms-playlist-limit-tracks'
;; should be updated.
;; (defun emms-playlist-limit-clear ()
;;   "Run in `emms-playlist-cleared-hook'."
;;   (setq emms-playlist-limit-tracks
;;         (append emms-playlist-limit-tracks
;;                 (emms-playlist-tracks-in-region
;;                  (point-min) (point-max)))))

(defun emms-playlist-limit-do (name value)
  "Limit by NAME with VALUE.
e.g.,
    (emms-playlist-limit-do 'info-artist \"Jane Zhang\")

When NAME is nil, show all tracks again.

See `emms-info-mp3find-arguments' for possible options for NAME."
  (with-current-emms-playlist
    (emms-playlist-ensure-playlist-buffer)
    (let ((curr (emms-playlist-current-selected-track))
          (tracks (emms-playlist-tracks-in-region (point-min) (point-max))))
      (erase-buffer)
      (run-hooks 'emms-playlist-cleared-hook)
      (if name
          (mapc (lambda (track)
                  (let ((track-value (emms-track-get track name)))
                    (when (and track-value (string-match value track-value))
                      (emms-playlist-insert-track track))))
                tracks)
        (mapc (lambda (track)
                (emms-playlist-insert-track track))
              emms-playlist-limit-tracks))
      (let ((pos (text-property-any (point-min) (point-max)
                                    'emms-track curr)))
        (if pos
            (emms-playlist-select pos)
          (emms-playlist-first)))
      (run-hooks 'emms-playlist-limit-hook)
      (emms-playlist-mode-center-current))))


(provide 'emms-playlist-limit)

;;; emms-playlist-limit.el ends here

;;; emms-playlist-sort.el --- sort emms playlist

;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Code:

(eval-when-compile (require 'cl))
(require 'emms-last-played)
(require 'emms-playlist-mode)

;;; User Customizations

(defgroup emms-playlist-sort nil
  "Sorting Emacs Multimedia System playlists."
  :prefix "emms-playlist-sort-"
  :group 'emms)

(defcustom emms-playlist-sort-list '(info-artist info-album)
  "Sorting list used by `emms-playlist-sort-by-list'.
Currently it understands the following fields: name info-artist
imfo-composer info-performer info-title info-album info-genre
info-playing-time info-tracknumber info-discnumber."
  :type 'symbol
  :group 'emms-playlist-sort)

(defcustom emms-playlist-sort-prefix "S"
  "Prefix key sequence for `emms-playlist-sort-map'.
Remember to call `emms-playlist-sort-map-setup' if you modify it."
  :type 'string
  :group 'emms-playlist-sort)


;;; User Interfaces

(defmacro define-emms-playlist-sort (attribute)
  "Macro for defining emms playlist sort functions on strings ."
  `(defun ,(intern (format "emms-playlist-sort-by-%s" attribute)) ()
     ,(format "Sort emms playlist by %s, increasingly.
With a prefix argument, decreasingly." attribute)
     (interactive)
     (emms-playlist-sort
      '(lambda (a b)
         (funcall 
          (if current-prefix-arg 'emms-string> 'emms-string<)
          (emms-track-get a (quote ,attribute))
          (emms-track-get b (quote ,attribute)))))))

(define-emms-playlist-sort name)
(define-emms-playlist-sort info-artist)
(define-emms-playlist-sort info-composer)
(define-emms-playlist-sort info-performer)
(define-emms-playlist-sort info-title)
(define-emms-playlist-sort info-album)
(define-emms-playlist-sort info-year)
(define-emms-playlist-sort info-note)

(defun emms-playlist-sort-by-natural-order ()
  "Sort emms playlist by natural order.
See `emms-sort-natural-order-less-p'."
  (interactive)
  (emms-playlist-sort 'emms-sort-natural-order-less-p))

(defun emms-playlist-sort-by-list ()
  "Sort emms playlist by `emms-playlist-sort-list'.
The sort will be carried out until comparsion succeeds, increasingly."
  (interactive)
  (emms-playlist-sort 'emms-playlist-sort-by-list-p))

(defun emms-playlist-sort-by-last-played ()
  "Sort emms playlist by last played time, increasingly.
With a prefix argument, decreasingly."
  (interactive)
  (emms-playlist-sort
   '(lambda (a b)
      (funcall 
       (if current-prefix-arg 'not 'identity)
       (time-less-p
        (or (emms-track-get a 'last-played) '(0 0 0))
        (or (emms-track-get b 'last-played) '(0 0 0)))))))

(defun emms-playlist-sort-by-play-count ()
  "Sort emms playlist by play-count, increasingly.
With a prefix argument, decreasingly."
  (interactive)
  (emms-playlist-sort
   '(lambda (a b)
      (funcall 
       (if current-prefix-arg 'not 'identity)
       (< (or (emms-track-get a 'play-count) 0)
          (or (emms-track-get b 'play-count) 0))))))

(defun emms-playlist-sort-by-file-extension ()
  "Sort emms playlist by file extension, increasingly.
With a prefix argument, decreasingly."
  (interactive)
  (emms-playlist-sort
   '(lambda (a b)
      (funcall 
       (if current-prefix-arg 'emms-string> 'emms-string<)
       (file-name-extension (emms-track-get a 'name))
       (file-name-extension (emms-track-get b 'name))))))

(defvar emms-playlist-sort-map nil)

(defun emms-playlist-sort-map-setup ()
  "Setup sort map with latest `emms-playlist-sort-prefix'."
  (setq emms-playlist-sort-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "n") 'emms-playlist-sort-by-natural-order)
          (define-key map (kbd "a") 'emms-playlist-sort-by-info-artist)
          (define-key map (kbd "c") 'emms-playlist-sort-by-play-count)
          (define-key map (kbd "b") 'emms-playlist-sort-by-info-album)
          (define-key map (kbd "l") 'emms-playlist-sort-by-last-played)
          (define-key map (kbd "t") 'emms-playlist-sort-by-info-title)
          (define-key map (kbd "e") 'emms-playlist-sort-by-file-extension)

          (define-key map (kbd "p") 'emms-playlist-sort-by-info-performer)
          (define-key map (kbd "y") 'emms-playlist-sort-by-info-year)
          (define-key map (kbd "o") 'emms-playlist-sort-by-info-note)
          (define-key map (kbd "C") 'emms-playlist-sort-by-info-composer)
          (define-key map (kbd "L") 'emms-playlist-sort-by-list)
          (define-key map (kbd "N") 'emms-playlist-sort-by-name)
          map))

  (define-key emms-playlist-mode-map
    emms-playlist-sort-prefix emms-playlist-sort-map))

(setq emms-playlist-sort-map (emms-playlist-sort-map-setup))


;;; Low Level Functions

(defun emms-playlist-sort (predicate)
  "Sort the playlist buffer by PREDICATE."
  (with-current-emms-playlist
    (emms-playlist-ensure-playlist-buffer)
    (let ((current (emms-playlist-selected-track))
          (tracks (nreverse
                   (emms-playlist-tracks-in-region
                    (point-min) (point-max)))))
      (delete-region (point-min) (point-max))
      (run-hooks 'emms-playlist-cleared-hook)
      (mapc 'emms-playlist-insert-track (sort tracks predicate))
      (let ((pos (text-property-any
                  (point-min) (point-max) 'emms-track current)))
        (if pos
            (emms-playlist-select pos)
          (emms-playlist-first))
        ;; (emms-playlist-mode-center-current)
        (goto-char (point-min))
        ))))

(defun emms-sort-natural-order-less-p (a b)
  "Sort two tracks by natural order.
This is the order in which albums where intended to be played.
ie. by album name and then by track number."
  (let ((album-a (emms-track-get a 'info-album))
        (album-b (emms-track-get b 'info-album))
	(discnum-a (string-to-number (or (emms-track-get a 'info-discnumber) "0")))
	(discnum-b (string-to-number (or (emms-track-get b 'info-discnumber) "0")))
	(tracknum-a (string-to-number (or (emms-track-get a 'info-tracknumber) "0")))
	(tracknum-b (string-to-number (or (emms-track-get b 'info-tracknumber) "0"))))
    (or (emms-string< album-a album-b)
        (and album-a album-b
             (string= album-a album-b)
	     (or (< discnum-a discnum-b)
		 (and (= discnum-a discnum-b)
		      (< tracknum-a tracknum-b)))))))

(defun emms-playlist-sort-by-list-p (a b)
  (catch 'return
    (dolist (info emms-playlist-sort-list)
      (case info
        ((name info-artist info-composer info-performer info-title info-album info-genre)
         (when (emms-string< (emms-track-get a info)
                        (emms-track-get b info))
           (throw 'return t)))
        ((info-playing-time)
         (when (< (emms-track-get a info)
                  (emms-track-get b info))
           (throw 'return t)))
        ((info-tracknumber info-discnumber)
         (when (< (string-to-number (or (emms-track-get a info) "0"))
                  (string-to-number (or (emms-track-get b info) "0")))
           (throw 'return t)))))))

(defun emms-string< (s1 s2)
  (string< (downcase (or s1 "")) (downcase (or s2 ""))))

(defun emms-string> (s1 s2)
  (let ((a (downcase (or s1 "")))
        (b (downcase (or s2 ""))))
    (not (or (string= a b) (string< a b)))))

(provide 'emms-playlist-sort)

;;; emms-playlist-sort.el ends here

;; emms-streams.el -- interface to add and play streams

;; Copyright (C) 2004, 2005, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Authors: Lucas Bonnet <lucas@rincevent.net>
;;          Jose A Ortega Ruiz <jao@gnu.org>
;;          Yoni Rabkin <yonirabkin@member.fsf.org>
;;          Michael Olson <mwolson@gnu.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; It is part of the EMMS package

;; Heavily based on bmk-mgr.el by Jose A Ortega Ruiz <jao@gnu.org>
;; thanks to you !

;;; Code:

(require 'emms)
(require 'later-do)

(defgroup emms-stream nil
  "*Add and play streams with EMMS."
  :group 'emms)

(defcustom emms-stream-bookmarks-file (concat (file-name-as-directory emms-directory) "streams")
  "*The file where you store your favorite emms streams."
  :type 'file
  :group 'emms-stream)

(defcustom emms-stream-default-action "add"
  "*The default action when you press RET in the EMMS Stream interface.
Can be either \"add\" or \"play\". The default is \"add\"."
  :type 'string
  :group 'emms-stream)

(defface emms-stream-name-face '((t (:bold t :weight bold)))
  "Face for stream names."
  :group 'emms-stream)

(defface emms-stream-url-face
  '((((class color) (background dark))
     (:foreground "LightSteelBlue"))
    (((class color) (background light))
     (:foreground "Blue")))
  "Face for stream URLs."
  :group 'emms-stream)

(defvar emms-stream-list nil
  "The list that contains your current stream bookmarks.")

(defvar emms-stream-buffer-name "*EMMS Streams*"
  "The name of the buffer used by emms-stream interface.")

(defvar emms-stream-play-hook nil
  "*A hook run when you add or play an EMMS stream via the popup.")

(defvar emms-stream-hook nil
"*A hook run when you call emms-streams or emms-stream-popup.")

(defvar emms-stream-current-stream nil
  "The stream currently being played.
Needed by the info method, as the track doesn't contain all the
needed info.")

(defvar emms-stream-popup-old-conf nil
  "Old window configuration.")

(defvar emms-stream-last-stream nil
  "The last stream added/played by EMMS.")

(defvar emms-stream-playlist-buffer nil
  "The EMMS playlist buffer associated with emms-streams.")

(defcustom emms-stream-repeat-p nil
  "*If non-nil, try to repeat a streamlist if it gets disconnected."
  :set (function
        (lambda (sym val)
          (when (buffer-live-p emms-stream-playlist-buffer)
            (with-current-buffer emms-stream-playlist-buffer
              (setq emms-repeat-playlist val)))
          (set sym val)))
  :type 'boolean
  :group 'emms-stream)

;; Format: (("descriptive name" url feed-number type))
;;
;; type could be either url, playlist, or lastfm. If url, then it
;; represents a direct IP, if streamlist it's a stream playlist, if
;; lastfm it's a lastfm station
(defvar emms-stream-default-list
 '(("SomaFM: Beatblender"
    "http://www.somafm.com/beatblender.pls" 1 streamlist)
   ("SomaFM: Secret Agent"
    "http://www.somafm.com/secretagent.pls" 1 streamlist)
   ("SomaFM: Groove Salad"
    "http://www.somafm.com/groovesalad.pls" 1 streamlist)
   ("SomaFM: Drone Zone"
    "http://www.somafm.com/dronezone.pls" 1 streamlist)
   ("SomaFM: Tag's Trance"
    "http://www.somafm.com/tagstrance.pls" 1 streamlist)
   ("SomaFM: Indie Pop Rocks"
    "http://www.somafm.com/indiepop.pls" 1 streamlist)
   ("SomaFM: Doomed"
    "http://www.somafm.com/doomed.pls" 1 streamlist)
   ("Digitally Imported, Trance"
    "http://www.di.fm/mp3/trance.pls" 1 streamlist)
   ("Digitally Imported, Deephouse"
    "http://www.di.fm/mp3/deephouse.pls" 1 streamlist)
   ("Digitally Imported, Chillout"
    "http://www.di.fm/mp3/chillout.pls" 1 streamlist)
   ("Digitally Imported, Drum and Bass"
    "http://www.di.fm/mp3/drumandbass.pls" 1 streamlist)
   ("SKY.fm, Mostly Classical"
    "http://www.sky.fm/mp3/classical.pls" 1 streamlist)
   ("SKY.fm, Jazz"
    "http://www.sky.fm/mp3/jazz.pls" 1 streamlist)
   ("Philosomatika, Goa-Trance"
    "http://www.shoutcast.com/sbin/shoutcast-playlist.pls?rn=1712&file=filename.pls" 1 streamlist)
   ("Drum and Bass Radio, BassDrive"
    "http://www.bassdrive.com/BassDrive.m3u" 1 streamlist)
   ("Flaresound, Jazzmusique"
    "http://64.236.34.196:80/stream/1016" 1 url)
   ("Flaresound, Jazzmusique"
    "http://205.188.234.4:8004" 2 url)
   ("Flaresound, L'Electric"
    "http://www.bp6.com:8002" 1 url)
   ("Stangs Garage, Eclectic"
    "http://www.stangsgarage.com/listen.pls" 1 streamlist)
   ("DNA Lounge, Live"
    "http://www.dnalounge.com/webcast/128.m3u" 1 streamlist)
   ("DNA Lounge Radio"
    "http://www.dnalounge.com/webcast/dnaradio.m3u" 1 streamlist)
   ("Virgin Radio, The Groove"
    "http://www.smgradio.com/core/audio/ogg/live.pls?service=grbb"
    1 streamlist)
   ("Virgin Radio, Virgin Classic"
    "http://www.smgradio.com/core/audio/ogg/live.pls?service=vcbb"
    1 streamlist)
   ("Virgin Radio, Virgin 1215AM"
    "http://www.smgradio.com/core/audio/ogg/live.pls?service=vrbb"
    1 streamlist)
   ("Voices From Within - Words From Beyond"
    "http://207.200.96.225:8024/listen.pls" 1 streamlist)
   ("WCPE, Classical Music"
    "http://www.ibiblio.org/wcpe/wcpe.pls" 1 streamlist)
   ("PLUG: Voices of the Free Software movement"
    "http://purduelug.org:8000/voices-free_software.ogg" 1 url)
   ("VGamp Radio, Video Game music"
    "http://vgamp.com/listen128.pls" 1 streamlist)
   ("Kohina - Old school game and demo music"
    "http://stream.nute.net/kohina/stream.ogg.m3u" 1 streamlist)
   ("Nectarine, Demoscene Radio"
    "http://www.scenemusic.eu:8002/high.ogg.m3u" 1 streamlist)
   ("idobi Radio"
    "http://www.idobi.com/radio/iradio.pls" 1 streamlist)
   ("radio.wazee - Modern Alternative Rock"
    "http://www.wazee.org/128.pls" 1 streamlist)
   ("ChroniX Aggression - Loud & Clear"
    "http://www.chronixradio.com/chronixaggression/listen/listen.pls"
    1 streamlist)
   ("WFMU, Freeform radio"
    "http://www.wfmu.org/wfmu.pls" 1 streamlist)
   ("KEXP - Seattle Community Radio"
    "http://kexp-mp3-128k.cac.washington.edu:8000/listen.pls" 1 streamlist)
   ("KRUU-LP - Fairfield, Iowa Community Radio"
    "http://kruufm.com/live.pls" 1 streamlist)
   ("WBCR-LP - Berkshire Community Radio"
    "http://nyc01.egihosting.com:6232/listen.pls" 1 streamlist)))

(defvar emms-stream-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "C-a") 'beginning-of-line)
    (define-key map (kbd "C-e") 'end-of-line)
    (define-key map (kbd "C-k") 'emms-stream-kill-bookmark)
    (define-key map (kbd "C-y") 'emms-stream-yank-bookmark)
    (define-key map (kbd "C-n") 'emms-stream-next-line)
    (define-key map (kbd "C-p") 'emms-stream-previous-line)
    (define-key map (kbd "Q") 'emms-stream-quit)
    (define-key map (kbd "a") 'emms-stream-add-bookmark)
    (define-key map (kbd "d") 'emms-stream-delete-bookmark)
    (define-key map (kbd "e") 'emms-stream-edit-bookmark)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "n") 'emms-stream-next-line)
    (define-key map (kbd "p") 'emms-stream-previous-line)
    (define-key map (kbd "q") 'emms-stream-quit)
    (define-key map (kbd "s") 'emms-stream-save-bookmarks-file)
    (define-key map (kbd "t") 'emms-stream-toggle-default-action)
;;    (define-key map (kbd "u") 'emms-stream-move-bookmark-up)
    (define-key map (kbd "i") 'emms-stream-info-bookmark)
    (define-key map (kbd "<up>") 'emms-stream-previous-line)
    (define-key map (kbd "<down>") 'emms-stream-next-line)
    (define-key map (kbd "<left>") 'beginning-of-line)
    (define-key map (kbd "<right>") 'end-of-line)
    (define-key map (kbd "RET") 'emms-stream-play)
    map)
  "Keymap for `emms-stream-menu'.")

;;;###autoload
(defun emms-streams ()
  "Opens the EMMS Streams interface."
  (interactive)
  (kill-buffer (get-buffer-create emms-stream-buffer-name))
  (set-buffer (get-buffer-create emms-stream-buffer-name))
  (erase-buffer)
  (when (string= emms-stream-default-action "play")
    (emms-stream-create-playlist))
  (emms-stream-mode)
  (switch-to-buffer emms-stream-buffer-name))

(defun emms-stream-mode ()
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'emms-stream-mode)
  (setq mode-name "EMMS Streams")
  (use-local-map emms-stream-mode-map)
  (emms-stream-init)
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'auto-hscroll-mode) t)
  (set (make-local-variable 'kill-whole-line) t)
  (set (make-local-variable 'next-line-add-newlines) nil)
  (goto-char 1)
  (emms-stream-display)
  (toggle-read-only 1)
  (run-hooks 'emms-stream-hook)
  (set-buffer-modified-p nil)
  (message "EMMS Stream Menu"))

(defun emms-stream-create-playlist ()
  "Create a new EMMS playlist and associate it with emms-streams.
This is used when `emms-stream-default-action' is \"play\"."
  (save-excursion
    (setq emms-stream-playlist-buffer
          (emms-playlist-set-playlist-buffer (emms-playlist-new)))
    (with-current-buffer emms-stream-playlist-buffer
      ;; if emms-stream-repeat-p is non-nil, make sure that we
      ;; continue to play the station, even if briefly disconnected
      (set (make-local-variable 'emms-repeat-playlist)
           emms-stream-repeat-p))))

(defun emms-stream-kill-playlist ()
  "Delete the EMMS playlist associated with emms-streams, if one exists."
  (when (buffer-live-p emms-stream-playlist-buffer)
    (save-excursion
      (if (eq emms-stream-playlist-buffer emms-playlist-buffer)
          (emms-playlist-current-kill)
        (kill-buffer emms-stream-playlist-buffer)))
    (setq emms-stream-playlist-buffer nil)))

(defun emms-stream-popup-revert ()
  "Revert to the window-configuration from before if there is one,
otherwise just remove the special bindings from the stream menu."
  (interactive)
  (remove-hook 'emms-pbi-manually-change-song-hook 'emms-pbi-popup-revert)
  (let ((streambuffer (get-buffer emms-stream-buffer-name)))
    (when streambuffer
      (save-excursion
	(set-buffer streambuffer)
	;; (local-unset-key (kbd "q"))
	(local-unset-key (kbd "TAB")))))
        ;; (local-unset-key (kbd "RET")))))
  (when emms-stream-popup-old-conf
    (set-window-configuration emms-stream-popup-old-conf))
  (remove-hook 'emms-stream-play-hook 'emms-stream-popup-revert)
  (remove-hook 'emms-stream-quit-hook 'emms-stream-popup-revert))

(defun emms-stream-popup (&optional popup-height)
  "Pops up the stream Menu, for the new stream selection.

POPUP-HEIGHT is the height of the new frame, defaulting to
`emms-popup-default-height'."
  (interactive)
  (setq popup-height (or popup-height (/ (window-height) 2)))
  ;; Split the current screen, and make the stream menu popup
  (let ((new-window-height (- (window-height) popup-height)))
    (if (not (> new-window-height 0))
	(error "Current window too small to popup menu!"))
    ;; Save the current window-configuration
    (setq emms-stream-popup-old-conf (current-window-configuration))
    ;; Split and select the menu
    (let ((buffer-down
           (split-window-vertically new-window-height)))
      (select-window buffer-down))

      (kill-buffer (get-buffer-create emms-stream-buffer-name))
      (switch-to-buffer (get-buffer-create emms-stream-buffer-name))
      (erase-buffer)
      (emms-stream-mode)

      (add-hook 'emms-stream-play-hook 'emms-stream-popup-revert)
      (add-hook 'emms-stream-quit-hook 'emms-stream-popup-revert)
      (local-set-key (kbd "TAB") 'emms-stream-popup-revert)
      (local-set-key (kbd "RET") 'emms-stream-play)
      ;; (local-set-key (kbd "q") 'delete-window)
      ;; Also, forget about the whole thing if the user does something
      ;; to the window-configuration
      ;; (add-hook 'window-configuration-change-hook 'emms-stream-popup-forget-conf)))
      ))

(defun emms-stream-init ()
  (setq emms-stream-list (emms-stream-read-file emms-stream-bookmarks-file)))

(defun emms-stream-read-file (file)
  "Returns a sexp."
  (let ((file (expand-file-name file)))
    (if (file-readable-p file)
        (with-temp-buffer
          (emms-insert-file-contents file)
          (goto-char (point-min))
          (read (current-buffer)))
      emms-stream-default-list)))

(defun emms-stream-save-bookmarks-file ()
  (interactive)
  (save-excursion
    (let ((buffer (find-file-noselect emms-stream-bookmarks-file)))
      (set-buffer buffer)
      (erase-buffer)
      (insert "(")
      (let ((firstp t))
        (dolist (stream emms-stream-list)
          (if (not firstp)
              (insert "\n ")
            (setq firstp nil))
          ;; make sure type identifier is a symbol, not a string
          (when (stringp (nth 3 stream))
            (setq stream (copy-alist stream))
            (setcar (nthcdr 3 stream) (intern (nth 3 stream))))
          (prin1 stream buffer)))
      (insert ")\n")
      (save-buffer)
      (kill-buffer buffer)))
  (set-buffer-modified-p nil))

(defun emms-stream-display-line (line)
  (insert (emms-stream-name line))
  (add-text-properties (point-at-bol) (point-at-eol)
                       '(face emms-stream-name-face))
  (add-text-properties (point-at-bol) (point-at-eol) `(emms-stream ,line))
  (insert "\n      ")
  (insert (emms-stream-url  line))
  (add-text-properties (point-at-bol) (point-at-eol)
                       '(face emms-stream-url-face))
  (insert "\n"))

(defun emms-stream-display ()
  "Displays the bookmark list in the current buffer, in a human
  readable way."
  (mapc 'emms-stream-display-line emms-stream-list)
  (goto-char (point-min)))

;; Helper functions
(defun emms-stream-take (n list)
  "Takes N elements from LIST."
  (let ((idx  0)
        (res '()))
    (while (< idx n)
      (setq res (append res (list (nth idx list))))
      (setq idx (+ idx 1)))
    res))

(defun emms-stream-insert-at (n elt list)
  "Inserts the element ELT in LIST, *before* position N.
Positions are counted starting with 0."
  (let* ((n-1     (- n 1))
         (before (emms-stream-take n-1 list))
         (after  (last list (- (length list) n-1))))
    (append before (list elt) after)))

(defun emms-stream-insert-several-at (n new-list list)
  "Inserts the list NEW-LIST in LIST, *before* position N.
Positions are counted starting with 0."
  (let* ((n-1     (- n 1))
         (before (emms-stream-take n-1 list))
         (after  (last list (- (length list) n-1))))
    (append before new-list after)))

(defun emms-stream-look-behind ()
  "Return non-nil if the position behind the point is an emms-stream."
  (and (not (bobp))
       (get-text-property (1- (point)) 'emms-stream)))

(defun emms-stream-back-to-stream ()
  "If we are not on a stream, move backwards to the nearest one."
  (unless (get-text-property (point) 'emms-stream)
    (unless (emms-stream-look-behind)
      (goto-char (or (previous-single-property-change (point) 'emms-stream)
                     (point-min))))
    (goto-char (or (previous-single-property-change (point) 'emms-stream)
                   (point-min)))))

(defun emms-stream-get-bookmark-at-point ()
  "Returns the bookmark under point."
  (emms-stream-back-to-stream)
  (get-text-property (point) 'emms-stream))

(defun emms-stream-redisplay ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (goto-char (point-min))
    (emms-stream-display)))

(defun emms-stream-determine-fd (name)
  "Return a feed descriptor, given NAME.
This is the count of the times NAME appears in the bookmark list,
plus one."
  (let ((count 1))
    (dolist (feed emms-stream-list)
      (when (string= (emms-stream-name feed) name)
        (setq count (1+ count))))
    count))

(defun emms-stream-add-bookmark (name url fd type)
  "Creates a new bookmark, and inserts it at point position.

Don't forget to run `emms-stream-save-bookmarks-file' after !"
  (interactive
   (list
    (read-string "Name of the bookmark: ")
    (read-string "URL: ")
    nil
    (emms-completing-read
     "Type (url, streamlist, or lastfm): "
     (mapcar #'list '("url" "streamlist" "lastfm")))))
  (unless fd (setq fd (emms-stream-determine-fd name)))
  (when (stringp type) (setq type (intern type)))
  (let* ((line     (emms-line-number-at-pos (point)))
         (index    (+ (/ line 2) 1)))
    (setq emms-stream-list (emms-stream-insert-at index (list name url fd type)
                                                  emms-stream-list))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun emms-stream-delete-bookmark ()
  "Deletes the bookmark under the point.

Don't forget to save your modifications !"
  (interactive)
  (let ((line (emms-line-number-at-pos (point))))
    (setq emms-stream-list
          (delete (emms-stream-get-bookmark-at-point) emms-stream-list))
    (emms-stream-redisplay)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun emms-stream-edit-bookmark ()
  "Change the information of current bookmark."
  (interactive)
  (let* ((bookmark (emms-stream-get-bookmark-at-point))
         (name     (read-from-minibuffer "Description: "
                                         (emms-stream-name bookmark)))
         (url      (read-from-minibuffer "URL: "
                                         (emms-stream-url bookmark)))
         (fd       (read-from-minibuffer "Feed Descriptor: "
                                         (int-to-string (emms-stream-fd bookmark))))
         (type     (read-from-minibuffer "Type (url, streamlist, or lastfm): "
                                         (format "%s" (emms-stream-type bookmark)))))
    (emms-stream-delete-bookmark)
    (emms-stream-add-bookmark name url (string-to-number fd) type)))

(defun emms-stream-name (el)
  (car el))
(defun emms-stream-url (el)
  (cadr el))
(defun emms-stream-fd (el)
  (car (cddr el)))
(defun emms-stream-type (el)
  (cadr (cddr el)))

(defun emms-stream-play ()
  (interactive)
  (let* ((line  (or (get-text-property (point) 'emms-stream)
                    (progn
                      (goto-char (or (previous-single-property-change
                                      (point) 'emms-stream)
                                     (point-min)))
                      (goto-char (or (previous-single-property-change
                                      (point) 'emms-stream)
                                     (point-min)))
                      (get-text-property (point) 'emms-stream))
                    (error "No stream found at point")))
         (name  (emms-stream-name line))
         (url   (emms-stream-url  line))
         (fd    (emms-stream-fd   line))
         (type  (emms-stream-type line))
         (player (read (concat "emms-" emms-stream-default-action "-"
                               (format "%s" type)))))
    (setq emms-stream-last-stream line)
;;    (funcall emms-stream-default-action url)
    (funcall player url)
    (if (string= emms-stream-default-action "add")
        (message "URL added to playlist")))
  (later-do 'emms-mode-line-alter)
  (run-hooks 'emms-stream-play-hook))

(defun emms-stream-info-bookmark ()
  "Return the station and track information for the streaming audio station under point."
  (interactive)
  (if (fboundp 'emms-stream-info-message)
      (let* ((line (get-text-property (point) 'emms-stream))
	     (url (emms-stream-url line)))
	(emms-stream-info-message url))
    (message "Streaming media info not available.")))

;; Killing and yanking
(defvar emms-stream-killed-streams ()
  "Bookmarks that have been killed.")

(defun emms-stream-kill-bookmark ()
  "Kill the current bookmark."
  (interactive)
  (let ((stream (emms-stream-get-bookmark-at-point)))
    (setq emms-stream-list (delete stream emms-stream-list)
          emms-stream-killed-streams (cons stream emms-stream-killed-streams)))
  (let ((inhibit-read-only t))
    (kill-line 2)))

(defun emms-stream-yank-bookmark ()
  "Yank bookmark into the streams buffer."
  (interactive)
  (emms-stream-back-to-stream)
  (let ((inhibit-read-only t)
        (streams nil))
    ;; get all valid streams
    (save-restriction
      (narrow-to-region (point) (point))
      (yank)
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (car emms-stream-killed-streams)
                  (looking-at "^\\(.+\\)\n      \\(.+\\)\n"))
        (setq streams (cons (car emms-stream-killed-streams) streams)
              emms-stream-killed-streams (cdr emms-stream-killed-streams))
        (goto-char (match-end 0)))
      (delete-region (point-min) (point-max)))
    ;; insert streams into list
    (if streams
        (let* ((line (emms-line-number-at-pos (point)))
               (index (+ (/ line 2) 1)))
          (setq emms-stream-list (emms-stream-insert-several-at
                                  index streams emms-stream-list))
          (setq line (+ line (* (length streams) 2)))
          (emms-stream-redisplay)
          (goto-char (point-min))
          (forward-line (1- line)))
      (message "Not yanking anything"))))

;; Navigation
(defun emms-stream-next-line ()
  (interactive)
  (when (get-text-property (point) 'emms-stream)
    (goto-char (or (next-single-property-change (point) 'emms-stream)
                   (point-max))))
  (goto-char (or (next-single-property-change (point) 'emms-stream)
                 (point-max)))
  (forward-line 0))

(defun emms-stream-previous-line ()
  (interactive)
  (emms-stream-back-to-stream)
  (goto-char (or (previous-single-property-change (point) 'emms-stream)
                   (point-min)))
  (goto-char (or (previous-single-property-change (point) 'emms-stream)
                 (point-min)))
  (forward-line 0))

(defun emms-stream-quit ()
  (interactive)
  (emms-stream-kill-playlist)
  (kill-this-buffer)
  (run-hooks 'emms-stream-quit-hook))

(defun emms-stream-toggle-default-action ()
"Toggle between adding to the current active playlist or play
right now (and thus erase the current active playlist)."
  (interactive)
  (if (string= emms-stream-default-action "play")
      (progn
        (emms-stream-kill-playlist)
        (setq emms-stream-default-action "add")
        (message "Default action is now add"))
    (emms-stream-create-playlist)
    (setq emms-stream-default-action "play")
    (message "Default action is now play")))

;; info part
; (define-emms-info-method emms-info-url
;    :providep 'emms-info-url-providep
;    :get 'emms-info-url-get)
;;   :set 'emms-info-url-set)

;; A way to get the last element.  it is either the only one, or the
;; last one added by emms-add-url. so in both cases, that's what we
;; want.
;; FIXME : not working with the new design. Yrk ?
; (defun emms-stream-last-element ()
;  (elt emms-playlist (- (length emms-playlist) 1)))

(defun emms-info-url-providep (track)
  (if (eq (emms-track-type track) 'url)
      t
    nil))

; (defun emms-info-url-get (track)
;   (make-emms-info
;    :title (emms-stream-url (emms-track-get track 'metadata))
;    :artist (emms-stream-name (emms-track-get track 'metadata))
;    :album " "
;    :note " "
;    :year " "
;    :genre " "
;    :file (emms-stream-url (emms-track-get track 'metadata))))

;; Then you register it with emms-info, by adding it to
;; `emms-info-methods-list'.

; (add-to-list 'emms-info-methods-list 'emms-info-url)

(defun emms-stream-add-data-to-track (track)
  (emms-track-set track 'metadata emms-stream-last-stream))

(add-to-list 'emms-track-initialize-functions
             'emms-stream-add-data-to-track)

; (when (featurep 'emms-info)
;   (eval-when-compile (require 'emms-info)) ; appease byte-compiler
;   (add-to-list 'emms-info-methods-list 'emms-info-streamlist)
;   (defun emms-info-streamlist-providep (track)
;     (if (eq (emms-track-type track) 'streamlist)
;         t
;       nil))
;   (define-emms-info-method emms-info-streamlist  ;; FIXME-PLS ?
;     :providep 'emms-info-streamlist-providep ;; FIXME-PLS ?
;     :get 'emms-info-url-get))

(provide 'emms-streams)
;;; emms-streams.el ends here

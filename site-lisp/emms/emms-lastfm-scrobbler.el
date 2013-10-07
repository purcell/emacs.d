;;; emms-lastfm-scrobbler.el --- Last.FM Music API

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Authors: Bram van der Kroef <bram@fortfrances.com>, Yoni Rabkin
;; <yonirabkin@member.fsf.org>

;; Keywords: emms, lastfm

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Code:

;;; ------------------------------------------------------------------
;;; Submission API [http://www.last.fm/api/submissions]
;;; ------------------------------------------------------------------

(require 'emms)
(require 'emms-playing-time)
(require 'emms-lastfm-client)

;; Variables referenced from emms-lastfm-client:
;;  emms-lastfm-client-username, emms-lastfm-client-api-key,
;;  emms-lastfm-client-api-secret-key, emms-lastfm-client-api-session-key,
;;   emms-lastfm-client-track
;; Functions referenced:
;;  emms-lastfm-client-xspf-get, emms-lastfm-client-xspf-extension,
;;  emms-lastfm-client-initialize-session

(defcustom emms-lastfm-scrobbler-submit-track-types '(file)
  "Specify what types of tracks to submit to Last.fm.
The default is to only submit files.

To submit every track to Last.fm, set this to t."
  :type '(choice (const :tag "All" t)
		 (set :tag "Types"
		      (const :tag "Files" file)
		      (const :tag "URLs" url)
		      (const :tag "Playlists" playlist)
		      (const :tag "Streamlists" streamlist)
		      (const :tag "Last.fm streams" lastfm-streaming)))
  :group 'emms-lastfm)

(defvar emms-lastfm-scrobbler-submission-protocol-number "1.2.1"
  "Version of the submissions protocol to which Emms conforms.")

(defvar emms-lastfm-scrobbler-published-version "1.0"
  "Version of this package published to the Last.fm service.")

(defvar emms-lastfm-scrobbler-submission-session-id nil
  "Scrobble session id, for now-playing and submission requests.")

(defvar emms-lastfm-scrobbler-submission-now-playing-url nil
  "URL that should be used for a now-playing request.")

(defvar emms-lastfm-scrobbler-submission-url nil
  "URL that should be used for submissions")

(defvar emms-lastfm-scrobbler-client-identifier "emm"
  "Client identifier for Emms (Last.fm define this, not us).")

(defvar emms-lastfm-scrobbler-track-play-start-timestamp nil
  "UTC timestamp.")

;; 1.3 Authentication Token for Web Services Authentication: token =
;; md5(shared_secret + timestamp)

(defun emms-lastfm-scrobbler-make-token-for-web-services (timestamp)
  (when (not (and emms-lastfm-client-api-secret-key timestamp))
    (error "secret and timestamp needed to make an auth token"))
  (md5 (concat emms-lastfm-client-api-secret-key timestamp)))

;; Handshake: The initial negotiation with the submissions server to
;; establish authentication and connection details for the session.

(defun emms-lastfm-scrobbler-handshake ()
  "Make handshake call."
  (let* ((url-request-method "GET"))
    (let ((response
	   (url-retrieve-synchronously
	    (emms-lastfm-scrobbler-make-handshake-call))))
      (emms-lastfm-scrobbler-handle-handshake
       (with-current-buffer response
	 (buffer-substring-no-properties
	  (point-min) (point-max)))))))

(defun emms-lastfm-scrobbler-make-handshake-call ()
  "Return a submission protocol handshake string."
  (when (not (and emms-lastfm-scrobbler-submission-protocol-number
		  emms-lastfm-scrobbler-client-identifier
		  emms-lastfm-scrobbler-published-version
		  emms-lastfm-client-username))
    (error "missing variables to generate handshake call"))
  (let ((timestamp (emms-lastfm-scrobbler-timestamp)))
    (concat
     "http://post.audioscrobbler.com/?hs=true"
     "&p=" emms-lastfm-scrobbler-submission-protocol-number
     "&c=" emms-lastfm-scrobbler-client-identifier
     "&v=" emms-lastfm-scrobbler-published-version
     "&u=" emms-lastfm-client-username
     "&t=" timestamp
     "&a=" (emms-lastfm-scrobbler-make-token-for-web-services timestamp)
     "&api_key=" emms-lastfm-client-api-key
     "&sk=" emms-lastfm-client-api-session-key)))

(defun emms-lastfm-scrobbler-handle-handshake (response)
  (let ((ok200 "HTTP/1.1 200 OK"))
    (when (not (string= ok200 (substring response 0 15)))
      (error "server not responding correctly"))
    (with-temp-buffer
      (insert response)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((status (buffer-substring-no-properties
		     (point-at-bol) (point-at-eol))))
	(cond ((string= status "OK")
	       (forward-line)
	       (setq emms-lastfm-scrobbler-submission-session-id
		     (buffer-substring-no-properties
		      (point-at-bol) (point-at-eol)))
	       (forward-line)
	       (setq emms-lastfm-scrobbler-submission-now-playing-url
		     (buffer-substring-no-properties
		      (point-at-bol) (point-at-eol)))
	       (forward-line)
	       (setq emms-lastfm-scrobbler-submission-url
		     (buffer-substring-no-properties
		      (point-at-bol) (point-at-eol))))
	      ((string= status "BANNED")
	       (error "this version of Emms has been BANNED"))
	      ((string= status "BADAUTH")
	       (error "bad authentication paramaters to handshake"))
	      ((string= status "BADTIME")
	       (error "handshake timestamp diverges too much"))
	      (t
	       (error "unhandled handshake failure")))))))

(defun emms-lastfm-scrobbler-assert-submission-handshake ()
  (when (not (and emms-lastfm-scrobbler-submission-session-id
		  emms-lastfm-scrobbler-submission-now-playing-url
		  emms-lastfm-scrobbler-submission-url))
    (error "cannot use submission API before handshake")))

(defun emms-lastfm-scrobbler-hexify-encode (str)
  "UTF-8 encode and URL-hexify STR."
  (url-hexify-string (encode-coding-string str 'utf-8)))

(defun emms-lastfm-scrobbler-timestamp ()
  "Return a UNIX UTC timestamp."
  (format-time-string "%s"))

(defun emms-lastfm-scrobbler-get-response-status ()
  "Check the http header and return the body"
  (let ((ok200 "HTTP/1.1 200 OK"))
    (if (< (point-max) 1)
	(error "No response from submission server"))
    (if (not (string= ok200 (buffer-substring-no-properties (point-min) 16)))
	(error "submission server not responding correctly"))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (buffer-substring-no-properties
     (point-at-bol) (point-at-eol))))

(defun emms-lastfm-scrobbler-submission-data (track rating)
  "Format the url parameters containing the track artist, title, rating, time the
  track was played, etc."
  ;;  (emms-lastfm-scrobbler-assert-submission-handshake)
  (setq rating
	(cond ((equal 'love rating) "L")
	      ((equal 'ban rating)  "B")
	      ((equal 'skip rating) "S")
	      (t "")))
  (let ((artist (emms-track-get track 'info-artist))
	(title  (emms-track-get track 'info-title))
	(album  (or (emms-track-get track 'info-album) ""))
	(track-number (emms-track-get track 'info-tracknumber))
	(musicbrainz-id "")
	(track-length (number-to-string
		       (or (emms-track-get track
					   'info-playing-time)
			   0))))
    (if (and artist title)
	(concat
	 "s=" (emms-lastfm-scrobbler-hexify-encode
	       emms-lastfm-scrobbler-submission-session-id)
	 "&a[0]=" (emms-lastfm-scrobbler-hexify-encode artist)
	 "&t[0]=" (emms-lastfm-scrobbler-hexify-encode title)
	 "&i[0]=" (emms-lastfm-scrobbler-hexify-encode
		   emms-lastfm-scrobbler-track-play-start-timestamp)
	 "&o[0]=" (if (equal (emms-track-type track)
			     'lastfm-streaming)
		      (concat "L"
			      (emms-lastfm-scrobbler-hexify-encode
			       (emms-lastfm-client-xspf-get
				'trackauth
				(emms-lastfm-client-xspf-extension
				 emms-lastfm-client-track))))
		    "P")
	 "&r[0]=" (emms-lastfm-scrobbler-hexify-encode rating)
	 "&l[0]=" track-length
	 "&b[0]=" (emms-lastfm-scrobbler-hexify-encode album)
	 "&n[0]=" track-number
	 "&m[0]=" musicbrainz-id)
      (error "Track title and artist must be known."))))

(defun emms-lastfm-scrobbler-nowplaying-data (track)
  "Format the parameters for the Now playing submission."
  ;;  (emms-lastfm-scrobbler-assert-submission-handshake)
  (let ((artist (emms-track-get track 'info-artist))
	(title  (emms-track-get track 'info-title))
	(album  (or (emms-track-get track 'info-album) ""))
	(track-number (emms-track-get track
				      'info-tracknumber))
	(musicbrainz-id "")
	(track-length (number-to-string
		       (or (emms-track-get track
					   'info-playing-time)
			   0))))
    (if (and artist title)
	(concat
	 "s=" (emms-lastfm-scrobbler-hexify-encode
	       emms-lastfm-scrobbler-submission-session-id)
	 "&a=" (emms-lastfm-scrobbler-hexify-encode artist)
	 "&t=" (emms-lastfm-scrobbler-hexify-encode title)
	 "&b=" (emms-lastfm-scrobbler-hexify-encode album)
	 "&l=" track-length
	 "&n=" track-number
	 "&m=" musicbrainz-id)
      (error "Track title and artist must be known."))))

(defun emms-lastfm-scrobbler-allowed-track-type (track)
  "Check if the track-type is one of the allowed types"
  (let ((track-type (emms-track-type track)))
    (or (eq emms-lastfm-scrobbler-submit-track-types t)
	(and (listp emms-lastfm-scrobbler-submit-track-types)
	     (memq track-type emms-lastfm-scrobbler-submit-track-types)))))

;;; ------------------------------------------------------------------
;;; EMMS hooks
;;; ------------------------------------------------------------------

(defun emms-lastfm-scrobbler-start-hook ()
  "Update the now playing info displayed on the user's last.fm page.  This
  doesn't affect the user's profile, so it con be done even for tracks that
  should not be submitted."
  ;; wait 5 seconds for the stop hook to submit the last track
  (sit-for 5)
  (let ((current-track (emms-playlist-current-selected-track)))
    (setq emms-lastfm-scrobbler-track-play-start-timestamp
	  (emms-lastfm-scrobbler-timestamp))
    (if (emms-lastfm-scrobbler-allowed-track-type current-track)
	(emms-lastfm-scrobbler-make-async-nowplaying-call
	 current-track))))

(defun emms-lastfm-scrobbler-stop-hook ()
  "Submit the track to last.fm if it has been played for 240
seconds or half the length of the track."
  (let ((current-track (emms-playlist-current-selected-track)))
    (let ((track-length (emms-track-get current-track 'info-playing-time)))
      (when (and track-length
		 (emms-lastfm-scrobbler-allowed-track-type current-track))
	(when (and
	       ;; track must be longer than 30 secs
	       (> track-length 30)
	       ;; track must be played for more than 240 secs or
	       ;;   half the tracks length, whichever comes first.
	       (> emms-playing-time (min 240 (/ track-length 2))))
	  (emms-lastfm-scrobbler-make-async-submission-call
	   current-track nil))))))

(defun emms-lastfm-scrobbler-enable ()
  "Enable the Last.fm scrobbler and submit the tracks EMMS plays
to last.fm"
  (interactive)
  (emms-lastfm-client-initialize-session)
  (if (not emms-lastfm-scrobbler-submission-session-id)
      (emms-lastfm-scrobbler-handshake))
  (add-hook 'emms-player-started-hook
	    'emms-lastfm-scrobbler-start-hook t)
  (add-hook 'emms-player-stopped-hook
	    'emms-lastfm-scrobbler-stop-hook)
  (add-hook 'emms-player-finished-hook
	    'emms-lastfm-scrobbler-stop-hook))

(defun emms-lastfm-scrobbler-disable ()
  "Stop submitting to last.fm"
  (interactive)
  (remove-hook 'emms-player-started-hook
	       'emms-lastfm-scrobbler-start-hook)
  (remove-hook 'emms-player-stopped-hook
	       'emms-lastfm-scrobbler-stop-hook)
  (remove-hook 'emms-player-finished-hook
	       'emms-lastfm-scrobbler-stop-hook))

;;; ------------------------------------------------------------------
;;; Asynchronous Submission
;;; ------------------------------------------------------------------


(defun emms-lastfm-scrobbler-make-async-submission-call (track rating)
  "Make asynchronous submission call."
  (let ((flarb (emms-lastfm-scrobbler-submission-data track rating)))
    (let* ((url-request-method "POST")
	   (url-request-data flarb)
	   (url-request-extra-headers
	    `(("Content-type" . "application/x-www-form-urlencoded"))))
      (url-retrieve emms-lastfm-scrobbler-submission-url
		    #'emms-lastfm-scrobbler-async-submission-callback
		    (list (cons track rating))))))

(defun emms-lastfm-scrobbler-async-submission-callback (status &optional cbargs)
  "Pass response of asynchronous submission call to handler."
  (emms-lastfm-scrobbler-assert-submission-handshake)
  (let ((response (emms-lastfm-scrobbler-get-response-status)))
    ;; From the API docs: This indicates that the
    ;; submission request was accepted for processing. It
    ;; does not mean that the submission was valid, but
    ;; only that the authentication and the form of the
    ;; submission was validated.
    (let ((track (car cbargs)))
      (cond ((string= response "OK")
	     (message "Last.fm: Submitted %s"
		      (emms-track-get track 'info-title)))
	    ((string= response "BADSESSION")
	     (emms-lastfm-scrobbler-handshake)
	     (emms-lastfm-scrobbler-make-async-submission-call (car cbargs) (cdr cbargs)))
	    (t
	     (error "unhandled submission failure"))))))

(defun emms-lastfm-scrobbler-make-async-nowplaying-call (track)
  "Make asynchronous now-playing submission call."
  (emms-lastfm-scrobbler-assert-submission-handshake)
  (let* ((url-request-method "POST")
	 (url-request-data
	  (emms-lastfm-scrobbler-nowplaying-data track))
	 (url-request-extra-headers
	  `(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-retrieve emms-lastfm-scrobbler-submission-now-playing-url
		  #'emms-lastfm-scrobbler-async-nowplaying-callback
		  (list (cons track nil)))))

(defun emms-lastfm-scrobbler-async-nowplaying-callback (status &optional cbargs)
  "Pass response of asynchronous now-playing submission call to handler."
  (let ((response (emms-lastfm-scrobbler-get-response-status)))
    (cond ((string= response "OK") nil)
	  ((string= response "BADSESSION")
	   (emms-lastfm-scrobbler-handshake)
	   (emms-lastfm-scrobbler-make-async-nowplaying-call (car cbargs)))
	  (t
	   (error "unhandled submission failure")))))

(provide 'emms-lastfm-scrobbler)

;;; emms-lastfm-scrobbler.el ends here.

;;; emms-lyrics.el --- Display lyrics synchronically

;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2013 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>
;; Keywords: emms music lyrics

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

;;; Commentary:

;; This package enables you to play music files and display lyrics
;; synchronically! :-) Plus, it provides a `emms-lyrics-mode' for
;; making lyric files.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;             (require 'emms-lyrics)
;;
;; Then either `M-x emms-lyrics-enable' or add (emms-lyrics 1) in
;; your .emacs to enable.

;;; TODO:

;; 1. Maybe the lyric setup should run before `emms-start'.
;; 2. Give a user a chance to choose when finding out multiple lyrics.
;; 3. Search .lrc format lyrics from internet ?

;;; Code:

(require 'emms)
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'time-date)
(require 'emms-url)
(require 'emms-compat)

;;; User Customization

(defgroup emms-lyrics nil
  "Lyrics module for EMMS."
  :group 'emms)

(defcustom emms-lyrics-display-on-modeline t
  "If non-nil, display lyrics on mode line."
  :type 'boolean
  :group 'emms-lyrics)

(defcustom emms-lyrics-display-on-minibuffer nil
  "If non-nil, display lyrics on minibuffer."
  :type 'boolean
  :group 'emms-lyrics)

(defcustom emms-lyrics-display-buffer nil
  "Non-nil will create deciated `emms-lyrics-buffer' to display lyrics."
  :type 'boolean
  :group 'emms-lyrics)

(defcustom emms-lyrics-dir "~/music/lyrics"
  "Local lyrics repository.
`emms-lyrics-find-lyric' will look for lyrics in current directory(i.e.,
same as the music file) and this directory."
  :type 'string
  :group 'emms-lyrics)

(defcustom emms-lyrics-display-format " %s "
  "Format for displaying lyrics."
  :type 'string
  :group 'emms-lyrics)

(defcustom emms-lyrics-coding-system nil
  "Coding system for reading lyrics files.

If all your lyrics use the same coding system, you can set this
variable to that value; else you'd better leave it to nil, and
rely on `prefer-coding-system', `file-coding-system-alist' or
\(info \"(emacs)File Variables\"), sorted by priority
increasingly."
  :type 'coding-system
  :group 'emms-lyrics)

(defcustom emms-lyrics-mode-hook nil
  "Normal hook run after entering Emms Lyric mode."
  :type 'hook
  :group 'emms-lyrics)

(defcustom emms-lyrics-find-lyric-function 'emms-lyrics-find-lyric
  "Function for finding lyric files."
  :type 'symbol
  :group 'emms-lyrics)

(defcustom emms-lyrics-scroll-p t
  "Non-nil value will enable lyrics scrolling on mode line.

Note: Even if this is set to t, it also depends on
`emms-lyrics-display-on-modeline' to be t."
  :type 'boolean
  :group 'emms-lyrics)

(defcustom emms-lyrics-scroll-timer-interval 0.4
  "Interval between scroller timers. The shorter, the faster."
  :type 'number
  :group 'emms-lyrics)


;;; User Interfaces

(defvar emms-lyrics-display-p t
  "If non-nil, will diplay lyrics.")

(defvar emms-lyrics-mode-line-string ""
  "Current lyric.")

(defvar emms-lyrics-buffer nil
  "Buffer to show lyrics.")

;;;###autoload
(defun emms-lyrics-enable ()
  "Enable displaying emms lyrics."
  (interactive)
  (emms-lyrics 1)
  (message "emms lyrics enabled."))

;;;###autoload
(defun emms-lyrics-disable ()
  "Disable displaying emms lyrics."
  (interactive)
  (emms-lyrics -1)
  (message "EMMS lyrics disabled"))

;;;###autoload
(defun emms-lyrics-toggle ()
  "Toggle displaying emms lyrics."
  (interactive)
  (if emms-lyrics-display-p
      (emms-lyrics-disable)
    (emms-lyrics-enable)))

(defun emms-lyrics-toggle-display-on-minibuffer ()
  "Toggle display lyrics on minibbufer."
  (interactive)
  (if emms-lyrics-display-on-minibuffer
      (progn
	(setq emms-lyrics-display-on-minibuffer nil)
	(message "Disable lyrics on minibufer"))
    (setq emms-lyrics-display-on-minibuffer t)
    (message "Enable lyrics on minibufer")))

(defun emms-lyrics-toggle-display-on-modeline ()
  "Toggle display lyrics on mode line."
  (interactive)
  (if emms-lyrics-display-on-modeline
      (progn
	(setq emms-lyrics-display-on-modeline nil
	      emms-lyrics-mode-line-string "")
	(message "Disable lyrics on mode line"))
    (setq emms-lyrics-display-on-modeline t)
    (message "Enable lyrics on mode line")))

(defun emms-lyrics-toggle-display-buffer ()
  "Toggle showing/hiding `emms-lyrics-buffer'."
  (interactive)
  (let ((w (get-buffer-window emms-lyrics-buffer)))
    (if w
        (delete-window w)
      (save-selected-window
        (pop-to-buffer emms-lyrics-buffer)
        (set-window-dedicated-p w t)))))

(defun emms-lyrics (arg)
  "Turn on emms lyrics display if ARG is positive, off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (progn
        (setq emms-lyrics-display-p t)
        (add-hook 'emms-player-started-hook     'emms-lyrics-start)
        (add-hook 'emms-player-stopped-hook     'emms-lyrics-stop)
        (add-hook 'emms-player-finished-hook    'emms-lyrics-stop)
        (add-hook 'emms-player-paused-hook      'emms-lyrics-pause)
        (add-hook 'emms-player-seeked-functions 'emms-lyrics-seek)
        (add-hook 'emms-player-time-set-functions 'emms-lyrics-sync))
    (emms-lyrics-stop)
    (setq emms-lyrics-display-p nil)
    (emms-lyrics-restore-mode-line)
    (remove-hook 'emms-player-started-hook     'emms-lyrics-start)
    (remove-hook 'emms-player-stopped-hook     'emms-lyrics-stop)
    (remove-hook 'emms-player-finished-hook    'emms-lyrics-stop)
    (remove-hook 'emms-player-paused-hook      'emms-lyrics-pause)
    (remove-hook 'emms-player-seeked-functions 'emms-lyrics-seek)
    (remove-hook 'emms-player-time-set-functions 'emms-lyrics-sync)))

(defun emms-lyrics-visit-lyric ()
  "Visit playing track's lyric file.
If we can't find it from local disk, then search it from internet."
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
         (name (emms-track-get track 'name))
         (lrc (funcall emms-lyrics-find-lyric-function
                       (emms-replace-regexp-in-string
                        (concat "\\." (file-name-extension name) "\\'")
                        ".lrc"
                        (file-name-nondirectory name)))))
    (if (and lrc (file-exists-p lrc) (not (string= lrc "")))
        (find-file lrc)
      (message "lyric file does not exist, search it from internet...")
      (let ((title (emms-track-get track 'title))
            (filename (file-name-sans-extension
                       (file-name-nondirectory name)))
            (url ""))
        (unless title
          (setq title filename))
        (cond ((string-match "\\cc" title) ; chinese lyrics
               ;; Since tag info might be encoded using various coding
               ;; systems, we'd better fall back on filename.
               (setq url (format
                          "http://mp3.baidu.com/m?f=ms&rn=10&tn=baidump3lyric&ct=150994944&word=%s&lm=-1"
                          (emms-url-quote-plus
                           (encode-coding-string filename 'gb2312)))))
              (t                        ; english lyrics
               (setq url (format "http://search.lyrics.astraweb.com/?word=%s"
                                 ;;"http://www.lyrics007.com/cgi-bin/s.cgi?q="
                                 (emms-url-quote-plus title)))))
        (browse-url url)
        (message "lyric file does not exist, search it from internet...done")))))


;;; EMMS Lyrics

(defvar emms-lyrics-alist nil
  "a list of the form: '((time0 . lyric0) (time1 . lyric1)...)). In
short, at time-i, display lyric-i.")

(defvar emms-lyrics-timers nil
  "timers for displaying lyric.")

(defvar emms-lyrics-start-time nil
  "emms lyric start time.")

(defvar emms-lyrics-pause-time nil
  "emms lyric pause time.")

(defvar emms-lyrics-elapsed-time 0
  "How long time has emms lyric played.")

(defvar emms-lyrics-scroll-timers nil
  "Lyrics scroller timers.")

(defun emms-lyrics-read-file (file &optional catchup)
  "Read a lyric file(LRC format).
Optional CATCHUP is for recognizing `emms-lyrics-catchup'.
FILE should end up with \".lrc\", its content looks like one of the
following:

    [1:39]I love you, Emacs!
    [00:39]I love you, Emacs!
    [00:39.67]I love you, Emacs!

FILE should be under the same directory as the music file, or under
`emms-lyrics-dir'."
  (or catchup
      (setq file (funcall emms-lyrics-find-lyric-function file)))
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (let ((coding-system-for-read emms-lyrics-coding-system))
        (emms-insert-file-contents file)
        (while (search-forward-regexp "\\[[0-9:.]+\\].*" nil t)
          (let ((lyric-string (match-string 0))
                (time 0)
                (lyric ""))
            (setq lyric
                  (emms-replace-regexp-in-string ".*\\]" "" lyric-string))
            (while (string-match "\\[[0-9:.]+\\]" lyric-string)
              (let* ((time-string (match-string 0 lyric-string))
                     (semi-pos (string-match ":" time-string)))
                (setq time
                      (+ (* (string-to-number
                             (substring time-string 1 semi-pos))
                            60)
                         (string-to-number
                          (substring time-string
                                     (1+ semi-pos)
                                     (1- (length time-string))))))
                (setq lyric-string
                      (substring lyric-string (length time-string)))
                (setq emms-lyrics-alist
                      (append emms-lyrics-alist `((,time . ,lyric))))
                (setq time 0)))))
        (setq emms-lyrics-alist
              (sort emms-lyrics-alist (lambda (a b) (< (car a) (car b))))))
      t)))

(defun emms-lyrics-create-buffer ()
  "Create `emms-lyrics-buffer' dedicated to lyrics. "
  ;; leading white space in buffer name to hide the buffer
  (setq emms-lyrics-buffer (get-buffer-create " *EMMS Lyrics*"))
  (set-buffer emms-lyrics-buffer)
  (setq buffer-read-only nil
        cursor-type nil)
  (erase-buffer)
  (mapc (lambda (time-lyric) (insert (cdr time-lyric) "\n"))
        emms-lyrics-alist)
  (goto-char (point-min))
  (emms-activate-highlighting-mode)
  (setq buffer-read-only t))

(defun emms-lyrics-start ()
  "Start displaying lryics."
  (setq emms-lyrics-start-time (current-time)
	emms-lyrics-pause-time nil
	emms-lyrics-elapsed-time 0)
  (when (let ((file
	       (emms-track-get
		(emms-playlist-current-selected-track)
		'name)))
	  (emms-lyrics-read-file
	   (emms-replace-regexp-in-string
	    (concat "\\." (file-name-extension file) "\\'")
            ".lrc"
            (file-name-nondirectory file))))
    (when emms-lyrics-display-buffer
      (emms-lyrics-create-buffer))
    (emms-lyrics-set-timer)))

(defun emms-lyrics-catchup (lrc)
  "Catchup with later downloaded LRC file(full path).
If you write some lyrics crawler, which is running asynchronically,
then this function would be useful to call when the crawler finishes its
job."
  (let ((old-start emms-lyrics-start-time))
    (setq emms-lyrics-start-time (current-time)
          emms-lyrics-pause-time nil
          emms-lyrics-elapsed-time 0)
    (emms-lyrics-read-file lrc t)
    (emms-lyrics-set-timer)
    (emms-lyrics-seek (float-time (time-since old-start)))))

(defun emms-lyrics-stop ()
  "Stop displaying lyrics."
  (interactive)
  (when emms-lyrics-alist
    (mapc #'emms-cancel-timer emms-lyrics-timers)
    (if (or (not emms-player-paused-p)
	    emms-player-stopped-p)
	(setq emms-lyrics-alist nil
	      emms-lyrics-timers nil
	      emms-lyrics-mode-line-string ""))))

(defun emms-lyrics-pause ()
  "Pause displaying lyrics."
  (if emms-player-paused-p
      (setq emms-lyrics-pause-time (current-time))
    (when emms-lyrics-pause-time
      (setq emms-lyrics-elapsed-time
	    (+ (float-time
		(time-subtract emms-lyrics-pause-time
			       emms-lyrics-start-time))
	       emms-lyrics-elapsed-time)))
    (setq emms-lyrics-start-time (current-time)))
  (when emms-lyrics-alist
    (if emms-player-paused-p
	(emms-lyrics-stop)
      (emms-lyrics-set-timer))))

(defun emms-lyrics-seek (sec)
  "Seek forward or backward SEC seconds lyrics."
  (setq emms-lyrics-elapsed-time
	(+ emms-lyrics-elapsed-time
	   (float-time (time-since emms-lyrics-start-time))
	   sec))
  (when (< emms-lyrics-elapsed-time 0)	; back to start point
    (setq emms-lyrics-elapsed-time 0))
  (setq emms-lyrics-start-time (current-time))
  (when emms-lyrics-alist
    (let ((paused-orig emms-player-paused-p))
      (setq emms-player-paused-p t)
      (emms-lyrics-stop)
      (setq emms-player-paused-p paused-orig))
    (emms-lyrics-set-timer)))

(defun emms-lyrics-sync (sec)
  "Synchronize the lyric display at SEC seconds."
  (setq emms-lyrics-start-time (current-time)
        emms-lyrics-elapsed-time 0)
  (emms-lyrics-seek sec))

(defun emms-lyrics-set-timer ()
  "Set timers for displaying lyrics."
  (setq emms-lyrics-timers '())
  (let ((lyrics-alist emms-lyrics-alist)
        (line 0))
    (while lyrics-alist
      (let ((time (- (caar lyrics-alist) emms-lyrics-elapsed-time))
            (lyric (cdar lyrics-alist))
            (next-time (and (cdr lyrics-alist)
                            (- (car (cadr lyrics-alist))
                               emms-lyrics-elapsed-time)))
            (next-lyric (and (cdr lyrics-alist)
                             (cdr (cadr lyrics-alist)))))
        (setq line (1+ line))
        (when (> time 0)
          (setq emms-lyrics-timers
                (append emms-lyrics-timers
                        (list
                         (run-at-time (format "%d sec" time)
                                      nil
                                      'emms-lyrics-display-handler
                                      lyric
                                      next-lyric
                                      line
                                      (and next-time (- next-time time)))))))
        (setq lyrics-alist (cdr lyrics-alist))))))

(defun emms-lyrics-mode-line ()
  "Add lyric to the mode line."
  (or global-mode-string (setq global-mode-string '("")))
  (unless (member 'emms-lyrics-mode-line-string
		  global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(emms-lyrics-mode-line-string)))))

(defun emms-lyrics-restore-mode-line ()
  "Restore the mode line."
  (setq global-mode-string
	(remove 'emms-lyrics-mode-line-string global-mode-string))
  (force-mode-line-update))

(defun emms-lyrics-display-handler (lyric next-lyric line diff)
  "DIFF is the timestamp differences between current LYRIC and
NEXT-LYRIC; LINE corresponds line number for LYRIC in `emms-lyrics-buffer'."
  (emms-lyrics-display (format emms-lyrics-display-format lyric) line)
  (when (and emms-lyrics-display-on-modeline emms-lyrics-scroll-p)
    (emms-lyrics-scroll lyric next-lyric diff)))

(defun emms-lyrics-display (lyric line)
  "Display LYRIC now.
See `emms-lyrics-display-on-modeline' and
`emms-lyrics-display-on-minibuffer' on how to config where to
display."
  (when emms-lyrics-alist
    (when emms-lyrics-display-on-modeline
      (emms-lyrics-mode-line)
      (setq emms-lyrics-mode-line-string lyric)
      ;; (setq emms-lyrics-mode-line-string ; make it fit scroller width
      ;;       (concat emms-lyrics-mode-line-string
      ;;               (make-string
      ;;                (abs (- emms-lyrics-scroll-width (length lyric)))
      ;;                (string-to-char " "))))
      (force-mode-line-update))

    (when emms-lyrics-display-on-minibuffer
      (unless (minibuffer-window-active-p (selected-window))
        (message lyric)))

    (when emms-lyrics-display-buffer
      (with-current-buffer emms-lyrics-buffer
        (when line
          (goto-char (point-min))
          (forward-line (1- line))
          (emms-line-highlight))))))

(defun emms-lyrics-find-lyric (file)
  "Return full path of found lrc FILE, or nil if not found.
Use `emms-source-file-directory-tree-function' to find lrc FILE under
current directory and `emms-lyrics-dir'.
e.g., (emms-lyrics-find-lyric \"abc.lrc\")"
  (let* ((track (emms-playlist-current-selected-track))
         (lyric-under-curr-dir
          (concat (file-name-directory (emms-track-get track 'name))
                  file)))
    (or (and (eq (emms-track-type track) 'file)
             (file-exists-p lyric-under-curr-dir)
             lyric-under-curr-dir)
        (car (funcall emms-source-file-directory-tree-function
                      emms-lyrics-dir
                      file)))))

;; (setq emms-lyrics-scroll-width 20)

(defun emms-lyrics-scroll (lyric next-lyric diff)
  "Scroll LYRIC to left smoothly in DIFF seconds.
DIFF is the timestamp differences between current LYRIC and
NEXT-LYRIC."
  (setq diff (floor diff))
  (setq emms-lyrics-scroll-timers '())
  (let ((scrolled-lyric (concat lyric " " next-lyric))
        (time 0)
        (pos 0))
    (catch 'return
      (while (< time diff)
        (setq emms-lyrics-scroll-timers
              (append emms-lyrics-scroll-timers
                      (list
                       (run-at-time time
                                    nil
                                    'emms-lyrics-display
                                    (if (>= (length lyric) pos)
                                        (substring scrolled-lyric pos)
                                      (throw 'return t))
                                    nil))))
        (setq time (+ time emms-lyrics-scroll-timer-interval))
        (setq pos (1+ pos))))))


;;; emms-lyrics-mode

(defvar emms-lyrics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'emms-lyrics-previous-line)
    (define-key map "n" 'emms-lyrics-next-line)
    (define-key map "i" 'emms-lyrics-insert-time)
    map)
  "Keymap for `emms-lyrics-mode'.")

(defun emms-lyrics-rem* (x y)
  "The remainder of X divided by Y, with the same sign as X."
  (let* ((q (floor x y))
         (rem (- x (* y q))))
    (if (= rem 0)
        0
      (if (eq (>= x 0) (>= y 0))
          rem
        (- rem y)))))

(defun emms-lyrics-insert-time ()
  "Insert lyric time in the form: [01:23.21], then goto the
beginning of next line."
  (interactive)
  (let* ((total (+ (float-time
		    (time-subtract (current-time)
				   emms-lyrics-start-time))
		   emms-lyrics-elapsed-time))
	 (min (/ (* (floor (/ total 60)) 100) 100))
	 (sec (/ (floor (* (emms-lyrics-rem* total 60) 100)) 100.0)))
    (insert (emms-replace-regexp-in-string
	     " " "0" (format "[%2d:%2d]" min sec))))
  (emms-lyrics-next-line))

(defun emms-lyrics-next-line ()
  "Goto the beginning of next line."
  (interactive)
  (forward-line 1))

(defun emms-lyrics-previous-line ()
  "Goto the beginning of previous line."
  (interactive)
  (forward-line -1))

(define-derived-mode emms-lyrics-mode nil "Emms Lyric"
  "Major mode for creating lyric files.
\\{emms-lyrics-mode-map}"
  (run-hooks 'emms-lyrics-mode-hook))

(provide 'emms-lyrics)

;;; emms-lyrics.el ends here

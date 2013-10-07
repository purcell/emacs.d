;;; emms-browser.el --- a track browser supporting covers and filtering

;; Copyright (C) 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Damien Elmes <emacs@repose.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code allows you to browse the metadata cache and add tracks to
;; your playlist. To be properly useful, you should M-x
;; emms-add-directory-tree to all the files you own at least once so
;; that the cache is fully populated.

;; Usage
;; -------------------------------------------------------------------

;; To use, run (emms-all) and then bind `emms-smart-browse' to a key,
;; like:

;; (global-set-key (kbd "<f2>") 'emms-smart-browse)

;; The 'smart browsing' code attempts to link the browser and playlist
;; windows together, so that closing one will close both. Activating
;; it will toggle between three states:

;; a) both windows displayed, with the browser focused
;; b) focus switched to the playlist window
;; c) the extra window closed, and both buffers buried

;; If you just want access to the browser, try M-x
;; emms-browse-by-TYPE, where TYPE is one of artist, album, composer,
;; genre or year. These commands can also be used while smart browsing to
;; change the browsing category.

;; If you don't want to activate the code with (emms-devel), you can
;; activate it manually with:

;; (require 'emms-browser)

;; Key bindings
;; -------------------------------------------------------------------

;; C-j             emms-browser-add-tracks-and-play
;; RET             emms-browser-add-tracks
;; SPC             emms-browser-toggle-subitems
;; /               emms-isearch-buffer
;; 1               emms-browser-collapse-all
;; 2               emms-browser-expand-to-level-2
;; 3               emms-browser-expand-to-level-3
;; 4               emms-browser-expand-to-level-4
;; <               emms-browser-previous-filter
;; >               emms-browser-next-filter
;; ?               describe-mode
;; C               emms-browser-clear-playlist
;; E               emms-browser-expand-all
;; d               emms-browser-view-in-dired
;; D               emms-browser-delete-files
;; q               emms-browser-bury-buffer
;; r               emms-browser-goto-random
;; n               next-line
;; p               previous-line
;; C-/             emms-playlist-mode-undo
;; <C-return>      emms-browser-add-tracks-and-play
;; <backtab>       emms-browser-prev-non-track
;; <tab>           emms-browser-next-non-track

;; s A             emms-browser-search-by-album
;; s a             emms-browser-search-by-artist
;; s c             emms-browser-search-by-composer
;; s s             emms-browser-search-by-names
;; s t             emms-browser-search-by-title
;; s p             emms-browser-search-by-performer

;; b 1             emms-browse-by-artist
;; b 2             emms-browse-by-album
;; b 3             emms-browse-by-genre
;; b 4             emms-browse-by-year
;; b 5             emms-browse-by-composer
;; b 6             emms-browse-by-performer

;; W a p           emms-browser-lookup-album-on-pitchfork
;; W a w           emms-browser-lookup-album-on-wikipedia

;; W A p           emms-browser-lookup-artist-on-pitchfork
;; W A w           emms-browser-lookup-artist-on-wikipedia

;; W C p           emms-browser-lookup-composer-on-pitchfork
;; W C w           emms-browser-lookup-composer-on-wikipedia

;; W P p           emms-browser-lookup-performer-on-pitchfork
;; W P w           emms-browser-lookup-performer-on-wikipedia

;; Displaying covers
;; -------------------------------------------------------------------

;; The browser will attempt to display cover images if they're
;; available. By default it looks for images cover_small.jpg,
;; cover_med.jpg, etc. Customize emms-browser-covers to use your own
;; covers. Note that you'll probably want to resize your existing
;; covers to particular sizes. Suggested sizes are 100x100 for small,
;; and 200x200 for medium.

;; Also emacs by default will jump around a lot when scrolling a
;; buffer with images. Set the following variables to prevent that:

;;   scroll-up-aggressively 0.0
;;   scroll-down-aggressively 0.0

;; To show a 'no cover' image for albums which don't have a cover, add
;; the following code to your .emacs:

;; (setq emms-browser-default-covers
;;   (list "/path/to/cover_small.jpg" nil nil)

;; (the medium and large images can be set too, if you want)

;; You can download an example 'no cover' image from:
;; http://repose.cx/cover_small.jpg

;; Filtering tracks
;; -------------------------------------------------------------------

;; If you want to display a subset of your collection (such as a
;; directory of 80s music, only avi files, etc), then you can make
;; some filters using code like this:

;; ;; show everything
;; (emms-browser-make-filter "all" 'ignore)

;; ;; Set "all" as the default filter
;; (emms-browser-set-filter (assoc "all" emms-browser-filters))

;; ;; show all files (no streamlists, etc)
;; (emms-browser-make-filter
;;  "all-files" (emms-browser-filter-only-type 'file))

;; ;; show only tracks in one folder
;; (emms-browser-make-filter
;;  "80s" (emms-browser-filter-only-dir "~/Mp3s/80s"))

;; ;; show all tracks played in the last month
;; (emms-browser-make-filter
;;  "last-month" (emms-browser-filter-only-recent 30))

;; After executing the above commands, you can use M-x
;; emms-browser-show-all, emms-browser-show-80s, etc to toggle
;; between different collections. Alternatively you can use '<' and
;; '>' to cycle through the available filters.

;; The second argument to make-filter is a function which returns t if
;; a single track should be filtered. You can write your own filter
;; functions to check the type of a file, etc.

;; Some more examples:

;; ;; show only tracks not played in the last year
;; (emms-browser-make-filter "not-played"
;;  (lambda (track)
;;   (not (funcall (emms-browser-filter-only-recent 365) track))))

;; ;; show all files that are not in the pending directory
;; (emms-browser-make-filter
;;  "all"
;;  (lambda (track)
;;    (or
;;     (funcall (emms-browser-filter-only-type 'file) track)
;;     (not (funcall
;;           (emms-browser-filter-only-dir "~/Media/pending") track)))))

;; Changing tree structure
;; -------------------------------------------------------------------

;; You can change the way the tree is displayed by modifying
;; `emms-browser-next-mapping-type'. The following code displays
;; artist->track instead of artist->album->track when you switch to
;; the 'singles' filter.

;; (defadvice emms-browser-next-mapping-type
;;                                 (after no-album (current-mapping))
;;   (when (eq ad-return-value 'info-album)
;;     (setq ad-return-value 'info-title)))

;; (defun toggle-album-display ()
;;   (if (string= emms-browser-current-filter-name "singles")
;;       (ad-activate 'emms-browser-next-mapping-type)
;;     (ad-deactivate 'emms-browser-next-mapping-type)))

;; (add-hook 'emms-browser-filter-changed-hook 'toggle-album-display)

;; Changing display format
;; -------------------------------------------------------------------

;; Format strings govern the way items are displayed in the browser
;; and playlist. You can customize these if you wish.

;; `emms-browser-default-format' controls the format to use when no
;; other format has been explicitly defined. By default, only track and
;; albums deviate from the default.

;; To customise the format of a particular type, find the name of the
;; field you want to use (eg `info-artist', `info-title', etc), and
;; insert that into emms-browser-<type>-format or
;; emms-browser-playlist-<type>-format. For example, if you wanted to
;; remove track numbers from tracks in both the browser and playlist,
;; you could do:

;; (defvar emms-browser-info-title-format "%i%n")
;; (defvar emms-browser-playlist-info-title-format
;;   emms-browser-info-title-format)

;; The format specifiers available include:

;; %i    indent relative to the current level
;; %n    the value of the item - eg -info-artist might be "pink floyd"
;; %y    the album year
;; %A    the album name
;; %a    the artist name of the track
;; %C    the composer name of the track
;; %p    the performer name of the track
;; %t    the title of the track
;; %T    the track number
;; %cS   a small album cover
;; %cM   a medium album cover
;; %cL   a big album cover

;; Note that if you use track-related items like %t, it will take the
;; data from the first track.

;; Changing display faces
;; -------------------------------------------------------------------

;; The faces used to display the various fields are also customizable.
;; They are in the format emms-browser-<type>-face, where type is one
;; of "year/genre", "artist", "composer", "performer", "album" or
;; "track". Note that faces lack the initial "info-" part. For example,
;; to change the artist face, type
;; M-x customize-face emms-browser-artist-face.

;; Deleting files
;; -------------------------------------------------------------------

;; You can use the browser to delete tracks from your hard disk.
;; Because this is dangerous, it is disabled by default.

;; The following code will delete covers at the same time, and remove
;; parent directories if they're now empty.

;; (defun de-kill-covers-and-parents (dir tracks)
;;   (when (> (length tracks) 1)
;;     ;; if we're not deleting an individual file, delete covers too
;;     (dolist (cover '("cover.jpg"
;;                      "cover_med.jpg"
;;                      "cover_small.jpg"
;;                      "folder.jpg"))
;;       (condition-case nil
;;           (delete-file (concat dir cover))
;;         (error nil)))
;;     ;; try and delete empty parents - we actually do the work of the
;;     ;; calling function here, too
;;     (let (failed)
;;       (while (and (not (string= dir "/"))
;;                   (not failed))
;;         (condition-case nil
;;             (delete-directory dir)
;;           (error (setq failed t)))
;;         (setq dir (file-name-directory (directory-file-name dir)))))))
;; (add-hook 'emms-browser-delete-files-hook 'de-kill-covers-and-parents)

;;; Code:

(require 'emms)
(require 'emms-cache)
(require 'emms-source-file)
(require 'emms-playlist-sort)
(require 'sort)

(eval-when-compile
  (require 'cl))

;; --------------------------------------------------
;; Variables and configuration
;; --------------------------------------------------

(defgroup emms-browser nil
  "*The Emacs Multimedia System browser"
  :prefix "emms-browser-"
  :group 'multimedia
  :group 'applications)

(defcustom emms-browser-default-browse-type
  'info-artist
  "*The default browsing mode."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-make-name-function
  'emms-browser-make-name-standard
  "*A function to make names for entries and subentries.
Overriding this function allows you to customise how various elements
are displayed. It is called with two arguments - track and type."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-get-track-field-function
  'emms-browser-get-track-field-simple
  "*A function to get an element from a track.
Change this to customize the way data is organized in the
browser. For example,
`emms-browser-get-track-field-use-directory-name' uses the
directory name to determine the artist. This means that
soundtracks, compilations and so on don't populate the artist
view with lots of 1-track elements."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-covers
  '("cover_small.jpg" "cover_med.jpg" "cover_large.jpg")
  "*Control how cover images are found.
Can be either a list of small, medium and large images (large
currently not used), a function which takes a directory and one
of the symbols `small', `medium' or `large', and should return a
path to the cover, or nil to turn off cover loading."
  :group 'emms-browser
  :type '(choice list function boolean))

(defcustom emms-browser-default-covers nil
  "*A list of default images to use if a cover isn't found."
  :group 'emms-browser
  :type 'list)

(defcustom emms-browser-comparison-test
  (if (fboundp 'define-hash-table-test)
      'case-fold
    'equal)
  "*A method for comparing entries in the cache.
The default is to compare case-insensitively."
  :group 'emms-browser
  :type 'symbol)

(defcustom emms-browser-track-sort-function
  'emms-sort-natural-order-less-p
  "*How to sort tracks in the browser.
Ues nil for no sorting."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-alpha-sort-function
  'string<
  "*How to sort artists/albums/etc. in the browser.
Use nil for no sorting."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-album-sort-function
  'emms-browser-sort-by-year-or-name
  "*How to sort artists/albums/etc. in the browser.
Use nil for no sorting."
  :group 'emms-browser
  :type 'function)

(defcustom emms-browser-show-display-hook nil
  "*Hooks to run when starting or switching to a browser buffer."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-hide-display-hook nil
  "*Hooks to run when burying or removing a browser buffer."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-tracks-added-hook nil
  "*Hooks to run when tracks are added to the playlist."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-filter-tracks-hook nil
  "*Given a track, return t if the track should be ignored."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-filter-changed-hook nil
  "*Hook run after the filter has changed."
  :group 'emms-browser
  :type 'hook)

(defcustom emms-browser-delete-files-hook nil
  "*Hook run after files have been deleted.
This hook can be used to clean up extra files, such as album covers.
Called once for each directory."
  :group 'emms-browser
  :type 'hook)

(defvar emms-browser-buffer nil
  "The current browser buffer, if any.")

(defvar emms-browser-buffer-name "*EMMS Browser*"
  "The default buffer name.")

(defvar emms-browser-top-level-hash nil
  "The current mapping db, eg. artist -> track.")
(make-variable-buffer-local 'emms-browser-top-level-hash)

(defvar emms-browser-top-level-type nil
  "The current mapping type, eg. 'info-artist.")
(make-variable-buffer-local 'emms-browser-top-level-type)

(defvar emms-browser-current-indent nil
  "Used to override the current indent, for the playlist, etc.")

(defvar emms-browser-current-filter-name nil
  "The name of the current filter in place, if any.")

(defconst emms-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'emms-browser-bury-buffer)
    (define-key map (kbd "/") 'emms-isearch-buffer)
    (define-key map (kbd "r") 'emms-browser-goto-random)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "C") 'emms-browser-clear-playlist)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "C-/") 'emms-playlist-mode-undo)
    (define-key map (kbd "SPC") 'emms-browser-toggle-subitems)
    (define-key map (kbd "RET") 'emms-browser-add-tracks)
    (define-key map (kbd "<C-return>") 'emms-browser-add-tracks-and-play)
    (define-key map (kbd "C-j") 'emms-browser-add-tracks-and-play)
    (define-key map (kbd "<tab>") 'emms-browser-next-non-track)
    (define-key map (kbd "<backtab>") 'emms-browser-prev-non-track)
    (define-key map (kbd "d") 'emms-browser-view-in-dired)
    (define-key map (kbd "D") 'emms-browser-delete-files)
    (define-key map (kbd "E") 'emms-browser-expand-all)
    (define-key map (kbd "1") 'emms-browser-collapse-all)
    (define-key map (kbd "2") 'emms-browser-expand-to-level-2)
    (define-key map (kbd "3") 'emms-browser-expand-to-level-3)
    (define-key map (kbd "4") 'emms-browser-expand-to-level-4)
    (define-key map (kbd "b 1") 'emms-browse-by-artist)
    (define-key map (kbd "b 2") 'emms-browse-by-album)
    (define-key map (kbd "b 3") 'emms-browse-by-genre)
    (define-key map (kbd "b 4") 'emms-browse-by-year)
    (define-key map (kbd "b 5") 'emms-browse-by-composer)
    (define-key map (kbd "b 6") 'emms-browse-by-performer)
    (define-key map (kbd "s a") 'emms-browser-search-by-artist)
    (define-key map (kbd "s c") 'emms-browser-search-by-composer)
    (define-key map (kbd "s p") 'emms-browser-search-by-performer)
    (define-key map (kbd "s A") 'emms-browser-search-by-album)
    (define-key map (kbd "s t") 'emms-browser-search-by-title)
    (define-key map (kbd "s s") 'emms-browser-search-by-names)
    (define-key map (kbd "W A w") 'emms-browser-lookup-artist-on-wikipedia)
    (define-key map (kbd "W A p") 'emms-browser-lookup-artist-on-pitchfork)
    (define-key map (kbd "W C w") 'emms-browser-lookup-composer-on-wikipedia)
    (define-key map (kbd "W C p") 'emms-browser-lookup-composer-on-pitchfork)
    (define-key map (kbd "W P w") 'emms-browser-lookup-performer-on-wikipedia)
    (define-key map (kbd "W P p") 'emms-browser-lookup-performer-on-pitchfork)
    (define-key map (kbd "W a w") 'emms-browser-lookup-album-on-wikipedia)
    (define-key map (kbd "W a p") 'emms-browser-lookup-album-on-pitchfork)
    (define-key map (kbd ">") 'emms-browser-next-filter)
    (define-key map (kbd "<") 'emms-browser-previous-filter)
    map)
  "Keymap for `emms-browser-mode'.")

(defconst emms-browser-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map emms-browser-mode-map)
    (define-key map (kbd "q") 'emms-browser-kill-search)
    map)
  "Keymap for `emms-browser-mode'.")

;; --------------------------------------------------
;; Compatability functions
;; --------------------------------------------------

(eval-and-compile
  (if (fboundp 'with-selected-window)
      (defalias 'emms-browser-with-selected-window 'with-selected-window)
    (defmacro emms-browser-with-selected-window (window &rest body)
      ;; this emulates the behavior introduced earlier, though it
      ;; might be best to do something with `window'
      `(save-selected-window ,body)))
  (put 'emms-browser-with-selected-window 'lisp-indent-function 1)
  (put 'emms-browser-with-selected-window 'edebug-form-spec '(form body))

  (if (fboundp 'run-mode-hooks)
      (defalias 'emms-browser-run-mode-hooks 'run-mode-hooks)
    (defalias 'emms-browser-run-mode-hooks 'run-hooks)))

;; --------------------------------------------------
;; General mode setup
;; --------------------------------------------------

(defun emms-browser ()
  "Launch or switch to the EMMS Browser."
  (interactive)
  (emms-browser-create-or-focus
   emms-browser-default-browse-type))

(defun emms-browser-create-or-focus (type)
  "Create a new browser buffer with BROWSE-FUNC, or switch.
BROWSE-FUNC should fill the buffer with something of interest. An
example function is `emms-browse-by-artist'."
  (let ((buf (emms-browser-get-buffer))
        wind)
    (if buf
        ;; if the buffer is displayed, switch the window instead
        (progn
          (setq wind (get-buffer-window buf))
          (if wind
              (select-window wind)
            (switch-to-buffer buf))
          (emms-browser-run-mode-hooks 'emms-browser-show-display-hook))
      ;; if there's no buffer, create a new window
      (emms-browser-create)
      (emms-browse-by type))))

(defun emms-browser-create ()
  "Create a new emms-browser buffer and start emms-browser-mode."
  (emms-browser-new-buffer)
  (emms-browser-mode)
  (emms-browser-run-mode-hooks 'emms-browser-show-display-hook))

(defun emms-browser-mode (&optional no-update)
  "A major mode for the Emms browser.
\\{emms-browser-mode-map}"
      ;; create a new buffer
  (interactive)

  (use-local-map emms-browser-mode-map)
  (setq major-mode 'emms-browser-mode
        mode-name "Emms-Browser")

  (setq buffer-read-only t)
  (unless no-update
    (setq emms-browser-buffer (current-buffer))))

(defun emms-browser-new-buffer ()
  "Create a new browser buffer, and switch to it."
  (switch-to-buffer (generate-new-buffer
                     emms-browser-buffer-name)))

(defun emms-browser-clear ()
  "Create or switch to a browser buffer, clearing it."
  (let ((buf (emms-browser-get-buffer)))
    (if buf
        (progn
          (switch-to-buffer buf)
          (emms-with-inhibit-read-only-t
           (delete-region (point-min) (point-max))))
      (emms-browser-create))))

(defun emms-browser-get-buffer ()
  "Return the current buffer if it exists, or nil."
  (unless (or (null emms-browser-buffer)
              (not (buffer-live-p emms-browser-buffer)))
    emms-browser-buffer))

(defun emms-browser-ensure-browser-buffer ()
  (unless (eq major-mode 'emms-browser-mode)
    (error "Current buffer is not an emms-browser buffer")))

(defun emms-browser-bury-buffer ()
  "Bury the browser buffer, running hooks."
  (interactive)
  (emms-browser-run-mode-hooks 'emms-browser-hide-display-hook)
  (bury-buffer))

;; --------------------------------------------------
;; Top-level browsing methods - by artist/album/etc
;; --------------------------------------------------

;; Since the number of tracks may be rather large, we use a hash to
;; sort the top level elements into various categories. All
;; subelements will be stored in a bdata alist structure.

(defmacro emms-browser-add-category (name type)
  "Create an interactive function emms-browse-by-NAME."
  (let ((funname (intern (concat "emms-browse-by-" name)))
        (funcdesc (concat "Browse by " name ".")))
  `(defun ,funname ()
     ,funcdesc
     (interactive)
     (emms-browse-by ,type))))

(defun emms-browse-by (type)
  "Render a top level buffer based on TYPE."
  ;; FIXME: assumes we only browse by info-*
  (let* ((name (substring (symbol-name type) 5))
         (modedesc (concat "Browsing by: " name))
         (hash (emms-browser-make-hash-by type)))
    (when emms-browser-current-filter-name
      (setq modedesc (concat modedesc
                             " [" emms-browser-current-filter-name "]")))
    (emms-browser-clear)
    (rename-buffer modedesc)
    (emms-browser-render-hash hash type)
    (setq emms-browser-top-level-hash hash)
    (setq emms-browser-top-level-type type)
    (unless (> (hash-table-count hash) 0)
      (emms-browser-show-empty-cache-message))
    (goto-char (point-min))))

(emms-browser-add-category "artist" 'info-artist)
(emms-browser-add-category "composer" 'info-composer)
(emms-browser-add-category "performer" 'info-performer)
(emms-browser-add-category "album" 'info-album)
(emms-browser-add-category "genre" 'info-genre)
(emms-browser-add-category "year" 'info-year)

(defun emms-browser-get-track-field (track type)
  "Return TYPE from TRACK.
This can be customized to group different artists into one for
compilations, etc."
  (funcall emms-browser-get-track-field-function track type))

(defun emms-browser-get-track-field-simple (track type)
  (emms-track-get track type "misc"))

(defun emms-browser-get-track-field-use-directory-name (track type)
  (if (eq type 'info-artist)
      (emms-browser-get-artist-from-path
       track)
    (emms-track-get track type "misc")))

(defun emms-browser-get-artist-from-path (track)
  (let* ((path (emms-track-get track 'name))
         (dir (file-name-directory path))
         (basedir
          (file-name-nondirectory
           (directory-file-name
            (file-name-directory dir)))))
    (car (split-string basedir " - "))))

(defun emms-browser-make-hash-by (type)
  "Make a hash, mapping with TYPE, eg artist -> tracks."
  (let ((hash (make-hash-table
               :test emms-browser-comparison-test))
        field existing-entry)
    (maphash (lambda (path track)
               (unless (run-hook-with-args-until-success
                        'emms-browser-filter-tracks-hook track)
                 (setq field
                       (emms-browser-get-track-field track type))
                 (when field
                   (setq existing-entry (gethash field hash))
                   (if existing-entry
                       (puthash field (cons track existing-entry) hash)
                     (puthash field (list track) hash)))))
             emms-cache-db)
    hash))

(defun emms-browser-render-hash (db type)
  "Render a mapping (DB) into a browser buffer."
  (maphash (lambda (desc data)
             (emms-browser-insert-top-level-entry desc data type))
           db)
  (emms-with-inhibit-read-only-t
   (let ((sort-fold-case t))
     (sort-lines nil (point-min) (point-max)))))

(defun case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))

(defun case-fold-string-hash (a)
  (sxhash (upcase a)))

(when (fboundp 'define-hash-table-test)
  (define-hash-table-test 'case-fold
    'case-fold-string= 'case-fold-string-hash))

(defun emms-browser-insert-top-level-entry (name tracks type)
  "Insert a single top level entry into the buffer."
  (emms-browser-ensure-browser-buffer)
  (let ((bdata (emms-browser-make-bdata-tree type 1 tracks name)))
    (emms-browser-insert-format bdata)))

(defun emms-browser-show-empty-cache-message ()
  "Display some help if the cache is empty."
  (emms-with-inhibit-read-only-t
   (insert "
Welcome to EMMS.

There are currently no files in the EMMS database.
To browse music, you need to tell EMMS where your
files are.

Try the following commands:

 M-x emms-add-directory-tree:
  Add all music in a directory and its subdirectories.

 M-x emms-add-directory:
  Add all music in a directory

 M-x emms-add-file: Add a single music file.

After you have added some files, wait for EMMS to say
'all track information loaded,' then return to the
browser, and hit 'b 1' to refresh.")))

;; --------------------------------------------------
;; Building a subitem tree
;; --------------------------------------------------

(defun emms-browser-next-mapping-type (current-mapping)
  "Return the next sensible mapping.
Eg. if CURRENT-MAPPING is currently 'info-artist, return 'info-album."
  (cond
   ((eq current-mapping 'info-artist) 'info-album)
   ((eq current-mapping 'info-composer) 'info-album)
   ((eq current-mapping 'info-performer) 'info-album)
   ((eq current-mapping 'info-album) 'info-title)
   ((eq current-mapping 'info-genre) 'info-artist)
   ((eq current-mapping 'info-year) 'info-artist)))

(defun emms-browser-make-bdata-tree (type level tracks name)
  "Build a tree of browser DB elements for tracks."
  (emms-browser-make-bdata
   (emms-browser-make-bdata-tree-recurse
    type level tracks)
   name
   type level))

(defun emms-browser-make-bdata-tree-recurse (type level tracks)
  "Build a tree of alists based on a list of tracks, TRACKS.
For example, if TYPE is 'info-year, return an alist like:
artist1 -> album1 -> *track* 1.."
  (let* ((next-type (emms-browser-next-mapping-type type))
         (next-level (1+ level))
         alist name new-db new-tracks)
    ;; if we're at a leaf, the db data is a list of tracks
    (if (eq type 'info-title)
        tracks
      ;; otherwise, make DBs from the sub elements
      (setq alist
            (emms-browser-make-sorted-alist
             next-type tracks))
      (mapcar (lambda (entry)
                (setq name (emms-browser-make-name
                            entry next-type))
                (setq new-tracks (cdr entry))
                (emms-browser-make-bdata
                 (emms-browser-make-bdata-tree-recurse
                  next-type next-level new-tracks)
                 name next-type next-level))
              alist))))

(defun emms-browser-make-name (entry type)
  "Return a name for ENTRY, used for making a bdata object."
  (let ((key (car entry))
        (track (cadr entry))
        artist title) ;; only the first track
  (cond
   ((eq type 'info-title)
    (setq artist (emms-track-get track 'info-artist))
    (setq title (emms-track-get track 'info-title))
    (if (not (and artist title))
        key
      (concat artist " - " title)))
   (t key))))

(defun emms-browser-track-number (track)
  "Return a string representation of a track number.
The string will end in a space. If no track number is available,
return an empty string."
  (let ((tracknum (emms-track-get track 'info-tracknumber)))
    (if (or (not (stringp tracknum)) (string= tracknum "0"))
        ""
      (concat
       (if (eq (length tracknum) 1)
           (concat "0" tracknum)
         tracknum)))))

(defun emms-browser-disc-number (track)
  "Return a string representation of a track number.
The string will end in a space. If no track number is available,
return an empty string."
  (let ((discnum (emms-track-get track 'info-discnumber)))
    (if (or (not (stringp discnum)) (string= discnum "0"))
        ""
      discnum)))

(defun emms-browser-year-number (track)
  "Return a string representation of a track's year.
This will be in the form '(1998) '."
  (let ((year (emms-track-get track 'info-year)))
    (if (or (not (stringp year)) (string= year "0"))
        ""
      (concat
       "(" year ") "))))

(defun emms-browser-track-duration (track)
  "Return a string representation of a track duration.
If no duration is available, return an empty string."
  (let ((pmin (emms-track-get track 'info-playing-time-min))
        (psec (emms-track-get track 'info-playing-time-sec))
        (ptot (emms-track-get track 'info-playing-time)))
    (cond ((and pmin psec) (format "%02d:%02d" pmin psec))
          (ptot (format  "%02d:%02d" (/ ptot 60) (% ptot 60)))
          (t ""))))

(defun emms-browser-make-bdata (data name type level)
  "Return a browser data item from ALIST.
DATA should be a list of DB items, or a list of tracks.
NAME is a name for the DB item.
TYPE is a category the data is organised by, such as 'info-artist.
LEVEL is the number of the sublevel the db item will be placed in."
  (list (cons 'type type)
        (cons 'level level)
        (cons 'name name)
        (cons 'data data)))

(defun emms-browser-make-alist (type tracks)
  "Make an alist mapping of TYPE -> TRACKS.
Items with no metadata for TYPE will be placed in 'misc'"
  (let (db key existing tracknum)
    (dolist (track tracks)
      (setq key (emms-browser-get-track-field track type))
      (when (eq type 'info-title)
          ;; try and make every track unique
        (setq tracknum (emms-browser-track-number track))
        (if (string= tracknum "")
            (setq key (file-name-nondirectory
                       (emms-track-get track 'name)))
          (setq key (concat tracknum key))))
      (setq existing (assoc key db))
      (if existing
          (setcdr existing (cons track (cdr existing)))
        (push (cons key (list track)) db)))
    ;; sort the entries we've built
    (dolist (item db)
      (setcdr item (nreverse (cdr item))))
    db))

(defun emms-browser-make-sorted-alist (type tracks)
  "Return a sorted alist of TRACKS.
TYPE is the metadata to make the alist by - eg. if it's
'info-artist, an alist of artists will be made."
  (emms-browser-sort-alist
   (emms-browser-make-alist type tracks)
   type))

;; --------------------------------------------------
;; BDATA accessors and predicates
;; --------------------------------------------------

(defun emms-browser-bdata-level (bdata)
  (cdr (assq 'level bdata)))

(defun emms-browser-bdata-name (bdata)
  (cdr (assq 'name bdata)))

(defun emms-browser-bdata-type (bdata)
  (cdr (assq 'type bdata)))

(defun emms-browser-bdata-data (bdata)
  (cdr (assq 'data bdata)))

(defun emms-browser-bdata-p (obj)
  "True if obj is a BDATA object."
  (consp (assq 'data obj)))

;; --------------------------------------------------
;; Sorting expanded entries
;; --------------------------------------------------

(defmacro emms-browser-sort-cadr (sort-func)
  "Return a function to sort an alist using SORT-FUNC.
This sorting predicate will compare the cadr of each entry.
SORT-FUNC should be a playlist sorting predicate like
`emms-playlist-sort-by-natural-order'."
  `(lambda (a b)
     (funcall ,sort-func (cadr a) (cadr b))))

(defmacro emms-browser-sort-car (sort-func)
  "Return a function to sort an alist using SORT-FUNC.
This sorting predicate will compare the car of each entry.
SORT-FUNC should be a playlist sorting predicate like
`emms-playlist-sort-by-natural-order'."
  `(lambda (a b)
     (funcall ,sort-func (car a) (car b))))

(defun emms-browser-sort-by-track (alist)
  "Sort an ALIST by the tracks in each entry.
Uses `emms-browser-track-sort-function'."
  (if emms-browser-track-sort-function
      (sort alist (emms-browser-sort-cadr
                  emms-browser-track-sort-function))
    alist))

(defun emms-browser-sort-by-name (alist)
  "Sort ALIST by keys alphabetically.
Uses `emms-browser-alpha-sort-function'."
  (if emms-browser-alpha-sort-function
      (sort alist (emms-browser-sort-car
                  emms-browser-alpha-sort-function))
    alist))

(defun emms-browser-sort-by-year-or-name (alist)
  "Sort based on year or name."
  (sort alist (emms-browser-sort-cadr
               'emms-browser-sort-by-year-or-name-p)))

(defun emms-browser-sort-by-year-or-name-p (a b)
  ;; FIXME: this is a bit of a hack
  (let ((a-desc (concat
                 (emms-browser-year-number a)
                 (emms-track-get a 'info-album "misc")))
        (b-desc (concat
                 (emms-browser-year-number b)
                 (emms-track-get b 'info-album "misc"))))
    (string< a-desc b-desc)))

(defun emms-browser-sort-alist (alist type)
  "Sort ALIST using the sorting function for TYPE."
  (let ((sort-func
         (cond
          ((or
            (eq type 'info-artist)
            (eq type 'info-composer)
            (eq type 'info-performer)
            (eq type 'info-year)
            (eq type 'info-genre))
           'emms-browser-sort-by-name)
          ((eq type 'info-album)
           emms-browser-album-sort-function)
          ((eq type 'info-title)
           'emms-browser-sort-by-track)
          (t (message "Can't sort unknown mapping!")))))
    (funcall sort-func alist)))

;; --------------------------------------------------
;; Subitem operations on the buffer
;; --------------------------------------------------

(defun emms-browser-bdata-at-point ()
  "Return the bdata object at point.
Includes information at point (such as album name), and metadata."
  (get-text-property (point-at-bol)
                     'emms-browser-bdata))

(defun emms-browser-data-at-point ()
  "Return the data stored under point.
This will be a list of DB items."
  (emms-browser-bdata-data (emms-browser-bdata-at-point)))

(defun emms-browser-level-at-point ()
  "Return the current level at point."
  (emms-browser-bdata-level (emms-browser-bdata-at-point)))

(defun emms-browser-tracks-at-point (&optional node)
  "Return a list of tracks at point."
  (let (tracks)
    (dolist (node (if node
                      node
                    (emms-browser-data-at-point)))
      (if (not (emms-browser-bdata-p node))
          (setq tracks (cons node tracks))
        (setq tracks
              (append tracks
                      (emms-browser-tracks-at-point
                       (emms-browser-bdata-data node))))))
    tracks))

(defun emms-browser-expand-one-level ()
  "Expand the current line by one sublevel."
  (interactive)
  (let* ((data (emms-browser-data-at-point)))
    (save-excursion
      (forward-line 1)
      (beginning-of-line)
      (dolist (data-item data)
        (emms-browser-insert-data-item data-item)))))

(defun emms-browser-insert-data-item (data-item)
  "Insert DATA-ITEM into the buffer.
This checks DATA-ITEM's level to determine how much to indent.
The line will have a property emms-browser-bdata storing subitem
information."
  (emms-browser-insert-format data-item))

(defun emms-browser-find-entry-more-than-level (level)
  "Move point to next entry more than LEVEL and return point.
If no entry exits, return nil.
Returns point if currently on a an entry more than LEVEL."
  (let ((old-pos (point))
        level-at-point)
    (forward-line 1)
    (setq level-at-point (emms-browser-level-at-point))
    (if (and level-at-point
             (> level-at-point level))
        (point)
      (goto-char old-pos)
      nil)))

(defun emms-browser-subitems-visible ()
  "True if there are any subentries visible point."
  (let ((current-level (emms-browser-level-at-point))
        new-level)
    (save-excursion
      (re-search-forward "\n" nil t)
      (when (setq new-level (emms-browser-level-at-point))
        (> new-level current-level)))))

(defun emms-browser-subitems-exist ()
  "True if it's possible to expand the current line."
  (not (eq (emms-browser-bdata-type
            (emms-browser-bdata-at-point))
           'info-title)))

(defun emms-browser-move-up-level (&optional direction)
  "Move up one level if possible.
Return true if we were able to move up.
If DIRECTION is 1, move forward, otherwise move backwards."
  (let ((moved nil)
        (continue t)
        (current-level (emms-browser-level-at-point)))
    (while (and
            continue
            (zerop (forward-line
                    (or direction -1))))
      (when (> current-level (emms-browser-level-at-point))
        (setq moved t)
        (setq continue nil)))
    moved))

(defun emms-browser-toggle-subitems ()
  "Show or hide (kill) subitems under the current line."
  (interactive)
  (if (emms-browser-subitems-visible)
      (emms-browser-kill-subitems)
    (if (emms-browser-subitems-exist)
        (emms-browser-show-subitems)
      (assert (emms-browser-move-up-level))
      (emms-browser-kill-subitems))))

(defun emms-browser-show-subitems ()
  "Show subitems under the current line."
  (unless (emms-browser-subitems-visible)
    (if (emms-browser-subitems-exist)
        (emms-browser-expand-one-level))))

(defun emms-browser-kill-subitems ()
  "Remove all subitems under the current line.
Stops at the next line at the same level, or EOF."
  (when (emms-browser-subitems-visible)
    (let ((current-level (emms-browser-level-at-point))
          (next-line (point-at-bol 2)))
      (emms-with-inhibit-read-only-t
       (delete-region next-line
                      (save-excursion
                        (while
                            (emms-browser-find-entry-more-than-level
                             current-level))
                        (point-at-bol 2)))))))

;; --------------------------------------------------
;; Dealing with the playlist (queuing songs, etc)
;; --------------------------------------------------

(defun emms-browser-playlist-insert-group (bdata)
  "Insert a group description into the playlist buffer."
  (let* ((type (emms-browser-bdata-type bdata))
         (short-type (substring (symbol-name type) 5))
         (name (emms-browser-format-line bdata 'playlist)))
    (with-current-emms-playlist
      (goto-char (point-max))
      (insert name "\n"))))

(defun emms-browser-playlist-insert-track (bdata)
  "Insert a track into the playlist buffer."
  (let ((name (emms-browser-format-line bdata 'playlist))
        (track (car (emms-browser-bdata-data bdata))))
    (with-current-emms-playlist
      (goto-char (point-max))
      (insert name "\n"))))

(defun emms-browser-playlist-insert-bdata (bdata starting-level)
  "Add all tracks in BDATA to the playlist."
  (let ((type (emms-browser-bdata-type bdata))
        (name (emms-browser-bdata-name bdata))
        (level (emms-browser-bdata-level bdata))
        emms-browser-current-indent)

    ;; adjust the indentation relative to the starting level
    (when starting-level
      (setq level (- level (1- starting-level))))
    ;; we temporarily rebind the current indent to the relative indent
    (setq emms-browser-current-indent
          (emms-browser-make-indent level))

    ;; add a group heading?
    (unless (eq type 'info-title)
      (emms-browser-playlist-insert-group bdata))

    ;; recurse or add tracks
    (dolist (item (emms-browser-bdata-data bdata))
      (if (not (eq type 'info-title))
          (emms-browser-playlist-insert-bdata item starting-level)
        (emms-browser-playlist-insert-track bdata)))))

;; --------------------------------------------------
;; Expanding/contracting
;; --------------------------------------------------

(defun emms-browser-expand-to-level (level)
  "Expand to a depth specified by LEVEL.
After expanding, jump to the currently marked entry."
  (goto-char (point-min))
  (while (not (eq (buffer-end 1) (point)))
    (if (< (emms-browser-level-at-point) level)
        (emms-browser-show-subitems))
    (emms-browser-next-non-track))
  (emms-browser-pop-mark)
  (recenter '(4)))

(defun emms-browser-mark-and-collapse ()
  "Save the current top level element, and collapse."
  (emms-browser-mark-entry)
  (goto-char (point-max))
  (while (not (eq (buffer-end -1) (point)))
    (emms-browser-prev-non-track)
    (emms-browser-kill-subitems)))

(defun emms-browser-find-top-level ()
  "Move up until reaching a top-level element."
  (while (not (eq (emms-browser-level-at-point) 1))
    (forward-line -1)))

(defun emms-browser-mark-entry ()
  "Mark the current top level entry."
  (save-excursion
    (emms-browser-find-top-level)
    (emms-with-inhibit-read-only-t
     (add-text-properties (point-at-bol)
                          (point-at-eol)
                          (list 'emms-browser-mark t)))))

(defun emms-browser-pop-mark ()
  "Return to the last marked entry, and remove the mark."
  (goto-char (point-min))
  (let ((pos (text-property-any (point-min) (point-max)
                                'emms-browser-mark t)))
    (if pos
        (progn
          (goto-char pos)
          (emms-with-inhibit-read-only-t
           (remove-text-properties (point-at-bol)
                                   (point-at-eol)
                                   (list 'emms-browser-mark))))
      (message "No mark saved!"))))

(defun emms-browser-go-to-parent ()
  "Move point to the parent of the current node.
Return point. If at level one, return the current point."
  (let ((current-level (emms-browser-level-at-point)))
    (unless (eq current-level 1)
      (while (<= current-level (emms-browser-level-at-point))
        (forward-line -1)))
    (point)))

(defun emms-browser-delete-current-node ()
  "Remove the current node, and empty parents."
  ;; set the data to empty
  (setcdr (assq 'data (emms-browser-bdata-at-point)) nil)
  (emms-browser-delete-node-if-empty))

(defun emms-browser-delete-node-if-empty ()
  "If empty, remove node and empty parents."
  (when (zerop (length (emms-browser-data-at-point)))
    (save-excursion
      (let ((child-bdata (emms-browser-bdata-at-point))
            parent-bdata parent-point)
        ;; record the parent's position before we delete anything
        (save-excursion
          (setq parent-point (emms-browser-go-to-parent)))
        ;; delete the current line
        (when (emms-browser-subitems-visible)
          (emms-browser-kill-subitems))
        (emms-with-inhibit-read-only-t
         (goto-char (point-at-bol))
         (kill-line 1))
        (unless (eq (emms-browser-bdata-level child-bdata) 1)
          ;; remove the node from the parent, and recurse
          (goto-char parent-point)
          (setq parent-bdata (emms-browser-bdata-at-point))
          (setcdr (assq 'data parent-bdata)
                  (delq child-bdata
                        (emms-browser-bdata-data parent-bdata)))
          (emms-browser-delete-node-if-empty))))))

;; --------------------------------------------------
;; User-visible commands
;; --------------------------------------------------

(defun emms-browser-add-tracks ()
  "Add all tracks at point.
Return the previous point-max before adding."
  (interactive)
  (let ((first-new-track (with-current-emms-playlist (point-max)))
        (bdata (emms-browser-bdata-at-point)))
    (emms-browser-playlist-insert-bdata
     bdata (emms-browser-bdata-level bdata))
    (run-hook-with-args 'emms-browser-tracks-added-hook
                        first-new-track)
    first-new-track))

(defun emms-browser-add-tracks-and-play ()
  "Add all tracks at point, and play the first added track."
  (interactive)
  (let ((old-pos (emms-browser-add-tracks)))
    (with-current-emms-playlist
      (goto-char old-pos)
      ;; if we're sitting on a group name, move forward
      (unless (emms-playlist-track-at (point))
        (emms-playlist-next))
      (emms-playlist-select (point)))
    ;; FIXME: is there a better way of doing this?
    (emms-stop)
    (emms-start)))

(defun emms-isearch-buffer ()
  "Isearch through the buffer."
  (interactive)
  (goto-char (point-min))
  (when (isearch-forward)
    (unless (emms-browser-subitems-visible)
      (emms-browser-show-subitems))))

(defun emms-browser-next-non-track (&optional direction)
  "Jump to the next non-track element."
  (interactive)
  (let ((continue t))
    (while (and continue
                (forward-line (or direction 1)))
      (unless (eq (emms-browser-bdata-type
                   (emms-browser-bdata-at-point)) 'info-title)
        (setq continue)))))

(defun emms-browser-prev-non-track ()
  "Jump to the previous non-track element."
  (interactive)
  (emms-browser-next-non-track -1))

(defun emms-browser-expand-all ()
  "Expand everything."
  (interactive)
  (emms-browser-expand-to-level 99))

(defun emms-browser-expand-to-level-2 ()
  "Expand all top level items one level."
  (interactive)
  (emms-browser-mark-and-collapse)
  (emms-browser-expand-to-level 2))

(defun emms-browser-expand-to-level-3 ()
  "Expand all top level items two levels."
  (interactive)
  (emms-browser-mark-and-collapse)
  (emms-browser-expand-to-level 3))

(defun emms-browser-expand-to-level-4 ()
  "Expand all top level items three levels."
  (interactive)
  (emms-browser-mark-and-collapse)
  (emms-browser-expand-to-level 4))

(defun emms-browser-collapse-all ()
  "Collapse everything, saving and restoring the mark."
  (interactive)
  (emms-browser-mark-and-collapse)
  (emms-browser-pop-mark)
  (recenter '(4)))

(defvar emms-browser-seed-pending t
  "Do we need to seed (random)?")

(defun emms-browser-goto-random ()
  (interactive)
  (when emms-browser-seed-pending
    (random t)
    (setq emms-browser-seed-pending nil))
  (goto-char (point-min))
  (forward-line (1- (random (count-lines (point-min) (point-max))))))

(defun emms-browser-view-in-dired (&optional bdata)
  "View the current directory in dired."
  ;; FIXME: currently just grabs the directory from the first track
  (interactive)
  (if bdata
      (if (eq (emms-browser-bdata-type bdata) 'info-title)
          (let* ((track (car (emms-browser-bdata-data bdata)))
                 (path (emms-track-get track 'name))
                 (dir (file-name-directory path)))
            (find-file dir))
        (emms-browser-view-in-dired (car (emms-browser-bdata-data bdata))))
    (emms-browser-view-in-dired (emms-browser-bdata-at-point))))

(defun emms-browser-delete-files ()
  "Delete all files under point.
Disabled by default."
  (interactive)
  (let ((tracks (emms-browser-tracks-at-point))
        dirs path)
    (unless (yes-or-no-p
             (format "Really permanently delete these %d tracks? "
                     (length tracks)))
      (error "Cancelled!"))
    (message "Deleting files..")
    (dolist (track tracks)
      (setq path (emms-track-get track 'name))
      (delete-file path)
      (add-to-list 'dirs (file-name-directory path))
      (emms-cache-del path))
    ;; remove empty dirs
    (dolist (dir dirs)
      (run-hook-with-args 'emms-browser-delete-files-hook dir tracks)
      (condition-case nil
          (delete-directory dir)
        (error nil)))
    ;; remove the item from the browser
    (emms-browser-delete-current-node)
    (message "Deleting files..done")))

(put 'emms-browser-delete-files 'disabled t)

(defun emms-browser-clear-playlist ()
  (interactive)
  (with-current-emms-playlist
    (emms-playlist-clear)))

(defun emms-browser-lookup (field url)
  (let ((data
         (emms-track-get (emms-browser-bdata-first-track
                          (emms-browser-bdata-at-point))
                         field)))
    (when data
      (browse-url
       (concat url data)))))

(defun emms-browser-lookup-wikipedia (field)
  (emms-browser-lookup
   field "http://en.wikipedia.org/wiki/Special:Search?search="))

(defun emms-browser-lookup-pitchfork (field)
  (emms-browser-lookup
   field "http://www.pitchforkmedia.com/search/record_reviews/query?query[keywords]="))

(defun emms-browser-lookup-artist-on-wikipedia ()
  (interactive)
  (emms-browser-lookup-wikipedia 'info-artist))

(defun emms-browser-lookup-composer-on-wikipedia ()
  (interactive)
  (emms-browser-lookup-wikipedia 'info-composer))

(defun emms-browser-lookup-performer-on-wikipedia ()
  (interactive)
  (emms-browser-lookup-wikipedia 'info-performer))

(defun emms-browser-lookup-album-on-wikipedia ()
  (interactive)
  (emms-browser-lookup-wikipedia 'info-album))

(defun emms-browser-lookup-artist-on-pitchfork ()
  (interactive)
  (emms-browser-lookup-pitchfork 'info-artist))

(defun emms-browser-lookup-composer-on-pitchfork ()
  (interactive)
  (emms-browser-lookup-pitchfork 'info-composer))

(defun emms-browser-lookup-performer-on-pitchfork ()
  (interactive)
  (emms-browser-lookup-pitchfork 'info-performer))

(defun emms-browser-lookup-album-on-pitchfork ()
  (interactive)
  (emms-browser-lookup-pitchfork 'info-album))

;; --------------------------------------------------
;; Linked browser and playlist windows
;; --------------------------------------------------

(defcustom emms-browser-switch-to-playlist-on-add
  nil
  "Whether to switch to to the playlist after adding files."
  :group 'emms-browser
  :type 'boolean)

(defun emms-smart-browse ()
  "Display browser and playlist.
Toggle between selecting browser, playlist or hiding both. Tries
to behave sanely if the user has manually changed the window
configuration."
  (interactive)
  (add-to-list 'emms-browser-show-display-hook
               'emms-browser-display-playlist)
  (add-to-list 'emms-browser-hide-display-hook
               'emms-browser-hide-linked-window)
  ;; switch to the playlist window when adding tracks?
  (add-to-list 'emms-browser-tracks-added-hook
               (lambda (start-of-tracks) (interactive)
                 (let (playlist-window)
                   (when emms-browser-switch-to-playlist-on-add
                     (emms-smart-browse))
                   ;; center on the first added track/group name
                   (when
                       (setq playlist-window
                             (emms-browser-get-linked-window))
                     (emms-browser-with-selected-window
                         playlist-window
                       (goto-char start-of-tracks)
                       (recenter '(4)))))))
  (let (wind buf)
  (cond
   ((eq major-mode 'emms-browser-mode)
    (setq buf (emms-browser-get-linked-buffer))
    (setq wind (emms-browser-get-linked-window))
    ;; if the playlist window is visible, select it
    (if wind
        (select-window wind)
      ;; otherwise display and select it
      (select-window (emms-browser-display-playlist))))
   ((eq major-mode 'emms-playlist-mode)
    (setq wind (emms-browser-get-linked-window))
    ;; if the playlist window is selected, and the browser is visible,
    ;; hide both
    (if wind
        (progn
          (select-window wind)
          (emms-browser-bury-buffer))
      ;; otherwise bury both
      (bury-buffer)
      (emms-browser-hide-linked-window)))
   (t
    ;; show both
    (emms-browser)))))

(defun emms-browser-get-linked-buffer ()
  "Return linked buffer (eg browser if playlist is selected."
  (cond
   ((eq major-mode 'emms-browser-mode)
    (car (emms-playlist-buffer-list)))
   ((eq major-mode 'emms-playlist-mode)
    emms-browser-buffer)))

(defun emms-browser-get-linked-window ()
  "Return linked window (eg browser if playlist is selected."
  (let ((buf (emms-browser-get-linked-buffer)))
    (when buf
      (get-buffer-window buf))))

(defun emms-browser-display-playlist ()
  "A hook to show the playlist when the browser is displayed.
Returns the playlist window."
  (interactive)
  (let ((pbuf (emms-browser-get-linked-buffer))
        (pwin (emms-browser-get-linked-window)))
    ;; if the window isn't alive..
    (unless (window-live-p pwin)
      (save-selected-window
        (split-window-horizontally)
        (other-window 1)
        (if pbuf
            (switch-to-buffer pbuf)
          ;; there's no playlist - create one
          (setq pbuf (emms-playlist-current-clear))
          (switch-to-buffer pbuf))
        ;; make q in the playlist window hide the linked browser
        (when (boundp 'emms-playlist-mode-map)
          (define-key emms-playlist-mode-map (kbd "q")
            (lambda ()
              (interactive)
              (emms-browser-hide-linked-window)
              (bury-buffer))))
        (setq pwin (get-buffer-window pbuf))))
    pwin))

(defun emms-browser-hide-linked-window ()
  "Delete a playlist or browser window when the other is hidden."
  (interactive)
  (let ((other-buf (emms-browser-get-linked-buffer))
        (other-win (emms-browser-get-linked-window)))
    (when (and other-win
               (window-live-p other-win))
      (delete-window other-win))
    ;; bury the buffer, or it becomes visible when we hide the
    ;; linked buffer
    (bury-buffer other-buf)))

;; --------------------------------------------------
;; Searching
;; --------------------------------------------------

(defun emms-browser-filter-cache (search-list)
  "Return a list of tracks that match SEARCH-LIST.
SEARCH-LIST is a list of cons pairs, in the form:

  ((field1 field2) string)

If string matches any of the fields in a cons pair, it will be
included."

  (let (tracks)
    (maphash (lambda (k track)
               (when (emms-browser-matches-p track search-list)
                 (push track tracks)))
             emms-cache-db)
    tracks))

(defun emms-browser-matches-p (track search-list)
  (let (no-match matched)
    (dolist (item search-list)
      (setq matched nil)
      (dolist (field (car item))
        (let ((track-field (emms-track-get track field "")))
          (when (and track-field (string-match (cadr item) track-field))
            (setq matched t))))
      (unless matched
        (setq no-match t)))
    (not no-match)))

(defun emms-browser-search-buffer-go ()
  "Create a new search buffer, or clean the existing one."
  (switch-to-buffer
   (get-buffer-create "*emms-browser-search*"))
  (emms-browser-mode t)
  (use-local-map emms-browser-search-mode-map)
  (emms-with-inhibit-read-only-t
   (delete-region (point-min) (point-max))))

(defun emms-browser-search (fields)
  "Search for STR using FIELDS."
  (let* ((prompt (format "Searching with %S: " fields))
         (str (read-string prompt)))
    (emms-browser-search-buffer-go)
    (emms-with-inhibit-read-only-t
     (emms-browser-render-search
      (emms-browser-filter-cache
       (list (list fields str)))))
    (emms-browser-expand-all)
    (goto-char (point-min))))

(defun emms-browser-render-search (tracks)
  (let ((entries
         (emms-browser-make-sorted-alist 'info-artist tracks)))
    (dolist (entry entries)
      (emms-browser-insert-top-level-entry (car entry)
                                           (cdr entry)
                                           'info-artist))))

;; hmm - should we be doing this?
(defun emms-browser-kill-search ()
  "Kill the buffer when q is hit."
  (interactive)
  (kill-buffer (current-buffer)))

(defun emms-browser-search-by-artist ()
  (interactive)
  (emms-browser-search '(info-artist)))

(defun emms-browser-search-by-composer ()
  (interactive)
  (emms-browser-search '(info-composer)))

(defun emms-browser-search-by-performer ()
  (interactive)
  (emms-browser-search '(info-performer)))

(defun emms-browser-search-by-title ()
  (interactive)
  (emms-browser-search '(info-title)))

(defun emms-browser-search-by-album ()
  (interactive)
  (emms-browser-search '(info-album)))

(defun emms-browser-search-by-names ()
  (interactive)
  (emms-browser-search '(info-artist info-composer info-performer info-title info-album)))

;; --------------------------------------------------
;; Album covers
;; --------------------------------------------------

(defun emms-browser-get-cover-from-album (bdata &optional size)
  (assert (eq (emms-browser-bdata-type bdata) 'info-album))
  (let* ((track1data (emms-browser-bdata-data bdata))
         (track1 (car (emms-browser-bdata-data (car track1data))))
         (path (emms-track-get track1 'name)))
    (emms-browser-get-cover-from-path path size)))

(defun emms-browser-get-cover-from-path (path &optional size)
  "Return a cover filename, if it exists."
  (unless size
    (setq size 'medium))
  (let* ((size-idx (cond
                    ((eq size 'small) 0)
                    ((eq size 'medium) 1)
                    ((eq size 'large) 2)))
         (cover
          (cond
           ((functionp emms-browser-covers)
            (funcall emms-browser-covers (file-name-directory path) size))
           ((and (listp emms-browser-covers)
                 (nth size-idx emms-browser-covers))
            (concat (file-name-directory path)
                    (nth size-idx emms-browser-covers))))))
    (if (and cover
             (file-readable-p cover))
        cover
      ;; no cover found, use default
      (when emms-browser-default-covers
        (nth size-idx emms-browser-default-covers)))))

(defun emms-browser-insert-cover (path)
  (insert (emms-browser-make-cover path)))

(defun emms-browser-make-cover (path)
  (let* ((ext (file-name-extension path))
         (type (cond
                ((string= ext "png")   'png)
                ((string= ext "xbm")   'xbm)
                ((string= ext "xpm")   'xpm)
                ((string= ext "pbm")   'pbm)
                ((string-match "e?ps"
                               ext)    'postscript)
                ((string= ext "gif")   'gif)
                ((string= ext "tiff")  'tiff)
                (t                     'jpeg))))
    (emms-propertize " "
                     'display `(image
                                :type ,type
                                :margin 5
                                :file ,path)
                     'rear-nonsticky '(display))))

(defun emms-browser-get-cover-str (path size)
  (let ((cover (emms-browser-get-cover-from-path path size)))
    (if cover
        (emms-browser-make-cover cover)
      ;; we use a single space so that cover & no cover tracks line up
      ;; in a terminal
      " ")))

;; --------------------------------------------------
;; Display formats
;; --------------------------------------------------

(defun emms-browser-bdata-first-track (bdata)
  "Return the first track from a given bdata.
If > album level, most of the track data will not make sense."
  (let ((type (emms-browser-bdata-type bdata)))
    (if (eq type 'info-title)
        (car (emms-browser-bdata-data bdata))
      ;; recurse
      (emms-browser-bdata-first-track
       (car (emms-browser-bdata-data bdata))))))

(defun emms-browser-insert-format (bdata)
  (emms-with-inhibit-read-only-t
   (insert
    (emms-browser-format-line bdata)
    "\n")))

(defun emms-browser-make-indent (level)
  (or
   emms-browser-current-indent
   (make-string (* 1 (1- level)) ?\s)))

(defun emms-browser-format-elem (format-string elem)
  (cdr (assoc elem format-string)))

(defun emms-browser-format-line (bdata &optional target)
  "Return a propertized string to be inserted in the buffer."
  (unless target
    (setq target 'browser))
  (let* ((name (or (emms-browser-bdata-name bdata) "misc"))
         (level (emms-browser-bdata-level bdata))
         (type (emms-browser-bdata-type bdata))
         (indent (emms-browser-make-indent level))
         (track (emms-browser-bdata-first-track bdata))
         (path (emms-track-get track 'name))
         (face (emms-browser-get-face bdata))
         (format (emms-browser-get-format bdata target))
         (props (list 'emms-browser-bdata bdata))
         (format-choices
          `(("i" . ,indent)
            ("n" . ,name)
            ("y" . ,(emms-track-get track 'info-year))
            ("A" . ,(emms-track-get track 'info-album))
            ("a" . ,(emms-track-get track 'info-artist))
            ("C" . ,(emms-track-get track 'info-composer))
            ("p" . ,(emms-track-get track 'info-performer))
            ("t" . ,(emms-track-get track 'info-title))
	    ("D" . ,(emms-browser-disc-number track))
            ("T" . ,(emms-browser-track-number track))
            ("d" . ,(emms-browser-track-duration track))
            ("cS" . ,(emms-browser-get-cover-str path 'small))
            ("cM" . ,(emms-browser-get-cover-str path 'medium))
            ("cL" . ,(emms-browser-get-cover-str path 'large))))
         str)

    (when (functionp format)
      (setq format (funcall format bdata format-choices)))

    (setq str
          (with-temp-buffer
            (insert format)
            (goto-char (point-min))
            (let ((start (point-min)))
              ;; jump over any image
              (when (re-search-forward "%c[SML]" nil t)
                (setq start (point)))
              ;; jump over the indent
              (when (re-search-forward "%i" nil t)
                (setq start (point)))
              (add-text-properties start (point-max)
                                   (list 'face face)))
            (buffer-string)))

    (setq str (emms-browser-format-spec str format-choices))

    ;; give tracks a 'boost' if they're not top-level
    ;; (covers take up an extra space)
    (when (and (eq type 'info-title)
               (not (string= indent "")))
      (setq str (concat " " str)))

    ;; if we're in playlist mode, add a track
    (when (and (eq target 'playlist)
               (eq type 'info-title))
      (setq props
            (append props `(emms-track ,track))))

    ;; add properties to the whole string
    (add-text-properties 0 (length str) props str)
    str))

(defun emms-browser-get-face (bdata)
  "Return a suitable face for BDATA."
  (let* ((type (emms-browser-bdata-type bdata))
         (name (cond
                ((or (eq type 'info-year)
                     (eq type 'info-genre)) "year/genre")
                 ((eq type 'info-artist) "artist")
                 ((eq type 'info-composer) "composer")
                 ((eq type 'info-performer) "performer")
                 ((eq type 'info-album) "album")
                 ((eq type 'info-title) "track"))))
    (intern
     (concat "emms-browser-" name "-face"))))

;; based on gnus code
(defun emms-browser-format-spec (format specification)
  "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"bash %u %k\",
while SPECIFICATION is an alist mapping from format spec characters
to values.  Any text properties on a %-spec itself are propagated to
the text that it generates."
  (with-temp-buffer
    (insert format)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
       ;; Quoted percent sign.
       ((eq (char-after) ?%)
        (delete-char 1))
       ;; Valid format spec.
       ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]+\\)")
        (let* ((num (match-string 1))
               (spec (match-string 2))
               (val (cdr (assoc spec specification))))
          (unless val
            (error "Invalid format character: %s" spec))
          ;; Pad result to desired length.
          (let ((text (format (concat "%" num "s") val)))
            ;; Insert first, to preserve text properties.
            (insert-and-inherit text)
            ;; Delete the specifier body.
            (delete-region (+ (match-beginning 0) (length text))
                           (+ (match-end 0) (length text)))
            ;; Delete the percent sign.
            (delete-region (1- (match-beginning 0)) (match-beginning 0)))))
       ;; Signal an error on bogus format strings.
       (t
        (error "Invalid format string"))))
    (buffer-string)))

;; --------------------------------------------------
;; Display formats - defaults
;; --------------------------------------------------

;; FIXME: optional format strings would avoid having to define a
;; function for specifiers which may be empty.

(defvar emms-browser-default-format "%i%n"
  "indent + name")

;; tracks
(defvar emms-browser-info-title-format
  'emms-browser-track-artist-and-title-format)
(defvar emms-browser-playlist-info-title-format
  'emms-browser-track-artist-and-title-format)

(defun emms-browser-get-format (bdata target)
  (let* ((type (emms-browser-bdata-type bdata))
         (target-str (or
                      (and (eq target 'browser) "")
                      (concat (symbol-name target) "-")))
         (sym
          (intern
           (concat "emms-browser-"
                   target-str
                   (symbol-name type)
                   "-format"))))
    (if (boundp sym)
        (symbol-value sym)
      emms-browser-default-format)))

(defun emms-browser-track-artist-and-title-format (bdata fmt)
  (concat
   "%i"
   (let ((track (emms-browser-format-elem fmt "T")))
     (if (and track (not (string= track "0")))
         "%T. "
       ""))
   "%n"))

;; albums - we define two formats, one for a small cover (browser),
;; and one for a medium sized cover (playlist).
(defvar emms-browser-info-album-format
  'emms-browser-year-and-album-fmt)
(defvar emms-browser-playlist-info-album-format
  'emms-browser-year-and-album-fmt-med)

(defun emms-browser-year-and-album-fmt (bdata fmt)
  (concat
   "%i%cS"
   (let ((year (emms-browser-format-elem fmt "y")))
     (if (and year (not (string= year "0")))
         "(%y) "
       ""))
   "%n"))

(defun emms-browser-year-and-album-fmt-med (bdata fmt)
  (concat
   "%i%cM"
   (let ((year (emms-browser-format-elem fmt "y")))
     (if (and year (not (string= year "0")))
         "(%y) "
       ""))
   "%n"))

;; --------------------------------------------------
;; Display faces
;; --------------------------------------------------

(defmacro emms-browser-make-face (name dark-col light-col height)
  (let ((face-name (intern (concat "emms-browser-" name "-face"))))
    `(defface ,face-name
       '((((class color) (background dark))
          (:foreground ,dark-col
                       :height ,height))
         (((class color) (background light))
          (:foreground ,light-col
                       :height ,height))
         (((type tty) (class mono))
          (:inverse-video t))
         (t (:background ,dark-col)))
       ,(concat "Face for "
                name
                " in a browser/playlist buffer.")
       :group 'emms-browser-mode)))

(emms-browser-make-face "year/genre" "#aaaaff" "#444477" 1.5)
(emms-browser-make-face "artist"     "#aaaaff" "#444477" 1.3)
(emms-browser-make-face "composer"   "#aaaaff" "#444477" 1.3)
(emms-browser-make-face "performer"  "#aaaaff" "#444477" 1.3)
(emms-browser-make-face "album"      "#aaaaff" "#444477" 1.1)
(emms-browser-make-face "track"      "#aaaaff" "#444477" 1.0)

;; --------------------------------------------------
;; Filtering
;; --------------------------------------------------

(defvar emms-browser-filters nil
  "A list of available filters.")

(defmacro emms-browser-make-filter (name func)
  "Make a user-level function for filtering tracks.
This:
 - defines an interactive function M-x emms-browser-show-NAME.
 - defines a variable emms-browser-filter-NAME of (name . func).
 - adds the filter to emms-browser-filters."
  (let ((funcnam (intern (concat "emms-browser-show-" name)))
        (var  (intern (concat "emms-browser-filter-" name)))
        (desc (concat "Filter the cache using rule '"
                       name "'")))
    `(progn
       (defvar ,var nil ,desc)
       (setq ,var (cons ,name ,func))
       (add-to-list 'emms-browser-filters ,var)
       (defun ,funcnam ()
         ,desc
         (interactive)
         (emms-browser-refilter ,var)))))

(defun emms-browser-set-filter (filter)
  "Set the current filter to be used on next update.
This does not refresh the current buffer."
  (setq emms-browser-filter-tracks-hook (cdr filter))
  (setq emms-browser-current-filter-name (car filter))
  (run-hooks 'emms-browser-filter-changed-hook))

(defun emms-browser-refilter (filter)
  "Filter and render the top-level tracks."
  (emms-browser-set-filter filter)
  (emms-browse-by (or emms-browser-top-level-type
                      emms-browser-default-browse-type)))

(defun emms-browser-next-filter (&optional reverse)
  "Redisplay with the next filter."
  (interactive)
  (let* ((list (if reverse
                  (reverse emms-browser-filters)
                 emms-browser-filters))
         (key emms-browser-current-filter-name)
         (next (cadr (member (assoc key list) list))))
    ;; wrapped
    (unless next
      (setq next (car list)))
    (emms-browser-refilter next)))

(defun emms-browser-previous-filter ()
  "Redisplay with the previous filter."
  (interactive)
  (emms-browser-next-filter t))

(defun emms-browser-filter-only-dir (path)
  "Generate a function which checks if a track is in path.
If the track is not in path, return t."
  `(lambda (track)
     (not (string-match ,(concat "^" (expand-file-name path))
                        (emms-track-get track 'name)))))

(defun emms-browser-filter-only-type (type)
  "Generate a function which checks a track's type.
If the track is not of TYPE, return t."
  `(lambda (track)
     (not (eq (quote ,type) (emms-track-get track 'type)))))

;; seconds in a day (* 60 60 24) = 86400
(defun emms-browser-filter-only-recent (days)
  "Show only tracks played within the last number of DAYS."
  `(lambda (track)
     (let ((min-date (time-subtract
                      (current-time)
                      (seconds-to-time (* ,days 86400))))
           last-played)
       (not (and (setq last-played
                       (emms-track-get track 'last-played nil))
                 (time-less-p min-date last-played))))))

(provide 'emms-browser)
;;; emms-browser.el ends here

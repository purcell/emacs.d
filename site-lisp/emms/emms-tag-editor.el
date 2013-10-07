;;; emms-tag-editor.el --- Edit track tags.

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@163.com>

;; This file is part of EMMS.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'emms-tag-editor)

;;; Code:

(eval-when-compile
  (require 'cl))
(condition-case nil
    (require 'overlay)
  (error nil))
(require 'emms)
(eval-when-compile
  (require 'emms-info-metaflac))
(require 'emms-info-mp3info)
(require 'emms-playlist-mode)
(require 'emms-mark)
(require 'format-spec)

(defvar emms-tag-editor-tags
  '((info-artist      . "a")
    (info-composer    . "C")
    (info-performer   . "p")
    (info-title       . "t")
    (info-album       . "l")
    (info-tracknumber . "n")
    (info-year        . "y")
    (info-genre       . "g")
    (info-date        . "d")
    (info-note        . "c"))
  "An alist to determine the format of various info tags.")

(defvar emms-tag-editor-edit-buffer "*EMMS-TAGS*"
  "Name of the buffer used for editing tags.")
(defvar emms-tag-editor-log-buffer "*EMMS-LOG*"
  "Name of emms-tag-editor's log buffer.")

(defun emms-tag-editor-make-format (tags)
  "Make a format string based on TAGS."
  (concat "%m\n" (emms-propertize (format "%-16s = " "name")
                             'read-only t 'rear-nonsticky t
                             'face 'bold)
          "%f\n"
          (mapconcat
           (lambda (tag)
             (concat (emms-propertize (format "%-16s = " (symbol-name tag))
                                 'read-only t 'rear-nonsticky t
                                 'face 'bold)
                     "%" (cdr (assoc tag emms-tag-editor-tags))))
           tags "\n")
          "\n\n"))

(defvar emms-tag-editor-formats
  (let* ((tags (mapcar 'car emms-tag-editor-tags))
         (default (emms-tag-editor-make-format (remove 'info-date tags))))
    `(("mp3" . ,default)
      ("ogg" . ,(emms-tag-editor-make-format (remove 'info-year tags)))
      ("flac" . ,(emms-tag-editor-make-format (remove 'info-year tags)))
      ("default" . ,default)))
  "Format to use when inserting the track.
The CAR part is the extension of the track name, and the CDR part
is the format template.  The format specification is like:

 m     --     Track description
 f     --     Track name
 a     --     Track info-artist
 c     --     Track info-composer
 p     --     Track info-performer
 t     --     Track info-title
 l     --     Track info-album
 n     --     Track info-tracknumber
 D     --     Track info-discnumber
 y     --     Track info-year
 g     --     Track info-genre
 ;     --     Track info-note

You can add new specifications in `emms-tag-editor-tags', and use
`emms-tag-editor-make-format' to create a new format string.

The CDR part also can be a function, which accepts one parameter,
the track, and returns a string to insert in
`emms-tag-editor-edit-buffer'.")

(defvar emms-tag-editor-get-format-function 'emms-tag-editor-get-format
  "Determines which function to call to get the format string, which is
used for inserting the track.")

(defvar emms-tag-editor-parse-function 'emms-tag-editor-default-parser
  "Function to parse tags in `emms-tag-editor-edit-buffer'.
It should find all modified tags, and return all the tracks.  The
tracks for which a tag has been modified should set a property
'tag-modified to t.  If the track name has been changed, the
function should set a new property 'newname instead of setting
the 'name directly.

See also `emms-tag-editor-default-parser'.")

(defvar emms-tag-editor-tagfile-functions
  '(("mp3" "mp3info"
     ((info-artist      . "a")
      (info-title       . "t")
      (info-album       . "l")
      (info-tracknumber . "n")
      (info-year        . "y")
      (info-genre       . "g")
      (info-note        . "c")))
    ("ogg" . emms-tag-editor-tag-ogg)
    ("flac" . emms-tag-editor-tag-flac))
  "An alist used when committing changes to tags in files.
If the external program sets tags by command line options
one-by-one such as mp3info, then the list should like:
 (EXTENSION PROGRAM COMMAND_LINE_OPTIONS)

Otherwise, a function that accepts a single parameter, the track,
should be given.

See also `emms-tag-editor-tag-file' and `emms-tag-editor-tag-ogg'.")

(defun emms-tag-editor-tag-flac (track)
  "Commit changes to an FLAC file according to TRACK."
  (require 'emms-info-metaflac)
  (with-temp-buffer
    (let (need val)
      (mapc (lambda (tag)
              (let ((info-tag (intern (concat "info-" tag))))
                (when (> (length (setq val (emms-track-get track info-tag))) 0)
                  (insert (upcase tag) "=" val "\n"))))
            '("artist" "composer" "performer" "title" "album" "tracknumber" "discnumber" "date" "genre" "note"))
      (when (buffer-string)
        (funcall #'call-process-region (point-min) (point-max)
                 emms-info-metaflac-program-name nil
                 (get-buffer-create emms-tag-editor-log-buffer)
                 nil
                 "--import-tags-from=-"
                 (emms-track-name track))))))

(defun emms-tag-editor-tag-ogg (track)
  "Commit changes to an OGG file according to TRACK."
  (let (args val)
    (mapc (lambda (tag)
            (let ((info-tag (intern (concat "info-" tag))))
              (when (> (length (setq val (emms-track-get track info-tag))) 0)
                (setq args (append (list "-t" (concat (upcase tag) "=" val)) args)))))
          '("artist" "composer" "performer" "title" "album" "tracknumber" "date" "genre" "note"))
    (when args
      (apply #'call-process "vorbiscomment" nil
             (get-buffer-create emms-tag-editor-log-buffer)
             nil
             "-w"
             (append args (list (emms-track-name track)))))))

(defun emms-tag-editor-tag-file (track program tags)
  "Change TAGS in FILE, using PROGRAM.
Valid tags are given by `emms-tag-editor-tagfile-functions'."
  (let (args val)
    (mapc (lambda (tag)
            (setq val (emms-track-get track (car tag)))
            (if (and val (stringp val))
                (setq args (append (list (concat "-" (cdr tag)) val) args))))
          tags)
    (apply 'call-process program
           nil (get-buffer-create emms-tag-editor-log-buffer) nil
           (nconc args (list filename)))))

(defun emms-tag-editor-get-format (track)
  "Get the format string to use for committing changes to TRACK."
  (let ((format
         (assoc (file-name-extension (emms-track-name track))
                emms-tag-editor-formats)))
    (if format
        (cdr format)
      (cdr (assoc "default" emms-tag-editor-formats)))))

(defun emms-tag-editor-format-track (track)
  "Return a string representing the info tags contained in TRACK.
This string is suitable for inserting into the tags buffer."
  (let ((format (funcall emms-tag-editor-get-format-function track)))
    (if (functionp format)
        (funcall format track)
      (format-spec
       format
       (apply 'format-spec-make
              ?m (emms-propertize (emms-track-force-description track)
                                  'face 'emms-playlist-track-face
                                  'emms-track (copy-sequence track))
              ?f (emms-track-name track)
              (apply 'append
                     (mapcar (lambda (tag)
                               (list (string-to-char (cdr tag))
                                     (or (emms-track-get track (car tag)) "")))
                             emms-tag-editor-tags)))))))

(defun emms-tag-editor-track-at (&optional pos)
  "Return a copy of the track at POS.  Defaults to point if POS is nil."
  (let ((track (emms-playlist-track-at pos))
        newtrack)
    (when track
      (setq newtrack (copy-sequence track))
      (emms-track-set newtrack 'position (point-marker))
      (emms-track-set newtrack 'orig-track track)
      newtrack)))

(defsubst emms-tag-editor-erase-buffer (&optional buf)
  "Erase the buffer BUF, and ensure that it exists."
  (let ((inhibit-read-only t))
    (save-excursion
      (set-buffer (get-buffer-create buf))
      (erase-buffer))))

(defsubst emms-tag-editor-insert-track (track)
  "Insert TRACK, if it is specified."
  (and track
       (insert (emms-tag-editor-format-track track))))

(defsubst emms-tag-editor-display-log-buffer-maybe ()
  "Display the log buffer if it has any contents."
  (if (> (buffer-size (get-buffer emms-tag-editor-log-buffer)) 0)
      (display-buffer emms-tag-editor-log-buffer)))

(defun emms-tag-editor-insert-tracks (tracks)
  "Insert TRACKS into the tag editor buffer."
  (save-excursion
    (emms-tag-editor-erase-buffer emms-tag-editor-log-buffer)
    (emms-tag-editor-erase-buffer emms-tag-editor-edit-buffer)
    (set-buffer (get-buffer emms-tag-editor-edit-buffer))
    (mapc 'emms-tag-editor-insert-track tracks)
    (emms-tag-editor-mode)
    (pop-to-buffer (current-buffer))
    (goto-char (point-min))
    (emms-tag-editor-display-log-buffer-maybe)))

(defun emms-tag-editor-edit-track (track)
  "Edit the track at point, or TRACK."
  (interactive (list (emms-tag-editor-track-at)))
  (if (null track)
      (message "No track at point!")
    (emms-tag-editor-insert-tracks (list track))))

(defun emms-tag-editor-edit-marked-tracks ()
  "Edit all tracks marked in the current buffer."
  (interactive)
  (let ((tracks (emms-mark-mapcar-marked-track 'emms-tag-editor-track-at t)))
    (if (null tracks)
        (message "No track marked!")
      (emms-tag-editor-insert-tracks tracks))))

(defun emms-tag-editor-edit ()
  "Edit tags of either the track at point or all marked tracks."
  (interactive)
  (if (emms-mark-has-markedp)
      (emms-tag-editor-edit-marked-tracks)
    (emms-tag-editor-edit-track (emms-tag-editor-track-at))))

(defvar emms-tag-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'emms-tag-editor-next-field)
    (define-key map [backtab] 'emms-tag-editor-prev-field)
    (define-key map "\C-c\C-n" 'emms-tag-editor-next-track)
    (define-key map "\C-c\C-p" 'emms-tag-editor-prev-track)
    (define-key map "\C-c\C-c" 'emms-tag-editor-submit-and-exit)
    (define-key map "\C-c\C-s" 'emms-tag-editor-submit)
    (define-key map "\C-x\C-s" 'emms-tag-editor-submit)
    (define-key map "\C-c\C-r" 'emms-tag-editor-set-all)
    (define-key map "\C-c\C-a" 'emms-tag-editor-replace-in-tag)
    (define-key map "\C-c\C-t" 'emms-tag-editor-transpose-tag)
    map)
  "Keymap for `emms-tag-editor-mode'.")
(define-key emms-playlist-mode-map "E" 'emms-tag-editor-edit)

(define-derived-mode emms-tag-editor-mode text-mode "Tag-Edit"
  "Major mode to edit track tags.
\\{emms-tag-editor-mode-map}")

(defun emms-tag-editor-set-all (tag value)
  "Set TAG to VALUE in all tracks.
If transient-mark-mode is turned on, you can apply the command to
a selected region.

 If `transient-mark-mode' is on and the mark is active, the
changes will only take effect on the tracks in the region."
  (interactive
   (list (emms-completing-read "Set tag: "
                               (mapcar (lambda (arg)
                                         (list (symbol-name (car arg))))
                                       emms-tag-editor-tags)
                               nil t)
         (read-from-minibuffer "To: ")))
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (re-search-forward (concat "^" (regexp-quote tag)) nil t)
        (skip-chars-forward " \t=")
        (delete-region (point) (line-end-position))
        (insert value)))))

(defun emms-tag-editor-replace-in-tag (tag from to)
  "Query and replace text in selected TAG.
For example, if the info-title tag is selected, then only perform
replacement in title tags.

If `transient-mark-mode' is on and the mark is active, the
changes will only take effect on the tracks in the region."
  (interactive
   (cons (emms-completing-read "Replace in tag: "
                               (mapcar (lambda (arg)
                                         (list (symbol-name (car arg))))
                                       emms-tag-editor-tags)
                               nil t)
         (let ((common (query-replace-read-args
                        (if (and transient-mark-mode mark-active)
                            "Query replace regexp in region"
                          "Query replace regexp")
                        t)))
           (butlast common))))
  (let ((overlay (make-overlay (point-min) (1+ (point-min)))))
    (overlay-put overlay 'face 'match)
    (unwind-protect
        (save-excursion
          (save-restriction
            (when (and mark-active transient-mark-mode)
              (narrow-to-region (region-beginning) (region-end))
              (deactivate-mark))
            (setq tag (concat (regexp-quote tag) "[ \t]+=[ \t]+"))
            (goto-char (point-min))
            (map-y-or-n-p
             (lambda (match)
               (move-overlay overlay (match-beginning 0) (match-end 0))
               (format "Replace %s to %s" match to))
             (lambda (match)
               (delete-region (- (point) (length match)) (point))
               (insert to))
             (lambda ()
               (if (and (save-excursion
                          (re-search-backward tag (line-beginning-position) t))
                        (re-search-forward from (line-end-position) t))
                   (match-string 0)
                 (let (found)
                   (while (and (not found)
                               (re-search-forward tag nil t))
                     (if (re-search-forward from (line-end-position) t)
                         (setq found t)))
                   (and found (match-string 0))))))))
      (delete-overlay overlay))))

(defun emms-tag-editor-transpose-tag (tag1 tag2)
  "Transpose value of TAG1 and TAG2.
If `transient-mark-mode' is on and the mark is active, the
changes will only take effect on the tracks in the region."
  (interactive
   (let* ((tag1 (intern (emms-completing-read
                         "Tag 1: "
                         (mapcar (lambda (arg)
                                         (list (symbol-name (car arg))))
                                       emms-tag-editor-tags)
                         nil t)))
          (tag2 (intern (emms-completing-read
                         "Tag 2: "
                         (mapcar (lambda (arg)
                                   (list (symbol-name (car arg))))
                                 (assq-delete-all
                                  tag1
                                  (copy-sequence emms-tag-editor-tags)))
                         nil t))))
     (list tag1 tag2)))
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      (let* ((emms-playlist-buffer-p t)
             (tracks (emms-playlist-tracks-in-region (point-min)
                                                     (point-max)))
             (inhibit-read-only t)
             temp)
        (erase-buffer)
        (dolist (track (nreverse tracks))
          (setq temp (emms-track-get track tag1))
          (emms-track-set track tag1 (emms-track-get track tag2))
          (emms-track-set track tag2 temp)
          (emms-track-set track 'tag-modified t)
          (emms-tag-editor-insert-track track))))))

(defun emms-tag-editor-guess-tag-filename (pattern fullname)
  "A pattern is a string like \"%a-%t-%y\" which stand for
the file name is constructed by artist, title, year with seperator '-'.
see `emms-tag-editor-compile-pattern' for detail about pattern syntax.
Available tags are list in `emms-tag-editor-tags'.

if with prefix argument, the information will extract from full
name, otherwise just match in file name.

An example to guess tag from file name, which the file directory is
the aritist and file name is the title. It can be done like:
C-u M-x emms-tag-editor-guess-tag-filename RET
%{a:[^/]+}/%{t:[^/]+}\.mp3 RET
"
  (interactive
   (list
    (read-from-minibuffer (format "Match in %sfile name(C-h for help): "
                                  (if current-prefix-arg "FULL " ""))
                          nil
     (let ((map (make-sparse-keymap)))
       (set-keymap-parent map minibuffer-local-map)
       (define-key map "\C-h"
         (lambda ()
           (interactive)
           (with-output-to-temp-buffer "*Help*"
             (princ
              "A pattern is a string like \"%a-%t-%y\" which stand for
the file name is constructed by artist, title, year with seperator '-'.
see `emms-tag-editor-compile-pattern' for detail about pattern syntax.

Available tags are:
")
             (mapc (lambda (tag)
                     (princ (format "\t%s - %S\n" (cdr tag) (car tag))))
                   emms-tag-editor-tags)
             (save-excursion
               (set-buffer standard-output)
               (help-mode)))))
       map))
    current-prefix-arg))
  (setq pattern (emms-tag-editor-compile-pattern pattern))
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      (let* ((emms-playlist-buffer-p t)
             (tracks (emms-playlist-tracks-in-region (point-min)
                                                     (point-max)))
             (inhibit-read-only t)
             filename)
        (erase-buffer)
        (dolist (track (nreverse tracks))
          (emms-track-set track 'tag-modified t)
          (setq filename (emms-track-name track))
          (or fullname (setq filename (file-name-nondirectory filename)))
          (when (string-match (car pattern) filename)
            (mapc (lambda (pair)
                    (emms-track-set
                     track
                     (car (rassoc (char-to-string (car pair))
                                  emms-tag-editor-tags))
                     (match-string (cdr pair) filename)))
                  (cdr pattern)))
          (emms-tag-editor-insert-track track))))))

(defun emms-tag-editor-compile-pattern (pattern)
  "A pattern to regexp convertor. \"%a-%{b:[a-z]+}\" will compile to
\"\\([^-]+\\)-\\([a-z]+\\)\"."
  (let ((index 0)
        (paren 0)
        (i 0)
        (len (length pattern))
        (compiled "")
        registers register match
        escape c)
    (while (< i len)
      (setq c (aref pattern i)
            i (1+ i))
      (cond ((= c ?\\)
             (setq c (aref pattern i)
                   i (1+ i))
             (cond ((= c ?\()
                    (setq paren (1+ paren)
                          index (1+ index)))
                   ((= c ?\))
                    (setq paren (1- paren))))
             (setq compiled (concat compiled "\\" (char-to-string c))))
            ((= c ?%)
             (setq c (aref pattern i)
                   i (1+ i))
             ;; How to repressent } in the pattern?
             (if (= c ?{)
                 (if (/= (aref pattern (1+ i)) ?:)
                     (error "Compile error")
                   (setq register (aref pattern i)
                         match ""
                         i (+ i 2))
                   (while (and (< i len)
                               (or escape (/= (aref pattern i) ?})))
                     (if escape
                         (setq escape nil)
                       (if (= (aref pattern i) ?\\)
                           (setq escape t)))
                     (setq match (concat match (char-to-string (aref pattern i)))
                           i (1+ i)))
                   (setq i (1+ i)))
               (setq register c
                     match "[^-]+"))
             (setq compiled (concat compiled "\\(" match "\\)")
                   index (1+ index))
             (add-to-list 'registers (cons register index)))
            (t (setq compiled (concat compiled (char-to-string c))))))
    (if (/= paren 0) (error "Paren not match!"))
    (cons compiled registers)))

(defun emms-tag-editor-next-field (arg)
  "Move to the next tag field."
  (interactive "p")
  (if (> arg 0)
      (re-search-forward "\\s-*=[ \t]*" nil nil arg)
    (emms-tag-editor-prev-field (- arg))))

(defun emms-tag-editor-prev-field (arg)
  "Move to the previous tag field."
  (interactive "p")
  (if (< arg 0)
      (emms-tag-editor-next-field (- arg))
    (skip-chars-backward " \t=")
    (re-search-backward "\\s-*=[ \t]*" nil nil arg)
    (skip-chars-forward " \t=")))

(defun emms-tag-editor-prev-track ()
  "Move to the previous track."
  (interactive)
  (let ((prev (previous-single-property-change (point)
                                               'emms-track)))
    (when (not prev)
      (error "No previous track"))
    (when (not (get-text-property prev 'emms-track))
      (setq prev (or (previous-single-property-change prev 'emms-track)
                     (point-min))))
    (when (or (not prev)
              (not (get-text-property prev 'emms-track)))
      (error "No previous track"))
    (goto-char prev)))

(defun emms-tag-editor-next-track ()
  "Move to the next track."
  (interactive)
  (let ((next (next-single-property-change (point)
                                           'emms-track)))
    (when (not next)
      (error "No next track"))
    (when (not (get-text-property next 'emms-track))
      (setq next (next-single-property-change next 'emms-track)))
    (when (or (not next)
              (= next (point-max)))
      (error "No next track"))
    (goto-char next)))

(defun emms-tag-editor-submit (arg)
  "Make modified tags take affect.
With prefix argument, bury the tag edit buffer."
  (interactive "P")
  (let ((tracks (funcall emms-tag-editor-parse-function)))
    (if (not (and tracks (y-or-n-p "Submit changes? ")))
        (message "No tags were modified")
      (emms-tag-editor-erase-buffer emms-tag-editor-log-buffer)
      (emms-tag-editor-apply tracks)))
  (if arg (bury-buffer)))

(defun emms-tag-editor-apply (tracks)
  "Apply all changes made to TRACKS."
  (message "Setting tags...")
  (let (filename func exit old pos val need-sync)
    (save-excursion
      (dolist (track tracks)
        (when (emms-track-get track 'tag-modified)
          (setq filename (emms-track-name track)
                old (emms-track-get track 'orig-track))
          ;; rename local file
          (when (and (emms-track-get track 'newname)
                     (eq (emms-track-get track 'type) 'file)
                     (file-writable-p (emms-track-name track))
                     (y-or-n-p (format "Rename %s to %s? "
                                       (emms-track-name track)
                                       (emms-track-get track 'newname))))
            (setq filename (emms-track-get track 'newname))
            (ignore-errors
              ;; Ignore errors so that renaming multiple files doesn't stop
              ;; because of one that fails.  In that case it's probably
              ;; old-file = newfile which causes the problem.
              (rename-file (emms-track-name track) filename 1))
            (emms-track-set old 'name filename)
            ;; for re-enter this function
            (emms-track-set track 'name filename)
            (setq need-sync t)
            ;; register to emms-cache-db
            (when (boundp 'emms-cache-modified-function)
              (funcall emms-cache-modified-function)
              (funcall emms-cache-set-function 'file filename old)))
          (emms-track-set track 'newname nil)
          ;; set tags to original track
          (dolist (tag emms-tag-editor-tags)
            (when (setq val (emms-track-get track (car tag)))
            (emms-track-set old (car tag) val)))
          ;; use mp3info to change tag in mp3 file
          (when (and (eq (emms-track-get track 'type) 'file)
                     (file-writable-p (emms-track-name track))
                     (setq func (assoc (file-name-extension filename)
                                       emms-tag-editor-tagfile-functions)))
            (setq exit
                  (if (functionp (cdr func))
                      (funcall (cdr func) track)
                    (emms-tag-editor-tag-file track (cadr func) (nth 2 func))))
            (if (zerop exit)
                (emms-track-get track 'info-mtime (butlast (current-time)))
              (emms-tag-editor-log
               "Changing tags of %s failed with exit value %d"
               filename exit)))
          ;; update track in playlist
          (when (and (setq pos (emms-track-get track 'position))
                     (marker-position pos))
            (set-buffer (marker-buffer pos))
            (goto-char pos)
            (funcall emms-playlist-update-track-function))
          ;; clear modified tag
          (emms-track-set track 'tag-modified nil))))
    (if (and (featurep 'emms-cache)
             need-sync
             (y-or-n-p "You have changed some track names; sync the cache? "))
        (and (fboundp 'emms-cache-sync) ; silence byte-compiler
             (emms-cache-sync)))
    (unless (emms-tag-editor-display-log-buffer-maybe)
      (message "Setting tags...done"))))

(defun emms-tag-editor-submit-and-exit ()
  "Submit changes to track information and exit the tag editor."
  (interactive)
  (emms-tag-editor-submit t))

(defun emms-tag-editor-default-parser ()
  "Default function used to parse tags in `emms-tag-editor-edit-buffer'."
  (let (next tracks track key val)
    (goto-char (point-min))
    (if (get-text-property (point) 'emms-track)
        (setq next (point))
      (setq next (next-single-property-change (point)
                                              'emms-track)))
    (when next
      (while
          (progn
            (goto-char next)
            (setq track (get-text-property (point) 'emms-track))
            (forward-line 1)
            (mapc (lambda (pair)
                    (when (string-match "\\s-*=\\s-*" pair)
                      (setq key (intern-soft (substring pair 0 (match-beginning 0)))
                            val (substring pair (match-end 0)))
                      (when (and key
                                 (let ((old (emms-track-get track key)))
                                   (if old
                                       (not (string= val old))
                                     (string< "" val))))
                        (if (eq key 'name)
                            (emms-track-set track 'newname val)
                          (emms-track-set track key val))
                        (emms-track-set track 'tag-modified t))))
                  (let ((end-point (next-single-property-change
                                    (point) 'emms-track)))
                    (if (and end-point (save-excursion
                                         (goto-char end-point)
                                         (bolp)))
                        (setq next end-point)
                      (progn
                        (setq next nil
                              end-point (point-max))))
                    (split-string (buffer-substring (point) end-point)
                                  "\n")))
            (if (emms-track-get track 'tag-modified)
                (push track tracks))
            next))
      tracks)))

(defun emms-tag-editor-log (&rest args)
  (with-current-buffer (get-buffer-create emms-tag-editor-log-buffer)
    (goto-char (point-max))
    (insert (apply 'format args) "\n")))

;;
;; Renaming files according their tags
;;

(defvar emms-tag-editor-rename-format "%a - %l - %n - %t"
  "When `emms-tag-editor-rename' is invoked the track's file will
be renamed according this format specification.  The file
extension will be added automatically.

It uses the format specs defined in `emms-tag-editor-tags'.")

(defun emms-tag-editor-rename ()
  "Rename the file corresponding to track at point or all marked
tracks according to the value of
`emms-tag-editor-rename-format'."
  (interactive)
  (if (emms-mark-has-markedp)
      (emms-tag-editor-rename-marked-tracks)
    (emms-tag-editor-rename-track (emms-tag-editor-track-at))))

(defun emms-tag-editor-rename-track (track &optional dont-apply)
  "Rename TRACK's file according `emms-tag-editor-rename-format's
value.

If DONT-APPLY is non-nil the changes won't be applied directly.
Then it's the callers job to apply them afterwards with
`emms-tag-editor-apply'."
  (if (eq (emms-track-get track 'type) 'file)
      (let* ((old-file (emms-track-name track))
             (path     (file-name-directory old-file))
             (suffix   (file-name-extension old-file))
             (new-file (concat
                        path
                        (format-spec
                         emms-tag-editor-rename-format
                         (apply 'format-spec-make
                                (apply 'append
                                       (mapcar
                                        (lambda (tag)
                                          (list (string-to-char (cdr tag))
                                                (or (emms-track-get track (car tag))
                                                    "")))
                                        emms-tag-editor-tags))))
                      "." suffix)))
        (emms-track-set track 'newname new-file)
        (emms-track-set track 'tag-modified t)
        (unless dont-apply
          (emms-tag-editor-apply (list track))))
    (message "Only files can be renamed.")))

(defun emms-tag-editor-rename-marked-tracks ()
  "Rename the files corresponding to all marked tracks according
`emms-tag-editor-rename-format's value."
  (let ((tracks (emms-mark-mapcar-marked-track
                 'emms-tag-editor-track-at t)))
    (if (null tracks)
        (message "No track marked!")
      (dolist (track tracks)
        (emms-tag-editor-rename-track track t))
      (emms-tag-editor-apply tracks))))

(define-key emms-playlist-mode-map "R" 'emms-tag-editor-rename)

(provide 'emms-tag-editor)
;;; Emms-tag-editor.el ends here

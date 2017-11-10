;;; org-link-edit.el --- Slurp and barf with Org links  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Kyle Meyer <kyle@kyleam.com>

;; Author:  Kyle Meyer <kyle@kyleam.com>
;; URL: https://gitlab.com/kyleam/org-link-edit
;; Keywords: convenience
;; Version: 1.1.1
;; Package-Requires: ((cl-lib "0.5") (org "8.2.10"))

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

;;; Commentary:

;; Org Link Edit provides Paredit-inspired slurping and barfing
;; commands for Org link descriptions.
;;
;; There are four slurp and barf commands, all which operate when
;; point is on an Org link.
;;
;; - org-link-edit-forward-slurp
;; - org-link-edit-backward-slurp
;; - org-link-edit-forward-barf
;; - org-link-edit-backward-barf
;;
;; Org Link Edit doesn't bind these commands to any keys.  Finding
;; good keys for these commands is difficult because, while it's
;; convenient to be able to quickly repeat these commands, they won't
;; be used frequently enough to be worthy of a short, repeat-friendly
;; binding.  Using Hydra [1] provides a nice solution to this.  After
;; an initial key sequence, any of the commands will be repeatable
;; with a single key.  (Plus, you get a nice interface that displays
;; the key for each command.)  Below is one example of how you could
;; configure this.
;;
;;     (define-key org-mode-map YOUR-KEY
;;       (defhydra hydra-org-link-edit ()
;;         "Org Link Edit"
;;         ("j" org-link-edit-forward-slurp "forward slurp")
;;         ("k" org-link-edit-forward-barf "forward barf")
;;         ("u" org-link-edit-backward-slurp "backward slurp")
;;         ("i" org-link-edit-backward-barf "backward barf")
;;         ("q" nil "cancel")))
;;
;; In addition to the slurp and barf commands, the command
;; `org-link-edit-transport-next-link' searches for the next (or
;; previous) link and moves it to point, using the word at point or
;; the selected region as the link's description.
;;
;; [1] https://github.com/abo-abo/hydra

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)

(defun org-link-edit--on-link-p (&optional element)
  (let ((el (or element (org-element-context))))
    ;; Don't use `org-element-lineage' because it isn't available
    ;; until Org version 8.3.
    (while (and el (not (memq (car el) '(link))))
      (setq el (org-element-property :parent el)))
    (eq (car el) 'link)))

(defun org-link-edit--link-data ()
  "Return list with information about the link at point.
The list includes
- the position at the start of the link
- the position at the end of the link
- the link text
- the link description (nil when on a plain link)"
  (let ((el (org-element-context)))
    (unless (org-link-edit--on-link-p el)
      (user-error "Point is not on a link"))
    (save-excursion
      (goto-char (org-element-property :begin el))
      (cond
       ;; Use match-{beginning,end} because match-end is consistently
       ;; positioned after ]], while the :end property is positioned
       ;; at the next word on the line, if one is present.
       ((looking-at org-bracket-link-regexp)
        (list (match-beginning 0)
              (match-end 0)
              (save-match-data
                (org-link-unescape (match-string-no-properties 1)))
              (or (and (match-end 3)
                       (match-string-no-properties 3))
                  "")))
       ((looking-at org-plain-link-re)
        (list (match-beginning 0)
              (match-end 0)
              (org-link-unescape (match-string-no-properties 0))
              nil))
       (t
        (error "What am I looking at?"))))))

(defun org-link-edit--forward-blob (n &optional no-punctuation)
  "Move forward N blobs (backward if N is negative).

A block of non-whitespace characters is a blob.  If
NO-PUNCTUATION is non-nil, trailing punctuation characters are
not considered part of the blob when going in the forward
direction.

If the edge of the buffer is reached before completing the
movement, return nil.  Otherwise, return t."
  (let* ((forward-p (> n 0))
         (nblobs (abs n))
         (skip-func (if forward-p 'skip-syntax-forward 'skip-syntax-backward))
         skip-func-retval)
    (while (/= nblobs 0)
      (funcall skip-func " ")
      (setq skip-func-retval (funcall skip-func "^ "))
      (setq nblobs (1- nblobs)))
    (when (and forward-p no-punctuation)
      (let ((punc-tail-offset (save-excursion (skip-syntax-backward "."))))
        ;; Don't consider trailing punctuation as part of the blob
        ;; unless the whole blob consists of punctuation.
        (unless (= skip-func-retval (- punc-tail-offset))
          (goto-char (+ (point) punc-tail-offset)))))
    (/= skip-func-retval 0)))

;;;###autoload
(defun org-link-edit-forward-slurp (&optional n)
  "Slurp N trailing blobs into link's description.

  The \[\[http://orgmode.org/\]\[Org mode\]\] site

                        |
                        v

  The \[\[http://orgmode.org/\]\[Org mode site\]\]

A blob is a block of non-whitespace characters.  When slurping
forward, trailing punctuation characters are not considered part
of a blob.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp leading blobs instead of trailing blobs."
  (interactive "p")
  (setq n (or n 1))
  (cond
   ((= n 0))
   ((< n 0)
    (org-link-edit-backward-slurp (- n)))
   (t
    (cl-multiple-value-bind (beg end link desc) (org-link-edit--link-data)
      (goto-char (save-excursion
                   (goto-char end)
                   (or (org-link-edit--forward-blob n 'no-punctuation)
                       (user-error "Not enough blobs after the link"))
                   (point)))
      (let ((slurped (buffer-substring-no-properties end (point))))
        (setq slurped (replace-regexp-in-string "\n+" " " slurped))
        (when (and (= (length desc) 0)
                   (string-match "^\\s-+\\(.*\\)" slurped))
          (setq slurped (match-string 1 slurped)))
        (setq desc (concat desc slurped)
              end (+ end (length slurped)))
        (delete-region beg (point))
        (insert (org-make-link-string link desc))
        (goto-char beg)
        slurped)))))

;;;###autoload
(defun org-link-edit-backward-slurp (&optional n)
  "Slurp N leading blobs into link's description.

  The \[\[http://orgmode.org/\]\[Org mode\]\] site

                        |
                        v

  \[\[http://orgmode.org/\]\[The Org mode\]\] site

A blob is a block of non-whitespace characters.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp trailing blobs instead of leading blobs."
  (interactive "p")
  (setq n (or n 1))
  (cond
   ((= n 0))
   ((< n 0)
    (org-link-edit-forward-slurp (- n)))
   (t
    (cl-multiple-value-bind (beg end link desc) (org-link-edit--link-data)
      (goto-char (save-excursion
                   (goto-char beg)
                   (or (org-link-edit--forward-blob (- n))
                       (user-error "Not enough blobs before the link"))
                   (point)))
      (let ((slurped (buffer-substring-no-properties (point) beg)))
        (when (and (= (length desc) 0)
                   (string-match "\\(.*\\)\\s-+$" slurped))
          (setq slurped (match-string 1 slurped)))
        (setq slurped (replace-regexp-in-string "\n+" " " slurped))
        (setq desc (concat slurped desc)
              beg (- beg (length slurped)))
        (delete-region (point) end)
        (insert (org-make-link-string link desc))
        (goto-char beg)
        slurped)))))

(defun org-link-edit--split-first-blobs (string n)
  "Split STRING into (N first blobs . other) cons cell.
'N first blobs' contains all text from the start of STRING up to
the start of the N+1 blob.  'other' includes the remaining text
of STRING.  If the number of blobs in STRING is fewer than N,
'other' is nil."
  (when (< n 0) (user-error "N cannot be negative"))
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (with-syntax-table org-mode-syntax-table
      (let ((within-bound (org-link-edit--forward-blob n)))
        (skip-syntax-forward " ")
        (cons (buffer-substring 1 (point))
              (and within-bound
                   (buffer-substring (point) (point-max))))))))

(defun org-link-edit--split-last-blobs (string n)
  "Split STRING into (other . N last blobs) cons cell.
'N last blobs' contains all text from the end of STRING back to
the end of the N+1 last blob.  'other' includes the remaining
text of STRING.  If the number of blobs in STRING is fewer than
N, 'other' is nil."
  (when (< n 0) (user-error "N cannot be negative"))
  (with-temp-buffer
    (insert string)
    (goto-char (point-max))
    (with-syntax-table org-mode-syntax-table
      (let ((within-bound (org-link-edit--forward-blob (- n))))
        (skip-syntax-backward " ")
        (cons (and within-bound
                   (buffer-substring 1 (point)))
              (buffer-substring (point) (point-max)))))))

;;;###autoload
(defun org-link-edit-forward-barf (&optional n)
  "Barf N trailing blobs from link's description.

  The \[\[http://orgmode.org/\]\[Org mode\]\] site

                        |
                        v

  The \[\[http://orgmode.org/\]\[Org\]\] mode site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf leading blobs instead of trailing blobs."
  (interactive "p")
  (setq n (or n 1))
  (cond
   ((= n 0))
   ((< n 0)
    (org-link-edit-backward-barf (- n)))
   (t
    (cl-multiple-value-bind (beg end link desc) (org-link-edit--link-data)
      (when (= (length desc) 0)
        (user-error "Link has no description"))
      (pcase-let ((`(,new-desc . ,barfed) (org-link-edit--split-last-blobs
                                           desc n)))
        (unless new-desc (user-error "Not enough blobs in description"))
        (goto-char beg)
        (delete-region beg end)
        (insert (org-make-link-string link new-desc))
        (when (string= new-desc "")
          (setq barfed (concat " " barfed)))
        (insert barfed)
        (goto-char beg)
        barfed)))))

;;;###autoload
(defun org-link-edit-backward-barf (&optional n)
  "Barf N leading blobs from link's description.

  The \[\[http://orgmode.org/\]\[Org mode\]\] site

                        |
                        v

  The Org \[\[http://orgmode.org/\]\[mode\]\] site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf trailing blobs instead of leading blobs."
  (interactive "p")
  (setq n (or n 1))
  (cond
   ((= n 0))
   ((< n 0)
    (org-link-edit-forward-barf (- n)))
   (t
    (cl-multiple-value-bind (beg end link desc) (org-link-edit--link-data)
      (when (= (length desc) 0)
        (user-error "Link has no description"))
      (pcase-let ((`(,barfed . ,new-desc) (org-link-edit--split-first-blobs
                                           desc n)))
        (unless new-desc (user-error "Not enough blobs in description"))
        (goto-char beg)
        (delete-region beg end)
        (insert (org-make-link-string link new-desc))
        (when (string= new-desc "")
          (setq barfed (concat barfed " ")))
        (goto-char beg)
        (insert barfed)
        barfed)))))

(defun org-link-edit--next-link-data (&optional previous)
  (save-excursion
    (if (funcall (if previous #'re-search-backward #'re-search-forward)
                 org-any-link-re nil t)
        (org-link-edit--link-data)
      (user-error "No %s link found" (if previous "previous" "next")))))

;;;###autoload
(defun org-link-edit-transport-next-link (&optional previous beg end)
  "Move the next link to point.

If the region is active, use the selected text as the link's
description.  Otherwise, use the word at point.

With prefix argument PREVIOUS, move the previous link instead of
the next link.

Non-interactively, use the text between BEG and END as the
description, moving the next (or previous) link relative BEG and
END."
  (interactive (cons current-prefix-arg
                     (and (use-region-p)
                          (list (region-beginning) (region-end)))))
  (let ((pt (point))
        (desc-bounds (cond
                      ((and beg end)
                       (cons (progn (goto-char beg)
                                    (point-marker))
                             (progn (goto-char end)
                                    (point-marker))))
                      ((not (looking-at-p "\\s-"))
                       (progn (skip-syntax-backward "w")
                              (let ((beg (point-marker)))
                                (skip-syntax-forward "w")
                                (cons beg (point-marker))))))))
    (when (or (and desc-bounds
                   (or (progn (goto-char (car desc-bounds))
                              (org-link-edit--on-link-p))
                       (progn (goto-char (cdr desc-bounds))
                              (org-link-edit--on-link-p))))
              (progn (goto-char pt)
                     (org-link-edit--on-link-p)))
      (user-error "Cannot transport next link with point on a link"))
    (goto-char (or (car desc-bounds) pt))
    (cl-multiple-value-bind (link-beg link-end link orig-desc)
        (org-link-edit--next-link-data previous)
      (unless (or (not desc-bounds) (= (length orig-desc) 0))
        (user-error "Link already has a description"))
      (delete-region link-beg link-end)
      (insert (org-make-link-string
               link
               (if desc-bounds
                   (delete-and-extract-region (car desc-bounds)
                                              (cdr desc-bounds))
                 orig-desc))))))

(provide 'org-link-edit)
;;; org-link-edit.el ends here

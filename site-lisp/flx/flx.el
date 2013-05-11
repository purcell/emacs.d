;;; flx.el --- fuzzy matching with good sorting

;; this file is not part of Emacs

;; Copyright (C) 2013 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: fuzzy matching with good sorting
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Wed Apr 17 01:01:41 2013 (+0800)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 12
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;;
;;
;;

;;; Commentary:

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:



;;; credit to scott frazer's blog entry here:http://scottfrazersblog.blogspot.com.au/2009/12/emacs-better-ido-flex-matching.html
;;; credit to ido-hacks for ido optimization

;;; Use defsubst instead of defun

;;; Notes:
;;;
;;; * Using bitmaps to check for matches worked out to be SLOWER than just
;;;   scanning the string and using `flx-get-matches'.
;;;
;;; * Consing causes GC, which can often slowdown Emacs more than the benefits
;;;   of an optimization.
;;;

(eval-when-compile (require 'cl))

(defface flx-highlight-face  '((t (:inherit font-lock-variable-name-face :bold t :underline t)))
  "Face used by flx for highlighting flx match characters."
  :group 'flx)


(defun flx-get-hash-for-string (str heatmap-func)
  "Return hash-table for string where keys are characters value
  is a sorted list of indexes for character occurrences."
  (let* ((res (make-hash-table :test 'eq :size 32))
         (str-len (length str))
         char)
    (loop for index from (1- str-len) downto 0
          do (progn
               (setq char (downcase (aref str index)))
               (push index (gethash char res))))
    (puthash 'heatmap (funcall heatmap-func str) res)
    res))

;;; Do we need more word separators than ST?
(defsubst flx-is-word (char)
  "returns t if char is word"
  (and char
       (not (memq char '(?\  ?- ?_ ?. ?/ ?\\)))))

(defsubst flx-is-capital (char)
  "returns t if char is word"
  (and char
       (and (<= char ?Z)
            (<= ?A char))))

(defsubst flx-is-boundary (last-char char)
  (or (and (not (flx-is-capital last-char))
           (flx-is-capital char))
      (null last-char)
      (and (not (flx-is-word last-char))
           (flx-is-word char))))

(defsubst flx-inc-vec (vec &optional inc beg end)
  "increment each element of vectory by INC(default=1)
from BEG (inclusive) to end (not inclusive).
"
  (or inc
      (setq inc 1))
  (or beg
      (setq beg 0))
  (or end
      (setq end (length vec)))
  (while (< beg end)
    (incf (aref vec beg) inc)
    (incf beg))
  vec)

;; So we store one fixnum per character.  Is this too memory inefficient?
(defun flx-get-heatmap-str (str &optional group-separator)
  "Generate heat map vector of string.

See documentation for logic."
  (let* ((str-len (length str))
         (str-last-index (1- str-len))
         ;; ++++ base
         (scores (make-vector str-len -35))
         (penalty-lead ?.)
         (groups-alist (list (list -1 0))))
    ;; ++++ final char bonus
    (incf (aref scores str-last-index) 1)
    ;; Establish baseline mapping
    (loop for char across str
          for index from 0
          with last-char = nil
          with group-word-count = 0
          do (progn
               (let ((effective-last-char
                      ;; before we find any words, all separaters are
                      ;; considered words of length 1.  This is so "foo/__ab"
                      ;; gets penalized compared to "foo/ab".
                      (if (zerop group-word-count) nil last-char)))
                 (when (flx-is-boundary effective-last-char char)
                   (setcdr (cdar groups-alist) (cons index (cddar groups-alist))))
                 (when (and (not (flx-is-word last-char))
                            (flx-is-word char))
                   (incf group-word-count)))
               ;; ++++ -45 penalize extension
               (when (eq last-char penalty-lead)
                 (incf (aref scores index) -45))
               (when (eq group-separator char )
                 (setcar (cdar groups-alist) group-word-count)
                 (setq group-word-count 0)
                 (push (nconc (list index group-word-count)) groups-alist))
               (if (= index str-last-index)
                   (setcar (cdar groups-alist) group-word-count)
                 (setq last-char char))))
    (let* ((group-count (length groups-alist))
           (separator-count (1- group-count)))
      ;; ++++ slash group-count penalty
      (unless (zerop separator-count)
        (flx-inc-vec scores (* -2 group-count)))
      ;; score each group further
      (loop for group in groups-alist
            for index from separator-count downto 0
            with last-group-limit = nil
            do (let ((group-start (car group))
                     (word-count (cadr group))
                     ;; this is the number of effective word groups
                     (words-length (length (cddr group)))
                     (basepath-p (not last-group-limit)))
                 (let (num)
                   (setq num
                         (if basepath-p
                             (+ 35
                                ;; ++++ basepath separator-count boosts
                                (if (> separator-count 1)
                                    (1- separator-count)
                                  0)
                                ;; ++++ basepath word count penalty
                                (- word-count))
                           ;; ++++ non-basepath penalties
                           (if (= index 0)
                               -3
                             (+ -5 (1- index)))))
                   (flx-inc-vec scores num (1+ group-start) last-group-limit))
                 (loop for word in (cddr group)
                       for word-index from (1- words-length) downto 0
                       with last-word = (or last-group-limit
                                            str-len)
                       do (progn
                            (incf (aref scores word)
                                  ;; ++++  beg word bonus AND
                                  85)
                            (loop for index from word below last-word
                                  for char-i from 0
                                  do (incf (aref scores index)
                                           (-
                                            ;; ++++ word order penalty
                                            (* -3 word-index)
                                            ;; ++++ char order penalty
                                            char-i)))
                            (setq last-word word)))
                 (setq last-group-limit (1+ group-start)))))
    scores))

(defun flx-get-heatmap-file (filename)
  "Return heatmap vector for filename."
  (flx-get-heatmap-str filename ?/))


(defsubst flx-bigger-sublist (sorted-list val)
  "return sublist bigger than VAL from sorted SORTED-LIST

  if VAL is nil, return entire list."
  (if val
      (loop for sub on sorted-list
            do (when (> (car sub) val)
                 (return sub)))
      sorted-list))

(defun flx-get-matches (hash query &optional greater-than q-index)
  "Return list of all unique indexes into str where query can match.

That is all character sequences of query that occur in str are returned.

HASH accept as the cached analysis of str.
sstr
e.g. (\"aab\" \"ab\") returns
       '((0 2) (1 2)
"

  (setq q-index (or q-index 0))
  (let* ((q-char (aref query q-index))
         (indexes (flx-bigger-sublist
                   (gethash q-char hash) greater-than)))
    (if (< q-index (1- (length query)))
        (apply                        ; `mapcan'
         'nconc
         (mapcar
          (lambda (index)
            (let ((next-matches-for-rest (flx-get-matches hash query  index (1+ q-index))))
              (when next-matches-for-rest
                (mapcar (lambda (match)
                          (cons index match))
                        next-matches-for-rest))))
          indexes))
      (mapcar 'list indexes))))

(defun flx-make-filename-cache ()
  "Return cache hashtable appropraite for storeing filenames."
  (flx-make-string-cache 'flx-get-heatmap-file))

(defun flx-make-string-cache (&optional heat-func)
  "Return cache hashtable appropraite for storeing strings."
  (let ((hash (make-hash-table :test 'equal
                               :size 4096)))
    (puthash 'heatmap-func (or heat-func 'flx-get-heatmap-str) hash)
    hash))

(defun flx-process-cache (str cache)
  "Get calculated heatmap from cache, add it if necessary."
  (let ((res (when cache
               (gethash str cache))))
    (or res
        (progn
          (setq res (flx-get-hash-for-string
                     str
                     (or (and cache (gethash 'heatmap-func cache))
                         'flx-get-heatmap-str)))
          (when cache
            (puthash str res cache))
          res))))


(defun flx-score (str query &optional cache)
  "return best score matching QUERY against STR"
  (setq query (downcase query))
  (unless (or (zerop (length query))
              (zerop (length str)))
    (let* ((info-hash (flx-process-cache str cache))
           (heatmap (gethash 'heatmap info-hash))
           (matches (flx-get-matches info-hash query))
           (query-length (length query))
           (full-match-boost (and (< query-length 5)
                                  (> query-length 1)))
           (best-score nil))
      (mapc (lambda (match-positions)
              (let ((score (if (and
                                full-match-boost
                                (= (length match-positions)
                                   (length str)))
                               10000
                             0))
                    (contiguous-count 0)
                    last-match)
                (loop for index in match-positions
                      do (progn
                           (if (and last-match
                                    (= (1+ last-match) index))
                               (incf contiguous-count)
                             (setq contiguous-count 0))
                           (incf score (aref heatmap index))
                           (when (> contiguous-count 0)
                             (incf score (+ 45 (* 15 (min contiguous-count 4)))))
                           (setq last-match index)))
                (if (or (null best-score)
                        (> score (car best-score)))
                    (setq best-score (cons score match-positions)))))
            matches)
      best-score)))


(defun flx-propertize (str score &optional add-score)
  "Return propertized string according to score."
  (let ((block-started (cadr score))
        (last-char nil))
    (loop for char in (cdr score)
          do (progn
               (when (and last-char
                          (not (= (1+ last-char) char)))
                 (put-text-property block-started  (1+ last-char) 'face 'flx-highlight-face str)
                 (setq block-started char))
               (setq last-char char)))
    (put-text-property block-started  (1+ last-char) 'face 'flx-highlight-face str)
    (when add-score
      (setq str (format "%s [%s]" str (car score))))
    str))



(defvar flx-file-cache (flx-make-filename-cache)
  "Cached heatmap info about strings.")

(defvar flx-strings-cache (flx-make-string-cache)
  "Cached heatmap info about filenames.")



(provide 'flx)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flx.el ends here
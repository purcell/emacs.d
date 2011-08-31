;;; sb-text.el -- shimbun backend class for text content -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2009
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(luna-define-class shimbun-text (shimbun) ())

;; Fast fill-region function

(defvar shimbun-fill-column (min 80 (- (frame-width) 4)))

(defconst shimbun-kinsoku-bol-list
  (append "!)-_~}]:;',.?、。，．・：；？！゛゜´｀¨＾￣＿ヽヾゝゞ〃\
仝々〆〇ー―‐／＼〜‖｜…‥’”）〕］｝〉》」』】°′″℃ぁぃぅぇぉ\
っゃゅょゎァィゥェォッャュョヮヵヶ" nil))

(defconst shimbun-kinsoku-eol-list
  (append "({[`‘“（〔［｛〈《「『【°′″§" nil))

(defun shimbun-fill-line ()
  (forward-line 0)
  (let ((top (point)) chr)
    (while (if (>= (move-to-column shimbun-fill-column)
		   shimbun-fill-column)
	       (if (memq (char-before) shimbun-kinsoku-eol-list)
		   (prog1
		       t
		     (backward-char)
		     (while (memq (char-before) shimbun-kinsoku-eol-list)
		       (backward-char))
		     (insert "\n"))
		 (while (memq (setq chr (char-after)) shimbun-kinsoku-bol-list)
		   (forward-char))
		 (if (looking-at "\\s-+")
		     (or (eolp) (delete-region (point) (match-end 0)))
		   (or (not chr)
		       (> (char-width chr) 1)
		       (re-search-backward "\\<" top t)
		       (end-of-line)))
		 (if (eolp)
		     nil
		   (insert "\n")
		   t)))
      (setq top (point))))
  (forward-line 1)
  (not (eobp)))

(defun shimbun-shallow-rendering ()
  (goto-char (point-min))
  (while (search-forward "<p>" nil t)
    (insert "\n\n"))
  (goto-char (point-min))
  (while (re-search-forward "<br\\(:? /\\)?>" nil t)
    (insert "\n"))
  (shimbun-remove-markup)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (while (shimbun-fill-line))
  (goto-char (point-min))
  (when (skip-chars-forward "\n")
    (delete-region (point-min) (point)))
  (while (re-search-forward "\n\n\n+" nil t)
    (replace-match "\n\n"))
  (goto-char (point-max))
  (when (skip-chars-backward "\n")
    (delete-region (point) (point-max)))
  (insert "\n"))

(defun shimbun-make-text-contents (shimbun header)
  (shimbun-header-insert-and-buffer-string
   shimbun header nil
   ;; When cleaning has been succeeded, this article is treated as a
   ;; text/plain message.  Otherwise, it is treated as a text/html
   ;; message.
   (if (shimbun-clear-contents shimbun header)
       (shimbun-shallow-rendering)
     t)))

(luna-define-method shimbun-make-contents ((shimbun shimbun-text)
					   header)
  (shimbun-make-text-contents shimbun header))

(provide 'sb-text)

;;; sb-text.el ends here

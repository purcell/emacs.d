;;; org-bullets.el --- Show bullets in org-mode as UTF-8 characters
;; Version: 0.2.2
;; Author: sabof
;; URL: https://github.com/sabof/org-bullets

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The project is hosted at https://github.com/sabof/org-bullets
;; The latest version, and all the relevant information can be found there.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup org-bullets nil
  "Display bullets as UTF-8 characters."
  :group 'org-appearance)

;; A nice collection of unicode bullets:
;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
(defcustom org-bullets-bullet-list
  '(;;; Large
    "◉"
    "○"
    "✸"
    "✿"
    ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
    ;; ► • ★ ▸
    )
  "This variable contains the list of bullets.
It can contain any number of one-character strings.
For levels beyond the size of the list, the stars will be
displayed using the first items again."
  :group 'org-bullets
  :type '(repeat (string :tag "Bullet character")))

(defcustom org-bullets-face-name nil
  "Allows to override `org-mode' bullets face.
If set to a name of a face, that face will be used.
Otherwise the face of the heading level will be used."
  :group 'org-bullets
  :type 'symbol)

(defvar org-bullets-bullet-map
  '(keymap
    (mouse-1 . org-cycle)
    (mouse-2 . (lambda (e)
		 (interactive "e")
		 (mouse-set-point e)
		 (org-cycle))))
  "Mouse events for bullets.
If this is undesirable, one can remove them with

\(setcdr org-bullets-bullet-map nil\)")

(defun org-bullets-level-char (level)
  "Return a character corresponding to LEVEL."
  (string-to-char
   (nth (mod (1- level)
             (length org-bullets-bullet-list))
        org-bullets-bullet-list)))

;;;###autoload
(define-minor-mode org-bullets-mode
  "UTF-8 bullets for `org-mode'."
  nil nil nil
  (let* ((keyword
	  `((,org-outline-regexp-bol
	     (0 (let (( level (- (match-end 0) (match-beginning 0) 1)))
		  (compose-region (- (match-end 0) 2)
				  (- (match-end 0) 1)
				  (org-bullets-level-char level))
		  (when (facep org-bullets-face-name)
		    (put-text-property (- (match-end 0) 2)
				       (- (match-end 0) 1)
				       'face
				       org-bullets-face-name))
		  (put-text-property (match-beginning 0)
				     (- (match-end 0) 2)
				     'face (list :foreground
						 (face-attribute
						  'default :background)))
		  (put-text-property (match-beginning 0)
				     (match-end 0)
				     'keymap
				     org-bullets-bullet-map)
		  nil))))))
    (if org-bullets-mode
        (progn (font-lock-add-keywords nil keyword)
               (font-lock-fontify-buffer))
      (save-excursion
	(goto-char (point-min))
	(font-lock-remove-keywords nil keyword)
	(while (re-search-forward org-outline-regexp-bol nil t)
	  (decompose-region (match-beginning 0) (match-end 0)))
	(font-lock-fontify-buffer)))))

(provide 'org-bullets)

;; Local Variables:
;; coding: utf-8-emacs
;; End:

;;; org-bullets.el ends here

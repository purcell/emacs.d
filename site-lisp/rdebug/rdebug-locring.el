;;; rdebug-locring.el --- Ruby debugger location ring

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-locring.el 769 2008-03-17 14:29:40Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains code dealing with filter of debugger output a large
;; part of which may contain annotations.

;;; Code:

(defvar rdebug-source-location-ring nil
  "Ring of the last `rdebug-source-location-ring-size' positions we've stopped aft.")

(defvar rdebug-source-location-ring-index -1
  "Position in `rdebug-source-location-ring' of where we are.")

(defun rdebug-locring-add (frame location-history-ring)
  "Add FRAME to LOCATION-HISTORY-RING if we are on the top frame and have a frame to add."
  ;; Switching frames shouldn't save a new ring
  ;; position. Also make sure no position is different.
  ;; Perhaps duplicates should be controlled by an option.
  (unless (and (not (ring-empty-p location-history-ring))
	       (equal (ring-ref location-history-ring
				(ring-length location-history-ring)) frame))
    (ring-insert-at-beginning location-history-ring frame)))

(defun rdebug-locring-clear ()
  "Clear out all source locations in `Go to the source location of the first stopping point."
  (interactive)
  (setq rdebug-source-location-ring-index -1)
  (while (not (ring-empty-p rdebug-source-location-ring))
    (ring-remove rdebug-source-location-ring)))

(defun rdebug-locring-goto (ring-position)
  "Go the source position RING-POSITION in the stopping history."
  (interactive "NSource location ring position (0 is oldest): ")
  (with-current-buffer gud-comint-buffer
    (setq rdebug-source-location-ring-index ring-position)
    (let* ((frame (ring-ref rdebug-source-location-ring ring-position))
	   (file (car frame))
	   (line (cdr frame)))
      (when file
	(rdebug-display-line file line)
	(message (format "%d %s:%d" rdebug-source-location-ring-index
			 file line))))))
    
(defun rdebug-locring-newer ()
  "Cycle through source location stopping history to get the next newer (more recently visited) location."
  (interactive)
  (with-current-buffer gud-comint-buffer
    (if (equal (+ 1 rdebug-source-location-ring-index)
	       (ring-length rdebug-source-location-ring))
	(progn
	  (message "At newest - Will set to wrap to oldest.")
	  (setq rdebug-source-location-ring-index -1))
      ;; else
      (rdebug-locring-goto
       (if (> rdebug-source-location-ring-index
	      (ring-length rdebug-source-location-ring))
	   0
	 ;; else
	 (ring-plus1 rdebug-source-location-ring-index
		     (ring-length rdebug-source-location-ring)))))))

(defun rdebug-locring-newest ()
  "Go to the source location of the first stopping point."
  (interactive)
  (rdebug-locring-goto (- (ring-length rdebug-source-location-ring) 1)))
  
(defun rdebug-locring-older ()
  "Cycle through source location stopping history to get the next older (least recently visited) location."
  (interactive)
  (with-current-buffer gud-comint-buffer
    (if (equal rdebug-source-location-ring-index 0)
	(progn
	  (message "At oldest - Will set to wrap to newest.")
	  (setq rdebug-source-location-ring-index
		(+ 1 (ring-length rdebug-source-location-ring))))
      ;; else
      (rdebug-locring-goto
       (if (or (not rdebug-source-location-ring-index)
	       (< rdebug-source-location-ring-index 0))
	   0
	 ;; else
	 (ring-minus1 rdebug-source-location-ring-index
		      (ring-length rdebug-source-location-ring)))))))

(defun rdebug-locring-oldest ()
  "Go to the oldest source position location."
  (interactive)
  (ring-ref rdebug-source-location-ring 0))

(provide 'rdebug-locring)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-locring.el ends here

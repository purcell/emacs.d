;;; wl-template.el --- Draft template feature for Wanderlust.

;; Copyright (C) 1998,1999,2000 Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:

;;; Code:
;;
(require 'elmo-util)
(require 'wl-vars)

;; Variables

(defvar wl-template-default-name "default")
(defvar wl-template-buffer-name "*WL-Template*")
(defvar wl-template-mode-map nil)

(defvar wl-template nil)
(defvar wl-template-cur-num 0)
(defvar wl-template-max-num 0)
(defvar wl-template-draft-buffer nil)
(defvar wl-template-preview nil)

;;; Code

(if wl-template-mode-map
    nil
  (setq wl-template-mode-map (make-sparse-keymap))
  (define-key wl-template-mode-map "p"     'wl-template-prev)
  (define-key wl-template-mode-map "n"     'wl-template-next)
  (define-key wl-template-mode-map "q"     'wl-template-abort)
  (define-key wl-template-mode-map "\r"    'wl-template-set)
  (define-key wl-template-mode-map "\n"    'wl-template-set))

(defun wl-template-preview-p ()
  "Return non-nil when preview template."
  wl-template-preview)

(defun wl-template-apply (name)
  "Apply NAME template to draft."
  (let (template wl-draft-idle-highlight)
    (when name
      (if (string= name "")
	  (setq name wl-template-default-name))
      (when (setq template (cdr (assoc name wl-template-alist)))
	(save-excursion
	  (setq wl-draft-config-variables
		(elmo-uniq-list
		 (nconc wl-draft-config-variables
			(save-excursion
			  (wl-draft-config-exec-sub template)))))
	  ;; rehighlight
	  (if wl-highlight-body-too
	      (let ((beg (point-min))
		    (end (point-max)))
		(put-text-property beg end 'face nil)
		(wl-highlight-message beg end t))))))))

(defun wl-template-mode ()
  "Major mode for Wanderlust template.
See info under Wanderlust for full documentation.

\\{wl-template-mode}

Entering WL-Template mode calls the value of `wl-template-mode-hook'."
  (kill-all-local-variables)
  (setq mode-name "Wl-Template"
	major-mode 'wl-template-mode)
  (use-local-map wl-template-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'wl-template-mode-hook))

(defun wl-template-select (&optional arg)
  "Select template from `wl-template-alist'."
  (interactive "P")
  (unless wl-template-alist
    (error "Please set `wl-template-alist'"))
  (if (not (if arg
	       (not wl-template-visible-select)
	     wl-template-visible-select))
      (wl-template-apply
       (completing-read (format "Template (%s): " wl-template-default-name)
			wl-template-alist))
    (let* ((begin wl-template-default-name)
	   (work wl-template-alist))
      (if (and begin (cdr (assoc begin wl-template-alist)))
	  (while (not (string= (car (car work)) begin))
	    (setq wl-template-cur-num (1+ wl-template-cur-num))
	    (setq work (cdr work))))
      (setq wl-template nil
	    wl-template-cur-num 0
	    wl-template-max-num (length wl-template-alist))
      (setq wl-template-draft-buffer (current-buffer))
      (if (get-buffer-window wl-template-buffer-name)
	  (select-window (get-buffer-window wl-template-buffer-name))
	(let* ((cur-win (selected-window))
	       (size (min
		      (- (window-height cur-win)
			 window-min-height 1)
		      (- (window-height cur-win)
			 (max window-min-height
			      (1+ wl-template-buffer-lines))))))
	  (split-window cur-win (if (> size 0) size window-min-height))
	  ;; goto the bottom of the two...
	  (select-window (next-window))
	  ;; make it display...
	  (let ((pop-up-windows nil))
	    (switch-to-buffer (get-buffer-create wl-template-buffer-name)))))
      (set-buffer wl-template-buffer-name)
      (wl-template-mode)
      (wl-template-show))))

(defun wl-template-show (&optional arg)
  "Show reference INDEX in `wl-template-alist'.
ARG is ignored."			; ARG ignored this version (?)
  (with-current-buffer wl-template-buffer-name
    (let ((buffer-read-only nil)
	  (wl-template-preview t)
	  (mail-header-separator  "--header separater--"))
      (erase-buffer)
      (goto-char (point-min))
      (wl-template-insert
       (setq wl-template (car (nth wl-template-cur-num wl-template-alist)))
       mail-header-separator)
      (wl-highlight-message (point-min) (point-max) t)
      (when wl-highlight-x-face-function
	(funcall wl-highlight-x-face-function
		 (point-min) (re-search-forward mail-header-separator nil t)))
      (setq mode-line-process (concat ":" wl-template))
      (set-buffer-modified-p nil))))

(defun wl-template-next ()
  "Display next reference in other buffer."
  (interactive)
  (if (= wl-template-max-num
	 (setq wl-template-cur-num (1+ wl-template-cur-num)))
      (setq wl-template-cur-num 0))
  (wl-template-show))

(defun wl-template-prev ()
  "Display previous reference in other buffer."
  (interactive)
  (setq wl-template-cur-num (if (zerop wl-template-cur-num)
				(1- wl-template-max-num)
			      (1- wl-template-cur-num)))
  (wl-template-show))

(defun wl-template-abort ()
  "Exit from electric reference mode without inserting reference."
  (interactive)
  (setq wl-template nil)
  (delete-window)
  (kill-buffer wl-template-buffer-name)
  (when (buffer-live-p wl-template-draft-buffer)
    (set-buffer wl-template-draft-buffer)
    (let ((win (get-buffer-window wl-template-draft-buffer)))
      (if win (select-window win)))))

(defun wl-template-set ()
  "Exit from electric reference mode and insert selected reference."
  (interactive)
  (if (and wl-template-confirm
	   (not (y-or-n-p "Are you sure ? ")))
      (message "")
    (delete-window)
    (kill-buffer wl-template-buffer-name)
    (when (buffer-live-p wl-template-draft-buffer)
      (set-buffer wl-template-draft-buffer)
      (wl-template-apply wl-template)
      (let ((win (get-buffer-window wl-template-draft-buffer)))
	(if win (select-window win))))))

(defun wl-template-insert (name &optional mail-header)
  "Insert NAME template.
Set header-separator is MAIL-HEADER."
  (let ((template (cdr (assoc name wl-template-alist)))
	(mail-header-separator (or mail-header
				   mail-header-separator)))
    (when template
      (if mail-header
	  (insert mail-header-separator "\n"))
      (wl-draft-config-exec-sub template))))

(require 'product)
(product-provide (provide 'wl-template) (require 'wl-version))

;;; wl-template.el ends here

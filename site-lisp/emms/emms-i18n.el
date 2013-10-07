;;; emms-i18n.el --- functions for handling coding systems

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Ye Wenbin <wenbinye@163.com>

;; This file is part of EMMS.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; When reading from process, first check the car part of
;; `emms-i18n-default-coding-system'; if non-nil, use this for
;; decoding, and never detect coding system; if nil, first call
;; `emms-i18n-coding-detect-functions' to get coding system, if
;; success, decode the result, otherwise, use
;; `emms-i18n-detect-coding-function', the Emacs detect coding
;; function, if the coding detected is not in
;; `emms-i18n-never-used-coding-system', decode it, otherwise use
;; locale-coding-system.

;; When writing/sending data to process, first check the cdr part of
;; `emms-i18n-default-coding-system', if non-nil, use this to encode
;; data, otherwise do nothing, that means use
;; `default-process-coding-system' or `process-coding-system-alist' to
;; encode data.

;; Put this file into your load-path and the following into your
;; ~/.emacs:

;;   (require 'emms-i18n)

;;; Code:

(provide 'emms-i18n)
(eval-when-compile
  (require 'cl))

;; TODO: Use defcustom.
(defvar emms-i18n-never-used-coding-system
  '(raw-text undecided)
  "If the `emms-i18n-coding-detect-functions' return a coding
system in this list, use `emms-i18n-default-coding-system'
instead.")

;; TODO: Use defcustom.
(defvar emms-i18n-coding-system-for-read
  'utf-8
  "If coding detect fails, use this for decoding.")

;; TODO: Use defcustom.
(defvar emms-i18n-default-coding-system
  '(no-conversion . no-conversion)
  "If non-nil, use this for decoding and encoding.")

;; TODO: Use defcustom.
(defvar emms-i18n-coding-detect-functions
  nil
  "A list of functions to call to detect codings.")

;; TODO: Use defcustom.
(defvar emms-i18n-detect-max-size
  10000
  "Maximum amount of bytes to detect the coding system.  nil
means to scan the whole buffer.")

(defun emms-i18n-iconv (from to str)
  "Convert string STR from FROM coding to TO coding."
  (if (and from to)
      (decode-coding-string
       (encode-coding-string str to)
       from)
    str))

(defun emms-i18n-iconv-region (beg end from to)
  (when (and from to)
    (save-restriction
      (narrow-to-region beg end)
      (encode-coding-region (point-min) (point-max) to)
      (decode-coding-region (point-min) (point-max) from))))

(defun emms-i18n-iconv-buffer (from to &optional buf)
  "Convert buffer BUF from FROM coding to TO coding.  BUF
defaults to the current buffer."
  (save-excursion
    (and buf (set-buffer buf))
    (emms-i18n-iconv-region (point-min) (point-max) from to)))

(defun emms-i18n-set-default-coding-system (read-coding write-coding)
  "Set `emms-i18n-default-coding-system'."
  (interactive "zSet coding system for read: \nzSet coding system for write: ")
  (setq emms-i18n-default-coding-system
        (cons
         (and (coding-system-p read-coding) read-coding)
         (and (coding-system-p write-coding) write-coding)))
  (message (concat
            (if (car emms-i18n-default-coding-system)
                (format "The coding system for reading is %S." (car emms-i18n-default-coding-system))
              "Good, you want me to detect the coding system!")
            (format " The coding system for writing is %S."
                    (or (cdr emms-i18n-default-coding-system)
                        (cdr default-process-coding-system))))))

(defun emms-i18n-call-process-simple (&rest args)
  "Run a program and return the program result.
If the car part of `emms-i18n-default-coding-system' is non-nil,
the program result will be decoded using the car part of
`emms-i18n-default-coding-system'.  Otherwise, use
`emms-i18n-coding-detect-functions' to detect the coding system
of the result.  If the `emms-i18n-coding-detect-functions'
failed, use `emms-i18n-detect-coding-function' to detect coding
system.  If all the coding systems are nil or in
`emms-i18n-never-used-coding-system', decode the result using
`emms-i18n-coding-system-for-read'.

ARGS are the same as in `call-process', except BUFFER should
always have the value t.  Otherwise the coding detection will not
be performed."
  (let ((default-process-coding-system (copy-tree default-process-coding-system))
        (process-coding-system-alist nil) exit pos)
    (when (eq (nth 2 args) 't)
      (setcar default-process-coding-system (car emms-i18n-default-coding-system))
      (setq pos (point)))
    (setq exit (apply 'call-process args))
    (when (and (eq (nth 2 args) 't)
               (eq (car emms-i18n-default-coding-system) 'no-conversion))
      (save-restriction
        (narrow-to-region pos (point))
        (decode-coding-region (point-min) (point-max) (emms-i18n-detect-buffer-coding-system))))
    exit))

;; TODO: Is this function useful?
(defun emms-i18n-call-process (&rest args)
  "Run the program like `call-process'.  If the cdr part of
`emms-i18n-default-coding-system' is non-nil, the string in ARGS
will be encoded by the cdr part of
`emms-i18n-default-coding-system'; otherwise, all parameters are
simply passed to `call-process'."
  (with-temp-buffer
    (if (cdr emms-i18n-default-coding-system)
        (let ((default-process-coding-system emms-i18n-default-coding-system)
              (process-coding-system-alist nil))
          (apply 'call-process args))
      (apply 'call-process args))))

(defun emms-i18n-detect-coding-function (size)
  (detect-coding-region (point)
                        (+ (if (null emms-i18n-detect-max-size)
                               size
                             (min size emms-i18n-detect-max-size))
                           (point)) t))

(defun emms-i18n-detect-buffer-coding-system (&optional buf)
  "Before calling this function, make sure the buffer is literal."
  (let ((size (- (point-max) (point-min)))
        (func (append emms-i18n-coding-detect-functions 'emms-i18n-detect-coding-function))
        coding)
    (save-excursion
      (and buf (set-buffer buf))
      (goto-char (point-min))
      (when (> size 0)
        (setq coding (run-hook-with-args-until-success 'func size))
        (if (member (coding-system-base coding) emms-i18n-never-used-coding-system)
            (setq coding (emms-i18n-detect-coding-function size))))
      (if (or (null coding) (member (coding-system-base coding) emms-i18n-never-used-coding-system))
          emms-i18n-coding-system-for-read
        coding))))

;;; emms-i18n.el ends here

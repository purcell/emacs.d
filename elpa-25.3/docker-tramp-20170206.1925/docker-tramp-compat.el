;;; docker-tramp-compat.el --- TRAMP integration for docker containers  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/docker-tramp.el
;; Keywords: docker, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;;; Code:
(require 'tramp-sh)

(when (version< tramp-version "2.3")
;; Overwrite `tramp-wait-for-output' to work with Alpine busy boxes in Tramp<2.3
;;
;; See:
;; + https://lists.gnu.org/archive/html/tramp-devel/2016-05/msg00000.html
;; + http://git.savannah.gnu.org/cgit/tramp.git/commit/?id=98a511248a9405848ed44de48a565b0b725af82c
(defconst tramp-device-escape-sequence-regexp "\e[[0-9]+n"
  "Terminal control escape sequences for device status.")

(defun tramp-wait-for-output (proc &optional timeout)
  "Wait for output from remote command."
  (unless (buffer-live-p (process-buffer proc))
    (delete-process proc)
    (tramp-error proc 'file-error "Process `%s' not available, try again" proc))
  (with-current-buffer (process-buffer proc)
    (let* (;; Initially, `tramp-end-of-output' is "#$ ".  There might
	   ;; be leading escape sequences, which must be ignored.
	   ;; Busyboxes built with the EDITING_ASK_TERMINAL config
	   ;; option send also escape sequences, which must be
	   ;; ignored.
	   (regexp (format "[^#$\n]*%s\\(%s\\)?\r?$"
			   (regexp-quote tramp-end-of-output)
			   tramp-device-escape-sequence-regexp))
	   ;; Sometimes, the commands do not return a newline but a
	   ;; null byte before the shell prompt, for example "git
	   ;; ls-files -c -z ...".
	   (regexp1 (format "\\(^\\|\000\\)%s" regexp))
	   (found (tramp-wait-for-regexp proc timeout regexp1)))
      (if found
	  (let (buffer-read-only)
	    ;; A simple-minded busybox has sent " ^H" sequences.
	    ;; Delete them.
	    (goto-char (point-min))
	    (when (re-search-forward "^\\(.\b\\)+$" (point-at-eol) t)
	      (forward-line 1)
	      (delete-region (point-min) (point)))
	    ;; Delete the prompt.
	    (goto-char (point-max))
	    (re-search-backward regexp nil t)
	    (delete-region (point) (point-max)))
	(if timeout
	    (tramp-error
	     proc 'file-error
	     "[[Remote prompt `%s' not found in %d secs]]"
	     tramp-end-of-output timeout)
	  (tramp-error
	   proc 'file-error
	   "[[Remote prompt `%s' not found]]" tramp-end-of-output)))
      ;; Return value is whether end-of-output sentinel was found.
      found)))
)

(provide 'docker-tramp-compat)

;;; docker-tramp-compat.el ends here

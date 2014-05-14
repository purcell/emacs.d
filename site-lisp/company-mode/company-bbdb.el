;;; company-bbdb.el --- company-mode completion back-end for BBDB in message-mode

;; Copyright (C) 2013-2014  Free Software Foundation, Inc.

;; Author: Jan Tatarik <jan.tatarik@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'company)
(eval-when-compile (require 'cl))

(declare-function bbdb-record-get-field "bbdb")
(declare-function bbdb-records "bbdb")
(declare-function bbdb-dwim-mail "bbdb-com")
(declare-function bbdb-search "bbdb-com")

;;;###autoload
(defun company-bbdb (command &optional arg &rest ignore)
  "`company-mode' completion back-end for `bbdb'."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-bbdb))
    (prefix (and (eq major-mode 'message-mode)
                 (featurep 'bbdb-com)
                 (looking-back "^\\(To\\|Cc\\|Bcc\\):.*"
                               (line-beginning-position))
                 (company-grab-symbol)))
    (candidates (mapcan (lambda (record)
                          (mapcar (lambda (mail) (bbdb-dwim-mail record mail))
                                  (bbdb-record-get-field record 'mail)))
                        (bbdb-search (bbdb-records) arg nil arg)))
    (sorted t)
    (no-cache t)))

(provide 'company-bbdb)
;;; company-bbdb.el ends here

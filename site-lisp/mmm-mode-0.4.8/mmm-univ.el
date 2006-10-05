;;; mmm-univ.el --- The "Universal" Submode Class

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: Michael Abraham Shulman <mas@kurukshetra.cjb.net>

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file defines the "universal" submode class, the default value
;; of `mmm-global-classes', which specifies a standard way to indicate
;; that part of a buffer should be in a different mode--for example,
;; in an email message.

;;; Code:

(require 'mmm-auto)
(require 'mmm-vars)

(defun mmm-univ-get-mode (string)
  (string-match "[a-zA-Z-]+" string)
  (setq string (match-string 0 string))
  (let ((modestr (intern (if (string-match "mode\\'" string)
                             string
                           (concat string "-mode")))))
    (or (mmm-ensure-modename modestr)
        (signal 'mmm-no-matching-submode nil))))

(mmm-add-classes
 `((universal
    :front "{%\\([a-zA-Z-]+\\)%}"
    :back "{%/~1%}"
    :insert ((?/ universal "Submode: " @ "{%" str "%}" @ "\n" _ "\n"
                 @ "{%/" str "%}" @))
    :match-submode mmm-univ-get-mode
    :save-matches 1
    )))

(provide 'mmm-univ)


;;; Local Variables:
;;; mmm-global-classes: nil
;;; End:

;;; mmm-univ.el ends here
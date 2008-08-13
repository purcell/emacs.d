;;; cit-srec.el --- Test SRecode template mapping and such.

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cit-srec.el,v 1.1 2008/02/24 18:21:56 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; SRecode Testing

;;; Code:

(defun cit-srecode-map-test ()
  "Test SRecode MAP path testintg."
  (interactive)

  (let ((extradir cit-src-dir)
	(oldpath srecode-map-load-path))
    (add-to-list 'srecode-map-load-path extradir)
    (srecode-map-update-map t)

    (srecode-load-tables-for-mode 'c++-mode)
    (srecode-load-tables-for-mode 'c++-mode 'cit-test)

    (unwind-protect
	(when (not (srecode-template-get-table (srecode-table 'c++-mode)
					       "cit-test-template"
					       "test"
					       'cit-test
					       ))
	  (error "Failed to find augmented template"))

      ;; Get rid of our adaptation.  Double check.
      (setq srecode-map-load-path oldpath)
      (srecode-map-update-map t))

;; This would be nice, but I'd have to purge and rebuild the table.
;; to do it, which is a waste for a feature few would ever need.
;;
;;    (when (srecode-template-get-table (srecode-table 'c++-mode)
;;				      "cit-test-template"
;;				      "test"
;;				      'cit-test
;;				      )
;;      (error "Failed to unload augmented template"))

    ))


(provide 'cit-srec)
;;; cit-srec.el ends here

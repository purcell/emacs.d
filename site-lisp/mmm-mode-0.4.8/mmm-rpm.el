;;; mmm-rpm.el --- MMM submode class for RPM spec files

;; Copyright (C) 2000 by Marcus Harnisch <Marcus.Harnisch@gmx.net>

;; Author:  Marcus Harnisch <Marcus.Harnisch@gmx.net>
;; Version: $Id: mmm-rpm.el,v 1.3 2001/01/11 00:56:30 mas Exp $

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

;; This file contains the definition of an MMM Mode submode class for
;; editing shell script sections within RPM (Redhat Package Manager)
;; spec files. I recommend to use it in combination with
;; rpm-spec-mode.el by Stig Bjørlykke <stigb@tihlde.hist.no> and Steve
;; Sanbeg <sanbeg@dset.com> (http://www.xemacs.org/~stigb/rpm-spec-mode.el)

;;; Installation:

;; 1. Copy this file where Emacs can find it.
;;
;; 2. Add the following lines to one of your startup files (e.g. ~/.emacs):
;;
;;   (add-to-list 'mmm-mode-ext-classes-alist
;;                '(rpm-spec-mode "\\.spec\\'" rpm-sh))

;;; Code:

(require 'mmm-auto)

(defconst mmm-rpm-sh-start-tags
  '("prep" "build" "install" "clean" "preun" "postun" "pre"
    "post" "triggerin" "triggerun" "triggerpostun")
  "List containing RPM tags that start a shell-script section in a spec file")

(defvar mmm-rpm-sh-end-tags
  (append '("files" "description" "package") mmm-rpm-sh-start-tags)
  "List containing RPM tags that end a shell-script section in a spec file")

(defvar mmm-rpm-sh-start-regexp
  (concat "^%" (mmm-regexp-opt mmm-rpm-sh-start-tags t) "\\b.*$")
  "Regexp matching RPM tags that start a shell-script section in a spec file")

(defvar mmm-rpm-sh-end-regexp
  (concat "\\'\\|^%" (mmm-regexp-opt mmm-rpm-sh-end-tags t) "\\b.*$")
  "Regexp matching RPM tags that end a shell-script section in a spec file")

(mmm-add-group
 'rpm
 `((rpm-sh
    :submode sh-mode
    :face mmm-code-submode-face
    ;; match tags that starts sh-script region
    :front ,mmm-rpm-sh-start-regexp
    ;; match end of buffer or next tag that ends sh-script region
    :back ,mmm-rpm-sh-end-regexp
    :front-offset 1
    :back-offset 0
    :save-matches 0
    )))

(provide 'mmm-rpm)

;;; mmm-rpm.el ends here
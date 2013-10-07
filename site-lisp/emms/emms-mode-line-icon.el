;; emms-mode-line-icon.el --- show an icon in the Emacs mode-line

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Version: 1.1
;; Keywords: emms

;; Author: Daniel Brockman <daniel@brockman.se>
;; Maintainer: Lucas Bonnet <lucas@rincevent.net>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;; Commentary: 

;; This EMMS extension shows an icon in the mode-line next to the
;; info-tag.

;; Code:

(require 'emms-mode-line)

(defvar emms-mode-line-icon-color "black"
  "Color of the little icon displayed in the mode-line.")

(defvar emms-mode-line-icon-before-format ""
  "String to put before the icon, in the mode-line.
For example, if you want to have something like :
\[ <icon> Foo - The Foo Song ]
You should set it to \"[\", and set emms-mode-line-format to \"%s ]\"")

(defvar emms-mode-line-icon-image-cache
  `(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " emms-mode-line-icon-color  "\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\"};")))


(defun emms-mode-line-icon-function ()
  (concat " "
          emms-mode-line-icon-before-format
          (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-playlist-current)))

(setq emms-mode-line-mode-line-function 'emms-mode-line-icon-function)

;; This is needed for text properties to work in the mode line.
(put 'emms-mode-line-string 'risky-local-variable t)

(provide 'emms-mode-line-icon)
;;; emms-mode-line-icone.el ends here

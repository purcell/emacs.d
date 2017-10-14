;;; mmm-defaults.el --- Friendly defaults for MMM Mode

;; Copyright (C) 2013 by Dmitry Gutov

;; Author: Dmitry Gutov <dgutov@yandex.ru>

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

;;; Commentary:

;; To enable multiple mode support in ERB, EJS and PHP files, just add the
;; following line to your init file:
;;
;; (require 'mmm-defaults)
;;
;; Note that for PHP you still need to have php-mode (installed separately).
;;
;; TODO: Add more file types and classes here. Mention this file in README.

;;; Code:

(require 'mmm-auto)

(setq mmm-global-mode 'auto)

;;; ERB and EJS

(mmm-add-mode-ext-class 'html-erb-mode "\\.erb\\'" 'erb)
(mmm-add-mode-ext-class 'html-erb-mode "\\.ejs\\'" 'ejs)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("/[^.]+\\.erb\\'"  . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

;;; PHP

(mmm-add-mode-ext-class 'html-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-mode nil 'html-css)
(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
(add-to-list 'auto-mode-alist '("\\.html\\.php\\'" . html-mode))
(add-to-list 'auto-mode-alist '("/[^.]+\\.php\\'"  . html-mode))

(provide 'mmm-defaults)

;;; mmm-defaults.el ends here

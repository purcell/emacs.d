;;; erc-viper.el --- Viper compatibility hacks for ERC

;; Copyright (C) 2005, 2007, 2008 Free Software Foundation, Inc.

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: emulation

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Viper is a VI emulation mode for Emacs. ERC and Viper don't quite get
;; along by default; the code in this file fixes that. A simple
;;   (require 'erc-viper)
;; in your ~/.ercrc.el should be all it takes for you to use ERC and
;; Viper together happily.

;;; Code:

(require 'viper)

;; We need this for `erc-mode-hook' and `erc-buffer-list'. Perhaps it
;; would be better to use an `eval-after-load', so that there could be
;; some autodetection / loading of this file from within erc.el?
(require 'erc)

;; Fix RET in ERC buffers, by telling Viper to pass RET through to the
;; normal keymap. Do this conditionally, as this version of Viper may
;; already do this for us.
(unless (assoc 'erc-mode viper-major-mode-modifier-list)
  (add-to-list 'viper-major-mode-modifier-list
               '(erc-mode insert-state viper-comint-mode-modifier-map))
  (add-to-list 'viper-major-mode-modifier-list
               '(erc-mode vi-state viper-comint-mode-modifier-map))
  (viper-apply-major-mode-modifiers))

;; Ensure that ERC buffers come up in insert state.
(add-to-list 'viper-insert-state-mode-list 'erc-mode)

;; Fix various local variables in Viper.
(add-hook 'erc-mode-hook 'viper-comint-mode-hook)

;; Fix ERC buffers that already exist (buffers in which `erc-mode-hook'
;; has already been run).
(mapc (lambda (buf)
        (with-current-buffer buf
          (viper-comint-mode-hook)
          ;; If there *is* a final newline in this buffer, delete it, as
          ;; it interferes with ERC /-commands.
          (let ((last (1- (point-max))))
            (when (eq (char-after last) ?\n)
              (goto-char last)
              (delete-char 1)))))
      (erc-buffer-list))

(provide 'erc-viper)

;; arch-tag: 659fa645-e9ad-428c-ad53-8304d9f900f6
;;; erc-viper.el ends here

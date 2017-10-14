;;; expand-region-custom.el --- Increase selected region by semantic units.

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region

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

;; This file holds customization variables.

;;; Code:

;;;###autoload
(defgroup expand-region nil
  "Increase selected region by semantic units."
  :group 'tools)

;;;###autoload
(defcustom expand-region-preferred-python-mode 'python
  "The name of your preferred python mode"
  :group 'expand-region
  :type '(choice (const :tag "Emacs' python.el" 'python)
                 (const :tag "fgallina's python.el" 'fgallina-python)
                 (const :tag "python-mode.el" 'python-mode)))

;;;###autoload
(defcustom expand-region-guess-python-mode t
  "If expand-region should attempt to guess your preferred python mode"
  :group 'expand-region
  :type '(choice (const :tag "Guess" t)
                 (const :tag "Do not guess" nil)))

(defun expand-region-guess-python-mode ()
  "Guess the user's preferred python mode."
  (setq expand-region-preferred-python-mode
        (if (fboundp 'python-setup-brm)
            'python
          'fgallina-python)))

;;;###autoload
(defcustom expand-region-autocopy-register ""
  "If set to a string of a single character (try \"e\"), then the
contents of the most recent expand or contract command will
always be copied to the register named after that character."
  :group 'expand-region
  :type 'string)

;;;###autoload
(defcustom expand-region-skip-whitespace t
  "If expand-region should skip past whitespace on initial expansion"
  :group 'expand-region
  :type '(choice (const :tag "Skip whitespace" t)
                 (const :tag "Do not skip whitespace" nil)))

;;;###autoload
(defcustom expand-region-fast-keys-enabled t
  "If expand-region should bind fast keys after initial expand/contract"
  :group 'expand-region
  :type '(choice (const :tag "Enable fast keys" t)
                 (const :tag "Disable fast keys" nil)))

;;;###autoload
(defcustom expand-region-contract-fast-key "-"
  "Key to use after an initial expand/contract to contract once more."
  :group 'expand-region
  :type 'string)

;;;###autoload
(defcustom expand-region-reset-fast-key "0"
  "Key to use after an initial expand/contract to undo."
  :group 'expand-region
  :type 'string)

;;;###autoload
(defcustom expand-region-exclude-text-mode-expansions
  '(html-mode nxml-mode)
  "List of modes which derive from `text-mode' for which text mode expansions are not appropriate."
  :group 'expand-region
  :type '(repeat (symbol :tag "Major Mode" unknown)))

;;;###autoload
(defcustom expand-region-smart-cursor nil
  "Defines whether the cursor should be placed intelligently after expansion.

If set to t, and the cursor is already at the beginning of the new region,
keep it there; otherwise, put it at the end of the region.

If set to nil, always place the cursor at the beginning of the region."
  :group 'expand-region
  :type '(choice (const :tag "Smart behaviour" t)
                 (const :tag "Standard behaviour" nil)))

(provide 'expand-region-custom)

;;; expand-region-custom.el ends here

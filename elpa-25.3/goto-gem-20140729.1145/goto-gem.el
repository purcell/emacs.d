;;; goto-gem.el --- Open dired in gem directory

;; Copyright (C) 2014  Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Keywords: gemfile, convenience
;; Package-Version: 20140729.1145
;; Version: 1.2
;; Package-Requires: ((s "1.9.0"))

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

;; Use M-x goto-gem to find a specific (installed) gem in dired.

;;; Code:

(require 'grep)

(defun goto-gem--gem-name (string)
 "Parse gem name from STRING."
 (car (split-string string)))

(defun goto-gem--installed-gems ()
 "Available gem names."
 (mapcar 'goto-gem--gem-name
  (process-lines "gem" "list")))

(defun goto-gem--directory ()
 "Gem directory."
 (format "%s/gems" (s-trim-right (shell-command-to-string "gem environment gemdir"))))

(defun goto-gem--read-gem ()
 "Prompt for gem."
 (completing-read "Gem: " (goto-gem--installed-gems)))

(defun goto-gem--read-gem-version (gem)
 "Prompt for GEM version."
 (completing-read
  "Version: "
  (mapcar #'cadr
   (s-match-strings-all
    "[ (]\\([0-9.]+\\)[,)]"
    (shell-command-to-string (format "gem list | grep -E '^%s \\('" gem))))))

;;;###autoload
(defun goto-gem ()
 "Navigate (dired) to gem directory."
 (interactive)
 (let* ((gem (goto-gem--read-gem))
        (version (goto-gem--read-gem-version gem)))
  (dired (format "%s/%s-%s" (goto-gem--directory) gem version))))

;;;###autoload
(defun goto-gem-grep-gem ()
 "Grep specified gem."
 (interactive)
 (let* ((gem (goto-gem--read-gem))
        (version (goto-gem--read-gem-version gem))
        (directory (format "%s/%s-%s" (goto-gem--directory) gem version))
        (query (read-string "Grep for: " (thing-at-point 'symbol))))
  (when (and gem version query)
   (grep-compute-defaults)
   (rgrep query "*.*" directory))))

;;;###autoload
(defun goto-gem-grep-all-gems ()
 "Grep in all gems."
 (interactive)
 (grep-compute-defaults)
 (rgrep
  (read-string "Grep for: " (thing-at-point 'symbol))
  "*.*"
  (goto-gem--directory)))

(provide 'goto-gem)
;;; goto-gem.el ends here

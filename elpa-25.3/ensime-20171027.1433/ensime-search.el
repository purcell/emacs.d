;;; ensime-search.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'dash)
(require 'ensime-helm)
(require 'ensime-ivy)

(defvar ensime-search-interface)
(defvar ensime-buffer-connection)

(autoload 'ensime-helm-search "ensime-helm")

(defvar ensime-search-buffer-name "*ensime-search*")

(defvar ensime-search-min-length 2
  "The minimum length a search must be
 before rpc call is placed..")

(defvar ensime-search-max-results 50
  "The max number of results to return per rpc call.")

(defun ensime-search (&optional arg)
  "ENSIME indexer search."
  (interactive "P")
  (if (equal arg '(4))
      (ensime-search-classic)
    (pcase ensime-search-interface
      (`classic
       (ensime-search-classic))
      (`helm
       (if (featurep 'helm)
           (ensime-helm-search)
         (message "Please ensure helm is installed and loaded.")))
      (`ivy
       (if (featurep 'ivy)
           (ensime-search-ivy)
         (message "Please ensure ivy is installed and loaded."))))))

(defun ensime-search-classic ()
  "Does a search an displays the result in a grep buffer."
  (interactive)
  (let* ((search-string (read-string "Search: "
                                     (substring-no-properties (or (word-at-point) ""))))
         (search-results (ensime-rpc-public-symbol-search
                          (split-string search-string " ") ensime-search-max-results)))
    (switch-to-buffer (get-buffer-create ensime-search-buffer-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (concat "Ensime search results for " search-string ":"  "\n\n"))
    (dolist (search-result search-results)
      (let* ((pos (ensime-search-sym-pos search-result))
             (formatted-pos (ensime-format-source-position pos))
             (name (ensime-search-sym-name search-result)))
        (insert formatted-pos)
        (insert ": ")
        (insert name)
        (insert "\n")))
    (goto-char 0)
    (grep-mode)))

(defun ensime-search-jump-to-item (item)
  "Opens the item in a new buffer if the item has a source location"
  ;; jump there..
  (let ((pos (ensime-search-sym-pos item)))
    (let* ((file-name (ensime-pos-file pos))
           (line (ensime-pos-line pos))
           (offset (ensime-pos-offset pos)))
      (if (and file-name
               (integerp (string-match
                          "\\.scala$\\|\\.java$"
                          file-name)))
          (progn
            (find-file file-name)
            (cond
             (offset (goto-char (ensime-internalize-offset offset)))
             (line (ensime-goto-line line))))))))

(provide 'ensime-search)

;; Local Variables:
;; End:


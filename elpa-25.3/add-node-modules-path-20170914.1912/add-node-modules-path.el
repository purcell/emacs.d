;;; add-node-modules-path.el --- Add node_modules to your exec-path

;; Copyright (C) 2016 Neri Marschik
;; This package uses the MIT License.
;; See the LICENSE file.

;; Author: Neri Marschik <marschik_neri@cyberagent.co.jp>
;; Version: 1.0
;; Package-Version: 20170914.1912
;; Package-Requires: ()
;; Keywords: javascript, node, node_modules, eslint
;; URL: https://github.com/codesuki/add-node-modules-path

;;; Commentary:
;;
;; This file provides `add-node-modules-path', which searches
;; the current files parent directories for the `node_modules/.bin/' directory
;; and adds it to the buffer local `exec-path'.
;; This allows Emacs to find project based installs of e.g. eslint.
;;
;; Usage:
;;     M-x add-node-modules-path
;;
;;     To automatically run it when opening a new buffer:
;;     (Choose depending on your favorite mode.)
;;
;;     (eval-after-load 'js-mode
;;       '(add-hook 'js-mode-hook #'add-node-modules-path))
;;
;;     (eval-after-load 'js2-mode
;;       '(add-hook 'js2-mode-hook #'add-node-modules-path))

;;; Code:

;;;###autoload
(defvar add-node-modules-path-debug nil
  "Enable verbose output when non nil.")

;;;###autoload
(defun add-node-modules-path ()
  "Search the current buffer's parent directories for `node_modules/.bin`.
If it's found, then add it to the `exec-path'."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name "node_modules/.bin/" root))))
    (if root
        (progn
          (make-local-variable 'exec-path)
          (add-to-list 'exec-path path)
          (when add-node-modules-path-debug
            (message (concat "added " path  " to exec-path"))))
      (when add-node-modules-path-debug
        (message (concat "node_modules not found in " root))))))

(provide 'add-node-modules-path)

;;; add-node-modules-path.el ends here

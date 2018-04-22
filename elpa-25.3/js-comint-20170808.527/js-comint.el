;;; js-comint.el --- Run a JavaScript interpreter in an inferior process window.

;;; Copyright (C) 2008 Paul Huff
;;; Copyright (C) 2015 Stefano Mazzucco
;;; Copyright (C) 2016-2017 Chen Bin

;;; Author: Paul Huff <paul.huff@gmail.com>, Stefano Mazzucco <MY FIRST NAME - AT - CURSO - DOT - RE>
;;; Maintainer: Chen Bin <chenbin.sh AT gmail DOT com>
;;; Created: 15 Feb 2014
;;; Version: 1.1.0
;; Package-Version: 20170808.527
;;; URL: https://github.com/js-comint/js-comint
;;; Package-Requires: ()
;;; Keywords: javascript, node, inferior-mode, convenience

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

;; This program is a comint mode for Emacs which allows you to run a
;; compatible javascript repl like Node.js/Spidermonkey/Rhino inside Emacs.
;; It also defines a few functions for sending javascript input to it
;; quickly.

;; Usage:
;;  Put js-comint.el in your load path
;;  Add (require 'js-comint) to your .emacs or ~/.emacs.d/init.el
;;
;;  Optionally, set the `js-comint-program-command' string
;;  and the `js-comint-program-arguments' list to the executable that runs
;;  the JS interpreter and the arguments to pass to it respectively.
;;  Do: `M-x js-comint-repl'
;;  Away you go.
;;  `node_modules' is *automatically* searched and appended into environment
;;  variable `NODE_PATH'. So 3rd party javascript is usable out of box.

;;  If you have nvm, you can select the versions of node.js installed and run
;;  them.  This is done thanks to nvm.el.
;;  Please note nvm.el is optional. So you need *manually* install it.
;;  To enable nvm support, run `js-do-use-nvm'.
;;  The first time you start the JS interpreter with `js-comint-repl', you will
;;be asked to select a version of Node.js
;;  If you want to change version of node js, run `js-comint-select-node-version'
;;
;;  `js-comint-clear' clears the content of REPL.
;;
;; You may get cleaner output by following setup (highly recommended):
;;
;;  Output matching `js-comint-drop-regexp' will be dropped silently
;;
;;  You can add the following lines to your .emacs to take advantage of
;;  cool keybindings for sending things to the javascript interpreter inside
;;  of Steve Yegge's most excellent js2-mode.
;;
;;   (add-hook 'js2-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
;;               (local-set-key (kbd "C-c b") 'js-send-buffer)
;;               ))

;;; Code:

(require 'js)
(require 'comint)
(require 'ansi-color)

(defgroup js-comint nil
  "Run a javascript process in a buffer."
  :group 'js-comint)

(defcustom js-comint-program-command "node"
  "JavaScript interpreter."
  :group 'js-comint)

(defvar js-comint-module-paths '()
  "List of modules paths which could be used by NodeJS
to search modules.")

(defvar js-comint-drop-regexp
  "\\(\x1b\\[[0-9]+[GJK]\\|^[ \t]*undefined[\r\n]+\\)"
  "Regex to silence matching output.")

(defcustom js-comint-program-arguments '()
  "List of command line arguments passed to the JavaScript interpreter."
  :group 'js-comint)

(defcustom js-comint-prompt "> "
  "Prompt used in `js-comint-mode'."
  :group 'js-comint
  :type 'string)

(defcustom js-comint-mode-hook nil
  "*Hook for customizing js-comint mode."
  :type 'hook
  :group 'js-comint)

(defcustom js-use-nvm nil
  "When t, use NVM.  Requires nvm.el."
  :type 'boolean
  :group 'js-comint)

(defvar js-comint-buffer "Javascript REPL"
  "Name of the inferior JavaScript buffer.")

;; process.stdout.columns should be set.
;; but process.stdout.columns in Emacs is infinity because Emacs returns 0 as winsize.ws_col.
;; The completion candidates won't be displayed if process.stdout.columns is infinity.
;; see also `handleGroup` function in readline.js
(defvar js-comint-code-format
  (concat
   "process.stdout.columns = %d;"
   "require('repl').start('%s', null, null, true, false, "
   "require('repl')['REPL_MODE_' + '%s'.toUpperCase()])"))

(defvar js-nvm-current-version nil
  "Current version of node.")

(defun js-comint-list-nvm-versions (prompt)
  "List all available node versions from nvm prompting the user with PROMPT.
Return a string representing the node version."
  (let ((candidates (sort (mapcar 'car (nvm--installed-versions)) 'string<)))
    (completing-read prompt
                     candidates nil t nil
                     nil
                     (car candidates))))

;;;###autoload
(defun js-do-use-nvm ()
  "Enable nvm."
  (setq js-use-nvm t))

;;;###autoload
(defun js-comint-select-node-version (&optional version)
  "Use a given VERSION of node from nvm."
  (interactive)
  (if version
      (setq js-nvm-current-version (nvm--find-exact-version-for version))
    (let ((old-js-nvm js-nvm-current-version))
      (setq js-nvm-current-version
            (nvm--find-exact-version-for
             (js-comint-list-nvm-versions
              (if old-js-nvm
                  (format "Node version (current %s): " (car old-js-nvm))
                "Node version: "))))))
  (progn
    (setq js-comint-program-command
          (concat
           (car (last js-nvm-current-version))
           "/bin"
           "/node"))))

(defun js-comint-guess-load-file-cmd (filename)
  (concat "require(\"" filename "\")\n"))

(defun js-comint-quit-or-cancel ()
  "Send ^C to Javascript REPL."
  (interactive)
  (process-send-string (js-comint-get-process) "\x03"))

(defun js-comint--path-sep ()
  (if (eq system-type 'windows-nt) ";" ":"))

(defun js-comint--suggest-module-path ()
  (let* ((dir (locate-dominating-file default-directory
                                      "node_modules")))
    (if dir (concat (file-name-as-directory dir)
                    "node_modules")
      default-directory)))

(defun js-comint-get-process ()
  (and js-comint-buffer
       (get-process js-comint-buffer)))

;;;###autoload
(defun js-comint-add-module-path ()
  "Add a directory to `js-comint-module-paths'."
  (interactive)
  (let* ((dir (read-directory-name "Module path:"
                                   (js-comint--suggest-module-path))))
    (when dir
      (add-to-list 'js-comint-module-paths (file-truename dir))
      (message "\"%s\" added to `js-comint-module-paths'" dir))))

;;;###autoload
(defun js-comint-delete-module-path ()
  "Delete a directory from `js-comint-module-paths'."
  (interactive)
  (cond
   ((not js-comint-module-paths)
    (message "`js-comint-module-paths' is empty."))
   (t
    (let* ((dir (ido-completing-read "Directory to delete: "
                                     js-comint-module-paths)))
      (when dir
        (setq js-comint-module-paths
              (delete dir js-comint-module-paths))
        (message "\"%s\" delete from `js-comint-module-paths'" dir))))))

;;;###autoload
(defun js-comint-save-setup ()
  "Save current setup to `.dir-locals.el'"
  (interactive)
  (let* (sexp
         (root (read-directory-name "Where to create .dir-locals.el: "
                                    default-directory))
         (file (concat (file-name-as-directory root)
                       ".dir-locals.el")))
    (cond
     (js-comint-module-paths
      (setq sexp (list (list nil (cons 'js-comint-module-paths js-comint-module-paths))))
      (with-temp-buffer
        (insert (format "%S" sexp))
        (write-file file)
        (message "%s created." file)))
     (t
      (message "Nothing to save. `js-comint-module-paths' is empty.")))))

(defun js-comint-setup-module-paths ()
  (let* ((paths (mapconcat 'identity
                           js-comint-module-paths
                           (js-comint--path-sep)))
         (node-path (getenv "NODE_PATH")))
    (cond
     ((or (not node-path)
          (string= "" node-path))
      ;; set
      (setenv "NODE_PATH" paths))
     ((not (string= "" paths))
      ;; append
      (setenv "NODE_PATH" (concat node-path (js-comint--path-sep) paths))
      (message "%s added into \$NODE_PATH" paths)))))

;;;###autoload
(defun js-comint-reset-repl ()
  "Kill existing REPL process if possible.  Create a new
Javascript REPL process.  The environment variable `NODE_PATH'
is setup by `js-comint-module-paths' before the process starts."
  (interactive)
  (when (js-comint-get-process)
    (process-send-string (js-comint-get-process) ".exit\n")
    ;; wait the process to be killed
    (sit-for 1))
  (js-comint-start-or-switch-to-repl))

(defun js-cominit-filter-output (string)
  "Filter extra escape sequences from output."
  (let ((beg (or comint-last-output-start
                 (point-min-marker)))
        (end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char beg)
      ;; Remove ansi escape sequences used in readline.js
      (while (re-search-forward js-comint-drop-regexp end t)
        (replace-match "")))))

(defun js-comint-get-buffer-name ()
  (format "*%s*" js-comint-buffer))

(defun js-comint-get-buffer ()
  (and js-comint-buffer
       (get-buffer (js-comint-get-buffer-name))))

;;;###autoload
(defun js-comint-clear ()
  "Clear the Javascript REPL."
  (interactive)
  (let* ((buf (js-comint-get-buffer) )
         (old-buf (current-buffer)))
    (save-excursion
      (cond
       ((buffer-live-p buf)
        (switch-to-buffer buf)
        (erase-buffer)
        (switch-to-buffer old-buf)
        (message "Javascript REPL cleared."))
       (t
        (error "Javascript REPL buffer doesn't exist!"))))))
(defalias 'js-clear 'js-comint-clear)


(defun js-comint-start-or-switch-to-repl ()
  (setenv "NODE_NO_READLINE" "1")
  (js-comint-setup-module-paths)
  (let* ((repl-mode (or (getenv "NODE_REPL_MODE") "magic"))
         (js-comint-code (format js-comint-code-format
                                 (window-width) js-comint-prompt repl-mode)))
    (pop-to-buffer
     (apply 'make-comint js-comint-buffer js-comint-program-command nil
            `(,@js-comint-program-arguments "-e" ,js-comint-code)))
    (js-comint-mode)))

;;;###autoload
(defun js-comint-repl (cmd)
  "Run an Javascript process.  The environment variable `NODE_PATH'
is setup by `js-comint-module-paths' before the process
starts."
  (interactive
   (list
    ;; You can select node version here
    (when current-prefix-arg
      (setq cmd
            (read-string "Run js: "
                         (format "%s %s"
                                 js-comint-program-command
                                 js-comint-program-arguments)))
      (when js-use-nvm
        (unless (featurep 'nvm)
          (require 'nvm))
        (unless js-nvm-current-version
          (js-comint-select-node-version)))
      (setq js-comint-program-arguments (split-string cmd))
      (setq js-comint-program-command (pop js-comint-program-arguments)))))
  (js-comint-start-or-switch-to-repl))
(defalias 'run-js 'js-comint-repl)

;;;###autoload
(defun js-comint-send-string (str)
  (comint-send-string (js-comint-get-process)
                      (concat str "\n")))

;;;###autoload
(defun js-comint-send-region ()
  "Send the current region to the inferior Javascript process.
If no region selected, you could manually input javascript expression."
  (interactive)
  (let* ((str (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string "input js expression: "))))
    (message "str=%s" str)
    (js-comint-send-string str)))

;;;###autoload
(defalias 'js-send-region 'js-comint-send-region)

;;;###autoload
(defun js-comint-send-last-sexp ()
  "Send the previous sexp to the inferior Javascript process.  `evil-mode' friendly."
  (interactive)
  (let* ((b (save-excursion
              (backward-sexp)
              (move-beginning-of-line nil)
              (point)))
         (e (if (and (boundp 'evil-mode)
                     evil-mode
                     (eq evil-state 'normal))
                (+ 1 (point))
              (point)))
         (str (buffer-substring-no-properties b e)))
    (js-comint-send-string str)))

;;;###autoload
(defalias 'js-send-last-sexp 'js-comint-send-last-sexp)

;;;###autoload
(defun js-comint-send-buffer ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (js-comint-send-string
   (buffer-substring-no-properties (point-min)
                                   (point-max))))

;;;###autoload
(defalias 'js-send-buffer 'js-comint-send-buffer)

;;;###autoload
(defun js-comint-load-file (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (js-comint-repl js-comint-program-command)
    (comint-send-string (js-comint-get-process) (js-comint-guess-load-file-cmd filename))))

;;;###autoload
(defalias 'js-load-file 'js-comint-load-file)

;;;###autoload
(defun js-comint-switch-to-repl (eob-p)
  "Switch to the javascript process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (js-comint-get-buffer)
      (pop-to-buffer (js-comint-get-buffer))
    (error "No current process buffer.  See variable `js-comint-buffer'")))
(defalias 'switch-to-js 'js-comint-switch-repl)

(defvar js-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'js-comint-quit-or-cancel)
    map))


;;;###autoload
(define-derived-mode js-comint-mode comint-mode "Javascript REPL"
  :group 'js-comint
  :syntax-table js-mode-syntax-table
  (setq-local font-lock-defaults (list js--font-lock-keywords))
  ;; No echo
  (setq comint-process-echoes t)
  ;; Ignore duplicates
  (setq comint-input-ignoredups t)
  (add-hook 'comint-output-filter-functions 'js-cominit-filter-output nil t)
  (use-local-map js-comint-mode-map)
  (ansi-color-for-comint-mode-on))

(provide 'js-comint)
;;; js-comint.el ends here

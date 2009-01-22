;;; auto-complete.el --- Auto completion with popup menu

;; Copyright (C) 2008  MATSUYAMA Tomohiro

;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Keywords: convenience
;; Version: 0.2.0

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

;; This extension provides a way to select a completion with
;; popup menu.
;;
;; I checked that this extension can work properly on GNU Emacs 22 or higher.

;; To use this extension, locate this file to load-path directory,
;; and add the following code to your .emacs.
;; ------------------------------
;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; ------------------------------

;; After installation, you can try this:
;;
;; 1. Switch to emacs-lisp-mode buffer such as .emacs
;; 2. Goto anywhere
;; 3. Type "def"
;; 4. You may see a pop menu after the cursor like:
;;    def-!-
;;    +-----------------+
;;    |defun            |    <- highlight
;;    |defvar           |
;;    |defmacro         |
;;    |       ...       |
;;    +-----------------+
;; 5. You can complete by seleting the menu item
;;    by pressing TAB, <down>, <up>, and RET.

;;; Tips:
;;
;; ================================
;; Use C-n/C-p to select candidates
;; ================================
;;
;; Add following code to your .emacs.
;; 
;; ------------------------------
;; (define-key ac-complete-mode-map "\C-n" 'ac-next)
;; (define-key ac-complete-mode-map "\C-p" 'ac-previous)
;; ------------------------------
;;
;;
;; ====================================
;; Don't start completion automatically
;; ====================================
;;
;; Add following code to your .emacs.
;;
;; ------------------------------
;; (setq ac-auto-start nil)
;; (global-set-key "\M-/" 'ac-start)
;; ------------------------------
;; 
;; Or
;;
;; ------------------------------
;; ;; start completion when entered 3 characters
;; (setq ac-auto-start 3)
;; ------------------------------
;;
;;
;; =================
;; Completion by TAB
;; =================
;;
;; Add following code to your .emacs.
;;
;; ------------------------------
;; (define-key ac-complete-mode-map "\t" 'ac-complete)
;; (define-key ac-complete-mode-map "\r" nil)
;; ------------------------------
;;
;;
;; ===================
;; Do What I Mean mode
;; ===================
;;
;; If DWIM (Do What I Mean) mode is enabled,
;; the following features is available:
;;
;; a. TAB (ac-expand) behave as completion (ac-complete)
;;    when only one candidate is left
;; b. TAB (ac-expand) behave as completion (ac-complete)
;;    after you select candidate
;;
;; DWIM mode is enabled by default.
;; You can disable this feature by
;; setting `ac-dwim' to nil.
;;
;;
;; ==================================
;; Change sources for particular mode
;; ==================================
;;
;; `ac-sources' is global variable, so you have to
;; make it local variable to change sources for particular mode like:
;;
;; ------------------------------
;; (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (make-local-variable 'ac-sources)
;;               (setq ac-sources '(ac-source-words-in-buffer ac-source-symbols))))
;; ------------------------------

;; This extension is so simple that you can extend
;; how Emacs find a prefix and how Emacs enumerate
;; candidates.
;; I don't have intention to implement heavy functions :-)
;;
;; Enjoy!

;;; History:

;; 2008-01-20 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * omni completion
;;
;; 2008-12-24 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * suppress errors on command hook
;;
;; 2008-12-03 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * changed ac-dwim to nil by default
;;      * made menu to be able to adjust width
;;
;; 2008-12-03 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * renamed ac-find-function to ac-prefix-function
;;      * renamed ac-target to ac-prefix
;;
;; 2008-11-26 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * auto-complete.el 0.1.0 released
;;
;; 2008-11-19 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * thanks for Taiki SUGAWARA <buzz.taiki@gmail.com>
;;      *   added source ac-source-abbrev
;;      *   added source ac-source-symbols
;;      * added ac-expand-common to expand common part
;;
;; 2008-11-18 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * added ac-auto-start switch
;;      * added ac-dwim switch
;;      * changed menu popup behavior at end of window
;;      *   thanks rubikitch <rubikitch@ruby-lang.org>, kazu-yamamoto.
;;      * fixed canceler bug
;;      * changed to use overriding-local-map instead of minor mode map
;;      * changed default key bindings
;;
;; 2008-11-16 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * supported candidates by using sources
;;      * added automatically start swtich
;;      * fixed some bug
;;      * added source ac-source-files-in-current-dir
;;      * added source ac-source-words-in-buffer
;;      * added source ac-source-yasnippet
;;      * renamed ac-enum-candidates-function to ac-candidate-function
;;      * renamed ac-find-target-function to ac-find-function
;;      * ac-find-function and ac-candidate-function is not buffer local variable now
;;      * made candidates visible when you are end of line
;;
;; 2008-11-11 MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * by reporting from rubikitch <rubikitch@ruby-lang.org>
;;      *   renamed hook name
;;      *   registered backward-delete-char as special command
;;      * fixed code for creating candidates
;;      * made auto-complete disabled when isearch-mode enabled
;;      * added some major-mode into ac-modes
;;
;; 2008-11-09  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * auto-complete.el 0.0.1 released
;;      * fixed double-width character displaying problem
;;      * fixed menu position following tab character
;;      * made candidates visible when you are end of window

;;; TODO:
;;
;; - performance issue (cache issue)
;; - single source mode
;; - fix narrowing bug (reported by Yuto Hayamizu <y.hayamizu@gmail.com>)
;; - care about undo (buffer-disable-undo)
;; - support scroll in menu
;; - use double candidate menu
;; - show description
;; - demo movie (YouTube)
;; - dictionary
;; - semantic
;; - use cl
;; - icon

;;; Code:



(defgroup auto-complete nil
  "Auto completion with popup menu"
  :group 'convenience
  :prefix "auto-complete-")

(defcustom ac-candidate-menu-height 10
  "Max height of candidate menu."
  :type 'number
  :group 'auto-complete)

(defcustom ac-candidate-max 10
  "Max of number of candidates."
  :type 'number
  :group 'auto-complete)

(defcustom ac-modes
  '(emacs-lisp-mode lisp-interaction-mode
                    c-mode cc-mode c++-mode java-mode
                    perl-mode cperl-mode python-mode ruby-mode
                    ecmascript-mode javascript-mode php-mode css-mode
                    makefile-mode sh-mode fortran-mode f90-mode ada-mode
                    xml-mode sgml-mode)
  "Major modes `auto-complete-mode' can run on."
  :type '(list symbol)
  :group 'auto-complete)

(defcustom ac-auto-start t
  "Non-nil means completion will be started automatically.
Positive integer means if a length of a word you entered is larger than the value,
completion will be started automatically.
If you specify `nil', never be started automatically."
  :group 'auto-complete)

(defcustom ac-dwim nil
  "Non-nil means `auto-complete' works based on Do What I Mean."
  :type 'boolean
  :group 'auto-complete)

(defface ac-selection-face
  '((t (:background "blue" :foreground "white")))
  "Face for the selected candidate."
  :group 'auto-complete)

(defface ac-menu-face
  '((t (:background "lightgray" :foreground "black")))
  "Face for candidate menu."
  :group 'auto-complete)

(defvar auto-complete-mode-hook nil
  "Hook for `auto-complete-mode'.")

(defvar ac-menu nil
  "Menu instance.")

(defvar ac-menu-direction 1
  "Positive integer means `ac-menu' grows forward.
Or, `ac-menu' grows backward.")

(defvar ac-menu-offset 0
  "Offset to contents.")

(defvar ac-completing nil
  "Non-nil means `auto-complete-mode' is now working on completion.")

(defvar ac-saved-window-start nil
  "Saved window start value for restore.")

(defvar ac-saved-window-hscroll nil
  "Saved window hscroll value for restore.")

(defvar ac-point nil
  "Start point of prefix.")

(defvar ac-old-point nil
  "Previous start point of prefix.")

(defvar ac-prefix nil
  "Prefix.")
(defvaralias 'ac-target 'ac-prefix)

(defvar ac-limit 0
  "Limit of number of candidates.")

(defvar ac-candidates nil
  "Current candidates.")

(defvar ac-selection nil
  "Current candidate index.")

(defvar ac-dwim-enable nil
  "Non-nil means DWIM completion will be allowed.")

(defvar ac-prefix-function 'ac-sources-prefix
  "When `auto-complete-mode' finds it can start completion
or update candidates, it will call this function to find a
start point of the prefix.

If this function returns a point `auto-complete-mode'
will set the substring between the point and current point to `ac-prefix'.
And also it will start completion or update candidates by using
the `ac-prefix'.

If this function returns `nil', `auto-complete-mode'
ignore starting completion or stop completing.")
(defvaralias 'ac-find-function 'ac-prefix-function)

(defvar ac-init-function 'ac-sources-init
  "This function will be called when candidate menu is setupped.")

(defvar ac-cleanup-function 'ac-sources-cleanup
  "This function will be called when candidate menu is cleanupped")

(defvar ac-candidate-function 'ac-sources-candidate
  "This function can return candidates as list by
using the `TARGET' that is given as a first argument.")

(defvar ac-complete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ac-expand)
    (define-key map "\r" 'ac-complete)
    
    (define-key map [down] 'ac-next)
    (define-key map [up] 'ac-previous)

    map)
  "Keymap for completion.")

(or (assq 'ac-completing minor-mode-map-alist)
    (push (cons 'ac-completing ac-complete-mode-map) minor-mode-map-alist))

(defvar ac-saved-local-map nil
  "Old keymap before `auto-complete' activated.")



;;;; Auto completion

(defun ac-setup (point width)
  "Setup popup menu."
  (when ac-menu
    ;; reposition
    (ac-menu-delete ac-menu)
    (setq ac-menu nil))
  (save-excursion
    (goto-char point)
    (let ((column (current-column))
          (line (line-number-at-pos)))
      (setq ac-saved-window-start (window-start))
      (setq ac-saved-window-hscroll (window-hscroll))
      (setq ac-menu-direction
            (if (and (> line ac-candidate-menu-height)
                     (> ac-candidate-menu-height
                        (-
                         (max 1 (- (window-height)
                                   (if mode-line-format 1 0)
                                   (if header-line-format 1 0)))
                         (1+ (count-lines (window-start) (point))))))
                -1
              1))
      (let ((window-width (window-width))
            (right (- (+ column width)
                      (window-hscroll))))
        (if (and (> right window-width)
                 (>= right width))
            (setq column (- column width))))
      (if (> ac-menu-direction 0)
          (progn
            (forward-line)
            (if (eq line (line-number-at-pos))
                (newline)
              (forward-line -1))
            (setq ac-menu (ac-menu-create (1+ line) column width ac-candidate-menu-height))
            (setq ac-point point))
        (setq ac-menu (ac-menu-create (- line ac-candidate-max) column width ac-candidate-menu-height))
        (setq ac-point point)))))

(defun ac-cleanup ()
  "Destroy popup menu."
  (ac-deactivate-mode-map)
  (when ac-menu
    (ac-menu-delete ac-menu)
    (set-window-start (selected-window) ac-saved-window-start)
    (set-window-hscroll (selected-window) ac-saved-window-hscroll))
  (setq ac-menu nil)
  (setq ac-completing nil)
  (setq ac-point nil)
  (setq ac-candidates nil)
  (setq ac-selection 0)
  (funcall ac-cleanup-function))

(defun ac-activate-mode-map ()
  "Activate `ac-complete-mode-map'."
  (setq ac-saved-local-map overriding-terminal-local-map)
  (if (eq ac-saved-local-map ac-complete-mode-map)
      ;; maybe never reach here
      (setq ac-saved-local-map nil))
  (setq overriding-terminal-local-map ac-complete-mode-map))

(defun ac-deactivate-mode-map ()
  "Deactivate `ac-complete-mode-map'."
  (when (eq overriding-terminal-local-map ac-complete-mode-map)
    (setq overriding-terminal-local-map ac-saved-local-map)
    (setq ac-saved-local-map nil)))

(defun ac-next ()
  "Select next candidate."
  (interactive)
  (if (interactive-p)
      (setq ac-dwim-enable t))
  (if ac-candidates
      (ac-select-candidate
       (let ((selection (1+ ac-selection)))
         (if (= selection (+ ac-menu-offset (min ac-candidate-menu-height (length ac-candidates))))
             ac-menu-offset
           selection)))))

(defun ac-previous ()
  "Select previous candidate."
  (interactive)
  (if (interactive-p)
      (setq ac-dwim-enable t))
  (if ac-candidates
      (ac-select-candidate
       (let ((selection (1- ac-selection)))
         (if (< selection ac-menu-offset)
             (1- (+ ac-menu-offset (min ac-candidate-menu-height (length ac-candidates))))
           selection)))))

(defun ac-expand-1 ()
  "Try expansion."
  (let ((string (overlay-get (ac-menu-line-overlay ac-menu ac-selection) 'real-string)))
    (delete-region ac-point (point))
    (insert string)
    (setq ac-prefix string)))

(defun ac-expand ()
  "Try expansion but select next if expanded twice."
  (interactive)
  (if (and ac-dwim ac-dwim-enable)
      (ac-complete)
    (let ((target ac-prefix)
          (string (ac-expand-1)))
      (when (equal target string)
        (ac-next)
        (ac-expand-1)))))

(defun ac-expand-common ()
  "Try expansion common part."
  (interactive)
  (let ((common (try-completion ac-prefix ac-candidates)))
    (when (stringp common)
      (delete-region ac-point (point))
      (insert common)
      (setq ac-prefix common))))

(defun ac-complete ()
  "Try completion."
  (interactive)
  (let* ((string (overlay-get (ac-menu-line-overlay ac-menu ac-selection) 'real-string))
         (source (get-text-property 0 'source string))
         (complete-function (and source (assoc-default 'action source))))
    (ac-expand-1)
    (if complete-function
        (funcall complete-function))
    (ac-abort)))

(defun ac-abort ()
  "Abort completion."
  (ac-cleanup))

(defun ac-update-candidates (candidates)
  "Update candidates of popup menu."
  (setq ac-menu-offset (if (> ac-menu-direction 0)
                           0
                         (- ac-candidate-menu-height
                            (min ac-candidate-menu-height
                                 (length candidates)))))
  (setq ac-selection ac-menu-offset)
  (setq ac-candidates candidates)
  (setq ac-dwim-enable (= (length candidates) 1))
  (if candidates
      (progn
        (setq ac-completing t)
        (ac-activate-mode-map))
    (setq ac-completing nil)
    (ac-deactivate-mode-map))
  (let ((i ac-menu-offset))
    ;; show line and set string to the line
    (mapcar
     (lambda (candidate)
       (when (< i ac-candidate-menu-height)
         (ac-menu-show-line ac-menu i)
         (ac-menu-set-line-string ac-menu i candidate (if (= i ac-selection) 'ac-selection-face))
         (setq i (1+ i))))
     candidates)
    ;; ensure lines visible
    (if (and (> ac-menu-direction 0)
             (> i (-
                   (max 1 (- (window-height)
                             (if mode-line-format 1 0)
                             (if header-line-format 1 0)))
                   (1+ (count-lines (window-start) (point))))))
        (recenter (- (1+ i))))
    (if (> i ac-menu-offset)
        (let ((window-width (window-width))
              (right (- (+ (ac-menu-column ac-menu) (ac-menu-width ac-menu))
                        (window-hscroll))))
          (if (> right window-width)
              (scroll-left (- right window-width)))))
    ;; hide remaining lines
    (if (> ac-menu-direction 0)
        (while (< i ac-candidate-menu-height)
          (ac-menu-hide-line ac-menu i)
          (setq i (1+ i)))
      (dotimes (i ac-menu-offset)
        (ac-menu-hide-line ac-menu i)))))

(defun ac-filter-candidates (candidates)
  "Return filtered candidates."
  (all-completions ac-prefix candidates))

(defun ac-select-candidate (selection)
  "Select candidate pointed by `SELECTION'."
  (when ac-candidates
    (ac-menu-set-line-string ac-menu ac-selection (nth (- ac-selection ac-menu-offset) ac-candidates))
    (ac-menu-set-line-string ac-menu selection (nth (- selection ac-menu-offset) ac-candidates) 'ac-selection-face)
    (setq ac-selection selection)))

(defun ac-start ()
  "Start completion."
  (interactive)
  (let* ((point (save-excursion (funcall ac-prefix-function)))
         (reposition (not (equal ac-point point))))
    (if (null point)
        (ac-abort)
      (setq ac-point point)
      (when (not (equal ac-point ac-old-point))
        (setq ac-old-point point))
      (setq ac-prefix (buffer-substring-no-properties point (point)))
      (setq ac-limit ac-candidate-max)
      (if (or reposition (null ac-menu))
          (save-excursion
            (funcall ac-init-function)))
      (let* ((candidates
              (if (or ac-completing
                      (eq ac-auto-start t)
                      (and (integerp ac-auto-start)
                           (>= (length ac-prefix) ac-auto-start)))
                  (save-excursion
                    (funcall ac-candidate-function))))
             (current-width (if ac-menu (ac-menu-width ac-menu) 0))
             (width (let ((w '(0)) s)
                      (dotimes (i ac-candidate-menu-height)
                        (setq s (nth i candidates))
                        (if (stringp s) (push (string-width s) w)))
                      (apply 'max w))))
        (if (or reposition
                (null ac-menu)
                (> width current-width)
                (< width (- current-width 20)))
            (ac-setup point (* (ceiling (/ width 20.0)) 20)))
        (ac-update-candidates candidates)))))

(defun ac-trigger-command-p ()
  "Return non-nil if `this-command' is a trigger command."
  (or (eq this-command 'self-insert-command)
      (and ac-completing
           (memq this-command
                 '(delete-backward-char
                   backward-delete-char
                   backward-delete-char-untabify)))))

(defun ac-on-pre-command ()
  (progn                                ; ignore-errors
    (if (and (not (ac-trigger-command-p))
             (or (not (symbolp this-command))
                 (not (string-match "^ac-" (symbol-name this-command)))))
        (ac-abort))))

(defun ac-on-post-command ()
  (progn                                ; ignore-errors
    (if (and ac-auto-start
             (not isearch-mode)
             (ac-trigger-command-p))
        (ac-start))))

(defun auto-complete-mode-maybe ()
  "What buffer `auto-complete-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ac-modes))
      (auto-complete-mode 1)))

(require 'easy-mmode)

(define-minor-mode auto-complete-mode
  "AutoComplete mode"
  :lighter " AC"
  :group 'auto-complete
  (if auto-complete-mode
      (progn
        (add-hook 'post-command-hook 'ac-on-post-command nil t)
        (add-hook 'pre-command-hook 'ac-on-pre-command nil t)
        (run-hooks 'auto-complete-mode-hook))
    (remove-hook 'post-command-hook 'ac-on-post-command t)
    (remove-hook 'pre-command-hook 'ac-on-pre-command t)
    (ac-abort)))

(if (fboundp 'define-global-minor-mode)
    (define-global-minor-mode global-auto-complete-mode
      auto-complete-mode auto-complete-mode-maybe
      :group 'auto-complete))



;;;; Sources implementation

(defvar ac-sources '(ac-source-words-in-buffer)
  "Sources for completion.

Source takes a form of alist:

init INIT-FUNC
  INIT-FUNC will be called before creating candidate every time.

candidates CANDIDATE-FUNC
  CANDIDATE-FUNC will return a list of string as candidates.
CANDIDATE-FUNC should care about `ac-limit' that is specified at limit for performance.

action ACTION-FUNC
  ACTION-FUNC will be called when `ac-complete' is called.

limit LIMIT-NUM
  A limit of candidates.

requires REQUIRES-NUM
  This source will be included when `ac-prefix' length is larger than REQUIRES-NUM.")

(defvar ac-current-sources nil
  "Current working sources.")

(defvar ac-omni-completion-sources
  '((c-mode . (("\\.\\=" . (ac-source-semantic))))
    (c++-mode . (("\\.\\=" . (ac-source-semantic))
                 ("->\\=" . (ac-source-semantic))))
    (java-mode . (("\\.\\=" . (ac-source-semantic))))
    (eshell-mode . (("/\\=" . (ac-source-filename))))))

(defvar ac-sources-omni-completion nil)

(defun ac-sources-init ()
  "Implementation for `ac-init-function' by sources."
  (or ac-current-sources (setq ac-current-sources ac-sources))
  (dolist (source ac-current-sources)
    (if (symbolp source)
        (setq source (symbol-value source)))
    (let ((init-function (assoc-default 'init source)))
      (if init-function
          (funcall init-function)))))

(defun ac-sources-cleanup ()
  "Implementation for `ac-cleanup-function' by sources."
  (setq ac-current-sources nil)
  (setq ac-sources-omni-completion nil))

(defun ac-sources-prefix ()
  "Implemention for `ac-prefix-function' by sources."
  (let (point
        (regexp-alist (assoc-default major-mode ac-omni-completion-sources)))
    (while regexp-alist
      (let ((pair (car regexp-alist)))
        (if (not (looking-back (car pair)))
            (setq regexp-alist (cdr regexp-alist))
          (setq ac-current-sources (cdr pair))
          (setq ac-sources-omni-completion t)
          (setq point (match-end 0))
          (setq regexp-alist nil))))
    (or point
        (if (and ac-completing ac-sources-omni-completion) ac-point)
        (progn
          (require 'thingatpt)
          (setq ac-current-sources ac-sources)
          (setq ac-sources-omni-completion nil)
          (car-safe (bounds-of-thing-at-point 'symbol))))))

(defun ac-sources-candidate ()
  "Implementation for `ac-cadidates-function' by sources."
  (let (candidates)
    (dolist (source ac-current-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (assoc-default 'limit source) ac-limit))
             (requires (assoc-default 'requires source))
             cand)
        (when (>= (length ac-prefix)
                  (if (integerp requires)
                      requires
                    1))
          (setq cand
                (delq nil
                      (mapcar (lambda (candidate)
                                (propertize candidate 'source source))
                              (funcall (assoc-default 'candidates source))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) (copy-sequence cand)) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(defun ac-candidate-words-in-buffer ()
  "Default implemention for `ac-candidate-function'."
  (if (> (length ac-prefix) 0)
      (let ((i 0)
            candidate
            candidates
            (regexp (concat "\\b" (regexp-quote ac-prefix) "\\(\\s_\\|\\sw\\)*\\b")))
        (save-excursion
          ;; search backward
          (goto-char ac-point)
          (while (and (< i ac-limit)
                      (re-search-backward regexp nil t))
            (setq candidate (match-string-no-properties 0))
            (unless (member candidate candidates)
              (push candidate candidates)
              (setq i (1+ i))))
          ;; search backward
          (goto-char (+ ac-point (length ac-prefix)))
          (while (and (< i ac-limit)
                      (re-search-forward regexp nil t))
            (setq candidate (match-string-no-properties 0))
            (unless (member candidate candidates)
              (push candidate candidates)
              (setq i (1+ i))))
          (nreverse candidates)))))

(defvar ac-source-words-in-buffer
  '((candidates . ac-candidate-words-in-buffer))
  "Simple source like dabbrev.")

(defvar ac-source-symbols
  '((candidates
     . (lambda ()
         (ac-filter-candidates obarray))))
  "Source for Emacs lisp symbols.")

(defvar ac-source-abbrev
  `((candidates
     . (lambda ()
         (ac-filter-candidates local-abbrev-table)))
    (action
     . expand-abbrev))
  "Source for abbrev.")

(defvar ac-source-files-in-current-dir
  '((candidates
     . (lambda ()
         (ac-filter-candidates (directory-files default-directory)))))
  "Source for listing files in current directory.")

(defun ac-filename-candidate ()
  (let ((dir (file-name-directory ac-prefix)))
    (ignore-errors
      (delq nil
            (mapcar (lambda (file)
                      (if (not (member file '("./" "../")))
                          (concat dir file)))
                    (file-name-all-completions
                     (file-name-nondirectory ac-prefix) dir))))))

(defvar ac-source-filename
  '((candidates . ac-filename-candidate))
  "Source for completing file name.")

(defvar ac-imenu-index nil
  "Imenu index.")

(defun ac-imenu-candidate ()
  (require 'imenu)
  (let ((i 0)
        (stack ac-imenu-index)
        candidates
        node)
    (while (and stack
                (< i ac-limit))
      (setq node (pop stack))
      (when (consp node)
        (let ((car (car node))
              (cdr (cdr node)))
          (if (consp cdr)
              (mapcar (lambda (child)
                        (push child stack))
                      cdr)
            (when (and (stringp car)
                       (string-match (concat "^" (regexp-quote ac-prefix)) car))
              (push car candidates)
              (setq i (1+ i)))))))
    (nreverse candidates)))

(defvar ac-source-imenu
  '((init
     . (lambda ()
         (require 'imenu)
         (setq ac-imenu-index
               (condition-case nil
                   (imenu--make-index-alist)
                 (error nil)))))
    (candidates . ac-imenu-candidate))
  "Source for imenu.")

(defun ac-yasnippet-candidate-1 (table)
  (let ((hashtab (yas/snippet-table-hash table))
        (parent (yas/snippet-table-parent table))
        candidates)
    (maphash (lambda (key value)
               (push key candidates))
             hashtab)
    (setq candidates (all-completions ac-prefix (nreverse candidates)))
    (if parent
        (setq candidates
              (append candidates (ac-yasnippet-candidate-1 parent))))
    candidates))

(defun ac-yasnippet-candidate ()
  (when (require 'yasnippet nil t)
    (let ((table (yas/snippet-table major-mode)))
      (if table
          (ac-yasnippet-candidate-1 table)))))

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (limit . 3))
  "Source for Yasnippet.")



;;;; Popup menu

(defun ac-menu-line (menu)
  "Line number of `MENU'."
  (nth 0 menu))

(defun ac-menu-column (menu)
  "Column of `MENU'."
  (nth 1 menu))

(defun ac-menu-width (menu)
  "Popup menu width of `MENU'."
  (nth 2 menu))

(defun ac-menu-height (menu)
  "Popup menu height of `MENU'."
  (nth 3 menu))

(defun ac-menu-overlays (menu)
  "Overlays that `MENU' contains."
  (nth 4 menu))

(defun ac-menu-line-overlay (menu line)
  "Return a overlay of `MENU' at `LINE'."
  (aref (ac-menu-overlays menu) line))

(defun ac-menu-hide-line (menu line)
  "Hide `LINE' in `MENU'."
  (let ((overlay (ac-menu-line-overlay menu line)))
    (overlay-put overlay 'invisible nil)
    (overlay-put overlay 'after-string nil)))

(defun ac-menu-show-line (menu line)
  "Show `LINE' in `MENU'."
  (let ((overlay (ac-menu-line-overlay menu line)))
    (overlay-put overlay 'invisible t)))

(defun ac-menu-set-line-string (menu line string &optional face)
  "Set contents of `LINE' in `MENU'."
  (let ((overlay (ac-menu-line-overlay menu line)))
    (overlay-put overlay 'real-string string)
    (funcall (overlay-get overlay 'set-string-function) overlay string face)))

(defun ac-menu-create-line-string (menu string)
  "Adjust `STRING' into `MENU'."
  (let ((length 0)
        (width 0)
        (menu-width (ac-menu-width menu))
        (chars (append string nil)))
    (while (and
            chars
            (<= (setq width (+ width (char-width (car chars)))) menu-width))
      (setq length (1+ length))
      (setq chars (cdr chars)))
    (if (< length (length string))
        (setq string (substring string 0 length)))
    (let ((string-width (string-width string)))
      (if (< string-width menu-width)
          (setq string (concat string
                               (make-string (- menu-width string-width) ? )))))
    string))

(defun ac-menu-hide (menu)
  "Hide `MENU'."
  (dotimes (i (ac-menu-height menu))
    (ac-menu-hide-line menu i)))

(defun ac-menu-last-line-of-buffer ()
  (save-excursion
    (not (eq (forward-line) 0))))

(defun ac-menu-create (line column width height)
  "Create popup menu."
  (save-excursion
    (let ((overlays (make-vector height nil))
          (window (selected-window)))
      (goto-line line)
      (dotimes (i height)
        (move-to-column column)
        (let (overlay begin w current-column (prefix "") (postfix ""))
          (setq current-column (current-column))
          (cond
           ((> current-column column)
            (backward-char)
            (setq current-column (current-column))
            (if (< current-column column)
                (setq prefix (make-string (- column current-column) ? ))))
           ((< current-column column)
            (setq prefix (make-string (- column current-column) ? ))))

          (setq begin (point))
          (setq w (+ width (length prefix)))
          (while (and (not (eolp))
                      (> w 0))
            (setq w (- w (char-width (char-after))))
            (forward-char))
          (if (< w 0)
              (setq postfix (make-string (- w) ? )))
          (if (ac-menu-last-line-of-buffer)
              (setq postfix (concat postfix "\n")))

          (setq overlay (make-overlay begin (point)))
          (overlay-put overlay 'window window)
          (overlay-put overlay 'prefix prefix)
          (overlay-put overlay 'postfix postfix)
          (overlay-put overlay 'width width)
          (overlay-put overlay 'set-string-function
                       (lambda (overlay string &optional face)
                         (overlay-put overlay
                                      'after-string
                                      (concat (overlay-get overlay 'prefix)
                                              (propertize (ac-menu-create-line-string menu string) 'face (or face 'ac-menu-face))
                                              (overlay-get overlay 'postfix)))))
          (aset overlays i overlay))
        (forward-line))
      (let ((i 100))
        (mapcar (lambda (overlay)
                  (overlay-put overlay 'priority i)
                  (setq i (1+ i)))
                (nreverse (append overlays nil))))
      (list line column width height overlays))))

(defun ac-menu-delete (menu)
  "Delete `MENU'."
  (mapcar 'delete-overlay (ac-menu-overlays menu)))

(provide 'auto-complete)
;;; auto-complete.el ends here

;;; auto-complete.el --- Auto completion

;; Copyright (C) 2008, 2009  MATSUYAMA Tomohiro

;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Keywords: convenience
;; Version: 0.3.0 alpha

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
;;
;; This extension provides a way to complete with popup menu like:
;;
;;     def-!-
;;     +-----------------+
;;     |defun::::::::::::|
;;     |defvar           |
;;     |defmacro         |
;;     |       ...       |
;;     +-----------------+
;;
;; You can complete by typing and selecting menu.
;; Enjoy!

;;; Qualification:
;;
;; This extension can work properly on GNU Emacs 22 or higher.

;;; Installation:
;;
;; To use this extension, locate auto-complete.el to your load-path directory.
;;
;;     $ cp auto-complete.el ~/.emacs.d/
;;
;; And write following code into your .emacs.
;;
;;     (require 'auto-complete)
;;     (global-auto-complete-mode t)

;;; Tips:
;;
;; Use C-n/C-p to select candidates
;; --------------------------------
;;
;; Add following code to your .emacs.
;; 
;;     (define-key ac-complete-mode-map "\C-n" 'ac-next)
;;     (define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;
;;
;; Don't start completion automatically
;; ------------------------------------
;;
;; Add following code to your .emacs.
;;
;;     (setq ac-auto-start nil)
;;     (global-set-key "\M-/" 'ac-start)
;;
;; or
;;
;;     ;; start completion when entered 3 characters
;;     (setq ac-auto-start 3)
;;
;;
;; Stop completion
;; ---------------
;;
;; Add following code to your .emacs.
;;
;;     (define-key ac-complete-mode-map "\M-/" 'ac-stop)
;;
;; Now you can stop completion by pressing M-/.
;;
;;
;; Completion by TAB
;; -----------------
;;
;; Add following code to your .emacs.
;;
;;     (define-key ac-complete-mode-map "\t" 'ac-complete)
;;     (define-key ac-complete-mode-map "\r" nil)
;;
;;
;; Do What I Mean mode
;; -------------------
;;
;; If DWIM (Do What I Mean) mode is enabled,
;; the following features is available:
;;
;; a. TAB (ac-expand) behave as completion (ac-complete)
;;    when only one candidate is left
;; b. TAB (ac-expand) behave as completion (ac-complete)
;;    after you select candidate
;; c. Disapear automatically when you
;;    complete a candidate.
;;
;; DWIM mode is enabled by default.
;; You can enable this feature by
;; setting `ac-dwim' to t.
;;
;;     (setq ac-dwim t)
;;
;;
;; Change default sources
;; ----------------------
;;
;;     (setq-default ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
;;
;;
;; Change sources for particular mode
;; ----------------------------------
;;
;;     (add-hook 'emacs-lisp-mode-hook
;;                 (lambda ()
;;                   (setq ac-sources '(ac-source-words-in-buffer ac-source-symbols))))

;;; History:
;;
;; 2009-06-21
;;      * fixed ac-source-words-in-all-buffer didn't collect all words in current buffer
;;      * added inline completion feature
;;
;; 2009-06-14
;;      * suppress wasting undo recording when expand (based on a patch from Vitaly Ostanin <vitaly.ostanin@gmail.com>)
;;      * fixed disrupted menu on long line (based on a patch from Vitaly Ostanin <vitaly.ostanin@gmail.com>)
;;
;; 2009-03-18
;;      * auto-complete.el 0.2.0 released
;;
;; 2009-03-04
;;      * fixed menu position bug
;;
;; 2009-03-02
;;      * made a source be able to be just a function which returns candidates
;;      * added ac-source-words-in-all-buffer
;;
;; 2009-03-01
;;      * added basic cache facility
;;
;; 2009-02-20
;;      * fixed menu position bug at long line (thanks rubikitch <rubikitch@ruby-lang.org>)
;;      * made dictionary source generator (ac-define-dictionary-source)
;;      * devided into some files (auto-complete-ruby.el, auto-complete-yasnippet.el, etc)
;;
;; 2009-02-19
;;      * added ac-trigger-commands switch
;;
;; 2009-02-10
;;      * added ac-stop function (suggestion from Andy Stewart)
;;      * added ac-override-local-map switch (suggestion from Andy Stewart)
;;
;; 2009-02-03
;;      * omni completion redesign
;;      * ac-sources is now buffer local for every buffer
;;      * fixed a menu position bug (thanks Andy Stewart)
;;      * fixed byte-compile warnings (thanks Andy Stewart)
;;
;; 2009-01-22
;;      * added face/selection-face property for sources
;;      * supported menu scroll
;;
;; 2009-01-20
;;      * omni completion
;;
;; 2008-12-24
;;      * suppress errors on command hook
;;
;; 2008-12-03
;;      * changed ac-dwim to nil by default
;;      * made menu to be able to adjust width
;;
;; 2008-12-03
;;      * renamed ac-find-function to ac-prefix-function
;;      * renamed ac-target to ac-prefix
;;
;; 2008-11-26
;;      * auto-complete.el 0.1.0 released
;;
;; 2008-11-19
;;      * thanks for Taiki SUGAWARA <buzz.taiki@gmail.com>
;;      *   added source ac-source-abbrev
;;      *   added source ac-source-symbols
;;      * added ac-expand-common to expand common part
;;
;; 2008-11-18
;;      * added ac-auto-start switch
;;      * added ac-dwim switch
;;      * changed menu popup behavior at end of window
;;      *   thanks rubikitch <rubikitch@ruby-lang.org>, kazu-yamamoto.
;;      * fixed canceler bug
;;      * changed to use overriding-local-map instead of minor mode map
;;      * changed default key bindings
;;
;; 2008-11-16
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
;; 2008-11-11
;;      * by reporting from rubikitch <rubikitch@ruby-lang.org>
;;      *   renamed hook name
;;      *   registered backward-delete-char as special command
;;      * fixed code for creating candidates
;;      * made auto-complete disabled when isearch-mode enabled
;;      * added some major-mode into ac-modes
;;
;; 2008-11-09
;;      * auto-complete.el 0.0.1 released
;;      * fixed double-width character displaying problem
;;      * fixed menu position following tab character
;;      * made candidates visible when you are end of window

;;; TODO:
;;
;; - etags, ctags
;; - emacswiki
;; - test facility
;; - support composed chars
;; - fuzzy match
;; - minibuffer completion (50%)
;; - dictionary
;; - documentation
;; - performance issue (cache issue)
;; - fix narrowing bug (reported by Yuto Hayamizu <y.hayamizu@gmail.com>)
;; - scroll bar (visual)
;; - show description
;; - semantic
;; - use cl
;; - icon
;; - refactoring (especially menu)
;; - linum.el bug (reported by Andy Stewart)
;; - flymake bug (reported by TiagoCamargo)

;;; Code:



(eval-when-compile
  (require 'cl))

(defgroup auto-complete nil
  "Auto completion"
  :group 'convenience
  :prefix "ac-")

(defcustom ac-candidate-menu-height 10
  "Max height of candidate menu."
  :type 'number
  :group 'auto-complete)

(defcustom ac-candidate-limit 10
  "Limit number of candidates."
  :type 'number
  :group 'auto-complete)
(defvaralias 'ac-candidate-max 'ac-candidate-limit)

(defcustom ac-modes
  '(emacs-lisp-mode lisp-interaction-mode
                    c-mode cc-mode c++-mode java-mode
                    perl-mode cperl-mode python-mode ruby-mode
                    ecmascript-mode javascript-mode js2-mode php-mode css-mode
                    makefile-mode sh-mode fortran-mode f90-mode ada-mode
                    xml-mode sgml-mode)
  "Major modes `auto-complete-mode' can run on."
  :type '(list symbol)
  :group 'auto-complete)

(defcustom ac-trigger-commands
  '(self-insert-command)
  "Trigger commands that specify whether `auto-complete' should start or not."
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

(defcustom ac-override-local-map nil
  "Non-nil mean use `ac-complete-mode-map' override local map.
Please set it to non-nil only if you faced to some problem about 
minor-mode keymap conflicts."
  :type 'boolean
  :group 'auto-complete)

(defface ac-completion-face
  '((t (:background "darkblue" :foreground "white")))
  "Face for inline completion"
  :group 'auto-complete)

(defface ac-candidate-face
  '((t (:background "lightgray" :foreground "black")))
  "Face for candidate."
  :group 'auto-complete)

(defface ac-selection-face
  '((t (:background "blue" :foreground "white")))
  "Face for selected candidate."
  :group 'auto-complete)

(defvar auto-complete-mode-hook nil
  "Hook for `auto-complete-mode'.")

(defvar ac-completion-overlay nil
  "Overlay of showing inline completion.")

(defvar ac-menu nil
  "Menu instance.")

(defvar ac-menu-direction 1
  "Positive integer means `ac-menu' grows forward.
Or, `ac-menu' grows backward.")

(defvar ac-menu-offset 0
  "Offset to contents.")

(defvar ac-menu-scroll 0
  "Scroll top of `ac-menu'.")

(defvar ac-completing nil
  "Non-nil means `auto-complete-mode' is now working on completion.")

(defvar ac-saved-window-start nil
  "Saved window start value for restore.")

(defvar ac-saved-window-hscroll nil
  "Saved window hscroll value for restore.")

(defvar ac-buffer nil
  "A buffer where auto-complete is started.")

(defvar ac-point nil
  "Start point of prefix.")

(defvar ac-old-point nil
  "Previous start point of prefix.")

(defvar ac-prefix nil
  "Prefix.")
(defvaralias 'ac-target 'ac-prefix)

(defvar ac-limit 0
  "Limit number of candidates.")

(defvar ac-candidates nil
  "Current candidates.")

(defvar ac-selection nil
  "Current candidate index.")

(defvar ac-dwim-enable nil
  "Non-nil means DWIM completion will be allowed.")

(defvar ac-setup-function 'ac-sources-setup
  "This function will be called when `auto-complete-mode' is enabled.")

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

(defvar ac-candidate-filter-function 'ac-adaptive-candidate-filter
  "This function filters candidates and returns new candidates
to be shown.")

(defvar ac-complete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ac-expand)
    (define-key map "\r" 'ac-complete)
    
    (define-key map [down] 'ac-next)
    (define-key map [up] 'ac-previous)

    map)
  "Keymap for completion.")

(defvar ac-saved-local-map nil
  "Old keymap before `auto-complete' activated.")



;;;; Auto completion

(defun ac-setup (menu-width)
  "Setup completion UI at current position."
  ;; Setup menu completion UI
  (when ac-menu
    ;; Reposition
    (ac-menu-delete ac-menu)
    (setq ac-menu nil))
  (save-excursion
    (goto-char ac-point)
    (let ((current-visual-column (ac-current-physical-column))
          (menu-column (current-column))
          (line (line-number-at-pos))
          (height 1))
      (setq ac-saved-window-start (window-start))
      (setq ac-saved-window-hscroll (window-hscroll))
      (setq height ac-candidate-menu-height)
      (setq ac-menu-direction
            (if (and (> line height)
                     (> height
                        (-
                         (max 1 (- (window-height)
                                   (if mode-line-format 1 0)
                                   (if header-line-format 1 0)))
                         (count-lines (window-start) ac-point))))
                -1
              1))
      (let ((window-width (window-width))
            (right (- (+ current-visual-column menu-width)
                      (window-hscroll))))
        (if (and (> right window-width)
                 (>= right menu-width)
                 (>= current-visual-column menu-width))
            (setq menu-column (- menu-column menu-width))))
      ;; Make a room to show menu at the end of buffer
      (forward-line 1)
      (if (eq line (line-number-at-pos))
          (newline)
        (forward-line -1))
      (setq ac-menu (ac-menu-create line menu-column menu-width height ac-menu-direction)))))

(defun ac-cleanup ()
  "Destroy popup menu."
  (ac-deactivate-mode-map)
  (ac-completion-delete)
  (when ac-menu
    (ac-menu-delete ac-menu)
    (set-window-start (selected-window) ac-saved-window-start)
    (set-window-hscroll (selected-window) ac-saved-window-hscroll))
  (setq ac-menu nil)
  (setq ac-menu-scroll 0)
  (setq ac-completing nil)
  (setq ac-point nil)
  (setq ac-candidates nil)
  (setq ac-selection 0)
  (setq ac-selection-scroll-top 0)
  (funcall ac-cleanup-function))

(defun ac-activate-mode-map ()
  "Activate `ac-complete-mode-map'."
  (if ac-override-local-map
      (progn
        (setq ac-saved-local-map overriding-terminal-local-map)
        (if (eq ac-saved-local-map ac-complete-mode-map)
            ;; Maybe never reach here
            (setq ac-saved-local-map nil))
        (setq overriding-terminal-local-map ac-complete-mode-map))
    ;; Rearrange ac-mode-map pair first
    (assq-delete-all 'ac-completing minor-mode-map-alist)
    (push (cons 'ac-completing ac-complete-mode-map) minor-mode-map-alist)))

(defun ac-deactivate-mode-map ()
  "Deactivate `ac-complete-mode-map'."
  (when (and ac-override-local-map
             (eq overriding-terminal-local-map ac-complete-mode-map))
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
         (if (= selection (+ ac-menu-offset (min (ac-menu-height ac-menu) (length ac-candidates))))
             (if (< (+ (- ac-selection ac-menu-offset) ac-menu-scroll) (1- (length ac-candidates)))
                 (prog1 ac-selection
                   (setq ac-menu-scroll (1+ ac-menu-scroll))
                   (ac-redraw-candidates))
               (setq ac-menu-scroll 0)
               (ac-redraw-candidates)
               ac-menu-offset)
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
             (if (= ac-menu-scroll 0)
                 (prog1 (1- (+ ac-menu-offset (min (ac-menu-height ac-menu) (length ac-candidates))))
                   (setq ac-menu-scroll (- (length ac-candidates) (min (ac-menu-height ac-menu) (length ac-candidates))))
                   (ac-redraw-candidates))
               (setq ac-menu-scroll (1- ac-menu-scroll))
               (ac-redraw-candidates)
               ac-selection)
           selection)))))

(defun ac-expand-string (string &optional remove-undo-boundary)
  "Expand `STRING' into the buffer and update `ac-prefix' to `STRING'.
This function records deletion and insertion sequences by `undo-boundary'.
If `remove-undo-boundary' is non-nil, this function also removes `undo-boundary'
that have been made before in this function."
  (undo-boundary)
  ;; We can't use primitive-undo since it undoes by
  ;; groups, divided by boundaries.
  ;; We don't want boundary between deletion and insertion.
  ;; So do it manually.
  ;; Delete region silently for undo:
  (if remove-undo-boundary
      (progn
        (let (buffer-undo-list)
          (save-excursion
            (delete-region ac-point (point))))
        (setq buffer-undo-list
              (nthcdr 2 buffer-undo-list)))
    (delete-region ac-point (point)))
  (insert string)
  ;; Sometimes, possible when omni-completion used, (insert) added
  ;; to buffer-undo-list strange record about position changes.
  ;; Delete it here:
  (when (and remove-undo-boundary
             (integerp (cadr buffer-undo-list)))
    (setcdr buffer-undo-list (nthcdr 2 buffer-undo-list)))
  (undo-boundary)
  (setq ac-prefix string))

(defun ac-expand ()
  "Try expansion but select next if expanded twice."
  (interactive)
  (if (and ac-dwim ac-dwim-enable)
      (ac-complete)
    (let ((old-prefix ac-prefix)
          (repeated (eq last-command this-command))
          string)
      (setq string (or
                    (and ac-completion-overlay
                         (prog1 (setq string
                                      (concat ac-prefix
                                              (overlay-get ac-completion-overlay 'string)))
                           (ac-completion-delete)))
                    (ac-get-selected-candidate)))
      (when (equal old-prefix string)
        (ac-next)
        (setq string (ac-get-selected-candidate)))
      (ac-expand-string string repeated)))
  ;; Do reposition if menu at long line
  (when (ac-menu-at-wrapped-line)
    (ac-setup (ac-menu-width ac-menu))
    (ac-redraw-candidates)))

(defun ac-expand-common ()
  "Try expansion common part."
  (interactive)
  (let ((common (try-completion ac-prefix ac-candidates))
        (buffer-undo-list t))
    (when (stringp common)
      (delete-region ac-point (point))
      (insert common)
      (setq ac-prefix common))))

(defun ac-complete ()
  "Try completion."
  (interactive)
  (let* ((candidate (ac-get-selected-candidate))
         (action (ac-get-candidate-action candidate)))
    (ac-expand-string candidate)
    (if action
        (funcall action))
    (ac-abort)))

(defun ac-abort ()
  "Abort completion."
  (ac-cleanup))

(defun ac-stop ()
  "Stop completiong."
  (interactive)
  (ac-abort))

(defun ac-redraw-candidates ()
  "Redraw the menu contents."
  ;; Show inline completion.
  (let ((string (try-completion ac-prefix ac-candidates)))
    (if (and (stringp string)
             (> (length string) (length ac-prefix)))
        (ac-completion-show (point) (substring string (length ac-prefix)))
      (ac-completion-delete)))
  
  ;; Show menu completion
  (let ((i ac-menu-offset))
    ;; Show line and set string to the line.
    (mapc
     (lambda (candidate)
       (when (< i (ac-menu-height ac-menu))
         (ac-menu-show-line ac-menu i)
         (ac-menu-set-line-string ac-menu i candidate
                                  (if (= i ac-selection)
                                      (or (ac-get-candidate-property 'selection-face candidate) 'ac-selection-face)
                                    (or (ac-get-candidate-property 'candidate-face candidate) 'ac-candidate-face)))
         (setq i (1+ i))))
     (nthcdr ac-menu-scroll ac-candidates))
    ;; If only one candidate is remaining,
    ;; make the candidate menu disappeared.
    ;(if (eq (- (length ac-candidates) ac-menu-scroll) 1)
    ;    (ac-menu-hide-line ac-menu ac-menu-offset))
    ;; Ensure lines visible
    (if (and (> ac-menu-direction 0)
             (> i (-
                   (max 1 (- (window-height)
                             (if mode-line-format 1 0)
                             (if header-line-format 1 0)))
                   (count-lines (window-start) (point)))))
        (recenter (- (1+ i))))
    ;; Scroll horizontally due to make sure menu is visible.
    ;(if (> i ac-menu-offset)
    ;    (let ((window-width (window-width))
    ;          (right (- (+ (ac-menu-column ac-menu) (ac-menu-width ac-menu))
    ;                    (window-hscroll))))
    ;      (if (> right window-width)
    ;          (scroll-left (- right window-width)))))

    ;; Hide remaining lines
    (if (> ac-menu-direction 0)
        (while (< i (ac-menu-height ac-menu))
          (ac-menu-hide-line ac-menu i)
          (setq i (1+ i)))
      (dotimes (i ac-menu-offset)
        (ac-menu-hide-line ac-menu i)))))

(defun ac-update-candidates (candidates)
  (setq ac-menu-offset (if (> ac-menu-direction 0)
                           0
                         (- (ac-menu-height ac-menu)
                            (min (ac-menu-height ac-menu)
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
  (ac-redraw-candidates))

(defun ac-get-selected-candidate ()
  (overlay-get (ac-menu-line-overlay ac-menu ac-selection) 'real-string))

(defun ac-get-candidate-action (candidate)
  (ac-get-candidate-property 'action candidate))

(defun ac-propertize-candidate (candidate &rest properties)
  (apply 'propertize candidate properties))

(defun ac-get-candidate-property (prop candidate)
  (get-text-property 0 prop candidate))

(defun ac-select-candidate (selection)
  "Select candidate pointed by `SELECTION'."
  (when ac-candidates
    (let ((c1 (nth (+ (- ac-selection ac-menu-offset) ac-menu-scroll) ac-candidates))
          (c2 (nth (+ (- selection ac-menu-offset) ac-menu-scroll) ac-candidates)))
      (ac-menu-set-line-string ac-menu ac-selection c1
                               (or (ac-get-candidate-property 'candidate-face c1)
                                   'ac-candidate-face))
      (ac-menu-set-line-string ac-menu selection c2
                               (or (ac-get-candidate-property 'selection-face c2)
                                   'ac-selection-face))
      (setq ac-selection selection))))

(defun ac-start ()
  "Start completion."
  (interactive)
  (let* ((point (save-excursion (funcall ac-prefix-function)))
         (reposition (or (not (equal ac-point point))
                         ;; If menu direction is positive and next visual line belongs
                         ;; to same buffer line, then need reposition
                         (and (> ac-menu-direction 0)
                              (ac-menu-at-wrapped-line)))))
    (if (null point)
        (ac-abort)
      (setq ac-buffer (current-buffer))
      (setq ac-point point)
      (when (not (equal ac-point ac-old-point))
        (setq ac-old-point point))
      (setq ac-prefix (buffer-substring-no-properties point (point)))
      (setq ac-limit ac-candidate-limit)
      (if (or reposition (null ac-menu))
          (save-excursion
            (funcall ac-init-function)))
      (let (candidates
            width
            (current-width (if ac-menu (ac-menu-width ac-menu) 0)))
        (setq candidates (if (or ac-completing
                                 (not (integerp ac-auto-start))
                                 (>= (length ac-prefix) ac-auto-start))
                             (save-excursion
                               (funcall ac-candidate-function))))
        (if ac-candidate-filter-function
            (setq candidates (funcall ac-candidate-filter-function candidates)))
        (setq width (let ((w '(0)) s)
                      (dotimes (i ac-candidate-menu-height)
                        (setq s (nth i candidates))
                        (if (stringp s) (push (string-width s) w)))
                      (apply 'max w)))
        (if (or reposition
                (null ac-menu)
                (> width current-width)
                (< width (- current-width 10)))
            (ac-setup (* (ceiling (/ width 10.0)) 10)))
        (ac-update-candidates candidates)))))

(defun ac-adaptive-candidate-filter (candidates)
  "Filter candidates according to length and history (not yet)."
  ;(if (> (length candidates) 1)
  ;    (let ((length (length ac-prefix)))
  ;      (delq nil
  ;            (mapcar (lambda (candidate)
  ;                      (if (> (- (length candidate) length) 2)
  ;                          candidate))
  ;                    candidates)))
  ;  candidates)
  candidates)

(defun ac-trigger-command-p ()
  "Return non-nil if `this-command' is a trigger command."
  (or (memq this-command ac-trigger-commands)
      (and ac-completing
           (memq this-command
                 '(delete-backward-char
                   backward-delete-char
                   backward-delete-char-untabify)))))

(defun ac-current-physical-column ()
  "Current physical column. (not logical column)"
  (- (current-column) (save-excursion (vertical-motion 0) (current-column))))

(defun ac-menu-at-wrapped-line ()
  "Return non-nil if current line is long and wrapped to next visual line."
  (eq (line-number-at-pos)
      (save-excursion
        (vertical-motion 1)
        (line-number-at-pos))))

(defun ac-handle-pre-command ()
  (condition-case var
      (if (or (ac-trigger-command-p)
              (and (symbolp this-command)
                   (string-match "^ac-" (symbol-name this-command))))
          ;; Not to cause inline completion to be disrupted.
          (ac-completion-hide)
        (ac-abort))
    (error (ac-error var))))

(defun ac-handle-post-command ()
  (condition-case var
      (if (and (or ac-auto-start
                   ac-completing)
               (not isearch-mode)
               (ac-trigger-command-p))
          (ac-start))
    (error (ac-error var))))

(defun ac-error (&optional var)
  "Report an error and disable `auto-complete-mode'."
  (ignore-errors
    (message "auto-complete error: %s" var)
    (auto-complete-mode nil)))

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
        (funcall ac-setup-function)
        (add-hook 'post-command-hook 'ac-handle-post-command nil t)
        (add-hook 'pre-command-hook 'ac-handle-pre-command nil t)
        (run-hooks 'auto-complete-mode-hook))
    (remove-hook 'post-command-hook 'ac-handle-post-command t)
    (remove-hook 'pre-command-hook 'ac-handle-pre-command t)
    (ac-abort)))

(define-global-minor-mode global-auto-complete-mode
  auto-complete-mode auto-complete-mode-maybe
  :group 'auto-complete)



;;;; Basic cache facility

(defvar ac-clear-variables-after-save nil)

(defun ac-clear-variable-after-save (variable)
  (push variable ac-clear-variables-after-save))

(defun ac-clear-variables-after-save ()
  (dolist (variable ac-clear-variables-after-save)
    (set variable nil)))



;;;; Sources implementation

(defvar ac-sources '(ac-source-words-in-buffer)
  "Sources for completion.

Source takes a form of just function which returns candidates or alist:

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
(make-variable-buffer-local 'ac-sources)

(defvar ac-sources-prefix-function 'ac-sources-prefix-default
  "Default prefix function for sources.
You should override this variable instead of ac-prefix-function.")

(defvar ac-current-sources nil
  "Current working sources.")

(defvar ac-omni-completion-sources nil
  "An alist of REGEXP and SOURCES.
If matched regexp, switch to omni-completion mode and
use SOURCES as `ac-sources'.")
(make-variable-buffer-local 'ac-omni-completion-sources)

(defvar ac-sources-omni-completion nil
  "Non-nil means `auto-complete-mode' is now working on omni-completion.")

(defun ac-sources-setup ()
  "Implementation for `ac-setup-function' by sources."
  (make-local-variable 'ac-clear-variables-after-save)
  (add-hook 'after-save-hook 'ac-clear-variables-after-save nil t))

(defun ac-sources-init ()
  "Implementation for `ac-init-function' by sources."
  (or ac-current-sources (setq ac-current-sources ac-sources))
  (dolist (source ac-current-sources)
    (let ((init-function (ac-get-source-property 'init source)))
      (if init-function
          (funcall init-function)))))

(defun ac-sources-cleanup ()
  "Implementation for `ac-cleanup-function' by sources."
  (setq ac-current-sources nil)
  (setq ac-sources-omni-completion nil))

(defun ac-sources-prefix ()
  "Implemention for `ac-prefix-function' by sources."
  (let (point)
    (dolist (pair ac-omni-completion-sources)
      (when (looking-back (car pair) nil t)
        (setq ac-current-sources (cdr pair))
        (setq ac-sources-omni-completion t)
        (setq ac-completing t)
        (setq point (match-end 0))))
    (or point
        (if (and ac-completing ac-sources-omni-completion)
            ac-point
          (setq ac-current-sources ac-sources)
          (setq ac-sources-omni-completion nil)
          (funcall ac-sources-prefix-function)))))

(defun ac-sources-prefix-default ()
  "Default implementation for `ac-sources-prefix-function'."
  (require 'thingatpt)
  (car-safe (bounds-of-thing-at-point 'symbol)))

(defun ac-sources-candidate ()
  "Implementation for `ac-cadidates-function' by sources."
  (let (candidates)
    (dolist (source ac-current-sources)
      (let* ((ac-limit (or (ac-get-source-property 'limit source) ac-limit))
             (requires (ac-get-source-property 'requires source))
             cand)
        (when (or ac-sources-omni-completion
                  (>= (length ac-prefix)
                      (if (integerp requires)
                          requires
                        1)))
          (setq cand
                (delq nil
                      (mapcar (lambda (candidate)
                                (ac-propertize-candidate candidate
                                                         'action (ac-get-source-property 'action source)
                                                         'face (ac-get-source-property 'candidate-face source)
                                                         'candidate-face (ac-get-source-property 'candidate-face source)
                                                         'selection-face (ac-get-source-property 'selection-face source)))
                              (funcall (ac-get-source-property 'candidates source))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) (copy-sequence cand)) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(defun ac-get-source-property (property source)
  (if (symbolp source)
      (setq source (symbol-value source)))
  (if (and (functionp source)
           (eq property 'candidates))
      source
    (if (consp source)
        (assoc-default property source))))



;;;; Standard sources

(defun ac-candidate-words-in-buffer (&optional limit)
  "Default implemention for `ac-candidate-function'."
  (or limit (setq limit ac-limit))
  (if (> (length ac-prefix) 0)
      (let ((i 0)
            candidate
            candidates
            (regexp (concat "\\b" (regexp-quote ac-prefix) "\\(\\s_\\|\\sw\\)*\\b")))
        (save-excursion
          ;; Search backward
          (goto-char ac-point)
          (while (and (or (eq limit t)
                          (< i limit))
                      (re-search-backward regexp nil t))
            (setq candidate (match-string-no-properties 0))
            (unless (member candidate candidates)
              (push candidate candidates)
              (setq i (1+ i))))
          ;; Search backward
          (goto-char (+ ac-point (length ac-prefix)))
          (while (and (or (eq limit t)
                          (< i limit))
                      (re-search-forward regexp nil t))
            (setq candidate (match-string-no-properties 0))
            (unless (member candidate candidates)
              (push candidate candidates)
              (setq i (1+ i))))
          (nreverse candidates)))))

(defvar ac-source-words-in-buffer
  '((candidates . ac-candidate-words-in-buffer))
  "Source for completing words in current buffer.")

(defvar ac-word-index nil
  "Word index for individual buffer.")

(ac-clear-variable-after-save 'ac-word-index)

(defvar ac-source-words-in-all-buffer
  '((init
     . (lambda ()
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (if (not (local-variable-p 'ac-word-index))
                 (make-local-variable 'ac-word-index))
             (if (eq buffer ac-buffer)
                 (setq ac-word-index (ac-candidate-words-in-buffer t))
               (if (and (null ac-word-index)
                        (< (buffer-size) 102400))
                   (save-excursion
                     (goto-char (point-min))
                     (while (re-search-forward "\\b\\(\\s_\\|\\sw\\)+\\b" nil t)
                       (let ((candidate (match-string-no-properties 0)))
                         (if (not (member candidate ac-word-index))
                             (push candidate ac-word-index))))
                     (setq ac-word-index (nreverse ac-word-index)))))))))
    (candidates
     . (lambda ()
         (let ((candidates)
               (buffers (buffer-list)))
           (while (and (< (length candidates) ac-limit)
                       buffers)
             (setq candidates (append candidates (all-completions ac-prefix (buffer-local-value 'ac-word-index (car buffers)))))
             (setq buffers (cdr buffers)))
           candidates))))
  "Source for completing words in all buffer.")

(defvar ac-source-symbols
  '((candidates
     . (lambda ()
         (all-completions ac-prefix obarray))))
  "Source for Emacs lisp symbols.")

(defvar ac-source-abbrev
  `((candidates
     . (lambda ()
         (append
          (all-completions ac-prefix global-abbrev-table)
          (all-completions ac-prefix local-abbrev-table))))
    (action
     . expand-abbrev))
  "Source for abbrev.")

(defvar ac-source-files-in-current-dir
  '((candidates
     . (lambda ()
         (all-completions ac-prefix (directory-files default-directory)))))
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
              (mapc (lambda (child)
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
               (ignore-errors (imenu--make-index-alist)))))
    (candidates . ac-imenu-candidate))
  "Source for imenu.")

(defmacro ac-define-dictionary-source (name list)
  "Define dictionary source named `NAME'.
`LIST' is a list of string.
This is useful if you just want to define a dictionary/keywords source."
  `(defvar ,name
     '((candidates . (lambda () (all-completions ac-prefix ,list))))))



;;;; Inline Completion

(defvar ac-completion-dummy-char-marker (make-marker))

(defun ac-completion-show (point string)
  "Show inline completion."
  (save-excursion
    (let ((width 0)
          (string-width (string-width string)))
      ;; Cacluate string space to show completion.
      (goto-char point)
      (while (and (not (eolp))
                  (< width string-width))
        (setq width (+ width (char-width (char-after))))
        (forward-char))

      ;; Show completion.
      (goto-char point)
      (cond
       ((= width 0)
        (set-marker ac-completion-dummy-char-marker point)
        (let ((buffer-undo-list t))
          (insert " "))
        (setq width 1))
       ((<= width string-width)
        ;; No space to show
        ;; Do nothing
        )
       ((> width string-width)
        ;; Need to fill space
        (setq string (concat string (make-string (- width string-width) ? )))))
      (setq string (propertize string 'face 'ac-completion-face))
      (if ac-completion-overlay
          (progn
            (move-overlay ac-completion-overlay point (+ point width))
            (overlay-put ac-completion-overlay 'invisible nil))
        (setq ac-completion-overlay (make-overlay point (+ point width)))
        (overlay-put ac-completion-overlay 'priority 9999))
      (overlay-put ac-completion-overlay 'display (substring string 0 1))
      ;; TODO no width but char
      (overlay-put ac-completion-overlay 'after-string (substring string 1))
      (overlay-put ac-completion-overlay 'string string))))

(defun ac-completion-hide ()
  "Hide inline completion."
  (when (marker-position ac-completion-dummy-char-marker)
    (let ((buffer-undo-list t))
      (save-excursion
        (goto-char ac-completion-dummy-char-marker)
        (delete-char 1)
        (set-marker ac-completion-dummy-char-marker nil))))
  (when ac-completion-overlay
    (move-overlay ac-completion-overlay (point-min) (point-min))
    (overlay-put ac-completion-overlay 'invisible t)
    (overlay-put ac-completion-overlay 'display nil)
    (overlay-put ac-completion-overlay 'after-string nil)))

(defun ac-completion-delete ()
  "Delete inline completion."
  (ac-completion-hide)
  (when ac-completion-overlay
    (delete-overlay ac-completion-overlay)
    (setq ac-completion-overlay nil)))

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
    (funcall (overlay-get overlay 'set-string-function) menu overlay string face)))

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

(defun ac-menu-create (line column width height direction)
  "Create popup menu."
  (save-excursion
    (let ((overlays (make-vector height nil))
          (window (selected-window))
          menu-visual-column
          current-visual-column)
      (goto-line line)
      (move-to-column column)
      (setq menu-visual-column (ac-current-physical-column))
      (dotimes (i height)
        (let (overlay begin w (prefix "") (postfix ""))
	  (vertical-motion direction)
          (move-to-column (+ (current-column) menu-visual-column))
	  (setq current-visual-column (ac-current-physical-column))

	  (cond
           ((> current-visual-column menu-visual-column)
            (backward-char)
            (setq current-visual-column (ac-current-physical-column))
            (if (< current-visual-column menu-visual-column)
                (setq prefix (make-string (- menu-visual-column current-visual-column) ? ))))
	   ;; Extend short buffer lines by menu prefix (line of spaces)
           ((< current-visual-column menu-visual-column)
            (setq prefix (make-string (- menu-visual-column current-visual-column) ? ))))

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
                       (lambda (menu overlay string &optional face)
                         (overlay-put overlay
                                      'after-string
                                      (concat (overlay-get overlay 'prefix)
                                              (propertize (ac-menu-create-line-string menu string) 'face face)
                                              (overlay-get overlay 'postfix)))))
          (aset overlays
		(if (> direction 0)
		    i
		  (- height i 1))
		overlay)))
      (let ((i 100))
        (mapc (lambda (overlay)
                (overlay-put overlay 'priority i)
                (setq i (1+ i)))
              (nreverse (append overlays nil))))
      (list line column width height overlays))))

(defun ac-menu-delete (menu)
  "Delete `MENU'."
  (mapcar 'delete-overlay (ac-menu-overlays menu)))

(provide 'auto-complete)
;;; auto-complete.el ends here

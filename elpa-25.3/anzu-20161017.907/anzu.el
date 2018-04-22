;;; anzu.el --- Show number of matches in mode-line while searching -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-anzu
;; Package-Version: 20161017.907
;; Version: 0.62
;; Package-Requires: ((emacs "24.3"))

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

;; `anzu.el' is an Emacs port of `anzu.vim'.
;;
;; `anzu.el' provides a minor mode which displays 'current match/total
;; matches' in the mode-line in various search modes.  This makes it
;; easy to understand how many matches there are in the current buffer
;; for your search query.

;; To use this package, add following code to your init.el or .emacs
;;
;;   (global-anzu-mode +1)
;;

;;; Code:

(require 'cl-lib)
(require 'thingatpt)

(declare-function migemo-forward 'migemo)

(defgroup anzu nil
  "Show searched position in mode-line"
  :group 'isearch)

(defcustom anzu-mode-lighter " Anzu"
  "Lighter of anzu-mode"
  :type 'string)

(defcustom anzu-cons-mode-line-p t
  "Set nil if you use your own mode-line setting"
  :type 'boolean)

(defcustom anzu-minimum-input-length 1
  "Minimum input length to enable anzu"
  :type 'integer)

(defcustom anzu-search-threshold nil
  "Limit of search number"
  :type '(choice (integer :tag "Threshold of search")
                 (const :tag "No threshold" nil)))

(defcustom anzu-replace-threshold nil
  "Limit of replacement overlays."
  :type '(choice (integer :tag "Threshold of replacement overlays")
                 (const :tag "No threshold" nil)))

(defcustom anzu-use-migemo nil
  "Flag of using migemo"
  :type 'boolean)

(defcustom anzu-mode-line-update-function #'anzu--update-mode-line-default
  "Function which return mode-line string. This must be non-nil."
  :type 'function)

(defcustom anzu-regexp-search-commands '(isearch-forward-regexp
                                         isearch-backward-regexp)
  "Search function which use regexp."
  :type '(repeat function))

(defcustom anzu-input-idle-delay 0.05
  "Idle second for updating modeline at replace commands"
  :type 'number)

(defcustom anzu-deactivate-region nil
  "Deactive region if you use anzu a replace command with region"
  :type 'boolean)

(defcustom anzu-replace-at-cursor-thing 'defun
  "Replace thing. This parameter is same as `thing-at-point'"
  :type 'symbol)

(defcustom anzu-replace-to-string-separator ""
  "Separator of `to' string"
  :type 'string)

(defface anzu-mode-line
  '((t (:foreground "magenta" :weight bold)))
  "face of anzu modeline")

(defface anzu-mode-line-no-match
  '((t (:inherit anzu-mode-line)))
  "face of anzu modeline in no match case")

(defface anzu-replace-highlight
  '((t :inherit query-replace))
  "highlight of replaced string")

(defface anzu-match-1
  '((((class color) (background light))
     :background "aquamarine" :foreground "black")
    (((class color) (background dark))
     :background "limegreen" :foreground "black")
    (t :inverse-video t))
  "First group of match.")

(defface anzu-match-2
  '((((class color) (background light))
     :background "springgreen" :foreground "black")
    (((class color) (background dark))
     :background "yellow" :foreground "black")
    (t :inverse-video t))
  "Second group of match.")

(defface anzu-match-3
  '((((class color) (background light))
     :background "yellow" :foreground "black")
    (((class color) (background dark))
     :background "aquamarine" :foreground "black")
    (t :inverse-video t))
  "Third group of match.")

(defface anzu-replace-to
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "yellow"))
  "highlight of replace string")

(defvar anzu--total-matched 0)
(defvar anzu--current-position 0)
(defvar anzu--overflow-p nil)
(defvar anzu--last-isearch-string nil)
(defvar anzu--cached-positions nil)
(defvar anzu--last-command nil)
(defvar anzu--state nil)
(defvar anzu--cached-count 0)
(defvar anzu--last-replace-input "")
(defvar anzu--last-search-state nil)
(defvar anzu--last-replaced-count nil)
(defvar anzu--outside-point nil)
(defvar anzu--history nil)
(defvar anzu--query-defaults nil)

(defun anzu--validate-regexp (regexp)
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defsubst anzu--construct-position-info (count overflow positions)
  (list :count count :overflow overflow :positions positions))

(defsubst anzu--case-fold-search (input)
  (when case-fold-search
    (let ((case-fold-search nil))
      (not (string-match-p "[A-Z]" input)))))

(defsubst anzu--word-search-p ()
  (and (not (memq anzu--last-command anzu-regexp-search-commands))
       (not isearch-regexp)))

(defsubst anzu--isearch-regexp-function ()
  (or (bound-and-true-p isearch-regexp-function)
      (bound-and-true-p isearch-word)))

(defun anzu--transform-input (str)
  (cond ((eq (anzu--isearch-regexp-function) 'isearch-symbol-regexp)
         (setq str (isearch-symbol-regexp str)))
        ((anzu--word-search-p)
         (setq str (regexp-quote str)))
        (t str)))

(defsubst anzu--use-migemo-p ()
  (when anzu-use-migemo
    (unless (featurep 'migemo)
      (error "Error: migemo is not loaded"))
    (bound-and-true-p migemo-isearch-enable-p)))

(defun anzu--search-all-position (str)
  (unless anzu--last-command
    (setq anzu--last-command last-command))
  (let ((input (anzu--transform-input str)))
    (if (not (anzu--validate-regexp input))
        anzu--cached-positions
      (save-excursion
        (goto-char (point-min))
        (let ((positions '())
              (count 0)
              (overflow nil)
              (finish nil)
              (search-func (if (anzu--use-migemo-p)
                               (lambda (word &optional bound noerror count)
                                 (ignore-errors
                                   (migemo-forward word bound noerror count)))
                             #'re-search-forward))
              (case-fold-search (anzu--case-fold-search input)))
          (while (and (not finish) (funcall search-func input nil t))
            (push (cons (match-beginning 0) (match-end 0)) positions)
            (cl-incf count)
            (when (= (match-beginning 0) (match-end 0)) ;; Case of anchor such as "^"
              (if (eobp)
                  (setq finish t)
                (forward-char 1)))
            (when (and anzu-search-threshold (>= count anzu-search-threshold))
              (setq overflow t finish t)))
          (let ((result (anzu--construct-position-info count overflow (reverse positions))))
            (setq anzu--cached-positions (copy-sequence result))
            result))))))

(defun anzu--where-is-here (positions here)
  (cl-loop for (start . end) in positions
           for i = 1 then (1+ i)
           when (and (>= here start) (<= here end))
           return i
           finally return 0))

(defun anzu--use-result-cache-p (input)
  (and (eq (anzu--isearch-regexp-function) (car anzu--last-search-state))
       (eq isearch-regexp (cdr anzu--last-search-state))
       (string= input anzu--last-isearch-string)))

(defun anzu--update (query)
  (when (>= (length query) anzu-minimum-input-length)
    (let ((result (if (anzu--use-result-cache-p query)
                      anzu--cached-positions
                    (anzu--search-all-position query))))
      (let ((curpos (anzu--where-is-here (plist-get result :positions) (point))))
        (setq anzu--total-matched (plist-get result :count)
              anzu--overflow-p (plist-get result :overflow)
              anzu--current-position curpos
              anzu--last-search-state (cons (anzu--isearch-regexp-function) isearch-regexp)
              anzu--last-isearch-string query)
        (force-mode-line-update)))))

(defun anzu--update-post-hook ()
  (anzu--update isearch-string))

(defconst anzu--mode-line-format '(:eval (anzu--update-mode-line)))

(defsubst anzu--mode-line-not-set-p ()
  (and (listp mode-line-format)
       (member anzu--mode-line-format mode-line-format)))

(defun anzu--cons-mode-line-search ()
  (anzu--cons-mode-line 'search))

(defun anzu--cons-mode-line (state)
  (setq anzu--state state)
  (when (and anzu-cons-mode-line-p (not (anzu--mode-line-not-set-p)))
    (setq mode-line-format (cons anzu--mode-line-format mode-line-format))))

(defsubst anzu--reset-status ()
  (setq anzu--total-matched 0
        anzu--current-position 0
        anzu--state nil
        anzu--last-command nil
        anzu--last-isearch-string nil
        anzu--overflow-p nil))

(defun anzu--reset-mode-line ()
  (anzu--reset-status)
  (when (and anzu-cons-mode-line-p (anzu--mode-line-not-set-p))
    (setq mode-line-format (delete anzu--mode-line-format mode-line-format))))

(defsubst anzu--format-here-position (here total)
  (if (and anzu--overflow-p (zerop here))
      (format "%d+" total)
    here))

(defun anzu--update-mode-line-default (here total)
  (when anzu--state
    (let ((status (cl-case anzu--state
                    (search (format "(%s/%d%s)"
                                    (anzu--format-here-position here total)
                                    total (if anzu--overflow-p "+" "")))
                    (replace-query (format "(%d replace)" total))
                    (replace (format "(%d/%d)" here total))))
          (face (if (and (zerop total) (not (string= isearch-string "")))
                    'anzu-mode-line-no-match
                  'anzu-mode-line)))
      (propertize status 'face face))))

(defun anzu--update-mode-line ()
  (funcall anzu-mode-line-update-function anzu--current-position anzu--total-matched))

;;;###autoload
(define-minor-mode anzu-mode
  "minor-mode which display search information in mode-line."
  :init-value nil
  :global     nil
  :lighter    anzu-mode-lighter
  (if anzu-mode
      (progn
        (setq-local anzu--state nil)
        (add-hook 'isearch-update-post-hook #'anzu--update-post-hook nil t)
        (add-hook 'isearch-mode-hook #'anzu--cons-mode-line-search nil t)
        (add-hook 'isearch-mode-end-hook #'anzu--reset-mode-line nil t))
    (remove-hook 'isearch-update-post-hook #'anzu--update-post-hook t)
    (remove-hook 'isearch-mode-hook #'anzu--cons-mode-line-search t)
    (remove-hook 'isearch-mode-end-hook #'anzu--reset-mode-line t)
    (anzu--reset-mode-line)))

(defun anzu--turn-on ()
  (unless (minibufferp)
    (anzu-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-anzu-mode anzu-mode anzu--turn-on)

(defsubst anzu--query-prompt-base (use-region use-regexp)
  (concat "Query replace"
          (if current-prefix-arg " word" "")
          (if use-regexp " regexp" "")
          (if use-region " in region" ""))  )

(defun anzu--query-prompt (use-region use-regexp at-cursor isearch-p)
  (let ((prompt (anzu--query-prompt-base use-region use-regexp)))
    (if (and anzu--query-defaults (not at-cursor) (not isearch-p))
        (format "%s (default %s -> %s) "
                prompt
                (query-replace-descr (caar anzu--query-defaults))
                (query-replace-descr (cdar anzu--query-defaults)))
      prompt)))

(defvar anzu--replaced-markers nil)
(defsubst anzu--set-marker (beg buf)
  (let ((m (make-marker)))
    (set-marker m beg buf)
    (push m anzu--replaced-markers)))

(defun anzu--make-overlay (begin end face prio)
  (let ((ov (make-overlay begin end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority prio)
    (overlay-put ov 'anzu-overlay t)
    ov))

(defun anzu--add-match-group-overlay (match-data groups)
  (when (>= groups 3)
    (anzu--make-overlay (cl-fifth match-data) (cl-sixth match-data)
                        'anzu-match-3 1001))
  (when (>= groups 2)
    (anzu--make-overlay (cl-third match-data) (cl-fourth match-data)
                        'anzu-match-2 1001))
  (anzu--make-overlay (cl-first match-data) (cl-second match-data)
                      'anzu-match-1 1001))

(defun anzu--add-overlay (beg end)
  (let* ((match-data (match-data))
         (groups (/ (- (length match-data) 2) 2)))
    (when (>= groups 1)
      (anzu--add-match-group-overlay (cddr match-data) groups))
    (let ((ov (anzu--make-overlay beg end 'anzu-replace-highlight 1000)))
      (overlay-put ov 'from-string (buffer-substring-no-properties beg end))
      (overlay-put ov 'anzu-replace t))))

(defsubst anzu--cleanup-markers ()
  (mapc (lambda (m) (set-marker m nil)) anzu--replaced-markers)
  (setq anzu--replaced-markers nil))

;; Return highlighted count
(defun anzu--count-and-highlight-matched (buf str replace-beg replace-end
                                          use-regexp overlay-limit case-sensitive)
  (anzu--cleanup-markers)
  (when (not use-regexp)
    (setq str (regexp-quote str)))
  (if (not (anzu--validate-regexp str))
      anzu--cached-count
    (with-current-buffer buf
      (save-excursion
        (let* ((overlay-beg replace-beg)
               (overlay-end (min replace-end overlay-limit)))
          (goto-char overlay-beg)
          (let ((count 0)
                (overlayed 0)
                (finish nil)
                (case-fold-search (if case-sensitive
                                      nil
                                    (anzu--case-fold-search str))))
            (while (and (not finish) (re-search-forward str replace-end t))
              (cl-incf count)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (when (= beg end)
                  (if (eobp)
                      (setq finish t)
                    (forward-char 1)))
                (when (and replace-end (> (point) replace-end))
                  (setq finish t))
                (when (and (>= beg overlay-beg) (<= end overlay-end) (not finish))
                  (cl-incf overlayed)
                  (anzu--add-overlay beg end))))
            (setq anzu--cached-count count)
            overlayed))))))

(defun anzu--search-outside-visible (buf input beg end use-regexp)
  (let ((searchfn (if use-regexp #'re-search-forward #'search-forward)))
    (when (or (not use-regexp) (anzu--validate-regexp input))
      (with-selected-window (get-buffer-window buf)
        (goto-char beg)
        (when (funcall searchfn input end t)
          (setq anzu--outside-point (match-beginning 0))
          (let ((overlay-limit (anzu--overlay-limit)))
            (anzu--count-and-highlight-matched buf input beg end use-regexp
                                               overlay-limit nil)))))))

(defconst anzu--from-to-separator
  (propertize
   (or (ignore-errors
         (if (char-displayable-p ?\u2192) " \u2192 " " -> "))
       " -> ")
   'face 'minibuffer-prompt))

(defsubst anzu--separator ()
  (propertize "\0" 'display anzu--from-to-separator 'separator t))

(defun anzu--check-minibuffer-input (buf beg end use-regexp overlay-limit)
  (let* ((content (minibuffer-contents))
         (to (when (and (string-match (anzu--separator) content)
                        (get-text-property (match-beginning 0) 'separator content))
               (substring-no-properties content (match-end 0))))
         (from (or (and to (substring-no-properties content 0 (match-beginning 0)))
                   content))
         (empty-p (string= from ""))
         (overlayed (if empty-p
                        (setq anzu--cached-count 0)
                      (anzu--count-and-highlight-matched buf from beg end use-regexp
                                                         overlay-limit nil))))
    (when anzu--outside-point
      (setq anzu--outside-point nil)
      (with-selected-window (get-buffer-window buf)
        (goto-char beg)))
    (when (and (not empty-p) (zerop overlayed))
      (anzu--search-outside-visible buf from beg end use-regexp))
    (when to
      (setq anzu--last-replace-input "")
      (anzu--append-replaced-string to buf beg end use-regexp overlay-limit from))
    (setq anzu--total-matched anzu--cached-count)
    (force-mode-line-update)))

(defun anzu--clear-overlays (buf beg end)
  (with-current-buffer buf
    (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
      (when (overlay-get ov 'anzu-overlay)
        (delete-overlay ov)))))

(defun anzu--transform-from-to-history ()
  (let ((separator (anzu--separator)))
    (append (mapcar (lambda (from-to)
                      (concat (query-replace-descr (car from-to))
                              separator
                              (query-replace-descr (cdr from-to))))
                    anzu--query-defaults)
            (symbol-value query-replace-from-history-variable))))

(defun anzu--read-from-string (prompt beg end use-regexp overlay-limit)
  (let ((curbuf (current-buffer))
        (blink-matching-paren nil)
        (anzu--history (anzu--transform-from-to-history))
        timer is-input)
    (unwind-protect
        (minibuffer-with-setup-hook
            #'(lambda ()
                (setq timer (run-with-idle-timer
                             (max anzu-input-idle-delay 0.01)
                             'repeat
                             (lambda ()
                               (anzu--clear-overlays curbuf nil nil)
                               (with-selected-window (or (active-minibuffer-window)
                                                         (minibuffer-window))
                                 (anzu--check-minibuffer-input
                                  curbuf beg end use-regexp overlay-limit))))))
          (prog1 (read-from-minibuffer (format "%s: " prompt)
                                       nil nil nil 'anzu--history nil t)
            (setq is-input t)))
      (when timer
        (cancel-timer timer)
        (setq timer nil)
        (unless is-input
          (goto-char beg))))))

(defun anzu--query-validate-from-regexp (from)
  (when (string-match "\\(?:\\`\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
    (let ((match (match-string 1 from)))
      (cond
       ((string= match "\\n")
        (message "`\\n' here doesn't match a newline; type C-q C-j instead!!"))
       ((string= match "\\t")
        (message "\\t' here doesn't match a tab; to do that, just type TAB!!")))
      (sit-for 2))))

(defun anzu--query-from-string (prompt beg end use-regexp overlay-limit)
  (let* ((from (anzu--read-from-string prompt beg end use-regexp overlay-limit))
         (is-empty (string= from "")))
    (when (and (not is-empty) (not anzu--query-defaults))
      (setq anzu--last-replaced-count anzu--total-matched))
    (if (and is-empty anzu--query-defaults)
        (cons (query-replace-descr (caar anzu--query-defaults))
              (query-replace-compile-replacement
               (query-replace-descr (cdar anzu--query-defaults)) use-regexp))
      (add-to-history query-replace-from-history-variable from nil t)
      (when use-regexp
        (unless (anzu--validate-regexp from)
          (error "'%s' is invalid regexp." from))
        (anzu--query-validate-from-regexp from))
      from)))

(defun anzu--compile-replace-text (str)
  (let ((compiled (ignore-errors
                    (query-replace-compile-replacement str t))))
    (when compiled
      (cond  ((stringp compiled) compiled)
             ((and (consp compiled) (functionp (car compiled)))
              compiled)
             ((and (consp compiled) (stringp (car compiled)))
              (car compiled))))))

(defun anzu--evaluate-occurrence (ov to-regexp replacements fixed-case from-regexp)
  (let ((from-string (overlay-get ov 'from-string))
        (compiled (anzu--compile-replace-text to-regexp)))
    (if (not compiled)
        ""
      (with-temp-buffer
        (insert from-string)
        (goto-char (point-min))
        (when (re-search-forward from-regexp nil t)
          (or (ignore-errors
                (if (consp compiled)
                    (replace-match (funcall (car compiled) (cdr compiled)
                                            replacements) fixed-case)
                  (replace-match compiled fixed-case))
                (buffer-substring (point-min) (point-max)))
              ""))))))

(defun anzu--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst anzu--overlays-in-range (beg end)
  (cl-loop for ov in (overlays-in beg end)
           when (overlay-get ov 'anzu-replace)
           collect ov into anzu-overlays
           finally
           return
           (let ((sorted (sort anzu-overlays 'anzu--overlay-sort)))
             (if anzu-replace-threshold
                 (cl-subseq sorted 0 (min (length sorted) anzu-replace-threshold))
               sorted))))

(defsubst anzu--propertize-to-string (str)
  (let ((separator (or anzu-replace-to-string-separator "")))
    (propertize (concat separator str) 'face 'anzu-replace-to)))

(defsubst anzu--replaced-literal-string (ov replaced from)
  (let ((str (buffer-substring-no-properties
              (overlay-start ov) (overlay-end ov))))
    (when (string-match (regexp-quote str) from)
      (replace-match replaced (not case-fold-search) t str))))

(defun anzu--append-replaced-string (content buf beg end use-regexp overlay-limit from)
  (let ((replacements 0))
    (unless (string= content anzu--last-replace-input)
      (setq anzu--last-replace-input content)
      (with-current-buffer buf
        (let ((case-fold-search (anzu--case-fold-search from)))
          (dolist (ov (anzu--overlays-in-range beg (min end overlay-limit)))
            (let ((replace-evaled
                   (if (not use-regexp)
                       (anzu--replaced-literal-string ov content from)
                     (prog1 (anzu--evaluate-occurrence ov content replacements
                                                       (not case-fold-search) from)
                       (cl-incf replacements)))))
              (overlay-put ov 'after-string (anzu--propertize-to-string replace-evaled)))))))))

(defsubst anzu--outside-overlay-limit (orig-beg orig-limit)
  (save-excursion
    (goto-char (+ anzu--outside-point (- orig-limit orig-beg)))
    (line-end-position)))

(defun anzu--read-to-string (from prompt beg end use-regexp overlay-limit)
  (let ((curbuf (current-buffer))
        (orig-beg beg)
        (to-prompt (format "%s %s with: " prompt (query-replace-descr from)))
        (history-add-new-input nil)
        (blink-matching-paren nil)
        timer is-input)
    (setq anzu--last-replace-input "")
    (when anzu--outside-point
      (setq beg anzu--outside-point
            overlay-limit (anzu--outside-overlay-limit orig-beg overlay-limit)
            anzu--outside-point nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            #'(lambda ()
                (setq timer (run-with-idle-timer
                             (max anzu-input-idle-delay 0.01)
                             'repeat
                             (lambda ()
                               (with-selected-window (or (active-minibuffer-window)
                                                         (minibuffer-window))
                                 (anzu--append-replaced-string
                                  (minibuffer-contents)
                                  curbuf beg end use-regexp overlay-limit from))))))
          (prog1 (read-from-minibuffer to-prompt
                                       nil nil nil
                                       query-replace-from-history-variable nil t)
            (setq is-input t)))
      (when timer
        (cancel-timer timer)
        (setq timer nil)
        (unless is-input
          (goto-char orig-beg))))))

(defun anzu--query-replace-read-to (from prompt beg end use-regexp overlay-limit)
  (query-replace-compile-replacement
   (let ((to (anzu--read-to-string from prompt beg end use-regexp overlay-limit)))
     (add-to-history query-replace-to-history-variable to nil t)
     (add-to-history 'anzu--query-defaults (cons from to) nil t)
     to)
   use-regexp))

(defun anzu--overlay-limit ()
  (save-excursion
    (move-to-window-line -1)
    (forward-line 1)
    (point)))

(defun anzu--query-from-at-cursor (buf beg end overlay-limit)
  (let ((symbol (thing-at-point 'symbol)))
    (unless symbol
      (error "No symbol at cursor!!"))
    (let ((symbol-regexp (concat "\\_<" (regexp-quote symbol) "\\_>")))
      (anzu--count-and-highlight-matched buf symbol-regexp beg end t overlay-limit t)
      (setq anzu--total-matched anzu--cached-count)
      (force-mode-line-update)
      symbol-regexp)))

(defun anzu--query-from-isearch-string (buf beg end use-regexp overlay-limit)
  (anzu--count-and-highlight-matched buf isearch-string beg end use-regexp overlay-limit t)
  (setq anzu--total-matched anzu--cached-count)
  (force-mode-line-update)
  (add-to-history query-replace-from-history-variable isearch-string nil t)
  isearch-string)

(defun anzu--thing-begin (thing)
  (let ((bound (bounds-of-thing-at-point thing)))
    (if bound
        (car bound)
      (let ((fallback-bound (bounds-of-thing-at-point 'symbol)))
        (if fallback-bound
            (car fallback-bound)
          (point))))))

(defsubst anzu--thing-end (thing)
  (let ((bound (bounds-of-thing-at-point thing)))
    (if bound
        (cdr bound)
      (point-max))))

(defun anzu--region-begin (use-region thing backward)
  (cond (use-region (region-beginning))
        (current-prefix-arg (line-beginning-position))
        (thing (anzu--thing-begin thing))
        (backward (point-min))
        (t (point))))

(defsubst anzu--line-end-position (num)
  (save-excursion
    (forward-line (1- num))
    (line-end-position)))

(defun anzu--region-end (use-region thing)
  (cond (use-region (region-end))
        (current-prefix-arg
         (anzu--line-end-position (prefix-numeric-value current-prefix-arg)))
        (thing (anzu--thing-end thing))
        (t (point-max))))

(defun anzu--begin-thing (at-cursor thing)
  (cond ((and at-cursor thing) thing)
        ((and at-cursor (not thing)) 'symbol)
        (t nil)))

(defun anzu--replace-backward-p (prefix)
  ;; This variable is introduced at Emacs 24.4, I should fix this variable to
  ;; version variable
  (and (boundp 'list-matching-lines-prefix-face)
       (and prefix (< prefix 0))))

(defun anzu--construct-perform-replace-arguments (from to delimited beg end backward query)
  (if backward
      (list from to query t delimited nil nil beg end backward)
    (list from to query t delimited nil nil beg end)))

(defun anzu--construct-query-replace-arguments (from to delimited beg end backward)
  (if backward
      (list from to delimited beg end backward)
    (list from to delimited beg end)))

(defsubst anzu--current-replaced-index (curpoint)
  (cl-loop for m in anzu--replaced-markers
           for i = 1 then (1+ i)
           for pos = (marker-position m)
           when (= pos curpoint)
           return i))

(defadvice replace-highlight (before anzu-replace-highlight activate)
  (when (and (eq anzu--state 'replace) anzu--replaced-markers)
    (let ((index (anzu--current-replaced-index (ad-get-arg 0))))
      (when (or (not index) (/= index anzu--current-position))
        (force-mode-line-update)
        (setq anzu--current-position (or index 1))))))

(defun anzu--set-replaced-markers (from beg end use-regexp)
  (save-excursion
    (goto-char beg)
    (cl-loop with curbuf = (current-buffer)
             with search-func = (if use-regexp #'re-search-forward #'search-forward)
             while (funcall search-func from end t)
             do
             (progn
               (anzu--set-marker (match-beginning 0) curbuf)
               (when (= (match-beginning 0) (match-end 0))
                 (if (eobp)
                     (cl-return nil)
                   (forward-char 1)))
               (when (and end (> (point) end))
                 (cl-return nil))))))

(cl-defun anzu--query-replace-common (use-regexp
                                      &key at-cursor thing prefix-arg (query t) isearch-p)
  (anzu--cons-mode-line 'replace-query)
  (let* ((use-region (use-region-p))
         (orig-point (point))
         (backward (anzu--replace-backward-p prefix-arg))
         (overlay-limit (anzu--overlay-limit))
         (beg (anzu--region-begin use-region (anzu--begin-thing at-cursor thing) backward))
         (end (anzu--region-end use-region thing))
         (prompt (anzu--query-prompt use-region use-regexp at-cursor isearch-p))
         (delimited (and current-prefix-arg (not (eq current-prefix-arg '-))))
         (curbuf (current-buffer))
         (clear-overlay nil))
    (when (and anzu-deactivate-region use-region)
      (deactivate-mark t))
    (unwind-protect
        (let* ((from (cond ((and at-cursor beg)
                            (setq delimited nil)
                            (anzu--query-from-at-cursor curbuf beg end overlay-limit))
                           (isearch-p
                            (anzu--query-from-isearch-string
                             curbuf beg end use-regexp overlay-limit))
                           (t (anzu--query-from-string
                               prompt beg end use-regexp overlay-limit))))
               (to (cond ((consp from)
                          (prog1 (cdr from)
                            (setq from (car from)
                                  anzu--total-matched anzu--last-replaced-count)))
                         ((string-match "\0" from)
                          (prog1 (substring-no-properties from (match-end 0))
                            (setq from (substring-no-properties from 0 (match-beginning 0)))))
                         (t
                          (anzu--query-replace-read-to
                           from prompt beg end use-regexp overlay-limit)))))
          (anzu--clear-overlays curbuf beg end)
          (anzu--set-replaced-markers from beg end use-regexp)
          (setq anzu--state 'replace anzu--current-position 0
                anzu--replaced-markers (reverse anzu--replaced-markers)
                clear-overlay t)
          (let ((case-fold-search (and case-fold-search (not at-cursor))))
            (if use-regexp
                (apply #'perform-replace (anzu--construct-perform-replace-arguments
                                          from to delimited beg end backward query))
              (apply #'query-replace (anzu--construct-query-replace-arguments
                                      from to delimited beg end backward)))))
      (progn
        (unless clear-overlay
          (anzu--clear-overlays curbuf beg end))
        (when (zerop anzu--current-position)
          (goto-char orig-point))
        (anzu--cleanup-markers)
        (anzu--reset-mode-line)
        (force-mode-line-update)))))

;;;###autoload
(defun anzu-query-replace-at-cursor ()
  "Replace symbol at cursor with to-string."
  (interactive)
  (anzu--query-replace-common t :at-cursor t))

;;;###autoload
(defun anzu-query-replace-at-cursor-thing ()
  "Replace symbol at cursor within `anzu-replace-at-cursor-thing' area."
  (interactive)
  (anzu--query-replace-common t :at-cursor t :thing anzu-replace-at-cursor-thing))

;;;###autoload
(defun anzu-query-replace (arg)
  "anzu version of `query-replace'."
  (interactive "p")
  (anzu--query-replace-common nil :prefix-arg arg))

;;;###autoload
(defun anzu-query-replace-regexp (arg)
  "anzu version of `query-replace-regexp'."
  (interactive "p")
  (anzu--query-replace-common t :prefix-arg arg))

;;;###autoload
(defun anzu-replace-at-cursor-thing ()
  "anzu-query-replace-at-cursor-thing without query."
  (interactive)
  (let ((orig (point-marker)))
    (anzu--query-replace-common t
                                :at-cursor t
                                :thing anzu-replace-at-cursor-thing
                                :query nil)
    (goto-char (marker-position orig))
    (set-marker orig nil)))

(defun anzu--isearch-query-replace-common (use-regexp arg)
  (isearch-done nil t)
  (isearch-clean-overlays)
  (let ((isearch-recursive-edit nil)
        (backward (< (prefix-numeric-value arg) 0)))
    (when (and isearch-other-end
               (if backward
                   (> isearch-other-end (point))
                 (< isearch-other-end (point)))
               (not (and transient-mark-mode mark-active
                         (if backward
                             (> (mark) (point))
                           (< (mark) (point))))))
      (goto-char isearch-other-end))
    (anzu--query-replace-common use-regexp :prefix-arg arg :isearch-p t)))

;;;###autoload
(defun anzu-isearch-query-replace (arg)
  "anzu version of `isearch-query-replace'."
  (interactive "p")
  (anzu--isearch-query-replace-common nil arg))

;;;###autoload
(defun anzu-isearch-query-replace-regexp (arg)
  "anzu version of `isearch-query-replace-regexp'."
  (interactive "p")
  (anzu--isearch-query-replace-common t arg))

(provide 'anzu)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; anzu.el ends here

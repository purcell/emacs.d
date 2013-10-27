;;; evil-commands.el --- Evil commands and operators
;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.0-dev

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

(require 'evil-common)
(require 'evil-digraphs)
(require 'evil-search)
(require 'evil-ex)
(require 'evil-types)

;;; Compatibility for Emacs 23
(unless (fboundp 'window-body-width)
  (defalias 'window-body-width 'window-width))

;;; Motions

;; Movement commands, or motions, are defined with the macro
;; `evil-define-motion'. A motion is a command with an optional
;; argument COUNT (interactively accessed by the code "<c>").
;; It may specify the :type command property (e.g., :type line),
;; which determines how it is handled by an operator command.
;; Furthermore, the command must have the command properties
;; :keep-visual t and :repeat motion; these are automatically
;; set by the `evil-define-motion' macro.

;;; Code:

(evil-define-motion evil-forward-char (count &optional crosslines noerror)
  "Move cursor to the right by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the end
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list evil-cross-lines
                           (evil-kbd-macro-suppress-motion-error)))
  (cond
   (noerror
    (condition-case nil
        (evil-forward-char count crosslines nil)
      (error nil)))
   ((not crosslines)
    ;; for efficiency, narrow the buffer to the projected
    ;; movement before determining the current line
    (evil-with-restriction
        (point)
        (save-excursion
          (evil-forward-char (1+ (or count 1)) t t)
          (point))
      (evil-narrow-to-line
        (evil-forward-char count t noerror))))
   (t
    (evil-motion-loop (nil (or count 1))
      (forward-char)
      ;; don't put the cursor on a newline
      (when (and evil-move-cursor-back
                 (not (evil-visual-state-p))
                 (not (evil-operator-state-p))
                 (eolp) (not (eobp)) (not (bolp)))
        (forward-char))))))

(evil-define-motion evil-backward-char (count &optional crosslines noerror)
  "Move cursor to the left by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the beginning
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list evil-cross-lines
                           (evil-kbd-macro-suppress-motion-error)))
  (cond
   (noerror
    (condition-case nil
        (evil-backward-char count crosslines nil)
      (error nil)))
   ((not crosslines)
    ;; restrict movement to the current line
    (evil-with-restriction
        (save-excursion
          (evil-backward-char (1+ (or count 1)) t t)
          (point))
        (1+ (point))
      (evil-narrow-to-line
        (evil-backward-char count t noerror))))
   (t
    (evil-motion-loop (nil (or count 1))
      (backward-char)
      ;; don't put the cursor on a newline
      (unless (or (evil-visual-state-p) (evil-operator-state-p))
        (evil-adjust-cursor))))))

(evil-define-motion evil-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move (or count 1))))

(evil-define-motion evil-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (let (line-move-visual)
    (evil-line-move (- (or count 1)))))

(evil-define-motion evil-next-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 1))))

(evil-define-motion evil-previous-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (- (or count 1)))))

;; used for repeated commands like "dd"
(evil-define-motion evil-line (count)
  "Move COUNT - 1 lines down."
  :type line
  (let (line-move-visual)
    ;; Catch bob and eob errors. These are caused when not moving
    ;; point starting in the first or last line, respectively. In this
    ;; case the current line should be selected.
    (condition-case err
        (evil-line-move (1- (or count 1)))
      ((beginning-of-buffer end-of-buffer)))))

(evil-define-motion evil-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (move-beginning-of-line nil))

(evil-define-motion evil-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (move-end-of-line count)
  (when evil-track-eol
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line))
  (unless (evil-visual-state-p)
    (evil-adjust-cursor)
    (when (eolp)
      ;; prevent "c$" and "d$" from deleting blank lines
      (setq evil-this-type 'exclusive))))

(evil-define-motion evil-beginning-of-visual-line ()
  "Move the cursor to the first character of the current screen line."
  :type exclusive
  (if (fboundp 'beginning-of-visual-line)
      (beginning-of-visual-line)
    (beginning-of-line)))

(evil-define-motion evil-end-of-visual-line (count)
  "Move the cursor to the last character of the current screen line.
If COUNT is given, move COUNT - 1 screen lines downward first."
  :type inclusive
  (if (fboundp 'end-of-visual-line)
      (end-of-visual-line count)
    (end-of-line count)))

(evil-define-motion evil-middle-of-visual-line ()
  "Move the cursor to the middle of the current visual line."
  :type exclusive
  (beginning-of-visual-line)
  (evil-with-restriction
      nil
      (save-excursion (end-of-visual-line) (point))
    (move-to-column (+ (current-column)
                       -1
                       (/ (with-no-warnings (window-body-width)) 2)))))

(evil-define-motion evil-beginning-of-line-or-digit-argument ()
  "Move the cursor to the beginning of the current line.
This function passes its command to `digit-argument' (usually a 0)
if it is not the first event."
  :type exclusive
  (cond
   (current-prefix-arg
    (setq this-command #'digit-argument)
    (call-interactively #'digit-argument))
   (t
    (setq this-command #'evil-beginning-of-line)
    (call-interactively #'evil-beginning-of-line))))

(evil-define-motion evil-first-non-blank ()
  "Move the cursor to the first non-blank character of the current line."
  :type exclusive
  (evil-narrow-to-line (back-to-indentation)))

(evil-define-motion evil-last-non-blank (count)
  "Move the cursor to the last non-blank character of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (goto-char
   (save-excursion
     (evil-move-beginning-of-line count)
     (if (re-search-forward "[ \t]*$")
         (max (line-beginning-position)
              (1- (match-beginning 0)))
       (line-beginning-position)))))

(evil-define-motion evil-first-non-blank-of-visual-line ()
  "Move the cursor to the first non blank character
of the current screen line."
  :type exclusive
  (evil-beginning-of-visual-line)
  (skip-chars-forward " \t\r"))

(evil-define-motion evil-next-line-first-non-blank (count)
  "Move the cursor COUNT lines down on the first non-blank character."
  :type line
  (evil-next-line (or count 1))
  (evil-first-non-blank))

(evil-define-motion evil-next-line-1-first-non-blank (count)
  "Move the cursor COUNT-1 lines down on the first non-blank character."
  :type line
  (evil-next-line (1- (or count 1)))
  (evil-first-non-blank))

(evil-define-motion evil-previous-line-first-non-blank (count)
  "Move the cursor COUNT lines up on the first non-blank character."
  :type line
  (evil-previous-line (or count 1))
  (evil-first-non-blank))

(evil-define-motion evil-goto-line (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (if (null count)
      (goto-char (point-max))
    (goto-char (point-min))
    (forward-line (1- count)))
  (evil-first-non-blank))

(evil-define-motion evil-goto-first-line (count)
  "Go to the first non-blank character of line COUNT.
By default the first line."
  :jump t
  :type line
  (evil-goto-line (or count 1)))

(evil-define-motion evil-move-empty-lines (count)
  "Move to the next or previous empty line, repeated COUNT times."
  :type exclusive
  (evil-motion-loop (var (or count 1))
    (cond
     ((< var 0)
      (goto-char
       (or (save-excursion
             (unless (bobp)
               (backward-char)
               (re-search-backward "^$" nil t)))
           (point))))
     (t
      (let ((orig (point)))
        (when (re-search-forward "^$" nil t)
          (if (eobp)
              (goto-char orig)
            (forward-char))))))))

(evil-define-union-move evil-move-word (count)
  "Move by words."
  (evil-move-chars "^ \t\r\n[:word:]" count)
  (let ((word-separating-categories evil-cjk-word-separating-categories)
        (word-combining-categories evil-cjk-word-combining-categories))
    (evil-forward-word count))
  (evil-move-empty-lines count))

(evil-define-union-move evil-move-WORD (count)
  "Move by WORDs."
  (evil-move-chars evil-bigword count)
  (evil-move-empty-lines count))

(evil-define-motion evil-forward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word))
        (orig (point)))
    (prog1 (if (and evil-want-change-word-to-end
                    (not (looking-at "[[:space:]]"))
                    (eq evil-this-operator #'evil-change))
               (evil-move-end count move)
             (evil-move-beginning count move))
      ;; if we reached the beginning of a word on a new line in
      ;; Operator-Pending state, go back to the end of the previous
      ;; line
      (when (and (evil-operator-state-p)
                 (> (line-beginning-position) orig)
                 (looking-back "^[[:space:]]*" (line-beginning-position)))
        ;; move cursor back as long as the line contains only
        ;; whitespaces and is non-empty
        (evil-move-end-of-line 0)
        ;; skip non-empty lines containing only spaces
        (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                    (not (<= (line-beginning-position) orig)))
          (evil-move-end-of-line 0))
        ;; but if the previous line is empty, delete this line
        (when (bolp) (forward-char))))))

(evil-define-motion evil-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word)))
    ;; if changing a one-letter word, don't move point to the
    ;; next word (which would change two words)
    (if (and (evil-operator-state-p)
             (looking-at "[[:word:]]"))
        (prog1 (evil-move-end count move)
          (unless (bobp) (backward-char)))
      (evil-move-end count move nil t))))

(evil-define-motion evil-backward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word)))
    (evil-move-beginning (- (or count 1)) move)))

(evil-define-motion evil-backward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word)))
    (evil-move-end (- (or count 1)) move nil t)))

(evil-define-motion evil-forward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th next WORD."
  :type exclusive
  (evil-forward-word-begin count t))

(evil-define-motion evil-forward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (evil-forward-word-end count t))

(evil-define-motion evil-backward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous WORD."
  :type exclusive
  (evil-backward-word-begin count t))

(evil-define-motion evil-backward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (evil-backward-word-end count t))

;; section movement
(evil-define-motion evil-forward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th next section."
  :jump t
  :type exclusive
  (beginning-of-defun (- (or count 1))))

(evil-define-motion evil-forward-section-end (count)
  "Move the cursor to the end of the COUNT-th next section."
  :jump t
  :type inclusive
  (end-of-defun (or count 1)))

(evil-define-motion evil-backward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous section."
  :jump t
  :type exclusive
  (beginning-of-defun (or count 1)))

(evil-define-motion evil-backward-section-end (count)
  "Move the cursor to the end of the COUNT-th previous section."
  :jump t
  :type inclusive
  (end-of-defun (- (or count 1))))

(evil-define-motion evil-forward-sentence (count)
  "Move to the next COUNT-th beginning of a sentence or end of a paragraph."
  :jump t
  :type exclusive
  (let ((count (or count 1))
        beg-sentence end-paragraph)
    (when (evil-eobp)
      (signal 'end-of-buffer nil))
    (evil-motion-loop (nil count)
      (unless (eobp)
        (setq beg-sentence
              (save-excursion
                (and (zerop (evil-move-beginning 1 #'evil-move-sentence))
                     (point)))
              end-paragraph
              (save-excursion
                (forward-paragraph)
                (point)))
        (evil-goto-min beg-sentence end-paragraph)))))

(evil-define-motion evil-backward-sentence (count)
  "Move to the previous COUNT-th beginning of a sentence or paragraph."
  :jump t
  :type exclusive
  (let ((count (or count 1))
        beg-sentence beg-paragraph)
    (when (bobp)
      (signal 'beginning-of-buffer nil))
    (evil-motion-loop (nil count)
      (unless (bobp)
        (setq beg-sentence
              (save-excursion
                (and (zerop (evil-move-beginning -1 #'evil-move-sentence))
                     (point)))
              beg-paragraph
              (save-excursion
                (backward-paragraph)
                (point)))
        (evil-goto-max beg-sentence beg-paragraph)))))

(evil-define-motion evil-forward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (evil-move-end count #'forward-paragraph #'backward-paragraph))

(evil-define-motion evil-backward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
  :type exclusive
  (evil-move-beginning (- (or count 1))
                       #'forward-paragraph #'backward-paragraph))

(evil-define-motion evil-jump-item (count)
  "Find the next item in this line after or under the cursor
and jump to the corresponding one."
  :jump t
  :type inclusive
  (cond
   ;; COUNT% jumps to a line COUNT percentage down the file
   (count
    (goto-char
     (evil-normalize-position
      (let ((size (- (point-max) (point-min))))
        (+ (point-min)
           (if (> size 80000)
               (* count (/ size 100))
             (/ (* count size) 100))))))
    (back-to-indentation)
    (setq evil-this-type 'line))
   ((and (evil-looking-at-start-comment t)
         (let ((pnt (point)))
           (forward-comment 1)
           (or (not (bolp))
               (prog1 nil (goto-char pnt)))))
    (backward-char))
   ((and (not (eolp)) (evil-looking-at-end-comment t))
    (forward-comment -1))
   ((and
     (memq major-mode '(c-mode c++-mode))
     (require 'hideif nil t)
     (with-no-warnings
       (let* ((hif-else-regexp (concat hif-cpp-prefix "\\(?:else\\|elif[ \t]+\\)"))
              (hif-ifx-else-endif-regexp
               (concat hif-ifx-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp)))
         (cond
          ((save-excursion (beginning-of-line) (or (hif-looking-at-ifX) (hif-looking-at-else)))
           (hif-find-next-relevant)
           (while (hif-looking-at-ifX)
             (hif-ifdef-to-endif)
             (hif-find-next-relevant))
           t)
          ((save-excursion (beginning-of-line) (hif-looking-at-endif))
           (hif-endif-to-ifdef)
           t))))))
   (t
    (let* ((open (point-max))
           (close (point-max))
           (open-pair (condition-case nil
                          (save-excursion
                            (setq open (1- (scan-lists (point) 1 -1)))
                            (when (< open (line-end-position))
                              (goto-char open)
                              (forward-list)
                              (1- (point))))
                        (error nil)))
           (close-pair (condition-case nil
                           (save-excursion
                             (setq close (1- (scan-lists (point) 1 1)))
                             (when (< close (line-end-position))
                               (goto-char (1+ close))
                               (backward-list)
                               (point)))
                         (error nil))))
      (cond
       ((not (or open-pair close-pair))
        ;; nothing found, check if we are inside a string
        (let ((pnt (point))
              (state (syntax-ppss (point))))
          (if (not (evil-in-string-p))
              ;; no, then we really failed
              (error "No matching item found on the current line")
            ;; yes, go to the end of the string and try again
            (let ((endstr (evil-string-end (point) (line-end-position))))
              (when (or (evil-in-string-p endstr) ; not at end of string
                        (condition-case nil
                            (progn
                              (goto-char endstr)
                              (evil-jump-item)
                              nil)
                          (error t)))
                ;; failed again, go back to original point
                (goto-char pnt)
                (error "No matching item found on the current line"))))))
       ((< open close) (goto-char open-pair))
       (t (goto-char close-pair)))))))

(evil-define-motion evil-previous-open-paren (count)
  "Go to [count] previous unmatched '('."
  :type exclusive
  (let ((range (save-excursion
                 (backward-char)
                 (evil-paren-range count nil nil nil ?\( ?\)))))
    (when range
      (goto-char (evil-range-beginning range)))))

(evil-define-motion evil-next-close-paren (count)
  "Go to [count] next unmatched ')'."
  :type exclusive
  (let ((range (save-excursion
                 (forward-char)
                 (evil-paren-range count nil nil nil ?\( ?\)))))
    (when range
      (goto-char (1- (evil-range-end range))))))

(evil-define-motion evil-previous-open-brace (count)
  "Go to [count] previous unmatched '{'."
  :type exclusive
  (let ((range (save-excursion
                 (backward-char)
                 (evil-paren-range count nil nil nil ?\{ ?\}))))
    (when range
      (goto-char (evil-range-beginning range)))))

(evil-define-motion evil-next-close-brace (count)
  "Go to [count] next unmatched '}'."
  :type exclusive
  (let ((range (save-excursion
                 (forward-char)
                 (evil-paren-range count nil nil nil ?\{ ?\}))))
    (when range
      (goto-char (1- (evil-range-end range))))))

(evil-define-motion evil-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :jump t
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq evil-last-find (list #'evil-find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless (prog1
                  (search-forward (char-to-string char)
                                  (unless evil-cross-lines
                                    (if fwd
                                        (line-end-position)
                                      (line-beginning-position)))
                                  t count)
                (when fwd (backward-char)))
        (error "Can't find %c" char)))))

(evil-define-motion evil-find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :jump t
  :type exclusive
  (interactive "<c><C>")
  (evil-find-char (- (or count 1)) char))

(evil-define-motion evil-find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :jump t
  :type inclusive
  (interactive "<c><C>")
  (unwind-protect
      (progn
        (evil-find-char count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar evil-last-find #'evil-find-char-to)))

(evil-define-motion evil-find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :jump t
  :type exclusive
  (interactive "<c><C>")
  (evil-find-char-to (- (or count 1)) char))

(evil-define-motion evil-repeat-find-char (count)
  "Repeat the last find COUNT times."
  :jump t
  :type inclusive
  (setq count (or count 1))
  (if evil-last-find
      (let ((cmd (car evil-last-find))
            (char (nth 1 evil-last-find))
            (fwd (nth 2 evil-last-find))
            evil-last-find)
        ;; ensure count is non-negative
        (when (< count 0)
          (setq count (- count)
                fwd (not fwd)))
        ;; skip next character when repeating t or T
        (and (eq cmd #'evil-find-char-to)
             evil-repeat-find-to-skip-next
             (= count 1)
             (or (and fwd (= (char-after (1+ (point))) char))
                 (and (not fwd) (= (char-before) char)))
             (setq count (1+ count)))
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 evil-last-find)
          (setq evil-this-type 'exclusive)))
    (error "No previous search")))

(evil-define-motion evil-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :jump t
  :type inclusive
  (evil-repeat-find-char (- (or count 1))))

;; ceci n'est pas une pipe
(evil-define-motion evil-goto-column (count)
  "Go to column COUNT on the current line.
Columns are counted from zero."
  :type exclusive
  (move-to-column (or count 0)))

(evil-define-command evil-goto-mark (char &optional noerror)
  "Go to the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type exclusive
  (interactive (list (read-char)))
  (let ((marker (evil-get-marker char)))
    (cond
     ((markerp marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker)))
     ((numberp marker)
      (goto-char marker))
     ((consp marker)
      (when (or (find-buffer-visiting (car marker))
                (and (y-or-n-p (format "Visit file %s again? "
                                       (car marker)))
                     (find-file (car marker))))
        (goto-char (cdr marker))))
     ((not noerror)
      (error "Marker `%c' is not set%s" char
             (if (evil-global-marker-p char) ""
               " in this buffer"))))))

(evil-define-command evil-goto-mark-line (char &optional noerror)
  "Go to the line of the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type line
  (interactive (list (read-char)))
  (evil-goto-mark char noerror)
  (evil-first-non-blank))

(evil-define-motion evil-jump-backward (count)
  "Go to older position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-forward]."
  (let ((current-pos (make-marker))
        (count (or count 1)) i)
    (unless evil-jump-list
      (move-marker current-pos (point))
      (add-to-list 'evil-jump-list current-pos))
    (evil-motion-loop (nil count)
      (setq current-pos (make-marker))
      ;; skip past duplicate entries in the mark ring
      (setq i (length mark-ring))
      (while (progn (move-marker current-pos (point))
                    (set-mark-command 0)
                    (setq i (1- i))
                    (and (= (point) current-pos) (> i 0))))
      ;; Already there?
      (move-marker current-pos (point))
      (unless (= current-pos (car-safe evil-jump-list))
        (add-to-list 'evil-jump-list current-pos)))))

(evil-define-motion evil-jump-forward (count)
  "Go to newer position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-backward]."
  (let ((count (or count 1))
        current-pos next-pos)
    (evil-motion-loop (nil count)
      (setq current-pos (car-safe evil-jump-list)
            next-pos (car (cdr-safe evil-jump-list)))
      (when next-pos
        (push-mark current-pos t nil)
        (unless (eq (marker-buffer next-pos) (current-buffer))
          (switch-to-buffer (marker-buffer next-pos)))
        (goto-char next-pos)
        (pop evil-jump-list)))))

(evil-define-motion evil-jump-to-tag (arg)
  "Jump to tag under point.
If called with a prefix argument, provide a prompt
for specifying the tag."
  :jump t
  (interactive "P")
  (if arg (call-interactively #'find-tag)
    (let ((tag (thing-at-point 'symbol)))
      (find-tag tag))))

(evil-define-motion evil-lookup ()
  "Look up the keyword at point.
Calls `evil-lookup-func'."
  (funcall evil-lookup-func))

(defun evil-ret-gen (count indent?)
  (let* ((field  (get-char-property (point) 'field))
         (button (get-char-property (point) 'button))
         (doc    (get-char-property (point) 'widget-doc))
         (widget (or field button doc)))
    (cond
     ((and widget
           (fboundp 'widget-type)
           (fboundp 'widget-button-press)
           (or (and (symbolp widget)
                    (get widget 'widget-type))
               (and (consp widget)
                    (get (widget-type widget) 'widget-type))))
      (when (evil-operator-state-p)
        (setq evil-inhibit-operator t))
      (when (fboundp 'widget-button-press)
        (widget-button-press (point))))
     ((and (fboundp 'button-at)
           (fboundp 'push-button)
           (button-at (point)))
      (when (evil-operator-state-p)
        (setq evil-inhibit-operator t))
      (push-button))
     ((or (evil-emacs-state-p)
          (and (evil-insert-state-p)
               (not buffer-read-only)))
      (if (not indent?)
          (newline count)
        (delete-horizontal-space t)
        (newline count)
        (indent-according-to-mode)))
     (t
      (evil-next-line-first-non-blank count)))))

(evil-define-motion evil-ret (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline."
  :type line
  (evil-ret-gen count nil))

(evil-define-motion evil-ret-and-indent (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline and indent."
  :type line
  (evil-ret-gen count t))

(evil-define-motion evil-window-top (count)
  "Move the cursor to line COUNT from the top of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (max (or count 0)
                            (if (= (point-min) (window-start))
                                0
                              scroll-margin)))
  (back-to-indentation))

(evil-define-motion evil-window-middle ()
  "Move the cursor to the middle line in the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line
   (/ (1+ (save-excursion (move-to-window-line -1))) 2))
  (back-to-indentation))

(evil-define-motion evil-window-bottom (count)
  "Move the cursor to line COUNT from the bottom of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (- (max (or count 1) (1+ scroll-margin))))
  (back-to-indentation))

;; scrolling
(evil-define-command evil-scroll-line-up (count)
  "Scrolls the window COUNT lines upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-down count))

(evil-define-command evil-scroll-line-down (count)
  "Scrolls the window COUNT lines downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-up count))

(evil-define-command evil-scroll-up (count)
  "Scrolls the window and the cursor COUNT lines upwards.
The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((p (point))
          (c (or count (/ (evil-num-visible-lines) 2))))
      (save-excursion
        (scroll-down (min (evil-max-scroll-up) c)))
      (forward-line (- c))
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'beginning-of-buffer nil)))))

(evil-define-command evil-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards.
The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((p (point))
          (c (or count (/ (evil-num-visible-lines) 2))))
      (save-excursion
        (scroll-up (min (evil-max-scroll-down) c)))
      (forward-line c)
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'end-of-buffer nil)))))

(evil-define-command evil-scroll-page-up (count)
  "Scrolls the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (scroll-down nil))))

(evil-define-command evil-scroll-page-down (count)
  "Scrolls the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (scroll-up nil))))

(evil-define-command evil-scroll-line-to-top (count)
  "Scrolls line number COUNT (or the cursor line) to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter 0)))

(evil-define-command evil-scroll-line-to-center (count)
  "Scrolls line number COUNT (or the cursor line) to the center of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (when count
      (goto-char (point-min))
      (forward-line (1- count)))
    (recenter nil)))

(evil-define-command evil-scroll-line-to-bottom (count)
  "Scrolls line number COUNT (or the cursor line) to the bottom of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter -1)))

(evil-define-command evil-scroll-bottom-line-to-top (count)
  "Scrolls the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-end))
    (evil-move-cursor-back))
  (recenter 0)
  (evil-first-non-blank))

(evil-define-command evil-scroll-top-line-to-bottom (count)
  "Scrolls the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-start)))
  (recenter -1)
  (evil-first-non-blank))

;;; Text objects

;; Text objects are defined with `evil-define-text-object'. In Visual
;; state, they modify the current selection; in Operator-Pending
;; state, they return a pair of buffer positions. Outer text objects
;; are bound in the keymap `evil-outer-text-objects-map', and inner
;; text objects are bound in `evil-inner-text-objects-map'.
;;
;; Common text objects like words, WORDS, paragraphs and sentences are
;; defined via a corresponding move-function. This function must have
;; the following properties:
;;
;;   1. Take exactly one argument, the count.
;;   2. When the count is positive, move point forward to the first
;;      character after the end of the next count-th object.
;;   3. When the count is negative, move point backward to the first
;;      character of the count-th previous object.
;;   4. If point is placed on the first character of an object, the
;;      backward motion does NOT count that object.
;;   5. If point is placed on the last character of an object, the
;;      forward motion DOES count that object.
;;   6. The return value is "count left", i.e., in forward direction
;;      count is decreased by one for each successful move and in
;;      backward direction count is increased by one for each
;;      successful move, returning the final value of count.
;;      Therefore, if the complete move is successful, the return
;;      value is 0.
;;
;; A useful macro in this regard is `evil-motion-loop', which quits
;; when point does not move further and returns the count difference.
;; It also provides a "unit value" of 1 or -1 for use in each
;; iteration. For example, a hypothetical "foo-bar" move could be
;; written as such:
;;
;;     (defun foo-bar (count)
;;       (evil-motion-loop (var count)
;;         (forward-foo var) ; `var' is 1 or -1 depending on COUNT
;;         (forward-bar var)))
;;
;; If "forward-foo" and "-bar" didn't accept negative arguments,
;; we could choose their backward equivalents by inspecting `var':
;;
;;     (defun foo-bar (count)
;;       (evil-motion-loop (var count)
;;         (cond
;;          ((< var 0)
;;           (backward-foo 1)
;;           (backward-bar 1))
;;          (t
;;           (forward-foo 1)
;;           (forward-bar 1)))))
;;
;; After a forward motion, point has to be placed on the first
;; character after some object, unless no motion was possible at all.
;; Similarly, after a backward motion, point has to be placed on the
;; first character of some object. This implies that point should
;; NEVER be moved to eob or bob, unless an object ends or begins at
;; eob or bob. (Usually, Emacs motions always move as far as possible.
;; But we want to use the motion-function to identify certain objects
;; in the buffer, and thus exact movement to object boundaries is
;; required.)

(evil-define-text-object evil-a-word (count &optional beg end type)
  "Select a word."
  (evil-an-object-range count beg end type #'evil-move-word))

(evil-define-text-object evil-inner-word (count &optional beg end type)
  "Select inner word."
  (evil-inner-object-range count beg end type #'evil-move-word))

(evil-define-text-object evil-a-WORD (count &optional beg end type)
  "Select a WORD."
  (evil-an-object-range count beg end type #'evil-move-WORD))

(evil-define-text-object evil-inner-WORD (count &optional beg end type)
  "Select inner WORD."
  (evil-inner-object-range count beg end type #'evil-move-WORD))

(evil-define-text-object evil-a-sentence (count &optional beg end type)
  "Select a sentence."
  (evil-an-object-range count beg end type #'evil-move-sentence nil nil t))

(evil-define-text-object evil-inner-sentence (count &optional beg end type)
  "Select inner sentence."
  (evil-inner-object-range count beg end type #'evil-move-sentence))

(evil-define-text-object evil-a-paragraph (count &optional beg end type)
  "Select a paragraph."
  :type line
  (evil-an-object-range count beg end type #'evil-move-paragraph nil nil t))

(evil-define-text-object evil-inner-paragraph (count &optional beg end type)
  "Select inner paragraph."
  :type line
  (evil-inner-object-range count beg end type #'evil-move-paragraph))

(evil-define-text-object evil-a-paren (count &optional beg end type)
  "Select a parenthesis."
  :extend-selection nil
  (evil-paren-range count beg end type ?\( ?\)))

(evil-define-text-object evil-inner-paren (count &optional beg end type)
  "Select inner parenthesis."
  :extend-selection nil
  (evil-paren-range count beg end type ?\( ?\) t))

(evil-define-text-object evil-a-bracket (count &optional beg end type)
  "Select a square bracket."
  :extend-selection nil
  (evil-paren-range count beg end type ?\[ ?\]))

(evil-define-text-object evil-inner-bracket (count &optional beg end type)
  "Select inner square bracket."
  :extend-selection nil
  (evil-paren-range count beg end type ?\[ ?\] t))

(evil-define-text-object evil-a-curly (count &optional beg end type)
  "Select a curly bracket (\"brace\")."
  :extend-selection nil
  (evil-paren-range count beg end type ?{ ?}))

(evil-define-text-object evil-inner-curly (count &optional beg end type)
  "Select inner curly bracket (\"brace\")."
  :extend-selection nil
  (evil-paren-range count beg end type ?{ ?} t))

(evil-define-text-object evil-an-angle (count &optional beg end type)
  "Select an angle bracket."
  :extend-selection nil
  (evil-paren-range count beg end type ?< ?>))

(evil-define-text-object evil-inner-angle (count &optional beg end type)
  "Select inner angle bracket."
  :extend-selection nil
  (evil-paren-range count beg end type ?< ?> t))

(evil-define-text-object evil-a-single-quote (count &optional beg end type)
  "Select a single-quoted expression."
  :extend-selection t
  (evil-quote-range count beg end type ?' ?'))

(evil-define-text-object evil-inner-single-quote (count &optional beg end type)
  "Select inner single-quoted expression."
  :extend-selection nil
  (evil-quote-range count beg end type ?' ?' t))

(evil-define-text-object evil-a-double-quote (count &optional beg end type)
  "Select a double-quoted expression."
  :extend-selection t
  (evil-quote-range count beg end type ?\" ?\"))

(evil-define-text-object evil-inner-double-quote (count &optional beg end type)
  "Select inner double-quoted expression."
  :extend-selection nil
  (evil-quote-range count beg end type ?\" ?\" t))

(evil-define-text-object evil-a-back-quote (count &optional beg end type)
  "Select a back-quoted expression."
  :extend-selection t
  (evil-quote-range count beg end type ?\` ?\`))

(evil-define-text-object evil-inner-back-quote (count &optional beg end type)
  "Select inner back-quoted expression."
  :extend-selection nil
  (evil-quote-range count beg end type ?\` ?\` t))

(evil-define-text-object evil-a-tag (count &optional beg end type)
  "Select a tag block."
  :extend-selection nil
  (evil-xml-range count beg end type))

(evil-define-text-object evil-inner-tag (count &optional beg end type)
  "Select inner tag block."
  :extend-selection nil
  (evil-xml-range count beg end type t))

(evil-define-text-object evil-a-symbol (count &optional beg end type)
  "Select a symbol."
  (require 'thingatpt)
  (evil-an-object-range count beg end type #'forward-symbol))

(evil-define-text-object evil-inner-symbol (count &optional beg end type)
  "Select inner symbol."
  (require 'thingatpt)
  (evil-inner-object-range count beg end type #'forward-symbol))

;;; Operator commands

(evil-define-operator evil-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (cond
   ((and (fboundp 'cua--global-mark-active)
         (fboundp 'cua-copy-region-to-global-mark)
         (cua--global-mark-active))
    (cua-copy-region-to-global-mark beg end))
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

(evil-define-operator evil-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (when (evil-visual-state-p)
    (unless (memq type '(line block))
      (let ((range (evil-expand beg end 'line)))
        (setq beg (evil-range-beginning range)
              end (evil-range-end range)
              type (evil-type range))))
    (evil-exit-visual-state))
  (evil-yank beg end type register))

(evil-define-operator evil-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (evil-yank beg end type register yank-handler)
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator evil-delete-line (beg end type register yank-handler)
  "Delete to end of line."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `evil-delete'. In this case we fake the call to
      ;; `evil-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `evil-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (evil-delete beg end 'block register yank-handler)))
     ((eq type 'line)
      (evil-delete beg end type register yank-handler))
     (t
      (evil-delete beg (line-end-position) type register yank-handler)))))

(evil-define-operator evil-delete-whole-line
  (beg end type register yank-handler)
  "Delete whole line."
  :motion evil-line
  (interactive "<R><x>")
  (evil-delete beg end type register yank-handler))

(evil-define-operator evil-delete-char (beg end type register)
  "Delete next character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-delete beg end type register))

(evil-define-operator evil-delete-backward-char (beg end type register)
  "Delete previous character."
  :motion evil-backward-char
  (interactive "<R><x>")
  (evil-delete beg end type register))

(evil-define-command evil-delete-backward-char-and-join (count)
  "Delete previous character and join lines.
If point is at the beginning of a line then the current line will
be joined with the previous line if and only if
`evil-backspace-join-lines'."
  (interactive "p")
  (if (or evil-backspace-join-lines (not (bolp)))
      (call-interactively 'delete-backward-char)
    (error "Beginning of line")))

(evil-define-command evil-delete-backward-word ()
  "Delete previous word."
  (if (and (bolp) (not (bobp)))
      (progn
        (unless evil-backspace-join-lines (error "Beginning of line"))
        (delete-char -1))
    (evil-delete (max
                  (save-excursion
                    (evil-backward-word-begin)
                    (point))
                  (line-beginning-position))
                 (point)
                 'exclusive
                 nil)))

(evil-define-operator evil-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'evil-delete))
        (nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg))))
        (opoint (save-excursion
                  (goto-char beg)
                  (line-beginning-position))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (if ( = opoint (point))
          (evil-open-above 1)
        (evil-open-below 1)))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator evil-change-line (beg end type register yank-handler)
  "Change to end of line."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (evil-change beg end type register yank-handler #'evil-delete-line))

(evil-define-operator evil-change-whole-line
  (beg end type register yank-handler)
  "Change whole line."
  :motion evil-line
  (interactive "<R><x>")
  (evil-change beg end type register yank-handler #'evil-delete-whole-line))

(evil-define-command evil-copy (beg end address)
  "Copy lines in BEG END below line given by ADDRESS."
  :motion evil-line
  (interactive "<r><addr>")
  (goto-char (point-min))
  (forward-line address)
  (let* ((txt (buffer-substring-no-properties beg end))
         (len (length txt)))
    ;; ensure text consists of complete lines
    (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
      (setq txt (concat txt "\n")))
    (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
    (insert txt)
    (forward-line -1)))

(evil-define-command evil-move (beg end address)
  "Move lines in BEG END below line given by ADDRESS."
  :motion evil-line
  (interactive "<r><addr>")
  (goto-char (point-min))
  (forward-line address)
  (let* ((m (set-marker (make-marker) (point)))
         (txt (buffer-substring-no-properties beg end))
         (len (length txt)))
    (delete-region beg end)
    (goto-char m)
    (set-marker m nil)
    ;; ensure text consists of complete lines
    (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
      (setq txt (concat txt "\n")))
    (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
    (insert txt)
    (forward-line -1)))

(evil-define-operator evil-substitute (beg end type register)
  "Change a character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-change beg end type register))

(evil-define-operator evil-upcase (beg end type)
  "Convert text to upper case."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-upcase beg end nil)
    (upcase-region beg end)))

(evil-define-operator evil-downcase (beg end type)
  "Convert text to lower case."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-downcase beg end nil)
    (downcase-region beg end)))

(evil-define-operator evil-invert-case (beg end type)
  "Invert case of text."
  (let (char)
    (if (eq type 'block)
        (evil-apply-on-block #'evil-invert-case beg end nil)
      (save-excursion
        (goto-char beg)
        (while (< beg end)
          (setq char (following-char))
          (delete-char 1 nil)
          (if (eq (upcase char) char)
              (insert-char (downcase char) 1)
            (insert-char (upcase char) 1))
          (setq beg (1+ beg)))))))

(evil-define-operator evil-invert-char (beg end type)
  "Invert case of character."
  :motion evil-forward-char
  (if (eq type 'block)
      (evil-apply-on-block #'evil-invert-case beg end nil)
    (evil-invert-case beg end)
    (when evil-this-motion
      (goto-char end))))

(evil-define-operator evil-rot13 (beg end type)
  "ROT13 encrypt text."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-rot13 beg end nil)
    (rot13-region beg end)))

(evil-define-operator evil-join (beg end)
  "Join the selected lines."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (join-line 1))))

(evil-define-operator evil-join-whitespace (beg end)
  "Join the selected lines without changing whitespace.
\\<evil-normal-state-map>Like \\[evil-join], \
but doesn't insert or remove any spaces."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (evil-move-end-of-line 1)
      (unless (eobp)
        (delete-char 1)))))

(evil-define-operator evil-fill (beg end)
  "Fill text."
  :move-point nil
  :type line
  (save-excursion
    (condition-case nil
        (fill-region beg end)
      (error nil))))

(evil-define-operator evil-fill-and-move (beg end)
  "Fill text and move point to the end of the filled region."
  :move-point nil
  :type line
  (let ((marker (make-marker)))
    (move-marker marker (1- end))
    (condition-case nil
        (progn
          (fill-region beg end)
          (goto-char marker)
          (evil-first-non-blank))
      (error nil))))

(evil-define-operator evil-indent (beg end)
  "Indent text."
  :move-point nil
  :type line
  (if (and (= beg (line-beginning-position))
           (= end (line-beginning-position 2)))
      ;; since some Emacs modes can only indent one line at a time,
      ;; implement "==" as a call to `indent-according-to-mode'
      (indent-according-to-mode)
    (goto-char beg)
    (indent-region beg end))
  (back-to-indentation))

(evil-define-operator evil-indent-line (beg end)
  "Indent the line."
  :motion evil-line
  (evil-indent beg end))

(evil-define-operator evil-shift-left (beg end &optional count)
  "Shift text from BEG to END to the left.
The text is shifted to the nearest multiple of `evil-shift-width'
\(the rounding can be disabled by setting `evil-shift-round').
See also `evil-shift-right'."
  :type line
  (interactive "<r><vc>")
  (let ((beg (set-marker (make-marker) beg))
        (end (set-marker (make-marker) end)))
    (dotimes (i (or count 1))
      (if (not evil-shift-round)
          (indent-rigidly beg end (- evil-shift-width))
        (let* ((indent
                (save-excursion
                  (goto-char beg)
                  (evil-move-beginning-of-line)
                  ;; ignore blank lines
                  (while (and (< (point) end) (looking-at "[ \t]*$"))
                    (forward-line))
                  (if (> (point) end) 0
                    (current-indentation))))
               (offset (1+ (mod (1- indent) evil-shift-width))))
          (indent-rigidly beg end (- offset)))))
    (set-marker beg nil)
    (set-marker end nil)))

(evil-define-operator evil-shift-right (beg end &optional count)
  "Shift text from BEG to END to the right.
The text is shifted to the nearest multiple of `evil-shift-width'
\(the rounding can be disabled by setting `evil-shift-round').
See also `evil-shift-left'."
  :type line
  (interactive "<r><vc>")
  (let ((beg (set-marker (make-marker) beg))
        (end (set-marker (make-marker) end)))
    (dotimes (i (or count 1))
      (if (not evil-shift-round)
          (indent-rigidly beg end evil-shift-width)
        (let* ((indent
                (save-excursion
                  (goto-char beg)
                  (evil-move-beginning-of-line nil)
                  (while (and (< (point) end) (looking-at "[ \t]*$"))
                    (forward-line))
                  (if (> (point) end) 0
                    (current-indentation))))
               (offset (- evil-shift-width (mod indent evil-shift-width))))
          (indent-rigidly beg end offset))))
    (set-marker beg nil)
    (set-marker end nil)))

(evil-define-command evil-shift-right-line (count)
  "Shift the current line COUNT times to the right.
The text is shifted to the nearest multiple of
`evil-shift-width'. Like `evil-shift-right' but always works on
the current line."
  (interactive "<c>")
  (evil-shift-right (line-beginning-position) (line-end-position) count))

(evil-define-command evil-shift-left-line (count)
  "Shift the current line COUNT times to the leeft.
The text is shifted to the nearest multiple of
`evil-shift-width'. Like `evil-shift-leeft' but always works on
the current line."
  (interactive "<c>")
  (evil-shift-left (line-beginning-position) (line-end-position) count))

(evil-define-operator evil-align-left (beg end type &optional width)
  "Right-align lines in the region at WIDTH columns.
The default for width is the value of `fill-column'."
  :motion evil-line
  :type line
  (interactive "<R><a>")
  (evil-justify-lines beg end 'left (if width
                                        (string-to-number width)
                                      0)))

(evil-define-operator evil-align-right (beg end type &optional width)
  "Right-align lines in the region at WIDTH columns.
The default for width is the value of `fill-column'."
  :motion evil-line
  :type line
  (interactive "<R><a>")
  (evil-justify-lines beg end 'right (if width
                                         (string-to-number width)
                                       fill-column)))

(evil-define-operator evil-align-center (beg end type &optional width)
  "Centers lines in the region between WIDTH columns.
The default for width is the value of `fill-column'."
  :motion evil-line
  :type line
  (interactive "<R><a>")
  (evil-justify-lines beg end 'center (if width
                                          (string-to-number width)
                                        fill-column)))

(evil-define-operator evil-replace (beg end type char)
  "Replace text from BEG to END with CHAR."
  :motion evil-forward-char
  (interactive "<R>"
               (evil-save-cursor
                 (evil-refresh-cursor 'replace)
                 (list (evil-read-key))))
  (when char
    (if (eq type 'block)
        (save-excursion
          (evil-apply-on-rectangle
           #'(lambda (begcol endcol char)
               (let ((maxcol (evil-column (line-end-position))))
                 (when (< begcol maxcol)
                   (setq endcol (min endcol maxcol))
                   (let ((beg (evil-move-to-column begcol nil t))
                         (end (evil-move-to-column endcol nil t)))
                     (delete-region beg end)
                     (insert (make-string (- endcol begcol) char))))))
           beg end char))
      (goto-char beg)
      (cond
       ((eq char ?\n)
        (delete-region beg end)
        (newline)
        (when evil-auto-indent
          (indent-according-to-mode)))
       (t
        (while (< (point) end)
          (if (eq (char-after) ?\n)
              (forward-char)
            (delete-char 1)
            (insert-char char 1)))
        (goto-char (max beg (1- end))))))))

(evil-define-command evil-paste-before
  (count &optional register yank-handler)
  "Pastes the latest yanked text before the cursor position.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste count register)
    (evil-with-undo
      (let* ((text (if register
                       (evil-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when text
          (if (functionp yank-handler)
              (let ((evil-paste-count count)
                    ;; for non-interactive use
                    (this-command #'evil-paste-before))
                (push-mark opoint t)
                (insert-for-yank text))
            ;; no yank-handler, default
            (set-text-properties 0 (length text) nil text)
            (push-mark opoint t)
            (dotimes (i (or count 1))
              (insert-for-yank text))
            (setq evil-last-paste
                  (list #'evil-paste-before
                        count
                        opoint
                        opoint    ; beg
                        (point))) ; end
            (evil-set-marker ?\[ opoint)
            (evil-set-marker ?\] (1- (point)))
            (when (> (length text) 0)
              (backward-char))))
        ;; no paste-pop after pasting from a register
        (when register
          (setq evil-last-paste nil))
        (and (> (length text) 0) text)))))

(evil-define-command evil-paste-after
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste count register)
    (evil-with-undo
      (let* ((text (if register
                       (evil-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when text
          (if (functionp yank-handler)
              (let ((evil-paste-count count)
                    ;; for non-interactive use
                    (this-command #'evil-paste-after))
                (insert-for-yank text))
            ;; no yank-handler, default
            (set-text-properties 0 (length text) nil text)
            (unless (eolp) (forward-char))
            (push-mark (point) t)
            ;; TODO: Perhaps it is better to collect a list of all
            ;; (point . mark) pairs to undo the yanking for COUNT > 1.
            ;; The reason is that this yanking could very well use
            ;; `yank-handler'.
            (let ((beg (point)))
              (dotimes (i (or count 1))
                (insert-for-yank text))
              (setq evil-last-paste
                    (list #'evil-paste-after
                          count
                          opoint
                          beg       ; beg
                          (point))) ; end
              (evil-set-marker ?\[ beg)
              (evil-set-marker ?\] (1- (point)))
              (when (evil-normal-state-p)
                (evil-move-cursor-back)))))
        (when register
          (setq evil-last-paste nil))
        (and (> (length text) 0) text)))))

(evil-define-command evil-visual-paste (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "P<x>")
  ;; evil-visual-paste is typically called from evil-paste-before or
  ;; evil-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'evil-visual-paste)
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         new-kill
         paste-eob)
    (evil-with-undo
      (let* ((kill-ring (list (current-kill 0)))
             (kill-ring-yank-pointer kill-ring))
        (when (evil-visual-state-p)
          (evil-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `evil-paste-after' because `evil-delete'
          ;; will move point to the line above
          (when (and (= evil-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (evil-delete evil-visual-beginning evil-visual-end
                       (evil-visual-type))
          (when (and (eq yank-handler #'evil-yank-line-handler)
                     (not (eq (evil-visual-type) 'line))
                     (not (= evil-visual-end (point-max))))
            (insert "\n"))
          (evil-normal-state)
          (setq new-kill (current-kill 0))
          (current-kill 1))
        (if paste-eob
            (evil-paste-after count register)
          (evil-paste-before count register)))
      (kill-new new-kill)
      ;; mark the last paste as visual-paste
      (setq evil-last-paste
            (list (nth 0 evil-last-paste)
                  (nth 1 evil-last-paste)
                  (nth 2 evil-last-paste)
                  (nth 3 evil-last-paste)
                  (nth 4 evil-last-paste)
                  t)))))

(defun evil-paste-from-register (register)
  "Paste from REGISTER."
  (interactive
   (let ((overlay (make-overlay (point) (point)))
         (string "\""))
     (unwind-protect
         (progn
           ;; display " in the buffer while reading register
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (list (or evil-this-register (read-char))))
       (delete-overlay overlay))))
  (when (evil-paste-before nil register t)
    ;; go to end of pasted text
    (forward-char)))

(evil-define-command evil-use-register (register)
  "Use REGISTER for the next command."
  :keep-visual t
  (interactive "<C>")
  (setq evil-this-register register))

(evil-define-command evil-record-macro (register)
  "Record a keyboard macro into REGISTER."
  :keep-visual t
  :suppress-operator t
  (interactive
   (list (unless (and evil-this-macro defining-kbd-macro)
           (or evil-this-register (read-char)))))
  (cond
   ((and evil-this-macro defining-kbd-macro)
    (condition-case nil
        (end-kbd-macro)
      (error nil))
    (when last-kbd-macro
      (when (member last-kbd-macro '("" []))
        (setq last-kbd-macro nil))
      (evil-set-register evil-this-macro last-kbd-macro))
    (setq evil-this-macro nil))
   (t
    (when defining-kbd-macro (end-kbd-macro))
    (setq evil-this-macro register)
    (evil-set-register evil-this-macro nil)
    (start-kbd-macro nil))))

(evil-define-command evil-execute-macro (count macro)
  "Execute keyboard macro MACRO, COUNT times.
When called with a non-numerical prefix \
\(such as \\[universal-argument]),
COUNT is infinite. MACRO is read from a register
when called interactively."
  :keep-visual t
  :suppress-operator t
  (interactive
   (let (count macro register)
     (setq count (if current-prefix-arg
                     (if (numberp current-prefix-arg)
                         current-prefix-arg
                       0) 1)
           register (or evil-this-register (read-char)))
     (if (eq register ?@)
         (setq macro last-kbd-macro)
       (setq macro (evil-get-register register t)))
     (list count macro)))
  (if (or (and (not (stringp macro))
               (not (vectorp macro)))
          (member macro '("" [])))
      ;; allow references to currently empty registers
      ;; when defining macro
      (unless evil-this-macro
        (error "No previous macro"))
    (condition-case err
        (evil-with-single-undo
          (execute-kbd-macro macro count))
      ;; enter Normal state if the macro fails
      (error
       (evil-normal-state)
       (evil-normalize-keymaps)
       (signal (car err) (cdr err))))))

;;; Visual commands

(evil-define-motion evil-visual-restore ()
  "Restore previous selection."
  (let* ((point (point))
         (mark (or (mark t) point))
         (dir evil-visual-direction)
         (type (evil-visual-type))
         range)
    (unless (evil-visual-state-p)
      (cond
       ;; No previous selection.
       ((or (null evil-visual-selection)
            (null evil-visual-mark)
            (null evil-visual-point)))
       ;; If the type was one-to-one, it is preferable to infer
       ;; point and mark from the selection's boundaries. The reason
       ;; is that a destructive operation may displace the markers
       ;; inside the selection.
       ((evil-type-property type :one-to-one)
        (setq range (evil-contract-range (evil-visual-range))
              mark (evil-range-beginning range)
              point (evil-range-end range))
        (when (< dir 0)
          (evil-swap mark point)))
       ;; If the type wasn't one-to-one, we have to restore the
       ;; selection on the basis of the previous point and mark.
       (t
        (setq mark evil-visual-mark
              point evil-visual-point)))
      (evil-visual-make-selection mark point type t))))

(evil-define-motion evil-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+           +---M
        |   |    <=>    |   |
        +---P           P---+

For example, if mark is in the upper left corner and point
in the lower right, this function puts mark in the upper right
corner and point in the lower left."
  (cond
   ((eq evil-visual-selection 'block)
    (let* ((point (point))
           (mark (or (mark t) point))
           (point-col (evil-column point))
           (mark-col (evil-column mark))
           (mark (save-excursion
                   (goto-char mark)
                   (evil-move-to-column point-col)
                   (point)))
           (point (save-excursion
                    (goto-char point)
                    (evil-move-to-column mark-col)
                    (point))))
      (evil-visual-refresh mark point)))
   (t
    (evil-exchange-point-and-mark)
    (evil-visual-refresh))))

(evil-define-command evil-visual-rotate (corner &optional beg end type)
  "In Visual Block selection, put point in CORNER.
Corner may be one of `upper-left', `upper-right', `lower-left'
and `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

When called interactively, the selection is rotated blockwise."
  :keep-visual t
  (interactive
   (let ((corners '(upper-left upper-right lower-right lower-left)))
     (list (or (cadr (memq (evil-visual-block-corner) corners))
               'upper-left))))
  (let* ((beg (or beg (point)))
         (end (or end (mark t) beg))
         (type (or type evil-this-type))
         range)
    (cond
     ((memq type '(rectangle block))
      (setq range (evil-block-rotate beg end :corner corner)
            beg (pop range)
            end (pop range))
      (unless (eq corner (evil-visual-block-corner corner beg end))
        (evil-swap beg end))
      (goto-char beg)
      (when (evil-visual-state-p)
        (evil-move-mark end)
        (evil-visual-refresh nil nil nil :corner corner)))
     ((memq corner '(upper-right lower-right))
      (goto-char (max beg end))
      (when (evil-visual-state-p)
        (evil-move-mark (min beg end))))
     (t
      (goto-char (min beg end))
      (when (evil-visual-state-p)
        (evil-move-mark (max beg end)))))))

;;; Insertion commands

(defun evil-insert (count &optional vcount skip-empty-lines)
  "Switch to Insert state just before point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT - 1 lines starting at the same column.
If SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of the
lines.  This is the default behaviour for Visual-state insertion."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (and (evil-visual-state-p)
              (memq (evil-visual-type) '(line block))
              (save-excursion
                ;; go to upper-left corner temporarily so
                ;; `count-lines' yields accurate results
                (evil-visual-rotate 'upper-left)
                (count-lines evil-visual-beginning evil-visual-end)))
         (evil-visual-state-p)))
  (if (and (evil-called-interactively-p)
           (evil-visual-state-p))
      (cond
       ((eq (evil-visual-type) 'line)
        (evil-visual-rotate 'upper-left)
        (evil-insert-line count vcount))
       ((eq (evil-visual-type) 'block)
        (let ((column (min (evil-column evil-visual-beginning)
                           (evil-column evil-visual-end))))
          (evil-visual-rotate 'upper-left)
          (move-to-column column t)
          (evil-insert count vcount skip-empty-lines)))
       (t
        (evil-visual-rotate 'upper-left)
        (evil-insert count vcount skip-empty-lines)))
    (setq evil-insert-count count
          evil-insert-lines nil
          evil-insert-vcount (and vcount
                                  (> vcount 1)
                                  (list (line-number-at-pos)
                                        (current-column)
                                        vcount))
          evil-insert-skip-empty-lines skip-empty-lines)
    (evil-insert-state 1)))

(defun evil-append (count &optional vcount skip-empty-lines)
  "Switch to Insert state just after point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT - 1 lines starting at the same column.  If
SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of
the lines."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (and (evil-visual-state-p)
              (memq (evil-visual-type) '(line block))
              (save-excursion
                ;; go to upper-left corner temporarily so
                ;; `count-lines' yields accurate results
                (evil-visual-rotate 'upper-left)
                (count-lines evil-visual-beginning evil-visual-end)))))
  (if (and (evil-called-interactively-p)
           (evil-visual-state-p))
      (cond
       ((or (eq (evil-visual-type) 'line)
            (and (eq (evil-visual-type) 'block)
                 (memq last-command '(next-line previous-line))
                 (numberp temporary-goal-column)
                 (= temporary-goal-column most-positive-fixnum)))
        (evil-visual-rotate 'upper-left)
        (evil-append-line count vcount))
       ((eq (evil-visual-type) 'block)
        (let ((column (max (evil-column evil-visual-beginning)
                           (evil-column evil-visual-end))))
          (evil-visual-rotate 'upper-left)
          (move-to-column column t)
          (evil-insert count vcount skip-empty-lines)))
       (t
        (evil-visual-rotate 'lower-right)
        (evil-append count)))
    (unless (eolp) (forward-char))
    (evil-insert count vcount skip-empty-lines)))

(defun evil-insert-resume (count)
  "Switch to Insert state at previous insertion point.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-goto-mark ?^ t)
  (evil-insert count))

(defun evil-maybe-remove-spaces ()
  "Remove space from newly opened empty line.
This function should be called from `post-command-hook' after
`evil-open-above' or `evil-open-below'.  If the last command
finished insert state and if the current line consists of
whitespaces only, then those spaces have been inserted because of
the indentation.  In this case those spaces are removed leaving a
completely empty line."
  (unless (memq this-command '(evil-open-above evil-open-below))
    (remove-hook 'post-command-hook 'evil-maybe-remove-spaces)
    (when (and (not (evil-insert-state-p))
               (save-excursion
                 (beginning-of-line)
                 (looking-at "^\\s-*$")))
      (delete-region (line-beginning-position)
                     (line-end-position)))))

(defun evil-open-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (evil-insert-state 1)
  (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
  (when evil-auto-indent
    (indent-according-to-mode)))

(defun evil-open-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (evil-insert-state 1)
  (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
  (when evil-auto-indent
    (indent-according-to-mode)))

(defun evil-insert-line (count &optional vcount)
  "Switch to insert state at beginning of current line.
Point is placed at the first non-blank character on the current
line.  The insertion will be repeated COUNT times.  If VCOUNT is
non nil it should be number > 0. The insertion will be repeated
in the next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (back-to-indentation)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'evil-first-non-blank
                   vcount)))
  (evil-insert-state 1))

(defun evil-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (evil-move-end-of-line)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-line
                   vcount)))
  (evil-insert-state 1))

(evil-define-command evil-insert-digraph (count)
  "Insert COUNT digraphs."
  :repeat change
  (interactive "p")
  (let ((digraph (evil-read-digraph-char 0)))
    (insert-char digraph count)))

(evil-define-command evil-ex-show-digraphs ()
  "Shows a list of all available digraphs."
  :repeat nil
  (evil-with-view-list "evil-digraphs"
    (let ((i 0)
          (digraphs
           (mapcar #'(lambda (digraph)
                       (cons (cdr digraph)
                             (car digraph)))
                   (append evil-digraphs-table
                           evil-digraphs-table-user))))
      (dolist (digraph digraphs)
        (insert (nth 0 digraph) "\t"
                (nth 1 digraph) " "
                (nth 2 digraph)
                (if (= i 2) "\n" "\t\t"))
        (setq i (mod (1+ i) 3))))))

(defun evil-copy-from-above (arg)
  "Copy characters from preceding non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move backward.
See also \\<evil-insert-state-map>\\[evil-copy-from-below]."
  (interactive
   (cond
    ;; if a prefix argument was given, repeat it for subsequent calls
    ((and (null current-prefix-arg)
          (eq last-command #'evil-copy-from-above))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (insert (evil-copy-chars-from-line 1 (- arg))))

(defun evil-copy-from-below (arg)
  "Copy characters from following non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move forward.
See also \\<evil-insert-state-map>\\[evil-copy-from-above]."
  (interactive
   (cond
    ((and (null current-prefix-arg)
          (eq last-command #'evil-copy-from-below))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (insert (evil-copy-chars-from-line 1 arg)))

;; adapted from `copy-from-above-command' in misc.el
(defun evil-copy-chars-from-line (n num &optional col)
  "Return N characters from line NUM, starting at column COL.
NUM is relative to the current line and can be negative.
COL defaults to the current column."
  (interactive "p")
  (let ((col (or col (current-column))) prefix)
    (save-excursion
      (forward-line num)
      (when (looking-at "[[:space:]]*$")
        (if (< num 0)
            (skip-chars-backward " \t\n")
          (skip-chars-forward " \t\n")))
      (evil-move-beginning-of-line)
      (move-to-column col)
      ;; if the column winds up in middle of a tab,
      ;; return the appropriate number of spaces
      (when (< col (current-column))
        (if (eq (preceding-char) ?\t)
            (let ((len (min n (- (current-column) col))))
              (setq prefix (make-string len ?\s)
                    n (- n len)))
          ;; if in middle of a control char, return the whole char
          (backward-char 1)))
      (concat prefix
              (buffer-substring (point)
                                (min (line-end-position)
                                     (+ n (point))))))))

;; completion
(evil-define-command evil-complete-next (&optional arg)
  "Complete to the nearest following word.
Search backward if a match isn't found.
Calls `evil-complete-next-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall evil-complete-next-minibuffer-func)
    (funcall evil-complete-next-func arg)))

(evil-define-command evil-complete-previous (&optional arg)
  "Complete to the nearest preceding word.
Search forward if a match isn't found.
Calls `evil-complete-previous-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall evil-complete-previous-minibuffer-func)
    (funcall evil-complete-previous-func arg)))

(evil-define-command evil-complete-next-line (&optional arg)
  "Complete a whole line.
Calls `evil-complete-next-line-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall evil-complete-next-minibuffer-func)
    (funcall evil-complete-next-line-func arg)))

(evil-define-command evil-complete-previous-line (&optional arg)
  "Complete a whole line.
Calls `evil-complete-previous-line-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall evil-complete-previous-minibuffer-func)
    (funcall evil-complete-previous-line-func arg)))

;;; Search

(defun evil-repeat-search (flag)
  "Called to record a search command.
FLAG is either 'pre or 'post if the function is called before resp.
after executing the command."
  (cond
   ((and (evil-operator-state-p) (eq flag 'pre))
    (evil-repeat-record (this-command-keys))
    (evil-clear-command-keys))
   ((and (evil-operator-state-p) (eq flag 'post))
    ;; The value of (this-command-keys) at this point should be the
    ;; key-sequence that called the last command that finished the
    ;; search, usually RET. Therefore this key-sequence will be
    ;; recorded in the post-command of the operator. Alternatively we
    ;; could do it here.
    (evil-repeat-record (if evil-regexp-search
                            (car-safe regexp-search-ring)
                          (car-safe search-ring))))
   (t (evil-repeat-motion flag))))

(evil-define-motion evil-search-forward ()
  (format "Search forward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat evil-repeat-search
  (evil-search-incrementally t evil-regexp-search))

(evil-define-motion evil-search-backward ()
  (format "Search backward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat evil-repeat-search
  (evil-search-incrementally nil evil-regexp-search))

(evil-define-motion evil-search-next (count)
  "Repeat the last search."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search (if evil-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 isearch-forward evil-regexp-search)))

(evil-define-motion evil-search-previous (count)
  "Repeat the last search in the opposite direction."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search (if evil-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 (not isearch-forward) evil-regexp-search)))

(evil-define-motion evil-search-symbol-backward (count)
  "Search backward for symbol under point."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search-symbol nil)))

(evil-define-motion evil-search-symbol-forward (count)
  "Search forward for symbol under point."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search-symbol t)))

(evil-define-motion evil-search-unbounded-symbol-backward (count)
  "Search backward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search-symbol nil t)))

(evil-define-motion evil-search-unbounded-symbol-forward (count)
  "Search forward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search-symbol t t)))

(evil-define-motion evil-goto-definition ()
  "Go to definition or first occurrence of symbol under point."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t))
         (search (format "\\_<%s\\_>" (regexp-quote string)))
         ientry ipos)
    ;; load imenu if available
    (unless (featurep 'imenu)
      (condition-case nil
          (require 'imenu)
        (error nil)))
    (if (null string)
        (error "No symbol under cursor")
      (setq isearch-forward t)
      ;; if imenu is available, try it
      (cond
       ((fboundp 'imenu--make-index-alist)
        (condition-case nil
            (setq ientry (imenu--make-index-alist))
          (error nil))
        (setq ientry (assoc string ientry))
        (setq ipos (cdr ientry))
        (when (and (markerp ipos)
                   (eq (marker-buffer ipos) (current-buffer)))
          (setq ipos (marker-position ipos)))
        (cond
         ;; imenu found a position, so go there and
         ;; highlight the occurrence
         ((numberp ipos)
          (evil-search search t t ipos))
         ;; imenu failed, so just go to first occurrence in buffer
         (t
          (evil-search search t t (point-min)))))
       ;; no imenu, so just go to first occurrence in buffer
       (t
        (evil-search search t t (point-min)))))))

;;; Folding

(evil-define-command evil-toggle-fold ()
  "Open or close a fold."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1)
    (with-no-warnings (hs-toggle-hiding))))

(evil-define-command evil-open-folds ()
  "Open all folds.
See also `evil-close-folds'."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1)
    (with-no-warnings (hs-show-all)))
  (when (memq major-mode '(c-mode c++-mode))
    (when (fboundp 'hide-ifdef-mode)
      (hide-ifdef-mode 1)
      (with-no-warnings (show-ifdefs)))))

(evil-define-command evil-close-folds ()
  "Close all folds.
See also `evil-open-folds'."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1)
    (with-no-warnings (hs-hide-all)))
  (when (memq major-mode '(c-mode c++-mode))
    (when (fboundp 'hide-ifdef-mode)
      (hide-ifdef-mode 1)
      (with-no-warnings (hide-ifdefs)))))

(evil-define-command evil-open-fold ()
  "Open fold.
See also `evil-close-fold'."
  (with-no-warnings
    (cond
     ((and (memq major-mode '(c-mode c++-mode))
           (fboundp 'hide-ifdef-mode)
           (hide-ifdef-mode 1)
           (save-excursion
             (beginning-of-line)
             (looking-at hif-ifx-else-endif-regexp)))
      (show-ifdef-block))
     ((fboundp 'hs-minor-mode)
      (hs-minor-mode 1)
      (hs-show-block)))))

(evil-define-command evil-close-fold ()
  "Close fold.
See also `evil-open-fold'."
  (with-no-warnings
    (cond
     ((and (memq major-mode '(c-mode c++-mode))
           (fboundp 'hide-ifdef-mode)
           (hide-ifdef-mode 1)
           (save-excursion
             (beginning-of-line)
             (looking-at hif-ifx-else-endif-regexp)))
      (hide-ifdef-block))
     ((fboundp 'hs-minor-mode)
      (hs-minor-mode 1)
      (hs-hide-block)))))

;;; Ex

(evil-define-operator evil-write (beg end type filename &optional bang)
  "Save the current buffer, from BEG to END, to FILENAME.
The current buffer's filename is not changed unless it has no
associated file and no region is specified.  If the file already
exists and the BANG argument is non-nil, it is overwritten
without confirmation."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<R><fsh><!>")
  (let ((bufname (buffer-file-name (buffer-base-buffer))))
    (when (zerop (length filename))
      (setq filename bufname))
    (cond
     ((zerop (length filename))
      (error "Please specify a file name for the buffer"))
     ;; execute command on region
     ((eq (aref filename 0) ?!)
      (shell-command-on-region beg end (substring filename 1)))
     ;; with region, always save to file without resetting modified flag
     ((and beg end)
      (write-region beg end filename nil nil nil (not bang)))
     ;; no current file
     ((null bufname)
      (write-file filename (not bang)))
     ;; save current buffer to its file
     ((string= filename bufname)
      (if (not bang) (save-buffer) (write-file filename)))
     ;; save to another file
     (t
      (write-region nil nil filename
                    nil (not bufname) nil
                    (not bang))))))

(evil-define-command evil-write-all (bang)
  "Saves all buffers visiting a file.
If BANG is non nil then read-only buffers are saved, too,
otherwise they are skipped. "
  :repeat nil
  :move-point nil
  (interactive "<!>")
  (if bang
      (save-some-buffers t)
    ;; save only buffer that are not read-only and
    ;; that are visiting a file
    (save-some-buffers t
                       #'(lambda ()
                           (and (not buffer-read-only)
                                (buffer-file-name))))))

(evil-define-command evil-save (filename &optional bang)
  "Save the current buffer to FILENAME.
Changes the file name of the current buffer to FILENAME.  If no
FILENAME is given, the current file name is used."
  :repeat nil
  :move-point nil
  (interactive "<f><!>")
  (when (zerop (length filename))
    (setq filename (buffer-file-name (buffer-base-buffer))))
  (write-file filename (not bang)))

(evil-define-command evil-edit (file &optional bang)
  "Open FILE.
If no FILE is specified, reload the current buffer from disk."
  :repeat nil
  (interactive "<f><!>")
  (if file
      (find-file file)
    (revert-buffer bang (or bang (not (buffer-modified-p))) t)))

(evil-define-command evil-read (count file)
  "Inserts the contents of FILE below the current line or line COUNT."
  :repeat nil
  :move-point nil
  (interactive "P<fsh>")
  (when (and file (not (zerop (length file))))
    (when count (goto-char (point-min)))
    (when (or (not (zerop (forward-line (or count 1))))
              (not (bolp)))
      (insert "\n"))
    (if (/= (aref file 0) ?!)
        (let ((result (insert-file-contents file)))
          (save-excursion
            (forward-char (cadr result))
            (unless (bolp) (insert "\n"))))
      (shell-command (substring file 1) t)
      (save-excursion
        (goto-char (mark))
        (unless (bolp) (insert "\n"))))))

(evil-define-command evil-show-buffers ()
  "Shows the buffer-list.
The same as `list-buffers' but selects the buffer window afterwards."
  :repeat nil
  (list-buffers)
  (select-window (get-buffer-window "*Buffer List*")))

(evil-define-command evil-show-files ()
  "Shows the file-list.
The same as `list-buffers', but shows only buffers visiting files
and selects the list window afterwards."
  :repeat nil
  (list-buffers 1)
  (select-window (get-buffer-window "*Buffer List*")))

(evil-define-command evil-buffer (buffer)
  "Switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (if buffer
      (when (or (get-buffer buffer)
                (y-or-n-p (format "No buffer with name \"%s\" exists. \
Create new buffer? " buffer)))
        (switch-to-buffer buffer))
    (switch-to-buffer (other-buffer))))

(evil-define-command evil-next-buffer (&optional count)
  "Goes to the `count'-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (next-buffer)))

(evil-define-command evil-prev-buffer (&optional count)
  "Goes to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (previous-buffer)))

(evil-define-command evil-delete-buffer (buffer &optional bang)
  "Deletes a buffer.
All windows currently showing this buffer will be closed except
for the last window in each frame."
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil)
      (dolist (process (process-list))
        (when (eq (process-buffer process) (current-buffer))
          (set-process-query-on-exit-flag process nil))))
    ;; get all windows that show this buffer
    (let ((wins (get-buffer-window-list (current-buffer) nil t)))
      ;; if the buffer which was initiated by emacsclient,
      ;; call `server-edit' from server.el to avoid
      ;; "Buffer still has clients" message
      (if (and (fboundp 'server-edit)
               (boundp 'server-buffer-clients)
               server-buffer-clients)
          (server-edit)
        (kill-buffer nil))
      ;; close all windows that showed this buffer
      (mapc #'(lambda (w)
                (condition-case nil
                    (delete-window w)
                  (error nil)))
            wins))))

(evil-define-command evil-quit (&optional bang)
  "Closes the current window, current frame, Emacs.
If the current frame belongs to some client the client connection
is closed."
  :repeat nil
  (interactive "<!>")
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (let ((proc (frame-parameter (selected-frame) 'client)))
           (if proc
               (evil-quit-all bang)
             (delete-frame)))
       (error
        (evil-quit-all bang))))))

(evil-define-command evil-quit-all (&optional bang)
  "Exits Emacs, asking for saving."
  :repeat nil
  (interactive "<!>")
  (if (null bang)
      (save-buffers-kill-terminal)
    (let ((proc (frame-parameter (selected-frame) 'client)))
      (if proc
          (with-no-warnings
            (server-delete-client proc))
        (dolist (process (process-list))
          (set-process-query-on-exit-flag process nil))
        (kill-emacs)))))

(evil-define-command evil-save-and-quit ()
  "Exits Emacs, without saving."
  (save-buffers-kill-terminal t))

(evil-define-command evil-save-and-close (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (evil-write nil nil nil file bang)
  (evil-quit))

(evil-define-command evil-save-modified-and-close (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (when (buffer-modified-p)
    (evil-write nil nil nil file bang))
  (evil-quit))

(evil-define-operator evil-shell-command
  (beg end command &optional previous)
  "Execute a shell command.
If BEG and END is specified, COMMAND is executed on the region,
which is replaced with the command's output. Otherwise, the
output is displayed in its own buffer. If PREVIOUS is non-nil,
the previous shell command is executed instead."
  :motion nil
  (interactive "<r><sh><!>")
  (when command
    (setq command (evil-ex-replace-special-filenames command)))
  (if (zerop (length command))
      (when previous (setq command evil-previous-shell-command))
    (setq evil-previous-shell-command command))
  (cond
   ((zerop (length command))
    (if previous (error "No previous shell command")
      (error "No shell command")))
   ((and beg end)
    (shell-command-on-region beg end command t)
    (goto-char beg)
    (evil-first-non-blank))
   (t
    (shell-command command))))

;; TODO: escape special characters (currently only \n) ... perhaps
;; there is some Emacs function doing this?
(evil-define-command evil-show-registers ()
  "Shows the contents of all registers."
  :repeat nil
  (evil-with-view-list "evil-registers"
    (setq truncate-lines t)
    (dolist (reg (evil-register-list))
      (when (cdr reg)
        (insert (format "\"%c\t%s"
                        (car reg)
                        (if (stringp (cdr reg))
                            (replace-regexp-in-string "\n" "^J" (cdr reg))
                          (cdr reg))))
        (newline)))))

(eval-when-compile (require 'ffap))
(evil-define-command evil-find-file-at-point-with-line ()
  "Opens the file at point and goes to line-number."
  (require 'ffap)
  (let ((fname (with-no-warnings (ffap-file-at-point))))
    (if fname
        (let ((line
               (save-excursion
                 (goto-char (cadr ffap-string-at-point-region))
                 (and (re-search-backward ":\\([0-9]+\\)\\="
                                          (line-beginning-position) t)
                      (string-to-number (match-string 1))))))
          (with-no-warnings (ffap-other-window))
          (when line
            (goto-char (point-min))
            (forward-line (1- line))))
      (error "File does not exist."))))

(evil-ex-define-argument-type state
  "Defines an argument type which can take state names."
  :collection
  (lambda (arg predicate flag)
    (let ((completions
           (append '("nil")
                   (mapcar #'(lambda (state)
                               (format "%s" (car state)))
                           evil-state-properties))))
      (when arg
        (cond
         ((eq flag nil)
          (try-completion arg completions predicate))
         ((eq flag t)
          (all-completions arg completions predicate))
         ((eq flag 'lambda)
          (test-completion arg completions predicate))
         ((eq (car-safe flag) 'boundaries)
          (cons 'boundaries
                (completion-boundaries arg
                                       completions
                                       predicate
                                       (cdr flag)))))))))

(evil-define-interactive-code "<state>"
  "A valid evil state."
  :ex-arg state
  (list (when (and (evil-ex-p) evil-ex-argument)
          (intern evil-ex-argument))))

;; TODO: should we merge this command with `evil-set-initial-state'?
(evil-define-command evil-ex-set-initial-state (state)
  "Set the initial state for the current major mode to STATE.
This is the state the buffer comes up in. See `evil-set-initial-state'."
  :repeat nil
  (interactive "<state>")
  (if (not (or (assq state evil-state-properties)
               (null state)))
      (error "State %s cannot be set as initial Evil state" state)
    (let ((current-initial-state (evil-initial-state major-mode)))
      (unless (eq current-initial-state state)
        ;; only if we selected a new mode
        (when (y-or-n-p (format "Major-mode `%s' has initial mode `%s'. \
Change to `%s'? "
                                major-mode
                                (or current-initial-state "DEFAULT")
                                (or state "DEFAULT")))
          (evil-set-initial-state major-mode state)
          (when (y-or-n-p "Save setting in customization file? ")
            (dolist (s (list current-initial-state state))
              (when s
                (let ((var (intern (format "evil-%s-state-modes" s))))
                  (customize-save-variable var (symbol-value var)))))))))))

(evil-define-command evil-force-normal-state ()
  "Switch to normal state without recording current command."
  :repeat abort
  :suppress-operator t
  (evil-normal-state))

(evil-define-motion evil-ex-search-next (count)
  "Goes to the next occurrence."
  :jump t
  :type exclusive
  (setq evil-ex-search-start-point (point)
        evil-ex-last-was-search t)
  (let ((orig (point))
        wrapped)
    (dotimes (i (or count 1))
      (if (eq evil-ex-search-direction 'backward)
          (unless (bobp) (backward-char))
        (unless (eobp) (forward-char))
        ;; maybe skip end-of-line
        (when (and evil-move-cursor-back
                   (eolp) (not (eobp)))
          (forward-char)))
      (let ((res (evil-ex-find-next)))
        (cond
         ((not res)
          (goto-char orig)
          (signal 'search-failed
                  (list (evil-ex-pattern-regex evil-ex-search-pattern))))
         ((eq res 'wrapped) (setq wrapped t)))))
    (if wrapped
        (let (message-log-max)
          (message "Search wrapped")))
    (goto-char (match-beginning 0))
    (setq evil-ex-search-match-beg (match-beginning 0)
          evil-ex-search-match-end (match-end 0))
    (evil-ex-search-goto-offset evil-ex-search-offset)
    (evil-ex-search-activate-highlight evil-ex-search-pattern)))

(evil-define-motion evil-ex-search-previous (count)
  "Goes the the previous occurrence."
  :jump t
  :type exclusive
  (let ((evil-ex-search-direction
         (if (eq evil-ex-search-direction 'backward) 'forward 'backward)))
    (evil-ex-search-next count)))

(defun evil-repeat-ex-search (flag)
  "Called to record a search command.
FLAG is either 'pre or 'post if the function is called before
resp.  after executing the command."
  (cond
   ((and (evil-operator-state-p) (eq flag 'pre))
    (evil-repeat-record (this-command-keys))
    (evil-clear-command-keys))
   ((and (evil-operator-state-p) (eq flag 'post))
    ;; The value of (this-command-keys) at this point should be the
    ;; key-sequence that called the last command that finished the
    ;; search, usually RET. Therefore this key-sequence will be
    ;; recorded in the post-command of the operator. Alternatively we
    ;; could do it here.
    (evil-repeat-record (evil-ex-pattern-regex evil-ex-search-pattern)))
   (t (evil-repeat-motion flag))))

(evil-define-motion evil-ex-search-forward (count)
  "Starts a forward search."
  :jump t
  :type exclusive
  :repeat evil-repeat-ex-search
  (evil-ex-start-search 'forward count))

(evil-define-motion evil-ex-search-backward (count)
  "Starts a forward search."
  :jump t
  :repeat evil-repeat-ex-search
  (evil-ex-start-search 'backward count))

(evil-define-motion evil-ex-search-symbol-forward (count)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-start-symbol-search nil 'forward count))

(evil-define-motion evil-ex-search-symbol-backward (count)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-start-symbol-search nil 'backward count))

(evil-define-motion evil-ex-search-unbounded-symbol-forward (count)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-start-symbol-search t 'forward count))

(evil-define-motion evil-ex-search-unbounded-symbol-backward (count)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-start-symbol-search t 'backward count))

(evil-define-operator evil-ex-substitute
  (beg end pattern replacement flags)
  "The Ex substitute command.
\[BEG,END]substitute/PATTERN/REPLACEMENT/FLAGS"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><s/>")
  (evil-ex-nohighlight)
  (unless pattern
    (error "No pattern given"))
  (setq replacement (or replacement ""))
  (setq evil-ex-last-was-search nil)
  (let* ((flags (append flags nil))
         (confirm (memq ?c flags))
         (case-fold-search (evil-ex-pattern-ignore-case pattern))
         (case-replace case-fold-search)
         (evil-ex-substitute-regex (evil-ex-pattern-regex pattern)))
    (setq evil-ex-substitute-pattern pattern
          evil-ex-substitute-replacement replacement
          evil-ex-substitute-flags flags
          isearch-string evil-ex-substitute-regex)
    (isearch-update-ring evil-ex-substitute-regex t)
    (if (evil-ex-pattern-whole-line pattern)
        ;; this one is easy, just use the built-in function
        (perform-replace evil-ex-substitute-regex
                         evil-ex-substitute-replacement
                         confirm t nil nil nil
                         beg
                         (if (and (> end (point-min))
                                  (= (char-after (1- end)) ?\n))
                             (1- end)
                           end))
      (let ((evil-ex-substitute-nreplaced 0)
            (evil-ex-substitute-last-point (point))
            markers
            transient-mark-mode)
        (save-excursion
          (goto-char beg)
          (beginning-of-line)
          (while (< (point) end)
            (push (move-marker (make-marker) (point)) markers)
            (forward-line)))
        (setq markers (nreverse markers))
        (if confirm
            (let ((evil-ex-substitute-overlay
                   (make-overlay (point) (point)))
                  (evil-ex-substitute-hl
                   (evil-ex-make-hl 'evil-ex-substitute)))
              (evil-ex-hl-change 'evil-ex-substitute pattern)
              (unwind-protect
                  ;; this one is more difficult: we have to do
                  ;; the highlighting and querying on our own
                  (progn
                    (overlay-put evil-ex-substitute-overlay
                                 'face 'isearch)
                    (overlay-put evil-ex-substitute-overlay
                                 'priority 1001)
                    (map-y-or-n-p
                     #'(lambda (x)
                         (set-match-data x)
                         (move-overlay evil-ex-substitute-overlay
                                       (match-beginning 0)
                                       (match-end 0))
                         (format "Query replacing %s with %s: "
                                 (match-string 0)
                                 (evil-match-substitute-replacement
                                  evil-ex-substitute-replacement
                                  (not case-replace))))
                     #'(lambda (x)
                         (set-match-data x)
                         (evil-replace-match evil-ex-substitute-replacement
                                             (not case-replace))
                         (setq evil-ex-substitute-last-point (point))
                         (setq evil-ex-substitute-nreplaced
                               (1+ evil-ex-substitute-nreplaced))
                         (evil-ex-hl-set-region 'evil-ex-substitute
                                                (save-excursion
                                                  (forward-line)
                                                  (point))
                                                (evil-ex-hl-get-max
                                                 'evil-ex-substitute)))
                     #'(lambda ()
                         (catch 'found
                           (while markers
                             (let ((m (pop markers)))
                               (goto-char m)
                               (move-marker m nil))
                             (when (re-search-forward evil-ex-substitute-regex
                                                      (line-end-position) t nil)
                               (goto-char (match-beginning 0))
                               (throw 'found (match-data))))))))
                (evil-ex-delete-hl 'evil-ex-substitute)
                (delete-overlay evil-ex-substitute-overlay)))

          ;; just replace the first occurrences per line
          ;; without highlighting and asking
          (while markers
            (let ((m (pop markers)))
              (goto-char m)
              (move-marker m nil))
            (when (re-search-forward evil-ex-substitute-regex
                                     (line-end-position) t nil)
              (setq evil-ex-substitute-nreplaced
                    (1+ evil-ex-substitute-nreplaced))
              (evil-replace-match evil-ex-substitute-replacement
                                  (not case-replace))
              (setq evil-ex-substitute-last-point (point)))))

        (while markers (move-marker (pop markers) nil))
        (goto-char evil-ex-substitute-last-point)

        (message "Replaced %d occurrence%s"
                 evil-ex-substitute-nreplaced
                 (if (/= evil-ex-substitute-nreplaced 1) "s" ""))))
    (evil-first-non-blank)))

(evil-define-operator evil-ex-repeat-substitute
  (beg end flags)
  "Repeat last substitute command.
This is the same as :s//~/"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><a>")
  (apply #'evil-ex-substitute beg end
         (evil-ex-get-substitute-info (concat "//~/" flags))))

(evil-define-operator evil-ex-repeat-substitute-with-flags
  (beg end flags)
  "Repeat last substitute command with last flags.
This is the same as :s//~/&"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><a>")
  (apply #'evil-ex-substitute beg end
         (evil-ex-get-substitute-info (concat "//~/&" flags))))

(evil-define-operator evil-ex-repeat-substitute-with-search
  (beg end flags)
  "Repeat last substitute command with last search pattern.
This is the same as :s//~/r"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><a>")
  (apply #'evil-ex-substitute beg end
         (evil-ex-get-substitute-info (concat "//~/r" flags))))

(evil-define-operator evil-ex-repeat-substitute-with-search-and-flags
  (beg end flags)
  "Repeat last substitute command with last search pattern and last flags.
This is the same as :s//~/&r"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><a>")
  (apply #'evil-ex-substitute beg end
         (evil-ex-get-substitute-info (concat "//~/&r" flags))))

(evil-define-operator evil-ex-repeat-global-substitute ()
  "Repeat last substitute command on the whole buffer.
This is the same as :%s//~/&"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive)
  (apply #'evil-ex-substitute (point-min) (point-max)
         (evil-ex-get-substitute-info (concat "//~/&"))))

(evil-define-operator evil-ex-global
  (beg end pattern command &optional invert)
  "The Ex global command.
\[BEG,END]global[!]/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (unless pattern
    (error "No pattern given"))
  (unless command
    (error "No command given"))
  (evil-with-single-undo
    (let ((case-fold-search
           (eq (evil-ex-regex-case pattern 'smart) 'insensitive))
          match markers)
      (when (and pattern command)
        (setq isearch-string pattern)
        (isearch-update-ring pattern t)
        (goto-char beg)
        (evil-move-beginning-of-line)
        (while (< (point) end)
          (setq match (re-search-forward pattern (line-end-position) t))
          (when (or (and match (not invert))
                    (and invert (not match)))
            (push (move-marker (make-marker)
                               (or (and match (match-beginning 0))
                                   (line-beginning-position)))
                  markers))
          (forward-line))
        (setq markers (nreverse markers))
        (unwind-protect
            (dolist (marker markers)
              (goto-char marker)
              (evil-ex-eval command))
          ;; ensure that all markers are deleted afterwards,
          ;; even in the event of failure
          (dolist (marker markers)
            (set-marker marker nil)))))))

(evil-define-operator evil-ex-global-inverted
  (beg end pattern command &optional invert)
  "The Ex vglobal command.
\[BEG,END]vglobal/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (evil-ex-global beg end pattern command (not invert)))

(evil-define-operator evil-ex-normal (beg end commands)
  "The Ex normal command.
Execute the argument as normal command on each line in the
range. The given argument is passed straight to
`execute-kbd-macro'.  The default is the current line."
  :motion evil-line
  (interactive "<r><a>")
  (evil-with-single-undo
    (let (markers evil-ex-current-buffer prefix-arg current-prefix-arg)
      (goto-char beg)
      (while
          (and (< (point) end)
               (progn
                 (push (move-marker (make-marker) (line-beginning-position))
                       markers)
                 (and (= (forward-line) 0) (bolp)))))
      (setq markers (nreverse markers))
      (deactivate-mark)
      (evil-force-normal-state)
      ;; replace ^[ by escape
      (setq commands
            (vconcat
             (mapcar #'(lambda (ch) (if (equal ch ?) 'escape ch))
                     (append commands nil))))
      (dolist (marker markers)
        (goto-char marker)
        (condition-case nil
            (execute-kbd-macro commands)
          (error nil))
        (evil-force-normal-state)
        (set-marker marker nil)))))

(evil-define-command evil-goto-char (position)
  "Go to POSITION in the buffer.
Default position is the beginning of the buffer."
  (interactive "p")
  (let ((position (evil-normalize-position
                   (or position (point-min)))))
    (goto-char position)))

(evil-define-operator evil-ex-line-number (beg end)
  "Print the last line number."
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r>")
  (message "%d" (count-lines (point-min) end)))

(evil-define-command evil-show-file-info ()
  "Shows basic file information."
  (let* ((nlines   (count-lines (point-min) (point-max)))
         (curr     (line-number-at-pos (point)))
         (perc     (* (/ (float curr) (float nlines)) 100.0))
         (file     (buffer-file-name (buffer-base-buffer)))
         (writable (and file (file-writable-p file)))
         (readonly (if (and file (not writable)) "[readonly] " "")))
    (if file
        (message "\"%s\" %d %slines --%d%%--" file nlines readonly perc)
      (message "%d lines --%d%%--" nlines perc))))

;;; Window navigation

(defun evil-resize-window (new-size &optional horizontal)
  "Set the current window's width or height to NEW-SIZE.
If HORIZONTAL is non-nil the width of the window is changed,
otherwise its height is changed."
  (let ((count (- new-size (if horizontal (window-width) (window-height)))))
    (if (>= emacs-major-version 24)
        (enlarge-window count horizontal)
      (let ((wincfg (current-window-configuration))
            (nwins (length (window-list)))
            (inhibit-redisplay t))
        (catch 'done
          (save-window-excursion
            (while (not (zerop count))
              (if (> count 0)
                  (progn
                    (enlarge-window 1 horizontal)
                    (setq count (1- count)))
                (progn
                  (shrink-window 1 horizontal)
                  (setq count (1+ count))))
              (if (= nwins (length (window-list)))
                  (setq wincfg (current-window-configuration))
                (throw 'done t)))))
        (set-window-configuration wincfg)))))

(defun evil-get-buffer-tree (wintree)
  "Extracts the buffer tree from a given window tree WINTREE."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'evil-get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))

(defun evil-restore-window-tree (win tree)
  "Restore the given buffer-tree layout as subwindows of WIN.
TREE is the tree layout to be restored."
  (cond
   ((and (consp tree) (cddr tree))
    (let ((newwin (split-window win nil (not (car tree)))))
      (evil-restore-window-tree win (cadr tree))
      (evil-restore-window-tree newwin (cons (car tree) (cddr tree)))))
   ((consp tree)
    (set-window-buffer win (cadr tree)))
   (t
    (set-window-buffer win tree))))

(evil-define-command evil-window-split (&optional count file)
  "Splits the current window horizontally, COUNT lines height,
editing a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (let ((new-win (split-window (selected-window) count)))
    (when file
      (evil-edit file))))

(evil-define-command evil-window-vsplit (&optional count file)
  "Splits the current window vertically, COUNT columns width,
editing a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (let ((new-win (split-window (selected-window) count t)))
    (when file
      (evil-edit file))))

(evil-define-command evil-split-buffer (buffer)
  "Splits window and switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (evil-window-split)
  (evil-buffer buffer))

(evil-define-command evil-split-next-buffer (&optional count)
  "Splits the window and goes to the COUNT-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-next-buffer count))

(evil-define-command evil-split-prev-buffer (&optional count)
  "Splits window and goes to the COUNT-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-prev-buffer count))

(evil-define-command evil-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-left)))

(evil-define-command evil-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-right)))

(evil-define-command evil-window-up (count)
  "Move the cursor to new COUNT-th window above the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-up)))

(evil-define-command evil-window-down (count)
  "Move the cursor to new COUNT-th window below the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-down)))

(evil-define-command evil-window-bottom-right ()
  "Move the cursor to bottom-right window."
  :repeat nil
  (while (let (success)
           (condition-case nil
               (progn
                 (windmove-right)
                 (setq success t))
             (error nil))
           (condition-case nil
               (progn
                 (windmove-down)
                 (setq success t))
             (error nil))
           success)))

(evil-define-command evil-window-top-left ()
  "Move the cursor to top-left window."
  :repeat nil
  (while (let (success)
           (condition-case nil
               (progn
                 (windmove-left)
                 (setq success t))
             (error nil))
           (condition-case nil
               (progn
                 (windmove-up)
                 (setq success t))
             (error nil))
           success)))

(evil-define-command evil-window-mru ()
  "Move the cursor to the previous (last accessed) buffer in another window.
More precisely, it selectes the most recently used buffer that is
shown in some other window, preferably of the current frame, and
is different from the current one."
  :repeat nil
  (catch 'done
    (dolist (buf (buffer-list (selected-frame)))
      (let ((win (get-buffer-window buf)))
        (when (and (not (eq buf (current-buffer)))
                   win
                   (not (eq win (selected-window))))
          (select-window win)
          (throw 'done nil))))))

(evil-define-command evil-window-next (count)
  "Move the cursor to the next window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "P")
  (if (not count)
      (select-window (next-window))
    (evil-window-top-left)
    (other-window (1- count))))

(evil-define-command evil-window-prev (count)
  "Move the cursor to the previous window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "P")
  (if (not count)
      (select-window (previous-window))
    (evil-window-top-left)
    (other-window (1- count))))

(evil-define-command evil-window-new (count file)
  "Splits the current window horizontally
and opens a new buffer or edits a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count)
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

(evil-define-command evil-window-vnew (count file)
  "Splits the current window vertically
and opens a new buffer name or edits a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count t)
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

(evil-define-command evil-window-increase-height (count)
  "Increase current window height by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (+ (window-height) count)))

(evil-define-command evil-window-decrease-height (count)
  "Decrease current window height by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (- (window-height) count)))

(evil-define-command evil-window-increase-width (count)
  "Increase current window width by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (+ (window-width) count) t))

(evil-define-command evil-window-decrease-width (count)
  "Decrease current window width by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (- (window-width) count) t))

(evil-define-command evil-window-set-height (count)
  "Sets the height of the current window to COUNT."
  :repeat nil
  (interactive "P")
  (evil-resize-window (or count (frame-height)) nil))

(evil-define-command evil-window-set-width (count)
  "Sets the width of the current window to COUNT."
  :repeat nil
  (interactive "P")
  (evil-resize-window (or count (frame-width)) t))

(evil-define-command evil-window-rotate-upwards ()
  "Rotates the windows according to the currenty cyclic ordering."
  :repeat nil
  (let ((wlist (window-list))
        (blist (mapcar #'(lambda (w) (window-buffer w))
                       (window-list))))
    (setq blist (append (cdr blist) (list (car blist))))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window (car (last (window-list))))))

(evil-define-command evil-window-rotate-downwards ()
  "Rotates the windows according to the currenty cyclic ordering."
  :repeat nil
  (let ((wlist (window-list))
        (blist (mapcar #'(lambda (w) (window-buffer w))
                       (window-list))))
    (setq blist (append (last blist) blist))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window (cadr (window-list)))))

(evil-define-command evil-window-move-very-top ()
  "Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-far-left ()
  "Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window-horizontally)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-far-right ()
  "Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window-horizontally)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-very-bottom ()
  "Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;; Mouse handling

;; Large parts of this code are taken from mouse.el which is
;; distributed with GNU Emacs
(defun evil-mouse-drag-region (start-event)
  "Set the region to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.

If the click is in the echo area, display the `*Messages*' buffer.

START-EVENT should be the event that started the drag."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (evil-mouse-drag-track start-event t))
(evil-set-command-property 'evil-mouse-drag-region :keep-visual t)

(defun evil-mouse-drag-track (start-event &optional
                                          do-mouse-drag-region-post-process)
  "Track mouse drags by highlighting area between point and cursor.
The region will be defined with mark and point.
DO-MOUSE-DRAG-REGION-POST-PROCESS should only be used by
`mouse-drag-region'."
  (mouse-minibuffer-check start-event)
  (setq mouse-selection-click-count-buffer (current-buffer))
  (deactivate-mark)
  (let* ((scroll-margin 0) ; Avoid margin scrolling (Bug#9541).
         (original-window (selected-window))
         ;; We've recorded what we needed from the current buffer and
         ;; window, now let's jump to the place of the event, where things
         ;; are happening.
         (_ (mouse-set-point start-event))
         (echo-keystrokes 0)
         (start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-window-start (window-start start-window))
         (start-hscroll (window-hscroll start-window))
         (bounds (window-edges start-window))
         (make-cursor-line-fully-visible nil)
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds))))
         (on-link (and mouse-1-click-follows-link
                       (or mouse-1-click-in-non-selected-windows
                           (eq start-window original-window))
                       ;; Use start-point before the intangibility
                       ;; treatment, in case we click on a link inside an
                       ;; intangible text.
                       (mouse-on-link-p start-posn)))
         (click-count (1- (event-click-count start-event)))
         (remap-double-click (and on-link
                                  (eq mouse-1-click-follows-link 'double)
                                  (= click-count 1)))
         ;; Suppress automatic hscrolling, because that is a nuisance
         ;; when setting point near the right fringe (but see below).
         (auto-hscroll-mode-saved auto-hscroll-mode)
         (auto-hscroll-mode nil)
         event end end-point)

    (setq mouse-selection-click-count click-count)
    ;; In case the down click is in the middle of some intangible text,
    ;; use the end of that text, and put it in START-POINT.
    (if (< (point) start-point)
        (goto-char start-point))
    (setq start-point (point))
    (if remap-double-click
        (setq click-count 0))

    (setq click-count (mod click-count 4))

    ;; activate correct visual state
    (let ((range (evil-mouse-start-end start-point start-point click-count)))
      (set-mark (nth 0 range))
      (goto-char (nth 1 range)))

    (cond
     ((= click-count 0)
      (when (evil-visual-state-p) (evil-exit-visual-state)))
     ((= click-count 1)
      (evil-visual-char)
      (evil-visual-post-command))
     ((= click-count 2)
      (evil-visual-line)
      (evil-visual-post-command))
     ((= click-count 3)
      (evil-visual-block)
      (evil-visual-post-command)))

    ;; Track the mouse until we get a non-movement event.
    (track-mouse
      (while (progn
               (setq event (read-event))
               (or (mouse-movement-p event)
                   (memq (car-safe event) '(switch-frame select-window))))
        (unless (evil-visual-state-p)
          (cond
           ((= click-count 0) (evil-visual-char))
           ((= click-count 1) (evil-visual-char))
           ((= click-count 2) (evil-visual-line))
           ((= click-count 3) (evil-visual-block))))

        (evil-visual-pre-command)
        (unless (memq (car-safe event) '(switch-frame select-window))
          ;; Automatic hscrolling did not occur during the call to
          ;; `read-event'; but if the user subsequently drags the
          ;; mouse, go ahead and hscroll.
          (let ((auto-hscroll-mode auto-hscroll-mode-saved))
            (redisplay))
          (setq end (event-end event)
                end-point (posn-point end))
          (if (and (eq (posn-window end) start-window)
                   (integer-or-marker-p end-point))
              (evil-mouse--drag-set-mark-and-point start-point
                                                   end-point click-count)
            (let ((mouse-row (cdr (cdr (mouse-position)))))
              (cond
               ((null mouse-row))
               ((< mouse-row top)
                (mouse-scroll-subr start-window (- mouse-row top)
                                   nil start-point))
               ((>= mouse-row bottom)
                (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                   nil start-point))))))
        (evil-visual-post-command)))

    ;; Handle the terminating event if possible.
    (when (consp event)
      ;; Ensure that point is on the end of the last event.
      (when (and (setq end-point (posn-point (event-end event)))
                 (eq (posn-window end) start-window)
                 (integer-or-marker-p end-point)
                 (/= start-point end-point))
        (evil-mouse--drag-set-mark-and-point start-point
                                             end-point click-count))

      ;; Find its binding.
      (let* ((fun (key-binding (vector (car event))))
             (do-multi-click (and (> (event-click-count event) 0)
                                  (functionp fun)
                                  (not (memq fun '(mouse-set-point
                                                   mouse-set-region))))))
        (if (and (or (/= (mark) (point))
                     (= click-count 1) ; word selection
                     (and (memq (evil-visual-type) '(line block))))
                 (not do-multi-click))

            ;; If point has moved, finish the drag.
            (let (last-command this-command)
              (and mouse-drag-copy-region
                   do-mouse-drag-region-post-process
                   (let (deactivate-mark)
                     (evil-visual-expand-region)
                     (copy-region-as-kill (mark) (point))
                     (evil-visual-contract-region))))

          ;; If point hasn't moved, run the binding of the
          ;; terminating up-event.
          (if do-multi-click
              (goto-char start-point)
            (deactivate-mark))
          (when (and (functionp fun)
                     (= start-hscroll (window-hscroll start-window))
                     ;; Don't run the up-event handler if the window
                     ;; start changed in a redisplay after the
                     ;; mouse-set-point for the down-mouse event at
                     ;; the beginning of this function.  When the
                     ;; window start has changed, the up-mouse event
                     ;; contains a different position due to the new
                     ;; window contents, and point is set again.
                     (or end-point
                         (= (window-start start-window)
                            start-window-start)))
            (when (and on-link
                       (= start-point (point))
                       (evil-mouse--remap-link-click-p start-event event))
              ;; If we rebind to mouse-2, reselect previous selected
              ;; window, so that the mouse-2 event runs in the same
              ;; situation as if user had clicked it directly.  Fixes
              ;; the bug reported by juri@jurta.org on 2005-12-27.
              (if (or (vectorp on-link) (stringp on-link))
                  (setq event (aref on-link 0))
                (select-window original-window)
                (setcar event 'mouse-2)
                ;; If this mouse click has never been done by the
                ;; user, it doesn't have the necessary property to be
                ;; interpreted correctly.
                (put 'mouse-2 'event-kind 'mouse-click)))
            (push event unread-command-events)))))))

;; This function is a plain copy of `mouse--drag-set-mark-and-point',
;; which is only available in Emacs 24
(defun evil-mouse--drag-set-mark-and-point (start click click-count)
  (let* ((range (evil-mouse-start-end start click click-count))
         (beg (nth 0 range))
         (end (nth 1 range)))
    (cond ((eq (mark) beg)
           (goto-char end))
          ((eq (mark) end)
           (goto-char beg))
          ((< click (mark))
           (set-mark end)
           (goto-char beg))
          (t
           (set-mark beg)
           (goto-char end)))))

;; This function is a plain copy of `mouse--remap-link-click-p',
;; which is only available in Emacs 23
(defun evil-mouse--remap-link-click-p (start-event end-event)
  (or (and (eq mouse-1-click-follows-link 'double)
           (= (event-click-count start-event) 2))
      (and
       (not (eq mouse-1-click-follows-link 'double))
       (= (event-click-count start-event) 1)
       (= (event-click-count end-event) 1)
       (or (not (integerp mouse-1-click-follows-link))
           (let ((t0 (posn-timestamp (event-start start-event)))
                 (t1 (posn-timestamp (event-end   end-event))))
             (and (integerp t0) (integerp t1)
                  (if (> mouse-1-click-follows-link 0)
                      (<= (- t1 t0) mouse-1-click-follows-link)
                    (< (- t0 t1) mouse-1-click-follows-link))))))))

(defun evil-mouse-start-end (start end mode)
  "Return a list of region bounds based on START and END according to MODE.
If MODE is not 1 then set point to (min START END), mark to (max
START END).  If MODE is 1 then set point to start of word at (min
START END), mark to end of word at (max START END)."
  (evil-sort start end)
  (setq mode (mod mode 4))
  (if (/= mode 1) (list start end)
    (list
     (save-excursion
       (goto-char (min (point-max) (1+ start)))
       (if (zerop (funcall evil-mouse-word -1))
           (let ((bpnt (point)))
             (funcall evil-mouse-word +1)
             (if (> (point) start) bpnt (point)))
         (point-min)))
     (save-excursion
       (goto-char end)
       (1-
        (if (zerop (funcall evil-mouse-word +1))
            (let ((epnt (point)))
              (funcall evil-mouse-word -1)
              (if (<= (point) end) epnt (point)))
          (point-max)))))))

;;; State switching

(evil-define-command evil-exit-emacs-state (&optional buffer message)
  "Exit Emacs state.
Changes the state to the previous state, or to Normal state
if the previous state was Emacs state."
  :keep-visual t
  :suppress-operator t
  (interactive '(nil t))
  (with-current-buffer (or buffer (current-buffer))
    (when (evil-emacs-state-p)
      (evil-change-to-previous-state buffer message)
      (when (evil-emacs-state-p)
        (evil-normal-state (and message 1))))))

(defun evil-execute-in-normal-state ()
  "Execute the next command in Normal state."
  (interactive)
  (evil-delay '(not (memq this-command
                          '(evil-execute-in-normal-state
                            evil-use-register
                            digit-argument
                            negative-argument
                            universal-argument
                            universal-argument-minus
                            universal-argument-more
                            universal-argument-other-key)))
      `(progn
         (evil-change-to-previous-state)
         (setq evil-move-cursor-back ',evil-move-cursor-back))
    'post-command-hook)
  (setq evil-move-cursor-back nil)
  (evil-normal-state)
  (evil-echo "Switched to Normal state for the next command ..."))

(defun evil-stop-execute-in-emacs-state ()
  (when (and (not (eq this-command #'evil-execute-in-emacs-state))
             (not (minibufferp)))
    (remove-hook 'post-command-hook 'evil-stop-execute-in-emacs-state)
    (when (buffer-live-p evil-execute-in-emacs-state-buffer)
      (with-current-buffer evil-execute-in-emacs-state-buffer
        (if (and (eq evil-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (evil-change-to-previous-state)
              (evil-exit-visual-state))
          (evil-change-to-previous-state))))
    (setq evil-execute-in-emacs-state-buffer nil)))

(evil-define-command evil-execute-in-emacs-state ()
  "Execute the next command in Emacs state."
  (add-hook 'post-command-hook #'evil-stop-execute-in-emacs-state t)
  (setq evil-execute-in-emacs-state-buffer (current-buffer))
  (cond
   ((evil-visual-state-p)
    (let ((mrk (mark))
          (pnt (point)))
      (evil-emacs-state)
      (set-mark mrk)
      (goto-char pnt)))
   (t
    (evil-emacs-state)))
  (evil-echo "Switched to Emacs state for the next command ..."))

(defun evil-exit-visual-and-repeat (event)
  "Exit insert state and repeat event.
This special command should be used if some command called from
visual state should actually be called in normal-state.  The main
reason for doing this is that the repeat system should *not*
record the visual state information for some command.  This
command should be bound to exactly the same event in visual state
as the original command is bound in normal state.  EVENT is the
event that triggered the execution of this command."
  (interactive "e")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (push event unread-command-events)))
(evil-declare-ignore-repeat 'evil-exit-visual-and-repeat)

(provide 'evil-commands)

;;; evil-commands.el ends here

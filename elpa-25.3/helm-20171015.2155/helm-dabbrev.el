;;; helm-dabbrev.el --- Helm implementation of dabbrev. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(require 'helm)
(require 'helm-lib)
(require 'helm-help)
(require 'helm-elisp) ; For show-completion.

(defgroup helm-dabbrev nil
  "Dabbrev related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-dabbrev-always-search-all t
  "Always search in all buffers when non--nil.
Note that even if nil, a search in all buffers
will occur if the length of candidates is <= than
`helm-dabbrev-max-length-result'."
  :group 'helm-dabbrev
  :type 'boolean)

(defcustom helm-dabbrev-max-length-result 20
  "Max length of candidates before searching in all buffers.
If number of candidates found in current-buffer is <= to this,
search in all buffers.
Have no effect when `helm-dabbrev-always-search-all' is non--nil."
  :group 'helm-dabbrev
  :type 'integer)

(defcustom helm-dabbrev-candidates-number-limit 600
  "Maximum number of candidates to collect before stopping.
Higher this number is slower the computation of candidates will be."
  :group 'helm-dabbrev
  :type 'integer)

(defcustom helm-dabbrev-ignored-buffers-regexps
  '("\\*helm" "\\*Messages" "\\*Echo Area" "\\*Buffer List")
  "List of regexps matching names of buffers that helm-dabbrev should not check."
  :group 'helm-dabbrev
  :type '(repeat regexp))

(defcustom helm-dabbrev-related-buffer-fn #'helm-dabbrev--same-major-mode-p
  "A function that decide if a buffer to search in is related to `current-buffer'.
This is actually determined by comparing `major-mode' of the buffer to search
and the `current-buffer'.
The function take one arg, the buffer which is current, look at
`helm-dabbrev--same-major-mode-p' for example.

When nil all buffers are considered related to `current-buffer'."
  :group 'helm-dabbrev
  :type 'function)

(defcustom helm-dabbrev-major-mode-assoc nil 
  "Major mode association alist.
This allow helm-dabbrev searching in buffers with the associated `major-mode'.
e.g \(emacs-lisp-mode . lisp-interaction-mode\)
will allow searching in the lisp-interaction-mode buffer when `current-buffer'
is an `emacs-lisp-mode' buffer and vice versa i.e
no need to provide \(lisp-interaction-mode . emacs-lisp-mode\) association.

When nil check is the searched buffer have same `major-mode'
than the `current-buffer'.
This have no effect when `helm-dabbrev-related-buffer-fn' is nil or of course
bound to a function that doesn't handle this var."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'helm-dabbrev)

(defcustom helm-dabbrev-lineno-around 30
  "Search first in this number of lines before an after point."
  :group 'helm-dabbrev
  :type 'integer)

(defcustom helm-dabbrev-cycle-threshold 5
  "Number of time helm-dabbrev cycle before displaying helm completion.
When nil or 0 disable cycling."
  :group 'helm-dabbrev
  :type '(choice (const :tag "Cycling disabled" nil) integer))

(defcustom helm-dabbrev-case-fold-search 'smart
  "Set `case-fold-search' in `helm-dabbrev'.
Same as `helm-case-fold-search' but for `helm-dabbrev'.
Note that this is not affecting searching in helm buffer,
but the initial search for all candidates in buffer(s)."
  :group 'helm-dabbrev
  :type '(choice (const :tag "Ignore case" t)
          (const :tag "Respect case" nil)
          (other :tag "Smart" 'smart)))

;; Check for beginning of line should happen last (^\n\\|^). 
(defvar helm-dabbrev-separator-regexp "\\s-\\|\t\\|[(\[\{\"'`=<$;,@.#+]\\|\\s\\\\|^\n\\|^"
  "Regexp matching the start of a dabbrev candidate.")
(defvaralias 'helm-dabbrev--regexp 'helm-dabbrev-separator-regexp)
(make-obsolete-variable 'helm-dabbrev--regexp 'helm-dabbrev-separator-regexp "2.8.3")


(defvar helm-dabbrev-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-/") 'helm-next-line)
    (define-key map (kbd "M-:") 'helm-previous-line)
    map))

;; Internal
(defvar helm-dabbrev--exclude-current-buffer-flag nil)
(defvar helm-dabbrev--cache nil)
(defvar helm-dabbrev--data nil)
(cl-defstruct helm-dabbrev-info dabbrev limits iterator)


(defun helm-dabbrev--buffer-list ()
  (cl-loop with lst = (buffer-list)
        for buf in (if helm-dabbrev--exclude-current-buffer-flag
                       (cdr lst) lst)
        unless (cl-loop for r in helm-dabbrev-ignored-buffers-regexps
                     thereis (string-match r (buffer-name buf)))
        collect buf))

(defun helm-dabbrev--same-major-mode-p (start-buffer)
  "Decide if current-buffer is related to START-BUFFER."
  (helm-same-major-mode-p start-buffer helm-dabbrev-major-mode-assoc))

(defun helm-dabbrev--collect (str limit ignore-case all)
  (let* ((case-fold-search ignore-case)
         (buffer1 (current-buffer))     ; start buffer.
         (minibuf (minibufferp buffer1))
         result pos-before pos-after
         (search-and-store
          (lambda (pattern direction)
            (while (and (<= (length result) limit)
                        (cl-case direction
                          (1   (search-forward pattern nil t))
                          (-1  (search-backward pattern nil t))
                          (2   (let ((pos
                                      (save-excursion
                                        (forward-line
                                         helm-dabbrev-lineno-around)
                                        (point))))
                                 (setq pos-after pos)
                                 (search-forward pattern pos t)))
                          (-2  (let ((pos
                                      (save-excursion
                                        (forward-line
                                         (- helm-dabbrev-lineno-around))
                                        (point))))
                                 (setq pos-before pos)
                                 (search-backward pattern pos t)))))
              (let* ((pbeg (match-beginning 0))
                     (replace-regexp (concat "\\(" helm-dabbrev-separator-regexp
                                             "\\)\\'"))
                     (match-word (helm-dabbrev--search
                                  pattern pbeg replace-regexp)))
                (when (and match-word (not (member match-word result)))
                  (push match-word result)))))))
    (cl-loop for buf in (if all (helm-dabbrev--buffer-list)
                          (list (current-buffer)))
             do (with-current-buffer buf
                  (when (or minibuf ; check against all buffers when in minibuffer.
                            (if helm-dabbrev-related-buffer-fn
                                (funcall helm-dabbrev-related-buffer-fn buffer1)
                              t))
                    (save-excursion
                      ;; Start searching before thing before point.
                      (goto-char (- (point) (length str)))
                      ;; Search the last 30 lines before point.
                      (funcall search-and-store str -2)) ; store pos [1]
                    (save-excursion
                      ;; Search the next 30 lines after point.
                      (funcall search-and-store str 2)) ; store pos [2]
                    (save-excursion
                      ;; Search all before point.
                      ;; If limit is reached in previous call of
                      ;; search-and-store pos-before is never set and
                      ;; goto-char will fail, so check it.
                      (when pos-before
                        (goto-char pos-before) ; start from [1]
                        (funcall search-and-store str -1)))
                    (save-excursion
                      ;; Search all after point.
                      ;; Same comment as above for pos-after.
                      (when pos-after
                        (goto-char pos-after) ; start from [2]
                        (funcall search-and-store str 1)))))
             when (>= (length result) limit) return (nreverse result)
             finally return (nreverse result))))

(defun helm-dabbrev--search (pattern beg sep-regexp)
  "Search word or symbol at point matching PATTERN.
Argument BEG is corresponding to the previous match-beginning search.
The search starts at (1- BEG) with a regexp starting with
`helm-dabbrev-separator-regexp' followed by PATTERN followed by a
regexp matching syntactically any word or symbol.
The possible false positives matching SEP-REGEXP at end are finally
removed."
  (let ((eol (point-at-eol))) 
    (save-excursion
      (goto-char (1- beg))
      (when (re-search-forward
             (concat "\\("
                     helm-dabbrev-separator-regexp
                     "\\)"
                     "\\(?99:\\("
                     (regexp-quote pattern)
                     "\\(\\sw\\|\\s_\\)+\\)\\)")
             eol t)
        (replace-regexp-in-string
         sep-regexp ""
         (match-string-no-properties 99))))))

(defun helm-dabbrev--get-candidates (abbrev)
  (cl-assert abbrev nil "[No Match]")
  (with-current-buffer (current-buffer)
    (let* ((dabbrev-get (lambda (str all-bufs)
                            (helm-dabbrev--collect
                             str helm-dabbrev-candidates-number-limit
                             (cl-case helm-dabbrev-case-fold-search
                               (smart (helm-set-case-fold-search-1 abbrev))
                               (t helm-dabbrev-case-fold-search))
                             all-bufs)))
           (lst (funcall dabbrev-get abbrev helm-dabbrev-always-search-all)))
      (if (and (not helm-dabbrev-always-search-all)
               (<= (length lst) helm-dabbrev-max-length-result))
          ;; Search all but don't recompute current-buffer.
          (let ((helm-dabbrev--exclude-current-buffer-flag t))
            (append lst (funcall dabbrev-get abbrev 'all-bufs)))
        lst))))

(defun helm-dabbrev-default-action (candidate)
  (with-helm-current-buffer
    (let* ((limits (helm-bounds-of-thing-before-point
                    helm-dabbrev-separator-regexp))
           (beg (car limits))
           (end (point)))
      (run-with-timer
       0.01 nil
       'helm-insert-completion-at-point
       beg end candidate))))

;;;###autoload
(defun helm-dabbrev ()
  "Preconfigured helm for dynamic abbreviations."
  (interactive)
  (let ((dabbrev (helm-thing-before-point nil helm-dabbrev-separator-regexp))
        (limits (helm-bounds-of-thing-before-point helm-dabbrev-separator-regexp))
        (enable-recursive-minibuffers t)
        (cycling-disabled-p (or (null helm-dabbrev-cycle-threshold)
                                (zerop helm-dabbrev-cycle-threshold)))
        (helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate
         (lambda ()
             (message "[Helm-dabbrev: No expansion found]"))))
    (cl-assert (and (stringp dabbrev) (not (string= dabbrev "")))
               nil "[Helm-dabbrev: Nothing found before point]")
    (when (and
           ;; have been called at least once.
           (helm-dabbrev-info-p helm-dabbrev--data)
           ;; But user have moved with some other command
           ;; in the meaning time.
           (not (eq last-command 'helm-dabbrev)))
      (setq helm-dabbrev--data nil))
    (when cycling-disabled-p
      (setq helm-dabbrev--cache (helm-dabbrev--get-candidates dabbrev)))
    (unless (or cycling-disabled-p
                (helm-dabbrev-info-p helm-dabbrev--data))
      (setq helm-dabbrev--cache (helm-dabbrev--get-candidates dabbrev))
      (setq helm-dabbrev--data
            (make-helm-dabbrev-info
             :dabbrev dabbrev
             :limits limits
             :iterator
             (helm-iter-list
              (cl-loop for i in helm-dabbrev--cache
                       when (and i (string-match
                                    (concat "^" (regexp-quote dabbrev)) i))
                       collect i into selection
                       when (and selection
                                 (= (length selection)
                                    helm-dabbrev-cycle-threshold))
                       ;; When selection len reach
                       ;; `helm-dabbrev-cycle-threshold'
                       ;; return selection.
                       return selection
                       ;; selection len never reach
                       ;; `helm-dabbrev-cycle-threshold'
                       ;; return selection.
                       finally return selection)))))
    (let ((iter (and (helm-dabbrev-info-p helm-dabbrev--data)
                     (helm-dabbrev-info-iterator helm-dabbrev--data)))
          deactivate-mark)
      ;; Cycle until iterator is consumed.
      (helm-aif (and iter (helm-iter-next iter))
          (progn
            (helm-insert-completion-at-point
             (car (helm-dabbrev-info-limits helm-dabbrev--data))
             (cdr limits) it)
            ;; Move already tried candidates to end of list.
            (setq helm-dabbrev--cache (append (remove it helm-dabbrev--cache)
                                              (list it))))
        ;; If the length of candidates is only one when computed
        ;; that's mean the unique matched item have already been
        ;; inserted by the iterator, so no need to reinsert the old dabbrev,
        ;; just let helm exiting with "No expansion found".
        (let ((old-dabbrev (if (helm-dabbrev-info-p helm-dabbrev--data)
                               (helm-dabbrev-info-dabbrev helm-dabbrev--data)
                               dabbrev)))
          (unless (cdr (all-completions old-dabbrev helm-dabbrev--cache))
            (setq cycling-disabled-p t))
          ;; Iterator is now empty, reset dabbrev to initial value
          ;; and start helm completion.
          (unless cycling-disabled-p
            (setq dabbrev old-dabbrev
                  limits  (helm-dabbrev-info-limits helm-dabbrev--data))
            (setq helm-dabbrev--data nil)
            (delete-region (car limits) (point))
            (insert dabbrev))
          (with-helm-show-completion (car limits) (cdr limits)
            (helm :sources (helm-build-in-buffer-source "Dabbrev Expand"
                             :data helm-dabbrev--cache
                             :persistent-action 'ignore
                             :persistent-help "DoNothing"
                             :keymap helm-dabbrev-map
                             :action 'helm-dabbrev-default-action
                             :group 'helm-dabbrev)
                  :buffer "*helm dabbrev*"
                  :input (concat "^" dabbrev " ")
                  :resume 'noresume
                  :allow-nest t)))))))

(provide 'helm-dabbrev)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-dabbrev.el ends here

;;; helm-buffers.el --- helm support for buffers. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(require 'cl-lib)
(require 'helm)
(require 'helm-utils)
(require 'helm-elscreen)
(require 'helm-grep)
(require 'helm-regexp)

(declare-function ido-make-buffer-list "ido" (default))

(defgroup helm-buffers nil
  "Buffers related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-boring-buffer-regexp-list
  '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf")
  "The regexp list that match boring buffers.
Buffer candidates matching these regular expression will be
filtered from the list of candidates if the
`helm-skip-boring-buffers' candidate transformer is used."
  :type  '(repeat (choice regexp))
  :group 'helm-buffers)

(defcustom helm-buffers-favorite-modes '(lisp-interaction-mode
                                         emacs-lisp-mode
                                         text-mode
                                         org-mode)
  "List of preferred mode to open new buffers with."
  :type '(repeat (choice function))
  :group 'helm-buffers)

(defcustom helm-buffer-max-length 20
  "Max length of buffer names before truncate.
When disabled (nil) use the longest buffer-name length found."
  :group 'helm-buffers
  :type  '(choice (const :tag "Disabled" nil)
           (integer :tag "Length before truncate")))

(defcustom helm-buffer-details-flag t
  "Always show details in buffer list when non--nil."
  :group 'helm-buffers
  :type 'boolean)

(defcustom helm-buffers-fuzzy-matching nil
  "Fuzzy matching buffer names when non--nil.
Only buffer names are fuzzy matched when this is enabled,
`major-mode' matching is not affected by this."
  :group 'helm-buffers
  :type 'boolean)


;;; Faces
;;
;;
(defface helm-buffer-saved-out
    '((t (:foreground "red" :background "black")))
  "Face used for buffer files modified outside of emacs."
  :group 'helm-buffers)

(defface helm-buffer-not-saved
    '((t (:foreground "Indianred2")))
  "Face used for buffer files not already saved on disk."
  :group 'helm-buffers)

(defface helm-buffer-size
    '((((background dark)) :foreground "RosyBrown")
      (((background light)) :foreground "SlateGray"))
  "Face used for buffer size."
  :group 'helm-buffers)

(defface helm-buffer-process
    '((t (:foreground "Sienna3")))
  "Face used for process status in buffer."
  :group 'helm-buffers)

(defface helm-buffer-directory
    '((t (:foreground "DarkRed" :background "LightGray")))
  "Face used for directories in `helm-buffers-list'."
  :group 'helm-buffers)


;;; Buffers keymap
;;
(defvar helm-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?")     'helm-buffer-help)
    ;; No need to have separate command for grep and zgrep
    ;; as we don't use recursivity for buffers.
    ;; So use zgrep for both as it is capable to handle non--compressed files.
    (define-key map (kbd "M-g s")     'helm-buffer-run-zgrep)
    (define-key map (kbd "C-s")       'helm-buffers-run-multi-occur)
    (define-key map (kbd "C-c o")     'helm-buffer-switch-other-window)
    (define-key map (kbd "C-c C-o")   'helm-buffer-switch-other-frame)
    (define-key map (kbd "C-c =")     'helm-buffer-run-ediff)
    (define-key map (kbd "M-=")       'helm-buffer-run-ediff-merge)
    (define-key map (kbd "C-=")       'helm-buffer-diff-persistent)
    (define-key map (kbd "M-U")       'helm-buffer-revert-persistent)
    (define-key map (kbd "C-c d")     'helm-buffer-run-kill-persistent)
    (define-key map (kbd "M-D")       'helm-buffer-run-kill-buffers)
    (define-key map (kbd "C-x C-s")   'helm-buffer-save-persistent)
    (define-key map (kbd "C-M-%")     'helm-buffer-run-query-replace-regexp)
    (define-key map (kbd "M-%")       'helm-buffer-run-query-replace)
    (define-key map (kbd "M-m")       'helm-toggle-all-marks)
    (define-key map (kbd "M-a")       'helm-mark-all)
    (define-key map (kbd "C-]")       'helm-toggle-buffers-details)
    (define-key map (kbd "C-c a")     'helm-buffers-toggle-show-hidden-buffers)
    map)
  "Keymap for buffer sources in helm.")

(defvar helm-buffers-ido-virtual-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?")   'helm-buffers-ido-virtual-help)
    (define-key map (kbd "C-c o")   'helm-ff-run-switch-other-window)
    (define-key map (kbd "C-c C-o") 'helm-ff-run-switch-other-frame)
    (define-key map (kbd "M-g s")   'helm-ff-run-grep)
    (define-key map (kbd "M-g z")   'helm-ff-run-zgrep)
    (define-key map (kbd "M-D")     'helm-ff-run-delete-file)
    (define-key map (kbd "C-c C-x") 'helm-ff-run-open-file-externally)
    map))


(defvar helm-buffers-list-cache nil)
(defvar helm-buffer-max-len-mode nil)
(defvar helm-source-buffers-list
  `((name . "Buffers")
    (init . (lambda ()
              ;; Issue #51 Create the list before `helm-buffer' creation.
              (setq helm-buffers-list-cache (helm-buffer-list))
              (let ((result (cl-loop for b in helm-buffers-list-cache
                                  maximize (length b) into len-buf
                                  maximize (length (with-current-buffer b
                                                     (symbol-name major-mode)))
                                  into len-mode
                                  finally return (cons len-buf len-mode))))
                (unless helm-buffer-max-length
                  (setq helm-buffer-max-length (car result)))
                (unless helm-buffer-max-len-mode
                  ;; If a new buffer is longer that this value
                  ;; this value will be updated
                  (setq helm-buffer-max-len-mode (cdr result))))))
    (candidates . helm-buffers-list-cache)
    (no-matchplugin)
    (type . buffer)
    (match helm-buffers-list--match-fn)
    (persistent-action . helm-buffers-list-persistent-action)
    (keymap . ,helm-buffer-map)
    (volatile)
    (mode-line . helm-buffer-mode-line-string)
    (persistent-help
     . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

(defvar helm-source-buffer-not-found
  `((name . "Create buffer")
    (keymap . ,helm-map)
    (dummy)
    (action . (lambda (candidate)
                (let ((mjm (and helm-current-prefix-arg
                                (intern (helm-comp-read
                                         "Major-mode: "
                                         helm-buffers-favorite-modes))))
                      (buffer (get-buffer-create candidate)))
                  (if mjm
                      (with-current-buffer buffer (funcall mjm))
                    (set-buffer-major-mode buffer))
                  (helm-switch-to-buffer buffer))))))

(defvar ido-temp-list)
(defvar ido-ignored-list)
(defvar ido-process-ignore-lists)

(defvar helm-source-ido-virtual-buffers
  `((name . "Ido virtual buffers")
    (candidates . (lambda ()
                    (let (ido-temp-list
                          ido-ignored-list
                          (ido-process-ignore-lists t))
                      (when ido-use-virtual-buffers
                        (ido-add-virtual-buffers-to-list)
                        ido-virtual-buffers))))
    (keymap . ,helm-buffers-ido-virtual-map)
    (mode-line . helm-buffers-ido-virtual-mode-line-string)
    (action . (("Find file" . helm-find-many-files)
               ("Find file other window" . find-file-other-window)
               ("Find file other frame" . find-file-other-frame)
               ("Find file as root" . helm-find-file-as-root)
               ("Grep File(s) `C-u recurse'" . helm-find-files-grep)
               ("Zgrep File(s) `C-u Recurse'" . helm-ff-zgrep)
               ("View file" . view-file)
               ("Delete file(s)" . helm-delete-marked-files)
               ("Open file externally (C-u to choose)"
                . helm-open-file-externally)))))


(defvar ido-use-virtual-buffers)
(defun helm-buffer-list ()
  "Return the current list of buffers.
Currently visible buffers are put at the end of the list.
See `ido-make-buffer-list' for more infos."
  (require 'ido)
  (let ((ido-process-ignore-lists t)
        ido-ignored-list
        ido-use-virtual-buffers)
    (ido-make-buffer-list nil)))

(defun helm-buffer-size (buffer)
  "Return size of BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (helm-file-human-size
       (- (position-bytes (point-max))
          (position-bytes (point-min)))))))

(defun helm-buffer--show-details (buf-name prefix help-echo
                                  size mode dir face1 face2
                                  proc details)
  (append
   (list
    (concat prefix
            (propertize buf-name 'face face1
                        'help-echo help-echo)))
   (and details
        (list size mode
              (propertize
               (if proc
                   (format "(%s %s in `%s')"
                           (process-name proc)
                           (process-status proc) dir)
                 (format "(in `%s')" dir))
               'face face2)))))

(defun helm-buffer--details (buffer &optional details)
  (let* ((mode (with-current-buffer buffer (format-mode-line mode-name)))
         (buf (get-buffer buffer))
         (size (propertize (helm-buffer-size buf)
                           'face 'helm-buffer-size))
         (proc (get-buffer-process buf))
         (dir (with-current-buffer buffer (abbreviate-file-name default-directory)))
         (file-name (helm-aif (buffer-file-name buf) (abbreviate-file-name it)))
         (name (buffer-name buf))
         (name-prefix (when (file-remote-p dir)
                        (propertize "@ " 'face 'helm-ff-prefix))))
    (cond
      ( ;; A dired buffer.
       (rassoc buf dired-buffers)
       (helm-buffer--show-details
        name name-prefix dir size mode dir
        'helm-buffer-directory 'helm-buffer-process nil details))
      ;; A buffer file modified somewhere outside of emacs.=>red
      ((and file-name (file-exists-p file-name)
            (not (verify-visited-file-modtime buf)))
       (helm-buffer--show-details
        name name-prefix file-name size mode dir
        'helm-buffer-saved-out 'helm-buffer-process nil details))
      ;; A new buffer file not already saved on disk.=>indianred2
      ((and file-name (not (verify-visited-file-modtime buf)))
       (helm-buffer--show-details
        name name-prefix file-name size mode dir
        'helm-buffer-not-saved 'helm-buffer-process nil details))
      ;; A buffer file modified and not saved on disk.=>orange
      ((and file-name (buffer-modified-p buf))
       (helm-buffer--show-details
        name name-prefix file-name size mode dir
        'helm-ff-symlink 'helm-buffer-process nil details))
      ;; A buffer file not modified and saved on disk.=>green
      (file-name
       (helm-buffer--show-details
        name name-prefix file-name size mode dir
        'font-lock-type-face 'helm-buffer-process nil details))
      ;; Any non--file buffer.=>grey italic
      (t
       (helm-buffer--show-details
        name (and proc name-prefix) dir size mode dir
        'italic 'helm-buffer-process proc details)))))

(defun helm-highlight-buffers (buffers _source)
  "Transformer function to highlight BUFFERS list.
Should be called after others transformers i.e (boring buffers)."
  (cl-loop for i in buffers
        for (name size mode meta) = (if helm-buffer-details-flag
                                        (helm-buffer--details i 'details)
                                      (helm-buffer--details i))
        for truncbuf = (if (> (string-width name) helm-buffer-max-length)
                           (helm-substring-by-width
                            name helm-buffer-max-length)
                         (concat name (make-string
                                       (- (+ helm-buffer-max-length 3)
                                          (string-width name)) ? )))
        for len = (length mode)
        when (> len helm-buffer-max-len-mode)
        do (setq helm-buffer-max-len-mode len)
        for fmode = (concat (make-string
                             (- (max helm-buffer-max-len-mode len) len) ? )
                            mode)
        ;; The max length of a number should be 1023.9X where X is the
        ;; units, this is 7 characters.
        for formatted-size = (and size (format "%7s" size))
        collect (cons (if helm-buffer-details-flag
                          (concat truncbuf "\t" formatted-size
                                  "  " fmode "  " meta)
                        name)
                      i)))

(defun helm-buffer--get-preselection (buffer-name)
  (concat "^"
          (if (and (null helm-buffer-details-flag)
                   (numberp helm-buffer-max-length)
                   (> (string-width buffer-name)
                      helm-buffer-max-length))
              (regexp-quote
               (helm-substring-by-width
                buffer-name helm-buffer-max-length))
            (concat (regexp-quote buffer-name)
                    (if helm-buffer-details-flag
                        "$" "[[:blank:]]+")))))

(defun helm-toggle-buffers-details ()
  (interactive)
  (let ((preselect (helm-buffer--get-preselection
                    (helm-get-selection))))
    (when helm-alive-p
      (setq helm-buffer-details-flag (not helm-buffer-details-flag))
      (helm-force-update preselect))))

(defun helm-buffers-sort-transformer (candidates _source)
  (if (string= helm-pattern "")
      candidates
    (sort candidates
          #'(lambda (s1 s2)
              (< (string-width s1) (string-width s2))))))


;;; match functions
;;
(defun helm-buffer--match-mjm (pattern mjm)
  (when (string-match "\\`\\*" pattern)
    (setq pattern (split-string (substring pattern 1) ","))
    (cl-loop for pat in pattern
          if (string-match "\\`!" pat)
          collect (string-match (substring pat 1) mjm) into neg
          else collect (string-match pat mjm) into pos
          finally return
          (or (and pos (cl-loop for i in pos
                             thereis (numberp i)))
              (and neg (not (cl-loop for i in neg
                                  thereis (numberp i))))))))

(defun helm-buffer--match-pattern (pattern candidate)
  (let ((fun (if helm-buffers-fuzzy-matching
                 #'helm--mapconcat-candidate
               #'identity)))
  (if (string-match "\\`!" pattern)
      (not (string-match (funcall fun (substring pattern 1))
                         candidate))
    (string-match (funcall fun pattern) candidate))))

(defun helm-buffers-list--match-fn (candidate)
  "Match maybe buffer by major-mode.
If you give a major-mode or partial major-mode,
it will list all buffers of this major-mode and/or buffers with name
matching this major-mode.
If you add a space after major-mode and then a space,
it will match all buffers of the major-mode
before space matching pattern after space.
If you give a pattern which doesn't match a major-mode, it will search buffer
with name matching pattern."
  (let* ((cand (replace-regexp-in-string "^\\s-\\{1\\}" "" candidate))
         (buf  (get-buffer cand))
         (buf-fname (buffer-file-name buf)))
    (when buf
      (with-current-buffer buf
        (let ((mjm   (format-mode-line mode-name))
              (split (split-string helm-pattern)))
          (cond ((string-match "^@" helm-pattern) ; match inside.
                 (or (helm-buffers-match-inside cand split)
                     (helm-buffer--match-pattern helm-pattern cand)))
                ;; Continue showing buffer after mjm matching and a space.
                ((string-match "\\`\\*.*\\s-$" helm-pattern)
                 (helm-buffer--match-mjm (car split) mjm))
                ((and (string-match "\\s-[@]" helm-pattern) (cdr split))
                 (and (or (helm-buffer--match-mjm (car split) mjm)
                          (and buf-fname
                               (string-match "\\`/" helm-pattern)
                               (string-match
                                (substring (car split) 1)
                                (helm-basedir buf-fname)))
                          (helm-buffer--match-pattern (car split) cand))
                      (helm-buffers-match-inside cand (cdr split))))
                ;; Continue showing buffers after entering @ after a space.
                ((string-match "\\s-[@]" helm-pattern)
                 (or (helm-buffer--match-mjm (car split) mjm)
                     (and buf-fname
                          (string-match "\\`/" helm-pattern)
                          (string-match
                           (substring (car split) 1)
                           (helm-basedir buf-fname)))
                     (helm-buffer--match-pattern (car split) cand)))
                ;; Match on major-mode and multiple patterns.
                ((and (string-match "\\`\\*" helm-pattern) (cdr split))
                 (and (helm-buffer--match-mjm (car split) mjm)
                      (cl-loop for i in (cdr split) always
                            (helm-buffer--match-pattern i cand))))
                ;; Match only on major-mode.
                ((string-match "\\`\\*" helm-pattern)
                 (helm-buffer--match-mjm (car split) mjm))
                ;; Match on dir of buffer-file-name and multiple patterns.
                ((and (string-match "\\`/" helm-pattern) buf-fname (cdr split))
                 ;; Exact match for this is better to match end of dir [1]. 
                 (and (string-match
                       (substring (car split) 1) (helm-basedir buf-fname))
                      (cl-loop for i in (cdr split) always
                            (helm-buffer--match-pattern i cand))))
                ;; Match only on dir of buffer-file-name.
                ((and (string-match "\\`/" helm-pattern) buf-fname)
                 ;; [1] same.
                 (string-match
                  (substring (car split) 1) (helm-basedir buf-fname)))
                ;; Normal string matching on multiple patterns.
                ((string-match "\\s-" helm-pattern)
                 (cl-loop for i in split always
                       (helm-buffer--match-pattern i cand)))
                ;; Normal string matching.
                (t (helm-buffer--match-pattern helm-pattern cand))))))))

(defun helm-buffers-match-inside (candidate lst)
  (cl-loop for i in lst always
        (cond ((string-match "\\`[\\]@" i)
               (helm-buffer--match-pattern i candidate))
              ((string-match "\\`@\\(.*\\)" i)
               (save-excursion
                 (let ((str (match-string 1 i)))
                   (goto-char (point-min))
                   (re-search-forward str nil t))))
              (t (helm-buffer--match-pattern i candidate)))))


(defun helm-buffer-query-replace-1 (&optional regexp-flag)
  "Query replace in marked buffers.
If REGEXP-FLAG is given use `query-replace-regexp'."
  (let ((fn     (if regexp-flag 'query-replace-regexp 'query-replace))
        (prompt (if regexp-flag "Query replace regexp" "Query replace"))
        (bufs   (helm-marked-candidates)))
    (cl-loop with replace = (query-replace-read-from prompt regexp-flag)
          with tostring = (unless (consp replace)
                            (query-replace-read-to
                             replace prompt regexp-flag))
          for buf in bufs
          do
          (save-window-excursion
            (helm-switch-to-buffer buf)
            (save-excursion
              (let ((case-fold-search t))
                (goto-char (point-min))
                (if (consp replace)
                    (apply fn (list (car replace) (cdr replace)))
                  (apply fn (list replace tostring)))))))))

(defun helm-buffer-query-replace-regexp (_candidate)
  (helm-buffer-query-replace-1 'regexp))

(defun helm-buffer-query-replace (_candidate)
  (helm-buffer-query-replace-1))

(defun helm-buffer-toggle-diff (candidate)
  "Toggle diff buffer CANDIDATE with it's file."
  (let (helm-persistent-action-use-special-display)
    (helm-aif (get-buffer-window "*Diff*")
        (progn (kill-buffer "*Diff*")
               (set-window-buffer it helm-current-buffer))
      (diff-buffer-with-file (get-buffer candidate)))))

(defun helm-buffer-diff-persistent ()
  "Toggle diff buffer without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'diff-action 'helm-buffer-toggle-diff)
    (helm-execute-persistent-action 'diff-action)))

(defun helm-revert-buffer (candidate)
  (with-current-buffer candidate
    (when (buffer-file-name) (revert-buffer t t))))

(defun helm-revert-marked-buffers (_ignore)
  (mapc 'helm-revert-buffer (helm-marked-candidates)))

(defun helm-buffer-revert-and-update (_candidate)
  (let ((marked (helm-marked-candidates))
        (preselect (helm-get-selection nil t)))
    (cl-loop for buf in marked do (helm-revert-buffer buf))
    (when (> (length marked) 1) (helm-unmark-all))
    (helm-force-update (regexp-quote preselect))))

(defun helm-buffer-revert-persistent ()
  "Revert buffer without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'revert-action '(helm-buffer-revert-and-update . never-split))
    (helm-execute-persistent-action 'revert-action)))

(defun helm-buffer-save-and-update (_candidate)
  (let ((marked (helm-marked-candidates))
        (preselect (helm-get-selection nil t))
        (enable-recursive-minibuffers t))
    (cl-loop for buf in marked do
          (with-current-buffer (get-buffer buf)
            (when (buffer-file-name) (save-buffer))))
    (when (> (length marked) 1) (helm-unmark-all))
    (helm-force-update (regexp-quote preselect))))

(defun helm-buffer-save-persistent ()
  "Save buffer without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'save-action '(helm-buffer-save-and-update . never-split))
    (helm-execute-persistent-action 'save-action)))

(defun helm-buffer-run-kill-persistent ()
  "Kill buffer without quitting helm."
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'kill-action '(helm-buffers-persistent-kill . never-split))
    (helm-execute-persistent-action 'kill-action)))

(defun helm-kill-marked-buffers (_ignore)
  (mapc 'kill-buffer (helm-marked-candidates)))

(defun helm-buffer-run-kill-buffers ()
  "Run kill buffer action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-kill-marked-buffers)))

(defun helm-buffer-run-grep ()
  "Run Grep action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-grep-buffers)))

(defun helm-buffer-run-zgrep ()
  "Run Grep action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-zgrep-buffers)))

(defun helm-buffer-run-query-replace-regexp ()
  "Run Query replace regexp action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-buffer-query-replace-regexp)))

(defun helm-buffer-run-query-replace ()
  "Run Query replace action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-buffer-query-replace)))

(defun helm-buffer-switch-other-window ()
  "Run switch to other window action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'switch-to-buffer-other-window)))

(defun helm-buffer-switch-other-frame ()
  "Run switch to other frame action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'switch-to-buffer-other-frame)))

(defun helm-buffer-switch-to-elscreen ()
  "Run switch to elscreen  action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-find-buffer-on-elscreen)))

(defun helm-buffer-run-ediff ()
  "Run ediff action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-ediff-marked-buffers)))

(defun helm-buffer-run-ediff-merge ()
  "Run ediff action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-ediff-marked-buffers-merge)))

(defun helm-buffers-persistent-kill (buffer)
  "Persistent action to kill buffer."
  (with-current-buffer (get-buffer buffer)
    (if (and (buffer-modified-p)
             (buffer-file-name (current-buffer)))
        (progn
          (save-buffer)
          (kill-buffer buffer))
      (kill-buffer buffer)))
  (helm-delete-current-selection)
  (when (helm-empty-source-p) (helm-next-source))
  (with-helm-temp-hook 'helm-after-persistent-action-hook
    (helm-force-update (regexp-quote (helm-get-selection nil t)))))

(defun helm-buffers-list-persistent-action (candidate)
  (if current-prefix-arg
      (helm-buffers-persistent-kill candidate)
    (helm-switch-to-buffer candidate)))

(defun helm-ediff-marked-buffers (_candidate &optional merge)
  "Ediff 2 marked buffers or CANDIDATE and `helm-current-buffer'.
With optional arg MERGE call `ediff-merge-buffers'."
  (let ((lg-lst (length (helm-marked-candidates)))
        buf1 buf2)
    (cl-case lg-lst
      (0
       (error "Error:You have to mark at least 1 buffer"))
      (1
       (setq buf1 helm-current-buffer
             buf2 (cl-first (helm-marked-candidates))))
      (2
       (setq buf1 (cl-first (helm-marked-candidates))
             buf2 (cl-second (helm-marked-candidates))))
      (t
       (error "Error:To much buffers marked!")))
    (if merge
        (ediff-merge-buffers buf1 buf2)
      (ediff-buffers buf1 buf2))))

(defun helm-ediff-marked-buffers-merge (candidate)
  "Ediff merge `helm-current-buffer' with CANDIDATE.
See `helm-ediff-marked-buffers'."
  (helm-ediff-marked-buffers candidate t))

(defun helm-multi-occur-as-action (_candidate)
  "Multi occur action for `helm-source-buffers-list'.
Can be used by any source that list buffers."
  (let ((helm-moccur-always-search-in-current
         (if helm-current-prefix-arg
             (not helm-moccur-always-search-in-current)
           helm-moccur-always-search-in-current))
        (buffers (helm-marked-candidates))
        (input (cl-loop for i in (split-string helm-pattern " " t)
                     thereis (and (string-match "\\`@\\(.*\\)" i)
                                  (match-string 1 i)))))
    (helm-multi-occur-1 buffers input)))

(defun helm-buffers-run-multi-occur ()
  "Run `helm-multi-occur-as-action' by key."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-multi-occur-as-action)))

(defun helm-buffers-toggle-show-hidden-buffers ()
  (interactive)
  (with-helm-alive-p
    (let ((filter-attrs (helm-attr 'filtered-candidate-transformer
                                   helm-source-buffers-list)))
      (if (memq 'helm-shadow-boring-buffers filter-attrs)
          (helm-attrset 'filtered-candidate-transformer
                        (cons 'helm-skip-boring-buffers
                              (remove 'helm-shadow-boring-buffers
                                      filter-attrs))
                        helm-source-buffers-list t)
        (helm-attrset 'filtered-candidate-transformer
                      (cons 'helm-shadow-boring-buffers
                            (remove 'helm-skip-boring-buffers
                                    filter-attrs))
                      helm-source-buffers-list t))
      (helm-force-update))))


;;; Candidate Transformers
;;
;;
(defun helm-skip-boring-buffers (buffers _source)
  (helm-skip-entries buffers helm-boring-buffer-regexp-list))

(defun helm-shadow-boring-buffers (buffers _source)
  "Buffers matching `helm-boring-buffer-regexp' will be
displayed with the `file-name-shadow' face if available."
  (helm-shadow-entries buffers helm-boring-buffer-regexp-list))


(define-helm-type-attribute 'buffer
    `((action
       ("Switch to buffer" . helm-switch-to-buffer)
       ,(and (locate-library "popwin") '("Switch to buffer in popup window" . popwin:popup-buffer))
       ("Switch to buffer other window" . switch-to-buffer-other-window)
       ("Switch to buffer other frame" . switch-to-buffer-other-frame)
       ,(and (locate-library "elscreen") '("Display buffer in Elscreen" . helm-find-buffer-on-elscreen))
       ("Query replace regexp" . helm-buffer-query-replace-regexp)
       ("Query replace" . helm-buffer-query-replace)
       ("View buffer" . view-buffer)
       ("Display buffer"   . display-buffer)
       ("Grep buffers (C-u grep all buffers)" . helm-zgrep-buffers)
       ("Multi occur buffer(s)" . helm-multi-occur-as-action)
       ("Revert buffer(s)" . helm-revert-marked-buffers)
       ("Insert buffer" . insert-buffer)
       ("Kill buffer(s)" . helm-kill-marked-buffers)
       ("Diff with file" . diff-buffer-with-file)
       ("Ediff Marked buffers" . helm-ediff-marked-buffers)
       ("Ediff Merge marked buffers" . (lambda (candidate)
                                         (helm-ediff-marked-buffers candidate t))))
      (persistent-help . "Show this buffer")
      (filtered-candidate-transformer helm-skip-boring-buffers
                                      helm-buffers-sort-transformer
                                      helm-highlight-buffers))
  "Buffer or buffer name.")

;;;###autoload
(defun helm-buffers-list ()
  "Preconfigured `helm' to list buffers."
  (interactive)
  (helm :sources '(helm-source-buffers-list
                   helm-source-ido-virtual-buffers
                   helm-source-buffer-not-found)
        :buffer "*helm buffers*"
        :keymap helm-buffer-map
        :truncate-lines t))

(provide 'helm-buffers)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-buffers.el ends here

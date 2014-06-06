;;; helm-regexp.el --- In buffer regexp searching and replacement for helm. -*- lexical-binding: t -*-

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


(defgroup helm-regexp nil
  "Regexp related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-moccur-always-search-in-current nil
  "Helm multi occur always search in current buffer when non--nil."
  :group 'helm-regexp
  :type 'boolean)

(defcustom helm-moccur-use-ioccur-style-keys t
  "Similar to `helm-grep-use-ioccur-style-keys' but for multi occur."
  :group 'helm-regexp
  :type 'boolean)


(defface helm-moccur-buffer
    '((t (:foreground "DarkTurquoise" :underline t)))
  "Face used to highlight moccur buffer names."
  :group 'helm-regexp)


(defvar helm-moccur-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'helm-moccur-help)
    (define-key map (kbd "C-c o")    'helm-moccur-run-goto-line-ow)
    (define-key map (kbd "C-c C-o")  'helm-moccur-run-goto-line-of)
    (define-key map (kbd "C-x C-s")  'helm-moccur-run-save-buffer)
    (when helm-moccur-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'helm-moccur-run-persistent-action)
      (define-key map (kbd "<left>")   'helm-moccur-run-default-action))
    (delq nil map))
  "Keymap used in Moccur source.")


(defvar helm-build-regexp-history nil)
(defun helm-query-replace-regexp (_candidate)
  "Query replace regexp from `helm-regexp'.
With a prefix arg replace only matches surrounded by word boundaries,
i.e Don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp (funcall (helm-attr 'regexp))))
    (apply 'query-replace-regexp
           (helm-query-replace-args regexp))))

(defun helm-kill-regexp-as-sexp (_candidate)
  "Kill regexp in a format usable in lisp code."
  (helm-regexp-kill-new
   (prin1-to-string (funcall (helm-attr 'regexp)))))

(defun helm-kill-regexp (_candidate)
  "Kill regexp as it is in `helm-pattern'."
  (helm-regexp-kill-new (funcall (helm-attr 'regexp))))

(defun helm-query-replace-args (regexp)
  "create arguments of `query-replace-regexp' action in `helm-regexp'."
  (let ((region-only (helm-region-active-p)))
    (list
     regexp
     (query-replace-read-to regexp
                            (format "Query replace %sregexp %s"
                                    (if helm-current-prefix-arg "word " "")
                                    (if region-only "in region " ""))
                            t)
     helm-current-prefix-arg
     (when region-only (region-beginning))
     (when region-only (region-end)))))

(defvar helm-source-regexp
  '((name . "Regexp Builder")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
                  'global (with-temp-buffer
                            (insert-buffer-substring helm-current-buffer)
                            (buffer-string)))))
    (candidates-in-buffer)
    (get-line . helm-regexp-get-line)
    (persistent-action . helm-regexp-persistent-action)
    (persistent-help . "Show this line")
    (multiline)
    (no-matchplugin)
    (requires-pattern . 2)
    (mode-line . "Press TAB to select action.")
    (regexp . (lambda () helm-input))
    (action . (("Kill Regexp as sexp" . helm-kill-regexp-as-sexp)
               ("Query Replace Regexp (C-u Not inside word.)"
                . helm-query-replace-regexp)
               ("Kill Regexp" . helm-kill-regexp)))))

(defun helm-regexp-get-line (s e)
  (let ((matches (match-data))
        (line    (buffer-substring s e)))
    (propertize
     (cl-loop with ln = (format "%5d: %s" (line-number-at-pos (1- s)) line)
           for i from 0 to (1- (/ (length matches) 2))
           concat (format "\n         %s'%s'" (format "Group %d: " i)
                          (match-string i))
           into ln1
           finally return (concat ln ln1))
     ;; match beginning
     ;; KLUDGE: point of helm-candidate-buffer is +1 than that of helm-current-buffer.
     ;; It is implementation problem of candidates-in-buffer.
     'helm-real-value (1- s))))

(defun helm-regexp-persistent-action (pt)
  (helm-goto-char pt)
  (helm-highlight-current-line))

(defun helm-regexp-kill-new (input)
  (kill-new input)
  (message "Killed: %s" input))

(defun helm-quote-whitespace (candidate)
  "Quote whitespace, if some, in string CANDIDATE."
  (replace-regexp-in-string " " "\\\\ " candidate))


;;; Occur
;;
;;
(defvar helm-source-occur nil)
(defun helm-occur-init-source ()
  (unless helm-source-occur
    (setq helm-source-occur (copy-alist helm-source-moccur))
    (helm-attrset 'name "Occur" helm-source-occur)))


;;; Multi occur
;;
;;

;; Internal
(defvar helm-multi-occur-buffer-list nil)

(defun helm-moccur-init ()
  "Create the initial helm multi occur buffer."
  (helm-init-candidates-in-buffer
      'global
    (cl-loop for buf in helm-multi-occur-buffer-list
          for bufstr = (with-current-buffer buf (buffer-string))
          do (add-text-properties
              0 (length bufstr)
              `(buffer-name ,(buffer-name (get-buffer buf)))
              bufstr)
          concat bufstr)))

(defun helm-moccur-get-line (beg end)
  "Format line for `helm-source-moccur'."
  (format "%s:%d:%s"
          (get-text-property beg 'buffer-name)
          (save-restriction
            (narrow-to-region (previous-single-property-change
                               (point) 'buffer-name)
                              (next-single-property-change
                               (point) 'buffer-name))
            (line-number-at-pos beg))
          (buffer-substring beg end)))

(cl-defun helm-moccur-action (candidate
                              &optional (method (quote buffer)) mark)
  "Jump to CANDIDATE with METHOD.
arg METHOD can be one of buffer, buffer-other-window, buffer-other-frame."
  (require 'helm-grep)
  (let* ((split (helm-grep-split-line candidate))
         (buf (car split))
         (lineno (string-to-number (nth 1 split)))
         (split-pat (if helm-occur-match-plugin-mode
                        (helm-mp-split-pattern helm-pattern)
                      (list helm-pattern))))
    (cl-case method
      (buffer              (switch-to-buffer buf))
      (buffer-other-window (switch-to-buffer-other-window buf))
      (buffer-other-frame  (switch-to-buffer-other-frame buf)))
    (helm-goto-line lineno)
    ;; Move point to the nearest matching regexp from bol.
    (cl-loop for reg in split-pat
          when (save-excursion
                 (re-search-forward reg (point-at-eol) t))
          collect (match-beginning 0) into pos-ls
          finally (goto-char (apply #'min pos-ls)))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))))

(defun helm-moccur-persistent-action (candidate)
  (helm-moccur-goto-line candidate)
  (helm-highlight-current-line))

(defun helm-moccur-run-persistent-action ()
  (interactive)
  (with-helm-alive-p
    (helm-execute-persistent-action)))

(defun helm-moccur-goto-line (candidate)
  "From multi occur, switch to buffer and go to nth 1 CANDIDATE line."
  (helm-moccur-action
   candidate 'buffer (or current-prefix-arg         ; persistent.
                         helm-current-prefix-arg))) ; exit.

(defun helm-moccur-goto-line-ow (candidate)
  "Go to CANDIDATE line in other window.
Same as `helm-moccur-goto-line' but go in other window."
  (helm-moccur-action
   candidate 'buffer-other-window
   (or current-prefix-arg         ; persistent.
       helm-current-prefix-arg))) ; exit.

(defun helm-moccur-goto-line-of (candidate)
  "Go to CANDIDATE line in new frame.
Same as `helm-moccur-goto-line' but go in new frame."
  (helm-moccur-action
   candidate 'buffer-other-frame
   (or current-prefix-arg         ; persistent.
       helm-current-prefix-arg))) ; exit.

(defun helm-moccur-run-goto-line-ow ()
  "Run goto line other window action from `helm-source-moccur'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-moccur-goto-line-ow)))

(defun helm-moccur-run-goto-line-of ()
  "Run goto line new frame action from `helm-source-moccur'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-moccur-goto-line-of)))

(defun helm-moccur-run-default-action ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-moccur-goto-line)))

;;;###autoload
(define-minor-mode helm-occur-match-plugin-mode
    "Turn On/Off `helm-match-plugin-mode' only for `helm-m/occur'."
  :global t
  :init-value t
  (if helm-occur-match-plugin-mode
      (setq helm-source-moccur
            (remove (assoc 'no-matchplugin helm-source-moccur)
                    helm-source-moccur)
            helm-source-occur helm-source-moccur)
    (helm-attrset 'no-matchplugin nil helm-source-moccur)
    (setq helm-source-occur helm-source-moccur)))

(defvar helm-source-moccur
  `((name . "Moccur")
    (init . (lambda ()
              (require 'helm-grep)
              (helm-moccur-init)))
    (candidates-in-buffer)
    (filter-one-by-one . helm-moccur-filter-one-by-one)
    (get-line . helm-moccur-get-line)
    (nohighlight)
    (migemo)
    (action . (("Go to Line" . helm-moccur-goto-line)
               ("Goto line other window" . helm-moccur-goto-line-ow)
               ("Goto line new frame" . helm-moccur-goto-line-of)))
    (persistent-action . helm-moccur-persistent-action)
    (persistent-help . "Go to line")
    (recenter)
    (candidate-number-limit . 9999)
    (mode-line . helm-moccur-mode-line)
    (keymap . ,helm-moccur-map)
    (history . ,'helm-grep-history)
    (requires-pattern . 2))
  "Helm source for multi occur.")

(defun helm-moccur-filter-one-by-one (candidate)
  "`filter-one-by-one' function for `helm-source-moccur'."
  (require 'helm-grep)
  (let* ((split  (helm-grep-split-line candidate))
         (buf    (car split))
         (lineno (nth 1 split))
         (str    (nth 2 split)))
    (cons (concat (propertize
                   buf
                   'face 'helm-moccur-buffer
                   'help-echo (buffer-file-name
                               (get-buffer buf))
                   'buffer-name buf)
                  ":"
                  (propertize lineno 'face 'helm-grep-lineno)
                  ":"
                  (helm-grep-highlight-match
                   str helm-occur-match-plugin-mode))
          candidate)))

(defun helm-multi-occur-1 (buffers &optional input)
  "Main function to call `helm-source-moccur' with BUFFERS list."
  (setq helm-multi-occur-buffer-list
        (if helm-moccur-always-search-in-current
            (cons
             ;; will become helm-current-buffer later.
             (buffer-name (current-buffer))
             (remove helm-current-buffer helm-multi-occur-buffer-list))
         buffers))
  (helm :sources 'helm-source-moccur
        :buffer "*helm multi occur*"
        :history 'helm-grep-history
        :input input
        :multi-occur-buffer-list helm-multi-occur-buffer-list
        :truncate-lines t))

;;;###autoload
(defun helm-moccur-run-save-buffer ()
  "Run grep save results action from `helm-do-grep-1'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-moccur-save-results)))


;;; helm-moccur-mode
;;
;;
(defvar helm-moccur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'helm-moccur-mode-goto-line)
    (define-key map (kbd "C-o")      'helm-moccur-mode-goto-line-ow)
    (define-key map (kbd "<C-down>") 'undefined)
    (define-key map (kbd "<C-up>")   'undefined)
    (define-key map (kbd "<M-down>") 'helm-gm-next-file)
    (define-key map (kbd "<M-up>")   'helm-gm-precedent-file)
    map))

(defun helm-moccur-mode-goto-line ()
  (interactive)
  (helm-moccur-goto-line
   (buffer-substring (point-at-bol) (point-at-eol))))

(defun helm-moccur-mode-goto-line-ow ()
  (interactive)
  (helm-moccur-goto-line-ow
   (buffer-substring (point-at-bol) (point-at-eol))))

(defun helm-moccur-save-results (_candidate)
  (helm-moccur-save-results-1))

(defun helm-moccur-save-results-1 ()
  "Save helm moccur results in a `helm-moccur-mode' buffer."
  (let ((buf "*hmoccur*")
        new-buf)
    (when (get-buffer buf)
      (setq new-buf (read-string "OccurBufferName: " buf))
      (cl-loop for b in (helm-buffer-list)
            when (and (string= new-buf b)
                      (not (y-or-n-p
                            (format "Buffer `%s' already exists overwrite? "
                                    new-buf))))
            do (setq new-buf (read-string "OccurBufferName: " "*hmoccur ")))
      (setq buf new-buf))
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "-*- mode: helm-moccur -*-\n\n"
                (format "Moccur Results for `%s':\n\n" helm-pattern))
        (save-excursion
          (insert (with-current-buffer helm-buffer
                    (goto-char (point-min)) (forward-line 1)
                    (buffer-substring (point) (point-max))))))
      (helm-moccur-mode) (pop-to-buffer buf))
    (message "Helm Moccur Results saved in `%s' buffer" buf)))

;;;###autoload
(define-derived-mode helm-moccur-mode
    special-mode "helm-moccur"
    "Major mode to provide actions in helm moccur saved buffer.

Special commands:
\\{helm-moccur-mode-map}"
    (set (make-local-variable 'helm-multi-occur-buffer-list)
         (with-helm-buffer helm-multi-occur-buffer-list))
    (set (make-local-variable 'revert-buffer-function)
         #'helm-moccur-mode--revert-buffer-function))

(defun helm-moccur-mode--revert-buffer-function (&optional _ignore-auto _noconfirm)
  (goto-char (point-min))
  (let (pattern)
    (when (re-search-forward "^Moccur Results for `\\(.*\\)'" nil t)
      (setq pattern (match-string 1))
      (forward-line 0)
      (when (re-search-forward "^$" nil t)
        (forward-line 1))
      (let ((inhibit-read-only t)
            (buffer (current-buffer))
            (buflst helm-multi-occur-buffer-list))
        (delete-region (point) (point-max))
        (message "Reverting buffer...")
        (save-excursion
          (with-temp-buffer
            (insert
             "\n"
             (cl-loop for buf in buflst
                      for bufstr = (with-current-buffer buf (buffer-string))
                      do (add-text-properties
                          0 (length bufstr)
                          `(buffer-name ,(buffer-name (get-buffer buf)))
                          bufstr)
                      concat bufstr)
             "\n")
            (goto-char (point-min))
            (cl-loop while (re-search-forward pattern nil t)
                     for line = (helm-moccur-get-line (point-at-bol) (point-at-eol))
                     when line
                     do (with-current-buffer buffer
                          (insert
                           (propertize
                            (car (helm-moccur-filter-one-by-one line))
                            'helm-real-value line)
                           "\n")))))
        (message "Reverting buffer done")))))


;;; Predefined commands
;;
;;

;;;###autoload
(defun helm-regexp ()
  "Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp."
  (interactive)
  (save-restriction
    (when (and (helm-region-active-p)
               ;; Don't narrow to region if buffer is already narrowed.
               (not (helm-current-buffer-narrowed-p (current-buffer))))
      (narrow-to-region (region-beginning) (region-end)))
    (helm :sources helm-source-regexp
          :buffer "*helm regexp*"
          :prompt "Regexp: "
          :history 'helm-build-regexp-history)))

;;;###autoload
(defun helm-occur ()
  "Preconfigured helm for Occur."
  (interactive)
  (setq helm-multi-occur-buffer-list (list (buffer-name (current-buffer))))
  (helm-occur-init-source)
  (helm :sources 'helm-source-occur
        :buffer "*helm occur*"
        :history 'helm-grep-history
        :preselect (and (memq 'helm-source-occur helm-sources-using-default-as-input)
                        (format "%s:%d:" (buffer-name) (line-number-at-pos (point))))
        :multi-occur-buffer-list helm-multi-occur-buffer-list
        :truncate-lines t))

;;;###autoload
(defun helm-occur-from-isearch ()
  "Invoke `helm-occur' from isearch."
  (interactive)
  (let ((input (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (setq helm-multi-occur-buffer-list (list (buffer-name (current-buffer))))
    (helm-occur-init-source)
    (helm :sources 'helm-source-occur
          :buffer "*helm occur*"
          :history 'helm-grep-history
          :input input
          :multi-occur-buffer-list helm-multi-occur-buffer-list
          :truncate-lines t)))

;;;###autoload
(defun helm-multi-occur (buffers)
  "Preconfigured helm for multi occur.

  BUFFERS is a list of buffers to search through.
With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling `helm-multi-occur'
or during the buffer selection."
  (interactive (list (helm-comp-read
                      "Buffers: " (helm-buffer-list)
                      :marked-candidates t)))
  (let ((helm-moccur-always-search-in-current
         (if (or current-prefix-arg
                 helm-current-prefix-arg)
             (not helm-moccur-always-search-in-current)
           helm-moccur-always-search-in-current)))
    (helm-multi-occur-1 buffers)))

;;;###autoload
(defun helm-multi-occur-from-isearch (&optional _arg)
  "Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection."
  (interactive "p")
  (let (buf-list
        helm-moccur-always-search-in-current
        (input (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (setq buf-list (helm-comp-read "Buffers: "
                                   (helm-buffer-list)
                                   :name "Occur in buffer(s)"
                                   :marked-candidates t))
    (setq helm-moccur-always-search-in-current
          (if (or current-prefix-arg
                  helm-current-prefix-arg)
              (not helm-moccur-always-search-in-current)
            helm-moccur-always-search-in-current))
    (helm-multi-occur-1 buf-list input)))


(provide 'helm-regexp)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-regexp.el ends here

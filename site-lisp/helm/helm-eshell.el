;;; helm-eshell.el --- pcomplete and eshell completion for helm. -*- lexical-binding: t -*-

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

;; Enable like this in .emacs:
;;
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))
;;

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-elisp)
(require 'helm-regexp)

(declare-function eshell-read-aliases-list "em-alias")
(declare-function eshell-send-input "esh-mode" (&optional use-region queue-p no-newline))
(declare-function eshell-bol "esh-mode")
(declare-function eshell-parse-arguments "esh-arg" (beg end))

(defvar helm-eshell-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-p") 'helm-next-line)
    map)
  "Keymap for `helm-eshell-history'.")

(defvar helm-esh-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "TAB") 'helm-next-line)
    map)
  "Keymap for `helm-esh-pcomplete'.")

(defvar helm-source-esh
  '((name . "Eshell completions")
    (init . (lambda ()
              (setq pcomplete-current-completions nil
                    pcomplete-last-completion-raw nil)
              ;; Eshell-command add this hook in all minibuffers
              ;; Remove it for the helm one. (Fixed in Emacs24)
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates . helm-esh-get-candidates)
    (nomark)
    (persistent-action . ignore)
    (filtered-candidate-transformer
     (lambda (candidates _sources)
       (cl-loop for i in (sort candidates 'helm-generic-sort-fn)
             collect
             (cond ((string-match "\\`~/?" helm-ec-target)
                    (abbreviate-file-name i))
                   ((string-match "\\`/" helm-ec-target) i)
                   (t
                    (file-relative-name i))))))
    (action . helm-ec-insert))
  "Helm source for Eshell completion.")

;; Internal.
(defvar helm-ec-target "")
(defun helm-ec-insert (candidate)
  "Replace text at point with CANDIDATE.
The function that call this should set `helm-ec-target' to thing at point."
  (let ((pt (point)))
    (when (and helm-ec-target
               (search-backward helm-ec-target nil t)
               (string= (buffer-substring (point) pt) helm-ec-target))
      (delete-region (point) pt)))
  (cond ((string-match "\\`~/?" helm-ec-target)
         (insert (helm-quote-whitespace (abbreviate-file-name candidate))))
        ((string-match "\\`/" helm-ec-target)
         (insert (helm-quote-whitespace candidate)))
        (t
         (insert (concat (and (string-match "\\`[.]/" helm-ec-target) "./")
                         (helm-quote-whitespace
                          (file-relative-name candidate)))))))

(defun helm-esh-get-candidates ()
  "Get candidates for eshell completion using `pcomplete'."
  (catch 'pcompleted
    (with-helm-current-buffer
      (let* ((pcomplete-stub)
             pcomplete-seen pcomplete-norm-func
             pcomplete-args pcomplete-last pcomplete-index
             (pcomplete-autolist pcomplete-autolist)
             (pcomplete-suffix-list pcomplete-suffix-list)
             (table (pcomplete-completions))
             (entry (or (try-completion helm-pattern
                                        (pcomplete-entries))
                        helm-pattern)))
        (cl-loop ;; expand entry too to be able to compare it with file-cand.
              with exp-entry = (and (stringp entry)
                                    (not (string= entry ""))
                                    (file-name-as-directory
                                     (expand-file-name entry default-directory)))
              for i in (all-completions pcomplete-stub table)
              ;; Transform the related names to abs names.
              for file-cand = (and exp-entry
                                   (if (file-remote-p i) i
                                     (expand-file-name
                                      i (file-name-directory entry))))
              ;; Compare them to avoid dups.
              for file-entry-p = (and (stringp exp-entry)
                                      (stringp file-cand)
                                      ;; Fix :/tmp/foo/ $ cd foo
                                      (not (file-directory-p file-cand))
                                      (file-equal-p exp-entry file-cand))
              if (and file-cand (or (file-remote-p file-cand)
                                    (file-exists-p file-cand))
                      (not file-entry-p))
              collect file-cand into ls
              else
              ;; Avoid adding entry here.
              unless file-entry-p collect i into ls
              finally return
              (if (and exp-entry
                       (file-directory-p exp-entry)
                       ;; If the car of completion list is
                       ;; an executable, probably we are in
                       ;; command completion, so don't add a
                       ;; possible file related entry here.
                       (and ls (not (executable-find (car ls))))
                       ;; Don't add entry if already in prompt.
                       (not (file-equal-p exp-entry pcomplete-stub)))
                  (append (list exp-entry)
                          ;; Entry should not be here now but double check.
                          (remove entry ls))
                ls))))))

;;; Eshell history.
;;
;;
(defvar helm-source-eshell-history
  `((name . "Eshell history")
    (init . (lambda ()
              (let (eshell-hist-ignoredups)
                (eshell-write-history eshell-history-file-name t)
                (with-current-buffer (helm-candidate-buffer 'global)
                  (insert-file-contents eshell-history-file-name)))
              ;; Same comment as in `helm-source-esh'
              (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
    (candidates-in-buffer)
    (nomark)
    (keymap . ,helm-eshell-history-map)
    (filtered-candidate-transformer . (lambda (candidates sources)
                                        (reverse candidates)))
    (candidate-number-limit . 9999)
    (action . (lambda (candidate)
                (eshell-kill-input)
                (insert candidate))))
  "Helm source for Eshell history.")

;;;###autoload
(defun helm-esh-pcomplete ()
  "Preconfigured helm to provide helm completion in eshell."
  (interactive)
  (let* ((helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         (end (point-marker))
         (beg (save-excursion (eshell-bol) (point)))
         (args (catch 'eshell-incomplete
                 (eshell-parse-arguments beg end)))
         ;; Use thing-at-point instead of last args value
         ;; to exclude possible delimiters e.g "(".
         (target (thing-at-point 'symbol))
         (first (car args)) ; Maybe lisp delimiter "(".
         last) ; Will be the last but parsed by pcomplete.
    (setq helm-ec-target (or target " ")
          end (point)
          ;; Reset beg for `with-helm-show-completion'.
          beg (or (and target (- end (length target)))
                  ;; Nothing at point.
                  (progn (insert " ") (point))))
    (cond ((eq first ?\()
           (helm-lisp-completion-or-file-name-at-point))
          ;; In eshell `pcomplete-parse-arguments' is called
          ;; with `pcomplete-parse-arguments-function'
          ;; locally bound to `eshell-complete-parse-arguments'
          ;; which is calling `lisp-complete-symbol',
          ;; calling it before would popup the
          ;; *completions* buffer.
          (t (setq last (car (last (ignore-errors
                                     (pcomplete-parse-arguments)))))
             (with-helm-show-completion beg end
               (helm :sources 'helm-source-esh
                     :buffer "*helm pcomplete*"
                     :keymap helm-esh-completion-map
                     :resume 'noresume
                     :input (and (stringp last)
                                 (helm-ff-set-pattern last))))))))

;;;###autoload
(defun helm-eshell-history ()
  "Preconfigured helm for eshell history."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion (eshell-bol) (point)))
         (input (buffer-substring beg end))
         flag-empty)
    (when (eq beg end)
      (insert " ")
      (setq flag-empty t)
      (setq end (point)))
    (unwind-protect
         (with-helm-show-completion beg end
           (helm :sources 'helm-source-eshell-history
                 :buffer "*helm eshell history*"
                 :resume 'noresume
                 :input input))
      (when (and flag-empty
                 (looking-back " "))
        (delete-char -1)))))

(provide 'helm-eshell)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-eshell ends here

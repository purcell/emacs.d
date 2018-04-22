;;; git-blamed.el --- Minor mode for incremental blame for Git  -*- coding: utf-8 -*-
;;
;; Copyright (C) 2007  David Kågedal
;;
;; Authors:    David Kågedal <davidk@lysator.liu.se>
;; Created:    31 Jan 2007
;; Message-ID: <87iren2vqx.fsf@morpheus.local>
;; License:    GPL
;; Keywords:   git, version control, release management
;; Package-Version: 20161028.1226
;;
;; Compatibility: Emacs21, Emacs22 and EmacsCVS
;;                Git 1.5 and up

;; This file is *NOT* part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; http://www.fsf.org/copyleft/gpl.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Here is an Emacs implementation of incremental git-blame.  When you
;; turn it on while viewing a file, the editor buffer will be updated by
;; setting the background of individual lines to a color that reflects
;; which commit it comes from.  And when you move around the buffer, a
;; one-line summary will be shown in the echo area.

;;; Installation:
;;
;; To use this package, put it somewhere in `load-path' (or add
;; directory with git-blamed.el to `load-path'), and add the following
;; line to your .emacs:
;;
;;    (require 'git-blamed)
;;
;; If you do not want to load this package before it is necessary, you
;; can make use of the `autoload' feature, e.g. by adding to your .emacs
;; the following lines
;;
;;    (autoload 'git-blamed-mode "git-blamed"
;;              "Minor mode for incremental blame for Git." t)
;;
;; Then first use of `M-x git-blamed-mode' would load the package.

;;; Compatibility:
;;
;; It requires GNU Emacs 21 or later and Git 1.5.0 and up
;;
;; If you'are using Emacs 20, try changing this:
;;
;;            (overlay-put ovl 'face (list :background
;;                                         (cdr (assq 'color (cddddr info)))))
;;
;; to
;;
;;            (overlay-put ovl 'face (cons 'background-color
;;                                         (cdr (assq 'color (cddddr info)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))			      ; to use `push', `pop'


(defun git-blamed-color-scale (&rest elements)
  "Given a list, returns a list of triples formed with each
elements of the list.

a b => bbb bba bab baa abb aba aaa aab"
  (let (result)
    (dolist (a elements)
      (dolist (b elements)
        (dolist (c elements)
          (setq result (cons (format "#%s%s%s" a b c) result)))))
    result))

;; (git-blamed-color-scale "0c" "04" "24" "1c" "2c" "34" "14" "3c") =>
;; ("#3c3c3c" "#3c3c14" "#3c3c34" "#3c3c2c" "#3c3c1c" "#3c3c24"
;; "#3c3c04" "#3c3c0c" "#3c143c" "#3c1414" "#3c1434" "#3c142c" ...)

(defmacro git-blamed-random-pop (l)
  "Select a random element from L and returns it. Also remove
selected element from l."
  ;; only works on lists with unique elements
  `(let ((e (elt ,l (random (length ,l)))))
     (setq ,l (remove e ,l))
     e))

(defvar git-blamed-dark-colors
  (git-blamed-color-scale "0c" "04" "24" "1c" "2c" "34" "14" "3c")
  "*List of colors (format #RGB) to use in a dark environment.

To check out the list, evaluate (list-colors-display git-blamed-dark-colors).")

(defvar git-blamed-light-colors
  (git-blamed-color-scale "c4" "d4" "cc" "dc" "f4" "e4" "fc" "ec")
  "*List of colors (format #RGB) to use in a light environment.

To check out the list, evaluate (list-colors-display git-blamed-light-colors).")

(defvar git-blamed-colors '()
  "Colors used by git-blamed. The list is built once when activating git-blamed
minor mode.")

(defvar git-blamed-ancient-color "dark green"
  "*Color to be used for ancient commit.")

(defvar git-blamed-autoupdate t
  "*Automatically update the blame display while editing")

(defvar git-blamed-proc nil
  "The running git-blamed process")
(make-variable-buffer-local 'git-blamed-proc)

(defvar git-blamed-overlays nil
  "The git-blamed overlays used in the current buffer.")
(make-variable-buffer-local 'git-blamed-overlays)

(defvar git-blamed-cache nil
  "A cache of git-blamed information for the current buffer")
(make-variable-buffer-local 'git-blamed-cache)

(defvar git-blamed-idle-timer nil
  "An idle timer that updates the blame")
(make-variable-buffer-local 'git-blamed-cache)

(defvar git-blamed-update-queue nil
  "A queue of update requests")
(make-variable-buffer-local 'git-blamed-update-queue)

;; FIXME: docstrings
(defvar git-blamed-file nil)
(defvar git-blamed-current nil)

(defvar git-blamed-mode nil)
(make-variable-buffer-local 'git-blamed-mode)

(defvar git-blamed-mode-line-string " blame"
  "String to display on the mode line when git-blamed is active.")

(or (assq 'git-blamed-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(git-blamed-mode git-blamed-mode-line-string) minor-mode-alist)))

;;;###autoload
(defun git-blamed-mode (&optional arg)
  "Toggle minor mode for displaying Git blame

With prefix ARG, turn the mode on if ARG is positive."
  (interactive "P")
  (cond
   ((null arg)
    (if git-blamed-mode (git-blamed-mode-off) (git-blamed-mode-on)))
   ((> (prefix-numeric-value arg) 0) (git-blamed-mode-on))
   (t (git-blamed-mode-off))))

(defun git-blamed-mode-on ()
  "Turn on git-blamed mode.

See also function `git-blamed-mode'."
  (make-local-variable 'git-blamed-colors)
  (if git-blamed-autoupdate
      (add-hook 'after-change-functions 'git-blamed-after-change nil t)
    (remove-hook 'after-change-functions 'git-blamed-after-change t))
  (git-blamed-cleanup)
  (let ((bgmode (cdr (assoc 'background-mode (frame-parameters)))))
    (if (eq bgmode 'dark)
	(setq git-blamed-colors git-blamed-dark-colors)
      (setq git-blamed-colors git-blamed-light-colors)))
  (setq git-blamed-cache (make-hash-table :test 'equal))
  (setq git-blamed-mode t)
  (git-blamed-run))

(defun git-blamed-mode-off ()
  "Turn off git-blamed mode.

See also function `git-blamed-mode'."
  (git-blamed-cleanup)
  (if git-blamed-idle-timer (cancel-timer git-blamed-idle-timer))
  (setq git-blamed-mode nil))

;;;###autoload
(defun git-reblame ()
  "Recalculate all blame information in the current buffer"
  (interactive)
  (unless git-blamed-mode
    (error "git-blamed is not active"))

  (git-blamed-cleanup)
  (git-blamed-run))

(defun git-blamed-run (&optional startline endline)
  (if git-blamed-proc
      ;; Should maybe queue up a new run here
      (message "Already running git blame")
    (let ((display-buf (current-buffer))
          (blame-buf (get-buffer-create
                      (concat " git blame for " (buffer-name))))
          (args '("--incremental" "--contents" "-")))
      (if startline
          (setq args (append args
                             (list "-L" (format "%d,%d" startline endline)))))
      (setq args (append args
                         (list (file-name-nondirectory buffer-file-name))))
      (setq git-blamed-proc
            (apply 'start-process
                   "git-blamed" blame-buf
                   "git" "blame"
                   args))
      (with-current-buffer blame-buf
        (erase-buffer)
        (make-local-variable 'git-blamed-file)
        (make-local-variable 'git-blamed-current)
        (setq git-blamed-file display-buf)
        (setq git-blamed-current nil))
      (set-process-filter git-blamed-proc 'git-blamed-filter)
      (set-process-sentinel git-blamed-proc 'git-blamed-sentinel)
      (process-send-region git-blamed-proc (point-min) (point-max))
      (process-send-eof git-blamed-proc))))

(defun remove-git-blamed-text-properties (start end)
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties start end '(point-entered nil))
    (set-buffer-modified-p modified)))

(defun git-blamed-cleanup ()
  "Remove all blame properties"
  (mapc 'delete-overlay git-blamed-overlays)
  (setq git-blamed-overlays nil)
  (remove-git-blamed-text-properties (point-min) (point-max)))

(defun git-blamed-update-region (start end)
  "Rerun blame to get updates between START and END"
  (let ((overlays (overlays-in start end)))
    (while overlays
      (let ((overlay (pop overlays)))
        (if (< (overlay-start overlay) start)
            (setq start (overlay-start overlay)))
        (if (> (overlay-end overlay) end)
            (setq end (overlay-end overlay)))
        (setq git-blamed-overlays (delete overlay git-blamed-overlays))
        (delete-overlay overlay))))
  (remove-git-blamed-text-properties start end)
  ;; We can be sure that start and end are at line breaks
  (git-blamed-run (1+ (count-lines (point-min) start))
                  (count-lines (point-min) end)))

(defun git-blamed-sentinel (proc status)
  (with-current-buffer (process-buffer proc)
    (with-current-buffer git-blamed-file
      (setq git-blamed-proc nil)
      (if git-blamed-update-queue
          (git-blamed-delayed-update))))
  ;;(kill-buffer (process-buffer proc))
  ;;(message "git blame finished")
  )

(defvar in-blame-filter nil)

(defun git-blamed-filter (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    (insert-before-markers str)
    (goto-char 0)
    (unless in-blame-filter
      (let ((more t)
            (in-blame-filter t))
        (while more
          (setq more (git-blamed-parse)))))))

(defun git-blamed-parse ()
  (cond ((looking-at "\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)\n")
         (let ((hash (match-string 1))
               (src-line (string-to-number (match-string 2)))
               (res-line (string-to-number (match-string 3)))
               (num-lines (string-to-number (match-string 4))))
           (setq git-blamed-current
                 (if (string= hash "0000000000000000000000000000000000000000")
                     nil
                   (git-blamed-new-commit
                    hash src-line res-line num-lines))))
         (delete-region (point) (match-end 0))
         t)
        ((looking-at "filename \\(.+\\)\n")
         (let ((filename (match-string 1)))
           (git-blamed-add-info "filename" filename))
         (delete-region (point) (match-end 0))
         t)
        ((looking-at "\\([a-z-]+\\) \\(.+\\)\n")
         (let ((key (match-string 1))
               (value (match-string 2)))
           (git-blamed-add-info key value))
         (delete-region (point) (match-end 0))
         t)
        ((looking-at "boundary\n")
         (setq git-blamed-current nil)
         (delete-region (point) (match-end 0))
         t)
        (t
         nil)))

(defun git-blamed-new-commit (hash src-line res-line num-lines)
  (save-excursion
    (set-buffer git-blamed-file)
    (let ((info (gethash hash git-blamed-cache))
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t))
      (when (not info)
	;; Assign a random color to each new commit info
	;; Take care not to select the same color multiple times
	(let ((color (if git-blamed-colors
			 (git-blamed-random-pop git-blamed-colors)
		       git-blamed-ancient-color)))
          (setq info (list hash src-line res-line num-lines
                           (git-describe-commit hash)
                           (cons 'color color))))
        (puthash hash info git-blamed-cache))
      (goto-line res-line)
      (while (> num-lines 0)
        (if (get-text-property (point) 'git-blamed)
            (forward-line)
          (let* ((start (point))
                 (end (progn (forward-line 1) (point)))
                 (ovl (make-overlay start end)))
            (push ovl git-blamed-overlays)
            (overlay-put ovl 'git-blamed info)
            (overlay-put ovl 'help-echo hash)
            (overlay-put ovl 'face (list :background
                                         (cdr (assq 'color (nthcdr 5 info)))))
            ;; the point-entered property doesn't seem to work in overlays
            ;;(overlay-put ovl 'point-entered
            ;;             `(lambda (x y) (git-blamed-identify ,hash)))
            (let ((modified (buffer-modified-p)))
              (put-text-property (if (= start 1) start (1- start)) (1- end)
                                 'point-entered
                                 `(lambda (x y) (git-blamed-identify ,hash)))
              (set-buffer-modified-p modified))))
        (setq num-lines (1- num-lines))))))

(defun git-blamed-add-info (key value)
  (if git-blamed-current
      (nconc git-blamed-current (list (cons (intern key) value)))))

(defun git-blamed-current-commit ()
  (let ((info (get-char-property (point) 'git-blamed)))
    (if info
        (car info)
      (error "No commit info"))))

(defun git-describe-commit (hash)
  (with-temp-buffer
    (call-process "git" nil t nil
                  "log" "-1" "--pretty=format:\"%H %an -- %s\""
                  hash)
    (buffer-substring (point-min) (1- (point-max)))))

(defvar git-blamed-last-identification nil)
(make-variable-buffer-local 'git-blamed-last-identification)
(defun git-blamed-identify (&optional hash)
  (interactive)
  (let ((info (gethash (or hash (git-blamed-current-commit)) git-blamed-cache)))
    (when (and info (not (eq info git-blamed-last-identification)))
      (message "%s" (nth 4 info))
      (setq git-blamed-last-identification info))))

;; (defun git-blamed-after-save ()
;;   (when git-blamed-mode
;;     (git-blamed-cleanup)
;;     (git-blamed-run)))
;; (add-hook 'after-save-hook 'git-blamed-after-save)

(defun git-blamed-after-change (start end length)
  (when git-blamed-mode
    (git-blamed-enq-update start end)))

(defvar git-blamed-last-update nil)
(make-variable-buffer-local 'git-blamed-last-update)
(defun git-blamed-enq-update (start end)
  "Mark the region between START and END as needing blame update"
  ;; Try to be smart and avoid multiple callouts for sequential
  ;; editing
  (cond ((and git-blamed-last-update
              (= start (cdr git-blamed-last-update)))
         (setcdr git-blamed-last-update end))
        ((and git-blamed-last-update
              (= end (car git-blamed-last-update)))
         (setcar git-blamed-last-update start))
        (t
         (setq git-blamed-last-update (cons start end))
         (setq git-blamed-update-queue (nconc git-blamed-update-queue
                                              (list git-blamed-last-update)))))
  (unless (or git-blamed-proc git-blamed-idle-timer)
    (setq git-blamed-idle-timer
          (run-with-idle-timer 0.5 nil 'git-blamed-delayed-update))))

(defun git-blamed-delayed-update ()
  (setq git-blamed-idle-timer nil)
  (if git-blamed-update-queue
      (let ((first (pop git-blamed-update-queue))
            (inhibit-point-motion-hooks t))
        (git-blamed-update-region (car first) (cdr first)))))

(provide 'git-blamed)

;;; git-blamed.el ends here

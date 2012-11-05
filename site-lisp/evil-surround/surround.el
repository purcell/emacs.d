;;; surround.el --- emulate surround.vim from Vim

;; Copyright (C) 2010, 2011 Tim Harper
;;
;; Author: Tim Harper <timcharper at gmail dot com>
;;      Vegard Øye <vegard_oye at hotmail dot com>
;; Maintainer: Please send bug reports to the mailing list (below).
;; Created: July 23 2011
;; Version: 0.1
;; Keywords: emulation, vi, evil
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package emulates surround.vim by Tim Pope.
;; The functionality is wrapped into a minor mode. To enable
;; it globally, add the following lines to ~/.emacs:
;;
;;     (require 'surround)
;;     (global-surround-mode 1)
;;
;; Alternatively, you can enable surround-mode along a major mode
;; by adding `turn-on-surround-mode' to the mode hook.
;;
;; This package uses Evil as its vi layer. It is available from:
;;
;;     http://gitorious.org/evil

;;; Code:

(require 'evil)

(defgroup surround nil
  "surround.vim for Emacs"
  :prefix "surround-"
  :group 'evil)

(defcustom surround-pairs-alist
  '((?\( . ("( " . " )"))
    (?\[ . ("[ " . " ]"))
    (?\{ . ("{ " . " }"))

    (?\) . ("(" . ")"))
    (?\] . ("[" . "]"))
    (?\} . ("{" . "}"))

    (?# . ("#{" . "}"))
    (?b . ("(" . ")"))
    (?B . ("{" . "}"))
    (?> . ("<" . ">"))
    (?t . surround-read-tag)
    (?< . surround-read-tag))
  "Association list of surround items.
Each item is of the form (TRIGGER . (LEFT . RIGHT)), all strings.
Alternatively, a function can be put in place of (LEFT . RIGHT).
This only affects inserting pairs, not deleting or changing them."
  :group 'surround
  :type '(repeat (cons (regexp :tag "Key")
                       (symbol :tag "Surround pair"))))
(make-variable-buffer-local 'surround-pairs-alist)

(defvar surround-read-tag-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map ">" 'exit-minibuffer)
    map)
  "Keymap used by `surround-read-tag'.")

(defun surround-read-tag ()
  "Read a XML tag from the minibuffer."
  (let* ((input (read-from-minibuffer "<" "" surround-read-tag-map))
         (match (string-match "\\([0-9a-z-]+\\)\\(.*?\\)[>]*$" input))
         (tag  (match-string 1 input))
         (rest (match-string 2 input)))
    (cons (format "<%s%s>" (or tag "") (or rest ""))
          (format "</%s>" (or tag "")))))

(defun surround-pair (char)
  "Return the surround pair of char.
This is a cons cell (LEFT . RIGHT), both strings."
  (let ((pair (assoc-default char surround-pairs-alist)))
    (cond
     ((functionp pair)
      (funcall pair))

     ((consp pair)
      pair)

     (t
      (cons (format "%c" char) (format "%c" char))))))

(defun surround-outer-overlay (char)
  "Return outer overlay for the delimited range represented by CHAR.
This overlay includes the delimiters.
See also `surround-inner-overlay'."
  (let ((outer (lookup-key evil-outer-text-objects-map (string char))))
    (when (functionp outer)
      (setq outer (funcall outer))
      (when (evil-range-p outer)
        (setq outer (make-overlay (evil-range-beginning outer)
                                  (evil-range-end outer)
                                  nil nil t))))))

(defun surround-trim-whitespace-from-range (range &optional regexp)
  "Given an evil-range, trim whitespace around range by shrinking the range such that it neither begins nor ends with whitespace. Does not modify the buffer."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (save-excursion
      (save-match-data
        (goto-char (evil-range-beginning range))
        (while (looking-at regexp) (forward-char))
        (evil-set-range-beginning range (point))
        (goto-char (evil-range-end range))
        (while (looking-back regexp) (backward-char))
        (evil-set-range-end range (point))))))

(defun surround-inner-overlay (char)
  "Return inner overlay for the delimited range represented by CHAR.
This overlay excludes the delimiters.
See also `surround-outer-overlay'."
  (let ((inner (lookup-key evil-inner-text-objects-map (string char))))
    (when (functionp inner)
      (setq inner (funcall inner))
      (when (evil-range-p inner)
        (when (eq (char-syntax char) ?\()
          (surround-trim-whitespace-from-range inner "[ \t]"))
        (setq inner (make-overlay (evil-range-beginning inner)
                                  (evil-range-end inner)
                                  nil nil t))))))

(defun surround-delete (char &optional outer inner)
  "Delete the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with
the overlays OUTER and INNER, where OUTER includes the delimiters
and INNER excludes them. The intersection (i.e., difference)
between these overlays is what is deleted."
  (interactive "c")
  (cond
   ((and outer inner)
    (delete-region (overlay-start outer) (overlay-start inner))
    (delete-region (overlay-end inner) (overlay-end outer))
    (goto-char (overlay-start outer)))
   (t
    ;; no overlays specified: create them on the basis of CHAR
    ;; and delete after use
    (let* ((outer (surround-outer-overlay char))
           (inner (surround-inner-overlay char)))
      (unwind-protect
          (when (and outer inner)
            (surround-delete char outer inner))
        (when outer (delete-overlay outer))
        (when inner (delete-overlay inner)))))))

(defun surround-change (char &optional outer inner)
  "Change the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with the
overlays OUTER and INNER, which are passed to `surround-delete'."
  (interactive "c")
  (cond
   ((and outer inner)
    (surround-delete char outer inner)
    (surround-region (overlay-start outer)
                     (overlay-end outer)
                     nil (read-char)))
   (t
    (let* ((outer (surround-outer-overlay char))
           (inner (surround-inner-overlay char)))
      (unwind-protect
          (when (and outer inner)
            (surround-change char outer inner))
        (when outer (delete-overlay outer))
        (when inner (delete-overlay inner)))))))

;; Dispatcher function in Operator-Pending state.
;; "cs" calls `surround-change', "ds" calls `surround-delete',
;; and "ys" calls `surround-region'.
(evil-define-command surround-edit (operation)
  "Edit the surrounding delimiters represented by CHAR.
If OPERATION is `change', call `surround-change'.
if OPERATION is `surround', call `surround-region'.
Otherwise call `surround-delete'."
  (interactive
   (progn
     ;; abort the calling operator
     (setq evil-inhibit-operator t)
     (list (assoc-default evil-this-operator
                          '((evil-change . change)
                            (evil-delete . delete))))))
  (cond
   ((eq operation 'change)
    (call-interactively 'surround-change))
   ((eq operation 'delete)
    (call-interactively 'surround-delete))
   (t
    (define-key evil-operator-shortcut-map "s" 'evil-line)
    (call-interactively 'surround-region))))

(evil-define-operator surround-region (beg end type char &optional force-new-line)
  "Surround BEG and END with CHAR.

When force-new-line is true, and region type is not line, the
following: (vertical bars indicate region start/end points)

   do |:thing|

Becomes this:

   do {
     :thing
   }"

  (interactive "<R>c")
  (let* ((overlay (make-overlay beg end nil nil t))
         (pair (surround-pair char))
         (open (car pair))
         (close (cdr pair)))
    (unwind-protect
        (progn
          (goto-char (overlay-start overlay))

          (cond ((eq type 'line)
                 (insert open)
                 (indent-according-to-mode)
                 (newline-and-indent)
                 (goto-char (overlay-end overlay))
                 (insert close)
                 (indent-according-to-mode)
                 (newline))

                (force-new-line
                 (insert open)
                 (indent-according-to-mode)
                 (newline-and-indent)
                 (goto-char (overlay-end overlay))
                 (newline-and-indent)
                 (insert close))

                (t
                 (insert open)
                 (goto-char (overlay-end overlay))
                 (insert close)))
          (goto-char (overlay-start overlay)))
      (delete-overlay overlay))))

(evil-define-operator Surround-region (beg end type char)
  "Call surround-region, toggling force-new-line"
  (interactive "<R>c")
  (surround-region beg end type char t))

(define-minor-mode surround-mode
  "Buffer-local minor mode to emulate surround.vim."
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

(defun turn-on-surround-mode ()
  "Enable surround-mode in the current buffer."
  (surround-mode 1))

(defun turn-off-surround-mode ()
  "Disable surround-mode in the current buffer."
  (surround-mode -1))

(define-globalized-minor-mode global-surround-mode
  surround-mode turn-on-surround-mode
  "Global minor mode to emulate surround.vim.")

(evil-define-key 'operator surround-mode-map "s" 'surround-edit)
(evil-define-key 'visual surround-mode-map "s" 'surround-region)
(evil-define-key 'visual surround-mode-map "S" 'Surround-region)

(provide 'surround)

;;; surround.el ends here

;;; evil-nerd-commenter --- Comment/uncomment lines efficiently. Like Nerd Commenter in Vim

;; Copyright (C) 2013 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-nerd-commenter
;; Version: 1.5.0
;; Keywords: commenter vim line evil
;;
;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-nerd-commenter
;;
;; evil-nerd-commenter is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-nerd-commenter is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This program emulates nerd-commenter.vim by Marty Grenfell.
;;
;; It help you comment/uncomment multiple lines without selecting them.
;;
;; `M-x evilnc-default-hotkeys` assigns hotkey `M-;` to `evilnc-comment-or-uncomment-lines`
;;
;; `M-x evilnc-comment-or-uncomment-lines` comment or uncomment lines.
;;
;; `M-x evilnc-quick-comment-or-uncomment-to-the-line` will comment/uncomment from current line to
;; the specified line number. The last digit(s) of line number is parameter of the command.
;;
;; For example, `C-u 9 evilnc-quick-comment-or-uncomment-to-the-line` will comment code from
;; current line to line 99 if you current line is 91.
;;
;; Check README for more use cases.
;;
;; Though this program could be used *independently*, I highly recommend you use it with
;; evil (http://gitorious.org/evil)
;;
;; Evil makes you take advantage of power of Vi to comment lines in shocking speed.
;; For example, you can press key `99,ci` to comment out 99 lines.


;;; Code:

;; Example, press ",,a{" will change C code:
;;   {printf("hello");} => /* {printf("hello");}*/
;; google "vim text object for more syntax"
(defcustom evilnc-hotkey-comment-operator ",," "The hot key for evilnc-comment-operator to (un)comment text object"
  :type 'string
  :group 'evil-nerd-commenter)

(defvar evilnc-invert-comment-line-by-line nil "if t then invert region comment status line by line")

;; shamelessly copied from goto-line
(defun evilnc--goto-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line))))
  )

(defun evilnc--fix-buggy-major-modes ()
  "fix major modes whose comment regex is buggy.
@see http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-03/msg00891.html"
  (when (string= major-mode "autoconf-mode")
    ;; since comment-use-syntax is nil in autoconf.el, the comment-start-skip need
    ;; make sure the its first parenthesized expression match the string exactly before
    ;; the "dnl", check the comment-start-skip in lisp-mode may give you some hint.
    ;; See code in (defun comment-search-forward) from emacs 24.2.1:
    ;; (if (not comment-use-syntax)
    ;;     (if (re-search-forward comment-start-skip limit noerror)
    ;;     (or (match-end 1) (match-beginning 0))
    ;; My regex make sure (match-end 1) return the position of comment starter
    (when (and (boundp 'comment-use-syntax) (not comment-use-syntax))
        ;; Maybe autoconf.el will (setq comment-use-syntax t) in the future?
        (setq comment-start-skip "^\\(\\s*\\)\\(dnl\\|#\\) +")
      )
    )
  )

(defun evilnc--operation-on-lines-or-region (fn &optional NUM)
  (if (not (region-active-p))
      (let ((b (line-beginning-position))
            e)
        (save-excursion
          (forward-line (- NUM 1))
          (setq e (line-end-position))
          )
        (funcall fn b e)
        )
    ;; expand selected region
    (progn
      (save-excursion
        (let ((b (region-beginning))
              (e (region-end))
              )
          ;; another work around for evil-visual-line bug:
          ;; in evil-mode, if we use hot key V `M-x evil-visual-line` to select line
          ;; the (line-beginning-position) of the line which is after the last selected
          ;; line is always (region-end)! Don't know why.
          (if (and (> e b) (= e (line-beginning-position)) (boundp 'evil-state) (string= evil-state 'visual))
              (setq e (1- e))
            )
          (goto-char b)
          (setq b (line-beginning-position))
          (goto-char e)
          (setq e (line-end-position))
          (funcall fn b e)
          ))
      )
    )
  )


(defun evilnc--get-one-paragraph-region ()
  (let (b e)
    (save-excursion
      (setq b (re-search-backward "^[ \t]*$" nil t))
      (if b (progn
              (forward-line)
              (setq b (line-beginning-position))
              )
        (setq b 1)
        )
      )
    (save-excursion

      (setq e (re-search-forward "^[ \t]*$" nil t))
      (if e (progn
              (forward-line -1)
              (setq e (line-end-position))
              )
        (setq e (point-max))
        )
      )
    (list b e)
    )
  )

(defun evilnc--in-comment-p (pos)
  (interactive)
  (let ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar #'(lambda (f)
                      ;; learn this trick from flyspell
                      (or (string= f 'font-lock-comment-face)
                          (string= f 'font-lock-comment-delimiter-face)))
                  fontfaces))))

;; @return (list beg end)
(defun evilnc--extend-to-whole-comment (beg end)
  (interactive)
  (if (evilnc--in-comment-p beg)
      (save-excursion
        (let ((newbeg beg)
              (newend end))

          ;; extend the beginning
          (goto-char newbeg)
          (while (and (>= newbeg (line-beginning-position)) (evilnc--in-comment-p newbeg))
            (decf newbeg)
            )
          ;; make sure newbeg is at the beginning of the comment
          (if (< newbeg beg) (incf newbeg))

          ;; extend the end
          (goto-char newend)
          (while (and (<= newend (line-end-position)) (evilnc--in-comment-p newend))
            (incf newend)
            )
          ;; make sure newend is at the end of the comment
          (if (> newend end) (decf newend))

          (list newbeg newend)
          )
        )
    (list beg end)
    ))

(defun evilnc--invert-comment (beg end)
  "scan the region line by line, invert its comment status"
  (let (done b e)
    (save-excursion
      (goto-char end)
      ;; comment (line-beginning-position line-end-position)
      ;; (setq old-b (line-beginning-position)
      ;; (forward-line -1)
      ;; (if (= old-b (line-beginning-position)) we did not move, out of loop
      ;; (if (<= (line-end-position) beg)), out of region, out of loop
      (while (not done)
        (setq b (line-beginning-position))
        (setq e (line-end-position))
        (funcall (if (comment-only-p b e)
                     'uncomment-region 'comment-region)
                 b e)

        (forward-line -1)
        (when (or (= (line-beginning-position) b) (< (line-end-position) beg))
          (setq done t))
        ))))

(defun evilnc--working-on-region (beg end fn)
  (let (info
        lang
        lang-f
        old-flag)
    (when (and (string= major-mode "org-mode")
               (fboundp 'org-edit-src-find-region-and-lang))
      (setq info (org-edit-src-find-region-and-lang)))

    (when info
      (setq lang (or (cdr (assoc (nth 2 info) org-src-lang-modes))
                     (nth 2 info)))
      (setq lang (if (symbolp lang) (symbol-name lang) lang))
      (setq lang-f (intern (concat lang "-mode")))
      )

    ;; turn on 3rd party language's major-mode temporarily
    (if lang-f (funcall lang-f))

    (if evilnc-invert-comment-line-by-line
        (evilnc--invert-comment beg end)
      (funcall fn beg end))

    ;; turn off  3rd party language's major-mode temporarily and clean the shit
    (when lang-f
      (setq old-flag org-inhibit-startup-visibility-stuff)
      ;; avoid org file automatically collapsed
      (setq org-inhibit-startup-visibility-stuff t)
      (org-mode)
      (setq org-inhibit-startup-visibility-stuff old-flag))
    ))

(defun evilnc--comment-or-uncomment-region (beg end)
  (cond
   ((string= major-mode "web-mode")
    ;; web-mode comment only works when region selected
    ;; uncomment only works when region not selected
    ;; test three sample point, comment or uncomment
    (cond
     ((and (save-excursion
             (goto-char beg)
             (goto-char (line-end-position))
             (web-mode-is-comment))
           (web-mode-is-comment (/ (+ beg end) 2))
           (save-excursion
             (goto-char end)
             (goto-char (line-beginning-position))
             (web-mode-is-comment))
           )
      ;; don't know why, but we need goto the middle of comment
      ;; in order to uncomment, or else trailing spaces will be appended
      (goto-char (/ (+ beg end) 2))
      (web-mode-uncomment (/ (+ beg end) 2))
      )
     (t
      (when (not (region-active-p))
        (push-mark beg t t)
        (goto-char end))
      (web-mode-comment (/ (+ beg end) 2)))
     )
    )
    (t
     (evilnc--working-on-region beg end 'comment-or-uncomment-region))
    ))

(defun evilnc--current-line-num ()
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun evilnc--find-dst-line-num (UNITS)
  (let ((cur-line-num (evilnc--current-line-num))
        dst-line-num)
    (if (>= (mod cur-line-num 10) UNITS)
        (setq UNITS (+ UNITS 10))
      )
    (setq dst-line-num (+ cur-line-num (- UNITS (mod cur-line-num 10))))
    ))

;; ==== below this line are public commands
;;;###autoload
(defun evilnc-comment-or-uncomment-paragraphs (&optional NUM)
  "Comment or uncomment paragraph(s). A paragraph is a continuation non-empty lines.
Paragraphs are separated by empty lines."
  (interactive "p")
  (let ((i 0)
        rlt
        (b (point-max))
        (e 0)
        )
    (catch 'break
      (while (< i NUM)
        (incf i)
        (setq rlt (evilnc--get-one-paragraph-region))
        (setq b (if (< (nth 0 rlt) b) (nth 0 rlt) b))
        (setq e (if (> (nth 1 rlt) e) (nth 1 rlt) e))

        ;; prepare for the next paragraph
        (if (and rlt (< i NUM))
            (progn
              ;; e should be the end of last non-empty line
              (goto-char e)

              ;; move to an empty line
              (forward-line)

              ;; move to next non-empty line
              (re-search-forward "^[ \t]*[^ \t]" nil t)

              (if (<= (line-beginning-position) e)
                  (throw 'break i)))
          (throw 'break i))
        ))
    (when (<= b e)
      (save-excursion
        (evilnc--fix-buggy-major-modes)
        (evilnc--comment-or-uncomment-region b e)
        ))
    ))

;;;###autoload
(defun evilnc-comment-or-uncomment-to-the-line (&optional LINENUM)
  "Comment or uncomment from the current line to the LINENUM line"
  (interactive "p")
  (if (not (region-active-p))
      (let ((b (line-beginning-position))
            (e (line-end-position)))
        (save-excursion
          (evilnc--goto-line LINENUM)
          (if (< (line-beginning-position) b)
              (setq b (line-beginning-position)))
          (if (> (line-end-position) e)
              (setq e (line-end-position)))
          (evilnc--fix-buggy-major-modes)
          (evilnc--comment-or-uncomment-region b e)
          ))))

;;;###autoload
(defun evilnc-quick-comment-or-uncomment-to-the-line (&optional UNITS)
  "Comment or uncomment to line number by specifying its last digit(s)
For exmaple, you can use 'C-u 53 M-x evilnc-quick-comment-or-uncomment-to-the-line'
or 'C-u 3 M-x evilnc-quick-comment-or-uncomment-to-the-line' to comment to the line 6453"
  (interactive "p")
  (let ((dst-line-num (evilnc--find-dst-line-num UNITS)))
    (evilnc-comment-or-uncomment-to-the-line dst-line-num)
    (evilnc--goto-line (+ 1 dst-line-num))
    ))

;;;###autoload
(defun evilnc-toggle-invert-comment-line-by-line ()
  (interactive)
  (if evilnc-invert-comment-line-by-line
      (setq evilnc-invert-comment-line-by-line nil)
    (setq evilnc-invert-comment-line-by-line t)
    )
  (message (if evilnc-invert-comment-line-by-line
               "Each line's comment status will be inverted"
             "Each line's comment status will NOT be inverted")))

;;;###autoload
(defun evilnc-toggle-comment-empty-lines ()
  (interactive)
  (if comment-empty-lines
      (setq comment-empty-lines nil)
    (setq comment-empty-lines t)
    )
  (message (if comment-empty-lines
               "Empty line(s) will be commented"
             "Empty line(s) will NOT be commented")))

;;;###autoload
(defun evilnc-comment-or-uncomment-lines (&optional NUM)
  "Comment or uncomment NUM lines. NUM could be negative.
   Case 1: If no region selected, comment/uncomment on current line. if NUM>1, comment/uncomment
   extra N-1 lines from next line
   Case 2: If a region selected, the region is expand to make sure the region contain
   whole lines. Then we comment/uncomment the expanded region. NUM is ignored."
  (interactive "p")
  ;; donot move the cursor
  (save-excursion
    ;; support negative number
    (when (< NUM 0)
      (forward-line (1+ NUM))
      (setq NUM (- 0 NUM)))

    (evilnc--operation-on-lines-or-region '(lambda (b e)
                                             (evilnc--fix-buggy-major-modes)
                                             (evilnc--comment-or-uncomment-region b e)
                                             )
                                          NUM)))

;;;###autoload
(defun evilnc-copy-and-comment-lines (&optional NUM)
  "Copy and paste NUM lines. Then comment the original lines. NUM could be negative.
   Case 1: If no region selected, operate on current line. if NUM>1, comment/uncomment
   extra N-1 lines from next line
   Case 2: If a region selected, the region is expand to make sure the region contain
   whole lines. Then we operate the expanded region. NUM is ignored.
"
  (interactive "p")

  ;; support negative number
  (when (< NUM 0)
    (forward-line (1+ NUM))
    (setq NUM (- 0 NUM)))

  (evilnc--operation-on-lines-or-region
   '(lambda (beg end)
      (evilnc--fix-buggy-major-modes)
      (let ((str (buffer-substring-no-properties beg end)))
        (goto-char end)
        (newline 1)
        (insert-before-markers str)
        (comment-region beg end)
        ))
   NUM))

;; {{ for non-evil user only
;;;###autoload
(defun evilnc-copy-to-line (&optional LINENUM)
  "Copy from the current line to the LINENUM line, for non-evil user only"
  (interactive "p")
  (if (not (region-active-p))
      (let ((b (line-beginning-position))
            (e (line-end-position)))
        (save-excursion
          (evilnc--goto-line LINENUM)
          (if (< (line-beginning-position) b)
              (setq b (line-beginning-position)))
          (if (> (line-end-position) e)
              (setq e (line-end-position)))
          (kill-new (buffer-substring-no-properties b e))
          ))))

;;;###autoload
(defun evilnc-kill-to-line (&optional LINENUM)
  "Kill from the current line to the LINENUM line, for non-evil user only"
  (interactive "p")
  (if (not (region-active-p))
      (let ((b (line-beginning-position))
            (e (line-end-position)))
        (save-excursion
          (evilnc--goto-line LINENUM)
          (if (< (line-beginning-position) b)
              (setq b (line-beginning-position)))
          (if (> (line-end-position) e)
              (setq e (line-end-position)))
          (kill-region b (+ 1 e)) ; +1 because we need remove the CR
          ))))

;;;###autoload
(defun evilnc-default-hotkeys ()
  "Set the hotkeys of evil-nerd-comment"
  (interactive)
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
  (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
  (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
  (eval-after-load 'evil
    '(progn
       (define-key evil-normal-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
       (define-key evil-normal-state-map ",cl" 'evilnc-quick-comment-or-uncomment-to-the-line)
       (define-key evil-normal-state-map ",ll" 'evilnc-quick-comment-or-uncomment-to-the-line)
       (define-key evil-normal-state-map ",cc" 'evilnc-copy-and-comment-lines)
       (define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
       (define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)
       (define-key evil-normal-state-map ",cv" 'evilnc-toggle-invert-comment-line-by-line))))

;; Attempt to define the operator on first load.
;; Will only work if evil has been loaded
(eval-after-load 'evil
  '(progn
     (evil-define-operator evilnc-comment-operator (beg end type register yank-handler)
       "Comments text from BEG to END with TYPE.
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
         (let ((newpos (evilnc--extend-to-whole-comment beg end) ))
           (evil-apply-on-block #'evilnc--comment-or-uncomment-region (nth 0 newpos) (nth 1 newpos) nil)
           )
         )
        ((and (eq type 'line)
              (= end (point-max))
              (or (= beg end)
                  (/= (char-before end) ?\n))
              (/= beg (point-min))
              (=  (char-before beg) ?\n))
         (evilnc--comment-or-uncomment-region (1- beg) end))
        ((eq type 'line)
           (evilnc--comment-or-uncomment-region beg end))
        (t
         (let ((newpos (evilnc--extend-to-whole-comment beg end) ))
           (evilnc--comment-or-uncomment-region (nth 0 newpos) (nth 1 newpos))
           )
         ))
       ;; place cursor on beginning of line
       (when (and (evil-called-interactively-p)
                  (eq type 'line))
         (evil-first-non-blank)))
     (define-key evil-normal-state-map evilnc-hotkey-comment-operator 'evilnc-comment-operator)
     (define-key evil-visual-state-map evilnc-hotkey-comment-operator 'evilnc-comment-operator)
     ))

(provide 'evil-nerd-commenter)

;;; evil-nerd-commenter.el ends here

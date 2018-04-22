;;; beacon.el --- Highlight the cursor whenever the window scrolls  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: https://github.com/Malabarba/beacon
;; Package-Version: 20170404.906
;; Keywords: convenience
;; Version: 1.3.3
;; Package-Requires: ((seq "2.14"))

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

;; This is a global minor-mode. Turn it on everywhere with:
;; ┌────
;; │ (beacon-mode 1)
;; └────
;;
;; Whenever the window scrolls a light will shine on top of your cursor so
;; you know where it is.
;;
;; That’s it.
;;
;; See the accompanying Readme.org for configuration details.

;;; Code:

(require 'seq)
(require 'faces)
(unless (fboundp 'seq-mapn)
  ;; This is for people who are on outdated Emacs snapshots. Will be
  ;; deleted in a couple of weeks.
  (defun seq-mapn (function sequence &rest sequences)
    "Like `seq-map' but FUNCTION is mapped over all SEQUENCES.
The arity of FUNCTION must match the number of SEQUENCES, and the
mapping stops on the shortest sequence.
Return a list of the results.

\(fn FUNCTION SEQUENCES...)"
    (let ((result nil)
          (sequences (seq-map (lambda (s) (seq-into s 'list))
                            (cons sequence sequences))))
      (while (not (memq nil sequences))
        (push (apply function (seq-map #'car sequences)) result)
        (setq sequences (seq-map #'cdr sequences)))
      (nreverse result))))

(defgroup beacon nil
  "Customization group for beacon."
  :group 'emacs
  :prefix "beacon-")

(defvar beacon--timer nil)

(defcustom beacon-push-mark 35
  "Should the mark be pushed before long movements?
If nil, `beacon' will not push the mark.
Otherwise this should be a number, and `beacon' will push the
mark whenever point moves more than that many lines."
  :type '(choice integer (const nil)))

(defcustom beacon-blink-when-point-moves-vertically nil
  "Should the beacon blink when moving a long distance vertically?
If nil, don't blink due to vertical movement.
If non-nil, this should be an integer, which is the minimum
movement distance (in lines) that triggers a beacon blink."
  :type '(choice integer (const nil)))

(defcustom beacon-blink-when-point-moves-horizontally nil
  "Should the beacon blink when moving a long distance horizontally?
If nil, don't blink due to horizontal movement.
If non-nil, this should be an integer, which is the minimum
movement distance (in columns) that triggers a beacon blink."
  :type '(choice integer (const nil)))

(defcustom beacon-blink-when-buffer-changes t
  "Should the beacon blink when changing buffer?"
  :type 'boolean)

(defcustom beacon-blink-when-window-scrolls t
  "Should the beacon blink when the window scrolls?"
  :type 'boolean)

(defcustom beacon-blink-when-window-changes t
  "Should the beacon blink when the window changes?"
  :type 'boolean)

(defcustom beacon-blink-when-focused nil
  "Should the beacon blink when Emacs gains focus?
Note that, due to a limitation of `focus-in-hook', this might
trigger false positives on some systems."
  :type 'boolean
  :package-version '(beacon . "0.2"))

(defcustom beacon-blink-duration 0.3
  "Time, in seconds, that the blink should last."
  :type 'number)

(defcustom beacon-blink-delay 0.3
  "Time, in seconds, before starting to fade the beacon."
  :type 'number)

(defcustom beacon-size 40
  "Size of the beacon in characters."
  :type 'number)

(defcustom beacon-color 0.5
  "Color of the beacon.
This can be a string or a number.

If it is a number, the color is taken to be white or
black (depending on the current theme's background) and this
number is a float between 0 and 1 specifing the brightness.

If it is a string, it is a color name or specification,
e.g. \"#666600\"."
  :type '(choice number color))

(defface beacon-fallback-background
  '((((class color) (background light)) (:background "black"))
    (((class color) (background dark)) (:background "white")))
  "Fallback beacon background color.
Used in cases where the color can't be determined by Emacs.
Only the background of this face is used.")

(defvar beacon-dont-blink-predicates nil
  "A list of predicates that prevent the beacon blink.
These predicate functions are called in order, with no
arguments, before blinking the beacon.  If any returns
non-nil, the beacon will not blink.

For instance, if you want to disable beacon on buffers where
`hl-line-mode' is on, you can do:

    (add-hook \\='beacon-dont-blink-predicates
              (lambda () (bound-and-true-p hl-line-mode)))")

(defun beacon--compilation-mode-p ()
  "Non-nil if this is some form of compilation mode."
  (or (derived-mode-p 'compilation-mode)
      (bound-and-true-p compilation-minor-mode)))

(add-hook 'beacon-dont-blink-predicates #'window-minibuffer-p)
(add-hook 'beacon-dont-blink-predicates #'beacon--compilation-mode-p)

(defcustom beacon-dont-blink-major-modes '(t magit-status-mode magit-popup-mode
                                       inf-ruby-mode
                                       mu4e-headers-mode
                                       gnus-summary-mode gnus-group-mode)
  "A list of major-modes where the beacon won't blink.
Whenever the current buffer satisfies `derived-mode-p' for
one of the major-modes on this list, the beacon will not
blink."
  :type '(repeat symbol))

(defcustom beacon-dont-blink-commands '(next-line previous-line
                                            forward-line)
  "A list of commands that should not make the beacon blink.
Use this for commands that scroll the window in very
predictable ways, when the blink would be more distracting
than helpful.."
  :type '(repeat symbol))

(defcustom beacon-before-blink-hook nil
  "Hook run immediately before blinking the beacon."
  :type 'hook)


;;; Internal variables
(defvar beacon--window-scrolled nil)
(defvar beacon--previous-place nil)
(defvar beacon--previous-mark-head nil)
(defvar beacon--previous-window nil)
(defvar beacon--previous-window-start 0)

(defun beacon--record-vars ()
  (unless (window-minibuffer-p)
    (setq beacon--previous-mark-head (car mark-ring))
    (setq beacon--previous-place (point-marker))
    (setq beacon--previous-window (selected-window))
    (setq beacon--previous-window-start (window-start))))


;;; Overlays
(defvar beacon--ovs nil)

(defconst beacon-overlay-priority (/ most-positive-fixnum 2)
  "Priotiy used on all of our overlays.")

(defun beacon--make-overlay (length &rest properties)
  "Put an overlay at point with background COLOR."
  (let ((ov (make-overlay (point) (+ length (point)))))
    (overlay-put ov 'beacon t)
    ;; Our overlay is very temporary, so we take the liberty of giving
    ;; it a high priority.
    (overlay-put ov 'priority beacon-overlay-priority)
    (overlay-put ov 'window (selected-window))
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    (push ov beacon--ovs)
    ov))

(defun beacon--colored-overlay (color)
  "Put an overlay at point with background COLOR."
  (beacon--make-overlay 1 'face (list :background color)))

(defun beacon--ov-put-after-string (overlay colors)
  "Add an after-string property to OVERLAY.
The property's value is a string of spaces with background
COLORS applied to each one.
If COLORS is nil, OVERLAY is deleted!"
  (if (not colors)
      (when (overlayp overlay)
        (delete-overlay overlay))
    (overlay-put overlay 'beacon-colors colors)
    (overlay-put overlay 'after-string
                 (propertize
                  (mapconcat (lambda (c) (propertize " " 'face (list :background c)))
                             colors
                             "")
                  'cursor 1000))))

(defun beacon--after-string-overlay (colors)
  "Put an overlay at point with an after-string property.
The property's value is a string of spaces with background
COLORS applied to each one."
  ;; The after-string must not be longer than the remaining columns
  ;; from point to right window-end else it will be wrapped around.
  (let ((colors (seq-take colors (- (window-width) (current-column)))))
    (beacon--ov-put-after-string (beacon--make-overlay 0) colors)))

(defun beacon--ov-at-point ()
  (car (or (seq-filter (lambda (o) (overlay-get o 'beacon))
                       (overlays-in (point) (point)))
           (seq-filter (lambda (o) (overlay-get o 'beacon))
                       (overlays-at (point))))))

(defun beacon--vanish (&rest _)
  "Turn off the beacon."
  (unless (string-match "\\` \\*\\(temp-buffer\\|Echo Area.*\\)\\*"
                        (buffer-name))
    (when (timerp beacon--timer)
      (cancel-timer beacon--timer))
    (mapc #'delete-overlay beacon--ovs)
    (setq beacon--ovs nil)))


;;; Colors
(defun beacon--int-range (a b)
  "Return a list of integers between A inclusive and B exclusive.
Only returns `beacon-size' elements."
  (let ((d (/ (- b a) beacon-size))
        (out (list a)))
    (dotimes (_ (1- beacon-size))
      (push (+ (car out) d) out))
    (nreverse out)))

(defun beacon--color-range ()
  "Return a list of background colors for the beacon."
  (let* ((default-bg (or (save-excursion
                           (unless (eobp)
                             (forward-line 1)
                             (unless (or (bobp) (not (bolp)))
                               (forward-char -1)))
                           (background-color-at-point))
                         (face-background 'default)))
         (bg (color-values (if (or (not (stringp default-bg))
                                   (string-match "\\`unspecified-" default-bg))
                               (face-attribute 'beacon-fallback-background :background)
                             default-bg)))
         (fg (cond
              ((stringp beacon-color) (color-values beacon-color))
              ((and (stringp bg)
                    (< (color-distance "black" bg)
                       (color-distance "white" bg)))
               (make-list 3 (* beacon-color 65535)))
              (t (make-list 3 (* (- 1 beacon-color) 65535))))))
    (when bg
      (apply #'seq-mapn (lambda (r g b) (format "#%04x%04x%04x" r g b))
             (mapcar (lambda (n) (butlast (beacon--int-range (elt fg n) (elt bg n))))
                     [0 1 2])))))


;;; Blinking
(defun beacon--shine ()
  "Shine a beacon at point."
  (let ((colors (beacon--color-range)))
    (save-excursion
      (while colors
        (if (looking-at "$")
            (progn
              (beacon--after-string-overlay colors)
              (setq colors nil))
          (beacon--colored-overlay (pop colors))
          (forward-char 1))))))

(defun beacon--dec ()
  "Decrease the beacon brightness by one."
  (pcase (beacon--ov-at-point)
    (`nil (beacon--vanish))
    ((and o (let c (overlay-get o 'beacon-colors)) (guard c))
     (beacon--ov-put-after-string o (cdr c)))
    (o
     (delete-overlay o)
     (save-excursion
       (while (and (condition-case nil
                       (progn (forward-char 1) t)
                     (end-of-buffer nil))
                   (setq o (beacon--ov-at-point)))
         (let ((colors (overlay-get o 'beacon-colors)))
           (if (not colors)
               (move-overlay o (1- (point)) (point))
             (forward-char -1)
             (beacon--colored-overlay (pop colors))
             (beacon--ov-put-after-string o colors)
             (forward-char 1))))))))

;;;###autoload
(defun beacon-blink ()
  "Blink the beacon at the position of the cursor.
Unlike `beacon-blink-automated', the beacon will blink
unconditionally (even if `beacon-mode' is disabled), and this can
be invoked as a user command or called from lisp code."
  (interactive)
  (beacon--vanish)
  (run-hooks 'beacon-before-blink-hook)
  (beacon--shine)
  (setq beacon--timer
        (run-at-time beacon-blink-delay
                     (/ beacon-blink-duration 1.0 beacon-size)
                     #'beacon--dec)))

(defun beacon-blink-automated ()
  "If appropriate, blink the beacon at the position of the cursor.
Unlike `beacon-blink', the blinking is conditioned on a series of
variables: `beacon-mode', `beacon-dont-blink-commands',
`beacon-dont-blink-major-modes', and
`beacon-dont-blink-predicates'."
  ;; Record vars here in case something is blinking outside the
  ;; command loop.
  (beacon--record-vars)
  (unless (or (not beacon-mode)
              (run-hook-with-args-until-success 'beacon-dont-blink-predicates)
              (seq-find #'derived-mode-p beacon-dont-blink-major-modes)
              (memq (or this-command last-command) beacon-dont-blink-commands))
    (beacon-blink)))


;;; Movement detection
(defun beacon--movement-> (delta-y &optional delta-x)
  "Return non-nil if latest vertical movement is > DELTA-Y.
If DELTA-Y is nil, return nil.
The same is true for DELTA-X and horizonta movement."
  (and delta-y
       (markerp beacon--previous-place)
       (equal (marker-buffer beacon--previous-place)
              (current-buffer))
       ;; Quick check that prevents running the code below in very
       ;; short movements (like typing).
       (> (abs (- (point) beacon--previous-place))
          delta-y)
       ;; Col movement.
       (or (and delta-x
                (> (abs (- (current-column)
                           (save-excursion
                             (goto-char beacon--previous-place)
                             (current-column))))
                   delta-x))
           ;; Check if the movement was >= DELTA lines by moving DELTA
           ;; lines. `count-screen-lines' is too slow if the movement had
           ;; thousands of lines.
           (save-excursion
             (let ((p (point)))
               (goto-char (min beacon--previous-place p))
               (vertical-motion delta-y)
               (> (max p beacon--previous-place)
                  (line-beginning-position)))))))

(defun beacon--maybe-push-mark ()
  "Push mark if it seems to be safe."
  (when (and (not mark-active)
             (beacon--movement-> beacon-push-mark))
    (let ((head (car mark-ring)))
      (when (and (eq beacon--previous-mark-head head)
                 (not (equal head beacon--previous-place)))
        (push-mark beacon--previous-place 'silent)))))

(defun beacon--post-command ()
  "Blink if point moved very far."
  (cond
   ;; Sanity check.
   ((not (markerp beacon--previous-place)))
   ;; Blink for switching buffers.
   ((and beacon-blink-when-buffer-changes
         (not (eq (marker-buffer beacon--previous-place)
                  (current-buffer))))
    (beacon-blink-automated))
   ;; Blink for switching windows.
   ((and beacon-blink-when-window-changes
         (not (eq beacon--previous-window (selected-window))))
    (beacon-blink-automated))
   ;; Blink for scrolling.
   ((and beacon--window-scrolled
         (equal beacon--window-scrolled (selected-window)))
    (beacon-blink-automated))
   ;; Blink for movement
   ((beacon--movement-> beacon-blink-when-point-moves-vertically
                  beacon-blink-when-point-moves-horizontally)
    (beacon-blink-automated)))
  (beacon--maybe-push-mark)
  (setq beacon--window-scrolled nil))

(defun beacon--window-scroll-function (win start-pos)
  "Blink the beacon or record that window has been scrolled.
If invoked during the command loop, record the current window so
that it may be blinked on post-command.  This is because the
scrolled window might not be active, but we only know that at
`post-command-hook'.

If invoked outside the command loop, `post-command-hook' would be
unreliable, so just blink immediately."
  (unless (or (and (equal beacon--previous-window-start start-pos)
                   (equal beacon--previous-window win))
              (not beacon-blink-when-window-scrolls))
    (if this-command
        (setq beacon--window-scrolled win)
      (setq beacon--window-scrolled nil)
      (beacon-blink-automated))))

(defun beacon--blink-on-focus ()
  "Blink if `beacon-blink-when-focused' is non-nil"
  (when beacon-blink-when-focused
    (beacon-blink-automated)))


;;; Minor-mode
(defcustom beacon-lighter
  (cond
   ;; ((char-displayable-p ?💡) " 💡")
   ;; ((char-displayable-p ?Λ) " Λ")
   (t " (*)"))
  "Lighter string used on the mode-line."
  :type 'string)

;;;###autoload
(define-minor-mode beacon-mode
  nil nil beacon-lighter nil
  :global t
  (if beacon-mode
      (progn
        (add-hook 'window-scroll-functions #'beacon--window-scroll-function)
        (add-hook 'focus-in-hook #'beacon--blink-on-focus)
        (add-hook 'post-command-hook #'beacon--post-command)
        (add-hook 'before-change-functions #'beacon--vanish)
        (add-hook 'pre-command-hook #'beacon--record-vars)
        (add-hook 'pre-command-hook #'beacon--vanish))
    (remove-hook 'focus-in-hook #'beacon--blink-on-focus)
    (remove-hook 'window-scroll-functions #'beacon--window-scroll-function)
    (remove-hook 'post-command-hook #'beacon--post-command)
    (remove-hook 'before-change-functions #'beacon--vanish)
    (remove-hook 'pre-command-hook #'beacon--record-vars)
    (remove-hook 'pre-command-hook #'beacon--vanish)))

(provide 'beacon)
;;; beacon.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

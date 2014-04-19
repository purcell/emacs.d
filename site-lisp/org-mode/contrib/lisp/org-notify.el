;;; org-notify.el --- Notifications for Org-mode

;; Copyright (C) 2012, 2013, 2014  Free Software Foundation, Inc.

;; Author: Peter Münster <pmrb@free.fr>
;; Keywords: notification, todo-list, alarm, reminder, pop-up

;; This file is not part of GNU Emacs.

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

;; Get notifications, when there is something to do.
;; Sometimes, you need a reminder a few days before a deadline, e.g. to buy a
;; present for a birthday, and then another notification one hour before to
;; have enough time to choose the right clothes.
;; For other events, e.g. rolling the dustbin to the roadside once per week,
;; you probably need another kind of notification strategy.
;; This package tries to satisfy the various needs.

;; In order to activate this package, you must add the following code
;; into your .emacs:
;;
;;   (require 'org-notify)
;;   (org-notify-start)

;; Example setup:
;;
;; (org-notify-add 'appt
;;                 '(:time "-1s" :period "20s" :duration 10
;;                   :actions (-message -ding))
;;                 '(:time "15m" :period "2m" :duration 100
;;                   :actions -notify)
;;                 '(:time "2h" :period "5m" :actions -message)
;;                 '(:time "3d" :actions -email))
;;
;; This means for todo-items with `notify' property set to `appt': 3 days
;; before deadline, send a reminder-email, 2 hours before deadline, start to
;; send messages every 5 minutes, then 15 minutes before deadline, start to
;; pop up notification windows every 2 minutes.  The timeout of the window is
;; set to 100 seconds.  Finally, when deadline is overdue, send messages and
;; make noise."

;; Take also a look at the function `org-notify-add'.

;;; Code:

(eval-when-compile (require 'cl))
(require 'org-element)

(declare-function appt-delete-window    "appt"          ())
(declare-function notifications-notify  "notifications" (&rest prms))
(declare-function article-lapsed-string "gnus-art"      (t &optional ms))

(defgroup org-notify nil
  "Options for Org-mode notifications."
  :tag "Org Notify"
  :group 'org)

(defcustom org-notify-audible t
  "Non-nil means beep to indicate notification."
  :type 'boolean
  :group 'org-notify)

(defconst org-notify-actions
  '("show" "show" "done" "done" "hour" "one hour later" "day" "one day later"
    "week" "one week later")
  "Possible actions for call-back functions.")

(defconst org-notify-window-buffer-name "*org-notify-%s*"
  "Buffer-name for the `org-notify-action-window' function.")

(defvar org-notify-map nil
  "Mapping between names and parameter lists.")

(defvar org-notify-timer nil
  "Timer of the notification daemon.")

(defvar org-notify-parse-file nil
  "Index of current file, that `org-element-parse-buffer' is parsing.")

(defvar org-notify-on-action-map nil
  "Mapping between on-action identifiers and parameter lists.")

(defun org-notify-string->seconds (str)
  "Convert time string STR to number of seconds."
  (when str
    (let* ((conv `(("s" . 1) ("m" . 60) ("h" . ,(* 60 60))
                   ("d" . ,(* 24 60 60)) ("w" . ,(* 7 24 60 60))
                   ("M" . ,(* 30 24 60 60))))
           (letters (concat
                     (mapcar (lambda (x) (string-to-char (car x))) conv)))
           (case-fold-search nil))
      (string-match (concat "\\(-?\\)\\([0-9]+\\)\\([" letters "]\\)") str)
      (* (string-to-number (match-string 2 str))
         (cdr (assoc (match-string 3 str) conv))
         (if (= (length (match-string 1 str)) 1) -1 1)))))

(defun org-notify-convert-deadline (orig)
  "Convert original deadline from `org-element-parse-buffer' to
simple timestamp string."
  (if orig
      (replace-regexp-in-string "^<\\|>$" ""
				(plist-get (plist-get orig 'timestamp)
					   :raw-value))))

(defun org-notify-make-todo (heading &rest ignored)
  "Create one todo item."
  (macrolet ((get (k) `(plist-get list ,k))
             (pr (k v) `(setq result (plist-put result ,k ,v))))
    (let* ((list (nth 1 heading))      (notify (or (get :notify) "default"))
           (deadline (org-notify-convert-deadline (get :deadline)))
	   (heading (get :raw-value))
           result)
      (when (and (eq (get :todo-type) 'todo) heading deadline)
        (pr :heading heading)     (pr :notify (intern notify))
        (pr :begin (get :begin))
        (pr :file (nth org-notify-parse-file (org-agenda-files 'unrestricted)))
        (pr :timestamp deadline)  (pr :uid (md5 (concat heading deadline)))
        (pr :deadline (- (org-time-string-to-seconds deadline)
                         (org-float-time))))
      result)))

(defun org-notify-todo-list ()
  "Create the todo-list for one org-agenda file."
  (let* ((files (org-agenda-files 'unrestricted))
         (max (1- (length files))))
    (setq org-notify-parse-file
          (if (or (not org-notify-parse-file) (>= org-notify-parse-file max))
              0
            (1+ org-notify-parse-file)))
    (save-excursion
      (with-current-buffer (find-file-noselect
                            (nth org-notify-parse-file files))
        (org-element-map (org-element-parse-buffer 'headline)
                         'headline 'org-notify-make-todo)))))

(defun org-notify-maybe-too-late (diff period heading)
  "Print waring message, when notified significantly later than defined by
PERIOD."
  (if (> (/ diff period) 1.5)
      (message "Warning: notification for \"%s\" behind schedule!" heading))
  t)

(defun org-notify-process ()
  "Process the todo-list, and possibly notify user about upcoming or
forgotten tasks."
  (macrolet ((prm (k) `(plist-get prms ,k))  (td (k) `(plist-get todo ,k)))
    (dolist (todo (org-notify-todo-list))
      (let* ((deadline (td :deadline))  (heading (td :heading))
             (uid (td :uid))            (last-run-sym
                                         (intern (concat ":last-run-" uid))))
        (dolist (prms (plist-get org-notify-map (td :notify)))
          (when (< deadline (org-notify-string->seconds (prm :time)))
            (let ((period (org-notify-string->seconds (prm :period)))
                  (last-run (prm last-run-sym))  (now (org-float-time))
                  (actions (prm :actions))       diff  plist)
              (when (or (not last-run)
                        (and period (< period (setq diff (- now last-run)))
                             (org-notify-maybe-too-late diff period heading)))
                (setq prms (plist-put prms last-run-sym now)
                      plist (append todo prms))
                (if (if (plist-member prms :audible)
                        (prm :audible)
                      org-notify-audible)
                    (ding))
                (unless (listp actions)
                  (setq actions (list actions)))
                (dolist (action actions)
                  (funcall (if (fboundp action) action
                             (intern (concat "org-notify-action"
                                             (symbol-name action))))
                           plist))))
            (return)))))))

(defun org-notify-add (name &rest params)
  "Add a new notification type.
The NAME can be used in Org-mode property `notify'.  If NAME is
`default', the notification type applies for todo items without
the `notify' property.  This file predefines such a default
notification type.

Each element of PARAMS is a list with parameters for a given time
distance to the deadline.  This distance must increase from one
element to the next.

List of possible parameters:

  :time      Time distance to deadline, when this type of notification shall
             start.  It's a string: an integral value (positive or negative)
             followed by a unit (s, m, h, d, w, M).
  :actions   A function or a list of functions to be called to notify the
             user.  Instead of a function name, you can also supply a suffix
             of one of the various predefined `org-notify-action-xxx'
             functions.
  :period    Optional: can be used to repeat the actions periodically.
             Same format as :time.
  :duration  Some actions use this parameter to specify the duration of the
             notification.  It's an integral number in seconds.
  :audible   Overwrite the value of `org-notify-audible' for this action.

For the actions, you can use your own functions or some of the predefined
ones, whose names are prefixed with `org-notify-action-'."
  (setq org-notify-map (plist-put org-notify-map name params)))

(defun org-notify-start (&optional secs)
  "Start the notification daemon.
If SECS is positive, it's the period in seconds for processing
the notifications of one org-agenda file, and if negative,
notifications will be checked only when emacs is idle for -SECS
seconds.  The default value for SECS is 20."
  (interactive)
  (if org-notify-timer
      (org-notify-stop))
  (setq secs (or secs 20)
        org-notify-timer (if (< secs 0)
                             (run-with-idle-timer (* -1 secs) t
                                                  'org-notify-process)
                           (run-with-timer secs secs 'org-notify-process))))

(defun org-notify-stop ()
  "Stop the notification daemon."
  (when org-notify-timer
    (cancel-timer org-notify-timer)
    (setq org-notify-timer nil)))

(defun org-notify-on-action (plist key)
  "User wants to see action."
  (let ((file (plist-get plist :file))
        (begin (plist-get plist :begin)))
    (if (string-equal key "show")
        (progn
          (switch-to-buffer (find-file-noselect file))
          (org-with-wide-buffer
           (goto-char begin)
           (show-entry))
          (goto-char begin)
          (search-forward "DEADLINE: <")
          (if (display-graphic-p)
              (x-focus-frame nil)))
      (save-excursion
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char begin)
           (search-forward "DEADLINE: <")
           (cond
            ((string-equal key "done")  (org-todo))
            ((string-equal key "hour")  (org-timestamp-change 60 'minute))
            ((string-equal key "day")   (org-timestamp-up-day))
            ((string-equal key "week")  (org-timestamp-change 7 'day)))))))))

(defun org-notify-on-action-notify (id key)
  "User wants to see action after mouse-click in notify window."
  (org-notify-on-action (plist-get org-notify-on-action-map id) key)
  (org-notify-on-close id nil))

(defun org-notify-on-action-button (button)
  "User wants to see action after button activation."
  (macrolet ((get (k) `(button-get button ,k)))
    (org-notify-on-action (get 'plist) (get 'key))
    (org-notify-delete-window (get 'buffer))
    (cancel-timer (get 'timer))))

(defun org-notify-delete-window (buffer)
  "Delete the notification window."
  (require 'appt)
  (let ((appt-buffer-name buffer)
        (appt-audible nil))
    (appt-delete-window)))

(defun org-notify-on-close (id reason)
  "Notification window has been closed."
  (setq org-notify-on-action-map (plist-put org-notify-on-action-map id nil)))

(defun org-notify-action-message (plist)
  "Print a message."
  (message "TODO: \"%s\" at %s!" (plist-get plist :heading)
           (plist-get plist :timestamp)))

(defun org-notify-action-ding (plist)
  "Make noise."
  (let ((timer (run-with-timer 0 1 'ding)))
    (run-with-timer (or (plist-get plist :duration) 3) nil
                    'cancel-timer timer)))

(defun org-notify-body-text (plist)
  "Make human readable string for remaining time to deadline."
  (require 'gnus-art)
  (format "%s\n(%s)"
          (replace-regexp-in-string
           " in the future" ""
           (article-lapsed-string
            (time-add (current-time)
                      (seconds-to-time (plist-get plist :deadline))) 2))
          (plist-get plist :timestamp)))

(defun org-notify-action-email (plist)
  "Send email to user."
  (compose-mail user-mail-address (concat "TODO: " (plist-get plist :heading)))
  (insert (org-notify-body-text plist))
  (funcall send-mail-function)
  (flet ((yes-or-no-p (prompt) t))
    (kill-buffer)))

(defun org-notify-select-highest-window ()
  "Select the highest window on the frame, that is not is not an
org-notify window.  Mostly copied from `appt-select-lowest-window'."
  (let ((highest-window (selected-window))
        (bottom-edge (nth 3 (window-edges)))
        next-bottom-edge)
    (walk-windows (lambda (w)
                    (when (and
                           (not (string-match "^\\*org-notify-.*\\*$"
                                              (buffer-name
                                               (window-buffer w))))
                           (> bottom-edge (setq next-bottom-edge
                                                (nth 3 (window-edges w)))))
                      (setq bottom-edge next-bottom-edge
                            highest-window w))) 'nomini)
    (select-window highest-window)))

(defun org-notify-action-window (plist)
  "Pop up a window, mostly copied from `appt-disp-window'."
  (save-excursion
    (macrolet ((get (k) `(plist-get plist ,k)))
      (let ((this-window (selected-window))
            (buf (get-buffer-create
                  (format org-notify-window-buffer-name (get :uid)))))
        (when (minibufferp)
          (other-window 1)
          (and (minibufferp) (display-multi-frame-p) (other-frame 1)))
        (if (cdr (assq 'unsplittable (frame-parameters)))
            (progn (set-buffer buf) (display-buffer buf))
          (unless (or (special-display-p (buffer-name buf))
                      (same-window-p (buffer-name buf)))
            (org-notify-select-highest-window)
            (when (>= (window-height) (* 2 window-min-height))
              (select-window (split-window nil nil 'above))))
          (switch-to-buffer buf))
        (setq buffer-read-only nil  buffer-undo-list t)
        (erase-buffer)
        (insert (format "TODO: %s, %s.\n" (get :heading)
                        (org-notify-body-text plist)))
        (let ((timer (run-with-timer (or (get :duration) 10) nil
                                     'org-notify-delete-window buf)))
          (dotimes (i (/ (length org-notify-actions) 2))
            (let ((key (nth (* i 2) org-notify-actions))
                  (text (nth (1+ (* i 2)) org-notify-actions)))
              (insert-button text 'action 'org-notify-on-action-button
                             'key key 'buffer buf 'plist plist 'timer timer)
              (insert "    "))))
        (shrink-window-if-larger-than-buffer (get-buffer-window buf t))
        (set-buffer-modified-p nil)       (setq buffer-read-only t)
        (raise-frame (selected-frame))    (select-window this-window)))))

(defun org-notify-action-notify (plist)
  "Pop up a notification window."
  (require 'notifications)
  (let* ((duration (plist-get plist :duration))
         (id (notifications-notify
              :title     (plist-get plist :heading)
              :body      (org-notify-body-text plist)
              :timeout   (if duration (* duration 1000))
              :actions   org-notify-actions
              :on-action 'org-notify-on-action-notify)))
    (setq org-notify-on-action-map
          (plist-put org-notify-on-action-map id plist))))

(defun org-notify-action-notify/window (plist)
  "For a graphics display, pop up a notification window, for a text
terminal an emacs window."
  (if (display-graphic-p)
      (org-notify-action-notify plist)
    (org-notify-action-window plist)))

;;; Provide a minimal default setup.
(org-notify-add 'default '(:time "1h" :actions -notify/window
				 :period "2m" :duration 60))

(provide 'org-notify)

;;; org-notify.el ends here

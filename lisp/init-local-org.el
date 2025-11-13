;; init-local-org.el --- org specific settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq org-directory "~/org/"
      org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq org-agenda-files
      (seq-filter (lambda(x) (not (string-match "/.stversions/"(file-name-directory x))))
                  (directory-files-recursively "~/org/" "\\.org$")
                  ))

(setq org-agenda-clockreport-parameter-plist
      (quote (:maxlevel 5 :fileskip0 t :compact t :narrow 80 :formula % )))

(with-eval-after-load 'org
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-startup-indented t
        org-adapt-indentation nil
        org-edit-src-content-indentation 0
        org-startup-truncated nil
        org-fontify-done-headline t
        org-fontify-todo-headline t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-pretty-entities t))


;;; config from https://doc.norang.ca/org-mode.html

(setq org-use-fast-todo-selection t)
(setq ido-max-directory-size 100000)

;; 9 Time Clocking
(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the selected task.  If no task is selected set the Organization task as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in."
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "6B6FB404-85A4-4212-B9D0-D4C2C527DD9D")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

;; 17 Reminders
;; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-agenda-finalize-hook 'bh/org-agenda-to-appt 'append)

;; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; 20 custom command by ken
(defun kk/org-clock-in-switch-task ()
  "Clock in and switch task."
  (interactive)
  (let ((current-prefix-arg '(4)))  ;; This sets the C-u prefix argument
    (call-interactively 'org-clock-in)))



(add-to-list 'load-path "~/github/org-now/")
(require 'org-now)
(setq org-now-location '("~/org/now.org" "Now"))

(setq org-now-window-side 'bottom)
;; fixme: when setting the window to the bottom, use this patch
;; it should be upstreamed to the original org-now
(with-eval-after-load 'org-now
  (defun org-now-buffer ()
    (org-now--ensure-configured)
    (or (get-buffer "*org-now*")
        (let ((origin (current-buffer)))
          (unwind-protect
              (org-with-point-at (org-now--marker)
                (let ((buf (clone-indirect-buffer "*org-now*" nil)))
                  (with-current-buffer buf
                    (when (> (length org-now-location) 1)
                      (org-narrow-to-subtree))
                    (setq header-line-format (propertize " org-now" 'face 'org-now-header))
                    (toggle-truncate-lines 1)
                    (rename-buffer "*org-now*")
                    (run-hooks 'org-now-hook)
                    (when org-now-default-cycle-level
                      (org-global-cycle org-now-default-cycle-level)))
                  buf))
            (when (buffer-live-p origin)
              (set-buffer origin))))))

  (defun org-now ()
    (interactive)
    (let* ((buffer (org-now-buffer))
           (window (get-buffer-window buffer)))
      (if (eq (selected-window) window)
          (quit-window nil window)
        (select-window
         (or window
             (display-buffer-in-side-window
              buffer
              `((side . ,org-now-window-side)
                (slot . 0)
                (window-parameters .
                                   ((no-delete-other-windows . t)
                                    (no-other-window . ,org-now-no-other-window)))
                ;; To keep using bottom layout, explicitly specify the height
                (window-height . 0.2)))))))))



;; Show only top-level headlines
(setq org-startup-folded 'content)

(provide 'init-local-org)
;;; init-local-org.el ends here

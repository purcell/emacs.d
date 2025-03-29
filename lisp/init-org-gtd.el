;;; init-org-gtd.el --- Opiniated GTD setup -*- lexical-binding: t -*-
;;; Commentary:

;;; Opinionated GTD implementation on top of the Purilli Emacs init
;;; framework.   Sets the location of my org files along with some simple
;;; ccapture templates.

;;; Code:
(setq org-agenda-files '("~/org/gtd/inbox.org"
                         "~/org/gtd/gtd.org"
                         "~/org/gtd/tickler.org"))
 ;;; org-capture
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/org/gtd/inbox.org" "Tasks")
                               "* WAITING %i%? \n:PROPERTIES:\n:Created: %U\n:CATEGORY: %^{Category}\n:END:")
                              ("b" "Brain" plain (function org-brain-goto-end)
                               "* %i%?" :empty-lines 1)
                              ("T" "Tickler" entry
                               (file+headline "~/org/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")
                              ))
 ;;; org-refile-targets
(setq org-refile-targets '(("~/org/gtd/notes/notes.org" :maxlevel . 4)
                           ("~/org/gtd/gtd.org" :maxlevel . 3)
                           ("~/org/gtd/someday.org" :level . 1)
                           ("~/org/gtd/tickler.org" :maxlevel . 2)))
 ;;; org-agenda-custom-commands
(setq org-agenda-custom-commands
      '(("o" "At the office" tags-todo "@tinder"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  "."
  (string= "TODO" (org-get-todo-state)))

(defun org-schedule-effort ()
  "Add the effort as a time interval."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* (
           (element (org-element-at-point))
           (effort (org-element-property :EFFORT element))
           (scheduled (org-element-property :scheduled element))
           (ts-year-start (org-element-property :year-start scheduled))
           (ts-month-start (org-element-property :month-start scheduled))
           (ts-day-start (org-element-property :day-start scheduled))
           (ts-hour-start (org-element-property :hour-start scheduled))
           (ts-minute-start (org-element-property :minute-start scheduled)) )
      (org-schedule nil (concat
                         (format "%s" ts-year-start)
                         "-"
                         (if (< ts-month-start 10)
                             (concat "0" (format "%s" ts-month-start))
                           (format "%s" ts-month-start))
                         "-"
                         (if (< ts-day-start 10)
                             (concat "0" (format "%s" ts-day-start))
                           (format "%s" ts-day-start))
                         " "
                         (if (< ts-hour-start 10)
                             (concat "0" (format "%s" ts-hour-start))
                           (format "%s" ts-hour-start))
                         ":"
                         (if (< ts-minute-start 10)
                             (concat "0" (format "%s" ts-minute-start))
                           (format "%s" ts-minute-start))
                         "+"
                         effort)) )))

;;; org-todo-keywords
(setq org-todo-keywords
      '((sequence "PROJECT(p!)" "WAITING(w!)" "TODO(t!)" "IN_PROGRESS(i!)" "ON_HOLD(h!)" "VERIFY(v!)" "|" "DONE(d!)" "DELEGATED(e@)" "CANCELLED(c@)")))

;;; ispell block skipps for ORG files
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

(provide 'init-org-gtd)
;;; init-org-gtd.el ends here

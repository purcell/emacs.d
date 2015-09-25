;;; init-my-org-custom-agenda-views.el --- Org customized views

;;; Commentary:

;;; Code:


;;================================================================
;; Base config
;;================================================================

(setq org-agenda-files (quote ("~/workspace/github/work-notes/personal"
                               "~/workspace/github/work-notes/project-schedules"
                               )))

;; Set the agenda view to show the tasks on day/week/month/year
(setq org-agenda-span 'week)
;; Set the agenda view to show the tasks on how many days, same effect as org-agenda-span
(setq org-agenda-ndays 10)
;; If you want to set the start day of the agenda view, set following variable
;; Default is 0, means start from today, but if org-agenda-span set to week, deafult is start from Monday
;; (setq org-agenda-start-day "+10d")

;; Config what is a stuck project
                                        ;(setq org-stuck-projects
                                        ;      `(,active-project-match ("MAYBE")))


;; In order to include entries from the Emacs diary into Org mode's agenda
;;(setq org-agenda-include-diary t)


;; Custom commands for the agenda -- start with a clean slate.
(setq org-agenda-custom-commands nil)

;; Do not dim blocked tasks
;;(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)


;;================================================================
;; Special defun for First
;;================================================================

(defconst leuven-org-completed-date-regexp
  (concat " \\("
          "CLOSED: \\[%Y-%m-%d"
          "\\|"
          "- State \"\\(DONE\\|CANX\\)\" * from .* \\[%Y-%m-%d"
          "\\|"
          "- State .* ->  *\"\\(DONE\\|CANX\\)\" * \\[%Y-%m-%d"
          "\\) ")
  "Matches any completion time stamp.")



;; =========================================================================================
;; Config for All tasks
;; =========================================================================================
(add-to-list 'org-agenda-custom-commands
             '("c" . "任务汇总...") t)

;; Qingdao Projects Schedule
(add-to-list 'org-agenda-custom-commands
             `("cq" "Qingdao Projects"
               ((alltodo ""))
               ((org-agenda-files (list ,"~/workspace/github/work-notes/qingdao-projects")))
               ((org-agenda-sorting-strategy '(priority-down time-down)))
               ) t)

;; Personal schedules
(add-to-list 'org-agenda-custom-commands
             `("cp" "My Personal Schedule"
               ((alltodo ""))
               ((org-agenda-files (list ,"~/workspace/github/work-notes/personal")))
               ((org-agenda-sorting-strategy '(priority-down time-down)))
               ) t)
;; My projects
(add-to-list 'org-agenda-custom-commands
             `("cm" "My Project Schedules"
               ((alltodo ""))
               ((org-agenda-files (list ,"~/workspace/github/work-notes/project-schedules")))
               ((org-agenda-sorting-strategy '(priority-down time-down)))
               ) t)
;; =========================================================================================
;; Config for All tasks
;; =========================================================================================
(add-to-list 'org-agenda-custom-commands
             `("i" "Refile..."
               ((org-agenda-files (list ,"~/workspace/github/work-notes/captures"))
                ;; Refile
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil))))) t)

;; =========================================================================================
;; Config for Status list
;; =========================================================================================
(add-to-list 'org-agenda-custom-commands
             '("f" . "按进度状态查看...") t)

(add-to-list 'org-agenda-custom-commands
             `("fq" "Qingdao Project Schedules"
               (
                ;; Events.
                (agenda ""
                        ((org-agenda-entry-types '(:timestamp :sexp))
                         (org-agenda-overriding-header
                          (concat "CALENDAR Today "
                                  (format-time-string "%a %d" (current-time))
                                  ;; #("__________________" 0 12 (face (:foreground "gray")))
                                  ))
                         (org-agenda-span 'day)))
                ;; Unscheduled new tasks (waiting to be prioritized and scheduled).
                (tags-todo "LEVEL=2"
                           ((org-agenda-overriding-header "Qingdao Projects (Unscheduled)")
                            (org-agenda-files (list ,"~/workspace/github/work-notes/qingdao-projects"))
                            ))
                ;; List of all TODO entries with deadline today.
                (tags-todo "DEADLINE=\"<+0d>\""
                           ((org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))
                            (org-agenda-sorting-strategy '(priority-down))))
                                        ; XXX Timed deadlines NOT shown!!!
                ;; List of all TODO entries with deadline before today.
                (tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))
                            (org-agenda-sorting-strategy '(priority-down))))
                ;; (agenda ""
                ;;         ((org-agenda-entry-types '(:deadline))
                ;;          (org-agenda-overriding-header "DUE DATES")
                ;;          (org-agenda-skip-function
                ;;           '(org-agenda-skip-entry-if 'todo 'done))
                ;;          (org-agenda-sorting-strategy
                ;;           '(priority-down time-down))
                ;;          (org-agenda-span 'day)
                ;;          (org-agenda-start-on-weekday nil)
                ;;          (org-agenda-time-grid nil)))
                (agenda ""
                        ((org-agenda-entry-types '(:scheduled))
                         (org-agenda-overriding-header "SCHEDULED")
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-sorting-strategy
                          '(priority-down time-down))
                         (org-agenda-span 'day)
                         (org-agenda-files (list ,"~/workspace/github/work-notes/qingdao-projects"))
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-time-grid nil)))
                ;; List of all TODO entries completed today.
                (todo "TODO|DONE|CANX" ; Includes repeated tasks (back in TODO).
                      ((org-agenda-overriding-header "COMPLETED TODAY")
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp)))
                       (org-agenda-sorting-strategy '(priority-down)))))
               ((org-agenda-format-date "")
                (org-agenda-files (list ,"~/workspace/github/work-notes/qingdao-projects"))
                (org-agenda-start-with-clockreport-mode nil))) t)

(add-to-list 'org-agenda-custom-commands
             `("fm" "My Project Schedules"
               (
                ;; Events.
                (agenda ""
                        ((org-agenda-entry-types '(:timestamp :sexp))
                         (org-agenda-overriding-header
                          (concat "CALENDAR Today "
                                  (format-time-string "%a %d" (current-time))
                                  ;; #("__________________" 0 12 (face (:foreground "gray")))
                                  ))
                         (org-agenda-span 'day)))
                ;; Unscheduled new tasks (waiting to be prioritized and scheduled).
                (tags-todo "LEVEL=2"
                           ((org-agenda-overriding-header "My Projects Schedule (Unscheduled)")
                            (org-agenda-files (list ,"~/workspace/github/work-notes/project-schedules"))
                            ))
                ;; List of all TODO entries with deadline today.
                (tags-todo "DEADLINE=\"<+0d>\""
                           ((org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))
                            (org-agenda-sorting-strategy '(priority-down))))
                                        ; XXX Timed deadlines NOT shown!!!
                ;; List of all TODO entries with deadline before today.
                (tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))
                            (org-agenda-sorting-strategy '(priority-down))))
                ;; (agenda ""
                ;;         ((org-agenda-entry-types '(:deadline))
                ;;          (org-agenda-overriding-header "DUE DATES")
                ;;          (org-agenda-skip-function
                ;;           '(org-agenda-skip-entry-if 'todo 'done))
                ;;          (org-agenda-sorting-strategy
                ;;           '(priority-down time-down))
                ;;          (org-agenda-span 'day)
                ;;          (org-agenda-start-on-weekday nil)
                ;;          (org-agenda-time-grid nil)))
                (agenda ""
                        ((org-agenda-entry-types '(:scheduled))
                         (org-agenda-overriding-header "SCHEDULED")
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-sorting-strategy
                          '(priority-down time-down))
                         (org-agenda-span 'day)
                         (org-agenda-files (list ,"~/workspace/github/work-notes/project-schedules"))
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-time-grid nil)))
                ;; List of all TODO entries completed today.
                (todo "TODO|DONE|CANX" ; Includes repeated tasks (back in TODO).
                      ((org-agenda-overriding-header "COMPLETED TODAY")
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp)))
                       (org-agenda-sorting-strategy '(priority-down)))))
               ((org-agenda-format-date "")
                (org-agenda-files (list ,"~/workspace/github/work-notes/project-schedules"))
                (org-agenda-start-with-clockreport-mode nil))) t)

(add-to-list 'org-agenda-custom-commands
             `("fp" "Personal Schedules"
               (
                ;; Events.
                (agenda ""
                        ((org-agenda-entry-types '(:timestamp :sexp))
                         (org-agenda-overriding-header
                          (concat "CALENDAR Today "
                                  (format-time-string "%a %d" (current-time))
                                  ;; #("__________________" 0 12 (face (:foreground "gray")))
                                  ))
                         (org-agenda-span 'day)))
                ;; Unscheduled new tasks (waiting to be prioritized and scheduled).
                (tags-todo "LEVEL=2"
                           ((org-agenda-overriding-header "Personal Schedule(Unscheduled)")
                            (org-agenda-files (list ,"~/workspace/github/work-notes/personal"))
                            ))
                ;; List of all TODO entries with deadline today.
                (tags-todo "DEADLINE=\"<+0d>\""
                           ((org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))
                            (org-agenda-sorting-strategy '(priority-down))))
                                        ; XXX Timed deadlines NOT shown!!!
                ;; List of all TODO entries with deadline before today.
                (tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))
                            (org-agenda-sorting-strategy '(priority-down))))
                ;; (agenda ""
                ;;         ((org-agenda-entry-types '(:deadline))
                ;;          (org-agenda-overriding-header "DUE DATES")
                ;;          (org-agenda-skip-function
                ;;           '(org-agenda-skip-entry-if 'todo 'done))
                ;;          (org-agenda-sorting-strategy
                ;;           '(priority-down time-down))
                ;;          (org-agenda-span 'day)
                ;;          (org-agenda-start-on-weekday nil)
                ;;          (org-agenda-time-grid nil)))
                (agenda ""
                        ((org-agenda-entry-types '(:scheduled))
                         (org-agenda-overriding-header "SCHEDULED")
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'todo 'done))
                         (org-agenda-sorting-strategy
                          '(priority-down time-down))
                         (org-agenda-span 'day)
                         (org-agenda-files (list ,"~/workspace/github/work-notes/personal"))
                         (org-agenda-start-on-weekday nil)
                         (org-agenda-time-grid nil)))
                ;; List of all TODO entries completed today.
                (todo "TODO|DONE|CANX" ; Includes repeated tasks (back in TODO).
                      ((org-agenda-overriding-header "COMPLETED TODAY")
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp)))
                       (org-agenda-sorting-strategy '(priority-down)))))
               ((org-agenda-format-date "")
                (org-agenda-files (list ,"~/workspace/github/work-notes/personal"))
                (org-agenda-start-with-clockreport-mode nil))) t)

(add-to-list 'org-agenda-custom-commands
             '("fh" "Hotlist"
               ;; tags-todo "DEADLINE<=\"<+1w>\"|PRIORITY={A}|FLAGGED"
               ((tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")))
                (tags-todo "DEADLINE>=\"<+0d>\"+DEADLINE<=\"<+1w>\""
                           ((org-agenda-overriding-header "DUE IN NEXT 7 DAYS")))
                (tags-todo "DEADLINE=\"\"+PRIORITY={A}|DEADLINE>\"<+1w>\"+PRIORITY={A}"
                           ((org-agenda-overriding-header "HIGH PRIORITY")))
                (tags-todo "DEADLINE=\"\"+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                           ((org-agenda-overriding-header "FLAGGED")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-when-regexp-matches))
                            (org-agenda-skip-regexp "\\[#A\\]")))
                ;; (tags-todo "DEADLINE=\"\"+PRIORITY<>{A}+FLAGGED|DEADLINE>\"<+1w>\"+PRIORITY<>{A}+FLAGGED"
                ;;            ((org-agenda-overriding-header "...FLAGGED...")))
                )
               ((org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-sorting-strategy '(deadline-up)))) t) ; FIXME sort not OK

;; ========================================================================
;; Config for Checking by Date
;; ========================================================================
(add-to-list 'org-agenda-custom-commands
             '("r" . "按日期及状态查看...") t)

(add-to-list 'org-agenda-custom-commands
             '("rq" "Qingdao Project All Tasks (grouped by Due Date)"
               (
                (tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+0d>\""
                           ((org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+1d>\""
                           ((org-agenda-overriding-header "DUE TOMORROW")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                           ((org-agenda-overriding-header "DUE WITHIN A WEEK")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                           ((org-agenda-overriding-header "DUE WITHIN A MONTH")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+28d>\""
                           ((org-agenda-overriding-header "DUE LATER")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))

                ;; (todo ""
                ;;            ((org-agenda-overriding-header "NO DUE DATE")
                ;;             (org-agenda-skip-function
                ;;              '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={STRT}"
                           ((org-agenda-overriding-header "NO DUE DATE / STARTED")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO<>{STRT\\|WAIT\\|SDAY}"
                           ((org-agenda-overriding-header "NO DUE DATE / NEXT")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={WAIT}"
                           ((org-agenda-overriding-header "NO DUE DATE / WAITING FOR")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={SDAY}"
                           ((org-agenda-overriding-header "NO DUE DATE / SOMEDAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline)))))
               ((org-agenda-sorting-strategy '(priority-down time-down))
                (org-agenda-files '("~/workspace/github/work-notes/qingdao-projects"))
                (org-agenda-write-buffer-name "All Tasks (grouped by Due Date)"))
               "~/org___all-tasks-by-due-date.pdf") t)


(add-to-list 'org-agenda-custom-commands
             '("rm" "My Project Schedules All Tasks (grouped by Due Date)"
               (

                (tags-todo "DEADLINE<\"<+0d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+0d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+1d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE TOMORROW")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE WITHIN A WEEK")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE WITHIN A MONTH")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+28d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE LATER")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))

                ;; (todo ""
                ;;            ((org-agenda-overriding-header "NO DUE DATE")
                ;;             (org-agenda-skip-function
                ;;              '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={STRT}"
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "NO DUE DATE / STARTED")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO<>{STRT\\|WAIT\\|SDAY}"
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "NO DUE DATE / NEXT")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={WAIT}"
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "NO DUE DATE / WAITING FOR")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={SDAY}"
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "NO DUE DATE / SOMEDAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline)))))
               ((org-agenda-sorting-strategy '(priority-down time-down))
                (org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                (org-agenda-write-buffer-name "All Tasks (grouped by Due Date)"))
               "~/org___all-tasks-by-due-date.pdf") t)

(add-to-list 'org-agenda-custom-commands
             '("rp" "My Personal Schedules All Tasks (grouped by Due Date)"
               (
                (tags-todo "DEADLINE<\"<+0d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+0d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+1d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE TOMORROW")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE WITHIN A WEEK")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE WITHIN A MONTH")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+28d>\""
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "DUE LATER")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))

                ;; (todo ""
                ;;            ((org-agenda-overriding-header "NO DUE DATE")
                ;;             (org-agenda-skip-function
                ;;              '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={STRT}"
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "NO DUE DATE / STARTED")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO<>{STRT\\|WAIT\\|SDAY}"
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "NO DUE DATE / NEXT")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={WAIT}"
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "NO DUE DATE / WAITING FOR")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={SDAY}"
                           (;;(org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                            (org-agenda-overriding-header "NO DUE DATE / SOMEDAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline)))))
               ((org-agenda-sorting-strategy '(priority-down time-down))
                (org-agenda-files '("~/workspace/github/work-notes/personal"))
                (org-agenda-write-buffer-name "All Tasks (grouped by Due Date)"))
               "~/org___all-tasks-by-due-date.pdf") t)


;; ===================================================================================
;; Config for Checking by Due Date
;; ===================================================================================
(add-to-list 'org-agenda-custom-commands
             '("d" . "按日期查看...") t)

;; Qingdao Projects
(add-to-list 'org-agenda-custom-commands
             `("dq" "Qingdao Projects All active tasks, by due date"
               ((agenda ""
                        ((org-agenda-overriding-header "Today")
                         ;; FIXME We don't see "timed" DEADLINE.
                         (org-agenda-skip-function
                          (lambda ()
                            (let* ((dl (org-entry-get nil "DEADLINE")))
                              (if (or (not dl)
                                      (equal dl "")
                                      (org-time> dl (org-time-today)))
                                  (progn (outline-next-heading) (point))))))
                         (org-agenda-skip-scheduled-if-deadline-is-shown t)
                         (org-agenda-span 'day)
                         (org-deadline-warning-days 0)))
                (agenda ""
                        ((org-agenda-entry-types '(:deadline))
                         (org-agenda-overriding-header "Tomorrow")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 1))
                         (org-deadline-warning-days 1)))
                (agenda ""
                        ((org-agenda-overriding-header "Next 5 days")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 2))
                         (org-deadline-warning-days 7)))
                (agenda ""
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "Next 3 weeks")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 7))
                         (org-deadline-warning-days 28))))
               ((org-agenda-deadline-faces '((0.0 . default)))
                (org-agenda-start-with-clockreport-mode nil)
                (org-agenda-format-date "")
                (org-agenda-span 'day)
                (org-agenda-files '("~/workspace/github/work-notes/qingdao-projects"))
                (org-agenda-sorting-strategy '(deadline-up))
                (org-agenda-use-time-grid nil)
                (org-agenda-write-buffer-name "Reminders"))) t)

;; Qingdao Projects
(add-to-list 'org-agenda-custom-commands
             `("dm" "My Projects All active tasks, by due date"
               ((agenda ""
                        ((org-agenda-overriding-header "Today")
                         ;; FIXME We don't see "timed" DEADLINE.
                         (org-agenda-skip-function
                          (lambda ()
                            (let* ((dl (org-entry-get nil "DEADLINE")))
                              (if (or (not dl)
                                      (equal dl "")
                                      (org-time> dl (org-time-today)))
                                  (progn (outline-next-heading) (point))))))
                         (org-agenda-skip-scheduled-if-deadline-is-shown t)
                         (org-agenda-span 'day)
                         (org-deadline-warning-days 0)))
                (agenda ""
                        ((org-agenda-entry-types '(:deadline))
                         (org-agenda-overriding-header "Tomorrow")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 1))
                         (org-deadline-warning-days 1)))
                (agenda ""
                        ((org-agenda-overriding-header "Next 5 days")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 2))
                         (org-deadline-warning-days 7)))
                (agenda ""
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "Next 3 weeks")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 7))
                         (org-deadline-warning-days 28))))
               ((org-agenda-deadline-faces '((0.0 . default)))
                (org-agenda-start-with-clockreport-mode nil)
                (org-agenda-format-date "")
                (org-agenda-span 'day)
                (org-agenda-files '("~/workspace/github/work-notes/project-schedules"))
                (org-agenda-sorting-strategy '(deadline-up))
                (org-agenda-use-time-grid nil)
                (org-agenda-write-buffer-name "Reminders"))) t)


;; Qingdao Projects
(add-to-list 'org-agenda-custom-commands
             `("dp" "Personal All active tasks, by due date"
               ((agenda ""
                        ((org-agenda-overriding-header "Today")
                         ;; FIXME We don't see "timed" DEADLINE.
                         (org-agenda-skip-function
                          (lambda ()
                            (let* ((dl (org-entry-get nil "DEADLINE")))
                              (if (or (not dl)
                                      (equal dl "")
                                      (org-time> dl (org-time-today)))
                                  (progn (outline-next-heading) (point))))))
                         (org-agenda-skip-scheduled-if-deadline-is-shown t)
                         (org-agenda-span 'day)
                         (org-deadline-warning-days 0)))
                (agenda ""
                        ((org-agenda-entry-types '(:deadline))
                         (org-agenda-overriding-header "Tomorrow")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 1))
                         (org-deadline-warning-days 1)))
                (agenda ""
                        ((org-agenda-overriding-header "Next 5 days")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 2))
                         (org-deadline-warning-days 7)))
                (agenda ""
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "Next 3 weeks")
                         (org-agenda-skip-function
                          '(leuven--skip-entry-unless-deadline-in-n-days-or-more 7))
                         (org-deadline-warning-days 28))))
               ((org-agenda-deadline-faces '((0.0 . default)))
                (org-agenda-start-with-clockreport-mode nil)
                (org-agenda-format-date "")
                (org-agenda-span 'day)
                (org-agenda-files '("~/workspace/github/work-notes/personal"))
                (org-agenda-sorting-strategy '(deadline-up))
                (org-agenda-use-time-grid nil)
                (org-agenda-write-buffer-name "Reminders"))) t)


(defun leuven--skip-entry-unless-deadline-in-n-days-or-more (n)
  "Skip entries that have no deadline, or that have a deadline earlier than in N days."
  (let* ((dl (org-entry-get nil "DEADLINE")))
    (if (or (not dl)
            (equal dl "")
            (org-time< dl (+ (org-time-today) (* n 86400))))
        (progn (outline-next-heading) (point)))))

(defun leuven--skip-entry-unless-overdue-deadline ()
  "Skip entries that have no deadline, or that have a deadline later than or equal to today."
  (let* ((dl (org-entry-get nil "DEADLINE")))
    (if (or (not dl)
            (equal dl "")
            (org-time>= dl (org-time-today)))
        (progn (outline-next-heading) (point)))))

(defun leuven--skip-entry-if-past-deadline ()
  "Skip entries that have a deadline earlier than today."
  (let* ((dl (org-entry-get nil "DEADLINE")))
    (if (org-time< dl (org-time-today))
        (progn (outline-next-heading) (point)))))

(defun leuven--skip-entry-if-deadline-in-less-than-n-days-or-schedule-in-less-than-n-days (n1 n2)
  "Skip entries that have a deadline in less than N1 days, or that have a
  scheduled date in less than N2 days, or that have no deadline nor scheduled."
  (let* ((dl (org-entry-get nil "DEADLINE"))
         (sd (org-entry-get nil "SCHEDULED")))
    (if (or (and dl
                 (not (equal dl ""))
                 (org-time< dl (+ (org-time-today) (* n1 86400))))
            (and sd
                 (not (equal sd ""))
                 (org-time< sd (+ (org-time-today) (* n2 86400))))
            (and (or (not dl)       ; No deadline.
                     (equal dl ""))
                 (or (not sd)       ; Nor scheduled.
                     (equal sd ""))))
        (progn (outline-next-heading) (point)))))

(defun leuven--skip-entry-if-deadline-or-schedule ()
  "Skip entries that have a deadline or that have a scheduled date."
  (let* ((dl (org-entry-get nil "DEADLINE"))
         (sd (org-entry-get nil "SCHEDULED")))
    (if (or (and dl
                 (not (equal dl "")))
            (and sd
                 (not (equal sd ""))))
        (progn (outline-next-heading) (point)))))

;; ===================================================================================
;; Config for Checking by Priority
;; ===================================================================================

(add-to-list 'org-agenda-custom-commands
             '("p" . "按优先级查看完成状态...") t)

(add-to-list 'org-agenda-custom-commands
             '("pq" "Qingdao Projects All Tasks (grouped by Priority)"
               ((tags-todo "PRIORITY={A}"
                           ((org-agenda-overriding-header "HIGH")))
                (tags-todo "PRIORITY={B}"
                           ((org-agenda-overriding-header "MEDIUM")))
                (tags-todo "PRIORITY=\"\""
                           ((org-agenda-overriding-header "NONE"))) ; = Medium.
                (tags-todo "PRIORITY={C}"
                           ((org-agenda-overriding-header "LOW")))
                (todo "DONE|CANX"
                      ((org-agenda-overriding-header "COMPLETED")
                       (org-agenda-sorting-strategy '(priority-down))))
                ((org-agenda-files '("~/workspace/github/work-notes/personal")))
                )) t)
;; My projects
(add-to-list 'org-agenda-custom-commands
             '("pm" "My Projects All Tasks (grouped by Priority)"
               ((tags-todo "PRIORITY={A}"
                           ((org-agenda-overriding-header "HIGH")))
                (tags-todo "PRIORITY={B}"
                           ((org-agenda-overriding-header "MEDIUM")))
                (tags-todo "PRIORITY=\"\""
                           ((org-agenda-overriding-header "NONE"))) ; = Medium.
                (tags-todo "PRIORITY={C}"
                           ((org-agenda-overriding-header "LOW")))
                (todo "DONE|CANX"
                      ((org-agenda-overriding-header "COMPLETED")
                       (org-agenda-sorting-strategy '(priority-down))))
                ((org-agenda-files '("~/workspace/github/work-notes/project-schedules")))
                )) t)

;; Personal
(add-to-list 'org-agenda-custom-commands
             '("pp" "My Projects All Tasks (grouped by Priority)"
               ((tags-todo "PRIORITY={A}"
                           ((org-agenda-overriding-header "HIGH")))
                (tags-todo "PRIORITY={B}"
                           ((org-agenda-overriding-header "MEDIUM")))
                (tags-todo "PRIORITY=\"\""
                           ((org-agenda-overriding-header "NONE"))) ; = Medium.
                (tags-todo "PRIORITY={C}"
                           ((org-agenda-overriding-header "LOW")))
                (todo "DONE|CANX"
                      ((org-agenda-overriding-header "COMPLETED")
                       (org-agenda-sorting-strategy '(priority-down))))
                ((org-agenda-files '("~/workspace/github/work-notes/personal")))
                )) t)



;; ===================================================================================
;; Config for Checking by 时间
;; ===================================================================================

(add-to-list 'org-agenda-custom-commands
             '("j" . "Timesheet for Clocking...") t)

;; Show what happened today.
(add-to-list 'org-agenda-custom-commands
             '("jd" "Daily Timesheet"
               ((agenda ""))
               ((org-agenda-log-mode-items '(clock closed))
                (org-agenda-overriding-header "DAILY TIMESHEET")
                (org-agenda-show-log 'clockcheck)
                (org-agenda-span 'day)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil))) t)
;; Show what happened this week.
(add-to-list 'org-agenda-custom-commands
             '("jw" "Qingdao Weekly Timesheet"
               ((agenda ""))
               (
                ;; (org-agenda-format-date "")
                (org-agenda-overriding-header "WEEKLY TIMESHEET")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                (org-agenda-span 'week)
                (org-agenda-start-on-weekday 1)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil))) t)

;; ===================================================================================
;; Config for Checking by Calendar
;; ===================================================================================

(add-to-list 'org-agenda-custom-commands
             '("k" . "Calendar...") t)

(add-to-list 'org-agenda-custom-commands
             '("k7" "Events and appointments for 7 days"
               ((agenda ""))
               ((org-agenda-entry-types '(:timestamp :sexp))
                ;; (org-agenda-overriding-header "Calendar for 7 days")
                ;; (org-agenda-repeating-timestamp-show-all t)
                (org-agenda-span 'week)
                (org-agenda-format-date "\n%a %d")
                ;; (org-agenda-date-weekend ... new face ...)
                (org-agenda-files '("~/workspace/github/work-notes/personal"))
                (org-agenda-time-grid nil))) t)

;; Calendar view for org-agenda.
(when (locate-library "calfw-org")

  (autoload 'cfw:open-org-calendar "calfw-org"
    "Open an Org schedule calendar." t)

  (add-to-list 'org-agenda-custom-commands
               '("km" "Calendar for current month"
                 (lambda (&rest ignore)
                   (cfw:open-org-calendar))) t)

  ;; (defun cfw:open-org-calendar-non-work (&args)
  ;;   (interactive)
  ;;   (let ((org-agenda-skip-function 'org-agenda-skip-work))
  ;;     (cfw:open-org-calendar)))
  ;;
  ;; (add-to-list 'org-agenda-custom-commands
  ;;              '("c" "Calendar (non-work) for current month"
  ;;                cfw:open-org-calendar-non-work) t)

  )
(add-to-list 'org-agenda-custom-commands
             '("u" . "Complete...") t)

(add-to-list 'org-agenda-custom-commands
             `("uC" "Completed view"
               (;; List of all TODO entries completed yesterday.
                (todo "TODO|DONE|CANX" ; includes repeated tasks (back in TODO)
                      ((org-agenda-overriding-header
                        (concat "YESTERDAY   "
                                (format-time-string "%a %d" (current-time-ndays-ago 1))
                                ;; #("__________________" 0 12 (face (:foreground "gray")))
                                ))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 1))))
                       (org-agenda-sorting-strategy '(priority-down))))
                ;; List of all TODO entries completed 2 days ago.
                (todo "TODO|DONE|CANX" ; includes repeated tasks (back in TODO)
                      ((org-agenda-overriding-header
                        (concat "2 DAYS AGO  "
                                (format-time-string "%a %d" (current-time-ndays-ago 2))))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 2))))
                       (org-agenda-sorting-strategy '(priority-down))))
                ;; List of all TODO entries completed 3 days ago.
                (todo "TODO|DONE|CANX" ; Includes repeated tasks (back in TODO).
                      ((org-agenda-overriding-header
                        (concat "3 DAYS AGO  "
                                (format-time-string "%a %d" (current-time-ndays-ago 3))))
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'notregexp
                          (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 3))))
                       (org-agenda-sorting-strategy '(priority-down)))))
               ((org-agenda-format-date "")
                (org-agenda-start-with-clockreport-mode nil))) t)

(defun current-time-ndays-ago (n)
  "Return the current time minus N days."
  (time-subtract (current-time) (days-to-time n)))

(add-to-list 'org-agenda-custom-commands
             '("ux" "Completed tasks with no CLOCK lines"
               ((todo "DONE|CANX"
                      ((org-agenda-overriding-header "Completed tasks with no CLOCK lines")
                       (org-agenda-skip-function
                        '(org-agenda-skip-entry-if
                          'regexp
                          (format-time-string "  CLOCK: .*--.* =>  .*")))
                       (org-agenda-sorting-strategy '(priority-down)))))) t)

(add-to-list 'org-agenda-custom-commands
             '("ur" "Recent items (past 7 days)"
               ;; Faster than tags.
               ((agenda ""))
               ((org-agenda-start-day "-7d")
                (org-agenda-span 7)
                (org-agenda-repeating-timestamp-show-all nil)
                ;; %s is only for agenda views
                ;; (org-agenda-prefix-format "%s")
                ;; maybe not make much difference ka
                ;; (org-agenda-use-tag-inheritance nil)
                (org-agenda-inactive-leader "Inactive:  ")
                (org-agenda-include-inactive-timestamps t))) t)

(add-to-list 'org-agenda-custom-commands
             '("uw" "Weekly review"
               ((tags "CATEGORY={@Collect}&LEVEL=2|TODO={NEW}"
                      ((org-agenda-overriding-header "NEW TASKS")))

                (agenda ""
                        ((org-agenda-clockreport-mode t)
                         (org-agenda-format-date
                          (concat "\n"
                                  "%Y-%m-%d" " %a "
                                  (make-string (window-width) ?_)))
                         (org-agenda-overriding-header "PAST WEEK")
                         (org-agenda-prefix-format " %?-11t %i %-12:c% s")
                         (org-agenda-show-log 'clockcheck)
                         (org-agenda-span 7)
                         (org-agenda-start-day "-1w") ; recently done
                         (org-deadline-warning-days 0)))

                (agenda ""
                        ((org-agenda-overriding-header "NEXT MONTH")
                         (org-agenda-span 'month)
                         (org-agenda-start-day "+0d")
                         (org-deadline-warning-days 0) ; XXX
                         ))

                (todo "PROJ"
                      ((org-agenda-overriding-header "PROJECT LIST")))

                ;; FIXME we should show which tasks (don't) have CLOCK lines: archived vs. deleted.
                (todo "DONE|PROJDONE"
                      ((org-agenda-overriding-header
                        "Candidates to be archived")))

                ;; (stuck ""
                ;;        ((org-agenda-overriding-header "Stuck projects")))

                (todo "STRT"
                      ((org-agenda-overriding-header "IN PROGRESS")
                       (org-agenda-todo-ignore-scheduled nil)))

                (todo "TODO"        ; Don't include items from CollectBox! XXX
                      ((org-agenda-overriding-header "ACTION LIST")))

                ;; Ignore scheduled and deadline entries, as they're visible
                ;; in the above agenda (for the past + for next month) or
                ;; scheduled/deadline'd for much later...
                (todo "WAIT"
                      ((org-agenda-format-date "")
                       (org-agenda-overriding-header "WAITING FOR")
                       (org-agenda-todo-ignore-deadlines 'all) ; Future?
                       (org-agenda-todo-ignore-scheduled t)))

                ;; Same reasoning as for WAIT.
                (todo "SDAY"
                      ((org-agenda-format-date "")
                       (org-agenda-overriding-header "SOMEDAY")
                       (org-agenda-todo-ignore-deadlines 'all)
                       (org-agenda-todo-ignore-scheduled t)))

                ;; ((org-agenda-start-with-clockreport-mode nil)
                ;;  (org-agenda-prefix-format " %i %?-12t% s")
                ;;  (org-agenda-write-buffer-name "Weekly task review"))
                ;; "~/org-weekly-review.html") t)
                )) t)

(add-to-list 'org-agenda-custom-commands
             '("uN" "Next"
               ((tags-todo "TODO<>{SDAY}"))
               ((org-agenda-overriding-header "List of all TODO entries with no due date (no SDAY)")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                (org-agenda-sorting-strategy '(priority-down)))) t)

(add-to-list 'org-agenda-custom-commands
             '("uW" "Waiting for"
               ((tags-todo "TODO={WAIT}"))
               ((org-agenda-overriding-header "Waiting for")
                (org-agenda-sorting-strategy '(deadline-up)))) t) ; FIXME does not work.

(add-to-list 'org-agenda-custom-commands
             '("uP" "Projects"
               ((tags-todo "project-DONE-CANX"))
               ((org-agenda-overriding-header "Projects (High Level)")
                (org-agenda-sorting-strategy nil))) t)

(add-to-list 'org-agenda-custom-commands
             '("+" . "MORE...") t)

;; Checking tasks that are assigned to me.
(add-to-list 'org-agenda-custom-commands
             `("+a" "Assigned to me"
               ((tags ,(concat "Assignee={" user-login-name "\\|"
                               user-mail-address "}")))
               ((org-agenda-overriding-header "ASSIGNED TO ME"))) t)

(add-to-list 'org-agenda-custom-commands
             '("E" . "Exported agenda files...") t)

;; Exporting agenda views.
(add-to-list 'org-agenda-custom-commands
             '("Ea"
               ((agenda ""))
               (;; (org-tag-faces nil)
                (ps-landscape-mode t)
                (ps-number-of-columns 1))
               ("~/workspace/github/publish-works/org-agenda.html" "~/workspace/github/publish-works/org-agenda.pdf")) t)

(add-to-list 'org-agenda-custom-commands
             '("Ep" "Call list"
               ((tags-todo "phone"))
               ((org-agenda-prefix-format " %-20:c [ ] " )
                (org-agenda-remove-tags t)
                ;; (org-agenda-with-colors nil)
                (org-agenda-write-buffer-name
                 "Phone calls that you need to make")
                (ps-landscape-mode t)
                (ps-number-of-columns 1))
               ("~/workspace/github/publish-works/org___calls.pdf")) t)

(add-to-list 'org-agenda-custom-commands
             '("A" . "ARCHIVE...") t)

(add-to-list 'org-agenda-custom-commands
             '("Aa" "Archive"
               ((tags-todo "ARCHIVE"))
               ((org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-sorting-strategy '(deadline-down)))) t)

(add-to-list 'org-agenda-custom-commands
             '("R" . "REFERENCE...") t)

(add-to-list 'org-agenda-custom-commands
             '("Rs" "Like s, but with extra files"
               ((search ""))
               ((org-agenda-text-search-extra-files
                 ;; FIXME Add `agenda-archives'
                 leuven-org-search-extra-files))) t)

(add-to-list 'org-agenda-custom-commands
             '("RS" "Like s, but only TODO entries"
               ((search ""))
               ((org-agenda-text-search-extra-files
                 ;; FIXME Add `agenda-archives'
                 leuven-org-search-extra-files))) t)

(add-to-list 'org-agenda-custom-commands
             '("Rn" "Organize thoughts to refile"
               ((tags "refile|capture"))
               ((org-agenda-overriding-header "Refile stuff"))) t)

;; Create a sparse tree (current buffer only) with all entries containing the
;; word `TODO', `FIXME' or `XXX'.
(add-to-list 'org-agenda-custom-commands
             '("1" "Task markers (in current buffer)"
               ((occur-tree "\\<TODO\\|FIXME\\|XXX\\>"))) t)


(provide 'init-my-org-custom-agenda-views)

;;; init-my-org-custom-agenda-views.el ends here

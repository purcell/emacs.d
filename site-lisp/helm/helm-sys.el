;;; helm-sys.el --- System related functions for helm. -*- lexical-binding: t -*-

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


(defgroup helm-sys nil
  "System related helm library."
  :group 'helm)

(defun helm-top-command-set-fn (var _value)
  (set var
       (cl-case system-type
         (darwin "env COLUMNS=%s ps -axo pid,user,pri,nice,ucomm,tty,start,vsz,%%cpu,%%mem,etime,command")
         (t      "env COLUMNS=%s top -b -n 1"))))

(defcustom helm-top-command "env COLUMNS=%s top -b -n 1"
  "Top command used to display output of top.
To use top command, a version supporting batch mode (-b option) is needed.
On Mac OSX top command doesn't support this, so ps command
is used by default instead.
If you modify this the number and order of elements displayed
should be the same as top command to have the sort commands
working properly, that is 12 elements with the 2 first being
PID and USER and the last 4 being %CPU, %MEM, TIME and COMMAND.
A format string where %s will be replaced with `frame-width'."
  :group 'helm-sys
  :type 'string
  :set  'helm-top-command-set-fn)


;;; Top (process)
;;
;;
(defvar helm-top-sort-fn nil)
(defvar helm-top-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?") 'helm-top-help)
    (define-key map (kbd "M-P")   'helm-top-run-sort-by-cpu)
    (define-key map (kbd "M-C")   'helm-top-run-sort-by-com)
    (define-key map (kbd "M-M")   'helm-top-run-sort-by-mem)
    (define-key map (kbd "M-U")   'helm-top-run-sort-by-user)
    map))

(defvar helm-source-top
  `((name . "Top")
    (header-name . (lambda (name) (concat name " (Press C-c C-u to refresh)"))) 
    (init . helm-top-init)
    (candidates-in-buffer)
    (nomark)
    (display-to-real . helm-top-display-to-real)
    (persistent-action . helm-top-sh-persistent-action)
    (persistent-help . "SIGTERM")
    (mode-line . helm-top-mode-line)
    (follow . never)
    (keymap . ,helm-top-map)
    (filtered-candidate-transformer . helm-top-sort-transformer)
    (action-transformer . helm-top-action-transformer)))

(defun helm-top-transformer (candidates _source)
  "Transformer for `helm-top'.
Return empty string for non--valid candidates."
  (cl-loop for disp in candidates collect
        (if (string-match "^ *[0-9]+" disp) disp (cons disp ""))))

(defun helm-top-action-transformer (actions _candidate)
  "Action transformer for `top'.
Show actions only on line starting by a PID."
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^ *[0-9]+" disp)
           (list '("kill (SIGTERM)" . (lambda (pid) (helm-top-sh "TERM" pid)))
                 '("kill (SIGKILL)" . (lambda (pid) (helm-top-sh "KILL" pid)))
                 '("kill (SIGINT)" .  (lambda (pid) (helm-top-sh "INT" pid)))
                 '("kill (Choose signal)"
                   . (lambda (pid)
                       (helm-top-sh
                        (helm-comp-read (format "Kill [%s] with signal: " pid)
                                        '("ALRM" "HUP" "INT" "KILL" "PIPE" "POLL"
                                          "PROF" "TERM" "USR1" "USR2" "VTALRM"
                                          "STKFLT" "PWR" "WINCH" "CHLD" "URG"
                                          "TSTP" "TTIN" "TTOU" "STOP" "CONT"
                                          "ABRT" "FPE" "ILL" "QUIT" "SEGV"
                                          "TRAP" "SYS" "EMT" "BUS" "XCPU" "XFSZ")
                                        :must-match t)
                        pid)))))
          (t actions))))

(defun helm-top-sh (sig pid)
  "Run kill shell command with signal SIG on PID for `helm-top'."
  (let ((cmd (format "kill -%s %s" sig pid)))
    (message "Executed %s\n%s" cmd (shell-command-to-string cmd))))

(defun helm-top-sh-persistent-action (pid)
  (delete-other-windows)
  (helm-top-sh "TERM" pid)
  (helm-force-update))

(defun helm-top-init ()
  "Insert output of top command in candidate buffer."
  (unless helm-top-sort-fn (helm-top-set-mode-line "CPU"))
  (with-current-buffer (helm-candidate-buffer 'global)
    (call-process-shell-command
     (format helm-top-command (frame-width))
     nil (current-buffer))))

(defun helm-top-display-to-real (line)
  "Return pid only from LINE."
  (car (split-string line)))

;; Sort top command

(defun helm-top-set-mode-line (str)
  (if (string-match "Sort:\\[\\(.*\\)\\] " helm-top-mode-line)
      (setq helm-top-mode-line (replace-match str nil nil helm-top-mode-line 1))
    (setq helm-top-mode-line (concat (format "Sort:[%s] " str) helm-top-mode-line))))

(defun helm-top-sort-transformer (candidates source)
  (helm-top-transformer
   (if helm-top-sort-fn
       (cl-loop for c in candidates
             if (string-match "^ *[0-9]+" c) collect c into pid-cands
             else collect c into header-cands
             finally return (append (if (cdr header-cands)
                                        (butlast header-cands)
                                      header-cands)
                                    (sort pid-cands helm-top-sort-fn)))
     candidates)
   source))

(defun helm-top-sort-by-com (s1 s2)
  (let* ((split-1 (split-string s1))
         (split-2 (split-string s2))
         (com-1 (nth 11 split-1))
         (com-2 (nth 11 split-2)))
    (string< com-1 com-2)))

(defun helm-top-sort-by-mem (s1 s2)
  (let* ((split-1 (split-string s1))
         (split-2 (split-string s2))
         (mem-1 (string-to-number (nth 9 split-1)))
         (mem-2 (string-to-number (nth 9 split-2))))
    (> mem-1 mem-2)))

(defun helm-top-sort-by-user (s1 s2)
  (let* ((split-1 (split-string s1))
         (split-2 (split-string s2))
         (user-1 (nth 1 split-1))
         (user-2 (nth 1 split-2)))
    (string< user-1 user-2)))

;;;###autoload
(defun helm-top-run-sort-by-com ()
  (interactive)
  (helm-top-set-mode-line "COM")
  (setq helm-top-sort-fn 'helm-top-sort-by-com)
  (helm-force-update))

;;;###autoload
(defun helm-top-run-sort-by-cpu ()
  (interactive)
  (helm-top-set-mode-line "CPU")
  (setq helm-top-sort-fn nil)
  (helm-force-update))

;;;###autoload
(defun helm-top-run-sort-by-mem ()
  (interactive)
  (helm-top-set-mode-line "MEM")
  (setq helm-top-sort-fn 'helm-top-sort-by-mem)
  (helm-force-update))

;;;###autoload
(defun helm-top-run-sort-by-user ()
  (interactive)
  (helm-top-set-mode-line "USER")
  (setq helm-top-sort-fn 'helm-top-sort-by-user)
  (helm-force-update))


;;; X RandR resolution change
;;
;;
;;; FIXME I do not care multi-display.

(defun helm-xrandr-info ()
  "Return a pair with current X screen number and current X display name."
  (with-temp-buffer
    (call-process "xrandr" nil (current-buffer) nil
                  "--current")
    (let (screen output)
      (goto-char (point-min))
      (save-excursion
        (when (re-search-forward "\\(^Screen \\)\\([0-9]\\):" nil t)
          (setq screen (match-string 2))))
      (when (re-search-forward "^\\(.*\\) connected" nil t)
        (setq output (match-string 1)))
      (list screen output))))

(defun helm-xrandr-screen ()
  "Return current X screen number."
  (car (helm-xrandr-info)))

(defun helm-xrandr-output ()
  "Return current X display name."
  (cadr (helm-xrandr-info)))

(defvar helm-source-xrandr-change-resolution
  '((name . "Change Resolution")
    (candidates
     . (lambda ()
         (with-temp-buffer
           (call-process "xrandr" nil (current-buffer) nil
                         "--screen" (helm-xrandr-screen) "-q")
           (goto-char 1)
           (cl-loop with modes = nil
                 while (re-search-forward "   \\([0-9]+x[0-9]+\\)" nil t)
                 for mode = (match-string 1)
                 unless (member mode modes)
                 collect mode into modes
                 finally return modes))))
    (action
     ("Change Resolution"
      . (lambda (mode)
          (call-process "xrandr" nil nil nil
                        "--screen" (helm-xrandr-screen)
                        "--output" (helm-xrandr-output)
                        "--mode" mode))))))


;;; Emacs process
;;
;;
(defvar helm-source-emacs-process
  '((name . "Emacs Process")
    (init . (lambda () (list-processes--refresh)))
    (candidates . (lambda () (mapcar #'process-name (process-list))))
    (persistent-action . (lambda (elm)
                           (delete-process (get-process elm))
                           (helm-delete-current-selection)))
    (update . list-processes--refresh)
    (persistent-help . "Kill Process")
    (action ("Kill Process" . (lambda (elm)
                                (delete-process (get-process elm)))))))


;;;###autoload
(defun helm-top ()
  "Preconfigured `helm' for top command."
  (interactive)
  (save-window-excursion
    (unless helm-alive-p (delete-other-windows))
    (helm :sources 'helm-source-top
          :buffer "*helm top*" :full-frame t
          :candidate-number-limit 9999)))

;;;###autoload
(defun helm-list-emacs-process ()
  "Preconfigured `helm' for emacs process."
  (interactive)
  (helm-other-buffer 'helm-source-emacs-process "*helm process*"))

;;;###autoload
(defun helm-xrandr-set ()
  (interactive)
  (helm :sources 'helm-source-xrandr-change-resolution
        :buffer "*helm xrandr*"))

(provide 'helm-sys)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-sys.el ends here

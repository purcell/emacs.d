;; init-local-org.el - org specific settings  -*- lexical-binding: t; -*-

(setq org-directory "~/org/"
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-agenda-files (list org-default-notes-file
                             (expand-file-name "software.org" org-directory)
                             (expand-file-name "web.org" org-directory)
                             (expand-file-name "network.org" org-directory)
                             (expand-file-name "bookmark.org" org-directory)
                             (expand-file-name "ai.org" org-directory)
                             (expand-file-name "weekly.org" org-directory)
                             (expand-file-name "habits.org" org-directory)
                             (expand-file-name "learning.org" org-directory)
                             (expand-file-name "diary.org" org-directory)
                             (expand-file-name "todo.org" org-directory)
                             (expand-file-name "shopping.org" org-directory)
                             (expand-file-name "game.org" org-directory)
                             (expand-file-name "coffee.org" org-directory)
                             (expand-file-name "movies.org" org-directory)
                             (expand-file-name "other.org" org-directory)
                             (expand-file-name "notes.org" org-directory)
                             (expand-file-name "camera.org" org-directory)
                             (expand-file-name "musics.org" org-directory)
                             (expand-file-name "now.org" org-directory)))

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


(provide 'init-local-org)

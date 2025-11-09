;;; Package --- keybinding -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; cliboard setting for mouse
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

;; You can also add the following code to enable commenting and uncommenting by pressing gcc
;; in normal mode and gc in visual mode
;; (thanks you to the Reddit user u/mistakenuser for this contribution, which replaces the evil-commentary package):
(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

;; (defun my/evil-delete (orig-fn beg end &optional type _ &rest args)
;;   "Make evil-delete use the black hole register (_) instead of default register."
;;   (apply orig-fn beg end type ?_ args))

;; (advice-add 'evil-delete :around 'my/evil-delete)

(defun my/evil-delete (orig-fn beg end &optional type _ &rest args)
  "Make `evil-delete' use the black hole register (_) instead of default register.
ORIG-FN is the original function being advised.
BEG and END are the region boundaries.
TYPE is the motion type.
ARGS are additional arguments."
  (apply orig-fn beg end type ?_ args))

(advice-add 'evil-delete :around #'my/evil-delete)

;; -------------------------------------------------------------------------- ;;
;; ----------------------------- customize key ------------------------------ ;;
;; -------------------------------------------------------------------------- ;;

;; Bind M-1 through M-9 globally to switch tabs
;; Assumes built-in tab-bar-mode or tab-line-mode
(dotimes (i 9)
  (let ((key (format "M-%d" (1+ i))) ; Create key string like "M-1", "M-2", etc.
        (tab-number (1+ i)))         ; Tab index (1-based)
    (global-set-key (kbd key)
                    ;; Define a command to select the corresponding tab
                    `(lambda () (interactive) (tab-bar-select-tab ,tab-number)))))

;; Bind Super+v to paste (yank)
(global-set-key (kbd "s-v") 'yank)
;; Disable the space key in Dired so that it can be used as a leader key.
(evil-collection-define-key 'normal 'dired-mode-map " " 'nil)

;; Set leader key
(evil-set-leader 'motion (kbd "SPC"))

(evil-define-key nil 'global
  ;; <leader>
  (kbd "<leader> :")  '("M-x" . execute-extended-command)
  (kbd "<leader> .")  '("Fine file" . find-file)
  (kbd "<leader> ,")  '("Switch buffer" . switch-to-buffer)
  (kbd "<leader> ;")  '("Insert timestamp" . insert-timestamp)

  ;; session
  (kbd "<leader> qq")  '("Quit Emacs" . save-buffers-kill-terminal)
  (kbd "<leader> qr")  '("Restart Emacs" . restart-emacs)

  ;; comment
  (kbd "<leader> /")  '("Commentary" . my-evil-comment-or-uncomment)

  ;; buffer
  (kbd "<leader> bn") '("Next buffer" . evil-next-buffer)
  (kbd "<leader> bp") '("Prev buffer" . evil-prev-buffer)
  (kbd "<leader> bs") '("Save buffer" . basic-save-buffer)
  (kbd "<leader> ba") '("Save all buffers" . evil-write-all)
  (kbd "<leader> bk") '("Kill current buffer" . kill-current-buffer)
  (kbd "<leader> bo") '("Read only mode" . read-only-mode)
  (kbd "<leader> bm") '("View message buffer" . view-echo-area-messages)
  (kbd "<leader> be") '("Eval buffer" . eval-buffer)
  (kbd "<leader> br") '("Revert buffer" . revert-buffer)

  ;; window
  (kbd "<leader> w") '("Window" . evil-window-map)

  ;; org
  (kbd "<leader> oa") '("Agenda" . org-agenda)
  (kbd "<leader> ol") '("Todo list" . org-todo-list)
  (kbd "<leader> om") '("Tags search" . org-tags-view)
  (kbd "<leader> ov") '("View search" . org-search-view)
  (kbd "<leader> ot") '("Todo change" . org-todo)
  (kbd "<leader> oc") '("Capture" . org-capture)
  (kbd "<leader> od") '("Insert deadline" . org-deadline)
  (kbd "<leader> os") '("Insert schedule" . org-schedule)
  (kbd "<leader> or") '("Refile" . org-refile)
  (kbd "<leader> oil") '("Inser link" . org-insert-link)
  (kbd "<leader> op") '("Change priority" . org-priority)
  (kbd "<leader> og") '("Lookup location" . org-goto)
  (kbd "<leader> oih") '("Insert heading" . org-insert-heading)
  (kbd "<leader> ois") '("Insert subheading" . org-insert-subheading)
  (kbd "<leader> onl") '("Add link to org-now" . org-now-link)
  (kbd "<leader> ont") '("Toggle org-now side window" . org-now)
  (kbd "<leader> oo") '("Open at point" . org-open-at-point)

  ;; clock
  (kbd "<leader> ct") '("Update time" . org-clock-update-time-maybe)
  (kbd "<leader> ci") '("Start clock" . org-clock-in)
  (kbd "<leader> co") '("Stop clock" . org-clock-out)
  (kbd "<leader> cpi") '("Punch in clock" . bh/punch-in)
  (kbd "<leader> cpo") '("Punch out clock" . bh/punch-out)
  (kbd "<leader> cg") '("Go to clock" . org-clock-goto)
  (kbd "<leader> clt") '("Clock in the interrupted task" . bh/clock-in-last-task)
  (kbd "<leader> cs") '("Switch task" . kk/org-clock-in-switch-task)

  ;; denote
  (kbd "<leader> dn") '("Creat a denote" . denote)
  (kbd "<leader> dr") '("Rename file" . denote-rename-file)

  ;; dired
  (kbd "<leader> dc") '("Cd current file directory" . ai/cd-to-current-buffer)
  (kbd "<leader> pwd") '("Show current directory" . pwd)
  (kbd "<leader> di") '("Dired" . dired)
  ;; zoxide
  (kbd "<leader> zf") '("Find file under a path saved in zoxide" . zoxide-find-file)
  (kbd "<leader> zt") '("Travel to a path saved in zoxide" . zoxide-travel)
  (kbd "<leader> zc") '("Change working directory to a path" . zoxide-cd)

  ;; delete
  (kbd "<leader> dd") '("Kill line" . kill-line)

  ;; tab
  (kbd "<leader> tn") '("Create new tab" . tab-new)
  (kbd "<leader> tc") '("Close tab" . tab-close)

  ;; vterm
  (kbd "<leader> vt") '("Create new vterm" . multi-vterm)

  ;; git
  (kbd "<leader> gs") '("Show status" . magit-status)
  (kbd "<leader> gb") '("Switch git status buffer" . emacs-solo/switch-git-status-buffer)

  ;; search
  (kbd "<leader> sgr") '("Search with rg" . rgrep)
  (kbd "<leader> scg") '("Search with consult rg" . consult-ripgrep)
  (kbd "<leader> scf") '("Search with consult fd" . consult-fd)
  (kbd "<leader> sch") '("Search with consult org heading" . consult-org-heading)
  (kbd "<leader> sghn") '("Search with rg everything" . rg-search-everything)

  ;; find
  (kbd "<leader> ff") '("Find file" . find-file)

  ;; markdown
  (kbd "<leader> mlp") '("Live Preview" . markdown-live-preview-mode)

  ;; system
  (kbd "<leader> ss")  '("Start server" . server-start)

  ;; Majutsu
  ;; (kbd "<leader> jj") '("Show status" . majutsu)
  ;; (kbd "<leader> jl") '("Show log" . majutsu-log)
  ;; (kbd "<leader> jd") '("Show diff" . majutsu-diff)

  ;; gptel
  (kbd "<leader> gpt") '("start a chat" . gptel)
  (kbd "<leader> gps") '("submit prompt to llm" . gptel-send)
  (kbd "<leader> gpm") '("change prompt" . gptel-menu)

  ;; denote journal
  (kbd "<leader> jn") '("Creat a entry" . my/denote-journal-new-or-existing-entry)
  (kbd "<leader> jo") '("Open current journal" . denote-journal-new-or-existing-entry))

;; Configure hjkl for Org Agenda
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "h") 'evil-backward-char)    ; Move left
  (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)  ; Move down
  (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line) ; Move up
  (define-key org-agenda-mode-map (kbd "l") 'evil-forward-char))    ; Move right

(provide 'init-local-keybinding)
;;; init-local-keybinding.el ends here

;;; workgroups.keys.el --- Set default workgroups keys
;;; Commentary:
;;; Code:

(require 'workgroups-variables)

(defvar wg-prefixed-map
  (wg-fill-keymap
   (make-sparse-keymap)


   ;; workgroup creation

   (kbd "C-c")        'wg-create-workgroup
   (kbd "c")          'wg-create-workgroup
   (kbd "C")          'wg-clone-workgroup


   ;; killing and yanking

   (kbd "C-k")        'wg-kill-workgroup
   (kbd "k")          'wg-kill-workgroup
   (kbd "M-W")        'wg-kill-ring-save-base-wconfig
   (kbd "M-w")        'wg-kill-ring-save-working-wconfig
   (kbd "C-y")        'wg-yank-wconfig
   (kbd "y")          'wg-yank-wconfig
   (kbd "M-k")        'wg-kill-workgroup-and-buffers
   (kbd "K")          'wg-delete-other-workgroups


   ;; updating and reverting

   (kbd "C-r")        'wg-revert-workgroup
   (kbd "r")          'wg-revert-workgroup
   (kbd "C-S-r")      'wg-revert-all-workgroups
   (kbd "R")          'wg-revert-all-workgroups


   ;; workgroup switching

   (kbd "C-'")        'wg-switch-to-workgroup
   (kbd "'")          'wg-switch-to-workgroup
   (kbd "C-v")        'wg-switch-to-workgroup
   (kbd "v")          'wg-switch-to-workgroup
   (kbd "M-v")        'wg-switch-to-workgroup-other-frame
   (kbd "C-j")        'wg-switch-to-workgroup-at-index
   (kbd "j")          'wg-switch-to-workgroup-at-index
   (kbd "0")          'wg-switch-to-workgroup-at-index-0
   (kbd "1")          'wg-switch-to-workgroup-at-index-1
   (kbd "2")          'wg-switch-to-workgroup-at-index-2
   (kbd "3")          'wg-switch-to-workgroup-at-index-3
   (kbd "4")          'wg-switch-to-workgroup-at-index-4
   (kbd "5")          'wg-switch-to-workgroup-at-index-5
   (kbd "6")          'wg-switch-to-workgroup-at-index-6
   (kbd "7")          'wg-switch-to-workgroup-at-index-7
   (kbd "8")          'wg-switch-to-workgroup-at-index-8
   (kbd "9")          'wg-switch-to-workgroup-at-index-9
   (kbd "C-p")        'wg-switch-to-workgroup-left
   (kbd "p")          'wg-switch-to-workgroup-left
   (kbd "C-n")        'wg-switch-to-workgroup-right
   (kbd "n")          'wg-switch-to-workgroup-right
   (kbd "M-p")        'wg-switch-to-workgroup-left-other-frame
   (kbd "M-n")        'wg-switch-to-workgroup-right-other-frame
   (kbd "C-a")        'wg-switch-to-previous-workgroup
   (kbd "a")          'wg-switch-to-previous-workgroup


   ;; wconfig undo/redo

   (kbd "<left>")     'wg-undo-wconfig-change
   (kbd "<right>")    'wg-redo-wconfig-change
   (kbd "[")          'wg-undo-wconfig-change
   (kbd "]")          'wg-redo-wconfig-change
   (kbd "{")          'wg-undo-once-all-workgroups
   (kbd "}")          'wg-redo-once-all-workgroups


   ;; wconfig save/restore

   ;; FIXME: come up with better keys for these:
   (kbd "C-d C-s")    'wg-save-wconfig
   (kbd "C-d C-'")    'wg-restore-saved-wconfig
   (kbd "C-d C-k")    'wg-kill-saved-wconfig


   ;; buffer-list

   (kbd "+")          'wg-associate-buffer-with-workgroup
   (kbd "~")          'wg-associate-visible-buffers-with-workgroup
   (kbd "-")          'wg-dissociate-buffer-from-workgroup
   (kbd "=")          'wg-cycle-buffer-association-type
   (kbd "*")          'wg-restore-workgroup-associated-buffers
   (kbd "_")          'wg-dissociate-weakly-associated-buffers
   (kbd "(")          'wg-next-buffer
   (kbd ")")          'wg-previous-buffer


   ;; workgroup movement

   (kbd "C-x")        'wg-swap-workgroups
   (kbd "C-,")        'wg-offset-workgroup-left
   (kbd "C-.")        'wg-offset-workgroup-right


   ;; file and buffer

   (kbd "C-s")        'wg-save-session
   (kbd "C-w")        'wg-write-session-file
   (kbd "C-f")        'wg-find-session-file
   (kbd "F")          'wg-find-file-in-new-workgroup
   (kbd "M-F")        'wg-find-file-read-only-in-new-workgroup
   (kbd "d")          'wg-dired-in-new-workgroup
   (kbd "C-b")        'wg-switch-to-buffer
   (kbd "b")          'wg-switch-to-buffer


   ;; window moving and frame reversal

   (kbd "<")          'wg-backward-transpose-window
   (kbd ">")          'wg-transpose-window
   (kbd "|")          'wg-reverse-frame-horizontally
   (kbd "\\")         'wg-reverse-frame-vertically
   (kbd "/")          'wg-reverse-frame-horizontally-and-vertically


   ;; toggling

   (kbd "C-t C-m")    'wg-toggle-mode-line-display
   (kbd "C-t C-b")    'wg-toggle-buffer-list-filtration
   (kbd "C-t C-d")    'wg-toggle-window-dedicated-p


   ;; echoing

   (kbd "S-C-e")      'wg-echo-current-workgroup
   (kbd "E")          'wg-echo-current-workgroup
   (kbd "C-e")        'wg-echo-all-workgroups
   (kbd "e")          'wg-echo-all-workgroups
   ;; FIXME: possibly get rid of the time stuff
   (kbd "T")          'wg-echo-time
   (kbd "V")          'wg-echo-version
   (kbd "C-m")        'wg-echo-last-message
   (kbd "m")          'wg-echo-last-message


   ;; misc

   (kbd "A")          'wg-rename-workgroup
   (kbd "!")          'wg-reset
   (kbd "?")          'wg-help

   )
  "The keymap that sits on `wg-prefix-key'.")

(defun wg-make-workgroups-mode-map ()
  "Return Workgroups' minor-mode-map.
This map includes `wg-prefixed-map' on `wg-prefix-key', as well
as Workgroups' command remappings."
  (let ((map (make-sparse-keymap)))
    (define-key map wg-prefix-key
      wg-prefixed-map)
    (when wg-remap-switch-to-buffer
      (define-key map [remap switch-to-buffer]
        'wg-switch-to-buffer))
    (when wg-remap-switch-to-buffer-other-window
      (define-key map [remap switch-to-buffer-other-window]
        'wg-switch-to-buffer-other-window))
    (when wg-remap-switch-to-buffer-other-frame
      (define-key map [remap switch-to-buffer-other-frame]
        'wg-switch-to-buffer-other-frame))
    (when wg-remap-next-buffer
      (define-key map [remap next-buffer]
        'wg-next-buffer))
    (when wg-remap-previous-buffer
      (define-key map [remap previous-buffer]
        'wg-previous-buffer))
    (when wg-remap-kill-buffer
      (define-key map [remap kill-buffer]
        'wg-kill-buffer))
    (when wg-remap-display-buffer
      (define-key map [remap display-buffer]
        'wg-display-buffer))
    (when wg-remap-insert-buffer
      (define-key map [remap insert-buffer]
        'wg-insert-buffer))
    (cond ((eq wg-remap-bury-buffer 'banish)
           (define-key map [remap bury-buffer]
             'wg-banish-buffer))
          (wg-remap-bury-buffer
           (define-key map [remap bury-buffer]
             'wg-bury-buffer)))
    (setq workgroups-mode-map map)))

(defvar wg-minibuffer-mode-map
  (wg-fill-keymap
   (make-sparse-keymap)
   (kbd "C-b")       'wg-backward-char-or-next-buffer-list-filter
   (kbd "C-c n")     'wg-next-buffer-list-filter
   (kbd "C-c C-n")   'wg-next-buffer-list-filter
   (kbd "C-S-b")     'wg-backward-char-or-previous-buffer-list-filter
   (kbd "C-c p")     'wg-previous-buffer-list-filter
   (kbd "C-c C-p")   'wg-previous-buffer-list-filter
   (kbd "C-c a")     'wg-associate-first-match
   (kbd "C-c C-a")   'wg-associate-first-match
   (kbd "C-c d")     'wg-dissociate-first-match
   (kbd "C-c C-d")   'wg-dissociate-first-match
   (kbd "C-c _")     'wg-minibuffer-mode-dissociate-weakly-associated-buffers
   )
  "`wg-minibuffer-mode's keymap.")


(provide 'workgroups-keys)
;;; workgroups-keys.el ends here

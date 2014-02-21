;;; wg-commands-minibuffer --- minibuffer commands
;;; Commentary:
;;; Code:

(require 'workgroups-utils-basic)

(defun wg-next-buffer-list-filter ()
  "Trigger a switch to the next buffer-list-filter."
  (interactive)
  (throw 'wg-action (list 'next (minibuffer-contents))))

(defun wg-previous-buffer-list-filter ()
  "Trigger a switch to the previous buffer-list-filter."
  (interactive)
  (throw 'wg-action (list 'prev (minibuffer-contents))))

(defun wg-backward-char-or-next-buffer-list-filter ()
  "Call `backward-char' unless `point' is right after the prompt,
in which case call `wg-next-buffer-list-filter'."
  (interactive)
  (if (> (point) (minibuffer-prompt-end)) (backward-char)
    (wg-next-buffer-list-filter)))

(defun wg-backward-char-or-previous-buffer-list-filter (&optional num)
  "Call `backward-char' unless `point' is right after the prompt,
in which case call `wg-previous-buffer-list-filter'."
  (interactive)
  (if (> (point) (minibuffer-prompt-end)) (backward-char)
    (wg-previous-buffer-list-filter)))

(defun wg-dissociate-first-match ()
  "Dissociate the first match from current workgroup."
  (interactive)
  (wg-when-let
      ((mode (wg-read-buffer-mode))
       (buffer (wg-current-match mode))
       (pos (cl-position buffer (wg-filtered-buffer-list t) :test 'equal)))
    (wg-workgroup-dissociate-bufobj (wg-current-workgroup) buffer)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list t) pos) mode)))

(defun wg-associate-first-match ()
  "Associate the first match with or update it in the current workgroup."
  (interactive)
  (wg-when-let
      ((mode (wg-read-buffer-mode))
       (buffer (wg-current-match mode))
       (pos (cl-position buffer (wg-filtered-buffer-list t) :test 'equal)))
    (wg-workgroup-associate-bufobj (wg-current-workgroup) buffer)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list t) pos) mode)))

(defun wg-minibuffer-mode-dissociate-weakly-associated-buffers ()
  "Dissociate weakly associated buffers and update the current matches."
  (interactive)
  (wg-workgroup-dissociate-weakly-associated-buffers (wg-current-workgroup))
  (wg-set-current-matches
   (let ((remaining (wg-filtered-buffer-list t)))
     (cl-remove-if-not (lambda (match) (member match remaining))
                       (wg-current-matches)))))

(provide 'workgroups-commands-minibuffer)
;;; workgroups-commands-minibuffer.el ends here

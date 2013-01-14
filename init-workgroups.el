(require 'workgroups)
;; save/load windows configuration into this file
(setq wg-file (expand-file-name "~/.emacs.d/.wg-file.el"))
;; actually, I use evil-leader instead
(setq wg-prefix-key (kbd "C-c w"))
;; I've already customized my mode-line for other packages
(setq wg-mode-line-on nil)
;; There is a bug in modeline, @see https://github.com/tlh/workgroups.el/issues/32
(fset 'wg-mode-line-add-display (lambda () nil))
(fset 'wg-mode-line-remove-display (lambda () nil))
;; no morph, performance is my first priority, always
(setq wg-morph-on nil)
;; now start it
(workgroups-mode 1)

(defun wgext-save ()
  (interactive)
  (wg-update-workgroup (wg-arg))
  (wg-save wg-file))

(defun wgext-load ()
  (interactive)
  (wg-load wg-file))

;; workflow
;; <prefix> c => new; <prefix> C-s => save; <prefix> C-l =>load;
(provide 'init-workgroups)

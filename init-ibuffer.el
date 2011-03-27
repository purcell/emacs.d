(require 'ibuffer)

(require 'ibuffer-vc)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (ibuffer-do-sort-by-filename/process))
(add-hook 'ibuffer-mode-hook 'ibuffer-set-up-preferred-filters)




;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8d" (buffer-size)))))


;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))


;; (setq ibuffer-saved-filter-groups
;;       (quote (("default"
;;                ("Org" ;; all org-related buffers
;;                 (mode . org-mode))
;;                ("Mail"
;;                 (or ;; mail-related buffers
;;                  (mode . message-mode)
;;                  (mode . mail-mode)
;;                  ;; etc.; all your mail related modes
;;                  ))
;;                ("Programming" ;; prog stuff not already in MyProjectX
;;                 (or
;;                  (mode . c-mode)
;;                  (mode . perl-mode)
;;                  (mode . python-mode)
;;                  (mode . emacs-lisp-mode)
;;                  ;; etc
;;                  ))
;;                ("ERC"   (mode . erc-mode))))))

;; (add-hook 'ibuffer-mode-hook
;;   (lambda ()
;;     (ibuffer-switch-to-saved-filter-groups "default")))


(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)

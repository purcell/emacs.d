(maybe-require-package 'colir)

(when (maybe-require-package 'ivy)
  (setq-default ivy-use-virtual-buffers t
                ivy-count-format "")
  (when (bound-and-true-p ido-ubiquitous-mode)
    (ido-ubiquitous-mode -1)
    (ido-mode -1))
  (ivy-mode 1)
  (when (maybe-require-package 'diminish)
    (diminish 'counsel-mode)))


(when (maybe-require-package 'counsel)
  (setq-default counsel-mode-override-describe-bindings t)
  (counsel-mode)
  (when (maybe-require-package 'diminish)
    (diminish 'counsel-mode)))


;; (when (maybe-require-package 'swiper)
;;   (define-key ivy-mode-map (kbd "C-s") 'swiper))



(provide 'init-ivy)

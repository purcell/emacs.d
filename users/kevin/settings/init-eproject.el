;; eproject global bindings
(defmacro .emacs-curry (function &rest args)
  `(lambda () (interactive)
     (,function ,@args)))

(defmacro .emacs-eproject-key (key command)
  (cons 'progn
        (loop for (k . p) in (list (cons key 4) (cons (upcase key) 1))
              collect
              `(global-set-key
                (kbd ,(format "C-x p %s" k))
                (.emacs-curry ,command ,p)))))

(.emacs-eproject-key "k" eproject-kill-project-buffers)
(.emacs-eproject-key "v" eproject-revisit-project)
(.emacs-eproject-key "b" eproject-ibuffer)
(.emacs-eproject-key "o" eproject-open-all-project-files)

;; define a project type for my notes files
(define-project-type e-note (generic) (look-for ".e-note"))

;; define a project type for front-end development project type files
(define-project-type e-front-end (generic) (look-for ".e-front"))

;; define a project type for front-end app development project type files
(define-project-type e-front-end-app (generic) (look-for ".e-app"))



(provide 'init-eproject)

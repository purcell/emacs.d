;; Work Note Documents

(defun custom-persp/work-notes ()
  (interactive)
  (custom-persp "work-notes"
                (find-file "~/workspace/github/work-notes/")))

(define-key persp-mode-map (kbd "C-x p n") 'custom-persp/work-notes)

;; Front-End Development

(defun custom-persp/front-end-project ()
  (interactive)
  (custom-persp "front-end-project"
                (find-file "~/workspace/javascript-frameworks/project-front-end-framework/")))

(define-key persp-mode-map (kbd "C-x p f") 'custom-persp/front-end-project)

(project-specifics "project-front-end-framework/less"
  (ffip-local-patterns  "*.less"  ))
(project-specifics "project-front-end-framework/js"
  (ffip-local-patterns "*.js"  "*.css"  "*.html" "*.md"))



;; APP Front-End Development

(defun custom-persp/app-front-end-project ()
  (interactive)
  (custom-persp "app-front-end-project"
                (find-file "~/workspace/javascript-frameworks/project-app-front-end-framework/")))

(define-key persp-mode-map (kbd "C-x p a") 'custom-persp/app-front-end-project)


(project-specifics "project-app-front-end-framework/less"
  (ffip-local-patterns  "*.less"  ))
(project-specifics "project-app-front-end-framework/js"
  (ffip-local-patterns "*.js"  "*.css"  "*.html" "*.md"))

;; Emacs

(defun custom-persp/emacs ()
  (interactive)
  (custom-persp "emacs"
                (find-file "~/.emacs.d/users/kevin/init.el")))

(project-specifics ".emacs.d"
  (ffip-local-excludes "swank")
  (ffip-local-patterns "*.el" "*.md" "*.org"))

(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)

(provide 'project-mappings)

(eval-after-load 'rinari
  `(let ((rinari-lib-dir (directory-of-library "rinari")))
     (unless (require 'jump nil t)
       (error "jump.el not found; please run 'git submodule update --init' in %s"
              rinari-lib-dir))

     ;; Prevent rinari from shadowing ruby-mode and inf-ruby with its bundled copies
     (setq load-path
           (remove (file-name-as-directory (expand-file-name "util/inf-ruby" rinari-lib-dir))
                   (remove (file-name-as-directory (expand-file-name "util" rinari-lib-dir))
                           load-path)))))

(dolist (hook '(nxml-mode-hook haml-mode-hook sass-mode-hook magit-mode-hook yaml-mode-hook))
  (add-hook hook 'rinari-launch))

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))


(provide 'init-rails)

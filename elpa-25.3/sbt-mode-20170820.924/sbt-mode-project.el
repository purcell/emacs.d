;;; sbt-mode-project.el - Functions for discovering the current sbt project
;;
;; Copyright(c) 2013 Heikki Vesalainen
;; For information on the License, see the LICENSE file

(require 'sbt-mode-vars)

(defvar sbt:buffer-project-root nil)

(defun sbt:find-root-impl (name-or-pred &optional dir best-root)
  (when (null dir) (setq dir default-directory))
  (let ((parent (if (string-match locate-dominating-stop-dir-regexp dir) nil
                  (file-name-directory (directory-file-name dir)))))
    (cond ((or (null parent)
               (equal dir parent))
           (and best-root (abbreviate-file-name best-root)))
          ((if (stringp name-or-pred)
               (file-exists-p (expand-file-name name-or-pred dir))
             (funcall name-or-pred dir))
           (if sbt:prefer-nested-projects
               dir
             (sbt:find-root-impl name-or-pred parent dir)))
          ('t
           (sbt:find-root-impl name-or-pred parent best-root)))))

(defun sbt:find-root ()
  "Starting from the current default-directory, find a parent
directory that is an sbt root. An sbt root directory is
identified by the following rules:

  - a directory containing a 'project/build.properties' in it.

  - a directory that contains a file matching one of the patterns
    '*.sbt' or 'project/*.scala' file in it.

The first rule is applied first and the second is used only if it
fails to find the sbt root.

The value of `sbt:prefer-nested-projects' determines the
stopping criteria."
  (or sbt:buffer-project-root
      (let ((root (or (sbt:find-root-impl "project/build.properties")
               (sbt:find-root-impl 
                (lambda (dir) 
                  (or (directory-files dir nil ".+\\.sbt$")
                      (and (file-exists-p (concat dir "project"))
                           (directory-files (concat dir "project") nil ".+\\.scala$"))))))))
        (when root 
          (setq-local sbt:buffer-project-root root)))))

(defun sbt:buffer-in-project-function (root)
  "Return a lambda that returns 't if the current buffer is in the ROOT project."
  `(lambda () (equal (sbt:find-root) ,root)))

(provide 'sbt-mode-project)

;; Author: Jonathan Rockway <jon@jrock.us>

(define-project-type perl (generic)
  (or (look-for "dist.ini") (look-for "Makefile.PL") (look-for "Build.PL"))
  :relevant-files ("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$")
  :irrelevant-files ("inc/" "blib/" "cover_db/")
  :file-name-map (lambda (root)
                   (lambda (root file)
                     (cond ((string-match "^lib/\\(.+\\)[.]pm$" file)
                            (let ((m (match-string 1 file)))
                              (while (string-match "/" m)
                                (setf m (replace-match "::" nil nil m)))
                              m))
                           (t file))))
  :xs-project-p (lambda (root)
                  (let ((default-directory root))
                    (> (length (file-expand-wildcards "*.xs")) 0)))
  :main-file "Makefile.PL")

(defun cperl-xs-project-p ()
  "Return T if this perl project contains XS code."
  (ignore-errors (eproject-attribute :xs-project-p)))

(defun cperl--tests ()
  (eproject-assert-type 'perl)
  (concat (eproject-root) "/t"))

(defun cperl--base-find-tests (find-function)
  (funcall find-function (cperl--tests)))

(defun cperl-find-tests ()
  "Visit the current perl project's test directory."
  (interactive)
  (cperl--base-find-tests 'find-file))

(defun perl-project-includes ()
  "Return list of -I flags to pass to perl."
  (eproject-assert-type 'perl)
  (list (concat (eproject-root) "/lib")))

(provide 'eproject-perl)


;; @see http://doc.norang.ca/org-mode.html#playingwithditaa for full tutorial (with exmaples, of course)
;; active Org-babel languages
(org-babel-do-load-languages
  'org-babel-load-languages
  '(;; other Babel languages
    (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/scripts/plantuml.jar"))

;@see http://eschulte.me/babel-dev/DONE-integrate-plantuml-support.html
;Example:
;#+begin_src plantuml :file tryout.png
;  Alice -> Bob: synchronous call
;  Alice ->> Bob: asynchronous call
;#+end_src
(provide 'init-uml)

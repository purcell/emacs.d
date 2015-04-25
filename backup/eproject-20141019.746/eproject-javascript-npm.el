;;; Author: Tung Dao <me@tungdao.com>

;; NPM JavaScript project
(define-project-type javascript-npm (generic)
  (or (look-for "package.json")
      (look-for "node_modules"))
  :irrelevant-files ("node_modules/")
  :tasks (("test" :shell "npm test")))


(provide 'eproject-javascript-npm)

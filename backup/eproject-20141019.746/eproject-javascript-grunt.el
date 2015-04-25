;;; Author: Tung Dao <me@tungdao.com>

;; Grunt JavaScript project
(define-project-type javascript-grunt (generic)
  (look-for "grunt.js")
  :tasks (("concat" :shell "grunt concat")
          ("lint" :shell "grunt lint")
          ("min" :shell "grunt min")
          ("qunit" :shell "grunt qunit")
          ("test" :shell "grunt test")))


(provide 'eproject-javascript-grunt)

;;; Author: Tung Dao <me@tungdao.com>

;; Bower JavaScript project
(define-project-type javascript-bower (generic)
  (look-for "component.json")
  :irrelevant-files ("components/"))


(provide 'eproject-javascript-bower)

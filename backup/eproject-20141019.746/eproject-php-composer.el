;;; Author: Tung Dao <me@tungdao.com>

;; Composer PHP Project
(define-project-type php-composer (generic)
  (look-for "composer.json")
  :irrelevant-files ("vendor/"))


(provide 'eproject-javascript)

;;; Author: Antono Vasiljev <self@antono.info>

;; Ruby on Rails project
(define-project-type ruby-on-rails (generic)
  (and (look-for "Gemfile") (look-for "config/application.rb"))
  :irrelevant-files ("app/assets/images/.*" "tmp/.*" "log/.*" "public/.*" "vendor/.*" ".*\\.sqlite?")
  :main-file "Gemfile")

(provide 'eproject-ruby-on-rails)

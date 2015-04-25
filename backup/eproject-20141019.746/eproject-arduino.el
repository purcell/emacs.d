;;; Author: Antono Vasiljev <self@antono.info>

;; Arduino project
(define-project-type arduino (generic)
  (look-for "ino.ini") ;; http://inotool.org/
  :main-file "ino.ini"
  :tasks (("build"       :shell "ino build")
          ("clean"       :shell "ino clean")
          ("upload"      :shell "ino upload")
          ("list models" :shell "ino list-models")
          ("serial"      :shell "ino serial")))

(provide 'eproject-arduino)

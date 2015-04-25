;;; Author: Antono Vasiljev <self@antono.info>

;; Android project
(define-project-type android (generic)
  (and (look-for "AndroidManifest.xml")
       (look-for "project.properties"))
  :main-file "AndroidManifest.xml")

(provide 'eproject-android)

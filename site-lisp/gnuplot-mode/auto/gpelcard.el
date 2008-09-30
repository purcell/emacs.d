(TeX-add-style-hook "gpelcard"
 (function
  (lambda ()
    (LaTeX-add-environments
     "Boxedminipage"
     "SqBoxedminipage")
    (TeX-add-symbols
     "version"
     "revised"
     "file"
     "key"
     "variable"
     "command"
     "Star")
    (TeX-run-style-hooks
     "fancybox"
     "latex2e"
     "art10"
     "article"
     "twocolumn"))))


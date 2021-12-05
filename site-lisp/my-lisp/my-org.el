;;; my-org.el --- Provide custom configurations
;;; Commentary:
;;; Code:

;; Org mode

;; Org preview
(require-package 'org-preview-html)

(setq org-latex-pdf-process
      (let
          ((cmd (concat "pdflatex -shell-escape -interaction nonstopmode"
                        " --synctex=1"
                        " -output-directory %o %f")))
        (list cmd
              "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
              "cd %o; bibtex %b"
              cmd
              cmd)))

(require-package 'org-roam)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Notes/Brain"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(require-package 'websocket)
(require 'org-roam-ui)
(require 'org-roam-protocol)

(plist-put org-format-latex-options :scale 3)

(provide 'my-org)
;;; End

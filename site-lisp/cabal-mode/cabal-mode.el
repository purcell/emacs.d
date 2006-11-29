;;;; cabal-mode -- Major mode for editting .cabal files, which are
;;;; specifications for a Haskell distribution and build system.
;;;;
;;;; Author: Matthew Danish <mrd@cs.cmu.edu>

(require 'tempo)

(add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'cabal-mode))

(defvar cabal-mode-font-lock-keywords
  '(("\\(--.*\\)" 1 'font-lock-comment-face)
    ("^[^:]*:" 1 'font-lock-keyword-face)))
  
(define-derived-mode cabal-mode fundamental-mode "Cabal"
  "Major mode to edit Cabal files.
Cabal is specified here: <http://haskell.org/cabal/>

This mode provides a number of templates for quick construction of basic Cabal files.

Type C-h b to see the full list."
  (set (make-local-variable 'font-lock-keywords)
       '(cabal-mode-font-lock-keywords))
  (set (make-local-variable 'comment-start) "--"))

(defvar cabal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") 'cabal-insert-basic) 
    (define-key map (kbd "C-c l") 'cabal-insert-basic-library)
    (define-key map (kbd "C-c e") 'cabal-insert-executable-stanza)
    map) 
  "Keymap for `cabal-mode'.")

(setq tempo-interactive t)
(tempo-define-template "basic-cabal"
                       '(& "Name:\t\t" (p "Name: ") &
                         "Version:\t" (p "Version: ") &
                         "License:\t" (cabal-prompt-license) &
                         "Author:\t\t" (cabal-prompt-author) &
                         "Maintainer:\t" (cabal-prompt-maintainer) &
                         "Build-Depends:\tbase" &
                         "Synopsis:\t" (p "Synopsis: ") &
                         "Description:" & "\t" (p "Description: ")))
(tempo-define-template "exposed-modules"
                       '(& "Exposed-Modules: " & "\t" (p "Exposed-Modules: ")))
(tempo-define-template "cabal-executable"
                       '(& "Executable:\t" (p "Executable: ") &
                           "Main-Is:\t" (p "Main-Is: ") &
                           "Hs-Source-Dirs:\t" (p "Hs-Source-Dirs: ") &
                           "Other-Modules:\t" (p "Other-Modules: ") &))

        
(defvar cabal-license-completions
  '(("AllRightsReserved")
    ("GPL")
    ("LGPL")
    ("BSD3")
    ("BSD4")
    ("PublicDomain")
    ("OtherLicense")))

(defun cabal-prompt-license ()
  (interactive)
  (cabal-prompt "License" cabal-license-completions "AllRightsReserved"))

(defun cabal-prompt-author ()
  (interactive)
  (let* ((name user-full-name)
         (email user-mail-address)
         (default (if name (concat name (if email (concat " <" email ">") "")) "")))
    (read-string (if name
                     (concat "Author (default: " default "): ")
                   "Author: ")
                 nil nil default)))

(defun cabal-prompt-maintainer ()
  (interactive)
  (cabal-prompt-email "Maintainer"))

(defun cabal-prompt-email (&optional label)
  (let ((default user-mail-address))
    (read-string (if default
                     (concat (if label (concat label " ") "") "E-mail (default: " default "): ")
                   (concat (if label (concat label " ") "") "E-mail: "))
                 nil nil default)))

(defun cabal-prompt (prompt table default)
  (let ((completion-ignore-case t))
    (completing-read (concat prompt " (default: " default "): ") table nil t nil nil default)))

;; --------------------------------------------------
;; | Keybound functions
(defun cabal-insert-basic ()
  (interactive)
  (tempo-template-basic-cabal))

(defun cabal-insert-basic-library ()
  (interactive)
  (tempo-template-basic-cabal)
  (insert "\n")
  (tempo-template-exposed-modules)
  (insert "\n"))

(defun cabal-insert-executable-stanza ()
  (interactive)
  (goto-char (point-max))
  (insert "\n\n")
  (tempo-template-cabal-executable))
;; --------------------------------------------------


(provide 'cabal-mode)

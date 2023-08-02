;; Install third-party treesitter-based modes
(require-package 'clojure-ts-mode)


;; You can download per-architecture pre-compiled release from
;; https://github.com/emacs-tree-sitter/tree-sitter-langs Rename
;; contained grammars to add prefix "libtree-sitter-", place in
;; ~/.emacs.d/tree-sitter.
;;
;; Nix users can pre-install all grammars alongside their Emacs, see
;; https://github.com/nix-community/emacs-overlay/issues/341
;;
;; Note that grammar files from different sources can be differently
;; named and configured, so there could be different results. Some
;; common remappings are included below.
(setq treesit-load-name-override-list nil
      major-mode-remap-alist nil)

;; Go through all the installed grammars and configure corresponding emacs ts-modes if they
;; exist.
(let ((alternates '(("c-sharp" . "csharp")
                    ("cpp" . "c++")
                    ("gomod" . "go-mod")
                    ("javascript" . "js"))))
  (dolist (dir (cons (expand-file-name "tree-sitter" user-emacs-directory) treesit-extra-load-path))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir))
        (let ((fname (file-name-sans-extension (file-name-nondirectory file))))
          (when (string-match "libtree-sitter-\\(.*\\)" fname)
            (let* ((file-lang (match-string 1 fname))
                   (emacs-lang (or (cdr (assoc-string file-lang alternates)) file-lang)))
              (unless (string-equal file-lang emacs-lang)
                (push (list (intern emacs-lang)
                            fname
                            (concat "tree_sitter_" (replace-regexp-in-string "-" "_" file-lang)))
                      treesit-load-name-override-list))
              ;; TODO: don't reconfigure if we've already found a lib earlier in the treesit load path
              (let ((ts-mode-name (intern (concat emacs-lang "-ts-mode")))
                    (regular-mode-name (intern (concat emacs-lang "-mode"))))
                (when (fboundp ts-mode-name)
                  (push (cons regular-mode-name ts-mode-name)
                        major-mode-remap-alist))))))))))


;; When there's js-ts-mode, we prefer it to js2-mode
(when-let (jsmap (assoc 'js-mode major-mode-remap-alist))
  (push (cons 'js2-mode (cdr jsmap)) major-mode-remap-alist))



;; Default
;; (setq treesit-font-lock-level 3)



(provide 'init-treesitter)

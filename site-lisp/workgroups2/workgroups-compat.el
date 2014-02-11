;;; workgroups-compat --- some functions for different versions of Emacs
;;; Commentary:
;; flet (Temporary redifinition) command caused a lot of warnings and is
;; obsolete
;;
;; So we need to define something new.
;; I picked up dflet.el module

;;; Code:

;; Emacs 24.3+, use "cl-labels" instead of "labels"
(if (version< emacs-version "24.3")
    (progn
      (require 'cl)
      (defalias 'wg-every 'every)
      (defalias 'wg-mapcan 'mapcan)
      (defalias 'wg-mapcar* 'mapcar*)
      (defalias 'wg-copy-list 'copy-list)
      (defalias 'wg-find 'find)
      (defalias 'wg-gensym 'gensym)
      (defalias 'wg-position 'position)
      (defalias 'wg-reduce 'reduce)
      (defalias 'wg-remove* 'remove*)
      (defalias 'wg-remove-duplicates 'remove-duplicates)
      (defalias 'wg-remove-if 'remove-if)
      (defalias 'wg-remove-if-not 'remove-if-not)
      (defalias 'wg-labels 'labels)
      (defalias 'wg-some 'some)
      (defalias 'wg-subsec 'subseq)
      (defalias 'wg-union 'union))
  (progn
    (require 'cl-lib)
    (defalias 'wg-every 'cl-every)
    (defalias 'wg-mapcan 'cl-mapcan)
    (defalias 'wg-mapcar* 'cl-mapcar)
    (defalias 'wg-copy-list 'cl-copy-list)
    (defalias 'wg-find 'cl-find)
    (defalias 'wg-gensym 'cl-gensym)
    (defalias 'wg-position 'cl-position)
    (defalias 'wg-reduce 'cl-reduce)
    (defalias 'wg-remove* 'cl-remove)
    (defalias 'wg-remove-duplicates 'cl-remove-duplicates)
    (defalias 'wg-remove-if 'cl-remove-if)
    (defalias 'wg-remove-if-not 'cl-remove-if-not)
    (defalias 'wg-labels 'cl-labels)
    (defalias 'wg-some 'cl-some)
    (defalias 'wg-subsec 'cl-subseq)
    (defalias 'wg-union 'cl-union)))


(provide 'workgroups-compat)
;;; workgroups-compat.el ends here

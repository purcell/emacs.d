;;; scala-mode-prettify-symbols.el --- Prettifying scala symbols -*- coding: utf-8; -*-

;; Copyright (c) 2016 Merlin Göttlinger
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;  Suggested `prettify-symbols' for Scala editing, enable
;;  `prettify-symbols-mode' and `setq' an alist of your choice
;;  for `prettify-symbols-alist'.

;;; Code:

(defconst
  scala-mode-pretty-bool-alist
  '(("<=" . ?≤)
    (">=" . ?≥)
    ("==" . ?≡)
    ("===" . ?≣)
    ("!" . ?¬)
    ("!=" . ?≢)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("true" . ?⊤)
    ("false" . ?⊥)
    ("Boolean" . ?𝔹))
  "Prettify rules for boolean related operations.")

(defconst
  scala-mode-pretty-collection-alist
  '(("empty" . ?∅)
    ("sum" . ?∑)
    ("product" . ?∏)
    ("contains" . ?∍)
    ("forall" . ?∀)
    ("any" . ?∃)
    ("intersect" . ?∩)
    ("union" . ?∪)
    ("diff" . ?≏)
    ("subsetOf" . ?⊆)
    ("++" . ?⧺)
    ("::" . ?⸬)
    ("--" . ?╌))
  "Prettify rules for collections related operations.")

(defconst
  scala-mode-pretty-arrows-alist
  '(("->" . ?→)
    ("<-" . ?←)
    ("=>" . ?⇒)
    ("<=>" . ?⇔)
    ("-->" . ?⟶)
    ("<->" . ?↔)
    ("<--" . ?⟵)
    ("<-->" . ?⟷)
    ("==>" . ?⟹)
    ("<==" . ?⟸)
    ("<==>" . ?⟺)
    ("~>" . ?⇝)
    ("<~" . ?⇜))
  "Prettify rules for arrow related code pieces.")

(defconst
  scala-mode-pretty-misc-alist
  '(("Unit" . ?∅)
    ("Int" . ?ℤ)
    ("assert" . ?⊦)
    (":=" . ?≔))
  "Prettify rules for other mixed code pieces.")

(defconst
  scala-mode-pretty-categories-alist
  '(("flatMap" . ?⤜)
    (">>=" . ?⤜)
    ("bind" . ?⤜)
    (">>" . ?≫)
    ("followedBy" . ?≫)
    ("<+>" . ?⊕))
  "Prettify rules for category theory related operators (for use with cats/scalaz/...).")

(defcustom
  scala-prettify-symbols-alist
  (append
   scala-mode-pretty-bool-alist
   scala-mode-pretty-collection-alist
   scala-mode-pretty-arrows-alist
   scala-mode-pretty-misc-alist
   scala-mode-pretty-categories-alist)
  "All prettify rules to be applied in scala code."
  :type 'alist
  :group 'scala)

(provide 'scala-mode-prettify-symbols)
;;; scala-mode-prettify-symbols.el ends here

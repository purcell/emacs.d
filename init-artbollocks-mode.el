(require 'artbollocks-mode)
;; Avoid these phrases
(setq weasel-words-regex
      (concat "\\b" (regexp-opt
                      '("one of the"
                        "possibily"
                        "should"
                        "just"
                        "sort of"
                        "a lot"
                        "probably"
                        "maybe"
                        "perhaps"
                        "I think"
                        "really"
                        "pretty"
                        "maybe"
                        "nice"
                        "action"
                        "utilize"
                        "leverage") t) "\\b"))
;; Fix a bug in the regular expression to catch repeated words
(setq lexical-illusions-regex "\\b\\(\\w+\\)\\W+\\(\\1\\)\\b")
;; Don't show the art critic words, or at least until I figure
;; out my own jargon
(setq artbollocks nil)
;; Make sure keywords are case-insensitive
(defadvice search-for-keyword (around sacha activate)
           "Match in a case-insensitive way."
           (let ((case-fold-search t))
             ad-do-it))
(add-hook 'text-mode-hook 'turn-on-artbollocks-mode)
(add-hook 'org-mode-hook 'turn-on-artbollocks-mode)

(provide 'init-artbollocks-mode)

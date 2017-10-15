;;; auto-complete-config.el --- auto-complete additional configuations

;; Copyright (C) 2009, 2010  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: convenience
;; Version: 1.5.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'auto-complete)

(declare-function semantic-analyze-current-context "semantic/analyze")
(declare-function semantic-tag-class "semantic/tag")
(declare-function semantic-tag-function-arguments "semantic/tag")
(declare-function semantic-format-tag-type "semantic/format")
(declare-function semantic-format-tag-name "semantic/format")
(declare-function yas-expand-snippet "yasnippet")
(declare-function oref "eieio" (obj slot))



;;;; Additional sources

;; imenu

(defvar ac-imenu-index nil)

(ac-clear-variable-every-10-minutes 'ac-imenu-index)

(defun ac-imenu-candidates ()
  (cl-loop with i = 0
           with stack = (progn
                          (unless (local-variable-p 'ac-imenu-index)
                            (make-local-variable 'ac-imenu-index))
                          (or ac-imenu-index
                              (setq ac-imenu-index
                                    (ignore-errors
                                      (with-no-warnings
                                        (imenu--make-index-alist))))))
           with result
           while (and stack (or (not (integerp ac-limit))
                                (< i ac-limit)))
           for node = (pop stack)
           if (consp node)
           do
           (let ((car (car node))
                 (cdr (cdr node)))
             (if (consp cdr)
                 (mapc (lambda (child)
                         (push child stack))
                       cdr)
               (when (and (stringp car)
                          (string-match (concat "^" (regexp-quote ac-prefix)) car))
                 ;; Remove extra characters
                 (if (string-match "^.*\\(()\\|=\\|<>\\)$" car)
                     (setq car (substring car 0 (match-beginning 1))))
                 (push car result)
                 (cl-incf i))))
           finally return (nreverse result)))

(ac-define-source imenu
  '((depends imenu)
    (candidates . ac-imenu-candidates)
    (symbol . "s")))

;; gtags

(defface ac-gtags-candidate-face
  '((t (:inherit ac-candidate-face :foreground "navy")))
  "Face for gtags candidate"
  :group 'auto-complete)

(defface ac-gtags-selection-face
  '((t (:inherit ac-selection-face :background "navy")))
  "Face for the gtags selected candidate."
  :group 'auto-complete)

(defun ac-gtags-candidate ()
  (ignore-errors
    (split-string (shell-command-to-string (format "global -ciq %s" ac-prefix)) "\n")))

(ac-define-source gtags
  '((candidates . ac-gtags-candidate)
    (candidate-face . ac-gtags-candidate-face)
    (selection-face . ac-gtags-selection-face)
    (requires . 3)
    (symbol . "s")))

;; yasnippet

(defface ac-yasnippet-candidate-face
  '((t (:inherit ac-candidate-face
                 :background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate."
  :group 'auto-complete)

(defface ac-yasnippet-selection-face
  '((t (:inherit ac-selection-face :background "coral3")))
  "Face for the yasnippet selected candidate."
  :group 'auto-complete)

(defun ac-yasnippet-table-hash (table)
  (cond
   ((fboundp 'yas/snippet-table-hash)
    (yas/snippet-table-hash table))
   ((fboundp 'yas/table-hash)
    (yas/table-hash table))))

(defun ac-yasnippet-table-parent (table)
  (cond
   ((fboundp 'yas/snippet-table-parent)
    (yas/snippet-table-parent table))
   ((fboundp 'yas/table-parent)
    (yas/table-parent table))))

(defun ac-yasnippet-candidate-1 (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (setq candidates (all-completions ac-prefix (nreverse candidates)))
      (if parent
          (setq candidates
                (append candidates (ac-yasnippet-candidate-1 parent))))
      candidates)))

(defun ac-yasnippet-candidates ()
  (with-no-warnings
    (cond (;; 0.8 onwards
           (fboundp 'yas-active-keys)
           (all-completions ac-prefix (yas-active-keys)))
          (;; >0.6.0
           (fboundp 'yas/get-snippet-tables)
           (apply 'append (mapcar 'ac-yasnippet-candidate-1
                                  (condition-case nil
                                      (yas/get-snippet-tables major-mode)
                                    (wrong-number-of-arguments
                                     (yas/get-snippet-tables)))))
           )
          (t
           (let ((table
                  (if (fboundp 'yas/snippet-table)
                      ;; <0.6.0
                      (yas/snippet-table major-mode)
                    ;; 0.6.0
                    (yas/current-snippet-table))))
             (if table
                 (ac-yasnippet-candidate-1 table)))))))

(ac-define-source yasnippet
  '((depends yasnippet)
    (candidates . ac-yasnippet-candidates)
    (action . yas/expand)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face)
    (symbol . "a")))

;; semantic

(defun ac-semantic-candidates (prefix)
  (with-no-warnings
    (delete ""            ; semantic sometimes returns an empty string
            (mapcar (lambda (elem)
                      (cons (semantic-tag-name elem)
                            (semantic-tag-clone elem)))
                    (ignore-errors
                      (or (semantic-analyze-possible-completions
                           (semantic-analyze-current-context))
                          (senator-find-tag-for-completion prefix)))))))

(defun ac-semantic-doc (symbol)
  (with-no-warnings
    (let* ((proto (semantic-format-tag-summarize-with-file symbol nil t))
           (doc (semantic-documentation-for-tag symbol))
           (res proto))
      (when doc
        (setq res (concat res "\n\n" doc)))
      res)))

(defun ac-semantic-action ()
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    (let* ((tag (car (last (oref (semantic-analyze-current-context) prefix))))
           (class (semantic-tag-class tag))
           (args))
      (when (eq class 'function)
        (setq args (semantic-tag-function-arguments tag))
        (yas-expand-snippet
         (concat "("
                 (mapconcat
                  (lambda (arg)
                    (let ((arg-type (semantic-format-tag-type arg nil))
                          (arg-name (semantic-format-tag-name arg nil)))
                      (concat "${"
                              (if (string= arg-name "")
                                  arg-type
                                (concat arg-type " " arg-name))
                              "}")))
                  args
                  ", ")
                 ")$0"))))))

(ac-define-source semantic
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . ac-semantic-doc)
    (action . ac-semantic-action)
    (prefix . cc-member)
    (requires . 0)
    (symbol . "m")))

(ac-define-source semantic-raw
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . ac-semantic-doc)
    (action . ac-semantic-action)
    (symbol . "s")))

;; eclim

(defun ac-eclim-candidates ()
  (with-no-warnings
    (cl-loop for c in (eclim/java-complete)
             collect (nth 1 c))))

(ac-define-source eclim
  '((candidates . ac-eclim-candidates)
    (prefix . c-dot)
    (requires . 0)
    (symbol . "f")))

;; css

;; Copied from company-css.el
(defconst ac-css-property-alist
  ;; see http://www.w3.org/TR/CSS21/propidx.html
  '(("azimuth" angle "left-side" "far-left" "left" "center-left" "center"
     "center-right" "right" "far-right" "right-side" "behind" "leftwards"
     "rightwards")
    ("background" background-color background-image background-repeat
     background-attachment background-position)
    ("background-attachment" "scroll" "fixed")
    ("background-color" color "transparent")
    ("background-image" uri "none")
    ("background-position" percentage length "left" "center" "right" percentage
     length "top" "center" "bottom" "left" "center" "right" "top" "center"
     "bottom")
    ("background-repeat" "repeat" "repeat-x" "repeat-y" "no-repeat")
    ("border" border-width border-style border-color)
    ("border-bottom" border)
    ("border-bottom-color" border-color)
    ("border-bottom-style" border-style)
    ("border-bottom-width" border-width)
    ("border-collapse" "collapse" "separate")
    ("border-color" color "transparent")
    ("border-left" border)
    ("border-left-color" border-color)
    ("border-left-style" border-style)
    ("border-left-width" border-width)
    ("border-right" border)
    ("border-right-color" border-color)
    ("border-right-style" border-style)
    ("border-right-width" border-width)
    ("border-spacing" length length)
    ("border-style" border-style)
    ("border-top" border)
    ("border-top-color" border-color)
    ("border-top-style" border-style)
    ("border-top-width" border-width)
    ("border-width" border-width)
    ("bottom" length percentage "auto")
    ("caption-side" "top" "bottom")
    ("clear" "none" "left" "right" "both")
    ("clip" shape "auto")
    ("color" color)
    ("content" "normal" "none" string uri counter "attr()" "open-quote"
     "close-quote" "no-open-quote" "no-close-quote")
    ("counter-increment" identifier integer "none")
    ("counter-reset" identifier integer "none")
    ("cue" cue-before cue-after)
    ("cue-after" uri "none")
    ("cue-before" uri "none")
    ("cursor" uri "*" "auto" "crosshair" "default" "pointer" "move" "e-resize"
     "ne-resize" "nw-resize" "n-resize" "se-resize" "sw-resize" "s-resize"
     "w-resize" "text" "wait" "help" "progress")
    ("direction" "ltr" "rtl")
    ("display" "inline" "block" "list-item" "run-in" "inline-block" "table"
     "inline-table" "table-row-group" "table-header-group" "table-footer-group"
     "table-row" "table-column-group" "table-column" "table-cell"
     "table-caption" "none")
    ("elevation" angle "below" "level" "above" "higher" "lower")
    ("empty-cells" "show" "hide")
    ("float" "left" "right" "none")
    ("font" font-style font-variant font-weight font-size "/" line-height
     font-family "caption" "icon" "menu" "message-box" "small-caption"
     "status-bar")
    ("font-family" family-name generic-family)
    ("font-size" absolute-size relative-size length percentage)
    ("font-style" "normal" "italic" "oblique")
    ("font-variant" "normal" "small-caps")
    ("font-weight" "normal" "bold" "bolder" "lighter" "100" "200" "300" "400"
     "500" "600" "700" "800" "900")
    ("height" length percentage "auto")
    ("left" length percentage "auto")
    ("letter-spacing" "normal" length)
    ("line-height" "normal" number length percentage)
    ("list-style" list-style-type list-style-position list-style-image)
    ("list-style-image" uri "none")
    ("list-style-position" "inside" "outside")
    ("list-style-type" "disc" "circle" "square" "decimal" "decimal-leading-zero"
     "lower-roman" "upper-roman" "lower-greek" "lower-latin" "upper-latin"
     "armenian" "georgian" "lower-alpha" "upper-alpha" "none")
    ("margin" margin-width)
    ("margin-bottom" margin-width)
    ("margin-left" margin-width)
    ("margin-right" margin-width)
    ("margin-top" margin-width)
    ("max-height" length percentage "none")
    ("max-width" length percentage "none")
    ("min-height" length percentage)
    ("min-width" length percentage)
    ("orphans" integer)
    ("outline" outline-color outline-style outline-width)
    ("outline-color" color "invert")
    ("outline-style" border-style)
    ("outline-width" border-width)
    ("overflow" "visible" "hidden" "scroll" "auto")
    ("padding" padding-width)
    ("padding-bottom" padding-width)
    ("padding-left" padding-width)
    ("padding-right" padding-width)
    ("padding-top" padding-width)
    ("page-break-after" "auto" "always" "avoid" "left" "right")
    ("page-break-before" "auto" "always" "avoid" "left" "right")
    ("page-break-inside" "avoid" "auto")
    ("pause" time percentage)
    ("pause-after" time percentage)
    ("pause-before" time percentage)
    ("pitch" frequency "x-low" "low" "medium" "high" "x-high")
    ("pitch-range" number)
    ("play-during" uri "mix" "repeat" "auto" "none")
    ("position" "static" "relative" "absolute" "fixed")
    ("quotes" string string "none")
    ("richness" number)
    ("right" length percentage "auto")
    ("speak" "normal" "none" "spell-out")
    ("speak-header" "once" "always")
    ("speak-numeral" "digits" "continuous")
    ("speak-punctuation" "code" "none")
    ("speech-rate" number "x-slow" "slow" "medium" "fast" "x-fast" "faster"
     "slower")
    ("stress" number)
    ("table-layout" "auto" "fixed")
    ("text-align" "left" "right" "center" "justify")
    ("text-decoration" "none" "underline" "overline" "line-through" "blink")
    ("text-indent" length percentage)
    ("text-transform" "capitalize" "uppercase" "lowercase" "none")
    ("top" length percentage "auto")
    ("unicode-bidi" "normal" "embed" "bidi-override")
    ("vertical-align" "baseline" "sub" "super" "top" "text-top" "middle"
     "bottom" "text-bottom" percentage length)
    ("visibility" "visible" "hidden" "collapse")
    ("voice-family" specific-voice generic-voice "*" specific-voice
     generic-voice)
    ("volume" number percentage "silent" "x-soft" "soft" "medium" "loud"
     "x-loud")
    ("white-space" "normal" "pre" "nowrap" "pre-wrap" "pre-line")
    ("widows" integer)
    ("width" length percentage "auto")
    ("word-spacing" "normal" length)
    ("z-index" "auto" integer))
  "A list of CSS properties and their possible values.")

(defconst ac-css-value-classes
  '((absolute-size "xx-small" "x-small" "small" "medium" "large" "x-large"
                   "xx-large")
    (border-style "none" "hidden" "dotted" "dashed" "solid" "double" "groove"
                  "ridge" "inset" "outset")
    (color "aqua" "black" "blue" "fuchsia" "gray" "green" "lime" "maroon" "navy"
           "olive" "orange" "purple" "red" "silver" "teal" "white" "yellow"
           "rgb")
    (counter "counter")
    (family-name "Courier" "Helvetica" "Times")
    (generic-family "serif" "sans-serif" "cursive" "fantasy" "monospace")
    (generic-voice "male" "female" "child")
    (margin-width "auto") ;; length percentage
    (relative-size "larger" "smaller")
    (shape "rect")
    (uri "url"))
  "A list of CSS property value classes and their contents.")

(defconst ac-css-pseudo-classes
  '("active" "after" "before" "first" "first-child" "first-letter" "first-line"
    "focus" "hover" "lang" "left" "link" "right" "visited")
  "Identifiers for CSS pseudo-elements and pseudo-classes.")

(defvar ac-css-property nil
  "Current editing property.")

(defun ac-css-prefix ()
  (when (save-excursion (re-search-backward "\\_<\\(.+?\\)\\_>\\s *:[^;]*\\=" nil t))
    (setq ac-css-property (match-string 1))
    (or (ac-prefix-symbol) (point))))

(defun ac-css-property-candidates ()
  (let ((list (assoc-default ac-css-property ac-css-property-alist)))
    (if list
        (cl-loop with seen
                 with value
                 while (setq value (pop list))
                 if (symbolp value)
                 do (unless (memq value seen)
                      (push value seen)
                      (setq list
                            (append list
                                    (or (assoc-default value ac-css-value-classes)
                                        (assoc-default (symbol-name value) ac-css-property-alist)))))
                 else collect value)
      ac-css-pseudo-classes)))

(ac-define-source css-property
  '((candidates . ac-css-property-candidates)
    (prefix . ac-css-prefix)
    (requires . 0)))

;; slime
(ac-define-source slime
  '((depends slime)
    (candidates . (car (slime-simple-completions ac-prefix)))
    (symbol . "s")
    (cache)))

;; ghc-mod
(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))



;;;; Not maintained sources

;; ropemacs

(defvar ac-ropemacs-loaded nil)
(defun ac-ropemacs-require ()
  (with-no-warnings
    (unless ac-ropemacs-loaded
      (pymacs-load "ropemacs" "rope-")
      (if (boundp 'ropemacs-enable-autoimport)
          (setq ropemacs-enable-autoimport t))
      (setq ac-ropemacs-loaded t))))

(defun ac-ropemacs-setup ()
  (ac-ropemacs-require)
  ;(setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
  (setq ac-omni-completion-sources '(("\\." ac-source-ropemacs))))

(defun ac-ropemacs-initialize ()
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (add-hook 'python-mode-hook 'ac-ropemacs-setup)
  t)

(defvar ac-ropemacs-completions-cache nil)
(defvar ac-source-ropemacs
  '((init
     . (lambda ()
         (setq ac-ropemacs-completions-cache
               (mapcar
                (lambda (completion)
                  (concat ac-prefix completion))
                (ignore-errors
                  (rope-completions))))))
    (candidates . ac-ropemacs-completions-cache)))

;; rcodetools

(defvar ac-source-rcodetools
  '((init . (lambda ()
              (require 'rcodetools)
              (condition-case x
                  (save-excursion
                    (rct-exec-and-eval rct-complete-command-name "--completion-emacs-icicles"))
                (error) (setq rct-method-completion-table nil))))
    (candidates . (lambda ()
                    (all-completions
                     ac-prefix
                     (mapcar
                      (lambda (completion)
                        (replace-regexp-in-string "\t.*$" "" (car completion)))
                      rct-method-completion-table))))))



;;;; Default settings

(defun ac-common-setup ()
  ;(add-to-list 'ac-sources 'ac-source-filename)
  )

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features ac-source-functions ac-source-yasnippet ac-source-variables ac-source-symbols) ac-sources)))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet ac-source-gtags) ac-sources)))

(defun ac-ruby-mode-setup ())

(defun ac-css-mode-setup ()
  (setq ac-sources (append '(ac-source-css-property) ac-sources)))

;;;###autoload
(defun ac-config-default ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(provide 'auto-complete-config)
;;; auto-complete-config.el ends here

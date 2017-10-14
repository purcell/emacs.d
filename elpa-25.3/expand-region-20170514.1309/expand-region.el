;;; expand-region.el --- Increase selected region by semantic units.

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region

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

;; Expand region increases the selected region by semantic units. Just keep
;; pressing the key until it selects what you want.

;; An example:

;;     (setq alphabet-start "abc def")

;; With the cursor at the `c`, it starts by marking the entire word `abc`, then
;; expand to the contents of the quotes `abc def`, then to the entire quote
;; `"abc def"`, then to the contents of the sexp `setq alphabet-start "abc def"`
;; and finally to the entire sexp.

;; You can set it up like this:

;;     (require 'expand-region)
;;     (global-set-key (kbd "C-=") 'er/expand-region)

;; There's also `er/contract-region` if you expand too far.

;; ## Video

;; You can [watch an intro to expand-region at Emacs Rocks](http://emacsrocks.com/e09.html).

;; ## Language support

;; Expand region works fairly well with most languages, due to the general
;; nature of the basic expansions:

;;     er/mark-word
;;     er/mark-symbol
;;     er/mark-method-call
;;     er/mark-inside-quotes
;;     er/mark-outside-quotes
;;     er/mark-inside-pairs
;;     er/mark-outside-pairs

;; However, most languages also will benefit from some specially crafted
;; expansions. For instance, expand-region comes with these extra expansions for
;; html-mode:

;;     er/mark-html-attribute
;;     er/mark-inner-tag
;;     er/mark-outer-tag

;; You can add your own expansions to the languages of your choice simply by
;; creating a function that looks around point to see if it's inside or looking
;; at the construct you want to mark, and if so - mark it.

;; There's plenty of examples to look at in these files.

;; After you make your function, add it to a buffer-local version of
;; the `er/try-expand-list`.

;; **Example:**

;; Let's say you want expand-region to also mark paragraphs and pages in
;; text-mode. Incidentally Emacs already comes with `mark-paragraph` and
;; `mark-page`. To add it to the try-list, do this:

;;     (defun er/add-text-mode-expansions ()
;;       (make-variable-buffer-local 'er/try-expand-list)
;;       (setq er/try-expand-list (append
;;                                 er/try-expand-list
;;                                 '(mark-paragraph
;;                                   mark-page))))

;;     (er/enable-mode-expansions 'text-mode 'er/add-text-mode-expansions)

;; Add that to its own file, and require it at the bottom of this one,
;; where it says "Mode-specific expansions"

;; **Warning:** Badly written expansions might slow down expand-region
;; dramatically. Remember to exit quickly before you start traversing
;; the entire document looking for constructs to mark.

;; ## Contribute

;; If you make some nice expansions for your favorite mode, it would be
;; great if you opened a pull-request. The repo is at:

;;     https://github.com/magnars/expand-region.el

;; Changes to `expand-region-core` itself must be accompanied by feature tests.
;; They are written in [Ecukes](http://ecukes.info), a Cucumber for Emacs.

;; To fetch the test dependencies:

;;     $ cd /path/to/expand-region
;;     $ git submodule init
;;     $ git submodule update

;; Run the tests with:

;;     $ ./util/ecukes/ecukes features

;; If you want to add feature-tests for your mode-specific expansions as well,
;; that is utterly excellent.

;; ## Contributors

;; * [Josh Johnston](https://github.com/joshwnj) contributed `er/contract-region`
;; * [Le Wang](https://github.com/lewang) contributed consistent handling of the mark ring, expanding into pairs/quotes just left of the cursor, and general code clean-up.
;; * [Matt Briggs](https://github.com/mbriggs) contributed expansions for ruby-mode.
;; * [Ivan Andrus](https://github.com/gvol) contributed expansions for python-mode, text-mode, LaTeX-mode and nxml-mode.
;; * [Raimon Grau](https://github.com/kidd) added support for when transient-mark-mode is off.
;; * [Gleb Peregud](https://github.com/gleber) contributed expansions for erlang-mode.
;; * [fgeller](https://github.com/fgeller) and [edmccard](https://github.com/edmccard) contributed better support for python and its multiple modes.
;; * [François Févotte](https://github.com/ffevotte) contributed expansions for C and C++.
;; * [Roland Walker](https://github.com/rolandwalker) added option to copy the contents of the most recent action to a register, and some fixes.
;; * [Damien Cassou](https://github.com/DamienCassou) added option to continue expanding/contracting with fast keys after initial expand.

;; Thanks!

;;; Code:

(require 'expand-region-core)
(require 'expand-region-custom)
(require 'er-basic-expansions)

;;;###autoload
(defun er/expand-region (arg)
  "Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time."
  (interactive "p")
  (if (< arg 1)
      (er/contract-region (- arg))
    (er--prepare-expanding)
    (while (>= arg 1)
      (setq arg (- arg 1))
      (when (eq 'early-exit (er--expand-region-1))
        (setq arg 0)))
    (when (and expand-region-fast-keys-enabled
               (not (memq last-command '(er/expand-region er/contract-region))))
      (er/prepare-for-more-expansions))))

(eval-after-load 'clojure-mode   '(require 'clojure-mode-expansions))
(eval-after-load 'css-mode       '(require 'css-mode-expansions))
(eval-after-load 'erlang-mode    '(require 'erlang-mode-expansions))
(eval-after-load 'feature-mode   '(require 'feature-mode-expansions))
(eval-after-load 'sgml-mode      '(require 'html-mode-expansions)) ;; html-mode is defined in sgml-mode.el
(eval-after-load 'rhtml-mode     '(require 'html-mode-expansions))
(eval-after-load 'nxhtml-mode    '(require 'html-mode-expansions))
(eval-after-load 'web-mode       '(require 'web-mode-expansions))
(eval-after-load 'js             '(require 'js-mode-expansions))
(eval-after-load 'js2-mode       '(require 'js-mode-expansions))
(eval-after-load 'js2-mode       '(require 'js2-mode-expansions))
(eval-after-load 'js3-mode       '(require 'js-mode-expansions))
(eval-after-load 'latex          '(require 'latex-mode-expansions))
(eval-after-load 'nxml-mode      '(require 'nxml-mode-expansions))
(eval-after-load 'octave-mod     '(require 'octave-expansions))
(eval-after-load 'octave         '(require 'octave-expansions))
(eval-after-load 'python         '(progn
                                    (when expand-region-guess-python-mode
                                      (expand-region-guess-python-mode))
                                    (if (eq 'python expand-region-preferred-python-mode)
                                        (require 'python-el-expansions)
                                      (require 'python-el-fgallina-expansions))))
(eval-after-load 'python-mode    '(require 'python-mode-expansions))
(eval-after-load 'ruby-mode      '(require 'ruby-mode-expansions))
(eval-after-load 'org            '(require 'the-org-mode-expansions))
(eval-after-load 'cc-mode        '(require 'cc-mode-expansions))
(eval-after-load "text-mode"      '(require 'text-mode-expansions))
(eval-after-load 'cperl-mode     '(require 'cperl-mode-expansions))
(eval-after-load 'sml-mode       '(require 'sml-mode-expansions))
(eval-after-load 'enh-ruby-mode  '(require 'enh-ruby-mode-expansions))
(eval-after-load 'subword-mode   '(require 'subword-mode-expansions))

(provide 'expand-region)

;;; expand-region.el ends here

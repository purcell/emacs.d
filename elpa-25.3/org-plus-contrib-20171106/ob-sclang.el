;;; ob-sclang.el --- SCLang support for Org-mode Babel
;;; -*- coding: utf-8 -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Version: 0.1
;; Keywords: babel sclang

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `ob-sclang' requires `sclang-interp' from SuperCollider.
;; Usually SuperCollider dependencies for Emacs are at /usr/share/emacs/site-lisp/SuperCollider/
;; You can install SuperCollider following this article:
;; https://github.com/supercollider/supercollider#building-the-source-code

;; Usage:

;; Support to evaluate sclang Org-mode src block with function `sclang-eval-string'.

;; For example:

;; #+BEGIN_SRC sclang :results none
;; "Hello World".postln;
;; #+END_SRC
;;
;; *NOTE* Temporary output to org-babel result output is not supported.
;; Because `sclang-eval-string' will send output to Sclang Post Buffer.
;; And command line `sclang' execute will not automatically stop after finished execution.
;;
;; #+BEGIN_SRC sclang :results none
;; // modulate a sine frequency and a noise amplitude with another sine
;; // whose frequency depends on the horizontal mouse pointer position
;; {
;; var x = SinOsc.ar(MouseX.kr(1, 100));
;; SinOsc.ar(300 * x + 800, 0, 0.1)
;; +
;; PinkNoise.ar(0.1 * x + 0.1)
;; }.play;
;; #+END_SRC


;;; Code:
;;; ----------------------------------------------------------------------------
(require 'org)
(require 'ob)

(require 'sclang-interp)

(defgroup ob-sclang nil
  "org-mode blocks for SuperCollider SCLang."
  :group 'org)

;;;###autoload
(defun org-babel-execute:sclang (body params)
  "Org-mode Babel sclang hook for evaluate `BODY' with `PARAMS'."
  (unless (or (equal (buffer-name) sclang-post-buffer)
              (sclang-get-process))
    (sclang-start))
  (sclang-eval-string body t))

(defvar org-babel-default-header-args:sclang nil)

(setq org-babel-default-header-args:sclang
      '((:session . "*SCLang:Workspace*")
	;; TODO: temporary can't find way to let sclang output to stdout for org-babel.
        (:output . "none")))

(eval-after-load "org"
  '(progn
     (add-to-list 'org-src-lang-modes '("sclang" . sclang))))

;;; ----------------------------------------------------------------------------

(provide 'ob-sclang)

;;; ob-sclang.el ends here

;;; origami-parsers.el --- Collection of parsers  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Version: 1.0
;; Keywords: parsers
;; URL: https://github.com/gregsexton/

;; The MIT License (MIT)

;; Copyright (c) 2014 Greg Sexton

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;;; Code:
(require 'cl)
(require 'dash)

(defun origami-get-positions (content regex)
  "Returns a list of positions where REGEX matches in CONTENT. A
position is a cons cell of the character and the numerical
position in the CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let (acc)
      (while (re-search-forward regex nil t)
        (let ((match (match-string 0)))
          (setq acc (cons (cons match (- (point) (length match)))
                          acc))))
      (reverse acc))))

(defun origami-indent-parser (create)
  (cl-labels ((lines (string) (origami-get-positions string ".*?\r?\n"))
              (annotate-levels (lines)
                               (-map (lambda (line)
                                       ;; TODO: support tabs
                                       (let ((indent (length (car (s-match "^ *" (car line)))))
                                             (beg (cdr line))
                                             (end (+ (cdr line) (length (car line)) -1)))
                                         (if (s-blank? (s-trim (car line)))
                                             'newline ;sentinel representing line break
                                           (vector indent beg end (- end beg)))))
                                     lines))
              (indent (line) (if (eq line 'newline) -1 (aref line 0)))
              (beg (line) (aref line 1))
              (end (line) (aref line 2))
              (offset (line) (aref line 3))
              (collapse-same-level (lines)
                                   (->>
                                    (cdr lines)
                                    (-reduce-from (lambda (acc line)
                                                    (cond ((and (eq line 'newline) (eq (car acc) 'newline)) acc)
                                                          ((= (indent line) (indent (car acc)))
                                                           (cons (vector (indent (car acc))
                                                                         (beg (car acc))
                                                                         (end line)
                                                                         (offset (car acc)))
                                                                 (cdr acc)))
                                                          (t (cons line acc))))
                                                  (list (car lines)))
                                    (remove 'newline)
                                    reverse))
              (create-tree (levels)
                           (if (null levels)
                               levels
                             (let ((curr-indent (indent (car levels))))
                               (->> levels
                                    (-partition-by (lambda (l) (= (indent l) curr-indent)))
                                    (-partition-all 2)
                                    (-mapcat (lambda (x)
                                        ;takes care of multiple identical levels, introduced when there are newlines
                                               (-concat
                                                (-map 'list (butlast (car x)))
                                                (list (cons (-last-item (car x)) (create-tree (cadr x)))))))))))
              (build-nodes (tree)
                           (if (null tree) (cons 0 nil)
                             ;; complexity here is due to having to find the end of the children so that the
                             ;; parent encompasses them
                             (-reduce-r-from (lambda (nodes acc)
                                               (destructuring-bind (children-end . children) (build-nodes (cdr nodes))
                                                 (let ((this-end (max children-end (end (car nodes)))))
                                                   (cons (max this-end (car acc))
                                                         (cons (funcall create
                                                                        (beg (car nodes))
                                                                        this-end
                                                                        (offset (car nodes))
                                                                        children)
                                                               (cdr acc))))))
                                             '(0 . nil)
                                             tree))))
    (lambda (content)
      (-> content
          lines
          annotate-levels
          collapse-same-level
          create-tree
          build-nodes
          cdr))))

(defun origami-build-pair-tree (create open close positions)
  (cl-labels ((build (positions)
                     ;; this is so horrible, but fast
                     (let (acc beg (should-continue t))
                       (while (and should-continue positions)
                         (cond ((equal (caar positions) open)
                                (if beg ;go down a level
                                    (let* ((res (build positions))
                                           (new-pos (car res))
                                           (children (cdr res)))
                                      (setq positions (cdr new-pos))
                                      (setq acc (cons (funcall create beg (cdar new-pos) (length open) children)
                                                      acc))
                                      (setq beg nil))
                                  ;; begin a new pair
                                  (setq beg (cdar positions))
                                  (setq positions (cdr positions))))
                               ((equal (caar positions) close)
                                (if beg
                                    (progn ;close with no children
                                      (setq acc (cons (funcall create beg (cdar positions) (length close) nil)
                                                      acc))
                                      (setq positions (cdr positions))
                                      (setq beg nil))
                                  (setq should-continue nil)))))
                       (cons positions (reverse acc)))))
    (cdr (build positions))))

;;; TODO: tag these nodes? have ability to manipulate nodes that are
;;; tagged? in a scoped fashion?
(defun origami-javadoc-parser (create)
  (lambda (content)
    (let ((positions (->> (origami-get-positions content "/\\*\\*\\|\\*/")
                          (-filter (lambda (position)
                                     (eq (get-text-property 0 'face (car position))
                                         'font-lock-doc-face))))))
      (origami-build-pair-tree create "/**" "*/" positions))))

(defun origami-c-style-parser (create)
  (lambda (content)
    (let ((positions (->> (origami-get-positions content "[{}]")
                          (remove-if (lambda (position)
                                       (let ((face (get-text-property 0 'face (car position))))
                                         (-any? (lambda (f)
                                                  (memq f '(font-lock-doc-face
                                                            font-lock-comment-face
                                                            font-lock-string-face)))
                                                (if (listp face) face (list face)))))))))
      (origami-build-pair-tree create "{" "}" positions))))

(defun origami-c-macro-parser (create)
  (lambda (content)
    (let ((positions (origami-get-positions content "#if\\|#endif")))
      (origami-build-pair-tree create "#if" "#endif" positions))))

(defun origami-c-parser (create)
  (let ((c-style (origami-c-style-parser create))
        (macros (origami-c-macro-parser create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge
        (origami-fold-root-node (funcall c-style content))
        (origami-fold-root-node (funcall macros content)))))))

(defun origami-java-parser (create)
  (let ((c-style (origami-c-style-parser create))
        (javadoc (origami-javadoc-parser create)))
    (lambda (content)
      (origami-fold-children
       (origami-fold-shallow-merge (origami-fold-root-node (funcall c-style content))
                                   (origami-fold-root-node (funcall javadoc content)))))))

(defun origami-python-parser (create)
  (lambda (content)
    (with-temp-buffer
      (insert content)
      (python-mode)
      (goto-char (point-min))
      (beginning-of-defun -1)
      (let (beg (end (point-max)) offset acc)
        (while (not (= (point) end))
          (setq beg (point))
          (search-forward-regexp ":" nil t)
          (setq offset (- (point) beg))
          (end-of-defun)
          (backward-char)
          (setq end (point))
          (when (> offset 0)
            (setq acc (cons (funcall create beg end offset nil) acc)))
          (beginning-of-defun -1))
        (reverse acc)))))

(defun origami-lisp-parser (create regex)
  (lambda (content)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (beginning-of-defun -1)
      (let (beg end offset acc)
        (while (< (point) (point-max))
          (setq beg (point))
          (search-forward-regexp regex nil t)
          (setq offset (- (point) beg))
          (end-of-defun)
          (backward-char)      ;move point to one after the last paren
          (setq end (1- (point))) ;don't include the last paren in the fold
          (when (> offset 0)
            (setq acc (cons (funcall create beg end offset nil) acc)))
          (beginning-of-defun -1))
        (reverse acc)))))

(defun origami-elisp-parser (create)
  (origami-lisp-parser create "(def\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))

(defun origami-clj-parser (create)
  (origami-lisp-parser create "(def\\(\\w\\|-\\)*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*\\[.*?\\]\\)?"))

(defun origami-markers-parser (start-marker end-marker)
  "Create a parser for simple start and end markers."
  (let ((regex (rx-to-string `(or ,start-marker ,end-marker))))
    (lambda (create)
      (lambda (content)
        (let ((positions (origami-get-positions content regex)))
          (origami-build-pair-tree create start-marker end-marker positions))))))

(defcustom origami-parser-alist
  `((java-mode             . origami-java-parser)
    (c-mode                . origami-c-parser)
    (c++-mode              . origami-c-style-parser)
    (perl-mode             . origami-c-style-parser)
    (cperl-mode            . origami-c-style-parser)
    (js-mode               . origami-c-style-parser)
    (js2-mode              . origami-c-style-parser)
    (js3-mode              . origami-c-style-parser)
    (go-mode               . origami-c-style-parser)
    (php-mode              . origami-c-style-parser)
    (python-mode           . origami-python-parser)
    (emacs-lisp-mode       . origami-elisp-parser)
    (lisp-interaction-mode . origami-elisp-parser)
    (clojure-mode          . origami-clj-parser)
    (triple-braces         . ,(origami-markers-parser "{{{" "}}}")))
  "alist mapping major-mode to parser function."
  :type 'hook
  :group 'origami)

(provide 'origami-parsers)

;;; origami-parsers.el ends here

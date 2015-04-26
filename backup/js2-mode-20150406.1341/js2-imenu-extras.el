;;; js2-imenu-extras.el --- Imenu support for additional constructs

;; Copyright (C) 2012-2014  Free Software Foundation, Inc.

;; Author:    Dmitry Gutov <dgutov@yandex.ru>
;; Keywords:  languages, javascript, imenu

;; This file is part of GNU Emacs.

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

;; This package adds Imenu support for additional framework constructs and
;; structural patterns to `js2-mode'.

;; Usage:

;; (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

;; To customize how it works:
;;   M-x customize-group RET js2-imenu RET

(eval-when-compile
  (require 'cl))

(require 'js2-mode)

(defvar js2-imenu-extension-styles
  `((:framework jquery
     :call-re   "\\_<\\(?:jQuery\\|\\$\\|_\\)\\.extend\\s-*("
     :recorder  js2-imenu-record-jquery-extend)

    (:framework jquery-ui
     :call-re   "^\\s-*\\(?:jQuery\\|\\$\\)\\.widget\\s-*("
     :recorder  js2-imenu-record-string-declare)

    (:framework dojo
     :call-re   "^\\s-*dojo.declare\\s-*("
     :recorder  js2-imenu-record-string-declare)

    (:framework backbone
     :call-re   ,(concat "\\_<" js2-mode-identifier-re "\\.extend\\s-*(")
     :recorder  js2-imenu-record-backbone-extend)

    (:framework enyo
     :call-re   "\\_<enyo\\.kind\\s-*("
     :recorder  js2-imenu-record-enyo-kind)

    (:framework react
     :call-re "\\_<React\\.createClass\\s-*("
     :recorder js2-imenu-record-react-class)

    (:framework sencha
     :call-re "^\\s-*Ext\\.define\\s-*("
     :recorder js2-imenu-record-sencha-class))
  "List of JavaScript class definition or extension styles.

:framework is a valid value in `js2-imenu-enabled-frameworks'.

:call-re is a regular expression that has no capturing groups.

:recorder is a function name that will be called when the regular
expression matches some text in the buffer.  When it's called, point will be
at the end of the match.  The function must keep the point position.")

(defconst js2-imenu-available-frameworks
  (mapcar (lambda (style) (plist-get style :framework)) js2-imenu-extension-styles)
  "List of available JavaScript framework symbols.")

(defcustom js2-imenu-enabled-frameworks js2-imenu-available-frameworks
  "Frameworks to be recognized by `js2-mode'."
  :type (cons 'set (mapcar (lambda (x) (list 'const x))
                           js2-imenu-available-frameworks))
  :group 'js2-imenu)

(defcustom js2-imenu-show-other-functions t
  "Non-nil to show functions not recognized by other mechanisms,
in a shared namespace."
  :type 'boolean
  :group 'js2-imenu)

(defcustom js2-imenu-other-functions-ns "?"
  "Namespace name to use for other functions."
  :type 'string
  :group 'js2-imenu)

(defcustom js2-imenu-show-module-pattern t
  "Non-nil to recognize the module pattern:

var foobs = (function(a) {
  return {fib: function() {}, fub: function() {}};
})(b);

We record the returned hash as belonging to the named module, and
prefix any functions defined inside the IIFE with the module name."
  :type 'boolean
  :group 'js2-imenu)

(defcustom js2-imenu-split-string-identifiers t
  "When non-nil, split string identifiers on dots.
Currently used for jQuery widgets, Dojo and Enyo declarations."
  :type 'boolean
  :group 'js2-imenu)

;;;###autoload
(defun js2-imenu-extras-setup ()
  (when js2-imenu-enabled-frameworks
    (add-hook 'js2-build-imenu-callbacks 'js2-imenu-record-declarations t t))
  (when (or js2-imenu-show-other-functions js2-imenu-show-module-pattern)
    (add-hook 'js2-build-imenu-callbacks 'js2-imenu-walk-ast t t)))

(defun js2-imenu-extras-remove ()
  (remove-hook 'js2-build-imenu-callbacks 'js2-imenu-record-declarations t)
  (remove-hook 'js2-build-imenu-callbacks 'js2-imenu-walk-ast t))

(defun js2-imenu-record-declarations ()
  (let* ((styles (loop for style in js2-imenu-extension-styles
                       when (memq (plist-get style :framework)
                                  js2-imenu-enabled-frameworks)
                       collect style))
         (re (mapconcat (lambda (style)
                          (concat "\\(" (plist-get style :call-re) "\\)"))
                        styles "\\|")))
    (goto-char (point-min))
    (while (js2-re-search-forward re nil t)
      (loop for i from 0 to (1- (length styles))
            when (match-beginning (1+ i))
            return (funcall (plist-get (nth i styles) :recorder))))))

(defun js2-imenu-record-jquery-extend ()
  (let ((pred (lambda (subject)
                (and
                 (js2-prop-get-node-p subject)
                 (string= (js2-name-node-name (js2-prop-get-node-right subject))
                          "prototype")))))
    (js2-imenu-record-extend-first-arg (1- (point)) pred
                                       'js2-compute-nested-prop-get)))

(defun js2-imenu-record-string-declare ()
  (js2-imenu-record-extend-first-arg
   (1- (point)) 'js2-string-node-p
   (lambda (node)
     (if js2-imenu-split-string-identifiers
         (split-string (js2-string-node-value node) "\\." t)
       (list (js2-string-node-value node))))))

(defun js2-imenu-record-extend-first-arg (point pred qname-fn)
  (let* ((node (js2-node-at-point point))
         (args (js2-call-node-args node))
         (subject (first args)))
    (when (funcall pred subject)
      (loop for arg in (cdr args)
            when (js2-object-node-p arg)
            do (js2-record-object-literal
                arg (funcall qname-fn subject) (js2-node-abs-pos arg))))))

(defun js2-imenu-record-backbone-or-react ()
  (let* ((node (js2-node-at-point (1- (point))))
         (args (js2-call-node-args node))
         (methods (first args))
         (parent (js2-node-parent node)))
    (when (js2-object-node-p methods)
      (let ((subject (cond ((js2-var-init-node-p parent)
                            (js2-var-init-node-target parent))
                           ((js2-assign-node-p parent)
                            (js2-assign-node-left parent)))))
        (when subject
          (js2-record-object-literal methods
                                     (js2-compute-nested-prop-get subject)
                                     (js2-node-abs-pos methods)))))))

(defalias 'js2-imenu-record-backbone-extend 'js2-imenu-record-backbone-or-react)

(defalias 'js2-imenu-record-react-class 'js2-imenu-record-backbone-or-react)

(defun js2-imenu-record-enyo-kind ()
  (let* ((node (js2-node-at-point (1- (point))))
         (args (js2-call-node-args node))
         (options (first args)))
    (when (js2-object-node-p options)
      (let ((name-value
             (loop for elem in (js2-object-node-elems options)
                   thereis
                   (let ((key (js2-object-prop-node-left elem))
                         (value (js2-object-prop-node-right elem)))
                     (when (and (equal
                                 (cond ((js2-name-node-p key)
                                        (js2-name-node-name key))
                                       ((js2-string-node-p key)
                                        (js2-string-node-value key)))
                                 "name")
                                (js2-string-node-p value))
                       (js2-string-node-value value))))))
        (when name-value
          (js2-record-object-literal options
                                     (if js2-imenu-split-string-identifiers
                                         (split-string name-value "\\.")
                                       (list name-value))
                                     (js2-node-abs-pos options)))))))

(defun js2-imenu-record-sencha-class ()
  (let* ((node (js2-node-at-point (1- (point))))
         (args (js2-call-node-args node))
         (name (first args))
         (methods (second args)))
    (when (and (js2-string-node-p name) (js2-object-node-p methods))
      (let ((name-value (js2-string-node-value name)))
        (js2-record-object-literal methods
                                   (if js2-imenu-split-string-identifiers
                                       (split-string name-value "\\." t)
                                     (list name-value))
                                   (js2-node-abs-pos methods))))))

(defun js2-imenu-walk-ast ()
  (js2-visit-ast
   js2-mode-ast
   (lambda (node end-p)
     (unless end-p
       (cond
        ((and js2-imenu-show-other-functions
              (js2-object-prop-node-p node))
         (js2-imenu-record-orphan-prop-node-function node))
        ((js2-assign-node-p node)
         (cond
          ((and js2-imenu-show-other-functions
                (js2-function-node-p
                 (js2-assign-node-right node)))
           (js2-imenu-record-orphan-assign-node-function
            (js2-assign-node-left node)
            (js2-assign-node-right node)))
          ((and js2-imenu-show-module-pattern
                (js2-call-node-p
                 (js2-assign-node-right node)))
           (js2-imenu-record-module-pattern
            (js2-assign-node-left node)
            (js2-assign-node-right node)))))
        ((js2-var-init-node-p node)
         (cond
          ((and js2-imenu-show-other-functions
                (js2-function-node-p
                 (js2-var-init-node-initializer node)))
           (js2-imenu-record-orphan-assign-node-function
            (js2-var-init-node-target node)
            (js2-var-init-node-initializer node)))
          ((and js2-imenu-show-module-pattern
                (js2-call-node-p
                 (js2-var-init-node-initializer node)))
           (js2-imenu-record-module-pattern
            (js2-var-init-node-target node)
            (js2-var-init-node-initializer node))))))
       t))))

(defun js2-imenu-parent-key-names (node)
  "Get the list of parent key names of NODE.

For example, for code

  {rules: {password: {required: function() {}}}}

when NODE is the inner `js2-object-prop-mode',
it returns `(\"rules\" \"password\")'."
  (let (rlt (n node))
    (while (setq n (js2-imenu-parent-prop-node n))
      (push (js2-prop-node-name (js2-object-prop-node-left n)) rlt))
    rlt))

(defun js2-imenu-parent-prop-node (node)
  "When the parent of NODE is `js2-object-node',
and the grandparent is `js2-object-prop-node',
return the grandparent."
  ;; Suppose the code is:
  ;; {parent-key: {required: function() {}}}
  ;; NODE is `required: function() {}'.
  (let (p2 p3)
    ;; Parent is `{required: function() {}}'.
    (setq p2 (js2-node-parent node))
    ;; GP is `parent-key: {required: function() {}}'.
    (when (and p2 (js2-object-node-p p2))
      (setq p3 (js2-node-parent p2))
      (if (and p3 (js2-object-prop-node-p p3)) p3))))

(defun js2-imenu-record-orphan-prop-node-function (node)
  "Record orphan function when it's the value of NODE.
NODE must be `js2-object-prop-node'."
  (when (js2-function-node-p (js2-object-prop-node-right node))
    (let ((fn-node (js2-object-prop-node-right node)))
      (unless (and js2-imenu-function-map
                   (gethash fn-node js2-imenu-function-map))
        (let ((key-node (js2-object-prop-node-left node))
              (parent-prop-node (js2-imenu-parent-prop-node node))
              chain)
          (setq chain (nconc (js2-imenu-parent-key-names node)
                             (list (js2-prop-node-name key-node))))
          (push js2-imenu-other-functions-ns chain)
          (js2-record-imenu-entry fn-node chain
                                  (js2-node-abs-pos key-node)))))))

(defun js2-imenu-record-orphan-assign-node-function (target-node fn-node)
  "Record orphan function FN-NODE assigned to node TARGET."
  (when (or (not js2-imenu-function-map)
            (eq 'skip
                (gethash fn-node js2-imenu-function-map 'skip)))
    (let ((chain (js2-compute-nested-prop-get target-node)))
      (when chain
        (push js2-imenu-other-functions-ns chain)
        (js2-record-imenu-entry fn-node chain (js2-node-abs-pos fn-node))))))

(defun js2-imenu-record-module-pattern (target init)
  "Recognize and record module pattern use instance.
INIT must be `js2-call-node'."
  (let ((callt (js2-call-node-target init)))
    ;; Just basic call form: (function() {...})();
    ;; TODO: Handle variations without duplicating `js2-wrapper-function-p'?
    (when (and (js2-paren-node-p callt)
               (js2-function-node-p (js2-paren-node-expr callt)))
      (let* ((fn (js2-paren-node-expr callt))
             (blk (js2-function-node-body fn))
             (ret (car (last (js2-block-node-kids blk)))))
        (when (and (js2-return-node-p ret)
                   (js2-object-node-p (js2-return-node-retval ret)))
          ;; TODO: Map function names when revealing module pattern is used.
          (let ((retval (js2-return-node-retval ret))
                (target-qname (js2-compute-nested-prop-get target)))
            (js2-record-object-literal retval target-qname
                                       (js2-node-abs-pos retval))
            (js2-record-imenu-entry fn target-qname
                                    (js2-node-abs-pos target))))))))

;;;###autoload
(define-minor-mode js2-imenu-extras-mode
  "Toggle Imenu support for frameworks and structural patterns."
  :lighter ""
  (if js2-imenu-extras-mode
      (js2-imenu-extras-setup)
    (js2-imenu-extras-remove)))

(provide 'js2-imenu-extras)

;;; helm-imenu.el --- Helm interface for Imenu -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'imenu)
(require 'helm-utils)


(defgroup helm-imenu nil
  "Imenu related libraries and applications for helm."
  :group 'helm)

(defcustom helm-imenu-delimiter " / "
  "Delimit types of candidates and his value in `helm-buffer'."
  :group 'helm-imenu
  :type 'string)

(defcustom helm-imenu-execute-action-at-once-if-one t
  "Goto the candidate when only one is remaining."
  :group 'helm-imenu
  :type 'boolean)

(defcustom helm-imenu-lynx-style-map t
  "Use Arrow keys to jump to occurences."
  :group 'helm-imenu
  :type  'boolean)


;;; keymap
(defvar helm-imenu-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?") 'helm-imenu-help)
    (when helm-imenu-lynx-style-map
      (define-key map (kbd "<left>")  'helm-exit-minibuffer)
      (define-key map (kbd "<right>") 'helm-execute-persistent-action))
    (delq nil map)))


;;; Internals
(defvar helm-cached-imenu-alist nil)
(make-variable-buffer-local 'helm-cached-imenu-alist)

(defvar helm-cached-imenu-candidates nil)
(make-variable-buffer-local 'helm-cached-imenu-candidates)

(defvar helm-cached-imenu-tick nil)
(make-variable-buffer-local 'helm-cached-imenu-tick)


(defvar helm-source-imenu
  `((name . "Imenu")
    (candidates . helm-imenu-candidates)
    (allow-dups)
    (candidate-transformer . helm-imenu-transformer)
    (persistent-action . helm-imenu-persistent-action)
    (persistent-help . "Show this entry")
    (keymap . ,helm-imenu-map)
    (mode-line . helm-imenu-mode-line)
    (action . helm-imenu-action)
    "See (info \"(emacs)Imenu\")"))


(defun helm-imenu-action (candidate)
  "Default action for `helm-source-imenu'."
  (imenu candidate)
  ;; If semantic is supported in this buffer
  ;; imenu used `semantic-imenu-goto-function'
  ;; and position have been highlighted,
  ;; no need to highlight again.
  (unless (eq imenu-default-goto-function
              'semantic-imenu-goto-function)
    (helm-highlight-current-line nil nil nil nil 'pulse)))

(defun helm-imenu-persistent-action (candidate)
  "Default persistent action for `helm-source-imenu'."
  (imenu candidate)
  (helm-highlight-current-line))

(defun helm-imenu-candidates ()
  (with-helm-current-buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq helm-cached-imenu-tick tick)
          helm-cached-imenu-candidates
        (setq imenu--index-alist nil)
        (setq helm-cached-imenu-tick tick
              helm-cached-imenu-candidates
              (let ((index (imenu--make-index-alist))) 
                (helm-imenu--candidates-1
                 (delete (assoc "*Rescan*" index) index))))))))

(defun helm-imenu--candidates-1 (alist)
  (cl-loop for elm in alist
        append (if (imenu--subalist-p elm)
                   (helm-imenu--candidates-1
                    (cl-loop for (e . v) in (cdr elm) collect
                          (cons (propertize
                                 e 'helm-imenu-type (car elm))
                                v)))
                 (and (cdr elm) ; bug in imenu, should not be needed.
                      (list elm)))))

(defun helm-imenu--get-prop (item)
  ;; property value of ITEM can have itself
  ;; a property value which have itself a property value
  ;; ...and so on; Return a list of all these
  ;; properties values starting at ITEM.
  (let* ((prop (get-text-property 0 'helm-imenu-type item))
         (lst  (list prop item)))
    (when prop
      (while prop
        (setq prop (get-text-property 0 'helm-imenu-type prop))
        (and prop (push prop lst)))
      lst)))

(defun helm-imenu-transformer (candidates)
  (cl-loop for (k . v) in candidates
        for types = (or (helm-imenu--get-prop k)
                        (list "Function" k))
        collect
        (cons (mapconcat (lambda (x)
                           (propertize
                            x 'face (cond ((string= x "Variables")
                                           'font-lock-variable-name-face)
                                          ((string= x "Function")
                                           'font-lock-function-name-face)
                                          ((string= x "Types")
                                           'font-lock-type-face))))
                         types helm-imenu-delimiter)
              (cons k v))))

;;;###autoload
(defun helm-imenu ()
  "Preconfigured `helm' for `imenu'."
  (interactive)
  (let ((imenu-auto-rescan t)
        (str (thing-at-point 'symbol))
        (helm-execute-action-at-once-if-one
         helm-imenu-execute-action-at-once-if-one))
    (helm :sources 'helm-source-imenu
          :default (list (concat "\\_<" str "\\_>") str)
          :candidate-number-limit 9999
          :buffer "*helm imenu*")))

(provide 'helm-imenu)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-imenu.el ends here

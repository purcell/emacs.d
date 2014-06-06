;;; helm-adaptive.el --- Adaptive Sorting of Candidates. -*- lexical-binding: t -*-

;; Original Author: Tamas Patrovics

;; Copyright (C) 2007 Tamas Patrovics
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


(defgroup helm-adapt nil
  "Adaptative sorting of candidates for Helm."
  :group 'helm)

(defcustom helm-adaptive-history-file
  "~/.emacs.d/helm-adaptive-history"
  "Path of file where history information is stored."
  :type 'string
  :group 'helm-adapt)

(defcustom helm-adaptive-history-length 50
  "Maximum number of candidates stored for a source."
  :type 'number
  :group 'helm-adapt)


;; Internal
(defvar helm-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar helm-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

(defun helm-adaptive-done-reset ()
  (setq helm-adaptive-done nil))

(define-minor-mode helm-adaptive-mode
    "Toggle adaptive sorting in all sources."
  :group 'helm-adapt
  :require 'helm-adaptive
  :global t
  (if helm-adaptive-mode
      (progn
        (unless helm-adaptive-history
          (helm-adaptive-maybe-load-history))
        (add-hook 'kill-emacs-hook 'helm-adaptive-save-history)
        ;; Should run at beginning of `helm-initial-setup'.
        (add-hook 'helm-before-initialize-hook 'helm-adaptive-done-reset)
        ;; Should run at beginning of `helm-exit-minibuffer'.
        (add-hook 'helm-before-action-hook 'helm-adaptive-store-selection)
        ;; Should run at beginning of `helm-select-action'.
        (add-hook 'helm-select-action-hook 'helm-adaptive-store-selection))
    (helm-adaptive-save-history)
    (setq helm-adaptive-history nil)
    (remove-hook 'kill-emacs-hook 'helm-adaptive-save-history)
    (remove-hook 'helm-before-initialize-hook 'helm-adaptive-done-reset)
    (remove-hook 'helm-before-action-hook 'helm-adaptive-store-selection)
    (remove-hook 'helm-select-action-hook 'helm-adaptive-store-selection)))

(defun helm-adapt-use-adaptive-p (&optional source-name)
  "Return current source only if it use adaptive history, nil otherwise."
  (when helm-adaptive-mode
    (let* ((source (or source-name (helm-get-current-source)))
           (adapt-source (or (assoc-default 'filtered-candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   helm-type-attributes))
                             (assoc-default 'candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   helm-type-attributes))
                             (assoc-default 'filtered-candidate-transformer source)
                             (assoc-default 'candidate-transformer source))))
      (if (listp adapt-source)
          (and (member 'helm-adaptive-sort adapt-source) source)
        (and (eq adapt-source 'helm-adaptive-sort) source)))))

(defun helm-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless helm-adaptive-done
    (setq helm-adaptive-done t)
    (let ((source (helm-adapt-use-adaptive-p)))
      (when source
        (let* ((source-name (or (assoc-default 'type source)
                                (assoc-default 'name source)))
               (source-info (or (assoc source-name helm-adaptive-history)
                                (progn
                                  (push (list source-name) helm-adaptive-history)
                                  (car helm-adaptive-history))))
               (selection (helm-get-selection))
               (selection-info (progn
                                 (setcdr source-info
                                         (cons
                                          (let ((found (assoc selection (cdr source-info))))
                                            (if (not found)
                                                ;; new entry
                                                (list selection)
                                              ;; move entry to the beginning of the
                                              ;; list, so that it doesn't get
                                              ;; trimmed when the history is
                                              ;; truncated
                                              (setcdr source-info
                                                      (delete found (cdr source-info)))
                                              found))
                                          (cdr source-info)))
                                 (cadr source-info)))
               (pattern-info (progn
                               (setcdr selection-info
                                       (cons
                                        (let ((found (assoc helm-pattern (cdr selection-info))))
                                          (if (not found)
                                              ;; new entry
                                              (cons helm-pattern 0)

                                            ;; move entry to the beginning of the
                                            ;; list, so if two patterns used the
                                            ;; same number of times then the one
                                            ;; used last appears first in the list
                                            (setcdr selection-info
                                                    (delete found (cdr selection-info)))
                                            found))
                                        (cdr selection-info)))
                               (cadr selection-info))))

          ;; increase usage count
          (setcdr pattern-info (1+ (cdr pattern-info)))

          ;; truncate history if needed
          (if (> (length (cdr selection-info)) helm-adaptive-history-length)
              (setcdr selection-info
                      (cl-subseq (cdr selection-info) 0 helm-adaptive-history-length))))))))

(defun helm-adaptive-maybe-load-history ()
  "Load `helm-adaptive-history-file' which contain `helm-adaptive-history'.
Returns nil if `helm-adaptive-history-file' doesn't exist."
  (when (file-readable-p helm-adaptive-history-file)
    (load-file helm-adaptive-history-file)))

(defun helm-adaptive-save-history (&optional arg)
  "Save history information to file given by `helm-adaptive-history-file'."
  (interactive "p")
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; History entries used for helm adaptive display.\n")
    (prin1 `(setq helm-adaptive-history ',helm-adaptive-history)
           (current-buffer))
    (insert ?\n)
    (write-region (point-min) (point-max) helm-adaptive-history-file nil
                  (unless arg 'quiet))))

(defun helm-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use with the
`filtered-candidate-transformer' attribute."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name helm-adaptive-history)))
    (if source-info
        (let ((usage
               ;; ... assemble a list containing the (CANIDATE . USAGE-COUNT)
               ;; pairs
               (mapcar (lambda (candidate-info)
                         (let ((count 0))
                           (cl-dolist (pattern-info (cdr candidate-info))
                             (if (not (equal (car pattern-info)
                                             helm-pattern))
                                 (cl-incf count (cdr pattern-info))

                               ;; if current pattern is equal to the previously
                               ;; used one then this candidate has priority
                               ;; (that's why its count is boosted by 10000) and
                               ;; it only has to compete with other candidates
                               ;; which were also selected with the same pattern
                               (setq count (+ 10000 (cdr pattern-info)))
                               (cl-return)))
                           (cons (car candidate-info) count)))
                       (cdr source-info))))
          (if (and usage (consp usage))
              ;; sort the list in descending order, so candidates with highest
              ;; priorty come first
              (progn
                (setq usage (sort usage (lambda (first second)
                                          (> (cdr first) (cdr second)))))

                ;; put those candidates first which have the highest usage count
                (cl-loop for (info . _freq) in usage
                      for member = (cl-member info candidates
                                              :test 'helm-adaptive-compare)
                      when member collect (car member) into sorted
                      and do
                      (setq candidates (cl-remove info candidates
                                                  :test 'helm-adaptive-compare))
                      finally return (append sorted candidates)))
            (message "Your `%s' is maybe corrupted or too old, \
you should reinitialize it with `helm-reset-adaptive-history'"
                     helm-adaptive-history-file)
            (sit-for 1)
            candidates))
      ;; if there is no information stored for this source then do nothing
      candidates)))

;;;###autoload
(defun helm-reset-adaptive-history ()
  "Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-adaptive-history-file'."
  (interactive)
  (when (y-or-n-p "Really delete all your `helm-adaptive-history'? ")
    (setq helm-adaptive-history nil)
    (delete-file helm-adaptive-history-file)))

(defun helm-adaptive-compare (x y)
  "Compare candidates X and Y taking into account that the
candidate can be in (DISPLAY . REAL) format."
  (equal (if (listp x) (cdr x) x)
         (if (listp y) (cdr y) y)))


(provide 'helm-adaptive)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-adaptive.el ends here

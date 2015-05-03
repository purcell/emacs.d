;;; init-tabbar.el --- Init for tabbar

;; Filename: init-tabbar.el
;; Description: Init for tabbar
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-29 23:50:27
;; Version: 0.1
;; Last-Updated: 2013-12-29 23:50:27
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-tabbar.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Init for tabbar
;;

;;; Installation:
;;
;; Put init-tabbar.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-tabbar)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-tabbar RET
;;

;;; Change log:
;;
;; 2013/12/29
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(require 'tabbar)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tabbar-select-end-tab ()
  "Select end tab of current tabset."
  (interactive)
  (tabbar-select-beg-tab t))

(defun tabbar-select-beg-tab (&optional backward type)
  "Select beginning tab of current tabs.
If BACKWARD is non-nil, move backward, otherwise move forward.
TYPE is default option."
  (interactive)
  (let* ((tabset (tabbar-current-tabset t))
         (ttabset (tabbar-get-tabsets-tabset))
         (cycle (if (and (eq tabbar-cycle-scope 'groups)
                         (not (cdr (tabbar-tabs ttabset))))
                    'tabs
                  tabbar-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (tabbar-selected-tab tabset))
      (setq tabset (tabbar-tabs tabset)
            tab (car (if backward (last tabset) tabset)))
      (tabbar-click-on-tab tab type))))

(defun tabbar-backward-tab-other-window (&optional reversed)
  "Move to left tab in other window.
Optional argument REVERSED default is move backward, if reversed is non-nil move forward."
  (interactive)
  (other-window 1)
  (if reversed
      (tabbar-forward-tab)
    (tabbar-backward-tab))
  (other-window -1))

(defun tabbar-forward-tab-other-window ()
  "Move to right tab in other window."
  (interactive)
  (tabbar-backward-tab-other-window t))



;;; Code:

(defcustom tabbar-hide-header-button t
  "Hide header button at left-up corner.
Default is t."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (setq
              tabbar-scroll-left-help-function nil ;don't show help information
              tabbar-scroll-right-help-function nil
              tabbar-help-on-tab-function nil
              tabbar-home-help-function nil
              tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
              tabbar-scroll-left-button (quote (("") ""))
              tabbar-scroll-right-button (quote (("") "")))))
  :group 'tabbar)

(defun tabbar-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun tabbar-filter-buffer-list ()
  (tabbar-filter
   (lambda (x)
     (let ((name (format "%s" x)))
       (and
        (not (string-prefix-p "*epc" name))
        (not (string-prefix-p "*helm" name))
        (not (string-prefix-p "*Messages*" name))
        )))
   (delq nil
         (mapcar #'(lambda (b)
                     (cond
                      ;; Always include the current buffer.
                      ((eq (current-buffer) b) b)
                      ((buffer-file-name b) b)
                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                      ((buffer-live-p b) b)))
                 (buffer-list)))))

(setq tabbar-buffer-list-function 'tabbar-filter-buffer-list)



(defvar tabbar-ruler-projectile-tabbar-buffer-group-calc nil
  "Buffer group for projectile.  Should be buffer local and speed up calculation of buffer groups.")
(defun tabbar-ruler-projectile-tabbar-buffer-groups ()
  "Return the list of group names BUFFER belongs to.
    Return only one group for each buffer."
  
  (if tabbar-ruler-projectile-tabbar-buffer-group-calc
      (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)
    (set (make-local-variable 'tabbar-ruler-projectile-tabbar-buffer-group-calc)
         
         (cond
          ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
          ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
          ((condition-case err
               (projectile-project-root)
             (error nil)) (list (projectile-project-name)))
          ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode c++-mode makefile-mode lua-mode vala-mode)) '("Coding"))
          ((memq major-mode '(javascript-mode js-mode nxhtml-mode html-mode css-mode)) '("HTML"))
          ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
          ((memq major-mode '(dired-mode)) '("Dir"))
          (t '("Main"))))
    (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)))

(defun tabbar-ruler-group-by-projectile-project()
  "Group by projectile project."
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-ruler-projectile-tabbar-buffer-groups))



;; group by projectile
(tabbar-ruler-group-by-projectile-project)
(tabbar-mode)

(provide 'init-tabbar)

;;; init-tabbar.el ends here

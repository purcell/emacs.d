;;; idle-require.el --- load elisp libraries while Emacs is idle
;;
;; Copyright (C) 2007-2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0
;; Keywords: internal
;; URL: http://nschum.de/src/emacs/idle-require/
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'idle-require)
;; (setq idle-require-symbols '(cedet nxml-mode)) ;; <- Specify packages here.
;; (idle-require 'cedet) ;; <- Or like this.
;; (idle-require-mode 1) ;; starts loading
;;
;; As soon as Emacs goes idle for `idle-require-idle-delay' seconds,
;; `idle-require-mode' will start loading the files, symbols or functions in
;; `idle-require-symbols'.  If that is nil, all autoload functions will be
;; loaded, one at a time.
;;
;; Use `idle-require-load-break' to give your CPU a break between each load.
;; Otherwise, you might create 100% CPU load on your system.
;;
;;; Change Log:
;;
;;    The order of calls to `idle-require' is now maintained.
;;    Made `idle-require' parameters compatible to `require'.
;;
;; 2008-02-26 (1.0)
;;    Added convenience function `idle-require'.
;;
;; 2007-05-04 (0.9)
;;    Initial release.
;;
;;; Code:

(defun idle-require-get-symbols ()
  "Return all symbols that will be autoloaded."
  (let (symbols symbol)
    (mapatoms
     (lambda (symbol)
       (and (functionp symbol)
            (eq (car-safe (symbol-function symbol)) 'autoload)
            (push symbol symbols))))
    symbols))

(defvar idle-require-idle-delay 45
  "Idle time in seconds after which autoload functions will be loaded.")

(defvar idle-require-load-break 1
  "Time in seconds between automatically loaded functions.
This keeps `idle-require-mode' from using up the CPU capacity.")

(defvar idle-require-symbols nil
  "Symbols which need to be autoloaded by `idle-require-mode'.
This list may contain either autoload functions, file names or features.")

(defvar idle-require-timer nil)

;;;###autoload
(defun idle-require (feature &optional filename noerror)
  "Add FEATURE to `idle-require-symbols'.
FILENAME and NOERROR are provided for compatibility to `require'.  If FILENAME
is non-nil, it is added instead of FEATURE.  NOERROR has no effect as that is
the default."
  (add-to-list 'idle-require-symbols (or filename feature) t))

;;;###autoload
(define-minor-mode idle-require-mode
  "Load unloaded autoload functions when Emacs becomes idle.
If `idle-require-symbols' is a list of files, those will be loaded.
Otherwise all autoload functions will be loaded.

Loading all autoload functions can easily triple Emacs' memory footprint."
  nil " idle-req" nil
  (if idle-require-mode
      ;; on
      (progn
        (unless (consp idle-require-symbols)
          (message "Loading ALL autoload functions")
          (setq idle-require-symbols (idle-require-get-symbols)))
        (unless idle-require-timer
        (setq idle-require-timer
              (run-with-idle-timer idle-require-idle-delay
                                   t 'idle-require-load-next))))
    ;; off
    (when idle-require-timer
      (cancel-timer idle-require-timer)
      (setq idle-require-timer nil))))

(defun idle-require-load-next ()
  "Load symbols from `idle-require-symbols.' until input occurs."
  (let (symbol)
    (message "Beginning idle-require")
    (while (and idle-require-symbols
                (not (input-pending-p)))
      (setq symbol (pop idle-require-symbols))
;;       (condition-case err
          (cond
           ((stringp symbol) (load symbol t))
           ((functionp symbol)
            (setq symbol (symbol-function symbol))
            (when (eq (car-safe symbol) 'autoload)
              ;; still not loaded
              (load (cadr symbol) t)))
           (t
            (message "idle-require: require %s" symbol)
            (require symbol)))
;;         (error (message "idle-require for %s failed" symbol)))
      (sit-for idle-require-load-break)))
  (when (null idle-require-symbols)
    (idle-require-mode 0)
    (message "idle-require finished")))

(provide 'idle-require)
;;; idle-require.el ends here

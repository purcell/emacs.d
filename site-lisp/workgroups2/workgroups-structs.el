;;; workgroups-structs.el --- Data structures for WG
;;; Commentary:
;;
;; Copyright (C) Sergey Pashinin
;; Author: Sergey Pashinin <sergey@pashinin.com>
;;
;; `wg-defstruct'
;;
;; It creates some functions named like "wg-buf-...", "wg-session-..."
;;
;; To get a value you can use:
;; (wg-session-... (wg-current-session))
;; Like:
;; (wg-session-file-name (wg-current-session))
;; (wg-workgroup-parameters (wg-current-workgroup))
;;
;; To set a value (in `wg-write-session-file'):
;;
;; (setf (wg-session-file-name (wg-current-session)) filename)
;;
;;; Code:

(require 'workgroups-utils-basic)

(wg-defstruct wg buf
  (uid (wg-generate-uid))
  (name)
  (file-name)
  (point)
  (mark)
  (local-vars)
  (special-data)
  ;; This may be used later:
  (gc))

(wg-defstruct wg win
  (uid)
  (parameters)
  (edges)
  (point)
  (start)
  (hscroll)
  (dedicated)
  (selected)
  (minibuffer-scroll)
  (buf-uid))

(wg-defstruct wg wtree
  (uid)
  (dir)
  (edges)
  (wlist))

(wg-defstruct wg wconfig
  (uid (wg-generate-uid))
  (name)
  (parameters)
  (left)
  (top)
  (width)
  (height)
  (vertical-scroll-bars)
  (scroll-bar-width)
  (wtree)
  ;;(fullscreen)
  )
;; wg-wconfig

(wg-defstruct wg workgroup
  (uid (wg-generate-uid))
  (name)
  (modified)
  (parameters)
  (base-wconfig)
  (selected-frame-wconfig)
  (saved-wconfigs)
  (strong-buf-uids)
  (weak-buf-uids))

(wg-defstruct wg session
  (uid (wg-generate-uid))
  (name)
  (modified)
  (parameters)
  (file-name)
  (version wg-version)
  (workgroup-list)
  (buf-list))

(wg-defstruct wg workgroup-state
  (undo-pointer)
  (undo-list))

(provide 'workgroups-structs)
;;; workgroups-structs.el ends here

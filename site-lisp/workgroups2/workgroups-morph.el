;;; workgroups-morph --- animation effects
;;; Commentary:
;;
;; TODO: Add upward and left morphing.  And once it's been added, add
;;       selection of a random morph direction.
;;

;;; Code:

(defun wg-morph-p ()
  "Return t when it's ok to morph, nil otherwise."
  (and after-init-time wg-morph-on))

(defun wg-morph-step-edges (w1 w2)
  "Step W1's edges toward W2's by `wg-morph-hsteps' and `wg-morph-vsteps'."
  (wg-step-edges (wg-w-edges w1) (wg-w-edges w2)
                 wg-morph-hsteps wg-morph-vsteps))

(defun wg-morph-determine-steps (gui-steps &optional term-steps)
  (max 1 (if (and (not window-system) term-steps) term-steps gui-steps)))

(defun wg-morph-match-wlist (wt1 wt2)
  "Return a wlist by matching WT1's wlist to WT2's.
When wlist1's and wlist2's lengths are equal, return wlist1.
When wlist1 is shorter than wlist2, add a window at the front of wlist1.
When wlist1 is longer than wlist2, package up wlist1's excess windows
into a wtree, so it's the same length as wlist2."
  (let* ((wl1 (wg-wtree-wlist wt1))
         (l1 (length wl1))
         (d1 (wg-wtree-dir wt1))
         (wl2 (wg-wtree-wlist wt2))
         (l2 (length wl2)))
    (cond ((= l1 l2) wl1)
          ((< l1 l2)
           (cons (wg-minified-copy-of-last-win (wg-rnth (1+ l1) wl2))
                 (if (< (wg-w-size (car wl1) d1)
                        (* 2 (wg-actual-min-size d1)))
                     wl1
                   (cons (wg-w-edge-operation (car wl1) wg-min-edges #'-)
                         (cdr wl1)))))
          ((> l1 l2)
           (append (wg-take wl1 (1- l2))
                   (list (wg-make-wtree
                          :dir d1
                          :edges wg-null-edges
                          :wlist (nthcdr (1- l2) wl1))))))))

(defun wg-morph-win-to-win (w1 w2 &optional swap)
  "Return a copy of W1 with its edges stepped once toward W2.
When SWAP is non-nil, return a copy of W2 instead."
  (wg-set-edges (wg-copy-win (if swap w2 w1)) (wg-morph-step-edges w1 w2)))

(defun wg-morph-win-to-wtree (win wtree)
  "Return a new wtree with WIN's edges and WTREE's last two windows."
  (wg-make-wtree
   :dir (wg-wtree-dir wtree)
   :edges (wg-morph-step-edges win wtree)
   :wlist (let ((wg-morph-hsteps 2) (wg-morph-vsteps 2))
            (wg-docar (w (wg-leave (wg-wtree-wlist wtree) 2))
              (wg-morph-win-to-win (wg-minified-copy-of-last-win w) w)))))

(defun wg-morph-wtree-to-win (wtree win &optional noswap)
  "Grow the first window of WTREE and its subtrees one step toward WIN.
This eventually wipes WTREE's components, leaving only a window.
Swap WTREE's first actual window for WIN, unless NOSWAP is non-nil."
  (if (wg-win-p wtree) (wg-morph-win-to-win wtree win (not noswap))
    (wg-make-wtree
     :dir (wg-wtree-dir wtree)
     :edges (wg-morph-step-edges wtree win)
     :wlist (wg-dbind (fwin . wins) (wg-wtree-wlist wtree)
              (cons (wg-morph-wtree-to-win fwin win noswap)
                    (wg-docar (sw wins)
                      (if (wg-win-p sw) sw
                        (wg-morph-wtree-to-win sw win t))))))))

(defun wg-morph-wtree-to-wtree (wt1 wt2)
  "Return a new wtree morphed one step toward WT2 from WT1.
Mutually recursive with `wg-morph-dispatch' to traverse the
structures of WT1 and WT2 looking for discrepancies."
  (let ((d1 (wg-wtree-dir wt1)) (d2 (wg-wtree-dir wt2)))
    (wg-make-wtree
     :dir d2
     :edges (wg-morph-step-edges wt1 wt2)
     :wlist (if (not (eq (wg-wtree-dir wt1) (wg-wtree-dir wt2)))
                (list (wg-minified-copy-of-last-win wt2) wt1)
              (wg-mapcar* #'wg-morph-dispatch
                       (wg-morph-match-wlist wt1 wt2)
                       (wg-wtree-wlist wt2))))))

(defun wg-morph-dispatch (w1 w2)
  "Return a wtree morphed one step toward W2 from W1.
Dispatches on each possible combination of types."
  (cond ((and (wg-win-p w1) (wg-win-p w2))
         (wg-morph-win-to-win w1 w2 t))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (wg-morph-wtree-to-wtree w1 w2))
        ((and (wg-win-p w1) (wg-wtree-p w2))
         (wg-morph-win-to-wtree w1 w2))
        ((and (wg-wtree-p w1) (wg-win-p w2))
         (wg-morph-wtree-to-win w1 w2))))

(defun wg-morph (to &optional from)
  "Morph from wtree FROM to wtree TO.
Assumes both FROM and TO fit in `selected-frame'."
  (let ((from (or from (wg-window-tree-to-wtree (window-tree))))
        (wg-morph-hsteps
         (wg-morph-determine-steps wg-morph-hsteps wg-morph-terminal-hsteps))
        (wg-morph-vsteps
         (wg-morph-determine-steps wg-morph-vsteps wg-morph-terminal-vsteps))
        (truncate-partial-width-windows wg-morph-truncate-partial-width-windows)
        (wg-record-incorrectly-restored-bufs nil)
        (wg-restore-scroll-bars nil)
        (wg-restore-fringes nil)
        (wg-restore-margins nil)
        (wg-restore-point nil)
        (wg-restore-mark nil)
        (watchdog 0))
    (wg-until (wg-equal-wtrees from to)
      (condition-case err
          (if (> (incf watchdog) wg-morph-max-steps)
              (error "`wg-morph-max-steps' exceeded")
            (setq from (wg-normalize-wtree (wg-morph-dispatch from to)))
            (wg-restore-window-tree from)
            (redisplay))
        (error (wg-dbind (sym data) err
                 (unless (and (stringp data)
                              (string-match "too small" data))
                   (signal sym data))))))
    to))


(provide 'workgroups-morph)
;;; workgroups-morph.el ends here

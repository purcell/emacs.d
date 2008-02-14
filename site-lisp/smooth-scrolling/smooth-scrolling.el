;; smooth-scrolling.el
;; $Id: smooth-scrolling.el,v 1.8 2007/09/11 23:38:09 adam Exp $
;; Adam Spiers <emacs-ss@adamspiers.org>
;; 
;; Make emacs scroll smoothly, keeping the point away from the top and
;; bottom of the current buffer's window in order to keep lines of
;; context around the point visible as much as possible, whilst
;; avoiding sudden scroll jumps which are visually confusing.
;;
;; This is a nice alternative to all the native scroll-* custom
;; variables, which unfortunately cannot provide this functionality
;; perfectly.  `scroll-margin' comes closest, but has some bugs
;; (e.g. with handling of mouse clicks).  See
;;
;;   http://www.emacswiki.org/cgi-bin/wiki/SmoothScrolling
;;
;; for the gory details.
;;
;;;_* Installation
;;
;; Put somewhere on your `load-path' and include
;;
;;   (require 'smooth-scrolling)
;;
;; in your .emacs initialization file.
;;
;;;_* Notes
;;
;; This only affects the behaviour of the `next-line' and
;; `previous-line' functions, usually bound to the cursor keys and
;; C-n/C-p.  Other methods of moving the point will behave as normal
;; according to the standard custom variables.
;;
;; Prefix arguments to `next-line' and `previous-line' are honoured.
;; The minimum number of lines are scrolled in order to keep the
;; point outside the margin.
;;
;; There is one case where moving the point in this fashion may cause
;; a jump: if the point is placed inside one of the margins by another
;; method (e.g. left mouse click, or M-x goto-line) and then moved in
;; the normal way, the advice code will scroll the minimum number of
;; lines in order to keep the point outside the margin.  This jump may
;; cause some slight confusion at first, but hopefully it is justified
;; by the benefit of automatically ensuring `smooth-scroll-margin'
;; lines of context are visible around the point as often as possible.
;;
;;;_* TODO
;;
;; - Maybe add option to avoid scroll jumps when point is within
;;   margin.
;; 
;;;_* Acknowledgements
;; 
;; Thanks to Mark Hulme-Jones and consolers on #emacs for helping
;; debug issues with line-wrapping.
;; 
;;;_* License
;; 
;; Released under the GNU General Public License v2 or later, with
;; all rights assigned to the Free Software Foundation.
;;

;;;_* Code follows
;;;_ + disable `scroll-margin'
(setq scroll-margin 0)
;;;_ + defcustoms
(defcustom smooth-scroll-margin 10
  "Number of lines of visible margin at the top and bottom of a window.
If the point is within these margins, then scrolling will occur
smoothly for `previous-line' at the top of the window, and for
`next-line' at the bottom.

This is very similar in its goal to `scroll-margin'.  However, it
is implemented by activating `smooth-scroll-down' and
`smooth-scroll-up' advise via `defadvice' for `previous-line' and
`next-line' respectively.  As a result it avoids problems
afflicting `scroll-margin', such as a sudden jump and unexpected
highlighting of a region when the mouse is clicked in the margin.

Scrolling only occurs when the point is closer to the window
boundary it is heading for (top or bottom) than the middle of the
window.  This is to intelligently handle the case where the
margins cover the whole buffer (e.g. `smooth-scroll-margin' set
to 5 and `window-height' returning 10 or less).

See also `smooth-scroll-strict-margins'."
  :type  'integer
  :group 'windows)

(defcustom smooth-scroll-strict-margins t
  "If true, the advice code supporting `smooth-scroll-margin'
will use `count-screen-lines' to determine the number of
*visible* lines between the point and the window top/bottom,
rather than `count-lines' which obtains the number of actual
newlines.  This is because there might be extra newlines hidden
by a mode such as folding-mode, outline-mode, org-mode etc., or
fewer due to very long lines being displayed wrapped when
`truncate-lines' is nil.

However, using `count-screen-lines' can supposedly cause
performance issues in buffers with extremely long lines.  Setting
`cache-long-line-scans' may be able to address this;
alternatively you can set this variable to nil so that the advice
code uses `count-lines', and put up with the fact that sometimes
the point will be allowed to stray into the margin."
  :type  'boolean
  :group 'windows)
;;;_ + helper functions
(defun smooth-scroll-lines-from-window-top ()
  "Work out, using the function indicated by
`smooth-scroll-strict-margins', what the current screen line is,
relative to the top of the window.  Counting starts with 1 referring
to the top line in the window."
  (interactive)
  (cond ((= (window-start) (point))
         ;; In this case, count-screen-lines would return 0, so we override.
         1)
        (smooth-scroll-strict-margins
         (count-screen-lines (window-start) (point) 'count-final-newline))
        (t
         (count-lines (window-start) (point)))))

(defun smooth-scroll-lines-from-window-bottom ()
  "Work out, using the function indicated by
`smooth-scroll-strict-margins', how many screen lines there are
between the point and the bottom of the window.  Counting starts
with 1 referring to the bottom line in the window."
  (interactive)
  (if smooth-scroll-strict-margins
      (count-screen-lines (point) (window-end))
    (count-lines (point) (window-end))))
;;;_ + after advice
(defadvice previous-line (after smooth-scroll-down
                            (&optional arg try-vscroll)
                            activate)
  "Scroll down smoothly if cursor is within `smooth-scroll-margin'
lines of the top of the window."
  (and
   ;; Only scroll down if there is buffer above the start of the window.
   (> (window-start) (buffer-end -1))
   (let ((lines-from-window-top
          (smooth-scroll-lines-from-window-top)))
     (and
      ;; Only scroll down if we're within the top margin
      (<= lines-from-window-top smooth-scroll-margin)
      ;; Only scroll down if we're in the top half of the window
      (<= lines-from-window-top
          ;; N.B. `window-height' includes modeline, so if it returned 21,
          ;; that would mean exactly 10 lines in the top half and 10 in
          ;; the bottom.  22 (or any even number) means there's one in the
          ;; middle.  In both cases the following expression will
          ;; yield 10:
          (/ (1- (window-height)) 2))
      (save-excursion
        (scroll-down
              (1+ (- smooth-scroll-margin lines-from-window-top))))))))
                            
(defadvice next-line (after smooth-scroll-up
                            (&optional arg try-vscroll)
                            activate)
  "Scroll up smoothly if cursor is within `smooth-scroll-margin'
lines of the bottom of the window."
  (and
   ;; Only scroll up if there is buffer below the end of the window.
   (< (window-end) (buffer-end 1))
   (let ((lines-from-window-bottom
          (smooth-scroll-lines-from-window-bottom)))
     (and
      ;; Only scroll up if we're within the bottom margin
      (<= lines-from-window-bottom smooth-scroll-margin)
      ;; Only scroll up if we're in the bottom half of the window.
      (<= lines-from-window-bottom
          ;; See above notes on `window-height'.
          (/ (1- (window-height)) 2))
      (save-excursion
        (scroll-up
         (1+ (- smooth-scroll-margin lines-from-window-bottom))))))))
;;;_ + provide
(provide 'smooth-scrolling)

;;;_* Local emacs variables

;;Local variables:
;;allout-layout: (0 : -1 0)
;;mode: allout
;;End:

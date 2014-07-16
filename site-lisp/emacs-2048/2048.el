;;; 2048.el --- implementation of 2048 for GNU Emacs

;; Copyright (c) 2014 Steve Sprang
;; Author: Steve Sprang <scs@stevesprang.com>
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on: http://gabrielecirulli.github.io/2048/
 
;;; Code:

(require 'cl-lib)

(defvar 2048-buffer-name "*2048*")
(defvar 2048-grid-dim 4)
(defvar 2048-grid nil)
(defvar 2048-score 0)
(defvar 2048-moves 0)
(defvar 2048-winning-value 11)

(make-variable-buffer-local '2048-buffer-name)
(make-variable-buffer-local '2048-grid-dim)
(make-variable-buffer-local '2048-grid)
(make-variable-buffer-local '2048-score)
(make-variable-buffer-local '2048-moves)
(make-variable-buffer-local '2048-winning-value)

;; ;;;;;;;;;;; utility functions ;;;;;;;;;;;;;;;;;;;;;;

(defun 2048-any (bools)
  "Return t if any element of BOOLS is not nil."
  (if (cl-some #'identity bools) t nil))

(defun 2048-common-neighbors (nums)
  "Return t if any two neighbors in NUMS are identical."
  (2048-any (cl-mapcar #'= nums (cdr nums))))

(defun 2048-contains (obj list)
  "Return t if OBJ is in LIST."
  (if (member obj list) t nil))

(defun 2048-pad-with-zeros (nums length)
  "Return a copy of NUMS padded with enough zeros to reach LENGTH."
  (let ((missing (- length (length nums))))
    (if (<= missing 0) nums
      (append nums (make-list missing 0)))))

(defun 2048-strip-zeros (nums)
  "Return a copy of NUMS with all zeros removed."
  (cl-remove-if #'zerop nums))

(defun 2048-make-empty-grid (size)
  "Make a grid of SIZE x SIZE zeros (as a list of lists)."
  (let (grid)
    (dotimes (n size grid)
      (push (make-list size 0) grid))))

(defun 2048-reverse-grid (grid)
  "Reverse the order of GRID left to right."
  (mapcar #'reverse grid))

(defun 2048-transpose (grid)
  "Interchange the rows and columns of GRID."
  (mapcar #'(lambda (col) (mapcar #'(lambda (row) (nth col row)) grid))
	  (number-sequence 0 (1- (length (car grid))))))

;; ;;;;;;;;;;; game lifecycle ;;;;;;;;;;;;;;;;;;;;;;

(defun 2048-reset-game! ()
  "Reset global variables in preparation for a new game."
  (setf 2048-moves 0)
  (setf 2048-score 0)
  (setf 2048-grid (2048-make-empty-grid 2048-grid-dim))
  (dotimes (n 2)
    (2048-add-new-cell! 2048-grid))
  (2048-draw-grid 2048-grid ""))
      
(defun 2048-game-over (result)
  "Display end of game data and reset the key map."
  (2048-draw-grid 2048-grid (if (equal result :won) "You won!" "Game Over!"))
  (use-local-map 2048-null-map))

(defun 2048-next-round ()
  "Add a new cell to the grid and make sure it's still playable."
  (2048-add-new-cell! 2048-grid)
  (if (not (2048-move-exists-p 2048-grid))
      (2048-game-over :lost)
    (2048-draw-grid 2048-grid "")))

(defun 2048-move-action! (direction)
  "Implement the mechanics of the game. Called via player action."
  (let ((start 2048-grid)
	(end (pcase direction
	       (:left (2048-move-left 2048-grid))
	       (:right (2048-move-right 2048-grid))
	       (:up (2048-move-up 2048-grid))
	       (:down (2048-move-down 2048-grid)))))
    (if (not (equal start end)) ; successful move?
	(progn (setf 2048-moves (1+ 2048-moves))
	       (setf 2048-grid end)
	       ;; (if (2048-winner-p 2048-grid)
	       ;;     (2048-game-over :won)
	       ;;   (2048-next-round))
               (2048-next-round)))))

(defun 2048-move-left (grid)
  "Try to move the GRID elements to the left."
  (mapcar #'2048-merge-left-padded grid))

(defun 2048-move-right (grid)
  "Try to move the GRID elements to the right."
  (2048-reverse-grid (2048-move-left (2048-reverse-grid grid))))

(defun 2048-move-up (grid)
  "Try to move the GRID elements up."
  (2048-transpose (2048-move-left (2048-transpose grid))))

(defun 2048-move-down (grid)
  "Try to move the GRID elements down."
  (2048-transpose (2048-move-right (2048-transpose grid))))

(defun 2048-merge-left-padded (nums)
  "Pre and post process NUMS to deal with zeros when merging."
  (let* ((stripped (2048-strip-zeros nums))
	 (merged (2048-merge-left! stripped)))
    (2048-pad-with-zeros merged 2048-grid-dim)))

(defun 2048-merge-left! (nums)
  "Merge matching pairs in NUMS to the left; update score as appropriate."
  (pcase nums
    (`(,x ,x . ,ys) ; matching pair at the front
     (setf 2048-score (+ 2048-score (expt 2 (1+ x)))) ; update score
     (cons (1+ x) (2048-merge-left! ys)))             ; reduce and recurse
    (`(,x . ,ys) (cons x (2048-merge-left! ys)))
    (_ nums)))

;; ;;;;;;;;;;; grid status ;;;;;;;;;;;;;;;;;;;;;;

(defun 2048-move-exists-p (grid)
  "Return t if there are any legal moves in GRID."
  (or (2048-any (mapcar (apply-partially #'2048-contains 0) grid))
      (2048-any (mapcar #'2048-common-neighbors grid))
      (2048-any (mapcar #'2048-common-neighbors (2048-transpose grid)))))

(defun 2048-winner-p (grid)
  "Return t if GRID contains the winning cell."
  (2048-any (mapcar (apply-partially #'2048-contains 2048-winning-value) grid)))

;; ;;;;;;;;;;; spawning new cells ;;;;;;;;;;;;;;;;;;;;;;

(defun 2048-available-cells (grid)
  "Return a list of conses representing open coordinates in the GRID."
  (let ((result nil) (y 0))
    (dolist (row grid result)
      (let ((x 0))
	(dolist (col row result)
	  (if (zerop col) (push (cons y x) result))
	  (setf x (1+ x))))
      (setf y (1+ y)))))

(defun 2048-add-new-cell! (grid)
  "Add a new cell to an open space in the GRID."
  (let* ((options (2048-available-cells grid))
	 (pos (random (length options)))
	 (lucky-cell (nth pos options))
	 (row (nth (car lucky-cell) grid)))
    (setf (nth (cdr lucky-cell) row) (2048-new-cell-value))))

(defun 2048-new-cell-value ()
  "Return a value for a new cell with 10% chance of being a 4 (i.e. (expt 2 2))."
  (if (= (random 10) 0) 2 1))

;; ;;;;;;;;;;; display ;;;;;;;;;;;;;;;;;;;;;;

(defun 2048-value-to-string (n)
  "Convert N to a string with appropriate padding."
  (if (zerop n) (format "%6s" "'")
    (format "%6d" (expt 2 n))))

(defun 2048-draw-row (row)
  "Draw a ROW of the grid."
  (insert "\t")
  (insert (mapconcat #'2048-value-to-string row ""))
  (insert "\n\n"))

(defun 2048-draw-grid (grid message)
  "Draw the GRID with status MESSAGE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n\n\tMerge matching numbers with the arrow keys to reach 2048.")
    (insert "\n\n\tType 'n' for a new game and 'q' to quit.")
    (insert "\n\n\n\n")
    (save-excursion
      (mapc #'2048-draw-row grid))
    (end-of-line)
    (insert (format "\tScore: %d" 2048-score))
    (forward-line 2) 
    (end-of-line)
    (insert (format "\tMoves: %d" 2048-moves))
    (forward-line 4)
    (end-of-line)
    (insert "\t")
    (insert message)))

;; ;;;;;;;;;;; key handlers ;;;;;;;;;;;;;;;;;;;;;;

(defun 2048-start-game ()
  "Start a new game of 2048."
  (interactive)
  (2048-reset-game!)
  (use-local-map 2048-mode-map))

(defun 2048-quit-game ()
  "Quit the current game of 2048."
  (interactive)
  (kill-buffer 2048-buffer-name))

(defun 2048-move-left-action ()
  "Attempt to shift the board to the left."
  (interactive)
  (2048-move-action! :left))

(defun 2048-move-right-action ()
  "Attempt to shift the board to the right."
  (interactive)
  (2048-move-action! :right))

(defun 2048-move-up-action ()
  "Attempt to shift the board up."
  (interactive)
  (2048-move-action! :up))

(defun 2048-move-down-action ()
  "Attempt to shift the board down."
  (interactive)
  (2048-move-action! :down))
  
;; ;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;

(defvar 2048-mode-map
  (let ((map (make-sparse-keymap '2048-mode-map)))
    (define-key map "n"		'2048-start-game)
    (define-key map "q"		'2048-quit-game)

    (define-key map [left]	'2048-move-left-action)
    (define-key map [right]	'2048-move-right-action)
    (define-key map [up]	'2048-move-up-action)
    (define-key map [down]	'2048-move-down-action)
    map))

(defvar 2048-null-map
  (let ((map (make-sparse-keymap '2048-null-map)))
    (define-key map "n"		'2048-start-game)
    (define-key map "q"		'2048-quit-game)
    map))

;; ;;;;;;;;;;; 2048 mode ;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode 2048-mode special-mode "2048"
  "A mode for playing 2048."
  (use-local-map 2048-null-map)
  (setf show-trailing-whitespace nil))

(defun 2048-play ()
  "Switch to `2048-mode' and start a game of 2048."
  (interactive)
  (select-window (or (get-buffer-window 2048-buffer-name)
		     (selected-window)))
  (switch-to-buffer 2048-buffer-name)
  (2048-mode)
  (2048-start-game))

;;; 2048.el ends here

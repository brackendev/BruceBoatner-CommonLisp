;;;; 8-queens
;;;; Objective is to place one queen per row such that they
;;;; cannot attack each other.  Queens can attack in either
;;;; the column (x) or row (y) direction, plus diagonally.
;;;; For ease of printing, the board is oriented with 0,0
;;;; at top left and 7,7 at bottom right.
;;;; Created 3/21/19
;;;; Bruce Boatner

;;----------------- Running the Program -------------------;;
;;                                                         ;;
;; To run the program, first compile-all in this buffer:   ;;
;; Ctrl-c Ctrl-k                                           ;;
;;                                                         ;;
;; At the REPL buffer prompt, type:                        ;;
;; CL-USER> (8-queens)                                     ;;
;;                                                         ;;
;;---------------------------------------------------------;;

(defparameter *size* 8)
(defparameter *limit* 7)
(defvar *board*)   ; global variable referenced by all routines
(defvar board-8x8) ; initial "clean" board loaded at start
(setf board-8x8 '(- - - - - - - -
                  - - - - - - - -
                  - - - - - - - -
                  - - - - - - - -
                  - - - - - - - -
                  - - - - - - - -
                  - - - - - - - -
                  - - - - - - - -))
      
;;----------------------- 8-queens ------------------------;;
;; Uses a backtracking algorithm to place n queens on a    ;;
;; 8 x 8 board in positions where they cannot attack each  ;;
;; other.  Queens are placed one each per column.          ;;
;;                                                         ;;
;; Approach:                                               ;;
;; 1. Place a queen in the first position in a column.     ;;
;; 2. Check to see if it is threatened by any other queen  ;;
;;    (if any) already on the board.                       ;;
;; 3. If so, move to the next position, and check again.   ;;
;; 4. Repeat until the queen is safe.                      ;;
;; 5. If the last position is reached and the queen is not ;;
;;    safe, backtrack by repositioning the previously      ;;
;;    placed queen(s) and trying again.                    ;;
;; 6. Iterate until all 8 queens are safe.                 ;;
;;                                                         ;;
;;---------------------------------------------------------;;
(defun 8-queens ()
  (setf *board* (copy-list board-8x8))
  (if (place-queens 0)
      (format t "~&Success!")
      (format t "~&Nope!")))

;;--------------------- place-queens ----------------------;;
;; This is the function that does the recursive back-      ;;
;; tracking. The function returns T or NIL so that the     ;;
;; success of the down-stream recursive calls can be       ;;
;; evaluated and backtracking can occur is necessary.      ;;
;; v01 4/21/19 Created.                                    ;;
;;---------------------------------------------------------;;
(defun place-queens (col)
  (if (> col *limit*)
      (return-from place-queens t)) ; success - all queens placed
  (dotimes (row *size*) ; start each queen at the top of a column..
    (set-xy col row 'Q)  ; ..and step down through the rows.
    (if (is-safe? col row) ; does this position appear safe?
        (progn             ; YES...proceed!
          (format t "~&~5t~S,~S " col row) ; show position
          (print-board)
          (delay 100)
          (if (place-queens (+ col 1)) ; try the next queen and..
              (return-from place-queens t)) ; ..report if success.
          (format t "~&Backtrack!")
          (set-xy col row '-)))       ; Else erase this queen and backtrack
    (set-xy col row '-))  ; NO, this row position is not safe
  nil) ; Here if could not place the queen in the column
  
;;---------------------- is-safe? -------------------------;;
;; Check to see if a position is safe from other queens.   ;;
;; It is assumed there is only one queen per column, so    ;;
;; there is no need for q-in-col?.                         ;;
;; v01 4/21/19  Created                                    ;;
;;---------------------------------------------------------;;
(defun is-safe? (x y)
  (if (q-in-row? y) (return-from is-safe? nil)) 
  (if (q-in-diag? x y) (return-from is-safe? nil))
  t)

;;-------------------- q-in-row? --------------------------;;
;; Is there another queen in this row? Returns T or NIL.   ;;
;; This assumes that we have already placed our queen on   ;;
;; the board.                                              ;;
;; v01 4/21/19                                             ;;
;;---------------------------------------------------------;;
(defun q-in-row? (row)
  (do ((idx (* row *size*) (+ idx 1))
       (q-count 0)        ; count number of queens in row
       (ctr 0 (+ ctr 1))) ; count places in row
      ((= ctr *size*) nil); quit when at end of row 
    (if (equal (nth idx *board*) 'Q) (incf q-count))
    (if (> q-count 1) (return t))))

;;-------------------- q-in-diag? -------------------------;;
;; Is there a queen in any of the diagonal positions?      ;;
;; Only the diagonals to the left of the position must be  ;;
;; checked, because the queens to the right have not been  ;;
;; placed yet. Returns T or NIL                            ;;
;; v01 4/21/19 Created                                     ;;
;; v02 4/22/19 Changed algorithm - check left side only.   ;;
;;---------------------------------------------------------;;
(defun q-in-diag? (col row)
;;;  Check upper diagonal on left side
  (do ((x (- col 1) (decf x))
       (y (- row 1) (decf y)))
      ((or (minusp x) (minusp y)) nil)
    (if (equal (get-xy x y) 'Q)
        (return-from q-in-diag? t)))
;;;  Check lower diagonal on left side
  (do ((x (- col 1) (decf x))
       (y (+ row 1) (incf y)))
      ((or (minusp x) (> y *limit*)) nil)
    (if (equal (get-xy x y) 'Q)
        (return-from q-in-diag? t))))

;;------------------------- get-xy ------------------------;;
;; Return data from the board structure list based on x-y  ;;
;; coordinates.  Return nil if out of bounds.              ;;
;; v01 4/21/19                                             ;;
;;---------------------------------------------------------;;
(defun get-xy (x y)
  (cond
    ((> x *limit*) nil)
    ((> y *limit*) nil)
    (t (nth (+ x (* *size* y)) *board*))))

;;------------------------- set-xy -------------------------;;
;; Set a value in the board matrix based on x-y coordinates.;;
;; v01 4/21/19                                              ;;
;;----------------------------------------------------------;;
(defun set-xy (x y val)
  (setf (nth (+ x (* *size* y)) *board*) val))

;;-------------------- print-board ------------------------;;
;; Formats the global var *board* into rows and columns,   ;;
;; v01 4/21/19                                             ;;
;;---------------------------------------------------------;;
(defun print-board ()
  (let ((bored (copy-list *board*)))
    (format t "~%")
    (dotimes (x *size*)
      (dotimes (y *size*)
        (format t "~S " (pop bored)))
      (format t "~&"))))

;;------------------------ delay --------------------------;;
;; Recursive delay routine.                                ;;
;; v01 4/21/19                                             ;;
;;---------------------------------------------------------::
(defun delay (n)
  (delay1 (* 1000000  n)))
(defun delay1 (n)
  (if (= n 0)
      nil
      (delay1 (- n 1))))

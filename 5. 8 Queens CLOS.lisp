;;
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;;
;;            8-Queens II: Object-Oriented Edition         ;;
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;;
;;                                                         ;;
;; The objective is to place one queen per row such that   ;;
;; they cannot attack each other.  Queens are vulnerable   ;;
;; in both the horizontal (x) direction and diagonally.    ;;
;; Since only one queen is placed per column, there is     ;;
;; not a concern about the vertical direction.             ;;
;;                                                         ;;
;;                       NOTES                             ;;
;; For ease of display/printing, the board is oriented     ;;
;; with 0,0 at top left and 7,7 at bottom right.           ;;
;;                                                         ;;
;; Many of the functions dealing with multiple instances   ;;
;; of queen objects could probably have been written with  ;;
;; MACROS to reduce the lines of code.  However, I feel    ;;
;; that clarity is far more important than brevity.        ;;
;; Feel free to experiment with your own macros when       ;;
;; dealing with the repetitive operations on the eight     ;;
;; queen objects.                                          ;;
;;                                                         ;;
;;~~~~~~~~~~~~~~~ About the CLOS edition ~~~~~~~~~~~~~~~~~~;;
;;                                                         ;;
;; The goal was to create a simple object that can be      ;;
;; replicated 8 times.  Each queen object would perform    ;;
;; the identical functions and exhibit identical behavior, ;;
;; except for each being confined to its own assigned row. ;; 
;;                                                         ;;
;; Created 5/6/19                                          ;;
;; Bruce Boatner                                           ;;
;;                                                         ;;
;;----------------- Running the Program -------------------;;
;;                                                         ;;
;; Paste the source code into the *scratch* buffer and     ;;
;; <Save-As> [filename].lisp                               ;;
;;                                                         ;;
;; To run the program, first compile-all in this buffer:   ;;
;;     Ctrl-c Ctrl-k                                       ;;
;; >>> You will get a lot of warnings because q0..q7 have  ;;
;;     not been defined yet.                               ;;
;;                                                         ;;
;; At the REPL buffer prompt, type:                        ;;
;;     CL-USER> (make-8-queens)                            ;;
;;                                                         ;;
;; Now repeat Ctrl-c Ctrl-k in source code buffer.  All    ;;
;; code should now compile without errors.                 ;;
;;                                                         ;;
;; At the REPL buffer prompt, type:                        ;;
;;     CL-USER> (8-queens)                                 ;;
;;                                                         ;;
;;---------------------------------------------------------;;

;; Constants
(defconstant +size+ 8)
(defconstant +limit+ 7)
(defconstant +t-delay+ 20)
;; Global Variables
(defparameter *done* nil)
(defparameter *cur-state* 0)
(defparameter *last-state* 0)
(defvar *board*)    ; working board
(defparameter board ; empty board
  '(- - - - - - - -
    - - - - - - - -
    - - - - - - - -
    - - - - - - - -
    - - - - - - - -
    - - - - - - - -
    - - - - - - - -
    - - - - - - - -))

;;------------------------------------------------------------;;
;;                Defining the queen class                    ;;
;;------------------------------------------------------------;;
(defclass queen ()
  ((col     :initarg :col :accessor col?)
   (y-pos   :initform  0  :accessor y-pos?) 
   (last-y  :initform  0  :accessor last-y?)))

(defun make-queen (x)    ; queen constructor
  (make-instance 'queen :col x))

(defun make-8-queens ()
  (defvar q0 (make-queen 0))
  (defvar q1 (make-queen 1))
  (defvar q2 (make-queen 2))
  (defvar q3 (make-queen 3))
  (defvar q4 (make-queen 4))
  (defvar q5 (make-queen 5))
  (defvar q6 (make-queen 6))
  (defvar q7 (make-queen 7)))

(defgeneric reset-queen (obj)
  (:documentation "Resets the y-pos & last-y vars."))

(defmethod reset-queen ((obj queen))
  (setf (y-pos? obj) 0)
  (setf (last-y? obj) 0))

(defun reset-queens ()
  (reset-queen q0)
  (reset-queen q1)
  (reset-queen q2)
  (reset-queen q3)
  (reset-queen q4)
  (reset-queen q5)
  (reset-queen q6)
  (reset-queen q7))

(defun reset-all ()
  (reset-queens)
  (setf *cur-state* 0)
  (setf *last-state* 0)
  (setf *done* nil)
  (setf *board* (copy-list board))
  (print-board))

;;------------------------ delay --------------------------;;
;; Recursive delay routine.                                ;;
;; v01 4/21/19  Created for first 8-queens program.        ;;
;;---------------------------------------------------------::
(defun delay (n)
  (delay1 (* 1000000  n)))
(defun delay1 (n)
  (if (= n 0)
      nil
      (delay1 (- n 1))))

;;-------------------- print-board ------------------------;;
;; Formats the global var *board* into rows and columns,   ;;
;; v01 4/21/19  Created for first 8-queens program.        ;;
;;---------------------------------------------------------;;
(defun print-board ()
  (let ((bored (copy-list *board*)))
    (format t "~%")
    (dotimes (x +size+)
      (dotimes (y +size+)
        (format t "~S " (pop bored)))
      (format t "~&"))))

;;---------------------- remove-queen -------------------------;;
;; Cloned mostly from show-pos method.                         ;;
;; v01 5/9/19 Created.                                         ;;
;;-------------------------------------------------------------;;
(defgeneric remove-queen (obj)
  (:documentation "Special case for backtracking: erase the last
    position of the active queen, then set the queen's x/y
    position to 0/0, but do not show the queen on the board."))

(defmethod remove-queen ((obj queen))
  (let ((x (col? obj))
        (last-y (last-y? obj)))
    (setf (nth (+ x (* +size+ last-y)) *board*) '-) ; erase last position
    (setf (y-pos? obj) 0)  ; reset position to top of column
    (setf (last-y? obj) 0) 
    (print-board)))

;;------------------------ show-pos --------------------------;;
;; Write a Q in the queen's position on the board and over-   ;;
;; write the last position with a dash ('-).  Update the      ;;
;; last-y variable with the y-pos. Print the board to show    ;;
;; the updated position.                                      ;;
;;                                                            ;;
;; v0.1 4/29/19 Created for Maze Runner.                      ;;
;; v0.2 4/29/19 Set *done* global if goal has been reached.   ;;
;; v0.3 4/30/19 Incorporated print-puzzle into method.        ;;
;; v0.4 5/7/19  Modified for 8 Queens II.                     ;; 
;;------------------------------------------------------------;;
(defgeneric show-pos (obj)
  (:documentation "Show the current position of a queen on the board
    and erase the previous position."))

(defmethod show-pos ((obj queen))
  (let ((x (col? obj))
        (y (y-pos? obj))
        (last-y (last-y? obj)))
    (unless (= last-y -1)
      (setf (nth (+ x (* +size+ last-y)) *board*) '-) ; erase last position
      (setf (nth (+ x (* +size+ y)) *board*) 'Q)) ; poke a Q into cur position
    (setf (last-y? obj) y)) ; update last-y position.
  (print-board))
                  
;;--------------------- queens-in-row? ----------------------;;
;; v01 4/21/19 Created for first 8-queens program.           ;;
;; v02 5/7/19  Counts the number of other queens in a row.   ;;
;;-----------------------------------------------------------;;
(defgeneric queens-in-row? (obj)
  (:documentation "Return the number of queens in this row, 
    not including the known queen."))

(defmethod queens-in-row? (obj)
  (let ((row (y-pos? obj)))
    (do ((idx (* row +size+) (+ idx 1))
         (q-count -1)       ; subtract out known queen
         (ctr 0 (+ ctr 1))) ; count places in row
        ((= ctr +size+) q-count); at end of row, return count
      (if (eq (nth idx *board*) 'Q) (incf q-count)))))

;;------------------------- get-xy ------------------------;;
;; Return data from the board structure list based on x-y  ;;
;; coordinates.  Return nil if out of bounds.  Needed for  ;;
;; queens-in-diag? function.                               ;;
;;                                                         ;;
;; v01 4/21/19  Created for first 8-queens program.        ;;
;;---------------------------------------------------------;;
(defun get-xy (x y)
  (cond
    ((> x +limit+) nil)
    ((> y +limit+) nil)
    (t (nth (+ x (* +size+ y)) *board*))))

;;---------------------- queens-in-diag? ------------------------;;
;; Count the number of other queens that exist in the upper      ;;
;; and lower diagonals.  Only the left side needs to be checked. ;;
;; v01 4/21/19 Created for 8-Queens                              ;;
;; v02 4/22/19 Changed algorithm - check left side only.         ;;
;; v03 5/7/19  Modified for OOP, checks on both sides.           ;;
;; v04 5/8/19  Removed right-side checking. In archive file.     ;;
;;---------------------------------------------------------------;;
(defgeneric queens-in-diag? (obj)
  (:documentation "Returns the count of other queens in upper
   and lower diagonals on the left-hand side, since queens have
   not yet been placed in columns to the right."))

(defmethod queens-in-diag? (obj)
  (let ((col (col? obj))
        (row (y-pos? obj))
        (q-count 0))
;;;  Check upper diagonal on left side
    (do ((x (1- col) (1- x))
         (y (1- row) (1- y)))
        ((or (minusp x) (minusp y)) nil)
      (if (eq (get-xy x y) 'Q)
          (setf q-count (1+ q-count))))
;;;  Check lower diagonal on left side
    (do ((x (1- col) (1- x))
         (y (1+ row) (1+ y)))
        ((or (minusp x) (> y +limit+)) nil)
      (if (eq (get-xy x y) 'Q)
          (setf q-count (1+ q-count))))
    q-count))

;;------------------- find-safe-square -----------------------;;
;; Difference between'fwd and 'back action:                   ;;
;;   'fwd: Check for safe first, step to next square after.   ;;
;;   'back: Step to next square before checking for safe.     ;;
;; Take only one action per pass. Do not iterate to end of    ;;
;; column looking for a safe square.                          ;;
;;                                                            ;;
;;        THIS PROCEDURE CONTROLS ALL STATE CHANGES.          ;;
;;                                                            ;;
;; v01 5/9/19 Created.                                        ;;
;;------------------------------------------------------------;;
(defgeneric find-safe-square (obj state-dir)
  (:documentation "Step down through the squares of a column to
   find one that is safe from other queens."))

(defmethod find-safe-square ((obj queen) state-dir)
  (cond ((eq state-dir 'fwd)
         (show-pos obj)
         (cond ((zerop (+ (queens-in-row? obj) (queens-in-diag? obj)))
                (format t "~s Safe" (1+ *cur-state*))
                (setf *last-state* *cur-state*)     ; safe square here
                (setf *cur-state* (1+ *cur-state*))) ; change states
               (t ; not a safe square here
                (format t "~s Not Safe" (1+ *cur-state*))
                (cond ((= (y-pos? obj) +limit+) ; at the last position?
                       (setf (y-pos? obj) 0)
                       (setf *last-state* *cur-state*)
                       (setf *cur-state* (1- *cur-state*))
                       (remove-queen obj)
                       (princ "Backtrack"))
                      (t               ; not last position - try next
                       (setf (y-pos? obj) (1+ (y-pos? obj)))))))) ; move down
        ((eq state-dir 'back) ; BACK-TRACKING
         (cond ((= (y-pos? obj) +limit+) ; at the last position?
                (setf (y-pos? obj) 0)
                (setf *last-state* *cur-state*)
                (setf *cur-state* (1- *cur-state*))
                (remove-queen obj)
                (princ "Backtrack"))
               (t
                (setf (y-pos? obj) (1+ (y-pos? obj))) ; move from safe square!
                (setf *last-state* *cur-state*)))))) ; back to usual search.

;;-------------------- 8-queens supervisor ---------------------;;
;;                                                              ;;
;; 8-queens is a state-oriented function that interacts with    ;;
;; the queen objects.  Each state represents one column and one ;;
;; queen. The *cur-state* and *last-state*  global variables    ;;
;; are used as a means of communication between the eight queen ;;
;; objects and the supervisor.                                  ;;
;;                                                              ;;
;; If all queens were safe on the first try, the states would   ;;
;; progress linearly from 0 to 7, with state 8 signalling       ;;
;; completion.  However, when a state transition is recognized  ;;
;; to be "backwards", then back-tracking actions are required . ;;
;;                                                              ;;
;; v01 5/8/19 Created.                                          ;;
;;--------------------------------------------------------------;;
(defun 8-queens ()
  (reset-all)
  (do ((state-dir nil)
       (ctr 0 (1+ ctr)))
      (*done*) ; Loop until *done* = T
    (delay +t-delay+)
    ;; Compare current and last states to determine direction.
    (cond ((= *cur-state* *last-state*) (setf state-dir 'fwd))
          ((> *cur-state* *last-state*) (setf state-dir 'fwd))
          ((< *cur-state* *last-state*) (setf state-dir 'back)))
    ;; Dispatch based on current state.
    (case *cur-state*
      ((0) (find-safe-square q0 state-dir))
      ((1) (find-safe-square q1 state-dir))
      ((2) (find-safe-square q2 state-dir))
      ((3) (find-safe-square q3 state-dir))
      ((4) (find-safe-square q4 state-dir))
      ((5) (find-safe-square q5 state-dir))
      ((6) (find-safe-square q6 state-dir))
      ((7) (find-safe-square q7 state-dir))
      (otherwise
       (format t "~&> Success in ~s moves!" ctr)
       (setf *done* t)))))

(setf *done* t) ; compile this to force program stop

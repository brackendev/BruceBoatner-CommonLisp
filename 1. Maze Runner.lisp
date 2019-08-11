;;
;;-------------------- Maze-Runner-20x20 ------------------;;
;; To run the program:                                     ;;
;;   Load this code into the Portacle *scratch* buffer.    ;;
;;   <Save-As> [filename].lisp                             ;;
;;   Compile-all: Ctrl-c Ctrl-k                            ;;
;;   In Slime REPL buffer type:                            ;;
;;                                                         ;;
;;       CL-USER> (run-maze maze1)                         ;;
;;                                                         ;;
;; Created 3/20/19                                         ;;
;; Bruce Boatner                                           ;;
;;---------------------------------------------------------;;

(defconstant +size+ 20)
(defconstant +limit+ 19)
(defparameter *delay* 30)
(defparameter *safety-count* 500) ; emergency halt
(defvar *dir*)  ; 1=right, 2=down, 3=left, 4=up
(defvar *x-y*)  ; global x-y position of bug in maze

(defparameter maze1 '(- - - - - - - - - - - - - - - - - - - - 
                      x x x x x x x x x - x x x x - x x x x -
                      x - - - - - - - x - x x x x - x x - x -
                      x x - x x x x - x - x - - - - - - - x -
                      x x - - x x x - x - x - x x x x x x x -
                      - - - - x x x - x - x - x x - x - - - -
                      x x x - x x x - x - x - x x x x - x x x
                      - - - - - - x - x - x - - - x - - x x x
                      - x - x - x x - x x x x - x x x - - - -
                      - - - x - - - - - - - - - - - x x x - x
                      x - - - - x x x x x x x x x x x - - - x
                      x - x x - x - x x - - - - x - x - x x x
                      x - x x - x - x x - x x - x - x - x x x
                      - - - - - x - x x - x x - x - x - - - -
                      - - - x x x - x x - x x x x - x x x x -
                      - x x - - - - - - - - x x x - x - - x -
                      - x - x - - - x - x - - - x - x - - - -
                      - - - x x - - x x x - x x x - x - - x x
                      x x - x - - x x - - - - - x - x x - x x
                      x x - - - - - - - x - x x x - - - - - -))

;;----------------------- run-maze -----------------------;;
;; There is a general rule for finding your way out of    ;;
;; any maze, no matter how complex (assuming there is     ;;
;; a way out). The simple rule is this:                   ;;
;; *** Choose a wall to your right or left, and follow    ;;
;;     that wall, maintaining it on your chosen side,     ;;
;;     until it leads you out of the maze. ***            ;;
;;                                                        ;;
;; Notice that in order to follow the wall on your right  ;;
;; side, for instance, there will be instances where you  ;;
;; will need to make a left turn in order to keep the     ;;
;; wall on your right.  For instance, if you have reached ;;
;; the end of a blind alley, you will need to make two    ;;
;; left turns to retrace your path.                       ;;
;;                                                        ;;
;; If you observe the program running, it will appear as  ;;
;; if a backtracking algorithm is at work, but in fact,   ;;
;; it is not.  Just this simple if-then-else loop will    ;;
;; accomplish the desired results:                        ;;
;;                                                        ;;
;; IF nothing to the right                                ;;
;;    THEN turn RIGHT and move forward                    ;;
;; ELSE-IF nothing ahead  <blocked right>                 ;;
;;    THEN move forward                                   ;;
;;    ELSE turn LEFT      <blocked right & forward>       ;;
;;                                                        ;;
;; Iterate the above until x/y = +limit+/+limit+          ;;
;;                                                        ;;
;; After compiling all w/ Ctrl-c Ctrl-k, to run the       ;;
;; program, type:                                         ;;
;;                CL-USER> (run-maze maze1)               ;;
;;                                                        ;;
;; v1.0 4/20/19 Created                                   ;;
;; v1.1 4/21/19 Documentation                             ;;
;; v1.2 5/2/19  Editing & documentation.                  ;;
;;------------------------------------------------------  ;;
(defun run-maze (maze)
  (setf *dir* 1) ; direction = right
  (setf *x-y* (copy-list '(0 0)))  ; global x, y coordinates
  (do ((n 0 (+ n 1)))   ; dead-man switch to end
      ((= *dir* 0) n)  ; end test
    (if (is-open-rgt? maze) ; if nothing on right..
        (progn        
          (turn-right)      ; .. turn right..
          (move-fwd)))      ; .. and move forward.
    (if (is-open-fwd? maze) ; if clear ahead..
        (move-fwd)          ; ..move forward..
        (turn-left))        ; ..else turn left.
    (if (and (= (car *x-y*) +limit+) (= (cadr *x-y*) +limit+))
        (setf *dir* 0))     ; maze finished.
    (if (> n *safety-count*)
        (setf *dir* 0))     ; force stop
    (delay *delay*)
    (format t "~%")
    (print-maze (update-pos maze))))

;;---------------------- turn-right -----------------------;;
;; v01 4/20/19 Created                                     ;;
;; v02 4/21/19 Use incf macro.                             ;;
;;---------------------------------------------------------;;
(defun turn-right ()
  (if (= *dir* 4)
      (setf *dir* 1) ; wrap around
      (setf *dir* (incf *dir*)))) ; else increment

;;---------------------- turn-left ------------------------;;
;; v01 4/20/19 Created                                     ;;
;; v02 4/21/19 Use decf macro.                             ;;
;;---------------------------------------------------------;;
(defun turn-left ()
  (if (= *dir* 1)
      (setf *dir* 4) ; wrap around
      (setf *dir* (decf *dir*)))) ; else increment

;;----------------------- move-fwd ------------------------;;
;; Move one space forward in the maze. This is dependent   ;;
;; on current position and direction of travel. No check-  ;;
;; ing is done in this routine, as it assumes it is called ;;
;; only after verifying that the space ahead is clear.     ;;
;; v 01 4/20/19 Created                                    ;;
;; v 02 4/21/19 Use incf & decf macros.                    ;;
;;---------------------------------------------------------;;
(defun move-fwd ()
  (let ((dir *dir*)  ; will not be modified here
        (x (car *x-y*))
        (y (cadr *x-y*)))
    (cond
      ((= dir 1) (setf (car *x-y*) (incf x)))   ; right
      ((= dir 2) (setf (cadr *x-y*) (incf y)))  ; down
      ((= dir 3) (setf (car *x-y*) (decf x)))   ; left 
      ((= dir 4) (setf (cadr *x-y*) (decf y)))))) ; up
        
;;------------------- is-open-rgt? ------------------------;;
;; Check to see if the position to the right of the mouse  ;;
;; is open/empty relative to the current position and      ;;
;; direction of movement.                                  ;;
;; 1 = right, 2 = down, 3 = left, 4 = up                   ;;
;; v 01 4/20/19 Created                                    ;;
;; v 02 4/21/19 Use incf & decf macros.                    ;;
;;---------------------------------------------------------;;
(defun is-open-rgt? (maze)
  (let ((dir *dir*)
        (x (car *x-y*))
        (y (cadr *x-y*))
        (val nil))
    (cond
      ((and (= dir 1) (= y +limit+)) (setf val 'x)) ; right
      ((and (= dir 2) (= x 0)) (setf val 'x))       ; down
      ((and (= dir 3) (= y 0)) (setf val 'x))       ; left
      ((and (= dir 4) (= x +limit+) (setf val 'x))) ; up
      ((= dir 1) (setf val (get-xy maze x (incf y))))
      ((= dir 2) (setf val (get-xy maze (decf x) y)))
      ((= dir 3) (setf val (get-xy maze x (decf y))))
      ((= dir 4) (setf val (get-xy maze (incf x) y))))
    (if (equal val '-)
        T
        nil)))

;;------------------- is-open-fwd? ------------------------;;
;; Check to see if the position in front of the mouse      ;;
;; is open/empty relative to the current position and      ;;
;; direction of movement.                                  ;;
;; 1 = right, 2 = down, 3 = left, 4 = up                   ;;
;; v01 4/20/19                                             ;;
;;---------------------------------------------------------;;
(defun is-open-fwd? (maze)
  (let ((dir *dir*)
        (x (car *x-y*))
        (y (cadr *x-y*))
        (val nil))
    (cond
      ((and (= dir 1) (= x +limit+)) (setf val 'x))
      ((and (= dir 2) (= y +limit+)) (setf val 'x))
      ((and (= dir 3) (= x 0)) (setf val 'x))
      ((and (= dir 4) (= y 0)) (setf val 'x))
      ((= dir 1) (setf val (get-xy maze (incf x) y)))
      ((= dir 2) (setf val (get-xy maze x (incf y))))
      ((= dir 3) (setf val (get-xy maze (decf x) y)))
      ((= dir 4) (setf val (get-xy maze x (decf y)))))
    (if (equal val '-)
        T
        nil)))

;;------------------------- get-xy ------------------------;;
;; Return data from the maze structure list based on x-y   ;;
;; coordinates.  Return nil if out of bounds.              ;;
;; v01 4/19/19                                             ;;
;;---------------------------------------------------------;;
(defun get-xy (maze x y)
  (cond
    ((> x +limit+) nil)
    ((> y +limit+) nil)
    (t (nth (+ x (* +size+ y)) maze))))

;;------------------------- set-xy -------------------------;;
;; Set a value in the maze matrix based on x-y coordinates. ;;
;; v01 4/19/19                                              ;;
;;----------------------------------------------------------;;
(defun set-xy (maze x y val)
  (setf (nth (+ x (* +size+ y)) maze) val))

;;---------------------- update-pos -----------------------;;
;; Poke a Zero into a copy of the maze at the present      ;;
;; mouse position, in preparation for printing/displaying. ;;
;; v01 4/20/19                                             ;;
;;---------------------------------------------------------;;
(defun update-pos (maze)
  (let ((work-maze (copy-list maze))
        (x (car *x-y*))
        (y (cadr *x-y*)))
    (set-xy work-maze x y 0)
    work-maze))

;;---------------------- print-maze -----------------------;;
;; From maze-runner-10                                     ;;
;; v01 4/19/19                                             ;;
;;---------------------------------------------------------;;
(defun print-maze (maze)
  (let ((amaze (copy-list maze)))
    (format t "~&")
    (dotimes (x +size+)
      (dotimes (y +size+)
        (format t "~S " (pop amaze)))
      (format t "~&"))))

;;------------------------ delay --------------------------;;
;; Recursive delay routine.                                ;;
;; v01 4/19/19                                             ;;
;;---------------------------------------------------------::
(defun delay (n)
  (delay1 (* 1000000  n)))
(defun delay1 (n)
  (if (= n 0)
      nil
      (delay1 (- n 1))))

(defun count-list (lst) ; use to verify maze integrity after editing
  (if (null lst)
      0
      (+ (count-list (cdr lst)) 1)))

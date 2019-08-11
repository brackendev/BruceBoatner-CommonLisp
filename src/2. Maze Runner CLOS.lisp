;;
;;----------------------- maze-runner-CLOS ------------------------;;
;;                                                                 ;;
;; Multiple mice will try to find their way out of the maze at the ;;
;; same time.  There will be two different types of mice:          ;;
;; right-handed mice and left-handed mice.  Right handed mice will ;;
;; follow these rules:                                             ;;
;;                                                                 ;;
;;  1. If it's open to the right, turn right and move forward.     ;;
;;  Else                                                           ;;
;;  2. If open ahead, move forward.                                ;;
;;  Else                                                           ;;
;;  3. Turn left.                                                  ;;
;;                                                                 ;;
;; Left-handed mice will perform the above rules in reverse.       ;;
;; The different types of mice can be pre-positioned at different  ;;
;; starting points in the maze, and given an initial direction.    ;;
;;                                                                 ;;
;; First compile the source code: Ctrl-c Ctrl-k                    ;;
;; Then, in the REPL window:      CL-USER> (run-maze)              ;;
;;                                                                 ;;
;;  v0.1 4/29/19 Created                                           ;;
;;  v1.0 4/30/19 Tested with rh & lh mice running simultaneously.  ;;
;;-----------------------------------------------------------------;;
(defvar *maze*)
(defparameter *size* 20)
(defparameter *limit* 19)
(defparameter *done* nil)
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
                      - x - x - x - x - x - - - x - x - - - -
                      - - - x - x - x x x - x x x - x - - x x
                      x x - x - x - x - - - - - x - x x - x x
                      - - - - - x - - - x - x x x - - - - - -))

;;------------------------------------------------------------;;
;;                      DEFINE CLASSES                        ;;
;;------------------------------------------------------------;;
(defclass mouse ()
  ((dir    :initform 2 :accessor dir?)   ; Default to West direction
   (x-pos  :initform 0 :accessor x-pos?) ; Default to top left-hand..
   (y-pos  :initform 0 :accessor y-pos?) ; ..corner of the maze.
   (last-x :initform 0 :accessor last-x?)
   (last-y :initform 0 :accessor last-y?)))
(defclass rh-mouse (mouse) ())  ; Different explore methods apply..
(defclass lh-mouse (mouse) ())  ; ..otherwise the same as mouse.

(defun make-rh-mouse ()         ; rh mouse constructor
  (make-instance 'rh-mouse))
(defun make-lh-mouse ()         ; lh mouse constructor
  (make-instance 'lh-mouse))

(defvar rh-m1 (make-rh-mouse))  ; Make your mice here
(defvar lh-m1 (make-lh-mouse))

;;------------------------------------------------------------;;
;;                      DEFINE METHODS                        ;;
;;------------------------------------------------------------;;
(defgeneric set-pos (obj dir x y)
  (:documentation "Sets the mouse to the specified direction and
starting position in the maze to start."))

(defmethod set-pos ((obj mouse) dir x y)
  (setf (dir? obj) dir)
  (setf (x-pos? obj) x)
  (setf (y-pos? obj) y)
  (setf (last-x? obj) x)
  (setf (last-y? obj) y))

;;------------------------ show-pos --------------------------;;
;; Write a 0 (zero) to the position that the mouse is, and    ;;
;; over-write the last position with a dash ('-).             ;;
;;                                                            ;;
;; v0.1 4/29/19 Created.                                      ;;
;; v0.2 4/29/19 Set *done* global if goal has been reached.   ;;
;; v0.3 4/30/19 Incorporated print-puzzle into method.        ;;
;;------------------------------------------------------------;;
(defgeneric show-pos (obj)
  (:documentation "Show current position of mouse in maze."))

(defmethod show-pos ((obj mouse))
  (let ((x (x-pos? obj))
        (y (y-pos? obj))
        (last-x (last-x? obj))
        (last-y (last-y? obj)))
    (if (and (= x *limit*) (= y *limit*)) ; set the done flag if finished
        (setf *done* t))
    (setf (nth (+ last-x (* *size* last-y)) *maze*) '-)  ; erase last position
    (setf (nth (+ x (* *size* y)) *maze*) 0)) ; poke a zero into cur position
  (print-maze))

;;------------------------ explore --------------------------;;
;; Explore is the basic set of rules for the mouse to follow ;;
;; to escape from the maze.  It can have 2 versions: the     ;;
;; right-hand rules and the left-hand rules (2 methods).     ;;
;; v01 4/29/19 Created.                                      ;;
;;-----------------------------------------------------------;;
(defgeneric explore (obj)
  (:documentation "Search for exit using right or left-turning rules."))

(defmethod explore ((obj rh-mouse)) ; right-hand rules
  (if (is-open-right? obj) ; If nothing on right..
      (progn      
        (turn-right obj)   ; .. turn right..
        (move-fwd obj))    ; .. and move forward.
      (if (is-open-fwd? obj)   ; If clear ahead..
          (move-fwd obj)     ; ..move forward.
          (turn-left obj)))  ; Else turn left.   
  (show-pos obj))

(defmethod explore ((obj lh-mouse)) ; left-hand rules
  (if (is-open-left? obj)   ; If nothing to the left..
      (progn      
        (turn-left obj)     ; .. turn left..
        (move-fwd obj))     ; .. and move forward.
      (if (is-open-fwd? obj)   ; If clear ahead..
          (move-fwd obj)     ; ..move forward.
          (turn-right obj))) ; Else turn right.   
  (show-pos obj))

;;------------------- is-open-right? ----------------------;;
;; Check to see if the position to the right of the mouse  ;;
;; is open/empty relative to the current position and      ;;
;; direction of movement.                                  ;;
;;                                                         ;;
;; v01 4/20/19 Created                                     ;;
;; v02 4/21/19 Use incf & decf macros.                     ;;
;; v03 4/29/19 Changed directions, 1,2,3,4 = N, E, S, W    ;;
;;             Converted to method for mouse class.        ;;
;;---------------------------------------------------------;;
(defgeneric is-open-right? (obj)
  (:documentation "Check if space to right is free."))

(defmethod  is-open-right? ((obj mouse))
  (let ((dir (dir? obj))
        (x (x-pos? obj))
        (y (y-pos? obj))
        (val nil))
    (cond
      ((and (= dir 1) (= x *limit*)) (setf val 'x)) ; Facing North
      ((and (= dir 2) (= y *limit*)) (setf val 'x)) ; Facing East
      ((and (= dir 3) (= x 0)) (setf val 'x))       ; Facing South
      ((and (= dir 4) (= y 0)) (setf val 'x))       ; Facing West
      ((= dir 1) (setf val (get-xy (incf x) y)))    ; North
      ((= dir 2) (setf val (get-xy x (incf y))))    ; East
      ((= dir 3) (setf val (get-xy (decf x) y)))    ; South
      ((= dir 4) (setf val (get-xy x (decf y)))))   ; West
    (if (equal val '-)
        T
        nil)))

;;-------------------- is-open-left? ----------------------;;
;; Check to see if the position to the left of the mouse   ;;
;; is open/empty relative to the current position and      ;;
;; direction of movement.                                  ;;
;;                                                         ;;
;; v01 4/30/19 Created method                              ;;
;;---------------------------------------------------------;;
(defgeneric is-open-left? (obj)
  (:documentation "Check if space to left is free."))

(defmethod  is-open-left? ((obj mouse))
  (let ((dir (dir? obj))
        (x (x-pos? obj))
        (y (y-pos? obj))
        (val nil))
    (cond
      ((and (= dir 1) (= x 0)) (setf val 'x))       ; Facing North
      ((and (= dir 2) (= y 0)) (setf val 'x))       ; Facing East
      ((and (= dir 3) (= x *limit*)) (setf val 'x)) ; Facing South
      ((and (= dir 4) (= y *limit*)) (setf val 'x)) ; Facing West
      ((= dir 1) (setf val (get-xy (decf x) y)))    ; North
      ((= dir 2) (setf val (get-xy x (decf y))))    ; East
      ((= dir 3) (setf val (get-xy (incf x) y)))    ; South
      ((= dir 4) (setf val (get-xy x (incf y)))))   ; West
    (if (equal val '-)
        T
        nil)))

;;------------------- is-open-fwd? ------------------------;;
;; Check to see if the position in front of the mouse      ;;
;; is open/empty relative to the current position and      ;;
;; direction of movement.                                  ;;
;;                                                         ;;
;; v01 4/20/19 Created                                     ;;
;; v02 4/29/19 New directions: 1,2,3,4 = N, E, S, W.       ;;
;;             Uses global var *maze* instead of param.    ;;
;;             Converted to method for mouse class.        ;;
;;---------------------------------------------------------;;
(defgeneric is-open-fwd? (obj)
  (:documentation "Check if space ahead is free."))

(defmethod is-open-fwd? ((obj mouse))
  (let ((dir (dir? obj))
        (x (x-pos? obj))
        (y (y-pos? obj))
        (val nil))
    (cond
      ((and (= dir 1) (= y 0)) (setf val 'x))       ; Facing North
      ((and (= dir 2) (= x *limit*)) (setf val 'x)) ; Facing East
      ((and (= dir 3) (= y *limit*)) (setf val 'x)) ; Facing South
      ((and (= dir 4) (= x 0)) (setf val 'x))       ; Facing West
      ((= dir 1) (setf val (get-xy x (decf y))))
      ((= dir 2) (setf val (get-xy (incf x) y)))
      ((= dir 3) (setf val (get-xy x (incf y))))
      ((= dir 4) (setf val (get-xy (decf x) y))))
    (if (equal val '-)
        T
        nil)))

;;---------------------- turn-right -----------------------;;
;; v01 4/20/19 Created                                     ;;
;; v03 4/29/19 Converted to method for mouse class.        ;;
;;---------------------------------------------------------;;
(defgeneric turn-right (obj)
  (:documentation "Turn right from current position"))

(defmethod turn-right ((obj mouse))
  (let ((dir (dir? obj)))
    (if (= dir 4)       ; if West
        (setf (dir? obj) 1)            ; wrap around to North
        (setf (dir? obj) (1+ dir)))))  ; else turn 90 degrees

;;---------------------- turn-left ------------------------;;
;; v01 4/20/19 Created                                     ;;
;; v03 4/29/19 Converted to method for mouse class.        ;;
;;---------------------------------------------------------;;
(defgeneric turn-left (obj)
  (:documentation "Turn left from current direction."))

(defmethod turn-left ((obj mouse))
  (let ((dir (dir? obj)))
    (if (= dir 1)
        (setf (dir? obj) 4)           ; wrap around to West
        (setf (dir? obj) (1- dir))))) ; else turn 90 degrees

;;----------------------- move-fwd ------------------------;;
;; v01 4/20/19 Created                                     ;;
;; v02 4/21/19 Use incf & decf macros.                     ;;
;; v03 4/29/19  Converted to method for mouse class.       ;;
;;---------------------------------------------------------;;
(defgeneric move-fwd (obj)
    (:documentation "Move the object one space in the direction it is facing.  
This function does not check to see that the space ahead is free - it assumes 
that has been done prior to calling it."))

(defmethod move-fwd ((obj mouse))
  (let ((dir (dir? obj)) ; 1 = North, 2 = East, 3 = South, 4 = West
        (x (x-pos? obj))
        (y (y-pos? obj)))
    (setf (last-x? obj) x)
    (setf (last-y? obj) y)
    (cond
      ((= dir 1) (setf (y-pos? obj) (decf y)))    ; North
      ((= dir 2) (setf (x-pos? obj) (incf x)))    ; East
      ((= dir 3) (setf (y-pos? obj) (incf y)))    ; South
      ((= dir 4) (setf (x-pos? obj) (decf x)))))) ; West 

;;------------------------ get-xy -------------------------;;
;; Return data from the maze structure list based on x-y   ;;
;; coordinates.  Return nil if out of bounds.              ;;
;; v01 4/19/19 Created.                                    ;;
;; v02 4/29/19 Uses global var *maze* instead of arg.      ;;
;;---------------------------------------------------------;;
(defun get-xy (x y)
  (cond
    ((> x *limit*) nil) ; If out of bounds..
    ((> y *limit*) nil) ; ..return nil.
    (t (nth (+ x (* *size* y)) *maze*)))) ; Else return value.

;;---------------------- print-maze -----------------------;;
;; From original maze-runner-10                            ;;
;; v01 4/19/19                                             ;;
;;---------------------------------------------------------;;
(defun print-maze ()
  (let ((amaze (copy-list *maze*)))
    (format t "~%")
    (dotimes (x *size*)
      (dotimes (y *size*)
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

;;------------------------ run-maze -----------------------;;
;; Main loop.                                              ;;
;; v0.1 4/29/19 Created.                                   ;;
;; v1.0 Tested w/ rh & lh mice running simultaneously.     ;;
;;---------------------------------------------------------;;
(defun run-maze ()
  (setf *maze* (copy-list maze1))
  (setf *done* nil)
  (set-pos rh-m1 2 0 0)  ; Set starting positions of all..
  (set-pos lh-m1 2 0 19) ; ..mice running in the maze.
  (do ((ctr 0 (+ ctr 1)))
       (*done* ctr)
    (explore rh-m1)
    (delay 20)
    (explore lh-m1)))

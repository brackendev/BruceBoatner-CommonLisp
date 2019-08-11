
;;--------------------- Sudoku Blaster ---------------------;;
;;                                                          ;;
;; Design: Bruce Boatner                                    ;;
;;                                                          ;;
;; Global Variables:                                        ;;
;;  1. *potentials*:  A list of 81 sub-lists, each with     ;;
;;     either a single pre-assigned number, or a list of    ;;
;;     potential numbers for each square, with superfluous  ;;
;;     numbers removed by the cull-potentials function.     ;;
;;                                                          ;;
;;  2. *cur-box-num*: An index to the current active        ;;
;;     *potentials* sub-list. The current square in the     ;;
;;      puzzle-matrix that we are currently testing.        ;;
;;                                                          ;;
;;  3. *num-idx-list*: A list of num-idx's pointing to      ;;
;;      the currently active numbers in each of the         ;;
;;      *potentials* sublists.                              ;;
;;                                                          ;;
;;  4. *matrix*: A working board for testing numbers and    ;;
;;      solutions for the puzzle.                           ;;
;;                                                          ;;
;;                       |----- *cur-box-num* pointer       ;;
;;  |--- *potentials*    |  |--- num-idx in *num-idx-list*  ;;
;;  v                    v  v                               ;;
;;  ((1 3 5) (2 3 8) (5) (2 4 7 8) (1 5 9) (4 8)...)        ;;
;; Box: 0       1     2      3        4      5  ...         ;;
;;                                                          ;;
;; Initialization:                                          ;;
;;   *done* = nil                                           ;;
;;   *potentials* = nil                                     ;;
;;   *matrix* = nil                                         ;;
;;   *cur-box-num* = 0                                      ;;
;;   *num-idx-list* = '(0 0 0 ...0)                         ;;
;;                                                          ;;
;; >Enter puzzle numbers from console or load saved puzzle. ;;
;;  Fill all empty spaces with potentials ranging (1..9).   ;;
;;  Create puzzle matrix showing numbers and blank squares. ;;
;;  Reduce *potentials* sublist lengths by cross-analysis.  ;;
;;                                                          ;;
;;  /--If <not backtrack>                                   ;;
;;  |    If <single digit>                                  ;;
;;  |      Increment *cur-box-num*                          ;;
;;  |    Else <not single digit>                            ;;
;;  |    /-If <no conflict>                                 ;;
;;  |    |   Poke number into *matrix* square               ;;
;;  |    | /-If <puzzle solved>                             ;;
;;  |    | |   Set *done* flag                              ;;
;;  |    | \-Else <not solved>                              ;;
;;  |    |     Increment *cur-box-num*                      ;;
;;  |    \-Else <conflict>                                  ;;
;;  |        Set *backtrack* flag                           ;;
;;  \--Else <backtrack>                                     ;;
;;       If <single digit>                                  ;;
;;         Decrement *cur-box-num*                          ;;
;;       Else <not single digit>                            ;;
;;         Un-poke number in *matrix* square                ;;
;;         Increment num-idx of *cur-box-num*               ;;
;;     /-If <num-idx > *limit*>                             ;;
;;     |   Set num-idx = 0                                  ;;
;;     |   Decrement *cur-box-num*                          ;;
;;     \-Else <idx not > limit>                             ;;
;;         Clear *backtrack* flag                           ;;
;;                                                          ;;
;; v01 5/11/19 Created.                                     ;;
;; v02 5/12/19 Pre-check position w/o placing number.       ;;
;; v03 5/13/19 Check for single digit and skip              ;;
;; V1.0 5/13/19 Solved "World's Hardest Sudoku" in 3/4 sec. ;;
;;----------------------------------------------------------;;
;;                       ~TO RUN~                           ;;
;; Select-all in *scratch* buffer and <Delete>              ;;
;; Copy this text into the *scratch* buffer.                ;;
;; <Save-As> filename.lisp                                  ;;
;; Compile-all: Ctrl-c Ctrl-k                               ;;
;; [You will get error messages on the first compile cycle] ;;
;; [This is due to defparameter puzzles at the end.]        ;;
;; Compile-all again: Ctrl-c Ctrl-k                         ;;
;; Go to your REPL buffer and type:                         ;;
;;   CL-USER> (sudoku)                                      ;;
;;                                                          ;;
;; Note: For a quick tutorial on emacs & IDE use, see my    ;;
;;       video on Common Lisp/Portacle Maze Runner.         ;;
;;----------------------------------------------------------;;
(defparameter *potentials* nil)  ; list of sublists of numbers
(defparameter *matrix* nil)      ; master puzzle-matrix list structure
(defparameter *input-list* nil)  ; List of puzzle numbers input by user

(defparameter *cur-box-num* 0)    ; currently working with this box
(defparameter *num-idx-list* nil) ; selected number entry for each box
(defparameter *backtrack* nil)    ; signals backtracking situation
(defparameter *done* nil)         ; puzzle complete

;;Fixed Constants
(defconstant +m-size+ 9)   ; Matrix x/y size 9x9
(defconstant +l-size+ 81)  ; Length of *potentials* & *matrix* lists
(defconstant +b-size+ 3)   ; Box size
(defconstant +num-box+ 9)  ; Number of boxes in a matrix

;; Initialize or re-initialize system before run.
(defun init-sys ()
  (setf *done* nil)
  (setf *backtrack* nil)
  (setf *cur-box-num* 0)
  (setf *num-idx-list* nil)
  (setf *potentials* nil) 
  (setf *matrix* nil)
  (dotimes (i +l-size+)
    (push 0 *num-idx-list*)))

(defun load-puzzle (p-list)
  (setf *potentials* (copy-list p-list)))

;;------------------------- sudoku -------------------------;;
;;                                                          ;;
;; 1. Input numbers from console or load saved puzzle.      ;;
;; 2. Fill 'empty' cells with a list of numbers (1..9)      ;;
;; 3. Simplify the matrix by culling conflicting numbers.   ;;
;; 4. Print the results.  Simple puzzles may already be     ;;
;;    solved at this point.                                 ;;
;;                                                          ;;
;; v01 4/17/19 Created.                                     ;;
;;----------------------------------------------------------;;

(defun sudoku ()
  (init-sys)
  (let ((cnt 0))
    (format t "~&Enter a number:")
    (format t "~&1: Manually enter puzzle")
    (format t "~&2: Load Easy/Medium Difficulty Puzzle")
    (format t "~&3: Load Medium/Difficult Puzzle")
    (format t "~&4: Load \"World's Hardest Sudoku Puzzle!\"")
    (format t "~&> ")
    (case (read)
      ((1) (input-fill-potentials))
      ((2) (load-puzzle puzzle1))
      ((3) (load-puzzle puzzle2))
      ((4) (load-puzzle hardest1))
      (otherwise (return-from sudoku nil)))
    (format-puzzle) ; before initial processing
    (print-puzzle)  ; show original puzzle
    (cull-potentials)
    (format-puzzle) ; after culling potentials
    (setf cnt (solve-it))
    (print-puzzle)  ; show solved puzzle
    (format t "~&Iterations: ~s" cnt)))

(defun poke (val)
  (setf (nth *cur-box-num* *matrix*) val))

(defun lookup-xy (idx)
  (let* ((y (floor (/ idx +m-size+)))
         (x (- idx (* y +m-size+))))
    (list x y)))

;;------------------------- solve-it ------------------------;;
;;                                                           ;;
;; v01 5/12/19 Created.                                      ;;
;; v02 5/13/19 Restructured to over-step single digits.      ;;
;; v03 5/13/19 Restructured as do-loop instead of recursive. ;;
;;-----------------------------------------------------------;;
(defun solve-it ()
  (do ((ctr 0 (incf ctr)))
      (*done* ctr)
;    (delay 50)
    (let ((sublist nil) ; ptr to *potentials* sublist for active box
          (sub-len 0)   ; length of active sublist
          (num-idx 0)   ; index to active number in *potentials* sublist
          (col 0)
          (row 0)
          (val 0))      ; number from *potentials* sublist to try in square
      (if (not *backtrack*)
          (progn ; Then <Not Backtrack>
            (setf sublist (nth *cur-box-num* *potentials*))
            (setf sub-len (length sublist))
            (setf num-idx (nth *cur-box-num* *num-idx-list*))
            (setf val (nth num-idx sublist))
; Debug     (format t "~%box: ~s subl: ~s num-idx: ~s val: ~s"
; Debug                *cur-box-num* sublist num-idx val)
            (if (= sub-len 1) ; Single digit?
              (setf *cur-box-num* (1+ *cur-box-num*)) ; Then <skip over>
              (progn ; Else <try next number>
                (setf col (car (lookup-xy *cur-box-num*)))  ; Convert index..
                (setf row (cadr (lookup-xy *cur-box-num*))) ; ..to row & col
                (if (is-pos-ok? col row val)
                   (progn ; Then <no conflict>
; Debug               (format t " OK")
                     (poke val)     ; write guess to puzzle
; Debug               (print-puzzle)
                     (if (= *cur-box-num* (1- +l-size+))
                         (setf *done* t) ; <Solved>
                         (setf *cur-box-num* (1+ *cur-box-num*)))) ; <Not solved>
                   (progn ; Else <conflict>
; Debug               (format t " Conflict")
                     (setf *backtrack* t))))))
          (progn ; Else <Backtrack>
; Debug      (format t  "~%Backtrack")
            (setf sub-len (length (nth *cur-box-num* *potentials*)))
            (if (= sub-len 1) ; Single digit?
              (setf *cur-box-num* (1- *cur-box-num*)) ; Then <skip over>
              (progn ; Else <not single digit>
                (poke '-) ; Clear the number at the current location
                (setf (nth *cur-box-num* *num-idx-list*)
                      (1+ (nth *cur-box-num* *num-idx-list*))) ; incr sublist num ptr
                (if (> (nth *cur-box-num* *num-idx-list*) (1- sub-len)) ; If end?
                    (progn ; Then <Exceeded sublist length>
                      (setf (nth *cur-box-num* *num-idx-list*) 0) ; reset sublist idx
                      (setf *cur-box-num* (1- *cur-box-num*))) ; decrement square ptr
                    (setf *backtrack* nil))))))))) ; Else <sublist not at end>

;;------------------------ is-pos-ok? ------------------------;;
;; Checks to see if a number placed in a given square will    ;;
;; conflict with any other number in its column, row & box.   ;;
;; These checks are made BEFORE the number is poked into the  ;;
;; *matrix*.                                                  ;;
;;                                                            ;;
;; v01 5/12/19 Created.                                       ;;
;; v02 5/12/19 Simplified by working directly on puzzle face. ;;
;;------------------------------------------------------------;;
(defun is-pos-ok? (col row val)
  (let ((t-f t))
    (cond 
      ((not (is-row-ok? row val)) (setf t-f nil))
      ((not (is-col-ok? col val)) (setf t-f nil))
      ((not (is-box-ok? col row val)) (setf t-f nil)))
  t-f))

; Walks the whole row
(defun is-row-ok? (row val)
  (let ((idx (* row +m-size+))) ; col 0 of row n
    (dotimes (n +m-size+)
      (if (equal (nth (+ idx n) *matrix*) val)
          (return-from is-row-ok? nil)))
    t))

(defun is-col-ok? (col val)
  (dotimes (n +m-size+)
    (if (equal (nth (+ col (* n +m-size+)) *matrix*) val)
        (return-from is-col-ok? nil)))
  t)

(defun is-box-ok? (col row val)
  (let* ((t-f t) ; default to T
         (h-box (floor (/ col +b-size+)))         ; first, find the box..
         (v-box (floor (/ row +b-size+)))         ; ..number that the..
         (box-num (+ h-box (* v-box +b-size+))) ; ..col & row are in.
         (box-idx (nth box-num '(0 3 6 27 30 33 54 57 60)))) ; start loc
     (dotimes (y +b-size+)
      (dotimes (x +b-size+)
        (if (equal (nth (+ (+ box-idx x) (* y +m-size+)) *matrix*) val)
            (setf t-f nil))))
    t-f)) ; if a match has been found anywhere, return nil

;;---------------------input-fill-potentials -------------------;;
;;                                                              ;;
;; Input:  *input-list*, numbers read from user at console.     ;;
;; Output: Fill matrix list with sublists of numbers:           ;;
;;         For pre-assigned puzzle number, enter the number.    ;;
;;         For empty spaces, fill with a list of numbers (1..9) ;;
;;                                                              ;;
;; v01 4/11/19 Created                                          ;;
;; v02 4/17/19 Removed print function to integrate better.      ;;
;; v03 5/11/19 Name changed input-puzzle to input-fill-matrix.  ;;
;; v04 5/12/19 Does not save to *save-matrix* here.  *matrix*   ;;
;;             will be simplified by cull-potentials first.     ;;
;; v05 5/12/19 Store data in *potentials*, not *matrix*.        ;;
;; v06 5/12/19 Changed name to input-fill-potentials            ;;
;;--------------------------------------------------------------;;
(defun input-fill-potentials ()
  (setf *potentials* '())   ; Establish global as empty list
  (get-nums)            ; Input starting numbers
  (dotimes (i +l-size+) ; for list length of 81
    (push '() *potentials*) ; Seed array with empty sublist for each entry
    (if (equal (car *input-list*) 0) ; Check for empty cells
        (setf (car *potentials*) '(1 2 3 4 5 6 7 8 9)) ; If empty, fill with potentials
        (push (car *input-list*) (car *potentials*))) ; Else input number
    (pop *input-list*)) ; Pop values off input list until done
  (setf *potentials* (reverse *potentials*))) ; Data is backwards in list

;;---------------------cull-potentials ---------------------;;
;;                                                          ;;
;; Iterate calls to cull-row, cull-col & cull-box           ;;
;; cull-row: look for occurrences of single digits in row   :;
;;           and remove them from other cell lists.         ;;
;; cull-col: same as cull-row, for one column               ;;
;; cull-box: cull a 3x3 box of rows and columns             ;;
;; v01 4/12/19 Created for sudoku-1                         ;;
;; v02 4/17/19 Changed function name from solve-puzzle      ;;
;;             to simplify-matrix.                          ;;
;; v03 5/11/19 Changed name to cull-potentials.             ;;
;;----------------------------------------------------------;;

(defun cull-potentials ()
  (dotimes (i +m-size+)
    (dotimes (j +m-size+) (cull-row j))
    (dotimes (k +m-size+) (cull-col k))
    (dotimes (l +num-box+) (cull-box l))))

;;------------------------ cull-row -------------------------;;
;; 4/16/19 Created                                           ;;
;; 5/12/19 Works with *potentials* rather than *matrix* now. ;;
;;-----------------------------------------------------------;;
(defun cull-row (x)        ; x = row selector
  (let ((singles-list '()) ; working list
        (new-val 0))       ; working digit     
    ; In this segment, we collect all the single digits in one row
    (dotimes (i +m-size+)  ; column selector
      ; Look for cells containing one digit only
      (if (= (length (nth (+ (* +m-size+ x) i) *potentials*)) 1)
          ; If a single digit is found, push it  onto the singles-list
          (push (car (nth (+ (* +m-size+ x) i) *potentials*)) singles-list)))
    ; All digits in the singles-list are deleted from the other row cells
    (dotimes (j (length singles-list))
      (setf new-val (car singles-list))
      (dotimes (i +m-size+)
        (if (> (length (nth (+ (* +m-size+ x) i) *potentials*)) 1)
            (setf (nth (+ (* +m-size+ x) i) *potentials*)
                  (remove new-val (nth (+ (* +m-size+ x) i) *potentials*))))) 
      (pop singles-list))))  ; toss last digit used

;;-------------------------- cull-col -----------------------;;
;; 4/16/19 Created                                           ;;
;; 5/12/19 Works with *potentials* rather than *matrix* now. ;;
;;-----------------------------------------------------------;;
(defun cull-col (x)        ; x = column selector
  (let ((singles-list '()) ; working list
        (new-val 0))       ; working digit     
    ; In this segment, we collect all the single digits in one column
    (dotimes (i +m-size+)  ; row selector
      ; Look for cells containing one digit only
      (if (= (length (nth (+ (* +m-size+ i) x) *potentials*)) 1)
          ; If a single digit is found, push it  onto the singles-list
          (push (car (nth (+ (* +m-size+ i) x) *potentials*)) singles-list)))
    ; All digits in the singles-list are deleted from the other row cells
    (dotimes (j (length singles-list))
      (setf new-val (car singles-list))
      (dotimes (i +m-size+)
        (if (> (length (nth (+ (* +m-size+ i) x) *potentials*)) 1)
            (setf (nth (+ (* +m-size+ i) x) *potentials*)
                  (remove new-val (nth (+ (* +m-size+ i) x) *potentials*))))) 
      (pop singles-list))))  ; toss last digit used

;;-------------------------- cull-box -----------------------;
;; 4/16/19 Created                                           ;;
;; 5/12/19 Works with *potentials* rather than *matrix* now. ;;
;;-----------------------------------------------------------;;
(defun cull-box (b)
  (let ((idx 0) ; Index to 1st element in box, set below
        (singles-list '())
        (new-val 0))
    ; In this segment, we collect all the single digits in the box
    (setf idx (nth b '(0 3 6 27 30 33 54 57 60)))
    (dotimes (r +b-size+)   ; Row counter
      (dotimes (c +b-size+) ; Col counter
                                        ; List the single digits in the box
        (if (= (length (nth (+ idx c) *potentials*)) 1)
            (push (car (nth (+ idx c) *potentials*)) singles-list)))
      (setf idx (+ idx 9))) ; Bump starting index to next row
    ; All digits in the singles-list are deleted from other cells in box
    (dotimes (j (length singles-list))
      (setf idx (nth b '(0 3 6 27 30 33 54 57 60))) ; Reset idx to box
      (setf new-val (car singles-list))
      (dotimes (r +b-size+)  ; Row counter
        (dotimes (c +b-size+) ; Col counter
          (if (> (length (nth (+ idx c) *potentials*)) 1)
              (setf (nth (+ idx c) *potentials*)
                    (remove new-val (nth (+ idx c) *potentials*)))))
        (setf idx (+ idx 9)))  ; Bump starting index to next row
      (pop singles-list))))  ; Toss last digit used

;;------------------------ get-nums -----------------------;;
;;                                                         ;;
;; Enter a puzzle manually, one square at a time.          ;;
;;                                                         ;;
;; Input:  User enters numbers from console, with ZEROS to ;;
;;         represent empty squares.                        ;;
;; Output: *input-list*                                    ;;
;;                                                         ;;
;; If a mistake has been made and the user wants to abort  ;;
;; the entry process, enter any non-valid (0..9) value.    ;;
;;                                                         ;;
;; v01 4/11/19 Created                                     ;;
;; v02 4/18/19 Remove copy to *save-input* - not used.     ;;
;; v03 4/18/19 Check for invalid/escape entry and exit.    ;;
;;---------------------------------------------------------;;
(defun get-nums ()
  (setf *input-list* '())
  (format t "Enter a '0' for empty cells ~&")
  (dotimes (r +m-size+)
    (dotimes (c +m-size+)
      (get-num (+ r 1) (+ c 1))
      (if (not (numberp (car *input-list*)))  (return-from get-nums nil))
      (if (> (car *input-list*) 9) (return-from get-nums nil))))
  (setf *input-list* (reverse *input-list*)))  ; Reverse list order

;;------------------------ get-num ------------------------;;
;; Prompt and input column and row entries to puzzle.      ;;
;; v01 4/11/19 Created                                     ;;
;;---------------------------------------------------------;;
(defun get-num (r c)
  (format t "Row ~S " r)
  (format t "Col ~S: " c)
  (let ((num (read)))
    (push  num *input-list*))) ; data pushed into list backwards

;;-------------------- format-puzzle ---------------------;;
;; Create the puzzle matrix that will be used for testing ;;
;; number combinations in the brute-force backtracking    ;;
;; approach.                                              ;;
;;                                                        ;;
;; v01 5/12/19 Created.                                   ;;
;;--------------------------------------------------------;;
(defun format-puzzle ()
  (setf *matrix* nil)
  (let ((sublist nil))
    (dotimes (i +l-size+)
      (setf sublist (nth i *potentials*))
      (if (> (length sublist) 1)
          (push '- *matrix*)
          (push (car sublist) *matrix*))))
  (setf *matrix* (reverse *matrix*)))

;;--------------------- print-puzzle -----------------------;;
;; Display puzzle matrix w/ gaps between boxes for clarity. ;;
;; Single elements in sublists are printed as existing      ;;
;; numbers in the puzzle, whereas sublists of more than one ;;
;; element are recognized as potential solutions for that   ;;
;; square.                                                  ;;
;;                                                          ;;
;; v01 4/13/19 Created.                                     ;;
;; v02 5/12/19 Simplified output.                           ;;
;;----------------------------------------------------------;;
(defun print-puzzle ()
  (format t "~%~%")
  (dotimes (r +m-size+)
    (if (member r '(3 6)) (format t "~& "))
    (format t "~&")
    (dotimes (c +m-size+)
      (format t "~s " (nth (+ (* r +m-size+) c) *matrix*))
      (if (member c '(2 5)) (format t " ")))))

;;------------------------ delay --------------------------;;
;; Recursive delay routine for demonstration and debug.    ;;
;; v01 4/21/19  Created for first 8-queens program.        ;;
;;---------------------------------------------------------::
(defun delay (n)
  (delay1 (* 1000000  n)))
(defun delay1 (n)
  (if (= n 0)
      nil
      (delay1 (- n 1))))

;;----------------------------------------------------------;;
;;                     Stored Puzzles                       ;;
;; Puzzles have been stored with blank spaces filled (1..9) ;;
;; This is the format output by 'input-fill-potentials'     ;;
;; function. To store a puzzle, enter it by hand with the   ;;
;; 'input-fill-potentials' function, with zeros for empty   ;;
;; boxes:                                                   ;;
;;   CL-USER> (input-fill-potentials)                       ;;
;; Copy the output from the REPL buffer into the source     ;;
;; defparameter statement, adding the single-quote and      ;;
;; appropriate parentheses.                                 ;;
;;----------------------------------------------------------;;

;; Simple - 8005 iterations
(defparameter puzzle1
 '((1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (7) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (5)
   (1 2 3 4 5 6 7 8 9) (8) (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (6) (4) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (3)
   (5) (6) (1 2 3 4 5 6 7 8 9) (7) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (5) (2) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (2) (1 2 3 4 5 6 7 8 9) (9)
   (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (4) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (6) (1 2 3 4 5 6 7 8 9) (9) (9) (7) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (2) (1) (4) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1)
   (1 2 3 4 5 6 7 8 9) (5) (1 2 3 4 5 6 7 8 9) (3) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (8) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
   (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)))

(defparameter puzzle2 ; Medium difficulty 69,975 iterations
  '((1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (7) (1 2 3 4 5 6 7 8 9) (9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (3) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (2)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (8)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (9) (6) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (5) (3) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (8)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (2) (6)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (4) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (3) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (4)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (7)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (7) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (3) (1 2 3 4 5 6 7 8 9)
     (1 2 3 4 5 6 7 8 9)))

(defparameter hardest1 ; "World's Hardest" 428,005 iterations
  '((8) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (3) (6) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (7) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (9)
    (1 2 3 4 5 6 7 8 9) (2) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (7) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (4) (5) (7) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (3) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (6) (8) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (8) (5) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (1) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (9)
    (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)
    (1 2 3 4 5 6 7 8 9) (4) (1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)))

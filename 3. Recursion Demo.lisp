;; "Understanding Recursive Functions"
;;  Bruce Boatner 2019
;;
;; This is a straight-forward function that will sum all of
;; the numbers in a list, regardless of its length, using
;; an iterative approach.
;;
;; CL-USER> (sigma-iter '(1 2 3 4 5 6 7 8 9))
;;
(defun sigma-iter (nums-list)
  (let ((sum 0))
    (dotimes (i (length nums-list))
      (setf sum (+ sum (nth i nums-list))) ; sum = sum + n
      (format t "~S " sum))                ; or, sum += n
        sum)) ; return sum                

;; Here's a very impractical example of how to sum the elements
;; of a number list, but it helps to explain how recursion works.
;;
;; CL-USER> (sigma-5 '(1 2 3 4 5))
;;
(defun sigma-5 (nums-list)  ; accepts 5-element lists only
  (+ (first nums-list)
     (sigma-4 (rest nums-list))))
(defun sigma-4 (nums-list)  ; accepts 4-element lists only
  (+ (first nums-list)
     (sigma-3 (rest nums-list))))
(defun sigma-3 (nums-list)  ; accepts 3-element lists only
  (+ (first nums-list)
     (sigma-2 (rest nums-list))))
(defun sigma-2 (nums-list)  ; accepts 2-element lists only
  (+ (first nums-list)
     (sigma-1 (rest nums-list))))
(defun sigma-1 (nums-list)  ; we could just return (first nums-list)..
  (+ (first nums-list)      ; ..here, but will continue for example.
     (sigma-0 (rest nums-list))))
(defun sigma-0 (nums-list)  ; this function receives an empty list..
  0)                        ; ..and returns a zero.

;; Here's how to accomplish exactly the same thing with only
;; a few lines of code, using recursion.
;;
;; CL-USER> (sigma-recur '(1 2 3 4 5 6 7 8 9))
;;
(defun sigma-recur (nums-list)
  (if (null nums-list)     ; termination test
      0                    ; termination value
      (+ (first nums-list) ; Else add in number and recur..
         (sigma-recur (rest nums-list))))) ; ..on shortened list

;; Here's a different approach to accomplish the same thing,
;; using tail recursion.
;;
;; CL-USER> (sigma-tail '(1 2 3 4 5 6 7 8 9))
;;
(defun sigma-tail (nums-list)     ; Accepts 1 argument and..
  (let ((sum 0))                  ; ..sets up to call sigma-tail1..
    (sigma-tail1 nums-list sum))) ; ..with 2 arguments.

(defun sigma-tail1 (nums-list sum); Recursive routine does all the work
  (format t "~&~s~3t~s " sum  nums-list)
  (if (null nums-list)            ; Last in chain returns the value
      (return-from sigma-tail1 sum))
  (setf sum (+ sum (first nums-list)))
  (sigma-tail1 (rest nums-list) sum))

;; This routine can be called directly with only 1 argument
;; and it will perform tail recursion using the second
;; [&optional] argument to pass the accumulated sum down the line.
;;
;; CL-USER> (sigma-tail2 '(1 2 3 4 5 6 7 8 9))
;;
(defun sigma-tail2 (nums-list &optional sum)
  (format t "~&~s~3t~s " sum  nums-list)
  (if (equal sum nil) (setf sum 0))
  (if (null nums-list)
        (return-from sigma-tail2 sum))
    (setf sum (+ sum (first nums-list)))
    (sigma-tail2 (rest nums-list) sum))
    

#!/usr/bin/env sbcl --script
(load "~/.sbclrc")
(ql:quickload "split-sequence")

(defparameter *day13* (read-file "d13.txt"))
(defparameter *my-arr-time* (parse-integer (car *day13*)))

(let ((bus-nums (mapcar
                 #'(lambda (x) (parse-integer x :junk-allowed t))
                 (split-sequence:split-sequence #\, (cadr *day13*)))))
  (defparameter *bus-numbers* (remove nil bus-nums))
  (defparameter *bus-positions*
    (loop for elem in bus-nums for i from 0
       if elem collect i)))

(defun waiting-time (my-arr-time bus-loop-duration)
  "Find amount of time you need to wait for the bus to take you to the airport."
  (- bus-loop-duration (mod my-arr-time bus-loop-duration)))

(defun argmin-helper (func &optional x y)
  "Return X if (FUNC X) < (FUNC Y) else return Y."
  (cond ((and x y)
         (let ((fx (funcall func x))
               (fy (funcall func y)))
           (if (> fx fy) y x)))
        (x x)))

(defun argmin (func &rest args)
  "Return the element of ARGS that minimizes FUNC."
  (cond ((<= (length args) 2) (apply #'argmin-helper func args))
        (t (destructuring-bind (x y &rest rest) args
             (apply #'argmin func (cons (argmin-helper func x y) rest))))))


;;;; Part 2

(defun egcd (a b)
  "Extended Euclidean algorithm. Return (r s t) where r is remainder and (s t)
  are such that s*a + t*b = r."
  ;; update rule looks like r_{i+1} = q*r_{i+1} - r_{i}
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r))) ; (new-r old-r)
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s))) ; (new-s old-s)
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u)))); (new-t old-t)
      ;; iterate until r_{I+1} = 0; return (r_{I} s_{I} t_{I})
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))
    ;; update quotient q
    (setf q (floor (cdr r) (car r)))))

(defun invmod (a m)
  "Return the inverse of a-inverse(mod m). Throw error if (a m) not co-prime."
  (multiple-value-bind (r s u) (egcd a m)
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not co-prime." a m))
    s))

(mapcar #'cdr (list '(1 . 2) '(3 . 4)))

(defun chinese-remainder (am)
  "am is a list of cons cells where the integers are the cars and the moduli are
  the cdrs."
  (loop for (a . m) in am
     with prod-m = (apply #'* (mapcar #'cdr am))
     and result = 0
     for yi = (/ prod-m m)
     for zi = (invmod yi m)
     finally (return (mod result prod-m))
     do (incf result (* a yi zi))))

;;;; final

(defun day13-part1 ()
  (let ((best-bus-number
         (apply
          #'argmin
          #'(lambda (bus-number)
              (waiting-time *my-arr-time* bus-number))
          *bus-numbers*)))
    (* best-bus-number (waiting-time *my-arr-time* best-bus-number))))


(defun day13-part2 ()
  "Goal is to find tstamp + offset_i \cong 0(mod bus-number_i). Compute this
   using the chinese remainder theorem with integers a given by the negative of
   the bus-positions, and the moduli m given as the bus numbers (which are all
   prime)."
  (chinese-remainder (pairlis (mapcar #'- *bus-positions*) *bus-numbers*)))

(verbose1
  (day13-part1)
  (day13-part2))

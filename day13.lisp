(ql:quickload 'split-sequence :silent t)

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

(defun bezout-coefficients (a b)
  "Given (a b) co-prime, compute (m n) such that 
                 a * m + b * n = d 
             where d = gcd(a, b)."
  (let ((old-r a)
        (new-r b)
        (old-s 1)
        (new-s 0)
        (old-t 0)
        (new-t 1)
        (quotient nil))
    (loop while (> new-r 0)
       do (progn
            (setf quotient (floor old-r new-r))
            (shiftf old-r new-r (- old-r (* quotient new-r)))
            (shiftf old-s new-s (- old-s (* quotient new-s)))
            (shiftf old-t new-t (- old-t (* quotient new-t))))
       finally (return (cons old-s old-t)))))


(defun crt2 (n0 n1 a0 a1)
  "Chinese Remainder Theorem for two co-prime numbers (n0 n1) with remainders (a0 a1). 
             Computes x such that:
                x \cong a0 (mod n0)
                x \cong a1 (mod n1)
            If n0 and n1 are not co-prime this function returns garbage."
  (let* ((bz (bezout-coefficients n0 n1))
         (m0 (car bz))
         (m1 (cdr bz)))
    (+ (* a0 m1 n1) (* a1 m0 n0))))

(defun chinese-remainder (numbers remainders)
  "Chinese Remainder Theorem for co-prime numbers (n0 n1 ... nk) with remainders (a0 a1 ... ak). 
             Computes x such that:
                x \cong a0 (mod n0)
                x \cong a1 (mod n1)
                   ...
                x \cong ak (mod nk)
            If ni are not co-prime this function returns garbage."
  (when (and numbers remainders
             (listp numbers) (listp remainders)
             (eql (length numbers) (length remainders)))
    (destructuring-bind (n0 n1 &rest ni) numbers
      (destructuring-bind (a0 a1 &rest ai) remainders
        (let* ((n01 (* n0 n1))
               (a01 (crt2 n0 n1 a0 a1)))
          (if (null ni)
              a01
              (chinese-remainder (cons n01 ni) (cons a01 ai))))))))

(defun silly-func (numbers remainders)
  "This works. The answer seems to be n0*n1*...*nk - x*(mod n0*n1...*nk), but
   unfortunately I don't know why!"
  (- (apply #'* numbers) (mod (chinese-remainder numbers remainders) (apply #'* numbers))))


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
  (silly-func *bus-numbers* *bus-positions*))

(verbose1
  (day13-part1)
  (day13-part2))

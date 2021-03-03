(ql:quickload 'split-sequence :silent t)

(defparameter *day13* (read-file "d13.txt"))
(defparameter *my-arr-time* (parse-integer (car *day13*)))
(defparameter *bus-numbers*
  (remove nil (mapcar
               #'(lambda (x)
                   (parse-integer x :junk-allowed t))
               (split-sequence:split-sequence #\, (cadr *day13*)))))

(defun waiting-time (my-arr-time bus-loop-duration)
  "Find amount of time you need to wait for the bus to take you to the airport."
  (- bus-loop-duration (mod my-arr-time bus-loop-duration)))

(defun argmin-helper (func &optional x y)
  (cond ((and x y)
         (let ((fx (funcall func x))
               (fy (funcall func y)))
           (if (> fx fy) y x)))
        (x x)))

(defun argmin (func &rest args)
  (cond ((<= (length args) 2) (apply #'argmin-helper func args))
        (t (destructuring-bind (x y &rest rest) args
             (apply #'argmin func (cons (argmin-helper func x y) rest))))))

(defun day13-part1 ()
  (let ((best-bus-number
         (apply
          #'argmin
          #'(lambda (bus-number)
              (waiting-time *my-arr-time* bus-number))
          *bus-numbers*)))
    (* best-bus-number (waiting-time *my-arr-time* best-bus-number))))


(defun day13-part2 ()
  nil)

(verbose1
  (day13-part1)
  (day13-part2))


;; 295 is too high

(load "my-utils.lisp")

;;;; def

(defparameter *day9* (mapcar #'read-from-string (read-file "d9.txt")))
(defparameter *preamble (subseq *day9* 0 25))
(defparameter *numbers* (make-array (length *day9*) :initial-contents *day9*))
(defparameter *cur* 25)
(defparameter *magic-number* 177777905)

;;;; part 1

(defun range (n &key (start 0))
  (when (and (numberp n) (>= n 0))
    (loop for i from start below n collect i)))

(defun validate-at-index (index)
  (when (>= (1- (length *numbers*)) index 25)
    (let ((prev (subseq *numbers* (- index 25) index))
          (cur (aref *numbers* index)))
      (loop for a across prev
         do (loop for b across prev
               if (and (/= b a) (= (+ b a) cur))
               do (return-from validate-at-index (+ b a)))))))

;;;; part 2

(defun too-small-p (value)
  (< value *magic-number*))

(defun too-large-p (value)
  (> value *magic-number*))

(defun get-value (index)
  (aref *numbers* index))

(defparameter *contig* nil)

(defun append-right (value) (setf *contig* (append *contig* (list value))))

(defun pop-left ()
  (let ((ret (car *contig*)))
    (setf *contig* (cdr *contig*))
    ret))

(defun get-sum ()
  (apply #'+ *contig*))

(defun search-for-contiguous-sum (&optional (left-idx 0) (right-idx 0))
  "Find a contiguous block of numbers whose sum is equals *MAGIC-NUMBER* (found in Part 1)."
    (cond ((too-small-p (get-sum))              ; if the sum is too small, 
           (append-right (get-value right-idx)) ; add the next number to the block
           (search-for-contiguous-sum left-idx (1+ right-idx))) ; & try again
          ((too-large-p (get-sum)) ; if the sum is too small,
           (pop-left)              ; remove the first number in the block
           (search-for-contiguous-sum (1+ left-idx) right-idx)) ; & try again
          ((>= (length *contig*) 2) ; >= 2 contiguous numbers with desired sum
           (+ (apply #'min *contig*) (apply #'max *contig*))) ; yay!
          (t (append-right (get-value right-idx))     ; keep trying!
             (search-for-contiguous-sum left-idx (1+ right-idx)))))

;;;; final

(defun day09-part1 ()
  (let ((indices (range (length *numbers*) :start 25)))
    (loop for index in indices
       while (validate-at-index index)
       finally (return (aref *numbers* index)))))

(defun day09-part2 ()
  (defparameter *contig* nil)
  (search-for-contiguous-sum))

(verbose1
  (day09-part1)
  (day09-part2))

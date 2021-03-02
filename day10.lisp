;; (load "my-utils.lisp")

(ql:quickload 'split-sequence :silent t)

(defparameter *day10* (mapcar #'parse-integer (read-file "d10.txt")))
(defparameter *device-joltage* (+ 3 (apply #'max *day10*)))


(defun sorted-joltages ()
  (sort (copy-seq *day10*) #'<))

(defparameter *sorted-joltages*
  (append (list 0) (sorted-joltages) (list *device-joltage*))
  "Includes outlet joltage and device joltage")

;;;; Part 1

(defun diff-func (seq)
  "Compute x1 - x0 where x0 is the first element of SEQ."
  (if (cadr seq)
      (- (cadr seq) (car seq))
      nil))

(defun diff (seq)
  "Computes the differences between consecutive elements in SEQ."
  (remove nil (maplist #'diff-func seq)))


;;;; Part 2

(defparameter *tribonacci-alist* nil)

(defun compute-tribonacci--alist (n)
  "Return n-th tribonacci number, computing recursively and using alist
memoization. Helper function for tribonacci--alist."
  (let ((t-n (+ (tribonacci--alist (- n 1))
                (tribonacci--alist (- n 2))
                (tribonacci--alist (- n 3)))))
    (push (cons n t-n) *tribonacci-alist*)
    t-n))

(defun tribonacci--alist (n)
  "Return the nth tribonacci number using memoization for speed-up."
  (cond
    ((or (not (numberp n)) (< n 0)) (format t "N must be a nonnegative integer.~%"))
    ((<= 0 n 1) 1) ((= n 2) 2)
    ((> n 2)
     (let ((t-n (cdr (assoc n *tribonacci-alist*))))
       (if t-n t-n
           (compute-tribonacci--alist n))))))


;;;; final

(defun day10-part1 ()
  (let ((diffs (diff *sorted-joltages*)))
    (* (count 1 diffs) (count 3 diffs))))

(defun day10-part2 ()
  "We can view the effect of removing adapters through the diffs. Namely, it's
equivalent to 'merging' contiguous 1s into 2s or 3s (and no higher, as per the
problem requirements). It so happens that finding the number of possible
mergings for a contiguous block of 1s of length n is given by the nth tribonacci
number. Simply multiply together the tribonacci number associated to each
contiguous block of 1s to obtain the result.

Examples are provided for contiguous 1 blocks of length 1 through 5:

1  [1]: (1)
2  [2]: (1 1)       (2)
3  [4]: (1 1 1)     (2 1)     (1 2)      (3)
4  [7]: (1 1 1 1)   (2 1 1)   (1 2 1)    (1 1 2)   (2 2)     
        (3 1)       (1 3)
5 [13]: (1 1 1 1 1) (2 1 1 1) (1 2 1 1)  (1 1 2 1) (1 1 1 2)
        (2 2 1)     (2 1 2)   (1 2 2)    (3 1 1)   (1 3 1)   
        (1 1 3)     (3 2)     (2 3)"
  (let* ((diffs (diff *sorted-joltages*))
         (contig-1s (split-sequence:split-sequence 3 diffs))
         (block-lengths (remove 0 (mapcar #'length contig-1s))))
    (apply #'* (mapcar #'tribonacci--alist block-lengths))))

(verbose1
  (day10-part1)
  (day10-part2))

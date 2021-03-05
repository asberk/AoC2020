(defparameter *day15* (list 1 12 0 20 8 16))
(defparameter *last-number* 16 "The last number that was spoken")
(defparameter *turn* 6 "The last turn number")
(defparameter *alist* nil)

(defun re-initialize ()
  (setf *alist* nil)
  (setf *last-number* 16)
  (setf *turn* 6)
  (loop for key in *day15* for value from 1
     do (push (cons key value) *alist*)))

(defun empty-alist ()
  (setf *alist* nil))

(defun cdr- (c1 c2)
  (if (and c1 c2)
      (- (cdr c1) (cdr c2))
      0))

(defun next-number ()
  (cdr- (assoc *last-number* *alist*)
        (assoc *last-number* (cdr *alist*))))

(defun update-alist ()
  (incf *turn*)
  (push (cons (next-number) *turn*) *alist*)
  (setf *last-number* (caar *alist*))
  (car *alist*))

;;;; Part 2

;;; alists were fine for part 1, but part 2 is too big and we must use a hash
;;; table afterall.

(defparameter *hashmap* (make-hash-table :size 10000))
(defparameter *cur* (cons 16 6) "current number")

(defun get-hashkeys ()
  (let ((keys nil))
    (maphash #'(lambda (k v) (push k keys)) *hashmap*)
    keys))

(defun empty-hashmap ()
  (let ((keys (get-hashkeys)))
    (dolist (k keys)
      (remhash k *hashmap*))))

(defmacro set-hash (key value)
  `(setf (gethash ,key *hashmap*) ,value))

(defun re-initialize2 ()
  (setf *cur* (cons 16 6))
  (setf *turn* 6)
  (empty-hashmap)
  (loop for key in *day15* for value from 1
     do (set-hash key value)))

(defun print-hashkeys ()
  (maphash #'(lambda (k v) (format t "k: ~a v: ~a~%" k v)) *hashmap*)
  (format t "----~%"))

(defun next-number ()
  (let ((last-seen (gethash (car *cur*) *hashmap*)))
    (if last-seen
        (- (cdr *cur*) last-seen)
        0)))

(defun update-hash ()
  (let ((tmp-cur *cur*))
    (setf *cur* (cons (next-number) (incf *turn*)))
    (set-hash (car tmp-cur) (cdr tmp-cur))))

;;;; final

(defun day15-part1 (&optional (end-num 2020))
  (re-initialize)
  (loop until (>= *turn* end-num)
     do (update-alist)
     finally (return (rassoc end-num *alist*))))

(defun day15-part2 ()
  (re-initialize2)
  (loop until (= (cdr *cur*) 30000000)
     do (update-hash)
     finally (return *cur*)))

(verbose1
  (day15-part1)
  (day15-part2))

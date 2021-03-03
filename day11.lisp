(defparameter *day11* (read-file "d11.txt"))
(defparameter *m* (length *day11*))
(defparameter *n* (length (car *day11*)))
(defparameter *seats* (make-array (list *m* *n*)))

(defun seating-char->num (elem)
  "Convert ELEM from numerical to character representation.
    0 <-> #\L
    1 <-> #\#
   -1 <-> #\. "
  (cond ((char= elem #\L) 0)
        ((char= elem #\#) 1)
        ((char= elem #\.) -1)
        (t nil)))

(defun seating-num->char (elem)
  "Convert ELEM from character to numerical representation.
    0 <-> #\L
    1 <-> #\#
   -1 <-> #\. "
  (cond ((= elem 0) #\L)
        ((= elem 1) #\#)
        ((= elem -1) #\.)
        (t nil)))

(defun print-seating (seating)
  "Print a SEATING that has been converted to 'num' type."
  (let ((converted (mapcar #'(lambda (row) (mapcar #'seating-num->char row)) seating)))
    (format t "~{~{~a~}~%~}~%" converted)))

(defun print-seats-arr ()
  (loop for i from 0 below *m*
     do (progn
          (loop for j from 0 below *n*
             do (format t "~a" (seating-num->char (aref *seats* i j))))
          (fresh-line)))
  (format t "~%~%"))

(defun initialize-seating-arr ()
  "Set the elements of *SEATS* to their initial values."
  (loop for row in *day11* for i from 0
     do (loop for elem across row for j from 0
           do (setf (aref *seats* i j) (seating-char->num elem)))))

;;;; Part 1


(defun get-elem (idx)
  "Return the element of *SEATS* given by IDX, which is a CONS cell with
   format (ROW . COLUMN)."
  (aref *seats* (car idx) (cdr idx)))

(defmacro test-elem (idx val)
  "Boilerplate for testing whether the IDX element of *SEATS* has value VAL."
  `(= (get-elem ,idx) ,val))

(defun empty-seat-p (idx) (test-elem idx 0))

(defun filled-seat-p (idx) (test-elem idx 1))

(defun seat-p (idx)
  "Predicate returning T if element IDX is a seat."
  (or (empty-seat-p idx) (filled-seat-p idx)))

(defun floor-p (idx) (test-elem idx -1))

(defun neighbour-relative-positions ()
  "Helper function returning the <=8 neighbouring indices about an index."
  (apply #'append
         (loop for i from -1 to 1
            collect (loop for j from -1 to 1
                       unless (and (= i 0) (= j 0))
                       collect (cons i j)))))

(defun idx-oob-p (idx)
  "Checks if index IDX is 'out of bounds'. IDX has the form (ROW . COLUMN),
   where 0 <= ROW < *m* and 0 <= COLUM < *n*."
  (or (or (< (car idx) 0) (>= (car idx) *m*))
      (or (< (cdr idx) 0) (>= (cdr idx) *n*))))

(defun idx+ (idx1 idx2)
  "Add two indices IDX1 and IDX2."
  (cons (+ (car idx1) (car idx2))
        (+ (cdr idx1) (cdr idx2))))

(defun get-neighbouring-indices (idx)
  "Get indices representing neighbours of IDX."
  (remove-if
   #'idx-oob-p
   (mapcar #'(lambda (ij) (idx+ idx ij))
           (neighbour-relative-positions))))

(defun get-neighbouring-values (idx)
  "Get values stored in neighbouring indices of IDX."
  (mapcar #'get-elem (get-neighbouring-indices idx)))

(defun num-neighbours-occupied (idx)
  "Count number of neighbours that are occupied seats."
  (count 1 (get-neighbouring-values idx)))

(defun set-elem (idx value)
  "Set element IDX of *SEATS* to VALUE."
  (setf (aref *seats* (car idx) (cdr idx)) value))

(defun update-seat (idx)
  "Apply seat update rule to element IDX of *SEATS*."
  (cond ((and (empty-seat-p idx) (= 0 (num-neighbours-occupied idx))) 1)
        ((and (filled-seat-p idx) (<= 4 (num-neighbours-occupied idx))) 0)
        (t (get-elem idx))))

(defun get-new-seating (&optional (seating-update-func #'update-seat))
  "Determine new seating by applying update rules simultaneously to all elements
   of *SEATS*."
  (loop for i from 0 below *m*
     collect (loop for j from 0 below *n*
                collect (funcall seating-update-func (cons i j)))))

(defun update-seating (new-seating)
  "Update seating arrangement by setting *SEATS* to be the result of
   GET-NEW-SEATING."
    (loop for row in new-seating
       for i from 0
       do (loop for elem in row
             for j from 0
             do (set-elem (cons i j) elem))))

(defun seating= (new-seating)
  "Compare NEW-SEATING to *SEATS*, returning T if all elements are equal."
  (loop for new-row in new-seating
     for i from 0
     do (loop for new-elem in new-row
           for j from 0
           do (when (/= new-elem (get-elem (cons i j))) (return-from seating= nil))))
  t)

(defun count-occupied ()
  "Count number of occupied seats in *SEATS*."
  (loop for i from 0 below *m*
     summing (loop for j from 0 below *n*
                counting (= (get-elem (cons i j)) 1))))


;;;; Part 2

(defun idx* (idx alpha)
  "Multiply IDX by a scalar ALPHA."
  (cons (* (car idx) alpha) (* (cdr idx) alpha)))

(defun relative-neighbour (idx slope r)
  "Get a 'neighbour' along SLOPE from IDX at l1-radius R."
  (idx+ idx (idx* slope r)))

(defun first-chair-along-ray (idx slope)
  "Return chair status (i.e. 0 or 1) for first chair found along a ray with
   slope SLOPE beginning from IDX (not including IDX). Return -1 if no chair
   found."
  (loop for r from 1
     for neighbour = (relative-neighbour idx slope r)
     until (idx-oob-p neighbour)
     if (seat-p neighbour)
     do (return-from first-chair-along-ray (get-elem neighbour))
     finally (return -1)))

(defun occupancy-in-lines-of-sight (idx)
  "Return list of chair occupancy along all <=8 lines of sight."
  (mapcar
   #'(lambda (slope) (first-chair-along-ray idx slope))
   (neighbour-relative-positions)))

(defun num-visible-occupancy (idx)
  "Count number of visibly occupied chairs along all <= 8 lines of sight."
  (count 1 (occupancy-in-lines-of-sight idx)))

(defun no-visible-occupancy-p (idx)
  (= 0 (num-visible-occupancy idx)))

(defun update-seat2 (idx)
  "Apply 2nd seat update rule to element IDX of *SEATS*."
  (cond ((and (empty-seat-p idx) (no-visible-occupancy-p idx)) 1)
        ((and (filled-seat-p idx) (<= 5 (num-visible-occupancy idx))) 0)
        (t (get-elem idx))))

(defun occupancy-los-array ()
  "For debugging. Make an array containing the number of visibly occupied chairs
from each index in *SEATS*."
  (loop for i from 0 below *m*
     collect (loop for j from 0 below *n*
                collect (num-visible-occupancy (cons i j)))))

(defun print-occupancy-los-array ()
  "For debugging. Print an array containing the number of visibly occupied
chairs from each index in *SEATS*."
  (let ((occupancy-arr (occupancy-los-array)))
    (format t "Occupancy-LOS-array:~%~{~{~a~}~%~}~%" occupancy-arr)))

;;;; final

(defun day11-part1 ()
  (initialize-seating-arr)
  (let ((new-seating (get-new-seating)))
    (loop until (seating= new-seating)
       do (progn
            (update-seating new-seating)
            (setf new-seating (get-new-seating)))
       finally (return (count-occupied)))))

(defun day11-part2 ()
  (initialize-seating-arr)
  (let ((new-seating (get-new-seating #'update-seat2)))
    (loop until (seating= new-seating)
       do (progn
            (update-seating new-seating)
            (setf new-seating (get-new-seating #'update-seat2)))
       finally (return (count-occupied)))))

(verbose1
  (day11-part1)
  (day11-part2))

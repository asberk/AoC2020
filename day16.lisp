(ql:quickload 'split-sequence :silent t)

(defparameter *day16* (read-file "d16.txt"))

(defparameter *ticket-field-rules*
  (loop for line in *day16*
     for line-length = (length line)
     for i from 0
     until (zerop line-length)
     if (not (zerop line-length))
     collect line))

(defun parse-ticket (ticket)
  (mapcar #'parse-integer (split-sequence:split-sequence #\, ticket)))

(defparameter *your-ticket*
  (parse-ticket
   (cadr
    (loop for line in *day16*
       for line-length = (length line)
       with read-flag = nil
       if (and read-flag (> line-length 0))
       collect line
       if (zerop line-length)
       do (setf read-flag (not read-flag))
       until (search "nearby ticket" line)))))

(defparameter *nearby-tickets*
  (mapcar #'parse-ticket
          (loop for line in *day16*
             with read-flag = nil
             if read-flag
             collect line
             if (search "nearby ticket" line)
             do (setf read-flag t))))


(defun to-cons (list)
  "Take a two element list '(A B) and return a dotted pair (A . B)."
  (when (eql 2 (length list))
    (setf (cdr list) (cadr list)))
  list)

(defun valid-range (ticket-field-rule)
  (let* ((range-string (cadr (split-sequence:split-sequence #\: ticket-field-rule)))
         (ranges-list (split-sequence:split-sequence #\Space range-string))
         (range-pairs
          (remove-if
           #'(lambda (x) (or (zerop (length x)) (not (find #\- x))))
           ranges-list)))
    (mapcar #'(lambda (x)
                (to-cons (mapcar #'parse-integer
                                 (split-sequence:split-sequence #\- x))))
            range-pairs)))

(defun dotted-pair-p (list)
  "Return T if list is a dotted pair CONS cell (A . B) and NIL otherwise."
  (and (listp list) (not (listp (cdr list)))))

(defun interval-union (i1 i2)
  "I1 and I2 are CONS cells with (CAR I1) < (CAR I2)."
  (cond ((null i1) i2)
        ((null i2) i1)
        ((<= (cdr i2) (cdr i1)) i1) ; i1lo <= i2lo <= i2up <= i1up
        ((<= (car i2) (cdr i1)) (cons (car i1) (cdr i2))) ; i1lo <= i2lo <= i1up <= i2up
        (t (list i1 i2))))

(defun unionize (i1 i2 &rest intervals)
  "Simplify a collection of intervals into a union (i.e. LIST) of
   non-overlapping intervals (i.e. CONS cells)."
  (let ((i12 (interval-union i1 i2)))
    (cond ((null intervals) i12)
          ((dotted-pair-p i12)
           (push i12 intervals)
           (apply #'unionize intervals))
          (t
           (push (cadr i12) intervals)
           (append (list (car i12)) (apply #'unionize intervals))))))

(defparameter *valid-range*
  (apply #'unionize
         (sort
          (apply #'append (mapcar #'valid-range *ticket-field-rules*))
          #'< :key #'car))
  "An interval (i.e. CONS cell) whose CAR is the lower end of the valid range,
  and CDR is the upper end of the valid range (as defined by part 1 of this
  challenge).")

(defun in-interval (value interval)
  "Return T if value is in the interval INTERVAL."
  (when (dotted-pair-p interval)
    (<= (car interval) value (cdr interval))))

(defun in-uoi (value uoi)
  "Return T if VALUE is in the union of intervals UOI."
  (cond ((dotted-pair-p uoi) (in-interval value uoi))
        ((listp uoi) (some #'(lambda (x) (in-interval value x)) uoi))
        (t (error "in-range: Unexpected condition reached; *valid-range* is not
        a list or dotted pair"))))

(defun in-range (value)
  (in-uoi value *valid-range*))


;;;; Part 2

(defparameter *valid-tickets*
  (remove-if
   #'(lambda (ticket)
       (notevery #'(lambda (value) (in-range value))
                 ticket))
   *nearby-tickets*))
(push *your-ticket* *valid-tickets*) ;; 191 tickets



(defun get-ticket-field-ranges ()
  (mapcar #'valid-range *ticket-field-rules*))

(defun possible-slots (ticket-field-range)
  "Compute the possible slots for the ticket-field-range. Return an array of T
   and NIL where an index has value T that index is a possible slot for the ticket
   field."
    (loop for ticket in *valid-tickets*
       with memb-arr = (make-array (length *your-ticket*) :initial-element t)
       do (loop
             for value in ticket
             for i from 0
             if (not (in-uoi value ticket-field-range))
             do (setf (aref memb-arr i) nil))
       finally (return memb-arr)))

(defun possible-slot-array ()
  "A column index j corresponds to a slot on the ticket; a row index i
  corresponds with a ticket field."
  (make-array '(20 20)
              :initial-contents
              (mapcar #'possible-slots (get-ticket-field-ranges))))

(defun possible-slot-counts ()
  "Returns a list where each element corresponds to the number of ticket fields
  that could be valid in that slot."
  (let ((arr (possible-slot-array)))
    (loop for j from 0 below 20
       collect (loop for i from 0 below 20
                  counting (aref arr i j)))))

(defun get-row-for-slot-count (n exclude)
  "Retrieve the row of (POSSIBLE-SLOT-ARRAY) where (POSSIBLE-SLOT-COUNTS) is
  N. Use EXCLUDE to exclude the TICKET-FIELD-INDEX values returned for lower
  values of N."
  (let* ((arr (possible-slot-array))
         (pslot-counts (possible-slot-counts))
         (search-column (loop
                           for c in pslot-counts
                           for i from 0
                           until (eql c n)
                           finally (return i))))
    (car
     (loop for i from 0 below 20
        if (and (aref arr i search-column) (not (find i exclude)))
        collect (list :ticket-field-index i :slot-number search-column)))))

(defun departure-slots ()
  "The departure ticket field indices are the first six. So we need the slot
  numbers corresponding to ticket-field-index 0 through 5."
  (loop for n from 1 to 20
     with exclude = nil
     for result = (get-row-for-slot-count n exclude)
     do (push (getf result :ticket-field-index) exclude)
     if (<= (getf result :ticket-field-index) 5)
     collect (getf result :slot-number)))

(defun get-your-ticket-value-at (slot)
  (nth slot *your-ticket*))


;;;; final

(defun day16-part1 ()
  (apply
   #'+
   (apply
    #'append
    (remove
     nil
     (mapcar
      #'(lambda (ticket)
          (remove-if
           #'(lambda (value)
               (in-range value))
           ticket))
      *nearby-tickets*)))))

(defun day16-part2 ()
  (apply #'* (mapcar #'get-your-ticket-value-at (departure-slots))))

(verbose1
  (day16-part1)
  (day16-part2))

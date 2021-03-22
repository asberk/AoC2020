#!/usr/bin/env sbcl --script
(load "my-utils.lisp")
(load "~/.sbclrc")
(ql:quickload "split-sequence")

(defun parse-input ()
  (labels ((load-data () (read-file "d20.txt"))
           (split-tiles ()
             (split-sequence:split-sequence-if
              #'(lambda (line)
                  (zerop (length line)))
              (load-data)))
           (parse-tile-name (line)
             (parse-integer (remove-if-not #'digit-char-p line)))
           (create-tile-array (lines)
             (make-array '(10 10) :initial-contents lines))
           (parse-tile (lines)
             (values (parse-tile-name (car lines))
                     (create-tile-array (cdr lines))))
           (hash-tile (tbl tile-lines)
             (when tile-lines
               (multiple-value-bind (tile-name tile-arr) (parse-tile tile-lines) 
                 (setf (gethash tile-name tbl) tile-arr)
                 tile-name)))
           (main ()
             (loop for tile-lines in (split-tiles)
                with tbl = (make-hash-table)
                with keys = nil
                if tile-lines
                do (push (hash-tile tbl tile-lines) keys)
                finally (return (values tbl keys)))))
    (main)))

(multiple-value-bind (tbl tile-keys) (parse-input)
  (defparameter *tbl* tbl)
  (defparameter *tile-keys* tile-keys))


(defun count-hashes-nesw (tile-name)
  (let* ((tile (gethash tile-name *tbl*))
         (east-west (loop for i from 0 to 9
                       counting (char= (aref tile i 0) #\#) into west
                       counting (char= (aref tile i 9) #\#) into east
                       finally (return (cons east west))))
         (north-south (loop for j from 0 to 9
                         counting (char= (aref tile 0 j) #\#) into north
                         counting (char= (aref tile 9 j) #\#) into south
                         finally (return (cons north south)))))
    (list (car north-south) (car east-west)
          (cdr north-south) (cdr east-west))))

(defun edges-match-p (edge1 edge2)
  (loop for c1 in edge1
     for c2 in edge2
     if (char/= c1 c2)
     do (return-from edges-match-p nil)
     finally (return t)))

(defun get-edge (tile-name side &optional (rev nil))
  (labels ((get-n-edge (tile)
             (loop for j from 0 to 9
             collect (aref tile 0 j)))
           (get-e-edge (tile)
             (loop for i from 0 to 9
                collect (aref tile i 9)))
           (get-s-edge (tile)
             (loop for j from 0 to 9
                collect (aref tile 9 j)))
           (get-w-edge (tile)
             (loop for i from 0 to 9
                  collect (aref tile i 0)))
           (get-edge-from (tile)
             (cond ((eq side :n) (get-n-edge tile))
                   ((eq side :e) (get-e-edge tile))
                   ((eq side :s) (get-s-edge tile))
                   ((eq side :w) (get-w-edge tile))))
           (reverse-maybe (edge) (if rev (reverse edge) edge)))
    (reverse-maybe (get-edge-from (gethash tile-name *tbl*)))))

;;;; If you're an edge tile you only match with two other tiles. Moreover, you
;;;; should only be able to match on one side if the problem is set up in the
;;;; easiest way. Let's proceed and hope for the best.

(defun test-tile-fit (tile-name1 tile-name2)
  (let ((directions (list :n :e :s :w)))
    (loop for d1 in directions
       for edge1 = (get-edge tile-name1 d1)
       do (loop for d2 in directions
             for edge2 = (get-edge tile-name2 d2)
             if (edges-match-p edge1 edge2)
             do (return-from test-tile-fit (list d1 d2 nil))
             if (edges-match-p (reverse edge1) edge2)
             do (return-from test-tile-fit (list d1 d2 :rev))))))

(defun e-match-p (tile-name1 tile-name2)
  (let ((directions (list :n :e :s :w))
        (edge1 (get-edge tile-name1 :e)))
    (loop for d in directions
       for edge2 = (get-edge tile-name2 d)
       if (edges-match-p edge1 edge2)
       do (return-from e-match-p (list tile-name2 d nil))
       if (edges-match-p edge1 (reverse edge2))
       do (return-from e-match-p (list tile-name2 d :rev)))))

(defun s-match-p (tile-name1 tile-name2)
  (let ((directions (list :n :e :s :w))
        (edge1 (get-edge tile-name1 :s)))
    (loop for d in directions
       for edge2 = (get-edge tile-name2 d)
       if (edges-match-p edge1 edge2)
       do (return-from s-match-p (list tile-name2 d nil))
       if (edges-match-p edge1 (reverse edge2))
       do (return-from s-match-p (list tile-name2 d :rev)))))

(defun brute-tile-fits ()
  (loop for key1 in *tile-keys*
   for key-tail on *tile-keys*
   append (loop for key2 in (cdr key-tail)
             for result = (test-tile-fit key1 key2)
             if result 
             collect (list (cons key1 key2) result))))

(defun brute-tile-fits-with-dups ()
  (loop for key1 in *tile-keys*
     append (loop for key2 in *tile-keys*
             for result = (test-tile-fit key1 key2)
             if (and  result (/= key1 key2))
             collect (list (cons key1 key2) result))))

(defparameter *tile-matches* (brute-tile-fits))
(defparameter *tile-matches-dup* (brute-tile-fits-with-dups))

(defun in-cons-cell (tile-name)
  #'(lambda (x) (or (= (caar x) tile-name)
                    (= (cdar x) tile-name))))

(defun get-matches-for (tile-name)
  (remove-if-not (in-cons-cell tile-name) *tile-matches*))

(defun get-matches-for-dup (tile-name)
  (remove-if-not #'(lambda (x) (= (caar x) tile-name)) *tile-matches-dup*))

(defun find-corners ()
  (loop for tile-name in *tile-keys*
     if (= (length (get-matches-for tile-name)) 2)
     collect tile-name))


;;;; Part 2

(defun dotted-pair-p (item)
  (and (listp item) (not (listp (cdr item)))))

(defun cons-pair (dotted-pair tile-name)
  (when (and (dotted-pair-p dotted-pair) (numberp tile-name))
    (cond ((= (car dotted-pair) tile-name) (cdr dotted-pair))
          ((= (cdr dotted-pair) tile-name) (car dotted-pair))
          (t nil))))

(defun get-tile-names-matching (tile-name)
  (let* ((match-pairs
          (mapcar #'car (get-matches-for tile-name))))
    (mapcar #'(lambda (dotted-pair) (cons-pair dotted-pair tile-name)) match-pairs)))

(defun find-mutual-matches (tile-name1 tile-name2)
  (let ((matches1 (get-tile-names-matching tile-name1))
        (matches2 (get-tile-names-matching tile-name2)))
    (intersection matches1 matches2)))

(defun flip-about-y-axis (the-array)
  "Flip THE-ARRAY about the y-axis."
  (destructuring-bind (m n &rest rest) (array-dimensions the-array)
    (when rest (error "rotate-90: 2D array required."))
    (make-array
     (list m n)
     :initial-contents
     (loop for i from 0 below m
        collect (loop for j from 0 below n
                   collect (aref the-array i (- (1- n) j)))))))

(defun flip-about-x-axis (the-array)
  "Flip THE-ARRAY about the x-axis."
  (destructuring-bind (m n &rest rest) (array-dimensions the-array)
    (when rest (error "rotate-90: 2D array required."))
    (make-array
     (list m n)
     :initial-contents
     (loop for i from 0 below m
        collect (loop for j from 0 below n
                   collect (aref the-array (- (1- m) i) j))))))

(defun rotate-90 (the-array)
  "Rotate a 2D array clockwise 90 degrees."
  (destructuring-bind (m n &rest rest) (array-dimensions the-array)
    (when rest (error "rotate-90: 2D array required."))
    (make-array
     (list m n)
     :initial-contents
     (loop for i from 0 below m
        collect (loop for j from 0 below n
                   collect (aref the-array (- (1- n) j) i))))))

(defun rotation-amount (dir1 dir2)
  "Return amount of clock-wise 90 degree rotations to perform on tile
  corresponding to DIR1. (DIR2 is the direction of the reference tile.) 
  DIR1 = current DIR2; = desired."
  (let* ((rot '((:n . 0) (:e . 1) (:s . 2) (:w . 3)))
        (s1 (cdr (assoc dir1 rot)))
        (s2 (cdr (assoc dir2 rot))))
    (mod (- s2 s1) 4)))

(defun rotator (arg times)
  (if (= times 0) arg (rotator (rotate-90 arg) (1- times))))

(defun arrange-helper (i j seen arr)
  (unless (and (= i 0) (= j 0))
    (let* ((i-nbr (if (= j 0) (1- i) i))
           (j-nbr (if (= j 0) j (1- j)))
           (tile-name (aref arr i-nbr j-nbr))
           (neighbours (remove-if #'(lambda (x) (find x seen))
                                  (get-tile-names-matching tile-name)))
           (match-p (if (= j 0) #'s-match-p #'e-match-p))
           (flip-func (if (= j 0) #'flip-about-y-axis #'flip-about-x-axis))
           (match-dir (if (= j 0) :n :w))
           (match-info
            (car (remove
                  nil
                  (mapcar #'(lambda (nbr) (funcall match-p tile-name nbr)) neighbours)))))
      (format t "~a: ~a ~a ... ~a~%" tile-name neighbours match-info match-p)
      (destructuring-bind (match-name dir rev) match-info
        (labels ((my-rotator (arg) (rotator arg (rotation-amount dir match-dir))))
          (push match-name seen)
          (setf (aref arr i j) match-name)
          (setf (gethash match-name *tbl*)
                (my-rotator (gethash match-name *tbl*)))
          (when rev
            (setf (gethash match-name *tbl*)
                  (funcall flip-func (gethash match-name *tbl*))))
          match-name)))))

(defun arrange-tiles ()
  (let* ((arr (make-array '(12 12) :initial-element nil))
         (seen nil))
    (setf (aref arr 0 0) 1873)
    (push 1873 seen)
    (loop for i from 0 below 12
       do (loop for j from 0 below 12
             do (let ((match-name  (arrange-helper i j seen arr)))
                  (when match-name
                    (push match-name seen)
                    (setf (aref arr i j) match-name)))))
    arr))

(defun make-full-image-array ()
  (let* ((square-width (- 10 2))
         (num-squares-per-side 12)
         (im-width (* square-width num-squares-per-side))
         (result (make-array (list im-width im-width) :initial-element #\.))
         (arrangement (arrange-tiles)))
    (loop for i0 from 0 below num-squares-per-side
       do (loop for j0 from 0 below num-squares-per-side
             do (let* ((square-name (aref arrangement i0 j0))
                       (square (gethash square-name *tbl*)))
                  (loop for i1 from 1 to square-width
                     do (loop for j1 from 1 to square-width
                           if (char= (aref square i1 j1) #\#)
                           do (setf (aref result
                                          (+ (* i0 square-width) (1- i1))
                                          (+ (* j0 square-width) (1- j1)))
                                    #\#))))))
    result))

(defparameter *final-image* (make-full-image-array))

(let ((sea-monster (list
                    "                  # "
                    "#    ##    ##    ###"
                    " #  #  #  #  #  #   ")))
  (defparameter *sea-monster*
    (make-array '(3 20)
                :initial-contents
                (loop for line in sea-monster
                   collect (loop for c across line
                              if (char= c #\#)
                              collect #\#
                              else collect nil)))))

(defun check-for-sea-monster-at (i0 j0)
  (loop for di from 0 below 3
     do (loop for dj from 0 below 20
           do (let ((image-char (aref *final-image* (+ i0 di) (+ j0 dj)))
                    (sm-char (aref *sea-monster* di dj)))
                (when (and sm-char (char= sm-char #\#))
                  (when (char/= sm-char image-char)
                    (return-from check-for-sea-monster-at nil))))))
  t)

(defun count-sea-monsters ()
  (destructuring-bind (m n) (array-dimensions *final-image*)
    (loop for i from 0 below (- m 3)
       summing (loop for j from 0 below (- n 20)
                  counting (check-for-sea-monster-at i j)))))

(defun find-final-image-orientation ()
  (let ((result nil))
    (loop for r from 0 to 4
       do (setf result (count-sea-monsters))
       if (and result (> result 0))
       do (return-from find-final-image-orientation result)
       else
       do (setf *final-image* (rotate-90 *final-image*)))
    (setf *final-image* (flip-about-y-axis *final-image*))
    (loop for r from 0 to 4
       do (setf result (count-sea-monsters))
       if (and result (> result 0))
       do (return-from find-final-image-orientation result)
       else
       do (setf *final-image* (rotate-90 *final-image*)))))

(defun mark-sea-monsters ()
  (destructuring-bind (m n) (array-dimensions *final-image*)
    (let ((arr *final-image*))
      (loop for i from 0 below (- m 3)
         do (loop for j from 0 below (- n 20)
               if (check-for-sea-monster-at i j)
               do (loop for di from 0 below 3
                     do (loop for dj from 0 below 20
                           do (let ((image-char (aref *final-image* (+ i di) (+ j dj)))
                                    (sm-char (aref *sea-monster* di dj)))
                                (when (and sm-char (char= sm-char #\#))
                                  (setf (aref arr (+ i di) (+ j dj)) #\*)))))))
      arr)))


;;;; Final


(defun day20-part1 ()
  (apply #'* (find-corners)))

(defun day20-part2 ()
  (let ((labelled-image (mark-sea-monsters)))
    (loop for i from 0 below 96
       summing (loop for j from 0 below 96
                  counting (char= #\# (aref labelled-image i j))))))

(verbose1
  (day20-part1)
  (day20-part2))

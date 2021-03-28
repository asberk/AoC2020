#!/usr/bin/env sbcl --script
(load "my-utils.lisp")

;;;; part 1

(defun fname (debug)
  (if debug "d24.test" "d24.txt"))

(defun direction-names ()
  (list :east
        :north-east
        :north-west
        :west
        :south-west
        :south-east))

(defun get-adjusted-vector (direction)
  "Return a vector for a neighbouring hexagon along DIRECTION in adjusted
   coordinates (i.e. y coord is scaled by 1/(sqrt 3))."
  (cond ((eql direction :east) (cons 1 0))
        ((eql direction :west) (cons -1 0))
        ((eql direction :north-east) (cons 1/2 1/2))
        ((eql direction :north-west) (cons -1/2 1/2))
        ((eql direction :south-west) (cons -1/2 -1/2))
        ((eql direction :south-east) (cons 1/2 -1/2))
        (t (error "direction not recognized: ~a" direction))))

(defun parse-input (&optional debug)
  (labels ((load-data () (read-file (fname debug)))
           (parse-direction (char-list)
             (cond ((and (find #\n char-list) (find #\e char-list)) :north-east)
                   ((and (find #\n char-list) (find #\w char-list)) :north-west)
                   ((and (find #\s char-list) (find #\e char-list)) :south-east)
                   ((and (find #\s char-list) (find #\w char-list)) :south-west)
                   ((find #\e char-list) :east)
                   ((find #\w char-list) :west)
                   (t (error "direction not recognized ~{~a~}" char-list))))
           (parse-line (line)
             (loop with buffer = nil for c across line
                do (push c buffer)
                if (or (char= c #\e) (char= c #\w))
                collect (parse-direction (nreverse buffer))
                and do (setf buffer nil)))
           (main () (mapcar #'parse-line (load-data))))
    (main)))

(defun cons+ (&rest vectors)
  (let ((cars (mapcar #'car vectors))
        (cdrs (mapcar #'cdr vectors)))
    (cons (apply #'+ cars) (apply #'+ cdrs))))

(defun get-coords (directions)
  (apply #'cons+ (mapcar #'get-adjusted-vector directions)))

(defun adjust-coord (coord &optional (tsfm #'/))
  (cons (car coord) (funcall tsfm (cdr coord) (sqrt 3))))

(defun print-hash (hash)
  (loop for key being the hash-keys of hash using (hash-value val)
     do (format t "~a: ~a~%" key (mod val 2))))

(defun count-black-tiles (hash)
  (loop for val being the hash-values of hash
     counting (and val (oddp val))))

(defun get-neighbours (coord)
  "COORD should be an adjusted coord; returned neighbours are in adjusted format."
  (mapcar #'(lambda (vector) (cons+ coord vector))
          (mapcar #'get-adjusted-vector (direction-names))))

(defun num-black-neighbours (coord hash)
  "COORD should be an adjusted coord."
  (let* ((neighbours (get-neighbours coord))
         (flips (mapcar #'(lambda (neighbour) (gethash neighbour hash)) neighbours)))
    (count-if #'(lambda (val) (and val (oddp val))) flips)))

(defun num-white-neighbours (coord hash)
  (- 6 (num-black-neighbours coord hash)))

(defun black-p (coord hash)
  (when (and coord hash)
    (let* ((val (gethash coord hash)))
      (and val (oddp val)))))

(defun white-p (coord hash)
  (when (and coord hash)
    (let* ((val (gethash coord hash)))
      (or (null val) (and val (evenp val))))))

(defun get-hash-keys (hash)
  (loop for key being the hash-keys of hash collect key))

(defun refresh-hash (hash)
  ;; need to take this approach because we can't add/modify/remove entries of a
  ;; hash table when iterating through it, except for the current entry pointed
  ;; to by the iteration.
  (let ((coords (get-hash-keys hash)))
    (loop for coord in coords
       if (black-p coord hash)
       do (let ((neighbours (get-neighbours coord)))
            (setf (gethash coord hash) 1)
            (loop for neighbour in neighbours
               for val = (gethash neighbour hash)
               if (null val) do (setf (gethash neighbour hash) 0)
               else do (setf (gethash neighbour hash) (mod val 2))))
       finally (return hash))))

(defun two-norm (coord1 coord2)
  (let ((x0 (car coord1))
        (x1 (cdr coord1))
        (y0 (car coord2))
        (y1 (cdr coord2)))
    (sqrt (+ (expt (- x0 y0) 2) (expt (- x1 y1) 2)))))

(defun pairwise-distances (hash)
  (loop for coord1 being the hash-keys of hash
     collect (loop for coord2 being the hash-keys of hash
                for dist = (two-norm coord1 coord2)
                if (> dist 0)
                collect (cons (list coord1 coord2) dist))
     into result
     finally (return (sort (apply #'append result) #'< :key #'cdr))))

(defun flip-black-p (coord hash)
  "Determine if black tile at COORD should be flipped to white."
  (when (black-p coord hash)
    (let ((num-black-neighbours (num-black-neighbours coord hash)))
      (or (= num-black-neighbours 0) (> num-black-neighbours 2)))))

(defun flip-white-p (coord hash)
  "Determine if white tile at COORD should be flipped to black."
  (when (white-p coord hash)
    (let ((num-black-neighbours (num-black-neighbours coord hash)))
      (= num-black-neighbours 2))))

(defun update-tiles (hash)
  (let ((h (make-hash-table :test #'equal))
        (hash (refresh-hash hash)))
    (loop for coord being the hash-keys of hash
       using (hash-value val)
       if (flip-black-p coord hash)
       do (setf (gethash coord h) 0)
       else if (flip-white-p coord hash)
       do (setf (gethash coord h) 1)
       else if (white-p coord hash)
       do (setf (gethash coord h) 0)
       else ;; if (black-p coord hash)
       do (setf (gethash coord h) 1)
       finally (return h))))

(defun run-simulation (days &optional debug verbose)
  (let ((hash (build-tile-flip-hash debug)))
    (when verbose (format t "Day 0: ~a~%" (count-black-tiles hash)))
    (when (zerop days)
      (return-from run-simulation hash))
    (loop for i from 1 to days
       with black-tile-count = 0
       do (setf hash (update-tiles hash))
       do (setf black-tile-count (count-black-tiles hash))
       do (when verbose (format t "Day ~a: ~a~%" i black-tile-count))
       finally (return hash))))

;;;; final

(defun day24-part1 (&optional debug)
  (let ((h (build-tile-flip-hash debug)))
    (count-black-tiles h)))

(defun day24-part2 ()
  (count-black-tiles (run-simulation 100)))

(verbose1
  (day24-part1)
  (day24-part2))

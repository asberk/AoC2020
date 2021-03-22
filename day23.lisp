(defparameter *cups* nil)
(defparameter *cups* nil)
(defparameter *current-cup* nil)
(defparameter *current-cup-index* 0)

(defparameter *number-of-cups* 9)

(defun initialize (&optional (debug nil))
  (let ((initial-data nil))
    (if debug
      (setf initial-data (list 3 8 9 1 2 5 4 6 7))
      (setf initial-data (list 6 1 4 7 5 2 8 3 9)))
    (setf *cups* (make-array *number-of-cups* :initial-element 0))
    (loop for i from 0 for datum in initial-data
       do (setf (aref *cups* i) datum))
    (loop for i from 9 below *number-of-cups*
         do (setf (aref *cups* i) (1+ i))))
  (setf *current-cup* (aref *cups* 0))
  (setf *current-cup-index* 0)
  *cups*)

(defun get-pick-up ()
  (loop for i from (1+ *current-cup-index*) below (+ 4 *current-cup-index*)
     collect  (aref *cups* (mod i *number-of-cups*))))

(defun get-destination (pick-up)
  (let ((destination (1- *current-cup*)))
    (loop if (= destination 0)
       do (setf destination *number-of-cups*)
       until (not (find destination pick-up))
       do (decf destination)
       finally (return destination))))

(defun new-cup-arrangement (pick-up destination)
  (loop
     with result = nil
     with i = 0

     for idx = (mod (+ *current-cup-index* i) *number-of-cups*)
     for cup = (aref *cups* idx)
     until (<= *number-of-cups* (length result))

     do (incf i)

     if (not (find cup pick-up))
     do (when (not (find cup result))
          (push cup result)
          (when (= cup destination)
            (setf result (append (reverse pick-up) result))))

     finally (return (nreverse result))))

(defun update-cups (round-number)
  (let* ((pick-up (get-pick-up))
         (destination (get-destination pick-up))
         (new-cups (new-cup-arrangement pick-up destination)))
    (when (> (length new-cups) *number-of-cups*)
      (error "Too many cups: ~a" *cups*))
    (format t "-- round ~a --~%" round-number)
    (format t "   cups: ~a~%" *cups*)
    (format t "   pick-up: ~a~%" pick-up)
    (format t "   destination: ~a~%" destination)
    (loop for i from 0 for datum in new-cups do (setf (aref *cups* i) datum))
    ;; (setf *cups* (make-array 9 :initial-contents new-cups))
    (setf *current-cup* (aref *cups* (mod (1+ (position *current-cup* *cups*)) *number-of-cups*)))
    (setf *current-cup-index* (position *current-cup* *cups*))
    *cups*))

(defun play-game (num-rounds &optional debug)
  (initialize debug)
  (loop for i from 1 to num-rounds
     do (update-cups i)
     finally (return *cups*)))

(defun create-answer ()
  (let* ((pos-1 (position 1 *cups*))
         (result (loop for i from 1 to 8
                    collect (aref *cups* (mod (+ pos-1 i) *number-of-cups*)))))
    (parse-integer (format nil "~{~a~}" result))))

(defun day23-part1 ()
  (play-game 100)
  (create-answer))

(defun day23-part2 ()
  (let ((*number-of-cups* (floor 1e6)))
    (play-game 100)
    (create-answer)))

(verbose1
  (day23-part1)
  (day23-part2))

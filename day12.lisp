#!/usr/bin/env sbcl --script
(defparameter *day12* (read-file "d12.txt"))
(defparameter *theta* 0 "Ship angle (initially aligned with x axis)")
(defparameter *x* 0 "Ship x position")
(defparameter *y* 0 "Ship y position")

(defun re-initialize-parms ()
  (setf *theta* 0)
  (setf *x* 0)
  (setf *y* 0))

(defun deg->rad (degrees)
  (* degrees (/ pi 180)))

(defun rotate (angle) (setf *theta* (- (mod (+ 180 (+ *theta* angle)) 360) 180)))
(defun rotate-right (angle) (rotate (- angle)))
(defun rotate-left (angle) (rotate angle))

(defun move-forward (distance)
  (incf *x* (* (cos (deg->rad *theta*)) distance))
  (incf *y* (* (sin (deg->rad *theta*)) distance)))

(defun move-north (distance) (incf *y* distance))
(defun move-south (distance) (incf *y* (- distance)))
(defun move-east (distance) (incf *x* distance))
(defun move-west (distance) (incf *x* (- distance)))

(defun search-for (letter string)
  (some #'(lambda (x) (char-equal x letter)) string))

(defun parse-action (line)
  (cond ((search-for #\r line) #'rotate-right)
        ((search-for #\l line) #'rotate-left)
        ((search-for #\n line) #'move-north)
        ((search-for #\s line) #'move-south)
        ((search-for #\e line) #'move-east)
        ((search-for #\w line) #'move-west)
        ((search-for #\f line) #'move-forward)
        (t nil)))


(defun parse-distance (line)
  (parse-integer (subseq line 1)))

(defun eval-line (line &optional (action-parser #'parse-action))
  (let ((action (funcall action-parser line)))
    (when action
      (funcall action (parse-distance line)))))

(defun manhattan-dist ()
  (floor (+ (abs *x*) (abs *y*))))


;;;; Part 2

(defparameter *wp-x* 10 "way-point x position")
(defparameter *wp-y* 1 "way-point y position")

(defun waypoint-radius ()
  (sqrt (+ (expt *wp-x* 2) (expt *wp-y* 2))))

(defun waypoint-angle ()
  (let ((principal (atan (/ *wp-y* *wp-x*))))
    (cond ((and (< *wp-x* 0) (< *wp-y* 0)) (- principal pi))
          ((< *wp-x* 0) (+ principal pi))
          (t principal))))

(defun rotate-waypoint-right (angle)
  (let ((radius (waypoint-radius))
        (theta (- (waypoint-angle) (deg->rad angle))))
    (setf *wp-x* (* radius (cos theta)))
    (setf *wp-y* (* radius (sin theta)))))

(defun rotate-waypoint-left (angle)
  (rotate-waypoint-right (- angle)))

(defun move-waypoint-north (distance) (incf *wp-y* distance))
(defun move-waypoint-south (distance) (incf *wp-y* (- distance)))
(defun move-waypoint-east (distance) (incf *wp-x* distance))
(defun move-waypoint-west (distance) (incf *wp-x* (- distance)))
(defun move-waypointward (distance)
  (incf *x* (* distance *wp-x*))
  (incf *y* (* distance *wp-y*)))

(defun parse-action2 (line)
  (cond ((search-for #\r line) #'rotate-waypoint-right)
        ((search-for #\l line) #'rotate-waypoint-left)
        ((search-for #\n line) #'move-waypoint-north)
        ((search-for #\s line) #'move-waypoint-south)
        ((search-for #\e line) #'move-waypoint-east)
        ((search-for #\w line) #'move-waypoint-west)
        ((search-for #\f line) #'move-waypointward)
        (t nil)))

;;;; final

(defun day12-part1 ()
  (loop for line in *day12*
     do (eval-line line)
     finally (return (manhattan-dist))))

(defun day12-part2 ()
  (re-initialize-parms)
  (loop for line in *day12*
     do (eval-line line #'parse-action2)
     finally (return (manhattan-dist))))

(verbose1
  (day12-part1)
  (day12-part2))

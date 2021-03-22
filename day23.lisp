#!/usr/bin/env sbcl --script
;;;; Parts 1 and 2
(load "my-utils.lisp")
(defparameter *cups* nil)

(defun initial-data (&optional debug)
  (if debug
      (list 3 8 9 1 2 5 4 6 7)
      (list 6 1 4 7 5 2 8 3 9)))

(defun make-cup-array (num-cups &optional debug)
  "Create an array where each index is a cup and each element is the next cup."
  (setf *cups* (make-array (1+ num-cups)))
  (let ((initial-data (initial-data debug))
        (remaining-data (loop for i from 9 below num-cups collect (1+ i))))
    (loop with cups = (append initial-data remaining-data)
       for cup in cups
       for next-cup in (cdr cups)
       do (setf (aref *cups* cup) next-cup)
       finally (setf (aref *cups* (car (last cups))) (car cups))))
  *cups*)

(defun get-pick-up (current-cup)
  (loop for i from 0 below 3
     with idx = current-cup
     for cup = (aref *cups* idx) 
     collect cup
     do (setf idx cup)))

(defun get-destination (current-cup num-cups pick-up)
  (let ((destination (1- current-cup)))
    (loop if (< destination 1)
       do (setf destination num-cups)
       until (not (find destination pick-up))
       do (decf destination)
       finally (return destination))))

(defun move (current-cup)
  (let* ((pick-up (get-pick-up current-cup))
         (next-cup-idx (nth 2 pick-up))
         (next-cup (aref *cups* next-cup-idx))
         (num-cups (1- (array-dimension *cups* 0)))
         (destination (get-destination current-cup num-cups pick-up)))
    (setf (aref *cups* current-cup) next-cup)
    (setf (aref *cups* next-cup-idx) (aref *cups* destination))
    (setf (aref *cups* destination) (car pick-up))
    next-cup))

(defun cup-code (cups idx)
  (loop with i = idx
     for cup = (aref cups i)
     until (= cup 1)
     collect cup into result
     do (setf i cup)
     finally (return (parse-integer (format nil "~{~a~}" result)))))

(defun crab-product (cups idx)
  (let* ((cup1 (aref cups idx))
         (cup2 (aref cups cup1)))
    (* cup1 cup2)))

(defun play-game (num-cups num-rounds &optional answer-type debug)
  (make-cup-array num-cups debug)
  (loop for i from 0 below num-rounds
     with current-cup = (car (initial-data debug))
     do (setf current-cup (move current-cup))
     finally (return (if (eql answer-type :cup-code)
                         (cup-code *cups* 1)
                         (crab-product *cups* 1)))))

;;;; final

(defun day23-part1 ()
  (play-game 9 100 :cup-code))

(defun day23-part2 ()
  (play-game (floor 1e6) (floor 1e7)))

(verbose1
  (day23-part1)
  (day23-part2))

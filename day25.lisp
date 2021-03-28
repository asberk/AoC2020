#!/usr/bin/env sbcl --script
(load "my-utils.lisp")

(defparameter *divisor* 20201227)

(defun parse-input ()
  "Load card's public key and door's public key (returned as a list)."
  (let ((data (read-file "d25.txt")))
    (mapcar #'parse-integer data)))

(defun transform (value &optional (subject-number 7))
  (rem (* subject-number value) *divisor*))

(defun get-public-key (loop-size &optional (subject-number 7))
  (loop for i from 1 to loop-size
     with value = 1
     do (setf value (transform value subject-number))
     finally (return value)))

(defun loop-sizes ()
  ;; There's a faster way to do this where you determine the offset from one
  ;; full pass before modding, but Lisp is fast enough that I don't need to
  ;; worry too much.
  (destructuring-bind (card-pub door-pub) (parse-input)
    (loop named loopy
       for i from 1 to 100000000
       with value = 1
       with loop-sizes = nil
       do (setf value (transform value))
       if (= card-pub value)
       do (push (cons :card i) loop-sizes)
       if (= door-pub value)
       do (push (cons :door i) loop-sizes)
       if (= (length loop-sizes) 2)
       do (return-from loopy loop-sizes)
       finally (return loop-sizes))))

(let ((loop-sizes (loop-sizes)))
  (defparameter *card-loop-size* (cdr (assoc :card loop-sizes)))
  (defparameter *door-loop-size* (cdr (assoc :door loop-sizes))))

(defun encryption-key ()
  (destructuring-bind (card-pub door-pub) (parse-input)
    (let ((e-key-1 (get-public-key *door-loop-size* card-pub))
          (e-key-2 (get-public-key *card-loop-size* door-pub)))
      (if (= e-key-1 e-key-2)
          e-key-1
          (error "Keys do not match: ~a /= ~a" e-key-1 e-key-2)))))

(defun day25-part1 ()
  (encryption-key))

(verbose1
  (day25-part1))

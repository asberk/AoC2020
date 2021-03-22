#!/usr/bin/env sbcl --script
(load "my-utils.lisp")

(defparameter *test-cases* (list "FBFBBFFRLR" "BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"))
(defparameter *test-seat-ids* (list 357 567 119 820))


(defun front-or-back-p (c)
  (or (char-equal c #\b) (char-equal c #\f)))

(defun front-or-back (c)
  (cond ((char-equal c #\b) 1)
        ((char-equal c #\f) 0)
        (t nil)))

(defun left-or-right (c)
  (cond ((char-equal c #\r) 1)
        ((char-equal c #\l) 0)
        (t nil)))

(defun get-bit-from-character (c)
  (or (front-or-back c) (left-or-right c)))

(defun get-bit-string (string)
  (loop for c across string collect (get-bit-from-character c)))

(defun get-integer-from-bit-string (bit-string)
  (apply #'+ (loop for b in (reverse bit-string)
                for j from 0
                if (= b 1)
                collect (expt 2 j))))

(defun left-right-substr (string)
  (remove-if #'front-or-back-p string))

(defun front-back-substr (string)
  (remove-if (complement #'front-or-back-p) string))

(defun get-row-from-string (string)
  (get-integer-from-bit-string (get-bit-string (front-back-substr string))))

(defun get-column-from-string (string)
  (get-integer-from-bit-string (get-bit-string (left-right-substr string))))

(defun seat-id (string)
  (+ (* 8 (get-row-from-string string)) (get-column-from-string string)))

(defun check-test-cases ()
  (when (notevery #'= (mapcar #'seat-id *test-cases*) *test-seat-ids*)
    (format t "Error: test cases did not pass")))

(check-test-cases)

(defun day5-part1 (&optional (data (read-file "d5.txt")))
  (loop for line in data maximizing (seat-id line)))

(defun day5-part2 ()
  (let* ((day5 (read-file "d5.txt"))
         (parsed (mapcar #'seat-id day5))
         (max-id (apply #'max parsed))
         (found (loop for i from 1 to max-id
                   if (and (not (member i parsed))
                           (member (1- i) parsed)
                           (member (1+ i) parsed))
                   collect i)))
    (when (= (length found) 1)
      (car found))))

(verbose1
  (day5-part1)
  (day5-part2))

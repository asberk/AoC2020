#!/usr/bin/env sbcl --script
(load "my-utils.lisp")

(defun day1-part1 ()
  ""
  (let ((d1-input (read-file "d1.txt" #'read)))
    (loop named outer-loop
       for a in d1-input
       do (loop for b in d1-input
             when (= (+ a b) 2020)
             do (return-from outer-loop (* a b))))))


(defun day1-part1-method2 ()
  (block loops
    (let ((d1-input (read-file "d1.txt" #'read)))
      (dolist (a d1-input)
        (dolist (b d1-input)
          (when (= (+ a b) 2020)
            (return-from loops (* a b))))))))

(defun day1-part2 ()
  (block loops
    (let ((d1-input (read-file "d1.txt" #'read)))
      (dolist (a d1-input)
        (dolist (b d1-input)
          (dolist (c d1-input)
            (when (= (+ a b c) 2020)
              (return-from loops (* a b c)))))))))

(verbose1
  (day1-part1)
  (day1-part1-method2)
  (day1-part2))

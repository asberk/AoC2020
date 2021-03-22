#!/usr/bin/env sbcl --script
(load "my-utils.lisp")

(defun get-char-at-pos (line pos)
  (subseq line pos (1+ pos)))

(defun tree-p (line pos)
  (string= "#" (get-char-at-pos line pos)))

(defun build-col-positions (nrows ncols slope)
  "Builds column positions for ski slope. SLOPE should be a cons cell
   corresponding with (right . down)."
  (loop for i from 0 to (ceiling (/ (1- nrows) (cdr slope)))
     collect (mod (* i (car slope)) ncols)))

(defun row-subset (seq every-other)
  (cond ((< every-other 1) nil)
        ((= every-other 1) seq)
        (t (loop for x in seq
              for i from 0
              if (= (mod i every-other) 0)
              collect x))))

(defun find-num-trees (data slope)
  "Find the number of trees you'd run into. SLOPE is a cons cell with
   entries (RIGHT . DOWN)."
  (let* ((nr (length data))        ; nrows
         (nc (length (car data)))) ; ncols
    (count t (mapcar #'tree-p (row-subset data (cdr slope))
                     (build-col-positions nr nc slope)))))

(defun day3-part1 ()
  (find-num-trees (read-file "d3.txt") (cons 3 1)))

(defun day3-part2 ()
  (let ((day3 (read-file "d3.txt"))
        (slopes (list (cons 1 1) (cons 3 1) (cons 5 1) (cons 7 1) (cons 1 2))))
    (apply #'* (loop for slope in slopes collect (find-num-trees day3 slope)))))

(verbose1
  (day3-part1)
  (day3-part2))

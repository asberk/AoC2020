#!/usr/bin/env sbcl --script
(load "my-utils.lisp")
(load "~/.sbclrc")
(ql:quickload "split-sequence")

(defun parse-instruction (line)
  (destructuring-bind (inst val) (split-sequence:split-sequence #\Space line)
    (cons (read-from-string inst) (parse-integer val))))

(defparameter *day8* (read-file "d8.txt"))
(defparameter *accumulator* 0)
(defparameter *pointer* 0)
(defparameter *instructions*
  (make-array (length *day8*) :initial-contents (mapcar #'parse-instruction *day8*)))
(defparameter *running* t)
(defparameter *visited* nil)

(defun jmp (&optional (val 1)) (incf *pointer* val))

(defun acc (val)
  (jmp)
  (incf *accumulator* val))

(defun nop (&optional val) (jmp))

(defun get-inst ()
  (cond ((<= 0 *pointer* (1- (length *instructions*)))
         (aref *instructions* *pointer*))
        (t
         (format t "pointer out of bounds with value ~a~%" *pointer*)
         (format t "accumulator has value: ~a.~%" *accumulator*)
         nil)))

(defun check-visited ()
  (cond ((find *pointer* *visited*)
         (setf *running* nil)
         *accumulator*)
        (*running*
         (push *pointer* *visited*)
         nil)))

(defun eval-instruction (inst)
  "Evaluate INST if non-nil."
  (let ((output-val (check-visited)))
    (cond (output-val output-val)
          (inst (funcall (car inst) (cdr inst)))
          (t nil))))

(defun reset-runtime ()
  "Reset runtime variables to initial states."
  (setf *accumulator* 0)
  (setf *pointer* 0)
  (setf *visited* nil)
  (setf *running* t))

;;;; part 2

;;; it stands to reason that we only need to change one of the entries in
;;; *visited* with jmp <--> nop. That leaves <=211 possibilities
(defun get-swap-candidates ()
  "Build list of candidates for part 2 swapping."
  (day08-part1)
  (remove-if-not #'(lambda (x) (or (eql x 'jmp) (eql x 'nop)))
                 *visited* :key #'get-inst-type))

(defun get-inst-symbol (pointer-val)
  "Gets the instruction symbol associated with POINTER-VAL"
  (when (<= 0 pointer-val (1- (length *instructions*)))
    (car (aref *instructions* pointer-val))))

(defun set-inst-symbol (pointer-val new-inst)
  "Set symbol at POINTER-VAL to NEW-INST."
  (when (or (eql 'jmp new-inst) (eql 'nop new-inst))
    (setf (aref *instructions* pointer-val)
          (cons new-inst (cdr (aref *instructions* pointer-val))))))

(defun swap-inst-symbol (pointer-val)
  "Swap 'JMP for 'NOP and vice versa at POINTER-VAL (or return NIL)."
  (let ((inst-func (get-inst-symbol pointer-val)))
    (cond ((eql inst-func 'jmp) (set-inst-symbol pointer-val 'nop))
          ((eql inst-func 'nop) (set-inst-symbol pointer-val 'jmp))
          (t nil))))

(defun eval-if-valid ()
  "Eval instruction associated with *pointer* only if *pointer* is
valid. Otherwise, stop runtime and return *accumulator* value."
  (cond ((<= 0 *pointer* (1- (length *instructions*)))
         (eval-instruction (get-inst)))
        (t (setf *running* nil)
           *accumulator*)))

(defun execute-swapped-runtime (swap-index)
  (reset-runtime)
  (swap-inst-symbol swap-index)
  (let ((result
         (loop until (not *running*)
            for x = (eval2-if-valid)
            finally (return x))))
    (swap-inst-symbol swap-index) ;; swap back!
    result))

;;;; final

(defun day08-part1 ()
  (reset-runtime)
  (loop until (not *running*)
     for x = (eval-instruction (get-inst))
     finally (return x)))


(defun day08-part2 ()
  (loop for swap-idx in (get-swap-candidates)
     for x = (execute-swapped-runtime swap-idx)
     until x
     finally (return x)))

(verbose1
  (day08-part1)
  (day08-part2))

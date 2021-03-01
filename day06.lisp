(load "my-utils.lisp")

(defparameter *day6* (read-file "d6.txt") "Day 6 puzzle input")
(defvar *lower*
  (loop with a = (char-code #\a)
        for i below 26
     collect (code-char (+ a i))))

(ql:quickload 'split-sequence :silent t)

(defun split-input-by-groups (data)
  "Return a list of lists from DATA, assuming each entry is a string."
  (split-sequence:split-sequence
   0 data :key #'(lambda (x) (length x))))

(defun output-formatter (string)
  (when string
    (sort (copy-seq (remove-duplicates string)) #'string<)))

(defun parse-group-any (group)
  "Return GROUP as all found questions with duplicates removed."
  (output-formatter (apply #'concatenate 'string group)))

(defun parse-group-every (group)
  "Return GROUP as all questions found for every GROUP member."
  (output-formatter
   (cond
     ((= (length group) 1) (car group))
     ((< (length group) 0) nil)
     (t (format nil "~{~A~}"
                (loop for letter across (car group)
                   if (every #'(lambda (x) (position letter x))
                             (cdr group))
                   collect letter))))))

(defun num-questions (data group-parser)
  (apply #'+ (mapcar
              (lambda (x) (length (funcall group-parser x)))
              (split-input-by-groups data))))

(defun day6-part1 ()
  (num-questions *day6* #'parse-group-any))

(defun day6-part2 ()
  (num-questions *day6* #'parse-group-every))

(verbose1
  (day6-part1)
  (day6-part2))

(load "my-utils.lisp")
(load "~/.sbclrc")
(ql:quickload "split-sequence")

(defun parse-rule (rule)
  (loop
     for pos = (position #\| rule) for subrule = (subseq rule 0 pos)
     while (> (length subrule) 0)
     collect (loop for letter across (concatenate 'string subrule " ")
                with prev-nan = 0 for i from 0
                if (and (not (digit-char-p letter)) (< prev-nan i))
                collect (parse-integer (subseq rule prev-nan i))
                if (not (digit-char-p letter))
                do (setf prev-nan (+ i 1)))
     do(setf rule (subseq rule (if pos (+ pos 1) (length rule))))))

(defun parse-input ()
  (labels ((load-data ()
             (split-sequence:split-sequence-if
              #'(lambda (line) (zerop (length line)))
              (read-file "d19.txt")))
           (parse-rules (rules)
             (loop for line in rules for div = (position #\: line)
                while (> (length line) 0)
                collect (cons
                         (parse-integer (subseq line 0 div))
                         (if (position #\" line)
                             (list (char line (+ div 3)))
                             (parse-rule (subseq line (+ div 2))))))))
    (destructuring-bind (rules texts) (load-data)
      (defparameter *rules* (parse-rules rules))
      texts)))

(defun check (id texts)
  (when texts
    (let ((rule (cdr (assoc id *rules*))))
      (if (typep (car rule) 'character)
          (map 'list #'(lambda (text) (subseq text 1))
               (remove-if-not
                #'(lambda (text)
                    (and
                     (> (length text) 0)
                     (char= (car rule) (char text 0))))
                texts))
          (reduce 'union
                  (map 'list #'(lambda (subrule)
                                 (reduce #'check (reverse subrule)
                                         :from-end t :initial-value texts))
                       rule))))))

(defun day19part1 ()
  (loop for line in (parse-input) while line
     count (loop for possibility in (check 0 (list line))
              if(= 0 (length possibility)) return t
              finally (return nil))))



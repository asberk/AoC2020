(ql:quickload 'split-sequence :silent t)

(defparameter *day19* (read-file "d19.txt"))

(defun zero-length-p (sequence)
  (zerop (length sequence)))

(defun str-base-rule-p (rule-text)
  (some #'(lambda (x) (find x rule-text)) (list #\a #\b)))

(defun base-rule-p (rule)
  (when (and (characterp (cdr rule))
             (or (char= (cdr rule) #\a)
                 (char= (cdr rule) #\b)))
    (cdr rule)))

(defun str-disjunction-p (rule-text)
  (find #\| rule-text))

(defun parse-input ()
  (labels ((load-data ()
             (destructuring-bind (rules messages)
                 (split-sequence:split-sequence-if
                  #'zero-length-p
                  (read-file "d19.txt"))
               (values rules messages)))
           (parse-rule-text (rule-text)
             (cond ((str-base-rule-p rule-text) (str-base-rule-p rule-text))
                   ((str-disjunction-p rule-text)
                    (mapcar #'(lambda (x) (if (string= x "|") #\| (parse-integer x)))
                            (remove-if
                             #'zero-length-p
                             (split-sequence:split-sequence-if
                              #'(lambda (x) (char= x #\Space))
                              rule-text))))
                   (t (mapcar #'parse-integer
                              (split-sequence:split-sequence #\Space rule-text)))))
           (parse-rule (rule-line)
             (destructuring-bind (rule-name rule-text)
                 (split-sequence:split-sequence #\: rule-line)
               (cons (parse-integer rule-name) (parse-rule-text (subseq rule-text 1))))))
    (multiple-value-bind (rules messages) (load-data)
      (values (mapcar #'parse-rule rules) messages))))

(multiple-value-bind (rules messages) (parse-input)
  (defparameter *rules* rules)
  (defparameter *messages* messages))

*rules*

(defun first-two (list)
  (when (listp list)
    (list (car list) (cadr list))))

(defun last-two (list)
  (when (listp list)
    (let ((rev (reverse list)))
      (list (cadr rev) (car rev)))))

(defun build-tree (rule-number)
  (let* ((rule (assoc rule-number *rules*))
         (rule-body (cdr rule)))
    (cond ((base-rule-p rule) (base-rule-p rule))
          (t
           (loop for number in rule-body
              if (numberp number)
              collect (build-tree number)
              else collect #\|)))))

(build-tree 0)

;;;; tree snippet
;; (#\b
;;      (#\a (((#\a #\| #\b) (#\a #\| #\b)) #\a #\| (#\b #\b #\| #\a #\a) #\b)
;;       #\| #\b (#\a (#\b #\a) #\| #\b (#\b #\b)))
;;      #\| #\a
;;      (#\a
;;       ((#\a #\a #\| #\b (#\a #\| #\b)) #\b #\|
;;        ((#\a #\| #\b) #\b #\| #\a #\a) #\a)
;;       #\| #\b
;;       (((#\a #\| #\b) (#\a #\| #\b)) #\a #\| (#\b #\b #\| #\a #\a) #\b)))

;; 0: 4 1 5
;; 1: 2 3 | 3 2
;; 2: 4 4 | 5 5
;; 3: 4 5 | 5 4
;; 4: "a"
;; 5: "b"
;; 
;;                     0
;;     4               1               5
;;     a     2     3   |   3     2     b
;;         44|55 45|54   45|54 44|55
;;         aa|bb ab|ba   ab|ba aa|bb
;; ababbb
;; bababa
;; abbbab
;; aaabbb
;; aaaabbb



(defun day19-part1 ()
  nil)

(defun day19-part2 ()
  nil)

(verbose1
  (day19-part1)
  (day19-part2))

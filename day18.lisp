(defparameter *day18* (read-file "d18.txt"))

(defun parse-line (line)
  (let* ((characters
          (loop for c across line
             if (and (char/= c #\Space) (digit-char-p c))
             collect (parse-integer (string c))
             else if (char/= c #\Space)
             collect c)))
    characters))


(defun plus-p (c)
  (and (characterp c) (char= #\+ c)))

(defun times-p (c)
  (and (characterp c) (char= #\* c)))

(defun operator-p (x)
  (and (characterp x) (or (char= #\+ x) (char= #\* x))))

(defun paren-p (x)
  (and (characterp x) (or (char= #\( x) (char= #\) x))))

(defun open-paren-p (x)
  (and (characterp x) (char= #\( x)))

(defun close-paren-p (x)
  (and (characterp x) (char= #\) x)))

(defun get-operator (c)
  (when (operator-p c)
    (cond ((plus-p c) #'+)
          ((times-p c) #'*)
          (t nil))))

(defun compute-line (expr)
  "Compute EXPR with first-come-first-served operator precedence."
  (loop for tail on expr for x in expr
     with paren-level = 0
     with digit-stack = nil
     with oper-stack = nil
     while (>= paren-level 0)
     ;; If #\(, compute the inner expression and add it to the digit-stack
     if (and (zerop paren-level) (open-paren-p x))
     do (progn
          (push (compute-line (cdr tail)) digit-stack)
          (incf paren-level))
     ;; Ignore nested #\(
     else if (open-paren-p x)
     do (incf paren-level)
     ;; Detect unnesting
     else if (close-paren-p x)
     do (decf paren-level)
     ;; Add numbers to the digit stack
     else if (and (zerop paren-level) (numberp x))
     do (push x digit-stack)
     ;; Add operators to the operator stack
     else if (and (zerop paren-level) (operator-p x))
     do (push x oper-stack)
     ;; Compute when possible - first come first served!
     if (and (= 2 (length digit-stack)) oper-stack)
     do (progn
          (let* ((a (pop digit-stack))
                 (b (pop digit-stack))
                 (oper-char (pop oper-stack))
                 (result (funcall (get-operator oper-char) a b)))
            (push result digit-stack)))
     ;; the final item on the digit stack is the final answer
     finally (return (pop digit-stack))))

(defun compute-line2 (expr)
  "Compute EXPR with addition-first operator precedence."
  (loop for tail on expr for x in expr
     with paren-level = 0
     with digit-stack = nil
     with oper-stack = nil
     while (>= paren-level 0)
     ;; If #\(, compute the inner expression and add it to the digit-stack
     if (and (zerop paren-level) (open-paren-p x))
     do (progn
          (push (compute-line2 (cdr tail)) digit-stack)
          (incf paren-level))
     ;; Ignore nested #\(
     else if (open-paren-p x)
     do (incf paren-level)
     ;; Detect unnesting
     else if (close-paren-p x)
     do (decf paren-level)
     ;; Add numbers to the digit stack
     else if (and (zerop paren-level) (numberp x))
     do (push x digit-stack)
     ;; Add operators to the operator stack
     else if (and (zerop paren-level) (operator-p x))
     do (push x oper-stack)
     ;; If we just added a number to the digit-stack and there's a + on the
     ;; oper-stack, then we're all set for adding.
     if (and (or (numberp x) (close-paren-p x))
             (>= (length digit-stack) 2)
             (plus-p (car oper-stack)))
     do (let* ((a (pop digit-stack))
               (b (pop digit-stack)))
          (pop oper-stack) ;; remove the #\+
          (push (funcall #'+ a b) digit-stack))
     ;; We did all the additions, so only multiplications remain;
     ;; return the product of the digit-stack.
     finally (return (apply #'* digit-stack))))

(defun report-result (val string ans)
  (let ((result (= val ans)))
    (format t "~:[FAIL~;pass~]: ~a -> ~a ~:[/=~;==~] ~a~%" result string val result ans)
    result))

(defmacro check-func (func &body string-ans-pairs)
  `(progn ,@(loop for (string ans) on string-ans-pairs
               if (and (stringp string))
               collect `(report-result
                         (funcall ,func (parse-line ,string))
                         ,string ,ans))))

(check-func #'compute-line
  "2 * 3 + (4 * 5)" 26
  "5 + (8 * 3 + 9 + 3 * 4 * 3)" 437
  "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 12240
  "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 13632)

(check-func #'compute-line2
  "1 + (2 * 3) + (4 * (5 + 6))" 51
  "2 * 3 + (4 * 5)" 46
  "5 + (8 * 3 + 9 + 3 * 4 * 3)" 1445
  "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 669060
  "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 23340)

(defun main (&optional (func #'compute-line))
  (apply #'+ (loop for line in *day18* collect (funcall func (parse-line line)))))

(defun day18-part1 ()
  (main))

(defun day18-part2 ()
  (main #'compute-line2))

(verbose1
  (day18-part1)
  (day18-part2))

(ql:quickload 'split-sequence :silent t)

(defparameter *day14* (read-file "d14.txt"))


;;;; Part 1

(defparameter *zero-mask* nil)
(defparameter *one-mask* nil)

(defun get-mask (line)
  (let ((split-line (split-sequence:split-sequence #\Space line)))
    (when (string= (car split-line) "mask")
      (car (last split-line)))))

(defun set-mask (mask-string)
  (when mask-string
    (setf *zero-mask* 0)
    (setf *one-mask* 0)
    (loop for c across mask-string
       for i from 35 downto 0
       if (char= c #\0)
       do (incf *zero-mask* (expt 2 i))
       if (char= c #\1)
       do (incf *one-mask* (expt 2 i))
       finally (return (values *zero-mask* *one-mask*)))))

(defun update-mask (line)
  (let ((mask-string (get-mask line)))
    (set-mask mask-string)))

(defun parse-mem-line (line)
  (if (get-mask line) (cons nil nil)
      (let* ((address-value (split-sequence:split-sequence #\= line))
             (address (remove-if-not #'digit-char-p (car address-value)))
             (value (remove-if-not #'digit-char-p (cadr address-value))))
        (cons (parse-integer address) (parse-integer value)))))

(defun apply-bitmask (value)
  (logior *one-mask* (logand (lognot *zero-mask*) value)))

(defun eval-mem-line (line)
  (destructuring-bind (address . value) (parse-mem-line line)
    (when (and address value)
      (cons address (apply-bitmask value)))))


;;;; Part 2

(defparameter *1s-mask* 0) ; to force bits to 1
(defparameter *0s-mask* 0) ; to keep the values that remain unchanged
(defparameter *floating-mask-bits* nil)
(defparameter *floating-masks* nil)


(defun powerset (s)
  "See https://rosettacode.org/wiki/Power_set#Common_Lisp"
  (if s (mapcan (lambda (x) (list (cons (car s) x) x)) 
                (powerset (cdr s))) 
      '(())))

(defun set-floating-masks-from-bits ()
  (loop for bit-set in (powerset *floating-mask-bits*)
     do (push (loop for b in bit-set summing (expt 2 b)) *floating-masks*)))

(defun set-floating-mask (mask-string)
  (when mask-string
    (setf *1s-mask* 0)
    (setf *0s-mask* 0)
    (setf *floating-mask-bits* nil)
    (setf *floating-masks* nil)
    (loop for c across mask-string
       for i from 35 downto 0
       if (char= c #\X)
       do (push i *floating-mask-bits*)
       if (char= c #\0)
       do (incf *0s-mask* (expt 2 i))
       if (char= c #\1)
       do (incf *1s-mask* (expt 2 i))
       finally (set-floating-masks-from-bits))))

(defun update-masks2 (line)
  "Update masks for floating bit mask challenge (part 2)."
  (let ((mask-string (get-mask line)))
    (set-floating-mask mask-string)))

(defun mem-addr-decode (address)
  (when (and address *floating-masks*)
    (let ((root-address (logior *1s-mask* (logand address *0s-mask*))))
      (mapcar #'(lambda (x) (logior root-address x)) *floating-masks*))))

(defun eval-mem-line2 (line)
  (destructuring-bind (address . value) (parse-mem-line line)
    (when (and address value)
      (mapcar #'(lambda (addr) (cons addr value)) (mem-addr-decode address)))))

;;;; final

(defun day14-part1 ()
  (loop
     for (addr . value) in
       (remove-duplicates
        (loop for line in *day14*
           for addr-val = (eval-mem-line line)
           do (update-mask line)
           if (numberp (car addr-val))
           collect addr-val)
        :key #'car)
     summing value))

(defun day14-part2 ()
  (loop
     for key being the hash-keys of
       (loop with my-memory = (make-hash-table :size 1000)
          for line in *day14*
          for addr-val-list = (eval-mem-line2 line)
          do (update-masks2 line)
          if addr-val-list
          do (loop for (addr . val) in addr-val-list
                do (setf (gethash addr my-memory) val))
          finally (return my-memory))
     using (hash-value value)
     summing value))

(verbose1
  (day14-part1)
  (day14-part2))

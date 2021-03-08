(defparameter *day17* (read-file "d17.txt"))
(defparameter *state* (make-hash-table :test 'equalp))

(defun initialize (&optional (dim 3))
  (labels
      ((clear-state ()
         (loop for key being the hash-keys of *state*
            do (remhash key *state*)))
       (cube-status (c)
         (cond ((char= c #\#) 1)
               ((char= c #\.) 0)
               (t nil)))
       (repeat (item times) (loop repeat times collect item))
       (indexer (dim &rest indices)
         (concatenate 'list indices (repeat 0 (- dim 2))))
       (parse-input ()
         (loop for line in *day17*
            for i from 0
            do (loop for c across line
                  for j from 0
                  do (setf (gethash (indexer dim i j) *state*)
                           (cube-status c))))))
    (clear-state)
    (parse-input)))

(defun get-state (idx)
  (gethash idx *state*))

(defun initialize-state (idx)
  (setf (gethash idx *state*) 0))

(defun uninitialized-p (idx)
  (null (gethash idx *state*)))

(defun active-p (idx)
  (eql 1 (gethash idx *state*)))

(defun print-cubes ()
  (maphash #'(lambda (k v)
             (format t "k: ")
             (princ k)
             (format t " v: ~a~%" v))
           *state*))

(defun gensym-vector (len)
  "Create a vector of length LEN of generated symbols."
  (map 'vector #'(lambda (x) (gensym)) (loop for i from 0 below len collect i)))

(defmacro get-state-bounds (&optional (state-var *state*) (dim 3))
  "Get min and max indices for *state*."
  (let ((bounds-vars (gensym-vector (* 2 dim))))
    `(loop for idx being the hash-keys of ,state-var
        using (hash-value state)
        if (not (null state))
          ,@(loop for d from 0 below dim
               append
                 `(minimize (nth ,d idx) into ,(aref bounds-vars (* 2 d)))
               append
                 `(maximize (nth ,d idx) into ,(aref bounds-vars (1+ (* 2 d)))))
        finally (return (values
                         ,@(loop for i from 0 below dim
                              collect `(cons ,(aref bounds-vars (* 2 i))
                                             ,(aref bounds-vars (1+ (* 2 i))))))))))

(defun pprint-state ()
  "Only works with 3 dimensions right now."
  (multiple-value-bind (ib jb kb) (get-state-bounds)
    (loop for k from (car kb) to (cdr kb)
       collect (loop for i from (car ib) to (cdr ib)
                  collect (loop for j from (car jb) to (cdr jb)
                             if (eql 1 (get-state (list i j k)))
                             collect #\# into result
                             else collect #\. into result
                             finally (return (concatenate 'string result)))
                    into result
                  finally (return (format nil "~{~a~^~%~}~%" result)))
       into result
       finally (format t "~{~a~^~%~}~%" result))))

(defmacro relative-neighbours (dim)
  "Compute all relative neighbours in dimension DIM."
  (let* ((indices (gensym-vector dim))
         (result `(for item = (list ,@(map 'list #'identity indices))
                       unless (every #'zerop item)
                       collect item)))
    (loop for d from 0 below dim
       if (= d 0)
       do (setf result `(loop for ,(aref indices (1- (- dim d)))
                           from -1 to 1 ,@result))
       else do (setf result
                     `(loop for ,(aref indices (1- (- dim d)))
                         from -1 to 1 append ,result))
       finally (return result))))

(defparameter *relative-neighbours3* (relative-neighbours 3))
(defparameter *relative-neighbours4* (relative-neighbours 4))

(defun idx-adder (idx)
  "Return a function that adds IDX to its input."
  (lambda (x) (mapcar #'+ idx x)))

(defun get-neighbours-of (idx)
  "Return a list of the neighbours of IDX."
  (cond ((= (length idx) 3) (mapcar (idx-adder idx) *relative-neighbours3*))
        ((= (length idx) 4) (mapcar (idx-adder idx) *relative-neighbours4*))
        (t (error "get-neighbours-of: idx should have length 3 or 4; 
other dimensions not implemented."))))

(defun uninitialized-neighbours-of (idx)
  "Return a list of the uninitialized neighbours "
  (remove-if-not #'uninitialized-p (get-neighbours-of idx)))

(defun count-active-neighbours (idx)
  "Return a count of neighbours with state 1."
  (count-if #'active-p (get-neighbours-of idx)))

(defun activation (idx)
  "Compute activation for cube at IDX."
  (when (uninitialized-p idx)
    (initialize-state idx))
  (let ((num-active-neighbours (count-active-neighbours idx)))
    (cond ((and (= (get-state idx) 1) (<= 2 num-active-neighbours 3)) 1)
          ((and (= (get-state idx) 0) (= num-active-neighbours 3)) 1)
          (t 0))))

(defmacro initialize-frontier (&optional (dim 3))
  "Initialize the *STATE* elements along the boundary of the slice."
  (let* ((vars (gensym-vector dim))
         (bounds (gensym-vector dim))
         (result `(loop for ,(aref vars 0)
                     from (1- (car ,(aref bounds 0)))
                     to (1+ (cdr ,(aref bounds 0)))
                     do (loop for ,(aref vars 1)
                           from (1- (car ,(aref bounds 1)))
                           to (1+ (cdr ,(aref bounds 1)))
                           do (activation (list ,@(map 'list #'identity vars)))))))
    `(multiple-value-bind ,(map 'list 'identity bounds) (get-state-bounds *state* ,dim)
       ,(loop for d from 2 below dim
           do (setf result `(loop for ,(aref vars d)
                               from (1- (car ,(aref bounds d)))
                               to (1+ (cdr ,(aref bounds d)))
                               do ,result))
             finally (return result)))))

(defun update-state ()
  "Update *STATE* with the next cycle's configuration."
  (multiple-value-bind (new-state uninit)
      (loop for idx being the hash-keys of *state*
         collect (cons idx (activation idx)) into new-state
         append (uninitialized-neighbours-of idx) into uninit
         finally (return (values new-state (remove-duplicates uninit :test 'equalp))))
    (loop for idx in uninit
       do (setf (gethash idx *state*) (activation idx)))
    (loop for item in new-state
       for idx = (car item)
       for state = (cdr item)
       do (setf (gethash idx *state*) state))))

(defun count-active-cubes ()
  "Count the number of active cubes in *STATE*."
  (loop for idx being the hash-keys of *state*
     using (hash-value state)
     counting (= state 1)))

(defun day17-part1 ()
  (initialize)
  (initialize-frontier)
  (loop repeat 6 do (update-state))
  (count-active-cubes))

(defun day17-part2 ()
  (initialize 4)
  (initialize-frontier 4)
  (loop repeat 6 do (update-state))
  (count-active-cubes))

(verbose1
  (day17-part1)
  (day17-part2))

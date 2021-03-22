(ql:quickload 'split-sequence :silent t)

(defun parse-input ()
  (labels ((load-data () (read-file "d21.txt"))
           (parse-ingredients (ingredients)
             (remove-if #'(lambda (x) (zerop (length x)))
                        (split-sequence:split-sequence #\Space ingredients)))
           (parse-allergens (allergens)
             (mapcar #'(lambda (x) (if (or (find #\) x) (find #\, x))
                                       (subseq x 0 (1- (length x))) x))
                     (subseq (split-sequence:split-sequence #\Space allergens) 1)))
           (parse-line (line)
             (destructuring-bind (ingredients allergens) (split-sequence:split-sequence #\( line)
               (list (parse-ingredients ingredients) (parse-allergens allergens))))
           (main ()
             (loop for line in (mapcar #'parse-line (load-data))
                with ingredients = nil
                with allergens = nil
                do (push (pop line) ingredients)
                do (push (pop line) allergens)
                finally (return (values ingredients allergens)))))
    (multiple-value-bind (ingredients allergens) (main)
      (defparameter *ingredients* ingredients)
      (defparameter *allergens* allergens)
      (defparameter *unique-ingredients* (remove-duplicates (apply #'append ingredients)
                                                            :test #'string=))
      (defparameter *unique-allergens* (remove-duplicates (apply #'append allergens)
                                                          :test #'string=)))))

(parse-input)

(defun build-hash ()
  (let ((h (make-hash-table :test #'equal)))
    (loop for allergen in *unique-allergens*
       do (loop for ingredient in *unique-ingredients*
             do (setf (gethash (cons ingredient allergen) h) t)
             do (loop named inner
                   for ingredients in *ingredients*
                   for allergens in *allergens*
                   if (and (find allergen allergens :test #'equal)
                           (not (find ingredient ingredients :test #'equal)))
                   do (progn (setf (gethash (cons ingredient allergen) h) nil)
                             (return-from inner))))
         )
    h))

(defun allergen-free-ingredients ()
  (loop with h = (build-hash)
     for ingredient in *unique-ingredients*
     for slice = (loop for allergen in *unique-allergens*
                    collect (gethash (cons ingredient allergen) h))
     if (every #'null slice)
     collect ingredient))


(defun day21-part1 ()
  "Count the number of times an allergen-free ingredient is used."
  (loop for ingredients in *ingredients*
   with allergen-free-ingredients = (allergen-free-ingredients)
   summing (loop for ingredient in ingredients
              counting (find ingredient allergen-free-ingredients
                             :test #'equal))))

(defun allergenic-ingredients ()
  "Create a copy of *ingredients* with inert ingredients removed."
  (let ((inert (allergen-free-ingredients)))
    (mapcar #'(lambda (line)
                (remove-if #'(lambda (x) (find x inert :test #'equal)) line))
            *ingredients*)))

(defun get-possible-allergen-assignments ()
  (let* ((h (make-hash-table :test #'equal))
         (allergenic-ingredients (allergenic-ingredients))
         (unique-allergenic-ingredients (remove-duplicates
                                         (apply #'append allergenic-ingredients)
                                         :test #'equal)))
    (loop for allergen in *unique-allergens*
       do (loop for ingredient in unique-allergenic-ingredients
             do (setf (gethash (cons ingredient allergen) h) t)
             do (loop for allergens in *allergens*
                   for ingredients in allergenic-ingredients
                   if (and (find allergen allergens :test #'equal)
                           (not (find ingredient ingredients :test #'equal)))
                   do (setf (gethash (cons ingredient allergen) h) nil))))
    (loop for allergen in *unique-allergens*
       collect (loop for ingredient in unique-allergenic-ingredients
                  if (gethash (cons ingredient allergen) h)
                  collect ingredient))))

(defun get-allergen-assignments ()
  (let ((assigned nil)
        (possible-allergen-assignments (get-possible-allergen-assignments)))
    (loop while (some #'(lambda (x) (> (length x) 1)) possible-allergen-assignments)
       do (setf possible-allergen-assignments
                (loop for items in possible-allergen-assignments
                   for allergen in *unique-allergens*
                   if (= (length items) 1)
                   do (when (not (find (car items) assigned :test #'equal))
                        (push (car items) assigned))
                   and collect items
                   else collect (remove-if #'(lambda (x)
                                               (find x assigned :test #'equal))
                                           items)))
       finally (return (sort
                        (pairlis *unique-allergens* (mapcar #'car possible-allergen-assignments))
                        #'string< :key #'car)))))


(defun day21-part2 ()
  (let ((allergen-assignments (get-allergen-assignments)))
    (format nil "~{~a~^,~}" (mapcar #'cdr allergen-assignments))))

(verbose1
  (day21-part1)
  (day21-part2))

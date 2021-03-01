(load "my-utils.lisp")

(defparameter *day7* (read-file "d7.txt"))

;;;; preprocessing

(ql:quickload 'split-sequence :silent t)

(defun split-input-by-groups (data)
  "Return a list of lists from DATA, assuming each entry is a string."
  (split-sequence:split-sequence
   0 data :key #'(lambda (x) (length x))))

(defun split-parent-child (line)
  (let* ((i0 (search " bags contain " line))
         (i1 (+ i0 14))
         (i2 (1- (length line))))
    (list (subseq line 0 i0) (subseq line i1 i2))))

(defun no-other-bags-p (children-string)
  (search "no other bags" children-string))

(defun remove-leading-space (string)
  "docstring"
  (let ((space-pos (position #\Space string)))
    (cond ((and space-pos (= space-pos 0)) (subseq string 1))
          (t string))))

(defun split-children (children-string)
  "Split a string of all children into a list of each child item"
  (cond ((no-other-bags-p children-string) nil)
        (t (mapcar
            #'remove-leading-space
            (split-sequence:split-sequence #\, children-string)))))

(defun parse-child (child-string)
  (let ((child (split-sequence:split-sequence #\Space child-string)))
    (cons
     (concatenate 'string (cadr child) " " (caddr child))
     (parse-integer (car child)))))

(defun parse-line (line)
  (let* ((parent-children (split-parent-child line))
         (parent          (car parent-children))
         (children        (split-children (cadr parent-children))))
    (cons (car parent-children)
          (cond (children (mapcar #'parse-child children))
                (t nil)))))

(defparameter *data* (mapcar #'parse-line *day7*))

;;;; part 1

(defun mk-contains-colour-p (colour)
  (lambda (entry) (assoc colour (cdr entry) :test #'string=)))

(defun mk-get-containing-colour (colour)
  (lambda (entry)
    (when (funcall (mk-contains-colour-p colour) entry)
      (car entry))))

(defun get-containing-colours (data &optional (colour "shiny gold"))
  (remove nil (mapcar (mk-get-containing-colour colour) data)))

(defun remove-visited (data visited)
  (remove-if #'(lambda (x) (member (car x) visited :test #'string=)) data))

(defun search-visited (all-visited colours data)
  (let* ((new-data (remove-visited data colours))
         (new-colours
          (mapcan #'(lambda (colour)
                      (get-containing-colours new-data colour))
                  colours)))
    (cond ((and new-colours (> (length new-data) 0))
           (search-visited (append all-visited new-colours) new-colours new-data))
          (t (remove-duplicates all-visited)))))

;;;; Part 2

(defun get-contained-bags (data colour)
  "Return a list of cons cells representing the bag colours and their required
numbers for a COLOUR bag."
  (cdr (assoc colour data :test #'string=)))

(defun bag-counter (data item)
  "Return the number of bags + the number of bags inside each of those bags."
  (let ((bag-colour (car item))
        (num-bags   (cdr item)))
    (+ num-bags (* num-bags (count-bags-inside bag-colour data)))))

(defun count-bags-helper (data contained-bags)
  "Apply BAG-COUNTER to each bag colour in CONTAINED-BAGS and sum the result."
  (apply #'+ (mapcar #'(lambda (item) (bag-counter data item)) contained-bags)))

(defun count-bags-inside (colour data)
  "Count the number of bags required inside a COLOUR bag."
  (let ((contained-bags (get-contained-bags data colour)))
    (cond (contained-bags (count-bags-helper data contained-bags))
          (t 0))))

;;;; final

(defun day7-part1 ()
  (length (search-visited nil (list "shiny gold") *data*)))

(defun day7-part2 ()
  (count-bags-inside "shiny gold" *data*))

(verbose1
  (day7-part1)
  (day7-part2))

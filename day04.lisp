(load "my-utils.lisp")

(defparameter
    *required-fields* (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")
  "Field names that must be contained in a passport.")

(defun get-colon-positions (string &optional (start 0))
  "Recursively return a list of indices corresponding to the colons in STRING."
  (let ((start (position #\: string :start start)))
    (cond (start (apply #'list start (get-colon-positions string (1+ start))))
          (t nil))))

(defun get-field (string colon-loc)
  "Return the passport field associated with a given COLON-LOC."
  (subseq string (- colon-loc 3) colon-loc))

(defun concat-strings (list)
  "Join a list of strings into one, separated by spaces."
  (if (and (listp list) (every #'stringp list))
      (with-output-to-string (s)
        (format s "~{~a~^ ~}" list))))

(defun get-fields-from-string (string)
  "Return a list of all found passport fields in STRING."
  (mapcar #'(lambda (colon-loc) (get-field string colon-loc))
          (get-colon-positions string)))

(defun get-fields-from-batch (batch)
  "Return a list of all found passport fields in BATCH. (helper function.)"
  (get-fields-from-string (concat-strings (reverse batch))))

(defun on-fields (func fields)
  "Apply FUNC using FIELDS as the sequence against which to test membership."
  (funcall func #'(lambda (x) (member x fields :test #'string=)) *required-fields*))

(defun num-required-fields (fields)
  "Count number of required fields found in FIELDS."
  (on-fields #'count-if fields))

(defun contains-required-fields-p (fields)
  "Return T if all required passport fields were found in FIELDS; NIL otherwise."
  (on-fields #'every fields))

(defun required-fields-p (line)
  "Return T if line contains all required passport fields (cf. *required-fields*)."
  (contains-required-fields-p (get-fields-from-string line)))

;; note: for some reason the last batch was not being processed by loop; we need
;; to collect it manually using this extra (slightly ugly) code.
(defun traverse-collecting-field-names (data)
  (let* ((batch nil)
         (results
          (loop for line in data
             if (string= line "")
             collect (get-fields-from-batch batch)
             and do (setf batch nil)
             else do (push line batch))))
    (cond (batch (append results (list (get-fields-from-batch batch))))
          (t results))))

     ;;; code for part 2

(defun lineify (data)
  "Transform DATA so that one passport entry corresponds with one string in the returned list."
  (let* ((batch nil)
         (results
          (loop for line in data
             if (string= line "")
             collect (concat-strings (reverse batch))
             and do (setf batch nil)
             else do (push line batch))))
    (cond (batch (append results (list (concat-strings (reverse batch)))))
          (t results))))

(defun whitespacep (c)
  "Return T if C is a space or newline character."
  (or (char= c #\Space) (char= c #\Newline)))

(defun colonp (c)
  "Return T if C is a colon character."
  (char= c #\:))

(defun split-string (string &optional (predicate #'whitespacep))
  "Split STRING into multiple strings at the locations determined by
        PREDICATE. Default is to split at spaces/newlines."
  (let ((locs (loop for c across string for i from 0 if (funcall predicate c) collect i)))
    (setf locs (concatenate 'list (list 0) locs (list (length string))))
    (loop for i0 in locs
       for i1 in (cdr locs)
       collect (remove-if predicate (subseq string i0 i1)))))

(defun parse-field-names-values (line)
  "Return a list of '(key value) lists for each key provided in LINE."
  (mapcar #'(lambda (x) (split-string x #'colonp)) (split-string line)))

(defun string-number-in-bounds-p (string lower upper)
  "Determine if STRING represents a number that lies between lower and
        upper (inclusive)."
  (let ((n (parse-integer string :junk-allowed t)))
    (when n (<= lower n upper))))

(defun valid-byr-p (byr)
  "Validate passport birth year IYR."
  (string-number-in-bounds-p byr 1920 2002))

(defun valid-iyr-p (iyr)
  "Validate passport issue year IYR."
  (string-number-in-bounds-p iyr 2010 2020))

(defun valid-eyr-p (eyr)
  "Validate passport expiration year EYR."
  (string-number-in-bounds-p eyr 2020 2030))

(defun valid-hgt-p (hgt)
  "Validate height HEIGHT."
  (cond ((search "cm" hgt) (string-number-in-bounds-p hgt 150 193))
        ((search "in" hgt) (string-number-in-bounds-p hgt 59 76))
        (t nil)))

(defun valid-hcl-p (hcl)
  "Validate hair colour HCL."
  (let ((hash-pos (position #\# hcl)) )
    (and hash-pos (= hash-pos 0) (= (length hcl) 7)
         (every
          #'(lambda (c) (position c "1234567890abcdef"))
          (subseq hcl 1)))))

(defun valid-ecl-p (ecl)
  "Validate eye colour ECL."
  (not (null (member ecl (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))))

(defun valid-pid-p (pid)
  "Validate a passport id PID."
  (and (= (length pid) 9) (every #'(lambda (c) (position c "1234567890")) pid)))

(defun valid-cid-p (cid) "Ignore CID entries and return T." t)

(defun select-validator (field-name)
  "Select a validator to use according to FIELD-NAME."
  (cond ((string= field-name "byr") #'valid-byr-p)
        ((string= field-name "iyr") #'valid-iyr-p)
        ((string= field-name "eyr") #'valid-eyr-p)
        ((string= field-name "hgt") #'valid-hgt-p)
        ((string= field-name "hcl") #'valid-hcl-p)
        ((string= field-name "ecl") #'valid-ecl-p)
        ((string= field-name "pid") #'valid-pid-p)
        ((string= field-name "cid") #'valid-cid-p)
        (t (format t "Error: could not parse field name") nil)))

(defun validate-field (field-kv)
  "Validate a passport key-value pair FIELD-KV, a two-element list."
  (funcall (select-validator (car field-kv)) (cadr field-kv)))

(defun all-field-values-valid-p (line)
  "Validate all provided passport fields in LINE."
  (let ((parsed (parse-field-names-values line)))
    (every #'validate-field parsed)))

(defun valid-passport-p (line)
  "Determine if LINE corresponds to a valid passport."
  (and (required-fields-p line) (all-field-values-valid-p line)))

(defun day4-part1 ()
  (count 7 (mapcar #'num-required-fields
                   (traverse-collecting-field-names (read-file "d4.txt")))))

(defun day4-part2 ()
  (count-if #'valid-passport-p (lineify (read-file "d4.txt"))))

(verbose1 
  (day4-part1)
  (day4-part2))

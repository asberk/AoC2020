(defparameter *aoc-dir* "code/aoc20/inputs/")

(defun get-puzzle-input-fpath (fname)
  "Return ~/code/aoc20/inputs/FNAME"
  (concatenate 'string (namestring (user-homedir-pathname)) *aoc-dir* fname))

(defun form-length (quoted-form)
  (length (princ-to-string quoted-form)))

(defun longest-form-length (&rest quoted-forms)
  (loop for f in quoted-forms maximizing (form-length f)))

(defun verbosify1 (form tablength result)
  "Tries to keep everything on one line."
  (format t "~a~v,0t : -> ~a~%" form tablength result))

(defun verbosify2 (form result)
  "Two-line formatting"
  (format t "~a~%~2t : -> ~a~%" form result))

(defmacro verbose1 (&body forms)
  (let ((len-name (gensym)))
    `(let ((,len-name (apply #'longest-form-length ',forms)))
       (progn
         ,@(loop for form in forms
              collect `(verbosify1 ',form ,len-name ,form))))))

(defmacro verbose2 (&body forms)
  `(progn
     ,@(loop for form in forms
          collect `(verbosify2 ',form ,form))))

(defun read-file (filename &optional (reader #'read-line))
  "Read a file from FILENAME and return it as a list of lines."
  (with-open-file (data (get-puzzle-input-fpath filename))
    (loop for line = (funcall reader data nil)
       while line collect line)))

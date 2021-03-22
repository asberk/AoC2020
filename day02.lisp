#!/usr/bin/env sbcl --script
(load "my-utils.lisp")

(defparameter *day2* (read-file "d2.txt"))

(defun parse-toboggan-password (line)
  (let ((hyphen-loc (search "-" line))
        (colon-loc (search ": " line)))
    (list
     (parse-integer (subseq line 0 hyphen-loc))
     (read-from-string (subseq line (1+ hyphen-loc)))
     (subseq line (1- colon-loc) colon-loc)
     (subseq line (+ 2 colon-loc)))))

(defun count-char-occurence (password letter)
  (count-if #'(lambda (c) (string= c letter)) password))

(defun letter-count-in-bounds-p (lower upper letter-count)
  (<= lower letter-count upper))

(defun p1-valid-password-p (line)
  (let* ((parsed (parse-toboggan-password line))
         (lower (first parsed))
         (upper (second parsed))
         (letter (third parsed))
         (password (fourth parsed)))
    (letter-count-in-bounds-p
     lower upper (count-char-occurence
                  password letter))))

(defun extract-letter-from-position (string pos)
  "Note that this is one-indexed as the challenge demands!"
  (subseq string (1- pos) pos))

(defun xor (a b)
  (and (or a b) (not (and a b))))


(defun p2-valid-password-p (line)
  (let* ((parsed (parse-toboggan-password line))
         (pos1 (first parsed))
         (pos2 (second parsed))
         (letter (third parsed))
         (password (fourth parsed))
         (letter1 (extract-letter-from-position password pos1))
         (letter2 (extract-letter-from-position password pos2)))
    (xor (string= letter1 letter) (string= letter2 letter))))

(defun day2-part1 ()
  (count-if #'p1-valid-password-p *day2*))

(defun day2-part2 ()
  (count-if #'p2-valid-password-p *day2*))

(verbose1
  (day2-part1)
  (day2-part2))

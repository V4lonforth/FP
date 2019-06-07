(defpackage :lab3.io
    (:export   
      :parse-number 
      :read-point
      :read-points
      :print-point))
(in-package :lab3.io)

(defun print-point (point)
  (format t "~%~,3F;~,3F" (car point) (cdr point)))

(defun parse-number (str)
  (when str
    (with-input-from-string (in str)
      (read in))))

(defun parse-line (line)
  (when line
    (cons (parse-number (subseq line 0 (position #\; line))) (parse-number (subseq line (+ (position #\; line) 1))))))

(defun read-points (count)
  (loop 
    for i from 1 to count
    for line = (read-line nil nil)
    until (null line)
    collect (parse-line line)))

(defun read-point ()
  (parse-line (read-line nil nil)))
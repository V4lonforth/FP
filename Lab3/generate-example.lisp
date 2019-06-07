(load "Lab3/number-generator.lisp")

(defpackage :lab3.args-parser
  (:export    
    :generate-example)
  (:use
    :cl
    :lab3.number-generator)
  (:import-from 
    :lab3.number-generator))
(in-package :lab3.args-parser)

(defun get-numbers()
  (let ((input (open "Lab1/input.txt" :if-does-not-exist nil)))
    (when input
      (defvar numbers 
        (loop for line = (read input nil)
          until (null line)
          collect line))
      (close input)
      numbers)))

(defun generate-example (f number-generator file-name)
  (let ((output (open file-name :direction :output)))
    (labels (
        (generate-points (f generator)
          (multiple-value-bind
              (generator number)
              (funcall generator)
            (when number
              (progn 
                (pprint number output)
                (write-char #\; output)
                (write (funcall f number) :stream output)
                (generate-points f generator))))))
      (generate-points f number-generator))))


(defun main ()
  (generate-example 'sqrt (get-generator 0 10000 1) "Lab3/examples/example3.csv"))
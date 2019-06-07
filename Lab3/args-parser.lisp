(load "Lab3/io.lisp")
(load "Lab3/number-generator.lisp")
(load "Lab3/interpolation/newton-interpolator.lisp")
(load "Lab3/interpolation/linear-segments-interpolator.lisp")

(defpackage :lab3.args-parser
  (:export    
    :get-interpolator
    :get-number-generator
    :get-help
    :get-start-points-count)
  (:use
    :cl
    :lab3.newton
    :lab3.linear
    :lab3.io
    :lab3.number-generator)
  (:import-from 
    :lab3.newton
    :lab3.linear
    :lab3.io
    :lab3.number-generator))
(in-package :lab3.args-parser)

(defun get-arg-value (name args)
  (let ((pos (position name args :test 'string=)))
    (when (and pos (< pos (- (length args) 1)))
      (nth (+ pos 1) args))))

(defun get-from (args)
  (or (parse-number (get-arg-value "from" args)) 0))
(defun get-to (args)
  (or (parse-number (get-arg-value "to" args)) 0))
(defun get-step (args)
  (or (parse-number (get-arg-value "step" args)) 1))

(defun parse-interpolator (name)
  (cond 
    ((string= name "newton") 'newton-interpolator)
    ((string= name "linear") 'linear-interpolator)
    (t 'newton-interpolator)))


(defun get-number-generator (args)
  (lambda () (get-generator (get-from args) (get-to args) (get-step args))))

(defun get-start-points-count (args)
  (let (
      (interpolator-name (get-arg-value "interpolator" args))
      (number (or (parse-number (get-arg-value "start-size" args)) 20)))
    (cond
      ((string= interpolator-name "linear") 2)
      ((string= interpolator-name "newton") number)
      (t number))))

(defun get-interpolator (args)
  (parse-interpolator (or (get-arg-value "interpolator" args) "newton")))

(defun get-help (args)
  (position "help" args :test 'string=))
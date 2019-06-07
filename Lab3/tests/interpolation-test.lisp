(load "Lab3/interpolation/newton-interpolator.lisp")
(load "Lab3/interpolation/linear-segments-interpolator.lisp")
(load "Lab3/interpolation/interpolator.lisp")
(load "C:/Users/V4lonforth/quicklisp/setup.lisp")
(ql:quickload :prove)

(defpackage :lab3.interpolation-test
  (:use
    :cl
    :prove
    :lab3.newton
    :lab3.linear
    :lab3.interpolator)
  (:import-from
    :lab3.newton
    :lab3.linear
    :lab3.interpolator))
(in-package :lab3.interpolation-test)

(defun float-equal (a b &optional (accuracy 0.0001))
  (< (abs (- a b)) accuracy))


(plan 2)

(subtest "Linear interpolation"
  (let ((interpolator (car (linear-interpolator '((0 . 0) (3 . 6))))))
    (ok (float-equal 
      (funcall interpolator 2)
      4))))

(subtest "Newton interpolation"
  (let ((interpolator (car (newton-interpolator '((-1 . -1) (-0.5 . -0.125) (0 . 0) (1 . 1))))))
    (ok
      (float-equal
        (funcall interpolator 0.5)
        0.125)))
  (let ((interpolator (car (newton-interpolator 
              (list (cons 0 0) 
                    (cons (/ pi 6) (sin (/ pi 6))) 
                    (cons (/ pi 4) (sin (/ pi 4))) 
                    (cons (/ pi 2) (sin (/ pi 2))) 
                    (cons (/ (* 3 pi) 4) (sin (/ (* 3 pi) 4))))))))
    (ok
      (float-equal
        (funcall interpolator (/ pi 3))
        (sin (/ pi 3))
        0.001))))

(finalize)
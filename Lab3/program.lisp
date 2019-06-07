(load "Lab3/io.lisp")
(load "Lab3/args-parser.lisp")
(load "Lab3/number-generator.lisp")

(defpackage lab3
  (:use
    :cl
    :lab3.args-parser
    :lab3.io
    :lab3.number-generator)
  (:import-from 
    :lab3.args-parser
    :lab3.io
    :lab3.number-generator))
(in-package :lab3)

(defun get-args ()
  (or 
    #+CLISP ext:*args*
    #+SBCL *posix-argv*  
    nil))

(defun print-command (str)
  (format t "~%~A~A" #\tab str))

(defun print-help ()
  (format t "Commands: ")
  (print-command "help")
  (print-command "interpolator (linear/newton)")
  (print-command "from")
  (print-command "to")
  (print-command "step")
  (print-command "start-size"))

(defun interpolate-number (interpolator number-generator)
  (print-point (cons (get-number number-generator) (funcall (car interpolator) (get-number number-generator))))
  (interpolate-numbers interpolator (funcall (get-next-generator number-generator))))

(defun interpolate-numbers (interpolator number-generator)
  (when (get-number number-generator)
    (if (<= (get-number number-generator) (caddr interpolator))
      (interpolate-number interpolator number-generator)
      (let ((next-point (read-point)))
        (if next-point
          (interpolate-numbers (funcall (cadr interpolator) next-point) number-generator)
          (interpolate-number interpolator number-generator))))))

(defun main ()
  (if (get-help (get-args))
    (print-help)
    (interpolate-numbers 
        (funcall  
          (get-interpolator (get-args)) 
          (read-points (get-start-points-count (get-args))))
      (funcall (get-number-generator (get-args)))))
  (ext:quit))

(EXT:SAVEINITMEM "prog" :init-function 'main)
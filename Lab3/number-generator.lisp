(defpackage :lab3.number-generator
  (:export
    :get-number
    :get-next-generator
    :get-generator)
  (:use
    :cl))
(in-package :lab3.number-generator)

(defun get-number (generator)
  (car generator))

(defun get-next-generator (generator)
  (cadr generator))

(defun get-generator (begin end step)
  (when (<= begin end)
    (list
      begin
      (lambda () (get-generator (+ begin step) end step)))))

(defun get-number-list (begin end step)
  (if (< begin end)
    (cons begin (get-number-list (+ begin step) end step))
    (list begin)))
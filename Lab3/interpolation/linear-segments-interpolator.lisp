(defpackage :lab3.linear
  (:export    
    :linear-interpolator))
(in-package :lab3.linear)

(defun left-x (line)
  (caar line))
(defun left-y (line)
  (cdar line))

(defun right-x (line)
  (caadr line))
(defun right-y (line)
  (cdadr line))

(defun interpolate (line x)
  (when line
    (+ (left-y line) (/ (* (- x (left-x line)) (- (right-y line) (left-y line))) (- (right-x line) (left-x line))))))

(defun linear-interpolator (start-line)
  (labels (
      (get-interpolator (line)
        (list
          (lambda (x) (interpolate line x))
          (lambda (point) (get-interpolator (append (cdr line) (list point))))
          (right-x line))))
    (get-interpolator start-line)))
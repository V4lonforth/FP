(defpackage :lab3.newton
  (:export    
    :newton-interpolator))
(in-package :lab3.newton)

(defun memoize (cache fn)
  (lambda (&rest args)
    (or (gethash args cache) 
        (setf (gethash args cache)
              (apply fn args)))))

(defun coefficient-constr ()
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (values-array i j)
      (labels (
          (get-coefficient (i j)
            (if (= i j)
              (cdr (aref values-array i))
              (/  (-  (get-coefficient-memoized (+ i 1) j) 
                      (get-coefficient-memoized i (- j 1)))
                  (-  (car (aref values-array j))
                      (car (aref values-array i))))))
          (get-coefficient-memoized (i j)
            (funcall (memoize cache #'get-coefficient) i j)))
        (get-coefficient-memoized i j)))))

(defun polynom-constr (values)
  (let ((get-coefficient (coefficient-constr)) (count (length values)))
    (lambda (values offset)
      (let ((values-array (make-array (length values) :initial-contents values)))
        (loop for i from offset to (- (length values-array) 1)
          collect (cons (funcall get-coefficient values-array offset i) (car (aref values-array i))))))))

(defun get-middle (polynom)
  (/  (loop for (koeff . x) in polynom
        sum x)
      (length polynom)))

(defun interpolate (polynom x)
  (labels (
    (sum (polynom var result)
      (if (/= 0 (length polynom))
        (sum (cdr polynom) (* var (- x (cdar polynom))) (+ result (* var (caar polynom))))
        result)))
    (sum polynom 1 0)))

(defun newton-interpolator (start-values)
  (let ((get-polynom (polynom-constr start-values)))
    (labels (
        (get-interpolator (values offset)
          (let ((polynom (funcall get-polynom values offset)))
            (list
              (lambda (x) (interpolate polynom x))
              (lambda (point) (get-interpolator (append values (list point)) (+ offset 1)))
              (get-middle polynom)))))
      (get-interpolator start-values 0))))
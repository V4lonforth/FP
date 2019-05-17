(defun print-number(number)
  (print (subseq (write-to-string number) 0 10))
)

(defun read-numbers(input numbers)
  (let ((num (read input nil)))
    (when (null num) (return-from read-numbers numbers))
    (read-numbers input (cons num numbers))
  )
)

(defun get-numbers()
  (let ((input (open "Lab1/input.txt" :if-does-not-exist nil)))
    (let ((numbers (read-numbers input '())))
      (close input)
      numbers
    )
  )
)

(defun get-sum(numbers)
  (reduce #'+ numbers)
)

(print-number (get-sum (get-numbers)))
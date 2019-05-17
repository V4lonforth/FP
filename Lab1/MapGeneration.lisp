(defun print-number(number)
  (print (subseq (write-to-string number) 0 10))
)

(defun read-lines(input numbers)
  (let ((line (read-line input nil)))
    (when (null line) (return-from read-lines numbers))
    (read-lines input (cons line numbers))
  )
)

(defun get-numbers()
  (let ((input (open "Lab1/input.txt" :if-does-not-exist nil)))
    (let ((numbers (mapcar #'parse-integer (read-lines input '()))))
      (close input)
      numbers
    )
  )
)

(defun get-sum(numbers)
  (reduce #'+ numbers)
)

(print-number (get-sum (get-numbers)))
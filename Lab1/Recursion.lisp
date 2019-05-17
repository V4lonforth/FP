(defun print-number(number)
  (print (subseq (write-to-string number) 0 10))
)

(defun sum(input)
  (let ((num (read input nil)))
    (if (null num) 0 (+ num (sum input)))
  )
)

(let ((input (open "Lab1/input.txt" :if-does-not-exist nil)))
  (when input
    (print-number (sum input))
    (close input)
  )
)
(defun print-number(number)
  (print (subseq (write-to-string number) 0 10))
)

(defun sum(input result)
  (let ((num (read input nil)))
    (when (null num) (return-from sum result))
    (sum input (+ result num))
  )
)

(let ((input (open "Lab1/input.txt" :if-does-not-exist nil)))
  (when input
    (print-number (sum input 0))
    (close input)
  )
)
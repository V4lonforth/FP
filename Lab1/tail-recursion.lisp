(defun print-number(number)
  (print (subseq (write-to-string number) 0 10)))

(defun sum(numbers result)
  (if numbers
    (sum (cdr numbers) (+ result (car numbers)))
    result))

(let ((input (open "Lab1/input.txt" :if-does-not-exist nil)))
  (when input
    (print-number (sum
      (loop for line = (read input nil)
        until (null line)
        collect line)
      0))
    (close input)))
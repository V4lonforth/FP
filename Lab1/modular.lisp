(defun print-number(number)
  (print (subseq (write-to-string number) 0 10)))

(defun get-numbers()
  (let ((input (open "Lab1/input.txt" :if-does-not-exist nil)))
    (when input
      (defvar numbers 
        (loop for line = (read input nil)
          until (null line)
          collect line))
      (close input)
      numbers)))

(defun get-sum(numbers)
  (reduce #'+ numbers))

(print-number (get-sum (get-numbers)))
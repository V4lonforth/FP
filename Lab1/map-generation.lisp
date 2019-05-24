(defun print-number(number)
  (print (subseq (write-to-string number) 0 10)))

(defun get-lines()
  (let ((input (open "Lab1/input.txt" :if-does-not-exist nil)))
    (when input
      (defvar numbers 
        (loop for line = (read-line input nil)
          until (null line)
          collect line))
      (close input)
      numbers)))
      
(defun get-numbers()
  (mapcar #' parse-integer (get-lines)))

(defun get-sum(numbers)
  (reduce #'+ numbers))

(print-number (get-sum (get-numbers)))
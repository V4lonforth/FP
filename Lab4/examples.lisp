(load "Lab4/match-pattern.lisp")
(in-package :lab4)


(defun parse-str (str)
  (match str
    ("[^@]{2,10}@[^\\.]{2,}\\.[^.@]{2,}" "E-mail")
    ("\\+[0-9]\\([0-9]*\\)[-0-9]*" "Phone number")
    (_ "Invalid string")))

;(print (macroexpand
;  '(match str
;    ("[^@]{2,10}@[^\\.]{2,}\\.[^.@]{2,}" "E-mail")
;    ("\\+[0-9]\\([0-9]*\\)[-0-9]*" "Phone number")
;    (_ "Invalid string"))))

(print (parse-str "+7(891)-173-18-67"))
(print (parse-str "mail@mail.ru"))
(print (parse-str "m@."))


(defun fibonacci (n)
  (match n
    ((guard n (< n 2)) 1)
    (n (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
    
(print (fibonacci 6))


(defun node (value &optional left right)
  (list value left right))

(defun small-left-turn (node)
  (match node
    ((node a (node x) (node b (node y) (node z))) (node b (node a (node x) (node y)) (node z)))
    (_ nil)))

(print (small-left-turn (node 3 (node 2) (node 5 (node 4) (node 6)))))
(print (small-left-turn (node 3)))


(defun get-childs (n)
  (match n
    ((node x nil nil) nil)
    ((or (node x child nil) (node x nil child)) child)
    ((node x l r) (list l r))))

(print (get-childs (node 5)))
(print (get-childs (node 5 (node 4))))
(print (get-childs (node 5 nil (node 6))))
(print (get-childs (node 5 (node 4) (node 6))))


(defun test-eq (list)
  (match list
    ((list a b a) (cons a b))
    ((list a a _) a)
    ((cons a b) b)))

(print (test-eq (list 1 2 1)))
(print (test-eq (list 1 1 3)))
(print (test-eq (list 1 2 3)))
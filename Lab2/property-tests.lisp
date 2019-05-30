(load "Lab2/binary-tree.lisp")
(load "C:/Users/V4lonforth/quicklisp/setup.lisp")
(ql:quickload :prove)

(in-package :lab2)
(use-package :prove)

(plan 3)

;Associative property of merge operation
;(a + b) + c = a + (b + c)
(let ((a (tree '(1 2 3 4)))
      (b (tree '(4 5 6 7)))
      (c (tree '(7 8 9 10))))
      (ok 
        (compare-trees 
          (merge-trees (merge-trees a b) c)
          (merge-trees a (merge-trees b c)))))

;Is structure a monoid
;ex = x = xe
(let ((a (tree '(1 2 3 4 5))))
  (is
    (compare-trees
      (merge-trees a nil)
      a)
    (compare-trees
      (merge-trees nil a)
      a)))

;Are keys unqiue
;a + a = a
(let ((a (tree 
  (loop for i from 1 to 100
    collect (random 10000)))))
  (ok (compare-trees a (merge-trees a a))))

(finalize)
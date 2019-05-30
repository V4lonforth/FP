(load "Lab2/binary-tree.lisp")
(load "C:/Users/V4lonforth/quicklisp/setup.lisp")
(ql:quickload :prove)

(in-package :lab2)
(use-package :prove)

(plan 3)

(defun gen-tree(count max-value)
  (tree (loop for i from 1 to count
    collect (random max-value))))

(let ((a (gen-tree 100 100000))
      (b (gen-tree 100 100000))
      (c (gen-tree 100 100000)))

  ;Associative property of merge operation
  ;(a + b) + c = a + (b + c)
  (ok 
    (compare-trees 
      (merge-trees (merge-trees a b) c)
      (merge-trees a (merge-trees b c))))
  
  ;Are keys unqiue
  ;a + a = a
  (ok (compare-trees a (merge-trees a a)))

  ;Is structure a monoid
  ;ex = x = xe
  (is
    (compare-trees
      (merge-trees a nil)
      a)
    (compare-trees
      (merge-trees nil a)
      a)))

(finalize)
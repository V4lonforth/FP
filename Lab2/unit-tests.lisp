(load "Lab2/binary-tree.lisp")
(load "C:/Users/V4lonforth/quicklisp/setup.lisp")
(ql:quickload :prove)

(in-package :lab2)
(use-package :prove)

(plan 10)

(subtest "Turns"
  (ok (equal 
    (small-left-turn (node 5 (node 4) (node 7 (node 6) (node 8))))
    (node 7 (node 5 (node 4) (node 6)) (node 8))))
           
  (ok (equal 
    (small-right-turn (node 5 (node 2 (node 1) (node 3)) (node 7)))
    (node 2 (node 1) (node 5 (node 3) (node 7)))))

  (ok (equal 
    (big-left-turn (node 4 (node 3) (node 8 (node 6 (node 5) (node 7)) (node 9))))
    (node 6 (node 4 (node 3) (node 5)) (node 8 (node 7) (node 9)))))
            
  (ok (equal 
    (big-right-turn (node 8 (node 4 (node 3) (node 6 (node 5) (node 7))) (node 9)))
    (node 6 (node 4 (node 3) (node 5)) (node 8 (node 7) (node 9)))))
            
  (ok (equal 
    (balance-node (node 3 (node 2) (node 8 (node 6 (node 5) (node 7)) (node 9))))
    (node 6 (node 3 (node 2) (node 5)) (node 8 (node 7) (node 9))))))

(subtest "Adding to tree"
  (ok (equal 
    (add-node (node 3 (node 2)) 1)
    (node 2 (node 1) (node 3))))
    
  (ok (equal
    (add-list (node 3) '(2 1))
    (node 2 (node 1) (node 3))))
    
  (ok (equal
    (tree '(3 2 1))
    (node 2 (node 1) (node 3)))))

(subtest "Comparing"
  (ok (compare-trees
    (tree '(1 2))
    (tree '(2 1))))
  (ok (not (compare-trees
    (tree '(1 2 3))
    (tree '(1 2)))))
  (ok (compare-trees
    (tree '(1 5 2 7 3 4 6 5))
    (tree '(1 2 3 4 5 5 6 7)))))

(subtest "Removing from tree"
  (ok (equal
    (remove-node (tree '(1 2 3)) 2)
    (tree '(1 3))))
  (is nil (remove-list (tree '(1 2 3 4 5)) '(3 4 1 2 5))))

(subtest "Properties"
  (is 3 (get-height (node 5 (node 4) (node 7 (node 6)))))
  (is 1 (get-balance (node 5 (node 4) (node 7 (node 6)))))
  (is 1 (get-min (tree '(7 3 9 8 1 2 4))))
  (is 9 (get-max (tree '(7 3 9 8 1 2 4)))))

(subtest "Listing"
  (ok (equal
    (tree-to-list (tree '(6 5 1 4 2 3)))
    '(1 2 3 4 5 6)))
    
  (ok (equal
    (tree-to-list-back (tree '(6 5 1 4 2 3)))
    '(6 5 4 3 2 1))))

(subtest "Merging"
  (ok (compare-trees
    (merge-trees (tree '(1 2 3)) (tree '(3 4 5)))
    (tree '(1 2 3 4 5)))))

(subtest "Mapping"
  (ok (compare-trees
    (map-tree '1+ (tree '(1 2 3 4)))
    (tree '(2 3 4 5)))))

(subtest "Filtering"
  (ok (compare-trees 
    (filter-tree (lambda (x) (> x 3)) (tree '(1 2 3 4 5)))
    (tree '(4 5)))))

(subtest "Reducing"
  (is 45 (reduce-tree '+ (tree '(1 2 3 4 5 6 7 8 9))))
  (is 45 (reduce-tree '+ (tree '(1 2 3 4 5 6 7 8 9)) :reverse t)))

(finalize)
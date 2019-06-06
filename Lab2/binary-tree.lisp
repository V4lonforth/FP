(defpackage :lab2
  (:export  
    :tree
    :get-height
    :get-max
    :get-min
    :add-node
    :add-list
    :merge-trees
    :remove-node
    :remove-list
    :map-tree
    :filter-tree
    :reduce-tree
    :reduce-tree-back
    :tree-to-list
    :tree-to-list-back
    :compare-trees))
(in-package :lab2)

(defun node (value &optional left right)
  (cons value (cons left right)))

(defun get-key (node)
  (car node))

(defun get-left (node)
  (cadr node))

(defun get-right (node)
  (cddr node))

(defun empty-tree ()
  (node nil))

(defun get-height (node)
  (if node 
    (+ 1 (max (get-height (get-left node)) (get-height (get-right node))))
    0))

(defun get-balance (node)
  (- (get-height (get-right node)) (get-height (get-left node))))


(defun small-left-turn (node)
  (let ((a node) (b (get-right node)))
    (node 
      (get-key b)
      (node 
        (get-key a)
        (get-left a)
        (get-left b))
      (get-right b))))
    
(defun small-right-turn (node)
  (let ((a node) (b (get-left node)))
    (node 
      (get-key b)
      (get-left b)
      (node 
        (get-key a)
        (get-right b)
        (get-right a)))))

(defun big-left-turn (node)
  (let ((a node) (b (get-right node)) (c (get-left (get-right node))))
    (node
      (get-key c)
      (node
        (get-key a)
        (get-left a)
        (get-left c))
      (node
        (get-key b)
        (get-right c)
        (get-right b)))))
        
(defun big-right-turn (node)
  (let ((a node)(b (get-left node))(c (get-right (get-left node))))
    (node
      (get-key c)
      (node
        (get-key b)
        (get-left b)
        (get-left c))
      (node
        (get-key a)
        (get-right c)
        (get-right a)))))

(defun balance-node (node &optional (test #'>))
  (when node
    (let ((new-node (node
      (get-key node)
      (balance-node (get-left node))
      (balance-node (get-right node)))))
    
      (let ((balance (get-balance new-node)))
        (cond
          ((funcall test balance 1) 
            (if (= (get-balance (get-right new-node)) -1)
              (big-left-turn new-node)
              (small-left-turn new-node)))
          ((funcall test -1 balance) 
            (if (= (get-balance (get-left new-node)) 1)
              (big-right-turn new-node)
              (small-right-turn new-node)))
          (t new-node))))))

(defun add-node (node value &optional (test #'>))
  (balance-node
    (labels ((add-node-rec (node value)
      (cond 
        ((null value) node)
        ((or (null node) (null (get-key node))) (node value))
        ((funcall test value (get-key node)) (node (get-key node) 
                                        (add-node-rec (get-left node) value) 
                                        (get-right node)))
        ((funcall test (get-key node) value) (node (get-key node) 
                                        (get-left node) 
                                        (add-node-rec (get-right node) value)))
        (t node))))
      (add-node-rec node value))
    test))

(defun add-list (node values &optional (test #'>))
  (if values 
    (add-list (add-node node (first values) test) (rest values) test)
    node))

(defun tree (values &optional (test #'>))
  (add-list nil values test))


(defun tree-to-list (node)
  (if node
    (append (tree-to-list (get-left node)) (list (get-key node)) (tree-to-list (get-right node)))))
    
(defun tree-to-list-back (node)
  (reverse (tree-to-list node)))

(defun merge-trees (node1 node2 &optional (test #'>))
  (tree (sort (append (tree-to-list node1) (tree-to-list node2)) test)))


(defun get-max (node)
  (if (get-right node)
    (get-max (get-right node))
    (get-key node)))

(defun get-min(node)
  (if (get-left node)
    (get-min (get-left node))
    (get-key node)))


(defun remove-node (node value)
  (balance-node 
    (labels ((remove-node-rec(node value)
      (cond
        ((null value) node)
        ((null node) node)
        ((is-less value (get-key node)) (node (get-key node) 
                                        (remove-node-rec (get-left node) value) 
                                        (get-right node)))
        ((is-greater value (get-key node)) (node (get-key node) 
                                        (get-left node) 
                                        (remove-node-rec (get-right node) value)))
        (t (if (null (get-left node))
          (if (null (get-right node))
            nil
            (get-right node))
          (if (null (get-right node))
            (get-left node)
            (let ((max-value (get-max (get-left node))))
              (node
                max-value
                (remove-node-rec (get-left node) max-value)
                (get-right node)))))))))
  
      (remove-node-rec node value))))

(defun remove-list (node values)
  (if values
    (remove-list (remove-node node (car values)) (cdr values))
    node))

(defun tree-contains (node value)
  (cond
    ((null node) nil)
    ((is-less value (get-key node)) (tree-contains (get-left node) value))
    ((is-greater value (get-key node)) (tree-contains (get-right node) value))
    (t t)))

(defun reduce-tree (f node &key reverse initial-value)
  (setf (fdefinition 'get-first) (if reverse #'get-right #'get-left))
  (setf (fdefinition 'get-second) (if reverse #'get-left #'get-right))
  (labels (
    (reduce-tree-rec (f node result count)
      (if node
        (multiple-value-bind
            (result count)
            (reduce-tree-rec f (get-first node) result count)
          (if (= 0 count)
            (reduce-tree-rec f (get-second node) (get-key node) 1)
            (reduce-tree-rec f (get-second node) (funcall f result (get-key node)) (+ count 1))))
        (values result count))))
    (if initial-value
      (reduce-tree-rec f node initial-value 1)
      (reduce-tree-rec f node nil 0))))

(defun get-size (node)
  (reduce-tree (lambda (x y) (+ x 1)) node))

(defun map-tree (f node)
  (reduce-tree (lambda (x y) (add-node x (funcall f y))) node :initial-value (empty-tree)))

(defun filter-tree (f node)
  (reduce-tree (lambda (x y) (if (funcall f y) (add-node x y) x)) node :initial-value (empty-tree)))

(defun compare-trees (tree1 tree2)
  (and 
    (= (get-size tree1) (get-size tree2))
    (reduce-tree (lambda (x y) (and x (tree-contains tree2 y))) tree1 :initial-value t)))
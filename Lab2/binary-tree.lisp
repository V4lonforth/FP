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

(defun node(value &optional left right)
  (cons value (cons left right)))

(defun get-key(node)
  (car node))

(defun get-left(node)
  (cadr node))

(defun get-right(node)
  (cddr node))


(defun get-height(node)
  (if node 
    (+ 1 (max (get-height (get-left node)) (get-height (get-right node))))
    0))

(defun get-balance(node)
  (- (get-height (get-right node)) (get-height (get-left node))))


(defun small-left-turn(node)
  (let ((a node) (b (get-right node)))
    (node 
      (get-key b)
      (node 
        (get-key a)
        (get-left a)
        (get-left b))
      (get-right b))))
    
(defun small-right-turn(node)
  (let ((a node) (b (get-left node)))
    (node 
      (get-key b)
      (get-left b)
      (node 
        (get-key a)
        (get-right b)
        (get-right a)))))

(defun big-left-turn(node)
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
        
(defun big-right-turn(node)
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

(defun balance-node(node)
  (when node
    (let ((new-node (node
      (get-key node)
      (balance-node (get-left node))
      (balance-node (get-right node)))))
    
      (let ((balance (get-balance new-node)))
        (cond
          ((> balance  1) 
            (if (= (get-balance (get-right new-node)) -1)
              (big-left-turn new-node)
              (small-left-turn new-node)))
          ((< balance -1) 
            (if (= (get-balance (get-left new-node)) 1)
              (big-right-turn new-node)
              (small-right-turn new-node)))
          (t new-node))))))


(defun add-node(node value)
  (balance-node 
    (labels ((add-node-rec (node value)
      (cond 
        ((null value) node)
        ((null node) (node value))
        ((< value (get-key node)) (node (get-key node) (add-node-rec (get-left node) value) (get-right node)))
        ((> value (get-key node)) (node (get-key node) (get-left node) (add-node-rec (get-right node) value)))
        (t node))))
      (add-node-rec node value))))

(defun add-list(node values)
  (if values 
    (add-list (add-node node (first values)) (rest values))
    node))

(defun tree(values)
  (add-list nil values))


(defun tree-to-list(node)
  (if node
    (append (tree-to-list(get-left node)) (list (get-key node)) (tree-to-list(get-right node)))))
    
(defun tree-to-list-back(node)
  (reverse (tree-to-list node)))

(defun merge-trees(node1 node2)
  (tree (sort (append (tree-to-list node1) (tree-to-list node2)) #'>)))


(defun get-max(node)
  (if (get-right node)
    (get-max (get-right node))
    (get-key node)))

(defun get-min(node)
  (if (get-left node)
    (get-min (get-left node))
    (get-key node)))


(defun remove-node(node value)
  (balance-node 
    (labels ((remove-node-rec(node value)
      (cond
        ((null value) node)
        ((null node) node)
        ((< value (get-key node)) (node (get-key node) (remove-node-rec (get-left node) value) (get-right node)))
        ((> value (get-key node)) (node (get-key node) (get-left node) (remove-node-rec (get-right node) value)))
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

(defun remove-list(node values)
  (if values
    (remove-list (remove-node node (car values)) (cdr values))
    node))


(defun map-tree(f node)
  (tree
    (loop for value in (tree-to-list node)
      collect (funcall f value))))


(defun filter-tree(f node)
  (tree
    (loop for value in (tree-to-list node)
      when (funcall f value)
        collect value)))

(defun reduce-tree(f node)
  (reduce f (tree-to-list node)))

(defun reduce-tree-back(f node)
  (reduce f (tree-to-list-back node)))


(defun compare-trees(tree1 tree2)
  (equal 
    (tree-to-list tree1)
    (tree-to-list tree2)))
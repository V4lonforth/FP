(load "C:/Users/V4lonforth/quicklisp/setup.lisp")
(ql:quickload "cl-ppcre")

(defpackage :lab4)
(in-package :lab4)
(use-package :ppcre)

(defmacro pattern-error () `'(nil))

(defmethod is-pattern (pattern)
  (and (consp pattern) (or (eq (car pattern) 'guard) (eq (car pattern) 'or))))

(defmethod eval-pattern (arg pattern)
  (cond
    ((eq (car pattern) 'guard) (guard-pattern arg (cdr pattern)))
    ((eq (car pattern) 'or) (or-pattern arg (cdr pattern)))))

(defmethod or-pattern (arg patterns)
  (loop
    for pattern in patterns
    do
      (let ((parsed-pattern (format-parsed-pattern (parse-pattern arg (format-pattern pattern)))))
        (print parsed-pattern)
        (when (validate-parsed-pattern parsed-pattern)
          (return-from or-pattern parsed-pattern))))
  (pattern-error))

(defmethod guard-pattern (arg pattern)
  (let ((parsed-pattern (parse-pattern arg (format-pattern (car pattern)))))
    (if (and (validate-parsed-pattern parsed-pattern) (eval-clause parsed-pattern (cadr pattern)))
      parsed-pattern
      (pattern-error))))

(defmethod format-pattern (pattern)
  (if (is-pattern pattern)
    `',pattern
    (cond
      ((consp pattern)
        (if (and (symbolp (car pattern)) (fboundp (car pattern)))
          `',(eval (cons (car pattern) (format-pattern (cdr pattern))))
          (cons (format-pattern (car pattern)) (format-pattern (cdr pattern)))))
      ((null pattern) nil)
      ((symbolp pattern) `',pattern)
      (t pattern))))

(defmethod parse-pattern (arg (pattern cons))
  (if (eq (car pattern) 'quote)
    (parse-pattern arg (cadr pattern))
    (if (is-pattern pattern)
      (eval-pattern arg pattern)
      (if (consp arg)
        (append (parse-pattern (car arg) (car pattern)) (parse-pattern (cdr arg) (cdr pattern)))
        (pattern-error)))))

(defmethod parse-pattern (arg (pattern symbol))
  (when (not (eq pattern '_))
    (list (list pattern `',arg))))

(defmethod parse-pattern ((arg number) (pattern number))
  (when (/= pattern arg) (pattern-error)))

(defmethod parse-pattern ((arg string) (pattern string))
  (multiple-value-bind
      (begin end)
      (scan pattern arg)
    (when (or (null begin) (null end) (/= (- end begin) (length arg)))
      (pattern-error))))

(defmethod parse-pattern (arg (pattern null))
  (when arg
    (pattern-error)))

(defmethod parse-pattern (arg pattern)
  (pattern-error))

(defmethod format-parsed-pattern (parsed-pattern)
  (when parsed-pattern
    (let ((var (find (car parsed-pattern) (cdr parsed-pattern) :test (lambda (x y) (equal (car x) (car y))))))
      (if var
        (if (equal (cdar parsed-pattern) (cdr var))
          (format-parsed-pattern (cdr parsed-pattern))
          (pattern-error))
        (cons (car parsed-pattern) (format-parsed-pattern (cdr parsed-pattern)))))))

(defun validate-parsed-pattern (parsed-pattern)
  (not (position nil parsed-pattern)))

(defun eval-clause (parsed-pattern body)
  (eval `(let ,parsed-pattern ,body)))

(defmacro match (arg &body clauses)
  `(block match-block
    ,@(loop
        for clause in clauses
        collect
          `(let ((parsed-pattern (format-parsed-pattern (parse-pattern ,arg (format-pattern ',(car clause))))))
            (when (validate-parsed-pattern parsed-pattern)
              ;(return-from match-block (eval `(let ,parsed-pattern ,(cadr ',clause)))))))))
              (return-from match-block (eval-clause parsed-pattern ',(cadr clause))))))))
              
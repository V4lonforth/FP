(load "Lab3/number-generator.lisp")
(load "C:/Users/V4lonforth/quicklisp/setup.lisp")
(ql:quickload :prove)

(in-package :lab3.number-generator)
(use-package :prove)

(subtest "Generation"
  (ok
    (equal
      (get-number-list 0 5 0.5)
      '(0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0))))
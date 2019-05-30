(load "C:/Users/V4lonforth/quicklisp/setup.lisp")
(ql:quickload :prove)

(prove:run #P"Lab2/unit-tests.lisp" :reporter :list)
(prove:run #P"Lab2/property-tests.lisp" :reporter :list)
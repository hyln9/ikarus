#!/usr/bin/env ikarus --script

(import scheme)

(define (test-one input str)
  (printf "  T: ~s " input)
  (let ([v0 (eval input)]) 
    (printf "c")
;    (collect)
    (let ([v1 (alt-compile input)])
      (printf "r")
      (if (equal? v0 v1)
          (printf "d ok\n")
          (error "values differed, expected ~s, got ~s" v0 v1)))))



(define-syntax add-tests-with-string-output 
  (syntax-rules (=>)
    [(_ name [test => res] ...)
     (begin
       (printf "TESTING ~a ...\n" 'name)
       (test-one 'test 'res) ...
       (printf "OK\n"))]))

(load "tests/tests-1.1-req.scm")
(load "tests/tests-1.2-req.scm")
(load "tests/tests-1.3-req.scm")
(load "tests/tests-1.4-req.scm")
(load "tests/tests-1.5-req.scm")
(load "tests/tests-1.6-req.scm")
(load "tests/tests-1.7-req.scm")
(load "tests/tests-1.8-req.scm")
(load "tests/tests-1.9-req.scm")
(load "tests/tests-2.1-req.scm")
(load "tests/tests-2.2-req.scm")
(load "tests/tests-2.3-req.scm")
(load "tests/tests-2.4-req.scm")
(load "tests/tests-2.6-req.scm")
(load "tests/tests-4.1-req.scm")
(load "tests/tests-new.scm")
;(load "tests/tests-5.2-req.scm")
;(load "tests/tests-5.3-req.scm")
(printf "HAPPY HAPPY JOY JOY\n")
(exit)

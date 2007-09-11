#!/usr/bin/env ikarus --r6rs-script
(import (ikarus))


      

(define (rationalize x eps) 
  (simplest (- x eps) (+ x eps)))


(define (simplest x y) 
  (cond
    [(< y x) (simplest y x)]
    [(= x y) x]
    [(> x 0)
     (let ([n (numerator x)] [d (denominator x)]
           [n^ (numerator y)] [d^ (denominator y)])
       (simplest^ n d n^ d^))]
    [(< y 0) 
     (let ([n (numerator x)] [d (denominator x)]
           [n^ (numerator y)] [d^ (denominator y)])
       (- (simplest^ (- n^) d^ (- n) d)))]
    [else 1]))

(define (simplest^ n d n^ d^)
  (let-values ([(q r) (quotient+remainder n d)])
    (if (= r 0) 
        q
        (let-values ([(q^ r^) (quotient+remainder n^ d^)])
          (if (= q q^) 
              (let ([v (simplest^ d^ r^ d r)])
                (let ([n^^ (numerator v)] [d^^ (denominator v)])
                  (/ (+ (* q n^^) d^^) n^^)))
              (+ q 1))))))

(define (test v0 v1 r) 
  (let ([s (rationalize v0 v1)])
    (unless (= s r) 
      (error 'test "failed in ~s ~s => ~s, should be ~s" 
             v0 v1 s r))))

(test 314/100 1/100 22/7)
(test (exact 0.3) 1/10 1/3)
(test 0.3 1/10 #i1/3) 
(test (/ 1.0 0.0) 3 (/ 1.0 0.0))
;;; dead
(test (/ 1.0 0.0) (/ 1.0 0.0) (/ (/ 0.0 0.0) (/ 1.0 0.0)))



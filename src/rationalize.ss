#!/usr/bin/env ikarus --r6rs-script
(import (ikarus))


      

#;
(define (rationalize x eps) 
  (define who 'rationalize)
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
  (define (go x eps)
    (simplest (- x eps) (+ x eps)))
  (cond
    [(flonum? x) 
     (if (flfinite? x)
         (cond
           [(flonum? eps) 
            (if (flfinite? eps) (go x eps) +nan.0)]
           [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
            (go x eps)]
           [else (error who "~s is not a number" eps)])
         (cond
           [(flonum? eps) 
            (if (flfinite? eps) x +nan.0)]
           [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
            x]
           [else (error who "~s is not a number" eps)]))]
    [(or (fixnum? x) (bignum? x) (ratnum? x))
     (cond
       [(flonum? eps) 
        (if (flfinite? eps) (go x eps) +nan.0)]
       [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
        (go x eps)]
       [else (error who "~s is not a number" eps)])]
    [else (error who "~s is not a number" x)]))


(define (test v0 v1 r) 
  (let ([s (time (rationalize v0 v1))])
    (unless (or (= s r) (and (flnan? s) (flnan? r)))
      (error 'test "failed in ~s ~s => ~s, should be ~s" 
             v0 v1 s r))))

(test 314/100 1/100 22/7)
(test #e0.3 1/10 1/3)
(test 0.3 1/10 #i1/3)
(test +inf.0 3 +inf.0)
(test +inf.0 +inf.0 +nan.0)



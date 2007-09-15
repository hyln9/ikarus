
(library (tests fxcarry)
  (export test-fxcarry)
  (import (ikarus) (tests framework))


(define (fx*/carry-reference fx1 fx2 fx3)
  (let* ([s (+ (* fx1 fx2) fx3)]
         [s0 (mod0 s (expt 2 (fixnum-width)))]
         [s1 (div0 s (expt 2 (fixnum-width)))])
    (values s0 s1)))

(define (fx+/carry-reference fx1 fx2 fx3)
  (let* ([s (+ (+ fx1 fx2) fx3)]
         [s0 (mod0 s (expt 2 (fixnum-width)))]
         [s1 (div0 s (expt 2 (fixnum-width)))])
    (values s0 s1)))

(define (fx-/carry-reference fx1 fx2 fx3)
  (let* ([s (- (- fx1 fx2) fx3)]
         [s0 (mod0 s (expt 2 (fixnum-width)))]
         [s1 (div0 s (expt 2 (fixnum-width)))])
    (values s0 s1)))

(define (test name fxop/carry fxop/carry-reference fx1 fx2 fx3)
  (let-values ([(s0 s1) (fxop/carry fx1 fx2 fx3)]
               [(s2 s3) (fxop/carry-reference fx1 fx2 fx3)])
    (unless (fx= s0 s2) 
      (error name "failed (value1) on ~s ~s ~s, got ~s, should be ~s" 
        fx1 fx2 fx3 s0 s2))
    (unless (fx= s1 s3) 
      (error name "failed (value2) on ~s ~s ~s, got ~s, should be ~s" 
        fx1 fx2 fx3 s1 s3))))

(define ls 
  (list 0 1 2 -1 -2 38734 -3843 2484598 -348732487 (greatest-fixnum) (least-fixnum)))

(define (test-fxcarry)
  (printf "[~s: test-fxcarry] " (expt (length ls) 3))
  (for-each 
    (lambda (fx1)
      (for-each 
        (lambda (fx2)
          (for-each 
            (lambda (fx3)
              (test 'fx*/carry fx*/carry fx*/carry-reference fx1 fx2 fx3)
              (test 'fx+/carry fx+/carry fx+/carry-reference fx1 fx2 fx3)
              (test 'fx-/carry fx-/carry fx-/carry-reference fx1 fx2 fx3))
            ls))
        ls))
    ls)
  (printf "Happy Happy Joy Joy\n"))

)

#!eof

(define (t x)
  (= (fxsra (fx+ x 1) 1) 
     (quotient x 2)))


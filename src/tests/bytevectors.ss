
(library (tests bytevectors)
  (export test-bytevectors)
  (import (ikarus) (tests framework))

  (define (not-bytevector? x) 
    (not (bytevector? x)))

  (define-tests test-bytevectors
    [bytevector? (make-bytevector 1)]
    [bytevector? (make-bytevector 1 17)]
    [bytevector? (make-bytevector 10 -17)]
    [not-bytevector? 'foo] 
    [not-bytevector? "hey"] 
    [not-bytevector? (current-output-port)] 
    [not-bytevector? (current-input-port)] 
    [not-bytevector? '#(2837 2398 239)] 
    [zero? (bytevector-length (make-bytevector 0))]
    [(lambda (x) (= x 100)) (bytevector-length (make-bytevector 100 -30))]
    [(lambda (x) (equal? x '(-127 129 -1 255)))
     (let ([b1 (make-bytevector 16 -127)]
           [b2 (make-bytevector 16 255)])
       (list 
         (bytevector-s8-ref b1 0)
         (bytevector-u8-ref b1 0)
         (bytevector-s8-ref b2 0)
         (bytevector-u8-ref b2 0)))]
    [(lambda (x) (equal? x '(-126 130 -10 246)))
     (let ([b (make-bytevector 16 -127)])
       (bytevector-s8-set! b 0 -126)
       (bytevector-u8-set! b 1 246)
       (list 
         (bytevector-s8-ref b 0)
         (bytevector-u8-ref b 0)
         (bytevector-s8-ref b 1)
         (bytevector-u8-ref b 1)))]
    [(lambda (x) (equal? x '(1 2 3 1 2 3 4 8)))
     (let ([b (u8-list->bytevector '(1 2 3 4 5 6 7 8))])
       (bytevector-copy! b 0 b 3 4)
       (bytevector->u8-list b))]
    [(lambda (x) (= x 17)) 
     (bytevector-uint-ref 
       (u8-list->bytevector '(17))
       0 'little 1)]
    [(lambda (x) (= x 17)) 
     (bytevector-uint-ref 
       (u8-list->bytevector '(17))
       0 'big 1)]
    [(lambda (x) (= x (+ 17 (* 54 256))))
     (bytevector-uint-ref 
       (u8-list->bytevector '(17 54))
       0 'little 2)]
    [(lambda (x) (= x (+ 17 (* 54 256))))
     (bytevector-uint-ref 
       (u8-list->bytevector (reverse '(17 54)))
       0 'big 2)] 
    [(lambda (x) (= x (+ 17 (* 54 256) (* 98 256 256))))
     (bytevector-uint-ref 
       (u8-list->bytevector '(17 54 98))
       0 'little 3)]
    [(lambda (x) (= x (+ 17 (* 54 256) (* 98 256 256))))
     (bytevector-uint-ref 
       (u8-list->bytevector (reverse '(17 54 98)))
       0 'big 3)] 
    [(lambda (x) (= x (+ 17 (* 54 256) (* 98 256 256) (* 120 256 256 256))))
     (bytevector-uint-ref 
       (u8-list->bytevector '(17 54 98 120))
       0 'little 4)]
    [(lambda (x) (= x #x123897348738947983174893204982390489))
     (bytevector-uint-ref 
       (u8-list->bytevector
         '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
           #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12))
       0 'little 18)]
    [(lambda (x) (= x #x123897348738947983174893204982390489))
     (bytevector-uint-ref 
       (u8-list->bytevector
         (reverse
           '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
             #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12)))
       0 'big 18)]
    [(lambda (x) (equal? x '(513 65283 513 513)))
     (let ([b (u8-list->bytevector '(1 2 3 255 1 2 1 2))])
       (bytevector->uint-list b 'little 2))]
    [(lambda (x) (equal? x '(513 -253 513 513)))
     (let ([b (u8-list->bytevector '(1 2 3 255 1 2 1 2))])
       (bytevector->sint-list b 'little 2))]
    [(lambda (x) (equal? x '(#xfffffffffffffffffffffffffffffffd
                             -3
                             (253 255 255 255 255 255 255 255
                              255 255 255 255 255 255 255 255))))
     (let ([b (make-bytevector 16 -127)])
       (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'little 16)
       (list 
         (bytevector-uint-ref b 0 'little 16)
         (bytevector-sint-ref b 0 'little 16)
         (bytevector->u8-list b)))]
    [(lambda (x) (equal? x '(#xfffffffffffffffffffffffffffffffd
                             -3
                             (255 255 255 255 255 255 255 255
                              255 255 255 255 255 255 255 253))))
     (let ([b (make-bytevector 16 -127)])
       (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'big 16)
       (list 
         (bytevector-uint-ref b 0 'big 16)
         (bytevector-sint-ref b 0 'big 16)
         (bytevector->u8-list b)))]
    ))






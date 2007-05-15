
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

    ))





